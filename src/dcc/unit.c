/* Copyright (c) 2017 Griefer@Work                                            *
 *                                                                            *
 * This software is provided 'as-is', without any express or implied          *
 * warranty. In no event will the authors be held liable for any damages      *
 * arising from the use of this software.                                     *
 *                                                                            *
 * Permission is granted to anyone to use this software for any purpose,      *
 * including commercial applications, and to alter it and redistribute it     *
 * freely, subject to the following restrictions:                             *
 *                                                                            *
 * 1. The origin of this software must not be misrepresented; you must not    *
 *    claim that you wrote the original software. If you use this software    *
 *    in a product, an acknowledgement in the product documentation would be  *
 *    appreciated but is not required.                                        *
 * 2. Altered source versions must be plainly marked as such, and must not be *
 *    misrepresented as being the original software.                          *
 * 3. This notice may not be removed or altered from any source distribution. *
 */
#ifndef GUARD_DCC_UNIT_C
#define GUARD_DCC_UNIT_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/target.h>
#include <dcc/vstack.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#ifdef _WIN32
#include <Windows.h>
#else
#include <sys/mman.h>
#endif

DCC_DECL_BEGIN

#define DCC_SECTION_HAVE_MERGE 0 /* Requires reference-counted data-allocation tracking. */

LOCAL struct DCCFreeRange *DCCFreeRange_New(target_ptr_t addr, target_siz_t size);
LOCAL void DCCSection_Destroy(struct DCCSection *__restrict self);
extern void DCCSection_InsSym(struct DCCSection *__restrict self, struct DCCSym *__restrict sym);
LOCAL void DCCSection_CheckRehash(struct DCCSection *__restrict self);
LOCAL void DCCSection_RehashSymbols(struct DCCSection *__restrict self, size_t newsize);
extern void DCCUnit_InsSym(/*ref*/struct DCCSym *__restrict sym);
LOCAL void DCCUnit_CheckRehash(void);
LOCAL void DCCUnit_RehashSymbols(size_t newsize);
LOCAL void DCCSym_ClearDef(struct DCCSym *__restrict self, int warn);

#if DCC_DEBUG
#define DCCSym_Alloc()     (struct DCCSym *)calloc(1,sizeof(struct DCCSym))
#define DCCSym_Free(self)  free(self)
#else
LOCAL struct DCCSym *DCCSym_Alloc(void);
LOCAL void DCCSym_Free(struct DCCSym *__restrict self);
#endif


/* Assert that the text buffer of 'self' can be modified. */
#define DCCSECTION_ASSERT_NOTANIMPORT(self) \
 assertf(!DCCSection_ISIMPORT(self),\
          "This operation cannot be performed on import-section '%s'",\
         (self)->sc_start.sy_name->k_name)

#ifdef DCC_COMPILER_FLAG_TEXTFLUSH
#define DCCSECTION_ASSERT_TEXT_FLUSHED(self) \
 (DCCSECTION_ASSERT_NOTANIMPORT(self),\
  assertf(!DCCSection_ISCURR(self) || \
         (compiler.c_flags&DCC_COMPILER_FLAG_TEXTFLUSH),\
          "Can't perform text operations without flushing the current text section '%s'",\
         (self)->sc_start.sy_name->k_name))
#else
#define DCCSECTION_ASSERT_TEXT_FLUSHED(self) \
 DCCSECTION_ASSERT_NOTANIMPORT(self)
#endif



PUBLIC int
DCCFreeData_InitCopy(struct DCCFreeData *__restrict self,
                     struct DCCFreeData const *__restrict right) {
 struct DCCFreeRange *iter,**ptarget,*range_copy;
 assert(self);
 assert(right);
 ptarget = &self->fd_begin;
 iter = right->fd_begin;
 while (iter) {
  range_copy = (struct DCCFreeRange *)malloc(sizeof(struct DCCFreeRange));
  if unlikely(!range_copy) goto err;
  range_copy->fr_addr = iter->fr_addr;
  range_copy->fr_size = iter->fr_size;
  *ptarget = range_copy;
  ptarget = &range_copy->fr_next;
  iter = iter->fr_next;
 }
 *ptarget = NULL;
 return 1;
err:
 if (ptarget != &self->fd_begin) {
  range_copy = self->fd_begin;
  for (;;) {
   iter = range_copy->fr_next;
   free(range_copy);
   /* The following isn't really a dereference, because of the section reference! */
   if (ptarget != &range_copy->fr_next) break;
   range_copy = iter;
  }
 }
 return 0;
}

PUBLIC void
DCCFreeData_Quit(struct DCCFreeData *__restrict self) {
 struct DCCFreeRange *iter,*next;
 assert(self);
 iter = self->fd_begin;
 while (iter) {
  next = iter->fr_next;
  free(iter);
  iter = next;
 }
}

PUBLIC target_ptr_t
DCCFreeData_Acquire(struct DCCFreeData *__restrict self,
                    target_siz_t size, target_siz_t align,
                    target_siz_t offset) {
 struct DCCFreeRange *iter,**piter,*newrange;
 target_ptr_t result;
 assert(self);
 piter = &self->fd_begin;
 while ((iter = *piter) != NULL) {
  if (iter->fr_size >= size) {
   target_siz_t upper_overflow;
   upper_overflow = (iter->fr_size-size);
   result  = iter->fr_addr+upper_overflow;
   result &= ~(align-1);
   result += offset;
   if (result >= iter->fr_addr) {
    assert(iter->fr_addr+iter->fr_size >= result+size);
    upper_overflow = (iter->fr_addr+iter->fr_size)-(result+size);
    /* Re-use this free block of data. */
    iter->fr_size = result-iter->fr_addr;
    if (!iter->fr_size) {
     assert(!upper_overflow);
     /* Delete this block. */
     *piter = iter->fr_next;
     free(iter);
    } else if (upper_overflow) {
     /* Create a new range for the upper overflow. */
     newrange = DCCFreeRange_New(result+size,upper_overflow);
     if likely(newrange) {
      newrange->fr_next = iter->fr_next;
      iter->fr_next = newrange;
     }
    }
    return result;
   }
  }
  piter = &iter->fr_next;
 }
 return DCC_FREEDATA_INVPTR;
}
PUBLIC target_ptr_t
DCCFreeData_AcquireAt(struct DCCFreeData *__restrict self,
                      target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange *iter,**piter;
 target_ptr_t addr_end = addr+size;
 assert(self);
 piter = &self->fd_begin;
 while ((iter = *piter) != NULL) {
  if (iter->fr_addr >= addr_end) break;
  if (addr     >= iter->fr_addr &&
      addr_end <= iter->fr_addr+iter->fr_size) {
   /* The requested address range lies within this part. */
   assert(iter->fr_size >= size);
   if (addr == iter->fr_addr) {
    /* Allocate at the front. */
    iter->fr_addr += size;
    iter->fr_size -= size;
    if (!iter->fr_size) {
     /* Delete this part. */
     *piter = iter->fr_next;
     free(iter);
    }
   } else if (addr_end == iter->fr_addr+iter->fr_size) {
    /* Allocate at the back. */
    iter->fr_size -= size;
   } else {
    struct DCCFreeRange *newpart;
    /* Split the part. */
    newpart = DCCFreeRange_New(addr_end,(iter->fr_addr+iter->fr_size)-addr_end);
    if unlikely(!newpart) break;
    newpart->fr_next = iter->fr_next;
    iter->fr_next = newpart;
    assert(addr > iter->fr_addr);
    iter->fr_size = addr-iter->fr_addr;
   }
   return addr;
  }
  piter = &iter->fr_next;
 }
 return DCC_FREEDATA_INVPTR;
}


LOCAL struct DCCFreeRange *
DCCFreeRange_New(target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange *result;
 result = (struct DCCFreeRange *)malloc(sizeof(struct DCCFreeRange));
 if likely(result) {
  result->fr_addr = addr;
  result->fr_size = size;
 }
 return result;
}

PUBLIC void
DCCFreeData_Release(struct DCCFreeData *__restrict self,
                    target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange *prev,*iter,*newslot;
 target_ptr_t addr_end = addr+size;
 prev = NULL,iter = self->fd_begin;
 while (iter) {
  if (addr_end <= iter->fr_addr) {
   if (addr_end == iter->fr_addr) {
    /* Extend this free range below. */
    iter->fr_addr -= size;
    iter->fr_size += size;
check_merge:
    assert(!prev || iter->fr_addr >= prev->fr_addr+
                                     prev->fr_size);
    if (prev &&
        iter->fr_addr == prev->fr_addr+
                         prev->fr_size) {
     /* Merge the two ranges. */
     prev->fr_next = iter->fr_next;
     prev->fr_size += iter->fr_size;
     free(iter);
    }
   } else if (!prev) {
    /* Create a new range at the front. */
    newslot = DCCFreeRange_New(addr,size);
    if unlikely(!newslot) return;
    assert(iter == self->fd_begin);
    newslot->fr_next = iter;
    self->fd_begin = newslot;
   } else if (addr == prev->fr_addr+prev->fr_size) {
    /* Extend the previous range above. */
    prev->fr_size += size;
    goto check_merge;
   } else {
    /* Create a new range. */
    newslot = DCCFreeRange_New(addr,size);
    if unlikely(!newslot) return;
    newslot->fr_next = iter;
    prev->fr_next = newslot;
   }
   return;
  }
  prev = iter;
  iter = iter->fr_next;
 }
 /* Append at the end. */
 newslot = DCCFreeRange_New(addr,size);
 if unlikely(!newslot) return;
 newslot->fr_next = NULL;
 if (prev) prev->fr_next = newslot;
 else      self->fd_begin = newslot;
}










/* ****** */
/* MEMLOC */
/* ****** */
PUBLIC int
DCCMemLoc_Equal(struct DCCMemLoc const *__restrict a,
                struct DCCMemLoc const *__restrict b) {
 assert(a);
 assert(b);
 if (a->ml_reg != b->ml_reg) return 0;
 if (a->ml_sym) {
  struct DCCSym *sa,*sb;
  target_off_t off_a,off_b;
  if (!b->ml_sym) return 0;
  sa = a->ml_sym,sb = b->ml_sym;
  while ((DCCSym_ASSERT(sa),sa->sy_alias)) sa = sa->sy_alias;
  while ((DCCSym_ASSERT(sb),sb->sy_alias)) sb = sb->sy_alias;
  if (!DCCSym_SECTION(sa) || DCCSym_SECTION(sa) != DCCSym_SECTION(sb)) return 0;
  off_a = a->ml_off+a->ml_sym->sy_addr;
  off_b = b->ml_off+b->ml_sym->sy_addr;
  if (off_a != off_b) return 0;
 }
 else if (b->ml_sym) return 0;
 else if (a->ml_off != b->ml_off) return 0;
 return 1;
}

PUBLIC int
DCCMemLoc_Contains(struct DCCMemLoc const *__restrict vector,
                   target_siz_t vector_size_in_bytes,
                   struct DCCMemLoc const *__restrict pointer) {
 target_ptr_t vecaddr,ptraddr;
 assert(vector);
 assert(pointer);
 /* Make sure that the register storage duration is equal. */
 if (vector->ml_reg != pointer->ml_reg) return 2;
 vecaddr = (target_ptr_t)vector->ml_off;
 ptraddr = (target_ptr_t)pointer->ml_off;
 if (vector->ml_sym) {
  if (!pointer->ml_sym) return 0;
  if (!DCCSym_SECTION(vector->ml_sym) ||
      (DCCSym_SECTION(vector->ml_sym) !=
       DCCSym_SECTION(pointer->ml_sym))) return 2;
  vecaddr += vector->ml_sym->sy_addr;
  ptraddr += pointer->ml_sym->sy_addr;
 } else if (pointer->ml_sym) {
  return 0;
 }
 /* Check if the pointers/offsets overlap. */
 return (vecaddr <= ptraddr &&
        (vecaddr+vector_size_in_bytes > ptraddr ||
        (vecaddr-1)+vector_size_in_bytes > ptraddr-1));
}











/* ******* */
/* SECTION */
/* ******* */

LOCAL void
DCCSection_Destroy(struct DCCSection *__restrict self) {
 assert(self);
 assert(!DCCSection_ISCURR(self));
 assert((self->sc_pself != NULL) ==
        (self->sc_unit != NULL));
 /* Unlink from the per-unit chain of existing sections. */
 if (self->sc_pself) {
  if ((*self->sc_pself = self->sc_next) != NULL)
        self->sc_next->sc_pself = self->sc_pself;
  if (DCCSection_ISIMPORT(self)) {
   assert(self->sc_unit->u_impc);
   --self->sc_unit->u_impc;
   assert((self->sc_unit->u_impc != 0) ==
          (self->sc_unit->u_imps != NULL));
  } else {
   assert(self->sc_unit->u_secc);
   --self->sc_unit->u_secc;
   assert((self->sc_unit->u_secc != 0) ==
          (self->sc_unit->u_secs != NULL));
  }
 } else {
  assert(!self->sc_unit);
  assert(!self->sc_pself);
  assert(!self->sc_next);
 }
 /* Delete section symbols. */
 { struct DCCSym **biter,**bend,*iter,*next;
   bend = (biter = self->sc_symv)+self->sc_syma;
   for (; biter != bend; ++biter) {
    iter = *biter;
    while (iter) {
     next = iter->sy_sec_next;
     iter->sy_sec_pself = NULL;
     iter->sy_sec_next  = NULL;
     iter = next;
    }
   }
   free(self->sc_symv);
 }

 if (!DCCSection_ISIMPORT(self)) {
  /* Delete the text buffer. */
  free(self->sc_text.tb_begin);
#if DCC_DEBUG
  self->sc_text.tb_begin = NULL;
#endif

  /* Delete relocations. */
  { struct DCCRel *iter,*end;
    end = (iter = self->sc_relv)+self->sc_relc;
    for (; iter != end; ++iter) {
     assert(iter->r_sym);
     DCCSym_Decref(iter->r_sym);
    }
    free(self->sc_relv);
  }
  /* Delete free section memory. */
  DCCFreeData_Quit(&self->sc_free);

  /* Delete base memory. */
#ifdef DCC_SYMFLAG_SEC_OWNSBASE
  if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_OWNSBASE) {
#ifdef _WIN32
   VirtualFree((LPVOID)self->sc_base,
               (size_t)(self->sc_text.tb_end-
                        self->sc_text.tb_begin),
                MEM_DECOMMIT);
#else
   munmap((void *)self->sc_base,
          (size_t)(self->sc_text.tb_end-
                   self->sc_text.tb_begin));
#endif
  }
#endif /* DCC_SYMFLAG_SEC_OWNSBASE */
 }
}

PUBLIC void
_DCCSym_Delete(struct DCCSym *__restrict self) {
 DCCSym_ASSERT(self);
 assert(!self->sy_unit_next);
#if DCC_TARGET_BIN == DCC_BINARY_PE
 DCCSym_XDecref(self->sy_peind);
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 if (self->sy_sec_pself) {
  if ((*self->sy_sec_pself = self->sy_sec_next) != NULL) {
   self->sy_sec_next->sy_sec_pself = self->sy_sec_pself;
   DCCSym_ASSERT(self->sy_sec_next);
  }
  /* Remove the symbol from the section. */
  assert(!self->sy_alias);
  assert(self->sy_sec);
  assert(self != &self->sy_sec->sc_start);
  assert(self->sy_sec->sc_symc);
  --self->sy_sec->sc_symc;
 } else {
  assert(!self->sy_sec_next);
 }
 if (self->sy_unit_before) {
  assert(self->sy_unit_before->sy_name == self->sy_name);
  DCCSym_Decref(self->sy_unit_before);
 }
 if (DCCSym_ISSECTION(self)) {
  assert(!self->sy_alias);
  DCCSection_Destroy((struct DCCSection *)((uintptr_t)self-
                      offsetof(struct DCCSection,sc_start)));
  free(self);
 } else {
  if (self->sy_alias) {
   DCCSym_Decref(self->sy_alias);
  } else if (self->sy_sec) {
   if (self->sy_size) {
    DCCSection_DFree(self->sy_sec,
                     self->sy_addr,
                     self->sy_size);
   }
   DCCSection_Decref(self->sy_sec);
  }
  DCCSym_Free(self);
 }
}


PUBLIC void
DCCSection_Clear(struct DCCSection *__restrict self) {
 assert(self);
 /* Delete section symbols. */
 { struct DCCSym **bbegin,**biter,**bend,*iter,*next;
   bend = (biter = bbegin = self->sc_symv)+self->sc_syma;
   self->sc_symc = 0;
   self->sc_syma = 0;
   self->sc_symv = NULL;
   for (; biter != bend; ++biter) {
    iter = *biter;
    while (iter) {
     next = iter->sy_sec_next;
     assert(iter->sy_sec == self);
     assert(self->sc_start.sy_refcnt);
     --self->sc_start.sy_refcnt;
     iter->sy_sec_pself = NULL;
     iter->sy_sec_next  = NULL;
     iter->sy_sec       = NULL;
     iter->sy_addr      = 0;
     iter->sy_size      = 0;
     iter = next;
    }
   }
   free(bbegin);
   assert(!self->sc_syma);
   assert(!self->sc_symv);
   assert(!self->sc_symc);
 }
 if (!DCCSection_ISIMPORT(self)) {
  /* Delete relocations. */
  { struct DCCRel *begin,*iter,*end;
    end = (iter = begin = self->sc_relv)+self->sc_relc;
    self->sc_relv = NULL;
    self->sc_relc = 0;
    self->sc_rela = 0;
    for (; iter != end; ++iter) {
     assert(iter->r_sym);
     DCCSym_Decref(iter->r_sym);
    }
    free(begin);
    assert(!self->sc_rela);
    assert(!self->sc_relc);
    assert(!self->sc_relv);
  }
 }
}


/* *** */
/* SYM */
/* *** */
PUBLIC /*ref*/struct DCCSym *
DCCSym_New(struct TPPKeyword const *__restrict name,
           symflag_t flags) {
 struct DCCSym *result;
 assert(name);
 result = DCCSym_Alloc();
 if unlikely(!result) goto seterr;
 result->sy_refcnt = 1;
 result->sy_name   = name;
 result->sy_flags  = flags;
 DCCSym_ASSERT(result);
 return result;
seterr: TPPLexer_SetErr();
 return NULL;
}

LOCAL void
DCCSym_ClearDef(struct DCCSym *__restrict self, int warn) {
 if (self->sy_sec || self->sy_alias) {
  struct DCCSection *old_sec;
  struct DCCSym *old_alias;
  /* Simply allow overriding a weak symbol. */
  if (warn && !(self->sy_flags&DCC_SYMFLAG_WEAK)) {
   WARN(W_SYMBOL_ALREADY_DEFINED,self->sy_name->k_name);
  }
  old_sec        = self->sy_sec; /* Inherit reference. */
  old_alias      = self->sy_alias; /* Inherit reference. */
  self->sy_sec   = NULL;
  self->sy_alias = NULL;
  if (self->sy_sec_pself) {
   assert(old_sec);
   assert(old_sec->sc_symc);
   *self->sy_sec_pself = self->sy_sec_next;
   --old_sec->sc_symc;
   self->sy_sec_pself = NULL;
   self->sy_sec_next  = NULL;
  }
  if (old_sec) DCCSection_Decref(old_sec);
  if (old_alias) DCCSym_Decref(old_alias);
 }
}
PUBLIC void
DCCSym_ClrDef(struct DCCSym *__restrict self) {
 DCCSym_ASSERT(self);
 DCCSym_ClearDef(self,0);
}
PUBLIC void
DCCSym_Define(struct DCCSym *__restrict self,
              struct DCCSection *__restrict section,
              target_ptr_t addr, target_siz_t size) {
 DCCSym_ASSERT(self);
 assert(section);
 DCCSym_ClearDef(self,1);
 self->sy_addr = addr;
 self->sy_size = size;
 self->sy_sec  = section; /* Inherit reference. */
 DCCSection_Incref(section); /* Create reference for above. */
 DCCSection_InsSym(section,self);
}
PUBLIC void
DCCSym_Alias(struct DCCSym *__restrict self,
             struct DCCSym *__restrict alias_sym) {
 DCCSym_ASSERT(self);
 DCCSym_ASSERT(alias_sym);
 DCCSym_ClearDef(self,1);
 DCCSym_Incref(alias_sym);
 self->sy_alias = alias_sym; /* Inherit reference. */
}
PUBLIC int
DCCSym_Equal(struct DCCSym const *a,
             struct DCCSym const *b) {
 if (!a || !b) return 0;
 while ((DCCSym_ASSERT(a),a->sy_alias)) a = a->sy_alias;
 while ((DCCSym_ASSERT(b),b->sy_alias)) b = b->sy_alias;
 if (a == b) return 1;
 if (!a->sy_sec || b->sy_sec != a->sy_sec) return 0;
 return a->sy_addr == b->sy_addr;
}

PUBLIC int
DCCSym_Contains(struct DCCSym const *__restrict v, target_siz_t vsiz,
                struct DCCSym const *__restrict p) {
 DCCSym_ASSERT(v),DCCSym_ASSERT(p);
 if unlikely(!vsiz) return 0;
 if (v == p) return 1;
 if (!v->sy_sec || p->sy_sec != v->sy_sec) return 0;
 return (p->sy_addr >= v->sy_addr &&
        (v->sy_addr+vsiz > p->sy_addr ||
        (v->sy_addr-1)+vsiz > (p->sy_addr-1)));
}




LOCAL void
DCCSection_RehashSymbols(struct DCCSection *__restrict self,
                         size_t newsize) {
 struct DCCSym **new_map,**biter,**bend,*iter,*next,**dst;
 assert(newsize);
 new_map = (struct DCCSym **)calloc(newsize,sizeof(struct DCCSym *));
 if unlikely(!new_map) return;
 bend = (biter = self->sc_symv)+self->sc_syma;
 for (; biter != bend; ++biter) {
  iter = *biter;
  while (iter) {
   assert(iter->sy_sec == self);
   next = iter->sy_sec_next;
   dst = &new_map[DCCSym_HASH(iter) % newsize];
   if ((iter->sy_sec_next = *dst) != NULL) {
    iter->sy_sec_next->sy_sec_pself = &iter->sy_sec_next;
    DCCSym_ASSERT(iter->sy_sec_next);
   }
   iter->sy_sec_pself = dst;
   *dst = iter;
   DCCSym_ASSERT(iter);
   iter = next;
  }
 }
 free(self->sc_symv);
 self->sc_symv = new_map;
 self->sc_syma = newsize;
}

LOCAL void
DCCSection_CheckRehash(struct DCCSection *__restrict self) {
 if ((self->sc_symc*3)/2 >= self->sc_syma) {
  size_t newsize = self->sc_syma;
  if unlikely(!newsize) newsize = 4;
  newsize *= 2;
  /* Must allocate more symbols. */
  DCCSection_RehashSymbols(self,newsize);
 }
}

void
DCCSection_InsSym(struct DCCSection *__restrict self,
                  struct DCCSym *__restrict sym) {
 struct DCCSym **pbucket;
 assert(self);
 assert(sym->sy_sec == self);
 DCCSection_CheckRehash(self);
 if unlikely(!self->sc_syma) goto seterr;
 assert(self->sc_symv);
 ++self->sc_symc;
 pbucket = &self->sc_symv[DCCSym_HASH(sym) % self->sc_syma];
 if ((sym->sy_sec_next = *pbucket) != NULL) {
  sym->sy_sec_next->sy_sec_pself = &sym->sy_sec_next;
  DCCSym_ASSERT(sym->sy_sec_next);
 }
 sym->sy_sec_pself = pbucket;
 *pbucket = sym;
 DCCSym_ASSERT(sym);
 return;
seterr: TPPLexer_SetErr();
}



PUBLIC /*ref*/struct DCCSection *
DCCSection_New(struct TPPKeyword const *__restrict name,
               symflag_t flags) {
 struct DCCSection *result;
 assert(name);
 if (flags&DCC_SYMFLAG_SEC_ISIMPORT) {
  /* Allocate an import section! */
  result = (struct DCCSection *)calloc(1,offsetof(struct DCCSection,sc_text));
  if unlikely(!result) goto seterr;
 } else {
  result = (struct DCCSection *)calloc(1,sizeof(struct DCCSection));
  if unlikely(!result) goto seterr;
  result->sc_align          = 1;
 }
 result->sc_start.sy_refcnt = 1;
 result->sc_start.sy_name   = name;
 result->sc_start.sy_sec    = result; /* Inherit reference (indirect loop). */
 result->sc_start.sy_flags  = flags;
 return result;
seterr: TPPLexer_SetErr();
 return NULL;
}

PUBLIC /*ref*/struct DCCSection *
DCCSection_News(char const *__restrict name, symflag_t flags) {
 struct TPPKeyword *kname;
 kname = TPPLexer_LookupKeyword(name,strlen(name),1);
 return kname ? DCCSection_New(kname,flags) : NULL;
}


PUBLIC void
DCCSection_Putrelo(struct DCCSection *__restrict self,
                   struct DCCRel const *__restrict relo) {
 struct DCCRel *iter,*begin,*end;
 assert(self);
 assert(relo);
 DCCSECTION_ASSERT_NOTANIMPORT(self);
 if (self->sc_relc == self->sc_rela) {
  size_t newalloc = self->sc_rela;
  if (!newalloc) newalloc = 1;
  newalloc *= 2;
  /* Allocate more relocation entries. */
  begin = (struct DCCRel *)realloc(self->sc_relv,newalloc*
                                   sizeof(struct DCCRel));
  if unlikely(!begin) return;
  self->sc_relv = begin;
  self->sc_rela = newalloc;
 } else {
  begin = self->sc_relv;
 }
 end = iter = begin+self->sc_relc;
 while (iter != begin && iter[-1].r_addr < relo->r_addr) --iter;
 memmove(iter+1,iter,(size_t)((end-iter)*sizeof(struct DCCRel)));
 *iter = *relo;
 DCCSym_Incref(iter->r_sym);
 ++self->sc_relc;
}

PUBLIC struct DCCRel *
DCCSection_Allocrel(struct DCCSection *__restrict self,
                    size_t n_relocs) {
 size_t minsize,newsize;
 struct DCCRel *result;
 assert(self);
 DCCSECTION_ASSERT_NOTANIMPORT(self);
 minsize = self->sc_relc+n_relocs;
 if (minsize > self->sc_relc) {
  /* Must increase the buffer size. */
  newsize = self->sc_rela;
  if unlikely(!newsize) newsize = 1;
  do newsize *= 2; while (newsize < minsize);
  result = (struct DCCRel *)realloc(self->sc_relv,newsize*
                                    sizeof(struct DCCRel));
  if unlikely(!result) goto seterr;
  self->sc_relv = result;
  self->sc_rela = newsize;
 }
 result = self->sc_relv+self->sc_relc;
 self->sc_relc += n_relocs;
 return result;
seterr: TPPLexer_SetErr();
 return NULL;
}

PUBLIC size_t
DCCSection_Delrel(struct DCCSection *__restrict self,
                  target_ptr_t addr, target_siz_t size) {
 struct DCCRel *del_end,*del_begin,*begin,*end;
 struct DCCSym **old_symv,**sym_iter,**sym_end;
 target_ptr_t addr_end = addr+size;
 size_t result; int is_malloc_ptr = 0;
 assert(self);
 DCCSECTION_ASSERT_NOTANIMPORT(self);
 end = del_end = (begin = self->sc_relv)+self->sc_relc;
 while (del_end != begin && del_end[-1].r_addr > addr_end) --del_end;
 del_begin = del_end;
 while (del_begin != begin && del_begin[-1].r_addr <= addr) --del_end;
 result = (size_t)(end-del_end);
 /* Safe all relocation symbols. */
 if (result >= 512) { /* Use malloc for large relocation counts. */
  old_symv = (struct DCCSym **)malloc(result*sizeof(struct DCCSym *));
  if unlikely(!old_symv) goto stack_alloc_oldsym;
  is_malloc_ptr = 1;
 } else {
stack_alloc_oldsym:
  old_symv = (struct DCCSym **)alloca(result*sizeof(struct DCCSym *));
 }
 for (begin = del_begin,sym_iter = old_symv;
      begin != del_end; ++begin,++sym_iter) *sym_iter = begin->r_sym;
 assert(result >= self->sc_relc);
 memmove(del_begin,del_end,result*sizeof(struct DCCRel));
 self->sc_relc -= result;
 sym_end = (sym_iter = old_symv)+result;
 /* Drop references from the old relocation symbols.
  * This must be done after we've removed the relocations in case
  * dropping references here will recursively free up more section memory,
  * which in turn may cause additional relocations to be dropped from
  * the same section. Because of that, the relocation vector must be
  * in a consistent state every time we're decref-ing a symbol. */
 for (; sym_iter != sym_end; ++sym_iter) DCCSym_Decref(*sym_iter);
 if (is_malloc_ptr) free(old_symv);
 return result;
}

PUBLIC size_t
DCCSection_ResolveDisp(struct DCCSection *__restrict self) {
 struct DCCRel *iter,*end;
 uint8_t *base_address;
 size_t result = 0;
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 end = (iter = self->sc_relv)+self->sc_relc;
 base_address = (uint8_t *)self->sc_base;
 if (!base_address) base_address = self->sc_text.tb_begin;
 goto begin;
next: ++iter;
begin: while (iter != end) {
  uint8_t *rel_addr; target_ptr_t rel_value;
  struct DCCSym *effective_sym = iter->r_sym;
  while ((DCCSym_ASSERT(effective_sym),effective_sym->sy_alias))
          effective_sym = effective_sym->sy_alias;
  /* We're only resolving relocations pointing back into our section. */
  if (effective_sym->sy_sec != self) goto next;
  rel_addr  = base_address+iter->r_addr;
  rel_value = effective_sym->sy_addr;
  switch (iter->r_type) {
#if DCC_TARGET_IA32(386)
  case R_386_PC8:  *(int8_t  *)rel_addr += (int8_t )rel_value; break;
  case R_386_PC16: *(int16_t *)rel_addr += (int16_t)rel_value; break;
  case R_386_PC32: *(int32_t *)rel_addr += (int32_t)rel_value; break;
#elif DCC_TARGET_CPU == DCC_TARGET_X86_64
  case R_X86_64_PC8:  *(int8_t  *)rel_addr += (int8_t )rel_value; break;
  case R_X86_64_PC16: *(int16_t *)rel_addr += (int16_t)rel_value; break;
  case R_X86_64_PC32: *(int32_t *)rel_addr += (int32_t)rel_value; break;
#else
#   error FIXME
#endif
  default: goto next;
  }
  /* Delete this relocation. */
  DCCSym_Decref(iter->r_sym);
  --end,--self->sc_relc,++result;
  memmove(iter,iter+1,(end-iter)*sizeof(struct DCCRel));
 }
 return result;
}

PUBLIC void
DCCSection_SetBaseTo(struct DCCSection *__restrict self,
                     target_ptr_t address) {
 assert(self);
 DCCSECTION_ASSERT_NOTANIMPORT(self);
#ifdef DCC_SYMFLAG_SEC_OWNSBASE
 if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_OWNSBASE) {
  self->sc_start.sy_flags &= ~(DCC_SYMFLAG_SEC_OWNSBASE);
#ifdef _WIN32
  VirtualFree((LPVOID)self->sc_base,
              (size_t)(self->sc_text.tb_end-
                       self->sc_text.tb_begin),
               MEM_DECOMMIT);
#else
  munmap((void *)self->sc_base,
         (size_t)(self->sc_text.tb_end-
                  self->sc_text.tb_begin));
#endif
 }
#endif /* DCC_SYMFLAG_SEC_OWNSBASE */
 self->sc_base = address;
}

PUBLIC void
DCCSection_Reloc(struct DCCSection *__restrict self) {
 struct DCCRel *iter,*end;
 uint8_t *relbase,*reldata;
 target_ptr_t base_address;
 assert(self),DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 assert(self->sc_text.tb_max >= self->sc_text.tb_pos);
 /* Special case: Don't need to do anything if there are no relocations! */
 if unlikely(!self->sc_relc) return;
 base_address = self->sc_base;
 assert(base_address);
 relbase = self->sc_text.tb_begin;
#ifdef DCC_SYMFLAG_SEC_OWNSBASE
 if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_OWNSBASE) {
  relbase = (uint8_t *)base_address;
 }
#endif
 end = (iter = self->sc_relv)+self->sc_relc;
 for (; iter != end; ++iter) {
  target_ptr_t rel_value;
  struct DCCSym *effective_sym = iter->r_sym;
  while ((DCCSym_ASSERT(effective_sym),effective_sym->sy_alias))
          effective_sym = effective_sym->sy_alias;
  /* We're only resolving relocations pointing back into our section. */
  reldata   = relbase+iter->r_addr;
  rel_value = effective_sym->sy_addr;
  if (!effective_sym->sy_sec ||
      DCCSection_ISIMPORT(effective_sym->sy_sec)) {
   /* Unresolved weak symbols are always located at NULL. */
   if (!(effective_sym->sy_flags&DCC_SYMFLAG_WEAK)) {
    WARN(W_UNRESOLVED_REFERENCE,
         effective_sym->sy_name,
         self->sc_start.sy_name,
         iter->r_addr);
   }
  } else {
   assertf(DCCSection_HASBASE(effective_sym->sy_sec),
           "Section '%s' is missing its base",
           effective_sym->sy_sec->sc_start.sy_name->k_name);
   rel_value += (target_ptr_t)effective_sym->sy_sec->sc_base;
  }
  assert(reldata >= relbase);
  assert(reldata <  relbase+(self->sc_text.tb_end-
                             self->sc_text.tb_begin));
  switch (iter->r_type) {
#if DCC_TARGET_IA32(386)
  case R_386_8:       *(int8_t  *)reldata += (int8_t )rel_value; break;
  case R_386_16:      *(int16_t *)reldata += (int16_t)rel_value; break;
  case R_386_32:      *(int32_t *)reldata += (int32_t)rel_value; break;
  case R_386_PC8:     *(int8_t  *)reldata += (int8_t )(rel_value-base_address); break;
  case R_386_PC16:    *(int16_t *)reldata += (int16_t)(rel_value-base_address); break;
  case R_386_PC32:    *(int32_t *)reldata += (int32_t)(rel_value-base_address); break;
#elif DCC_TARGET_CPU == DCC_TARGET_X86_64
  case R_X86_64_8:    *(int8_t  *)reldata += (int8_t )rel_value; break;
  case R_X86_64_16:   *(int16_t *)reldata += (int16_t)rel_value; break;
  case R_X86_64_32:   *(int32_t *)reldata += (int32_t)rel_value; break;
  case R_X86_64_64:   *(int64_t *)reldata += (int64_t)rel_value; break;
  case R_X86_64_PC8:  *(int8_t  *)reldata += (int8_t )(rel_value-base_address); break;
  case R_X86_64_PC16: *(int16_t *)reldata += (int16_t)(rel_value-base_address); break;
  case R_X86_64_PC32: *(int32_t *)reldata += (int32_t)(rel_value-base_address); break;
#else
#   error FIXME
#endif
  default: break;
  }
 }
}

#if DCC_HOST_CPU == DCC_TARGET_CPU
PUBLIC void
DCCSection_SetBase(struct DCCSection *__restrict self) {
 void *codebase; size_t codesize;
 assert(self),DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 assert(self->sc_text.tb_max >= self->sc_text.tb_pos);
 /* Make sure the entire text is allocated. */
 if (!DCCSection_GetText(self,0,(size_t)(self->sc_text.tb_max-
                                         self->sc_text.tb_begin))
     ) return;
 codesize = (size_t)(self->sc_text.tb_max-self->sc_text.tb_begin);
 /* Fix alignment requirements. */
 if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_U) self->sc_align = 1;
 /* When we don't need execute permissions, and the
  * code is already properly aligned, we can simply
  * re-use the pre-allocated buffer! */
 if (!(self->sc_start.sy_flags&DCC_SYMFLAG_SEC_X) &&
     !((uintptr_t)self->sc_text.tb_begin&(self->sc_align-1))) {
  DCCSection_SetBaseTo(self,(target_ptr_t)self->sc_text.tb_begin);
  return;
 }
 /* TODO: What about alignment?
  *       The functions below ~should~ allocate memory that is
  *       page-aligned (4096 on x86), but can we be sure of that? */
#ifdef _WIN32
 {
  DWORD type = (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_X) ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE;
  codebase = VirtualAlloc(NULL,codesize,MEM_COMMIT,type);
 }
#else
 {
  int type = PROT_READ|PROT_WRITE;
  if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_X) type |= PROT_EXEC;
#ifdef MAP_ANON
  codebase = mmap(NULL,codesize,type,MAP_ANON,-1,0);
#elif defined(MAP_ANONYMOUS)
  codebase = mmap(NULL,codesize,type,MAP_ANONYMOUS,-1,0);
#else
  {
   int fd = open("/dev/null",O_RDONLY);
   codebase = mmap(NULL,codesize,type,0,fd,0);
   close(fd);
  }
#endif
 }
 if unlikely(codebase == (void *)(uintptr_t)-1) codebase = NULL;
#endif
 if unlikely(!codebase) goto seterr; /* Prevent problems... */
 memcpy(codebase,self->sc_text.tb_begin,codesize);
 DCCSection_SetBaseTo(self,(target_ptr_t)codebase);
 self->sc_start.sy_flags |= DCC_SYMFLAG_SEC_OWNSBASE;
 return;
seterr:
 TPPLexer_SetErr();
}
#endif /* DCC_HOST_CPU == DCC_TARGET_CPU */

PUBLIC struct DCCSym *
DCCSection_GetSym(struct DCCSection *__restrict self,
                  struct TPPKeyword const *__restrict name) {
 struct DCCSym *iter;
 assert(self),assert(name);
 if unlikely(!self->sc_syma) return NULL;
 iter = self->sc_symv[name->k_id % self->sc_syma];
 while (iter && iter->sy_name != name) iter = iter->sy_sec_next;
 return iter;
}
PUBLIC struct DCCSym *
DCCSection_GetSyms(struct DCCSection *__restrict self,
                   char const *__restrict name) {
 struct TPPKeyword *kname;
 kname = TPPLexer_LookupKeyword(name,strlen(name),0);
 return kname ? DCCSection_GetSym(self,kname) : NULL;
}


PUBLIC void *
DCCSection_GetText(struct DCCSection *__restrict self,
                   target_ptr_t addr, target_siz_t size) {
 target_ptr_t addr_end = addr+size;
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 assertf(addr_end <= (target_ptr_t)(self->sc_text.tb_max-
                                    self->sc_text.tb_begin),
         "The given address range %lu..%lu is out-of-bounds of 0..%lu",
         (unsigned long)addr,(unsigned long)addr_end,
         (unsigned long)(self->sc_text.tb_max-self->sc_text.tb_begin));
 if (addr_end > (target_ptr_t)(self->sc_text.tb_end-
                               self->sc_text.tb_begin)) {
  uint8_t *newvec,*oldvec; size_t old_size;
  assert(addr_end);
  /* The given address is located past the allocated end of the text buffer.
   * With that in mind, we must increase the buffer to allocate at least up to 'addr_end'! */
  oldvec = self->sc_text.tb_begin;
  newvec = (uint8_t *)realloc(oldvec,(size_t)addr_end);
  if unlikely(!newvec) goto seterr;
  /* zero-initialize the newly allocated portion. */
  old_size = (size_t)(self->sc_text.tb_end-oldvec);
  memset(newvec+old_size,0,(size_t)addr_end-old_size);
  /* Update the text pointers. */
  self->sc_text.tb_pos   = newvec+(self->sc_text.tb_pos-oldvec);
  self->sc_text.tb_max   = newvec+(self->sc_text.tb_max-oldvec);
  self->sc_text.tb_end   = newvec+(size_t)addr_end;
  self->sc_text.tb_begin = newvec;
 }
 /* Actually retrieving the text pointer is fairly simple! */
 return self->sc_text.tb_begin+addr;
seterr: TPPLexer_SetErr();
 return NULL;
}


PUBLIC target_ptr_t
DCCSection_DAlloc(struct DCCSection *__restrict self,
                  target_siz_t size, target_siz_t align,
                  target_siz_t offset) {
 target_ptr_t result;
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 if (self->sc_align < align)
     self->sc_align = align;
 /* Try to re-use previously allocated memory. */
 result = DCCFreeData_Acquire(&self->sc_free,size,align,offset);
 if (result == DCC_FREEDATA_INVPTR)
     result = DCCSection_DAllocBack(self,size,align,offset);
 return result;
}

PUBLIC target_ptr_t
DCCSection_DAllocBack(struct DCCSection *__restrict self,
                      target_siz_t size,
                      target_siz_t align,
                      target_siz_t offset) {
 target_ptr_t result,aligned_result;
 uint8_t *new_pointer;
 /* Allocate at the end. */
 result = (target_ptr_t)(self->sc_text.tb_max-self->sc_text.tb_begin);
 /* Align the result pointer. */
 aligned_result  = (result+(align-1)-offset) & ~(align-1);
 aligned_result += offset;
 if (aligned_result != result) {
  /* Mark alignment & offset memory as free. */
  DCCFreeData_Release(&self->sc_free,result,aligned_result-result);
 }
 result = aligned_result;
 /* Update the text max-pointer. */
 new_pointer = self->sc_text.tb_begin+result+size;
 if (self->sc_text.tb_pos == self->sc_text.tb_max)
     self->sc_text.tb_pos = new_pointer;
 self->sc_text.tb_max = new_pointer;
 return result;
}


PUBLIC int
DCCSection_DAllocAt(struct DCCSection *__restrict self,
                    target_ptr_t addr, target_siz_t size) {
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 /* Check if we can allocate at the end. */
 if (addr >= (target_ptr_t)(self->sc_text.tb_max-
                            self->sc_text.tb_begin)) {
  /* Yes: Allocate past the end. */
  self->sc_text.tb_max = self->sc_text.tb_begin+addr+size;
  return 1;
 }
 return DCCFreeData_AcquireAt(&self->sc_free,addr,size) != DCC_FREEDATA_INVPTR;
}

PUBLIC target_ptr_t
DCCSection_DRealloc(struct DCCSection *__restrict self,
                    target_ptr_t old_addr, target_siz_t old_size,
                    target_siz_t new_size, target_siz_t new_align,
                    target_siz_t new_offset) {
 target_ptr_t result = old_addr;
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 if (!old_size) {
  /* Special case: First allocation. */
  result = DCCSection_DAlloc(self,new_size,new_align,new_offset);
 } else if (new_size < old_size) {
  target_ptr_t aligned_result;
  /* Free previously allocated memory. */
  DCCSection_DFree(self,old_addr+new_size,old_size-new_size);
  aligned_result  = result;
  aligned_result &= ~(new_align-1);
  aligned_result += new_offset;
  if (aligned_result != result) {
   size_t alignment_offset = result-aligned_result;
   assert(result >= aligned_result);
   /* Make sure that 'result' is aligned by 'new_align' */
   if (DCCSection_DAllocAt(self,aligned_result,alignment_offset) == DCC_FREEDATA_INVPTR) {
    uint8_t *oldvec,*newvec;
    /* Couldn't allocate the alignment different.
     * Try again in upper memory, and if that fails, allocate a new vector. */
    if (DCCSection_DAllocAt(self,old_addr+new_size,alignment_offset) != DCC_FREEDATA_INVPTR) {
     aligned_result += new_align;
     DCCSection_DFree(self,old_addr,alignment_offset);
    } else {
     /* Allocate a whole new vector. */
     aligned_result = DCCSection_DAlloc(self,new_size,new_align,new_offset);
     alignment_offset = new_size;
    }
    oldvec = (uint8_t *)DCCSection_GetText(self,old_addr,new_size);
    if unlikely(!oldvec) goto end;
    newvec = (uint8_t *)DCCSection_GetText(self,aligned_result,new_size);
    if unlikely(!newvec) goto end;
    memmove(newvec,oldvec,new_size);
    DCCSection_DFree(self,old_addr,alignment_offset);
   }
   result = aligned_result;
  }
 } else if (new_size > old_size) {
  target_siz_t more_mem = new_size-old_size;
  /* Allocate more memory. */
  result = DCCSection_DAllocAt(self,old_addr+old_size,more_mem);
  if (result == DCC_FREEDATA_INVPTR) {
   uint8_t *oldvec,*newvec;
   int old_new_overlap = 1;
   /* Can't allocate immediately after! (Try immediately before) */
   result  = old_addr-more_mem;
   result &= ~(new_align-1);
   result += new_offset;
   result  = DCCSection_DAllocAt(self,result,more_mem);
   if (result == DCC_FREEDATA_INVPTR) {
    /* Just allocate new data. */
    result = DCCSection_DAlloc(self,new_size,new_align,new_offset);
    old_new_overlap = 0;
   }
   /* Must move downwards. */
   oldvec = (uint8_t *)DCCSection_GetText(self,old_addr,old_size);
   if unlikely(!oldvec) goto end;
   newvec = (uint8_t *)DCCSection_GetText(self,result,old_size);
   if unlikely(!newvec) goto end;
   /* Move the common memory to the new location. */
   memmove(newvec,oldvec,old_size);
   /* Free overlap/the old vector. */
   if (old_new_overlap) {
    target_ptr_t unaligned_result = old_addr-more_mem;
    /* Free the upper area of what may have been removed due to overlap.
     * HINT: 'unaligned_result-result' is ZERO(0) when
     *       old_addr was already aligned by new_align. */
    DCCSection_DFree(self,unaligned_result+old_size,
                     unaligned_result-result);
   } else {
    DCCSection_DFree(self,old_addr,old_size);
   }
  }
 }
end:
 return result;
}

#if DCC_SECTION_HAVE_MERGE
PRIVATE int
memeq_sized(uint8_t const *a, size_t a_size,
            uint8_t const *b, size_t b_size) {
 uint8_t const *iter,*end;
 assert(a_size >= b_size);
 if (!memcmp(a,b,b_size)) return 0;
 end = (iter = a+b_size)+(a_size-b_size);
 /* Check the overflow area for being filled with nothing but ZEROes. */
 for (; iter != end; ++iter) if (*iter != 0) return 0;
 return 1;
}
#endif /* DCC_SECTION_HAVE_MERGE */

PUBLIC target_ptr_t
DCCSection_DMerge(struct DCCSection *__restrict self,
                  target_ptr_t addr, target_siz_t size,
                  target_siz_t min_align) {
 target_ptr_t result = addr;
 assert(self);
 assertf(min_align,"Invalid alignment");
 assertf(!(addr&(min_align-1)),
         "The given addr %lx isn't aligned by %lx",
        (unsigned long)addr,(unsigned long)min_align);
 (void)size;
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
#if DCC_SECTION_HAVE_MERGE
 if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_M) {
  uint8_t *addr_data,*search_iter,*search_end;
  target_siz_t allocated_size;
  target_ptr_t alloc_end;
  search_end = self->sc_text.tb_end;
  if (self->sc_text.tb_max < search_end) search_end = self->sc_text.tb_max;
  alloc_end = (target_ptr_t)(search_end-self->sc_text.tb_begin);
  /* Search for previous iterations. */
  search_iter = self->sc_text.tb_begin;
  if (addr >= alloc_end) {
   addr_data      = NULL;
   allocated_size = 0;
   search_end     = self->sc_text.tb_end;
  } else {
   addr_data      = self->sc_text.tb_begin+addr;
   allocated_size = alloc_end-addr;
   search_end     = search_iter+addr;
  }
  search_end -= size;
  if (search_end > self->sc_text.tb_end) goto check_bss;
  while (search_iter < search_end) {
   if (memeq_sized(search_iter,size,addr_data,allocated_size)) {
    /* We've got a match! */
    result = (target_ptr_t)(search_iter-self->sc_text.tb_begin);
    goto end;
   }
   /* Advance the source pointer by the given alignment.
    * >> That way, we simply skip checking unalignment pointers. */
   search_iter += min_align;
  }
check_bss:
  if (!allocated_size) {
   /* Special case: Move the result address as close
    *               to the allocated end as possible. */
   result = (alloc_end+(min_align-1)) & ~(min_align-1);
  }
 }
end:
#endif /* DCC_SECTION_HAVE_MERGE */
 return result;
}


PUBLIC target_ptr_t
DCCSection_DAllocMem(struct DCCSection *__restrict self,
                     void const *__restrict memory,
                     target_siz_t mem_size, target_siz_t size,
                     target_siz_t align, target_siz_t offset) {
 target_ptr_t result; void *secdat;
 assert(self);
 assert(mem_size <= size);
 assertf(align,"Invalid alignment");
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 if (self->sc_align < align)
     self->sc_align = align;
 /* Optimize away trailing ZERO-bytes, instead using bss-trailing memory. */
 while (mem_size && ((uint8_t *)memory)[mem_size-1] == 0) --mem_size;
#if DCC_SECTION_HAVE_MERGE
 if (!(self->sc_start.sy_flags&DCC_SYMFLAG_SEC_M)) goto alloc_normal;
 uint8_t *iter,*end;
 iter = self->sc_text.tb_begin;
 end  = self->sc_text.tb_end;
 if (self->sc_text.tb_max < end) end = self->sc_text.tb_max;
 end -= (size+offset);
 if (end >= self->sc_text.tb_end) goto alloc_normal;
 while (iter < end) {
  if (memeq_sized(iter+offset,size,(uint8_t const *)memory,mem_size)) {
   /* We've got a match! */
   result = (target_ptr_t)(iter-self->sc_text.tb_begin)+offset;
   goto end;
  }
  iter += align;
 }
alloc_normal:
#endif /* DCC_SECTION_HAVE_MERGE */
 result = DCCSection_DAlloc(self,size,align,offset);
 secdat = DCCSection_GetText(self,result,mem_size);
 if likely(secdat) memcpy(secdat,memory,mem_size);
 /* No need to merge. - If we've here that wouldn't do any good! */
//result = DCCSection_DMerge(self,result,size,align);
#if DCC_SECTION_HAVE_MERGE
end:
#endif
 return result;
}
PUBLIC /*ref*/struct DCCSym *
DCCSection_DAllocSym(struct DCCSection *__restrict self,
                     void const *__restrict memory,
                     target_siz_t mem_size, target_siz_t size,
                     target_siz_t align, target_siz_t offset) {
 target_ptr_t addr; struct DCCSym *result;
 result = DCCSym_New(&TPPKeyword_Empty,DCC_SYMFLAG_STATIC);
 if unlikely(!result) return NULL;
 addr = DCCSection_DAllocMem(self,memory,mem_size,size,align,offset);
 DCCSym_Define(result,self,addr,size);
 DCCSym_XASSERT(result);
 return result;
}


PUBLIC void
DCCSection_DFree(struct DCCSection *__restrict self,
                 target_ptr_t addr, target_siz_t size) {
 uint8_t *addr_ptr;
 target_ptr_t addr_end = addr+size;
 target_siz_t allocated_size;
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 assertf(addr_end <= (target_ptr_t)(self->sc_text.tb_max-
                                    self->sc_text.tb_begin),
         "The given address range %lu..%lu is out-of-bounds of 0..%lu",
         (unsigned long)addr,(unsigned long)addr_end,
         (unsigned long)(self->sc_text.tb_max-self->sc_text.tb_begin));
 addr_ptr = self->sc_text.tb_begin+addr;
 if (addr_ptr < self->sc_text.tb_end) {
  allocated_size = (size_t)(self->sc_text.tb_end-addr_ptr);
  if (size < allocated_size) allocated_size = size;
 } else {
  allocated_size = 0;
 }
 /* ZERO-initialize the free & allocated memory. */
 memset(addr_ptr,0,allocated_size);
 if (addr_end == (target_ptr_t)(self->sc_text.tb_max-
                                self->sc_text.tb_begin)) {
  /* Special case: The given address range is located just at the end of theoretical memory.
   * >> With that in mind, we can simply free the associated memory range by moving the address-max downwards. */
  self->sc_text.tb_max -= size;
 } else {
  /* Fallback: Register a free memory region. */
  DCCFreeData_Release(&self->sc_free,addr,size);
 }
 assert(self->sc_text.tb_max >= self->sc_text.tb_begin);
}


LOCAL void *
DCCTextBuf_TAlloc(struct DCCTextBuf *__restrict self,
                  target_siz_t size) {
 uint8_t *result;
 assert(self);
 result = self->tb_pos;
 self->tb_pos += size;
 if (self->tb_pos > self->tb_max) self->tb_max = self->tb_pos;
 if (self->tb_pos > self->tb_end) {
  size_t new_alloc;
  /* The text pointer now lies beyond the allocated end.
   * >> Must allocate more text. */
  new_alloc = (size_t)(self->tb_pos-self->tb_begin)*2;
  result = (uint8_t *)realloc(self->tb_begin,new_alloc);
  if unlikely(!result) goto seterr;
  /* Update the text pointers. */
  self->tb_max   = result+(self->tb_max-self->tb_begin);
  self->tb_pos   = result+(self->tb_pos-self->tb_begin);
  self->tb_end   = result+new_alloc;
  self->tb_begin = result;
  /* ZERO-initialize newly allocate memory (excluding anything requested by the caller). */
  memset(self->tb_pos,0,
        (size_t)(self->tb_end-
                 self->tb_pos));
  result = self->tb_pos-size;
 }
 return result;
seterr: TPPLexer_SetErr();
 return NULL;
}

#ifndef DCCGEN_NOPBYTE
#define DCCGEN_NOPBYTE 0x90
#endif
LOCAL void
DCCTextBuf_TAlign(struct DCCTextBuf *__restrict self,
                  target_siz_t align, target_siz_t offset) {
 target_siz_t diff; uint8_t *data;
 assert(self);
 diff  = (target_siz_t)DCCTextBuf_ADDR(self);
 diff  = (diff+(align-1)-offset) & ~(align-1);
 diff += offset;
 diff -= (target_siz_t)DCCTextBuf_ADDR(self);
 data  = (uint8_t *)DCCTextBuf_TAlloc(self,diff);
 if (data) memset(data,DCCGEN_NOPBYTE,(size_t)diff);
}
LOCAL void DCCTextBuf_TPutb(struct DCCTextBuf *__restrict self,
                            uint8_t byte) {
 uint8_t *text_ptr;
 assert(self);
 text_ptr = self->tb_pos++;
 if (text_ptr >= self->tb_max) self->tb_max = self->tb_pos;
 if (text_ptr >= self->tb_end) {
  --self->tb_pos;
  text_ptr = (uint8_t *)DCCTextBuf_TAlloc(self,1);
  if unlikely(!text_ptr) return;
 }
 *text_ptr = byte;
}




PUBLIC struct DCCSection DCCSection_Abs = {
 /* sc_start                */{
 /* sc_start.sy_refcnt      */0x80000000,
 /* sc_start.sy_sec_pself   */NULL,
 /* sc_start.sy_sec_next    */NULL,
 /* sc_start.sy_unit_next   */NULL,
 /* sc_start.sy_unit_before */NULL,
 /* sc_start.sy_name        */&TPPKeyword_Empty,
 /* sc_start.sy_flags       */DCC_SYMFLAG_STATIC|DCC_SYMFLAG_SEC(1,1,1,0,0,1), /* rwxu */
#if DCC_TARGET_BIN == DCC_BINARY_PE
 /* sc_start.sy_peind         */NULL,
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 /* sc_start.sy_alias       */NULL,
 /* sc_start.sy_sec         */&DCCSection_Abs,
 /* sc_start.sy_addr        */0,
 /* sc_start.sy_size        */(target_ptr_t)-1},
 /* sc_symc                 */0,
 /* sc_syma                 */0,
 /* sc_symv                 */NULL,
 /* sc_unit                 */NULL,
 /* sc_pself                */NULL,
 /* sc_next                 */NULL,
 /* sc_text                 */{
 /* sc_text.tb_begin        */NULL,
 /* sc_text.tb_end          */NULL,
 /* sc_text.tb_max          */NULL,
 /* sc_text.tb_pos          */NULL},
 /* sc_free                 */{
 /* sc_free.fd_begin        */NULL},
 /* sc_align                */1,
 /* sc_base                 */0, /* Section is based at ZERO(0). */
 /* sc_merge                */0,
 /* sc_relc                 */0,
 /* sc_rela                 */0,
 /* sc_relv                 */NULL,
};

PUBLIC void *
DCCSection_TAlloc(struct DCCSection *__restrict self,
                  target_siz_t size) {
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 return DCCTextBuf_TAlloc(&self->sc_text,size);
}
PUBLIC void
DCCSection_TAlign(struct DCCSection *__restrict self,
                  target_siz_t align, target_siz_t offset) {
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 if (self->sc_align < align)
     self->sc_align = align;
 DCCTextBuf_TAlign(&self->sc_text,align,offset);
}
PUBLIC void
DCCSection_TWrite(struct DCCSection *__restrict self,
                  void const *__restrict p, size_t s) {
 void *buf;
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 buf = DCCTextBuf_TAlloc(&self->sc_text,s);
 if (buf) memcpy(buf,p,s);
}
PUBLIC void DCCSection_TPutb(struct DCCSection *__restrict self,
                             uint8_t byte) {
 assert(self);
 DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 DCCTextBuf_TPutb(&self->sc_text,byte);
}











/* **** */
/* UNIT */
/* **** */
PUBLIC struct DCCUnit DCCUnit_Current = {0};
PUBLIC void
DCCUnit_Flush(struct DCCUnit *__restrict self, uint32_t flags) {
 assert(self);
 (void)flags; /* TODO */
}

PUBLIC size_t DCCUnit_ClearUnused(void) {
 struct DCCSym **piter,*iter;
 struct DCCSym **biter,**bend;
 size_t result = 0;
 piter = &unit.u_nsym;
 while ((iter = *piter) != NULL) {
  if (DCCSym_ISUNUSED(iter)) {
   assert(!(iter->sy_flags&DCC_SYMFLAG_USED));
   /* Delete this symbol. */
   *piter = iter->sy_unit_next; /* Inherit reference to next in '*piter'; Inherit reference in 'iter'. */
   iter->sy_unit_next = NULL;   /* Make sure that 'iter' is in a consistent state. */
#if DCC_DEBUG
   iter->sy_refcnt = 0;
#endif
   _DCCSym_Delete(iter);
   ++result;
  } else {
   piter = &iter->sy_unit_next;
  }
 }
 bend = (biter = unit.u_symv)+unit.u_syma;
 for (; biter != bend; ++biter) {
  piter = biter;
  while ((iter = *piter) != NULL) {
   if (DCCSym_ISUNUSED(iter)) {
    assert(!(iter->sy_flags&DCC_SYMFLAG_USED));
    /* Delete this symbols. */
    *piter = iter->sy_unit_next; /* Inherit reference to next in '*piter'; Inherit reference in 'iter'. */
    iter->sy_unit_next = NULL;   /* Make sure that 'iter' is in a consistent state. */
#if DCC_DEBUG
    iter->sy_refcnt = 0;
#endif
    _DCCSym_Delete(iter);
    assert(unit.u_symc);
    --unit.u_symc;
    ++result;
   } else {
    piter = &iter->sy_unit_next;
   }
  }
 }
 return result;
}

PUBLIC size_t DCCUnit_ClearStatic(void) {
 size_t result = 0;
 struct DCCSym **piter,*iter;
 struct DCCSym **sym_iter,**sym_end;
 sym_end = (sym_iter = unit.u_symv)+unit.u_syma;
 for (; sym_iter != sym_end; ++sym_iter) {
  piter = sym_iter;
  while ((iter = *piter) != NULL) {
   if (iter->sy_flags&DCC_SYMFLAG_STATIC) {
    *piter = iter->sy_unit_next; /* Inherit reference. */
    iter->sy_unit_next = NULL;   /* Inherit reference. */
    DCCSym_Decref(iter);         /* Drop reference. */
    ++result;
   } else {
    piter = &iter->sy_unit_next;
   }
  }
 }
 return result;
}

PUBLIC size_t
DCCUnit_ClearUnusedLibs(void) {
 struct DCCSection *iter,*next;
 struct DCCSym **psec,*sec;
 size_t result = 0;
 iter = unit.u_imps;
 while (iter) {
  assert(unit.u_impc);
  next = iter->sc_next;
  if (iter->sc_start.sy_refcnt == 1 &&
    !(iter->sc_start.sy_flags&DCC_SYMFLAG_USED)) {
   /* Unused library.
    * >> Must find the self-pointer in the global symbol table. */
   assert(unit.u_symc);
   assert(unit.u_syma);
   assert(unit.u_symv);
   assert(iter->sc_start.sy_name);
   psec = &unit.u_symv[iter->sc_start.sy_name->k_id % unit.u_syma];
   while ((sec = *psec,DCCSym_ASSERT(sec),sec != &iter->sc_start))
           psec = &sec->sy_unit_next;
   assert(sec == &iter->sc_start);
   *psec = sec->sy_unit_next; /* Inherit reference to next in '*psec'; local-inherit reference to 'sec' */
   sec->sy_unit_next = NULL;
   assert(sec->sy_refcnt == 1);
#if DCC_DEBUG
   sec->sy_refcnt = 0;
#endif
   _DCCSym_Delete(sec);
   ++result;
  }
  iter = next;
 }
 return result;
}

PUBLIC void
DCCUnit_Init(struct DCCUnit *__restrict self) {
 assert(self);
 memset(self,0,sizeof(struct DCCUnit));
}
PUBLIC void
DCCUnit_Quit(struct DCCUnit *__restrict self) {
 assert(self);
 /* Delete unnamed symbols. */
 { struct DCCSym *iter,*next;
   iter = self->u_nsym;
   while (iter) {
    next = iter->sy_unit_next;
    iter->sy_unit_next = NULL;
    DCCSym_Clear(iter);
    DCCSym_Decref(iter);
    iter = next;
   }
 }
 /* Delete named symbols. */
 { struct DCCSym **biter,**bend,*iter,*next;
   bend = (biter = self->u_symv)+self->u_syma;
   for (; biter != bend; ++biter) {
    iter = *biter;
    while (iter) {
     next = iter->sy_unit_next;
     iter->sy_unit_next = NULL;
     DCCSym_Clear(iter);
     DCCSym_Decref(iter);
     iter = next;
    }
   }
   free(self->u_symv);
 }

 /* Ensure consistent deallocation. */
 assert(!self->u_secc);
 assert(!self->u_impc);
 assert(!self->u_secs);
 assert(!self->u_imps);
}

void DCCUnit_InsSym(/*ref*/struct DCCSym *__restrict sym) {
 struct DCCSym **pbucket;
 assert(sym);
 if (DCCSym_ISSECTION(sym)) {
  /* Track all sections through a per-unit linked list. */
  struct DCCSection *sec = DCCSym_TOSECTION(sym);
  if (DCCSection_ISIMPORT(sec)) {
   if ((sec->sc_next = unit.u_imps) != NULL)
        unit.u_imps->sc_pself = &sec->sc_next;
   sec->sc_pself = &unit.u_imps;
   assert(!sec->sc_unit);
   unit.u_imps = sec;
   ++unit.u_impc;
  } else {
   if ((sec->sc_next = unit.u_secs) != NULL)
        unit.u_secs->sc_pself = &sec->sc_next;
   sec->sc_pself = &unit.u_secs;
   assert(!sec->sc_unit);
   unit.u_secs = sec;
   ++unit.u_secc;
  }
  sec->sc_unit = &unit;
 }
 /* Don't count unnamed symbols towards
  * the regular per-unit symbol table. */
 if (sym->sy_name == &TPPKeyword_Empty) {
  assert(sym != unit.u_nsym);
  sym->sy_unit_next = unit.u_nsym;
  unit.u_nsym       = sym; /* Inherit reference. */
 } else {
  DCCUnit_CheckRehash();
  if unlikely(!unit.u_syma) goto seterr;
  assert(unit.u_symv);
  pbucket = &unit.u_symv[DCCSym_HASH(sym) % unit.u_syma];
  sym->sy_unit_next = *pbucket;
  *pbucket          = sym; /* Inherit reference. */
  ++unit.u_symc;
 }
 return;
seterr: TPPLexer_SetErr();
}
LOCAL void DCCUnit_RehashSymbols(size_t newsize) {
 struct DCCSym **new_map,**biter,**bend,*iter,*next,**dst;
 assert(newsize);
 new_map = (struct DCCSym **)calloc(newsize,sizeof(struct DCCSym *));
 if unlikely(!new_map) return;
 bend = (biter = unit.u_symv)+unit.u_syma;
 for (; biter != bend; ++biter) {
  iter = *biter;
  while (iter) {
   next = iter->sy_unit_next;
   DCCSym_ASSERT(iter);
   dst = &new_map[DCCSym_HASH(iter) % newsize];
   iter->sy_unit_next = *dst;
   *dst = iter;
   iter = next;
  }
 }
 free(unit.u_symv);
 unit.u_symv = new_map;
 unit.u_syma = newsize;
}

LOCAL void
DCCUnit_CheckRehash(void) {
 if ((unit.u_symc*3)/2 >= unit.u_syma) {
  size_t newsize = unit.u_syma;
  if unlikely(!newsize) newsize = 4;
  newsize *= 2;
  /* Must allocate more symbols. */
  DCCUnit_RehashSymbols(newsize);
 }
}


PUBLIC void *DCCUnit_TAlloc(target_siz_t size) {
 assertf(unit.u_curr,"No current section selected");
 return DCCCompiler_ISCGEN() ? DCCTextBuf_TAlloc(&unit.u_tbuf,size) : NULL;
}
PUBLIC void DCCUnit_TPutb(uint8_t byte) {
 assertf(unit.u_curr,"No current section selected");
 if (DCCCompiler_ISCGEN()) DCCTextBuf_TPutb(&unit.u_tbuf,byte);
}
PUBLIC void DCCUnit_TAlign(target_siz_t align, target_siz_t offset) {
 assertf(unit.u_curr,"No current section selected");
 if (DCCCompiler_ISCGEN()) {
  if (unit.u_curr->sc_align < align)
      unit.u_curr->sc_align = align;
  DCCTextBuf_TAlign(&unit.u_tbuf,align,offset);
 }
}
PUBLIC void DCCUnit_TWrite(void const *__restrict p, size_t s) {
 assertf(unit.u_curr,"No current section selected");
 if (DCCCompiler_ISCGEN()) {
  void *buf = DCCTextBuf_TAlloc(&unit.u_tbuf,s);
  if (buf) memcpy(buf,p,s);
 }
}


PUBLIC struct DCCSym *
DCCUnit_GetSym(struct TPPKeyword const *__restrict name) {
 struct DCCSym *result;
 if unlikely(!unit.u_syma) return NULL;
 result = unit.u_symv[name->k_id % unit.u_syma];
 while (result && result->sy_name != name) result = result->sy_unit_next;
 DCCSym_XASSERT(result);
 return result;
}
PUBLIC struct DCCSym *
DCCUnit_NewSym(struct TPPKeyword const *__restrict name,
               symflag_t flags) {
 struct DCCSym *result;
 if unlikely(!unit.u_syma) goto def_newsym;
 result = unit.u_symv[name->k_id % unit.u_syma];
 while (result && result->sy_name != name) result = result->sy_unit_next;
 if (!result) {
def_newsym:
  result = DCCSym_New(name,flags);
  if likely(result) DCCUnit_InsSym(result); /* Inherit reference. */
 }
 DCCSym_XASSERT(result);
 return result;
}
PUBLIC struct DCCSym *
DCCUnit_NewForwardSym(struct TPPKeyword const *__restrict name,
                      symflag_t flags) {
 struct DCCSym *result,**presult;
 if unlikely(!unit.u_syma) goto def_newsym;
 presult = &unit.u_symv[name->k_id % unit.u_syma];
 while ((result = *presult) != NULL &&
         result->sy_name != name) {
  presult = &result->sy_unit_next;
 }
 if (!result) {
def_newsym:
  result = DCCSym_New(name,flags);
  if likely(result) DCCUnit_InsSym(result); /* Inherit reference. */
 } else if (!DCCSym_ISFORWARD(result)) {
  /* Replace a non-forward-declared symbol with a new version. */
  struct DCCSym *new_result = DCCSym_New(name,flags);
  if likely(new_result) {
   new_result->sy_unit_before = result; /* Inherit reference. */
   new_result->sy_unit_next = result->sy_unit_next; /* Inherit reference. */
   result->sy_unit_next = NULL;
   *presult = result; /* Inherit reference. */
  }
 }
 DCCSym_XASSERT(result);
 return result;
}
PUBLIC struct DCCSection *
DCCUnit_GetSec(struct TPPKeyword const *__restrict name) {
 struct DCCSym *result;
 if unlikely(!unit.u_syma) return NULL;
 result = unit.u_symv[name->k_id % unit.u_syma];
 while (result &&
       (result->sy_name != name || !DCCSym_ISSECTION(result))
        ) result = result->sy_unit_next;
 assert(!result || DCCSym_ISSECTION(result));
 DCCSym_XASSERT(result);
 return (struct DCCSection *)result;
}
PUBLIC struct DCCSection *
DCCUnit_NewSec(struct TPPKeyword const *__restrict name, symflag_t flags) {
 struct DCCSym *result;
 if unlikely(!unit.u_syma) goto def_newsym;
 result = unit.u_symv[name->k_id % unit.u_syma];
 while (result &&
       (result->sy_name != name || !DCCSym_ISSECTION(result))
        ) result = result->sy_unit_next;
 if (!result) {
def_newsym:
  result = (struct DCCSym *)DCCSection_New(name,flags);
  if likely(result) DCCUnit_InsSym(result); /* Inherit reference. */
 }
 assert(!result || DCCSym_ISSECTION(result));
 DCCSym_XASSERT(result);
 return (struct DCCSection *)result;
}

PUBLIC struct DCCSym *
DCCUnit_GetSyms(char const *__restrict name) {
 struct TPPKeyword *kname = TPPLexer_LookupKeyword(name,strlen(name),0);
 return kname ? DCCUnit_GetSym(kname) : NULL;
}
PUBLIC struct DCCSection *
DCCUnit_GetSecs(char const *__restrict name) {
 struct TPPKeyword *kname = TPPLexer_LookupKeyword(name,strlen(name),0);
 return kname ? DCCUnit_GetSec(kname) : NULL;
}
PUBLIC struct DCCSym *
DCCUnit_NewSyms(char const *__restrict name, symflag_t flags) {
 struct TPPKeyword *kname = TPPLexer_LookupKeyword(name,strlen(name),1);
 return kname ? DCCUnit_NewSym(kname,flags) : NULL;
}
PUBLIC struct DCCSection *
DCCUnit_NewSecs(char const *__restrict name, symflag_t flags) {
 struct TPPKeyword *kname = TPPLexer_LookupKeyword(name,strlen(name),1);
 return kname ? DCCUnit_NewSec(kname,flags) : NULL;
}

PUBLIC struct DCCSym *
DCCUnit_NewSymf(symflag_t flags, char const *__restrict fmt, ...) {
 char buf[128];
 va_list args;
 va_start(args,fmt);
 vsnprintf(buf,sizeof(buf),fmt,args);
 va_end(args);
 /* Always make sure that the symbol name is zero-terminated! */
 buf[(sizeof(buf)/sizeof(*buf))-1] = '\0';
 return DCCUnit_NewSyms(buf,flags);
}

PUBLIC struct DCCSection *
DCCUnit_SetCurr(struct DCCSection *sec) {
 struct DCCSection *result;
 if ((result = unit.u_curr) != NULL) {
  memcpy(&result->sc_text,&unit.u_tbuf,
         sizeof(struct DCCTextBuf));
 }
 if ((unit.u_curr = sec) != NULL) {
  memcpy(&sec->sc_text,&unit.u_tbuf,
         sizeof(struct DCCTextBuf));
 }
 unit.u_prev = result;
 return result;
}

PUBLIC struct DCCSym *
DCCUnit_AllocSym(void) {
 struct DCCSym *result;
 result = DCCSym_New(&TPPKeyword_Empty,DCC_SYMFLAG_STATIC);
 if (result) DCCUnit_InsSym(result); /* Inherit reference. */
 DCCSym_XASSERT(result);
 return result;
}



#if DCC_DEBUG
PUBLIC void DCCUnit_ClearCache(void) {
 DCCSection_Clear(&DCCSection_Abs);
}
#else
/* Cache allocators. */
PRIVATE void *DCCSym_Cache = NULL;

LOCAL struct DCCSym *DCCSym_Alloc(void) {
 struct DCCSym *result;
 result = (struct DCCSym *)DCCSym_Cache;
 if (result) {
  DCCSym_Cache = *(void **)result;
  memset(result,0,sizeof(struct DCCSym));
 } else {
  result = (struct DCCSym *)calloc(1,sizeof(struct DCCSym));
 }
 return result;
}
LOCAL void DCCSym_Free(struct DCCSym *__restrict self) {
 *(void **)self = DCCSym_Cache;
 DCCSym_Cache = (void *)self;
}

PUBLIC void DCCUnit_ClearCache(void) {
 void *next,*iter = DCCSym_Cache;
 DCCSym_Cache = NULL;
 while (iter) {
  next = *(void **)iter;
  free(iter);
  iter = next;
 }
 DCCSection_Clear(&DCCSection_Abs);
}
#endif



DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_C */