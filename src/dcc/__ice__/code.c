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
#ifndef GUARD_DCC_CODE_C
#define GUARD_DCC_CODE_C 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/code.h>
#include <dcc/symbol.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdio.h>
#include <dcc/target.h>
#ifdef _WIN32
#include <Windows.h>
#else
#include <sys/mman.h>
#endif
#undef s_addr

DCC_DECL_BEGIN


#if DCC_DEBUG
LOCAL int DCCTarget_IsSection(struct DCCTarget *__restrict self,
                              struct DCCSection *__restrict sec) {
 struct DCCSection **iter,**end;
 end = (iter = self->t_secv)+self->t_secc;
 for (; iter != end; ++iter) if (*iter == sec) return 1;
 return 0;
}
#endif


LOCAL int
DCCTextbuf_ResizeCode(struct DCCTextBuf *__restrict self,
                      size_t newsize) {
 uint8_t *newdata; int result = 1;
 assert(newsize >= (size_t)(self->tb_end-self->tb_begin));
 newdata = (uint8_t *)realloc(self->tb_begin,newsize);
 if unlikely(!newdata) goto err;
 self->tb_pos = newdata+(self->tb_pos-self->tb_begin);
 self->tb_end = (self->tb_begin = newdata)+newsize;
end: return result;
err: result = 0; goto end;
}

PRIVATE void *
DCCTextbuf_Alloc(struct DCCTextBuf *__restrict self, size_t s) {
 void *result; size_t avail,minsize;
 /* Make sure not to do anything when code generation is disabled. */
 avail = (size_t)(self->tb_end-self->tb_pos);
 if (avail < s) {
  minsize = (size_t)(self->tb_pos-self->tb_begin)+s;
  avail = (size_t)(self->tb_end-self->tb_begin);
  if unlikely(!avail) avail = 1;
  do avail *= 2; while (minsize > avail);
  if unlikely(!DCCTextbuf_ResizeCode(self,avail)) goto seterr;
 }
 result = self->tb_pos;
 self->tb_pos += s;
 assert(self->tb_pos <= self->tb_end);
end: return result;
seterr:
 TPPLexer_SetErr();
 result = NULL;
 goto end;
}

LOCAL int
DCCTextbuf_IncCodeSize(struct DCCTextBuf *__restrict self) {
 size_t newsize; uint8_t *newtext;
 assert(self);
 newsize = (size_t)(self->tb_end-self->tb_begin);
 newsize = newsize ? newsize*2 : 2;
 newtext = (uint8_t *)realloc(self->tb_begin,newsize);
 if unlikely(!newtext) goto seterr;
 self->tb_pos = newtext+(self->tb_pos-self->tb_begin);
 self->tb_end = (self->tb_begin = newtext)+newsize;
 return 1;
seterr:
 TPPLexer_SetErr();
 return 0;
}





LOCAL void
DCCSection_Quit(struct DCCSection *__restrict self) {
 assert(self);
 assert(self->s_align != 0);
 assert(self->s_text.tb_begin <= self->s_text.tb_pos);
 assert(self->s_text.tb_pos <= self->s_text.tb_end);
 assert(self->s_rela >= self->s_relc);
 assert(self->s_base || ((self->s_text.tb_begin != NULL) == (self->s_text.tb_pos != NULL)));
 assert(self->s_base || ((self->s_text.tb_begin != NULL) == (self->s_text.tb_end != NULL)));
 assert((self->s_relv != NULL) == (self->s_rela != 0));
 assert(strlen(self->s_name) == self->s_nmsz);
 assert(TPP_Hashof(self->s_name,self->s_nmsz) == self->s_nmhs);
 if (self->s_base && self->s_base != self->s_text.tb_begin) {
#ifdef _WIN32
  VirtualFree(self->s_base,(size_t)(self->s_text.tb_end-self->s_text.tb_begin),MEM_DECOMMIT);
#else
  munmap(self->s_base,(size_t)(self->s_text.tb_end-self->s_text.tb_begin));
#endif
 }
 {
  struct DCCRel *iter,*end; struct DCCSym *sym;
  end = (iter = self->s_relv)+self->s_relc;
  for (; iter != end; ++iter) {
   if ((sym = iter->r_symbol) != NULL) {
    assert(sym->s_kind == DCC_SYMTYPE_LABEL);
    assert(sym->s_label.sl_relc != 0);
    sym->s_label.sl_relc = (size_t)-1; /* Prevent infinite recursion. */
    DCCSym_Decref(sym);
   }
  }
 }
 free(self->s_text.tb_begin);
 free(self->s_relv);
 free(self);
}

PUBLIC void
DCCSection_Reloc(struct DCCSection *__restrict self,
                 int free_data) {
 struct DCCRel *iter,*end; struct DCCSym *sym;
 struct DCCSection *sym_sec;
 uintptr_t addr,newaddr,base_address;
 /* Make sure the section has a base set. */
 base_address = (uintptr_t)self->s_base;
 if unlikely(!base_address) return;
 assert(self);
 end = (iter = self->s_relv)+self->s_relc;
 for (; iter != end; ++iter) {
  newaddr = addr = base_address+iter->r_offset;
  if ((sym = iter->r_symbol) != NULL) {
   assert(sym->s_label.sl_relc != 0);
   while ((assert(sym->s_kind == DCC_SYMTYPE_LABEL),
           sym->s_label.sl_alias)) sym = sym->s_label.sl_alias;
   sym_sec = sym->s_label.sl_sec;
   if unlikely(!sym_sec || !DCCSection_HASBASE(sym_sec)) {
    /* Warning: unresolved symbol. */
    WARN(W_UNRESOLVED_REFERENCE,
         sym->s_file,sym->s_line,
         sym->s_name,self->s_name,
         iter->r_offset);
   } else {
    /* Figure out the absolute address of this symbol. */
    newaddr = (uintptr_t)sym_sec->s_base+sym->s_label.sl_addr;
   }
  }
  switch (iter->r_type) {
#define P(T)  (*(T *)addr)
  case DCC_REL_OPSET|DCC_REL_SIZE8:   P(uint8_t ) = (uint8_t )newaddr; break;
  case DCC_REL_OPSET|DCC_REL_SIZE16:  P(uint16_t) = (uint16_t)newaddr; break;
  case DCC_REL_OPSET|DCC_REL_SIZE32:  P(uint32_t) = (uint32_t)newaddr; break;
  case DCC_REL_OPSET|DCC_REL_SIZE64:  P(uint64_t) = (uint64_t)newaddr; break;
  case DCC_REL_OPADD|DCC_REL_SIZE8:   P(int8_t ) += (int8_t )newaddr; break;
  case DCC_REL_OPADD|DCC_REL_SIZE16:  P(int16_t) += (int16_t)newaddr; break;
  case DCC_REL_OPADD|DCC_REL_SIZE32:  P(int32_t) += (int32_t)newaddr; break;
  case DCC_REL_OPADD|DCC_REL_SIZE64:  P(int64_t) += (int64_t)newaddr; break;
  case DCC_REL_OPDISP|DCC_REL_SIZE8:  P(int8_t ) += (int8_t )(newaddr-base_address); break;
  case DCC_REL_OPDISP|DCC_REL_SIZE16: P(int16_t) += (int16_t)(newaddr-base_address); break;
  case DCC_REL_OPDISP|DCC_REL_SIZE32: P(int32_t) += (int32_t)(newaddr-base_address); break;
  case DCC_REL_OPDISP|DCC_REL_SIZE64: P(int64_t) += (int64_t)(newaddr-base_address); break;
  default: break;
#undef P
  }
  if (free_data && sym) DCCSym_Decref(sym);
 }
 if (free_data) {
  free(self->s_relv);
  self->s_rela = 0;
  self->s_relc = 0;
  self->s_relv = NULL;
 }
}

PUBLIC size_t
DCCSection_ResolveDisp(struct DCCSection *__restrict self,
                       int warn_unknown_targets) {
 struct DCCRel *iter,*end; struct DCCSym *sym;
 uint8_t *code_base,*dest_addr;
 size_t result = 0; int_t target_val;
 assert(self);
 code_base = (self == dcc_current->t_curr)
  ? dcc_current->t_currbuf.tb_begin
  : self->s_text.tb_begin;
 end = (iter = self->s_relv)+self->s_relc;
next_iter:
 while (iter != end) {
  assert(iter < end);
  if ((iter->r_type&0xf) == DCC_REL_OPDISP) {
   target_val = 0,dest_addr = code_base+iter->r_offset;
   if ((sym = iter->r_symbol) != NULL) {
    assert(sym->s_kind == DCC_SYMTYPE_LABEL);
    assert(sym->s_label.sl_relc != 0);
    if (!sym->s_label.sl_sec) {
     /* DISP to undefined section. */
     if (warn_unknown_targets) {
      /* Warning: unresolved symbol. */
      WARN(W_UNRESOLVED_REFERENCE,
           sym->s_file,sym->s_line,
           sym->s_name,self->s_name,
           iter->r_offset);
     }
     goto next;
    }
    /* DISP to different section (can't resolve now) */
    if (sym->s_label.sl_sec != self) goto next;
    target_val = sym->s_label.sl_addr;
   }
   /* Perform the relocation. */
   switch (iter->r_type&0xf0) {
   case DCC_REL_SIZE8:  *(int8_t  *)dest_addr += (int8_t )target_val; break;
   case DCC_REL_SIZE16: *(int16_t *)dest_addr += (int16_t)target_val; break;
   case DCC_REL_SIZE32: *(int32_t *)dest_addr += (int32_t)target_val; break;
   case DCC_REL_SIZE64: *(int64_t *)dest_addr += (int64_t)target_val; break;
   default: goto next;
   }
   /* Delete the relocation. */
   if (sym) { --sym->s_label.sl_relc; DCCSym_Decref(sym); }
   --end,--self->s_relc,++result;
   memmove(iter,iter+1,(size_t)(end-iter)*sizeof(struct DCCRel));
   /* Continue checking for DISP relocation as iter. */
   goto next_iter;
  }
next:
  ++iter;
 }
 return result;
}


PUBLIC int
DCCSection_SetBase(struct DCCSection *__restrict self, int free_data) {
 void *codebase;
 size_t codesize;
 assert(self);
 if unlikely(self->s_base) return 1; /* A base was already allocated */
 codesize = (size_t)(self->s_text.tb_pos-self->s_text.tb_begin);
 /* When we don't need execute permissions, and the
  * code is already properly aligned, we can simply
  * re-use the pre-allocated buffer! */
 if (!(self->s_type&DCC_SECTION_X) &&
     !((uintptr_t)self->s_text.tb_begin&(self->s_align-1))) {
  self->s_base = self->s_text.tb_begin;
  return 1;
 }
 /* TODO: What about alignment?
  *       The functions below ~should~ allocate memory that is
  *       page-aligned (4096 on x86), but can we be sure of that? */
#ifdef _WIN32
 {
  DWORD type = (self->s_type&DCC_SECTION_X) ? PAGE_EXECUTE_READWRITE : PAGE_READWRITE;
  codebase = VirtualAlloc(NULL,codesize,MEM_COMMIT,type);
 }
#else
 {
  int type = PROT_READ|PROT_WRITE;
  if (self->s_type&DCC_SECTION_X) type |= PROT_EXEC;
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
 if unlikely(codebase == (void *)(uintptr_t)-1) return 0;
#endif
 if unlikely(!codebase) return 0; /* Prevent problems... */
 memcpy(codebase,self->s_text.tb_begin,codesize);
 self->s_base = codebase;
 if (free_data) {
  self->s_text.tb_pos -= (uintptr_t)self->s_text.tb_begin;
  self->s_text.tb_end = self->s_text.tb_pos;
  free(self->s_text.tb_begin);
  self->s_text.tb_begin = NULL;
 }
 assert((size_t)(self->s_text.tb_end-self->s_text.tb_begin) == codesize);
 return 1;
}


PUBLIC void
DCCSection_PutReloc(struct DCCSection *__restrict self,
                    rel_t type, struct DCCSym *sym) {
 struct DCCRel *newvec; size_t newsize;
 assert(self);
 assert(!sym || sym->s_kind == DCC_SYMTYPE_LABEL);
 /* Make sure not to do anything when code generation is disabled. */
 if (dcc_current->t_flags&DCC_TARGET_FLAG_NOCGEN) return;
 newvec = self->s_relv;
 if (self->s_relc == self->s_rela) {
  newsize = self->s_rela ? self->s_rela*2 : 2;
  newvec = (struct DCCRel *)realloc(newvec,newsize*sizeof(struct DCCRel));
  if unlikely(!newvec) goto seterr;
  self->s_relv = newvec;
  self->s_rela = newsize;
 }
 newvec += self->s_relc++;
 if (self == dcc_current->t_curr) {
  newvec->r_offset = (target_ptr_t)(dcc_current->t_currbuf.tb_pos-
                                    dcc_current->t_currbuf.tb_begin);
 } else {
  newvec->r_offset = (target_ptr_t)(self->s_text.tb_pos-
                                    self->s_text.tb_begin);
 }
 newvec->r_type   = type;
 newvec->r_symbol = sym;
 if (sym) { DCCSym_Incref(sym); ++sym->s_label.sl_relc; }
 return;
seterr:
 TPPLexer_SetErr();
}

PUBLIC void
DCCSection_PutRelocAt(struct DCCSection *__restrict self,
                      rel_t type, struct DCCSym *sym,
                      target_ptr_t offset) {
 struct DCCRel *newvec; size_t newsize;
 assert(self);
 assert(!sym || sym->s_kind == DCC_SYMTYPE_LABEL);
 newvec = self->s_relv;
 if (self->s_relc == self->s_rela) {
  newsize = self->s_rela ? self->s_rela*2 : 2;
  newvec = (struct DCCRel *)realloc(newvec,newsize*sizeof(struct DCCRel));
  if unlikely(!newvec) goto seterr;
  self->s_relv = newvec;
  self->s_rela = newsize;
 }
 newvec += self->s_relc++;
 newvec->r_offset = offset;
 newvec->r_type   = type;
 newvec->r_symbol = sym;
 if (sym) { DCCSym_Incref(sym); ++sym->s_label.sl_relc; }
 return;
seterr:
 TPPLexer_SetErr();
}


PUBLIC void
DCCSection_Putb(struct DCCSection *__restrict self, uint8_t b) {
 assert(self);
 /* Make sure not to do anything when code generation is disabled. */
 if (dcc_current->t_flags&DCC_TARGET_FLAG_NOCGEN) return;
 if unlikely(self->s_text.tb_pos ==
             self->s_text.tb_end) {
  /* Must relocate the section buffer. */
  if unlikely(!DCCTextbuf_IncCodeSize(&self->s_text)) return;
 }
 *self->s_text.tb_pos++ = b;
}
PUBLIC void
DCCSection_Write(struct DCCSection *__restrict self,
                 void const *__restrict p, size_t s) {
 void *buf; assert(self);
 if ((dcc_current->t_flags&(DCC_TARGET_FLAG_NOCGEN|DCC_TARGET_FLAG_SINIT)) ==
                           (DCC_TARGET_FLAG_NOCGEN)) return;
 buf = DCCTextbuf_Alloc(&self->s_text,s);
 if (buf) {
  if (p) memcpy(buf,p,s);
  else   memset(buf,0,s);
 }
}
PUBLIC void *
DCCSection_Alloc(struct DCCSection *__restrict self, size_t s) {
 assert(self);
 if ((dcc_current->t_flags&(DCC_TARGET_FLAG_NOCGEN|DCC_TARGET_FLAG_SINIT)) ==
                           (DCC_TARGET_FLAG_NOCGEN)) return NULL;
 return DCCTextbuf_Alloc(&self->s_text,s);
}
PUBLIC void *
DCCSection_AAlloc(struct DCCSection *__restrict self, size_t s, size_t a) {
 target_ptr_t addr,aligned_addr;
 assert(self);
 if ((dcc_current->t_flags&(DCC_TARGET_FLAG_NOCGEN|DCC_TARGET_FLAG_SINIT)) ==
                           (DCC_TARGET_FLAG_NOCGEN)) return NULL;
 addr = DCCSection_ADDR(self);
 aligned_addr = (addr+(a-1)) & ~(a-1);
 DCCSection_Skip(self,aligned_addr-addr);
 return DCCTextbuf_Alloc(&self->s_text,s);
}

LOCAL int mem_is_byte(int byte, uint8_t const *p, size_t s) {
 while (s--) if (*p++ != byte) return 0;
 return 1;
}

LOCAL target_ptr_t
DCCSection_FindData(struct DCCSection *__restrict self,
                    void const *__restrict p,
                    size_t s, size_t align) {
 uint8_t *iter,*end;
 assert(self);
 iter = self->s_text.tb_begin;
 end  = self->s_text.tb_pos;
 end -= s;
 /* Make sure we didn't underflow! */
 if (end <= self->s_text.tb_pos) {
  for (; iter <= end; iter += align) {
   if (!memcmp(iter,p,s)) {
    return (target_ptr_t)(iter-self->s_text.tb_begin);
   }
  }
 }
 return (target_ptr_t)-1;
}

LOCAL target_ptr_t
DCCSection_FindBytes(struct DCCSection *__restrict self,
                     int byte, size_t s, size_t align) {
 uint8_t *iter,*end;
 assert(self);
 iter = self->s_text.tb_begin;
 end  = self->s_text.tb_pos;
 end -= s;
 /* Make sure we didn't underflow! */
 if (end <= self->s_text.tb_pos) {
  for (; iter <= end; iter += align) {
   if (mem_is_byte(byte,iter,s)) {
    return (target_ptr_t)(iter-self->s_text.tb_begin);
   }
  }
 }
 return (target_ptr_t)-1;
}


PUBLIC target_ptr_t
DCCSection_Putdata(struct DCCSection *__restrict self,
                   void const *__restrict p, size_t s, size_t align) {
 target_ptr_t addr,aligned_addr;
 assert(self);
 /* Track the max alignment of the section. */
 if (align > self->s_align) self->s_align = align;
 if (!(self->s_type&DCC_SECTION_W)) {
  /* Search for previous instance. */
  aligned_addr = DCCSection_FindData(self,p,s,align);
  if (aligned_addr != (target_ptr_t)-1) return aligned_addr;
 }
 addr = DCCSection_ADDR(self);
 aligned_addr = (addr+(align-1)) & ~(align-1);
 DCCSection_Write(self,NULL,aligned_addr-addr);
 DCCSection_Write(self,p,s);
 return aligned_addr;
}
PUBLIC target_ptr_t
DCCSection_Putbyte(struct DCCSection *__restrict self,
                   int byte, size_t s, size_t align) {
 target_ptr_t addr,aligned_addr;
 void *buf;
 assert(self);
 /* Track the max alignment of the section. */
 if (align > self->s_align) self->s_align = align;
 if (!(self->s_type&DCC_SECTION_W)) {
  /* Search for previous instance. */
  aligned_addr = DCCSection_FindBytes(self,byte,s,align);
  if (aligned_addr != (target_ptr_t)-1) return aligned_addr;
 }
 addr = DCCSection_ADDR(self);
 aligned_addr = (addr+(align-1)) & ~(align-1);
 DCCSection_Write(self,NULL,aligned_addr-addr);
 buf = DCCSection_Alloc(self,s);
 if (buf) memset(buf,byte,s);
 return aligned_addr;
}

PUBLIC struct DCCSym *
DCCSection_PutdataSym(struct DCCSection *__restrict self,
                      void const *__restrict p, size_t s, size_t align) {
 char buffer[256]; struct DCCSym *result;
 struct TPPKeyword *name_kwd;
 target_ptr_t symaddr = DCCSection_Putdata(self,p,s,align);
 snprintf(buffer,sizeof(buffer),":%s:%lx",self->s_name,symaddr);
 name_kwd = TPPLexer_LookupKeyword(buffer,strlen(buffer),1);
 if unlikely(!name_kwd) return NULL;
 /* TODO: Using a symtab here is a really bad idea!
  *    >> Not just because of the unnecessary overhead, but since
  *       a symtab keeps references to its symbols, this prevents
  *       unused constants from being deleted early, meaning that
  *       they can't be used to reclaim unused memory in the section.
  */
 result = DCCSymtab_New(&dcc_current->t_intern,name_kwd);
 if unlikely(!result) return NULL;
 assert(result->s_name);
 assertf(result->s_kind == DCC_SYMTYPE_NONE ||
         result->s_kind == DCC_SYMTYPE_LABEL,
         "Invalid declaration for '%s' in 't_intern' symbol table",
         result->s_name->k_name);
 if (result->s_kind == DCC_SYMTYPE_NONE) {
  result->s_kind          = DCC_SYMTYPE_LABEL;
  result->s_label.sl_addr = symaddr;
  result->s_label.sl_sec  = self;
 }
 return result;
}

PUBLIC struct DCCSym *
DCCSection_PutbyteSym(struct DCCSection *__restrict self,
                      int byte, size_t s, size_t align) {
 char buffer[256]; struct DCCSym *result;
 struct TPPKeyword *name_kwd;
 target_ptr_t symaddr = DCCSection_Putbyte(self,byte,s,align);
 snprintf(buffer,sizeof(buffer),".%s:%lx",self->s_name,symaddr);
 name_kwd = TPPLexer_LookupKeyword(buffer,strlen(buffer),1);
 if unlikely(!name_kwd) return NULL;
 result = DCCSymtab_New(&dcc_current->t_intern,name_kwd);
 if unlikely(!result) return NULL;
 assert(result->s_name);
 assertf(result->s_kind == DCC_SYMTYPE_NONE ||
         result->s_kind == DCC_SYMTYPE_LABEL,
         "Invalid declaration for '%s' in 't_intern' symbol table",
         result->s_name->k_name);
 if (result->s_kind == DCC_SYMTYPE_NONE) {
  result->s_kind          = DCC_SYMTYPE_LABEL;
  result->s_label.sl_addr = symaddr;
  result->s_label.sl_sec  = self;
 }
 return result;
}

PUBLIC target_ptr_t
DCCSection_ResizeData(struct DCCSection *__restrict self,
                      target_ptr_t addr, target_ptr_t old_size,
                      target_ptr_t new_size) {
 target_ptr_t old_end,new_addr;
 assert(self);
 if (new_size == old_size) goto done;
 if (self == dcc_curr) DCCTarget_FlushTextSection();
 if (new_size < old_size) {
  /* Free section data. */
  DCCSection_FreeData(self,addr+new_size,
                      old_size-new_size,1);
 } else if unlikely(!old_size) {
  uint8_t *newmem;
  addr = DCCSection_ADDR(self);
  newmem = (uint8_t *)DCCSection_Alloc(self,new_size);
  if (newmem) memset(newmem,0,new_size);
 } else {
  old_end = addr+old_size;
  new_addr = DCCSection_ADDR(self);
  assertf(old_end <= new_addr,
          "The old address range %lu..%lu is out-of-bounds of 0..%lu",
         (unsigned long)addr,(unsigned long)old_end,
         (unsigned long)new_addr);
  if (old_end == new_addr) {
   uint8_t *newmem; size_t newsize;
   /* Simple case: The address is located at the end, meaning
    *              we can simply allocate memory there! */
   newsize = (size_t)(new_size-old_size);
   newmem = (uint8_t *)DCCSection_Alloc(self,newsize);
   if (newmem) memset(newmem,0,newsize);
  } else {
   uint8_t *oldmem,*newmem;
   struct DCCRel *iter,*end;
   target_ptr_t rel_diff;
   /* Difficult case: Must allocate a new block of data. */
   newmem = (uint8_t *)DCCSection_Alloc(self,new_size);
   if unlikely(!newmem) goto end;
   oldmem = self->s_text.tb_begin+addr;
   memcpy(newmem,oldmem,old_size);
   memset(newmem+old_size,0,new_size-old_size);
   rel_diff = new_addr-addr;
   end = (iter = self->s_relv)+self->s_relc;
   for (; iter != end; ++iter) {
    if (iter->r_offset >= addr && iter->r_offset < old_end) {
     /* Relocation is part of the old address range.
      * >> Adjust to point to the new area. */
     iter->r_offset += rel_diff;
    }
   }
   /* Mark the old area (addr+=old_size) as unused */
   DCCSection_FreeData(self,addr,old_size,0);
   addr = new_addr;
  }
 }
end:  if (self == dcc_curr) DCCTarget_LoadTextSection();
done: return addr;
}

PUBLIC void
DCCSection_FreeData(struct DCCSection *__restrict self,
                    target_ptr_t addr, target_ptr_t size,
                    int free_relocations) {
 target_ptr_t addr_end;
 assert(self);
 if unlikely(!size) return;
 if (self == dcc_curr) DCCTarget_FlushTextSection();
 addr_end = addr+size;
 assertf(addr_end <= DCCSection_ADDR(self),
         "The given address range %lu..%lu is out-of-bounds of 0..%lu",
        (unsigned long)addr,(unsigned long)addr_end,
        (unsigned long)DCCSection_ADDR(self));
 if (addr_end == DCCSection_ADDR(self)) {
  /* Simple case: Free data at the end by moving the section pointer. */
  self->s_text.tb_pos -= size;
 } else {
  /* TODO: Mark area as unused, thus allowing
   *       future allocating calls to reclaim it. */
 }
  /* Fill the address range with cc-bytes (for safety) */
 memset(self->s_text.tb_begin+addr,0xcc,size);
 if (free_relocations) {
  struct DCCRel *iter,*end;
  end = (iter = self->s_relv)+self->s_relc;
  for (; iter != end; ++iter) {
   if (iter->r_offset >= addr && iter->r_offset < addr_end) {
    /* Free this relocation. */
    if (iter->r_symbol) {
     assert(iter->r_symbol->s_kind == DCC_SYMTYPE_LABEL);
     assert(iter->r_symbol->s_label.sl_relc != 0);
     --iter->r_symbol->s_label.sl_relc;
     DCCSym_Decref(iter->r_symbol);
    }
    --end;
    memmove(iter,iter+1,(size_t)((end-iter)*
            sizeof(struct DCCRel)));
   }
  }
  /* Update the relocation count. */
  self->s_relc = (size_t)(end-self->s_relv);
 }
 if (self == dcc_curr) DCCTarget_LoadTextSection();
}





PUBLIC target_ptr_t
DCCStackAllocator_Alloc(target_ptr_t s,
                        target_ptr_t a) {
 target_ptr_t result;
 assert(dcc_current);
 result  = dcc_current->t_stackalloc.sa_curoffset;
 result += (s+(a-1));
 result &= ~(a-1);
 dcc_current->t_stackalloc.sa_curoffset = result;
 if (result > dcc_current->t_stackalloc.sa_maxoffset)
  dcc_current->t_stackalloc.sa_maxoffset = result;
 return result;
}




PUBLIC struct DCCSection DCCSection_Global = {
 /* s_text                   */{NULL,NULL,NULL},
 /* s_base                   */NULL,
 /* s_type                   */(DCC_SECTION_R|DCC_SECTION_W|DCC_SECTION_X),
 /* s_align                  */1,
 /* s_rela                   */0,
 /* s_relc                   */0,
 /* s_relv                   */NULL,
 /* s_start                  */{
 /* s_start.s_refcnt         */0x80000000,
 /* s_start.s_next           */NULL,
 /* s_start.s_before         */NULL,
 /* s_start.s_name           */&TPPKeyword_Empty,
 /* s_start.s_file           */&TPPFile_Empty,
 /* s_start.s_line           */0,
 /* s_start.s_kind           */DCC_SYMTYPE_LABEL,
 /* s_start.s_flag           */DCC_SYMFLAG_NONE,
 /* s_start.s_type           */{0,NULL},
 /* s_start.s_attr           */NULL,
 /* s_start.s_label          */{{
 /* s_start.s_label.sl_alias */NULL,
 /* s_start.s_label.sl_sec   */&DCCSection_Global,
 /* s_start.s_label.sl_addr  */0}}},
 /* s_nmsz                   */0,
 /* s_nmhs                   */1,
 /* s_name                   */{0},
};

PUBLIC struct DCCTarget dcc_target;

PUBLIC void
DCCTarget_Quit(struct DCCTarget *__restrict self) {
 struct DCCSection **begin,**iter,**end;
 assert(self);
#if DCC_DEBUG
 assert(!self->t_data || DCCTarget_IsSection(self,self->t_data));
 assert(!self->t_text || DCCTarget_IsSection(self,self->t_text));
 assert(!self->t_bss  || DCCTarget_IsSection(self,self->t_bss));
 assert(!self->t_curr || DCCTarget_IsSection(self,self->t_curr));
 assert(!self->t_prev || DCCTarget_IsSection(self,self->t_prev));
#endif
 if (self->t_curr) {
  memcpy(&self->t_curr->s_text,
         &self->t_currbuf,
         sizeof(struct DCCTextBuf));
 }
 end = (begin = self->t_secv)+self->t_secc;
 self->t_secc = 0;
 self->t_secv = NULL;
 assert(self->t_vstack.v_bottom ==
        self->t_vstack.v_end);
 free(self->t_vstack.v_begin);
 DCCSymtab_Quit(&self->t_asmlbl,1);
 DCCSymtab_Quit(&self->t_intern,1);
 assert(!self->t_scope.s_prev);
 DCCSymtab_Quit(&self->t_scope.s_tabs[0],1);
 DCCSymtab_Quit(&self->t_scope.s_tabs[1],1);
 DCCSymtab_Quit(&self->t_scope.s_tabs[2],1);
 DCCSymlst_Quit(&self->t_symlst);
 for (iter = begin; iter != end; ++iter) DCCSection_Quit(*iter);
 free(begin);
}


PUBLIC void
DCCTarget_SetCurrSection(struct DCCSection *section) {
 assert(dcc_current);
#if DCC_DEBUG
 assert(!dcc_current->t_curr || DCCTarget_IsSection(dcc_current,dcc_current->t_curr));
 assert(!section || DCCTarget_IsSection(dcc_current,section));
#endif
 /* Store the previously selected section. */
 dcc_current->t_prev = dcc_current->t_curr;
 if likely(dcc_current->t_curr != section) {
  if (dcc_current->t_curr) {
   memcpy(&dcc_current->t_curr->s_text,
          &dcc_current->t_currbuf,
          sizeof(struct DCCTextBuf));
  }
  dcc_current->t_curr = section;
  if (section) {
   memcpy(&dcc_current->t_currbuf,
          &section->s_text,
          sizeof(struct DCCTextBuf));
  }
 }
}
PUBLIC void DCCTarget_LoadTextSection(void) {
 if (dcc_current->t_curr) {
  memcpy(&dcc_current->t_currbuf,
         &dcc_current->t_curr->s_text,
         sizeof(struct DCCTextBuf));
 }
}
PUBLIC void DCCTarget_FlushTextSection(void) {
 if (dcc_current->t_curr) {
  memcpy(&dcc_current->t_curr->s_text,
         &dcc_current->t_currbuf,
         sizeof(struct DCCTextBuf));
 }
}



PUBLIC struct DCCSection *
DCCTarget_GetSection(char const *__restrict name, size_t name_size) {
 struct DCCSection **iter,**end,*elem;
 hash_t h;
 assert(dcc_current);
 h = TPP_Hashof(name,name_size);
 end = (iter = dcc_current->t_secv)+dcc_current->t_secc;
 for (; iter != end; ++iter) {
  elem = *iter;
  if (elem->s_nmhs == h && elem->s_nmsz == name_size &&
     !memcmp(elem->s_name,name,name_size*sizeof(char))) goto end;
 }
 elem = NULL;
end:
 return elem;
}

PUBLIC struct DCCSection *
DCCTarget_NewSection(char const *__restrict name,
                     size_t name_size,
                     uint32_t type) {
 struct DCCSection **iter,**end,*elem;
 hash_t h;
 assert(dcc_current);
 h = TPP_Hashof(name,name_size);
 end = (iter = dcc_current->t_secv)+dcc_current->t_secc;
 for (; iter != end; ++iter) {
  elem = *iter;
  if (elem->s_nmhs == h && elem->s_nmsz == name_size &&
     !memcmp(elem->s_name,name,name_size*sizeof(char))) goto end;
 }
 iter = (struct DCCSection **)realloc(dcc_current->t_secv,
                                     (dcc_current->t_secc+1)*
                                      sizeof(struct DCCSection *));
 if unlikely(!iter) goto seterr;
 dcc_current->t_secv = iter;
 elem = (struct DCCSection *)calloc(1,offsetof(struct DCCSection,s_name)+
                                   (name_size+1)*sizeof(char));
 if unlikely(!elem) goto seterr;
 iter[dcc_current->t_secc++]   = elem;
 elem->s_type                  = type;
 elem->s_align                 = 1;
 TPPFile_Incref(&TPPFile_Empty);
 elem->s_start.s_refcnt        = 1;
 elem->s_start.s_file          = &TPPFile_Empty;
 elem->s_start.s_kind          = DCC_SYMTYPE_LABEL;
 elem->s_start.s_label.sl_sec  = elem;
 elem->s_nmsz                  = name_size;
 elem->s_nmhs                  = h;
 memcpy(elem->s_name,name,name_size*sizeof(char));
 elem->s_name[name_size] = '\0';
end: return elem;
seterr: TPPLexer_SetErr();
 elem = NULL;
 goto end;
}

PUBLIC struct DCCSym *
DCCTarget_ScopeGet(struct TPPKeyword const *__restrict name, int ns) {
 struct DCCScope *iter = &dcc_current->t_scope;
 struct DCCSym *result;
 do if ((result = DCCSymtab_Get(&iter->s_tabs[ns],name)) != NULL) break;
 while ((iter = iter->s_prev) != NULL);
 return result;
}
PUBLIC struct DCCSym *
DCCTarget_ScopeNew(struct TPPKeyword const *__restrict name, int ns) {
 struct DCCSym *result = DCCTarget_ScopeGet(name,ns);
 if (!result) result = DCCSymtab_New(&dcc_current->t_scope.s_tabs[ns],name);
 return result;
}


PRIVATE int DCCTarget_ResizeText(void) {
 return DCCTextbuf_IncCodeSize(&dcc_current->t_currbuf);
}

PUBLIC void
DCCTarget_TextPutb(uint8_t b) {
 assert(dcc_current);
 assertf(dcc_current->t_curr,"No section selected");
 if (dcc_current->t_flags&DCC_TARGET_FLAG_NOCGEN) return;
 if unlikely(dcc_current->t_currbuf.tb_pos ==
             dcc_current->t_currbuf.tb_end) {
  /* Must relocate the section buffer. */
  if unlikely(!DCCTarget_ResizeText()) return;
 }
 *dcc_current->t_currbuf.tb_pos++ = b;
}

PUBLIC void *
DCCTarget_TextAlloc(size_t s) {
 assert(dcc_current);
 assertf(dcc_current->t_curr,"No section selected");
 return (dcc_current->t_flags&DCC_TARGET_FLAG_NOCGEN)
  ? NULL : DCCTextbuf_Alloc(&dcc_current->t_currbuf,s);
}
PUBLIC void
DCCTarget_TextWrite(void const *__restrict p, size_t s) {
 void *buf = DCCTarget_TextAlloc(s);
 if (buf) memcpy(buf,p,s);
}
PUBLIC void
DCCTarget_TextFill(int byte, size_t s) {
 void *buf = DCCTarget_TextAlloc(s);
 if (buf) memset(buf,byte,s);
}
PUBLIC void
DCCTarget_TextAlign(int byte, size_t a) {
 target_ptr_t old_textaddr,new_textaddr;
 old_textaddr = DCCTarget_TEXTADDR();
 new_textaddr = (old_textaddr+(a-1)) & ~(a-1);
 DCCTarget_TextFill(byte,(size_t)(new_textaddr-old_textaddr));
}

PUBLIC struct DCCSym *
DCCTarget_LookupInternalSym(char const *name) {
 struct TPPKeyword *name_kwd; struct DCCSym *result;
 name_kwd = TPPLexer_LookupKeyword(name,strlen(name),1);
 if unlikely(!name_kwd) return NULL;
 result = DCCSymtab_New(&dcc_current->t_public,name_kwd);
 if unlikely(!result) return NULL;
 if (result->s_kind == DCC_SYMTYPE_NONE) {
  result->s_kind  = DCC_SYMTYPE_LABEL;
  result->s_flag |= DCC_SYMFLAG_COMPDEF;
  assert(!result->s_label.sl_alias);
  assert(!result->s_label.sl_sec);
  assert(!result->s_label.sl_addr);
  assert(!result->s_type.t_base);
  assert(result->s_type.t_type == DCCTYPE_INT);
  DCCType_MkOldFunc(&result->s_type);
 } else if (result->s_kind != DCC_SYMTYPE_LABEL) {
  WARN(W_COMPILER_FUNCTION_NOT_DEFINED_AS_LABEL,result);
  result = NULL;
 }
 return result;
}
PUBLIC struct DCCSym *
DCCTarget_LookupInternalSymf(char const *fmt, ...) {
 char buffer[128]; va_list args;
 va_start(args,fmt);
 vsnprintf(buffer,sizeof(buffer),fmt,args);
 va_end(args);
 buffer[(sizeof(buffer)/sizeof(*buffer))-1] = '\0';
 return DCCTarget_LookupInternalSym(buffer);
}


LOCAL int_t
DCCTarget_PutSecrel(struct DCCSymExpr const *__restrict expr,
                    rel_t rel_size) {
 struct DCCSym *xsym; int_t xval;
 assert(expr);
 assert(dcc_current);
 xval = expr->e_int;
 if ((xsym = expr->e_sym) != NULL) {
  while ((assert(xsym->s_kind == DCC_SYMTYPE_LABEL),
          xsym->s_label.sl_alias)) xsym = xsym->s_label.sl_alias;
#if 1
  if (xsym->s_label.sl_sec && DCCSection_HASBASE(xsym->s_label.sl_sec)) {
   /* The associated section has a fixed base associated with it.
    * >> We don't need to emit a relocation, because
    *    we can simply add the base value here!
    */
   xval += (int_t)xsym->s_label.sl_addr;
   xval += (int_t)(target_ptr_t)xsym->s_label.sl_sec->s_base;
  } else
#endif
  {
   /* Generate a relocation at the current address.
    * >> This relocation must later add its base to the symbol. */
   DCCSection_PutReloc(dcc_current->t_curr,xval
                       ? DCC_REL_OPADD|rel_size
                       : DCC_REL_OPSET|rel_size,
                       xsym);
  }
 }
 return xval;
}

PUBLIC void DCCTarget_TextExpr8(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutb((uint8_t)DCCTarget_PutSecrel(expr,DCC_REL_SIZE8)); }
PUBLIC void DCCTarget_TextExpr16(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutw((uint16_t)DCCTarget_PutSecrel(expr,DCC_REL_SIZE16)); }
PUBLIC void DCCTarget_TextExpr32(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutl((uint32_t)DCCTarget_PutSecrel(expr,DCC_REL_SIZE32)); }
#if DCC_TARGET_SIZEOF_POINTER >= 8
PUBLIC void DCCTarget_TextExpr64(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutq((uint64_t)DCCTarget_PutSecrel(expr,DCC_REL_SIZE64)); }
#endif

LOCAL int_t
DCCTarget_PutDisprel(struct DCCSymExpr const *__restrict expr,
                     rel_t rel_size) {
 struct DCCSym *xsym; int_t xval;
 assert(expr);
 xval = expr->e_int;
 if ((xsym = expr->e_sym) != NULL) {
  while ((assert(xsym->s_kind == DCC_SYMTYPE_LABEL),
          xsym->s_label.sl_alias)) xsym = xsym->s_label.sl_alias;
  if (xsym->s_label.sl_sec == dcc_current->t_curr) {
   /* Emit to current section (no relocation required). */
   xval += xsym->s_label.sl_addr;
  } else {
   DCCSection_PutReloc(dcc_current->t_curr,DCC_REL_OPDISP|rel_size,xsym);
  }
  xval -= DCCTarget_TEXTADDR();
 } else {
  if (dcc_current->t_curr == &DCCSection_Global) {
   /* Emit to current section (no relocation required). */
   xval += xsym->s_label.sl_addr;
  } else {
   /* Because this is a DISP, we must create a
    * fake symbol apart of the global section. */
   DCCSection_PutReloc(dcc_current->t_curr,DCC_REL_OPDISP|rel_size,
                      &DCCSection_Global.s_start);
  }
  xval -= DCCTarget_TEXTADDR();
 }
 return xval;
}

DCCFUN void DCCTarget_TextDisp8(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutb((uint8_t)((int8_t)DCCTarget_PutDisprel(expr,DCC_REL_SIZE8)-1)); }
DCCFUN void DCCTarget_TextDisp16(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutw((uint16_t)((int16_t)DCCTarget_PutDisprel(expr,DCC_REL_SIZE16)-2)); }
DCCFUN void DCCTarget_TextDisp32(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutl((uint32_t)((int32_t)DCCTarget_PutDisprel(expr,DCC_REL_SIZE32)-4)); }
#if DCC_TARGET_SIZEOF_POINTER >= 8
DCCFUN void DCCTarget_TextDisp64(struct DCCSymExpr const *__restrict expr) { DCCTarget_TextPutq((uint64_t)((int64_t)DCCTarget_PutDisprel(expr,DCC_REL_SIZE64)-8)); }
#endif




DCC_DECL_END

#endif /* !GUARD_DCC_CODE_C */
