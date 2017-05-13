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
#ifndef GUARD_DCC_TPP_SYMBOL_C
#define GUARD_DCC_TPP_SYMBOL_C 1

#include <dcc/common.h>
#include <dcc/symbol.h>
#include <dcc/lexer.h>
#include <dcc/code.h>

#include <stdlib.h>
#include <string.h>

DCC_DECL_BEGIN


PRIVATE struct DCCSym *sym_cache = NULL;
DCCFUN void DCCSym_ClearCache(void) {
 struct DCCSym *iter,*next;
 iter = sym_cache,sym_cache = NULL;
 while (iter) {
  next = iter->s_next;
  free(iter);
  iter = next;
 }
}

PUBLIC int
DCCSym_Equal(struct DCCSym const *__restrict a,
             struct DCCSym const *__restrict b,
             int same_declaration) {
 assert(a);
 assert(b);
 if (a == b) return 1;
 if (a->s_kind != b->s_kind) return 0;
 if (a->s_kind&DCC_SYMTYPE_TYPE) {
  if (!DCCType_IsCompatible(&a->s_type,&b->s_type,0)) return 0;
  switch (a->s_kind) {
  {
   struct DCCStructField *ai,*ae,*bi;
  case DCC_SYMTYPE_STRUCT:
  case DCC_SYMTYPE_UNION:
   if (a->s_ctype.st_struct_size != b->s_ctype.st_struct_size ||
       a->s_ctype.st_struct_align != b->s_ctype.st_struct_align) return 0;
  case DCC_SYMTYPE_FUNCTION:
  case DCC_SYMTYPE_OLDFUNCTION:
   if (a->s_ctype.st_size != b->s_ctype.st_size) return 0;
   ae = (ai = a->s_ctype.st_fieldv)+a->s_ctype.st_size;
   bi = b->s_ctype.st_fieldv;
   for (; ai != ae; ++ai,++bi) {
    if (ai->sf_off != bi->sf_off || !DCCSym_Equal(ai->sf_sym,bi->sf_sym,same_declaration)) return 0;
   }
  } break;
  case DCC_SYMTYPE_ARRAY:
  case DCC_SYMTYPE_VLA:
   if (a->s_ctype.st_vlaoff != b->s_ctype.st_vlaoff) return 0;
   break;
  default: break;
  }
 } else {
  if (same_declaration && !DCCType_IsCompatible(&a->s_type,&b->s_type,0)) return 0;
  if (a->s_kind == DCC_SYMTYPE_OFFSET) {
   return (a->s_offset.so_off == b->s_offset.so_off) &&
          (a->s_offset.so_reg == b->s_offset.so_reg) &&
          (a->s_offset.so_scope == b->s_offset.so_scope);
  } else {
   while (a->s_label.sl_alias) a = a->s_label.sl_alias,assert(a->s_kind == DCC_SYMTYPE_LABEL);
   while (b->s_label.sl_alias) b = b->s_label.sl_alias,assert(b->s_kind == DCC_SYMTYPE_LABEL);
   if (a->s_label.sl_addr != b->s_label.sl_addr ||
       a->s_label.sl_sec != b->s_label.sl_sec) return 0;
  }
 }
 return 1;
}
PUBLIC int
DCCSym_Contains(struct DCCSym const *__restrict vector,
                target_siz_t vector_size_in_bytes,
                struct DCCSym const *__restrict pointer) {
 target_ptr_t vecaddr,ptraddr;
 assert(vector && (vector->s_kind == DCC_SYMTYPE_OFFSET ||
                   vector->s_kind == DCC_SYMTYPE_LABEL));
 assert(pointer && (pointer->s_kind == DCC_SYMTYPE_OFFSET ||
                    pointer->s_kind == DCC_SYMTYPE_LABEL));
 if (vector->s_kind != pointer->s_kind) return 2;
 if (vector->s_kind == DCC_SYMTYPE_OFFSET) {
  if (vector->s_offset.so_reg != pointer->s_offset.so_reg) return 2;
  vecaddr = (target_ptr_t)vector->s_offset.so_off;
  ptraddr = (target_ptr_t)pointer->s_offset.so_off;
 } else {
  if (!vector->s_label.sl_sec ||
      (vector->s_label.sl_sec != pointer->s_label.sl_sec)) return 2;
  vecaddr = vector->s_label.sl_addr;
  ptraddr = pointer->s_label.sl_addr;
 }
 /* Check if the pointers/offsets overlap. */
 return (vecaddr <= ptraddr &&
        (vecaddr+vector_size_in_bytes > ptraddr ||
        (vecaddr-1)+vector_size_in_bytes > ptraddr-1));
}



PUBLIC struct DCCSym *DCCSym_New(void) {
 struct DCCSym *result;
 if ((result = sym_cache) != NULL) {
  sym_cache = result->s_next;
  memset(result,0,sizeof(struct DCCSym));
  goto fill_result;
 }
 result = (struct DCCSym *)calloc(1,sizeof(struct DCCSym));
 if likely(result) {
fill_result:
  result->s_refcnt = 1;
  result->s_name = &TPPKeyword_Empty;
  result->s_file = TPPLexer_Textfile();
  result->s_line = TPPFile_LineAt(result->s_file,result->s_file->f_pos);
  TPPFile_Incref(result->s_file);
 }
 return result;
}
PUBLIC void
DCCSym_Clear(struct DCCSym *__restrict self, int force) {
 assert(self);
 if (self->s_type.t_base) {
  struct DCCSym *base_sym = self->s_type.t_base;
  self->s_type.t_type = 0;
  self->s_type.t_base = NULL;
  if (base_sym != self) {
   if (force) DCCSym_Clear(base_sym,force);
   DCCSym_Decref(base_sym);
  }
 }
 if (self->s_kind&DCC_SYMTYPE_TYPE) {
  if (self->s_ctype.st_fieldv) {
   struct DCCStructField *iter,*end,*start;
   end = (iter = start = self->s_ctype.st_fieldv)+self->s_ctype.st_size;
   self->s_ctype.st_fieldv = NULL;
   for (; iter != end; ++iter) {
    assert(iter->sf_sym);
    DCCSym_Clear(iter->sf_sym,force);
    DCCSym_Decref(iter->sf_sym);
   }
   free(start);
  }
 } else if (self->s_kind == DCC_SYMTYPE_LABEL &&
            self->s_refcnt == 1) {
  if (self->s_label.sl_alias) {
   struct DCCSym *alias;
   assert(!self->s_label.sl_sec);
   alias = self->s_label.sl_alias;
   self->s_label.sl_alias = NULL;
   assert(alias != self);
   if (force) DCCSym_Clear(alias,force);
   DCCSym_Decref(alias);
  } else {
   self->s_label.sl_sec = NULL;
  }
 }
}
PUBLIC void
DCCSym_Destroy(struct DCCSym *__restrict self) {
 assert(self);
 assert(!self->s_refcnt);
 /* Make sure this isn't the start symbol of a section. */
 assertf(self->s_kind != DCC_SYMTYPE_LABEL ||
        !self->s_label.sl_sec ||
        &self->s_label.sl_sec->s_start != self,
        "Can't destroy start symbol of section '%s'"
        ,self->s_label.sl_sec->s_name);
 TPPFile_Decref(self->s_file);
 if (self->s_attr) {
  DCCAttrDecl_Quit(self->s_attr);
  free(self->s_attr);
 }
 if (self->s_type.t_base != self) {
  DCCType_Quit(&self->s_type);
 }
 if (self->s_kind&DCC_SYMTYPE_TYPE) {
  struct DCCStructField *iter,*end;
  if ((iter = self->s_ctype.st_fieldv) != NULL) {
   end = iter+self->s_ctype.st_size;
   for (; iter != end; ++iter) {
    assert(iter->sf_sym);
    DCCSym_Decref(iter->sf_sym);
   }
   free(self->s_ctype.st_fieldv);
  }
 } else if (self->s_kind == DCC_SYMTYPE_LABEL) {
  if (self->s_label.sl_alias) {
   assert(self->s_label.sl_alias != self);
   DCCSym_Decref(self->s_label.sl_alias);
  } else if (self->s_label.sl_sec && !self->s_label.sl_relc) {
   /* Free section data now no longer used. */
   DCCSection_FreeData(self->s_label.sl_sec,self->s_label.sl_addr,
                       self->s_label.sl_size,0);
  }
 }
 /* Append the symbol to the cache. */
 self->s_next = sym_cache;
 sym_cache = self;
}

PUBLIC void
DCCSym_SetAttr(struct DCCSym *__restrict self,
               struct DCCAttrDecl const *__restrict attr) {
 assert(self);
 assert(attr);
 if (DCCAttrDecl_IsEmpty(attr)) return;
 if (self->s_attr) DCCAttrDecl_Merge(self->s_attr,attr);
 else {
  self->s_attr = (struct DCCAttrDecl *)malloc(sizeof(struct DCCAttrDecl));
  if unlikely(!self->s_attr) TPPLexer_SetErr();
  else DCCAttrDecl_InitCopy(self->s_attr,attr);
 }
}

PUBLIC void
DCCSym_AllocStorage(struct DCCSym *__restrict self, int has_init) {
 int is_global,is_const; tyid_t storage;
 struct DCCAttrDecl *attr; target_ptr_t s,a;
 assert(self);
 assert(self->s_kind == DCC_SYMTYPE_NONE);
 is_global = !dcc_current->t_function;
 storage   = self->s_type.t_type&DCCTYPE_STOREMASK;
 is_const  = self->s_type.t_type&DCCTYPE_CONST;
 self->s_type.t_type &= ~(DCCTYPE_STOREMASK);
 if ((attr = self->s_attr) != NULL) {
  struct DCCSym *alias;
  switch (attr->a_flags&DCC_ATTRFLAG_MASK_VISIBILITY) {
  case DCC_ATTRFLAG_VIS_DEFAULT  : self->s_flag |= DCC_SYMFLAG_PUBLIC; break;
  case DCC_ATTRFLAG_VIS_HIDDEN   : self->s_flag |= DCC_SYMFLAG_HIDDEN; break;
  case DCC_ATTRFLAG_VIS_PROTECTED: self->s_flag |= DCC_SYMFLAG_HIDDEN; break; /* ??? */
#if DCC_TARGET_BIN == DCC_BINARY_PE
  case DCC_ATTRFLAG_DLLIMPORT    : self->s_flag |= DCC_SYMFLAG_DLLIMPORT; break;
  case DCC_ATTRFLAG_DLLEXPORT    : self->s_flag |= DCC_SYMFLAG_DLLEXPORT; break;
#endif
  default: break;
  }
  if (attr->a_flags&DCC_ATTRFLAG_WEAK) self->s_flag |= DCC_SYMFLAG_WEAK;
  if ((alias = attr->a_alias) != NULL) {
   /* This symbol is aliasing another. */
   assertf(alias->s_kind == DCC_SYMTYPE_LABEL ||
           alias->s_kind == DCC_SYMTYPE_OFFSET,
           "Can only alias labels, or offset-variables");
   attr->a_alias = NULL; /* Inherit reference. */
   if (storage != DCCTYPE_AUTOMATIC &&
      (storage != DCCTYPE_EXTERN || alias->s_kind == DCC_SYMTYPE_OFFSET)
      ) WARN(W_ALIAS_WITHOUT_AUTOMATIC_STORAGE,self->s_name);
   if (has_init) WARN(W_ALIAS_WITH_INITIALIZER,self->s_name);
   /* Warn if this and the alias have incompatible types. */
   if (!DCCType_IsCompatible(&self->s_type,&alias->s_type,0)
       ) WARN(W_INCOMPATIBLE_ALIAS_TYPES,self->s_name,alias->s_name);
   if (alias->s_kind == DCC_SYMTYPE_OFFSET) {
    self->s_kind            = DCC_SYMTYPE_OFFSET;
    self->s_offset.so_off   = alias->s_offset.so_off;
    self->s_offset.so_reg   = alias->s_offset.so_reg;
    self->s_offset.so_scope = alias->s_offset.so_scope;
    DCCSym_Decref(alias);
   } else {
    self->s_kind = DCC_SYMTYPE_LABEL;
    self->s_label.sl_alias = alias; /* Inherit reference. */
    assert(!self->s_label.sl_sec);
   }
   return;
  }
 }
 if (storage == DCCTYPE_EXTERN) {
  /* Extern declaration (if an initializer is given, this is a public one) */
  if (has_init) {
   if (!is_global) WARN(W_EXTERN_VARIABLE_LOCALLY_INITIALIZED);
   /* Declare as an automatic storage symbol. */
   storage = DCCTYPE_AUTOMATIC;
  } else {
forward_decl:
   self->s_kind = DCC_SYMTYPE_LABEL;
   /* Extern symbol declaration. */
   assert(!self->s_label.sl_alias);
   assert(!self->s_label.sl_sec);
   return;
  }
 }
 if (DCCTYPE_GROUP(self->s_type.t_type) == DCCTYPE_FUNCTION) {
  /* Force forward/extern declaration for function types. */
  if (storage == DCCTYPE_STATIC) {
   self->s_flag &= ~(DCC_SYMFLAG_VISIBILITY);
   self->s_flag |= DCC_SYMFLAG_PROTECTED; /* Hide inside unit. */
  }
  goto forward_decl;
 }
 if (!is_global &&
     DCCTYPE_GROUP(self->s_type.t_type) == DCCTYPE_ARRAY &&
     self->s_type.t_base->s_kind == DCC_SYMTYPE_VLA) {
  /* Setup an offset-style variable. */
  self->s_kind            = DCC_SYMTYPE_OFFSET;
  self->s_offset.so_reg   = DCC_RR_XBP;
  self->s_offset.so_off   = -(target_off_t)DCCStackAllocator_Alloc(DCC_TARGET_SIZEOF_POINTER,
                                                                   DCC_TARGET_SIZEOF_POINTER);
  self->s_offset.so_scope = dcc_current->t_scope.s_id;
  /* Allocate VLA memory using alloca functionality. */
  DCCVStack_PushSizeof(&self->s_type); /* Load the runtime size of the VLA type. */
  vpushxr(DCC_RR_XSP); /* siz, %esp */
  vswap();             /* %esp, siz */
  vgen2('-');          /* %esp-siz*/
  vdup(0),vpop(1);     /* Force apply disposition. */

  /* At this point, '%ESP' contains the address where the VLA is allocated at.
   * >> Now we simply need to store '%esp' in 'self' */
  vpushs(self);  /* %esp, self */
  vprom();
  vbottom->sv_flags |= DCC_SFLAG_LVALUE;
  vswap();       /* self, %esp */
  vcast(&vbottom[1].sv_ctype,0); /* Cast %esp to the correct type. */
  vstore(1);     /* self=%esp */
  vpop(1);       /* . */
  return;
 }


 assert(storage != DCCTYPE_EXTERN);
 s = DCCType_Sizeof(&self->s_type,&a,1);
 if (attr) { /* Respect alignment/packing from attributes. */
  if (attr->a_flags&DCC_ATTRFLAG_PACKED) {
   a = (attr->a_flags&DCC_ATTRFLAG_FIXEDALIGN) ? attr->a_align : 1;
  } else if (attr->a_flags&DCC_ATTRFLAG_FIXEDALIGN && attr->a_align > a) {
   a = attr->a_align;
  }
 }
 if (storage == DCCTYPE_STATIC || is_global) {
  struct DCCSection *storage_section;
  /* Allocate static storage duration (either read-write, or read-only).
   * NOTE: If an explicit section was specified, use that instead! */
  if (attr && attr->a_section) storage_section = attr->a_section;
  else storage_section = is_const ? dcc_current->t_data : dcc_current->t_bss;
  assert(storage_section);
  self->s_kind = DCC_SYMTYPE_LABEL;
  assert(!self->s_label.sl_alias);
  assert(!self->s_label.sl_sec);
  self->s_label.sl_sec  = storage_section;
  self->s_label.sl_addr = DCCSection_Putbyte(storage_section,0,s,a);
  if (storage == DCCTYPE_STATIC) {
   self->s_flag &= ~(DCC_SYMFLAG_VISIBILITY);
   self->s_flag |= DCC_SYMFLAG_PROTECTED; /* Hide inside unit. */
  }
  return;
 }
 if (storage == DCCTYPE_REGISTER) { /* TODO: Allocate register memory. */ }
 /* Default case: Allocate local memory. */
 self->s_kind            = DCC_SYMTYPE_OFFSET;
 self->s_offset.so_reg   = DCC_RR_XBP;
 self->s_offset.so_off   = -(target_off_t)DCCStackAllocator_Alloc(s,a);
 self->s_offset.so_scope = dcc_current->t_scope.s_id;
}




PUBLIC void
DCCSymtab_Quit(struct DCCSymtab *__restrict self, int force) {
 struct DCCSym **iter,**end,*elem,*next,*next2;
 assert(self);
 end = (iter = self->st_symv)+self->st_syma;
 for (; iter != end; ++iter) {
  elem = *iter;
  while (elem) {
   /* Free old instances of this label. */
   next = elem->s_before;
   while (next) {
    assert(!next->s_next);
    next2 = next->s_before;
    next->s_before = NULL;
    DCCSym_Clear(next,force);
    DCCSym_Decref(next);
    next = next2;
   }
   next = elem->s_next;
   elem->s_before = NULL;
   elem->s_next = NULL;
   DCCSym_Clear(elem,force);
   DCCSym_Decref(elem);
   elem = next;
  }
 }
 free(self->st_symv);
}

LOCAL void
DCCSymtab_Rehash(struct DCCSymtab *__restrict self,
                 size_t newsize) {
 struct DCCSym **newvec,**iter,**end,*elem,*next,**bucket;
 assert(newsize >= self->st_syma);
 newvec = (struct DCCSym **)calloc(newsize,sizeof(struct DCCSym *));
 if unlikely(!newvec) goto end;
 end = (iter = self->st_symv)+self->st_syma;
 for (; iter != end; ++iter) {
  elem = *iter;
  /* Rehash all existing symbols. */
  while (elem) {
   next = elem->s_next;
   bucket = &newvec[elem->s_name->k_id % newsize];
   elem->s_next = *bucket;
   *bucket = elem;
   elem = next;
  }
 }
 self->st_symv = newvec;
 self->st_syma = newsize;
end:
 return;
}

LOCAL int
DCCSym_Access(struct DCCSym *__restrict self) {
 struct DCCAttrDecl *attr;
 assert(self);
 /* Don't warn inside of dead branches.
  * This must be done to be compatible with code like this:
  * >> extern void invalid_argument(void) __attribute__((error("Invalid argument")));
  * >> 
  * >> if (__builtin_constant_p(x)) {
  * >>   switch (x) {
  * >>     case 42:
  * >>       do_something();
  * >>       break;
  * >>     default:
  * >>       invalid_argument();
  * >>   }
  * >> } else {
  * >>   assert(x == 42);
  * >>   do_something();
  * >> }
  */
 if (dcc_current->t_flags&DCC_TARGET_FLAG_DEAD) return 1;
 if ((attr = self->s_attr) != NULL && attr->a_reach) {
  int warning = 0;
  switch (attr->a_flags&DCC_ATTRFLAG_MASK_REACHABLE) {
  case DCC_ATTRFLAG_REACH_DEPR:  warning = W_ATTRIBUTE_DEPRECATED_WARNING; break;
  case DCC_ATTRFLAG_REACH_WARN:  warning = W_ATTRIBUTE_WARNING_WARNING; break;
  case DCC_ATTRFLAG_REACH_ERROR: warning = W_ATTRIBUTE_ERROR_WARNING; break;
  default: break;
  }
  if (warning) {
   WARN(warning,self,attr->a_reach->s_text);
   return OK;
  }
 }
 return 1;
}


PUBLIC struct DCCSym *
DCCSymtab_Get(struct DCCSymtab *__restrict self,
              struct TPPKeyword const *__restrict name) {
 struct DCCSym *result; hash_t h;
 assert(self);
 assert(name);
 if unlikely(!self->st_syma) return NULL;
 h = name->k_id % self->st_syma;
 result = self->st_symv[h];
 while (result && result->s_name != name) result = result->s_next;
 if (result && !DCCSym_Access(result)) result = NULL;
 return result;
}
PUBLIC struct DCCSym *
DCCSymtab_New(struct DCCSymtab *__restrict self,
              struct TPPKeyword const *__restrict name) {
 struct DCCSym *result,**bucket; hash_t h;
 size_t newsize;
 assert(self);
 assert(name);
 if unlikely(!self->st_syma) goto force_rehash;
 h = name->k_id % self->st_syma;
 result = self->st_symv[h];
 while (result) {
  if (result->s_name == name) goto end;
  result = result->s_next;
 }
 if (self->st_symc >= self->st_syma*2) {
force_rehash:
  newsize = self->st_symc ? self->st_symc : 32;
  DCCSymtab_Rehash(self,newsize);
  if unlikely(!self->st_symv) goto seterr;
 }
 result = DCCSym_New();
 if unlikely(!result) goto seterr;
 bucket         = &self->st_symv[name->k_id % self->st_syma];
 result->s_name = name;
 result->s_next = *bucket;
 *bucket        = result;
 ++self->st_symc;
end:
 if (!DCCSym_Access(result)) result = NULL;
 return result;
seterr: TPPLexer_SetErr();
 result = NULL;
 goto end;
}
PUBLIC struct DCCSym *
DCCSymtab_Lookup(struct DCCSymtab *__restrict self,
                 char const *__restrict name,
                 int create_missing) {
 struct TPPKeyword *kwd;
 kwd = TPPLexer_LookupKeyword(name,strlen(name),create_missing);
 return kwd ? (create_missing
               ? DCCSymtab_New(self,kwd)
               : DCCSymtab_Get(self,kwd))
            : NULL;
}
PUBLIC struct DCCSym *
DCCSymtab_NewForwardLabel(struct DCCSymtab *__restrict self,
                          struct TPPKeyword *__restrict name) {
 struct DCCSym *elem,*result,**bucket; hash_t h;
 size_t newsize;
 assert(self);
 assert(name);
 if unlikely(!self->st_syma) goto force_rehash;
 h = name->k_id % self->st_syma;
 bucket = &self->st_symv[h];
 while ((result = *bucket) != NULL) {
  if (result->s_name == name) {
   /* Found an existing instance of the symbol.
    * If that instance isn't forward-declared,
    * we must replace it with a new one. */
   if (!(result->s_flag&DCC_SYMFLAG_FORWARD)) {
    elem = result;
    result = DCCSym_New();
    if unlikely(!result) goto seterr;
    result->s_before = elem;
    result->s_name   = name;
    result->s_next   = elem->s_next;
    result->s_kind   = DCC_SYMTYPE_LABEL;
    result->s_flag   = DCC_SYMFLAG_FORWARD;
    *bucket          = result;
   }
   return result;
  }
  bucket = &result->s_next;
 }
 if (self->st_symc >= self->st_syma*2) {
force_rehash:
  newsize = self->st_symc ? self->st_symc : 32;
  DCCSymtab_Rehash(self,newsize);
  if unlikely(!self->st_symv) goto seterr;
 }
 bucket = &self->st_symv[name->k_id % self->st_syma];
 result = DCCSym_New();
 if unlikely(!result) goto seterr;
 result->s_name = name;
 result->s_next = *bucket;
 result->s_kind = DCC_SYMTYPE_LABEL;
 result->s_flag = DCC_SYMFLAG_FORWARD;
 *bucket        = result;
 ++self->st_symc;
end: return result;
seterr: TPPLexer_SetErr();
 result = NULL;
 goto end;
}


DCC_DECL_END

#endif /* !GUARD_DCC_TPP_SYMBOL_C */
