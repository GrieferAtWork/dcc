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
#ifndef GUARD_DCC_COMPILER_C
#define GUARD_DCC_COMPILER_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/target.h>
#include <dcc/vstack.h>

#include <stdlib.h>
#include <string.h>

DCC_DECL_BEGIN

#define DCC_SECTION_HAVE_MERGE 0 /* Requires reference-counted data-allocation tracking. */
LOCAL void DCCDecltab_CheckRehash(struct DCCDecltab *__restrict self);
LOCAL void DCCDecltab_RehashSymbols(struct DCCDecltab *__restrict self, size_t newsize);

#if DCC_DEBUG
#define DCCDecl_Alloc()    (struct DCCDecl *)calloc(1,sizeof(struct DCCDecl))
#define DCCDecl_Free(self) free(self)
#else
LOCAL struct DCCDecl *DCCDecl_Alloc(void);
LOCAL void DCCDecl_Free(struct DCCDecl *__restrict self);
#endif

/* **** */
/* DECL */
/* **** */

PUBLIC void
_DCCDecl_Delete(struct DCCDecl *__restrict self) {
 struct DCCDecl *tydecl;
 struct DCCAttrDecl *dattr;
 assert(self);
 assert(self->d_file);
 tydecl = self->d_type.t_base;
 self->d_type.t_type = DCCTYPE_INT;
 self->d_type.t_base = NULL;
 dattr = self->d_attr;
 self->d_attr = NULL;
 if (self->d_kind == DCC_DECLKIND_MLOC) {
  if (self->d_mdecl.md_loc.ml_sym) {
   struct DCCSym *sym = self->d_mdecl.md_loc.ml_sym;
   self->d_mdecl.md_loc.ml_sym = NULL;
   DCCSym_Decref(sym);
  }
 } else if (self->d_kind&DCC_DECLKIND_TYPE) {
  if (self->d_kind != DCC_DECLKIND_ARRAY &&
      self->d_kind != DCC_DECLKIND_VLA) {
   struct DCCStructField *begin,*iter,*end;
   end = (iter = begin = self->d_tdecl.td_fieldv)+
                         self->d_tdecl.td_size;
   self->d_tdecl.td_fieldv = NULL;
   self->d_tdecl.td_size   = 0;
   for (; iter != end; ++iter) {
    assert(iter->sf_decl);
    DCCDecl_Decref(iter->sf_decl);
   }
   free(begin);
  }
 }
 DCCDecl_XDecref(tydecl);
 if (dattr) DCCAttrDecl_Quit(dattr),free(dattr);
 TPPFile_Decref(self->d_file);
 DCCDecl_Free(self);
}

PUBLIC /*ref*/struct DCCDecl *
DCCDecl_New(struct TPPKeyword const *__restrict name) {
 struct DCCDecl *result = DCCDecl_Alloc();
 assert(name);
 if unlikely(!result) goto seterr;
 result->d_refcnt = 1;
 result->d_name   = name;
 result->d_file   = TPPLexer_Textfile();
 assert(result->d_file);
 TPPFile_Incref(result->d_file);
 result->d_line   = TPPFile_LineAt(result->d_file,
                                   result->d_file->f_pos);
 return result;
seterr: TPPLexer_SetErr();
 return NULL;
}

PUBLIC void
DCCDecl_Clear(struct DCCDecl *__restrict self) {
 struct DCCDecl *tydecl;
 struct DCCAttrDecl *dattr;
 uint16_t old_kind;
 assert(self);
 assert(self->d_file);
 tydecl = self->d_type.t_base;
 old_kind = self->d_kind;
 self->d_kind = DCC_DECLKIND_NONE;
 self->d_type.t_type = DCCTYPE_INT;
 self->d_type.t_base = NULL;
 dattr = self->d_attr;
 self->d_attr = NULL;
 if (old_kind == DCC_DECLKIND_MLOC) {
  if (self->d_mdecl.md_loc.ml_sym) {
   struct DCCSym *sym = self->d_mdecl.md_loc.ml_sym;
   self->d_mdecl.md_loc.ml_sym = NULL;
   DCCSym_Decref(sym);
  }
  memset(&self->d_mdecl,0,sizeof(struct DCCMemDecl));
 } else if (old_kind&DCC_DECLKIND_TYPE) {
  if (old_kind != DCC_DECLKIND_ARRAY &&
      old_kind != DCC_DECLKIND_VLA) {
   struct DCCStructField *begin,*iter,*end;
   end = (iter = begin = self->d_tdecl.td_fieldv)+
                         self->d_tdecl.td_size;
   self->d_tdecl.td_fieldv = NULL;
   self->d_tdecl.td_size   = 0;
   for (; iter != end; ++iter) {
    assert(iter->sf_decl);
    DCCDecl_Clear(iter->sf_decl);
    DCCDecl_Decref(iter->sf_decl);
   }
   free(begin);
  }
  if (tydecl) DCCDecl_Clear(tydecl);
  memset(&self->d_tdecl,0,sizeof(struct DCCTypeDef));
 }
 DCCDecl_XDecref(tydecl);
 if (dattr) DCCAttrDecl_Quit(dattr),free(dattr);
}

PUBLIC int
DCCDecl_Equal(struct DCCDecl const *__restrict a,
              struct DCCDecl const *__restrict b,
              int same_type) {
 assert(a);
 assert(b);
 if (a == b) return 1;
 if ((a->d_kind&DCC_DECLKIND_TYPE) &&
     (b->d_kind&DCC_DECLKIND_TYPE)) {
  if (!DCCType_IsCompatible(&a->d_type,&b->d_type,0)) return 0;
  switch (a->d_kind) {
  {
   struct DCCStructField *ai,*ae,*bi;
  case DCC_DECLKIND_STRUCT:
  case DCC_DECLKIND_UNION:
   if (a->d_tdecl.td_struct_size != b->d_tdecl.td_struct_size ||
       a->d_tdecl.td_struct_align != b->d_tdecl.td_struct_align) return 0;
  case DCC_DECLKIND_FUNCTION:
  case DCC_DECLKIND_OLDFUNCTION:
   if (a->d_tdecl.td_size != b->d_tdecl.td_size) return 0;
   ae = (ai = a->d_tdecl.td_fieldv)+a->d_tdecl.td_size;
   bi = b->d_tdecl.td_fieldv;
   for (; ai != ae; ++ai,++bi) {
    if (ai->sf_off != bi->sf_off || !DCCDecl_Equal(ai->sf_decl,bi->sf_decl,same_type)) return 0;
   }
  } break;
  case DCC_DECLKIND_ARRAY:
  case DCC_DECLKIND_VLA:
   if (a->d_tdecl.td_vlaoff != b->d_tdecl.td_vlaoff) return 0;
   break;
  default: break;
  }
  return 1;
 } else if (a->d_kind == DCC_DECLKIND_MLOC &&
            b->d_kind == DCC_DECLKIND_MLOC) {
  if (same_type && !DCCType_IsCompatible(&a->d_type,&b->d_type,0)) return 0;
  return DCCMemLoc_Equal(&a->d_mdecl.md_loc,&b->d_mdecl.md_loc);
 }
 return 0;
}

PUBLIC symflag_t
DCCDecl_CalculateSymflags(struct DCCDecl const *__restrict self) {
 symflag_t result = DCC_SYMFLAG_NONE;
 struct DCCAttrDecl *attr;
 assert(self);
 if ((attr = self->d_attr) != NULL) {
#if DCC_TARGET_BIN == DCC_BINARY_PE
  if (attr->a_flags&DCC_ATTRFLAG_DLLIMPORT) result |= DCC_SYMFLAG_DLLIMPORT;
  if (attr->a_flags&DCC_ATTRFLAG_DLLEXPORT) result |= DCC_SYMFLAG_DLLEXPORT;
#endif
  if (attr->a_flags&DCC_ATTRFLAG_WEAK) result |= DCC_SYMFLAG_WEAK;
  if (attr->a_flags&DCC_ATTRFLAG_USED) result |= DCC_SYMFLAG_USED;
  switch (attr->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY) {
  default: goto default_visibility;
  case DCC_ATTRFLAG_VIS_DEFAULT  : result |= DCC_SYMFLAG_NONE; break;
  case DCC_ATTRFLAG_VIS_HIDDEN   : result |= DCC_SYMFLAG_PRIVATE; break;
  case DCC_ATTRFLAG_VIS_PROTECTED: result |= DCC_SYMFLAG_PROTECTED; break;
  case DCC_ATTRFLAG_VIS_INTERNAL : result |= DCC_SYMFLAG_INTERNAL; break;
  }
 } else {
default_visibility:
  /* Load the default symbol visibility. */
  result |= compiler.c_visibility.vs_viscur;
 }

 if ((self->d_type.t_type&DCCTYPE_STOREMASK) == DCCTYPE_STATIC)
      result |= DCC_SYMFLAG_STATIC; /* Hide inside unit. */
 return result;
}


PRIVATE int
DCCDecl_FixLValue(struct DCCDecl *__restrict self) {
 if (DCCTYPE_GROUP(self->d_type.t_type) == DCCTYPE_LVALUE) {
  WARN(W_LVALUE_REQUIRES_INITIALIZER,&self->d_type);
  /* While this type fix-up necessary to prevent DCC from crashing, it
   * prevents code from being generated that could do nothing _but_ crash. */
  DCCType_MkBase(&self->d_type);
  return 1;
 }
 return 0;
}

PUBLIC void
DCCDecl_AllocStorage(struct DCCDecl *__restrict self, int has_init,
                     struct TPPKeyword const *asmname) {
 int is_global,is_const; tyid_t storage; target_siz_t s,a;
 struct DCCAttrDecl *attr;
 symflag_t symflags;
 assert(self);
 assert(self->d_kind == DCC_DECLKIND_NONE);
 is_global = !compiler.c_fun;
 is_const  = self->d_type.t_type&DCCTYPE_CONST;
 storage   = self->d_type.t_type&DCCTYPE_STOREMASK;
 symflags  = DCCDecl_CalculateSymflags(self);
 self->d_type.t_type &= ~(DCCTYPE_STOREMASK);
 /* TODO: When non-NULL, interpret 'asmname' as a register name for local declarations:
  *   >> int EAX __asm__("%eax");
  *   >> printf("EAX = %d\n",EAX);
  *   >> printf("EAX = %d\n",%eax); // Same thing!
  */
 if (!asmname) asmname = DCCDecl_GenAsmname(self);
 if (asmname == &TPPKeyword_Empty &&
   !(symflags&DCC_SYMFLAG_STATIC)) {
  /* Don't export unnamed symbols. */
  WARN(W_DECL_UNNAMED_IMPLIES_STATIC);
  symflags |= DCC_SYMFLAG_STATIC;
 }

 if ((attr = self->d_attr) != NULL && attr->a_alias) {
  /* This symbol is aliasing another. */
  if (storage != DCCTYPE_AUTOMATIC && storage != DCCTYPE_EXTERN)
      WARN(W_ALIAS_WITHOUT_AUTOMATIC_STORAGE,self->d_name);
  if (has_init) WARN(W_ALIAS_WITH_INITIALIZER,self->d_name);
  /* Warn if this and the alias have incompatible types. */
  self->d_kind = DCC_DECLKIND_MLOC;
  assert(self->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
  assert(self->d_mdecl.md_loc.ml_off == 0);
  self->d_mdecl.md_loc.ml_sym = DCCUnit_NewSym(asmname,symflags);
  if unlikely(!self->d_mdecl.md_loc.ml_sym) return;
  /* Create reference for assignment above. */
  DCCSym_Incref(self->d_mdecl.md_loc.ml_sym);
  DCCSym_Alias(self->d_mdecl.md_loc.ml_sym,attr->a_alias);
  return;
 }
 if (storage == DCCTYPE_EXTERN) {
  /* Extern declaration (if an initializer is given, this is a public one) */
  if (has_init) {
   if (!is_global) WARN(W_EXTERN_VARIABLE_LOCALLY_INITIALIZED);
   /* Declare as an automatic storage symbol. */
   storage = DCCTYPE_AUTOMATIC;
  } else {
   struct DCCSym *forward_sym;
forward_decl:
   self->d_kind = DCC_DECLKIND_MLOC;
   /* Extern symbol declaration. */
   assert(self->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
   assert(self->d_mdecl.md_loc.ml_off == 0);
   assert(self->d_mdecl.md_loc.ml_sym == NULL);
   forward_sym = DCCUnit_NewSym(asmname,symflags);
   if unlikely(!forward_sym) return;
   forward_sym->sy_flags |= symflags;
   self->d_mdecl.md_loc.ml_sym = forward_sym;
   DCCSym_Incref(forward_sym); /* Create reference. */
   if (attr && attr->a_section &&
     /*DCCSym_ISFORWARD(forward_sym) &&*/
       DCCSection_ISIMPORT(attr->a_section)) {
    /* Special case: Define an explicit dll-import symbol. */
    DCCSym_Define(forward_sym,attr->a_section,0,0);
#if DCC_TARGET_BIN == DCC_BINARY_PE
    forward_sym->sy_flags |= DCC_SYMFLAG_PE_ITA_IND;
#endif
   }
   return;
  }
 }
 if (DCCTYPE_GROUP(self->d_type.t_type) == DCCTYPE_FUNCTION) {
  /* Force forward/extern declaration for function types. */
  goto forward_decl;
 }
 if (!is_global &&
     DCCTYPE_GROUP(self->d_type.t_type) == DCCTYPE_ARRAY &&
     self->d_type.t_base->d_kind == DCC_DECLKIND_VLA) {
  /* Setup an offset-style variable. */
  self->d_kind                = DCC_DECLKIND_MLOC;
  self->d_mdecl.md_loc.ml_reg = DCC_RR_XBP;
  self->d_mdecl.md_loc.ml_off = DCCCompiler_HWStackAlloc(DCC_TARGET_SIZEOF_POINTER,
                                                     DCC_TARGET_SIZEOF_POINTER,0);
  self->d_mdecl.md_scope      = compiler.c_scope.s_id;
  /* Allocate VLA memory using alloca functionality. */
  DCCVStack_PushSizeof(&self->d_type); /* Load the runtime size of the VLA type. */
  vpushxr(DCC_RR_XSP); /* siz, %esp */
  vswap();             /* %esp, siz */
  vgen2('-');          /* %esp-siz */

  /* At this point, '%ESP' contains the address where the VLA is allocated at.
   * >> Now we simply need to store '%esp' in 'self' */
  vpushd(self);  /* %esp, self */
  vprom();
  vbottom->sv_flags |= DCC_SFLAG_LVALUE;
  vswap();       /* self, %esp */
  vcast(&vbottom[1].sv_ctype,1); /* Cast %esp to the correct type. */
  vstore(1);     /* self=%esp */
  vpop(1);       /* . */
  return;
 }

 assert(storage != DCCTYPE_EXTERN);
reload_size:
 s = DCCType_Sizeof(&self->d_type,&a,1);
 if (attr) { /* Respect alignment/packing from attributes. */
  if (attr->a_flags&DCC_ATTRFLAG_PACKED) {
   a = (attr->a_flags&DCC_ATTRFLAG_FIXEDALIGN) ? attr->a_align : 1;
  } else if (attr->a_flags&DCC_ATTRFLAG_FIXEDALIGN && attr->a_align > a) {
   a = attr->a_align;
  }
 }
 if (storage == DCCTYPE_STATIC || is_global) {
  struct DCCSection *storage_section;
  struct DCCSym *decl_sym;
  target_ptr_t symaddr;
  /* Allocate static storage duration (either read-write, or read-only).
   * NOTE: If an explicit section was specified, use that instead! */
  self->d_kind = DCC_DECLKIND_MLOC;
  assert(self->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
  assert(self->d_mdecl.md_loc.ml_off == 0);
  assert(self->d_mdecl.md_loc.ml_sym == NULL);
  assert(self->d_mdecl.md_scope      == 0);
  decl_sym = DCCUnit_NewSym(asmname,symflags);
  if unlikely(!decl_sym) return;
  decl_sym->sy_flags |= symflags;
  self->d_mdecl.md_loc.ml_sym = decl_sym;
  DCCSym_Incref(decl_sym); /* Create reference. */
  if (attr && attr->a_section) {
   storage_section = attr->a_section;
   if (DCCSection_ISIMPORT(storage_section)) {
    /* Special case: Define an explicit dll-import symbol. */
    DCCSym_Define(decl_sym,storage_section,0,0);
#if DCC_TARGET_BIN == DCC_BINARY_PE
    decl_sym->sy_flags |= DCC_SYMFLAG_PE_ITA_IND;
#endif
    return;
   }
  } else {
   storage_section = is_const ? unit.u_data : unit.u_bss;
  }
  if (!has_init && DCCDecl_FixLValue(self)) goto reload_size;
  assert(storage_section);
  if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) {
   symaddr = 0; /* ~shrugs~ */
  } else if (DCCSym_ISFORWARD(decl_sym) ||
             /* Don't re-define if already defined and this is a weak declaration. */
            !attr || !(attr->a_flags&DCC_ATTRFLAG_WEAK)) {
   /* Warn if allocating within the current
    * text-section, but not when inside the global scope. */
   if (storage_section == unit.u_curr &&
       compiler.c_scope.s_prev) {
    WARN(W_ALLOC_OBJECT_IN_TEXT,self,storage_section);
   }
   if (!(storage_section->sc_start.sy_flags&DCC_SYMFLAG_SEC_W) &&
       !(self->d_type.t_type&DCCTYPE_CONST)
       ) WARN(W_DECL_NONCONST_IN_RO_SECTION,self,storage_section);
   DCCSection_TBEGIN(storage_section);
   symaddr = DCCSection_DAlloc(storage_section,s,a,0);
   DCCSection_TEND(storage_section);
  } else {
   return;
  }
  DCCSym_Define(decl_sym,storage_section,symaddr,s);
  return;
 }
 if (!has_init && DCCDecl_FixLValue(self)) goto reload_size;
 if (storage == DCCTYPE_REGISTER) { /* todo: Allocate register memory? */ }
 /* Default case: Allocate local memory. */
 self->d_kind                = DCC_DECLKIND_MLOC;
 self->d_mdecl.md_scope      = compiler.c_scope.s_id;
 self->d_mdecl.md_loc.ml_reg = DCC_RR_XBP;
 self->d_mdecl.md_loc.ml_off = DCCCompiler_HWStackAlloc(s,a,0);
 assert(self->d_mdecl.md_loc.ml_sym == NULL);
}

PUBLIC struct TPPKeyword const *
DCCDecl_GenAsmname(struct DCCDecl *__restrict self) {
 assert(self);
 /* todo: C++ name encoding when declared as 'extern "C++"' */
 return self->d_name;
}


PUBLIC void
DCCDecl_SetAttr(struct DCCDecl *__restrict self,
                struct DCCAttrDecl const *__restrict attr) {
 assert(self);
 assert(attr);
 if (DCCAttrDecl_IsEmpty(attr)) return;
 if (self->d_attr) DCCAttrDecl_Merge(self->d_attr,attr);
 else {
  self->d_attr = (struct DCCAttrDecl *)malloc(sizeof(struct DCCAttrDecl));
  if unlikely(!self->d_attr) TPPLexer_SetErr();
  else DCCAttrDecl_InitCopy(self->d_attr,attr);
 }
}








/* ******* */
/* DECLTAB */
/* ******* */

PUBLIC void
DCCDecltab_Quit(struct DCCDecltab *__restrict self) {
 struct DCCDecl **biter,**bend,*iter,*next;
 assert(self);
 bend = (biter = self->dt_declv)+self->dt_decla;
 for (; biter != bend; ++biter) {
  iter = *biter;
  while (iter) {
   next = iter->d_next;
   iter->d_next = NULL;
   DCCDecl_Clear(iter);
   DCCDecl_Decref(iter);
   iter = next;
  }
 }
 free(self->dt_declv);
}
LOCAL void
DCCDecltab_CheckRehash(struct DCCDecltab *__restrict self) {
 if ((unit.u_symc*3)/2 >= self->dt_decla) {
  size_t newsize = self->dt_decla;
  if unlikely(!newsize) newsize = 4;
  newsize *= 2;
  /* Must allocate more symbols. */
  DCCDecltab_RehashSymbols(self,newsize);
 }
}
LOCAL void
DCCDecltab_RehashSymbols(struct DCCDecltab *__restrict self,
                         size_t newsize) {
 struct DCCDecl **new_map,**biter,**bend,*iter,*next,**dst;
 assert(newsize);
 new_map = (struct DCCDecl **)calloc(newsize,sizeof(struct DCCDecl *));
 if unlikely(!new_map) return;
 bend = (biter = self->dt_declv)+self->dt_decla;
 for (; biter != bend; ++biter) {
  iter = *biter;
  while (iter) {
   next = iter->d_next;
   dst = &new_map[iter->d_name->k_id % newsize];
   iter->d_next = *dst;
   *dst = iter;
   iter = next;
  }
 }
 free(self->dt_declv);
 self->dt_declv = new_map;
 self->dt_decla = newsize;
}

PUBLIC struct DCCDecl *
DCCDecltab_GetDecl(struct DCCDecltab *__restrict self,
                   struct TPPKeyword const *__restrict name) {
 struct DCCDecl *result;
 if unlikely(!self->dt_decla) return NULL;
 result = self->dt_declv[name->k_id % self->dt_decla];
 while (result && result->d_name != name) result = result->d_next;
#if DCC_DEBUG
 if (result && result->d_kind == DCC_DECLKIND_MLOC)
  DCCSym_XASSERT(result->d_mdecl.md_loc.ml_sym);
#endif
 return result;
}
PUBLIC struct DCCDecl *
DCCDecltab_NewDecl(struct DCCDecltab *__restrict self,
                   struct TPPKeyword const *__restrict name) {
 struct DCCDecl *result,**pbucket;
 if likely(self->dt_decla) {
  result = self->dt_declv[name->k_id % self->dt_decla];
  while (result && result->d_name != name) result = result->d_next;
  if (result) {
#if DCC_DEBUG
   if (result->d_kind == DCC_DECLKIND_MLOC)
    DCCSym_XASSERT(result->d_mdecl.md_loc.ml_sym);
#endif
   return result;
  }
 }
 DCCDecltab_CheckRehash(self);
 result = self->dt_decla ? DCCDecl_New(name) : NULL;
 if likely(result) {
  assert(result->d_name == name);
  pbucket = &self->dt_declv[name->k_id % self->dt_decla];
  result->d_next = *pbucket;
  *pbucket = result; /* Inherit reference. */
 }
#if DCC_DEBUG
 if (result && result->d_kind == DCC_DECLKIND_MLOC)
  DCCSym_XASSERT(result->d_mdecl.md_loc.ml_sym);
#endif
 return result;
}











/* ******** */
/* COMPILER */
/* ******** */
PUBLIC struct DCCCompiler DCCCompiler_Current = {0};
PUBLIC void
DCCCompiler_Flush(struct DCCCompiler *__restrict self, uint32_t flags) {
 size_t unused_mem,required_mem;
 assert(self);
 if (flags&DCCCOMPILER_FLUSHFLAG_VSTACK) {
  assert(self->c_vstack.v_begin >= self->c_vstack.v_bottom);
  unused_mem = (size_t)(self->c_vstack.v_begin-self->c_vstack.v_bottom);
  if (unused_mem) {
   struct DCCStackValue *new_begin;
   required_mem = (size_t)(self->c_vstack.v_end-
                           self->c_vstack.v_bottom)*
                    sizeof(struct DCCStackValue);
   memmove(self->c_vstack.v_begin,
           self->c_vstack.v_bottom,
           required_mem);
   new_begin = (struct DCCStackValue *)realloc(self->c_vstack.v_begin,
                                               required_mem);
   if unlikely(!new_begin) new_begin = self->c_vstack.v_begin;
   self->c_vstack.v_bottom = self->c_vstack.v_begin;
   self->c_vstack.v_end = (struct DCCStackValue *)((uintptr_t)self->c_vstack.v_begin+required_mem);
  }
 }
 /* TODO: All the other things? */
}

PUBLIC void
DCCCompiler_ClearDecl(void) {
 struct DCCScope *iter;
 iter = &compiler.c_scope;
 do DCCScope_Quit(iter);
 while ((iter = iter->s_prev) != NULL);
 memset(&compiler.c_scope,0,sizeof(struct DCCScope));
}

PUBLIC void
DCCCompiler_Init(struct DCCCompiler *__restrict self) {
 assert(self);
 memset(self,0,sizeof(struct DCCCompiler));
}
PUBLIC void
DCCCompiler_Quit(struct DCCCompiler *__restrict self) {
 assert(self);
 /* Clear all remaining declaration scopes. */
 { struct DCCScope *iter;
   iter = &self->c_scope;
   do DCCScope_Quit(iter);
   while ((iter = iter->s_prev) != NULL);
 }

 /* Delete miscellaneous data. */
 DCCFreeData_Quit(&self->c_hwstack.hws_free);
 assertf(self->c_vstack.v_bottom == 
         self->c_vstack.v_end,
         "Invalid v-stack size: %lu too many",
        (unsigned long)(self->c_vstack.v_end-
                        self->c_vstack.v_bottom));
 free(self->c_vstack.v_begin);
 free(self->c_pack.ps_stackv);
 free(self->c_visibility.vs_stackv);
}

PUBLIC struct DCCDecl *
DCCCompiler_NewLabel(struct TPPKeyword const *__restrict name) {
 struct DCCDecl *result;
 struct DCCScope *scope;
 assert(name);
 /* Search for scoped */
 scope = &compiler.c_scope;
 for (;;) {
  if (scope->s_kind == DCC_SCOPEKIND_FUNC) {
   /* Search the last scope.
    * >> This is the top-scope of the function, meaning
    *    that this is where missing labels are created.
    * >> All the other scopes are only able to contain
    *    label declarations when GCC's scoped-label
    *    '__label__' extension was used. */
   result = DCCDecltab_NewDecl(&scope->s_tabs[DCC_NS_LABELS],name);
   break;
  }
  result = DCCDecltab_GetDecl(&scope->s_tabs[DCC_NS_LABELS],name);
  if (result) goto found_label;
  assertf(scope->s_prev,"The last scope (the global one) must be a function-scope");
  scope = scope->s_prev;
 }
 if unlikely(!result) return NULL;
found_label:
 if (result->d_kind == DCC_DECLKIND_NONE) {
  /* First declaration! */
  assert(result->d_type.t_base         == NULL);
  assert(result->d_type.t_type         == DCCTYPE_INT);
  assert(result->d_mdecl.md_scope      == 0);
  assert(result->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
  assert(result->d_mdecl.md_loc.ml_off == 0);
  assert(result->d_mdecl.md_loc.ml_sym == NULL);
  result->d_type.t_type = DCCTYPE_VOID;
  result->d_kind        = DCC_DECLKIND_MLOC;
  /* Label symbols are unnamed, meaning that they are
   * implicitly protected and not addressable from assembly. */
  result->d_mdecl.md_loc.ml_sym = DCCUnit_AllocSym();
  DCCSym_XIncref(result->d_mdecl.md_loc.ml_sym); /* Create reference. */
 }
 return result;
}

PUBLIC struct DCCDecl *
DCCCompiler_GetDecl(struct TPPKeyword const *__restrict name, ns_t ns) {
 struct DCCScope *scope;
 struct DCCDecl *result = NULL;
 DCCCompiler_ENUMSCOPE(scope) {
  result = DCCDecltab_GetDecl(&scope->s_tabs[ns],name);
  if (result) goto end;
 }
end:
 return result;
}
PUBLIC struct DCCDecl *
DCCCompiler_NewDecl(struct TPPKeyword const *__restrict name, ns_t ns) {
 struct DCCDecl *result; struct DCCScope *scope;
 DCCCompiler_ENUMSCOPE(scope) {
  result = DCCDecltab_GetDecl(&scope->s_tabs[ns],name);
  if (result) return result;
 }
 return DCCDecltab_NewDecl(&compiler.c_scope.s_tabs[ns],name);
}
PUBLIC struct DCCDecl *
DCCCompiler_GetDecls(char const *__restrict name, ns_t ns) {
 struct TPPKeyword *kname = TPPLexer_LookupKeyword(name,strlen(name),0);
 return kname ? DCCCompiler_GetDecl(kname,ns) : NULL;
}
PUBLIC struct DCCDecl *
DCCCompiler_NewDecls(char const *__restrict name, ns_t ns) {
 struct TPPKeyword *kname = TPPLexer_LookupKeyword(name,strlen(name),1);
 return kname ? DCCCompiler_NewDecl(kname,ns) : NULL;
}


PUBLIC target_off_t
DCCCompiler_HWStackAlloc(target_siz_t size,
                         target_siz_t align,
                         target_siz_t offset) {
 target_ptr_t result;
 target_siz_t free_offset;
 if (DCCCompiler_ISCGEN()) {
  if (compiler.c_hwstack.hws_minalign < align)
      compiler.c_hwstack.hws_minalign = align;
 }
 free_offset = offset;
 if (align > size) free_offset += align-size;
 result = DCCFreeData_Acquire(&compiler.c_hwstack.hws_free,
                              size,align,free_offset);
 if (result != DCC_FREEDATA_INVPTR) {
  result += size;
  assertf(!((result-offset) & (align-1)),"Invalid alignment");
 } else {
  result  =  compiler.c_hwstack.hws_curoffset+size;
  result  = (result+(align-1)-offset) & ~(align-1);
  result +=  offset;
  free_offset = (result-compiler.c_hwstack.hws_curoffset);
  assert(free_offset >= size);
  free_offset -= size;
  if (free_offset) {
   /* Make the alignment offset as free data. */
   DCCFreeData_Release(&compiler.c_hwstack.hws_free,
                       compiler.c_hwstack.hws_curoffset,
                       free_offset);
  }
  compiler.c_hwstack.hws_curoffset = result;
  if (result > compiler.c_hwstack.hws_maxoffset)
   compiler.c_hwstack.hws_maxoffset = result;
 }
 return -(target_off_t)result;
}
PUBLIC void
DCCCompiler_HWStackFree(target_off_t addr, target_siz_t size) {
 assert(addr+size <= 0);
 if unlikely(!size) return;
 addr = -addr;
 if ((target_ptr_t)addr == compiler.c_hwstack.hws_curoffset) {
  /* Simple case: Take away from the current offset. */
  compiler.c_hwstack.hws_curoffset -= size;
 } else {
  addr -= size;
  assert(addr >= 0);
  /* Mark the given address range as free. */
  DCCFreeData_Release(&compiler.c_hwstack.hws_free,addr,size);
 }
}




PUBLIC void DCCCompiler_PackPush(void) {
 target_siz_t *vec = compiler.c_pack.ps_stackv;
 if (compiler.c_pack.ps_stackc ==
     compiler.c_pack.ps_stacka) {
  size_t newsize = compiler.c_pack.ps_stacka;
  if (!newsize) newsize = 1;
  newsize *= 2;
  vec = (target_siz_t *)DCC_Realloc(vec,newsize*sizeof(target_siz_t),
                                    DCCCOMPILER_FLUSHFLAG_PACKSTACK);
  if unlikely(!vec) return;
  compiler.c_pack.ps_stackv = vec;
  compiler.c_pack.ps_stacka = newsize;
 }
 assert(compiler.c_pack.ps_stackc <
        compiler.c_pack.ps_stacka);
 vec[compiler.c_pack.ps_stackc++] =
     compiler.c_pack.ps_pack;
}
PUBLIC void DCCCompiler_PackPop(void) {
 if (!compiler.c_pack.ps_stackc) {
  WARN(W_PRAGMA_PACK_NOTHING_TO_POP);
  return;
 }
 /* Restore the old packing value. */
 compiler.c_pack.ps_pack = compiler.c_pack.ps_stackv[
                         --compiler.c_pack.ps_stackc];
}

PUBLIC void DCCCompiler_VisibilityPush(void) {
 symflag_t *vec = compiler.c_visibility.vs_stackv;
 if (compiler.c_visibility.vs_stackc ==
     compiler.c_visibility.vs_stacka) {
  size_t newsize = compiler.c_visibility.vs_stacka;
  if (!newsize) newsize = 1;
  newsize *= 2;
  vec = (symflag_t *)DCC_Realloc(vec,newsize*sizeof(symflag_t),
                                 DCCCOMPILER_FLUSHFLAG_VISISTACK);
  if unlikely(!vec) return;
  compiler.c_visibility.vs_stackv = vec;
  compiler.c_visibility.vs_stacka = newsize;
 }
 assert(compiler.c_visibility.vs_stackc <
        compiler.c_visibility.vs_stacka);
 vec[compiler.c_visibility.vs_stackc++] =
     compiler.c_visibility.vs_viscur;
}
PUBLIC void DCCCompiler_VisibilityPop(void) {
 if (!compiler.c_visibility.vs_stackc) {
  WARN(W_PRAGMA_GCC_VISIBILITY_NOTHING_TO_POP);
  return;
 }
 /* Restore the old default visibility. */
 compiler.c_visibility.vs_viscur = compiler.c_visibility.vs_stackv[
                                 --compiler.c_visibility.vs_stackc];
}





#if DCC_DEBUG
PUBLIC void DCCCompiler_ClearCache(void) {}
#else
/* Cache allocators. */
PRIVATE void *DCCDecl_Cache = NULL;
LOCAL struct DCCDecl *DCCDecl_Alloc(void) {
 struct DCCDecl *result;
 result = (struct DCCDecl *)DCCDecl_Cache;
 if (result) {
  DCCDecl_Cache = *(void **)result;
  memset(result,0,sizeof(struct DCCDecl));
 } else {
  result = (struct DCCDecl *)calloc(1,sizeof(struct DCCDecl));
 }
 return result;
}
LOCAL void DCCDecl_Free(struct DCCDecl *__restrict self) {
 *(void **)self = DCCDecl_Cache;
 DCCDecl_Cache = (void *)self;
}
PUBLIC void DCCCompiler_ClearCache(void) {
 void *next,*iter = DCCDecl_Cache;
 DCCDecl_Cache = NULL;
 while (iter) {
  next = *(void **)iter;
  free(iter);
  iter = next;
 }
}
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_COMPILER_C */
