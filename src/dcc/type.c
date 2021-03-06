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
#ifndef GUARD_DCC_TPP_TYPE_C
#define GUARD_DCC_TPP_TYPE_C 1

#include <dcc/common.h>
#include <dcc/type.h>
#include <dcc/compiler.h>

#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

DCC_DECL_BEGIN

struct struct_DCCTypeDecl {
 /* Must preserve binary compatibility with 'DCCDecl'. */
 unsigned int             d_refcnt;
 struct DCCDecl          *d_next;
 struct TPPKeyword const *d_name;
 /*ref*/struct TPPFile   *d_file;
 line_t                   d_line;
 uint16_t                 d_kind;
 uint16_t                 d_flag;
 scopedepth_t             d_depth;
 struct DCCType           d_type;
 struct DCCAttrDecl      *d_attr;
 struct DCCTypeDef        d_tdecl;
};

#define BUILTIN_TYPE(id) \
{ /* d_refcnt */0x80000000,\
  /* d_next   */NULL,\
  /* d_name   */&TPPKeyword_Empty,\
  /* d_file   */&TPPFile_Empty,\
  /* d_line   */0,\
  /* d_kind   */DCC_DECLKIND_TYPE,\
  /* d_flag   */DCC_DECLFLAG_INTERN,\
  /* d_depth  */0,\
  /* d_type   */{id,NULL},\
  /* d_attr   */NULL,\
  /* d_tdecl  */{{0},NULL,{0},0}\
}

INTERN struct struct_DCCTypeDecl t_int = BUILTIN_TYPE(DCCTYPE_INT);
INTERN struct struct_DCCTypeDecl t_byte = BUILTIN_TYPE(DCCTYPE_BYTE);
INTERN struct struct_DCCTypeDecl t_word = BUILTIN_TYPE(DCCTYPE_WORD);
INTERN struct struct_DCCTypeDecl t_ib8 = BUILTIN_TYPE(DCCTYPE_IB8);
INTERN struct struct_DCCTypeDecl t_uint = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_INT);
INTERN struct struct_DCCTypeDecl t_ubyte = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_BYTE);
INTERN struct struct_DCCTypeDecl t_uword = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_WORD);
INTERN struct struct_DCCTypeDecl t_uib8 = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_IB8);
INTERN struct struct_DCCTypeDecl t_float = BUILTIN_TYPE(DCCTYPE_FLOAT);
INTERN struct struct_DCCTypeDecl t_double = BUILTIN_TYPE(DCCTYPE_DOUBLE);
INTERN struct struct_DCCTypeDecl t_ldouble = BUILTIN_TYPE(DCCTYPE_LDOUBLE);
INTERN struct struct_DCCTypeDecl t_bool = BUILTIN_TYPE(DCCTYPE_BOOL);
INTERN struct struct_DCCTypeDecl t_void = BUILTIN_TYPE(DCCTYPE_VOID);
INTERN struct struct_DCCTypeDecl t_const_int = BUILTIN_TYPE(DCCTYPE_INT|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_byte = BUILTIN_TYPE(DCCTYPE_BYTE|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_word = BUILTIN_TYPE(DCCTYPE_WORD|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_ib8 = BUILTIN_TYPE(DCCTYPE_IB8|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_uint = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_INT|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_ubyte = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_BYTE|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_uword = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_WORD|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_uib8 = BUILTIN_TYPE(DCCTYPE_UNSIGNED|DCCTYPE_IB8|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_float = BUILTIN_TYPE(DCCTYPE_FLOAT|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_double = BUILTIN_TYPE(DCCTYPE_DOUBLE|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_ldouble = BUILTIN_TYPE(DCCTYPE_LDOUBLE|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_bool = BUILTIN_TYPE(DCCTYPE_BOOL|DCCTYPE_CONST);
INTERN struct struct_DCCTypeDecl t_const_void = BUILTIN_TYPE(DCCTYPE_VOID|DCCTYPE_CONST);

#define t_builtin(x) (((x) >= DCCType_BuiltinPointers && (x) < DCCType_BuiltinPointers+16) || \
                      ((x) >= DCCType_BuiltinConstPointers && (x) < DCCType_BuiltinConstPointers+16))
PUBLIC struct DCCType const DCCType_BuiltinPointers[16] = {
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_int},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_byte},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_word},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_ib8},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_uint},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_ubyte},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_uword},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_uib8},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_float},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_double},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_ldouble},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_bool},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_void}, /* placeholder */
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_void}, /* placeholder */
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_void}, /* placeholder */
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_void},
};
PUBLIC struct DCCType const DCCType_BuiltinConstPointers[16] = {
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_int},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_byte},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_word},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_ib8},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_uint},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_ubyte},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_uword},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_uib8},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_float},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_double},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_ldouble},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_bool},
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_void}, /* placeholder */
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_void}, /* placeholder */
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_void}, /* placeholder */
 {DCCTYPE_POINTER,(struct DCCDecl *)&t_const_void},
};

PUBLIC struct DCCType *
DCCType_Effective(struct DCCType const *__restrict self) {
 assert(self);
 while (DCCTYPE_GROUP(self->t_type) == DCCTYPE_LVALUE)
        assert(self->t_base),
        self = &self->t_base->d_type;
 return (struct DCCType *)self;
}


PUBLIC void
DCCType_MkPointer(struct DCCType *__restrict self) {
 assert(self);
 assert(!t_builtin(self));
 DCCType_ASSERT(self);
 if (DCCTYPE_GROUP(self->t_type) == DCCTYPE_BUILTIN) {
  assert(!self->t_base);
  if (!(self->t_type&DCCTYPE_FLAGSMASK)) {
   /* Special case: Pointer to builtin type (these are pre-allocated) */
   self->t_base = DCCType_BuiltinPointers[DCCTYPE_BASIC(self->t_type)].t_base;
  } else if ((self->t_type&DCCTYPE_FLAGSMASK) == DCCTYPE_CONST) {
   /* Special case: Pointer to constant builtin type (these are pre-allocated) */
   self->t_base = DCCType_BuiltinConstPointers[DCCTYPE_BASIC(self->t_type)].t_base;
  } else goto default_ptr;
  DCCDecl_Incref(self->t_base);
  self->t_type &= DCCTYPE_STOREMASK;
  self->t_type |= DCCTYPE_POINTER;
 } else {
  struct DCCDecl *ptr_base;
default_ptr:
  /* Fallback: Must allocate a new symbol. */
  ptr_base = DCCDecl_New(&TPPKeyword_Empty);
  if unlikely(!ptr_base) return;
  ptr_base->d_kind = DCC_DECLKIND_TYPE;
  ptr_base->d_type.t_type = self->t_type&~(DCCTYPE_STOREMASK);
  ptr_base->d_type.t_base = self->t_base; /* Inherit reference. */
  self->t_type &= DCCTYPE_STOREMASK;
  self->t_type |= DCCTYPE_POINTER;
  self->t_base  = ptr_base; /* Inherit reference. */
 }
 DCCType_ASSERT(self);
}
PUBLIC void
DCCType_MkLValue(struct DCCType *__restrict self) {
 struct DCCDecl *lv_base;
 assert(self);
 assert(!t_builtin(self));
 DCCType_ASSERT(self);
 /* Fallback: Must allocate a new symbol. */
 lv_base = DCCDecl_New(&TPPKeyword_Empty);
 if unlikely(!lv_base) return;
 lv_base->d_kind = DCC_DECLKIND_TYPE;
 lv_base->d_type.t_type = self->t_type&~(DCCTYPE_STOREMASK);
 lv_base->d_type.t_base = self->t_base; /* Inherit reference. */
 self->t_type &= DCCTYPE_STOREMASK;
 self->t_type |= DCCTYPE_LVALUE;
 self->t_base  = lv_base; /* Inherit reference. */
 DCCType_ASSERT(self);
}
PUBLIC void
DCCType_MkBase(struct DCCType *__restrict self) {
 struct DCCDecl *basesym;
 assert(self);
 DCCType_ASSERT(self);
 basesym = self->t_base;
 assert(basesym);
 assert(basesym->d_kind&DCC_DECLKIND_TYPE);
 /* Don't delete qualifiers for array types. */
 if (!DCCTYPE_ISARRAY(self->t_type)) self->t_type &= ~(DCCTYPE_QUAL);
 self->t_type &= (DCCTYPE_STOREMASK|DCCTYPE_QUAL);
 self->t_type |= basesym->d_type.t_type;
 basesym = basesym->d_type.t_base;
 if (basesym) DCCDecl_Incref(basesym);
 DCCDecl_Decref(self->t_base);
 self->t_base = basesym; /* Inherit reference. */
 DCCType_ASSERT(self);
}
PUBLIC void
DCCType_MkArray(struct DCCType *__restrict self,
                target_siz_t n_elem) {
 struct DCCDecl *array_base;
 assert(self);
 assert(!t_builtin(self));
 DCCType_ASSERT(self);
 /* Fallback: Must allocate a new symbol. */
 array_base = DCCDecl_New(&TPPKeyword_Empty);
 if unlikely(!array_base) return;
 array_base->d_kind = DCC_DECLKIND_ARRAY;
 array_base->d_type.t_type = self->t_type&~(DCCTYPE_STOREMASK);
 array_base->d_type.t_base = self->t_base; /* Inherit reference. */
 assert(!array_base->d_tdecl.td_fieldv);
 array_base->d_tdecl.td_size = n_elem;
 self->t_type &= (DCCTYPE_STOREMASK|DCCTYPE_QUAL);
 self->t_type |=  DCCTYPE_ARRAY;
 self->t_base  =  array_base; /* Inherit reference. */
 DCCType_ASSERT(self);
}
PUBLIC void
DCCType_MkVArray(struct DCCType *__restrict self) {
 struct DCCDecl *array_base;
 assert(self);
 assert(!t_builtin(self));
 DCCType_ASSERT(self);
 /* Fallback: Must allocate a new symbol. */
 array_base = DCCDecl_New(&TPPKeyword_Empty);
 if unlikely(!array_base) return;
 array_base->d_kind = DCC_DECLKIND_ARRAY;
 array_base->d_type.t_type = self->t_type&~(DCCTYPE_STOREMASK);
 array_base->d_type.t_base = self->t_base; /* Inherit reference. */
 assert(!array_base->d_tdecl.td_size);
 self->t_type &= (DCCTYPE_STOREMASK|DCCTYPE_QUAL);
 self->t_type |=  DCCTYPE_VARRAY;
 self->t_base  =  array_base; /* Inherit reference. */
 DCCType_ASSERT(self);
}
PUBLIC void
DCCType_MkVLA(struct DCCType *__restrict self,
              target_off_t abssize_ebp_offset,
              scopeid_t scope) {
 struct DCCDecl *array_base;
 assert(self);
 assert(!t_builtin(self));
 DCCType_ASSERT(self);
 assert(scope <= compiler.c_scope.s_id);
 /* Fallback: Must allocate a new symbol. */
 array_base = DCCDecl_New(&TPPKeyword_Empty);
 if unlikely(!array_base) return;
 array_base->d_kind = DCC_DECLKIND_VLA;
 array_base->d_type.t_type = self->t_type&~(DCCTYPE_STOREMASK);
 array_base->d_type.t_base = self->t_base; /* Inherit reference. */
 array_base->d_tdecl.td_vlaoff   = abssize_ebp_offset;
 array_base->d_tdecl.td_vlascope = scope;
 self->t_type &= DCCTYPE_STOREMASK;
 self->t_type |= DCCTYPE_ARRAY;
 self->t_base  = array_base; /* Inherit reference. */
 DCCType_ASSERT(self);
}

/* old-style function declaration returning an int. */
INTERN struct struct_DCCTypeDecl t_int_oldfun =
{ /* d_refcnt */0x80000000,
  /* d_next   */NULL,
  /* d_name   */&TPPKeyword_Empty,
  /* d_file   */&TPPFile_Empty,
  /* d_line   */0,
  /* d_kind   */DCC_DECLKIND_OLDFUNCTION,
  /* d_flag   */DCC_DECLFLAG_INTERN,
  /* d_depth  */0,
  /* d_type   */{DCCTYPE_INT,NULL},
  /* d_attr   */NULL,
  /* d_tdecl  */{{0},NULL,{0},0}
};
#if DCC_TARGET_SIZEOF_POINTER*DCC_TARGET_BITPERBYTE == 64
INTERN struct struct_DCCTypeDecl t_int64_oldfun =
{ /* d_refcnt */0x80000000,
  /* d_next   */NULL,
  /* d_name   */&TPPKeyword_Empty,
  /* d_file   */&TPPFile_Empty,
  /* d_line   */0,
  /* d_kind   */DCC_DECLKIND_OLDFUNCTION,
  /* d_flag   */DCC_DECLFLAG_INTERN,
  /* d_depth  */0,
  /* d_type   */{DCCTYPE_IB8,NULL},
  /* d_attr   */NULL,
  /* d_tdecl  */{{0},NULL,{0},0}
};
#endif
INTERN struct struct_DCCTypeDecl t_void_oldfun =
{ /* d_refcnt */0x80000000,
  /* d_next   */NULL,
  /* d_name   */&TPPKeyword_Empty,
  /* d_file   */&TPPFile_Empty,
  /* d_line   */0,
  /* d_kind   */DCC_DECLKIND_OLDFUNCTION,
  /* d_flag   */DCC_DECLFLAG_INTERN,
  /* d_depth  */0,
  /* d_type   */{DCCTYPE_VOID,NULL},
  /* d_attr   */NULL,
  /* d_tdecl  */{{0},NULL,{0},0}
};

PUBLIC void
DCCType_MkOldFunc(struct DCCType *__restrict self) {
 struct DCCDecl *funbase;
 assert(self);
 assert(!t_builtin(self));
 DCCType_ASSERT(self);
 switch ((self->t_type&(DCCTYPE_BASICMASK|DCCTYPE_GROUPMASK|
                       (DCCTYPE_FLAGSMASK&~(DCCTYPE_STOREMASK))))) {
  if (DCC_MACRO_FALSE) { case DCCTYPE_INT: self->t_base = (struct DCCDecl *)&t_int_oldfun; }
  if (DCC_MACRO_FALSE) { case DCCTYPE_VOID: self->t_base = (struct DCCDecl *)&t_void_oldfun; }
#if DCC_TARGET_SIZEOF_POINTER*DCC_TARGET_BITPERBYTE == 64
  if (DCC_MACRO_FALSE) { case DCCTYPE_INT64: self->t_base = (struct DCCDecl *)&t_int64_oldfun; }
#endif
  DCCDecl_Incref(self->t_base);
  break;
 default:
  /* Fallback: Must allocate a new symbol. */
  funbase = DCCDecl_New(&TPPKeyword_Empty);
  if unlikely(!funbase) return;
  funbase->d_kind = DCC_DECLKIND_OLDFUNCTION;
  funbase->d_type.t_type = self->t_type&~(DCCTYPE_STOREMASK);
  funbase->d_type.t_base = self->t_base; /* Inherit reference. */
  assert(funbase->d_tdecl.td_size   == 0);
  assert(funbase->d_tdecl.td_fieldv == NULL);
  self->t_base  = funbase; /* Inherit reference. */
  break;
 }
 /* Update the type ID. */
 self->t_type &= DCCTYPE_STOREMASK;
 self->t_type |= DCCTYPE_FUNCTION;
 DCCType_ASSERT(self);
}

PUBLIC void
DCCType_ForceDynamic(struct DCCType *__restrict self) {
 struct DCCDecl *new_decl,*base_decl = self->t_base;
 if (!base_decl || !(base_decl->d_flag&DCC_DECLFLAG_INTERN)) return;
 new_decl = DCCDecl_New(&TPPKeyword_Empty);
 if likely(new_decl) {
  new_decl->d_type = base_decl->d_type;
  if (new_decl->d_type.t_base) DCCDecl_Incref(new_decl->d_type.t_base);
  new_decl->d_kind  = base_decl->d_kind;
  /* NOTE: No builtin types make use of type declarations.
   *       But if they did, we'd need to copy the field-vector here! */
  new_decl->d_tdecl = base_decl->d_tdecl;
 }
 DCCDecl_Decref(base_decl);
 self->t_base = new_decl; /* Inherit reference. */
}

PUBLIC int
DCCType_IsComplete(struct DCCType const *__restrict self) {
 int result = 1;
 assert(self);
 switch (DCCTYPE_GROUP(self->t_type)) {
 case DCCTYPE_STRUCTURE:
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  result = !(self->t_base->d_flag&DCC_DECLFLAG_FORWARD);
  break;
 case DCCTYPE_VARRAY:
 case DCCTYPE_FUNCTION:
  result = 0;
  break;
 default: break;
 }
 return result;
}

PUBLIC int
DCCType_IsScalar(struct DCCType const *__restrict self) {
 tyid_t tyid = self->t_type;
 switch (DCCTYPE_GROUP(tyid)) {
 case DCCTYPE_BUILTIN:
  tyid &= DCCTYPE_BASICMASK;
  return (tyid < DCCTYPE_FLOAT) || tyid == DCCTYPE_BOOL;
 case DCCTYPE_STRUCTURE:
  assert(self->t_base);
  return self->t_base->d_attr && !!(self->t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC);
 default: break;
 }
 return 0;
}

PUBLIC void
DCCType_FixComplete(struct DCCType *__restrict self) {
 assert(self);
 switch (DCCTYPE_GROUP(self->t_type)) {
 case DCCTYPE_STRUCTURE:
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  /* Convert forward-structures to lvalue-to-structure. */
  if (self->t_base->d_flag&DCC_DECLFLAG_FORWARD)
   DCCType_MkLValue(self);
  break;
 case DCCTYPE_VARRAY:
  /* Convert variadic array types to single-element array types. */
  DCCType_MkBase(self);
  DCCType_MkArray(self,1);
  break;
 case DCCTYPE_FUNCTION:
  /* Convert function types to pointer-to-function types. */
  DCCType_MkPointer(self);
  break;
 default: break;
 }
}

PUBLIC void
DCCType_FixRedeclaration(struct DCCType *__restrict self,
                         struct DCCType const *__restrict old_type) {
 assert(self);
 assert(old_type);
 assert(self != old_type);
 switch (DCCTYPE_GROUP(self->t_type)) {

 case DCCTYPE_BUILTIN:
  if (self->t_type == (DCCTYPE_AUTO|DCCTYPE_BUILTIN)) {
   /* Load any typing from 'old_type' into 'self' */
   assert(!self->t_base);
   DCCType_InitCopy(self,old_type);
  }
  break;

 case DCCTYPE_VARRAY:
  assert(self->t_base);
  assert(self->t_base->d_kind == DCC_DECLKIND_ARRAY);
  if (DCCTYPE_GROUP(old_type->t_type) != DCCTYPE_ARRAY) break;
  assert(old_type->t_base);
  assert(old_type->t_base->d_kind == DCC_DECLKIND_ARRAY);
  assert(self->t_base != old_type->t_base);
  /* Fix array base types. */
  DCCType_FixRedeclaration(&self->t_base->d_type,
                           &old_type->t_base->d_type);
  if unlikely(!DCCType_IsCompatible(&self->t_base->d_type,
                                    &old_type->t_base->d_type,
                                    0)
              ) return;
  /* Fix array size. */
#if 1
  self->t_type &= ~(DCCTYPE_GROUPMASK);
  self->t_type |=   DCCTYPE_ARRAY;
  self->t_base->d_tdecl.td_size = old_type->t_base->d_tdecl.td_size;
  DCCType_ASSERT(self);
#else
  DCCType_MkBase(self);
  DCCType_MkArray(self,old_type->t_base->d_tdecl.td_size);
#endif
  break;

 case DCCTYPE_LVALUE:
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  if (DCCTYPE_GROUP(old_type->t_type) != DCCTYPE_LVALUE) break;
  if (self->t_base == old_type->t_base) break; /* Prevent an assertion failure. */
  assert(old_type->t_base);
  assert(old_type->t_base->d_kind&DCC_DECLKIND_TYPE);
  /* Fix l-value base type. */
  DCCType_FixRedeclaration(&self->t_base->d_type,
                           &old_type->t_base->d_type);

  break;

 default: break;
 }
}



PUBLIC target_siz_t
DCCType_Sizeof(struct DCCType const *__restrict self,
               target_siz_t *align,
               int real_sizeof) {
 target_siz_t result;
 assert(self);
again:
 switch (DCCTYPE_GROUP(self->t_type)) {

 { /* Size of a function/variadic array type.
    * True variadic array size must be determined at runtime. */
 case DCCTYPE_VARRAY:
 case DCCTYPE_FUNCTION:
imcomplete:
  result = 0;
 } break;

 { /* Size of a structure type. */
 case DCCTYPE_STRUCTURE:
  assert(self->t_base);
  assert(self->t_base->d_kind == DCC_DECLKIND_STRUCT ||
         self->t_base->d_kind == DCC_DECLKIND_UNION);
  if (self->t_base->d_flag&DCC_DECLFLAG_FORWARD) goto imcomplete;
  if (align) *align = self->t_base->d_tdecl.td_struct_align;
  result = self->t_base->d_tdecl.td_struct_size;
  goto end;
 } break;

 { /* Size of any pointer-type. */
 case DCCTYPE_POINTER:
  result = DCC_TARGET_SIZEOF_POINTER;
 } break;

 { /* Array size (sizeof(base) * element-count) */
 case DCCTYPE_ARRAY:
  assert(self->t_base);
  assert(self->t_base->d_kind == DCC_DECLKIND_ARRAY ||
         self->t_base->d_kind == DCC_DECLKIND_VLA);
  result = DCCType_Sizeof(&self->t_base->d_type,align,real_sizeof);
  if (self->t_base->d_kind == DCC_DECLKIND_VLA) {
   result = DCC_TARGET_SIZEOF_POINTER;
  } else {
   result *= self->t_base->d_tdecl.td_size;
   if (!result && align) *align = 0; /* Fix alignment for empty array types. */
  }
  goto end;
 } break;

 { /* LValue sizeof (Either target pointer, or lvalue-base-size) */
 case DCCTYPE_LVALUE:
  if (!real_sizeof) {
   assert(self->t_base);
   assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
   self = &self->t_base->d_type;
   goto again;
  }
  result = DCC_TARGET_SIZEOF_POINTER;
 } break;

 { /* Size of a basic/builtin type. */
 default:
  switch (self->t_type&DCCTYPE_BASICMASK) {
  case DCCTYPE_INT : case DCCTYPE_UNSIGNED|DCCTYPE_INT : result = DCC_TARGET_SIZEOF_INT; break;
  case DCCTYPE_BYTE: case DCCTYPE_UNSIGNED|DCCTYPE_BYTE: result = DCC_TARGET_SIZEOF_BYTE; break;
  case DCCTYPE_WORD: case DCCTYPE_UNSIGNED|DCCTYPE_WORD: result = DCC_TARGET_SIZEOF_WORD; break;
  case DCCTYPE_IB8 : case DCCTYPE_UNSIGNED|DCCTYPE_IB8 :
   result = 8;
#if DCC_TARGET_SIZEOF_LONG_LONG == 8 && \
    DCC_TARGET_ALIGNOF_LONG_LONG != 8
   if (align) *align = DCC_TARGET_ALIGNOF_LONG_LONG;
   goto end;
#endif
   break;
  case DCCTYPE_FLOAT:
   result = DCC_TARGET_SIZEOF_FLOAT;
#if DCC_TARGET_ALIGNOF_FLOAT != DCC_TARGET_SIZEOF_FLOAT
   if (align) *align = DCC_TARGET_ALIGNOF_FLOAT;
   goto end;
#endif
   break;
  case DCCTYPE_DOUBLE:
   result = DCC_TARGET_SIZEOF_DOUBLE;
#if DCC_TARGET_ALIGNOF_DOUBLE != DCC_TARGET_SIZEOF_DOUBLE
   if (align) *align = DCC_TARGET_ALIGNOF_DOUBLE;
   goto end;
#endif
   break;
  case DCCTYPE_LDOUBLE:
   result = DCC_TARGET_SIZEOF_LONG_DOUBLE;
#if DCC_TARGET_ALIGNOF_LONG_DOUBLE != DCC_TARGET_SIZEOF_LONG_DOUBLE
   if (align) *align = DCC_TARGET_ALIGNOF_LONG_DOUBLE;
   goto end;
#endif
   break;
  case DCCTYPE_BOOL: result = DCC_TARGET_SIZEOF_BOOL; break;
  default: result = 0; break;
  }
 } break;
 }
 if (align) *align = result;
end:
 assertf(!align || (result ? *align != 0 : *align == 0),
         "Invalid alignment '%lu' for type size '%lu'",
        (unsigned long)*align,(unsigned long)result);
 assertf(!align || !((*align)&((*align)-1)),
         "Invalid alignment '%lu'",(unsigned long)*align);
 return result;
}


PUBLIC int
DCCType_IsCompatible(struct DCCType const *__restrict a,
                     struct DCCType const *__restrict b,
                     int unqualified) {
 int result = a == b;
 assert(a);
 assert(b);
 assert(!a->t_base || a->t_base->d_kind&DCC_DECLKIND_TYPE);
 assert(!b->t_base || b->t_base->d_kind&DCC_DECLKIND_TYPE);
 if (!result) {
  tyid_t ta,tb;
  /* Make sure that the alt-name masks of both types match. */
  if ((a->t_type&DCCTYPE_ALTMASK) !=
      (b->t_type&DCCTYPE_ALTMASK)) return 0;
  ta = a->t_type & ~(DCCTYPE_FLAGSMASK&~(DCCTYPE_QUAL|DCCTYPE_ALTMASK));
  tb = b->t_type & ~(DCCTYPE_FLAGSMASK&~(DCCTYPE_QUAL|DCCTYPE_ALTMASK));
  if (unqualified) {
   ta &= ~(DCCTYPE_QUAL);
   tb &= ~(DCCTYPE_QUAL);
  }
  result = ta == tb;
  if (result) {
   struct DCCDecl *sa = a->t_base,*sb = b->t_base;
   switch (DCCTYPE_GROUP(ta)) {

   { /* Compare array base and element count. */
   case DCCTYPE_ARRAY:
    assert(sa),assert(sa->d_kind&DCC_DECLKIND_TYPE);
    assert(sb),assert(sb->d_kind&DCC_DECLKIND_TYPE);
    result = sa->d_tdecl.td_size == sb->d_tdecl.td_size;
    if (!result) break;
   case DCCTYPE_POINTER:
   case DCCTYPE_VARRAY:
   case DCCTYPE_LVALUE:
    /* Compare pointer base types. */
    assert(sa),assert(sb);
    result = DCCType_IsCompatible(&sa->d_type,
                                  &sb->d_type,
                                  unqualified >= 2 ? 2 : 0);
   } break;

   { /* Compare function arguments / structure fields. */
    struct DCCStructField *lhs,*rhs,*end;
   case DCCTYPE_FUNCTION:
    assert(sa),assert(sb);
    result = DCCType_IsCompatible(&sa->d_type,
                                  &sb->d_type,
                                  unqualified >= 2 ? 2 : 0);
    if (!result) break;
    /* Old-stype functions are always cross-compatible. */
    if (sa->d_kind == DCC_DECLKIND_OLDFUNCTION ||
        sb->d_kind == DCC_DECLKIND_OLDFUNCTION) break;
    if (DCC_MACRO_FALSE) {
   case DCCTYPE_STRUCTURE:
     assert(sa),assert(sb);
     /* struct/union types are never cross-compatible. */
     result = (sa->d_kind == sb->d_kind) && HAS(EXT_STRUCT_COMPATIBLE);
     if (!result) break;
    }
    assert(sa->d_kind&DCC_DECLKIND_TYPE);
    assert(sb->d_kind&DCC_DECLKIND_TYPE);
    /* Check structure fields / argument names. */
    result = (sa->d_tdecl.td_size == sb->d_tdecl.td_size);
    if (!result) break;
    lhs = sa->d_tdecl.td_fieldv;
    rhs = sb->d_tdecl.td_fieldv;
    end = lhs+sa->d_tdecl.td_size;
    for (; lhs != end; ++lhs,++rhs) {
     assert(lhs->sf_decl);
     assert(rhs->sf_decl);
     result = (lhs->sf_off == rhs->sf_off) &&
               DCCType_IsCompatible(&lhs->sf_decl->d_type,
                                    &rhs->sf_decl->d_type,
                                    unqualified >= 2 ? 2 : 0);
     if (!result) break;
    }
   } break;

   default: break;
   }
  }
 }
 return result;
}

PUBLIC void
DCCType_CheckWritable(struct DCCType const *__restrict self) {
 struct DCCType const *self_iter = self;
 assert(self_iter);
 while (DCCTYPE_GROUP(self_iter->t_type) == DCCTYPE_LVALUE)
        assert(self_iter->t_base),self_iter = &self_iter->t_base->d_type;
 if (self_iter->t_type&DCCTYPE_POINTER) {
  DCCType_CheckWritablePtr(self);
 } else if (self_iter->t_type&DCCTYPE_CONST) {
  struct DCCType tycopy = *self_iter;
  tycopy.t_type &= ~(DCCTYPE_CONST);
  WARN(W_CAST_CONST_LVALUE,self,&tycopy);
 }
}
PUBLIC void
DCCType_CheckWritablePtr(struct DCCType const *__restrict self) {
 struct DCCType const *self_iter = self;
 assert(self_iter);
 while (DCCTYPE_GROUP(self_iter->t_type) == DCCTYPE_LVALUE)
        assert(self_iter->t_base),self_iter = &self_iter->t_base->d_type;
 if (self_iter->t_type&DCCTYPE_POINTER) {
  assert(self_iter->t_base),self_iter = &self_iter->t_base->d_type;
  if (self_iter->t_type&DCCTYPE_CONST) {
   struct DCCType tycopy;
   DCCType_InitCopy(&tycopy,self_iter);
   tycopy.t_type &= ~(DCCTYPE_CONST);
   DCCType_MkPointer(&tycopy);
   WARN(W_CAST_CONST_POINTER,self,&tycopy);
   DCCType_Quit(&tycopy);
  }
 }
}


#define WRITE(p,s) \
{ size_t const len = (s); \
  newpos = iter+len;\
  if (newpos < end) memcpy(iter,p,len*sizeof(char));\
  iter = newpos;\
}

PRIVATE size_t DCCType_DoToString(char *buf, size_t buflen, struct DCCType const *__restrict self, struct TPPKeyword const *name);
PRIVATE size_t DCCType_PutPrefix(char *buf, size_t buflen, struct DCCType const *__restrict self, int add_paren);
PRIVATE size_t DCCType_PutSuffix(char *buf, size_t buflen, struct DCCType const *__restrict self, int add_paren);



PRIVATE size_t
DCCType_PutPrefix(char *buf, size_t buflen,
                  struct DCCType const *__restrict self,
                  int add_paren) {
 char *iter,*end,*newpos;
 assert(self);
 end = (iter = buf)+buflen;
 switch (DCCTYPE_GROUP(self->t_type)) {

 { /* Array/Function types */
  struct DCCType type_base;
 case DCCTYPE_ARRAY:
 case DCCTYPE_VARRAY:
 case DCCTYPE_FUNCTION:
  type_base         = self->t_base->d_type;
  /* Share qualifiers with the base type! */
  type_base.t_type |= self->t_type&DCCTYPE_QUAL;
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  iter += DCCType_PutPrefix(buf,buflen,&type_base,0);
 } break;

 { /* Pointer/L-Value types. */
  char tychar;
  struct DCCType *base_type;
  struct DCCAttrDecl *attr;
  if (DCC_MACRO_FALSE) { case DCCTYPE_POINTER: tychar = '*'; }
  if (DCC_MACRO_FALSE) { case DCCTYPE_LVALUE:  tychar = '&'; }
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  base_type = &self->t_base->d_type;
  attr = self->t_base->d_attr;
  if (DCCTYPE_GROUP(base_type->t_type) == DCCTYPE_BUILTIN) {
   iter += DCCType_PutPrefix(buf,buflen,base_type,0);
   if (iter < end) *iter = ' ';
   ++iter;
  } else if (DCCTYPE_GROUP(base_type->t_type) != DCCTYPE_POINTER &&
             DCCTYPE_GROUP(base_type->t_type) != DCCTYPE_LVALUE) {
   iter += DCCType_PutPrefix(buf,buflen,base_type,1);
  } else {
   iter += DCCType_PutPrefix(buf,buflen,base_type,0);
  }
  if (attr) {
#ifdef DCC_ATTRFLAG_MASK_86SEG
   if (DCC_ATTRFLAG_HAS_86SEG(attr->a_flags)) {
    WRITE("__seg",5);
    WRITE(DCCAsmReg_86SegNames[DCC_ATTRFLAG_GET_86SEG(attr->a_flags)],2);
    WRITE(" ",1);
   }
#endif
   //WRITE("__attribute__((",15);
   /* TODO: Add support for other types. */
   //WRITE("))",2);
  }
  if (iter < end) *iter = tychar;
  ++iter;
  if (self->t_type&DCCTYPE_CONST) WRITE("const ",6);
  if (self->t_type&DCCTYPE_VOLATILE) WRITE("volatile ",9);
 } break;

 { /* structure type. */
  struct DCCDecl *struct_decl;
 case DCCTYPE_STRUCTURE:
  if (self->t_type&DCCTYPE_CONST) WRITE("const ",6);
  if (self->t_type&DCCTYPE_VOLATILE) WRITE("volatile ",9);
  if (self->t_type&DCCTYPE_UNSIGNED) WRITE("unsigned ",9);
  struct_decl = self->t_base;
  assert(struct_decl);
  assert(struct_decl->d_kind == DCC_DECLKIND_STRUCT ||
         struct_decl->d_kind == DCC_DECLKIND_UNION);
  assert(struct_decl->d_name);
  if (struct_decl->d_kind == DCC_DECLKIND_STRUCT) {
   WRITE("struct",6);
  } else {
   WRITE("union",5);
  }
  if (struct_decl->d_name != &TPPKeyword_Empty) {
   if (iter < end) *iter = ' ';
   ++iter;
   WRITE(struct_decl->d_name->k_name,
         struct_decl->d_name->k_size);
  }
  if (!(struct_decl->d_flag&DCC_DECLFLAG_FORWARD)) {
   /* XXX: full representation? */
   WRITE(" { ... }",8);
  }
 } break;

 { /* Basic type. */
  char const *basic_name;
 default:
  if (self->t_type&DCCTYPE_CONST) WRITE("const ",6);
  if (self->t_type&DCCTYPE_VOLATILE) WRITE("volatile ",9);
  if (self->t_type&DCCTYPE_ALTLONG) {
   assert(DCCTYPE_BASIC(self->t_type) == (DCCTYPE_LONG) ||
          DCCTYPE_BASIC(self->t_type) == (DCCTYPE_LONG|DCCTYPE_UNSIGNED));
   basic_name = (self->t_type&DCCTYPE_UNSIGNED) ? "unsigned long" : "long";
  } else switch (DCCTYPE_BASIC(self->t_type)) {
   default: basic_name = "int"; break;
   case DCCTYPE_UNSIGNED: basic_name = "unsigned int"; break;
   case DCCTYPE_UNSIGNED|DCCTYPE_BYTE: basic_name = "unsigned char"; break;
   case DCCTYPE_BYTE: basic_name = (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED) ? "signed char" : "char"; break;
   case DCCTYPE_UNSIGNED|DCCTYPE_WORD: basic_name = "unsigned short"; break;
   case DCCTYPE_WORD: basic_name = "short"; break;
#if DCC_TARGET_SIZEOF_LONG_LONG == 8
   case DCCTYPE_UNSIGNED|DCCTYPE_IB8: basic_name = "unsigned long long";  break;
   case DCCTYPE_IB8: basic_name = "long long"; break;
#elif DCC_TARGET_BITPERBYTE == 8
   case DCCTYPE_UNSIGNED|DCCTYPE_IB8: basic_name = "unsigned __int64";  break;
   case DCCTYPE_IB8: basic_name = "__int64"; break;
#else
   case DCCTYPE_UNSIGNED|DCCTYPE_IB8: basic_name = "unsigned <int-8-byte>";  break;
   case DCCTYPE_IB8: basic_name = "<int-8-byte>"; break;
#endif
   case DCCTYPE_FLOAT: basic_name = "float"; break;
   case DCCTYPE_DOUBLE: basic_name = "double"; break;
   case DCCTYPE_LDOUBLE: basic_name = "long double"; break;
   case DCCTYPE_BOOL: basic_name = "_Bool"; break;
   case DCCTYPE_AUTO: basic_name = "__auto_type"; break;
   case DCCTYPE_VOID: basic_name = "void"; break;
  }
  WRITE(basic_name,strlen(basic_name));
 } break;

 }
 if (add_paren) { if (iter < end) *iter = '('; ++iter; }
 return (size_t)(iter-buf);
}
PRIVATE size_t
DCCType_PutSuffix(char *buf, size_t buflen,
                  struct DCCType const *__restrict self,
                  int add_paren) {
 char *iter,*end,*newpos;
 assert(self);
 end = (iter = buf)+buflen;
 if (add_paren) { if (iter < end) *iter = ')'; ++iter; }
 switch (DCCTYPE_GROUP(self->t_type)) {

 { /* Array type. */
 case DCCTYPE_ARRAY:
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  if (self->t_base->d_kind == DCC_DECLKIND_VLA) {
   if (iter < end) { *iter = '['; } ++iter;
   if (iter < end) { *iter = '?'; } ++iter;
   if (iter < end) { *iter = ']'; } ++iter;
  } else {
   iter += snprintf(iter,iter > end ? 0 : (size_t)(end-iter),"[%lu]",
                   (unsigned long)self->t_base->d_tdecl.td_size);
  }
  iter += DCCType_PutSuffix(iter,iter > end ? 0 :
                           (size_t)(end-iter),
                           &self->t_base->d_type,0);
 } break;

 { /* Variadic array type. */
 case DCCTYPE_VARRAY:
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  if (iter < end) *iter = '[';
  ++iter;
  if (iter < end) *iter = ']';
  ++iter;
  iter += DCCType_PutSuffix(iter,iter > end ? 0 :
                           (size_t)(end-iter),
                           &self->t_base->d_type,0);
 } break;

 { /* Function representation. */
 case DCCTYPE_FUNCTION:
  assert(self->t_base);
  assert(self->t_base->d_kind == DCC_DECLKIND_FUNCTION ||
         self->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION);
  if (iter < end) *iter = '(';
  ++iter;
  if (self->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION) {
  } else {
   struct DCCStructField *fiter,*fend;
   fend = (fiter = self->t_base->d_tdecl.td_fieldv)+
                   self->t_base->d_tdecl.td_size;
   if (fiter == fend) {
    if (self->t_base->d_flag&DCC_DECLFLAG_VARIADIC)
    { WRITE("...",3); } else { WRITE("void",4); }
   } else {
    for (; fiter != fend; ++fiter) {
     assert(fiter->sf_decl);
     iter += DCCType_DoToString(iter,iter > end ? 0 : (size_t)(end-iter),
                                &fiter->sf_decl->d_type,fiter->sf_decl->d_name);
     if (fiter != fend-1) {
      if (iter < end) *iter = ',';
      ++iter;
      if (iter < end) *iter = ' ';
      ++iter;
     }
    }
    /* Write the var-args suffix. */
    if (self->t_base->d_flag&DCC_DECLFLAG_VARIADIC) {
     WRITE(", ...",5);
    }
   }
  }
  if (iter < end) *iter = ')';
  ++iter;
 } break;

 { /* Pointer/L-Value type. */
  case DCCTYPE_POINTER:
  case DCCTYPE_LVALUE:
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  self = &self->t_base->d_type;
  if (DCCTYPE_GROUP(self->t_type) != DCCTYPE_BUILTIN &&
      DCCTYPE_GROUP(self->t_type) != DCCTYPE_POINTER &&
      DCCTYPE_GROUP(self->t_type) != DCCTYPE_LVALUE) {
   iter += DCCType_PutSuffix(buf,buflen,self,1);
  } else {
   iter += DCCType_PutSuffix(buf,buflen,self,0);
  }
 } break;

 default: break;
 }
 return (size_t)(iter-buf);
}

PUBLIC size_t
DCCType_DoToString(char *buf, size_t buflen,
                   struct DCCType const *__restrict self,
                   struct TPPKeyword const *name) {
 char *iter,*end,*newpos;
 assert(self);
 end = (iter = buf)+buflen;
 switch (self->t_type&DCCTYPE_STOREBASE) {
 { char const *cls_name;
   if (DCC_MACRO_FALSE) { case DCCTYPE_STATIC  : cls_name = "static "; }
   if (DCC_MACRO_FALSE) { case DCCTYPE_EXTERN  : cls_name = "extern "; }
   if (DCC_MACRO_FALSE) { case DCCTYPE_REGISTER: cls_name = "register "; }
   if (DCC_MACRO_FALSE) { case DCCTYPE_TYPEDEF : cls_name = "typedef "; }
   WRITE(cls_name,strlen(cls_name));
 } break;
 default: break;
 }
 if (self->t_type&DCCTYPE_ATOMIC) { WRITE("atomic ",7); }
 if (self->t_type&DCCTYPE_INLINE) { WRITE("inline ",7); }
#ifdef DCCTYPE_TLS
 if (self->t_type&DCCTYPE_TLS) { WRITE("thread_local ",13); }
#endif
 iter += DCCType_PutPrefix(iter,buflen,self,0);
 if (name && name->k_size) {
  struct DCCType const *prefix_type = self;
  while (DCCTYPE_ISARRAY(prefix_type->t_type))
         assert(prefix_type->t_base),
         prefix_type = &prefix_type->t_base->d_type;
  if (DCCTYPE_GROUP(prefix_type->t_type) != DCCTYPE_POINTER &&
      DCCTYPE_GROUP(prefix_type->t_type) != DCCTYPE_LVALUE) {
   /* Insert a space character. */
   if (iter < end) *iter = ' ';
   ++iter;
  }
  WRITE(name->k_name,name->k_size);
 }
 iter += DCCType_PutSuffix(iter,iter > end ? 0 : (size_t)(end-iter),self,0);
 return (size_t)(iter-buf);
#undef WRITE
}

PUBLIC size_t
DCCType_ToString(char *buf, size_t buflen,
                 struct DCCType const *__restrict self,
                 struct TPPKeyword const *name) {
 char *iter = buf,*end = buf+buflen;
 iter += DCCType_DoToString(buf,buflen,self,name);
 if (iter < end) *iter = '\0';
 ++iter;
 return (size_t)(iter-buf);
}

PUBLIC /*ref*/struct TPPString *
DCCType_ToTPPString(struct DCCType const *__restrict self,
                    struct TPPKeyword const *name) {
 struct TPPString *result,*newresult;
 size_t reqsize,bufsize = 256;
 result = TPPString_NewSized(bufsize);
 if likely(result) for (;;) {
  reqsize = DCCType_ToString(result->s_text,++bufsize,self,name);
  if (reqsize != bufsize) {
   newresult = (struct TPPString *)realloc(result,
                                           DCC_COMPILER_OFFSETOF(struct TPPString,s_text)+
                                           reqsize*sizeof(char));
   if (newresult) result = newresult;
   else if (reqsize > bufsize) {
    TPPString_Decref(result);
    break;
   }
  }
  if (reqsize <= bufsize) {
   if (reqsize != bufsize) {
    newresult = (struct TPPString *)realloc(result,
                                            DCC_COMPILER_OFFSETOF(struct TPPString,s_text)+
                                            reqsize*sizeof(char));
    if (newresult) result = newresult;
   }
   result->s_size = reqsize-1;
   return result;
  }
 }
 return TPPString_NewEmpty();
}


DCC_DECL_END

#endif /* !GUARD_DCC_TPP_TYPE_C */
