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
#ifndef GUARD_DCC_LEXER_CTYPE_C_INL
#define GUARD_DCC_LEXER_CTYPE_C_INL 1

#include <dcc/lexer.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#include "lexer-priv.h"

#include <string.h>

DCC_DECL_BEGIN

PUBLIC struct TPPKeyword *DCC_PARSE_CALL
DCCParse_CTypeOnly(struct DCCType *__restrict self,
                   struct DCCAttrDecl *__restrict attr) {
 struct TPPKeyword *result;
 assert(self);
 assert(attr);
 /* NOTE: If 'self' is an LVA type, code may still be generated
 *        by this unless the caller set the NOCGEN flag. */
 result = DCCParse_CType(self,attr);
 if (!result) {
  pushf();
  /* Make sure no code is generated for the discarded expression. */
  compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
  DCCParse_Expr1();
  DCCType_InitCopy(self,&vbottom->sv_ctype);
  vpop(1);
  popf();
  result = &TPPKeyword_Empty;
 }
 return result;
}

PUBLIC struct TPPKeyword *DCC_PARSE_CALL
DCCParse_CType(struct DCCType *__restrict self,
               struct DCCAttrDecl *__restrict attr) {
 struct TPPKeyword *result = NULL;
 if (DCCParse_CTypePrefix(self,attr)) {
  result = DCCParse_CTypeSuffix(self,attr);
 }
 return result;
}


LEXPRIV void DCC_PARSE_CALL
DCCType_PromoteFunArg(struct DCCType *__restrict self) {
 assert(self);
 switch (DCCTYPE_GROUP(self->t_type)) {
 case DCCTYPE_ARRAY:
 case DCCTYPE_VARRAY:
  /* Convert arrays to pointers in arguments. */
  DCCType_MkBase(self);
 case DCCTYPE_FUNCTION:
  DCCType_MkPointer(self);
  break;
 case DCCTYPE_AUTO:
  /* TODO: Warn about auto used as argument type. */
  self->t_type = DCCTYPE_INT;
  break;
 default: break;
 }
}


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif

LEXPRIV void DCC_PARSE_CALL
DCCParse_OldFunctionArgWithBase(struct DCCDecl *__restrict fundecl,
                                struct DCCType     const *__restrict arg_basetype,
                                struct DCCAttrDecl const *__restrict arg_baseattr) {
 struct DCCType     arg_type;
 struct DCCAttrDecl arg_attr;
 struct TPPKeyword *arg_name;
 assert(fundecl);
 assert(fundecl->d_kind == DCC_DECLKIND_FUNCTION);
 DCCType_InitCopy(&arg_type,arg_basetype);
 DCCAttrDecl_InitCopy(&arg_attr,arg_baseattr);
 arg_name = DCCParse_CTypeSuffix(&arg_type,&arg_attr);
 assert(arg_name);
 if (arg_name != &TPPKeyword_Empty) {
  struct DCCStructField *iter,*end;
  end = (iter = fundecl->d_tdecl.td_fieldv)+
                fundecl->d_tdecl.td_size;
  for (; iter != end; ++iter) {
   assert(iter->sf_decl);
   if (iter->sf_decl->d_name == arg_name) {
    if (iter->sf_decl->d_type.t_type == DCCTYPE_INT) {
     target_ptr_t new_offset;
     target_off_t size_diff;
     target_siz_t s,a;
     s = DCCType_Sizeof(&arg_type,&a,1);
     assert(!iter->sf_decl->d_type.t_base);
     iter->sf_decl->d_type = arg_type; /* Inherit object. */
     arg_type.t_base       = NULL;
     new_offset   = (iter->sf_off+(a-1)) & ~(a-1);
     size_diff    = (s-DCC_TARGET_SIZEOF_INT)+(new_offset-iter->sf_off);
     iter->sf_off = new_offset;
     if (size_diff) {
      /* Must update the offsets of all following arguments. */
      while (++iter != end) iter->sf_off += size_diff;
     }
    } else {
     /* Warning: Argument already defined. */
     WARN(W_DECL_ALREADY_DEFINED,iter->sf_decl);
    }
    goto done;
   }
  }
  /* Warning: Unknown argument. */
  WARN(W_UNKNOWN_FUNCTION_ARGUMENT,arg_name);
 }
done:
 DCCAttrDecl_Quit(&arg_attr);
 DCCType_Quit(&arg_type);
}

LEXPRIV int DCC_PARSE_CALL
DCCParse_OldFunctionArg(struct DCCDecl *__restrict fundecl) {
 struct DCCType     arg_type;
 struct DCCAttrDecl arg_attr = DCCATTRDECL_INIT;
 int result;
 assert(fundecl);
 result = DCCParse_CTypePrefix(&arg_type,&arg_attr);
 if (!result) goto end;
 if (fundecl->d_kind == DCC_DECLKIND_OLDFUNCTION)
     fundecl->d_kind =  DCC_DECLKIND_FUNCTION;
 assert(fundecl->d_kind == DCC_DECLKIND_FUNCTION);
 while (TOK != ';') {
  DCCParse_OldFunctionArgWithBase(fundecl,&arg_type,&arg_attr);
  if (TOK != ',') break;
  YIELD();
 }
 if (TOK != ';') WARN(W_EXPECTED_SEMICOLON); else YIELD();
 DCCType_Quit(&arg_type);
end:
 DCCAttrDecl_Quit(&arg_attr);
 return result;
}
LEXPRIV void DCC_PARSE_CALL
DCCParse_OldFunctionArgs(struct DCCDecl *__restrict fundecl) {
 while (DCCParse_OldFunctionArg(fundecl));
}


LEXPRIV int DCC_PARSE_CALL
DCCParse_TryFunctionPrototype(struct DCCDecl *__restrict fundecl, int try_parse) {
 struct DCCStructField *argv,*new_argv;
 size_t argc,arga; struct DCCDecl *argdecl;
 target_off_t current_offset;
 assert(fundecl);
 assert(fundecl->d_kind == DCC_DECLKIND_FUNCTION);
 assert(!fundecl->d_tdecl.td_size);
 assert(!fundecl->d_tdecl.td_fieldv);
 argc = 0,arga = 2,argdecl = NULL;
 argv = (struct DCCStructField *)malloc(arga*sizeof(struct DCCStructField));
 if unlikely(!argv) goto seterr;
 current_offset = 0;
 while (TOK > 0) {
  if (TOK == TOK_DOTS) {
   /* varargs-function. */
   fundecl->d_flag |= DCC_DECLFLAG_VARIADIC;
   YIELD();
   break;
  }
  {
   struct DCCStructField *arg;
   struct DCCAttrDecl attr = DCCATTRDECL_INIT;
   struct TPPKeyword *arg_name;
   target_ptr_t s,a;
   if (!argdecl && unlikely((argdecl = DCCDecl_New(&TPPKeyword_Empty)) == NULL)) goto seterr;
   arg_name = DCCParse_CType(&argdecl->d_type,&attr);
   if unlikely(!arg_name) {
    if (try_parse && !argc) {
     DCCDecl_Decref(argdecl);
     DCCAttrDecl_Quit(&attr);
     free(argv);
     return 0;
    }
    WARN(W_EXPECTED_TYPE_FOR_PROTOTYPE_ARGUMENT);
    if (!TPP_ISKEYWORD(TOK)) goto next;
    /* If we've at least got a keyword,
     * let's guess this is an unnamed type?
     * >> The type is already default-initialized to 'int'. */
    arg_name = TOKEN.t_kwd;
    YIELD();
   } else if (TOK != ',' && arg_name == &TPPKeyword_Empty &&
              DCCTYPE_ISBASIC(argdecl->d_type.t_type,DCCTYPE_VOID) && !argc) {
    /* First (and only) argument is unnamed void --> empty parameter list. */
    DCCAttrDecl_Quit(&attr);
    break;
   }
   if (argc == arga) {
    new_argv = (struct DCCStructField *)realloc(argv,(arga*2)*
                                                sizeof(struct DCCStructField));
    if unlikely(!new_argv) goto seterr;
    argv = new_argv,arga *= 2;
   }
   DCCType_PromoteFunArg(&argdecl->d_type);
   s = DCCType_Sizeof(&argdecl->d_type,&a,1);
   /* Require at least integer alignment. */
   if (a < DCC_TARGET_SIZEOF_INT) a = DCC_TARGET_SIZEOF_INT;
   /* Respect explicit alignment through attributes. */
   if (attr.a_flags&DCC_ATTRFLAG_PACKED) {
    a = (attr.a_flags&DCC_ATTRFLAG_FIXEDALIGN) ? attr.a_align : 1;
   } else if (attr.a_flags&DCC_ATTRFLAG_FIXEDALIGN && attr.a_align > a) {
    a = attr.a_align;
   }
   current_offset = (current_offset+(a-1)) & ~(a-1);
   argdecl->d_kind = DCC_DECLKIND_TYPE;
   argdecl->d_name = arg_name;
   DCCDecl_SetAttr(argdecl,&attr);
   arg = &argv[argc++];
   arg->sf_off = current_offset;
   arg->sf_decl = argdecl;
   argdecl = NULL;
   current_offset += s;
next:
   DCCAttrDecl_Quit(&attr);
   if (TOK != ',') break;
   YIELD();
  }
 }
 if (argdecl) DCCDecl_Decref(argdecl);
 if (argc != arga) {
  if (!argc) free(argv),argv = NULL;
  else {
   new_argv = (struct DCCStructField *)realloc(argv,argc*sizeof(struct DCCStructField));
   if likely(new_argv) argv = new_argv;
  }
 }
 assert((argc != 0) == (argv != NULL));
 fundecl->d_tdecl.td_size   = argc;
 fundecl->d_tdecl.td_fieldv = argv;
 return 1;
seterr:
 if (argdecl) DCCDecl_Decref(argdecl);
 new_argv = argv+argc;
 while (new_argv-- != argv) {
  assert(new_argv->sf_decl);
  DCCDecl_Decref(new_argv->sf_decl);
 }
 TPPLexer_SetErr();
 return 1;
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeArrayExt(struct DCCType *__restrict self,
                       struct DCCAttrDecl *__restrict attr) {
 int_t array_size;
 assert(self),assert(attr);
 if (TOK == ']') {
  DCCType_MkVArray(self);
  YIELD();
  return;
 }
 pushf();
 /* Don't generate code for the array size
  * expression while inside the global scope. */
 if (!compiler.c_fun ||
     !HAS(EXT_VARIABLE_LENGTH_ARRAYS)
     ) compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 /* TODO: Reclaim temporary storage used by this expression. */
 DCCParse_Expr();
 popf();
 if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
 if (visconst_xval()) {
  array_size = vgtconst_int();
compiletime_array:
  if (vbottom->sv_sym) WARN(W_ARRAY_SIZE_DEPENDS_ON_SYMBOL,vbottom->sv_sym->sy_name);
  if (array_size < 0) { WARN(W_ARRAY_SIZE_NEGATIVE,self); array_size = 0; }
  DCCType_MkArray(self,(target_ptr_t)array_size);
  if (!array_size) WARN(W_ARRAY_SIZE_ZERO,self);
 } else if (!compiler.c_fun ||
            !HAS(EXT_VARIABLE_LENGTH_ARRAYS)) {
  WARN(W_VARIABLE_LENGTH_ARRAYS_NOT_ALLOWED_HERE);
  array_size = 1;
  goto compiletime_array;
 } else {
  struct DCCStackValue runtime_sizeof;
  target_off_t val_size_offset;
  /* Allocate stack memory for the vla size. */
  val_size_offset = DCCCompiler_HWStackAlloc(DCC_TARGET_SIZEOF_SIZE_T,
                                         DCC_TARGET_SIZEOF_SIZE_T,0);
  /* Make sure to work with a copy of the runtime element-count. */
  vrcopy();
  vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
  /* VLA arrays: Figure out how big this should be! */
  DCCVStack_PushSizeof(self);
  /* Multiply the the runtime element count. */
  vswap(),vgen2('*');
  /* Load the known-at-runtime sizeof() the VLA array (in bytes). */
  runtime_sizeof.sv_ctype.t_type = DCCTYPE_SIZE|DCCTYPE_UNSIGNED;
  runtime_sizeof.sv_ctype.t_base = NULL;
  runtime_sizeof.sv_const.it     = 0;
  runtime_sizeof.sv_const.offset = val_size_offset;
  runtime_sizeof.sv_flags        = DCC_SFLAG_LVALUE;
  runtime_sizeof.sv_sym          = NULL;
  runtime_sizeof.sv_reg          = DCC_RR_XBP;
  runtime_sizeof.sv_reg2         = DCC_RC_CONST;
  vpush(&runtime_sizeof);
  vswap();
  vstore(1); /* Store the VLA size in the hidden local. */
  /* Create the VLA type. */
  DCCType_MkVLA(self,val_size_offset,compiler.c_scope.s_id);
 }
 vpop(1);
}

LEXPRIV int DCC_PARSE_CALL
DCCParse_CTypeTryParenthesis(struct DCCType *__restrict self,
                             struct DCCAttrDecl *__restrict attr) {
 /* TODO: Old-style functions listed the parameter names here!
  *    >> The following declarations were only used to describe
  *       their typing that, when omitted, defaults to 'int'! */
 /* TODO: Ambiguity:
  *                                                  v - HERE (Is it the declaration name, or an old-style argument)
  * Named new-style function 'a': >> auto f = ({ int(a)(int x, int y) { return a+b; } });
  * Unnamed old-style function:   >> auto f = ({ int(a,b) int a,b; { return a+b; } });
  * (Possible) solution: Old-style function types with non-empty parameter list must be named.
  *                   >> What did K&R say about unnamed symbols? Did they even exist?
  */
 if (TOK == ')') {
  WARN(W_OLD_STYLE_FUNCTION_DECLARATION);
  YIELD();
  DCCType_MkOldFunc(self);
  if unlikely(!self->t_base) return 0;
  DCCParse_Attr(attr);
  DCCDecl_SetAttr(self->t_base,attr);
 } else {
  struct DCCDecl *fun_decl = DCCDecl_New(&TPPKeyword_Empty);
  if unlikely(!fun_decl) return 0;
  if (!DCCType_IsComplete(self)) WARN(W_EXPECTED_COMPLETE_TYPE_FOR_FUNCTION_BASE,self);
  fun_decl->d_type = *self;
  DCCDecl_XIncref(self->t_base);
  fun_decl->d_type.t_type &= ~(DCCTYPE_STOREMASK);
  fun_decl->d_kind = DCC_DECLKIND_FUNCTION;
  /* Parse a function prototype. */
  if (!DCCParse_TryFunctionPrototype(fun_decl,1)) {
   DCCDecl_Decref(fun_decl);
   return 0;
  } else {
   DCCDecl_XDecref(self->t_base);
   self->t_type &= (DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK));
   self->t_type |= (DCCTYPE_FUNCTION);
   self->t_base  = fun_decl; /* Inherit reference. */
  }
 }
 assert(self->t_base);
 DCCParse_OldFunctionArgs(self->t_base);
 return 1;
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeSuffix2(struct DCCType *__restrict self,
                      struct DCCAttrDecl *__restrict attr) {
 if (TOK == '(') {
  struct DCCDecl *fun_decl = DCCDecl_New(&TPPKeyword_Empty);
  if unlikely(!fun_decl) return;
  if (!DCCType_IsComplete(self)) WARN(W_EXPECTED_COMPLETE_TYPE_FOR_FUNCTION_BASE,self);
  fun_decl->d_type = *self; /* Inherit data. */
  fun_decl->d_type.t_type &= ~(DCCTYPE_STOREMASK);
  self->t_type &= (DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK));
  self->t_type |= (DCCTYPE_FUNCTION);
  self->t_base  = fun_decl; /* Inherit reference. */
  YIELD();
  if (TOK == ')') {
   WARN(W_OLD_STYLE_FUNCTION_DECLARATION);
   YIELD();
   fun_decl->d_kind = DCC_DECLKIND_OLDFUNCTION;
   assert(fun_decl->d_tdecl.td_size   == 0);
   assert(fun_decl->d_tdecl.td_fieldv == NULL);
  } else {
   fun_decl->d_kind = DCC_DECLKIND_FUNCTION;
   /* Parse a function prototype. */
   DCCParse_TryFunctionPrototype(fun_decl,0);
   if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
  }
  DCCParse_Attr(attr);
  DCCParse_OldFunctionArgs(fun_decl);
  DCCDecl_SetAttr(fun_decl,attr);
 } else if (TOK == '[') {
  YIELD(); /* Array/VArray declaration. */
  DCCParse_CTypeArrayExt(self,attr);
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  /* Continue working on the base type, thus ensuring correct type order. */
  self = &self->t_base->d_type;
  DCCParse_Attr(attr);
  DCCParse_CTypeSuffix2(self,attr);
  if (!DCCType_IsComplete(self)) {
   WARN(W_EXPECTED_COMPLETE_TYPE_FOR_ARRAY_BASE,self);
   /* Try to fix incomplete types. */
   DCCType_FixComplete(self);
  }
 }
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

PUBLIC struct TPPKeyword *DCC_PARSE_CALL
DCCParse_CTypeSuffix(struct DCCType *__restrict self,
                     struct DCCAttrDecl *__restrict attr) {
 struct DCCType ty2;
 struct TPPKeyword *result;
 int has_ty2 = 0;
 assert(self);
 assert(attr);
next_prefix:
 DCCType_ASSERT(self);
 DCCParse_Attr(attr);
 switch (TOK) {

 { /* Parse const/volatile qualifiers. */
  tyid_t qual;
  if (DCC_MACRO_FALSE) { case KWD_const:    case KWD___const:    case KWD___const__:    qual = DCCTYPE_CONST; }
  if (DCC_MACRO_FALSE) { case KWD_volatile: case KWD___volatile: case KWD___volatile__: qual = DCCTYPE_VOLATILE; }
  if (DCCTYPE_GROUP(self->t_type) == DCCTYPE_LVALUE) WARN(W_QUAL_ON_LVALUE);
  else if (DCCTYPE_ISBASIC(self->t_type,DCCTYPE_AUTO)) WARN(W_QUAL_ON_AUTO_TYPE);
  else { if (self->t_type&qual) WARN(W_QUALIFIER_ALREADY_IN_USE); self->t_type |= qual; }
  goto yield_next_prefix;
 } break;

 case '&':
 case '*':
  if (DCCTYPE_GROUP(self->t_type) == DCCTYPE_LVALUE)
   WARN(TOK == '&' ? W_ALREADY_AN_LVALUE : W_LVALUE_POINTER);
  /* Disable typing for special l-value conditions.
   * NOTE: Internally, an l-value of an l-value does has a meaning,
   *       in that it describes the result of killing a stack-value
   *       that was already stored at an l-value-style memory location.
   *       But don't think about it too hard. - It gets complicated
   *       ~real~ quick, and isn't something that should really happen.
   * >> For those reasons, better not confuse the user and simply
   *    not allow multi-layer l-values, or pointer-to-lvalues. */
#if 1
  else
#endif
  if (TOK == '*') {
   if (DCCTYPE_ISBASIC(self->t_type,DCCTYPE_AUTO)) {
    WARN(W_AUTO_TYPE_USED_AS_POINTER_BASE);
    self->t_type = DCCTYPE_INT;
   }
   DCCType_MkPointer(self);
  } else {
   DCCType_MkLValue(self);
  }
  goto yield_next_prefix;

 case KWD_restrict:
 case KWD___restrict:
 case KWD___restrict__:
  if (DCCTYPE_GROUP(self->t_type) != DCCTYPE_POINTER)
      WARN(W_RESTRICT_EXPECTS_POINTER);
yield_next_prefix:
  YIELD();
  goto next_prefix;

 default: break;
 }
 if (TOK == '(') {
  YIELD();
  /* Allow for a function parameter list here. */
  if (DCCParse_CTypeTryParenthesis(self,attr)) {
   goto unnamed_type;
  } else {
   struct DCCAttrDecl attr2 = DCCATTRDECL_INIT;
   DCCParse_Attr(attr);
   has_ty2 = 1;
   ty2.t_type = 0;
   ty2.t_base = NULL;
   result = DCCParse_CTypeSuffix(&ty2,&attr2);
   DCCAttrDecl_Quit(&attr2);
  }
  if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
 } else if (TPP_ISKEYWORD(TOK)) {
  result = TOKEN.t_kwd;
  YIELD();
  DCCParse_Attr(attr);
 } else {
unnamed_type:
  result = &TPPKeyword_Empty;
 }
 DCCParse_CTypeSuffix2(self,attr);
 if (has_ty2) {
  struct DCCType *iter;
  /* Must override 'self' as lowest-possible base
   * of 'ty2', the fill '*self' with 'ty2' */
  DCCType_ForceDynamic(&ty2);
  iter = &ty2;
  while (iter->t_base) {
   assert(iter->t_base->d_kind&DCC_DECLKIND_TYPE);
   iter = &iter->t_base->d_type;
  }
  assertf(iter->t_type == 0,"As set by 'ty2.t_type = 0' above!");
  *iter = *self; /* Inherit reference. */
  ty2.t_type |= (self->t_type&(DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK)));
  *self = ty2;   /* Inherit reference. */
 }
 /* Apply mode attributes. */
 if (attr->a_flags&DCC_ATTRFLAG_MASK_MODE) {
  tyid_t modeid;
  switch (attr->a_flags&DCC_ATTRFLAG_MASK_MODE) {
  {
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_QI: modeid = DCCTYPE_INTN(1); }
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_HI: modeid = DCCTYPE_INTN(2); }
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_SI: modeid = DCCTYPE_INTN(4); }
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_DI: modeid = DCCTYPE_INTN(8); }
#if DCC_TARGET_SIZEOF_FLOAT == 4
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_SF: modeid = DCCTYPE_FLOAT; }
#elif DCC_TARGET_SIZEOF_DOUBLE == 4
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_SF: modeid = DCCTYPE_DOUBLE; }
#else
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_SF: modeid = DCCTYPE_LDOUBLE; }
#endif
#if DCC_TARGET_SIZEOF_FLOAT == 8
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_DF: modeid = DCCTYPE_FLOAT; }
#elif DCC_TARGET_SIZEOF_DOUBLE == 8
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_DF: modeid = DCCTYPE_DOUBLE; }
#else
   if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_DF: modeid = DCCTYPE_LDOUBLE; }
#endif
   if (DCCTYPE_GROUP(self->t_type) != DCCTYPE_BUILTIN) {
    WARN(W_ATTRIBUTE_MODE_EXPECTS_BASIC_TYPE);
   } else {
    self->t_type &= ~(DCCTYPE_BASICMASK&~(DCCTYPE_UNSIGNED));
    if (modeid >= 8) self->t_type &= ~(DCCTYPE_UNSIGNED);
    self->t_type |= modeid;
   }
   attr->a_flags &= ~(DCC_ATTRFLAG_MASK_MODE);
  } break;
  default: break;
  }
 }
 return result;
}


PUBLIC int DCC_PARSE_CALL
DCCParse_CTypePrefix(struct DCCType *__restrict self,
                     struct DCCAttrDecl *__restrict attr) {
 int flags; struct DCCDecl *decl;
#define FLAG_FOUND_SOMETHING 0x01
#define FLAG_FOUND_INT       0x02
#define FLAG_FOUND_SIGN      0x04
#define FLAG_FOUND_LENGTH    0x08
#define FLAG_FOUND_AUTO      0x10 /* Now where are the keys... */
#define FLAG_FOUND_STORAGE   0x20
 assert(self);
 assert(attr);
 /* Initialized to int. */
 self->t_type = DCCTYPE_INT;
 self->t_base = NULL;
 flags = 0;
 goto begin;
next: YIELD();
next_noyield:
 flags |= FLAG_FOUND_SOMETHING;
 /* Make sure that '_Atomic' is only used with integral types. */
 if ((self->t_type&DCCTYPE_ATOMIC) &&
     (DCCTYPE_GROUP(self->t_type) != DCCTYPE_POINTER) &&
     (DCCTYPE_GROUP(self->t_type) != DCCTYPE_BUILTIN ||
      DCCTYPE_ISFLOAT(self->t_type))) {
  WARN(W_TYPE_MODIFIER_ATOMIC_REQUIRES_INTEGRAL);
  self->t_type &= ~(DCCTYPE_ATOMIC);
 }
begin: DCCParse_Attr(attr);
 switch (TOK) {

#if DCC_TARGET_OS == DCC_OS_WINDOWS
 case KWD___w64: goto next;
#endif

 { /* Parse const/volatile qualifiers. */
  tyid_t qual;
  if (DCC_MACRO_FALSE) { case KWD_const:    case KWD___const:    case KWD___const__:    qual = DCCTYPE_CONST; }
  if (DCC_MACRO_FALSE) { case KWD_volatile: case KWD___volatile: case KWD___volatile__: qual = DCCTYPE_VOLATILE; }
  if (DCCTYPE_ISBASIC(self->t_type,DCCTYPE_AUTO)) WARN(W_QUAL_ON_AUTO_TYPE);
  else { if (self->t_type&qual) WARN(W_QUALIFIER_ALREADY_IN_USE); self->t_type |= qual; }
  goto next;
 } break;

 {
  tyid_t length;
 case KWD_long:
  self->t_type |= DCCTYPE_ALTLONG;
  if (flags&FLAG_FOUND_LENGTH) {
   if ((self->t_type&(DCCTYPE_BASICMASK&~(DCCTYPE_UNSIGNED))) == DCCTYPE_LONG) {
    /* 'long long' / 'unsigned long long' */
    self->t_type &= (DCCTYPE_FLAGSMASK|DCCTYPE_UNSIGNED)&~(DCCTYPE_ALTMASK);
    self->t_type |=  DCCTYPE_LLONG;
    goto next;
   } else if ((self->t_type&DCCTYPE_BASICMASK) == DCCTYPE_DOUBLE) {
    /* long double. */
    self->t_type &= ~(DCCTYPE_ALTMASK);
    self->t_type |=   DCCTYPE_LDOUBLE;
    goto next;
   }
  }
  length = DCCTYPE_LONG;
  /* fallthrough */
  if (DCC_MACRO_FALSE) { case KWD_short: length = DCCTYPE_SHORT; }
  if (DCC_MACRO_FALSE) {
    if (DCC_MACRO_FALSE) { case KWD___int8:  length = DCCTYPE_INT8; }
    if (DCC_MACRO_FALSE) { case KWD___int16: length = DCCTYPE_INT16; }
    if (DCC_MACRO_FALSE) { case KWD___int32: length = DCCTYPE_INT32; }
    if (DCC_MACRO_FALSE) { case KWD___int64: length = DCCTYPE_INT64; }
    if (!HAS(EXT_FIXED_LENGTH_INTEGER_TYPES)) break;
  }
  if (DCC_MACRO_FALSE) {
 case KWD_char:
   length = DCCTYPE_CHAR;
   if (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED &&
      !(flags&FLAG_FOUND_SIGN)) self->t_type |= DCCTYPE_UNSIGNED;
  }
  if (flags&FLAG_FOUND_LENGTH) WARN(W_QUALIFIER_ALREADY_IN_USE);
  flags |= FLAG_FOUND_LENGTH;
  self->t_type |= length;
  goto next;
 } break;

 {
  tyid_t flag;
 case KWD_unsigned:
 case KWD___unsigned:
 case KWD___unsigned__:
  flag = DCCTYPE_UNSIGNED;
  if (DCC_MACRO_FALSE) {
 case KWD_signed:
 case KWD___signed:
 case KWD___signed__:
   flag = 0;
  }
  if (flags&FLAG_FOUND_SIGN) WARN(W_QUALIFIER_ALREADY_IN_USE);
  flags |= FLAG_FOUND_SIGN;
  /* Apply the new sign flag. */
  self->t_type &= ~(DCCTYPE_UNSIGNED);
  if ((DCCTYPE_GROUP(self->t_type) == DCCTYPE_BUILTIN &&
       DCCTYPE_BASIC(self->t_type) < 8) ||
     (DCCTYPE_GROUP(self->t_type) == DCCTYPE_STRUCTURE &&
      self->t_base->d_attr &&
     (self->t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC))
     ) self->t_type |= flag;
  else {
   WARN(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,self);
  }
  goto next;
 } break;

 {
 case KWD_int:
  if (flags&FLAG_FOUND_INT) WARN(W_QUALIFIER_ALREADY_IN_USE);
  flags |= FLAG_FOUND_INT;
  /* HINT: 'DCCTYPE_INT' equals ZERO(0), so it's already set implicitly. */
  goto next;
 } break;

 case KWD___auto_type:
  if ((flags&(FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH)) ||
      (self->t_type&DCCTYPE_QUAL)) break;
  self->t_type |= DCCTYPE_AUTO;
  flags |= (FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH);
  goto next;

 {
  tyid_t newflags;
  if (DCC_MACRO_FALSE) { case KWD__Bool: newflags = DCCTYPE_BOOL; WARN(W_BUILTIN_TYPE_BOOL_C99); }
  if (DCC_MACRO_FALSE) { case KWD_void:  newflags = DCCTYPE_VOID; }
  if (DCC_MACRO_FALSE) { case KWD_float: newflags = DCCTYPE_FLOAT; }
  if (flags&(FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH)) break;
  self->t_type |= newflags;
  flags |= (FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH);
  goto next;
 } break;

 { /* floating-point type: double */
 case KWD_double:
  if (flags&(FLAG_FOUND_INT|FLAG_FOUND_SIGN)) break;
  if (flags&FLAG_FOUND_LENGTH) {
   if ((self->t_type&~(DCCTYPE_FLAGSMASK)) != DCCTYPE_LONG) break;
   self->t_type &= (DCCTYPE_FLAGSMASK);
   self->t_type |= (DCCTYPE_LDOUBLE);
  } else {
   self->t_type &= (DCCTYPE_FLAGSMASK);
   self->t_type |= (DCCTYPE_DOUBLE);
  }
  flags |= (FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH);
  goto next;
 } break;

 { /* Storage modifiers. */
  tyid_t new_storage;
 case KWD_auto: /* Automatic storage / auto-type. */
  if (HAS(EXT_AUTO_FOR_AUTOTYPE)) { flags |= FLAG_FOUND_AUTO; goto next; }
  new_storage = DCCTYPE_AUTOMATIC;
  if (DCC_MACRO_FALSE) { case KWD_static:   new_storage = DCCTYPE_STATIC; }
  if (DCC_MACRO_FALSE) { case KWD_extern:   new_storage = DCCTYPE_EXTERN; }
  if (DCC_MACRO_FALSE) { case KWD_register: new_storage = DCCTYPE_REGISTER; }
  if (DCC_MACRO_FALSE) { case KWD_typedef:  new_storage = DCCTYPE_TYPEDEF; }
  if (self->t_type&(DCCTYPE_STOREMASK&~(DCCTYPE_INLINE))) {
   WARN(W_TYPE_STORAGE_CLASS_ALREADY_DEFINED);
   self->t_type &= ~(DCCTYPE_STOREMASK&~(DCCTYPE_INLINE));
  }
  self->t_type |= new_storage;
  flags |= FLAG_FOUND_STORAGE;
  goto next;
 } break;

 { /* Storage flag: inline. */
 case KWD_inline:
 case KWD___inline:
 case KWD___inline__:
  if (self->t_type&DCCTYPE_INLINE)
   WARN(W_TYPE_STORAGE_CLASS_ALREADY_DEFINED);
  self->t_type |= DCCTYPE_INLINE;
  goto next;
 } break;

 {
 case KWD__Atomic:
  WARN(W_TYPE_MODIFIER_ATOMIC_C11);
  if (self->t_type&DCCTYPE_ATOMIC)
   WARN(W_TYPE_MODIFIER_ATOMIC_ALREADY_DEFINED);
  self->t_type |= DCCTYPE_ATOMIC;
  goto next;
 } break;

 { /* struct/union/enum declaration */
  uint16_t decl_flag;
  if (DCC_MACRO_FALSE) { case KWD_enum:   decl_flag = DCC_DECLKIND_ENUM; }
  if (DCC_MACRO_FALSE) { case KWD_struct: decl_flag = DCC_DECLKIND_STRUCT; }
  if (DCC_MACRO_FALSE) { case KWD_union:  decl_flag = DCC_DECLKIND_UNION; }
  if (flags&(FLAG_FOUND_INT|FLAG_FOUND_LENGTH)) break;
  YIELD();
  DCCParse_Attr(attr);
  if (TPP_ISKEYWORD(TOK)) {
   decl = DCCCompiler_NewDecl(TOKEN.t_kwd,DCC_NS_STRUCT);
   if unlikely(!decl) break;
   YIELD();
   DCCParse_Attr(attr);
   DCCDecl_Incref(decl);
  } else {
   decl = DCCDecl_New(&TPPKeyword_Empty);
   if unlikely(!decl) break;
  }
  if (decl->d_kind == DCC_DECLKIND_NONE) {
   /* TODO: Warn about struct/union/enum declaration in argument list. */
   /* (Forward) declare a new type. */
   decl->d_kind  = DCC_DECLKIND_TYPE|decl_flag;
   decl->d_flag |= DCC_DECLFLAG_FORWARD;
   /* The following are already initialized thanks to zero-initialization. */
   assert((decl->d_type.t_type&DCCTYPE_BASICMASK) == DCCTYPE_INT);
   assert(decl->d_type.t_base == NULL);
   assert(decl->d_tdecl.td_size == 0);
   assert(decl->d_tdecl.td_fieldv == NULL);
  }
  assertf(decl->d_kind&DCC_DECLKIND_TYPE,
          "The struct symtab should only contain types, but '%s' isn't",
          decl->d_name->k_name);
  flags |= (FLAG_FOUND_INT|FLAG_FOUND_LENGTH);
  if (TOK == '{') {
   /* The type is actually being declared _here_! */
   if (!(decl->d_flag&DCC_DECLFLAG_FORWARD)) {
    WARN(W_TYPE_NOT_FORWARD,decl);
    if (decl->d_depth >= compiler.c_scope.s_depth) {
     DCCDecl_Clear(decl,compiler.c_scope.s_depth);
    } else {
     struct DCCDecl *newdecl;
     newdecl = DCCDecl_New(decl->d_name);
     DCCDecl_Decref(decl);
     if unlikely(!newdecl) break;
     decl = newdecl;
    }
    decl->d_kind = decl_flag;
   }
   decl->d_flag &= ~(DCC_DECLFLAG_FORWARD);
   YIELD();
   if (decl_flag == DCC_DECLKIND_STRUCT ||
       decl_flag == DCC_DECLKIND_UNION) {
    DCCParse_Struct(decl);
   } else {
    /* TODO: Unless unnamed, STD-C doesn't want enum
     *       constants to be type-compatible with int!
     *    >> enum color { red = 0, green = 1, blue = 2 };
     *    >> enum color c;
     *    >> c = 1;                        // WARNING
     *    >> c = (enum color)1;            // OK
     *    >> c = green;                    // OK
     *    >> c = red+1;                    // WARNING (twice: enum->int, int->enum)
     *    >> c = (enum color)((int)red+1); // OK
     * TODO: Enum classes:
     *    >> enum nesw: uint8_t { north, east, south, west };
     *    >> printf("%z\n",sizeof(enum nesw));  // "1\n"
     *    >> printf("%z\n",sizeof(north));      // "1\n"
     *    >> printf("%z\n",sizeof((int)north)); // "4\n"
     */
    DCCParse_Enum();
   }
   if (TOK != '}') WARN(W_EXPECTED_RBRACE); else YIELD();
   DCCParse_Attr(attr);
   DCCDecl_SetAttr(decl,attr);
   if (decl_flag == DCC_DECLKIND_STRUCT ||
       decl_flag == DCC_DECLKIND_UNION) {
    DCCDecl_CalculateStructureOffsets(decl);
   }
  }
  if (decl_flag == DCC_DECLKIND_STRUCT ||
      decl_flag == DCC_DECLKIND_UNION) {
   self->t_base  = decl; /* Inherit reference. */
   self->t_type |= DCCTYPE_STRUCTURE;
   DCCType_ASSERT(self);
   if (flags&FLAG_FOUND_SIGN && (!decl->d_attr ||
     !(decl->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC))) {
    WARN(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,self);
    self->t_type &= ~(DCCTYPE_UNSIGNED);
   }
  } else {
   /* Enum types don't reference the actual enum symbol.
    * Instead, they simply behave as an alias for 'int'. */
   assert(self->t_base == NULL);
   assert((decl->d_type.t_type&DCCTYPE_BASICMASK) ==  DCCTYPE_INT ||
          (decl->d_type.t_type&DCCTYPE_BASICMASK) == (DCCTYPE_INT|DCCTYPE_UNSIGNED));
   DCCDecl_Decref(decl);
  }
  goto next_noyield;
 } break;

 { /* GCC-extension: '__typeof__(...)' */
  int has_paren;
  struct DCCAttrDecl attr2;
  struct TPPKeyword *kwd2;
 case KWD_typeof:
 case KWD___typeof:
 case KWD___typeof__:
  if (flags&(FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH)) break;
  flags |= (FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH);
  assert(!self->t_base);
  YIELD();
  has_paren = (TOK == '(');
  if (has_paren) YIELD();
  memset(&attr2,0,sizeof(attr2));
  /* Give 'typeof' a chance to referr to a type declaration directly,
   * meaning that similar to 'sizeof', you may also specify a type
   * after 'typeof', to which 'typeof' will simply do nothing and
   * return that same type! */
  kwd2 = DCCParse_CType(self,&attr2);
  DCCAttrDecl_Quit(&attr2);
  if (!kwd2) {
   /* Simple enough: Just disable code generation & parse a regular, old expression.
    *                Once that's done, the expression can be found on the vstack! */
   pushf();
   compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
   DCCParse_Expr();
   *self = vbottom->sv_ctype;
   if (self->t_base) DCCDecl_Incref(self->t_base);
   vpop(1);
   popf();
  }
  DCCType_ASSERT(self);
  if (has_paren) {
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  }
 } break;

 default:
  if (TPP_ISKEYWORD(TOK) &&
    !(flags&(FLAG_FOUND_INT|FLAG_FOUND_LENGTH))) {
   /* Lookup a user-defined type. */
   decl = DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_LOCALS);
   if (decl && decl->d_kind&DCC_DECLKIND_TYPE) {
    flags |= (FLAG_FOUND_INT|FLAG_FOUND_LENGTH);
    assert(!self->t_base);
    self->t_type &= (DCCTYPE_UNSIGNED|DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK)); /* Keep flags! */
    if (flags&FLAG_FOUND_SIGN) {
     /* Allow sign modifiers on arithmetic structures, or integral builtins. */
     if ((DCCTYPE_GROUP(decl->d_type.t_type) == DCCTYPE_BUILTIN &&
          DCCTYPE_BASIC(decl->d_type.t_type) < 8) ||
        (DCCTYPE_GROUP(decl->d_type.t_type) == DCCTYPE_STRUCTURE &&
         decl->d_type.t_base->d_attr &&
        (decl->d_type.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)));
     else {
      WARN(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,&decl->d_type);
      self->t_type &= ~(DCCTYPE_UNSIGNED);
     }
    }
    DCCType_ASSERT(&decl->d_type);
    self->t_type |= decl->d_type.t_type;
    self->t_base  = decl->d_type.t_base;
    if (decl->d_attr) DCCAttrDecl_Merge(attr,decl->d_attr);
    if (self->t_base) DCCDecl_Incref(self->t_base);
    DCCType_ASSERT(self);
    goto next;
   }
  }
  break;
 }
 if (!(flags&(FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH))) {
  if (flags&FLAG_FOUND_AUTO) {
   /* Special case: Only found 'auto', but nothing else!
    * >> In this case, and since 'FLAG_FOUND_AUTO' can only be set
    *    when that extension is enabled, actually return '__auto_type'! */
   assert(!self->t_base);
   self->t_type = DCCTYPE_AUTO;
   flags = (FLAG_FOUND_INT|FLAG_FOUND_SIGN|FLAG_FOUND_LENGTH);
  } else if (flags) {
   /* This can happen if all that's been given was a storage specifier.
    * e.g.: >> extern;
    */
   WARN(W_INCOMPLETE_TYPE_DESCRIPTOR,self);
  }
 } else if (flags&FLAG_FOUND_AUTO) {
  WARN(W_AUTO_USED_AS_STORAGE_CLASS);
 }
 if ((self->t_type&DCCTYPE_INLINE) &&
    !(flags&FLAG_FOUND_STORAGE)) {
  assert((self->t_type&DCCTYPE_STOREMASK) == DCCTYPE_INLINE);
  /* A raw inline storage modifier causes the
   * storage class to default to static duration. */
  self->t_type |= DCCTYPE_STATIC;
  flags        |= FLAG_FOUND_STORAGE;
 }
 return flags;
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_CTYPE_C_INL */
