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

DCC_DECL_BEGIN

INTERN void
DCCType_SetTypeMode(struct DCCType *__restrict self,
                    uint32_t mode_flags) {
 tyid_t modeid;
 switch (mode_flags/*&DCC_ATTRFLAG_MASK_MODE*/) {
 {
#ifdef DCCTYPE_IB1
  if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_QI: modeid = DCCTYPE_IB1; }
#endif
#ifdef DCCTYPE_IB2
  if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_HI: modeid = DCCTYPE_IB2; }
#endif
#ifdef DCCTYPE_IB4
  if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_SI: modeid = DCCTYPE_IB4; }
#endif
#ifdef DCCTYPE_IB8
  if (DCC_MACRO_FALSE) { case DCC_ATTRFLAG_MODE_DI: modeid = DCCTYPE_IB8; }
#endif
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
 } break;
 default: break;
 }
}

INTERN void
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

INTERN void
DCCDecl_CalculateFunctionOffsets(struct DCCDecl *__restrict funtydecl) {
 target_off_t current_offset = 0;
 target_siz_t s,a;
 struct DCCStructField *iter,*end;
 assert(funtydecl);
 assert(funtydecl->d_kind == DCC_DECLKIND_FUNCTION ||
        funtydecl->d_kind == DCC_DECLKIND_OLDFUNCTION);
 end = (iter = funtydecl->d_tdecl.td_fieldv)+
               funtydecl->d_tdecl.td_size;
 for (; iter != end; ++iter) {
  struct DCCDecl *decl = iter->sf_decl;
  assert(decl);
  assert(decl->d_kind == DCC_DECLKIND_TYPE);
  DCCType_PromoteFunArg(&decl->d_type);
  s = DCCType_Sizeof(&decl->d_type,&a,1);
  if (a < DCC_TARGET_STACKALIGN) a = DCC_TARGET_STACKALIGN;
  if (decl->d_attr) {
   if (decl->d_attr->a_specs&DCC_ATTRSPEC_PACKED) {
    a = (decl->d_attr->a_specs&DCC_ATTRSPEC_FIXEDALIGN) ? decl->d_attr->a_align : 1;
   } else if (decl->d_attr->a_specs&DCC_ATTRSPEC_FIXEDALIGN && decl->d_attr->a_align > a) {
    a = decl->d_attr->a_align;
   }
  }
  current_offset  = (current_offset+(a-1)) & ~(a-1);
  iter->sf_off    =  current_offset;
  current_offset += s;
 }
}


/* Parse the contents of a new-style argument list.
 * int add(int x, int y);
 *         ^^^^^^^^^^^^ (When 'opt_first*' is NULL)
 *              ^^^^^^^ (When 'opt_first*' isn't NULL)
 * 'opt_first*' may be passed to describe the first type
 * in the event that the caller was required to parse it.
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeNewArgumentList(struct DCCDecl *__restrict funtydecl,
                              struct DCCType     *opt_firsttype,
                              struct TPPKeyword  *opt_firstname,
                              struct DCCAttrDecl *opt_firstattr) {
 struct DCCStructField *argv,*new_argv;
 size_t argc,arga; int is_first = 1;
 assert(funtydecl);
 assert(funtydecl->d_kind == DCC_DECLKIND_FUNCTION);
 assert((opt_firsttype != NULL) == (opt_firstname != NULL));
 assert((opt_firsttype != NULL) == (opt_firstattr != NULL));
 argv = NULL; argc = arga = 0;
 for (;;) {
  struct DCCType     arg_type;
  struct DCCAttrDecl arg_attr = DCCATTRDECL_INIT;
  struct DCCDecl *arg_decl;
  if (!opt_firstname) {
   if (TOK == ')' || TOK <= 0) break;
   if (TOK == TOK_DOTS) {
    /* Variable argument list. */
    funtydecl->d_flag |= DCC_DECLFLAG_VARIADIC;
    YIELD();
    goto nextarg;
   }
   opt_firstname = DCCParse_CType(&arg_type,&arg_attr);
   if unlikely(!opt_firstname) break;
   opt_firsttype = &arg_type;
   opt_firstattr = &arg_attr;
   is_first      = 0;
  }
  assert(opt_firstname);
  if (argc == arga) {
   /* Allocate more storage. */
   if (!arga) arga = 1;
   arga *= 2;
   new_argv = (struct DCCStructField *)realloc(argv,arga*sizeof(struct DCCStructField));
   if unlikely(!new_argv) goto err_argv;
   argv = new_argv;
  }
  arg_decl = DCCDecl_New(opt_firstname);
  if unlikely(!arg_decl) goto err_argv;
  DCCDecl_SetAttr(arg_decl,opt_firstattr);
  if (is_first) DCCDecl_XIncref(opt_firsttype->t_base);
  else          DCCAttrDecl_Quit(opt_firstattr);
  arg_decl->d_kind = DCC_DECLKIND_TYPE;
  arg_decl->d_type = *opt_firsttype; /* Inherit object. */
  argv[argc++].sf_decl = arg_decl; /* Inherit reference. */
nextarg:
  if (TOK != ',') break;
  YIELD();
  opt_firstname = NULL;
 }
 if (argc == 1 && argv[0].sf_decl->d_name == &TPPKeyword_Empty &&
     DCCTYPE_ISBASIC(argv[0].sf_decl->d_type.t_type,DCCTYPE_VOID)) {
  /* Special case: void argument list. */
  DCCDecl_Decref(argv[0].sf_decl);
  free(argv);
  argv = NULL;
  argc = 0;
 } else if (argc != arga) {
  assert(argc);
  new_argv = (struct DCCStructField *)realloc(argv,argc*sizeof(struct DCCStructField));
  if (new_argv) argv = new_argv;
 }
 funtydecl->d_tdecl.td_size   = argc;
 funtydecl->d_tdecl.td_fieldv = argv;
 return;
err_argv:
 new_argv = argv+argc;
 while (new_argv-- != argv) {
  assert(new_argv->sf_decl);
  DCCDecl_Decref(new_argv->sf_decl);
 }
 free(argv);
 TPPLexer_SetErr();
}

/* Parse the contents of an old-style argument list.
 * int add(x,y);
 *         ^^^ (When 'opt_first*' is NULL)
 *          ^^ (When 'opt_first*' isn't NULL)
 * 'opt_first*' may be passed to describe the first argument
 * in the event that the caller was required to parse it.
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeOldArgumentList(struct DCCDecl *__restrict funtydecl,
                              struct TPPKeyword  *opt_firstname,
                              struct DCCAttrDecl *opt_firstattr) {
 struct DCCStructField *argv,*new_argv;
 size_t argc,arga; int is_first = 1;
 assert(funtydecl);
 assert(funtydecl->d_kind == DCC_DECLKIND_OLDFUNCTION);
 assert((opt_firstname != NULL) == (opt_firstattr != NULL));
 assert(!opt_firstname || (TOK == ',' || TOK == ')'));
 WARN(W_OLD_STYLE_FUNCTION_DECLARATION);
 argv = NULL; argc = arga = 0;
 for (;;) {
  struct DCCAttrDecl arg_attr = DCCATTRDECL_INIT;
  struct DCCDecl *arg_decl;
  if (!opt_firstname) {
   if (TOK == ')' || TOK <= 0) break;
   if (TOK == TOK_DOTS) {
    WARN(W_OLD_STYLE_FUNCTION_VARARGS);
    funtydecl->d_flag |= DCC_DECLFLAG_VARIADIC;
    YIELD();
    goto nextarg;
   }
   DCCParse_Attr(&arg_attr);
   if (TPP_ISKEYWORD(TOK)) {
    opt_firstname = TOKEN.t_kwd;
    YIELD();
    DCCParse_Attr(&arg_attr);
   } else {
    /* Special warning for unnamed arguments. */
    WARN(TOK == ',' ? W_OLD_STYLE_FUNCTION_UNNAMED_ARGUMENT
                    : W_OLD_STYLE_FUNCTION_EXPECTED_ARGUMENT_KEYWORD);
    opt_firstname = &TPPKeyword_Empty;
   }
   opt_firstattr = &arg_attr;
   is_first      = 0;
  }
  assert(opt_firstname);
  if (argc == arga) {
   /* Allocate more storage. */
   if (!arga) arga = 1;
   arga *= 2;
   new_argv = (struct DCCStructField *)realloc(argv,arga*sizeof(struct DCCStructField));
   if unlikely(!new_argv) goto err_argv;
   argv = new_argv;
  }
  arg_decl = DCCDecl_New(opt_firstname);
  if unlikely(!arg_decl) goto err_argv;
  arg_decl->d_kind = DCC_DECLKIND_TYPE;
  DCCDecl_SetAttr(arg_decl,opt_firstattr);
  if (!is_first) DCCAttrDecl_Quit(opt_firstattr);
  assert(arg_decl->d_type.t_type == DCCTYPE_INT);
  assert(arg_decl->d_type.t_base == NULL);
  argv[argc++].sf_decl = arg_decl; /* Inherit reference. */
nextarg:
  if (TOK != ',') break;
  YIELD();
  opt_firstname = NULL;
 }
 if (argc == 1 && argv[0].sf_decl->d_name == &TPPKeyword_Empty &&
     DCCTYPE_ISBASIC(argv[0].sf_decl->d_type.t_type,DCCTYPE_VOID)) {
  /* Special case: void argument list. */
  DCCDecl_Decref(argv[0].sf_decl);
  free(argv);
  argv = NULL;
  argc = 0;
 } else if (argc != arga) {
  assert(argc);
  new_argv = (struct DCCStructField *)realloc(argv,argc*sizeof(struct DCCStructField));
  if (new_argv) argv = new_argv;
 }
 funtydecl->d_tdecl.td_size   = argc;
 funtydecl->d_tdecl.td_fieldv = argv;
 return;
err_argv:
 new_argv = argv+argc;
 while (new_argv-- != argv) {
  assert(new_argv->sf_decl);
  DCCDecl_Decref(new_argv->sf_decl);
 }
 free(argv);
 TPPLexer_SetErr();
}

/* Parse a single chain of definitions for an old-style argument list:
 * >> int add(x,y) int x,y;
 *                     ^^^
 * >> int add(x,y) int x; int y;
 *                     ^
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeOldArgumentListDefWithBase(struct DCCDecl *__restrict funtydecl,
                                         struct DCCType     const *__restrict base_type,
                                         struct DCCAttrDecl const *__restrict base_attr) {
 struct DCCType type;
 struct DCCAttrDecl attr;
 struct TPPKeyword *arg_name;
 assert(funtydecl);
 assert(funtydecl->d_kind == DCC_DECLKIND_OLDFUNCTION);
 assert(base_type);
 assert(base_attr);
 for (;;) {
  DCCType_InitCopy(&type,base_type);
  DCCAttrDecl_InitCopy(&attr,base_attr);
  arg_name = DCCParse_CTypeSuffix(&type,&attr);
  assert(arg_name);
  if (arg_name != &TPPKeyword_Empty) {
   struct DCCStructField *iter,*end;
   end = (iter = funtydecl->d_tdecl.td_fieldv)+
                 funtydecl->d_tdecl.td_size;
   for (; iter != end; ++iter) {
    assert(iter->sf_decl);
    if (iter->sf_decl->d_name == arg_name) {
     if (iter->sf_decl->d_type.t_type == DCCTYPE_INT) {
      /* Warn about using types that cannot be
       * properly aligned as old-style arguments. */
      if (DCCType_Sizeof(&type,NULL,1) > DCC_TARGET_STACKALIGN)
          WARN(W_OLD_STYLE_ARGUMENT_TYPE_TOO_LARGE,&type);
      iter->sf_decl->d_type = type; /* Inherit object. */
      type.t_base           = NULL;
     } else {
      /* Warning: Argument already defined. */
      WARN(W_DECL_ALREADY_DEFINED,iter->sf_decl);
     }
     goto done;
    }
   }
   WARN(W_UNKNOWN_FUNCTION_ARGUMENT,arg_name);
  }
done:
  DCCAttrDecl_Quit(&attr);
  DCCType_Quit(&type);
  if (TOK != ',') break;
  YIELD();
 }
}


/* Parse all definitions for an old-style argument list:
 * >> int add(x,y) int x,y;
 *                 ^^^^^^^^
 * NOTE: No-op when no definitions were found.
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeOldArgumentListDef(struct DCCDecl *__restrict funtydecl,
                                 struct DCCAttrDecl *empty_attr) {
 int is_first = 1;
 assert(funtydecl);
 assert(funtydecl->d_kind == DCC_DECLKIND_OLDFUNCTION);
 for (;;) {
  struct DCCType type;
  struct DCCAttrDecl attr = DCCATTRDECL_INIT;
  if (!DCCParse_CTypePrefix(&type,&attr)) {
   if (is_first && empty_attr) DCCAttrDecl_Merge(empty_attr,&attr);
   DCCAttrDecl_Quit(&attr);
   break;
  }
  if (is_first) WARN(W_OLD_STYLE_FUNCTION_IMPLEMENTATION);
  DCCParse_CTypeOldArgumentListDefWithBase(funtydecl,&type,&attr);
  DCCAttrDecl_Quit(&attr);
  DCCType_Quit(&type);
  if (TOK != ';') WARN(W_EXPECTED_SEMICOLON);
  else YIELD();
  if (TOK == TOK_DOTS) {
   /* Old-style varargs declaration. */
   if (funtydecl->d_flag&DCC_DECLFLAG_VARIADIC)
       WARN(W_OLD_STYLE_FUNCTION_VARARGS_ALREADY);
   else {
    funtydecl->d_flag |= (DCC_DECLFLAG_VARIADIC|
                          DCC_DECLFLAG_VARIADICLAST);
   }
   YIELD();
  }
  is_first = 0;
 }
}

/* Parse all definitions for an old-style argument list:
 * >> int add(x,y) int x,y;
 *                     ^^^^
 * >> int add(x,y) int x; int y;
 *                     ^^^^^^^^^
 * NOTE: No-op when no definitions were found.
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeOldArgumentListDefWithFirstBase(struct DCCDecl *__restrict funtydecl,
                                              struct DCCType     *__restrict firstbase_type,
                                              struct DCCAttrDecl *__restrict firstbase_attr) {
 assert(funtydecl);
 assert(funtydecl->d_kind == DCC_DECLKIND_OLDFUNCTION);
 assert(firstbase_type);
 assert(firstbase_attr);
 DCCParse_CTypeOldArgumentListDefWithBase(funtydecl,firstbase_type,firstbase_attr);
 DCCParse_CTypeOldArgumentListDef(funtydecl,NULL);
}



/* Parse the array extension suffix of a type declaration:
 * >> int foo[42];
 *           ^^^^
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeArrayExt(struct DCCType *__restrict self,
                       struct DCCAttrDecl *__restrict attr) {
 int_t array_size;
 assert(self);
 assert(attr);
 assert(TOK == '[');
 YIELD();
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
 if (!DCCType_IsComplete(self)) {
  WARN(W_EXPECTED_COMPLETE_TYPE_FOR_ARRAY_BASE,self);
  /* Try to fix incomplete types. */
  DCCType_FixComplete(self);
 }
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





/* Parse optional trailing type modifiers:
 * >> int x[42];
 *         ^^^^
 * >> int (*p)(int foo);
 *            ^^^^^^^^^
 */
LEXPRIV void DCC_PARSE_CALL
DCCParse_CTypeTrail(struct DCCType     *__restrict self,
                    struct DCCAttrDecl *__restrict attr) {
 assert(self);
 assert(attr);
 DCCParse_Attr(attr);
 if (TOK == '(') {
  struct DCCDecl *funtydecl;
  funtydecl = DCCDecl_New(&TPPKeyword_Empty);
  if unlikely(!funtydecl) return;
  if (!DCCType_IsComplete(self)) WARN(W_EXPECTED_COMPLETE_TYPE_FOR_FUNCTION_BASE,self);
  funtydecl->d_type = *self; /* Inherit data. */
  funtydecl->d_type.t_type &= ~(DCCTYPE_STOREMASK);
  self->t_type &= (DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK));
  self->t_type |= (DCCTYPE_FUNCTION);
  self->t_base  = funtydecl; /* Inherit reference. */
  YIELD();
  if (TOK == ')') {
   WARN(W_OLD_STYLE_FUNCTION_DECLARATION_NOARGS);
   YIELD();
   funtydecl->d_kind  = DCC_DECLKIND_OLDFUNCTION;
   assert(funtydecl->d_tdecl.td_size   == 0);
   assert(funtydecl->d_tdecl.td_fieldv == NULL);
   /* Although any argument specified would be unknown, we must
    * still make sure to handle that case for code integrity. */
   DCCParse_CTypeOldArgumentListDef(self->t_base,attr);
  } else {
   struct DCCType     firstarg_type;
   struct TPPKeyword *firstarg_name;
   struct DCCAttrDecl firstarg_attr = DCCATTRDECL_INIT;
   int is_old_funimpl = 0;
   /* Parse a function prototype:
    * #1: >> int add(int x, int y); // New-style declaration with 2 named arguments.
    *     >> int add(int,int);      // New-style declaration with 2 unnamed arguments.
    *     >> int add(void);         // New-style declaration with empty argument list.
    * #2: >> int add(x,y);          // Old-style declaration with implicit argument types.
    *     >> int add(x,y) int x,y;  // Old-style declaration with explicit argument types.
    *     >> int add(,);            // Old-style declaration with 2 unnamed arguments.
    */
   if (TOK == ',') {
    /* Old-style argument list with an unnamed first argument. */
oldstyle_arglist:
    funtydecl->d_kind = DCC_DECLKIND_OLDFUNCTION;
    DCCParse_CTypeOldArgumentList(funtydecl,NULL,NULL);
    is_old_funimpl = 1;
   } else if ((firstarg_name = DCCParse_CType(&firstarg_type,&firstarg_attr)) != NULL) {
    funtydecl->d_kind = DCC_DECLKIND_FUNCTION;
    DCCParse_CTypeNewArgumentList(funtydecl,&firstarg_type,firstarg_name,&firstarg_attr);
    DCCType_Quit(&firstarg_type);
   } else {
    /* Fallback: Old-style argument list. */
    goto oldstyle_arglist;
   }
   DCCAttrDecl_Quit(&firstarg_attr);
   if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
   if (is_old_funimpl) {
    /* Parse the type definitions of an old-style argument list. */
    assert(DCCTYPE_GROUP(self->t_type) == DCCTYPE_FUNCTION);
    assert(self->t_base);
    assert(self->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION);
    DCCParse_CTypeOldArgumentListDef(self->t_base,attr);
   }
  }
  DCCParse_Attr(attr);
  DCCDecl_SetAttr(funtydecl,attr);
  if (self->t_base &&
     (self->t_base->d_kind == DCC_DECLKIND_FUNCTION ||
      self->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
   DCCDecl_CalculateFunctionOffsets(self->t_base);
  }
 } else if (TOK == '[') {
  /* Array/VArray declaration. */
  DCCParse_CTypeArrayExt(self,attr);
  assert(self->t_base);
  assert(self->t_base->d_kind&DCC_DECLKIND_TYPE);
  /* Continue working on the base type, thus ensuring correct type order. */
  self = &self->t_base->d_type;
  DCCParse_Attr(attr);
  DCCParse_CTypeTrail(self,attr);
 }
}



PUBLIC struct TPPKeyword *DCC_PARSE_CALL
DCCParse_CTypeSuffix(struct DCCType     *__restrict self,
                     struct DCCAttrDecl *__restrict attr) {
 DCCType_ASSERT(self);
 struct DCCType inner_type;
 int        has_inner_type = 0;
 struct TPPKeyword *result = &TPPKeyword_Empty;
parse_leading:
 DCCParse_Attr(attr);
 /* Parse leading suffix modifiers for const/volatile/pointer/l-value */
 switch (TOK) {

 { /* Parse leading const/volatile qualifiers. */
  tyid_t qual;
  if (DCC_MACRO_FALSE) { case KWD_const:    case KWD___const:    case KWD___const__:    qual = DCCTYPE_CONST; }
  if (DCC_MACRO_FALSE) { case KWD_volatile: case KWD___volatile: case KWD___volatile__: qual = DCCTYPE_VOLATILE; }
  if (DCCTYPE_GROUP(self->t_type) == DCCTYPE_LVALUE) WARN(W_QUAL_ON_LVALUE);
  else if (DCCTYPE_ISBASIC(self->t_type,DCCTYPE_AUTO)) WARN(W_QUAL_ON_AUTO_TYPE);
  else { if (self->t_type&qual) WARN(W_QUALIFIER_ALREADY_IN_USE); self->t_type |= qual; }
  goto next_leading;
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
    assert(!self->t_base);
    WARN(W_AUTO_TYPE_USED_AS_POINTER_BASE);
    self->t_type = DCCTYPE_INT;
   }
   DCCType_MkPointer(self);
  } else {
   DCCType_MkLValue(self);
  }
  goto next_leading;

  /* Skip restrict-pointer qualifiers. */
 case KWD_restrict:
 case KWD___restrict:
 case KWD___restrict__:
  if (DCCTYPE_GROUP(self->t_type) != DCCTYPE_POINTER)
      WARN(W_RESTRICT_EXPECTS_POINTER);
  goto next_leading;

 default: break;
 }

 if (TOK == '(') {
  int is_old_funimpl = 0;
  struct DCCAttrDecl paren_attr = DCCATTRDECL_INIT;
  /* This gets ~really~ complicated.
   * Here, we must differentiate between:
   * #1: >> int (int foo) { return foo; }      // Unnamed new-style function.
   *     >> int (int);                         // Unnamed new-style function with unnamed argument.
   * #2: >> int (foo)(int bar) { return bar; } // Parenthesis recursion in named type.
   *     >> int (*)(int bar);                  // Parenthesis recursion around inner qualifiers.
   *     >> int (x);                           // Technically ambigous (may be an unnamed old-style function w/ 1 int-argument 'x'), but is parsed as variable type 'int x;'
   * #3: >> int (foo) int foo; { return foo; } // Old-style function arguments for unnamed type.
   *     >> int (x,y) int x,y; { return x+y; } // Multiple old-style function arguments.
   *     >> int (,y) int y; { return y; }      // Old-style function with unnamed first argument.
   *     >> int ();                            // Unnamed old-style function with empty argument list.
   */
  YIELD();
  DCCParse_Attr(&paren_attr);
  if (TOK == ')') {
/*parcase_3:*/ /* Unnamed old-style argument list. */
   WARN(W_OLD_STYLE_FUNCTION_DECLARATION_NOARGS);
   DCCType_MkOldFunc(self);
   is_old_funimpl = 1;
   goto innerend_yield;
  } else if (TOK == '*' || TOK == '&') {
   /* Optimization: For these tokens, we can effortlessly determine case #2. */
parcase_2:
   DCCAttrDecl_Merge(&paren_attr,attr);
   has_inner_type    = 1;
   inner_type.t_type = 0;
   inner_type.t_base = NULL;
   result = DCCParse_CTypeSuffix(&inner_type,&paren_attr);
inner_endparen:
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else innerend_yield: YIELD();
  } else if ((result = DCCParse_CType(&inner_type,&paren_attr)) != NULL) {
   struct DCCDecl *newfun_decl;
   /* A type is located inside the parenthesis. - This is case #1. */
/*parcase_1:*/
   newfun_decl = DCCDecl_New(&TPPKeyword_Empty);
   if likely(newfun_decl) {
    newfun_decl->d_kind = DCC_DECLKIND_FUNCTION;
    newfun_decl->d_type = *self; /* Inherit data. */
    self->t_base  = newfun_decl; /* Inherit reference. */
    self->t_type &= (DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK));
    self->t_type |= (DCCTYPE_FUNCTION);
    DCCDecl_SetAttr(newfun_decl,attr);
    /* Parse a new-style argument list. */
    DCCParse_CTypeNewArgumentList(newfun_decl,
                                 &inner_type,result,
                                 &paren_attr);
   }
   result = &TPPKeyword_Empty;
   DCCType_Quit(&inner_type);
   goto inner_endparen;
  } else if (TPP_ISKEYWORD(TOK)) {
   /* Ambiguity between cases #2 and #3.
    * >> When the next token is ',' this is case #3 for sure.
    * >> When the next token is ')' this is case #3 if an old-style argument list follows.
    * >>                                ... case #2 otherwise.
    */
   result = TOKEN.t_kwd;
   YIELD();
   DCCParse_Attr(&paren_attr);
   if (TOK == ',') {
    struct DCCDecl *oldfun_decl;
oldstyle_arglist:
    oldfun_decl = DCCDecl_New(&TPPKeyword_Empty);
    if likely(oldfun_decl) {
     /* Old-style argument list. */
     oldfun_decl->d_kind = DCC_DECLKIND_OLDFUNCTION;
     oldfun_decl->d_type = *self; /* Inherit data. */
     self->t_base  = oldfun_decl; /* Inherit reference. */
     self->t_type &= (DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK));
     self->t_type |= (DCCTYPE_FUNCTION);
     DCCDecl_SetAttr(oldfun_decl,attr);
     DCCParse_CTypeOldArgumentList(oldfun_decl,result,&paren_attr);
     is_old_funimpl = 1;
    }
    goto inner_endparen;
   }
   if (TOK == ')') {
    /* Either a simple recursive type, or the first argument of an unnamed old-style function. */
    YIELD();
    DCCParse_Attr(attr);
    if (DCCParse_CTypePrefix(&inner_type,&paren_attr)) {
     struct DCCDecl *oldfun_decl;
     oldfun_decl = DCCDecl_New(&TPPKeyword_Empty);
     if likely(oldfun_decl) {
      /* Old-style argument list. */
      oldfun_decl->d_kind = DCC_DECLKIND_OLDFUNCTION;
      oldfun_decl->d_type = *self; /* Inherit data. */
      self->t_base  = oldfun_decl; /* Inherit reference. */
      self->t_type &= (DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK));
      self->t_type |= (DCCTYPE_FUNCTION);
      DCCDecl_SetAttr(oldfun_decl,attr);
      DCCParse_CTypeOldArgumentListDefWithFirstBase(oldfun_decl,
                                                   &inner_type,
                                                   &paren_attr);
     }
     DCCType_Quit(&inner_type);
    } else {
     /* The r-paren isn't followed by a type, meaning this isn't an old-style argument list. */
     DCCAttrDecl_Merge(attr,&paren_attr);
    }
    goto inner_end;
   }
   /* Everything else is case #2 (The keyword we found is the name of the resulting type) */
   DCCAttrDecl_Merge(attr,&paren_attr);
   goto inner_endparen;
  } else if (TOK == ',') {
   /* Old-style argument list with an unnamed first argument. */
   result = &TPPKeyword_Empty;
   goto oldstyle_arglist;
  } else {
   /* Anything else is considered case #2 */
   goto parcase_2;
  }
inner_end:
  DCCAttrDecl_Quit(&paren_attr);
  if (is_old_funimpl) {
   /* Parse the type definitions of an old-style argument list. */
   assert(DCCTYPE_GROUP(self->t_type) == DCCTYPE_FUNCTION);
   assert(self->t_base);
   assert(self->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION);
   DCCParse_CTypeOldArgumentListDef(self->t_base,attr);
  }
  /* Calculate function offsets. */
  if (self->t_base &&
     (self->t_base->d_kind == DCC_DECLKIND_FUNCTION ||
      self->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
   DCCDecl_CalculateFunctionOffsets(self->t_base);
  }
 } else if (TPP_ISKEYWORD(TOK)) {
  /* Simple named type: 'int x;' */
  result = TOKEN.t_kwd;
  YIELD();
 } else {
  /* Unnamed type case. */
  result = &TPPKeyword_Empty;
 }

 /* Parse trailing type modifiers. */
 DCCParse_CTypeTrail(self,attr);

 /* Check if we must load 'self' as the base of an inner type.
  * Required for situations like:
  * >> int (*foo)(int x, int y);
  */
 if (has_inner_type) {
  struct DCCType *iter;
  /* Must override 'self' as lowest-possible base
   * of 'inner_type', the fill '*self' with 'inner_type' */
  DCCType_ForceDynamic(&inner_type);
  iter = &inner_type;
  while (iter->t_base) {
   assert(iter->t_base->d_kind&DCC_DECLKIND_TYPE);
   iter = &iter->t_base->d_type;
  }
  assertf(iter->t_type == 0,"As set by 'inner_type.t_type = 0' above!");
  *iter = *self; /* Inherit reference. */
  inner_type.t_type |= (self->t_type&(DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK)));
  *self = inner_type; /* Inherit reference. */
 }
 /* Apply & delete attribute mode flags. */
 if (attr->a_flags&DCC_ATTRFLAG_MASK_MODE) {
  DCCType_SetTypeMode(self,attr->a_flags&DCC_ATTRFLAG_MASK_MODE);
  attr->a_flags &= ~(DCC_ATTRFLAG_MASK_MODE);
 }
 return result;
next_leading: YIELD(); goto parse_leading;
}




PUBLIC int DCC_PARSE_CALL
DCCParse_CTypePrefix(struct DCCType *__restrict self,
                     struct DCCAttrDecl *__restrict attr) {
#define F_INT     0x01 /* 'int' */
#define F_SIGN    0x02 /* 'signed', 'unsigned' */
#define F_WIDTH   0x04 /* 'char', 'short', 'long' */
#define F_STORAGE 0x08 /* 'static', 'extern', 'typedef', 'register' */
#define F_INLINE  0x10 /* 'inline' */
#define F_AUTO    0x20 /* 'auto' (NOTE: Only used for automatic detection of 'auto' as storage/type) */
#define F_MISC    0x40 /* Everything else (e.g.: '_Atomic', 'const', 'volatile') */
#define FIX_AUTO() \
do{ if (flags&F_AUTO) {\
     WARN(W_AUTO_STORAGE_ALREADY_BY_DEFAULT);\
     self->t_type &= ~(DCCTYPE_BASICMASK);\
     flags        &= ~(F_AUTO);\
    }\
}while(DCC_MACRO_FALSE)
 unsigned int flags = 0;
 assert(self);
 assert(attr);
 /* Initialized to int. */
 self->t_type = DCCTYPE_INT;
 self->t_base = NULL;
again:
 DCCParse_Attr(attr);
 switch (TOK) {
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
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

 { /* Integer type flag. */
 case KWD_int:
  /* HINT: 'DCCTYPE_INT' equals ZERO(0), so it's already set implicitly. */
  if (flags&F_INT) WARN(W_QUALIFIER_ALREADY_IN_USE);
  else { FIX_AUTO(); flags |= F_INT; }
  goto next;
 } break;

 case KWD___auto_type:
  if ((flags&(F_INT|F_SIGN|F_WIDTH)) ||
      (self->t_type&DCCTYPE_QUAL)) break; /* TODO: Warning? */
  self->t_type |= DCCTYPE_AUTO;
  flags        |= (F_INT|F_SIGN|F_WIDTH);
  flags        &= ~(F_AUTO); /* A previous 'auto' was a storage modifier. */
  goto next;

 /* Double-precision floating point modifier. */
 case KWD_double:
  if (flags&(F_INT|F_SIGN)) break;
  if (self->t_type&DCCTYPE_ALTLONG) {
   self->t_type &= ~(DCCTYPE_BASICMASK|DCCTYPE_ALTMASK);
   self->t_type |=   DCCTYPE_LDOUBLE;
  } else {
   self->t_type &= ~(DCCTYPE_BASICMASK);
   self->t_type |=   DCCTYPE_DOUBLE;
  }
  flags |= (F_INT|F_SIGN|F_WIDTH);
  goto next;

 { /* Type width modifiers. */
  tyid_t newwidth;
 case KWD_long:
  if (flags&F_WIDTH) {
   /* Extended long modifiers. */
   if ((self->t_type&(DCCTYPE_BASICMASK&~(DCCTYPE_UNSIGNED))) == DCCTYPE_LONG) {
    self->t_type &= ~(DCCTYPE_BASICMASK|DCCTYPE_ALTMASK);
    self->t_type |=   DCCTYPE_LLONG;
    goto next;
   }
   if ((self->t_type&DCCTYPE_BASICMASK) == DCCTYPE_DOUBLE) {
    self->t_type &= ~(DCCTYPE_BASICMASK|DCCTYPE_ALTMASK);
    self->t_type |=   DCCTYPE_LDOUBLE;
    goto next;
   }
  }
  newwidth = (DCCTYPE_LONG|DCCTYPE_ALTLONG);
  if (DCC_MACRO_FALSE) { case KWD_short: newwidth = DCCTYPE_SHORT; }
  if (DCC_MACRO_FALSE) { case KWD_char:  newwidth = DCCTYPE_CHAR; if (CURRENT.l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED && !(flags&F_SIGN)) newwidth |= DCCTYPE_UNSIGNED; }
  if (DCC_MACRO_FALSE) {
#ifdef DCCTYPE_INT8
    if (DCC_MACRO_FALSE) { case KWD___int8:  newwidth = DCCTYPE_INT8; }
#endif
#ifdef DCCTYPE_INT16
    if (DCC_MACRO_FALSE) { case KWD___int16: newwidth = DCCTYPE_INT16; }
#endif
#ifdef DCCTYPE_INT32
    if (DCC_MACRO_FALSE) { case KWD___int32: newwidth = DCCTYPE_INT32; }
#endif
#ifdef DCCTYPE_INT64
    if (DCC_MACRO_FALSE) { case KWD___int64: newwidth = DCCTYPE_INT64; }
#endif
    if (!HAS(EXT_FIXED_LENGTH_INTEGER_TYPES)) break;
  }
  if (flags&F_WIDTH) WARN(W_QUALIFIER_ALREADY_IN_USE);
  self->t_type |= newwidth;
  flags        |= F_WIDTH;
  goto next;
 } break;


#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
  /* Letting DCC chew through the windows headers, I came across a ~very~ strange construct:
   * >> #define WTF_BOOL /##/
   * >> typedef WTF_BOOL bool;
   * Now while DCC doesn't perform concatenation in that case, it appears that '//' was
   * hacked into visual C as a token representing what C99 would later define as '_Bool'.
   * The code below tries to detect this _really_ weird construct, translating to to '_Bool'.
   */
 {
  char *next_tok;
  struct TPPFile *next_file;
 case '/':
  next_tok = peek_next_token(&next_file);
  if (next_tok[0] == '/' && next_tok == TOKEN.t_end) { YIELD(); goto w32_bool; }
  if (*next_tok++ != '#') break;
  next_tok = peek_next_advance(next_tok,&next_file);
  if (*next_tok++ != '#') break;
  next_tok = peek_next_advance(next_tok,&next_file);
  if (*next_tok++ != '/') break;
  while (TOKEN.t_file != next_file) YIELD();
  TOKEN.t_file->f_pos = next_tok;
  goto w32_bool;
 } break;
#endif

 { /* Special builtin types. */
  tyid_t newflags;
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
  if (DCC_MACRO_FALSE) { case KWD__Bool: WARN(W_BUILTIN_TYPE_BOOL_C99); w32_bool: newflags = DCCTYPE_BOOL; }
#else
  if (DCC_MACRO_FALSE) { case KWD__Bool: WARN(W_BUILTIN_TYPE_BOOL_C99);           newflags = DCCTYPE_BOOL; }
#endif
  if (DCC_MACRO_FALSE) { case KWD_void:  newflags = DCCTYPE_VOID; }
  if (DCC_MACRO_FALSE) { case KWD_float: newflags = DCCTYPE_FLOAT; }
  if (flags&(F_INT|F_SIGN|F_WIDTH)) break;
  FIX_AUTO();
  assert(!self->t_base);
  self->t_type |= newflags;
  flags        |= (F_INT|F_SIGN|F_WIDTH);
  goto next;
 } break;

 { /* Sign modifiers */
  tyid_t new_sign;
  if (DCC_MACRO_FALSE) { case KWD_unsigned: case KWD___unsigned: case KWD___unsigned__: new_sign = DCCTYPE_UNSIGNED; }
  if (DCC_MACRO_FALSE) { case KWD_signed:   case KWD___signed:   case KWD___signed__:   new_sign = 0; }
  if (flags&F_SIGN) WARN(W_QUALIFIER_ALREADY_IN_USE);
  FIX_AUTO();
  flags |= F_SIGN;
  /* Apply the new sign flag. */
  self->t_type &= ~(DCCTYPE_UNSIGNED);
  if ((DCCTYPE_GROUP(self->t_type) == DCCTYPE_BUILTIN &&
       DCCTYPE_BASIC(self->t_type) < 8) ||
     (DCCTYPE_GROUP(self->t_type) == DCCTYPE_STRUCTURE &&
      self->t_base->d_attr &&
     (self->t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC))
      ) self->t_type |= new_sign;
  else WARN(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,self);
  goto next;
 } break;

 { /* Storage modifiers. */
  tyid_t new_storage;
 case KWD_auto: /* Automatic storage / auto-type. */
  if (HAS(EXT_AUTO_FOR_AUTOTYPE) &&
    !(flags&(F_INT|F_SIGN|F_WIDTH))) {
   self->t_type |= DCCTYPE_AUTO;
   flags        |= F_AUTO;
  }
  new_storage = DCCTYPE_AUTOMATIC;
  if (DCC_MACRO_FALSE) { case KWD_static:   new_storage = DCCTYPE_STATIC; }
  if (DCC_MACRO_FALSE) { case KWD_extern:   new_storage = DCCTYPE_EXTERN; }
  if (DCC_MACRO_FALSE) { case KWD_register: new_storage = DCCTYPE_REGISTER; }
  if (DCC_MACRO_FALSE) { case KWD_typedef:  new_storage = DCCTYPE_TYPEDEF; }
  if (flags&F_STORAGE) {
   WARN(W_TYPE_STORAGE_CLASS_ALREADY_DEFINED);
   self->t_type &= ~(DCCTYPE_STOREMASK&~(DCCTYPE_INLINE));
  } else if (flags&F_INLINE) {
   self->t_type &= ~(DCCTYPE_STOREMASK&~(DCCTYPE_INLINE));
  }
  if (new_storage != DCCTYPE_AUTOMATIC) FIX_AUTO();
  self->t_type |= new_storage;
  flags        |= F_STORAGE;
  goto next;
 } break;

 { /* Storage flag: inline. */
 case KWD_inline:
  if (!HAS(EXT_SHORT_EXT_KEYWORDS)) break;
 case KWD___inline:
 case KWD___inline__:
  if (flags&F_INLINE)
      WARN(W_TYPE_STORAGE_CLASS_ALREADY_DEFINED);
  self->t_type |= DCCTYPE_INLINE;
  flags        |= F_INLINE;
  /* Inline storage (tries to) imply static storage duration. */
  if (!(flags&F_STORAGE)) self->t_type |= DCCTYPE_STATIC;
  goto next;
 } break;

#if DCC_TARGET_TLS != DCC_TARGET_TLSMODE_NONE
 case KWD__Thread_local:
 case KWD___thread:
  self->t_type |= DCCTYPE_TLS;
  goto next;
#endif

 { /* Type flag: Atomic accessor. */
 case KWD__Atomic:
  WARN(W_TYPE_MODIFIER_ATOMIC_C11);
  if (self->t_type&DCCTYPE_ATOMIC)
      WARN(W_TYPE_MODIFIER_ATOMIC_ALREADY_DEFINED);
  self->t_type |= DCCTYPE_ATOMIC;
  goto next;
 } break;

 { /* GCC-extension: '__typeof__(...)' */
  int has_paren;
  struct DCCAttrDecl typeof_attr;
 case KWD_typeof:
  if (!HAS(EXT_SHORT_EXT_KEYWORDS)) break;
 case KWD___typeof:
 case KWD___typeof__:
  if (flags&(F_INT|F_SIGN|F_WIDTH)) break;
  flags |= (F_INT|F_SIGN|F_WIDTH);
  assert(!self->t_base);
  YIELD();
  has_paren = (TOK == '(');
  if (has_paren) YIELD();
  memset(&typeof_attr,0,sizeof(typeof_attr));
  DCCParse_CTypeOnly(self,&typeof_attr);
  DCCType_ASSERT(self);
  if (has_paren) {
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  }
 } break;

 { /* struct/union/enum declaration */
  uint16_t decl_flag;
  struct DCCDecl *tydecl;
  if (DCC_MACRO_FALSE) { case KWD_enum:   decl_flag = DCC_DECLKIND_ENUM; }
  if (DCC_MACRO_FALSE) { case KWD_struct: decl_flag = DCC_DECLKIND_STRUCT; }
  if (DCC_MACRO_FALSE) { case KWD_union:  decl_flag = DCC_DECLKIND_UNION; }
  /* NOTE: Allow sign modifiers for arithmetic structure types. */
  if (flags&(F_INT|F_WIDTH)) break;
  flags |= (F_INT|F_WIDTH);
  assert(!self->t_base);
  YIELD();
  DCCParse_Attr(attr);
  if (TPP_ISKEYWORD(TOK)) {
   tydecl = DCCCompiler_NewDecl(TOKEN.t_kwd,DCC_NS_STRUCT);
   if unlikely(!tydecl) break;
   YIELD();
   DCCParse_Attr(attr);
   DCCDecl_Incref(tydecl);
  } else {
   tydecl = DCCDecl_New(&TPPKeyword_Empty);
   if unlikely(!tydecl) break;
  }
  if (tydecl->d_kind == DCC_DECLKIND_NONE) {
   /* TODO: Warn about struct/union/enum declaration in argument list. */
   /* (Forward) declare a new type. */
   tydecl->d_kind  = DCC_DECLKIND_TYPE|decl_flag;
   tydecl->d_flag |= DCC_DECLFLAG_FORWARD;
   /* The following are already initialized thanks to zero-initialization. */
   assert((tydecl->d_type.t_type&DCCTYPE_BASICMASK) == DCCTYPE_INT);
   assert(tydecl->d_type.t_base                     == NULL);
   assert(tydecl->d_tdecl.td_size                   == 0);
   assert(tydecl->d_tdecl.td_fieldv                 == NULL);
  }
  assertf(tydecl->d_kind&DCC_DECLKIND_TYPE,
          "The struct symtab should only contain types, but '%s' isn't",
          tydecl->d_name->k_name);
  if (TOK == '{') {
   /* The type is actually being declared _here_! */
   if (!(tydecl->d_flag&DCC_DECLFLAG_FORWARD)) {
    WARN(W_TYPE_NOT_FORWARD,tydecl);
    if (tydecl->d_depth >= compiler.c_scope.s_depth) {
     DCCDecl_Clear(tydecl,compiler.c_scope.s_depth);
    } else {
     struct DCCDecl *newdecl;
     newdecl = DCCDecl_New(&TPPKeyword_Empty);
     DCCDecl_Decref(tydecl);
     if unlikely(!newdecl) break;
     tydecl = newdecl;
    }
    tydecl->d_kind = decl_flag;
   }
   tydecl->d_flag &= ~(DCC_DECLFLAG_FORWARD);
   YIELD();
   if (decl_flag == DCC_DECLKIND_STRUCT ||
       decl_flag == DCC_DECLKIND_UNION) {
    DCCParse_Struct(tydecl);
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
   DCCDecl_SetAttr(tydecl,attr);
   if (decl_flag == DCC_DECLKIND_STRUCT ||
       decl_flag == DCC_DECLKIND_UNION) {
    DCCDecl_CalculateStructureOffsets(tydecl);
   }
  }
  if (decl_flag == DCC_DECLKIND_STRUCT ||
      decl_flag == DCC_DECLKIND_UNION) {
   self->t_base  = tydecl; /* Inherit reference. */
   self->t_type |= DCCTYPE_STRUCTURE;
   DCCType_ASSERT(self);
   if ((flags&F_SIGN) && (!tydecl->d_attr ||
      !(tydecl->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC))) {
    WARN(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,self);
    self->t_type &= ~(DCCTYPE_UNSIGNED);
   }
  } else {
   /* Enum types don't reference the actual enum symbol.
    * Instead, they simply behave as an alias for 'int'. */
   assert(self->t_base == NULL);
   assert((tydecl->d_type.t_type&DCCTYPE_BASICMASK) ==  DCCTYPE_INT ||
          (tydecl->d_type.t_type&DCCTYPE_BASICMASK) == (DCCTYPE_INT|DCCTYPE_UNSIGNED));
   DCCDecl_Decref(tydecl);
  }
  goto next_noyield;
 } break;

 default:
  if (TPP_ISKEYWORD(TOK) && !(flags&(F_INT|F_WIDTH))) {
   struct DCCDecl *tydecl;
   /* Lookup a user-defined type. */
   tydecl = DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_LOCALS);
   if (tydecl && tydecl->d_kind&DCC_DECLKIND_TYPE) {
    flags |= (F_INT|F_WIDTH);
    assert(!self->t_base);
    self->t_type &= (DCCTYPE_UNSIGNED|DCCTYPE_FLAGSMASK&~(DCCTYPE_ALTMASK)); /* Keep flags! */
    if (flags&F_SIGN) {
     /* Allow sign modifiers on arithmetic structures, or integral builtins. */
     if ((DCCTYPE_GROUP(tydecl->d_type.t_type) == DCCTYPE_BUILTIN &&
          DCCTYPE_BASIC(tydecl->d_type.t_type) < 8) ||
         (DCCTYPE_GROUP(tydecl->d_type.t_type) == DCCTYPE_STRUCTURE &&
          tydecl->d_type.t_base->d_attr &&
         (tydecl->d_type.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)));
     else {
      WARN(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,&tydecl->d_type);
      self->t_type &= ~(DCCTYPE_UNSIGNED);
     }
    }
    DCCType_ASSERT(&tydecl->d_type);
    self->t_type |= tydecl->d_type.t_type;
    self->t_base  = tydecl->d_type.t_base;
    if (tydecl->d_attr) DCCAttrDecl_Merge(attr,tydecl->d_attr);
    if (self->t_base) DCCDecl_Incref(self->t_base);
    DCCType_ASSERT(self);
    goto next;
   }
  }
  break;
 }
 return flags != 0;
next:         YIELD();
next_noyield: flags |= F_MISC;
 if ((self->t_type&DCCTYPE_ATOMIC) &&
     (DCCTYPE_GROUP(self->t_type) != DCCTYPE_POINTER) &&
     (DCCTYPE_GROUP(self->t_type) != DCCTYPE_BUILTIN ||
      DCCTYPE_ISFLOAT(self->t_type))) {
  WARN(W_TYPE_MODIFIER_ATOMIC_REQUIRES_INTEGRAL);
  self->t_type &= ~(DCCTYPE_ATOMIC);
 }
 goto again;
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

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_CTYPE_C_INL */
