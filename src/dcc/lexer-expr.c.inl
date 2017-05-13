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
#ifndef GUARD_DCC_LEXER_EXPR_C_INL
#define GUARD_DCC_LEXER_EXPR_C_INL 1

#include <dcc/common.h>
#include <dcc/vstack.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/gen.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

LEXPRIV void DCC_PARSE_CALL
DCCParse_ExprUnarySuffix(void) {
again:
 switch (TOK) {

 {
  tok_t t;
 case TOK_INC:
 case TOK_DEC:
  t = TOK;
  if (DCC_MACRO_FALSE) { case TOK_TILDE_TILDE: t = '~'; }
  YIELD();
  vdup(1);  /* x, dx */
  vswap();  /* dx, x */
  vgen1(t); /* dx, x* */
  vpop(1);  /* dx */
  goto again;
 } break;

 case '[':
  YIELD();
  /* Array sub-script. */
  vdup(1);          /* x, dx */
  vswap();          /* dx, x */
  vpop(1);          /* dx */
  DCCParse_Expr1(); /* dx, y */
  if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
  vgen2('+');       /* dx+y */
  vgen1('*');       /* *(dx+y) */
  goto again;

 {
 case TOK_ARROW:
  vgen1('*');      /* Dereference before subscript. */
 case '.':
  YIELD();
  if (TPP_ISKEYWORD(TOK)) {
   DCCVStack_Subscript(TOKEN.t_kwd);
   YIELD();
  } else {
   WARN(W_EXPECTED_KEYWORD_FOR_SUBSCRIPT);
  }
  goto again;
 } break;

 { /* Function call. */
  size_t n_args;
 case KWD___pack:
  YIELD();
  if (TOK == '(') goto function_call;
  n_args = DCCParse_Exprn();
  goto gen_call;
 case '(':
function_call:
  YIELD();
  n_args = DCCParse_Exprn();
  if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
gen_call:
  {
   int is_noreturn = 0;
   struct DCCType *function_type;
   function_type = &vbottom[n_args].sv_ctype;
   if (DCCTYPE_GROUP(function_type->t_type) == DCCTYPE_FUNCTION &&
      (assert(function_type->t_base),function_type->t_base->d_attr) &&
      (function_type->t_base->d_attr->a_flags&DCC_ATTRFLAG_NORETURN)
       ) is_noreturn = 1; /* Calling a function marked as __attribute__((noreturn)) */
   DCCVStack_Call(n_args);
   if (is_noreturn) {
    /* Mark the current branch as dead if this function call doesn't return. */
    compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                             DCC_COMPILER_FLAG_DEAD);
   }
  }
 } break;

 default: break;
 }
}

/* Register name token groups mapped to register classes. */
static rc_t const register_clases[] = {
 DCC_RC_I8,
 DCC_RC_I16|DCC_RC_I8,
 DCC_RC_I32|DCC_RC_I16|DCC_RC_I8,
#if DCC_TARGET_CPU == DCC_TARGET_X86_64
 DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8,
#endif
 DCC_RC_MMX,
 DCC_RC_SSE,
 DCC_RC_CR,
 DCC_RC_TR,
 DCC_RC_DB,
 DCC_RC_DR,
};

LEXPRIV rc_t DCC_PARSE_CALL DCCParse_Register(void) {
 rc_t reg_name,id;
 assert(TOK == '%');
 /* Use yield without macro expansion here.
  * >> Register names are just _so_ short. - Don't deny
  *    the user the ability to use their names as macros! */
 TPPLexer_YieldPP();
 if (TOK >= KWD_al && TOK <= KWD_dr7) {
  /* Case for almost all registers. */
  reg_name = (rc_t)(TOK-KWD_al);
  id = reg_name & 7;
  reg_name >>= 3;
  reg_name  = register_clases[reg_name];
  /* Don't specify high-order 16-bit
   * registers as 8-bit compatible. */
  if (reg_name == (DCC_RC_I32|DCC_RC_I8) &&
     (id&4)) reg_name &= ~(DCC_RC_I8);
  reg_name |= id;
  YIELD();
 } else if (TOK >= KWD_es && TOK <= KWD_gs) {
  /* Segment register. */
  reg_name = DCC_RC_SEG|(rc_t)(TOK-KWD_es);
  YIELD();
 } else if (TOK == KWD_st) {
  int_t rc_id; YIELD();
  DCCParse_ParPairBegin();
  rc_id = DCCParse_CExpr(0);
  if (rc_id < 0 || rc_id > 7) WARN(W_ASM_INVALID_ST_REGISTER,(int)rc_id);
  DCCParse_ParPairEnd();
  reg_name = DCC_RC_ST|(rc_id&7);
 } else {
  WARN(W_EXPECTED_REGISTER_NAME);
  reg_name = DCC_RR_XAX;
 }
 return reg_name;
}

LEXPRIV void DCC_PARSE_CALL DCCParse_SkipExpr(void) {
 int recursion = 0;
 while (TOK > 0 && TOK != ',') {
       if (TOK == '(') ++recursion;
  else if (TOK == ')' && !recursion--) break;
  YIELD();
 }
}

/* Fix a given type using the value from 'vbottom'. */
LEXPRIV void DCC_PARSE_CALL
DCCParse_FixType(struct DCCType *__restrict type) {
 struct DCCType *real_sym_type = type;
 assert(type);
 /* Special handling for 'auto &x = y;' and 'char (&c)[] = z;' */
 if (DCCTYPE_GROUP(real_sym_type->t_type) == DCCTYPE_LVALUE)
     assert(real_sym_type->t_base),
     real_sym_type = &real_sym_type->t_base->d_type;
 if (real_sym_type->t_base &&
     DCCTYPE_ISBASIC(real_sym_type->t_base->d_type.t_type,DCCTYPE_AUTO) &&
    (DCCTYPE_GROUP(real_sym_type->t_type) == DCCTYPE_ARRAY ||
     DCCTYPE_GROUP(real_sym_type->t_type) == DCCTYPE_VARRAY) &&
     DCCTYPE_GROUP(vbottom->sv_ctype.t_type) == DCCTYPE_ARRAY) {
  /* Special case: 'auto' used as base of array. */
  assert(vbottom->sv_ctype.t_base);
  assert(!real_sym_type->t_base->d_type.t_base);
  DCCType_InitCopy(&real_sym_type->t_base->d_type,
                   &vbottom->sv_ctype.t_base->d_type);
 }

 if (DCCTYPE_ISBASIC(real_sym_type->t_type,DCCTYPE_AUTO)) {
  /* Promote functions here to prevent assignments to function types. */
  DCCStackValue_PromoteFunction(vbottom);
  /* Fix automatically typed variables. */
  assert(!real_sym_type->t_base);
  DCCType_InitCopy(real_sym_type,&vbottom->sv_ctype);
 } else if (DCCTYPE_GROUP(real_sym_type->t_type) == DCCTYPE_VARRAY) {
  size_t array_size;
  if (DCCTYPE_GROUP(vbottom->sv_ctype.t_type) == DCCTYPE_ARRAY) {
   struct DCCDecl *array_decl = vbottom->sv_ctype.t_base;
   assert(array_decl);
   assert(real_sym_type->t_base);
   assert(array_decl->d_kind == DCC_DECLKIND_ARRAY);
   if (!DCCType_IsCompatible(&real_sym_type->t_base->d_type,&array_decl->d_type,1)) {
    WARN(W_INCOMPATIBLE_TYPES_FOR_VARRAY_INITIALIZER,
         &real_sym_type->t_base->d_type,&array_decl->d_type);
   }
   array_size = array_decl->d_tdecl.td_size;
  } else {
   WARN(W_EXPECTED_ARRAY_FOR_VARRAY_INITIALIZER,
        &real_sym_type->t_base->d_type,
        &vbottom->sv_ctype.t_type);
   array_size = 1;
  }
  /* Initialize a variadic array type. */
  DCCType_MkBase(real_sym_type);
  DCCType_MkArray(real_sym_type,array_size);
 }
}


LEXPRIV int DCC_PARSE_CALL
DCCParse_ExprMissingSym(void) {
 struct DCCDecl *decl;
 /* Last chance: Try to declare a new, missing symbol
  *              as an old-style function returning int. */
 if (!TPP_ISKEYWORD(TOK)) return 0;
 WARN(W_UNKNOWN_SYMBOL_IN_EXPRESSION,TOKEN.t_kwd);
 /* Declare a new public symbol. */
 decl = DCCCompiler_NewDecl(TOKEN.t_kwd,DCC_NS_LOCALS);
 if unlikely(!decl) return 0;
 /* Declare as old-style function returning 'int'. */
 DCCType_MkOldFunc(&decl->d_type);
 decl->d_kind = DCC_DECLKIND_MLOC;
 assert(decl->d_mdecl.md_loc.ml_off == 0);
 assert(decl->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
 decl->d_mdecl.md_loc.ml_sym = DCCUnit_NewSym(TOKEN.t_kwd,DCC_SYMFLAG_NONE);
 DCCSym_XIncref(decl->d_mdecl.md_loc.ml_sym);
 YIELD();
 vpushd(decl);
 return 1;
}

LEXPRIV int DCC_PARSE_CALL DCCParse_ExprType(void) {
 struct DCCType type; int error;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 /* Parse a type prefix. - Don't parse a full type due to ambiguity between:
  * >> auto x = int(*)(int) (42); // This just looks weird. - Also: The '(*' is ambiguous
  */
 error = DCCParse_CTypePrefix(&type,&attr);
 if (!error) goto end;
 /* Special case: Still allow array extensions! */
 if (TOK == '[') DCCParse_CTypeSuffix(&type,&attr);
 /* Warn about use of types in expressions. */
 WARN(W_TYPE_IN_EXPRESSION,&type);
 pushf();
 if ((type.t_type&DCCTYPE_STOREMASK) == DCCTYPE_STATIC ||
     (type.t_type&DCCTYPE_STOREMASK) == DCCTYPE_EXTERN
     ) compiler.c_flags |= (DCC_COMPILER_FLAG_SINIT);
 if (TOK == '(' || TOK == KWD___pack) {
  /* Parse a type initializer.
   * NOTE: This is written in a way that also allows: 'int[] ({ 10,20,30 })' */
  DCCParse_ParPairBegin();
  DCCParse_Init(&type,&attr,NULL,1);
  DCCParse_FixType(&type);
  DCCParse_ParPairEnd();
 } else {
  if (TOK != '{') WARN(W_EXPECTED_LPAREN_AFTER_TYPE_IN_EXPRESSION,&type);
  /* Parse a type initializer. */
  DCCParse_Init(&type,&attr,NULL,1);
  DCCParse_FixType(&type);
 }
 popf();
 vcast(&type,0);
end:
 DCCAttrDecl_Quit(&attr);
 DCCType_Quit(&type);
 return error;
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprDefault(void) {
 struct DCCDecl *decl;
 struct TPPKeyword *ident = NULL;
 if (TPP_ISKEYWORD(TOK)) {
  decl = DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_LOCALS);
  if (decl) {
   YIELD();
   if (decl->d_kind&DCC_DECLKIND_TYPE) {
    WARN(W_GOT_TYPE_IN_EXPRESSION);
    YIELD();
    goto push_int0;
   }
   vpushd(decl);
  } else {
   ident = TOKEN.t_kwd;
default_expr:
   if (DCCParse_ExprType()) return;
   if (DCCParse_ExprMissingSym()) return;
   if (ident) {
    WARN(W_UNKNOWN_IDENTIFIER,ident);
    YIELD();
   } else {
    WARN(W_UNEXPECTED_TOKEN_IN_C_EXPRESSION);
   }
push_int0: /* fallback... */
   vpushi(DCCTYPE_INT,0);
  }
 } else {
  goto default_expr;
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprGeneric(void) {
 struct DCCType ctrl_type;
 int found_target = 0;
 assert(TOK == KWD__Generic);
 WARN(W_GENERIC_EXPRESSION_C11);
 YIELD();
 DCCParse_ParPairBegin();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr1();
 popf();
 ctrl_type = vbottom->sv_ctype;
 vbottom->sv_ctype.t_base = NULL; /* Inherit reference. */
 vpop(1);
 /* Promote function to pointer-to-function. */
 if (DCCTYPE_GROUP(ctrl_type.t_type) == DCCTYPE_FUNCTION)
     DCCType_MkPointer(&ctrl_type);

 /* Now parse all the cases. */
 for (;;) {
  int is_target;
  if (TOK != ',') break;
  YIELD();
  if (TOK == KWD_default) {
   /* Default case */
   is_target = found_target ? 0 : 2;
   YIELD();
  } else {
   struct DCCType     case_type;
   struct DCCAttrDecl case_attr = DCCATTRDECL_INIT;
   struct TPPKeyword *type_name;
   type_name = DCCParse_CType(&case_type,&case_attr);
   DCCAttrDecl_Quit(&case_attr);
   if (!type_name) { WARN(W_GENERIC_EXPRESSION_EXPECTED_TYPE,&ctrl_type); continue; }
   is_target = DCCType_IsCompatible(&case_type,&ctrl_type,1);
   DCCType_Quit(&case_type);
  }
  if (TOK != ':') WARN(W_GENERIC_EXPRESSION_EXPECTED_COLON,&ctrl_type); else YIELD();
  if (!is_target || found_target) {
   if (is_target && found_target) {
    WARN(W_GENERIC_EXPRESSION_SECOND_MATCH,&ctrl_type);
   }
   pushf();
   compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
   DCCParse_Expr1();
   popf();
   vpop(1);
  } else {
   DCCParse_Expr1();
   found_target = 1;
  }
  if (is_target == 2 && TOK == ',') {
   WARN(W_GENERIC_EXPRESSION_DEFAULT_NONLAST,&ctrl_type);
  }
 }
 if (!found_target) {
  WARN(W_GENERIC_EXPRESSION_NO_MATCH,&ctrl_type);
  vpushv();
 }
 DCCType_Quit(&ctrl_type);
 DCCParse_ParPairEnd();
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprIf(void) {
 struct DCCSym *tt_label,*old_deadjmp;
 int is_true,is_dead = 0;
 assert(TOK == KWD_if);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 DCCParse_ParPairEnd();
 is_true = visconst_bool() ? vgtconst_bool() : 2;
 tt_label = DCCUnit_AllocSym();
 if unlikely(!tt_label) return;
 DCCVStack_KillAll(); /* Kill all registers before the jump. */
 vpushs(tt_label);
 vgen1('&');
 vjcc(1);             /* Jump across the true-branch. */
 pushf();
 if (is_true == 0) {
  compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                           DCC_COMPILER_FLAG_DEAD);
  old_deadjmp = compiler.c_deadjmp;
  compiler.c_deadjmp = tt_label;
  DCCParse_Expr1();     /* Generate the true-branch. */
  compiler.c_deadjmp = old_deadjmp;
 } else {
  DCCParse_Expr1();     /* Generate the true-branch. */
 }
 if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) is_dead |= 1|(is_true == 2 ? 2 : 0);
 popf();
 if (TOK != KWD_else) {
  if (is_dead == (1|2)) compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                                                 DCC_COMPILER_FLAG_DEAD);
  vpop(1);
  /* This is where we jump to skip the false-branch. */
  t_defsym(tt_label);
  vpushv();
 } else {
  struct DCCSym *ff_label;
  struct DCCStackValue shared_storage;
  /* At this point, we have parsed the true-branch,
   * who's value is not in 'vbottom'. */
  if ((vbottom->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_COPY)) ||
      (vbottom->sv_reg == DCC_RC_CONST) ||
       /* Always copy protected registers. This can happen if the user writes something like:
        * >> x = foo() ? %esp : %ebp;
        * In this situation, the code must look something like this:
        * >>     call foo
        * >>     test %eax, %eax
        * >>     je ff
        * >> tt: mov %esp, %eax
        * >>     jmp 1f
        * >> ff: mov %ebp, %eax
        * >> 1:  mov %eax, x
        * Without this additional check, '%esp' would be used as temporary register!
        */
       DCCStackValue_ISPROTECTED(vbottom)) {
   /* Special case: When the true-branch is an l-value,
    *               it is likely that it actually describes
    *               a store operation to a local variable.
    *               With that in mind, the false branch may
    *               not write to that same variable, but
    *               instead must share a register-storage
    *               together with the true-branch.
    *            >> For that reason, we must load the true-branch
    *               into volatile register storage that can be
    *               shared with the false-branch.
    * WARNING: Since not everything can fit into register storage,
    *          sadly we must sometimes allocate a copy on the stack.
    */
   if (DCCType_Sizeof(&vbottom->sv_ctype,NULL,1) <= DCC_TARGET_SIZEOF_POINTER) {
    DCCStackValue_Load(vbottom);
   } else {
    /* Use the stack for shared storage.
     * WARNING: This may potentially be wasteful... */
    DCCStackValue_Kill(vbottom);
   }
  }
  shared_storage = *vbottom; /* Keep track of how the true-branch was stored (so that the false-branch can mirror it). */
  ++vbottom;               /* Don't destroy this. */
  ff_label = DCCUnit_AllocSym();
  /* dirty hack! (This is only used to prevent DCC from crashing)
   * >> It has no syntactic meaning! */
  if unlikely(!ff_label) ff_label = tt_label;
  vpushs(ff_label);
  vgen1('&');
  vjmp(); /* Jump across the false-branch, when we're from the true-branch. */
  /* This is where we jump if the condition failed. */
  t_defsym(tt_label);

  /* Parse the false-branch. */
  pushf();
  YIELD();
  if (is_true == 1) {
   old_deadjmp = compiler.c_deadjmp;
   compiler.c_deadjmp = ff_label;
   DCCParse_Expr1();
   compiler.c_deadjmp = old_deadjmp;
  } else {
   if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) is_dead |= 2;
  }
  popf();

  if (is_dead == (1|2)) {
   /* When both branches are dead, we know that the entire expression is, too. */
   compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  }
  /* Now that the false-branch has been generated,
   * we must store its value in the shared storage. */
  --vbottom,*vbottom = shared_storage;
  /* Duplicate the shared storage virtual value, but don't copy it for real */
  vdup(0);   /* ..., rhs, shared, shared */
  vrrot(3);  /* ..., shared, shared, rhs */
  vstore(1); /* ..., shared, shared */
  vpop(0);   /* ..., shared */
  /* This is where we jump to skip the false-branch. */
  t_defsym(ff_label);
 }

}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprUnary(void) {
 switch (TOK) {

 { /* Push an immediate, constant integral. */
  int_t intval;
  int int_kind;
  tyid_t tyid;
 case TOK_INT:
 case TOK_CHAR:
  int_kind = TPP_Atoi(&intval);
  if unlikely(!int_kind) goto push_int0;
  YIELD();
  tyid = 0;
  if (int_kind&TPP_ATOI_UNSIGNED) tyid |= DCCTYPE_UNSIGNED;
  switch (int_kind&TPP_ATOI_TYPE_MASK) {
   case TPP_ATOI_TYPE_LONG    : tyid |= DCCTYPE_ALTLONG|DCCTYPE_LONG; break;
   case TPP_ATOI_TYPE_LONGLONG: tyid |= DCCTYPE_LLONG; break;
   case TPP_ATOI_TYPE_INT8    : tyid |= DCCTYPE_INT8; break;
   case TPP_ATOI_TYPE_INT16   : tyid |= DCCTYPE_INT16; break;
   case TPP_ATOI_TYPE_INT32   : tyid |= DCCTYPE_INT32; break;
   case TPP_ATOI_TYPE_INT64   : tyid |= DCCTYPE_INT64; break;
   default: break;
  }
  vpushi(tyid,intval);
 } break;

 { /* Allocate and push a string. */
  struct TPPString *str;
 case TOK_STRING:
parse_string:
  str = DCCParse_String();
  if unlikely(!str) goto push_int0;
  DCCVStack_PushStr(str->s_text,str->s_size);
  TPPString_Decref(str);
 } break;

 {
  /* Push an assembly register.
   * >> This is actually a pretty interesting extension.
   *    Even though there is no guaranty that DCC won't
   *    simply clobber any of the registers you've
   *    modified between statements, it still allows
   *    for some _pretty_ darn interesting code:
   * >> int add(int x, int y) {
   * >>   // This generates pretty much the same
   * >>   // code as 'return x+y' would have.
   * >>   %eax = x;
   * >>   %eax += y;
   * >>   return %eax;
   * >> }
   * This extension also makes it very easy and straight-forward
   * to test the code generated by various kinds of expressions.
   */
  struct DCCStackValue sval;
 case '%':
  if (!HAS(EXT_ASM_REGISTERS)) break;
  /* Push an explicitly defined register stack value. */
  sval.sv_reg          = DCCVStack_KillXNon(DCCParse_Register());
  sval.sv_reg2         = DCC_RC_CONST;
  /* Setting the explicit register flag here forces
   * a register copy in a situation like the following:
   * >> int x = 10,y;
   * >> y = %eax+x;
   * If the flag isn't set:
   * >> add x, %eax    # EAX is always modified by the operation.
   * >> mov %eax, y
   * If the flag is set:
   * >> mov %eax, %ecx # Copy explicit register operands before using them
   * >> add x, %ecx
   * >> mov %ecx, y
   */
  sval.sv_flags        = DCC_SFLAG_XREGISTER;
  sval.sv_const.it     = 0;
  sval.sv_sym          = NULL;
  sval.sv_ctype.t_base = NULL;
  sval.sv_ctype.t_type = DCC_RC_GETTYPE(sval.sv_reg);
  if (TOK == ':' && *peek_next_token() == '%') {
   /* Parse a register pair.
    * NOTE: Here, both the first & second register must be 32-bit wide. */
   YIELD();
   if (TOK != '%') WARN(W_EXPECTED_PERCENT_BEFORE_REGISTER_NAME);
   else {
    sval.sv_reg2 = DCCParse_Register();
    if ((sval.sv_reg&DCC_RC_I3264) != DCC_RC_I32 ||
        (sval.sv_reg2&DCC_RC_I3264) != DCC_RC_I32) {
     WARN(W_INVALID_REGISTER_PAIR);
     sval.sv_reg2 = DCC_RC_CONST;
     /* Don't allow invalid register pairs. */
    } else {
     sval.sv_reg2 = DCCVStack_KillXNon(sval.sv_reg2);
     sval.sv_ctype.t_type = DCCTYPE_INT64|DCCTYPE_UNSIGNED;
    }
   }
  }
  vpush(&sval);
 } break;

 { /* GCC-style label addressing. */
 case TOK_LAND:
  if (!HAS(EXT_GCC_LABEL_EXPR)) goto default_case;
  YIELD();
  if (TPP_ISKEYWORD(TOK)) {
   struct DCCDecl *label_decl = DCCCompiler_NewLabel(TOKEN.t_kwd);
   if unlikely(!label_decl) goto push_int0;
   vpushd(label_decl);
   vgen1('&');
   YIELD();
  } else {
   WARN(W_EXPECTED_KEYWORD_AFTER_LABEL_ADDRESS);
   goto push_int0;
  }
 } break;

 {
 case '.':
  if (!HAS(EXT_ASM_ADDRESS)) goto default_case;
  YIELD();
  /* Push the current text address. */
#if 1 /* Use an anonymous symbol to allow for relocation. */
  {
   struct DCCSym *sym = DCCUnit_AllocSym();
   sym ? vpushs(sym),t_defsym(sym)
       : DCCVStack_PushAddr(unit.u_curr,t_addr); /* fallback... */
  }
#else
  DCCVStack_PushAddr(unit.u_curr,t_addr);
#endif
  vgen1('&');
 } break;

 {
  tok_t t;
 case '+':
 case '-':
 case '*':
 case '&':
 case '~':
 case '!':
  t = TOK;
  YIELD();
  DCCParse_ExprUnary();
  if (t == '&' || t == '*') {
   /* don't duplicate the value for this operator! */
   vgen1(t); /* x */
  } else if (t != '+') {
   vdup(1);  /* x, dx  */
   vgen1(t); /* x, *dx */
   vswap();  /* *dx, x */
   vpop(1);  /* *dx */
  }
 } break;

 case KWD___pack:
 case '(': {
  struct DCCType     cast_type;
  struct DCCAttrDecl cast_attr = DCCATTRDECL_INIT;
  struct TPPKeyword *cast_name;
  tok_t t = TOK;
  YIELD();
  if (TOK == '{' && HAS(EXT_GCC_EXPRSTMT)) {
   /* GCC statement expressions. */
   DCCParse_Scope(DCC_PFLAG_USED);
   goto check_rparen_break;
  }
  cast_name = DCCParse_CType(&cast_type,&cast_attr);
  DCCAttrDecl_Quit(&cast_attr);
  if (cast_name) {
   /* C-style cast. */
   if (t != KWD___pack) {
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    else YIELD();
   }
   /* Extension: Allow initializer here. */
   /* TODO: Add a switch for this! */
   pushf();
   if ((cast_type.t_type&DCCTYPE_STOREMASK) == DCCTYPE_STATIC ||
       (cast_type.t_type&DCCTYPE_STOREMASK) == DCCTYPE_EXTERN
       ) compiler.c_flags |= (DCC_COMPILER_FLAG_SINIT);
   DCCParse_Init(&cast_type,&cast_attr,NULL,1);
   DCCParse_FixType(&cast_type);
   vcast(&cast_type,1);
   popf();
   DCCType_Quit(&cast_type);
  } else {
   /* Parenthesis. */
   DCCParse_Expr();
check_rparen_break:
   if (t != KWD___pack) {
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    else YIELD();
   }
  }
 } break;

 {
  tok_t t;
 case TOK_INC:
 case TOK_DEC:
  t = TOK;
  if (DCC_MACRO_FALSE) { case TOK_TILDE_TILDE: t = '~'; }
  YIELD();
  DCCParse_ExprUnary();
  vgen1(t);
  break;
 }

 { /* Expand to the string representation of the current function. */
  char const *name; size_t size;
 case KWD___FUNCTION__:
  if (HAS(EXT_FUNCTION_STRING_LITERALS)) goto parse_string;
 case KWD___func__:
  if (!compiler.c_fun) {
outside_function:
   WARN(W_EXPR_FUNC_OUTSIDE_OF_FUNCTION);
   name = NULL;
   size = 0;
  } else {
   struct TPPKeyword const *name_kwd;
   name_kwd = compiler.c_fun->d_name;
   assert(name_kwd);
   name = name_kwd->k_name;
   size = name_kwd->k_size;
  }
  DCCVStack_PushStr(name,size);
  YIELD();
  break;
 }

 { /* Push the name + prototype of the current function as string. */
  struct TPPString *proto;
 case KWD___PRETTY_FUNCTION__:
  if (HAS(EXT_FUNCTION_STRING_LITERALS)) goto parse_string;
  if unlikely(!compiler.c_fun) goto outside_function;
  proto = DCCType_ToTPPString(&compiler.c_fun->d_type,
                               compiler.c_fun->d_name);
  DCCVStack_PushStr(proto->s_text,
                    proto->s_size);
  TPPString_Decref(proto);
  YIELD();
 } break;

 { /* sizeof(...), alignof(...) */
  tok_t t;
  struct TPPKeyword *function;
 case KWD___builtin_alignof:
 case KWD_sizeof:
 case KWD__Alignof:
 case KWD___alignof:
 case KWD___alignof__:
  t = TOK,function = TOKEN.t_kwd;
  YIELD();
  /* Missing parenthesis here degrades code
   * quality as far as I'm concerned... */
  if (TOK != '(' && TOK != KWD___pack)
   WARN(W_SIZEOF_WITHOUT_PARENTHESIS,function);
  DCCParse_ParPairOptBegin();
  {
   target_ptr_t res_size,res_align;
   struct DCCType     query_type;
   struct DCCAttrDecl query_attr = DCCATTRDECL_INIT;
   struct TPPKeyword *query_name;
   struct DCCType    *used_type;
   query_name = DCCParse_CType(&query_type,&query_attr);
   DCCAttrDecl_Quit(&query_attr);
   if (query_name) {
    if (query_name != &TPPKeyword_Empty)
     WARN(W_CONTAINED_TYPENAME_IGNORED,query_name);
    used_type = &query_type;
   } else {
    pushf();
    compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
    DCCParse_ParPairOptHas() ? DCCParse_Expr() : DCCParse_ExprUnary();
    popf();
    used_type = &vbottom->sv_ctype;
   }
   if (!DCCType_IsComplete(used_type)) WARN(W_SIZEOF_INCOMPLETE_TYPE,used_type);
   res_size = DCCType_Sizeof(used_type,&res_align,0);
   if (DCCTYPE_GROUP(used_type->t_type) == DCCTYPE_FUNCTION ||
       DCCTYPE_ISBASIC(used_type->t_type,DCCTYPE_VOID)) {
    if (HAS(EXT_VOID_ARITHMETIC)) res_size = res_align = 1;
    else WARN(W_SIZEOF_VOID_OR_FUNCTION,used_type);
   }
   if (t == KWD_sizeof) {
    /* Warn about size queries on VLA array types. */
    if (DCCTYPE_GROUP(used_type->t_type) == DCCTYPE_ARRAY &&
       (used_type->t_base->d_kind == DCC_DECLKIND_VLA)
        ) WARN(W_SIZEOF_VLA_ARRAY_TYPE,used_type);
    DCCVStack_PushSizeof(used_type);
    if (!query_name) vswap(),vpop(1);
   } else {
    if (!query_name) vpop(1);
    if (t != KWD_sizeof) res_size = res_align;
    vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,res_size);
   }
   DCCType_Quit(&query_type);
  }
  DCCParse_ParPairOptEnd();
 } break;

 /* Builtin functions. */
 case KWD___builtin_typestr: DCCParse_BuiltinTypeStr(); break;
 case KWD___builtin_constant_p: DCCParse_BuiltinConstantP(); break;
 case KWD___builtin_choose_expr: DCCParse_BuiltinChooseExpr(); break;
 case KWD___builtin_types_compatible_p: DCCParse_BuiltinTypesCompatibleP(); break;
 case KWD___builtin_unreachable: case KWD___builtin_breakpoint:
 case KWD___builtin_trap: case KWD___builtin_sfence:
 case KWD___builtin_lfence: case KWD___builtin_mfence: DCCParse_BuiltinFence(); break;
 case KWD___builtin_alloca: DCCParse_BuiltinAlloca(); break;
 case KWD___builtin_alloca_with_align: DCCParse_BuiltinAllocaWithAlign(); break;
 case KWD___builtin_assume: case KWD___assume: DCCParse_BuiltinAssume(); break;
 case KWD___builtin_expect: DCCParse_BuiltinExpect(); break;
 case KWD___builtin_FILE: DCCParse_BuiltinFILE(); break;
 case KWD___builtin_LINE: DCCParse_BuiltinLINE(); break;
 case KWD___builtin_FUNCTION: DCCParse_BuiltinFUNCTION(); break;
 case KWD___builtin_bitfield: DCCParse_BuiltinBitField(); break;
 case KWD___builtin_offsetof: DCCParse_BuiltinOffsetof(); break;
 case KWD___builtin_bswap16: case KWD___builtin_bswap32:
 case KWD___builtin_bswap64: case KWD___builtin_bswapcc: DCCParse_BuiltinBSwap(); break;
 case KWD___builtin_va_start: DCCParse_BuiltinVaStart(); break;
 case KWD___builtin_va_copy: DCCParse_BuiltinVaCopy(); break;
 case KWD___builtin_va_end: DCCParse_BuiltinVaEnd(); break;
 case KWD___builtin_va_arg: DCCParse_BuiltinVaArg(); break;
 case KWD___builtin_setjmp: DCCParse_BuiltinSetJmp(); break;
 case KWD___builtin_longjmp: DCCParse_BuiltinLongJmp(); break;
  /* Builtin string operations. */
 case KWD___builtin_memcpy:
 case KWD___builtin_memmove: DCCParse_BuiltinMemcpy(); break;
 case KWD___builtin_memset: DCCParse_BuiltinMemset(); break;
 case KWD___builtin_memcmp: DCCParse_BuiltinMemcmp(); break;
 case KWD___builtin_strlen: DCCParse_BuiltinStrlen(); break;
  /* bit-scanning functions. */
 case KWD___builtin_ffs:  case KWD___builtin_ffsll:
 case KWD___builtin_ffsl: case KWD___builtin_ffscc:
 case KWD___builtin_clz:  case KWD___builtin_clzll:
 case KWD___builtin_clzl: case KWD___builtin_clzcc: DCCParse_BuiltinScanner(); break;
 case KWD___builtin_assume_aligned: DCCParse_BuiltinAssumeAligned(); break;
  /* Atomic (aka. __sync_*) builtins */
 case KWD___sync_val_compare_and_swap:
 case KWD___sync_bool_compare_and_swap: DCCParse_SyncCompareAndSwap(); break;
 case KWD___sync_add:   case KWD___sync_fetch_and_add:   case KWD___sync_add_and_fetch:
 case KWD___sync_sub:   case KWD___sync_fetch_and_sub:   case KWD___sync_sub_and_fetch:
 case KWD___sync_or:    case KWD___sync_fetch_and_or:    case KWD___sync_or_and_fetch:
 case KWD___sync_and:   case KWD___sync_fetch_and_and:   case KWD___sync_and_and_fetch:
 case KWD___sync_xor:   case KWD___sync_fetch_and_xor:   case KWD___sync_xor_and_fetch:
 case KWD___sync_nand:  case KWD___sync_fetch_and_nand:  case KWD___sync_nand_and_fetch:
 case KWD___sync_store: case KWD___sync_fetch_and_store: case KWD___sync_store_and_fetch:
 case KWD___sync_lock_test_and_set: DCCParse_SyncBinary(); break;
 case KWD___sync_inc: case KWD___sync_fetch_and_inc: case KWD___sync_inc_and_fetch:
 case KWD___sync_dec: case KWD___sync_fetch_and_dec: case KWD___sync_dec_and_fetch:
 case KWD___sync_neg: case KWD___sync_fetch_and_neg: case KWD___sync_neg_and_fetch:
 case KWD___sync_not: case KWD___sync_fetch_and_not: case KWD___sync_not_and_fetch:
 case KWD___sync_lock_release: DCCParse_SyncUnary(); break;

 case KWD__Generic: DCCParse_ExprGeneric(); break;

 case KWD_if:
  if (!HAS(EXT_IFELSE_IN_EXPR)) goto default_case;
  DCCParse_ExprIf();
  break;

 default:default_case:
  DCCParse_ExprDefault();
  break;
 }
 DCCParse_ExprUnarySuffix();
 return;
push_int0: /* fallback... */
 vpushi(DCCTYPE_INT,0);
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprProd(void) {
 tok_t func;
 DCCParse_ExprUnary();
 while (TOK == '*' || TOK == '/' || TOK == '%') {
  func = TOK;
  YIELD();
  vdup(1);              /* x, dx */
  vswap();              /* dx, x */
  vpop(1);              /* dx */
  DCCParse_ExprUnary(); /* dx, y */
  vgen2(func);          /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprSum(void) {
 tok_t func;
 DCCParse_ExprProd();
 while (TOK == '+' || TOK == '-') {
  func = TOK;
  YIELD();
  vdup(1);             /* x, dx */
  vswap();             /* dx, x */
  vpop(1);             /* dx */
  DCCParse_ExprProd(); /* dx, y */
  vgen2(func);         /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprShift(void) {
 tok_t func;
 DCCParse_ExprSum();
 while (TOK == TOK_SHL || TOK == TOK_SHR) {
  func = TOK;
  YIELD();
  vdup(1);            /* x, dx */
  vswap();            /* dx, x */
  vpop(1);            /* dx */
  DCCParse_ExprSum(); /* dx, y */
  vgen2(func);        /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprCmp(void) {
 tok_t func;
 DCCParse_ExprShift();
 while (TOK == TOK_LOWER ||
        TOK == TOK_LOWER_EQUAL ||
        TOK == TOK_GREATER ||
        TOK == TOK_GREATER_EQUAL) {
  func = TOK;
  YIELD();
  vdup(1);              /* x, dx */
  vswap();              /* dx, x */
  vpop(1);              /* dx */
  DCCParse_ExprShift(); /* dx, y */
  vgen2(func);          /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprCmpEq(void) {
 tok_t func;
 DCCParse_ExprCmp();
 while (TOK == TOK_EQUAL ||
        TOK == TOK_NOT_EQUAL) {
  func = TOK;
  YIELD();
  vdup(1);            /* x, dx */
  vswap();            /* dx, x */
  vpop(1);            /* dx */
  DCCParse_ExprCmp(); /* dx, y */
  vgen2(func);        /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprAnd(void) {
 DCCParse_ExprCmpEq();
 while (TOK == '&') {
  YIELD();
  vdup(1);              /* x, dx */
  vswap();              /* dx, x */
  vpop(1);              /* dx */
  DCCParse_ExprCmpEq(); /* dx, y */
  vgen2('&');           /* dx#y */
 }
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprXor(void) {
 DCCParse_ExprAnd();
 while (TOK == '^') {
  YIELD();
  vdup(1);            /* x, dx */
  vswap();            /* dx, x */
  vpop(1);            /* dx */
  DCCParse_ExprAnd(); /* dx, y */
  vgen2('^');         /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprOr(void) {
 DCCParse_ExprXor();
 while (TOK == '|') {
  YIELD();
  vdup(1);            /* x, dx */
  vswap();            /* dx, x */
  vpop(1);            /* dx */
  DCCParse_ExprXor(); /* dx, y */
  vgen2('|');         /* dx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprLAnd(void) {
 struct DCCSym *sym = NULL;
 DCCParse_ExprOr();
 while (TOK == TOK_LAND) {
  YIELD();
  if (!sym && (sym = DCCUnit_AllocSym()) == NULL) return;
  if (visconst_bool()) {
   if (vgtconst_bool()) {
    vpop(1);
    DCCParse_ExprOr();
   } else {
    struct DCCSym *old_deadjmp;
    pushf();
    old_deadjmp = compiler.c_deadjmp;
    compiler.c_deadjmp = sym;
    compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                             DCC_COMPILER_FLAG_DEAD);
    DCCParse_ExprOr();
    compiler.c_deadjmp = old_deadjmp;
    popf();
    vpop(1);
   }
  } else {
   DCCVStack_KillAll(); /* Kill all registers before doing the jump.
                         * NOTE: This mustn't modify EFLAGS, so we're safe! */
   vpushs(sym);
   vgen1('&');
   vjcc(1);           /* Jump over the second operand(s) if first was false. */
   DCCParse_ExprOr(); /* Parse the second operand. */
  }
  vgen1('!'),vgen1('!'); /* Force the second expression into a boolean. */
 }
 if (sym) t_defsym(sym);
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprLXor(void) {
 DCCParse_ExprLAnd();
 while (TOK == TOK_LXOR && HAS(EXT_LXOR)) {
  YIELD();
  vdup(1);               /* x, dx */
  vswap();               /* dx, x */
  vpop(1);                /* dx */
  DCCParse_ExprLAnd();   /* dx, y */
  vgen2('^');            /* dx#y */
  vgen1('!'),vgen1('!'); /* Force to boolean. */
 }
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprLOr(void) {
 struct DCCSym *sym = NULL;
 DCCParse_ExprLXor();
 while (TOK == TOK_LOR) {
  YIELD();
  if (!sym && (sym = DCCUnit_AllocSym()) == NULL) return;
  if (visconst_bool()) {
   if (vgtconst_bool()) {
    struct DCCSym *old_deadjmp;
    pushf();
    old_deadjmp = compiler.c_deadjmp;
    compiler.c_deadjmp = sym;
    compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                             DCC_COMPILER_FLAG_DEAD);
    DCCParse_ExprLXor();
    compiler.c_deadjmp = old_deadjmp;
    popf();
    vpop(1);
   } else {
    vpop(1);
    DCCParse_ExprLXor();
   }
  } else {
   DCCVStack_KillAll(); /* Kill all registers before doing the jump.
                         * NOTE: This mustn't modify EFLAGS, so we're safe! */
   vpushs(sym);
   vgen1('&');
   vjcc(0);             /* Jump over the second operand(s) if first was true. */
   DCCParse_ExprLXor(); /* Parse the second operand. */
  }
  vgen1('!'),vgen1('!'); /* Force the second expression into a boolean. */
 }
 if (sym) t_defsym(sym);
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprCond(void) {
 DCCParse_ExprLOr();
 if (TOK == '?') {
  YIELD();
#if 1 /* Special optimization for handling constants. */
  if (visconst_bool()) {
   tyid_t tt_type;
   struct DCCSym *old_deadjmp;
   struct DCCSym *jmp_sym = NULL,*jmp_sym2 = NULL;
   int is_true = vgtconst_bool();
   /* The condition is a constant boolean. */
   if (TOK == ':' && HAS(EXT_GCC_IFELSE)) {
    /* _very_ simple case: Don't do anything, act
     * as though the condition was the true-branch. */
   } else {
    vpop(1);
    if (!is_true) {
     pushf();
     jmp_sym = DCCUnit_AllocSym();
     if unlikely(!jmp_sym) return;
     old_deadjmp = compiler.c_deadjmp;
     compiler.c_deadjmp = jmp_sym;
     compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                              DCC_COMPILER_FLAG_DEAD);
     DCCParse_Expr();
     compiler.c_deadjmp = old_deadjmp;
     /* If the true branch contained a label,
      * we must generate a jump across false. */
     if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD)) {
      jmp_sym2 = DCCUnit_AllocSym();
      if unlikely(!jmp_sym2) return;
      vpushs(jmp_sym2),vgen1('&'),vjmp();
     }
     popf();
    } else {
     DCCParse_Expr();
    }
   }
   tt_type = vbottom->sv_ctype.t_type;
   if (!is_true) vpop(1); /* Pop the true-branch if it's unwanted. */
   if (jmp_sym) t_defsym(jmp_sym);
   if (TOK != ':') {
    WARN(W_EXPECTED_COLLON_AFTER_QUESTION);
    /* Push a constant ZERO */
    vpushi(tt_type&DCCTYPE_BASICMASK,0);
   } else {
    YIELD();
    if (is_true) {
     pushf();
     if (!jmp_sym2) {
      jmp_sym2 = DCCUnit_AllocSym();
      if unlikely(!jmp_sym2) return;
     }
     old_deadjmp = compiler.c_deadjmp;
     compiler.c_deadjmp = jmp_sym2;
     compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                              DCC_COMPILER_FLAG_DEAD);
     DCCParse_ExprCond();
     compiler.c_deadjmp = old_deadjmp;
     popf();
    } else {
     DCCParse_ExprCond();
    }
   }
   if (jmp_sym2) t_defsym(jmp_sym2);
   if (is_true) vpop(1); /* Pop the false-branch if it's unwanted. */
  } else
#endif
   /* TODO: Cast to common type? */
  {
   struct DCCSym *tt_label,*ff_label;
   struct DCCStackValue shared_storage;
   int is_dead = 0;
   tt_label = DCCUnit_AllocSym();
   if unlikely(!tt_label) return;
   if (TOK == ':' && HAS(EXT_GCC_IFELSE)) {
    /* Special case: gcc's if-else. */
    vdup(1);             /* Duplicate the condition for use as true-operand. */
    DCCVStack_KillAll(); /* Kill all registers (including the condition value). */
    vpushs(tt_label);
    vgen1('&');
    vjcc(1);             /* Jump across the true-branch. */
   } else {
    DCCVStack_KillAll(); /* Kill all registers before the jump. */
    vpushs(tt_label);
    vgen1('&');
    vjcc(1);             /* Jump across the true-branch. */
    pushf();
    DCCParse_Expr();     /* Generate the true-branch. */
    if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) is_dead |= 1;
    popf();
   }
   /* At this point, we have parsed the true-branch,
    * who's value is not in 'vbottom'. */
   if ((vbottom->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_COPY)) ||
       (vbottom->sv_reg == DCC_RC_CONST) ||
        /* Always copy protected registers. This can happen if the user writes something like:
         * >> x = foo() ? %esp : %ebp;
         * In this situation, the code must look something like this:
         * >>     call foo
         * >>     test %eax, %eax
         * >>     je ff
         * >> tt: mov %esp, %eax
         * >>     jmp 1f
         * >> ff: mov %ebp, %eax
         * >> 1:  mov %eax, x
         * Without this additional check, '%esp' would be used as temporary register!
         */
        DCCStackValue_ISPROTECTED(vbottom)) {
    /* Special case: When the true-branch is an l-value,
     *               it is likely that it actually describes
     *               a store operation to a local variable.
     *               With that in mind, the false branch may
     *               not write to that same variable, but
     *               instead must share a register-storage
     *               together with the true-branch.
     *            >> For that reason, we must load the true-branch
     *               into volatile register storage that can be
     *               shared with the false-branch.
     * WARNING: Since not everything can fit into register storage,
     *          sadly we must sometimes allocate a copy on the stack.
     */
    if (DCCType_Sizeof(&vbottom->sv_ctype,NULL,1) <= DCC_TARGET_SIZEOF_POINTER) {
     DCCStackValue_Load(vbottom);
    } else {
     /* Use the stack for shared storage.
      * WARNING: This may potentially be wasteful... */
     DCCStackValue_Kill(vbottom);
    }
   }
   shared_storage = *vbottom; /* Keep track of how the true-branch was stored (so that the false-branch can mirror it). */
   ++vbottom;               /* Don't destroy this. */
   ff_label = DCCUnit_AllocSym();
   /* dirty hack! (This is only used to prevent DCC from crashing)
    * >> It has no syntactic meaning! */
   if unlikely(!ff_label) ff_label = tt_label;
   vpushs(ff_label);
   vgen1('&');
   vjmp(); /* Jump across the false-branch, when we're from the true-branch. */
   /* This is where we jump if the condition failed. */
   t_defsym(tt_label);

   /* Parse the false-branch. */
   if (TOK != ':') {
    WARN(W_EXPECTED_COLLON_AFTER_QUESTION);
    /* Push a constant ZERO */
    vpushi(shared_storage.sv_ctype.t_type&DCCTYPE_BASICMASK,0);
   } else {
    YIELD();
    pushf();
    DCCParse_ExprCond();
    if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) is_dead |= 2;
    popf();
   }

   if (is_dead == (1|2)) {
    /* When both branches are dead, we know that the entire expression is, too. */
    compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
   }
   /* Now that the false-branch has been generated,
    * we must store its value in the shared storage. */
   --vbottom,*vbottom = shared_storage;
   /* Duplicate the shared storage virtual value, but don't copy it for real */
   vdup(0);   /* ..., rhs, shared, shared */
   vrrot(3);  /* ..., shared, shared, rhs */
   vstore(1); /* ..., shared, shared */
   vpop(0);   /* ..., shared */

   /* This is where we jump to skip the false-branch. */
   t_defsym(ff_label);
  }
 }
}

PUBLIC void DCC_PARSE_CALL DCCParse_Expr1(void) {
 DCCParse_ExprCond();
 for (;;) switch (TOK) {
 case '=':
  YIELD();
  /* Warn about brace-initializer assignment. */
  if (TOK == '{') {
   WARN(W_BRACE_INITIALIZER_DURING_ASSIGNMENT,
        &vbottom->sv_ctype);
  }
  /* Set the initializer target to directly modify vbottom */
  if ((vbottom->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_BITFLD|DCC_SFLAG_TEST)) ==
                         (DCC_SFLAG_LVALUE)) {
   struct DCCMemLoc target;
   target.ml_reg = vbottom->sv_reg;
   target.ml_sym = vbottom->sv_sym;
   target.ml_off = vbottom->sv_const.offset;
   DCCParse_Init(&vbottom->sv_ctype,NULL,&target,0);
   vpop(0);
  } else {
   DCCParse_Init(&vbottom->sv_ctype,NULL,NULL,0);
   vstore(0);
  }
  break;

 { /* Inplace operations. */
  tok_t t;
  if (DCC_MACRO_FALSE) { case TOK_ADD_EQUAL: t = '+'; }
  if (DCC_MACRO_FALSE) { case TOK_SUB_EQUAL: t = '-'; }
  if (DCC_MACRO_FALSE) { case TOK_MUL_EQUAL: t = '*'; }
  if (DCC_MACRO_FALSE) { case TOK_DIV_EQUAL: t = '/'; }
  if (DCC_MACRO_FALSE) { case TOK_MOD_EQUAL: t = '%'; }
  if (DCC_MACRO_FALSE) { case TOK_SHL_EQUAL: t = TOK_SHL; }
  if (DCC_MACRO_FALSE) { case TOK_SHR_EQUAL: t = TOK_SHR; }
  if (DCC_MACRO_FALSE) { case TOK_AND_EQUAL: t = '&'; }
  if (DCC_MACRO_FALSE) { case TOK_OR_EQUAL:  t = '|'; }
  if (DCC_MACRO_FALSE) { case TOK_XOR_EQUAL: t = '^'; }
  YIELD();
  DCCParse_Expr1();
  vgen2(t);
 } break;

 default: goto done;
 }
done:;
}

PUBLIC void DCC_PARSE_CALL DCCParse_Expr(void) {
 for (;;) {
  DCCParse_Expr1();
  if (TOK != ',') break;
  YIELD();
  vpop(1);
 }
}
PUBLIC size_t DCC_PARSE_CALL DCCParse_Exprn(void) {
 size_t result = 0;
 if (DCCParse_IsExpr()) while (TOK > 0) {
  DCCParse_Expr1();
  ++result;
  if (TOK != ',') break;
  YIELD();
 }
 return result;
}

DCCFUN void DCC_PARSE_CALL
DCCParse_ExprDiscard(void) {
 while (TOK > 0) {
  DCCParse_Expr1();
  vpop(1);
  if (TOK != ',') break;
  YIELD();
 }
}
DCCFUN int DCC_PARSE_CALL
DCCParse_IsExpr(void) {
 /* TODO: This can be done better! */
 return TOK != ',' && TOK != ';' && TOK != ')' &&
        TOK != ']' && TOK != '}';
}



PUBLIC void DCC_PARSE_CALL
DCCParse_CExpr2(int one, struct DCCSymExpr *__restrict result) {
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 one ? DCCParse_Expr1() : DCCParse_Expr();
 popf();
 result->e_int = vbottom->sv_const.it;
 result->e_sym = vbottom->sv_sym;
 /* TODO: Check 'vbottom' C type. */
 if (vbottom->sv_reg != DCC_RC_CONST ||
     vbottom->sv_flags&DCC_SFLAG_LVALUE) {
  WARN(W_EXPECTED_CONSTANT_EXPRESSION);
 }
 vpop(1);
}
PUBLIC int_t DCC_PARSE_CALL DCCParse_CExpr(int one) {
 struct DCCSymExpr result; DCCParse_CExpr2(one,&result);
 if (result.e_sym) WARN(W_CONSTANT_EXPR_DEPENDS_ON_SYMBOL,result.e_sym);
 return result.e_int;
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_EXPR_C_INL */
