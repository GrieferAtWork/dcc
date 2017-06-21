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
  vrval();  /* x, rdx */
  vswap();  /* rdx, x */
  vgen1(t); /* rdx, x* */
  vpop(1);  /* rdx */
  goto again;
 } break;

 case '[':
  YIELD();
  /* Array sub-script. */
  vprom();
  vrcopy();         /* dx */
  DCCParse_Expr1(); /* dx, y */
  if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
  vpromi2();
  vgen2('+');       /* dx+y */
  vgen1('*');       /* *(dx+y) */
  goto again;

 {
 case TOK_ARROW:
  vprom();
  vgen1('*');      /* Dereference before subscript. */
 case '.':
  YIELD();
  if (TPP_ISKEYWORD(TOK)) {
   vsubscript(TOKEN.t_kwd);
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
  vprom();
  YIELD();
  n_args = DCCParse_Exprn();
  if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
gen_call:
  DCCUnit_MkDebugLC(DCCUNIT_DEBUGLC_EXPR);
  DCCVStack_Call(n_args);
  goto again;
 } break;

 default: break;
 }
}

/* Register name token groups mapped to register classes. */
static rc_t const register_clases[] = {
 DCC_RC_I8,
 DCC_RC_I16|DCC_RC_I8,
 DCC_RC_I32|DCC_RC_I16|DCC_RC_I8,
#if DCC_TARGET_HASF(F_X86_64)
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
 tyid_t orig_qual;
 struct DCCType *real_sym_type = type;
 assert(type);
 /* Special handling for 'auto &x = y;' and 'char (&c)[] = z;' */
 if (DCCTYPE_GROUP(real_sym_type->t_type) == DCCTYPE_LVALUE)
     assert(real_sym_type->t_base),
     real_sym_type = &real_sym_type->t_base->d_type;
 if (real_sym_type->t_base && DCCTYPE_ISARRAY(real_sym_type->t_type) &&
     DCCTYPE_ISBASIC(real_sym_type->t_base->d_type.t_type,DCCTYPE_AUTO) &&
     DCCTYPE_GROUP(vbottom->sv_ctype.t_type) == DCCTYPE_ARRAY) {
  struct DCCType *array_base;
  /* Special case: 'auto' used as base of array. */
  array_base = &real_sym_type->t_base->d_type;
  assert(vbottom->sv_ctype.t_base);
  assert(!real_sym_type->t_base->d_type.t_base);
  orig_qual = array_base->t_type&(DCCTYPE_QUAL|DCCTYPE_ATOMIC|DCCTYPE_STOREMASK);
  DCCType_InitCopy(array_base,&vbottom->sv_ctype.t_base->d_type);
  if (real_sym_type == type) {
   /* Special case (Don't inherit const qualifiers):
    * >> auto foo[] = "foobar";    // Compile as 'char foo[7] = "foobar";'
    * >> auto (&foo)[] = "foobar"; // Compile as 'char const (&foo)[7] = "foobar";'
    */
   assert(DCCTYPE_GROUP(type->t_type) != DCCTYPE_LVALUE);
   array_base->t_type &= ~(DCCTYPE_CONST|DCCTYPE_ATOMIC|DCCTYPE_STOREMASK);
   array_base->t_type |=  (orig_qual);
   type->t_type       &= ~(DCCTYPE_CONST);
  }
  type->t_type &= ~(DCCTYPE_ATOMIC|DCCTYPE_STOREMASK);
  type->t_type |=  (orig_qual);
 }

 if (DCCTYPE_ISBASIC(real_sym_type->t_type,DCCTYPE_AUTO)) {
  /* Promote functions here to prevent assignments to function types. */
  DCCStackValue_PromoteFunction(vbottom);
  /* Fix automatically typed variables. */
  assert(!real_sym_type->t_base);
  orig_qual = real_sym_type->t_type&(DCCTYPE_QUAL|DCCTYPE_ATOMIC|DCCTYPE_STOREMASK);
  DCCType_InitCopy(real_sym_type,&vbottom->sv_ctype);
  if (real_sym_type == type) {
   struct DCCType *ty_iter;
   assert(DCCTYPE_GROUP(type->t_type) != DCCTYPE_LVALUE);
   /* Special case (Don't inherit const qualifiers):
    * >> auto foo = "foobar";  // Compile as 'char foo[7] = "foobar";'
    * >> auto &foo = "foobar"; // Compile as 'char const (&foo)[7] = "foobar";'
    */
   ty_iter = real_sym_type;
   for (;;) {
    ty_iter->t_type &= ~(DCCTYPE_CONST|DCCTYPE_ATOMIC|DCCTYPE_STOREMASK);
    ty_iter->t_type |=  (orig_qual);
    if (!ty_iter->t_base ||
       (DCCTYPE_GROUP(ty_iter->t_type) != DCCTYPE_ARRAY &&
        DCCTYPE_GROUP(ty_iter->t_type) != DCCTYPE_VARRAY)) break;
    ty_iter = &ty_iter->t_base->d_type;
   }
   type->t_type &= ~(DCCTYPE_ATOMIC|DCCTYPE_STOREMASK);
   type->t_type |=  (orig_qual);
  }
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


#if defined(_MSC_VER) || defined(__DCC_VERSION__)
#define INT_C(x)  x##i64
#elif defined(TPP_HAVE_LONGLONG)
#define INT_C(x)  x##ll
#else
#define INT_C(x)  x##l
#endif

#define DCC_MIN_S1   (-INT_C(127)-INT_C(1))
#define DCC_MAX_S1     INT_C(127)
#define DCC_MAX_U1     INT_C(0xffu)
#define DCC_MIN_S2   (-INT_C(32767)-INT_C(1))
#define DCC_MAX_S2     INT_C(32767)
#define DCC_MAX_U2     INT_C(0xffffu)
#define DCC_MIN_S4   (-INT_C(2147483647)-INT_C(1))
#define DCC_MAX_S4     INT_C(2147483647)
#define DCC_MAX_U4     INT_C(0xffffffffu)
#define DCC_MIN_S8   (-INT_C(9223372036854775807)-INT_C(1))
#define DCC_MAX_S8     INT_C(9223372036854775807)
#define DCC_MAX_U8     INT_C(0xffffffffffffffffu)

#define DCC_PRIVATE_MIN_S(sz) DCC_MIN_S##sz
#define DCC_PRIVATE_MAX_S(sz) DCC_MAX_S##sz
#define DCC_PRIVATE_MAX_U(sz) DCC_MAX_U##sz
#define DCC_MIN_S(sz) DCC_PRIVATE_MIN_S(sz)
#define DCC_MAX_S(sz) DCC_PRIVATE_MAX_S(sz)
#define DCC_MAX_U(sz) DCC_PRIVATE_MAX_U(sz)


struct missing_symbol {
 char const *ms_name; /*< [1..1|eof(0..0)] Name of the symbol. */
 size_t      ms_size; /*< == strlen(ms_name). */
 tyid_t      ms_type; /*< Symbol type. */
 int_t       ms_val;  /*< Symbol value. */
};
#define SYM(name,ty,val) {name,DCC_COMPILER_STRLEN(name),ty,val}
PRIVATE struct missing_symbol const msyms[] = {
 SYM("NULL",DCCTYPE_INT,0),

#if 1 /* endian.h */
 SYM("BYTE_ORDER",      DCCTYPE_INT,DCC_TARGET_BYTEORDER),
 SYM("FLOAT_WORD_ORDER",DCCTYPE_INT,DCC_TARGET_FLOAT_WORD_ORDER),
 SYM("LITTLE_ENDIAN",   DCCTYPE_INT,1234),
 SYM("BIG_ENDIAN",      DCCTYPE_INT,4321),
 SYM("PDP_ENDIAN",      DCCTYPE_INT,3412),
#endif
#if 1 /* limits.h */
 SYM("CHAR_BIT",  DCCTYPE_INT, DCC_TARGET_SIZEOF_CHAR*DCC_TARGET_BITPERBYTE),
 SYM("SCHAR_MIN", DCCTYPE_CHAR,DCC_MIN_S(DCC_TARGET_SIZEOF_CHAR)),
 SYM("SCHAR_MAX", DCCTYPE_CHAR,DCC_MAX_S(DCC_TARGET_SIZEOF_CHAR)),
 SYM("UCHAR_MAX", DCCTYPE_CHAR|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_CHAR)),
 SYM("SHRT_MIN",  DCCTYPE_SHORT,DCC_MIN_S(DCC_TARGET_SIZEOF_SHORT)),
 SYM("SHRT_MAX",  DCCTYPE_SHORT,DCC_MAX_S(DCC_TARGET_SIZEOF_SHORT)),
 SYM("USHRT_MAX", DCCTYPE_SHORT|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_SHORT)),
 SYM("INT_MIN",   DCCTYPE_INT,DCC_MIN_S(DCC_TARGET_SIZEOF_INT)),
 SYM("INT_MAX",   DCCTYPE_INT,DCC_MAX_S(DCC_TARGET_SIZEOF_INT)),
 SYM("UINT_MAX",  DCCTYPE_INT|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT)),
 SYM("LONG_MIN",  DCCTYPE_ALTLONG|DCCTYPE_LONG,DCC_MIN_S(DCC_TARGET_SIZEOF_LONG)),
 SYM("LONG_MAX",  DCCTYPE_ALTLONG|DCCTYPE_LONG,DCC_MAX_S(DCC_TARGET_SIZEOF_LONG)),
 SYM("ULONG_MAX", DCCTYPE_ALTLONG|DCCTYPE_LONG|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_LONG)),
 SYM("LLONG_MIN", DCCTYPE_LLONG,DCC_MIN_S(DCC_TARGET_SIZEOF_LONG_LONG)),
 SYM("LLONG_MAX", DCCTYPE_LLONG,DCC_MAX_S(DCC_TARGET_SIZEOF_LONG_LONG)),
 SYM("ULLONG_MAX",DCCTYPE_LLONG|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_LONG_LONG)),
#endif
#if 1 /* stdint.h */
 SYM("INTMAX_MIN",      DCCTYPE_INTMAX,DCC_MIN_S(DCC_TARGET_SIZEOF_INTMAX_T)),
 SYM("INTMAX_MAX",      DCCTYPE_INTMAX,DCC_MAX_S(DCC_TARGET_SIZEOF_INTMAX_T)),
 SYM("UINTMAX_MAX",     DCCTYPE_INTMAX|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INTMAX_T)),
#if DCC_TARGET_BITPERBYTE == 8
 SYM("INT8_MIN",        DCCTYPE_IB1,DCC_MIN_S1),
 SYM("INT16_MIN",       DCCTYPE_IB2,DCC_MIN_S2),
 SYM("INT32_MIN",       DCCTYPE_IB4,DCC_MIN_S4),
 SYM("INT64_MIN",       DCCTYPE_IB8,DCC_MIN_S8),
 SYM("INT8_MAX",        DCCTYPE_IB1,DCC_MAX_S1),
 SYM("INT16_MAX",       DCCTYPE_IB2,DCC_MAX_S2),
 SYM("INT32_MAX",       DCCTYPE_IB4,DCC_MAX_S4),
 SYM("INT64_MAX",       DCCTYPE_IB8,DCC_MAX_S8),
 SYM("UINT8_MAX",       DCCTYPE_IB1|DCCTYPE_UNSIGNED,DCC_MAX_U1),
 SYM("UINT16_MAX",      DCCTYPE_IB2|DCCTYPE_UNSIGNED,DCC_MAX_U2),
 SYM("UINT32_MAX",      DCCTYPE_IB4|DCCTYPE_UNSIGNED,DCC_MAX_U4),
 SYM("UINT64_MAX",      DCCTYPE_IB8|DCCTYPE_UNSIGNED,DCC_MAX_U8),
#else
#   error FIXME
#endif
 SYM("INT_LEAST8_MIN",  DCCTYPE_INT_LEAST8, DCC_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST8_T)),
 SYM("INT_LEAST16_MIN", DCCTYPE_INT_LEAST16,DCC_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST16_T)),
 SYM("INT_LEAST32_MIN", DCCTYPE_INT_LEAST32,DCC_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST32_T)),
 SYM("INT_LEAST64_MIN", DCCTYPE_INT_LEAST64,DCC_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST64_T)),
 SYM("INT_LEAST8_MAX",  DCCTYPE_INT_LEAST8, DCC_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST8_T)),
 SYM("INT_LEAST16_MAX", DCCTYPE_INT_LEAST16,DCC_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST16_T)),
 SYM("INT_LEAST32_MAX", DCCTYPE_INT_LEAST32,DCC_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST32_T)),
 SYM("INT_LEAST64_MAX", DCCTYPE_INT_LEAST64,DCC_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST64_T)),
 SYM("UINT_LEAST8_MAX", DCCTYPE_INT_LEAST8| DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST8_T)),
 SYM("UINT_LEAST16_MAX",DCCTYPE_INT_LEAST16|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST16_T)),
 SYM("UINT_LEAST32_MAX",DCCTYPE_INT_LEAST32|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST32_T)),
 SYM("UINT_LEAST64_MAX",DCCTYPE_INT_LEAST64|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST64_T)),
 SYM("INT_FAST8_MIN",   DCCTYPE_INT_FAST8, DCC_MIN_S(DCC_TARGET_SIZEOF_INT_FAST8_T)),
 SYM("INT_FAST16_MIN",  DCCTYPE_INT_FAST16,DCC_MIN_S(DCC_TARGET_SIZEOF_INT_FAST16_T)),
 SYM("INT_FAST32_MIN",  DCCTYPE_INT_FAST32,DCC_MIN_S(DCC_TARGET_SIZEOF_INT_FAST32_T)),
 SYM("INT_FAST64_MIN",  DCCTYPE_INT_FAST64,DCC_MIN_S(DCC_TARGET_SIZEOF_INT_FAST64_T)),
 SYM("INT_FAST8_MAX",   DCCTYPE_INT_FAST8, DCC_MAX_S(DCC_TARGET_SIZEOF_INT_FAST8_T)),
 SYM("INT_FAST16_MAX",  DCCTYPE_INT_FAST16,DCC_MAX_S(DCC_TARGET_SIZEOF_INT_FAST16_T)),
 SYM("INT_FAST32_MAX",  DCCTYPE_INT_FAST32,DCC_MAX_S(DCC_TARGET_SIZEOF_INT_FAST32_T)),
 SYM("INT_FAST64_MAX",  DCCTYPE_INT_FAST64,DCC_MAX_S(DCC_TARGET_SIZEOF_INT_FAST64_T)),
 SYM("UINT_FAST8_MAX",  DCCTYPE_INT_FAST8| DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_FAST8_T)),
 SYM("UINT_FAST16_MAX", DCCTYPE_INT_FAST16|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_FAST16_T)),
 SYM("UINT_FAST32_MAX", DCCTYPE_INT_FAST32|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_FAST32_T)),
 SYM("UINT_FAST64_MAX", DCCTYPE_INT_FAST64|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_INT_FAST64_T)),
 SYM("INTPTR_MIN",      DCCTYPE_INTPTR,DCC_MIN_S(DCC_TARGET_SIZEOF_POINTER)),
 SYM("INTPTR_MAX",      DCCTYPE_INTPTR,DCC_MAX_S(DCC_TARGET_SIZEOF_POINTER)),
 SYM("UINTPTR_MAX",     DCCTYPE_INTPTR|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_POINTER)),
 SYM("SIZE_MAX",        DCCTYPE_SIZE|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_SIZE_T)),
 SYM("PTRDIFF_MIN",     DCCTYPE_PTRDIFF,DCC_MIN_S(DCC_TARGET_SIZEOF_PTRDIFF_T)),
 SYM("PTRDIFF_MAX",     DCCTYPE_PTRDIFF,DCC_MAX_S(DCC_TARGET_SIZEOF_PTRDIFF_T)),
 SYM("SIG_ATOMIC_MIN",  DCCTYPE_SIG_ATOMIC,DCC_MIN_S(DCC_TARGET_SIZEOF_SIG_ATOMIC_T)),
 SYM("SIG_ATOMIC_MAX",  DCCTYPE_SIG_ATOMIC,DCC_MAX_S(DCC_TARGET_SIZEOF_SIG_ATOMIC_T)),
 SYM("WCHAR_MIN",       DCCTYPE_WCHAR|DCCTYPE_UNSIGNED,0),
 SYM("WCHAR_MAX",       DCCTYPE_WCHAR|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_WCHAR_T)),
 SYM("WINT_MIN",        DCCTYPE_WINT,DCC_MIN_S(DCC_TARGET_SIZEOF_WINT_T)),
 SYM("WINT_MAX",        DCCTYPE_WINT,DCC_MAX_S(DCC_TARGET_SIZEOF_WINT_T)),
#endif
 {NULL,0,0,0}
};
#undef SYM


LEXPRIV int DCC_PARSE_CALL
DCCParse_ExprMissingSym(void) {
 struct DCCDecl *decl;
 char const *kwd_name; size_t kwd_size;
 struct missing_symbol const *iter;

 /* Last chance: Try to declare a new, missing symbol
  *              as an old-style function returning int. */
 if (!TPP_ISKEYWORD(TOK)) return 0;
 /* Explicitly recognize some special keywords such as 'NULL' */
 kwd_name = TOKEN.t_kwd->k_name;
 kwd_size = TOKEN.t_kwd->k_size;
 /* Strip leading/trailing underscores from the identifier. */
 while (kwd_size && kwd_name[0] == '_') ++kwd_name,--kwd_size;
 while (kwd_size && kwd_name[kwd_size-1] == '_') --kwd_size;
 for (iter = msyms; iter->ms_name; ++iter) {
  if (iter->ms_size == kwd_size &&
     !memcmp(kwd_name,iter->ms_name,kwd_size*sizeof(char))) {
   /* Found the replacement! */
   vpushi(iter->ms_type,iter->ms_val);
   goto missing;
  }
 }
 if (kwd_size == 8) {
  if (!memcmp(kwd_name,"CHAR_MIN",8*sizeof(char))) {
   (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED)
    ? vpushi(DCCTYPE_CHAR|DCCTYPE_UNSIGNED,0)
    : vpushi(DCCTYPE_CHAR,DCC_MIN_S(DCC_TARGET_SIZEOF_CHAR));
   goto missing;
  }
  if (!memcmp(kwd_name,"CHAR_MAX",8*sizeof(char))) {
   (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED)
    ? vpushi(DCCTYPE_CHAR|DCCTYPE_UNSIGNED,DCC_MAX_U(DCC_TARGET_SIZEOF_CHAR))
    : vpushi(DCCTYPE_CHAR,DCC_MAX_S(DCC_TARGET_SIZEOF_CHAR));
   goto missing;
  }
 }

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
missing:
 WARN(W_MISSING_SYMBOL_IN_EXPRESSION,TOKEN.t_kwd);
 YIELD();
 return 1;
}

LEXPRIV int DCC_PARSE_CALL DCCParse_ExprType(void) {
 struct DCCType type; int error;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 /* Parse a type prefix. - Don't parse a full type due to ambiguity between:
  * >> auto x = int(*)(int) (42); // This just looks weird. - Also: The '(*' is ambiguous
  */
 error = !!DCCParse_CTypePrefix(&type,&attr);
 if (!error) {
  if (TPP_ISKEYWORD(TOK)) {
   struct DCCDecl *tydecl;
   /* >> struct point { int x,y; };
    * >> static int x = sizeof(point); // Wrong: should be 'sizeof(struct point)'
    * Check if a struct/union/enum type named 'TOKEN.t_kwd' exists, and it one does,
    * warn about the missing type prefix before using that type here. */
   if ((tydecl = DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_STRUCT)) != NULL) {
    int wid = 0;
    if (tydecl->d_kind == DCC_DECLKIND_STRUCT) {
     wid = W_TYPE_IN_EXPRESSION_MISSING_STRUCT_PREFIX;
     goto found_struct;
    } else if (tydecl->d_kind == DCC_DECLKIND_UNION) {
     wid = W_TYPE_IN_EXPRESSION_MISSING_UNION_PREFIX;
found_struct:
     type.t_type = DCCTYPE_STRUCTURE;
     type.t_base = tydecl;
     DCCDecl_Incref(tydecl);
    } else if (tydecl->d_kind == DCC_DECLKIND_ENUM) {
     wid = W_TYPE_IN_EXPRESSION_MISSING_ENUM_PREFIX;
     assert(!type.t_base);
     assert(type.t_type == DCCTYPE_INT);
    }
    if (wid) { /* Should always be the case... */
     WARN(wid,&type);
     YIELD();
     error = 2;
     goto got_type;
    }
   }
  }
  goto end;
 }
got_type:
 /* Special case: Still allow array extensions! */
 if (TOK == '[') DCCParse_CTypeSuffix(&type,&attr);
 /* Warn about use of types in expressions. */
 WARN(W_TYPE_IN_EXPRESSION,&type);
 pushf();
 if ((type.t_type&DCCTYPE_STOREBASE) == DCCTYPE_STATIC ||
     (type.t_type&DCCTYPE_STOREBASE) == DCCTYPE_EXTERN
     ) compiler.c_flags |= (DCC_COMPILER_FLAG_SINIT);
 if (TOK == '(' || TOK == KWD___pack) {
  /* Parse a type initializer.
   * NOTE: This is written in a way that also allows: 'int[] ({ 10,20,30 })' */
  DCCParse_ParPairBegin();
  DCCParse_Init(&type,&attr,NULL,DCCPARSE_INITFLAG_INITIAL);
  DCCParse_FixType(&type);
  DCCParse_ParPairEnd();
 } else {
  if (TOK != '{') WARN(W_EXPECTED_LPAREN_AFTER_TYPE_IN_EXPRESSION,&type);
  /* Parse a type initializer. */
  DCCParse_Init(&type,&attr,NULL,DCCPARSE_INITFLAG_INITIAL);
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
  if (DCC_ISSTMTKWD(TOK)) WARN(W_EXPRESSION_CONTAINS_STMT_KEYWORD);
  decl = DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_LOCALS);
  if (decl) {
   YIELD();
   if (decl->d_kind&DCC_DECLKIND_TYPE) {
    WARN(W_EXPRESSION_CONTAINS_TYPE);
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
    WARN(W_EXPRESSION_CONTAINS_UNKNOWN_KEYWORD,ident);
    YIELD();
   } else {
    WARN(W_EXPRESSION_CONTAINS_UNKNOWN_TOKEN);
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
   type_name = DCCParse_CTypeUnknown(&case_type,&case_attr);
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
 struct DCCSym *tt_label;
 int is_true,is_dead = 0;
 /* TODO: This probably doesn't work anymore... */
 assert(TOK == KWD_if);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 DCCParse_ParPairEnd();
 is_true = visconst_bool() ? vgtconst_bool() : 2;
 tt_label = DCCUnit_AllocSym();
 if unlikely(!tt_label) return;
 DCCVStack_KillAll(1); /* Kill all registers before the jump. */
 vpushs(tt_label);
 vgen1('&');
 vjcc(1);              /* Jump across the true-branch. */
 pushf();
 if (is_true == 0) {
  compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                       DCC_COMPILER_FLAG_DEAD);
  if (!compiler.c_deadjmp) compiler.c_deadjmp = tt_label;
  DCCParse_Expr1();     /* Generate the true-branch. */
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
   if (DCCType_Sizeof(&vbottom->sv_ctype,NULL,1) <= DCC_TARGET_SIZEOF_GP_REGISTER) {
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
   if (!compiler.c_deadjmp) compiler.c_deadjmp = ff_label;
   DCCParse_Expr1();
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
 int force_extensions = 0;
again:
 DCCUnit_MkDebugLC(DCCUNIT_DEBUGLC_EXPR);
 switch (TOK) {

 case KWD___extension__:
  force_extensions = 1;
  YIELD();
  goto again;

 { /* Push an immediate, constant integral. */
  int_t intval,intmask;
  int int_kind;
  tyid_t tyid;
 case TOK_INT:
 case TOK_CHAR:
  int_kind = TPP_Atoi(&intval);
  if unlikely(!int_kind) goto yield_push_int0;
#define MASK_1    0xffffffffffffff00ll
#define MASK_2    0xffffffffffff0000ll
#define MASK_4    0xffffffff00000000ll
#define MASK_8    0x0000000000000000ll
#define MASK2(sz) MASK_##sz
#define MASK(sz)  MASK2(sz)
  switch (int_kind&TPP_ATOI_TYPE_MASK) {
   case TPP_ATOI_TYPE_LONG    : tyid = DCCTYPE_ALTLONG|DCCTYPE_LONG;
                                intmask = MASK(DCC_TARGET_SIZEOF_LONG);
                                break;
   case TPP_ATOI_TYPE_LONGLONG: tyid = DCCTYPE_LLONG;
                                intmask = MASK(DCC_TARGET_SIZEOF_LONG_LONG);
                                break;
#ifdef DCCTYPE_INT8
   case TPP_ATOI_TYPE_INT8    : tyid = DCCTYPE_INT8; intmask = MASK(1); break;
#endif
#ifdef DCCTYPE_INT16
   case TPP_ATOI_TYPE_INT16   : tyid = DCCTYPE_INT16; intmask = MASK(2); break;
#endif
#ifdef DCCTYPE_INT32
   case TPP_ATOI_TYPE_INT32   : tyid = DCCTYPE_INT32; intmask = MASK(4); break;
#endif
#ifdef DCCTYPE_INT64
   case TPP_ATOI_TYPE_INT64   : tyid = DCCTYPE_INT64; intmask = MASK(8); break;
#endif
   default:
    if (TOK == TOK_CHAR) {
     /* Character constant. */
     intmask = MASK(DCC_TARGET_SIZEOF_CHAR);
     tyid = DCCTYPE_CHAR;
     if (CURRENT.l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED)
         tyid |= DCCTYPE_UNSIGNED;
    } else {
     intmask = MASK(DCC_TARGET_SIZEOF_INT);
     tyid = DCCTYPE_INT;
    }
    break;
  }
  if (int_kind&TPP_ATOI_UNSIGNED) tyid |= DCCTYPE_UNSIGNED;
  if (intval&intmask) {
   struct DCCType ty;
   ty.t_type = tyid;
   ty.t_base = NULL;
   WARN(W_TRUNC_INTEGRAL_CONSTANT,&ty);
   intval &= ~(intmask);
  }
  vpushi(tyid,intval);
  /* Don't emit a warning if the stack-value depends on a symbol or register. */
  YIELD();
 } break;

#if 1
 {
  float_t fltval;
  int flt_kind;
  tyid_t tyid;
 case TOK_FLOAT:
  flt_kind = TPP_Atof(&fltval);
  if unlikely(!flt_kind) goto yield_push_int0;
  switch (flt_kind&TPP_ATOF_TYPE_MASK) {
  case TPP_ATOF_TYPE_FLOAT:      tyid = DCCTYPE_FLOAT; break;
  case TPP_ATOF_TYPE_LONGDOUBLE: tyid = DCCTYPE_LDOUBLE; break;
  default:                       tyid = DCCTYPE_DOUBLE; break;
  }
  vpushf(tyid,fltval);
  YIELD();
 } break;
#endif

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
  if (!force_extensions) WARN(W_EXT_ASM_REGISTERS_IN_EXPRESSIONS);
  /* Push an explicitly defined register stack value. */
  sval.sv_reg  = DCCVStack_KillXNon(DCCParse_Register());
  sval.sv_reg2 = DCC_RC_CONST;
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
  if (TOK == ':') {
   char const *next_token = peek_next_token(NULL);
   if ((sval.sv_reg&DCC_RC_MASK) == DCC_RC_SEG) {
    /* Segment register prefix. */
    if (*next_token != ')' &&
        *next_token != ']' &&
        *next_token != '}' &&
        *next_token != ',' &&
        *next_token != ';') {
     YIELD();
     sval.sv_reg = DCC_RC_86SEGP(sval.sv_reg&DCC_RI_MASK);
     if (!DCCParse_IsExpr()) goto parse_second_register;
     /* Parse a pointer expression */
     DCCParse_ExprUnary();
     /* Make sure to load l-value operands now, as any
      * register/offset indirection pairs must be
      * dereferenced with the general-purpose segment,
      * instead of the explicitly selected segment. */
     DCCStackValue_LoadLValue(vbottom);
     if ((vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
         /* When both stack-values make use of the same base segment,
          * no need to dereference the operand beforehand. */
         (DCC_RC_86SEGPOF(vbottom->sv_reg) != DCC_RC_GET_86SEGP(sval.sv_reg)))
          DCCStackValue_Load(vbottom);
     vused();
     DCCType_InitCopy(&sval.sv_ctype,&vbottom->sv_ctype);
     sval.sv_const.it = vbottom->sv_const.it;
     sval.sv_sym      = vbottom->sv_sym;
     sval.sv_reg     |= vbottom->sv_reg;
     sval.sv_reg2     = vbottom->sv_reg2;
     vpop(1);
     goto pushval;
    }
   }
   if (*next_token == '%') {
    /* Parse a register pair.
     * NOTE: Here, both the first & second register must be 32-bit wide. */
    YIELD();
parse_second_register:
    if (TOK != '%')
        WARN(W_EXPECTED_PERCENT_BEFORE_REGISTER_NAME);
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
  }
pushval:
  vpush(&sval);
 } break;

 { /* GCC-style label addressing. */
 case TOK_LAND:
  if (!force_extensions) WARN(W_EXT_LABEL_EXPRESSIONS);
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
  if (!force_extensions)
       WARN(W_EXT_ASM_ADDRESS_IN_EXPRESSIONS);
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
   /* don't duplicate the value for these operators! */
   vgen1(t); /* x */
  } else {
   vpromi(); /* xi */
   if (t != '+') {
    vrcopy(); /* dxi */
    vgen1(t); /* dnxi */
    vrval();  /* rdnxi */
   }
  }
 } break;

 case KWD___pack:
 case '(': {
  struct DCCType     cast_type;
  struct DCCAttrDecl cast_attr = DCCATTRDECL_INIT;
  struct TPPKeyword *cast_name;
  tok_t t = TOK;
  YIELD();
  if (TOK == KWD___extension__ && *peek_next_token(NULL) == '(') {
   /* Explicitly allow 'int x = (__extension__({ 42; }))'
    * Without this exception, the '__extension__' above
    * would be parsed as part of an otherwise missing type. */
   YIELD();
   if (TOK == '(') {
    YIELD();
    if (TOK == '{')
         DCCParse_Scope(DCC_PFLAG_USED);
    else DCCParse_Expr();
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    else YIELD();
    goto check_rparen_break;
   }
  }
  if (TOK == '{') {
   if (!force_extensions) WARN(W_EXT_EXPRESSION_STATEMENTS);
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
   if ((cast_type.t_type&DCCTYPE_STOREBASE) == DCCTYPE_STATIC ||
       (cast_type.t_type&DCCTYPE_STOREBASE) == DCCTYPE_EXTERN
       ) compiler.c_flags |= (DCC_COMPILER_FLAG_SINIT);
   if (TOK == '{')
        DCCParse_Init(&cast_type,&cast_attr,NULL,DCCPARSE_INITFLAG_INITIAL);
   else DCCParse_ExprUnary();
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
 } break;

 { /* Expand to the string representation of the current function. */
  char const *name; size_t size;
 case KWD___FUNCTION__:
  if (HAS(EXT_FUNCTION_STRING_LITERALS) || force_extensions) goto parse_string;
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
  if (HAS(EXT_FUNCTION_STRING_LITERALS) || force_extensions) goto parse_string;
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
    /* Warn about size queries on VLA array types
     * (Which is allowed but not a compile-time constant). */
    if (DCCType_ISVLA(used_type)) {
     WARN(W_SIZEOF_VLA_ARRAY_TYPE,used_type);
     DCCVStack_PushSizeof(used_type);
    } else {
     vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,res_size);
    }
    if (!query_name) vswap(),vpop(1);
   } else {
    if (!query_name) vpop(1);
    vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,res_align);
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
 case KWD___builtin_strlen:
 case KWD___builtin_strnlen: DCCParse_BuiltinStrlen(); break;
  /* Builtin dynamic memory. */
 case KWD___builtin_malloc:
 case KWD___builtin_calloc: DCCParse_BuiltinMalloc(); break;
 case KWD___builtin_realloc: DCCParse_BuiltinRealloc(); break;
 case KWD___builtin_free: DCCParse_BuiltinFree(); break;
  /* Builtin string scanners. */
 case KWD___builtin_memchr: case KWD___builtin_memlen: case KWD___builtin_memend:
 case KWD___builtin_memrchr: case KWD___builtin_memrlen: case KWD___builtin_memrend:
 case KWD___builtin_rawmemchr: case KWD___builtin_rawmemlen:
 case KWD___builtin_rawmemrchr: case KWD___builtin_rawmemrlen: DCCParse_BuiltinScas(); break;
  /* Builtin ctype operations. */
 case KWD___builtin_iscntrl: case KWD___builtin_isblank:
 case KWD___builtin_isspace: case KWD___builtin_isupper:
 case KWD___builtin_islower: case KWD___builtin_isalpha:
 case KWD___builtin_isdigit: case KWD___builtin_isxdigit:
 case KWD___builtin_isalnum: case KWD___builtin_ispunct:
 case KWD___builtin_isgraph: case KWD___builtin_isprint:
 case KWD___builtin_tolower: case KWD___builtin_toupper: DCCParse_BuiltinCType(); break;
  /* min/max functions. */
 case KWD___builtin_min: case KWD___builtin_max: DCCParse_BuiltinMinMax(); break;
  /* bit-scanning functions. */
 case KWD___builtin_ffs:  case KWD___builtin_ffsll:
 case KWD___builtin_ffsl: case KWD___builtin_ffscc:
 case KWD___builtin_clz:  case KWD___builtin_clzll:
 case KWD___builtin_clzl: case KWD___builtin_clzcc: DCCParse_BuiltinScanner(); break;
 case KWD___builtin_assume_aligned: DCCParse_BuiltinAssumeAligned(); break;
  /* CPU Information functions. */
 case KWD___builtin_cpu_init: DCCParse_BuiltinCPUInit(); break;
 case KWD___builtin_cpu_is:
 case KWD___builtin_cpu_supports: DCCParse_BuiltinCPUQuery(); break;
 case KWD___builtin_cpu_vendor: DCCParse_BuiltinCPUVendor(); break;
  /* Return/Frame pointers. */
 case KWD___builtin_return_address:
 case KWD___builtin_frame_address: DCCParse_BuiltinReturnAddr(); break;
 case KWD___builtin_extract_return_addr:
 case KWD___builtin_frob_return_address: DCCParse_BuiltinExtractReturnAddr(); break;
 case KWD___builtin_noop: case KWD___noop: DCCParse_BuiltinNoop(); break;
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
yield_push_int0: /* fallback... */
 YIELD();
push_int0: /* fallback... */
 vpushi(DCCTYPE_INT,0);
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprProd(void) {
 tok_t func;
 DCCParse_ExprUnary();
 while (TOK == '*' || TOK == '/' || TOK == '%') {
  vprom();
  func = TOK;
  YIELD();
  vrcopy();             /* dx */
  DCCParse_ExprUnary(); /* dx, y */
  vpromi2();            /* dxi, yi */
  vgen2(func);          /* dxi#yi */
  vrval();              /* rdxi#yi */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprSum(void) {
 tok_t func;
 DCCParse_ExprProd();
 while (TOK == '+' || TOK == '-') {
  vprom();
  func = TOK;
  YIELD();
  vrcopy();            /* dx */
  DCCParse_ExprProd(); /* dx, y */
  vpromi2();           /* dxi, yi */
  vgen2(func);         /* dxi#yi */
  vrval();             /* rdxi#yi */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprShift(void) {
 tok_t func;
 DCCParse_ExprSum();
 while (TOK == TOK_SHL || TOK == TOK_SHR) {
  vprom();
  vpromi();
  func = TOK;
  YIELD();
  vrcopy();           /* dx */
  DCCParse_ExprSum(); /* dx, y */
  vprom();            /* dxi, yi */
  vgen2(func);        /* dxi#yi */
  vrval();            /* rdxi#yi */
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
  vprom();
  vrcopy();             /* dx */
  DCCParse_ExprShift(); /* dx, y */
  vgen2(func);          /* dx#y */
  vrval();              /* rdx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprCmpEq(void) {
 tok_t func;
 DCCParse_ExprCmp();
 while (TOK == TOK_EQUAL ||
        TOK == TOK_NOT_EQUAL) {
  vprom();
  func = TOK;
  YIELD();
  vrcopy();           /* dx */
  DCCParse_ExprCmp(); /* dx, y */
  vgen2(func);        /* dx#y */
  vrval();            /* rdx#y */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprAnd(void) {
 DCCParse_ExprCmpEq();
 while (TOK == '&') {
  vprom();
  YIELD();
  vrcopy();             /* dx */
  DCCParse_ExprCmpEq(); /* dx, y */
  vpromi2();            /* dxi, yi */
  vgen2('&');           /* dxi#yi */
  vrval();              /* rdxi#yi */
 }
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprXor(void) {
 DCCParse_ExprAnd();
 while (TOK == '^') {
  vprom();
  YIELD();
  vrcopy();           /* dx */
  DCCParse_ExprAnd(); /* dx, y */
  vpromi2();          /* dxi, yi */
  vgen2('^');         /* dxi#yi */
  vrval();            /* rdxi#yi */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprOr(void) {
 DCCParse_ExprXor();
 while (TOK == '|') {
  vprom();
  YIELD();
  vrcopy();           /* dx */
  DCCParse_ExprXor(); /* dx, y */
  vpromi2();          /* dxi, yi */
  vgen2('|');         /* dxi#yi */
  vrval();            /* rdxi#yi */
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ExprLAnd(void) {
 DCCParse_ExprOr();
 if (TOK == TOK_LAND) {
  struct DCCSym *sym = DCCUnit_AllocSym();
  test_t common_test = DCC_UNITST_FIRST;
  target_ptr_t last_text_offset;
#define F_CFALSE 0x01 /* Found a constant-false operand (meaning that all following are unreachable). */
#define F_RTEVAL 0x02 /* Found an operand that can only be evaluated at runtime. */
#define F_NORET  0x04 /* Found an operand that doesn't return (meaning that all following are unreachable). */
#define F_ANORET 0x08 /* Set when a noreturn operand is encountered when neither 'F_RTEVAL' nor 'F_CFALSE' are set. (With this set, the land doesn't return, either) */
  int flags = 0;
  if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD)
      flags |= (F_NORET|F_ANORET);
  if (!sym) return;
  do {
   last_text_offset = t_addr;
   vprom();
   YIELD();
   if (visconst_bool()) {
    if (!vgtconst_bool()) { flags |= F_CFALSE; goto normal; }
    vpop(1);
    DCCParse_ExprOr();
    common_test = DCCVStack_UniTst(common_test);
   } else {
normal:
    if (flags&(F_CFALSE|F_NORET)) {
     pushf();
     if (!compiler.c_deadjmp) compiler.c_deadjmp = sym;
     compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                          DCC_COMPILER_FLAG_DEAD);
     DCCParse_ExprOr();
     vpop(1);
     popf();
    } else {
     common_test = DCCVStack_UniTst(common_test);
     DCCVStack_KillAll(1); /* Kill all registers before doing the jump.
                            * NOTE: This mustn't modify EFLAGS, so we're safe! */
     vpushs(sym);
     vgen1('&');
     vjcc(1);              /* Jump over the second operand(s) if first was false. */
     pushf();
     last_text_offset = t_addr;
     DCCParse_ExprOr();    /* Parse the second operand. */
     common_test = DCCVStack_UniTst(common_test);
     if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) {
      flags |= F_NORET;
      if (!(flags&(F_CFALSE|F_RTEVAL)))
            flags |= F_ANORET;
     }
     flags |= F_RTEVAL;
     popf();
    }
   }
  } while (TOK == TOK_LAND);
  vprom();
  if (flags&F_ANORET) compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  if (visconst_bool() && !vgtconst_bool()) flags |= F_CFALSE;
  if (flags&F_CFALSE) {
   vpop(1);
   vpushi(DCCTYPE_BOOL,0);
  } else if (common_test != DCC_UNITST_FIRST) {
   if (!(vbottom->sv_flags&DCC_SFLAG_TEST) &&
         /* 'vbottom' must be a constant true.
          * But since other components of the branch can
          * only be evaluated at runtime, and since the
          * latest branch that ended in true may have
          * contained code that modified EFLAGS, we must
          * somehow force those flags to mirror
          * 'common_test' for a constant true. */
        (assert(visconst_bool()),assert(vgtconst_bool()),
         /* NOTE: Don't do this if the last expression didn't generate code
          *      (which can be determined by comparing text pointer offsets) */
         last_text_offset != t_addr)) {
    vpop(1);
    DCCDisp_SetTst(common_test);
   } else {
    vpop(1);
   }
   DCCVStack_PushTst(common_test);
  }
  if (sym) t_defsym(sym);
#undef F_ANORET
#undef F_NORET
#undef F_RTEVAL
#undef F_CFALSE
 }
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprLXor(void) {
 DCCParse_ExprLAnd();
 while (TOK == TOK_LXOR && HAS(EXT_LXOR)) {
  vprom();
  YIELD();
  vrcopy();              /* dx */
  DCCParse_ExprLAnd();   /* dx, y */
  vgen2('^');            /* dx#y */
  vgen1('!'),vgen1('!'); /* Force to boolean. */
 }
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprLOr(void) {
 DCCParse_ExprLXor();
 if (TOK == TOK_LOR) {
  struct DCCSym *sym = DCCUnit_AllocSym();
  test_t common_test = DCC_UNITST_FIRST;
  target_ptr_t last_text_offset;
#define F_CTRUE  0x01 /* Found a constant-true operand (meaning that all following are unreachable). */
#define F_RTEVAL 0x02 /* Found an operand that can only be evaluated at runtime. */
#define F_NORET  0x04 /* Found an operand that doesn't return (meaning that all following are unreachable). */
#define F_ANORET 0x08 /* Set when a noreturn operand is encountered when neither 'F_RTEVAL' nor 'F_CFALSE' are set. (With this set, the land doesn't return, either) */
  int flags = 0;
  if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD)
      flags |= (F_NORET|F_ANORET);
  if (!sym) return;
  do {
   last_text_offset = t_addr;
   vprom();
   YIELD();
   if (visconst_bool()) {
    if (vgtconst_bool()) { flags |= F_CTRUE; goto normal; }
    /* Constant false. */
    vpop(1);
    DCCParse_ExprLXor();
    common_test = DCCVStack_UniTst(common_test);
   } else {
normal:
    if (flags&(F_CTRUE|F_NORET)) {
     pushf();
     if (!compiler.c_deadjmp) compiler.c_deadjmp = sym;
     compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                          DCC_COMPILER_FLAG_DEAD);
     DCCParse_ExprLXor();
     vpop(1);
     popf();
    } else {
     common_test = DCCVStack_UniTst(common_test);
     DCCVStack_KillAll(1); /* Kill all registers before doing the jump.
                            * NOTE: This mustn't modify EFLAGS, so we're safe! */
     vpushs(sym);
     vgen1('&');
     vjcc(0);              /* Jump over the second operand(s) if first was true. */
     pushf();
     last_text_offset = t_addr;
     DCCParse_ExprLXor();  /* Parse the second operand. */
     common_test = DCCVStack_UniTst(common_test);
     if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) {
      flags |= F_NORET;
      if (!(flags&(F_CTRUE|F_RTEVAL)))
            flags |= F_ANORET;
     }
     flags |= F_RTEVAL;
     popf();
    }
   }
  } while (TOK == TOK_LOR);
  vprom();
  if (flags&F_ANORET) compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  if (visconst_bool() && vgtconst_bool()) flags |= F_CTRUE;
  if (flags&F_CTRUE) {
   vpop(1);
   vpushi(DCCTYPE_BOOL,1);
  } else if (common_test != DCC_UNITST_FIRST) {
   if (!(vbottom->sv_flags&DCC_SFLAG_TEST) &&
         /* 'vbottom' must be a constant false.
          * But since other components of the branch can
          * only be evaluated at runtime, and since the
          * latest branch that ended in false may have
          * contained code that modified EFLAGS, we must
          * somehow force those flags to mirror
          * 'common_test' for a constant false. */
        (assert(visconst_bool()),assert(!vgtconst_bool()),
         /* NOTE: Don't do this if the last expression didn't generate code
          *      (which can be determined by comparing text pointer offsets) */
         last_text_offset != t_addr)) {
    vpop(1);
    DCCDisp_SetTst(DCC_TEST_NOT(common_test));
   } else {
    vpop(1);
   }
   vpusht(common_test);
  }
  if (sym) t_defsym(sym);
 }
}
LEXPRIV void DCC_PARSE_CALL DCCParse_ExprCond(void) {
 DCCParse_ExprLOr();
 if (TOK == '?') {
  vprom();
  YIELD();
#if 1 /* Special optimization for handling constants. */
  if (visconst_bool()) {
   tyid_t tt_type;
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
     if (!compiler.c_deadjmp) compiler.c_deadjmp = jmp_sym;
     compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                              DCC_COMPILER_FLAG_DEAD);
     DCCParse_Expr();
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
   vprom();
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
     if (!compiler.c_deadjmp) compiler.c_deadjmp = jmp_sym2;
     compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                              DCC_COMPILER_FLAG_DEAD);
     DCCParse_ExprCond();
     popf();
    } else {
     DCCParse_ExprCond();
    }
   }
   /* TODO: vpromi2() between the two branches! */
   if (jmp_sym2) t_defsym(jmp_sym2);
   if (is_true) vpop(1); /* Pop the false-branch if it's unwanted. */
  } else
#endif
#define PROM_CONDVAL 1
   /* TODO: Cast to common type? */
  {
   struct DCCSym *tt_label,*ff_label;
   struct DCCStackValue shared_storage;
   int is_dead = 0;
   tt_label = DCCUnit_AllocSym();
   if unlikely(!tt_label) return;
   if (TOK == ':' && HAS(EXT_GCC_IFELSE)) {
    /* Special case: gcc's if-else. */
    vdup(1);              /* Duplicate the condition for use as true-operand. */
    DCCVStack_KillAll(1); /* Kill all registers (including the condition value). */
    vpushs(tt_label);
    vgen1('&');
    vjcc(1);              /* Jump across the true-branch. */
   } else {
    DCCVStack_KillAll(1); /* Kill all registers before the jump. */
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
#if PROM_CONDVAL
   vprom();
#endif
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
    if ((vbottom->sv_flags&DCC_SFLAG_LVALUE) ||
        (DCCType_Sizeof(&vbottom->sv_ctype,NULL,1) <= DCC_TARGET_SIZEOF_GP_REGISTER)) {
     DCCStackValue_Load(vbottom);
    } else {
     /* Use the stack for shared storage.
      * WARNING: This may potentially be wasteful... */
     DCCStackValue_Kill(vbottom);
    }
   }
   /* Make sure any potential register offsets have been flushed. */
   DCCStackValue_FixRegOffset(vbottom);
   shared_storage = *vbottom; /* Keep track of how the true-branch was stored (so that the false-branch can mirror it). */
   ++vbottom;                 /* Don't destroy this. */
   assert(!(shared_storage.sv_flags&DCC_SFLAG_COPY));
   /* Prevent warnings about assignment to r-value.
    * NOTE: Remember that 'DCC_SFLAG_RVALUE' does not change the semantics of
    *       operation with stack-values. - It merely controls some warnings. */
   shared_storage.sv_flags &= ~(DCC_SFLAG_RVALUE);
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
#if PROM_CONDVAL
   vprom();
#endif

   if (is_dead == (1|2)) {
    /* When both branches are dead, we know that the entire expression is, too. */
    compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
   }
   /* Now that the false-branch has been generated,
    * we must store its value in the shared storage. */
   --vbottom,*vbottom = shared_storage;
   /* Duplicate the shared storage virtual value, but don't copy it for real */
   vpromi2(); /* ..., rhs, shared */
   vdup(0);   /* ..., rhs, shared, shared */
   vrrot(3);  /* ..., shared, shared, rhs */
   vstore(1); /* ..., shared, shared */
   vpop(0);   /* ..., shared */
   vrval();   /* ..., rshared */

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
   if (vbottom->sv_ctype.t_type&DCCTYPE_CONST)
       WARN(W_ASSIGN_CONSTANT_TYPE,&vbottom->sv_ctype,&vbottom->sv_ctype);
   if (vbottom->sv_flags&DCC_SFLAG_RVALUE)
       WARN(W_ASSIGN_RVALUE_TYPE,&vbottom->sv_ctype,&vbottom->sv_ctype);
   DCCParse_VInit(DCCPARSE_INITFLAG_NONE);
  } else {
   DCCParse_Init(&vbottom->sv_ctype,NULL,NULL,
                 DCCPARSE_INITFLAG_NONE);
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
  vprom();
  vgen2(t);  /* nx */
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
 switch (TOK) {
 if (DCC_MACRO_FALSE) { case KWD_if: if (!HAS(EXT_IFELSE_IN_EXPR)) break; }
 case '%': case TOK_LAND: case '.': case KWD___extension__:
 case TOK_INT: case TOK_CHAR: case TOK_FLOAT: case TOK_STRING:
 case '+': case '-': case '*': case '&': case '~': case '!':
 case '(': case TOK_INC: case TOK_DEC: goto yes;
 default:
  if (!TPP_ISKEYWORD(TOK)) break;
  /* Don't consider statement/type keywords part of expressions.
   * NOTE: Yes: Even though they can (somewhat) be parsed inside,
   *            types are _NOT_ considered allowed inside of expressions. */
  if (DCC_ISSTMTKWD(TOK)) break;
yes: return 1;
 }
 return 0;
}



PUBLIC void DCC_PARSE_CALL
DCCParse_CExpr2(int one, struct DCCSymExpr *__restrict result) {
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 one ? DCCParse_Expr1() : DCCParse_Expr();
 vused();
 result->e_int = vbottom->sv_const.it;
 result->e_sym = vbottom->sv_sym;
 /* TODO: Check 'vbottom' C type. */
 if (!visconst_xval()) WARN(W_EXPECTED_CONSTANT_EXPRESSION);
 vpop(1);
 popf();
}
PUBLIC int_t DCC_PARSE_CALL DCCParse_CExpr(int one) {
 struct DCCSymExpr result; DCCParse_CExpr2(one,&result);
 if (result.e_sym) WARN(W_CONSTANT_EXPR_DEPENDS_ON_SYMBOL,result.e_sym->sy_name);
 return result.e_int;
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_EXPR_C_INL */
