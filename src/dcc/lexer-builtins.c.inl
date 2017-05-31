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
#ifndef GUARD_DCC_LEXER_BUILTINS_C_INL
#define GUARD_DCC_LEXER_BUILTINS_C_INL 1

#include <dcc/common.h>
#include <dcc/vstack.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/byteorder.h>
#include <dcc/gen.h>

#include <string.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN


/*  __builtin_typestr  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinTypeStr(void) {
 struct DCCType     query_type;
 struct DCCAttrDecl query_attr = DCCATTRDECL_INIT;
 struct TPPKeyword *query_name;
 struct TPPString *type_str;
 assert(TOK == KWD___builtin_typestr);
 YIELD();
 DCCParse_ParPairBegin();
 query_name = DCCParse_CType(&query_type,&query_attr);
 DCCAttrDecl_Quit(&query_attr);
 if (query_name) {
  type_str = DCCType_ToTPPString(&query_type,query_name);
 } else {
  struct DCCSym *name_sym;
  pushf();
  compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
  DCCParse_Expr();
  popf();
  name_sym = vbottom->sv_sym;
  type_str = DCCType_ToTPPString(&vbottom->sv_ctype,
                                 name_sym ? name_sym->sy_name : NULL);
  vpop(1);
 }
 DCCType_Quit(&query_type);
 assert(type_str),
 DCCVStack_PushStr(type_str->s_text,type_str->s_size);
 TPPString_Decref(type_str);
 DCCParse_ParPairEnd();
}


/*  __builtin_constant_p  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinConstantP(void) {
 int is_constant;
 assert(TOK == KWD___builtin_constant_p);
 YIELD();
 DCCParse_ParPairBegin();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr();
 popf();
 DCCParse_ParPairEnd();
 is_constant = DCCStackValue_ISCONST(vbottom);
 vpop(1);
 vpushi(DCCTYPE_BOOL,is_constant);
}


/*  __builtin_choose_expr  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinChooseExpr(void) {
 int is_true;
 assert(TOK == KWD___builtin_choose_expr);
 YIELD();
 DCCParse_ParPairBegin();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr();
 popf();
 if (visconst_bool()) {
  is_true = vgtconst_bool();
 } else {
  WARN(W_BUILTIN_CHOOSE_EXPR_EXPECTED_CONSTANT_EXPRESSION);
  is_true = 0;
 }
 vpop(1);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (is_true) DCCParse_Expr(); else DCCParse_SkipExpr();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (is_true) DCCParse_SkipExpr(); else DCCParse_Expr();
 DCCParse_ParPairEnd();
}



/*  __builtin_types_compatible_p  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinTypesCompatibleP(void) {
 struct DCCType ta,tb;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 assert(TOK == KWD___builtin_types_compatible_p);
 YIELD();
 DCCParse_ParPairBegin();
 if (!DCCParse_CType(&ta,&attr)) WARN(W_EXPECTED_TYPE_AFTER_BUILTIN_TYPES_COMPATIBLE_P);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (!DCCParse_CType(&ta,&attr)) WARN(W_EXPECTED_TYPE_AFTER_BUILTIN_TYPES_COMPATIBLE_P);
 DCCParse_ParPairEnd();
 DCCAttrDecl_Quit(&attr);
 vpushi(DCCTYPE_BOOL,DCCType_IsCompatible(&ta,&tb,0));
 DCCType_Quit(&tb);
 DCCType_Quit(&ta);
}



/*  __builtin_unreachable, __builtin_trap, __builtin_breakpoint  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFence(void) {
 tok_t t = TOK;
 YIELD();
 DCCParse_ParPairBegin();
 DCCDisp_Fence(t);
 DCCParse_ParPairEnd();
 vpushv();
}




/*  __builtin_bswap(16|32|64)  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinBSwap(void) {
 tok_t mode = TOK;
 target_siz_t size,arg_size;
 YIELD();
 DCCParse_ParPairBegin();
 switch (mode) {
 default:
  DCCParse_Expr1();
  vprom();
  if (TOK == ',') {
   YIELD();
   size = (target_siz_t)DCCParse_CExpr(1);
  } else {
   size = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
  }
  break;
 {
  if (DCC_MACRO_FALSE) { case KWD___builtin_bswap16: size = 2; }
  if (DCC_MACRO_FALSE) { case KWD___builtin_bswap32: size = 4; }
  if (DCC_MACRO_FALSE) { case KWD___builtin_bswap64: size = 8; }
  DCCParse_Expr();
  vprom();
 } break;
 }
 DCCParse_ParPairEnd();
 DCCStackValue_LoadLValue(vbottom);
 DCCStackValue_Cow(vbottom);
 DCCStackValue_FixTest(vbottom);
 DCCStackValue_FixBitfield(vbottom);
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) vrcopy();
 switch (size) {
 {
  struct DCCType arg_type;
#ifdef DCCTYPE_IB1
  if (DCC_MACRO_FALSE) { case 1: arg_type.t_type = DCCTYPE_UNSIGNED|DCCTYPE_IB1; }
#endif
#ifdef DCCTYPE_IB2
  if (DCC_MACRO_FALSE) { case 2: arg_type.t_type = DCCTYPE_UNSIGNED|DCCTYPE_IB2; }
#endif
#ifdef DCCTYPE_IB4
  if (DCC_MACRO_FALSE) { case 4: arg_type.t_type = DCCTYPE_UNSIGNED|DCCTYPE_IB4; }
#endif
  if (DCC_MACRO_FALSE) { case 8: arg_type.t_type = DCCTYPE_UNSIGNED|DCCTYPE_IB8; }
  arg_type.t_base = NULL;
  vcast(&arg_type,0);
 } break;
 default: break;
 }
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST) {
  if (vbottom->sv_sym) DCCStackValue_Load(vbottom);
  else { /* Constant optimizations. */
   switch (size) {
   case 1: return;
   case 2: vbottom->sv_const.u16 = DCC_BSWAP16(vbottom->sv_const.u16); break;
   case 4: vbottom->sv_const.u32 = DCC_BSWAP32(vbottom->sv_const.u32); break;
   case 8: vbottom->sv_const.u64 = DCC_BSWAP64(vbottom->sv_const.u64); break;
   default: goto copy_elem;
   }
   return;
  }
 } else {
copy_elem:
  /* Create a copy of the argument. */
  vrcopy();
 }
 arg_size = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
 if (arg_size != size) {
  WARN(W_BUILTIN_BSWAP_INCORRECT_SIZE,&vbottom->sv_ctype,size);
  if (size < arg_size) arg_size = size;
 }
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc loc;
  loc.ml_reg = vbottom->sv_reg;
  loc.ml_off = vbottom->sv_const.offset;
  loc.ml_sym = vbottom->sv_sym;
  DCCDisp_BSwapMem(&loc,arg_size);
 } else {
  assert(vbottom->sv_reg);
  DCCDisp_BSwapReg(vbottom->sv_reg);
  if (vbottom->sv_reg2) {
   rc_t temp;
   /* Swap the second register. */
   DCCDisp_BSwapReg(vbottom->sv_reg2);
   /* Exchange the registers. */
   temp = vbottom->sv_reg2;
   vbottom->sv_reg2 = vbottom->sv_reg;
   vbottom->sv_reg = temp;
  }
 }
}




/*  __builtin_alloca */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAlloca(void) {
 /* Allocate stack memory. */
 assert(TOK == KWD___builtin_alloca);
 DCCParse_WarnAllocaInLoop();
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 DCCParse_ParPairEnd();
 vpushxr(DCC_RR_XSP); /* x, %esp */
 vswap();             /* %esp, x */
 vgen2('-');          /* %esp */
 vpop(1);             /* Force apply disposition. */
 vpushxr(DCC_RR_XSP); /* Push the ESP register again. */
 vcast_pt(DCCTYPE_VOID,1);
}



/*  __builtin_alloca_with_align */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAllocaWithAlign(void) {
 /* Allocate stack memory. */
 assert(TOK == KWD___builtin_alloca_with_align);
 DCCParse_WarnAllocaInLoop();
 YIELD();
 /* generate code equivalent to:
  * >> %esp -= s;
  * >> %esp &= -a;
  * >> return %esp
  */
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vpushxr(DCC_RR_XSP); /* s, %esp */
 vswap();             /* %esp, s */
 vgen2('-');          /* %esp-=s */
 vpop(1);             /* . */
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1();
 vrcopy();            /* na=a   (Make sure alignment is just a copy of itself) */
 /* Prevent warnings about using '-' with an unsigned operand. */
 if (DCCTYPE_ISUNSIGNED(vbottom->sv_ctype.t_type))
     vbottom->sv_ctype.t_type &= ~(DCCTYPE_UNSIGNED);
 vgen1('-');          /* na=-na (Same as 'na = ~(na-1)') */
 vpushxr(DCC_RR_XSP); /* na, %esp */
 vswap();             /* %esp, na */
 vgen2('&');          /* %esp &= na */
 vcast_pt(DCCTYPE_VOID,1);
 DCCParse_ParPairEnd();
}



/*  __builtin_assume  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAssume(void) {
 assert(TOK == KWD___builtin_assume ||
        TOK == KWD___assume);
 YIELD();
 DCCParse_ParPairBegin();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr();
 popf();
 DCCParse_ParPairEnd();
 /* If a compile-time assumption fails, it means
  * this code location can't possibly be reached. */
 if (visconst_bool() && !vgtconst_bool()) {
  if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD) &&
        compiler.c_scope.s_kind == DCC_SCOPEKIND_FUNC) {
   /* Warn if this failed assumption can't be avoided. */
   WARN(W_FAILED_ASSUMPTION_UNAVOIDABLE);
  }
  DCCDisp_Fence(DCC_FENCE_UNREACHABLE);
 }
 /* xxx: maybe do something else with 'vbottom?' */
 vpop(1);
 vpushv();
}



/*  __builtin_expect  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinExpect(void) {
 assert(TOK == KWD___builtin_expect);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr();
 vpop(1); /* Simply ignore assumptions. */
 popf();
 DCCParse_ParPairEnd();
 vrval();
}



/*  __builtin_FILE  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFILE(void) {
 char const *p; size_t s;
 assert(TOK == KWD___builtin_FUNCTION);
 p = TPPLexer_FILE(&s);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_ParPairEnd();
 DCCVStack_PushStr(p,s);
}



/*  __builtin_LINE  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinLINE(void) {
 int line;
 assert(TOK == KWD___builtin_LINE);
 line = TPPLexer_LINE();
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_ParPairEnd();
 vpushi(DCCTYPE_INT,line);
}



/*  __builtin_FUNCTION  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFUNCTION(void) {
 struct TPPKeyword const *function_name;
 assert(TOK == KWD___builtin_FUNCTION);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_ParPairEnd();
 function_name = compiler.c_fun
  ? compiler.c_fun->d_name
  : &TPPKeyword_Empty;
 assert(function_name);
 DCCVStack_PushStr(function_name->k_name,
                   function_name->k_size);
}



/*  __builtin_bitfield  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinBitField(void) {
 int_t bit_index,bit_size;
 assert(TOK == KWD___builtin_bitfield);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 /* bit-wise integral access. */
 bit_index = DCCParse_CExpr(1),bit_size = 1;
 if (TOK == ',') { YIELD(); bit_size = DCCParse_CExpr(1); }
 DCCParse_ParPairEnd();
 vbitfld((uint8_t)bit_index,(uint8_t)bit_size);
}




/*  __builtin_offsetof  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinOffsetof(void) {
 struct DCCType type;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 assert(TOK == KWD___builtin_offsetof);
 YIELD();
 /* This builtin is only provided for compatibility with gcc.
  * DCC already implicitly supports the dump approach to such
  * functionality (which can also already be resolved at compile-time):
  * >> #define offsetof(s,m)  (__SIZE_TYPE__)&((s *)0)->m
  */
 DCCParse_ParPairBegin();
 DCCParse_CTypeOnly(&type,&attr);
 DCCAttrDecl_Quit(&attr);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCType_MkPointer(&type);
 vpushc(&type,0);
 DCCType_Quit(&type);
 vgen1('*');
 /* Really hacky way to get the suffix
  * parser to start by reading a '.' */
 assert(TOKEN.t_begin >= TOKEN.t_file->f_begin);
 assert(TOKEN.t_begin <= TOKEN.t_file->f_end);
 TOKEN.t_file->f_pos = TOKEN.t_begin;
 TOKEN.t_id = '.';

 /* Parse unary suffix expressions. */
 DCCParse_ExprUnarySuffix();
 vgen1('&');
 /* Cast the result to 'size_t'. */
 vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
 DCCParse_ParPairEnd();
}




/*  __builtin_assume_aligned  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAssumeAligned(void) {
 assert(TOK == KWD___builtin_assume_aligned);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vcast_pt(DCCTYPE_VOID,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 /* Discard all other arguments. */
 DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
}


LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinScanner(void) {
 target_siz_t arg_size,size;
 rc_t result_register;
 int clz_mode = 0;
 tok_t mode = TOK; YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 switch (mode) {
 case KWD___builtin_clz:   clz_mode = 1;
 case KWD___builtin_ffs:   size = DCC_TARGET_SIZEOF_INT; break;
 case KWD___builtin_clzl:  clz_mode = 1;
 case KWD___builtin_ffsl:  size = DCC_TARGET_SIZEOF_LONG; break;
 case KWD___builtin_clzll: clz_mode = 1;
 case KWD___builtin_ffsll: size = DCC_TARGET_SIZEOF_LONG_LONG; break;
 case KWD___builtin_clzcc: clz_mode = 1;
 default:
  if (TOK != ',') size = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
  else YIELD(),size = (target_siz_t)DCCParse_CExpr(1);
  break;
 }
 DCCParse_ParPairEnd();
 vprom();
 DCCStackValue_FixBitfield(vbottom);
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) vrcopy();
 switch (size) {
 {
  struct DCCType arg_type;
#ifdef DCCTYPE_IB1
  if (DCC_MACRO_FALSE) { case 1: arg_type.t_type = DCCTYPE_IB1; }
#endif
#ifdef DCCTYPE_IB2
  if (DCC_MACRO_FALSE) { case 2: arg_type.t_type = DCCTYPE_IB2; }
#endif
#ifdef DCCTYPE_IB4
  if (DCC_MACRO_FALSE) { case 4: arg_type.t_type = DCCTYPE_IB4; }
#endif
#ifdef DCCTYPE_IB8
  if (DCC_MACRO_FALSE) { case 8: arg_type.t_type = DCCTYPE_IB8; }
#endif
  arg_type.t_base = NULL;
  vcast(&arg_type,0);
 } break;
 default: break;
 }
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST) {
  if (vbottom->sv_sym)cstdef: DCCStackValue_Load(vbottom);
  else { /* Constant optimizations. */
   uint64_t iv,shift; int result;
   switch (size) {
   case 1: iv = (uint64_t)vbottom->sv_const.u8; break;
   case 2: iv = (uint64_t)vbottom->sv_const.u16; break;
   case 4: iv = (uint64_t)vbottom->sv_const.u32; break;
   case 8: iv = (uint64_t)vbottom->sv_const.u64; break;
   default: goto default_ffs;
   }
   if (clz_mode) {
    result = 0,shift = (uint64_t)1 << 63;
    while (!(iv&shift)) {
     /* Generate code and let the CPU decide on undefined behavior! */
     if (++result == 64) goto cstdef;
     shift >>= 1;
    }
    /* Adjust for zero-padding on constants. */
    result -= (8-size)*8;
   } else {
    result = 1,shift = 1;
    while (!(iv&shift)) {
     if (++result == 64) break;
     shift <<= 1;
    }
   }
   DCCType_Quit(&vbottom->sv_ctype);
   vbottom->sv_ctype.t_type = DCCTYPE_INT;
   vbottom->sv_ctype.t_base = NULL;
   assert(!vbottom->sv_sym);
   vbottom->sv_const.it = (int_t)result;
   vbottom->sv_flags    = DCC_SFLAG_NONE;
   return;
  }
 }
default_ffs:
 arg_size = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
 if (arg_size != size) {
  WARN(W_BUILTIN_FFS_INCORRECT_SIZE,&vbottom->sv_ctype,size);
  if (size < arg_size) arg_size = size;
 }
 if ((vbottom->sv_reg&DCC_RC_IN(DCC_TARGET_SIZEOF_INT)) &&
     !(vbottom->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_TEST)) &&
     !DCC_ASMREG_ISSPTR(vbottom->sv_reg&DCC_RI_MASK)
     ) result_register = vbottom->sv_reg;
 else result_register = DCCVStack_GetReg(DCC_RC_IN(DCC_TARGET_SIZEOF_INT),1);
 if (vbottom->sv_flags&DCC_SFLAG_TEST) {
  /* Special optimization: Since a test is either 0/1, its ffs-value is equal to it! */
  vcast_t(DCCTYPE_INT,1);
  return;
 }
 vpushr(result_register); /* x, res */
 vswap();                 /* res, x */
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc loc;
  loc.ml_reg = vbottom->sv_reg;
  loc.ml_off = vbottom->sv_const.offset;
  loc.ml_sym = vbottom->sv_sym;
  clz_mode ? DCCDisp_CLZMem(&loc,result_register,arg_size)
           : DCCDisp_FFSMem(&loc,result_register,arg_size);
 } else {
  /* Scan register(s). */
  clz_mode ? DCCDisp_CLZRegs(vbottom->sv_reg,vbottom->sv_reg2,result_register)
           : DCCDisp_FFSRegs(vbottom->sv_reg,vbottom->sv_reg2,result_register);
 }
 vpop(1);                 /* res */
 vrval();                 /* rres */
}

// TODO: int __builtin_ctz(unsigned int x);
// TODO: int __builtin_popcount(unsigned int x);
// TODO: int __builtin_parity(unsigned int x);



#define VA_SIZE(x) (((x)+(DCC_TARGET_VA_ALIGN-1)) & ~(DCC_TARGET_VA_ALIGN-1))

PRIVATE target_off_t dcc_va_off(void) {
 target_off_t result = DCC_TARGET_SIZEOF_POINTER*2;
 if (compiler.c_fun) {
  struct DCCDecl *funty_decl = compiler.c_fun->d_type.t_base;
  if (funty_decl &&
     (funty_decl->d_kind == DCC_DECLKIND_FUNCTION ||
      funty_decl->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
   if (!(funty_decl->d_flag&DCC_DECLFLAG_VARIADIC))
         WARN(W_BUILTIN_VA_START_NO_VARARGS);
   if (funty_decl->d_tdecl.td_size) {
    struct DCCStructField *last_field;
    if (funty_decl->d_flag&DCC_DECLFLAG_VARIADICLAST) {
     if (funty_decl->d_tdecl.td_size >= 2) {
      /* Add the Alignment offset of the last argument. */
      last_field = &funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-2];
      assert(last_field->sf_decl);
      result += VA_SIZE(DCCType_Sizeof(&last_field->sf_decl->d_type,NULL,0));
      result += last_field->sf_off;
     }
    } else {
     last_field = &funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-1];
     assert(last_field->sf_decl);
     result   += last_field->sf_off;
     result   += VA_SIZE(DCCType_Sizeof(&last_field->sf_decl->d_type,NULL,0));
    }
   }
   if ((funty_decl->d_attr) &&
       (funty_decl->d_attr->a_specs&DCC_ATTRSPEC_NAKED))
        result -= DCC_TARGET_SIZEOF_POINTER;
  }
 }
 return result;
}
PRIVATE rc_t dcc_va_base(void) {
 rc_t result = DCC_RR_XBP;
 if (compiler.c_fun) {
  struct DCCDecl *funty_decl = compiler.c_fun->d_type.t_base;
  if (funty_decl->d_attr &&
     (funty_decl->d_attr->a_specs&DCC_ATTRSPEC_NAKED))
      result = DCC_RR_XSP;
 }
 return result;
}
PRIVATE target_off_t dcc_va_start(void) {
 target_off_t result = DCC_TARGET_SIZEOF_POINTER*2;
 if (compiler.c_fun) {
  struct DCCDecl *funty_decl = compiler.c_fun->d_type.t_base;
  if (funty_decl &&
     (funty_decl->d_kind == DCC_DECLKIND_FUNCTION ||
      funty_decl->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
   if (!(funty_decl->d_flag&DCC_DECLFLAG_VARIADIC))
         WARN(W_BUILTIN_VA_START_NO_VARARGS);
   if (funty_decl->d_tdecl.td_size) {
    if (funty_decl->d_flag&DCC_DECLFLAG_VARIADICLAST) {
     if (funty_decl->d_tdecl.td_size >= 2) {
      result += funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-2].sf_off;
     }
    } else {
     result += funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-1].sf_off;
    }
   }
   if ((funty_decl->d_attr) &&
       (funty_decl->d_attr->a_specs&DCC_ATTRSPEC_NAKED))
        result -= DCC_TARGET_SIZEOF_POINTER;
  }
 }
 return result;
}

/*  __builtin_va_start  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaStart(void) {
 target_siz_t alignment;
 assert(TOK == KWD___builtin_va_start);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') {
  /* Load the default location for the va-argument. */
  struct DCCStackValue vararg;
  WARN(W_BUILTIN_VA_START_MISSING_SECOND_ARGUMENT);
  vararg.sv_ctype.t_type = DCCTYPE_VOID;
  vararg.sv_ctype.t_base = NULL;
  vararg.sv_flags        = DCC_SFLAG_LVALUE;
  vararg.sv_reg          = dcc_va_base();
  vararg.sv_reg2         = DCC_RC_CONST;
  vararg.sv_const.it     = (int_t)dcc_va_off();
  vararg.sv_sym          = NULL;
  vpush(&vararg);
 } else {
  target_off_t default_off;
  YIELD();
  DCCParse_Expr1();
  /* Check if the va-argument is the last element before the '...' */
  if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) ||
       (vbottom->sv_sym != NULL) ||
       (vbottom->sv_reg != dcc_va_base()) ||
       ((default_off     = dcc_va_start()) != 0 &&
        (vbottom->sv_const.offset != default_off))) {
   WARN(W_BUILTIN_VA_START_NOT_LAST_ARG);
  }
 }
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 alignment = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
 alignment = VA_SIZE(alignment);
 /* ap = (va_list)(((uintptr_t)&start)+alignment) */
 vgen1('&');
 vrcopy();
 vcast_t(DCCTYPE_UNSIGNED|DCCTYPE_INTPTR,1);
 vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,alignment);
 vgen2('+');
 /* Store the calculated address in 'ap'. */
 vcast_pt(DCCTYPE_CHAR,1);
 vstore(0);
 vpop(1);
 vpushv();
}

/*  __builtin_va_copy  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaCopy(void) {
 assert(TOK == KWD___builtin_va_copy);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 vstore(0);
 vpop(1);
 vpushv();
}

/*  __builtin_va_end  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaEnd(void) {
 assert(TOK == KWD___builtin_va_end);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 if (!(compiler.c_flags&DCC_COMPILER_FLAG_NOCLEANUP)) {
  vpushi(DCCTYPE_INT,0);
  vstore(0);
 }
 vpop(1);
 vpushv();
}

/*  __builtin_va_arg  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaArg(void) {
 struct DCCType va_type;
 struct DCCAttrDecl va_attr = DCCATTRDECL_INIT;
 target_siz_t va_size;
 assert(TOK == KWD___builtin_va_arg);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_CTypeOnly(&va_type,&va_attr);
 DCCAttrDecl_Quit(&va_attr);
 va_size = VA_SIZE(DCCType_Sizeof(&va_type,NULL,1));
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 vdup(1);           /* ap, dap */
 vswap();           /* dap, ap */
 vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,va_size); /* dap, ap, siz */
 vgen2('+');        /* dap, ap+=siz */
 vpop(1);           /* dap */
 DCCType_MkPointer(&va_type);
 vcast(&va_type,1); /* (T *)dap */
 DCCType_Quit(&va_type);
 vgen1('*');        /* *(T *)dap */
 vrval();           /* Cause warnings when the caller tries to assign to the VA-argument. */
}


/* Layout of a target jump buffer (the last field is used for EIP) */
static rc_t const target_jmpbuf[] = {
 DCC_RR_XBX,DCC_RR_XDI,DCC_RR_XSI,
 DCC_RR_XSP,DCC_RR_XBP,DCC_RC_CONST
};
/* Signal-value register used for setjmp/longjmp */
#define TARGET_LONGJMP_REGISTER   (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX)


/*  __builtin_setjmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinSetJmp(void) {
 struct DCCMemLoc jmpbuf_loc;
 struct DCCSymAddr eip; rc_t const *iter;
 assert(TOK == KWD___builtin_setjmp);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 DCCParse_ParPairEnd();
 DCCStackValue_LoadLValue(vbottom);
 DCCType_CheckWritable(&vbottom->sv_ctype);
 DCCVStack_KillAll(); /* Kill all registers. */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 if (DCCType_Sizeof(&vbottom->sv_ctype,NULL,1) != DCC_TARGET_SIZEOF_JMP_BUF)
  WARN(W_BUILTIN_JMPBUF_HAS_INCORRECT_SIZE,&vbottom->sv_ctype);
 jmpbuf_loc.ml_reg = vbottom->sv_reg;
 jmpbuf_loc.ml_off = vbottom->sv_const.offset;
 jmpbuf_loc.ml_sym = vbottom->sv_sym;
 for (iter = target_jmpbuf; *iter; ++iter) {
  DCCDisp_RegMovMem(*iter,&jmpbuf_loc);
  jmpbuf_loc.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 }
 eip.sa_sym = DCCUnit_AllocSym();
 if unlikely(!eip.sa_sym) return;
 eip.sa_off = 0;
 /* Save EIP */
 DCCDisp_CstMovMem(&eip,&jmpbuf_loc,DCC_TARGET_SIZEOF_POINTER);
 vpop(1); /* pop the jump buffer. */
 /* Clear the longjmp register. */
 DCCDisp_RegBinReg('^',TARGET_LONGJMP_REGISTER,
                       TARGET_LONGJMP_REGISTER,1);
 /* This is where a longjmp will jmp to! */
 t_defsym(eip.sa_sym);
 /* Push the longjmp register. */
 vpushr(TARGET_LONGJMP_REGISTER);
 vrval();
}

/*  __builtin_longjmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinLongJmp(void) {
 struct DCCMemLoc jmpbuf_loc;
 struct DCCMemLoc restore_last_loc;
 rc_t const *iter;
 assert(TOK == KWD___builtin_longjmp);
 YIELD();
 /* NOTE: No need to kill all registers in here, because this function function
  *       will simply mark the control flow as dead, meaning that all registers
  *       still alive when one gets here are, will be meaningless afterwards! */
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1();
 DCCStackValue_LoadLValue(&vbottom[1]);
 if (!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(&vbottom[1]);
 if (DCCType_Sizeof(&vbottom[1].sv_ctype,NULL,1) != DCC_TARGET_SIZEOF_JMP_BUF)
  WARN(W_BUILTIN_JMPBUF_HAS_INCORRECT_SIZE,&vbottom[1].sv_ctype);
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
fix_zero_signal:
  /* Load the signal value into the longjmp register. */
  DCCStackValue_LoadExplicit(vbottom,TARGET_LONGJMP_REGISTER);
  /* Load '1' into '%ECX' (used below in conditional mov) */
  DCCDisp_IntMovReg(1,DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_ECX);
  /* Test the register for being ZERO(0). */
  DCCDisp_RegBinReg('t',TARGET_LONGJMP_REGISTER,
                        TARGET_LONGJMP_REGISTER,1);
  /* If the longjmp register was ZERO(0), mov '%ECX' (which is ONE(1)) into it. */
  DCCDisp_RegCMovReg(DCC_TEST_Z,DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_ECX,
                                TARGET_LONGJMP_REGISTER,1);
 } else if (vbottom->sv_sym) {
  /* non-lvalue symbol means that a non-zero integer is added to the
   * signal, thus implying that the signal should never be ZERO(0). */
 } else if (vbottom->sv_reg != DCC_RC_CONST) {
  /* The value is dependent on a register. - Must make that that register isn't ZERO(0). */
  goto fix_zero_signal;
 } else if (vbottom->sv_const.it == 0) {
  /* symbol-independent constant expression is ZERO(0). */
  WARN(W_BUILTIN_LONGJMP_VALZERO_IS_ONE);
  vbottom->sv_const.it = 1;
 }
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 vpushr(TARGET_LONGJMP_REGISTER); /* buf, val, %REG */
 vswap();                         /* buf, %REG, val */
 vstore(1);                       /* buf, %REG=val */
 vpop(1);                         /* buf */
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 jmpbuf_loc.ml_reg = vbottom->sv_reg;
 jmpbuf_loc.ml_off = vbottom->sv_const.offset;
 jmpbuf_loc.ml_sym = vbottom->sv_sym;
 vpop(1);                         /* . */
 restore_last_loc.ml_reg = DCC_RC_CONST;
 /* Restore all regular registers (except for a buffer-location-dependent one). */
 for (iter = target_jmpbuf; *iter; ++iter) {
  if ((jmpbuf_loc.ml_reg&DCC_RC_I) &&
     (*iter&7) == (jmpbuf_loc.ml_reg&7)) {
   /* This register must be restored last! */
   restore_last_loc = jmpbuf_loc;
  } else {
   /* Restore this register. */
   DCCDisp_MemMovReg(&jmpbuf_loc,*iter);
  }
  jmpbuf_loc.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 }
 if (restore_last_loc.ml_reg != DCC_RC_CONST) {
  /* When restoring a last register, we know that the completion of
   * that action will make it impossible to access the jump buffer again.
   * Yet because it still contains the EIP of where we're supposed to jump to,
   * we must load that address into a temporary register (which must be an IA-32 clobber)
   * >> With '%EAX' out of the question (already used as jump
   *    signal value), that leaves us either '%ECX' or '%EDX' (we choose %ECX here) */
  DCCDisp_MemMovReg(&jmpbuf_loc,DCC_RR_XCX);
  DCCDisp_MemMovReg(&restore_last_loc,jmpbuf_loc.ml_reg);
  DCCDisp_RegJmp(DCC_RR_XCX);
 } else {
  /* The jump buffer memory location does not depend on a register.
   * With that in mind, we can simply to a direct jmp to
   * the stored EIP, to which 'jmpbuf_loc' already points. */
  DCCDisp_MemJmp(&jmpbuf_loc,DCC_TARGET_SIZEOF_POINTER);
 }
 /* mark the current branch as dead. */
 compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                      DCC_COMPILER_FLAG_DEAD);
 vpushv(); /* void */
}


PRIVATE void 
push_vpfun_sym(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_VOID,NULL};
 DCCType_MkPointer(&type);
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
PRIVATE void 
push_ifun_sym(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_INT,NULL};
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
PRIVATE void 
push_szfun_sym(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_SIZE|DCCTYPE_UNSIGNED,NULL};
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}


/*  __builtin_memcpy, __builtin_memmove  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinMemcpy(void) {
 int may_overlap = TOK == KWD___builtin_memmove;
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 /* dst, src, size */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST &&
      !vbottom->sv_sym) {
  struct DCCMemLoc src_loc,dst_loc;
  target_siz_t copy_size = (target_siz_t)vbottom->sv_const.offset;
  int compiletime_overlap;
  if (!copy_size) {
   /* Special case: Empty copy/move. */
fix_stack:
   vpop(1);  /* dst, src */
   vpop(1);  /* dst */
   vrcopy(); /* ddst */
   return;
  }
  /* Generate a compile-time memcpy. */
  if (vbottom[2].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[2]);
  if (vbottom[1].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[1]);
  assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE));
  src_loc.ml_reg = vbottom[1].sv_reg;
  src_loc.ml_off = vbottom[1].sv_const.offset;
  src_loc.ml_sym = vbottom[1].sv_sym;
  dst_loc.ml_reg = vbottom[2].sv_reg;
  dst_loc.ml_off = vbottom[2].sv_const.offset;
  dst_loc.ml_sym = vbottom[2].sv_sym;
                                compiletime_overlap = DCCMemLoc_Contains(&src_loc,copy_size,&dst_loc);
  if (compiletime_overlap != 1) compiletime_overlap = DCCMemLoc_Contains(&dst_loc,copy_size,&src_loc);
  if (compiletime_overlap == 2) {
   /* Overlap could not be determined at compile-time. */
   if (may_overlap) goto call_extern; /*  */
  } else if (compiletime_overlap == 1) {
   if (DCCMemLoc_Equal(&src_loc,&dst_loc)) {
    WARN(W_BUILTIN_MEMCPY_POINTERS_ALWAYS_EQUAL);
    goto fix_stack;
   }
   /* The pointers _always_ overlap. */
   if (may_overlap) goto call_extern;
   WARN(W_BUILTIN_MEMCPY_POINTERS_ALWAYS_OVERLAP);
  } else if (may_overlap) {
   WARN(W_BUILTIN_MEMMOVE_POINTERS_NEVER_OVERLAP);
   goto call_extern;
  }
  assert(!may_overlap);
  /* Mark the destination pointer to copy-on-write. */
  vpop(1);  /* dst, src */
  vswap();  /* src, dst */
  vrcopy(); /* src, ddst */
  vswap();  /* ddst, src */
  /* Dispense compiler-generated memcpy() instructions. */
  DCCDisp_MemMovMem(&src_loc,copy_size,
                    &dst_loc,copy_size,1);
  vpop(1);  /* ddst */
 } else {
  struct DCCSym *funsym;
call_extern:
  funsym = DCCUnit_NewSyms(may_overlap ? "memmove" : "memcpy",DCC_SYMFLAG_NONE);
  if (funsym) push_vpfun_sym(funsym);
  else vpushv(); /* dst, src, size, memcpy */
  vlrot(4);      /* memcpy, dst, src, size */
  vcall(3);      /* ret */
 }
}





/*  __builtin_memset  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinMemset(void) {
 assert(TOK == KWD___builtin_memset);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID,0);
 DCCType_CheckWritablePtr(&vbottom->sv_ctype);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_INT,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 /* Compile-time optimizations for known memset sizes. */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST &&
      !vbottom->sv_sym) {
  struct DCCMemLoc dst_loc;
  target_siz_t fill_size = (target_siz_t)vbottom->sv_const.offset;
  if (!fill_size) {
   /* Special case: Empty memset. */
   vpop(1);  /* dst, byt */
   vpop(1);  /* dst */
   vrcopy(); /* ddst */
   return;
  }
  if (vbottom[2].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[2]);
  if (vbottom[1].sv_flags&DCC_SFLAG_LVALUE || vbottom[1].sv_sym) DCCStackValue_Load(&vbottom[1]);
  assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_sym));
  dst_loc.ml_reg = vbottom[2].sv_reg;
  dst_loc.ml_off = vbottom[2].sv_const.offset;
  dst_loc.ml_sym = vbottom[2].sv_sym;
  vpop(1);  /* dst, byt */
  vswap();  /* byt, dst */
  vrcopy(); /* byt, ddst */
  vswap();  /* ddst, byt */
  if (vbottom->sv_reg == DCC_RC_CONST) {
   /* The filler byte is known at compile-time. */
   DCCDisp_BytMovMem(vbottom->sv_const.u8,fill_size,&dst_loc,fill_size,1);
  } else {
   /* The filler byte is known at runtime. */
   DCCDisp_ByrMovMem(vbottom->sv_reg,fill_size,&dst_loc,fill_size,1);
  }
  vpop(1);
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms("memset",DCC_SYMFLAG_NONE);
  if (funsym) push_vpfun_sym(funsym);
  else vpushv(); /* dst, byte, size, memset */
  vlrot(4);      /* memset, dst, byte, size */
  vcall(3);      /* ret */
 }
}





/*  __builtin_memcmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinMemcmp(void) {
 assert(TOK == KWD___builtin_memcmp);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 /* a, b, size */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST &&
      !vbottom->sv_sym) {
  struct DCCMemLoc a_loc,b_loc; rc_t resreg,tempreg;
  target_siz_t cmp_size = (target_siz_t)vbottom->sv_const.offset;
  if (!cmp_size) {
   /* Special case: Empty copy/move. */
fix_stack:
   vpop(1);               /* a, b */
   vpop(1);               /* a */
   vpop(1);               /* . */
   vpushi(DCCTYPE_INT,0); /* 0 */
   return;
  }
  /* Generate a compile-time memcmp. */
  if (vbottom[2].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[2]);
  if (vbottom[1].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[1]);
  assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE));
  a_loc.ml_reg = vbottom[1].sv_reg;
  a_loc.ml_off = vbottom[1].sv_const.offset;
  a_loc.ml_sym = vbottom[1].sv_sym;
  b_loc.ml_reg = vbottom[2].sv_reg;
  b_loc.ml_off = vbottom[2].sv_const.offset;
  b_loc.ml_sym = vbottom[2].sv_sym;
  if (DCCMemLoc_Equal(&a_loc,&b_loc)) {
   WARN(W_BUILTIN_MEMCMP_POINTERS_ALWAYS_EQUAL);
   goto fix_stack;
  }
  vpop(1);  /* a, b */
  { /* Compile-time memcmp. */
   void const *data_a,*data_b;
   if ((data_a = DCCMemLoc_CompilerData(&a_loc,cmp_size)) != NULL &&
       (data_b = DCCMemLoc_CompilerDataUpdate(&b_loc,(void **)&data_a,cmp_size)) != NULL) {
    int result = memcmp(data_a,data_b,cmp_size);
    /* Clamp the result to -1, 0 or 1 */
         if (result < 0) result = -1;
    else if (result > 0) result = 1;
    vpop(1);
    vpop(1);
    vpushi(DCCTYPE_INT,result);
    return;
   }
  }

  /* Dispense compiler-generated memcpy() instructions. */
  DCCDisp_MemBinMem('?',&a_loc,cmp_size,
                        &b_loc,cmp_size,1);
  vpop(1); /* a */
  vpop(1); /* . */
  resreg = DCCVStack_GetReg(DCC_RC_IN(DCC_TARGET_SIZEOF_INT),1);
  /* Clear the result register. */
  { struct DCCSymAddr val = {0,NULL};
    DCCDisp_CstMovRegRaw(&val,resreg);
  }
  /* push the result register. */
  vpushr(resreg);
  vrval();
  /* Allocate a second temporary register.
   * NOTE: Only do so after we've pushed the result register,
   *       so-as to prevent the same register from being allocated. */
  tempreg = DCCVStack_GetReg(DCC_RC_IN(DCC_TARGET_SIZEOF_INT),1);
  { struct DCCSymAddr val = {-1,NULL};
    /* Store 0/1 in the result register when 'a > b'. */
    DCCDisp_SccReg(DCC_TEST_G,resreg&~(DCC_RC_I3264|DCC_RC_I16));
    /* Return -1 if 'a < b' */
    DCCDisp_CstMovRegRaw(&val,tempreg);
    DCCDisp_RegCMovReg(DCC_TEST_L,tempreg,resreg,1);
  }
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms("memcmp",DCC_SYMFLAG_NONE);
  if (funsym) push_ifun_sym(funsym);
  else vpushv(); /* a, b, size, memcmp */
  vlrot(4);      /* memcmp, a, b, size */
  vcall(3);      /* ret */
 }
}





/*  __builtin_strlen  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinStrlen(void) {
 assert(TOK == KWD___builtin_strlen);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_CHAR|DCCTYPE_CONST,0);
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST && vbottom->sv_sym &&
       DCCSym_ISDEFINED(vbottom->sv_sym)) {
  /* Special case: The address of the string is known at compile-time. */
  struct DCCSection *sec = DCCSym_SECTION(vbottom->sv_sym);
  uint8_t *section_begin; target_ptr_t secmax;
  target_ptr_t secaddr = vbottom->sv_sym->sy_addr;
  if (sec == unit.u_curr) {
   section_begin = unit.u_tbuf.tb_begin;
   secmax = (target_ptr_t)(unit.u_tbuf.tb_pos-section_begin);
  } else {
   section_begin = sec->sc_text.tb_begin;
   secmax = (target_ptr_t)(sec->sc_text.tb_pos-section_begin);
  }
  /* shouldn't happen, but make sure the string is really located inside the section. */
  if (secaddr >= secmax) secaddr = 0;
  else secaddr = (target_ptr_t)strnlen((char *)(section_begin+secaddr),
                                       (size_t)(secmax-secaddr));
  vpop(1); /* Push the actual string length. */
  vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,secaddr);
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms("strlen",DCC_SYMFLAG_NONE);
  if (funsym) push_szfun_sym(funsym);
  else vpushv(); /* str, strlen */
  vswap();       /* strlen, str */
  vcall(1);      /* ret */
 }
}





/*  __sync_val_compare_and_swap  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncCompareAndSwap(void) {
 struct DCCType *cas_type; struct DCCMemLoc cas_mem;
 target_siz_t cas_size; rc_t reg_class; int bool_mode;
 assert(TOK == KWD___sync_bool_compare_and_swap ||
        TOK == KWD___sync_val_compare_and_swap);
 bool_mode = (TOK == KWD___sync_bool_compare_and_swap);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vgen1('*'); /* Dereference first argument. */
 DCCStackValue_LoadLValue(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 cas_type = &vbottom->sv_ctype;
 cas_mem.ml_reg = vbottom->sv_reg;
 cas_mem.ml_off = vbottom->sv_const.offset;
 cas_mem.ml_sym = vbottom->sv_sym;
 cas_size = DCCType_Sizeof(cas_type,NULL,1);
 if (cas_size != 1 && cas_size != 2 && cas_size != 4)
  WARN(W_UNSUPPORTED_CAS_SIZE,cas_type),cas_size = 4;
 reg_class = cas_size == 1 ? DCC_RC_I8 :
             cas_size == 2 ? DCC_RC_I16|DCC_RC_I8 :
                             DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast(cas_type,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast(cas_type,0);
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCStackValue_LoadExplicit(&vbottom[1],(rc_t)(DCC_ASMREG_EAX|reg_class));
 DCCStackValue_LoadClass(&vbottom[0],reg_class,1);
 assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
 assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
 assert(vbottom[1].sv_reg == (DCC_ASMREG_EAX|reg_class));
 assert(vbottom[0].sv_reg != DCC_RC_CONST);
 /* *dst, EAX, reg */
 DCCDisp_AtomicRegCmpXchMemAX(vbottom[0].sv_reg,&cas_mem);
 if (bool_mode) {
  struct DCCStackValue test;
  test.sv_ctype.t_type = DCCTYPE_BOOL;
  test.sv_ctype.t_base = NULL;
  test.sv_flags        = DCC_SFLAG_MKTEST(DCC_TEST_E);
  test.sv_reg          = DCC_RC_CONST;
  test.sv_reg2         = DCC_RC_CONST;
  test.sv_sym          = NULL;
  test.sv_const.it     = 0;
  vpush(&test); /* *dst, EAX, reg, test */
  vlrot(4);     /* test, *dst, EAX, reg */
 }
 vpop(1); /* [test], *dst, EAX */
 vswap(); /* [test], EAX, *dst */
 vpop(1); /* [test], EAX */
 if (bool_mode) vpop(1); /* test */
 vrval(); /* rEAX/rtext */
}



/*  __sync_synchronize  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncSynchronize(void) {
 assert(TOK == KWD___sync_synchronize);
 YIELD();
 DCCParse_ParPairBegin();
 if (DCCParse_IsExpr()) DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCDisp_Fence(DCC_FENCE_MFENCE);
 vpushv();
}



/*  __sync_... */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncBinary(void) {
 struct DCCType *cas_type; struct DCCMemLoc cas_mem;
 target_siz_t cas_size; rc_t reg_class; tok_t opcode;
 int fmode = DCCDISP_ATOMICBIN_FETCHNEVER;
 switch (TOK) {
#define TRIPLE(op,fop,opf,id) \
 case fop: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE;\
 if (DCC_MACRO_FALSE) { case opf: fmode = DCCDISP_ATOMICBIN_FETCHAFTER; }\
 case op: opcode = id; break
 default:
 TRIPLE(KWD___sync_add,  KWD___sync_fetch_and_add,  KWD___sync_add_and_fetch,  '+');
 TRIPLE(KWD___sync_sub,  KWD___sync_fetch_and_sub,  KWD___sync_sub_and_fetch,  '-');
 TRIPLE(KWD___sync_or,   KWD___sync_fetch_and_or,   KWD___sync_or_and_fetch,   '|');
 TRIPLE(KWD___sync_and,  KWD___sync_fetch_and_and,  KWD___sync_and_and_fetch,  '&');
 TRIPLE(KWD___sync_xor,  KWD___sync_fetch_and_xor,  KWD___sync_xor_and_fetch,  '^');
 TRIPLE(KWD___sync_nand, KWD___sync_fetch_and_nand, KWD___sync_nand_and_fetch, '~');
 TRIPLE(KWD___sync_store,KWD___sync_fetch_and_store,KWD___sync_store_and_fetch,'=');
#undef TRIPLE
 case KWD___sync_lock_test_and_set: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE; opcode = '='; break;
 }
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vgen1('*'); /* Dereference first argument. */
 DCCStackValue_LoadLValue(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 cas_type = &vbottom->sv_ctype;
 cas_mem.ml_reg = vbottom->sv_reg;
 cas_mem.ml_off = vbottom->sv_const.offset;
 cas_mem.ml_sym = vbottom->sv_sym;
 cas_size = DCCType_Sizeof(cas_type,NULL,1);
 if (cas_size != 1 && cas_size != 2 && cas_size != 4)
  WARN(W_UNSUPPORTED_CAS_SIZE,cas_type),cas_size = 4;
 reg_class = cas_size == 1 ? DCC_RC_I8 :
             cas_size == 2 ? DCC_RC_I16|DCC_RC_I8 :
                             DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast(cas_type,0);
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCStackValue_LoadClass(&vbottom[0],reg_class,1);
 assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
 assert(vbottom[0].sv_reg != DCC_RC_CONST);
 DCCDisp_AtomicRegBinMem(opcode,fmode,vbottom[0].sv_reg,&cas_mem);
 vswap(); /* value, ptr */
 vpop(1); /* value */
 vrval(); /* rvalue */
 if (fmode == DCCDISP_ATOMICBIN_FETCHNEVER) {
  vpop(1);  /* . */
  vpushv(); /* void */
 }
}




/*  __sync_lock_release  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncUnary(void) {
 struct DCCType *cas_type; struct DCCMemLoc cas_mem;
 target_siz_t cas_size; tok_t opcode; struct DCCStackValue result;
 int fmode = DCCDISP_ATOMICBIN_FETCHNEVER;
 switch (TOK) {
#define TRIPLE(op,fop,opf,id) \
 case fop: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE;\
 if (DCC_MACRO_FALSE) { case opf: fmode = DCCDISP_ATOMICBIN_FETCHAFTER; }\
 case op: opcode = id; break
 default:
 TRIPLE(KWD___sync_inc,KWD___sync_fetch_and_inc,KWD___sync_inc_and_fetch,TOK_INC);
 TRIPLE(KWD___sync_dec,KWD___sync_fetch_and_dec,KWD___sync_dec_and_fetch,TOK_DEC);
 TRIPLE(KWD___sync_neg,KWD___sync_fetch_and_neg,KWD___sync_neg_and_fetch,'-');
 TRIPLE(KWD___sync_not,KWD___sync_fetch_and_not,KWD___sync_not_and_fetch,'~');
#undef TRIPLE
 case KWD___sync_lock_release: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE; opcode = '='; break;
 }
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vgen1('*'); /* Dereference first argument. */
 DCCStackValue_LoadLValue(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 cas_type = &vbottom->sv_ctype;
 cas_mem.ml_reg = vbottom->sv_reg;
 cas_mem.ml_off = vbottom->sv_const.offset;
 cas_mem.ml_sym = vbottom->sv_sym;
 cas_size = DCCType_Sizeof(cas_type,NULL,1);
 if (cas_size != 1 && cas_size != 2 && cas_size != 4)
  WARN(W_UNSUPPORTED_CAS_SIZE,cas_type),cas_size = 4;
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 result.sv_reg      = cas_size == 1 ? DCC_RC_I8 :
                      cas_size == 2 ? DCC_RC_I16|DCC_RC_I8 :
                                      DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 result.sv_reg2     = DCC_RC_CONST;
 result.sv_const.it = 0;
 result.sv_sym      = NULL;
 result.sv_flags    = DCC_SFLAG_RVALUE;
 if (fmode != DCCDISP_ATOMICBIN_FETCHNEVER) {
  result.sv_reg = DCCVStack_GetReg(result.sv_reg,1);
  DCCType_InitCopy(&result.sv_ctype,cas_type);
 } else {
  result.sv_ctype.t_base = NULL;
  result.sv_ctype.t_type = DCCTYPE_VOID;
 }
 vpush(&result);
 if (opcode == '=') {
  /* Store '0' in the result register, which will be exchanged. */
  DCCDisp_IntMovReg(0,result.sv_reg);
  DCCDisp_AtomicRegBinMem(opcode,DCCDISP_ATOMICBIN_FETCHBEFORE,
                          result.sv_reg,&cas_mem);
 } else {
  DCCDisp_AtomicUnaryMem(opcode,fmode,result.sv_reg,&cas_mem);
  result.sv_reg = DCC_RC_CONST;
 }
 vswap(); /* result, ptr */
 vpop(1); /* result */
}




/* https://gcc.gnu.org/onlinedocs/gcc/Return-Address.html */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinReturnAddr(void) {
 struct DCCStackValue sval;
 int want_frame;
 target_off_t level;
 assert(TOK == KWD___builtin_return_address ||
        TOK == KWD___builtin_frame_address);
 want_frame = TOK == KWD___builtin_frame_address;
 YIELD();
 DCCParse_ParPairBegin();
 pushf();
 DCCParse_Expr1();
 /* unsigned ptrdiff makes more sense in my opinion... */
 vcast_t(DCCTYPE_PTRDIFF|DCCTYPE_UNSIGNED,0);
 if (visconst_int()) level = (target_off_t)vgtconst_int();
 else WARN(W_BUILTIN_RETURN_ADDRESS_CONST_LEVEL),level = 0;
 if (level < 0) WARN(W_BUILTIN_RETURN_ADDRESS_NEG_LEVEL),level = -level;
 vpop(1);
 popf();
 DCCParse_ParPairEnd();
 if (level == 0) {
  if (want_frame) {
   /* Simply push the frame register. */
   sval.sv_ctype.t_type = DCCTYPE_VOID;
   sval.sv_ctype.t_base = NULL;
   DCCType_MkPointer(&sval.sv_ctype);
   sval.sv_flags        = DCC_SFLAG_RVALUE;
   sval.sv_reg          = DCCDISP_RETURNFRAME_REG;
   sval.sv_reg2         = DCC_RC_CONST;
   sval.sv_const.it     = 0;
   sval.sv_sym          = NULL;
   goto end_pushsval;
  } else {
   /* Special case: Same as the ASM '.'-extension (aka. current text address)
    * >> For this case, no code must be generated! */
   struct DCCSym *sym = DCCUnit_AllocSym();
   sym ? vpushs(sym),t_defsym(sym)
       : DCCVStack_PushAddr(unit.u_curr,t_addr); /* fallback... */
   vgen1('&');
   vrval();
  }
  return;
 }
 sval.sv_ctype.t_type = DCCTYPE_VOID;
 sval.sv_ctype.t_base = NULL;
 DCCType_MkPointer(&sval.sv_ctype);
 sval.sv_flags        = DCC_SFLAG_LVALUE|DCC_SFLAG_RVALUE;
 sval.sv_reg2         = DCC_RC_CONST;
 sval.sv_const.it     = want_frame ? DCCDISP_RETURNFRAME_OFF : DCCDISP_RETURNADDR_OFF;
 sval.sv_sym          = NULL;
 if (level == 1) {
  /* Special case: The first-level return address can be accessed directly. */
  sval.sv_reg = DCCDISP_RETURNADDR_REG;
#if DCCDISP_RETURNFRAME_REG != DCCDISP_RETURNADDR_REG
  if (want_frame) sval.sv_reg = DCCDISP_RETURNFRAME_REG;
#endif
 } else {
  struct DCCMemLoc parent_frame;
  /* Difficult case: Must actually generate code to access the return/frame address. */
  sval.sv_reg         = DCCVStack_GetReg(DCC_RC_PTR,1);
  parent_frame.ml_reg = DCCDISP_RETURNFRAME_REG;
  parent_frame.ml_off = DCCDISP_RETURNFRAME_OFF;
  parent_frame.ml_sym = NULL;
  while (level-- >= 2) {
   DCCDisp_MemMovReg(&parent_frame,sval.sv_reg);
   parent_frame.ml_reg = sval.sv_reg;
  }
 }
end_pushsval:
 vpush(&sval);
 assert(vbottom->sv_flags&DCC_SFLAG_RVALUE);
 assert(sval.sv_ctype.t_base);
 DCCDecl_Decref(sval.sv_ctype.t_base);
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinExtractReturnAddr(void) {
 assert(TOK == KWD___builtin_extract_return_addr ||
        TOK == KWD___builtin_frob_return_address);
 YIELD();
 /* None of DCC's targets so weird stuff to the pointer! */
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vcast_pt(DCCTYPE_VOID,0);
 DCCParse_ParPairEnd();
}



DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_BUILTINS_C_INL */
