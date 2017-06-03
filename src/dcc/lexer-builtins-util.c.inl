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
#ifndef GUARD_DCC_LEXER_BUILTINS_UTIL_C_INL
#define GUARD_DCC_LEXER_BUILTINS_UTIL_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/vstack.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

/*  __builtin_bswap(16|32|64)  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinBSwap(void) {
 tok_t mode = TOK;
 target_siz_t size,arg_size;
 YIELD();
 DCCParse_ParPairBegin();
 switch (mode) {
 default:
  DCCParse_Expr1(),vused();
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
  DCCParse_Expr1(),vused();
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
 vwunused();
}


LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinScanner(void) {
 target_siz_t arg_size,size;
 rc_t result_register;
 int clz_mode = 0;
 tok_t mode = TOK; YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
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
 vwunused();
}

// TODO: int __builtin_ctz(unsigned int x);
// TODO: int __builtin_popcount(unsigned int x);
// TODO: int __builtin_parity(unsigned int x);

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
 DCCParse_Expr1(),vused();
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
 vwunused();
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinExtractReturnAddr(void) {
 assert(TOK == KWD___builtin_extract_return_addr ||
        TOK == KWD___builtin_frob_return_address);
 YIELD();
 /* None of DCC's targets so weird stuff to the pointer! */
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
 vcast_pt(DCCTYPE_VOID,0);
 DCCParse_ParPairEnd();
 vwunused();
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinMinMax(void) {
 tok_t mode;
 int first = 1;
 assert(TOK == KWD___builtin_min ||
        TOK == KWD___builtin_max);
 mode = TOK == KWD___builtin_min ? '<' : '>';
 YIELD();
 DCCParse_ParPairBegin();
 while (DCCParse_IsExpr()) {
  DCCParse_Expr1();
  vrcopy();
  if (first) first = 0;
  else {
   vpromi2();
   vminmax(mode);
  }
  if (TOK != ',') break;
  YIELD();
 }
 DCCParse_ParPairEnd();
 if (first) vpushi(DCCTYPE_INT,0);
 vwunused();
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_BUILTINS_UTIL_C_INL */
