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
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused(),vrcopy();
 switch (mode) {
 case KWD___builtin_bswap16: vcast_t(DCCTYPE_INT16|DCCTYPE_UNSIGNED,0); break;
 case KWD___builtin_bswap32: vcast_t(DCCTYPE_INT32|DCCTYPE_UNSIGNED,0); break;
 case KWD___builtin_bswap64: vcast_t(DCCTYPE_INT64|DCCTYPE_UNSIGNED,0); break;
 default:
  if (TOK == ',') {
   target_siz_t size;
   YIELD();
   size = (target_siz_t)DCCParse_CExpr(1);
   if (size != DCCType_Sizeof(&vbottom->sv_ctype,NULL,0))
       WARN(W_BUILTIN_BSWAP_INCORRECT_SIZE,&vbottom->sv_ctype,size);
  }
  break;
 }
 DCCVStack_BSwap();
 vrval();
 vwunused();
 DCCParse_ParPairEnd();
}


LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinScanner(void) {
 tok_t mode = TOK; YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused(),vrcopy();
 switch (mode) {
 case KWD___builtin_ffs:
 case KWD___builtin_clz:
  vcast_t(DCCTYPE_INT,0);
  break;
 {
  if (DCC_MACRO_FALSE) { case KWD___builtin_ffsl: mode = KWD___builtin_ffs; }
  if (DCC_MACRO_FALSE) { case KWD___builtin_clzl: mode = KWD___builtin_clz; }
  vcast_t(DCCTYPE_LONG|DCCTYPE_ALTLONG,0);
 } break;
 {
  if (DCC_MACRO_FALSE) { case KWD___builtin_ffsll: mode = KWD___builtin_ffs; }
  if (DCC_MACRO_FALSE) { case KWD___builtin_clzll: mode = KWD___builtin_clz; }
  vcast_t(DCCTYPE_LLONG,0);
 } break;
 {
  if (DCC_MACRO_FALSE) { case KWD___builtin_ffscc: mode = KWD___builtin_ffs; }
  if (DCC_MACRO_FALSE) { case KWD___builtin_clzcc: mode = KWD___builtin_clz; }
 default:
  if (TOK == ',') {
   target_siz_t size; YIELD();
   size = (target_siz_t)DCCParse_CExpr(1);
   if (size != DCCType_Sizeof(&vbottom->sv_ctype,NULL,0))
       WARN(W_BUILTIN_FFS_INCORRECT_SIZE,&vbottom->sv_ctype,size);
  }
 } break;
 }
 DCCVStack_Scanner(mode);
 vrval();
 vwunused();
 DCCParse_ParPairEnd();
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
   vxminmax(mode);
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
