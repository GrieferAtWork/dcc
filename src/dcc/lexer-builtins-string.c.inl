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
#ifndef GUARD_DCC_LEXER_BUILTINS_STRING_C_INL
#define GUARD_DCC_LEXER_BUILTINS_STRING_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/vstack.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>

#include <string.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

/*  __builtin_memcpy, __builtin_memmove  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinMemcpy(void) {
 int may_overlap = TOK == KWD___builtin_memmove;
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCVStack_Memcpy(may_overlap);
}


/*  __builtin_memset  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinMemset(void) {
 assert(TOK == KWD___builtin_memset);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID,0),vused();
 DCCType_CheckWritablePtr(&vbottom->sv_ctype);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_INT,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCVStack_Memset();
}


/*  __builtin_memcmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinMemcmp(void) {
 assert(TOK == KWD___builtin_memcmp);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCVStack_Memcmp();
 vwunused();
}


/*  __builtin_strlen  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinStrlen(void) {
 int nlen_mode;
 assert(TOK == KWD___builtin_strlen ||
        TOK == KWD___builtin_strnlen);
 nlen_mode = TOK == KWD___builtin_strnlen;
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_CHAR|DCCTYPE_CONST,0),vused();
 if (nlen_mode) {
  if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
  DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 }
 DCCVStack_Strlen(nlen_mode);
 vwunused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_BUILTINS_STRING_C_INL */
