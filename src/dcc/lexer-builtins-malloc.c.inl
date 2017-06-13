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
#ifndef GUARD_DCC_LEXER_BUILTINS_MALLOC_C_INL
#define GUARD_DCC_LEXER_BUILTINS_MALLOC_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/vstack.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

#ifdef DCC_SYMFLAG_DLLIMPORT
#define MALL_SYM(name) DCCUnit_NewSyms(name,DCC_SYMFLAG_DLLIMPORT)
#else
#define MALL_SYM(name) DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE)
#endif

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinMalloc(void) {
 int cmode; struct DCCSym *sym;
 /* void *__builtin_malloc(size_t n_bytes),__builtin_calloc(size_t count, size_t n_bytes); */
 assert(TOK == KWD___builtin_malloc ||
        TOK == KWD___builtin_calloc);
 cmode = TOK == KWD___builtin_calloc;
 sym = MALL_SYM(cmode ? "calloc" : "malloc");
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
 vused();
 if (cmode) {
  if (TOK != ',') WARN(W_EXPECTED_COMMA);
  else YIELD();
  DCCParse_Expr1();
  vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
  vused();
 }
 /* xxx: Compile-time optimizations for small malloc() sizes? */
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 sym ? DCCVStack_PushSym_vpfun(sym) : vpushv();
                 /* [count], n_bytes, malloc */
 vlrot(2+cmode); /* malloc, [count], n_bytes */
 vcall(1+cmode); /* ret */
 DCCParse_ParPairEnd();
 vwunused();
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinRealloc(void) {
 struct DCCSym *sym;
 /* void *__builtin_realloc(void *ptr, size_t n_bytes); */
 sym = MALL_SYM("realloc");
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vcast_pt(DCCTYPE_VOID,0);
 vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1();
 vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
 vused();
 /* TODO: Compile-time optimization for 'realloc(NULL,42)' --> 'malloc(42)' */
 sym ? DCCVStack_PushSym_vpfun(sym) : vpushv();
           /* ptr, size, realloc */
 vlrot(3); /* realloc, ptr, size */
 vcall(2); /* ret */
 DCCParse_ParPairEnd();
 vwunused();
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFree(void) {
 struct DCCSym *sym;
 /* void  __builtin_free(void *ptr); */
 assert(TOK == KWD___builtin_free);
 sym = MALL_SYM("free");
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vcast_pt(DCCTYPE_VOID,0);
 vused();
 /* TODO: Compile-time optimization for 'free(NULL)' --> '(void)0' */
 sym ? DCCVStack_PushSym_vfun(sym) : vpushv();
           /* ptr, free */
 vswap();  /* free, ptr */
 vcall(1); /* ret */
 DCCParse_ParPairEnd();
}


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_BUILTINS_MALLOC_C_INL */
