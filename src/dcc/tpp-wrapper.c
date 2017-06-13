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
#ifndef GUARD_DCC_TPP_WRAPPER_C
#define GUARD_DCC_TPP_WRAPPER_C 1
#define _GNU_SOURCE /* enable 'memrchr' */

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/linker.h>
#include <dcc/unit.h>
#include <dcc/type.h>
#include <dcc/compiler.h>

#include <stdio.h>
#include <stdarg.h>
#include <string.h>

#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW32__)
#include <dcc_winmin.h>
#endif

DCC_DECL_BEGIN

INTDEF void dcc_voutf(char const *fmt, va_list args);
INTDEF void dcc_outf(char const *fmt, ...);

PRIVATE void dcc_warnf(char const *fmt, ...) {
 va_list args;
 va_start(args,fmt);
 dcc_voutf(fmt,args);
 va_end(args);
}

PRIVATE int
print_symaddr(struct DCCSymAddr const *__restrict symaddr) {
 char const *path;
 struct A2lState info;
 if (!DCCA2l_LookupAdr(&info,symaddr)) return 0;
 path = DCCA2lState_GetPath(&info);
 dcc_warnf(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
           ? "%s%s%s(%d,%d) : %s : " : "%s%s%s:%d:%d: %s : ",
           path ? path : "",path ? "/" : "",
           DCCA2lState_GetFile(&info),
           DCCA2lState_GetLine(&info),
           DCCA2lState_GetCol(&info),
           DCCA2lState_GetName(&info));
 return 1;
}

PRIVATE int print_sym(struct DCCSym *sym, target_off_t offset) {
 struct DCCSymAddr symaddr;
 symaddr.sa_sym = sym;
 symaddr.sa_off = offset;
 return print_symaddr(&symaddr);
}


#define TPP_USERLINES \
{\
 struct DCCSection *sec; \
case W_UNRESOLVED_REFERENCE: \
 sec  = ARG(struct DCCSection *); \
 if (!print_sym(&sec->sc_start,ARG(target_off_t))) goto default_lines; \
 macro_name = NULL; \
} break; \
default_lines:; \


DCC_DECL_END

#undef CURRENT
#undef TOK
#undef TOKEN
#undef KWD
#undef YIELD
#undef HAS
#undef WARN
#undef OK
#undef TPP
#undef pushf
#undef popf
  

#undef PRIVATE
#undef PUBLIC
#undef LOCAL
#undef likely
#undef unlikely

/* Re-define all private declarations as internal,
 * so that other parts of the compiler can re-use
 * some of the more useful helper functions. */
#define PRIVDEF    INTDEF
#define PRIVATE    INTERN
#define NO_INCLUDE_WINDOWS_H

/* Custom warning print callback (used for adding colors). */
#define TPP_WARNF  dcc_warnf

#include "../tpp/tpp.c"

#endif /* !GUARD_DCC_TPP_WRAPPER_C */
