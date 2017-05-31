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

#if DCC_DEBUG && !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#include <Windows.h>
#endif

DCC_DECL_BEGIN

PRIVATE void dcc_warnf(char const *fmt, ...) {
 char buffer[1024];
 va_list args;
 size_t bufsiz;
 va_start(args,fmt);
#ifdef _MSC_VER
 _vsnprintf(buffer,sizeof(buffer),fmt,args);
#else
  vsnprintf(buffer,sizeof(buffer),fmt,args);
#endif
 va_end(args);
 bufsiz = strlen(buffer);
 fwrite(buffer,sizeof(char),bufsiz,stderr);
#if DCC_DEBUG && !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
 OutputDebugStringA(buffer);
#endif
}

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

/*
#define TPP_USERLINES \
{\
 struct TPPFile *fp;\
case W_UNRESOLVED_REFERENCE: \
 fp = ARG(struct TPPFile *);\
 WARNF(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT\
       ? "%s(%d) : " : "%s:%d: ",\
       TPPFile_Filename(fp,NULL),(int)ARG(line_t)+1);\
 macro_name = NULL;\
 break;\
}
*/
  

#undef PRIVATE
#undef PUBLIC
#undef LOCAL
#undef likely
#undef unlikely
#undef tpp
#undef c

/* Re-define all private declarations as internal,
 * so that other parts of the compiler can re-use
 * some of the more useful helper functions. */
#define PRIVDEF    INTDEF
#define PRIVATE    INTERN

/* Custom warning print callback (used for adding colors). */
#define TPP_WARNF  dcc_warnf

#include DCC_TPP_FILE(tpp.c)

#endif /* !GUARD_DCC_TPP_WRAPPER_C */
