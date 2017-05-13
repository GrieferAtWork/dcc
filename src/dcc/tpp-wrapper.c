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

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

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
#define PRIVATE /* nothing */

DCC_DECL_BEGIN
extern uint8_t const chrattr[256];
extern char *skip_whitespace_and_comments(char *iter, char *end);
DCC_DECL_END

#undef tpp
#undef c

#include DCC_TPP_FILE(tpp.c)

#endif /* !GUARD_DCC_TPP_WRAPPER_C */
