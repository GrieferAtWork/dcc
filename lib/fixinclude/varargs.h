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
#pragma once
#if __has_warning("-Wold-function-decl")
#warning "<varargs.h> is obsolete; new code should use <stdarg.h> instead"
#endif

#pragma GCC system_header
#if __has_include_next(<varargs.h>)
/* Many standard libraries still implement <varargs.h>, only to
 * emit a #error directive telling you that they actually don't.
 * >> But DCC _does_ implement it! - So in order to shut up
 *    #error, or #warning directives, we simply disable '-Wuser'
 *    warning (aka. warnings explicitly emit by user-code). */
#pragma warning(push)
#pragma warning("-Wno-user")
#include_next <varargs.h>
#pragma warning(pop)
#endif

/* Fixed/optimized system header <varargs.h> for DCC */

#undef va_list
typedef __builtin_va_list va_list;

/* NOTE: '__builtin_va_alist' could be refactored to anything... */
#define va_alist     __builtin_va_alist
#define va_dcl   int __builtin_va_alist; ...

/* void va_start(va_list &ap); */
/* void va_start(va_list &ap, type &last_argument); */
#define va_start   __builtin_va_start

/* void va_end(va_list &ap); */
#define va_end     __builtin_va_end

/* t va_arg(va_list &ap, type t); */
#define va_arg     __builtin_va_arg








