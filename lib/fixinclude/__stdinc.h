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
#pragma GCC system_header

#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#ifndef __has_attribute
#define __has_attribute(x) 0
#endif
#ifndef __has_include
#define __has_include(x) 0
#endif
#ifndef __has_include_next
#define __has_include_next(x) 0
#endif
#ifndef __has_warning
#define __has_warning(x) 0
#endif

#if __has_include_next(<__stdinc.h>)
#include_next <__stdinc.h>
#else

#ifndef __DCC_VERSION__
#ifdef __INTELLISENSE__
#include "__stdinc-syntax.h"
#else
#warning "These headers are only meant for DCC"
#endif
#endif /* !__DCC_VERSION__ */

#ifdef __PE__
#   define __IMP __attribute__((__dllimport__))
#else
#   define __IMP
#endif
#define __WUNUSED __attribute__((__warn_unused_result__))

#if 0
#   define __CRT_DCC 1
#elif defined(_WIN32) || defined(__CYGWIN32__)
#   define __CRT_MSVC 1
#else
#   define __CRT_GLIBC 1
#endif

#define __STDLIB_VERSION__      201112L /* C11 */
#define __STDLIB_UNSUPPORTED(x)

#ifdef __STRICT_ANSI__
#define __STRICT_ANSI_HEADER \
  __pragma(tpp_exec("#warning \"<malloc.h> should not be included when -ansi is passed\"\n"))
#else
#define __STRICT_ANSI_HEADER /* nothing */
#endif

#endif