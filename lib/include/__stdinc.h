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

#if __has_include_next(<__stdinc.h>)
#include_next <__stdinc.h>
#else

#ifdef __PE__
#   define __IMP __attribute__((__dllimport__))
#else
#   define __IMP
#endif
#define __WUNUSED __attribute__((__warn_unused_result__))

#if defined(_WIN32) || defined(__CYGWIN32__)
#   assert __CRT(msvc)
#else
#   assert __CRT(glibc)
#endif

#define __STDLIB_VERSION__ 201112L /* C11 */

#define __STDLIB_UNSUPPORTED(x)
#endif

