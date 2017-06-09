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

#ifndef __has_include_next
#define __has_include_next(x) 0
#endif

#if __has_include_next(<assert.h>)
#include_next <assert.h>
#else
#include <__stdinc.h>

#ifndef __DCC_ASSERT_LIB_DEFINED
#define __DCC_ASSERT_LIB_DEFINED 1
#ifdef __CRT_MSVC
__IMP _Noreturn void (__assertion_failed)(char const *,char const *,unsigned int) __asm__("_assert");
#else
#error FIXME
#endif
#endif /* !__DCC_ASSERT_LIB_DEFINED */

/* NOTE: '__builtin_assume' will mark code flow as
 *       unreachable for compile-time false expressions:
 *       >> assert(0); // Mark control flow as unreachable, but check the fact at runtime!
 */
#ifdef NDEBUG
#define assert         __builtin_assume
#else
#define assert(expr)  (void)((expr) || (__assertion_failed(#expr,__FILE__,__LINE__),__builtin_breakpoint(),0))
#endif
#endif
