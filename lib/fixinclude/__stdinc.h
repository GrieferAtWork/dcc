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
#	include_next <__stdinc.h>
#endif

#ifndef __DCC_VERSION__
#ifdef __INTELLISENSE__
#	include "__stdinc-syntax.h"
#else
#	warning "These headers are only meant for DCC"
#endif
#endif /* !__DCC_VERSION__ */

#ifdef __PE__
#   define __IMP __attribute__((__dllimport__))
#else
#   define __IMP
#endif
#define __WUNUSED __attribute__((__warn_unused_result__))

#if 0
#	define __CRT_DCC 1
#elif defined(__KOS__)
#	define __CRT_KOS 1
#elif defined(_WIN32) || defined(__CYGWIN32__)
#	define __CRT_MSVC 1
#	define __MS_LONG  __int32
#else
#	define __CRT_GLIBC 1
#endif
#ifdef __CRT_DCC
#   undef __CRT_GLIBC
#   undef __CRT_KOS
#   undef __CRT_MSVC
#endif
#ifdef __CRT_MSVC
#   undef __CRT_DCC
#   undef __CRT_GLIBC
#   undef __CRT_KOS
#endif
#ifdef __CRT_GLIBC
#   undef __CRT_DCC
#   undef __CRT_KOS
#   undef __CRT_MSVC
#endif
#ifdef __CRT_KOS
#   undef __CRT_DCC
#   undef __CRT_GLIBC
#   undef __CRT_KOS
#   undef __CRT_MSVC
#endif

#define __CRT_UNSUPPORTED(crt) [[__error__("Function is unsupported by <" crt ">")]]
#define __CRT_WORKAROUND(crt)  [[__warning__("Function only works in <" crt "> thanks to a workaround")]]

#ifdef __CRT_MSVC
#	define __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED("MSVC")
#	define __CRT_WORKAROUND_MSVC  __CRT_WORKAROUND("MSVC")
#else
#	define __CRT_UNSUPPORTED_MSVC
#	define __CRT_WORKAROUND_MSVC
#endif

#ifdef __CRT_GLIBC
#	define __CRT_UNSUPPORTED_GLIBC __CRT_UNSUPPORTED("GLIBC")
#	define __CRT_WORKAROUND_GLIBC  __CRT_WORKAROUND("GLIBC")
#else
#	define __CRT_UNSUPPORTED_GLIBC
#	define __CRT_WORKAROUND_GLIBC
#endif

#ifdef __CRT_KOS
#	define __CRT_UNSUPPORTED_KOS __CRT_UNSUPPORTED("KOS")
#	define __CRT_WORKAROUND_KOS  __CRT_WORKAROUND("KOS")
#else
#	define __CRT_UNSUPPORTED_KOS
#	define __CRT_WORKAROUND_KOS
#endif

#ifdef __STRICT_ANSI__
#	define __STRICT_ANSI_HEADER \
	  __pragma(tpp_exec("#warning \"<" __FILE__ "> should not be included when -ansi is passed\"\n"))
#else
#	define __STRICT_ANSI_HEADER /* nothing */
#endif
