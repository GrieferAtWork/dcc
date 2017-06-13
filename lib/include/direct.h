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

#if __has_include_next(<direct.h>)
#include_next <direct.h>
#else
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#ifdef __CRT_MSVC
#define __DIRECT_FUN(x) __asm__("_" x)
#else
#define __DIRECT_FUN(x)
#endif

#ifndef getcwd
#if !defined(__CRT_MSVC) || (__SIZEOF_SIZE_T__ == 4)
__IMP __WUNUSED char *(getcwd)(char *__buf, __SIZE_TYPE__ __bufsize) __DIRECT_FUN("getcwd");
#else
__IMP __WUNUSED char *(__msvc_getcwd)(char *__buf, __UINT32_TYPE__ __bufsize) __DIRECT_FUN("getcwd");
#define getcwd(buf,bufsize)  __msvc_getcwd(buf,(__UINT32_TYPE__)(bufsize))
#endif
#endif

__IMP int (chdir)(char const *__path) __DIRECT_FUN("chdir");
__IMP int (mkdir)(char const *__path) __DIRECT_FUN("mkdir");
__IMP int (rmdir)(char const *__path) __DIRECT_FUN("rmdir");

#undef __DIRECT_FUN
#endif
