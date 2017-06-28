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

#if __has_include_next(<xlocale.h>)
#include_next <xlocale.h>
#else
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#include <features.h>

#ifdef __CRT_MSVC
#pragma push_macro(undef,"threadlocaleinfostruct","threadmbcinfostruct")
typedef struct __locale_struct {
 struct threadlocaleinfostruct *__msvc_locinfo;
 struct threadmbcinfostruct    *__msvc_mbcinfo;
} *__locale_t;
#pragma pop_macro("threadlocaleinfostruct","threadmbcinfostruct")
#elif defined(__CRT_GLIBC)
typedef struct __locale_struct {
 struct __locale_data  *__locales[13];
 __UINT16_TYPE__ const *__ctype_b;
 __INT32_TYPE__ const  *__ctype_tolower;
 __INT32_TYPE__ const  *__ctype_toupper;
 char const            *__names[13];
} *__locale_t;
#else
typedef int *__locale_t; /* ??? */
#endif

typedef __locale_t locale_t;

#endif
