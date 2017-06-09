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

#include <__stdinc.h>

#if __has_include_next(<ctype.h>)
#include_next <ctype.h>
#endif

#define isalpha(ch)  __builtin_isalpha((ch))
#define isupper(ch)  __builtin_isupper((ch))
#define islower(ch)  __builtin_islower((ch))
#define isdigit(ch)  __builtin_isdigit((ch))
#define isxdigit(ch) __builtin_isxdigit((ch))
#define isspace(ch)  __builtin_isspace((ch))
#define ispunct(ch)  __builtin_ispunct((ch))
#define isalnum(ch)  __builtin_isalnum((ch))
#define isprint(ch)  __builtin_isprint((ch))
#define isgraph(ch)  __builtin_isgraph((ch))
#define iscntrl(ch)  __builtin_iscntrl((ch))
#define toupper(ch)  __builtin_toupper((ch))
#define tolower(ch)  __builtin_tolower((ch))
#if __STDLIB_VERSION__ >= 201112L
#define isblank(ch)  __builtin_isblank((ch))
#endif

