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

#if __has_include_next(<string.h>)
#include_next <string.h>
#else
#include <__stdinc.h>

#include "features.h"

#undef size_t
typedef __SIZE_TYPE__ size_t;

#define NULL  __NULL__

__IMP void *(memcpy)(void *,void const *,size_t);
__IMP void *(memmove)(void *,void const *,size_t);
__IMP char *(strcpy)(char *,char const *);
__IMP char *(strncpy)(char *,char const *,size_t);

__IMP char *(strcat)(char *,char const *);
__IMP char *(strncat)(char *,char const *,size_t);

__IMP __WUNUSED int (memcmp)(void const *,void const *,size_t);
__IMP __WUNUSED int (strcmp)(char const *,char const *);
__IMP __WUNUSED int (strcoll)(char const *,char const *);
__IMP __WUNUSED int (strncmp)(char const *,char const *,size_t);
__IMP __WUNUSED size_t (strxfrm)(char const *,char const *,size_t);

__IMP __WUNUSED void *(memchr)(void const *,int,size_t);
__IMP __WUNUSED char *(strchr)(char const *,int);
__IMP __WUNUSED size_t (strcspn)(char const *,char const *);
__IMP __WUNUSED char *(strpbrk)(char const *,char const *);
__IMP __WUNUSED char *(strrchr)(char const *,int);
__IMP __WUNUSED size_t (strspn)(char const *,char const *);
__IMP __WUNUSED char *(strstr)(char const *,char const *);
__IMP __WUNUSED char *(strtok)(char *,char const *);

__IMP void *(memset)(void *,int,size_t);
__IMP __WUNUSED char *(strerror)(int);
__IMP __WUNUSED size_t (strlen)(char const *);

#ifdef __USE_XOPEN2K8
__IMP __WUNUSED size_t (strnlen)(const char *,size_t);
#endif

#if defined(__USE_GNU) || defined(__USE_DCC)
#if defined(__CRT_GLIBC) || defined(__CRT_DCC) || defined(__CRT_KOS)
__IMP __WUNUSED void *(memrchr)(void const *,int,size_t);
__IMP __WUNUSED void *(rawmemchr)(const void *,int)
#if defined(__CRT_KOS)
	__asm__("umemend");
#endif
;
#else
#   define memrchr    __builtin_memrchr
#   define rawmemchr  __builtin_rawmemchr
#endif
#endif

#if defined(__USE_MISC) || defined (__USE_XOPEN)
#if defined(__CRT_GLIBC) || defined(__CRT_MSVC)
__IMP void *(memccpy)(void *__restrict,void const *__restrict,int,size_t);
#else
__STDLIB_UNSUPPORTED("memccpy")
#endif
#endif


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
__IMP __WUNUSED char *(strdup)(char const *)
#if defined(__CRT_MSVC)
	__asm__("_strdup")
#endif
;
#endif

#ifdef __USE_XOPEN2K8
#if defined(__CRT_GLIBC)
__IMP __WUNUSED char *(strndup)(const char *,size_t);
#else
__STDLIB_UNSUPPORTED("strndup")
#endif
#endif


#ifdef __USE_DCC /* DCC Extensions. */
#if defined(__CRT_DCC)
__IMP __WUNUSED void *(memrend)(void const *,int,size_t);
__IMP __WUNUSED size_t (memrlen)(void const *,int,size_t);
__IMP __WUNUSED size_t (rawmemrlen)(void const *,int,size_t)
__IMP __WUNUSED size_t (rawmemrchr)(void const *,int,size_t)
#else
#define memrend    __builtin_memrend
#define memrlen    __builtin_memrlen
#define rawmemrlen __builtin_rawmemrlen
#define rawmemrchr __builtin_rawmemrchr
#endif
#if defined(__CRT_DCC) || defined(__CRT_KOS)
__IMP __WUNUSED char *(strend)(char const *);
__IMP __WUNUSED char *(strnend)(char const *,size_t);
__IMP __WUNUSED void *(memend)(void const *,int,size_t);
__IMP __WUNUSED size_t (memlen)(void const *,int,size_t);
__IMP __WUNUSED size_t (rawmemlen)(void const *,int,size_t)
#if defined(__CRT_KOS)
	__asm__("umemlen");
#endif
;
#else
#   define strend(s)     (char *)__builtin_rawmemchr(s,'\0')
#   define strnend(s,n)  (char *)__builtin_memend(s,'\0',n)
#   define memend         __builtin_memend
#   define memlen         __builtin_memlen
#   define rawmemlen      __builtin_rawmemlen
#endif
#endif




#ifdef __KOS__ /* KOS-specific string function aliases. */
__IMP __WUNUSED void *(umemend)(const void *,int)
#if !defined(__CRT_KOS)
	__asm__("rawmemchr");
#endif
;
#if defined(__CRT_DCC) || defined(__CRT_KOS)
__IMP __WUNUSED void *(umemlen)(const void *,int)
#if !defined(__CRT_KOS)
	__asm__("rawmemlen");
#endif
;
#else
#   define umemlen __builtin_rawmemlen
#endif
#endif

#ifdef __USE_GNU
#define strdupa(s) \
(__extension__({\
	const char *const __old = (s);\
	size_t const __len = __builtin_strlen(__old)+1;\
	char *const __new = (char *)__builtin_alloca(__len);\
	(char *)memcpy(__new,__old,__len);\
}))
#define strndupa(s,n) \
(__extension__({\
	const char *const __old = (s);\
	size_t const __len = __builtin_strnlen(__old,(n));\
	char *const __new = (char *)__builtin_alloca(__len+1);\
	__new[__len] = '\0';\
	(char *)memcpy(__new,__old,__len);\
}))
#endif


#endif
