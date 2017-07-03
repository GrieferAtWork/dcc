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

__IMP void *(memcpy)(void *__restrict __dst, void const *__restrict __src, size_t __n_bytes);
__IMP void *(memmove)(void *__dst, void const *__src, size_t __n_bytes);

__IMP char *(strcpy)(char *__dst, char const *__src);
__IMP char *(strncpy)(char *__dst, char const *__src, size_t __max_chars);
#ifndef __INTELLISENSE__
#   define strcpy(d,s)    __builtin_strcpy((d),(s))
#   define strncpy(d,s,n) __builtin_strncpy((d),(s),(n))
#endif

__IMP char *(strcat)(char *__dst, char const *__src);
__IMP char *(strncat)(char *__dst, char const *__src, size_t __max_chars);
#ifndef __INTELLISENSE__
#   define strcat(d,s)    __builtin_strcat((d),(s))
#   define strncat(d,s,n) __builtin_strncat((d),(s),(n))
#endif

__IMP __WUNUSED int (memcmp)(void const *__ptra, void const *__ptrb, size_t __max_bytes);
__IMP __WUNUSED int (strcmp)(char const *__stra, char const *__strb);
__IMP __WUNUSED int (strcoll)(char const *__stra, char const *__strb);
__IMP __WUNUSED int (strncmp)(char const *__stra, char const *__strb, size_t __max_chars);
__IMP __WUNUSED size_t (strxfrm)(char const *__dst, char const *__src, size_t __max_chars);
#ifndef __INTELLISENSE__
#   define memcmp(a,b,n) __builtin_memcmp((a),(b),(n))
#endif

__IMP __WUNUSED void *(memchr)(void const *__ptr, int __byte, size_t __max_bytes);
__IMP __WUNUSED char *(strchr)(char const *__str, int __char);
__IMP __WUNUSED size_t (strcspn)(char const *__str, char const *__reject);
__IMP __WUNUSED char *(strpbrk)(char const *__str, char const *__accept);
__IMP __WUNUSED char *(strrchr)(char const *__str, int __char);
__IMP __WUNUSED size_t (strspn)(char const *__str, char const *__accept);
__IMP __WUNUSED char *(strstr)(char const *__haystack, char const *__needle);
__IMP __WUNUSED char *(strtok)(char *__str, char const *__sep);

__IMP char *(__strtok_r)(char *__restrict __s, const char *__restrict __delim,
                         char **__restrict __save_ptr)
#ifdef __CRT_MSVC
    __asm__("strtok_s")
#elif defined(__CRT_DCC) || defined(__CRT_KOS)
    __asm__("strtok_r")
#endif
;
#ifdef __USE_POSIX
__IMP char *(strtok_r)(char *__restrict __s, const char *__restrict __delim,
                       char **__restrict __save_ptr)
#ifdef __CRT_MSVC
    __asm__("strtok_s")
#endif
;
#endif /* __USE_POSIX */

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC char *(strcasestr)(const char *__haystack, const char *__needle);
#endif

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC void *(memmem)(const void *__haystack, size_t __haystacklen, const void *__needle, size_t __needlelen);
__IMP __CRT_UNSUPPORTED_MSVC void *(__mempcpy)(void *__restrict __dest, const void *__restrict __src, size_t __n);
__IMP __CRT_UNSUPPORTED_MSVC void *(mempcpy)(void *__restrict __dest, const void *__restrict __src, size_t __n);
#endif

__IMP void *(memset)(void *__ptr, int __byte, size_t __max_bytes);
__IMP __WUNUSED char *(strerror)(int __errno);
__IMP __WUNUSED size_t (strlen)(char const *__str);
#ifndef __INTELLISENSE__
#   define strnlen(s) __builtin_strlen((s))
#endif

#ifdef __USE_XOPEN2K8
__IMP __WUNUSED size_t (strnlen)(char const *__str, size_t __max_chars);
#ifndef __INTELLISENSE__
#   define strnlen(s,n) __builtin_strnlen((s),(n))
#endif
#endif

#if defined(__USE_GNU) || defined(__USE_DCC)
#if defined(__CRT_GLIBC) || defined(__CRT_DCC) || \
    defined(__CRT_KOS) || defined(__INTELLISENSE__)
__IMP __WUNUSED void *(memrchr)(void const *__ptr, int __byte, size_t __max_bytes);
__IMP __WUNUSED void *(rawmemchr)(const void *__ptr, int __byte)
#if defined(__CRT_KOS)
	__asm__("_umemend");
#endif
;
#endif
#ifndef __INTELLISENSE__
#	define memrchr(p,c,n)  __builtin_memrchr((p),(c),(n))
#	define rawmemchr(p,c)  __builtin_rawmemchr((p),(c))
#endif
#endif

#if defined(__USE_MISC) || defined (__USE_XOPEN)
__IMP void *(memccpy)(void *__restrict __dst, void const *__restrict __src,
                      int __end_byte, size_t __max_bytes)
#ifdef __CRT_MSVC
    __asm__("_memccpy")
#endif
;
#endif


#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __WUNUSED char *(strdup)(char const *__str)
#if defined(__CRT_MSVC)
	__asm__("_strdup")
#endif
;
#endif

#ifdef __USE_XOPEN2K8
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED
char *(strndup)(char const *__str, size_t __max_chars);
#endif

#ifdef __USE_GNU
#if defined(__CRT_GLIBC) || defined(__INTELLISENSE__)
__IMP char *(strchrnul)(const char *__s, int __c);
#else
#   define strchrnul(s,c) __builtin_strchrnul((s),(c))
#endif
#endif

#ifdef __USE_DCC /* DCC Extensions. */
#if defined(__CRT_DCC) || defined(__INTELLISENSE__)
__IMP __WUNUSED void *(memrend)(void const *__ptr, int __char, size_t __max_bytes);
__IMP __WUNUSED size_t (memrlen)(void const *__ptr, int __char, size_t __max_bytes);
__IMP __WUNUSED size_t (rawmemrlen)(void const *__ptr, int __char);
__IMP __WUNUSED size_t (rawmemrchr)(void const *__ptr, int __char);
#endif
#if defined(__CRT_DCC) || defined(__CRT_KOS) || defined(__INTELLISENSE__)
__IMP __WUNUSED char *(strend)(char const *__str);
__IMP __WUNUSED char *(strnend)(char const *__str, size_t __max_chars);
__IMP __WUNUSED void *(memend)(void const *__ptr, int __byte, size_t __max_bytes);
__IMP __WUNUSED size_t (memlen)(void const *__ptr, int __byte, size_t __max_bytes);
__IMP __WUNUSED size_t (rawmemlen)(void const *__ptr, int __byte)
#if defined(__CRT_KOS)
	__asm__("umemlen");
#endif
;
#endif
#ifndef __INTELLISENSE__
#	define memrend(p,c,n)  __builtin_memrend((p),(c),(n))
#	define memrlen(p,c,n)  __builtin_memrlen((p),(c),(n))
#	define rawmemrlen(p,c) __builtin_rawmemrlen((p),(c))
#	define rawmemrchr(p,c) __builtin_rawmemrchr((p),(c))
#	define strend(s)      (char *)__builtin_rawmemchr((s),'\0')
#	define strnend(s,n)   (char *)__builtin_memend((s),'\0',(n))
#	define memend(p,c,n)   __builtin_memend((p),(c),(n))
#	define memlen(p,c,n)   __builtin_memlen((p),(c),(n))
#	define rawmemlen(p,c)  __builtin_rawmemlen((p),(c))
#endif
#endif


#ifdef __KOS__ /* KOS-specific string function aliases. */
__IMP __WUNUSED void *(umemend)(const void *__ptr, int __byte)
#if !defined(__CRT_KOS)
	__asm__("rawmemchr");
#endif
;
#if defined(__CRT_DCC) || defined(__CRT_KOS)
__IMP __WUNUSED void *(umemlen)(const void *__ptr, int __byte)
#if !defined(__CRT_KOS)
	__asm__("rawmemlen");
#endif
;
#else
#   define umemlen __builtin_rawmemlen
#endif
#endif

#ifdef __USE_GNU
#ifdef __INTELLISENSE__
extern char *strdupa(char const *__str);
extern char *strndupa(char const *__str, size_t __max_chars);
#else
#define strdupa(s) \
(__extension__({\
	char const *const __old = (s);\
	size_t const __len = __builtin_strlen(__old)+1;\
	char *const __new = (char *)__builtin_alloca(__len);\
	(char *)memcpy(__new,__old,__len);\
}))
#define strndupa(s,n) \
(__extension__({\
	char const *const __old = (s);\
	size_t const __len = __builtin_strnlen(__old,(n));\
	char *const __new = (char *)__builtin_alloca(__len+1);\
	__new[__len] = '\0';\
	(char *)memcpy(__new,__old,__len);\
}))
#endif
#endif


#ifdef __USE_XOPEN2K8
#include <xlocale.h>
__IMP __CRT_UNSUPPORTED_KOS
int (strcoll_l)(const char *__s1, const char *__s2, __locale_t __l)
#ifdef __CRT_MSVC
    __asm__("_strcoll_l")
#endif
;
__IMP __CRT_UNSUPPORTED_KOS
size_t (strxfrm_l)(char *__dest, const char *__src, size_t __n, __locale_t __l)
#ifdef __CRT_MSVC
    __asm__("_strxfrm_l")
#endif
;
#endif



#endif
