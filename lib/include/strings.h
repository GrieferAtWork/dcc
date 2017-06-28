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

#if __has_include_next(<strings.h>)
#include_next <strings.h>
#else /* include_next... */
#include <__stdinc.h>
#include <features.h>

#undef size_t
typedef __SIZE_TYPE__ size_t;


#if defined(__USE_MISC) || !defined(__USE_XOPEN2K8)
#if defined(__CRT_GLIBC) || defined(__INTELLISENSE__)
__IMP int (bcmp)(void const *__s1, void const *__s2, size_t __n);
__IMP void (bcopy)(void const *__src, void *__dest, size_t __n);
__IMP void (bzero)(void *__s, size_t __n);
#endif
#ifndef __INTELLISENSE__
#define bcmp(s1,s2,n)           __builtin_memcmp((s1),(s2),(n))
#define bcopy(src,dest,n) (void)__builtin_memcpy((dest),(src),(n))
#define bzero(s,n)        (void)__builtin_memset((s),0,(n))
#endif

#if defined(__CRT_GLIBC) || defined(__INTELLISENSE__)
__IMP char *(index)(char const *__s, int __c);
__IMP char *(rindex)(char const *__s, int __c);
#else
#define index(s,c)  ((c) ? __builtin_strchr(s,c) : __builtin_strend(s))
#define rindex(s,c) ((c) ? __builtin_strrchr(s,c) : __builtin_strend(s))
#endif
#endif

#if defined(__USE_MISC) || !defined(__USE_XOPEN2K8) || defined(__USE_XOPEN2K8XSI)
#if defined(__CRT_GLIBC) || defined(__CRT_KOS) || defined(__INTELLISENSE__)
__IMP int (ffs)(int __i);
#endif
#ifndef __INTELLISENSE__
#define ffs(i) __builtin_ffs((i))
#endif
#endif

__IMP int (strcasecmp)(char const *__s1, char const *__s2)
#if defined(__CRT_MSVC)
    __asm__("_stricmp")
#elif defined(__CRT_KOS)
    __asm__("stricmp")
#endif
;
__IMP int (strncasecmp)(char const *__s1, char const *__s2, size_t __n)
#if defined(__CRT_MSVC)
    __asm__("_strnicmp")
#elif defined(__CRT_KOS)
    __asm__("strnicmp")
#endif
;

#ifdef	__USE_XOPEN2K8
#include <xlocale.h>
__IMP __CRT_UNSUPPORTED_KOS int (strcasecmp_l)(char const *__s1, char const *__s2, __locale_t __loc)
#ifdef __CRT_MSVC
    __asm__("_stricmp_l")
#endif
;
__IMP __CRT_UNSUPPORTED_KOS int (strncasecmp_l)(char const *__s1, char const *__s2, size_t __n, __locale_t __loc)
#ifdef __CRT_MSVC
    __asm__("_strnicmp_l")
#endif
;
#endif /* __USE_XOPEN2K8 */

#endif /* !include_next... */
