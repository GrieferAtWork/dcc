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

#if __has_include_next(<string.h>)
#include_next <string.h>
#else
#include "__stdinc.h"
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

#ifdef __USE_GNU
#if #__CRT(glibc)
__IMP __WUNUSED void *(memrchr)(void const *,int,size_t);
__IMP __WUNUSED void *(rawmemchr)(const void *,int);
#else
__STDLIB_UNSUPPORTED("memrchr")
__STDLIB_UNSUPPORTED("rawmemchr")
#endif
#endif

#if defined(__USE_MISC) || defined (__USE_XOPEN)
#if #__CRT(glibc) || #__CRT(msvc)
__IMP void *(memccpy)(void *__restrict,void const *__restrict,int,size_t);
#else
__STDLIB_UNSUPPORTED("memrchr")
#endif
#endif


#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K8
__IMP __WUNUSED char *(strdup)(char const *)
#if #__CRT(msvc)
	__asm__("_strdup")
#endif
;
#endif

#ifdef __USE_XOPEN2K8
#if #__CRT(glibc)
__IMP __WUNUSED char *(strndup)(const char *,size_t);
#else
__STDLIB_UNSUPPORTED("strndup")
#endif
#endif


#ifdef __USE_GNU
#define strdupa(s) (__extension__({\
	const char *const __old = (s);\
	size_t const __len = strlen(__old)+1;\
	char *const __new = (char *)__builtin_alloca(__len);\
	(char *)memcpy(__new,__old,__len);\
}))
#define strndupa(s,n) (__extension__({\
	const char *const __old = (s);\
	size_t const __len = strnlen(__old,(n));\
	char *const __new = (char *)__builtin_alloca(__len+1);\
	__new[__len] = '\0';\
	(char *)memcpy(__new,__old,__len);\
}))
#endif


#endif
