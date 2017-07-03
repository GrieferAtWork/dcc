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
#ifndef GUARD_DCC_TARGET_CRT_H
#define GUARD_DCC_TARGET_CRT_H 1

#include "common.h"
#include "target.h"

#include <string.h>

DCC_DECL_BEGIN


#define DCC_CRT_NONE    0 /* No CRT at all. */
#define DCC_CRT_UNKNOWN 0 /* An unknown CRT. */
#define DCC_CRT_GENERIC 1 /* Generic CRT supporting the C standard. */
#define DCC_CRT_MSVC    2 /* msvcrt[...].dll */
#define DCC_CRT_GLIBC   3 /* GLIBC & compatible, as seen on unix. */
#define DCC_CRT_KOS     4 /* The standard C library of my home-brew OS. */
#define DCC_CRT_DCC     5 /* Theoretical CRT supporting all of DCC's made up functions. */

#ifndef DCC_TARGET_CRT
/* Select the most appropriate CRT for the target OS. */
#if DCC_TARGET_OS == DCC_OS_KOS
#   define DCC_TARGET_CRT  DCC_CRT_KOS
#elif !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
#   define DCC_TARGET_CRT  DCC_CRT_MSVC
#elif !!(DCC_TARGET_OS&DCC_OS_F_UNIX)
#   define DCC_TARGET_CRT  DCC_CRT_GLIBC
#elif defined(DCC_CONFIG_NO_CRT)
#   define DCC_TARGET_CRT  DCC_CRT_NONE
#else
#   define DCC_TARGET_CRT  DCC_CRT_GENERIC
#endif
#endif /* !DCC_TARGET_CRT */


/* NOTE: For full functionality of all builtin functions,
/*       the runtime should implement at least:
 *    >> int   memcmp(void const *a, void const *b, size_t s);
 *    >> void *memset(void *dst, int byte, size_t s);
 *    >> void *memmove(void *dst, void const *src, size_t n_bytes); 
 *    >> void *memchr(void const *p, int c, size_t s);
 *    >> void *memrchr(void const *p, int c, size_t s);
 *    >> char *strncpy(char *dst, char const *src, size_t max);
 *    >> char *strncat(char *dst, char const *src, size_t max);
 */

#if DCC_TARGET_CRT == DCC_CRT_GLIBC
#   define DCC_TARGET_RT_STDC  1
#   define DCC_TARGET_RT_GLIBC 1
#   define DCC_TARGET_RT_DCC   0
#elif DCC_TARGET_CRT == DCC_CRT_DCC
#   define DCC_TARGET_RT_STDC  1
#   define DCC_TARGET_RT_GLIBC 1
#   define DCC_TARGET_RT_DCC   1
#elif DCC_TARGET_CRT == DCC_CRT_NONE
#   define DCC_TARGET_RT_STDC  0
#   define DCC_TARGET_RT_GLIBC 0
#   define DCC_TARGET_RT_DCC   0
#else
#   define DCC_TARGET_RT_STDC  1
#   define DCC_TARGET_RT_GLIBC 0
#   define DCC_TARGET_RT_DCC   0
#endif

/* Optional functions that DCC can assume to always be provided by the runtime.
 * NOTE: These are only used in fallback code of __builtin_-functions.
 * NOTE: Some of these functions are not standardized and may not be provided by _any_ runtime... */
#define DCC_TARGET_RT_HAVE_MEMCPY      DCC_TARGET_RT_STDC  /* void  *memcpy(void *dst, void const *src, size_t n_bytes); */
#define DCC_TARGET_RT_HAVE_STRLEN      DCC_TARGET_RT_STDC  /* size_t strlen(char const *s); */
#define DCC_TARGET_RT_HAVE_STRNLEN     DCC_TARGET_RT_STDC  /* size_t strnlen(char const *s, size_t max); */
#define DCC_TARGET_RT_HAVE_STREND      DCC_TARGET_RT_DCC   /* char  *strend(char const *s); */
#define DCC_TARGET_RT_HAVE_STRNEND     DCC_TARGET_RT_DCC   /* char  *strnend(char const *s, size_t max); */
#define DCC_TARGET_RT_HAVE_RAWMEMLEN   DCC_TARGET_RT_DCC   /* size_t rawmemlen(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_RAWMEMRLEN  DCC_TARGET_RT_DCC   /* size_t rawmemrlen(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_RAWMEMCHR   DCC_TARGET_RT_GLIBC /* void  *rawmemchr(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_RAWMEMRCHR  DCC_TARGET_RT_DCC   /* void  *rawmemrchr(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_MEMLEN      DCC_TARGET_RT_DCC   /* size_t memlen(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_MEMRLEN     DCC_TARGET_RT_DCC   /* size_t memrlen(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_MEMEND      DCC_TARGET_RT_DCC   /* void  *memend(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_MEMREND     DCC_TARGET_RT_DCC   /* void  *memrend(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_STROFF      DCC_TARGET_RT_DCC   /* size_t stroff(char const *s, int c); */
#define DCC_TARGET_RT_HAVE_STRROFF     DCC_TARGET_RT_DCC   /* size_t strroff(char const *s, int c); */
#define DCC_TARGET_RT_HAVE_STRCHR      DCC_TARGET_RT_STDC  /* char  *strchr(char const *s, int c); */
#define DCC_TARGET_RT_HAVE_STRRCHR     DCC_TARGET_RT_STDC  /* char  *strrchr(char const *s, int c); */
#define DCC_TARGET_RT_HAVE_STRCHRNUL   DCC_TARGET_RT_GLIBC /* char  *strchrnul(char const *s, int c); */
#define DCC_TARGET_RT_HAVE_STRRCHRNUL  DCC_TARGET_RT_DCC   /* char  *strrchrnul(char const *s, int c); */
#define DCC_TARGET_RT_HAVE_STRNOFF     DCC_TARGET_RT_DCC   /* size_t strnoff(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_HAVE_STRNROFF    DCC_TARGET_RT_DCC   /* size_t strnroff(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_HAVE_STRNCHR     DCC_TARGET_RT_GLIBC /* char  *strnchr(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_HAVE_STRNRCHR    DCC_TARGET_RT_GLIBC /* char  *strnrchr(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_HAVE_STRNCHRNUL  DCC_TARGET_RT_DCC   /* char  *strnchrnul(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_HAVE_STRNRCHRNUL DCC_TARGET_RT_DCC   /* char  *strnrchrnul(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_HAVE_STRCPY      DCC_TARGET_RT_STDC  /* char  *strcat(char *dst, char const *src); */
#define DCC_TARGET_RT_HAVE_STRCAT      DCC_TARGET_RT_STDC  /* char  *strchr(char *dst, char const *src); */

/* Minimal set of functions required for full __builtin_* support. */
#define DCC_TARGET_RT_NAME_MEMCMP      "memcmp"     /* int   memcmp(void const *a, void const *b, size_t s); */
#define DCC_TARGET_RT_NAME_MEMSET      "memset"     /* void *memset(void *dst, int byte, size_t s); */
#define DCC_TARGET_RT_NAME_MEMMOVE     "memmove"    /* void *memmove(void *dst, int byte, size_t s); */
#define DCC_TARGET_RT_NAME_MEMCHR      "memchr"     /* void *memchr(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_NAME_MEMRCHR     "memrchr"    /* void *memrchr(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_NAME_STRNCPY     "strncpy"    /* char *strncpy(char *dst, char const *src, size_t max); */
#define DCC_TARGET_RT_NAME_STRNCAT     "strncat"    /* char *strncat(char *dst, char const *src, size_t max); */

/* Optional functions that may generate improved assembly when present. */
#define DCC_TARGET_RT_NAME_MEMCPY      "memcpy"     /* void  *memcpy(void *dst, int byte, size_t s); */
#define DCC_TARGET_RT_NAME_STRLEN      "strlen"     /* size_t strlen(char const *s); */
#define DCC_TARGET_RT_NAME_STRNLEN     "strnlen"    /* size_t strnlen(char const *s, size_t max); */
#define DCC_TARGET_RT_NAME_STREND      "strend"     /* char  *strend(char const *s); */
#define DCC_TARGET_RT_NAME_STRNEND     "strnend"    /* char  *strnend(char const *s, size_t max); */
#define DCC_TARGET_RT_NAME_RAWMEMLEN   "rawmemlen"  /* size_t rawmemlen(void const *p, int c); */
#define DCC_TARGET_RT_NAME_RAWMEMRLEN  "rawmemrlen" /* size_t rawmemrlen(void const *p, int c); */
#define DCC_TARGET_RT_NAME_RAWMEMCHR   "rawmemchr"  /* void  *rawmemchr(void const *p, int c); */
#define DCC_TARGET_RT_NAME_RAWMEMRCHR  "rawmemrchr" /* void  *rawmemrchr(void const *p, int c); */
#define DCC_TARGET_RT_NAME_MEMLEN      "memlen"     /* size_t memlen(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_NAME_MEMRLEN     "memrlen"    /* size_t memrlen(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_NAME_MEMEND      "memend"     /* void  *memend(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_NAME_MEMREND     "memrend"    /* void  *memrend(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_NAME_STROFF      "stroff"     /* size_t stroff(char const *s, int c); */
#define DCC_TARGET_RT_NAME_STRROFF     "strroff"    /* size_t strroff(char const *s, int c); */
#define DCC_TARGET_RT_NAME_STRCHR      "strchr"     /* char  *strchr(char const *s, int c); */
#define DCC_TARGET_RT_NAME_STRRCHR     "strrchr"    /* char  *strrchr(char const *s, int c); */
#define DCC_TARGET_RT_NAME_STRCHRNUL   "strchrnul"  /* char  *strchrnul(char const *s, int c); */
#define DCC_TARGET_RT_NAME_STRRCHRNUL  "strrchrnul" /* char  *strrchrnul(char const *s, int c); */
#define DCC_TARGET_RT_NAME_STRNOFF     "strnoff"    /* size_t strnoff(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_NAME_STRNROFF    "strnroff"   /* size_t strnroff(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_NAME_STRNCHR     "strnchr"    /* char  *strnchr(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_NAME_STRNRCHR    "strnrchr"   /* char  *strnrchr(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_NAME_STRNCHRNUL  "strnchrnul" /* char  *strnchrnul(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_NAME_STRNRCHRNUL "strnrchrnul"/* char  *strnrchrnul(char const *s, int c, size_t max); */
#define DCC_TARGET_RT_NAME_STRCPY      "strcat"     /* char  *strcat(char *dst, char const *src); */
#define DCC_TARGET_RT_NAME_STRCAT      "strchr"     /* char  *strchr(char *dst, char const *src); */

#if DCC_TARGET_CRT == DCC_CRT_KOS
#undef DCC_TARGET_RT_HAVE_RAWMEMLEN
#undef DCC_TARGET_RT_NAME_RAWMEMLEN
#define DCC_TARGET_RT_HAVE_RAWMEMLEN   1
#define DCC_TARGET_RT_NAME_RAWMEMLEN   "_umemlen"

#undef DCC_TARGET_RT_HAVE_RAWMEMCHR
#undef DCC_TARGET_RT_NAME_RAWMEMCHR
#define DCC_TARGET_RT_HAVE_RAWMEMCHR   1
#define DCC_TARGET_RT_NAME_RAWMEMCHR   "_umemend"

#undef DCC_TARGET_RT_HAVE_MEMLEN
#undef DCC_TARGET_RT_NAME_MEMLEN
#define DCC_TARGET_RT_HAVE_MEMLEN      1
#define DCC_TARGET_RT_NAME_MEMLEN      "_memlen"

#undef DCC_TARGET_RT_HAVE_MEMEND
#undef DCC_TARGET_RT_NAME_MEMEND
#define DCC_TARGET_RT_HAVE_MEMEND      1
#define DCC_TARGET_RT_NAME_MEMEND      "_memend"

#undef DCC_TARGET_RT_HAVE_STREND
#undef DCC_TARGET_RT_NAME_STREND
#define DCC_TARGET_RT_HAVE_STREND      1
#define DCC_TARGET_RT_NAME_STREND      "_strend"

#undef DCC_TARGET_RT_HAVE_STRNEND
#undef DCC_TARGET_RT_NAME_STRNEND
#define DCC_TARGET_RT_HAVE_STRNEND     1
#define DCC_TARGET_RT_NAME_STRNEND     "_strnend"

#undef DCC_TARGET_RT_HAVE_STRNCHR
#undef DCC_TARGET_RT_NAME_STRNCHR
#define DCC_TARGET_RT_HAVE_STRNCHR     1
#define DCC_TARGET_RT_NAME_STRNCHR     "_strnchr"

#undef DCC_TARGET_RT_HAVE_STRNRCHR
#undef DCC_TARGET_RT_NAME_STRNRCHR
#define DCC_TARGET_RT_HAVE_STRNRCHR    1
#define DCC_TARGET_RT_NAME_STRNRCHR    "_strnrchr"
#endif


/* Names of CRT internal functions. */
#define DCC_TARGET_CRT_ALLOCA_EAX      "__pe_alloca_eax"
#define DCC_TARGET_CRT_ALLOCA          "__pe_alloca"


DCC_DECL_END

#endif /* !GUARD_DCC_TARGET_CRT_H */
