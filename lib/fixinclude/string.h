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

#if __has_include_next(<string.h>)
#pragma push_macro(undef,"calloc","free","malloc","malloc_usable_size","mallopt",\
                         "realloc","cfree","memalign","aligned_alloc","pvalloc",\
                         "valloc","memdup","strdup","strndup","strdupf","vstrdupf")
#include_next <string.h>
#pragma pop_macro(undef,"calloc","free","malloc","malloc_usable_size","mallopt",\
                        "realloc","cfree","memalign","aligned_alloc","pvalloc",\
                        "valloc","memdup","strdup","strndup","strdupf","vstrdupf")
#elif defined(__INTELLISENSE__)
#   include "../include/string.h"
#endif

/* Fixed/optimized system header <string.h> for DCC */
#undef size_t
typedef __SIZE_TYPE__ size_t;

#define NULL  __NULL__

/* Use compiler-optimized versions for string operations.
 * NOTE: Unlike other builtins, these will generate regular function
 *       calls to if no special optimizations can be performed. */
#define memcpy(dst,src,size)  __builtin_memcpy((dst),(src),(size))
#define memmove(dst,src,size) __builtin_memmove((dst),(src),(size))
#define memset(dst,byt,size)  __builtin_memset((dst),(byt),(size))
#define memcmp(a,b,size)      __builtin_memcmp((a),(b),(size))
#define strlen(str)           __builtin_strlen((str))

#include <features.h>
#if defined(__USE_DCC) || defined(_GNU_SOURCE) || \
   (_XOPEN_SOURCE-0) >= 700 || \
   (_POSIX_C_SOURCE-0) >= 200809L
#define strnlen(str,maxlen)   __builtin_strnlen((str),(maxlen))
#endif

#define memchr(p,c,s)         __builtin_memchr((p),(c),(s))
#define strchr(s,c)           __builtin_strchr((s),(c))
#define strrchr(s,c)          __builtin_strrchr((s),(c))
#define strcat(dst,src)       __builtin_strcat((dst),(src))
#define strcpy(dst,src)       __builtin_strcpy((dst),(src))
#define strncat(dst,src,max)  __builtin_strncat((dst),(src),(max))
#define strncpy(dst,src,max)  __builtin_strncpy((dst),(src),(max))

/* Additional memory functions offered by DCC as extension.
 * NOTE: A 'character' is always as 'byte'!
 * >> __memrchr(p,c,s)      --> Same as memchr(p,c,s), but scanning in
 *                              reverse order, the first character checked
 *                              is 'p+(s-1)' and the last is 'p'.
 *                              When 'c' is not found, NULL is returned.
 * >> __strend(str)         --> Same as: str+strlen(str)
 * >> __strnend(str,maxlen) --> Same as: str+strnlen(str,maxlen)
 * >> __mem(r)end(p,c,s)    --> Same as mem(r)chr(p,c,s), but where memchr
 *                              would normally return NULL for a character
 *                              not found, return the one index past the
 *                              last searched character (aka. 'p-1' or 'p+s')
 *                              HINT: strend() could then be implemented using
 *                                    "(char *)__memend(s,'\0',(size_t)-1)"
 * >> __mem(r)len(p,c,s)    --> Sase as mem(r)end(p,c,s), but return the offset
 *                              from 'p' to the found character as size_t.
 *                              HINT: strlen() could then be implemented using
 *                                    "__memlen(s,'\0',(size_t)-1)"
 * >> __rawmem(r)len(p,c)   --> Same as __mem(r)len(p,c,(size_t)-1)
 * >> __rawmem(r)chr(p,c)   --> Same as __mem(r)chr(p,c,(size_t)-1)
 *                              HINT: Because of the infinite search size,
 *                                    this function will probably crash
 *                                    before returning NULL, and depending
 *                                    on its implementation, may not even
 *                                    be able to return NULL for that reason!
 * // Builtin string helper functions
 * >> __str(r)off(s,c)         --> Same as __mem(r)len(s,c,strlen(s))
 * >> __str(r)chr(s,c)         --> Same as __mem(r)chr(s,c,strlen(s))
 * >> __str(r)chrnul(s,c)      --> Same as __mem(r)end(s,c,strlen(s))
 * >> __strn(r)off(s,c,max)    --> Same as __mem(r)len(s,c,strnlen(s,max))
 * >> __strn(r)chr(s,c,max)    --> Same as __mem(r)chr(s,c,strnlen(s,max))
 * >> __strn(r)chrnul(s,c,max) --> Same as __mem(r)end(s,c,strnlen(s,max))
 */
#define __strnlen(str,maxlen) __builtin_strnlen((str),(maxlen))
#define __memrchr(p,c,s)      __builtin_memrchr((p),(c),(s))
#define __strend(str)         __builtin_rawmemlen((str),'\0')
#define __strnend(str,maxlen) __builtin_memlen((str),'\0',(maxlen))
#define __memlen(p,c,s)       __builtin_memlen((p),(c),(s))
#define __memend(p,c,s)       __builtin_memend((p),(c),(s))
#define __memrlen(p,c,s)      __builtin_memrlen((p),(c),(s))
#define __memrend(p,c,s)      __builtin_memrend((p),(c),(s))
#define __rawmemchr(p,c)      __builtin_rawmemchr((p),(c))
#define __rawmemlen(p,c)      __builtin_rawmemlen((p),(c))
#define __rawmemrchr(p,c)     __builtin_rawmemrchr((p),(c))
#define __rawmemrlen(p,c)     __builtin_rawmemrlen((p),(c))
#define __stroff(s,c)         __builtin_stroff((s),(c))
#define __strroff(s,c)        __builtin_strroff((s),(c))
#define __strchrnul(s,c)      __builtin_strchrnul((s),(c))
#define __strrchrnul(s,c)     __builtin_strrchrnul((s),(c))
#define __strnoff(s,c,max)    __builtin_strnoff((s),(c),(max))
#define __strnroff(s,c,max)   __builtin_strnroff((s),(c),(max))
#define __strnchr(s,c,max)    __builtin_strnchr((s),(c),(max))
#define __strnrchr(s,c,max)   __builtin_strnrchr((s),(c),(max))
#define __strnchrnul(s,c,max) __builtin_strnchrnul((s),(c),(max))
#define __strnrchrnul(s,c,max) __builtin_strnrchrnul((s),(c),(max))

#if defined(_GNU_SOURCE) || defined(__USE_DCC)
#   define memrchr(p,c,s)     __builtin_memrchr((p),(c),(s))
#   define rawmemchr(p,c)     __builtin_rawmemchr((p),(c))
#   define strchrnul(s,c)     __builtin_strchrnul((s),(c))
#endif


#ifdef __USE_DCC /* Enable DCC extension functions. */
#   define strend(str)        __builtin_rawmemlen((str),'\0')
#   define strnend(str,maxlen) __builtin_memlen((str),'\0',(maxlen))
#   define memlen(p,c,s)      __builtin_memlen((p),(c),(s))
#   define memend(p,c,s)      __builtin_memend((p),(c),(s))
#   define memrlen(p,c,s)     __builtin_memrlen((p),(c),(s))
#   define memrend(p,c,s)     __builtin_memrend((p),(c),(s))
#   define rawmemlen(p,c)     __builtin_rawmemlen((p),(c))
#   define rawmemrchr(p,c)    __builtin_rawmemrchr((p),(c))
#   define rawmemrlen(p,c)    __builtin_rawmemrlen((p),(c))
#   define stroff(s,c)        __builtin_stroff((s),(c))
#   define strroff(s,c)       __builtin_strroff((s),(c))
#   define strrchrnul(s,c)    __builtin_strrchrnul((s),(c))
#   define strnoff(s,c,max)   __builtin_strnoff((s),(c),(max))
#   define strnroff(s,c,max)  __builtin_strnroff((s),(c),(max))
#   define strnchr(s,c,max)   __builtin_strnchr((s),(c),(max))
#   define strnrchr(s,c,max)  __builtin_strnrchr((s),(c),(max))
#   define strnchrnul(s,c,max) __builtin_strnchrnul((s),(c),(max))
#   define strnrchrnul(s,c,max) __builtin_strnrchrnul((s),(c),(max))
#endif

#ifdef __KOS__
/* KOS actually has some of these. */
#   define _strend(str)       __builtin_rawmemlen((str),'\0')
#   define _strnend(str,maxlen) __builtin_memlen((str),'\0',(maxlen))
#   define _memend(p,c,s)     __builtin_memend((p),(c),(s))
#   define _umemend(p,c)      __builtin_rawmemchr((p),(c))
#ifndef __STDC_PURE__
#   define strend(str)        __builtin_rawmemlen((str),'\0')
#   define strnend(str,maxlen) __builtin_memlen((str),'\0',(maxlen))
#   define memend(p,c,s)      __builtin_memend((p),(c),(s))
#   define umemend(p,c)       __builtin_rawmemchr((p),(c))
#endif /* !__STDC_PURE__ */
#endif /* __KOS__ */

#ifdef _GNU_SOURCE
#   define ffsl(x)            __builtin_ffsl((x))
#   define ffsll(x)           __builtin_ffsll((x))
#endif
