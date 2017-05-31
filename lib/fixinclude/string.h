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

#pragma push_macro(undef,"calloc","free","malloc","malloc_usable_size","mallopt",\
                         "realloc","cfree","memalign","aligned_alloc","pvalloc",\
                         "valloc","memdup","strdup","strndup","strdupf","vstrdupf")
#if __has_include_next(<string.h>)
#include_next <string.h>
#else

#pragma push_macro("__IMP")
#ifdef __PE__
#define __IMP [[__dllimport__]]
#else
#define __IMP
#endif

__IMP void *(memcpy)(void *,void const *,__SIZE_TYPE__);
__IMP void *(memmove)(void *,void const *,__SIZE_TYPE__);
__IMP char *(strcpy)(char *,char const *);
__IMP char *(strncpy)(char *,char const *,__SIZE_TYPE__);

__IMP char *(strcat)(char *,char const *);
__IMP char *(strncat)(char *,char const *,__SIZE_TYPE__);

__IMP [[__warn_unused_result__]] int (memcmp)(void const *,void const *,__SIZE_TYPE__);
__IMP [[__warn_unused_result__]] int (strcmp)(char const *,char const *);
__IMP [[__warn_unused_result__]] int (strcoll)(char const *,char const *);
__IMP [[__warn_unused_result__]] int (strncmp)(char const *,char const *,__SIZE_TYPE__);
__IMP [[__warn_unused_result__]] __SIZE_TYPE__ (strxfrm)(char const *,char const *,__SIZE_TYPE__);

__IMP [[__warn_unused_result__]] void *(memchr)(void const *,int,__SIZE_TYPE__);
__IMP [[__warn_unused_result__]] char *(strchr)(char const *,int);
__IMP [[__warn_unused_result__]] __SIZE_TYPE__ (strcspn)(char const *,char const *);
__IMP [[__warn_unused_result__]] char *(strpbrk)(char const *,char const *);
__IMP [[__warn_unused_result__]] char *(strrchr)(char const *,int);
__IMP [[__warn_unused_result__]] __SIZE_TYPE__ (strspn)(char const *,char const *);
__IMP [[__warn_unused_result__]] char *(strstr)(char const *,char const *);
__IMP [[__warn_unused_result__]] char *(strtok)(char *,char const *);

__IMP void *(memset)(void *,int,__SIZE_TYPE__);
__IMP [[__warn_unused_result__]] char *(strerror)(int);
__IMP [[__warn_unused_result__]] __SIZE_TYPE__ (strlen)(char const *);

#pragma pop_macro("__IMP")
#endif
#pragma pop_macro(undef,"calloc","free","malloc","malloc_usable_size","mallopt",\
                        "realloc","cfree","memalign","aligned_alloc","pvalloc",\
                        "valloc","memdup","strdup","strndup","strdupf","vstrdupf")

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




