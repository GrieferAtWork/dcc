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
#if __has_include_next(<stdlib.h>)
#include_next <stdlib.h>
#else

typedef struct { int quot,rem; } div_t;
typedef struct { long quot,rem; } ldiv_t;
typedef struct { long long quot,rem; } lldiv_t;

#pragma push_macro("__IMP")
#ifdef __PE__
#define __IMP [[__dllimport__]]
#else
#define __IMP
#endif

__IMP [[__warn_unused_result__]] float (atof)(char const *);
__IMP [[__warn_unused_result__]] int (atoi)(char const *);
__IMP [[__warn_unused_result__]] long (atol)(char const *);
__IMP [[__warn_unused_result__]] long long (atoll)(char const *);

__IMP [[__warn_unused_result__]] float (strtof)(char const *,char **);
__IMP [[__warn_unused_result__]] double (strtod)(char const *,char **);
__IMP [[__warn_unused_result__]] long double (strtold)(char const *,char **);
__IMP [[__warn_unused_result__]] long (strtol)(char const *,char **);
__IMP [[__warn_unused_result__]] long long (strtoll)(char const *,char **);
__IMP [[__warn_unused_result__]] unsigned long (strtoul)(char const *,char **);
__IMP [[__warn_unused_result__]] unsigned long long (strtoull)(char const *,char **);

__IMP [[__warn_unused_result__]] int (rand)(void);
__IMP int (srand)(unsigned int);
#define RAND_MAX  32767
#define rand() (rand() % RAND_MAX)

__IMP [[__warn_unused_result__]] void *(calloc)(__SIZE_TYPE__,__SIZE_TYPE__);
__IMP [[__warn_unused_result__]] void *(malloc)(__SIZE_TYPE__);
__IMP [[__warn_unused_result__]] void *(realloc)(void *,__SIZE_TYPE__);
__IMP void (free)(void *);

__IMP _Noreturn void (abort)(void);
__IMP int (atexit)(void (*)(void));
__IMP int (at_quick_exit)(void (*)(void));

__IMP _Noreturn void (exit)(int);
__IMP _Noreturn void (_Exit)(int);
__IMP _Noreturn void (quick_exit)(int);

__IMP [[__warn_unused_result__]] char *(getenv)(char const *);
__IMP int system(char const *);

__IMP void *(bsearch)(void const *,void const *,__SIZE_TYPE__,__SIZE_TYPE__,int(*)(void const *,void const *));
__IMP void (qsort)(void *,__SIZE_TYPE__,__SIZE_TYPE__,int(*)(void const *,void const *));

__IMP [[__warn_unused_result__]] int (abs)(int);
__IMP [[__warn_unused_result__]] long (labs)(long);
__IMP [[__warn_unused_result__]] long (llabs)(long);

__IMP [[__warn_unused_result__]] div_t (div)(int,int);
__IMP [[__warn_unused_result__]] ldiv_t (ldiv)(long,long);
__IMP [[__warn_unused_result__]] lldiv_t (lldiv)(long long,long long);

__IMP [[__warn_unused_result__]] int (mblen)(char const *,__SIZE_TYPE__);
__IMP int (mbtowc)(__WCHAR_TYPE__,char const *,__SIZE_TYPE__);
__IMP int (wctomb)(char *,__WCHAR_TYPE__);

__IMP __SIZE_TYPE__ (mbstowcs)(__WCHAR_TYPE__ *,char const *,__SIZE_TYPE__);
__IMP __SIZE_TYPE__ (wcstombs)(char *,__WCHAR_TYPE__ const *,__SIZE_TYPE__);

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
#define MB_CUR_MAX   8 /* ??? */

#pragma pop_macro("__IMP")

#endif
#pragma pop_macro(undef,"calloc","free","malloc","malloc_usable_size","mallopt",\
                        "realloc","cfree","memalign","aligned_alloc","pvalloc",\
                        "valloc","memdup","strdup","strndup","strdupf","vstrdupf")

/* Fixed/optimized system header <stdlib.h> for DCC */
#undef size_t
typedef __SIZE_TYPE__ size_t;

#define NULL  __NULL__
