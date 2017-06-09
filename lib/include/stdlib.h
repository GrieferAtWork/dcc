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

#if __has_include_next(<stdlib.h>)
#include_next <stdlib.h>
#else
#include <__stdinc.h>

#undef size_t
typedef __SIZE_TYPE__ size_t;
#define NULL  __NULL__

typedef struct { int quot,rem; } div_t;
typedef struct { long quot,rem; } ldiv_t;

__IMP __WUNUSED float (atof)(char const *);
__IMP __WUNUSED int (atoi)(char const *);
__IMP __WUNUSED long (atol)(char const *);

__IMP __WUNUSED double (strtod)(char const *,char **);
__IMP __WUNUSED long (strtol)(char const *,char **);
__IMP __WUNUSED unsigned long (strtoul)(char const *,char **);

__IMP __WUNUSED int (rand)(void);
__IMP int (srand)(unsigned int);
#define RAND_MAX  32767
#define rand() (rand() % RAND_MAX)

__IMP __WUNUSED void *(calloc)(size_t,size_t);
__IMP __WUNUSED void *(malloc)(size_t);
__IMP __WUNUSED void *(realloc)(void *,size_t);
__IMP void (free)(void *);

__IMP _Noreturn void (abort)(void);
__IMP int (atexit)(void (*)(void));

__IMP _Noreturn void (exit)(int);

__IMP __WUNUSED char *(getenv)(char const *);
__IMP int system(char const *);

__IMP void *(bsearch)(void const *,void const *,size_t,size_t,int(*)(void const *,void const *));
__IMP void (qsort)(void *,size_t,size_t,int(*)(void const *,void const *));

__IMP __WUNUSED int (abs)(int);
__IMP __WUNUSED long (labs)(long);

__IMP __WUNUSED div_t (div)(int,int);
__IMP __WUNUSED ldiv_t (ldiv)(long,long);

__IMP __WUNUSED int (mblen)(char const *,size_t);
__IMP int (mbtowc)(__WCHAR_TYPE__,char const *,size_t);
__IMP int (wctomb)(char *,__WCHAR_TYPE__);

__IMP size_t (mbstowcs)(__WCHAR_TYPE__ *,char const *,size_t);
__IMP size_t (wcstombs)(char *,__WCHAR_TYPE__ const *,size_t);


#if __STDLIB_VERSION__ >= 201112L
typedef struct { long long quot,rem; } lldiv_t;

__IMP __WUNUSED long long (atoll)(char const *);
__IMP __WUNUSED float (strtof)(char const *,char **);
__IMP __WUNUSED long double (strtold)(char const *,char **);
__IMP __WUNUSED long long (strtoll)(char const *,char **);
__IMP __WUNUSED unsigned long long (strtoull)(char const *,char **);

__IMP int (at_quick_exit)(void (*)(void));
__IMP _Noreturn void (quick_exit)(int);
__IMP _Noreturn void (_Exit)(int);

__IMP __WUNUSED long long (llabs)(long long);
__IMP __WUNUSED lldiv_t (lldiv)(long long,long long);
#endif

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0
#define MB_CUR_MAX   8 /* ??? */

#endif
