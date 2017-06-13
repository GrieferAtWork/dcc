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
#include <features.h>

#undef size_t
typedef __SIZE_TYPE__ size_t;
#define NULL  __NULL__

#define EXIT_FAILURE 1
#define EXIT_SUCCESS 0

#ifdef __CRT_MSVC
#define MB_CUR_MAX ((size_t)___mb_cur_max_func())
__IMP __WUNUSED int (___mb_cur_max_func)(void);
#elif defined(__CRT_GLIBC)
#define	MB_CUR_MAX	   (__ctype_get_mb_cur_max())
__IMP __WUNUSED size_t (__ctype_get_mb_cur_max)(void);
#else
#	define MB_CUR_MAX 8 /* ??? */
#endif

#pragma push_macro(undef,"quot","rem")
#undef div_t
#undef ldiv_t
typedef struct { int quot,rem; } div_t;
typedef struct { long quot,rem; } ldiv_t;
#ifdef __USE_ISOC99
#undef lldiv_t
typedef struct { long long quot,rem; } lldiv_t;
#endif
#pragma pop_macro("quot","rem")

__IMP __WUNUSED double (strtod)(char const *__restrict __nptr, char **__restrict __endptr);
__IMP __WUNUSED long (strtol)(char const *__restrict __nptr, char **__restrict __endptr, int __base);
__IMP __WUNUSED unsigned long (strtoul)(char const *__restrict __nptr, char **__restrict __endptr, int __base);

#ifdef __USE_MISC
__IMP __WUNUSED long long (strtoq)(char const *__restrict __nptr,
                                   char **__restrict __endptr, int __base)
#if defined(__CRT_MSVC) || defined(__CRT_KOS)
	__asm__("strtoll")
#endif
;
__IMP __WUNUSED unsigned long long (strtouq)(char const *__restrict __nptr,
                                             char **__restrict __endptr, int __base)
#if defined(__CRT_MSVC) || defined(__CRT_KOS)
	__asm__("strtoull")
#endif
;
#endif /* __USE_MISC */

#ifdef __USE_ISOC99
__IMP float (strtof)(char const *__restrict __nptr, char **__restrict __endptr);
__IMP long double (strtold)(char const *__restrict __nptr, char **__restrict __endptr);
__IMP long long (strtoll)(char const *__restrict __nptr, char **__restrict __endptr, int __base);
__IMP unsigned long long (strtoull)(char const *__restrict __nptr, char **__restrict __endptr, int __base);
#endif /* __USE_ISOC99 */

#ifdef __CRT_MSVC
#	define __LOCALE_FUNC(x) __asm__("_" x "_l")
#else
#	define __LOCALE_FUNC(x) __asm__(x "_l")
#endif

#ifdef __USE_GNU
#include <xlocale.h>
__IMP long (strtol_l)(char const *__restrict __nptr, char **__restrict __endptr, int __base, __locale_t __loc) __LOCALE_FUNC("strtol");
__IMP unsigned long (strtoul_l)(char const *__restrict __nptr, char **__restrict __endptr, int __base, __locale_t __loc) __LOCALE_FUNC("strtoul");
__IMP long long (strtoll_l)(char const *__restrict __nptr, char **__restrict __endptr, int __base, __locale_t __loc) __LOCALE_FUNC("strtoll");
__IMP unsigned long long (strtoull_l)(char const *__restrict __nptr, char **__restrict __endptr, int __base, __locale_t __loc) __LOCALE_FUNC("strtoull");
__IMP double (strtod_l)(char const *__restrict __nptr, char **__restrict __endptr, __locale_t __loc) __LOCALE_FUNC("strtod");
__IMP float (strtof_l)(char const *__restrict __nptr, char **__restrict __endptr, __locale_t __loc) __LOCALE_FUNC("strtof");
__IMP long double (strtold_l)(char const *__restrict __nptr, char **__restrict __endptr, __locale_t __loc) __LOCALE_FUNC("strtold");
#endif /* __USE_GNU */

#undef __LOCALE_FUNC

__IMP __WUNUSED float (atof)(char const *__nptr);
__IMP __WUNUSED int (atoi)(char const *__nptr);
__IMP __WUNUSED long (atol)(char const *__nptr);
#ifdef __USE_ISOC99
__IMP __WUNUSED long long (atoll)(char const *__restrict __nptr);
#endif

#ifndef __INTELLISENSE__
#	define atof(nptr)  strtod(nptr,(char **)0)
#if __SIZEOF_INT__ == __SIZEOF_LONG__
#	define atoi(nptr) (int)strtol(nptr,(char **)0,10)
#endif /* __SIZEOF_INT__ == __SIZEOF_LONG__ */
#	define atol(nptr)  strtol(nptr,(char **)0,10)
#ifdef __USE_ISOC99
#	define atoll(nptr) strtoll(nptr,(char **)0,10)
#endif /* __USE_ISOC99 */
#endif

#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __WUNUSED char *(l64a)(long int __n);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __WUNUSED long int (a64l)(char const *__s);
#endif


#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS long int (random)(void);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS void (srandom)(unsigned int __seed);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS char *(initstate)(unsigned int __seed, char *__statebuf, size_t __statelen);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS char *(setstate)(char *__statebuf);

#ifdef __USE_MISC
struct __CRT_UNSUPPORTED_MSVC random_data {
	__INT32_TYPE__ *fptr;
	__INT32_TYPE__ *rptr;
	__INT32_TYPE__ *state;
	int             rand_type;
	int             rand_deg;
	int             rand_sep;
	__INT32_TYPE__ *end_ptr;
};
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (random_r)(struct random_data *__restrict __buf, __INT32_TYPE__ *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (srandom_r)(unsigned int __seed, struct random_data *__buf);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (initstate_r)(unsigned int __seed, char *__restrict __statebuf, size_t __statelen, struct random_data *__restrict __buf);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (setstate_r)(char *__restrict __statebuf, struct random_data *__restrict __buf);
#endif /* __USE_MISC */
#endif


__IMP int (srand)(unsigned int);
#ifdef __CRT_MSVC
__IMP __WUNUSED int (rand)(void);
#	define RAND_MAX  32767
#elif defined(__CRT_GLIBC)
__IMP __WUNUSED int (rand)(void);
#	define RAND_MAX  2147483647
#else
__IMP __WUNUSED int (__libc_rand)(void) __asm__("rand");
#	define RAND_MAX  32767
#	define rand()   (__libc_rand() % RAND_MAX)
#endif

#ifdef __USE_POSIX
/* Reentrant interface according to POSIX.1.  */
__IMP __CRT_UNSUPPORTED_KOS __WUNUSED int (rand_r)(unsigned int *__seed)
#ifdef __CRT_MSVC
	__asm__("rand_s")
#endif
;
#endif

#if defined(__USE_MISC) || defined(__USE_XOPEN)
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS double (drand48)(void);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS double (erand48)(unsigned short int __xsubi[3]);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS long int (lrand48)(void);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS long int (nrand48)(unsigned short int __xsubi[3]);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS long int (mrand48)(void);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS long int (jrand48)(unsigned short int __xsubi[3]);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS void (srand48)(long int __seedval);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS unsigned short int *(seed48)(unsigned short int __seed16v[3]);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS void (lcong48)(unsigned short int __param[7]);
#ifdef __USE_MISC
struct drand48_data {
	unsigned short     __x[3];
	unsigned short     __old_x[3];
	unsigned short     __c;
	unsigned short     __init;
	unsigned long long __a;
};
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (drand48_r)(struct drand48_data *__restrict __buffer, double *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (erand48_r)(unsigned short int __xsubi[3], struct drand48_data *__restrict __buffer, double *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (lrand48_r)(struct drand48_data *__restrict __buffer, long int *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (nrand48_r)(unsigned short int __xsubi[3], struct drand48_data *__restrict __buffer, long int *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (mrand48_r)(struct drand48_data *__restrict __buffer, long int *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (jrand48_r)(unsigned short int __xsubi[3], struct drand48_data *__restrict __buffer, long int *__restrict __result);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (srand48_r)(long int __seedval, struct drand48_data *__buffer);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (seed48_r)(unsigned short int __seed16v[3], struct drand48_data *__buffer);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (lcong48_r)(unsigned short int __param[7], struct drand48_data *__buffer);
#endif /* __USE_MISC */
#endif /* defined(__USE_MISC) || defined(__USE_XOPEN) */

__IMP __WUNUSED void *(calloc)(size_t __count, size_t __size);
__IMP __WUNUSED void *(malloc)(size_t __size);
__IMP __WUNUSED void *(realloc)(void *__ptr, size_t __size);
__IMP void (free)(void *__ptr);

#ifdef	__USE_MISC
__IMP void (cfree)(void *__ptr)
#ifdef __CRT_MSVC
	__asm__("free")
#endif
;
#include <alloca.h>
#endif /* __USE_MISC */

#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_XOPEN2K))
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED void *(valloc)(size_t __size);
#endif
#ifdef __USE_XOPEN2K
__IMP __CRT_UNSUPPORTED_MSVC int (posix_memalign)(void **__memptr, size_t __alignment, size_t __size);
#endif
#ifdef __USE_ISOC11
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED void *aligned_alloc(size_t __alignment, size_t __size);
#endif

__IMP _Noreturn void (abort)(void);
__IMP int (atexit)(void (*)(void));
#if defined(__USE_ISOC11) || defined(__USE_ISOCXX11)
__IMP __CRT_UNSUPPORTED_MSVC int (at_quick_exit)(void (*)(void));
__IMP __CRT_UNSUPPORTED_MSVC _Noreturn void (quick_exit)(int);
#endif
#ifdef	__USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC int (on_exit)(void (*__func)(int __status, void *__arg), void *__arg);
#endif

__IMP _Noreturn void (exit)(int);

#ifdef __USE_ISOC99
__IMP _Noreturn void (_Exit)(int);
#endif

__IMP __WUNUSED char *(getenv)(char const *);
#ifdef __USE_GNU
__IMP __CRT_WORKAROUND_MSVC __CRT_WORKAROUND_KOS
__WUNUSED char *(secure_getenv)(char const *__name)
#if defined(__CRT_MSVC) || defined(__CRT_KOS)
	__asm__("getenv")
#endif
;
#endif

#if defined(__USE_MISC) || defined(__USE_XOPEN)
__IMP int (putenv)(char *__string)
#ifdef __CRT_MSVC
	__asm__("_putenv")
#endif
;
#endif

#ifdef __USE_XOPEN2K
#ifdef __CRT_MSVC
__IMP __CRT_WORKAROUND_MSVC int (__msvc_setenv)(char const *__name, char const *__value) __asm__("_putenv_s");
#define setenv(name,value,replace) \
	((replace) ? __msvc_setenv((name),(value)) : __extension__({ \
		register char const *__n = (name); \
		register char *__o = getenv(__n); \
		!__o || !*__o ? __msvc_setenv(__n,(value)) : ((void)(value),0); \
	}))
#define unsetenv(name) __msvc_setenv((name),"")
#else
__IMP int (setenv)(char const *__name, char const *__value, int __replace);
extern int (unsetenv)(char const *__name);
#endif
#endif

#ifdef	__USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC int (clearenv)(void);
#endif

#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_XOPEN2K8))
__IMP char *(mktemp)(char *__template)
#ifdef __CRT_MSVC
	__asm__("_mktemp")
#endif
;
#endif

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __CRT_WORKAROUND_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkstemp)(char *__template)
#ifdef __USE_FILE_OFFSET64
	__asm__("mkstemp64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __CRT_WORKAROUND_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkstemp64)(char *__template);
#endif
#endif

#ifdef __USE_MISC
__IMP __CRT_WORKAROUND_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkstemps)(char *__template, int __suffixlen)
#ifdef __USE_FILE_OFFSET64
	__asm__("mkstemps64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __CRT_WORKAROUND_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkstemps64)(char *__template, int __suffixlen);
#endif
#endif

#ifdef __USE_XOPEN2K8
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS char *(mkdtemp)(char *__template);
#endif

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkostemp)(char *__template, int __flags)
#ifndef __USE_FILE_OFFSET64
	__asm__("mkostemp64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkostemp64)(char *__template, int __flags);
#endif
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkostemps)(char *__template, int __suffixlen, int __flags)
#ifdef __USE_FILE_OFFSET64
	__asm__("mkostemps64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (mkostemps64)(char *__template, int __suffixlen, int __flags);
#endif
#endif /* __USE_GNU */

__IMP int (system)(char const *);

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED char *(canonicalize_file_name)(char const *__name);
#endif

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED char *(realpath)(char const *__restrict __name,
                           char *__restrict __resolved);
#endif


#ifndef __COMPAR_FN_T
#define __COMPAR_FN_T
typedef int (*__compar_fn_t)(const void *, const void *);
#endif
#ifdef __USE_GNU
typedef __compar_fn_t comparison_fn_t;
#endif
#ifdef __USE_GNU
typedef int (*__compar_d_fn_t)(const void *, const void *, void *);
#endif

__IMP __WUNUSED void *(bsearch)(const void *__key, const void *__base,
                                size_t __nmemb, size_t __size,
                                __compar_fn_t __compar);

#if defined(__USE_EXTERN_INLINES) && \
    __has_include(<bits/stdlib-bsearch.h>)
#	include <bits/stdlib-bsearch.h>
#endif

__IMP void (qsort)(void *__base, size_t __nmemb,
                   size_t __size, __compar_fn_t __compar);
#ifdef __USE_GNU
__IMP void (qsort_r)(void *__base, size_t __nmemb, size_t __size,
                     __compar_d_fn_t __compar, void *__arg)
#ifdef __CRT_MSVC
	__asm__("qsort_s")
#endif
;
#endif /* __USE_GNU */

__IMP __WUNUSED int (abs)(int __x);
__IMP __WUNUSED long (labs)(long __x);
#ifdef __USE_ISOC99
__IMP __WUNUSED long long (llabs)(long long __x);
#endif

__IMP __WUNUSED div_t (div)(int __numer, int __denom);
__IMP __WUNUSED ldiv_t (ldiv)(long __numer, long __denom);
#ifdef __USE_ISOC99
__IMP __WUNUSED lldiv_t (lldiv)(long long __numer, long long __denom);
#endif

#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_XOPEN2K8))
__IMP __WUNUSED __CRT_UNSUPPORTED_KOS
char *(ecvt)(double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign)
#ifdef __CRT_MSVC
	__asm__("_ecvt")
#endif
;
__IMP __WUNUSED __CRT_UNSUPPORTED_KOS
char *(fcvt)(double __value, int __ndigit, int *__restrict __decpt, int *__restrict __sign)
#ifdef __CRT_MSVC
	__asm__("_fcvt")
#endif
;
__IMP __WUNUSED __CRT_UNSUPPORTED_KOS
char *(gcvt)(double __value, int __ndigit, char *__buf)
#ifdef __CRT_MSVC
	__asm__("_gcvt")
#endif
;
#endif

#ifdef __USE_MISC
/* Long double versions of above functions.  */
__IMP __WUNUSED __CRT_UNSUPPORTED_KOS __CRT_UNSUPPORTED_MSVC
char *(qecvt)(long double __value, int __ndigit,
              int *__restrict __decpt, int *__restrict __sign);
__IMP __WUNUSED __CRT_UNSUPPORTED_KOS __CRT_UNSUPPORTED_MSVC
char *(qfcvt)(long double __value, int __ndigit,
              int *__restrict __decpt, int *__restrict __sign);
__IMP __WUNUSED __CRT_UNSUPPORTED_KOS __CRT_UNSUPPORTED_MSVC
char *(qgcvt)(long double __value, int __ndigit, char *__buf);

#if defined(__CRT_MSVC) && !defined(__INTELLISENSE__)
__IMP int (__msvc_ecvt_s)(char *__buf, size_t __len, double __value,
                          int __ndigit, int *__decpt, int *__sign)
                          __asm__("_ecvt_s");
__IMP int (__msvc_fcvt_s)(char *__buf, size_t __len, double __value,
                          int __ndigit, int *__decpt, int *__sign)
                          __asm__("_fcvt_s");
#define ecvt_r(value,ndigit,decpt,sign,buf,len) \
 __msvc_ecvt_s(buf,len,value,ndigit,decpt,sign)
#define fcvt_r(value,ndigit,decpt,sign,buf,len) \
 __msvc_fcvt_s(buf,len,value,ndigit,decpt,sign)
#else
__IMP __CRT_UNSUPPORTED_KOS
int (ecvt_r)(double __value, int __ndigit, int *__restrict __decpt,
             int *__restrict __sign, char *__restrict __buf, size_t __len);
__IMP __CRT_UNSUPPORTED_KOS
int (fcvt_r)(double __value, int __ndigit, int *__restrict __decpt,
             int *__restrict __sign, char *__restrict __buf, size_t __len);
#endif
__IMP __CRT_UNSUPPORTED_KOS __CRT_UNSUPPORTED_MSVC
int (qecvt_r)(long double __value, int __ndigit,
              int *__restrict __decpt, int *__restrict __sign,
              char *__restrict __buf, size_t __len);
__IMP __CRT_UNSUPPORTED_KOS __CRT_UNSUPPORTED_MSVC
int (qfcvt_r)(long double __value, int __ndigit,
              int *__restrict __decpt, int *__restrict __sign,
              char *__restrict __buf, size_t __len);
#endif

__IMP __WUNUSED int (mblen)(char const *__s, size_t __n);
__IMP int (mbtowc)(__WCHAR_TYPE__ *__restrict __pwc, char const *__restrict __s, size_t __n);
__IMP int (wctomb)(char *__s, __WCHAR_TYPE__ __wchar);

__IMP size_t (mbstowcs)(__WCHAR_TYPE__ *__restrict __pwcs, char const *__restrict __s, size_t __n);
__IMP size_t (wcstombs)(char *__restrict __s, __WCHAR_TYPE__ const *__restrict __pwcs, size_t __n);

#ifdef __USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (rpmatch)(char const *__response);
#endif

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (getsubopt)(char **__restrict __optionp,
                          char *const *__restrict __tokens,
                          char **__restrict __valuep);
#endif

#ifdef __USE_XOPEN
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
void (setkey)(char const *__key);
#endif

#ifdef __USE_XOPEN2KXSI
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
__WUNUSED int (posix_openpt)(int __oflag);
#endif

#ifdef __USE_XOPEN
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (grantpt)(int __fd);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (unlockpt)(int __fd);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __WUNUSED char *(ptsname)(int __fd);
#endif

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (ptsname_r)(int __fd, char *__buf, size_t __buflen);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (getpt)(void);
#endif

#ifdef __USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS
int (getloadavg)(double __loadavg[], int __nelem);
#endif

#if __has_include(<bits/stdlib-float.h>)
#include <bits/stdlib-float.h>
#endif

#if defined(__USE_FORTIFY_LEVEL) && \
  ((__USE_FORTIFY_LEVEL-0) > 0) && \
    defined(__fortify_function) && \
    __has_include(<bits/stdlib.h>)
#	include <bits/stdlib.h>
#endif
#if defined(__LDBL_COMPAT) && \
    __has_include(<bits/stdlib-ldbl.h>)
#	include <bits/stdlib-ldbl.h>
#endif

#endif /* !include_next */
