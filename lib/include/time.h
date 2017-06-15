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

#if __has_include_next(<time.h>)
#include_next <time.h>
#else
#include <__stdinc.h>
#include <features.h>
#include <bits/types.h>

#undef size_t
typedef __SIZE_TYPE__ size_t;

#undef time_t
typedef __time_t  time_t;

#undef clock_t
typedef __clock_t clock_t;

#define NULL  __NULL__

struct tm;

#ifdef __CRT_MSVC
#define CLOCKS_PER_SEC  1000

#ifdef _USE_32BIT_TIME_T
#	define __TIMENAM(n)  __asm__("_" n "32")
#	define __TIMENAMS(n) __asm__("_" n "32_s")
#else
#	define __TIMENAM(n)  __asm__("_" n "64")
#	define __TIMENAMS(n) __asm__("_" n "64_s")
#endif

__IMP __WUNUSED double (difftime)(time_t __time1, time_t __time2) __TIMENAM("difftime");
__IMP __WUNUSED char *(ctime)(time_t const *__timep) __TIMENAM("ctime");
__IMP __WUNUSED struct tm *(gmtime)(time_t const *__timep) __TIMENAM("gmtime");
__IMP __WUNUSED struct tm *(localtime)(time_t const *__timep) __TIMENAM("localtime");
__IMP __WUNUSED time_t (mktime)(struct tm *__tm) __TIMENAM("mktime");
__IMP __WUNUSED time_t (time)(time_t *__timep) __TIMENAM("time");

__IMP __WUNUSED time_t (__cdecl __msvc_mkgmtime)(struct tm *__tm) __TIMENAM("mkgmtime");
__IMP /*errno_t*/int   (__cdecl __msvc_ctime_s)(char *__buf, size_t __sizeinbytes, time_t const *__timep) __TIMENAMS("ctime");
__IMP /*errno_t*/int   (__cdecl __msvc_gmtime_s)(struct tm *__tm, time_t const *__timep) __TIMENAMS("gmtime");
__IMP /*errno_t*/int   (__cdecl __msvc_localtime_s)(struct tm *__tm, time_t const *__timep) __TIMENAMS("localtime");
__IMP /*errno_t*/int   (__cdecl __msvc_asctime_s)(char *__buf, size_t __sizeinbytes, struct tm const *__tm) __TIMENAMS("asctime");

#ifdef __USE_POSIX
#ifdef __INTELLISENSE__
char *(asctime_r)(const struct tm *tm, char *buf);
char *(ctime_r)(const time_t *timep, char *buf);
struct tm *(gmtime_r)(const time_t *timep, struct tm *result);
struct tm *(localtime_r)(const time_t *timep, struct tm *result);
#else
#	define asctime_r(tm,buf)         __extension__({ char *const __buf = (buf); __msvc_asctime_s(__buf,(size_t)-1,tm) ? (char *)0 : __buf; })
#	define ctime_r(tm,buf)           __extension__({ char *const __buf = (buf); __msvc_ctime_s(__buf,(size_t)-1,tm) ? (char *)0 : __buf; })
#	define gmtime_r(timep,result)    __extension__({ struct tm *const __r = (result); __msvc_gmtime_s(__r,timep) ? (struct tm *)0 : __r; })
#	define localtime_r(timep,result) __extension__({ struct tm *const __r = (result); __msvc_localtime_s(__r,timep) ? (struct tm *)0 : __r; })
#endif
#endif

#undef __TIMENAMS
#undef __TIMENAM
#else

__IMP __WUNUSED double (difftime)(time_t __time1, time_t __time2);
__IMP __WUNUSED char *(ctime)(time_t const *__timep);
__IMP __WUNUSED struct tm *(gmtime)(time_t const *__timep);
__IMP __WUNUSED struct tm *(localtime)(time_t const *__timep);
__IMP __WUNUSED time_t (mktime)(struct tm *__tm);
__IMP __WUNUSED time_t (time)(time_t *__timep);

#ifdef __USE_POSIX
__IMP char *(asctime_r)(const struct tm *, char *);
__IMP char *(ctime_r)(const time_t *, char *);
__IMP struct tm *(gmtime_r)(const time_t *, struct tm *);
__IMP struct tm *(localtime_r)(const time_t *, struct tm *);
#endif

#endif

__IMP __WUNUSED clock_t clock(void);

struct tm {
	int tm_sec;     /* seconds after the minute - [0,59] */
	int tm_min;     /* minutes after the hour - [0,59] */
	int tm_hour;    /* hours since midnight - [0,23] */
	int tm_mday;    /* day of the month - [1,31] */
	int tm_mon;     /* months since January - [0,11] */
	int tm_year;    /* years since 1900 */
	int tm_wday;    /* days since Sunday - [0,6] */
	int tm_yday;    /* days since January 1 - [0,365] */
	int tm_isdst;   /* daylight savings time flag */
#ifdef __CRT_GLIBC
#ifdef __USE_MISC
	long int    tm_gmtoff;   /* Seconds east of UTC.  */
	char const *tm_zone;     /* Timezone abbreviation.  */
#else
	long int    __tm_gmtoff; /* Seconds east of UTC.  */
	char const *__tm_zone;   /* Timezone abbreviation.  */
#endif
#endif
};

__IMP __WUNUSED char *(asctime)(struct tm const *);
__IMP size_t (strftime)(char *,size_t,char const *,struct tm const *);


#endif
