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

#include "__stdinc.h"

#undef size_t
typedef __SIZE_TYPE__ size_t;

#define NULL  __NULL__

struct tm;

#if defined(_WIN32) || defined(__CYGWIN32__)
typedef __int32 clock_t;
#define CLOCKS_PER_SEC  1000

#ifdef _USE_32BIT_TIME_T
#define __TIMENAM(n) __asm__("_" n "32")
typedef __int32 time_t;
#else
#define __TIMENAM(n) __asm__("_" n "64")
typedef __int64 time_t;
#endif

__IMP __WUNUSED double (difftime)(time_t,time_t) __TIMENAM("difftime");
__IMP __WUNUSED time_t (time)(time_t *) __TIMENAM("time");
__IMP __WUNUSED time_t (mktime)(struct tm *) __TIMENAM("mktime");

#undef __TIMENAM
#else
#error FIXME
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
};

__IMP __WUNUSED char *(asctime)(struct tm const *);
__IMP __WUNUSED char *(ctime)(time_t const *);
__IMP __WUNUSED struct tm *(gmtime)(time_t const *);
__IMP __WUNUSED struct tm *(localtime)(time_t const *);
__IMP size_t (strftime)(char *,size_t,char const *,struct tm const *);
