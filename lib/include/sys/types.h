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

#if __has_include_next(<sys/types.h>)
#include_next <sys/types.h>
#else
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#include <features.h>
#include <bits/types.h>

#ifdef __USE_MISC
typedef __u_char   u_char;
typedef __u_short  u_short;
typedef __u_int    u_int;
typedef __u_long   u_long;
typedef __quad_t   quad_t;
typedef __u_quad_t u_quad_t;
#ifdef __fsid_t
typedef __fsid_t   fsid_t;
#endif
#endif
typedef __loff_t   loff_t;

typedef __dev_t   dev_t;
typedef __gid_t   gid_t;
typedef __mode_t  mode_t;
typedef __nlink_t nlink_t;
typedef __uid_t   uid_t;
typedef __pid_t   pid_t;
typedef __ssize_t ssize_t;

#ifndef __USE_FILE_OFFSET64
typedef __ino_t   ino_t;
typedef __off_t   off_t;
#else
typedef __ino64_t ino_t;
typedef __off64_t off_t;
#endif

#ifdef __USE_LARGEFILE64
typedef __ino64_t ino64_t;
typedef __off64_t off64_t;
#endif

#if defined(__id_t) && \
   (defined(__USE_XOPEN) || defined(__USE_XOPEN2K8))
typedef __id_t id_t;
#endif


#ifdef __USE_MISC
#ifdef __daddr_t
typedef __daddr_t daddr_t;
#endif
typedef __caddr_t caddr_t;
#endif

#if defined(__key_t) && \
   (defined(__USE_MISC) || defined(__USE_XOPEN))
typedef __key_t key_t;
#endif

#if defined(__USE_XOPEN) || defined(__USE_XOPEN2K8)
typedef __clock_t clock_t;
#endif

typedef __time_t    time_t;
#ifdef __timer_t
typedef __timer_t   timer_t;
#endif
#ifdef __clockid_t
typedef __clockid_t clockid_t;
#endif

#ifdef __USE_XOPEN
#ifdef __useconds_t
typedef __useconds_t  useconds_t;
#endif
#ifdef __suseconds_t
typedef __suseconds_t suseconds_t;
#endif
#endif

typedef __SIZE_TYPE__ size_t;

#ifdef __USE_MISC
typedef unsigned long int  ulong;
typedef unsigned short int ushort;
typedef unsigned int       uint;
#endif

typedef __INT8_TYPE__   int8_t;
typedef __INT16_TYPE__  int16_t;
typedef __INT32_TYPE__  int32_t;
typedef __INT64_TYPE__  int64_t;
typedef __UINT8_TYPE__  u_int8_t;
typedef __UINT16_TYPE__ u_int16_t;
typedef __UINT32_TYPE__ u_int32_t;
typedef __UINT64_TYPE__ u_int64_t;

typedef int register_t;

#define __BIT_TYPES_DEFINED__ 1

#ifdef __USE_MISC
#if __has_include(<endian.h>)
#	include <endian.h>
#endif
#if __has_include(<sys/select.h>)
#	include <sys/select.h>
#endif
#if __has_include(<sys/sysmacros.h>)
#	include <sys/sysmacros.h>
#endif
#endif /* Use misc.  */


#if defined(__blksize_t) && \
   (defined(__USE_UNIX98) || defined(__USE_XOPEN2K8))
typedef __blksize_t blksize_t;
#endif

#if defined(__blkcnt_t) && defined(__fsblkcnt_t) && defined(__fsfilcnt_t)
#ifndef __USE_FILE_OFFSET64
typedef __blkcnt_t   blkcnt_t;
typedef __fsblkcnt_t fsblkcnt_t;
typedef __fsfilcnt_t fsfilcnt_t;
#else
typedef __blkcnt64_t   blkcnt_t;
typedef __fsblkcnt64_t fsblkcnt_t;
typedef __fsfilcnt64_t fsfilcnt_t;
#endif
#ifdef __USE_LARGEFILE64
typedef __blkcnt64_t   blkcnt64_t;
typedef __fsblkcnt64_t fsblkcnt64_t;
typedef __fsfilcnt64_t fsfilcnt64_t;
#endif
#endif

#if (defined(__USE_POSIX199506) || defined(__USE_UNIX98)) && \
     __has_include(<bits/pthreadtypes.h>)
#	include <bits/pthreadtypes.h>
#endif

#endif
