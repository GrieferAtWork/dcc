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

#if __has_include_next(<bits/types.h>)
#include_next <bits/types.h>
#else

#include <features.h>
#include <__stdinc.h>

/* Convenience types. */
typedef unsigned char      __u_char;
typedef unsigned short int __u_short;
typedef unsigned int       __u_int;
typedef unsigned long int  __u_long;

/* Fixed-size types, underlying types depend on word size and compiler. */
typedef __INT8_TYPE__   __int8_t;
typedef __UINT8_TYPE__  __uint8_t;
typedef __INT16_TYPE__  __int16_t;
typedef __UINT16_TYPE__ __uint16_t;
typedef __INT32_TYPE__  __int32_t;
typedef __UINT32_TYPE__ __uint32_t;
typedef __INT64_TYPE__  __int64_t;
typedef __UINT64_TYPE__ __uint64_t;
typedef __INT64_TYPE__  __quad_t;
typedef __UINT64_TYPE__ __u_quad_t;

#undef  __ssize_t
typedef __SSIZE_TYPE__  __ssize_t;
#define __ssize_t       __ssize_t

#undef  __qaddr_t
typedef __quad_t        __qaddr_t;
#define __qaddr_t       __qaddr_t

#undef  __caddr_t
typedef char           *__caddr_t;
#define __caddr_t       __caddr_t

#undef  __intptr_t
typedef __INTPTR_TYPE__ __intptr_t;
#define __intptr_t      __intptr_t

#ifdef __CRT_MSVC
/* MSVC-specific type definitions. */

#undef  __time32_t
typedef __INT32_TYPE__  __time32_t;
#define __time32_t      __time32_t

#undef  __time64_t
typedef __INT64_TYPE__  __time64_t;
#define __time64_t      __time64_t

#ifdef _USE_32BIT_TIME_T
typedef __time32_t      __time_t;
#else
typedef __time64_t      __time_t;
#endif
#define __time_t        __time_t

#undef  __pid_t
typedef __INT32_TYPE__  __pid_t;
#define __pid_t         __pid_t

#undef  __clock_t
typedef __INT32_TYPE__  __clock_t;
#define __clock_t       __clock_t

#undef  __uid_t
typedef __INT16_TYPE__  __uid_t;
#define __uid_t         __uid_t

#undef  __gid_t
typedef __INT16_TYPE__  __gid_t;
#define __gid_t         __gid_t

#undef  __nlink_t
typedef __INT16_TYPE__  __nlink_t;
#define __nlink_t       __nlink_t

#undef  __mode_t
typedef __UINT16_TYPE__ __mode_t;
#define __mode_t        __mode_t

#undef  __loff_t
typedef __MS_LONG       __loff_t;
#define __loff_t        __loff_t

#undef  __dev_t
typedef __UINT32_TYPE__ __dev_t;
#define __dev_t         __dev_t

#undef  __ino_t
typedef __INT32_TYPE__  __ino_t;
#define __ino_t         __ino_t

#undef  __ino64_t
typedef __INT32_TYPE__  __ino64_t; /* Yes, this is 32-bit! */
#define __ino64_t       __ino64_t

#undef  __off_t
typedef __MS_LONG       __off_t;
#define __off_t         __off_t

#undef  __off64_t
typedef __INT64_TYPE__  __off64_t;
#define __off64_t       __off64_t

#undef  __socklen_t
typedef __INT32_TYPE__  __socklen_t;
#define __socklen_t     __socklen_t

/* UNSUPPORTED TYPES */
#undef  __fsid_t
typedef struct { int __val[2]; } __fsid_t;
#define __fsid_t          __fsid_t
#undef  __rlim_t
typedef __UINTPTR_TYPE__ __rlim_t;
#define __rlim_t         __rlim_t
#undef  __rlim64_t
typedef __UINT64_TYPE__  __rlim64_t;
#define __rlim64_t       __rlim64_t
#undef  __id_t
typedef __UINT32_TYPE__  __id_t;
#define __id_t           __id_t
#undef  __useconds_t
typedef __UINT32_TYPE__  __useconds_t;
#define __useconds_t     __useconds_t
#undef  __suseconds_t
typedef __INTPTR_TYPE__  __suseconds_t;
#define __suseconds_t    __suseconds_t
#undef  __daddr_t
typedef __INT32_TYPE__   __daddr_t;
#define __daddr_t        __daddr_t
#undef  __key_t
typedef __INT32_TYPE__   __key_t;
#define __key_t          __key_t
#undef  __clockid_t
typedef __INT32_TYPE__   __clockid_t;
#define __clockid_t      __clockid_t
#undef  __timer_t
typedef void            *__timer_t;
#define __timer_t        __timer_t
#undef  __syscall_slong_t
typedef __INTPTR_TYPE__   __syscall_slong_t;
#define __syscall_slong_t __syscall_slong_t
#undef  __syscall_ulong_t
typedef __UINTPTR_TYPE__  __syscall_ulong_t;
#define __syscall_ulong_t __syscall_ulong_t
#undef  __fsword_t
typedef __INTPTR_TYPE__  __fsword_t;
#define __fsword_t       __fsword_t
#undef  __blksize_t
typedef __INTPTR_TYPE__  __blksize_t;
#define __blksize_t      __blksize_t
#undef  __blkcnt_t
typedef __INTPTR_TYPE__  __blkcnt_t;
#define __blkcnt_t       __blkcnt_t
#undef  __blkcnt64_t
typedef __INT64_TYPE__   __blkcnt64_t;
#define __blkcnt64_t     __blkcnt64_t
#undef  __fsblkcnt_t
typedef __UINTPTR_TYPE__ __fsblkcnt_t;
#define __fsblkcnt_t     __fsblkcnt_t
#undef  __fsblkcnt64_t
typedef __UINT64_TYPE__  __fsblkcnt64_t;
#define __fsblkcnt64_t   __fsblkcnt64_t
#undef  __fsfilcnt_t
typedef __UINTPTR_TYPE__ __fsfilcnt_t;
#define __fsfilcnt_t     __fsfilcnt_t
#undef  __fsfilcnt64_t
typedef __UINT64_TYPE__  __fsfilcnt64_t;
#define __fsfilcnt64_t   __fsfilcnt64_t

#else

#if defined(__x86_64__) && defined(__ILP32__)
#define __SYSCALL_SLONG_TYPE  __INT64_TYPE__
#define __SYSCALL_ULONG_TYPE  __UINT64_TYPE__
#else
#define __SYSCALL_SLONG_TYPE  __INTPTR_TYPE__
#define __SYSCALL_ULONG_TYPE  __UINTPTR_TYPE__
#endif

#undef  __fsid_t
typedef struct { int __val[2]; } __fsid_t;
#define __fsid_t          __fsid_t

#undef  __rlim_t
typedef __SYSCALL_ULONG_TYPE  __rlim_t;
#define __rlim_t              __rlim_t

#undef  __rlim64_t
typedef __UINT64_TYPE__       __rlim64_t;
#define __rlim64_t            __rlim64_t

#undef  __id_t
typedef __UINT32_TYPE__       __id_t;
#define __id_t                __id_t

#undef  __useconds_t
typedef __UINT32_TYPE__       __useconds_t;
#define __useconds_t          __useconds_t

#undef  __suseconds_t
typedef __SYSCALL_SLONG_TYPE  __suseconds_t;
#define __suseconds_t         __suseconds_t

#undef  __daddr_t
typedef __INT32_TYPE__        __daddr_t;
#define __daddr_t             __daddr_t

#undef  __key_t
typedef __INT32_TYPE__        __key_t;
#define __key_t               __key_t

#undef  __clockid_t
typedef __INT32_TYPE__        __clockid_t;
#define __clockid_t           __clockid_t

#undef  __timer_t
typedef void                 *__timer_t;
#define __timer_t             __timer_t

#undef  __syscall_slong_t
typedef __SYSCALL_SLONG_TYPE __syscall_slong_t;
#define __syscall_slong_t    __syscall_slong_t

#undef  __syscall_ulong_t
typedef __SYSCALL_ULONG_TYPE __syscall_ulong_t;
#define __syscall_ulong_t    __syscall_ulong_t

#undef  __pid_t
typedef __INT32_TYPE__       __pid_t;
#define __pid_t              __pid_t

#undef  __clock_t
typedef __SYSCALL_SLONG_TYPE __clock_t;
#define __clock_t            __clock_t

#undef  __time_t
typedef __SYSCALL_SLONG_TYPE __time_t;
#define __time_t             __time_t

#undef  __uid_t
typedef __UINT32_TYPE__      __uid_t;
#define __uid_t              __uid_t

#undef  __gid_t
typedef __UINT32_TYPE__      __gid_t;
#define __gid_t              __gid_t

#undef  __nlink_t
#undef  __fsword_t
#ifdef __x86_64__
typedef __SYSCALL_ULONG_TYPE __nlink_t;
typedef __SYSCALL_SLONG_TYPE __fsword_t;
#else
typedef __UINTPTR_TYPE__     __nlink_t;
typedef __INTPTR_TYPE__      __fsword_t;
#endif
#define __nlink_t            __nlink_t
#define __fsword_t           __fsword_t

#undef  __dev_t
typedef __UINT64_TYPE__      __dev_t;
#define __dev_t              __dev_t

#undef  __mode_t
typedef __UINT32_TYPE__      __mode_t;
#define __mode_t             __mode_t

#undef  __ino_t
typedef __SYSCALL_ULONG_TYPE __ino_t;
#define __ino_t              __ino_t

#undef  __ino64_t
typedef __UINT64_TYPE__      __ino64_t;
#define __ino64_t            __ino64_t

#undef  __off_t
typedef __SYSCALL_SLONG_TYPE __off_t;
#define __off_t              __off_t

#undef  __off64_t
typedef __INT64_TYPE__       __off64_t;
#define __off64_t            __off64_t

#undef  __loff_t
typedef __off64_t            __loff_t;
#define __loff_t             __loff_t

#undef  __blksize_t
typedef __SYSCALL_SLONG_TYPE __blksize_t;
#define __blksize_t          __blksize_t

#undef  __blkcnt_t
typedef __SYSCALL_SLONG_TYPE __blkcnt_t;
#define __blkcnt_t           __blkcnt_t

#undef  __blkcnt64_t
typedef __INT64_TYPE__       __blkcnt64_t;
#define __blkcnt64_t         __blkcnt64_t

#undef  __fsblkcnt_t
typedef __SYSCALL_ULONG_TYPE __fsblkcnt_t;
#define __fsblkcnt_t         __fsblkcnt_t

#undef  __fsblkcnt64_t
typedef __UINT64_TYPE__      __fsblkcnt64_t;
#define __fsblkcnt64_t       __fsblkcnt64_t

#undef  __fsfilcnt_t
typedef __SYSCALL_ULONG_TYPE __fsfilcnt_t;
#define __fsfilcnt_t         __fsfilcnt_t

#undef  __fsfilcnt64_t
typedef __UINT64_TYPE__      __fsfilcnt64_t;
#define __fsfilcnt64_t       __fsfilcnt64_t

#undef  __socklen_t
typedef __UINT32_TYPE__      __socklen_t;
#define __socklen_t          __socklen_t

#undef __SYSCALL_SLONG_TYPE
#undef __SYSCALL_ULONG_TYPE
#endif
#endif
