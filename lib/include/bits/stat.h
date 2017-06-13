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

#if __has_include_next(<bits/stat.h>)
#include_next <bits/stat.h>
#else

#include <__stdinc.h>
#include <bits/types.h>

#ifndef __CRT_MSVC
#ifndef __timespec_defined
#define __timespec_defined 1
struct timespec {
 time_t            tv_sec;
#ifdef __CRT_MSVC
 __MS_LONG         tv_nsec;
#else
 __syscall_slong_t tv_nsec;
#endif
};
#endif /* !__timespec_defined */
#endif

struct stat {
#ifdef __CRT_MSVC
 __dev_t    st_dev;
 __ino_t    st_ino;
 __mode_t   st_mode;
 __nlink_t  st_nlink;
 __uid_t    st_uid;
 __gid_t    st_gid;
 __dev_t    st_rdev;
#ifdef __USE_FILE_OFFSET64
 /* aka. 'struct _stat64' */
 __off64_t  st_size;
 __time64_t st_atime;
 __time64_t st_mtime;
 __time64_t st_ctime;
#else
 __off_t    st_size;
 __time32_t st_atime;
 __time32_t st_mtime;
 __time32_t st_ctime;
#endif
#else
 __dev_t         st_dev;
#ifndef __x86_64__
 unsigned short int __pad1;
#endif
#if defined __x86_64__ || !defined __USE_FILE_OFFSET64
 __ino_t         st_ino;
#else
 __ino_t         __st_ino;
#endif
#ifndef __x86_64__
 __mode_t        st_mode;
 __nlink_t       st_nlink;
#else
 __nlink_t       st_nlink;
 __mode_t        st_mode;
#endif
 __uid_t         st_uid;
 __gid_t         st_gid;
#ifdef __x86_64__
 int __pad0;
#endif
 __dev_t         st_rdev;
#ifndef __x86_64__
 unsigned short int __pad2;
#endif
#if defined __x86_64__ || !defined __USE_FILE_OFFSET64
 __off_t         st_size;
#else
 __off64_t       st_size;
#endif
 __blksize_t     st_blksize;
#if defined __x86_64__  || !defined __USE_FILE_OFFSET64
 __blkcnt_t      st_blocks;
#else
 __blkcnt64_t    st_blocks;
#endif
#ifdef __USE_XOPEN2K8
 struct timespec st_atim;
 struct timespec st_mtim;
 struct timespec st_ctim;
#define st_atime st_atim.tv_sec
#define st_mtime st_mtim.tv_sec
#define st_ctime st_ctim.tv_sec
#else
 __time_t          st_atime;
 __syscall_ulong_t st_atimensec;
 __time_t          st_mtime;
 __syscall_ulong_t st_mtimensec;
 __time_t          st_ctime;
 __syscall_ulong_t st_ctimensec;
#endif
#ifdef __x86_64__
 __syscall_slong_t __glibc_reserved[3];
#else
#ifndef __USE_FILE_OFFSET64
 unsigned long int __glibc_reserved4;
 unsigned long int __glibc_reserved5;
#else
 __ino64_t         st_ino;
#endif
#endif
#endif
};


#ifdef __USE_LARGEFILE64
struct stat64 {
#ifdef __CRT_MSVC
 /* aka. 'struct _stat64' */
 __dev_t    st_dev;
 __ino_t    st_ino;
 __mode_t   st_mode;
 __nlink_t  st_nlink;
 __uid_t    st_uid;
 __gid_t    st_gid;
 __dev_t    st_rdev;
 __off64_t  st_size;
 __time64_t st_atime;
 __time64_t st_mtime;
 __time64_t st_ctime;
#else
 __dev_t   st_dev;
#ifdef __x86_64__
 __ino64_t st_ino;
 __nlink_t st_nlink;
 __mode_t  st_mode;
#else
 unsigned int __pad1;
 __ino_t   __st_ino;
 __mode_t  st_mode;
 __nlink_t st_nlink;
#endif
 __uid_t   st_uid;
 __gid_t   st_gid;
# ifdef __x86_64__
 int       __pad0;
 __dev_t   st_rdev;
 __off_t   st_size;
# else
 __dev_t   st_rdev;
 unsigned int __pad2;
 __off64_t st_size;
# endif
 __blksize_t  st_blksize;
 __blkcnt64_t st_blocks;
#ifdef __USE_XOPEN2K8
 struct timespec st_atim;
 struct timespec st_mtim;
 struct timespec st_ctim;
#else
 __time_t          st_atime;
 __syscall_ulong_t st_atimensec;
 __time_t          st_mtime;
 __syscall_ulong_t st_mtimensec;
 __time_t          st_ctime;
 __syscall_ulong_t st_ctimensec;
#endif
#ifdef __x86_64__
 __syscall_slong_t __glibc_reserved[3];
#else
 __ino64_t         st_ino;
#endif
#endif
};
#endif

#define _STATBUF_ST_RDEV
#ifndef __CRT_MSVC
#define	_STATBUF_ST_BLKSIZE
#define _STATBUF_ST_NSEC
#endif

#define	__S_IFMT    0170000 /* These bits determine file type.  */
#define	__S_IFDIR   0040000 /* Directory.  */
#define	__S_IFCHR   0020000 /* Character device.  */
#define	__S_IFBLK   0060000 /* Block device.  */
#define	__S_IFREG   0100000 /* Regular file.  */
#define	__S_IFIFO   0010000 /* FIFO.  */
#define	__S_IFLNK   0120000 /* Symbolic link.  */
#define	__S_IFSOCK  0140000 /* Socket.  */
#define __S_TYPEISMQ(buf)  ((buf)->st_mode - (buf)->st_mode)
#define __S_TYPEISSEM(buf) ((buf)->st_mode - (buf)->st_mode)
#define __S_TYPEISSHM(buf) ((buf)->st_mode - (buf)->st_mode)
#define	__S_ISUID   04000   /* Set user ID on execution.  */
#define	__S_ISGID   02000   /* Set group ID on execution.  */
#define	__S_ISVTX   01000   /* Save swapped text after use (sticky).  */
#define	__S_IREAD    0400   /* Read by owner.  */
#define	__S_IWRITE   0200   /* Write by owner.  */
#define	__S_IEXEC    0100   /* Execute by owner.  */

#ifndef __CRT_MSVC
#ifdef __USE_ATFILE
#define UTIME_NOW  ((1l << 30) - 1l)
#define UTIME_OMIT ((1l << 30) - 2l)
#endif
#endif /* !__CRT_MSVC */

#endif
