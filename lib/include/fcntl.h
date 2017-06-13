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

#if __has_include_next(<fcntl.h>)
#include_next <fcntl.h>
#else /* include_next... */
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#include <bits/types.h>
#include <bits/fcntl.h>

#ifdef __CRT_MSVC
#   define __FCNTL_FUN(x) __asm__("_" x)
#else
#   define __FCNTL_FUN(x)
#endif

#ifdef __O_TMPFILE
#   define __OPEN_NEEDS_MODE(oflag) (((oflag) & O_CREAT) != 0 || ((oflag) & __O_TMPFILE) == __O_TMPFILE)
#else
#   define __OPEN_NEEDS_MODE(oflag) (((oflag) & O_CREAT) != 0)
#endif

typedef __mode_t  mode_t;
#ifndef __USE_FILE_OFFSET64
typedef __off_t   off_t;
#else
typedef __off64_t off_t;
#endif
#ifdef __USE_LARGEFILE64
typedef __off64_t off64_t;
#endif
typedef __pid_t   pid_t;

#if defined(__USE_XOPEN) || \
    defined(__USE_XOPEN2K8)
#include <bits/stat.h>

typedef __time_t time_t;
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

#define S_IFMT      __S_IFMT
#define S_IFDIR     __S_IFDIR
#define S_IFCHR     __S_IFCHR
#define S_IFBLK     __S_IFBLK
#define S_IFREG     __S_IFREG
#ifdef __S_IFIFO
#   define S_IFIFO  __S_IFIFO
#endif
#ifdef __S_IFLNK
#   define S_IFLNK  __S_IFLNK
#endif
#if defined(__S_IFSOCK) && \
   (defined(__USE_UNIX98) || defined(__USE_XOPEN2K8))
#   define S_IFSOCK __S_IFSOCK
#endif

#define S_ISUID    __S_ISUID
#define S_ISGID    __S_ISGID
#if defined(__USE_MISC) || defined(__USE_XOPEN)
#   define S_ISVTX __S_ISVTX
#endif
#define S_IRUSR    __S_IREAD
#define S_IWUSR    __S_IWRITE
#define S_IXUSR    __S_IEXEC
#define S_IRWXU   (__S_IREAD|__S_IWRITE|__S_IEXEC)
#define S_IRGRP   (S_IRUSR >> 3)
#define S_IWGRP   (S_IWUSR >> 3)
#define S_IXGRP   (S_IXUSR >> 3)
#define S_IRWXG   (S_IRWXU >> 3)
#define S_IROTH   (S_IRGRP >> 3)
#define S_IWOTH   (S_IWGRP >> 3)
#define S_IXOTH   (S_IXGRP >> 3)
#define S_IRWXO   (S_IRWXG >> 3)
#endif
#ifdef	__USE_MISC
#   define R_OK 4
#   define W_OK 2
#   define X_OK 1
#   define F_OK 0
#endif
#if defined(__USE_XOPEN) || defined(__USE_XOPEN2K8)
#   define SEEK_SET	0
#   define SEEK_CUR	1
#   define SEEK_END	2
#endif

__IMP __CRT_UNSUPPORTED_MSVC int (fcntl)(int __fd, int __cmd, ...);

__IMP __WUNUSED int (open)(const char *__file, int __oflag, ...)
#ifdef __CRT_MSVC
    __asm__("_open")
#elif defined(__USE_FILE_OFFSET64) && !defined(__CRT_KOS)
    __FCNTL_FUN("open64")
#else
    __FCNTL_FUN("open")
#endif
;

#ifdef __USE_LARGEFILE64
__IMP __WUNUSED int (open64)(const char *__file, int __oflag, ...)
#ifdef __CRT_MSVC
    __asm__("_open")
#elif defined(__CRT_KOS)
    __asm__("open")
#else
    __FCNTL_FUN("open64")
#endif
;
#endif

#ifdef __USE_ATFILE
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC
int (openat)(int __fd, const char *__file, int __oflag, ...)
#if defined(__USE_FILE_OFFSET64) && !defined(__CRT_KOS)
    __FCNTL_FUN("openat64")
#else
    __FCNTL_FUN("openat")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC
int (openat64)(int __fd, const char *__file, int __oflag, ...)
#ifdef __CRT_KOS
    __FCNTL_FUN("openat")
#endif
;

#endif
#endif

__IMP __WUNUSED int (creat)(const char *__file, mode_t __mode)
#ifdef __CRT_MSVC
    __asm__("_creat")
#elif defined(__USE_FILE_OFFSET64) && !defined(__CRT_KOS)
    __asm__("creat64")
#endif
;

#ifdef __USE_LARGEFILE64
__IMP __WUNUSED int (creat64)(const char *__file, mode_t __mode)
#ifdef __CRT_MSVC
    __asm__("_creat")
#elif defined(__CRT_KOS)
    __asm__("creat")
#endif
;
#endif

#if !defined(F_ULOCK) && (defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED))
#define F_ULOCK 0 /* Unlock a previously locked region.  */
#define F_LOCK  1 /* Lock a region for exclusive use.  */
#ifndef __CRT_MSVC
#define F_TLOCK 2 /* Test and lock a region for exclusive use.  */
#define F_TEST  3 /* Test a region for other processes locks.  */
#endif

#ifdef __CRT_MSVC
__IMP int __msvc_locking(int __fd, int __cmd, __UINT32_TYPE__ __len) __asm__("_locking");
#define lockf(fd,cmd,len)    __msvc_locking((fd),(cmd),(__UINT32_TYPE__)(len))
#ifdef __USE_LARGEFILE64
#define lockf64(fd,cmd,len) __msvc_locking((fd),(cmd),(__UINT32_TYPE__)(len))
#endif
#else /* __CRT_MSVC */
__IMP int (lockf)(int __fd, int __cmd, off_t __len) __UNISTD_FUN32("lockf");
#ifdef __USE_LARGEFILE64
__IMP int (lockf64)(int __fd, int __cmd, off64_t __len) __UNISTD_FUN64("lockf");
#endif
#endif /* !__CRT_MSVC */
#endif

#ifdef __USE_XOPEN2K
#ifdef __CRT_GLIBC
__IMP int (posix_fadvise)(int __fd, off_t __offset, off_t __len, int __advise)
#ifdef __USE_FILE_OFFSET64
    __asm__("posix_fadvise64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP int (posix_fadvise64)(int __fd, off64_t __offset, off64_t __len, int __advise);
#endif
extern int (posix_fallocate)(int __fd, off_t __offset, off_t __len)
#ifdef __USE_FILE_OFFSET64
    __asm__("posix_fallocate64")
#endif
;
# ifdef __USE_LARGEFILE64
extern int (posix_fallocate64)(int __fd, off64_t __offset, off64_t __len);
# endif
#else /* __CRT_GLIBC */
#   define posix_fadvise(fd,offset,len,advise)   ((fd),(offset),(len),(advise),0)
#   define posix_fallocate(fd,offset,len)        ((fd),(offset),(len),0)
#   pragma deprecated(posix_fadvise)
#   pragma deprecated(posix_fallocate)
#ifdef __USE_LARGEFILE64
#   define posix_fadvise64(fd,offset,len,advise) ((fd),(offset),(len),(advise),0)
#   define posix_fallocate64(fd,offset,len)      ((fd),(offset),(len),0)
#   pragma deprecated(posix_fadvise64)
#   pragma deprecated(posix_fallocate64)
#endif
#endif /* !__CRT_GLIBC */
#endif /* __USE_XOPEN2K */

#if defined(__USE_FORTIFY_LEVEL) && \
  ((__USE_FORTIFY_LEVEL-0) > 0) && \
    defined(__fortify_function) && \
    defined(__va_arg_pack_len) && \
    __has_include(<bits/fcntl2.h>)
#include <bits/fcntl2.h>
#endif


#undef __FCNTL_FUN
#endif /* !include_next... */
