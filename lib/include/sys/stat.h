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

#if __has_include_next(<sys/stat.h>)
#include_next <sys/stat.h>
#else
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#include <features.h>
#include <bits/types.h>

#if defined(__USE_XOPEN) || defined(__USE_XOPEN2K)
typedef __time_t time_t;
#endif

#ifdef __USE_ATFILE
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

#include <bits/stat.h>

#if defined(__USE_MISC) || defined(__USE_XOPEN)
#	define S_IFMT      __S_IFMT
#	define S_IFDIR     __S_IFDIR
#	define S_IFCHR     __S_IFCHR
#	define S_IFBLK     __S_IFBLK
#	define S_IFREG     __S_IFREG
#ifdef __S_IFIFO
#	define S_IFIFO  __S_IFIFO
#endif
#ifdef __S_IFLNK
#	define S_IFLNK  __S_IFLNK
#endif
#if defined(__S_IFSOCK) && \
   (defined(__USE_MISC) || defined(__USE_UNIX98))
#	define S_IFSOCK __S_IFSOCK
#endif
#endif

#	define __S_ISTYPE(mode,mask) (((mode)&__S_IFMT) == (mask))
#	define S_ISDIR(mode)     __S_ISTYPE((mode),__S_IFDIR)
#	define S_ISCHR(mode)     __S_ISTYPE((mode),__S_IFCHR)
#	define S_ISBLK(mode)     __S_ISTYPE((mode),__S_IFBLK)
#	define S_ISREG(mode)     __S_ISTYPE((mode),__S_IFREG)
#ifdef __S_IFIFO
#	define S_ISFIFO(mode) __S_ISTYPE((mode),__S_IFIFO)
#endif
#ifdef __S_IFLNK
#	define S_ISLNK(mode)  __S_ISTYPE((mode),__S_IFLNK)
#endif

#if defined(__USE_MISC) && !defined(__S_IFLNK)
#	define S_ISLNK(mode)  0
#endif
#if defined(__S_IFSOCK) && \
   (defined(__USE_UNIX98) || defined(__USE_XOPEN2K))
#	define S_ISSOCK(mode) __S_ISTYPE((mode),__S_IFSOCK)
#elif defined(__USE_XOPEN2K)
#	define S_ISSOCK(mode) 0
#endif

#ifdef __USE_POSIX199309
#	define S_TYPEISMQ(buf)  __S_TYPEISMQ(buf)
#	define S_TYPEISSEM(buf) __S_TYPEISSEM(buf)
#	define S_TYPEISSHM(buf) __S_TYPEISSHM(buf)
#endif

#	define S_ISUID __S_ISUID
#	define S_ISGID    __S_ISGID
#if defined(__USE_MISC) || defined(__USE_XOPEN)
#	define S_ISVTX __S_ISVTX
#endif

#	define S_IRUSR  __S_IREAD
#	define S_IWUSR  __S_IWRITE
#	define S_IXUSR  __S_IEXEC
#	define S_IRWXU (__S_IREAD|__S_IWRITE|__S_IEXEC)

#ifdef __USE_MISC
#	define S_IREAD  S_IRUSR
#	define S_IWRITE S_IWUSR
#	define S_IEXEC  S_IXUSR
#endif

#	define S_IRGRP  (S_IRUSR >> 3)
#	define S_IWGRP  (S_IWUSR >> 3)
#	define S_IXGRP  (S_IXUSR >> 3)
#	define S_IRWXG  (S_IRWXU >> 3)
#	define S_IROTH  (S_IRGRP >> 3)
#	define S_IWOTH  (S_IWGRP >> 3)
#	define S_IXOTH  (S_IXGRP >> 3)
#	define S_IRWXO  (S_IRWXG >> 3)

#ifdef    __USE_MISC
#	define ACCESSPERMS (S_IRWXU|S_IRWXG|S_IRWXO)
#	define ALLPERMS    (S_ISUID|S_ISGID|S_ISVTX|S_IRWXU|S_IRWXG|S_IRWXO)
#	define DEFFILEMODE (S_IRUSR|S_IWUSR|S_IRGRP|S_IWGRP|S_IROTH|S_IWOTH)
#	define S_BLKSIZE    512
#endif

#ifdef __CRT_MSVC
#	define __STAT_FUN(name)   __asm__("_" name)
#	define __STAT_FUN(name)   __asm__("_" name)
#ifdef __USE_FILE_OFFSET64
#	define __STAT_FUN64(name) __asm__("_" name "64")
#	define __STAT_FUN32(name) __asm__("_" name "64")
#else
#	define __STAT_FUN64(name) __asm__("_" name "64")
#	define __STAT_FUN32(name) __asm__("_" name "32")
#endif

__IMP int (stat)(char const *__restrict __file, struct stat *__restrict __buf) __STAT_FUN32("stat");
__IMP int (fstat)(int __fd, struct stat *__buf) __STAT_FUN32("fstat");
#ifdef __USE_LARGEFILE64
__IMP int (stat64)(char const *__restrict __file, struct stat64 *__restrict __buf) __STAT_FUN64("stat");
__IMP int (fstat64)(int __fd, struct stat64 *__buf) __STAT_FUN64("fstat");
#endif

#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (fstatat)(int __fildes, char const *__filename, struct stat *__stat_buf, int __flag) __STAT_FUN32("__fxstatat");
#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC int (fstatat64)(int __fildes, char const *__filename, struct stat64 *__stat_buf, int __flag) __STAT_FUN64("__fxstatat");
#endif
#endif

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K)
/* While not ~really~ correct, simply re-direct 'lstat' to 'stat'. */
__IMP __CRT_WORKAROUND_MSVC int (lstat)(char const *__restrict __file, struct stat *__restrict __buf) __STAT_FUN32("stat");
#ifdef __USE_LARGEFILE64
__IMP __CRT_WORKAROUND_MSVC int (lstat64)(char const *__restrict __file, struct stat64 *__restrict __buf) __STAT_FUN64("stat");
#endif
#endif

#if defined __USE_MISC || defined __USE_XOPEN_EXTENDED
__IMP __CRT_UNSUPPORTED_MSVC int (mknod)(int __ver, char const *__path, __mode_t __mode, __dev_t *__dev) __STAT_FUN("__xmknod");
#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (mknodat)(int __ver, int __fd, char const *__path, __mode_t __mode, __dev_t *__dev) __STAT_FUN("__xmknodat");
#endif
#endif

#undef __STAT_FUN32
#undef __STAT_FUN64
#else
#	define __STAT_FUN(name)   /* nothing */
#	define __STAT_FUN64(name) /* nothing */
#ifdef __USE_FILE_OFFSET64
#	define __STAT_FUN32(name) __asm__(name "64")
#else
#	define __STAT_FUN32(name) /* nothing */
#endif

#ifndef _STAT_VER
#define _STAT_VER  0
#endif
#ifndef _MKNOD_VER
#define _MKNOD_VER 0
#endif

__IMP int (__fxstat)(int __ver, int __fildes, struct stat *__stat_buf) __STAT_FUN32("__fxstat");
__IMP int (__xstat)(int __ver, char const *__filename, struct stat *__stat_buf) __STAT_FUN32("__xstat");
__IMP int (__lxstat)(int __ver, char const *__filename, struct stat *__stat_buf) __STAT_FUN32("__lxstat");
__IMP int (__fxstatat)(int __ver, int __fildes, char const *__filename, struct stat *__stat_buf, int __flag) __STAT_FUN32("__fxstatat");
#ifdef __USE_LARGEFILE64
__IMP int (__fxstat64)(int __ver, int __fildes, struct stat64 *__stat_buf) __STAT_FUN64("__fxstat");
__IMP int (__xstat64)(int __ver, char const *__filename, struct stat64 *__stat_buf) __STAT_FUN64("__xstat");
__IMP int (__lxstat64)(int __ver, char const *__filename, struct stat64 *__stat_buf) __STAT_FUN64("__lxstat");
__IMP int (__fxstatat64)(int __ver, int __fildes, char const *__filename, struct stat64 *__stat_buf, int __flag) __STAT_FUN64("__fxstatat");
#endif
__IMP int (__xmknod)(int __ver, char const *__path, __mode_t __mode, __dev_t *__dev) __STAT_FUN("__xmknod");
__IMP int (__xmknodat)(int __ver, int __fd, char const *__path, __mode_t __mode, __dev_t *__dev) __STAT_FUN("__xmknodat");

#	define stat(path,statbuf)   __xstat(_STAT_VER,(path),(statbuf))
#	define fstat(fd,statbuf)    __fxstat(_STAT_VER,(fd),(statbuf))
#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
#	define lstat(path,statbuf)) __lxstat(_STAT_VER,(path),(statbuf))
#endif
#ifdef __USE_MISC
#	define mknod(path,mode,dev) __extension__({ __dev_t __d = (dev); __xmknod(_MKNOD_VER,path,mode,&__d); })
#endif
#ifdef __USE_ATFILE
#	define fstatat(fd,filename,statbuf,flag) __fxstatat(_STAT_VER,(fd),(filename),(statbuf),(flag))
#	define mknodat(fd,path,mode,dev) __extension__({ __dev_t __d = (dev); __xmknodat(_MKNOD_VER,fd,path,mode,&__d); })
#endif

#ifdef __USE_LARGEFILE64
#	define stat64(path,statbuf) __xstat64(_STAT_VER,(path),(statbuf))
#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
#	define lstat64(path,statbuf) __lxstat64(_STAT_VER,(path),(statbuf))
#endif
#	define fstat64(fd,statbuf) __fxstat64(_STAT_VER,(fd),(statbuf))
#ifdef __USE_ATFILE
#	define fstatat64(fd,filename,statbuf,flag) __fxstatat64(_STAT_VER,(fd),(filename),(statbuf),(flag))
#endif
#endif /* __USE_LARGEFILE64 */

#undef __STAT_FUN32
#undef __STAT_FUN64
#endif /* CRT... */

__IMP int (chmod)(char const *__file, __mode_t __mode) __STAT_FUN("chmod");
#ifdef __USE_MISC
__IMP __CRT_WORKAROUND_MSVC int (lchmod)(char const *__file, __mode_t __mode)
#ifdef __CRT_MSVC
	__STAT_FUN("chmod")
#else
	__STAT_FUN("lchmod")
#endif
;
#endif

#ifdef __USE_POSIX
__IMP __CRT_UNSUPPORTED_MSVC int (fchmod)(int __fd, __mode_t __mode) __STAT_FUN("fchmod");
#endif

#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (fchmodat)(int __fd, char const *__file, __mode_t __mode, int __flag) __STAT_FUN("fchmodat");
#endif

__IMP __mode_t (umask)(__mode_t __mask) __STAT_FUN("umask");

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC __mode_t (getumask)(void);
#endif

#ifdef __CRT_MSVC
__IMP int (__msvc_mkdir)(char const *__path) __asm__("_mkdir");
/* Ignore all additional arguments. */
#	define mkdir(path,...) (__builtin_noop(__VA_ARGS__),__msvc_mkdir(path))
#else
__IMP int (mkdir)(char const *__path, __mode_t __mode) __STAT_FUN("mkdir");
#endif

#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (mkdirat)(int __fd, char const *__path, __mode_t __mode) __STAT_FUN("mkdirat");
#endif

__IMP __CRT_UNSUPPORTED_MSVC int (mkfifo)(char const *__path, __mode_t __mode) __STAT_FUN("mkfifo");
#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (mkfifoat)(int __fd, char const *__path, __mode_t __mode) __STAT_FUN("mkfifoat");
#endif
 
#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (utimensat)(int __fd, char const *__path, struct timespec const __times[2], int __flags) __STAT_FUN("utimensat");
#endif

#ifdef __USE_XOPEN2K8
__IMP __CRT_UNSUPPORTED_MSVC int (futimens)(int __fd, struct timespec const __times[2]) __STAT_FUN("futimens");
#endif

#undef __STAT_FUN
#endif
