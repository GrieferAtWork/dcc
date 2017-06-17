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

#if __has_include_next(<unistd.h>)
#include_next <unistd.h>
#else
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#include <features.h>
#include <errno.h>

#ifdef __USE_XOPEN2K8
# define _POSIX_VERSION	200809L
#elif defined __USE_XOPEN2K
# define _POSIX_VERSION	200112L
#elif defined __USE_POSIX199506
# define _POSIX_VERSION	199506L
#elif defined __USE_POSIX199309
# define _POSIX_VERSION	199309L
#else
# define _POSIX_VERSION	199009L
#endif

#ifdef __USE_XOPEN2K8
#	define __POSIX2_THIS_VERSION 200809L
#elif defined __USE_XOPEN2K
#	define __POSIX2_THIS_VERSION 200112L
#elif defined __USE_POSIX199506
#	define __POSIX2_THIS_VERSION 199506L
#else
#	define __POSIX2_THIS_VERSION 199209L
#endif

#define _POSIX2_VERSION     __POSIX2_THIS_VERSION
#define _POSIX2_C_VERSION   __POSIX2_THIS_VERSION
#define _POSIX2_C_BIND      __POSIX2_THIS_VERSION
#define _POSIX2_C_DEV       __POSIX2_THIS_VERSION
#define _POSIX2_SW_DEV      __POSIX2_THIS_VERSION
#define _POSIX2_LOCALEDEF   __POSIX2_THIS_VERSION

#ifdef __USE_XOPEN2K8
#	define _XOPEN_VERSION 700
#elif defined __USE_XOPEN2K
#	define _XOPEN_VERSION 600
#elif defined __USE_UNIX98
#	define _XOPEN_VERSION 500
#else
#	define _XOPEN_VERSION 4
#endif

#define _XOPEN_XCU_VERSION 4
#define _XOPEN_XPG2        1
#define _XOPEN_XPG3        1
#define _XOPEN_XPG4        1
#define _XOPEN_UNIX        1
#define _XOPEN_CRYPT       1
#define _XOPEN_ENH_I18N    1
#define _XOPEN_LEGACY      1

#define STDIN_FILENO   0 /* Standard input. */
#define STDOUT_FILENO  1 /* Standard output. */
#define STDERR_FILENO  2 /* Standard error output. */

#include <bits/types.h>

typedef __SSIZE_TYPE__ ssize_t;
typedef __SIZE_TYPE__  size_t;
#define NULL           __NULL__

#if defined(__USE_XOPEN) || defined(__USE_XOPEN2K)
typedef __gid_t   gid_t;
typedef __uid_t   uid_t;
#ifndef __USE_FILE_OFFSET64
typedef __off_t   off_t;
#else
typedef __off64_t off_t;
#endif
#ifdef __USE_LARGEFILE64
typedef __off64_t off64_t;
#endif
#ifdef __useconds_t
typedef __useconds_t useconds_t;
#endif
typedef __pid_t pid_t;
#endif

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K)
typedef __intptr_t intptr_t;
#endif

#if defined(__USE_MISC) || defined(__USE_XOPEN)
typedef __socklen_t socklen_t;
#endif

#ifdef __CRT_MSVC
/* MSVC doesn't support 64-bit IO (for some reason?)
 * So we link everything against 32-bit. */
#	define __UNISTD_FUN(x)   __asm__("_" x)
#	define __UNISTD_FUN32(x) __asm__("_" x)
#	define __UNISTD_FUN64(x) __asm__("_" x)
#elif defined(__USE_FILE_OFFSET64)
#	define __UNISTD_FUN32(x) __asm__(x "64")
#	define __UNISTD_FUN64(x) __asm__(x "64")
#else
#	define __UNISTD_FUN32(x) /* nothing */
#	define __UNISTD_FUN64(x) __asm__(x "64")
#endif
#ifndef __UNISTD_FUN
#	define __UNISTD_FUN(x) 
#endif


#define	R_OK 4 /* Test for read permission. */
#define	W_OK 2 /* Test for write permission. */
#define	X_OK 1 /* Test for execute permission. */
#define	F_OK 0 /* Test for existence. */
__IMP __WUNUSED int (access)(char const *__name, int __type) __UNISTD_FUN("access");

#ifdef __CRT_MSVC
__IMP __WUNUSED __CRT_WORKAROUND_MSVC int (euidaccess)(char const *__name, int __type) __UNISTD_FUN("access");
__IMP __WUNUSED __CRT_WORKAROUND_MSVC int (eaccess)(char const *__name, int __type) __UNISTD_FUN("access");
#else
__IMP __WUNUSED int (euidaccess)(char const *__name, int __type) __UNISTD_FUN("euidaccess");
__IMP __WUNUSED int (eaccess)(char const *__name, int __type) __UNISTD_FUN("eaccess");
#endif
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC int (faccessat)(int __fd, char const *__file, int __type, int __flag);

#	define SEEK_SET  0 /* Seek from beginning of file.  */
#	define SEEK_CUR  1 /* Seek from current position.  */
#	define SEEK_END  2 /* Seek from end of file.  */
#ifdef __USE_GNU
#	define SEEK_DATA 3 /* Seek to next data.  */
#	define SEEK_HOLE 4 /* Seek to next hole.  */
#ifdef __CRT_MSVC
#	pragma deprecated(SEEK_DATA)
#	pragma deprecated(SEEK_HOLE)
#endif
#endif

#ifdef __USE_MISC
#	define L_SET  SEEK_SET
#	define L_INCR SEEK_CUR
#	define L_XTND SEEK_END
#endif

__IMP off_t (lseek)(int __fd, off_t __offset, int __whence)
#ifdef __CRT_MSVC
#ifdef __USE_FILE_OFFSET64
	__asm__("_lseeki64")
#else
	__asm__("_lseek")
#endif
#else
	__UNISTD_FUN32("lseek")
#endif
;

#ifdef __USE_LARGEFILE64
__IMP off64_t (lseek64)(int __fd, off64_t __offset, int __whence)
#ifdef __CRT_MSVC
	__asm__("_lseeki64")
#else
	__UNISTD_FUN64("lseek")
#endif
;
#endif

__IMP int (close)(int __fd) __UNISTD_FUN("close");

#if !defined(__CRT_MSVC) || (__SIZEOF_SIZE_T__ == 4)
__IMP ssize_t (read)(int __fd, void *__buf, size_t __nbytes) __UNISTD_FUN("read");
__IMP ssize_t (write)(int __fd, void const *__buf, size_t __nbytes) __UNISTD_FUN("write");
#else
__IMP /*__CRT_WORKAROUND_MSVC*/ __INT32_TYPE__ (__msvc_read)(int __fd, void *__buf, __UINT32_TYPE__ __nbytes) __UNISTD_FUN("read");
__IMP /*__CRT_WORKAROUND_MSVC*/ __INT32_TYPE__ (__msvc_write)(int __fd, void const *__buf, __UINT32_TYPE__ __nbytes) __UNISTD_FUN("write");
#define read(fd,buf,nbytes)  ((ssize_t)__msvc_read((fd),(buf),(__UINT32_TYPE__)(nbytes)))
#define write(fd,buf,nbytes) ((ssize_t)__msvc_write((fd),(buf),(__UINT32_TYPE__)(nbytes)))
#endif


#if defined(__USE_UNIX98) || defined(__USE_XOPEN2K8)
__IMP __CRT_UNSUPPORTED_MSVC ssize_t (pread)(int __fd, void *__buf, size_t __nbytes, off_t __offset) __UNISTD_FUN32("pread");
__IMP __CRT_UNSUPPORTED_MSVC ssize_t (pwrite)(int __fd, void const *__buf, size_t __nbytes, off_t __offset) __UNISTD_FUN32("pwrite");
#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC ssize_t (pread64)(int __fd, void *__buf, size_t __nbytes, off64_t __offset) __UNISTD_FUN64("pread");
__IMP __CRT_UNSUPPORTED_MSVC ssize_t (pwrite64)(int __fd, void const *__buf, size_t __n, off64_t __offset) __UNISTD_FUN64("pwrite");
#endif
#endif

#if defined(__CRT_MSVC) && !defined(__INTELLISENSE__)
__IMP __WUNUSED int (__msvc_pipe)(int __pipedes[2], __UINT32_TYPE__ __pipesize, int __textmode) __UNISTD_FUN("pipe");
#define pipe(pipedes) __msvc_pipe((pipedes),0x400,0x8000/*_O_BINARY*/)
#ifdef __USE_GNU
#define pipe(pipedes,flags) __msvc_pipe((pipedes),0x400,(flags)|0x8000/*_O_BINARY*/)
#endif
#else
__IMP int (pipe)(int __pipedes[2]);
#ifdef __USE_GNU
__IMP int (pipe2)(int __pipedes[2], int __flags);
#endif
#endif

__IMP __CRT_UNSUPPORTED_MSVC unsigned int (alarm)(unsigned int __seconds) __UNISTD_FUN("alarm");
#ifdef __CRT_MSVC
__IMP void (__msvc_sleep)(unsigned int __seconds) __UNISTD_FUN("_sleep");
#define sleep(seconds)    (__msvc_sleep(__seconds),0)
#else
__IMP unsigned int (sleep)(unsigned int __seconds) __UNISTD_FUN("sleep");
#endif
__IMP __CRT_UNSUPPORTED_MSVC __useconds_t (ualarm)(__useconds_t __value, __useconds_t __interval) __UNISTD_FUN("ualarm");
__IMP __CRT_UNSUPPORTED_MSVC int (usleep)(__useconds_t __useconds) __UNISTD_FUN("usleep");
__IMP __CRT_UNSUPPORTED_MSVC int (pause)(void) __UNISTD_FUN("pause");
__IMP __CRT_UNSUPPORTED_MSVC int (chown)(char const *__file, __uid_t __owner, __gid_t __group) __UNISTD_FUN("chown");
#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __CRT_UNSUPPORTED_MSVC int (fchown)(int __fd, __uid_t __owner, __gid_t __group) __UNISTD_FUN("fchown");
__IMP __CRT_UNSUPPORTED_MSVC int (lchown)(char const *__file, __uid_t __owner, __gid_t __group) __UNISTD_FUN("lchown");
#endif
#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (fchownat)(int __fd, char const *__file, __uid_t __owner, __gid_t __group, int __flag) __UNISTD_FUN("fchownat");
#endif

__IMP int (chdir)(char const *__path) __UNISTD_FUN("chdir");

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __CRT_UNSUPPORTED_MSVC int (fchdir)(int __fd) __UNISTD_FUN("fchdir");
#endif

#ifndef getcwd
#if !defined(__CRT_MSVC) || (__SIZEOF_SIZE_T__ == 4)
__IMP __WUNUSED char *(getcwd)(char *__buf, __SIZE_TYPE__ __bufsize) __UNISTD_FUN("getcwd");
#else
__IMP __WUNUSED char *(__msvc_getcwd)(char *__buf, __UINT32_TYPE__ __bufsize) __UNISTD_FUN("getcwd");
#define getcwd(buf,bufsize)  __msvc_getcwd(buf,(__UINT32_TYPE__)(bufsize))
#endif
#endif

#ifdef __USE_GNU
#ifdef __CRT_MSVC
#define get_current_dir_name() getcwd(NULL,0)
#else
__IMP char *(get_current_dir_name)(void) __UNISTD_FUN("get_current_dir_name");
#endif
#endif

#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_XOPEN2K8))
#ifdef __CRT_MSVC
#	pragma deprecated(getwd)
#	define getwd(buf) getcwd(buf,PATH_MAX)
#else
__IMP __WUNUSED [[__deprecated__]] char *(getwd)(char *__buf) __UNISTD_FUN("getwd");
#endif
#endif

#ifdef __CRT_MSVC
__IMP int (__msvc_setmode)(int __fd, int __mode) __UNISTD_FUN("setmode");
#endif

__IMP __WUNUSED int (dup)(int __fd) __UNISTD_FUN("dup");
__IMP int (dup2)(int __fdsrc, int __fddst) __UNISTD_FUN("dup2");

#ifdef __USE_GNU
#if defined(__CRT_MSVC) && !defined(__INTELLISENSE__)
#	define dup3(fd,fd2,flags) (dup2(fd,fd2) ?: __msvc_setmode(fd2,flags))
#else
__IMP int (dup3)(int __fdsrc, int __fddst, int __flags) __UNISTD_FUN("dup3");
#endif
#endif

#undef __environ
#undef environ
#ifndef __CRT_MSVC
__IMP extern char **__environ;
__IMP extern char **environ;
#elif !defined(_M_CEE_PURE)
__IMP extern char **__environ __asm__("_environ");
__IMP extern char **environ   __asm__("_environ");
#else
__IMP extern char ***(__cdecl __p__environ)(void);
#	define __environ (*__p__environ())
#	define environ   (*__p__environ())
#endif

#ifdef __CRT_MSVC
#if __SIZEOF_POINTER__ == __SIZEOF_INT__
#	define __PROC_RETURN __pid_t
#else
#	define __PROC_RETURN __INTPTR_TYPE__
#endif
#else
#	define __PROC_RETURN int
#endif

__IMP __PROC_RETURN (execve)(char const *__path, char *const __argv[], char *const __envp[]) __UNISTD_FUN("execve");
#ifdef __USE_XOPEN2K8
__IMP __CRT_UNSUPPORTED_MSVC __PROC_RETURN (fexecve)(int __fd, char *const __argv[], char *const __envp[]) __UNISTD_FUN("fexecve");
#endif
__IMP __PROC_RETURN (execv)(char const *__path, char *const __argv[]) __UNISTD_FUN("execv");
__IMP __PROC_RETURN (execle)(char const *__path, char const *__arg, ...) __UNISTD_FUN("execle");
__IMP __PROC_RETURN (execl)(char const *__path, char const *__arg, ...) __UNISTD_FUN("execl");
__IMP __PROC_RETURN (execvp)(char const *__file, char *const __argv[]) __UNISTD_FUN("execvp");
__IMP __PROC_RETURN (execlp)(char const *__file, char const *__arg, ...) __UNISTD_FUN("execlp");
#ifdef __USE_GNU
__IMP __PROC_RETURN (execvpe)(char const *__file, char *const __argv[], char *const __envp[]) __UNISTD_FUN("execvpe");
#endif
#undef __PROC_RETURN

#if defined(__USE_MISC) || defined(__USE_XOPEN)
__IMP __CRT_UNSUPPORTED_MSVC int (nice)(int __inc) __UNISTD_FUN("nice");
#endif

__IMP _Noreturn void (_exit)(int __code);


#if __has_include(<bits/confname.h>)
#	include <bits/confname.h>
#endif

__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC long int (pathconf)(char const *__path, int __name) __UNISTD_FUN("pathconf");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC long int (fpathconf)(int __fd, int __name) __UNISTD_FUN("fpathconf");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC long int (sysconf)(int __name) __UNISTD_FUN("sysconf");
#ifdef	__USE_POSIX2
__IMP __CRT_UNSUPPORTED_MSVC size_t (confstr)(int __name, char *__buf, size_t __len) __UNISTD_FUN("confstr");
#endif

__IMP __WUNUSED __pid_t (getpid)(void) __UNISTD_FUN("getpid");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __pid_t (getppid)(void) __UNISTD_FUN("getppid");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __pid_t (getpgrp)(void) __UNISTD_FUN("getpgrp");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __pid_t (__getpgid)(__pid_t __pid) __UNISTD_FUN("__getpgid");
#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __pid_t (getpgid)(__pid_t __pid) __UNISTD_FUN("getpgid");
#endif
__IMP __CRT_UNSUPPORTED_MSVC int (setpgid)(__pid_t __pid, __pid_t __pgid) __UNISTD_FUN("setpgid");

#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
__IMP __CRT_UNSUPPORTED_MSVC int (setpgrp)(void) __UNISTD_FUN("setpgrp");
#endif
__IMP __CRT_UNSUPPORTED_MSVC __pid_t (setsid)(void) __UNISTD_FUN("setsid");

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
__IMP __CRT_UNSUPPORTED_MSVC __pid_t (getsid)(__pid_t __pid) __UNISTD_FUN("getsid");
#endif
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __uid_t (getuid)(void) __UNISTD_FUN("getuid");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __uid_t (geteuid)(void) __UNISTD_FUN("geteuid");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __gid_t (getgid)(void) __UNISTD_FUN("getgid");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC __gid_t (getegid)(void) __UNISTD_FUN("getegid");
__IMP __WUNUSED __CRT_UNSUPPORTED_MSVC int (getgroups)(int __size, __gid_t __list[]) __UNISTD_FUN("getgroups");

#ifdef	__USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC int (group_member)(__gid_t __gid) __UNISTD_FUN("group_member");
#endif

__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setuid)(__uid_t __uid) __UNISTD_FUN("setuid");
#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setreuid)(__uid_t __ruid, __uid_t __euid) __UNISTD_FUN("setreuid");
#endif
#ifdef __USE_XOPEN2K
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (seteuid)(__uid_t __uid) __UNISTD_FUN("seteuid");
#endif
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setgid)(__gid_t __gid) __UNISTD_FUN("setgid");
#if defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED)
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setregid)(__gid_t __rgid, __gid_t __egid) __UNISTD_FUN("setregid");
#endif
#ifdef __USE_XOPEN2K
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setegid)(__gid_t __gid) __UNISTD_FUN("setegid");
#endif
#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC int (getresuid)(__uid_t *__ruid, __uid_t *__euid, __uid_t *__suid) __UNISTD_FUN("getresuid");
__IMP __CRT_UNSUPPORTED_MSVC int (getresgid)(__gid_t *__rgid, __gid_t *__egid, __gid_t *__sgid) __UNISTD_FUN("getresgid");
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setresuid)(__uid_t __ruid, __uid_t __euid, __uid_t __suid) __UNISTD_FUN("setresuid");
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED int (setresgid)(__gid_t __rgid, __gid_t __egid, __gid_t __sgid) __UNISTD_FUN("setresgid");
#endif

__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED __pid_t (fork)(void) __UNISTD_FUN("fork");
#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_XOPEN2K8))
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED __pid_t (vfork)(void) __UNISTD_FUN("vfork");
#endif

__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED char *(ttyname)(int __fd) __UNISTD_FUN("ttyname");
__IMP __CRT_UNSUPPORTED_MSVC int (ttyname_r)(int __fd, char *__buf, size_t __buflen) __UNISTD_FUN("ttyname_r");
__IMP __WUNUSED int (isatty)(int __fd) __UNISTD_FUN("isatty");

#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_UNIX98))
__IMP __CRT_UNSUPPORTED_MSVC int (ttyslot)(void) __UNISTD_FUN("ttyslot");
#endif

__IMP __CRT_UNSUPPORTED_MSVC int (link)(char const *__from, char const *__to) __UNISTD_FUN("link");

#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (linkat)(int __fromfd, char const *__from, int __tofd, char const *__to, int __flags) __UNISTD_FUN("linkat");
#endif

#if defined __USE_XOPEN_EXTENDED || defined __USE_XOPEN2K
__IMP __CRT_UNSUPPORTED_MSVC int (symlink)(char const *__from, char const *__to) __UNISTD_FUN("symlink");
__IMP __CRT_UNSUPPORTED_MSVC ssize_t (readlink)(char const *__restrict __path, char *__restrict __buf, size_t __len) __UNISTD_FUN("readlink");
#endif

#ifdef __USE_ATFILE
__IMP __CRT_UNSUPPORTED_MSVC int (symlinkat)(char const *__from, int __tofd, char const *__to) __UNISTD_FUN("symlinkat");
__IMP __CRT_UNSUPPORTED_MSVC ssize_t (readlinkat)(int __fd, char const *__restrict __path, char *__restrict __buf, size_t __len) __UNISTD_FUN("readlinkat");
#endif

__IMP int (unlink)(char const *__name) __UNISTD_FUN("unlink");
#ifdef __USE_ATFILE
__IMP int (unlinkat)(int __fd, char const *__name, int __flag) __UNISTD_FUN("unlinkat");
#endif
__IMP int (rmdir)(char const *__path) __UNISTD_FUN("rmdir");
__IMP __CRT_UNSUPPORTED_MSVC __pid_t (tcgetpgrp)(int __fd) __UNISTD_FUN("tcgetpgrp");
__IMP __CRT_UNSUPPORTED_MSVC int (tcsetpgrp)(int __fd, __pid_t __pgrp_id) __UNISTD_FUN("tcsetpgrp");
__IMP __CRT_UNSUPPORTED_MSVC char *(getlogin)(void) __UNISTD_FUN("getlogin");
#if defined(__USE_REENTRANT) || defined(__USE_POSIX199506)
__IMP __CRT_UNSUPPORTED_MSVC int (getlogin_r)(char *__name, size_t __name_len) __UNISTD_FUN("getlogin_r");
#endif
#ifdef	__USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC int (setlogin)(char const *__name) __UNISTD_FUN("setlogin");
#endif

#ifdef __USE_POSIX2
#pragma push_macro(undef,"optarg","optind","opterr","optopt")
__IMP __CRT_UNSUPPORTED_MSVC extern char *(optarg) __UNISTD_FUN("optarg");
__IMP __CRT_UNSUPPORTED_MSVC extern int (optind) __UNISTD_FUN("optind");
__IMP __CRT_UNSUPPORTED_MSVC extern int (opterr) __UNISTD_FUN("opterr");
__IMP __CRT_UNSUPPORTED_MSVC extern int (optopt) __UNISTD_FUN("optopt");
#pragma pop_macro("optarg","optind","opterr","optopt")
__IMP __CRT_UNSUPPORTED_MSVC int (getopt)(int ___argc, char *const *___argv, char const *__shortopts)
#ifdef __CRT_GLIBC
	__asm__("__posix_getopt")
#endif
;
#endif

#if defined(__USE_UNIX98) || defined(__USE_XOPEN2K)
__IMP __CRT_UNSUPPORTED_MSVC int (gethostname)(char *__name, size_t __len) __UNISTD_FUN("gethostname");
#endif

#if defined(__USE_MISC)
__IMP __CRT_UNSUPPORTED_MSVC int (sethostname)(char const *__name, size_t __len) __UNISTD_FUN("sethostname");
__IMP __CRT_UNSUPPORTED_MSVC int (sethostid)(long int __id) __UNISTD_FUN("sethostid");
__IMP __CRT_UNSUPPORTED_MSVC int (getdomainname)(char *__name, size_t __len) __UNISTD_FUN("getdomainname");
__IMP __CRT_UNSUPPORTED_MSVC int (setdomainname)(char const *__name, size_t __len) __UNISTD_FUN("setdomainname");
__IMP __CRT_UNSUPPORTED_MSVC int (vhangup)(void) __UNISTD_FUN("vhangup");
__IMP __CRT_UNSUPPORTED_MSVC int (revoke)(char const *__file) __UNISTD_FUN("revoke");
__IMP __CRT_UNSUPPORTED_MSVC int (profil)(unsigned short int *__sample_buffer, size_t __size, size_t __offset, unsigned int __scale) __UNISTD_FUN("profil");
__IMP __CRT_UNSUPPORTED_MSVC int (acct)(char const *__name) __UNISTD_FUN("acct");
__IMP __CRT_UNSUPPORTED_MSVC __WUNUSED char *(getusershell)(void) __UNISTD_FUN("getusershell");
__IMP __CRT_UNSUPPORTED_MSVC void (endusershell)(void) __UNISTD_FUN("endusershell");
__IMP __CRT_UNSUPPORTED_MSVC void (setusershell)(void) __UNISTD_FUN("setusershell");
__IMP __CRT_UNSUPPORTED_MSVC int (daemon)(int __nochdir, int __noclose) __UNISTD_FUN("daemon");
#endif

#if defined(__USE_MISC) || \
   (defined(__USE_XOPEN) && !defined(__USE_XOPEN2K))
__IMP __CRT_UNSUPPORTED_MSVC int (chroot)(char const *__path) __UNISTD_FUN("chroot");
__IMP __CRT_UNSUPPORTED_MSVC char *(getpass)(char const *__prompt) __UNISTD_FUN("getpass");
#endif

__IMP int (fsync)(int __fd)
#ifdef __CRT_MSVC
	__asm__("_commit")
#else
	__UNISTD_FUN("fsync")
#endif
;

#ifdef __USE_GNU
__IMP __CRT_WORKAROUND_MSVC int (syncfs)(int __fd)
#ifdef __CRT_MSVC
	__asm__("_commit")
#else
	__UNISTD_FUN("syncfs")
#endif
;
#endif

#if defined(__USE_MISC) || \
    defined(__USE_XOPEN_EXTENDED)
__IMP __CRT_UNSUPPORTED_MSVC long int (gethostid)(void) __UNISTD_FUN("gethostid");
__IMP __CRT_UNSUPPORTED_MSVC void (sync)(void) __UNISTD_FUN("sync");
#if defined(__USE_MISC) || !defined(__USE_XOPEN2K)
#if defined(__CRT_MSVC) && defined(__i386__)
#   define getpagesize()          4096
#else
__IMP __CRT_UNSUPPORTED_MSVC int (getpagesize)(void) __UNISTD_FUN("getpagesize");
#endif
__IMP __CRT_UNSUPPORTED_MSVC int (getdtablesize)(void) __UNISTD_FUN("getdtablesize");
#endif
#endif

#if defined(__USE_XOPEN_EXTENDED) || defined(__USE_XOPEN2K8)
#ifdef __CRT_MSVC
__IMP int (__msvc_open)(char const *__name, int __openflag, ...) __asm__("_open");
__IMP int (__msvc_ftruncate32)(int __fd, __UINT32_TYPE__ __sz) __asm__("_chsize");
__IMP int (__msvc_ftruncate64)(int __fd, __UINT64_TYPE__ __sz) __asm__("_chsize_s");
/* Work-around for truncate linked against MSVCRT */
#define __msvc_truncate(file,callback) \
 __extension({ int __fd = __msvc_open(file,0x0102,0777);\
               int __err = __fd >= 0 ? (callback) : -1;\
               close(__fd);\
               __err; })
#ifdef __USE_FILE_OFFSET64
#   define truncate(file,length)   __msvc_truncate(file,__msvc_ftruncate64(__fd,length))
#else
#   define truncate(file,length)   __msvc_truncate(file,__msvc_ftruncate32(__fd,length))
#endif
#ifdef __USE_LARGEFILE64
#   define truncate64(file,length) __msvc_truncate(file,__msvc_ftruncate64(__fd,length))
#endif
#else
__IMP int (truncate)(char const *__file, off_t __length) __UNISTD_FUN32("truncate");
#ifdef __USE_LARGEFILE64
__IMP int (truncate64)(char const *__file, off64_t __length) __UNISTD_FUN64("truncate");
#endif
#endif
#endif

#if defined(__USE_POSIX199309) || \
    defined(__USE_XOPEN_EXTENDED) || \
    defined(__USE_XOPEN2K)
__IMP int (ftruncate)(int __fd, off_t __length)
#ifdef __CRT_MSVC
#ifdef __USE_FILE_OFFSET64
	__asm__("_chsize_s")
#else
	__asm__("_chsize")
#endif
#else
	__UNISTD_FUN32("ftruncate")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP int (ftruncate64)(int __fd, off64_t __length)
#ifdef __CRT_MSVC
	__asm__("_chsize_s")
#else
	__UNISTD_FUN64("ftruncate")
#endif
;
#endif
#endif

#if (defined(__USE_XOPEN_EXTENDED) && !defined(__USE_XOPEN2K)) || \
     defined(__USE_MISC)
__IMP __CRT_UNSUPPORTED_MSVC int (brk)(void *__addr) __UNISTD_FUN("brk");
__IMP __CRT_UNSUPPORTED_MSVC void *(sbrk)(intptr_t __delta) __UNISTD_FUN("sbrk");
#endif

#ifdef __USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC __INTPTR_TYPE__ (syscall)(__INTPTR_TYPE__ __sysno, ...) __UNISTD_FUN("syscall");
#endif

#if !defined(F_ULOCK) && (defined(__USE_MISC) || defined(__USE_XOPEN_EXTENDED))
#	define F_ULOCK 0 /* Unlock a previously locked region.  */
#	define F_LOCK  1 /* Lock a region for exclusive use.  */
#ifndef __CRT_MSVC
#	define F_TLOCK 2 /* Test and lock a region for exclusive use.  */
#	define F_TEST  3 /* Test a region for other processes locks.  */
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

#ifdef __USE_GNU
# define TEMP_FAILURE_RETRY(expression) \
 __extension__({ long int __r; \
                 do __r = (long int)(expression); \
                 while (__r == -1L && errno == EINTR); \
                 __r; })
#endif

#if defined(__USE_POSIX199309) || defined(__USE_UNIX98)
__IMP __CRT_WORKAROUND_MSVC int (fdatasync)(int __fildes)
#ifdef __CRT_MSVC
	__asm__("_commit")
#else
	__UNISTD_FUN("fdatasync")
#endif
;
#endif

#ifdef	__USE_XOPEN
__IMP __CRT_UNSUPPORTED_MSVC char *(crypt)(char const *__key, char const *__salt) __UNISTD_FUN("crypt");
__IMP __CRT_UNSUPPORTED_MSVC void (encrypt)(char *__glibc_block, int __edflag) __UNISTD_FUN("encrypt");
__IMP void (swab)(void const *__restrict __from, void *__restrict __to, ssize_t __n) __UNISTD_FUN("swab");
#endif

#if defined __USE_XOPEN && !defined __USE_XOPEN2K
__IMP __CRT_UNSUPPORTED_MSVC char *(ctermid)(char *__s) __UNISTD_FUN("ctermid");
#endif

#if defined(__USE_FORTIFY_LEVEL) && \
  ((__USE_FORTIFY_LEVEL-0) > 0) && \
    defined(__fortify_function) && \
    __has_include(<bits/unistd.h>)
#	include <bits/unistd.h>
#endif

//__IMP __WUNUSED int (chsize)(int __fd, __MS_LONG __size) __UNISTD_FUN("chsize");
//__IMP __WUNUSED int (eof)(int __fd) __UNISTD_FUN("eof");
//__IMP __WUNUSED __MS_LONG (filelength)(int __fd) __UNISTD_FUN("filelength");
//__IMP __WUNUSED __MS_LONG (tell)(int __fd) __UNISTD_FUN("tell");

#undef __UNISTD_FUN64
#undef __UNISTD_FUN32
#undef __UNISTD_FUN

#endif
