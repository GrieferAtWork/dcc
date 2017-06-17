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

#if __has_include_next(<bits/fcntl-linux.h>)
#include_next <bits/fcntl-linux.h>
#else

#include <__stdinc.h>
#include <features.h>

#ifdef __CRT_MSVC
#	define O_RDONLY       0x00000
#	define O_WRONLY       0x00001
#	define O_RDWR         0x00002
#	define O_APPEND       0x00008
#	define O_CREAT        0x00100
#	define O_TRUNC        0x00200
#	define O_EXCL         0x00400
#	define O_TEXT         0x04000
#	define O_BINARY       0x08000
#	define O_NOINHERIT    0x00080
#	define O_TEMPORARY    0x00040
#	define O_SEQUENTIAL   0x00020
#	define O_RANDOM       0x00010
#	define _O_WTEXT       0x10000
#	define _O_U16TEXT     0x20000
#	define _O_U8TEXT      0x40000
#	define _O_RAW         O_BINARY
#	define _O_SHORT_LIVED 0x01000
#	define _O_OBTAIN_DIR  0x02000
#elif defined(__CRT_DCC) && defined(__WINDOWS__)
#   define __O_ACCESS_MASK  0xc00001ff
#   define __O_ACCESS_SHIFT 0
#   define __O_ACCESS_FLAGS 0

#   define __O_DISP_MASK    0x00000e00
#   define __O_DISP_SHIFT   9
#   define __O_DISP_FLAGS   0

#   define __O_SHARE_MASK   0
#   define __O_SHARE_SHIFT  0
#   define __O_SHARE_RSHIFT 0
#   define __O_SHARE_FLAGS  0x00000007 /* FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE */

#   define __O_ATTR_MASK    0x0ffff000
#   define __O_ATTR_SHIFT   0
#   define __O_ATTR_RSHIFT  4
#   define __O_ATTR_FLAGS   0x00000080

#   define __O_ATTR2_MASK   0x30000000
#   define __O_ATTR2_SHIFT  20
#   define __O_ATTR2_RSHIFT 0
#   define __O_ATTR2_FLAGS  0


#   define O_RDONLY        0x80000089 /* GENERIC_READ|FILE_READ_DATA|FILE_READ_EA|FILE_READ_ATTRIBUTES */
#   define O_WRONLY        0x40000112 /* GENERIC_WRITE|FILE_WRITE_DATA|FILE_WRITE_EA|FILE_WRITE_ATTRIBUTES */
#	define __O_DIRECTORY   0x00200060 /* FILE_TRAVERSE|FILE_DELETE_CHILD|FILE_FLAG_BACKUP_SEMANTICS */
#	define O_APPEND        0x00000004 /* FILE_APPEND_DATA */
#	define O_RDWR         (O_RDONLY|O_WRONLY)
#	define O_CREAT         0x00000200
#	define O_EXCL          0x00000400
#	define O_TRUNC         0x00000800
#	define __O_TMPFILE     0x10400000 /* FILE_ATTRIBUTE_TEMPORARY|FILE_FLAG_DELETE_ON_CLOSE */
#	define __O_LARGEFILE   0
#	define __O_DIRECT      0x0a000000 /* FILE_FLAG_NO_BUFFERING|FILE_FLAG_WRITE_THROUGH */
#	define __O_NOFOLLOW    0x00020000 /* FILE_FLAG_OPEN_REPARSE_POINT */
#	define O_ASYNC         0x04000000 /* FILE_FLAG_OVERLAPPED */
#	define O_SYNC          0
#	define O_NONBLOCK      O_ASYNC
#	define O_NDELAY        O_ASYNC
#	define O_FSYNC         O_SYNC

#	define O_NOCTTY        0
#	define __O_CLOEXEC     0
#	define __O_NOATIME     0
#	define __O_PATH        0
#	define __O_DSYNC       0

#ifdef __USE_LARGEFILE64
#	define O_LARGEFILE  __O_LARGEFILE
#endif
#ifdef __USE_XOPEN2K8
#	define O_DIRECTORY  __O_DIRECTORY
#	define O_NOFOLLOW   __O_NOFOLLOW
#	define O_CLOEXEC    __O_CLOEXEC
#endif
#ifdef __USE_GNU
#	define O_DIRECT     __O_DIRECT
#	define O_NOATIME    __O_NOATIME
#	define O_PATH       __O_PATH
#	define O_TMPFILE    __O_TMPFILE
#endif

#if defined(__USE_POSIX199309) || \
    defined(__USE_UNIX98)
#	define O_DSYNC  __O_DSYNC
#if defined(__O_RSYNC)
#	define O_RSYNC  __O_RSYNC
#else
#	define O_RSYNC  O_SYNC
#endif
#endif

/* Values for the second argument to `fcntl'.  */
#	define F_DUPFD       0 /* Duplicate file descriptor.  */
#	define F_GETFD       1 /* Get file descriptor flags.  */
#	define F_SETFD       2 /* Set file descriptor flags.  */
#	define F_GETFL       3 /* Get file status flags.  */
#	define F_SETFL       4 /* Set file status flags.  */
#ifndef __F_SETOWN
#	define __F_SETOWN    8
#	define __F_GETOWN    9
#endif
#if defined __USE_UNIX98 || defined __USE_XOPEN2K8
#	define F_SETOWN      __F_SETOWN
#	define F_GETOWN      __F_GETOWN
#endif
#ifndef __F_SETSIG
#	define __F_SETSIG    10
#	define __F_GETSIG    11
#endif
#ifndef __F_SETOWN_EX
#	define __F_SETOWN_EX 15
#	define __F_GETOWN_EX 16
#endif
#ifdef __USE_GNU
#	define F_SETSIG      __F_SETSIG
#	define F_GETSIG      __F_GETSIG
#	define F_SETOWN_EX   __F_SETOWN_EX
#	define F_GETOWN_EX   __F_GETOWN_EX
#endif
#ifdef __USE_GNU
#	define F_SETLEASE    1024
#	define F_GETLEASE    1025
#	define F_NOTIFY      1026
#	define F_SETPIPE_SZ  1031
#	define F_GETPIPE_SZ  1032
#endif
#ifdef __USE_XOPEN2K8
#	define F_DUPFD_CLOEXEC 1030
#endif

/* For F_[GET|SET]FD.  */
#define FD_CLOEXEC      1
#ifndef F_RDLCK
#	define F_RDLCK      0
#	define F_WRLCK      1
#	define F_UNLCK      2
#endif
#ifndef F_EXLCK
#	define F_EXLCK      4
#	define F_SHLCK      8
#endif
#ifdef __USE_MISC
#	define LOCK_SH      1
#	define LOCK_EX      2
#	define LOCK_NB      4
#	define LOCK_UN      8
#endif
#ifdef __USE_GNU
#	define LOCK_MAND    32
#	define LOCK_READ    64
#	define LOCK_WRITE   128
#	define LOCK_RW      192
#endif
#ifdef __USE_GNU
#	define DN_ACCESS    0x00000001
#	define DN_MODIFY    0x00000002
#	define DN_CREATE    0x00000004
#	define DN_DELETE    0x00000008
#	define DN_RENAME    0x00000010
#	define DN_ATTRIB    0x00000020
#	define DN_MULTISHOT 0x80000000
#endif
#ifdef __USE_GNU
enum __pid_type {
	F_OWNER_TID = 0,
	F_OWNER_PID,
	F_OWNER_PGRP,
	F_OWNER_GID = F_OWNER_PGRP
};
struct f_owner_ex {
	enum __pid_type type;
	__pid_t         pid;
};
#endif
#ifdef __USE_MISC
#	define FAPPEND   O_APPEND
#	define FFSYNC    O_FSYNC
#	define FASYNC    O_ASYNC
#	define FNONBLOCK O_NONBLOCK
#	define FNDELAY   O_NDELAY
#endif
#ifndef __POSIX_FADV_DONTNEED
#	define __POSIX_FADV_DONTNEED 4
#	define __POSIX_FADV_NOREUSE  5
#endif
#ifdef __USE_XOPEN2K
#	define POSIX_FADV_NORMAL     0
#	define POSIX_FADV_RANDOM     1
#	define POSIX_FADV_SEQUENTIAL 2
#	define POSIX_FADV_WILLNEED   3
#	define POSIX_FADV_DONTNEED   __POSIX_FADV_DONTNEED
#	define POSIX_FADV_NOREUSE    __POSIX_FADV_NOREUSE
#endif
#ifdef __USE_GNU
#	define SYNC_FILE_RANGE_WAIT_BEFORE 1
#	define SYNC_FILE_RANGE_WRITE       2
#	define SYNC_FILE_RANGE_WAIT_AFTER  4
#	define SPLICE_F_MOVE               1
#	define SPLICE_F_NONBLOCK           2
#	define SPLICE_F_MORE               4
#	define SPLICE_F_GIFT               8
#	define FALLOC_FL_KEEP_SIZE         1
#	define FALLOC_FL_PUNCH_HOLE        2
#	define FALLOC_FL_COLLAPSE_RANGE    8
#	define FALLOC_FL_ZERO_RANGE       16

struct file_handle {
	unsigned int  handle_bytes;
	int           handle_type;
	unsigned char f_handle[0];
};
#	define MAX_HANDLE_SZ 128
#endif
#ifdef __USE_ATFILE
#	define AT_FDCWD             (-100)
#	define AT_SYMLINK_NOFOLLOW  0x100
#	define AT_REMOVEDIR         0x200
#	define AT_SYMLINK_FOLLOW    0x400
#ifdef __USE_GNU
#	define AT_NO_AUTOMOUNT  0x0800
#	define AT_EMPTY_PATH    0x1000
#endif
#	define AT_EACCESS       0x0200
#endif


#else
#	define O_ACCMODE         0003
#	define O_RDONLY          00
#	define O_WRONLY          01
#	define O_RDWR            02
#	define O_CREAT           0100
#	define O_EXCL            0200
#	define O_NOCTTY          0400
#	define O_TRUNC           01000
#	define O_APPEND          02000
#	define O_NONBLOCK        04000
#	define O_NDELAY          O_NONBLOCK
#	define O_SYNC            04010000
#	define O_FSYNC           O_SYNC
#	define O_ASYNC           020000
#ifndef __O_LARGEFILE
#	define __O_LARGEFILE  0100000
#endif
#	define __O_DIRECTORY     0200000
#	define __O_NOFOLLOW      0400000
#	define __O_CLOEXEC       02000000
#	define __O_DIRECT        040000
#	define __O_NOATIME       01000000
#	define __O_PATH          010000000
#	define __O_DSYNC         010000
#	define __O_TMPFILE      (020000000 | __O_DIRECTORY)
#ifndef F_GETLK
#ifndef __USE_FILE_OFFSET64
#	define F_GETLK  5
#	define F_SETLK  6
#	define F_SETLKW 7
#else
#	define F_GETLK  F_GETLK64
#	define F_SETLK  F_SETLK64
#	define F_SETLKW F_SETLKW64
#endif
#endif
#ifndef F_GETLK64
#	define F_GETLK64  12
#	define F_SETLK64  13
#	define F_SETLKW64 14
#endif

#ifdef __USE_GNU
#	define F_OFD_GETLK  36
#	define F_OFD_SETLK  37
#	define F_OFD_SETLKW 38
#endif
#ifdef __USE_LARGEFILE64
#	define O_LARGEFILE  __O_LARGEFILE
#endif
#ifdef __USE_XOPEN2K8
#	define O_DIRECTORY  __O_DIRECTORY
#	define O_NOFOLLOW   __O_NOFOLLOW
#	define O_CLOEXEC    __O_CLOEXEC
#endif
#ifdef __USE_GNU
#	define O_DIRECT     __O_DIRECT
#	define O_NOATIME    __O_NOATIME
#	define O_PATH       __O_PATH
#	define O_TMPFILE    __O_TMPFILE
#endif

#if defined(__USE_POSIX199309) || \
    defined(__USE_UNIX98)
#	define O_DSYNC  __O_DSYNC
#if defined(__O_RSYNC)
#	define O_RSYNC  __O_RSYNC
#else
#	define O_RSYNC  O_SYNC
#endif
#endif

/* Values for the second argument to `fcntl'.  */
#	define F_DUPFD       0 /* Duplicate file descriptor.  */
#	define F_GETFD       1 /* Get file descriptor flags.  */
#	define F_SETFD       2 /* Set file descriptor flags.  */
#	define F_GETFL       3 /* Get file status flags.  */
#	define F_SETFL       4 /* Set file status flags.  */
#ifndef __F_SETOWN
#	define __F_SETOWN    8
#	define __F_GETOWN    9
#endif
#if defined __USE_UNIX98 || defined __USE_XOPEN2K8
#	define F_SETOWN      __F_SETOWN
#	define F_GETOWN      __F_GETOWN
#endif
#ifndef __F_SETSIG
#	define __F_SETSIG    10
#	define __F_GETSIG    11
#endif
#ifndef __F_SETOWN_EX
#	define __F_SETOWN_EX 15
#	define __F_GETOWN_EX 16
#endif
#ifdef __USE_GNU
#	define F_SETSIG      __F_SETSIG
#	define F_GETSIG      __F_GETSIG
#	define F_SETOWN_EX   __F_SETOWN_EX
#	define F_GETOWN_EX   __F_GETOWN_EX
#endif
#ifdef __USE_GNU
#	define F_SETLEASE    1024
#	define F_GETLEASE    1025
#	define F_NOTIFY      1026
#	define F_SETPIPE_SZ  1031
#	define F_GETPIPE_SZ  1032
#endif
#ifdef __USE_XOPEN2K8
#	define F_DUPFD_CLOEXEC 1030
#endif

/* For F_[GET|SET]FD.  */
#define FD_CLOEXEC      1
#ifndef F_RDLCK
#	define F_RDLCK      0
#	define F_WRLCK      1
#	define F_UNLCK      2
#endif
#ifndef F_EXLCK
#	define F_EXLCK      4
#	define F_SHLCK      8
#endif
#ifdef __USE_MISC
#	define LOCK_SH      1
#	define LOCK_EX      2
#	define LOCK_NB      4
#	define LOCK_UN      8
#endif
#ifdef __USE_GNU
#	define LOCK_MAND    32
#	define LOCK_READ    64
#	define LOCK_WRITE   128
#	define LOCK_RW      192
#endif
#ifdef __USE_GNU
#	define DN_ACCESS    0x00000001
#	define DN_MODIFY    0x00000002
#	define DN_CREATE    0x00000004
#	define DN_DELETE    0x00000008
#	define DN_RENAME    0x00000010
#	define DN_ATTRIB    0x00000020
#	define DN_MULTISHOT 0x80000000
#endif
#ifdef __USE_GNU
enum __pid_type {
	F_OWNER_TID = 0,
	F_OWNER_PID,
	F_OWNER_PGRP,
	F_OWNER_GID = F_OWNER_PGRP
};
struct f_owner_ex {
	enum __pid_type type;
	__pid_t         pid;
};
#endif
#ifdef __USE_MISC
#	define FAPPEND   O_APPEND
#	define FFSYNC    O_FSYNC
#	define FASYNC    O_ASYNC
#	define FNONBLOCK O_NONBLOCK
#	define FNDELAY   O_NDELAY
#endif
#ifndef __POSIX_FADV_DONTNEED
#	define __POSIX_FADV_DONTNEED 4
#	define __POSIX_FADV_NOREUSE  5
#endif
#ifdef __USE_XOPEN2K
#	define POSIX_FADV_NORMAL     0
#	define POSIX_FADV_RANDOM     1
#	define POSIX_FADV_SEQUENTIAL 2
#	define POSIX_FADV_WILLNEED   3
#	define POSIX_FADV_DONTNEED   __POSIX_FADV_DONTNEED
#	define POSIX_FADV_NOREUSE    __POSIX_FADV_NOREUSE
#endif
#ifdef __USE_GNU
#	define SYNC_FILE_RANGE_WAIT_BEFORE 1
#	define SYNC_FILE_RANGE_WRITE       2
#	define SYNC_FILE_RANGE_WAIT_AFTER  4
#	define SPLICE_F_MOVE               1
#	define SPLICE_F_NONBLOCK           2
#	define SPLICE_F_MORE               4
#	define SPLICE_F_GIFT               8
#	define FALLOC_FL_KEEP_SIZE         1
#	define FALLOC_FL_PUNCH_HOLE        2
#	define FALLOC_FL_COLLAPSE_RANGE    8
#	define FALLOC_FL_ZERO_RANGE       16

struct file_handle {
	unsigned int  handle_bytes;
	int           handle_type;
	unsigned char f_handle[0];
};
#	define MAX_HANDLE_SZ 128
#endif
#ifdef __USE_ATFILE
#	define AT_FDCWD             (-100)
#	define AT_SYMLINK_NOFOLLOW  0x100
#	define AT_REMOVEDIR         0x200
#	define AT_SYMLINK_FOLLOW    0x400
#ifdef __USE_GNU
#	define AT_NO_AUTOMOUNT  0x0800
#	define AT_EMPTY_PATH    0x1000
#endif
#	define AT_EACCESS       0x0200
#endif
#endif /* CRT... */

#ifdef __USE_GNU
#include <bits/types.h>

__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __ssize_t (readahead)(int __fd, __off64_t __offset, __SIZE_TYPE__ __count);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (sync_file_range)(int __fd, __off64_t __offset, __off64_t __count, unsigned int __flags);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __ssize_t (vmsplice)(int __fdout, struct iovec const *__iov, size_t __count, unsigned int __flags);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __ssize_t (splice)(int __fdin, __off64_t *__offin, int __fdout, __off64_t *__offout, size_t __len, unsigned int __flags);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS __ssize_t (tee)(int __fdin, int __fdout, size_t __len, unsigned int __flags);
#ifdef __USE_FILE_OFFSET64
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (fallocate)(int __fd, int __mode, __off64_t __offset, __off64_t __len) __asm__("fallocate64");
#else
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (fallocate)(int __fd, int __mode, __off_t __offset, __off_t __len);
#endif
#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (fallocate64)(int __fd, int __mode, __off64_t __offset, __off64_t __len);
#endif
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (name_to_handle_at)(int __dfd, char const *__name, struct file_handle *__handle, int *__mnt_id, int __flags);
__IMP __CRT_UNSUPPORTED_MSVC __CRT_UNSUPPORTED_KOS int (open_by_handle_at)(int __mountdirfd, struct file_handle *__handle, int __flags);
#endif /* __USE_GNU */

#endif /* include_next */
