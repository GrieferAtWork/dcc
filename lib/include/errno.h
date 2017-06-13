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

#if __has_include_next(<errno.h>)
#include_next <errno.h>
#else
#include <__stdinc.h>

#ifdef __CRT_MSVC

__IMP int *(__msvc_errno)(void) __asm__("_errno");
#define errno  (*__msvc_errno())

#define EPERM           1
#define ENOENT          2
#define ESRCH           3
#define EINTR           4
#define EIO             5
#define ENXIO           6
#define E2BIG           7
#define ENOEXEC         8
#define EBADF           9
#define ECHILD          10
#define EAGAIN          11
#define ENOMEM          12
#define EACCES          13
#define EFAULT          14
#define EBUSY           16
#define EEXIST          17
#define EXDEV           18
#define ENODEV          19
#define ENOTDIR         20
#define EISDIR          21
#define ENFILE          23
#define EMFILE          24
#define ENOTTY          25
#define EFBIG           27
#define ENOSPC          28
#define ESPIPE          29
#define EROFS           30
#define EMLINK          31
#define EPIPE           32
#define EDOM            33
#define EDEADLK         36
#define ENAMETOOLONG    38
#define ENOLCK          39
#define ENOSYS          40
#define ENOTEMPTY       41

#define EINVAL          22
#define ERANGE          34
#define EILSEQ          42
#define STRUNCATE       80

#define EADDRINUSE      100
#define EADDRNOTAVAIL   101
#define EAFNOSUPPORT    102
#define EALREADY        103
#define EBADMSG         104
#define ECANCELED       105
#define ECONNABORTED    106
#define ECONNREFUSED    107
#define ECONNRESET      108
#define EDESTADDRREQ    109
#define EHOSTUNREACH    110
#define EIDRM           111
#define EINPROGRESS     112
#define EISCONN         113
#define ELOOP           114
#define EMSGSIZE        115
#define ENETDOWN        116
#define ENETRESET       117
#define ENETUNREACH     118
#define ENOBUFS         119
#define ENODATA         120
#define ENOLINK         121
#define ENOMSG          122
#define ENOPROTOOPT     123
#define ENOSR           124
#define ENOSTR          125
#define ENOTCONN        126
#define ENOTRECOVERABLE 127
#define ENOTSOCK        128
#define ENOTSUP         129
#define EOPNOTSUPP      130
#define EOTHER          131
#define EOVERFLOW       132
#define EOWNERDEAD      133
#define EPROTO          134
#define EPROTONOSUPPORT 135
#define EPROTOTYPE      136
#define ETIME           137
#define ETIMEDOUT       138
#define ETXTBSY         139
#define EWOULDBLOCK     140

#else
#error FIXME
#endif

#endif
