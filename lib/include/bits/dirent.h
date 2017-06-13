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

#if __has_include_next(<bits/dirent.h>)
#include_next <bits/dirent.h>
#else
#include <__stdinc.h>
#include <bits/types.h>

#ifdef __CRT_MSVC
struct dirent {
#ifdef __USE_FILE_OFFSET64
	__ino64_t       d_ino;
#else
	__ino_t         d_ino;
#endif
union{
	unsigned char   d_type;
	unsigned        __msvc_attrib;
};
#ifdef __USE_FILE_OFFSET64
	__time64_t      __msvc_time_create;
	__time64_t      __msvc_time_access;
	__time64_t      __msvc_time_write;
	__UINT64_TYPE__ __msvc_size;
#else
	__time32_t      __msvc_time_create;
	__time32_t      __msvc_time_access;
	__time32_t      __msvc_time_write;
	__UINT32_TYPE__ __msvc_size;
#endif
	char            d_name[262];
};

struct dirent64 {
	__ino64_t       d_ino;
union{
	unsigned char   d_type;
	unsigned        __msvc_attrib;
};
	__time64_t      __msvc_time_create;
	__time64_t      __msvc_time_access;
	__time64_t      __msvc_time_write;
	__UINT64_TYPE__ __msvc_size;
	char            d_name[262];
};

#undef  _DIRENT_HAVE_D_NAMLEN
#undef  _DIRENT_HAVE_D_RECLEN
#undef  _DIRENT_HAVE_D_OFF
#define _DIRENT_HAVE_D_TYPE

#else /* __CRT_MSVC */

struct dirent {
#ifndef __USE_FILE_OFFSET64
	__ino_t            d_ino;
	__off_t            d_off;
#else
	__ino64_t          d_ino;
	__off64_t          d_off;
#endif
	unsigned short int d_reclen;
	unsigned char      d_type;
	char               d_name[256];
};
#ifdef __USE_LARGEFILE64
struct dirent64 {
	__ino64_t          d_ino;
	__off64_t          d_off;
	unsigned short int d_reclen;
	unsigned char      d_type;
	char               d_name[256];
};
#endif

#define d_fileno d_ino

#undef  _DIRENT_HAVE_D_NAMLEN
#define _DIRENT_HAVE_D_RECLEN
#define _DIRENT_HAVE_D_OFF
#define _DIRENT_HAVE_D_TYPE

#if defined(__OFF_T_MATCHES_OFF64_T) && \
    defined(__INO_T_MATCHES_INO64_T)
#define _DIRENT_MATCHES_DIRENT64 1
#endif

#endif /* CRT... */

#endif /* !include_next... */
