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

#if __has_include_next(<bits/fcntl.h>)
#include_next <bits/fcntl.h>
#else /* include_next... */

#include <__stdinc.h>
#include <features.h>
#include <bits/types.h>

#ifdef __x86_64__
#	define __O_LARGEFILE  0
#	define F_GETLK64  5
#	define F_SETLK64  6
#	define F_SETLKW64 7
#endif

struct __CRT_UNSUPPORTED_MSVC flock {
	short int l_type;
	short int l_whence;
#ifndef __USE_FILE_OFFSET64
	__off_t   l_start;
	__off_t   l_len;
#else
	__off64_t l_start;
	__off64_t l_len;
#endif
	__pid_t   l_pid;
};

#ifdef __USE_LARGEFILE64
struct __CRT_UNSUPPORTED_MSVC flock64 {
	short int l_type;
	short int l_whence;
	__off64_t l_start;
	__off64_t l_len;
	__pid_t   l_pid;
};
#endif

#include <bits/fcntl-linux.h>

#endif /* !include_next... */
