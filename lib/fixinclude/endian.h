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

#include <__stdinc.h>

__STRICT_ANSI_HEADER

#if __has_include_next(<endian.h>)
#include_next <endian.h>
#endif

#include <features.h>

#define __LITTLE_ENDIAN    __ORDER_LITTLE_ENDIAN__
#define __BIG_ENDIAN       __ORDER_BIG_ENDIAN__
#define __PDP_ENDIAN       __ORDER_PDP_ENDIAN__

#if __has_include(<bits/endian.h>)
#	include <bits/endian.h>
#endif

#define __BYTE_ORDER       __BYTE_ORDER__
#define __FLOAT_WORD_ORDER __FLOAT_WORD_ORDER__

#ifdef __USE_MISC
#	define LITTLE_ENDIAN __LITTLE_ENDIAN
#	define BIG_ENDIAN    __BIG_ENDIAN
#	define PDP_ENDIAN    __PDP_ENDIAN
#	define BYTE_ORDER    __BYTE_ORDER
#endif

#if __BYTE_ORDER == __LITTLE_ENDIAN
#	define __LONG_LONG_PAIR(HI,LO) LO,HI
#elif __BYTE_ORDER == __BIG_ENDIAN
#	define __LONG_LONG_PAIR(HI,LO) HI,LO
#endif

#if defined(__USE_MISC) && !defined(__ASSEMBLER__)
#if __has_include(<bits/byteswap.h>) || defined(__INTELLISENSE__)
#	include <bits/byteswap.h>
#endif
#if __BYTE_ORDER == __LITTLE_ENDIAN
#	define htobe16(x) __bswap_16(x)
#	define htole16(x) (x)
#	define be16toh(x) __bswap_16(x)
#	define le16toh(x) (x)
#	define htobe32(x) __bswap_32(x)
#	define htole32(x) (x)
#	define be32toh(x) __bswap_32(x)
#	define le32toh(x) (x)
#	define htobe64(x) __bswap_64(x)
#	define htole64(x) (x)
#	define be64toh(x) __bswap_64(x)
#	define le64toh(x) (x)
#else
#	define htobe16(x) (x)
#	define htole16(x) __bswap_16(x)
#	define be16toh(x) (x)
#	define le16toh(x) __bswap_16(x)
#	define htobe32(x) (x)
#	define htole32(x) __bswap_32(x)
#	define be32toh(x) (x)
#	define le32toh(x) __bswap_32(x)
#	define htobe64(x) (x)
#	define htole64(x) __bswap_64(x)
#	define be64toh(x) (x)
#	define le64toh(x) __bswap_64(x)
#endif
#endif
