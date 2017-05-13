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
#ifndef GUARD_DCC_BYTEORDER_H
#define GUARD_DCC_BYTEORDER_H 1

#include "common.h"
#include "target.h"

DCC_DECL_BEGIN

#if defined(__BYTE_ORDER__)
#   define DCC_HOST_BYTEORDER   __BYTE_ORDER__
#elif defined(__BYTE_ORDER)
#   define DCC_HOST_BYTEORDER   __BYTE_ORDER
#elif defined(_BYTE_ORDER)
#   define DCC_HOST_BYTEORDER   _BYTE_ORDER
#elif defined(BYTE_ORDER)
#   define DCC_HOST_BYTEORDER   BYTE_ORDER
#elif (defined(__hppa__))\
   || (defined(__m68k__) || defined(__MC68000__) || defined(_M_M68K))\
   || (defined(__MIPS__) && defined(__MISPEB__))\
   || (defined(__ppc__) || defined(__powerpc__) || defined(_M_PPC))\
   || (defined(__ARMEB__) || defined(__sparc__)) /* Big endian... */
#   define DCC_HOST_BYTEORDER  4321
#else /* Fallback: Assume little-endian. */
#   define DCC_HOST_BYTEORDER  1234
#endif

#ifdef _MSC_VER
extern unsigned __int64 __cdecl _byteswap_uint64(unsigned __int64);
extern unsigned long __cdecl _byteswap_ulong(unsigned long);
extern unsigned short __cdecl _byteswap_ushort(unsigned short);
#pragma intrinsic(_byteswap_uint64)
#pragma intrinsic(_byteswap_ulong)
#pragma intrinsic(_byteswap_ushort)
#endif

#define DCC_CONSTANT_BSWAP16(x) \
 ((uint16_t)(x) >> 8 | (uint16_t)(x) << 8)
#define DCC_CONSTANT_BSWAP32(x) \
 ((uint32_t)(x) << 24 | ((uint32_t)(x) & 0xff00) << 8 |\
 (((uint32_t)(x) >> 8) & 0xff00) | ((uint32_t)(x) >> 24))
#define DCC_CONSTANT_BSWAP64(x) \
 ((uint64_t)(x) << 56 |\
 ((uint64_t)(x) & 0x000000000000FF00ull) << 40 |\
 ((uint64_t)(x) & 0x0000000000FF0000ull) << 24 |\
 ((uint64_t)(x) & 0x00000000FF000000ull) << 8 |\
 ((uint64_t)(x) & 0x000000FF00000000ull) >> 8 |\
 ((uint64_t)(x) & 0x0000FF0000000000ull) >> 24 |\
 ((uint64_t)(x) & 0x00FF000000000000ull) >> 40 |\
 ((uint64_t)(x) >> 56))


#if DCC_COMPILER_GCC(4,8,0) || \
   (defined(__powerpc__) && DCC_COMPILER_GCC(4,6,0)) || \
    __has_builtin(__builtin_bswap16)
#   define DCC_COMPILER_BSWAP16 __builtin_bswap16
#elif defined(_MSC_VER)
#   define DCC_COMPILER_BSWAP16 _byteswap_ushort
#else
DCC_LOCAL uint16_t dcc_byteswap16_impl(uint16_t x) { return DCC_CONSTANT_BSWAP16(x); }
#   define DCC_COMPILER_BSWAP16 dcc_byteswap16_impl
#endif

#if DCC_COMPILER_GCC(4,4,0) || __has_builtin(__builtin_bswap32)
#   define DCC_COMPILER_BSWAP32 __builtin_bswap32
#elif defined(_MSC_VER)
#   define DCC_COMPILER_BSWAP32 _byteswap_ulong
#else
DCC_LOCAL uint32_t dcc_byteswap32_impl(uint32_t x) { return DCC_CONSTANT_BSWAP32(x); }
#   define DCC_COMPILER_BSWAP32 dcc_byteswap32_impl
#endif

#if DCC_COMPILER_GCC(4,4,0) || __has_builtin(__builtin_bswap64)
#   define DCC_COMPILER_BSWAP64 __builtin_bswap64
#elif defined(_MSC_VER)
#   define DCC_COMPILER_BSWAP64 _byteswap_uint64
#else
DCC_LOCAL uint64_t dcc_byteswap64_impl(uint64_t x) { return DCC_CONSTANT_BSWAP64(x); }
#   define DCC_COMPILER_BSWAP64 dcc_byteswap64_impl
#endif


#if defined(__GNUC__) || __has_builtin(__builtin_constant_p)
#   define DCC_BSWAP16(x) (__builtin_constant_p(x) ? DCC_CONSTANT_BSWAP16(x) : DCC_COMPILER_BSWAP16(x))
#   define DCC_BSWAP32(x) (__builtin_constant_p(x) ? DCC_CONSTANT_BSWAP32(x) : DCC_COMPILER_BSWAP32(x))
#   define DCC_BSWAP64(x) (__builtin_constant_p(x) ? DCC_CONSTANT_BSWAP64(x) : DCC_COMPILER_BSWAP64(x))
#else
#   define DCC_BSWAP16     DCC_COMPILER_BSWAP16
#   define DCC_BSWAP32     DCC_COMPILER_BSWAP32
#   define DCC_BSWAP64     DCC_COMPILER_BSWAP64
#endif

/* Convert between host & target integers. */
#if DCC_HOST_BYTEORDER == DCC_TARGET_BYTEORDER
#   define DCC_H2T16   /* nothing */
#   define DCC_H2T32   /* nothing */
#   define DCC_H2T64   /* nothing */
#   define DCC_T2H16   /* nothing */
#   define DCC_T2H32   /* nothing */
#   define DCC_T2H64   /* nothing */
#else
#   define DCC_H2T16   DCC_BSWAP16
#   define DCC_H2T32   DCC_BSWAP32
#   define DCC_H2T64   DCC_BSWAP64
#   define DCC_T2H16   DCC_BSWAP16
#   define DCC_T2H32   DCC_BSWAP32
#   define DCC_T2H64   DCC_BSWAP64
#endif



DCC_DECL_END

#endif /* !GUARD_DCC_TARGET_H */
