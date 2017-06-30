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

#ifdef __INT8_C
#   define DCC_UINT8_C  __INT8_C
#   define DCC_UINT16_C __INT16_C
#   define DCC_UINT32_C __INT32_C
#   define DCC_UINT64_C __INT64_C
#else
#   define DCC_UINT8_C(x)  x##u
#   define DCC_UINT16_C(x) x##u
#   define DCC_UINT32_C(x) x##ul
#   define DCC_UINT64_C(x) x##ull
#endif

#define DCC_CONSTANT_BSWAP16(x) \
 ((uint16_t)(x) >> 8 | (uint16_t)(x) << 8)
#define DCC_CONSTANT_BSWAP32(x) \
 ((uint32_t)(x) << 24 | ((uint32_t)(x) & DCC_UINT16_C(0xff00)) << 8 |\
 (((uint32_t)(x) >> 8) & DCC_UINT16_C(0xff00)) | ((uint32_t)(x) >> 24))
#define DCC_CONSTANT_BSWAP64(x) \
 ((uint64_t)(x) << 56 |\
 ((uint64_t)(x) & DCC_UINT64_C(0x000000000000FF00)) << 40 |\
 ((uint64_t)(x) & DCC_UINT64_C(0x0000000000FF0000)) << 24 |\
 ((uint64_t)(x) & DCC_UINT64_C(0x00000000FF000000)) << 8 |\
 ((uint64_t)(x) & DCC_UINT64_C(0x000000FF00000000)) >> 8 |\
 ((uint64_t)(x) & DCC_UINT64_C(0x0000FF0000000000)) >> 24 |\
 ((uint64_t)(x) & DCC_UINT64_C(0x00FF000000000000)) >> 40 |\
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


#if (defined(__GNUC__) || __has_builtin(__builtin_constant_p)) && \
    !defined(__DCC_VERSION__) /* DCC already performs constant folding in __builtin_bswapCC */
#   define DCC_BSWAP16(x) (__builtin_constant_p(x) ? DCC_CONSTANT_BSWAP16(x) : DCC_COMPILER_BSWAP16(x))
#   define DCC_BSWAP32(x) (__builtin_constant_p(x) ? DCC_CONSTANT_BSWAP32(x) : DCC_COMPILER_BSWAP32(x))
#   define DCC_BSWAP64(x) (__builtin_constant_p(x) ? DCC_CONSTANT_BSWAP64(x) : DCC_COMPILER_BSWAP64(x))
#else
#   define DCC_BSWAP16     DCC_COMPILER_BSWAP16
#   define DCC_BSWAP32     DCC_COMPILER_BSWAP32
#   define DCC_BSWAP64     DCC_COMPILER_BSWAP64
#endif

#define DCC_LE2LE16  /* nothing */
#define DCC_BE2LE16  DCC_BSWAP16
#define DCC_LE2LE32  /* nothing */
#define DCC_BE2LE32  DCC_BSWAP32
#define DCC_LE2LE64  /* nothing */
#define DCC_BE2LE64  DCC_BSWAP64
#define DCC_LE2BE16  DCC_BSWAP16
#define DCC_BE2BE16  /* nothing */
#define DCC_LE2BE32  DCC_BSWAP32
#define DCC_BE2BE32  /* nothing */
#define DCC_LE2BE64  DCC_BSWAP64
#define DCC_BE2BE64  /* nothing */

#define DCC_LE2PE16  /* nothing */
#define DCC_BE2PE16  DCC_BSWAP16
#define DCC_LE2PE32(x) ((x)&DCC_UINT32_C(0x0000ffff) << 16 | /* 1234 --> 3412 */ \
                        (x)&DCC_UINT32_C(0xffff0000) >> 16)
#define DCC_BE2PE32(x) (DCC_BSWAP16((x)&DCC_UINT16_C(    0xffff)) | /* 4321 --> 3412 */\
                        DCC_BSWAP16((x)&DCC_UINT32_C(0xffff0000) >> 16) << 16) 
#define DCC_LE2PE64(x) (DCC_LE2PE32((x)&DCC_UINT32_C(0xffffffff)) | /* 12345678 --> 78563412 */\
                        DCC_LE2PE32((x) >> 32) << 32)
#define DCC_BE2PE64(x) (DCC_BSWAP16((x)&DCC_UINT16_C(            0xffff)) | /* 87654321 --> 78563412 */ \
                        DCC_BSWAP16((x)&DCC_UINT32_C(        0xffff0000) >> 16) << 16 | \
                        DCC_BSWAP16((x)&DCC_UINT64_C(0x0000ffff00000000) >> 32) << 32 | \
                        DCC_BSWAP16((x)&DCC_UINT64_C(0xffff000000000000) >> 48) << 48)

#define DCC_PE2LE16  DCC_LE2PE16
#define DCC_PE2BE16  DCC_BE2PE16
#define DCC_PE2PE16  /* nothing */
#define DCC_PE2LE32  DCC_LE2PE32
#define DCC_PE2BE32  DCC_BE2PE32
#define DCC_PE2PE32  /* nothing */
#define DCC_PE2LE64  DCC_LE2PE64
#define DCC_PE2BE64  DCC_BE2PE64
#define DCC_PE2PE64  /* nothing */

/* Little-endian/Big-endian --> Host */
#if DCC_HOST_BYTEORDER == 1234
#   define DCC_LE2H16  DCC_LE2LE16
#   define DCC_BE2H16  DCC_BE2LE16
#   define DCC_PE2H16  DCC_PE2LE16
#   define DCC_LE2H32  DCC_LE2LE32
#   define DCC_BE2H32  DCC_BE2LE32
#   define DCC_PE2H32  DCC_PE2LE32
#   define DCC_LE2H64  DCC_LE2LE64
#   define DCC_BE2H64  DCC_BE2LE64
#   define DCC_PE2H64  DCC_PE2LE64
#   define DCC_H2LE16  DCC_LE2LE16
#   define DCC_H2BE16  DCC_LE2BE16
#   define DCC_H2PE16  DCC_LE2PE16
#   define DCC_H2LE32  DCC_LE2LE32
#   define DCC_H2BE32  DCC_LE2BE32
#   define DCC_H2PE32  DCC_LE2PE32
#   define DCC_H2LE64  DCC_LE2LE64
#   define DCC_H2BE64  DCC_LE2BE64
#   define DCC_H2PE64  DCC_LE2PE64
#elif DCC_HOST_BYTEORDER == 4321
#   define DCC_LE2H16  DCC_LE2BE16
#   define DCC_BE2H16  DCC_BE2BE16
#   define DCC_PE2H16  DCC_PE2BE16
#   define DCC_LE2H32  DCC_LE2BE32
#   define DCC_BE2H32  DCC_BE2BE32
#   define DCC_PE2H32  DCC_PE2BE32
#   define DCC_LE2H64  DCC_LE2BE64
#   define DCC_BE2H64  DCC_BE2BE64
#   define DCC_PE2H64  DCC_PE2BE64
#   define DCC_H2LE16  DCC_BE2LE16
#   define DCC_H2BE16  DCC_BE2BE16
#   define DCC_H2PE16  DCC_BE2PE16
#   define DCC_H2LE32  DCC_BE2LE32
#   define DCC_H2BE32  DCC_BE2BE32
#   define DCC_H2PE32  DCC_BE2PE32
#   define DCC_H2LE64  DCC_BE2LE64
#   define DCC_H2BE64  DCC_BE2BE64
#   define DCC_H2PE64  DCC_BE2PE64
#elif DCC_HOST_BYTEORDER == 3412
#   define DCC_LE2H16  DCC_LE2PE16
#   define DCC_BE2H16  DCC_BE2PE16
#   define DCC_PE2H16  DCC_PE2PE16
#   define DCC_LE2H32  DCC_LE2PE32
#   define DCC_BE2H32  DCC_BE2PE32
#   define DCC_PE2H32  DCC_PE2PE32
#   define DCC_LE2H64  DCC_LE2PE64
#   define DCC_BE2H64  DCC_BE2PE64
#   define DCC_PE2H64  DCC_PE2PE64
#   define DCC_H2LE16  DCC_PE2LE16
#   define DCC_H2BE16  DCC_PE2BE16
#   define DCC_H2PE16  DCC_PE2PE16
#   define DCC_H2LE32  DCC_PE2LE32
#   define DCC_H2BE32  DCC_PE2BE32
#   define DCC_H2PE32  DCC_PE2PE32
#   define DCC_H2LE64  DCC_PE2LE64
#   define DCC_H2BE64  DCC_PE2BE64
#   define DCC_H2PE64  DCC_PE2PE64
#else
#   error "FIXME (Unsupported value for 'DCC_HOST_BYTEORDER')"
#endif

#ifndef DCC_TARGET_DATA_BYTEORDER
#define DCC_TARGET_DATA_BYTEORDER DCC_TARGET_BYTEORDER
#endif

/* Little-endian/Big-endian --> Target */
#if DCC_TARGET_DATA_BYTEORDER == 1234
#   define DCC_LE2T16  DCC_LE2LE16
#   define DCC_BE2T16  DCC_BE2LE16
#   define DCC_PE2T16  DCC_PE2LE16
#   define DCC_LE2T32  DCC_LE2LE32
#   define DCC_BE2T32  DCC_BE2LE32
#   define DCC_PE2T32  DCC_PE2LE32
#   define DCC_LE2T64  DCC_LE2LE64
#   define DCC_BE2T64  DCC_BE2LE64
#   define DCC_PE2T64  DCC_PE2LE64
#   define DCC_T2LE16  DCC_LE2LE16
#   define DCC_T2BE16  DCC_LE2BE16
#   define DCC_T2PE16  DCC_LE2PE16
#   define DCC_T2LE32  DCC_LE2LE32
#   define DCC_T2BE32  DCC_LE2BE32
#   define DCC_T2PE32  DCC_LE2PE32
#   define DCC_T2LE64  DCC_LE2LE64
#   define DCC_T2BE64  DCC_LE2BE64
#   define DCC_T2PE64  DCC_LE2PE64
#elif DCC_TARGET_DATA_BYTEORDER == 4321
#   define DCC_LE2T16  DCC_LE2BE16
#   define DCC_BE2T16  DCC_BE2BE16
#   define DCC_PE2T16  DCC_PE2BE16
#   define DCC_LE2T32  DCC_LE2BE32
#   define DCC_BE2T32  DCC_BE2BE32
#   define DCC_PE2T32  DCC_PE2BE32
#   define DCC_LE2T64  DCC_LE2BE64
#   define DCC_BE2T64  DCC_BE2BE64
#   define DCC_PE2T64  DCC_PE2BE64
#   define DCC_T2LE16  DCC_BE2LE16
#   define DCC_T2BE16  DCC_BE2BE16
#   define DCC_T2PE16  DCC_BE2PE16
#   define DCC_T2LE32  DCC_BE2LE32
#   define DCC_T2BE32  DCC_BE2BE32
#   define DCC_T2PE32  DCC_BE2PE32
#   define DCC_T2LE64  DCC_BE2LE64
#   define DCC_T2BE64  DCC_BE2BE64
#   define DCC_T2PE64  DCC_BE2PE64
#elif DCC_TARGET_DATA_BYTEORDER == 3412
#   define DCC_LE2T16  DCC_LE2PE16
#   define DCC_BE2T16  DCC_BE2PE16
#   define DCC_PE2T16  DCC_PE2PE16
#   define DCC_LE2T32  DCC_LE2PE32
#   define DCC_BE2T32  DCC_BE2PE32
#   define DCC_PE2T32  DCC_PE2PE32
#   define DCC_LE2T64  DCC_LE2PE64
#   define DCC_BE2T64  DCC_BE2PE64
#   define DCC_PE2T64  DCC_PE2PE64
#   define DCC_T2LE16  DCC_PE2LE16
#   define DCC_T2BE16  DCC_PE2BE16
#   define DCC_T2PE16  DCC_PE2PE16
#   define DCC_T2LE32  DCC_PE2LE32
#   define DCC_T2BE32  DCC_PE2BE32
#   define DCC_T2PE32  DCC_PE2PE32
#   define DCC_T2LE64  DCC_PE2LE64
#   define DCC_T2BE64  DCC_PE2BE64
#   define DCC_T2PE64  DCC_PE2PE64
#else
#   error "FIXME (Unsupported value for 'DCC_TARGET_DATA_BYTEORDER')"
#endif

/* Convert between host & target integers. */
#if DCC_TARGET_DATA_BYTEORDER == 1234
#   define DCC_H2T16   DCC_H2LE16
#   define DCC_H2T32   DCC_H2LE32
#   define DCC_H2T64   DCC_H2LE64
#   define DCC_T2H16   DCC_LE2H16
#   define DCC_T2H32   DCC_LE2H32
#   define DCC_T2H64   DCC_LE2H64
#elif DCC_TARGET_DATA_BYTEORDER == 4321
#   define DCC_H2T16   DCC_H2BE16
#   define DCC_H2T32   DCC_H2BE32
#   define DCC_H2T64   DCC_H2BE64
#   define DCC_T2H16   DCC_BE2H16
#   define DCC_T2H32   DCC_BE2H32
#   define DCC_T2H64   DCC_BE2H64
#elif DCC_TARGET_DATA_BYTEORDER == 3412
#   define DCC_H2T16   DCC_H2PE16
#   define DCC_H2T32   DCC_H2PE32
#   define DCC_H2T64   DCC_H2PE64
#   define DCC_T2H16   DCC_PE2H16
#   define DCC_T2H32   DCC_PE2H32
#   define DCC_T2H64   DCC_PE2H64
#else
#   error "FIXME (Unsupported value for 'DCC_TARGET_DATA_BYTEORDER')"
#endif

DCC_DECL_END

#endif /* !GUARD_DCC_TARGET_H */
