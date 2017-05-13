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

#if __has_include_next(<limits.h>)
#include_next <limits.h>
#endif

/* Fixed/optimized system header <limits.h> for DCC */
#define CHAR_BIT    __CHAR_BIT__
#define SCHAR_MIN   __SCHAR_MIN__
#define SCHAR_MAX   __SCHAR_MAX__
#define UCHAR_MAX   __UCHAR_MAX__
#define CHAR_MIN    __CHAR_MIN__
#define CHAR_MAX    __CHAR_MAX__
#define SHRT_MIN    __SHRT_MIN__
#define SHRT_MAX    __SHRT_MAX__
#define USHRT_MAX   __USHRT_MAX__
#define INT_MIN     __INT_MIN__
#define INT_MAX     __INT_MAX__
#define UINT_MAX    __UINT_MAX__
#define LONG_MIN    __LONG_MIN__
#define LONG_MAX    __LONG_MAX__
#define ULONG_MAX   __ULONG_MAX__
#define LLONG_MIN   __LLONG_MIN__
#define LLONG_MAX   __LLONG_MAX__
#define ULLONG_MAX  __ULLONG_MAX__
