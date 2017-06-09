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

#if __has_include_next(<stdint.h>)
#include_next <stdint.h>
#endif

/* Fixed/optimized system header <stdint.h> for DCC */
#undef intmax_t
#undef uintmax_t
#undef int8_t
#undef uint8_t
#undef int16_t
#undef uint16_t
#undef int32_t
#undef uint32_t
#undef int64_t
#undef uint64_t
#undef int_least8_t
#undef uint_least8_t
#undef int_least16_t
#undef uint_least16_t
#undef int_least32_t
#undef uint_least32_t
#undef int_least64_t
#undef uint_least64_t
#undef int_fast8_t
#undef uint_fast8_t
#undef int_fast16_t
#undef uint_fast16_t
#undef int_fast32_t
#undef uint_fast32_t
#undef int_fast64_t
#undef uint_fast64_t
#undef intptr_t
#undef uintptr_t

typedef __INTMAX_TYPE__       intmax_t;
typedef __UINTMAX_TYPE__      uintmax_t;
typedef __INT8_TYPE__         int8_t;
typedef __UINT8_TYPE__        uint8_t;
typedef __INT16_TYPE__        int16_t;
typedef __UINT16_TYPE__       uint16_t;
typedef __INT32_TYPE__        int32_t;
typedef __UINT32_TYPE__       uint32_t;
typedef __INT64_TYPE__        int64_t;
typedef __UINT64_TYPE__       uint64_t;
typedef __INT_LEAST8_TYPE__   int_least8_t;
typedef __UINT_LEAST8_TYPE__  uint_least8_t;
typedef __INT_LEAST16_TYPE__  int_least16_t;
typedef __UINT_LEAST16_TYPE__ uint_least16_t;
typedef __INT_LEAST32_TYPE__  int_least32_t;
typedef __UINT_LEAST32_TYPE__ uint_least32_t;
typedef __INT_LEAST64_TYPE__  int_least64_t;
typedef __UINT_LEAST64_TYPE__ uint_least64_t;
typedef __INT_FAST8_TYPE__    int_fast8_t;
typedef __UINT_FAST8_TYPE__   uint_fast8_t;
typedef __INT_FAST16_TYPE__   int_fast16_t;
typedef __UINT_FAST16_TYPE__  uint_fast16_t;
typedef __INT_FAST32_TYPE__   int_fast32_t;
typedef __UINT_FAST32_TYPE__  uint_fast32_t;
typedef __INT_FAST64_TYPE__   int_fast64_t;
typedef __UINT_FAST64_TYPE__  uint_fast64_t;
typedef __INTPTR_TYPE__       intptr_t;
typedef __UINTPTR_TYPE__      uintptr_t;


#define INTMAX_MIN        __INTMAX_MIN__
#define INTMAX_MAX        __INTMAX_MAX__
#define UINTMAX_MAX       __UINTMAX_MAX__
#define INT8_MIN          __INT8_MIN__
#define INT16_MIN         __INT16_MIN__
#define INT32_MIN         __INT32_MIN__
#define INT64_MIN         __INT64_MIN__
#define INT8_MAX          __INT8_MAX__
#define INT16_MAX         __INT16_MAX__
#define INT32_MAX         __INT32_MAX__
#define INT64_MAX         __INT64_MAX__
#define UINT8_MAX         __UINT8_MAX__
#define UINT16_MAX        __UINT16_MAX__
#define UINT32_MAX        __UINT32_MAX__
#define UINT64_MAX        __UINT64_MAX__
#define INT_LEAST8_MIN    __INT_LEAST8_MIN__
#define INT_LEAST16_MIN   __INT_LEAST16_MIN__
#define INT_LEAST32_MIN   __INT_LEAST32_MIN__
#define INT_LEAST64_MIN   __INT_LEAST64_MIN__
#define INT_LEAST8_MAX    __INT_LEAST8_MAX__
#define INT_LEAST16_MAX   __INT_LEAST16_MAX__
#define INT_LEAST32_MAX   __INT_LEAST32_MAX__
#define INT_LEAST64_MAX   __INT_LEAST64_MAX__
#define UINT_LEAST8_MAX   __UINT_LEAST8_MAX__
#define UINT_LEAST16_MAX  __UINT_LEAST16_MAX__
#define UINT_LEAST32_MAX  __UINT_LEAST32_MAX__
#define UINT_LEAST64_MAX  __UINT_LEAST64_MAX__
#define INT_FAST8_MIN     __INT_FAST8_MIN__
#define INT_FAST16_MIN    __INT_FAST16_MIN__
#define INT_FAST32_MIN    __INT_FAST32_MIN__
#define INT_FAST64_MIN    __INT_FAST64_MIN__
#define INT_FAST8_MAX     __INT_FAST8_MAX__
#define INT_FAST16_MAX    __INT_FAST16_MAX__
#define INT_FAST32_MAX    __INT_FAST32_MAX__
#define INT_FAST64_MAX    __INT_FAST64_MAX__
#define UINT_FAST8_MAX    __UINT_FAST8_MAX__
#define UINT_FAST16_MAX   __UINT_FAST16_MAX__
#define UINT_FAST32_MAX   __UINT_FAST32_MAX__
#define UINT_FAST64_MAX   __UINT_FAST64_MAX__
#define INTPTR_MIN        __INTPTR_MIN__
#define INTPTR_MAX        __INTPTR_MAX__
#define UINTPTR_MAX       __UINTPTR_MAX__

#define SIZE_MAX          __SIZE_MAX__
#define PTRDIFF_MIN       __PTRDIFF_MIN__
#define PTRDIFF_MAX       __PTRDIFF_MAX__
#define SIG_ATOMIC_MIN    __SIG_ATOMIC_MIN__
#define SIG_ATOMIC_MAX    __SIG_ATOMIC_MAX__
#define WCHAR_MIN         __WCHAR_MIN__
#define WCHAR_MAX         __WCHAR_MAX__
#define WINT_MIN          __WINT_MIN__
#define WINT_MAX          __WINT_MAX__

#define INTMAX_C          __INTMAX_C
#define UINTMAX_C         __UINTMAX_C
#define INT8_C            __INT8_C
#define INT16_C           __INT16_C
#define INT32_C           __INT32_C
#define INT64_C           __INT64_C
#define UINT8_C           __UINT8_C
#define UINT16_C          __UINT16_C
#define UINT32_C          __UINT32_C
#define UINT64_C          __UINT64_C
