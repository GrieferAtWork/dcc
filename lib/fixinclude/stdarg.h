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

#if __has_include_next(<stdarg.h>)
#include_next <stdarg.h>
#endif

/* Fixed/optimized system header <stdarg.h> for DCC */
#undef va_list
typedef __builtin_va_list va_list;

/* void va_start(va_list &ap); */
/* void va_start(va_list &ap, type &last_argument); */
#define va_start   __builtin_va_start

/* void va_end(va_list &ap); */
#define va_end     __builtin_va_end

/* void va_copy(va_list &dst_ap, va_list &src_ap); */
#define va_copy    __builtin_va_copy
#define __va_copy  __builtin_va_copy

/* t va_arg(va_list &ap, type t); */
#define va_arg     __builtin_va_arg


