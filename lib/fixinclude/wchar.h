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

#if __has_include_next(<wchar.h>)
#include_next <wchar.h>
#endif

/* Fixed/optimized system header <wchar.h> for DCC */
#undef size_t
#undef wchar_t
#undef wint_t
typedef __SIZE_TYPE__  size_t;
typedef __WCHAR_TYPE__ wchar_t;
typedef __WINT_TYPE__  wint_t;

#define NULL       __NULL__
#define WCHAR_MIN  __WCHAR_MIN__
#define WCHAR_MAX  __WCHAR_MAX__




