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

#if __has_include_next(<strings.h>)
#   include_next <strings.h>
#elif defined(__INTELLISENSE__)
#   include "../include/strings.h"
#endif

#include <features.h>

#if _SVID_SOURCE || _BSD_SOURCE || \
    _POSIX_C_SOURCE >= 200809L || \
    _XOPEN_SOURCE >= 700
#   define ffs(x)  __builtin_ffs((x))
#endif

