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

#if __has_include_next(<ctype.h>)
#include_next <ctype.h>
#else
#include "__stdinc.h"

__IMP __WUNUSED int (isalpha)(int);
__IMP __WUNUSED int (isupper)(int);
__IMP __WUNUSED int (islower)(int);
__IMP __WUNUSED int (isdigit)(int);
__IMP __WUNUSED int (isxdigit)(int);
__IMP __WUNUSED int (isspace)(int);
__IMP __WUNUSED int (ispunct)(int);
__IMP __WUNUSED int (isalnum)(int);
__IMP __WUNUSED int (isprint)(int);
__IMP __WUNUSED int (isgraph)(int);
__IMP __WUNUSED int (iscntrl)(int);
__IMP __WUNUSED int (toupper)(int);
__IMP __WUNUSED int (tolower)(int);

#if __STDLIB_VERSION__ >= 201112L
__IMP __WUNUSED int (isblank)(int);
#endif

#endif
