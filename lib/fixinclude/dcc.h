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

/* Compiler-intrinsic functionality for DCC */
typedef struct {
	char const *path; /*< [0..1] Path of the associated source file (Don't print when NULL). */
	char const *file; /*< [0..1] File name of the associated source file. */
	void       *__pad1[2];
	int         line; /*< 1-based source line, or ZERO(0) when unknown. */
	int         col;  /*< 1-based source column, or ZERO(0) when unknown. */
	void       *__pad2[2];
} lc_t;

/* Quick and simple solution for retrieving source information
 * about a given address in debug-mode application builds.
 * WARNING: Only units compiled with '-g' include addr2line
 *          debug information, and the whole application
 *          must be linked with '-g' again to ensure that
 *          this function is defined.
 * HINT: When given, '*INFO' is filled even upon failure.
 * @param: IP:   Instruction pointer that should be queried.
 * @param: INFO: User-provided buffer to fill with information.
 * @return: 0:   The given IP could not be found. */
extern _Bool _addr2line(void *__ip, lc_t *__info)
	__asm__("__dcc_dbg_addr2line");





