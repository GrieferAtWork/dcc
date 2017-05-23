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

#if __has_include_next(<setjmp.h>)
#include_next <setjmp.h>
#endif

/* Fixed/optimized system header <setjmp.h> for DCC */
#ifdef __SIZEOF_JMP_BUF__

#undef jmp_buf

typedef struct {
	__INT8_TYPE__ __buf[__SIZEOF_JMP_BUF__];
} jmp_buf[1];

/* Use builtin functions to allow for compiler-optimizations. */

/* int setjmp(jmp_buf buf); */
#define setjmp(buf)   __builtin_setjmp((buf))

/* void longjmp(jmp_buf buf, int sig) __attribute__((noreturn)); */
#define longjmp(buf,sig)  __builtin_longjmp((buf),(sig))
#endif
