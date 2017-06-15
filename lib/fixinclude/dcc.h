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
	char const *name; /*< [0..1] Name of the surrounding function symbol. */
	void       *__pad1[1];
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
 * @return: 0:   The given IP could not be found.
 * @return: !0:  Successfully queried information about IP. */
extern _Bool _addr2line(void *__ip, lc_t *__info)
	__asm__("__dcc_dbg_addr2line");


/* 64-bit arithmetic runtime functions. */
extern __INT64_TYPE__  __ashlti3(__INT64_TYPE__ __x, int __shift);          /* 'return (uint64_t|int64_t)x << shift' */
extern __INT64_TYPE__  __ashrti3(__INT64_TYPE__ __x, int __shift);          /* 'return (int64_t)x >> shift' */
extern __UINT64_TYPE__ __lshrti3(__UINT64_TYPE__ __x, int __shift);         /* 'return (uint64_t)x >> shift' */
extern __UINT64_TYPE__ __udivti3(__UINT64_TYPE__ __x, __UINT64_TYPE__ __y); /* 'return (uint64_t)x / y' */
extern __INT64_TYPE__  __divti3(__INT64_TYPE__ __x, __INT64_TYPE__ __y);    /* 'return (int64_t)x / y' */
extern __UINT64_TYPE__ __umodti3(__UINT64_TYPE__ __x, __INT64_TYPE__ __y);  /* 'return (uint64_t)x % y' */
extern __INT64_TYPE__  __modti3(__INT64_TYPE__ __x, __INT64_TYPE__ __y);    /* 'return (int64_t)x % y' */
extern __INT64_TYPE__  __multi3(__INT64_TYPE__ __x, __INT64_TYPE__ __y);    /* 'return (uint64_t|int64_t)x * y' */



struct [[__packed__]] __cpuinfo {
#if defined(__i386__) || defined(__x86_64__)
	unsigned __int32 __cpuid_1_eax;
	unsigned __int32 __cpuid_1_ebx;
	unsigned __int32 __cpuid_1_ecx;
	unsigned __int32 __cpuid_1_edx;
	unsigned __int32 __cpuid_7_ebx;
	unsigned __int32 __cpuid_7_edx;
	unsigned __int32 __cpuid_7_ecx;
	unsigned __int32 __cpuid_0_eax;
union [[__packed__]] {
struct [[__packed__]] {
	unsigned __int32 __cpuid_0_ebx;
	unsigned __int32 __cpuid_0_edx;
	unsigned __int32 __cpuid_0_ecx;
	unsigned __int8  __cpuid_zero;
};
	char             __cpuid_vendor[12];
};
#else
	int              __placeholder;
#endif
};


/* CPU Information structure allocated and initialized
 * if the application calls '__builtin_cpu_init()'
 * NOTE: Access to this structure should always be performed using:
 * >> int __builtin_cpu_is(char const *cpuname);
 * >> int __builtin_cpu_supports(char const *feature);
 */
extern struct __cpuinfo __cpu_info [[__weak__]];





