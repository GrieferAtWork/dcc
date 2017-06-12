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

#ifndef __has_include_next
#define __has_include_next(x) 0
#endif

#if __has_include_next(<malloc.h>)
#include_next <malloc.h>
#else
#include <__stdinc.h>

#include <features.h>
#include <stddef.h>
#include <stdio.h>

__IMP __WUNUSED void *(malloc)(__SIZE_TYPE__ __size);
__IMP __WUNUSED void *(calloc)(__SIZE_TYPE__ __nmemb, __SIZE_TYPE__ __size);
__IMP __WUNUSED void *(realloc)(void *__ptr, __SIZE_TYPE__ __size);
__IMP void (free)(void *__ptr);
__IMP void (cfree)(void *__ptr)
#if !defined(__CRT_GLIBC) && !defined(__CRT_KOS)
	__asm__("free")
#endif
;

#if defined(__CRT_GLIBC) || defined(__CRT_KOS)
__IMP __WUNUSED void *(memalign)(__SIZE_TYPE__ __alignment, __SIZE_TYPE__ __size);
__IMP __WUNUSED void *(valloc)(__SIZE_TYPE__ __size);
__IMP __WUNUSED void *(pvalloc)(__SIZE_TYPE__ __size);

struct mallinfo {
	int arena;    /* non-mmapped space allocated from system */
	int ordblks;  /* number of free chunks */
	int smblks;   /* number of fastbin blocks */
	int hblks;    /* number of mmapped regions */
	int hblkhd;   /* space in mmapped regions */
	int usmblks;  /* maximum total allocated space */
	int fsmblks;  /* space available in freed fastbin blocks */
	int uordblks; /* total allocated space */
	int fordblks; /* total free space */
	int keepcost; /* top-most, releasable (via malloc_trim) space */
};

__IMP __WUNUSED struct mallinfo mallinfo (void) __THROW;

#ifndef M_MXFAST
#define M_MXFAST  1    /* maximum request size for "fastbins" */
#endif
#ifndef M_NLBLKS
#define M_NLBLKS  2    /* UNUSED in this malloc */
#endif
#ifndef M_GRAIN
#define M_GRAIN   3    /* UNUSED in this malloc */
#endif
#ifndef M_KEEP
#define M_KEEP    4    /* UNUSED in this malloc */
#endif

#define M_TRIM_THRESHOLD (-1)
#define M_TOP_PAD        (-2)
#define M_MMAP_THRESHOLD (-3)
#define M_MMAP_MAX       (-4)
#define M_CHECK_ACTION   (-5)
#define M_PERTURB        (-6)
#define M_ARENA_TEST     (-7)
#define M_ARENA_MAX      (-8)

__IMP int (mallopt)(int __param, int __val);
__IMP int (malloc_trim)(__SIZE_TYPE__ __pad);
__IMP __SIZE_TYPE__ (malloc_usable_size)(void *__ptr);
__IMP void (malloc_stats)(void);

#else /* __CRT_GLIBC */
__STDLIB_UNSUPPORTED("memalign")
__STDLIB_UNSUPPORTED("valloc")
__STDLIB_UNSUPPORTED("pvalloc")
__STDLIB_UNSUPPORTED("mallinfo")
#ifdef __CRT_MSVC
__IMP __WUNUSED __SIZE_TYPE__ (malloc_usable_size)(void *__ptr) __asm__("_msize");
#else
__STDLIB_UNSUPPORTED("malloc_usable_size")
#endif
#define mallopt(param,val) ((void)(param),(void)(val),0)
#define malloc_trim(pad)   ((void)(pad),0)
#define malloc_stats(pad)  ((void)0)
#endif

#if defined(__CRT_GLIBC)
__IMP int (malloc_info)(int __options, FILE *__fp);
__IMP __WUNUSED void *(malloc_get_state)(void);
__IMP int (malloc_set_state)(void *__ptr);
__IMP void (*__malloc_initialize_hook)(void);
__IMP void (*__free_hook)(void *__ptr, const void *);
__IMP void *(*__malloc_hook)(__SIZE_TYPE__ __size, const void *);
__IMP void *(*__realloc_hook)(void *__ptr, __SIZE_TYPE__ __size, const void *);
__IMP void *(*__memalign_hook)(__SIZE_TYPE__ __alignment, __SIZE_TYPE__ __size, const void *);
__IMP void (*__after_morecore_hook)(void);
__IMP void (__malloc_check_init)(void);
#else /* __CRT_GLIBC */
__STDLIB_UNSUPPORTED("malloc_info")
__STDLIB_UNSUPPORTED("malloc_get_state")
__STDLIB_UNSUPPORTED("malloc_set_state")
__STDLIB_UNSUPPORTED("__malloc_initialize_hook")
__STDLIB_UNSUPPORTED("__free_hook")
__STDLIB_UNSUPPORTED("__malloc_hook")
__STDLIB_UNSUPPORTED("__realloc_hook")
__STDLIB_UNSUPPORTED("__memalign_hook")
__STDLIB_UNSUPPORTED("__after_morecore_hook")
__STDLIB_UNSUPPORTED("__malloc_check_init")
#endif /* !__CRT_GLIBC */

#endif
