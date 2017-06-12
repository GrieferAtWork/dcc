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

#if __has_include_next(<mcheck.h>)
#include_next <mcheck.h>
#else

enum mcheck_status {
	MCHECK_DISABLED = -1,         /* Consistency checking is not turned on.  */
	MCHECK_OK,                    /* Block is fine.  */
	MCHECK_FREE,                  /* Block freed twice.  */
	MCHECK_HEAD,                  /* Memory before the block was clobbered.  */
	MCHECK_TAIL                   /* Memory after the block was clobbered.  */
};

#if defined(__CRT_GLIBC)
/* Link original GLIBC functions. */
__IMP int (mcheck)(void (*__abortfunc)(enum mcheck_status));
__IMP int (mcheck_pedantic)(void (*__abortfunc)(enum mcheck_status));
__IMP void (mcheck_check_all)(void);
__IMP enum mcheck_status (mprobe)(void *__ptr);
__IMP void (mtrace)(void);
__IMP void (muntrace)(void);
#elif defined(__CRT_MSVC)
/* Try to emulate mcheck-behavior as best as we can... */
__IMP extern int __msvc_crt_dbg_flag __asm__("_crtDbgFlag");
__IMP void mcheck_check_all(void) __asm__("_CrtCheckMemory");
#define mcheck(__abortfunc)          (__msvc_crt_dbg_flag |= (0x01),0)
#define mcheck_pedantic(__abortfunc) (__msvc_crt_dbg_flag |= (0x01|0x04),0)
#define mprobe(p)                    (mcheck_check_all(),MCHECK_OK)
#define mtrace()                     (void)(__msvc_crt_dbg_flag |= (0x01|0x04))
#define muntrace()                   (void)(__msvc_crt_dbg_flag &= ~(0x04))
#else
/* Link stub-macros. */
#define mcheck(__abortfunc)                0
#define mcheck_pedantic(__abortfunc)       0
#define mcheck_check_all()           (void)0
#define mprobe(__ptr)                 MCHECK_OK
#define mtrace()                     (void)0
#define muntrace()                   (void)0
#endif

__END_DECLS
#endif /* mcheck.h */
