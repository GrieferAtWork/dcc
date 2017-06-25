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
#ifndef GUARD_SRC_DRT_DRT_H
#define GUARD_SRC_DRT_DRT_H 1

#include <dcc/common.h>
#include <dcc/target.h>
#if DCC_CONFIG_HAVE_DRT

#include <drt/drt.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#ifdef _MSC_VER
#   include <intrin.h>
#   define MEMORY_BARRIER() _ReadWriteBarrier()
#else
#   define MEMORY_BARRIER() __asm__ __volatile__("" : : : "memory");
#endif

#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#   include <dcc_winmin.h>
#else
#   include <errno.h>
#   include <pthread.h>
#   define GetLastError()               errno
#   define ExitThread(code)             pthread_exit((void *)(code))
#if __has_include(<asm/cachectl.h>) || \
    defined(HAVE_ASM_CACHECTL) || \
    defined(HAVE_INCLUDE_ASM_CACHECTL)
#   include <asm/cachectl.h>
#   define FlushInstructionCache(h,p,s) cacheflush(p,s,ICACHE)
#else
#   define NO_FlushInstructionCache
#   define FlushInstructionCache(h,p,s) (void)0
#endif
#endif


DCC_DECL_BEGIN

DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_Sync(int warn_failure);
DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_SyncAll(target_int_t *exitcode);

/* Find and return the address associated with the given user-section address 'addr'. */
INTDEF struct DCCSection *DRT_FindUserSection(void DRT_USER *addr);

INTDEF int
DCCSection_RTMirrorData(struct DCCSection *__restrict self,
                        target_ptr_t addr, target_siz_t size,
                        target_siz_t *__restrict psize_ok,
                        int warn_failure);

/* Try to resolve the given relocation 'rel' from 'sec',
 * targeting memory in 'uaddr' with section data from 'haddr'. */
INTDEF int
DRT_ResolveRel(uint8_t DRT_USER *__restrict uaddr,
               uint8_t DRT_HOST *__restrict haddr,
               struct DCCRel const *__restrict rel,
               struct DCCSection const *__restrict sec,
               int warn_failure);

/* Setup a faulty relocation that will trigger
 * a DRT data fetch when dereferenced. */
INTDEF void
DRT_FaultRel(uint8_t DRT_USER *__restrict uaddr,
             struct DCCRel const *__restrict rel);


/* User-code functions.
 * All of these functions can be called either
 * directly, or indirectly from DRT user-space. */

#define DRT_U_STACKRESERVE 0x1000 /* Amount of user-stack bytes to reserve for DRT itself. */
#define DRT_U_FILLER       0xf4   /* hlt - Will raise a #GP because DCC isn't meant to run in kernel space. */


INTDEF void DRT_USER DRT_U_WaitEvent(uint32_t code);

/* Fetch data & relocations within the given address range.
 * @param: min_size: The min amount of bytes to fetch. (ZERO isn't allowed)
 * @return: 0/1: Indicate success/failure of fetching data. */
INTDEF int DRT_USER DRT_U_FetchText(void DRT_USER *addr);
INTDEF int DRT_USER DRT_U_FetchData(void DRT_USER *addr, size_t min_size);
INTDEF int DRT_USER DRT_U_FetchRelo(void DRT_USER *addr, size_t size);

#ifndef DCC_NO_ATTRIBUTE_FASTCALL
INTDEF void DRT_USER DCC_ATTRIBUTE_FASTCALL DRT_U_ProbeN(void DRT_USER *p, size_t n);
#else
INTDEF void DRT_USER DRT_U_ProbeN(void);
#endif

/* Using the value in EAX as return value, exit the calling thread. */
INTDEF void DRT_U_ThreadExit(void);


typedef struct {
 char DRT_USER const *lc_path; /*< [0..1] Path of the associated source file (Don't print when NULL). */
 char DRT_USER const *lc_file; /*< [0..1] File name of the associated source file. */
 char DRT_USER const *lc_name; /*< [0..1] Name of the surrounding function symbol. */
 void                *lc_pad1[1];
 target_int_t         lc_line; /*< 1-based source line, or ZERO(0) when unknown. */
 target_int_t         lc_col;  /*< 1-based source column, or ZERO(0) when unknown. */
 void                *lc_pad2[2];
} target_lc_t;

/* The function that '_addr2line' from <dcc.h> is linked against in DRT mode. */
INTDEF target_bool_t DRT_USER DRT_U_Addr2line(void DRT_USER *ip, target_lc_t *info);


DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_SRC_DRT_DRT_H */
