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

DCC_DECL_BEGIN

#define DRT_U_STACKRESERVE 0x1000 /* Amount of user-stack bytes to reserve for DRT itself. */
#define DRT_U_FILLER       0xf4   /* hlt - Will raise a GP because DCC isn't meant to run in kernel space. */


INTDEF void DRT_USER DRT_U_WaitEvent(uint32_t code);

/* Fetch data & relocations within the given address range.
 * @param: min_size: The min amount of bytes to fetch. (ZERO isn't allowed)
 * @return: * : The actual amount of bytes successfully fetched. (ZERO indicates an invalid pointer)
 * NOTE: 'DRT_U_FetchSome' will not return until at least ONE(1) byte was fetched.
 * NOTE: 'DRT_U_FetchSome' may not fetch all relocations within the affected code area. */
INTDEF size_t DRT_USER DRT_U_Fetch(void DRT_USER *addr, size_t min_size);
INTDEF size_t DRT_USER DRT_U_FetchSome(void DRT_USER *addr);

#ifndef DCC_NO_ATTRIBUTE_FASTCALL
INTDEF void DRT_USER DCC_ATTRIBUTE_FASTCALL DRT_U_ProbeN(void DRT_USER *p, size_t n);
#else
INTDEF void DRT_USER DRT_U_ProbeN(void);
#endif

DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_Sync(int warn_failure);
DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_SyncAll(void);

/* Find and return the address associated with the given user-section address 'addr'. */
INTDEF struct DCCSection *DRT_FindUserSection(void DRT_USER *addr);

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

/* Using the value in EAX as return value, exit the calling thread. */
INTDEF void DRT_ThreadExit(void);


DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_SRC_DRT_DRT_H */
