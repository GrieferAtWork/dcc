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
#ifndef GUARD_DRT_DRT_USER_C_INL
#define GUARD_DRT_DRT_USER_C_INL 1

#include <dcc/common.h>
#include <dcc/target.h>
#if DCC_CONFIG_HAVE_DRT

#include <dcc/compiler.h>
#include <dcc/unit.h>
#include <drt/drt.h>

#include "drt.h"

#include <stdio.h>

DCC_DECL_BEGIN


#ifdef _MSC_VER
#if !!(DCC_HOST_CPUF&DCC_CPUF_X86_64)
#define XCX rcx
#define XDX rdx
#else
#define XCX ecx
#define XDX edx
#endif
INTERN void DRT_USER __declspec(naked)
DCC_ATTRIBUTE_FASTCALL DRT_U_ProbeN(void DRT_USER *p, size_t n) {
 (void)p; (void)n;
 __asm pushfd;
 /* TODO: Test all the bytes! */
 __asm test dword ptr [XCX], 0x0;
 __asm popfd;
 __asm ret;
}
#undef XCX
#undef XCX
#else
#error TODO
#endif



INTERN void DRT_USER DRT_U_WaitEvent(uint32_t code) {
 assert(drt.rt_event.ue_code == DRT_EVENT_NONE);
 assert(code                 != DRT_EVENT_NONE);
 drt.rt_event.ue_code = code;
 MEMORY_BARRIER();
 if (drt.rt_flags&DRT_FLAG_JOINING2) {
  /* The compiler thread is no more. - We're in charge now! */
  /* Really hacky: Do the synchronization ourself. */
  if (DRT_H_Sync(1) != DRT_SYNC_OK) {
   /* Exit the thread if nothing else can be done!
    * NOTE: Warnings were already emit by 'DRT_H_Sync'. */
   ExitThread(1);
  }
 } else if (WaitForSingleObject(drt.rt_event.ue_sem,INFINITE) == WAIT_FAILED) {
  fprintf(stderr,"Failed to wait for DRT event\n");
  exit(1);
 }
}

INTERN size_t DRT_USER
DRT_U_Fetch(void DRT_USER *addr, size_t size) {
 memset(&drt.rt_event.ue_fetch,0,sizeof(drt.rt_event.ue_fetch));
 drt.rt_event.ue_fetch.f_addr = addr;
 drt.rt_event.ue_fetch.f_size = size;
 DRT_U_WaitEvent(DRT_EVENT_FETCH);
 return drt.rt_event.ue_fetch.f_datsz;
}
INTERN size_t DRT_USER
DRT_U_FetchSome(void DRT_USER *addr) {
 memset(&drt.rt_event.ue_fetch,0,sizeof(drt.rt_event.ue_fetch));
 drt.rt_event.ue_fetch.f_addr = addr;
 DRT_U_WaitEvent(DRT_EVENT_FETCHSOME);
 return drt.rt_event.ue_fetch.f_datsz;
}

DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_USER_C_INL */
