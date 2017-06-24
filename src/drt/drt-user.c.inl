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


#if !!(DCC_HOST_CPUF&DCC_CPUF_X86_64)
#define XCX rcx
#define XDX rdx
#else
#define XCX ecx
#define XDX edx
#endif
#ifdef _MSC_VER
INTERN void DRT_USER __declspec(naked)
DCC_ATTRIBUTE_FASTCALL DRT_U_ProbeN(void DRT_USER *p, size_t n) {
 (void)p; (void)n;
 __asm pushfd;
 /* TODO: Test all the bytes! */
 __asm test dword ptr [XCX], 0x0;
 __asm popfd;
 __asm ret;
}
#else
INTERN void DRT_USER __attribute__((__naked__))
DCC_ATTRIBUTE_FASTCALL DRT_U_ProbeN(void DRT_USER *p, size_t n) {
 /* TODO: Test all the bytes! */
 __asm__("pushfl\n"
         "testl $0, (%" DCC_PP_STR(XCX) ")\n"
         "popfl\n"
         "ret\n");
 __builtin_unreachable();
}
#endif
#undef XDX
#undef XCX



INTERN void DRT_USER DRT_U_WaitEvent(uint32_t code) {
 assert(drt.rt_event.ue_code == DRT_EVENT_NONE);
 assert(code                 != DRT_EVENT_NONE);
 drt.rt_event.ue_code = code;
 MEMORY_BARRIER();
 if (drt.rt_flags&DRT_FLAG_JOINING2) {
  /* The compiler thread is no more. - We're in charge now! */
  /* Really hacky: Do the synchronization ourself. */
  if (DRT_H_Sync(1) == DRT_SYNC_UNRESOLVED) {
   /* Exit the thread if nothing else can be done!
    * NOTE: Warnings were already emit by 'DRT_H_Sync'. */
   ExitThread(1);
  }
 } else {
#if 0 /*< Don't do this to prevent deadlocks after a sync was triggered inside of printf() & friends. */
  /* Flush various global buffers before starting to wait.
   * >> To be honest, this is mainly done to ensure
   *    the printf() example working flawlessly. */
  fflush(stdout);
  fflush(stderr);
#endif
  if (WaitForSingleObject(drt.rt_event.ue_sem,INFINITE) == WAIT_FAILED) {
   fprintf(stderr,"Failed to wait for DRT event (%d)\n",GetLastError());
   ExitThread(1);
  }
 }
}

INTERN int DRT_USER
DRT_U_FetchText(void DRT_USER *addr) {
 drt.rt_event.ue_text.te_addr       = addr;
 drt.rt_event.ue_text.te_relc_ok    = 0;
 drt.rt_event.ue_text.te_size_ok    = 0;
 drt.rt_event.ue_text.te_size_total = 0;
 DRT_U_WaitEvent(DRT_EVENT_MIRROR_TEXT);
 return drt.rt_event.ue_text.te_relc_ok ||
        drt.rt_event.ue_text.te_size_ok ||
        drt.rt_event.ue_text.te_size_total;
}
INTERN int DRT_USER
DRT_U_FetchData(void DRT_USER *addr, size_t size) {
 drt.rt_event.ue_data.de_addr = addr;
 drt.rt_event.ue_data.de_size = size;
 DRT_U_WaitEvent(DRT_EVENT_MIRROR_DATA);
 return /*drt.rt_event.ue_data.de_size &&*/
        drt.rt_event.ue_data.de_size != (size_t)-1;
}
INTERN int DRT_USER
DRT_U_FetchRelo(void DRT_USER *addr, size_t size) {
 drt.rt_event.ue_relo.re_addr = addr;
 drt.rt_event.ue_relo.re_size = size;
 DRT_U_WaitEvent(DRT_EVENT_MIRROR_RELO);
 return drt.rt_event.ue_relo.re_size != 0;
}

DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_USER_C_INL */
