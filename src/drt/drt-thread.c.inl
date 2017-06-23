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
#ifndef GUARD_DRT_DRT_THREAD_C_INL
#define GUARD_DRT_DRT_THREAD_C_INL 1

#include <dcc/common.h>
#include <dcc/target.h>
#if DCC_CONFIG_HAVE_DRT

#include <dcc/compiler.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <drt/drt.h>

#include "../dcc/x86_util.h"
#include "drt.h"

#include <stdio.h>

DCC_DECL_BEGIN

INTERN EXCEPTION_DISPOSITION NTAPI
DRT_U_W32ExceptionHandler(EXCEPTION_RECORD *ExceptionRecord, PVOID EstablisherFrame,
                          CONTEXT *ContextRecord, PVOID DispatcherContext) {
 (void)EstablisherFrame;
 (void)DispatcherContext;
 if (ExceptionRecord && ContextRecord) {
  DWORD code = ExceptionRecord->ExceptionCode;
  target_ptr_t eip_end,eip = (target_ptr_t)ContextRecord->Eip;
  if (code == EXCEPTION_ACCESS_VIOLATION) {
   target_ptr_t access_addr = ExceptionRecord->ExceptionInformation[1];
   /* If a non-faulty address was accessed, load it as an operand. */
   if (access_addr < DRT_FAULT_ADDRESS) {
    if (!DRT_U_FetchSome((void *)access_addr)) goto done;
   }
   /* Wait for data and relocations within the caller's current instruction. */
   /* TODO: This is a dirty hack to get most probe code working, but
    *       filling in the relocation to one symbol must somehow cause
    *       all other undefined relocations to the same symbol to also
    *       be filled in!
    * >> testl $0, foo // If the relocation to 'foo' is fixed here,
    * >>               // we must ensure it is fixed below as well!
    * >> call foo
    */
load_eip:
   eip_end = (target_ptr_t)x86_instrlen((uint8_t const *)eip);
   eip_end = (target_ptr_t)x86_instrlen((uint8_t const *)eip_end);
   /* If we've managed to load data or relocations, try again. */
   if (!DRT_U_Fetch((void *)eip,eip_end-eip))
        goto done;
   return ExceptionContinueExecution;
  } else if (code == EXCEPTION_PRIV_INSTRUCTION) {
   /* When unmapped filler memory is encountered, load more instructions. */
   if (*(uint8_t *)eip == DRT_U_FILLER) goto load_eip;
  }
 }
done:
 return ExceptionContinueSearch;
}

PRIVATE DWORD WINAPI
DRT_ThreadEntry(struct DCPUState *pstate) {
 struct DCPUState my_state = *pstate;
 NT_TIB *tib = (NT_TIB *)__readfsdword(0x18);
 EXCEPTION_REGISTRATION_RECORD root_handler;
 void *u_stack;
 free(pstate);
 /* Reserve part of the DRT thread's stack for internal use. */
 u_stack = (void *)((target_ptr_t)tib->StackBase-DRT_U_STACKRESERVE);

 my_state.cs_gpreg.u_gp[DCC_ASMREG_ESP] =
 my_state.cs_gpreg.u_gp[DCC_ASMREG_EBP] = (target_ptr_t)u_stack;

 /* Register the root exception handler responsible for wait on data. */
 root_handler.Handler = &DRT_U_W32ExceptionHandler;
 root_handler.Next    = tib->ExceptionList;
 tib->ExceptionList   = &root_handler;
 
 /* Switch to the given CPU state. */
 DRT_SetCPUState(&my_state);
}


PUBLIC void
DRT_Start(struct DCCSym *__restrict entry_point,
          struct DCPUState const *misc_state) {
 struct DCPUState *state = NULL;
 assert(entry_point);
 assertf( (drt.rt_flags&DRT_FLAG_ENABLED),"DRT has not been enabled");
 assertf(!(drt.rt_flags&DRT_FLAG_STARTED),"DRT has already been started");

 /* Copy the initial state. */
 state = (struct DCPUState *)malloc(sizeof(struct DCPUState));
 if unlikely(!state) goto err0;
 if (misc_state) *state = *misc_state;
 else memset(state,0,sizeof(struct DCPUState));
 assert(drt.rt_event.ue_code == DRT_EVENT_NONE);
 drt.rt_event.ue_sem = CreateSemaphoreA(NULL,0,0x1000,NULL);
 if unlikely(!drt.rt_event.ue_sem || drt.rt_event.ue_sem == INVALID_HANDLE_VALUE) goto err1;

 /* This part gets a bit hacky, because we create a small section
  * who's only purpose is going to be to hold a small bit of code
  * containing a relocation to the entry point symbol.
  * >> jmp entry_point
  * This is required because in the (likely) event that 'entry_point'
  * isn't defined yet, we somehow need to trick DRT into setting up
  * a compiler event that waits for this exact symbol to become
  * defined, essentially meaning that we need some existing code
  * that we can (ab-)use to somehow contain a relocation against
  * the entry point symbol. */
 {
  struct DCCSection *startup,*old_text,*prev_text;
  uint8_t DRT_USER *bootstrap_udata;
  target_ptr_t bootstrap_addr;
  target_siz_t bootstrap_size;
  struct DCCMemLoc entry_loc;
  startup = DCCUnit_NewSecs(".drt.init",DCC_SYMFLAG_SEC(1,0,1,0,0,0));
  if unlikely(!startup) goto err2;
  /* Redirect the text section towards our startup
   * section, so we can easily write some bootstrap code. */
  prev_text = unit.u_prev;
  old_text  = DCCUnit_SetCurr(startup);
  bootstrap_addr = t_addr;

  /* Now, simply generate a small bit of code for jumping to 'entry_point'. */
  entry_loc.ml_off = 0;
  entry_loc.ml_reg = DCC_RC_CONST;
  entry_loc.ml_sym = entry_point;
  DCCDisp_LocJmp(&entry_loc);

  /* Figure out how big the bootstrap is & restore the old text section. */
  bootstrap_size = t_addr-bootstrap_addr;
  DCCUnit_SetCurr(old_text);
  unit.u_prev = prev_text;
 
  /* Allocate the proper user-memory for the bootstrap code. */
  bootstrap_udata = (uint8_t DRT_USER *)DCCSection_RTAlloc(startup,bootstrap_addr,bootstrap_size,1);
  if unlikely(bootstrap_udata == DRT_VERROR) goto err2;
  /* Copy the bootstrap code. */
  if unlikely(!DCCSection_RTCopy(startup,bootstrap_udata,
                                 bootstrap_addr,bootstrap_size)) goto err2;
  /* Notify the startup section that we're done writing code. */
  DCCSection_RTDoneWrite(startup,bootstrap_addr,bootstrap_size);
  /* At this point, 'bootstrap_udata' is a pointer to
   * user-space memory containing a bit of bootstrap code
   * who's only purpose is to contain a relocation against
   * the entry point, meaning that once it is called, DRT will
   * do its job and wait until that exact symbol is defined. */
  state->cs_ipreg.i_code = bootstrap_udata;
 }

 drt.rt_flags |= DRT_FLAG_STARTED;
 drt.rt_thread = CreateThread(NULL,DRT_U_STACKRESERVE+drt.rt_stacksize,
                             (LPTHREAD_START_ROUTINE)&DRT_ThreadEntry,
                              state,0,NULL);
 if unlikely(!drt.rt_thread || drt.rt_thread == INVALID_HANDLE_VALUE) goto err2;


 return;
 /* TODO: Warnings? */
err2: CloseHandle(drt.rt_event.ue_sem);
err1: free(state);
err0: drt.rt_flags &= ~(DRT_FLAG_STARTED|DRT_FLAG_ENABLED);
}

DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_THREAD_C_INL */
