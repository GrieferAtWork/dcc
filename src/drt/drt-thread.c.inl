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

struct sec_info {
 struct sec_info const *si_next; /*< [0..1] Information about the next section. */
 void                  *si_addr; /*< [1..1] Section start address. */
 size_t                 si_size; /*< Section size. */
 a2l_op_t const        *si_a2l;  /*< [0..1] Addr2line code. */
};

/* Data for a stub-definition of section information used when DRT is enabled.
 * >> Defining this early prevents a compile-cycle dependency that would
 *    otherwise (and correctly so) prevent DRT from continuing execution
 *    until the compiler finishes. */
PRIVATE struct sec_info const stub_secinfo = {
 /* si_next */NULL,
 /* si_addr */NULL,
 /* si_size */0,
 /* si_a2l  */NULL,
};



INTERN EXCEPTION_DISPOSITION NTAPI
DRT_U_W32ExceptionHandler(EXCEPTION_RECORD *ExceptionRecord, PVOID EstablisherFrame,
                          CONTEXT *ContextRecord, PVOID DispatcherContext) {
 (void)EstablisherFrame;
 (void)DispatcherContext;
 if (ExceptionRecord && ContextRecord) {
  DWORD code = ExceptionRecord->ExceptionCode;
  target_ptr_t eip;
  eip = (target_ptr_t)ExceptionRecord->ExceptionAddress;
  if (!eip) eip = (target_ptr_t)ContextRecord->Eip; /* Should never happen... */
  if (code == EXCEPTION_ACCESS_VIOLATION) {
   target_ptr_t access_addr = ExceptionRecord->ExceptionInformation[1];
   /* If a non-faulty address was accessed, try to load it as an operand. */
   if (access_addr < DRT_FAULT_ADDRESS) {
    if (DRT_U_FetchData((void *)access_addr,1))
        return ExceptionContinueExecution;
   }
#if 0 /* TODO: This isn't working yet. */
   {
    /* Wait for symbol relocations within the current instruction.
     * >> TODO: What about relocations to the same symbol in upcoming addresses?
     */
    uintptr_t eip_end = (uintptr_t)x86_instrlen((uint8_t const *)eip);
    if (DRT_U_FetchRelo((void *)eip,(size_t)(eip_end-eip)))
        return ExceptionContinueExecution;
   }
#else
   /* This basically does the same, but does unnecessary checks
    * for mirroring data & doesn't guaranty to capture at least
    * one instruction in the event that said instruction was
    * already mirrored. */
   if (DRT_U_FetchText((void *)eip))
       return ExceptionContinueExecution;
#endif
  } else if (code == EXCEPTION_PRIV_INSTRUCTION) {
   /* When unmapped filler memory is encountered, load more instructions. */
   if (*(uint8_t *)eip == DRT_U_FILLER) {
    if (DRT_U_FetchText((void *)eip) ||
        drt.rt_event.ue_text.te_size_total)
        return ExceptionContinueExecution;
   }
  }
 }
 return ExceptionContinueSearch;
}

INTERN LONG NTAPI
DRT_U_W32VExceptionHandler(EXCEPTION_POINTERS *ExceptionInfo) {
 if (GetCurrentThreadId() == drt.rt_threadid) {
  EXCEPTION_DISPOSITION disp;
  if unlikely(!ExceptionInfo) return EXCEPTION_CONTINUE_SEARCH;
  disp = DRT_U_W32ExceptionHandler(ExceptionInfo->ExceptionRecord,NULL,
                                   ExceptionInfo->ContextRecord,NULL);
  if (disp == ExceptionContinueExecution)
      return EXCEPTION_CONTINUE_EXECUTION;
 }
 return EXCEPTION_CONTINUE_SEARCH;
}

#ifdef _MSC_VER
INTERN void __declspec(naked) DRT_U_ThreadExit(void) {
 __asm mov ecx, eax;
 __asm jmp ExitThread;
}
#else
INTERN void __attribute__((__naked__)) DRT_U_ThreadExit(void) {
 __asm__("mov %eax, %ecx\n"
         "jmp ExitThread\n");
 __builtin_unreachable();
}
#endif


struct DRTStartupInfo {
 struct DCPUState si_cpu;
 size_t           si_argsiz;    /*  */
 uint8_t          si_argdat[1]; /* [si_argsiz] Initial input data. */
};

PRIVATE DWORD WINAPI
DRT_ThreadEntry(struct DRTStartupInfo *info) {
 struct DCPUState initcpu = info->si_cpu;
#ifdef __DCC_VERSION__
#define tib   ((NT_TIB *)__extension__ %fs:0)
#else
 NT_TIB *tib = (NT_TIB *)__readfsdword(0x18);
#endif
 //EXCEPTION_REGISTRATION_RECORD root_handler;
 void **u_stack;
 /* Reserve part of the DRT thread's stack for internal use. */
 u_stack = (void **)((target_ptr_t)tib->StackBase-DRT_U_STACKRESERVE);

#define PUSH(x) *--u_stack = (x)
 /* Push some NULL-pointers to (potentially) ease debugging. */
 PUSH(NULL);
 PUSH(NULL);

 /* Copy argument memory. */
 *(uintptr_t *)&u_stack -= info->si_argsiz;
 memcpy(u_stack,info->si_argdat,info->si_argsiz);

 /* Setup the user-stack in a way that when the DRT entry point returns,
  * it will call 'ExitThread()' with the value from its EAX register. */
 PUSH((void *)&DRT_U_ThreadExit);

 initcpu.cs_gpreg.u_gp[DCC_ASMREG_ESP] = (target_ptr_t)u_stack;
 initcpu.cs_gpreg.u_gp[DCC_ASMREG_EBP] = (target_ptr_t)u_stack;
#undef PUSH
 free(info);

#if 0
 /* Register the root exception handler responsible for wait on data. */
 root_handler.Handler = &DRT_U_W32ExceptionHandler;
 root_handler.Next    = tib->ExceptionList;
 tib->ExceptionList   = &root_handler;
#endif

 /* Register a vectored exception handler, which although doing the same,
  * is (supposedly) called much sooner during exception handling.
  * If I understand it correctly, passing '(ULONG)-1' will
  * set up the handler to be called first '_ALWAYS_'. */
#ifdef __DRT__
 AddVectoredExceptionHandler((ULONG)-2,&DRT_U_W32VExceptionHandler);
#else
 AddVectoredExceptionHandler((ULONG)-1,&DRT_U_W32VExceptionHandler);
#endif
 
 /* Switch to the given CPU state. */
 DRT_SetCPUState(&initcpu);
#undef tib
}


PUBLIC void
DRT_Start(struct DCCSym *__restrict entry_point,
          struct DCPUState const *misc_state,
           void const *argdat, size_t argsize) {
 struct DRTStartupInfo *info = NULL;
 assert(entry_point);
 assertf( (drt.rt_flags&DRT_FLAG_ENABLED),"DRT has not been enabled");
 assertf(!(drt.rt_flags&DRT_FLAG_STARTED),"DRT has already been started");

 /* Copy the initial state. */
 info = (struct DRTStartupInfo *)malloc(DCC_COMPILER_OFFSETOF(struct DRTStartupInfo,si_argdat)+argsize);
 if unlikely(!info) goto err0;
 if (misc_state) info->si_cpu = *misc_state;
 else memset(&info->si_cpu,0,sizeof(struct DCPUState));
 info->si_argsiz = argsize;
 memcpy(info->si_argdat,argdat,argsize);
 assert(drt.rt_event.ue_code == DRT_EVENT_NONE);
 drt.rt_event.ue_sem = CreateSemaphoreA(NULL,0,0x1000,NULL);
 if unlikely(!drt.rt_event.ue_sem ||
              drt.rt_event.ue_sem == INVALID_HANDLE_VALUE)
              goto err1;

 {
  struct DCCSym *a2l_info;
  a2l_info = DCCUnit_NewSyms("__dcc_dbg_secinfo",DCC_SYMFLAG_HIDDEN);
  if (a2l_info && DCCSym_ISFORWARD(a2l_info)) {
   DCCSym_Define(a2l_info,&DCCSection_Abs,
                (target_ptr_t)&stub_secinfo,
                 sizeof(stub_secinfo),
                 DCC_COMPILER_ALIGNOF(stub_secinfo));
  }
 }

 {
  /* Define the DRT-mode version for '_addr2line' from <dcc.h> */
  struct DCCSym *a2l_callback;
  a2l_callback = DCCUnit_NewSyms("__drt_dbg_addr2line",DCC_SYMFLAG_NONE);
  if (a2l_callback && DCCSym_ISFORWARD(a2l_callback)) {
   DCCSym_Define(a2l_callback,&DCCSection_Abs,
                (target_ptr_t)&DRT_U_Addr2line,0,1);
  }
 }

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
  target_siz_t bootstrap_size,bootstrap_sizeok;
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
  DCCSection_RTMirrorData(startup,
                          bootstrap_addr,bootstrap_size,
                         &bootstrap_sizeok,0);
  /* Notify the startup section that we're done writing code. */
  DCCSection_RTDoneWrite(startup,bootstrap_addr,bootstrap_size);
  /* At this point, 'bootstrap_udata' is a pointer to
   * user-space memory containing a bit of bootstrap code
   * who's only purpose is to contain a relocation against
   * the entry point, meaning that once it is called, DRT will
   * do its job and wait until that exact symbol is defined. */
  info->si_cpu.cs_ipreg.i_code = bootstrap_udata;
 }

 drt.rt_flags |= DRT_FLAG_STARTED;
 drt.rt_thread = CreateThread(NULL,DRT_U_STACKRESERVE+drt.rt_stacksize,
                             (LPTHREAD_START_ROUTINE)&DRT_ThreadEntry,
                              info,0,&drt.rt_threadid);
 if unlikely(!drt.rt_thread ||
              drt.rt_thread == INVALID_HANDLE_VALUE)
              goto err2;

 return;
 /* TODO: Warnings? */
err2: CloseHandle(drt.rt_event.ue_sem);
err1: free(info);
err0: drt.rt_flags &= ~(DRT_FLAG_STARTED|DRT_FLAG_ENABLED);
}

DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_THREAD_C_INL */
