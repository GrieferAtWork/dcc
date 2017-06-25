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

#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#   include <dcc_winmin.h>
#else
#   include <pthread.h>
#   include <signal.h>
#   include <sys/ucontext.h>
#endif

DCC_DECL_BEGIN

struct sec_info {
 struct sec_info const *sci_next; /*< [0..1] Information about the next section. */
 void                  *sci_addr; /*< [1..1] Section start address. */
 size_t                 sci_size; /*< Section size. */
 a2l_op_t const        *sci_a2l;  /*< [0..1] Addr2line code. */
};

/* Data for a stub-definition of section information used when DRT is enabled.
 * >> Defining this early prevents a compile-cycle dependency that would
 *    otherwise (and correctly so) prevent DRT from continuing execution
 *    until the compiler finishes. */
PRIVATE struct sec_info const stub_secinfo = {
 /* sci_next */NULL,
 /* sci_addr */NULL,
 /* sci_size */0,
 /* sci_a2l  */NULL,
};


/* Thread-exit callback (executed when the DRT main function returns) */
#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#ifdef _MSC_VER
INTERN void __declspec(naked) DRT_U_ThreadExit(void) {
 __asm mov ecx, eax;
 __asm jmp ExitThread;
}
#else
INTERN void DRT_U_ThreadExit(void);
__asm__("DRT_U_ThreadExit:\n"
        "    mov %eax, %ecx\n"
        "    jmp ExitThread\n"
        ".size DRT_U_ThreadExit, . - DRT_U_ThreadExit\n");
#endif
#else /* Windows... */
#ifdef _MSC_VER
INTERN void __declspec(naked) DRT_U_ThreadExit(void) {
#if DCC_TARGET_SIZEOF_POINTER == 4
 __asm pushd eax;
#else
 __asm pushq rax;
#endif
 __asm jmp pthread_exit;
}
#else
INTERN void DRT_U_ThreadExit(void);
__asm__("DRT_U_ThreadExit:\n"
#if DCC_TARGET_SIZEOF_POINTER == 4
        "    pushl %eax\n"
#else
        "    pushq %rax\n"
#endif
        "    jmp pthread_exit\n"
        ".size DRT_U_ThreadExit, . - DRT_U_ThreadExit\n");
#endif
#endif /* pthread... */





/* Pagefault handler. */
PRIVATE int DRT_USER
DRT_U_IRQ_PF(void DRT_USER *ip, void DRT_USER *addr) {
 /* Ignore faulty data pointers. */
 if ((uintptr_t)addr <  DRT_FAULT_ADDRESS &&
     (uintptr_t)addr >= DCC_TARGET_PAGESIZE) {
  if (DRT_U_FetchData(addr,1)) return 1;
 }
#if 0 /* TODO: This isn't working yet. */
 {
  /* Wait for symbol relocations within the current instruction.
   * >> TODO: What about relocations to the same symbol in upcoming addresses?
   */
  uintptr_t ip_end = (uintptr_t)x86_instrlen((uint8_t const *)ip);
  if (DRT_U_FetchRelo(ip,(size_t)(ip_end-(uintptr_t)ip))) return 1;
 }
#else
 /* This basically does the same, but does unnecessary checks
  * for mirroring data & doesn't guaranty to capture at least
  * one instruction in the event that said instruction was
  * already mirrored. */
 if (DRT_U_FetchText(ip)) return 1;
#endif
 return 0;
}

/* General protection handler. */
PRIVATE int DRT_USER
DRT_U_IRQ_GP(void DRT_USER *ip) {
 if (*(uint8_t *)ip == DRT_U_FILLER) {
  if (DRT_U_FetchText(ip) ||
      drt.rt_event.ue_text.te_size_total)
      return 1;
 }
 return 0;
}



#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#if 0
INTERN EXCEPTION_DISPOSITION NTAPI
DRT_U_W32ExceptionHandler(EXCEPTION_RECORD *ExceptionRecord, PVOID EstablisherFrame,
                          CONTEXT *ContextRecord, PVOID DispatcherContext) {
 (void)EstablisherFrame;
 (void)DispatcherContext;
 if (ExceptionRecord) {
  void DRT_USER *ip = ExceptionRecord->ExceptionAddress;
#if DCC_HOST_CPUI == DCC_CPUI_X86
  if (!ip && ContextRecord)
       ip = (void DRT_USER *)ContextRecord->Eip;
#endif
  switch (ExceptionRecord->ExceptionCode) {
  case EXCEPTION_ACCESS_VIOLATION:
   if (DRT_U_IRQ_PF(ip,(void DRT_USER *)ExceptionRecord->ExceptionInformation[1]))
       return ExceptionContinueExecution;
   break;
  case EXCEPTION_PRIV_INSTRUCTION:
   if (DRT_U_IRQ_GP(ip))
       return ExceptionContinueExecution;
   break;
  default: break;
  }
 }
 return ExceptionContinueSearch;
}
#endif

INTERN LONG NTAPI
DRT_U_W32VExceptionHandler(EXCEPTION_POINTERS *ExceptionInfo) {
 if (GetCurrentThreadId() == drt.rt_threadid && ExceptionInfo) {
  EXCEPTION_RECORD *record; void DRT_USER *ip;
  record = ExceptionInfo->ExceptionRecord;
  if unlikely(!record) goto done;
  ip = record->ExceptionAddress;
#if DCC_HOST_CPUI == DCC_CPUI_X86
  if (!ip && ExceptionInfo->ContextRecord)
       ip = (void DRT_USER *)ExceptionInfo->ContextRecord->Eip;
#endif
  switch (record->ExceptionCode) {
  case EXCEPTION_ACCESS_VIOLATION:
   if (DRT_U_IRQ_PF(ip,(void DRT_USER *)record->ExceptionInformation[1]))
       return EXCEPTION_CONTINUE_EXECUTION;
   break;
  case EXCEPTION_PRIV_INSTRUCTION:
   if (DRT_U_IRQ_GP(ip))
       return EXCEPTION_CONTINUE_EXECUTION;
   break;
  default: break;
  }
 }
done:
 return EXCEPTION_CONTINUE_SEARCH;
}
#else

#if DCC_HOST_CPUI == DCC_CPUI_X86
#if !!(DCC_HOST_CPUF&DCC_CPUF_X86_64)
#   define UCONTEXT_IP(c) ((void *)(c)->uc_mcontext.gregs[REG_RIP])
#   define UCONTEXT_PF(c) ((void *)(c)->uc_mcontext.gregs[REG_CR2])
#else
#   define UCONTEXT_IP(c) ((void *)(c)->uc_mcontext.gregs[REG_EIP])
#   define UCONTEXT_PF(c) ((void *)(c)->uc_mcontext.cr2)
#endif
#else
#   error FIXME
#endif

struct SignalPair {
 struct sigaction sp_old; /*< Old/original action. */
 struct sigaction sp_new; /*< New/override action. */
};

PRIVATE void
SignalPair_ReRaise(struct SignalPair *__restrict pair,
                   int signo, siginfo_t *info, ucontext_t *context) {
 assert(pair);
 if (pair->sp_old.sa_handler == SIG_ERR ||
     pair->sp_old.sa_handler == SIG_DFL
#ifdef SIG_HOLD
  || pair->sp_old.sa_handler == SIG_HOLD
#endif
     ) {
  sigaction(signo,&pair->sp_old,NULL);
  raise(signo); /* Why is there no extended version of this that accepts futher information? */
  sigaction(signo,&pair->sp_new,NULL);
 } else if (pair->sp_old.sa_handler == SIG_IGN) {
  /* Nothing to do here. */
 } else if (pair->sp_old.sa_flags&SA_SIGINFO) {
  /* Call a signal action. */
  (*pair->sp_old.sa_sigaction)(signo,info,context);
 } else {
  /* Call a regular, old signal handler. */
  (*pair->sp_old.sa_handler)(signo);
 }
}

PRIVATE struct SignalPair DRT_SigSegv;
PRIVATE struct SignalPair DRT_SigIll;

INTERN void
DRT_U_PosixSigAction(int signo, siginfo_t *info, ucontext_t *context) {
 /* There's no point in trying to deny it.
  * >> We _have_ to use function that are supposedly illegal to
  *    use, most notably a 'sem_wait()' further down the line.
  *    And there is _absolutely_ _no_ _way_ _around_ _it_!
  * So we might as well face the facts and hope that the
  * associated APIs aren't hugging their standards too hard,
  * but simply _work_ as one would expect them to, even from
  * inside a signal handler such as this one.
  */
 if (pthread_equal(drt.rt_thread,pthread_self()) && context) {
  if (signo == SIGSEGV) {
   if (!DRT_U_IRQ_PF(UCONTEXT_IP(context),
                     UCONTEXT_PF(context)))
        goto dont_handle;
  } else if (signo == SIGILL) {
   /* SIGILL is also send for privileged instructions. */
   if (!DRT_U_IRQ_GP(UCONTEXT_IP(context)))
        goto dont_handle;
  } else {
   goto dont_handle;
  }
  /* XXX: Is this correct for continue-execution? */
  return;
 }
dont_handle:
      if (signo == SIGSEGV) SignalPair_ReRaise(&DRT_SigSegv,signo,info,context);
 else if (signo == SIGILL)  SignalPair_ReRaise(&DRT_SigIll,signo,info,context);
 else {
  /* Shouldn't get here... (Restore the default signal handler and call it) */
  signal(signo,SIG_DFL);
  raise(signo);
 }
}
#endif


struct DRTStartupInfo {
 struct DCPUState si_cpu;
 size_t           si_argsiz;    /*  */
 uint8_t          si_argdat[1]; /* [si_argsiz] Initial input data. */
};

#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
PRIVATE DWORD WINAPI
DRT_ThreadEntry(struct DRTStartupInfo *info)
#else
PRIVATE void *DRT_ThreadEntry(struct DRTStartupInfo *info)
#endif
{
 void **u_stack;
 struct DCPUState initcpu = info->si_cpu;
#if !!(DCC_HOST_OS&DCC_OS_F_UNIX)
 pthread_attr_t attr;
 void *stackaddr; size_t stacksize;
 pthread_getattr_np(pthread_self(),&attr);
 pthread_attr_getstack(&attr,&stackaddr,&stacksize);
 /* Reserve part of the DRT thread's stack for internal use. */
 u_stack = (void **)((uintptr_t)stackaddr+stacksize-DRT_U_STACKRESERVE);
#else
#ifdef __DCC_VERSION__
#define tib   ((NT_TIB *)__extension__ %fs:0)
#else
 NT_TIB *tib = (NT_TIB *)__readfsdword(0x18);
#endif
 /* Reserve part of the DRT thread's stack for internal use. */
 u_stack = (void **)((target_ptr_t)tib->StackBase-DRT_U_STACKRESERVE);
#endif

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

#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#if 0
 EXCEPTION_REGISTRATION_RECORD root_handler;
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
 AddVectoredExceptionHandler((ULONG)-1-(__DRT__+0),&DRT_U_W32VExceptionHandler);
#else
 AddVectoredExceptionHandler((ULONG)-1,&DRT_U_W32VExceptionHandler);
#endif
#else
 /* Change pthread_cancel() to directly terminate the current thread.
  * Additional code is in place to try and work
  * around the user potentially overwriting this. */
 pthread_setcanceltype(PTHREAD_CANCEL_ASYNCHRONOUS,NULL);
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
 if unlikely(!info)   { DCC_AllocFailed(DCC_COMPILER_OFFSETOF(struct DRTStartupInfo,si_argdat)+argsize); goto err0; }
 if (misc_state) info->si_cpu = *misc_state;
 else memset(&info->si_cpu,0,sizeof(struct DCPUState));
 info->si_argsiz = argsize;
 memcpy(info->si_argdat,argdat,argsize);
 assert(drt.rt_event.ue_code == DRT_EVENT_NONE);

 /* Create the event semaphore. */
 if unlikely(!DCC_semaphore_init(drt.rt_event.ue_sem,0))
              goto err1; /* TODO: Warning? */

#if !!(DCC_HOST_OS&DCC_OS_F_UNIX)
 /* Setup signal actions to-be taken for fetching more data/code */
 memset(&DRT_SigSegv,0,sizeof(struct SignalPair));
 DRT_SigSegv.sp_new.sa_sigaction = (void (*)(int,siginfo_t *,void *))&DRT_U_PosixSigAction;
 DRT_SigSegv.sp_new.sa_flags     = SA_SIGINFO|SA_NODEFER;
 DRT_SigIll = DRT_SigSegv;
 if (sigaction(SIGSEGV,&DRT_SigSegv.sp_new,&DRT_SigSegv.sp_old) < 0) goto err1;
 if (sigaction(SIGILL,&DRT_SigIll.sp_new,&DRT_SigIll.sp_old) < 0) goto err1;
#endif

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
#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
 drt.rt_thread = CreateThread(NULL,DRT_U_STACKRESERVE+drt.rt_stacksize,
                             (LPTHREAD_START_ROUTINE)&DRT_ThreadEntry,
                              info,0,(LPDWORD)&drt.rt_threadid);
 if unlikely(!drt.rt_thread ||
              drt.rt_thread == INVALID_HANDLE_VALUE)
              goto err2; /* TODO: Warning? */
#else
 {
  int error;
  pthread_attr_t attr,*pattr = &attr;
  if (pthread_attr_init(&attr) < 0) pattr = NULL;
  else {
   pthread_attr_setstacksize(&attr,DRT_U_STACKRESERVE+drt.rt_stacksize);
  }
  error = pthread_create(&drt.rt_thread,pattr,
                        (void *(*)(void *))&DRT_ThreadEntry,
                         info);
  if (pattr) pthread_attr_destroy(pattr);
  if (error < 0) goto err2; /* TODO: Warning? */
 }
#endif
 return;
err2: DCC_semaphore_quit(drt.rt_event.ue_sem);
err1: free(info);
err0: drt.rt_flags &= ~(DRT_FLAG_STARTED|DRT_FLAG_ENABLED);
}

DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_THREAD_C_INL */
