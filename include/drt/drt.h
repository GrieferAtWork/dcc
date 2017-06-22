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
#ifndef GUARD_DRT_DRT_H
#define GUARD_DRT_DRT_H 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>


/* 
 *  ...
 *  TODO: Here be explanation of how DRT (Direct-Run-Time) works
 *  ...
 *
 */


#if DCC_CONFIG_HAVE_DRT
#include <stdint.h>
#ifndef _WIN32
#include <pthread.h>
#endif

DCC_DECL_BEGIN

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4201)
#endif

#define DCPUGPREGISTER_OFFSETOF_AX   0
#define DCPUGPREGISTER_OFFSETOF_AL   0
#define DCPUGPREGISTER_OFFSETOF_AH   1
#define DCPUGPREGISTER_OFFSETOF_CX  (DCC_TARGET_SIZEOF_POINTER)
#define DCPUGPREGISTER_OFFSETOF_CL  (DCC_TARGET_SIZEOF_POINTER)
#define DCPUGPREGISTER_OFFSETOF_CH  (DCC_TARGET_SIZEOF_POINTER+1)
#define DCPUGPREGISTER_OFFSETOF_DX  (DCC_TARGET_SIZEOF_POINTER*2)
#define DCPUGPREGISTER_OFFSETOF_DL  (DCC_TARGET_SIZEOF_POINTER*2)
#define DCPUGPREGISTER_OFFSETOF_DH  (DCC_TARGET_SIZEOF_POINTER*2+1)
#define DCPUGPREGISTER_OFFSETOF_BX  (DCC_TARGET_SIZEOF_POINTER*3)
#define DCPUGPREGISTER_OFFSETOF_BL  (DCC_TARGET_SIZEOF_POINTER*3)
#define DCPUGPREGISTER_OFFSETOF_BH  (DCC_TARGET_SIZEOF_POINTER*3+1)
#define DCPUGPREGISTER_OFFSETOF_SP  (DCC_TARGET_SIZEOF_POINTER*4)
#define DCPUGPREGISTER_OFFSETOF_BP  (DCC_TARGET_SIZEOF_POINTER*5)
#define DCPUGPREGISTER_OFFSETOF_SI  (DCC_TARGET_SIZEOF_POINTER*6)
#define DCPUGPREGISTER_OFFSETOF_DI  (DCC_TARGET_SIZEOF_POINTER*7)
#define DCPUGPREGISTER_SIZEOF       (DCC_TARGET_SIZEOF_POINTER*8)
union DCPUGPRegister {
#if DCC_TARGET_HASI(I_X86)
#if DCC_TARGET_HASF(F_X86_64)
#define REGISTER8(name) \
 union{ uint64_t u_r##name; uint32_t u_e##name; uint16_t u_##name; \
         int64_t s_r##name;  int32_t s_e##name;  int16_t s_##name; \
 }
#define REGISTER4(name) \
 union{ uint64_t u_r##name##x; uint32_t u_e##name##x; uint16_t u_##name##x; struct { uint8_t u_##name##l,u_##name##h; }; \
         int64_t s_r##name##x;  int32_t s_e##name##x;  int16_t s_##name##x; struct {  int8_t s_##name##l,u_##name##h; }; \
 }
#else /* F_X86_64 */
#define REGISTER8(name) \
 union{ uint32_t u_e##name; uint16_t u_##name; \
         int32_t s_e##name;  int16_t s_##name; \
 }
#define REGISTER4(name) \
 union{ uint32_t u_e##name##x; uint16_t u_##name##x; struct { uint8_t u_##name##l,u_##name##h; }; \
         int32_t s_e##name##x;  int16_t s_##name##x; struct {  int8_t s_##name##l,s_##name##h; }; \
 }
#endif /* !F_X86_64 */
struct{
 /* Pretty names for general purpose registers. */
 REGISTER4(a);  /* AX */
 REGISTER4(c);  /* CX */
 REGISTER4(d);  /* DX */
 REGISTER4(b);  /* BX */
 REGISTER8(sp); /* SP */
 REGISTER8(bp); /* BP */
 REGISTER8(si); /* SI */
 REGISTER8(di); /* DI */
};
#undef REGISTER8
#undef REGISTER4
 target_off_t s_gp[8]; /* Signed GP registers. */
 target_ptr_t u_gp[8]; /* Unsigned GP registers. */
#else
#error FIXME
#endif
};

#define DCPUEFREGISTER_OFFSETOF_FLAGS 0
#define DCPUEFREGISTER_SIZEOF         4
union DCPUEFRegister {
 uint16_t i_flags;
 uint32_t i_eflags;
#if DCC_TARGET_HASF(F_X86_64)
#undef DCPUEFREGISTER_SIZEOF
#define DCPUEFREGISTER_SIZEOF 8
 uint64_t i_rflags;
#endif
};

#define DCPUIPREGISTER_OFFSETOF_IP 0
#define DCPUIPREGISTER_SIZEOF      4
union DCPUIPRegister {
 uint16_t i_ip;
 uint32_t i_eip;
#if DCC_TARGET_HASF(F_X86_64)
#undef DCPUIPREGISTER_SIZEOF
#define DCPUIPREGISTER_SIZEOF 8
 uint64_t i_rip;
#endif
 void    *i_code; /* The absolute code address of the runtime.
                   * This member is usually be located within a known section. */
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif


/* Structure used to store the CPU state. */

#define DCPUSTATE_OFFSETOF_GPREG   0
#define DCPUSTATE_OFFSETOF_EFREG   DCPUGPREGISTER_SIZEOF
#define DCPUSTATE_OFFSETOF_IPREG  (DCPUGPREGISTER_SIZEOF+DCPUEFREGISTER_SIZEOF)
#define DCPUSTATE_SIZEOF (DCPUGPREGISTER_SIZEOF+DCPUEFREGISTER_SIZEOF+DCPUIPREGISTER_SIZEOF)
struct DCPUState {
 union DCPUGPRegister cs_gpreg; /*< General purpose register. */
 union DCPUEFRegister cs_efreg; /*< CPU Eflags register. */
 union DCPUIPRegister cs_ipreg; /*< Instruction pointer register. */
 /* TODO: Floating point registers? */
};


#ifdef _WIN32
typedef void     *DCC(thread_t);
typedef void     *DCC(semaphore_t);
#else
typedef pthread_t DCC(thread_t);
typedef sem_t     DCC(semaphore_t);
#endif


#define DRT_EVENT_NONE      0x00000000 /*< No event. */
#define DRT_EVENT_FETCH     0x00000001 /*< Load data & relocations in a given address range. */
#define DRT_EVENT_FETCHSOME 0x00000002 /*< Similar to 'DRT_EVENT_FETCH', but it's ok if only ~something~ could be fetched. */

struct DRTUserEvent {
 DCC(semaphore_t)   ue_sem;      /*< Semaphore that the user-thread may wait for when  */
 /*atomic*/uint32_t ue_code;     /*< Event code (One of 'DRT_EVENT_*'). */
 union{
  struct { /* DRT_EVENT_FETCH|DRT_EVENT_FETCHSOME */
   void  *f_addr;   /*< [in] Start of memory to fetch. */
union{
   size_t f_size;   /*< [DRT_EVENT_FETCH][in]      Min amount of bytes to fetch. */
   size_t f_norelc; /*< [DRT_EVENT_FETCHSOME][out] Amount of relocations that could not be resolved. */
};
   size_t f_okrelc; /*< [out] Amount of successfully relocations. */
   size_t f_datsz;  /*< [out] Amount of filled bytes of memory (ZERO(0) if no data could be fetched). */
  } ue_fetch;
 };
};


#define DRT_FLAG_NONE     0x00000000
#define DRT_FLAG_ENABLED  0x00000001 /*< Set if DRT is enabled. */
#define DRT_FLAG_STARTED  0x00000002 /*< Set if DRT has been started. */
#define DRT_FLAG_JOINING  0x00000004 /*< Set once the compiler starts joining DRT. */
#define DRT_FLAG_JOINING2 0x00000008 /*< Set after 'DRT_FLAG_JOINING' when the compiler isn't going to do anything anymore. */

#define DRT_DEFAULT_STACKSIZE  0x00004000 /*< Default stack size. */
#define DRT_DEFAULT_MAXSECTION 0x00100000 /*< Default max-section size. */
#define DRT_DEFAULT_BASEADDR   0x40000000 /*< Starting address for DRT section memory. */

#define DRT_FAULT_ADDRESS 0xfffff001 /* Faulty address used for filling in unresolved relocations in user-code. */

struct DRT {
 uint32_t            rt_flags;      /*< [flags] RT flags (Set of 'DRT_FLAG_*'). */
 DCC(target_siz_t)   rt_stacksize;  /*< Initial stack size for the RT thread. */
 DCC(target_siz_t)   rt_maxsection; /*< The min amount of addressable bytes that
                                     *  should be reserved between RT section. */
 uint8_t DRT_USER   *rt_baseaddr;   /*< The effective image base-address for DRT code.
                                     *  This address usually lies in high memory to
                                     *  ensure that the DRT has a lot of addressable
                                     *  memory to call its own. */
 uint8_t DRT_USER   *rt_nextaddr;   /*< The base address of the next uninitialized section. */
 DCC(thread_t)       rt_thread;     /*< [valid_if(DRT_FLAG_STARTED)] RT thread handle. */
 struct DRTUserEvent rt_event;      /*< [valid_if(DRT_FLAG_STARTED)] The pending user-thread event. */
};



DCCDAT struct DRT DRT_Current;

/* Initialize/Finalize DRT data structures. */
DCCFUN void DRT_Init(void);
DCCFUN void DRT_Quit(void);

/* Enable DRT (called when '-d' is passed on the commandline) */
#define DRT_Enable()  (void)(DRT_Current.rt_flags |= DRT_FLAG_ENABLED)

/* Start the DRT execution thread.
 * The thread will begin execution at 'entry_point'.
 * All registers will be pre-initialized to those
 * described by 'misc_state', with a new exceptions:
 *  - Kernel-hooks have been configured for lazy initialization of section data.
 *  - The instruction pointer is fixed to point at 'entry_point'
 *  - A unique stack of dynamic memory has been allocated for
 *    the thread meaning that X86's 'SP' and 'BP' pointers
 *    now point to the base of that stack.
 *  - An empty stack-frame with a return address pointing
 *    to code that will terminate the application will
 *    be called should the given 'entry_point' ever return.
 * WARNING: The caller is responsible to enable DRT because calling this function.
 */
DCCFUN void DRT_Start(struct DCCSym *__restrict entry_point,
                      struct DCPUState const *misc_state);

/* Activate the given CPU state. */
DCCFUN DCC_ATTRIBUTE_NORETURN void
DRT_SetCPUState(struct DCPUState const *__restrict state);

#define DRT_ENABLED()  (DRT_Current.rt_flags&DRT_FLAG_ENABLED)
#define DRT_STARTED()  (DRT_Current.rt_flags&DRT_FLAG_STARTED)

/* DRT synchronization point.
 * This function should be called periodically from the compiler thread
 * to give DRT a chance for synchronizing section data, as well as
 * exchange other messages such as relocation information without
 * the need of expensive individual synchronization of said data
 * by implicitly ensuring thread safety by having the compiler thread
 * post its data to DRT on its own behalf instead of DRT trying to
 * read data randomly and either doing some hacky shenanigans or
 * needing to setup hundreds of locks around every section or symbol.
 * NOTE: This function is a no-op when DRT is disabled.
 * 
 * 'DRT_SyncAll' behaves similar to 'DRT_Sync', but will not
 * return until all user-requests for data have been handled.
 * With that in mind, 'DRT_SyncAll' will also eventually join()
 * the user-thread, meaning that it should only be called once
 * all input files have been compiled.
 * @return: DRT_SYNC_NONE: No data was synchronized.
 * @return: DRT_SYNC_OK:   Data was synchronized.
 * @return: DRT_SYNC_FAIL: Failed to synchronize data due to undefined symbols / missing data.
 */
#ifdef __INTELLISENSE__
DCCFUN int DRT_Sync(void);
DCCFUN int DRT_SyncAll(void);
#else
DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_Sync(int warn_failure);
DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_SyncAll(void);
#define DRT_Sync()    (DRT_STARTED() ? DRT_H_Sync(0) : 0)
#define DRT_SyncAll() (DRT_STARTED() ? DRT_H_SyncAll() : 0)
#endif
#define DRT_SYNC_NONE 0
#define DRT_SYNC_OK   1
#define DRT_SYNC_FAIL 2


/* Allocate/Free virtual memory, or set permissions.
 * @param: vaddr:       The virtual address to allocate memory at.
 *                      [DRT_VMall] When 'DRT_VANY', let the kernel decide where to allocate memory.
 * @param: n_bytes:     The amount of bytes of allocate/update/free.
 * @param: perm:        Memory permissions (Set of 'DCC_SYMFLAG_SEC_(R|W|X)')
 * @return: * :         Virtual base address of allocated memory (always equal to 'vaddr' if not 'DRT_VANY')
 * @return: DRT_VERROR: Failed to allocate memory for one reason or another (_NO_ lexer error is set). */
DCCFUN void DRT_USER *DRT_VMall(void DRT_USER *vaddr, size_t n_bytes, DCC(symflag_t) prot);
DCCFUN void DRT_USER *DRT_VProt(void DRT_USER *vaddr, size_t n_bytes, DCC(symflag_t) prot);
DCCFUN void           DRT_VFree(void DRT_USER *vaddr, size_t n_bytes);
#define DRT_VANY   ((void DRT_USER *)(uintptr_t)0)
#if DCC_HOST_OS == DCC_OS_WINDOWS
#define DRT_VERROR ((void DRT_USER *)(uintptr_t)0)
#else
#define DRT_VERROR ((void DRT_USER *)(uintptr_t)-1)
#endif

#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
DCCDAT struct DRT drt;
#else
#define drt  DRT_Current
#endif
#endif

DCC_DECL_END
#else /* DCC_CONFIG_HAVE_DRT */
#define DRT_Init()            (void)0
#define DRT_Quit()            (void)0
#define DRT_Sync()            (void)0
#define DRT_SyncAll()         (void)0
#define DRT_ENABLED()          DCC_MACRO_COND(0)
#define DRT_STARTED()          DCC_MACRO_COND(0)
#define DRT_SignalSymbol(sym)           (void)0
#define DRT_VERROR           ((void DRT_USER *)(uintptr_t)0)
#endif /* !DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_H */
