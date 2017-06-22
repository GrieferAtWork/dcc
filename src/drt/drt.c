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
#ifndef GUARD_DRT_DRT_C
#define GUARD_DRT_DRT_C 1

#include <dcc/common.h>
#include <dcc/target.h>
#if DCC_CONFIG_HAVE_DRT

#include <dcc/compiler.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <drt/drt.h>

#include "drt.h"

#if DCC_HOST_OS == DCC_OS_WINDOWS
#   include <Windows.h>
#else
#   include <sys/mman.h>
#endif

DCC_DECL_BEGIN

PUBLIC struct DRT DRT_Current;

#define PROT_MASK   (DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)
#define PROT(prot) (((prot)&PROT_MASK) >> 16)
#if DCC_HOST_OS == DCC_OS_WINDOWS
PRIVATE DWORD const mall_prot[] = {
 /* [PROT(0)]                                                     = */0,
 /* [PROT(DCC_SYMFLAG_SEC_R)]                                     = */PAGE_READONLY,
 /* [PROT(DCC_SYMFLAG_SEC_W)]                                     = */PAGE_READWRITE,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)]                   = */PAGE_READWRITE,
 /* [PROT(DCC_SYMFLAG_SEC_X)]                                     = */PAGE_EXECUTE,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_X)]                   = */PAGE_EXECUTE_READ,
 /* [PROT(DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)]                   = */PAGE_EXECUTE_READWRITE,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)] = */PAGE_EXECUTE_READWRITE,
};

PUBLIC void DRT_USER *
DRT_VMall(void DRT_USER *vaddr, size_t n_bytes, symflag_t prot) {
 return VirtualAlloc(vaddr,n_bytes,MEM_COMMIT|MEM_RESERVE,mall_prot[PROT(prot)]);
}
PUBLIC void DRT_USER *
DRT_VProt(void DRT_USER *vaddr, size_t n_bytes, symflag_t prot) {
 DWORD old_prot;
 return VirtualProtect(vaddr,n_bytes,mall_prot[PROT(prot)],&old_prot) ? vaddr : DRT_VERROR;
}
PUBLIC void
DRT_VFree(void DRT_USER *vaddr, size_t n_bytes) {
 VirtualFree(vaddr,n_bytes,MEM_DECOMMIT);
}
#else
#ifndef PROT_NONE
#define PROT_NONE 0
#endif
#ifndef MAP_PRIVATE
#define MAP_PRIVATE 0
#endif
#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
#define MAP_ANONYMOUS MAP_ANON
#endif
#ifndef MAP_FIXED
#error "Cannot allocate fixed memory (DCC must be compiled with '-DDCC_CONFIG_HAVE_DRT=0')"
#endif

#ifndef __KOS__
#define HAVE_MPROTECT
#endif

PRIVATE DWORD const mall_prot[] = {
 /* [PROT(0)]                                                     = */PROT_NONE,
#ifdef HAVE_MPROTECT
 /* [PROT(DCC_SYMFLAG_SEC_R)]                                     = */PROT_READ,
 /* [PROT(DCC_SYMFLAG_SEC_W)]                                     = */PROT_WRITE,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)]                   = */PROT_READ|PROT_WRITE,
 /* [PROT(DCC_SYMFLAG_SEC_X)]                                     = */PROT_EXEC,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_X)]                   = */PROT_READ|PROT_EXEC,
 /* [PROT(DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)]                   = */PROT_WRITE|PROT_EXEC,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)] = */PROT_READ|PROT_WRITE|PROT_EXEC,
#else
 /* [PROT(DCC_SYMFLAG_SEC_R)]                                     = */PROT_READ|PROT_WRITE,
 /* [PROT(DCC_SYMFLAG_SEC_W)]                                     = */PROT_WRITE,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)]                   = */PROT_READ|PROT_WRITE,
 /* [PROT(DCC_SYMFLAG_SEC_X)]                                     = */PROT_WRITE|PROT_EXEC,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_X)]                   = */PROT_READ|PROT_WRITE|PROT_EXEC,
 /* [PROT(DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)]                   = */PROT_WRITE|PROT_EXEC,
 /* [PROT(DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)] = */PROT_READ|PROT_WRITE|PROT_EXEC,
#endif
};


PUBLIC void DRT_USER *
DRT_VMall(void DRT_USER *vaddr, size_t n_bytes, symflag_t prot) {
#ifdef MAP_ANONYMOUS
 return mmap(vaddr,n_bytes,mall_prot[PROT(prot)],
             MAP_PRIVATE|MAP_ANONYMOUS|
            (vaddr != DRT_VANY ? MAP_FIXED : 0),-1,0);
#else
 static int fd_null = -1;
 if (fd_null < 0) fd_null = open("/dev/null",O_RDONLY);
 if (fd_null < 0) return DRT_VERROR;
 return mmap(vaddr,n_bytes,mall_prot[PROT(prot)],
             MAP_PRIVATE|
            (vaddr != DRT_VANY ? MAP_FIXED : 0),fd_null,0);
#endif
}
PUBLIC void DRT_USER *
DRT_VProt(void DRT_USER *vaddr, size_t n_bytes, symflag_t prot) {
#ifdef HAVE_MPROTECT
 if (mprotect(vaddr,n_bytes,mall_prot[PROT(prot)]) < 0) return DRT_VERROR;
 return vaddr;
#else
 (void)n_bytes,(void)prot;
 return vaddr;
#endif
}
PUBLIC void
DRT_VFree(void DRT_USER *vaddr, size_t n_bytes) {
 munmap(vaddr,n_bytes);
}
#endif



#if DCC_HOST_CPUI == DCC_CPUI_X86
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4731)
PUBLIC /*__declspec(naked)*/ DCC_ATTRIBUTE_NORETURN
void DRT_SetCPUState(struct DCPUState const *__restrict state) {
#ifdef _WIN64
 __asm mov rax, state;
 __asm push dword ptr [eax+DCPUSTATE_OFFSETOF_EFREG+DCPUEFREGISTER_OFFSETOF_FLAGS];
 __asm popfq;
 __asm mov rcx, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_CX];
 __asm mov rdx, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_DX];
 __asm mov rbx, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_BX];
 __asm mov rsp, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_SP];
 __asm mov rbp, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_BP];
 __asm mov rsi, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_SI];
 __asm mov rdi, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_DI];
 __asm push dword ptr [rax+DCPUSTATE_OFFSETOF_IPREG+DCPUIPREGISTER_OFFSETOF_IP];
 __asm mov rax, dword ptr [rax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_AX];
 __asm ret;
#else
 __asm mov eax, state;
 __asm push dword ptr [eax+DCPUSTATE_OFFSETOF_EFREG+DCPUEFREGISTER_OFFSETOF_FLAGS];
 __asm popfd;
 __asm mov ecx, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_CX];
 __asm mov edx, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_DX];
 __asm mov ebx, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_BX];
 __asm mov esp, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_SP];
 __asm mov ebp, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_BP];
 __asm mov esi, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_SI];
 __asm mov edi, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_DI];
 __asm push dword ptr [eax+DCPUSTATE_OFFSETOF_IPREG+DCPUIPREGISTER_OFFSETOF_IP];
 __asm mov eax, dword ptr [eax+DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_AX];
 __asm ret;
#endif
}
#undef LOAD_STATE
#pragma warning(pop)
#else
PUBLIC __attribute__((__naked__)) DCC_ATTRIBUTE_NORETURN
void DRT_SetCPUState(struct DCPUState const *__restrict state) {
#if !!(DCC_HOST_CPUF&DCC_CPUF_X86_64)
#   define LEVEL "r"
#else
#   define LEVEL "e"
#endif
 __asm__("push " DCC_PP_STR(DCPUSTATE_OFFSETOF_EFREG+DCPUEFREGISTER_OFFSETOF_FLAGS) "(%%" LEVEL "ax)\n\t"
         "popfd\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_CX) "(%%" LEVEL "ax), %%" LEVEL "cx\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_DX) "(%%" LEVEL "ax), %%" LEVEL "dx\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_BX) "(%%" LEVEL "ax), %%" LEVEL "bx\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_SP) "(%%" LEVEL "ax), %%" LEVEL "sp\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_BP) "(%%" LEVEL "ax), %%" LEVEL "bp\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_SI) "(%%" LEVEL "ax), %%" LEVEL "si\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_DI) "(%%" LEVEL "ax), %%" LEVEL "di\n\t"
         "push " DCC_PP_STR(DCPUSTATE_OFFSETOF_IPREG+DCPUIPREGISTER_OFFSETOF_IP) "(%%" LEVEL "ax)\n\t"
         "mov " DCC_PP_STR(DCPUSTATE_OFFSETOF_GPREG+DCPUGPREGISTER_OFFSETOF_AX) "(%%" LEVEL "ax), %%" LEVEL "ax\n\t"
         "ret\n\t"
         : : "a" (state) : "cc", "memory");
#undef LEVEL
 __builtin_unreachable();
}
#endif
#else
#error FIXME
#endif

PUBLIC void DCCSection_RTInit(struct DCCSection *__restrict self) {
 self->sc_rt.rs_vaddr = drt.rt_nextaddr;
 self->sc_rt.rs_allocpagea = 0;
 self->sc_rt.rs_allocpagev = NULL;
 if (!DRT_ENABLED()) return;
 drt.rt_nextaddr += drt.rt_maxsection;
}
PUBLIC void DCCSection_RTQuit(struct DCCSection *__restrict self) {
 void DRT_USER *addr; size_t size;
 /* Free all allocated virtual memory. */
 DCCRTSECTION_FOREACH_BEGIN(&self->sc_rt,addr,size) {
  DRT_VFree(addr,size);
 }
 DCCRTSECTION_FOREACH_END;
 free(self->sc_rt.rs_allocpagev);
 DCCFreeData_Quit(&self->sc_rt.rs_mirror);
}

PUBLIC void DRT_USER *
DCCSection_RTAlloc(struct DCCSection *__restrict self,
                   target_ptr_t addr, target_siz_t size,
                   int for_write) {
 uint8_t DRT_USER *result;
 size_t i,page_min,page_max;
 int has_existing_pages = 0;
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 assert(!DCCSection_ISCURR(self) ||
       (compiler.c_flags&DCC_COMPILER_FLAG_TEXTFLUSH));
 assert(addr+size >= addr);
 assert(addr+size <= (size_t)(self->sc_text.tb_max-
                              self->sc_text.tb_begin));
 result = self->sc_rt.rs_vaddr+addr;
 if unlikely(!size) return result; /* Handle special case: empty range. */
 page_min = (addr)/DCC_TARGET_PAGESIZE;
 page_max = (addr+size)/DCC_TARGET_PAGESIZE;
 assert(page_min <= page_max);
 if (page_max*DCC_TARGET_PAGESIZE >= drt.rt_maxsection) {
  /* Once a section grows larger than the predefined maximum,
   * the chance that it'll start using memory intended for
   * use by another section becomes present.
   * >> Eventually, everything may fail when the other section try to allocate its memory,
   *    but even before then, the DRT thread may access memory intended for another section. */
  WARN(W_DRT_SECTION_TOO_LARGE,
       self->sc_start.sy_name->k_name,
       drt.rt_maxsection);
  if unlikely(!OK) goto err;
 }

 /* Make sure the page allocation tracker has sufficient length. */
 if (page_max >= self->sc_rt.rs_allocpagea) {
  uint8_t *new_allocv; size_t newsize;
  newsize = self->sc_rt.rs_allocpagea;
  if unlikely(!newsize) newsize = 1;
  do newsize *= 2; while (page_max >= newsize);
  new_allocv = (uint8_t *)realloc(self->sc_rt.rs_allocpagev,
                                  newsize*sizeof(uint8_t));
  if unlikely(!new_allocv) { DCC_AllocFailed(newsize*sizeof(uint8_t)); goto err; }
  memset(new_allocv+self->sc_rt.rs_allocpagea,0,
        (newsize-self->sc_rt.rs_allocpagea)*sizeof(uint8_t));
  self->sc_rt.rs_allocpagea = newsize;
  self->sc_rt.rs_allocpagev = new_allocv;
 }
 /* Allocate all pages  */
 for (i = page_min; i <= page_max; ++i) {
  if (!DCCRTSection_ISALLOCATED_(&self->sc_rt,i)) {
   size_t alloc_begin = i,alloc_end = i;
   symflag_t flags; void *base_address;
   size_t alloc_size;
   do ++alloc_end;
   while (alloc_end <= page_max &&
         !DCCRTSection_ISALLOCATED_(&self->sc_rt,alloc_end));
   flags = self->sc_start.sy_flags;
   if (for_write) flags |= DCC_SYMFLAG_SEC_W;
   base_address = (void *)DCCRTSection_PAGEADDR(&self->sc_rt,alloc_begin);
   alloc_size   = (size_t)(alloc_end-alloc_begin)*DCC_TARGET_PAGESIZE;
   if (DRT_VMall(base_address,alloc_size,flags) == DRT_VERROR) {
    WARN(W_DRT_VMALL_FAILED_ALLOC,
         self->sc_start.sy_name->k_name,
        (void *)(base_address),
        (void *)((uintptr_t)base_address+alloc_size-1),
        (int)GetLastError());
    goto err;
   }
   /* Pre-initialize DRT memory. */
   if (flags&DCC_SYMFLAG_SEC_W)
       memset(base_address,DRT_U_FILLER,alloc_size);
   /* Mark all pages as allocated. */
   do DCCRTSection_SETALLOCATED_(&self->sc_rt,alloc_begin);
   while (++alloc_begin != alloc_end);
  } else {
   has_existing_pages = 1;
  }
 }

 if (for_write && has_existing_pages &&
   !(self->sc_start.sy_flags&DCC_SYMFLAG_SEC_W)) {
  /* Must update memory protection on the entire address range. */
  if (DRT_VProt(result,size,self->sc_start.sy_flags|DCC_SYMFLAG_SEC_W) == DRT_VERROR) {
   WARN(W_DRT_VPROT_FAILED_WRITABLE,
        self->sc_start.sy_name->k_name,
       (void *)result,(void *)(result+(size-1)),
       (int)GetLastError());
   goto err;
  }
 }
 return result;
err: return DRT_VERROR;
}

PUBLIC void
DCCSection_RTDoneWrite(struct DCCSection *__restrict self,
                       target_ptr_t addr, target_siz_t size) {
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 assert(!DCCSection_ISCURR(self) ||
       (compiler.c_flags&DCC_COMPILER_FLAG_TEXTFLUSH));
 assert(addr+size >= addr);
 assert(addr+size <= (size_t)(self->sc_text.tb_max-
                              self->sc_text.tb_begin));
 /* Nothing to do if the section is naturally writable. */
 if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_W) return;
 if (DRT_VProt(DCCRTSection_ADDR(&self->sc_rt,addr),size,
               self->sc_start.sy_flags) == DRT_VERROR) {
  WARN(W_DRT_VPROT_FAILED_READONLY,
       self->sc_start.sy_name->k_name,
      (void *)DCCRTSection_ADDR(&self->sc_rt,addr),
      (void *)(DCCRTSection_ADDR(&self->sc_rt,addr)+(size-1)),
      (int)GetLastError());
 }
#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
 if (self->sc_start.sy_flags&DCC_SYMFLAG_SEC_X) {
  /* Flush the instruction cache after writing to an executable section. */
  FlushInstructionCache(GetCurrentProcess(),
                        DCCRTSection_ADDR(&self->sc_rt,addr),
                        size);
 }
#endif /* OS_F_WINDOWS */
}


PUBLIC int
DCCSection_RTCopy(struct DCCSection *__restrict self,
                  void DRT_USER *__restrict target,
                  target_ptr_t addr, target_siz_t size) {
 struct DCCRel *rel_iter,*rel_end; size_t relc;
 void *host_data = DCCSection_GetText(self,addr,size);
 if unlikely(!host_data) return 0;
 memcpy(target,host_data,size);
 rel_iter = DCCSection_GetRel(self,addr,size,&relc);
 if (relc) {
  /* Setup faulty relocations to trigger DRT data load. */
  *(uintptr_t *)&target -= addr;
  rel_end = rel_iter+relc;
  for (; rel_iter != rel_end; ++rel_iter) {
   DRT_FaultRel((uint8_t DRT_USER *)((uintptr_t)target+rel_iter->r_addr),rel_iter);
  }
 }
 return 1;
}



PUBLIC void DRT_Init(void) {
 memset(&drt,0,sizeof(drt));
 drt.rt_stacksize  = DRT_DEFAULT_STACKSIZE;
 drt.rt_maxsection = DRT_DEFAULT_MAXSECTION;
 drt.rt_baseaddr   = (uint8_t DRT_USER *)DRT_DEFAULT_BASEADDR;
 drt.rt_nextaddr   = (uint8_t DRT_USER *)DRT_DEFAULT_BASEADDR;
}
PUBLIC void DRT_Quit(void) {
 if (drt.rt_flags&DRT_FLAG_STARTED) {
  /* Destroy the RT thread. */
  TerminateThread(drt.rt_thread,42);
  CloseHandle(drt.rt_thread);
  CloseHandle(drt.rt_event.ue_sem);
  /* Just in case the drt thread still had a pending
   * event set, clear that even to prevent any false
   * detection of a DRT synchronization point. */
  memset(&drt.rt_event,0,sizeof(drt.rt_event));
  drt.rt_flags &= ~(DRT_FLAG_STARTED);
 }
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "drt-sync.c.inl"
#include "drt-thread.c.inl"
#include "drt-user.c.inl"
#endif
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_C */
