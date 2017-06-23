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
#ifndef GUARD_DRT_DRT_SYNC_C_INL
#define GUARD_DRT_DRT_SYNC_C_INL 1

#include <dcc/common.h>
#include <dcc/target.h>
#if DCC_CONFIG_HAVE_DRT

#include <dcc/compiler.h>
#include <dcc/unit.h>
#include <drt/drt.h>

#include "drt.h"

#include <stdio.h>

DCC_DECL_BEGIN

#define USER(T)           (*(T *)uaddr)
#define HOST(T)           (*(T *)haddr)
#define USER_BASE_ADDRESS ((target_ptr_t)uaddr)

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4310)
#endif

INTERN void
DRT_FaultRel(uint8_t DRT_USER *__restrict uaddr,
             struct DCCRel const *__restrict rel) {
 assert(rel);
 assert(rel->r_sym);
 switch (rel->r_type) {
 case DCC_R_DATA_8 : USER(int8_t)  = (int8_t)DRT_FAULT_ADDRESS; break;
 case DCC_R_DATA_16: USER(int16_t) = (int16_t)DRT_FAULT_ADDRESS; break;
 case DCC_R_DATA_32: USER(int32_t) = (int32_t)DRT_FAULT_ADDRESS; break;
#ifdef DCC_R_DATA_64
 case DCC_R_DATA_64: USER(int64_t) = (int64_t)DRT_FAULT_ADDRESS; break;
#endif
 case DCC_R_DISP_8 : USER(int8_t)  = (int8_t)(DRT_FAULT_ADDRESS-(USER_BASE_ADDRESS+1)); break;
 case DCC_R_DISP_16: USER(int16_t) = (int16_t)(DRT_FAULT_ADDRESS-(USER_BASE_ADDRESS+2)); break;
 case DCC_R_DISP_32: USER(int32_t) = (int32_t)(DRT_FAULT_ADDRESS-(USER_BASE_ADDRESS+4)); break;
 case DCC_R_RELATIVE: USER(target_ptr_t) = (target_ptr_t)DRT_FAULT_ADDRESS; break;
 default: break;
 }
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#undef USER_BASE_ADDRESS


#define USER_BASE_ADDRESS ((target_ptr_t)sec->sc_dat.sd_rt.rs_vaddr)
INTERN int
DRT_ResolveRel(uint8_t DRT_USER *__restrict uaddr,
               uint8_t DRT_HOST *__restrict haddr,
               struct DCCRel const *__restrict rel,
               struct DCCSection const *__restrict sec,
               int warn_failure) {
 target_ptr_t rel_value;
 struct DCCSymAddr symaddr;
 assert(rel);
 assert(rel->r_sym);
 if (rel->r_type == DCC_R_RELATIVE) {
  USER(target_ptr_t) = HOST(target_ptr_t)+(target_ptr_t)drt.rt_baseaddr;
  return 1;
 }
 /* We're only resolving relocations pointing back into our section. */
 /* TODO: Lazily dlopen() import sections. */
 if (!DCCSym_LoadAddr(rel->r_sym,&symaddr,1)) {
  /* Undefined weak symbol can only be resolved once the compiler has shut down. */
  if ((rel->r_sym->sy_flags&DCC_SYMFLAG_WEAK) &&
      (drt.rt_flags&DRT_FLAG_JOINING)) rel_value = 0;
  else {
missing_reference:
   if (warn_failure) {
    WARN(W_UNRESOLVED_REFERENCE,
         sec,rel->r_addr,
         rel->r_sym->sy_name,
         sec->sc_start.sy_name,
         rel->r_addr);
   }
   return 0; /* Unresolved symbol. */
  }
 } else if (DCCSection_ISIMPORT(symaddr.sa_sym->sy_sec)) {
  rel_value = (target_ptr_t)DCCSection_DLImport(symaddr.sa_sym->sy_sec,
                                                symaddr.sa_sym->sy_name->k_name);
  if (!rel_value) goto missing_reference;
  rel_value += symaddr.sa_off;
 } else {
  rel_value = symaddr.sa_off+(target_ptr_t)symaddr.sa_sym->sy_sec->sc_dat.sd_rt.rs_vaddr;
  if (!DCCSym_ISSECTION(symaddr.sa_sym))
       rel_value += symaddr.sa_sym->sy_addr;
 }
 switch (rel->r_type) {
 case DCC_R_DATA_8  : USER(int8_t)  = HOST(int8_t) +(int8_t) rel_value; break;
 case DCC_R_DATA_16 : USER(int16_t) = HOST(int16_t)+(int16_t)rel_value; break;
 case DCC_R_DATA_32 : USER(int32_t) = HOST(int32_t)+(int32_t)rel_value; break;
#ifdef DCC_R_DATA_64
 case DCC_R_DATA_64 : USER(int64_t) = HOST(int64_t)+(int64_t)rel_value; break;
#endif
 case DCC_R_DISP_8  : USER(int8_t)  = HOST(int8_t) +(int8_t) (rel_value-USER_BASE_ADDRESS); break;
 case DCC_R_DISP_16 : USER(int16_t) = HOST(int16_t)+(int16_t)(rel_value-USER_BASE_ADDRESS); break;
 case DCC_R_DISP_32 : USER(int32_t) = HOST(int32_t)+(int32_t)(rel_value-USER_BASE_ADDRESS); break;
 default: break;
 }
 return 1;
}

#undef USER_BASE_ADDRESS
#undef HOST
#undef USER




INTERN struct DCCSection *
DRT_FindUserSection(void DRT_USER *addr) {
 struct DCCSection *result = NULL,*iter;
 uintptr_t nearest_distance = (uintptr_t)-1;
 DCCUnit_ENUMSEC(iter) {
  uintptr_t distance;
  if ((uintptr_t)addr < (uintptr_t)iter->sc_dat.sd_rt.rs_vaddr) continue;
  distance = (uintptr_t)addr-(uintptr_t)iter->sc_dat.sd_rt.rs_vaddr;
  if (distance < nearest_distance) {
   result = iter;
   if (distance < DCC_TARGET_PAGESIZE) break;
   nearest_distance = distance;
  }
 }
 return result;
}

/* Try to fetch data starting at 'addr'.
 * @return: * : The amount of loaded bytes after 'addr'.
 * @return: (size_t)-1 : An invalid address was given.
 */
PRIVATE size_t
DRT_TryFetchData(void DRT_USER *addr,
                 size_t *__restrict pmissing_relc,
                 size_t *__restrict pfetched_relc,
                 int warn_failure) {
 struct DCCSection *sec; size_t result;
 target_ptr_t sec_max,sec_addr;
 sec = DRT_FindUserSection(addr);
 if unlikely(!sec) goto invalid;
 DCCSection_TBEGIN(sec);
 sec_max  = (target_ptr_t)(sec->sc_dat.sd_text.tb_max-sec->sc_dat.sd_text.tb_begin);
 sec_addr = (target_ptr_t)((uintptr_t)addr-(uintptr_t)sec->sc_dat.sd_rt.rs_vaddr);
 if (sec_addr >= sec_max) {
  result = 0;
  if (warn_failure) { /* TODO: Warning */ }
  *pmissing_relc = 0;
  *pfetched_relc = 0;
 } else {
  size_t relc; struct DCCRel *rel_iter,*rel_end;
  size_t missing_relc = 0;
  size_t fetched_relc = 0;
  result = sec_max-sec_addr;
  /* TODO: Only load memory that hasn't been mirrored yet.
   *       Otherwise, we might reset writable memory in '.bss' */
  addr = DCCSection_RTAlloc(sec,sec_addr,result,1);
  if unlikely(addr == DRT_VERROR) goto none;
  if unlikely(!DCCSection_RTCopy(sec,addr,sec_addr,result)) goto none;
  /* Load relocations inside. */
  rel_iter = DCCSection_GetRel(sec,sec_addr,result,&relc);
  if (relc) {
   rel_end = rel_iter+relc;
   *(uintptr_t *)&addr -= sec_addr;
   for (; rel_iter != rel_end; ++rel_iter) {
    if (rel_iter->r_type == DCC_R_NONE) continue;
    if (DRT_ResolveRel((uint8_t *)addr+rel_iter->r_addr,
                       (uint8_t *)sec->sc_dat.sd_text.tb_begin+rel_iter->r_addr,
                        rel_iter,sec,warn_failure)) {
     /* Destroy relocations once they've been resolved in user-space,
      * thereby ensuring that every relocation is only resolved once. */
     /*rel_iter->r_type = DCC_R_NONE;*/ /* Can't do this until LoadMemory only loads non-mirrored data. */
     ++fetched_relc;
    } else {
     ++missing_relc;
    }
   }
  }
  *pmissing_relc = missing_relc;
  *pfetched_relc = fetched_relc;
  DCCFreeData_ReleaseMerge(&sec->sc_dat.sd_rt.rs_mirror,sec_addr,result);
  DCCSection_RTDoneWrite(sec,sec_addr,result);
 }
 DCCSection_TEND(sec);
 return result;
none: DCCSection_TEND(sec);
 *pmissing_relc = 0;
 *pfetched_relc = 0;
 return 0;
invalid:
 *pmissing_relc = 0;
 *pfetched_relc = 0;
 return (size_t)-1;
}

PUBLIC int DCC_ATTRIBUTE_FASTCALL DRT_H_Sync(int warn_failure) {
 uint32_t code;
 int result = DRT_SYNC_FAIL;
 assert(drt.rt_flags&DRT_FLAG_STARTED);
 code = drt.rt_event.ue_code;
 MEMORY_BARRIER();
 if (code == DRT_EVENT_NONE) return DRT_SYNC_NONE;
 switch (code) {
 {
  size_t missing_relc,fetched_relc;
 case DRT_EVENT_FETCH:
  assertf(drt.rt_event.ue_fetch.f_size,"Invalid fetch size");
  drt.rt_event.ue_fetch.f_datsz = DRT_TryFetchData(drt.rt_event.ue_fetch.f_addr,
                                                  &missing_relc,&fetched_relc,
                                                   warn_failure);
  if (drt.rt_event.ue_fetch.f_datsz == (size_t)-1) {
   /* Invalid address. */
   drt.rt_event.ue_fetch.f_datsz  = 0;
   drt.rt_event.ue_fetch.f_okrelc = 0;
   goto post;
  }
  drt.rt_event.ue_fetch.f_okrelc = fetched_relc; /* TODO: This must be summed eventually. */
  /* When data is missing, there would be no point in trying to resolve relocations. */
  if (missing_relc || drt.rt_event.ue_fetch.f_datsz <
                      drt.rt_event.ue_fetch.f_size) break;
  assert(drt.rt_event.ue_fetch.f_datsz >=
         drt.rt_event.ue_fetch.f_size);
  goto post;
 }

 case DRT_EVENT_FETCHSOME:
  drt.rt_event.ue_fetch.f_datsz = DRT_TryFetchData(drt.rt_event.ue_fetch.f_addr,
                                                  &drt.rt_event.ue_fetch.f_norelc,
                                                  &drt.rt_event.ue_fetch.f_okrelc,
                                                   warn_failure);
  if (drt.rt_event.ue_fetch.f_datsz == (size_t)-1) {
   /* Invalid address. */
   drt.rt_event.ue_fetch.f_datsz  = 0;
   drt.rt_event.ue_fetch.f_okrelc = 0;
   drt.rt_event.ue_fetch.f_norelc = 0;
  }
  goto post;

 default: break;
 }
 return result;
post:
 if unlikely(!OK) return DRT_SYNC_FAIL;
 drt.rt_event.ue_code = DRT_EVENT_NONE;
 MEMORY_BARRIER();
 if (!(drt.rt_flags&DRT_FLAG_JOINING)) {
  ReleaseSemaphore(drt.rt_event.ue_sem,1,NULL);
 }
 return DRT_SYNC_OK;
}

PUBLIC int DCC_ATTRIBUTE_FASTCALL DRT_H_SyncAll(void) {
 int error;
 assert(drt.rt_flags&DRT_FLAG_STARTED);
 drt.rt_flags |= DRT_FLAG_JOINING;
 MEMORY_BARRIER();
 /* Wake the DRT thread in the event that it
  * is currently waiting for the compiler.
  * Once it notices that the compiler is gone, it will
  * no longer attempt to wait for the event semaphore,
  * but do its own synchronizing. */
 error = DRT_H_Sync(1);
 if (error != DRT_SYNC_FAIL) {
  /* The semaphore event wasn't posted because 'DRT_FLAG_JOINING' was already set. */
  drt.rt_flags |= DRT_FLAG_JOINING2;
  MEMORY_BARRIER();
  ReleaseSemaphore(drt.rt_event.ue_sem,1,NULL);
  WaitForSingleObject(drt.rt_thread,INFINITE);
 } else {
  drt.rt_flags &= ~(DRT_FLAG_JOINING);
  MEMORY_BARRIER();
 }

 return error;
}

DCC_DECL_END
#endif /* DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_SYNC_C_INL */
