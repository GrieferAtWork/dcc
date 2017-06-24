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
#include "../dcc/x86_util.h"
#if DCC_TARGET_BIN == DCC_BINARY_PE
#include "../dcc/linker-pe.h"
#endif

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
 case DCC_R_RELATIVE:
 case DCC_R_EXT_SIZE:
  USER(target_ptr_t) = (target_ptr_t)DRT_FAULT_ADDRESS;
  break;
 default: break;
 }
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif
#undef USER_BASE_ADDRESS

/* Recursively check all relocations part of a given symbol's
 * definition and return ZERO(0) if the symbol itself, or any
 * symbol referred to by a relocation isn't defined.
 * return ONE(1) otherwise. */
PRIVATE int
DRT_CheckSymbolReferences(struct DCCSym *__restrict sym,
                          struct DCCSection const *__restrict ref_section,
                          target_ptr_t ref_addr, int warn_failure) {
 struct DCCSymAddr symaddr; target_ptr_t sym_acc,sym_end;
 DCCSym_ASSERT(sym);
 if unlikely(!DCCSym_LoadAddr(sym,&symaddr,1)) goto fail;
 assert(symaddr.sa_sym);
 assert(DCCSym_ISDEFINED(symaddr.sa_sym));
 sym_acc = symaddr.sa_sym->sy_addr+symaddr.sa_off;
 sym_end = symaddr.sa_sym->sy_addr+symaddr.sa_sym->sy_size;
 if (sym_acc < sym_end) {
  struct DCCRel *rel_iter,*rel_end; size_t relc;
  struct DCCSection *sec = symaddr.sa_sym->sy_sec;
  /* Make sure that the address range from
   * 'rel_value..sym_end' doesn't contain unresolved relocations. */
  rel_iter = DCCSection_GetRel(sec,sym_acc,
                              (target_siz_t)(sym_end-sym_acc),
                              &relc);
  rel_end = rel_iter+relc;
  for (; rel_iter != rel_end; ++rel_iter) {
   if (!DRT_CheckSymbolReferences(rel_iter->r_sym,sec,
                                  rel_iter->r_addr,
                                  warn_failure))
        goto fail; /* Recursively display reference warnings. */
  }
 }
 return 1;
fail:
 if (warn_failure) {
  WARN(W_UNRESOLVED_REFERENCE,
       ref_section,ref_addr,sym->sy_name,
       ref_section->sc_start.sy_name,ref_addr);
 }
 return 0;
}


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
 if (!DCCSym_LoadAddr(rel->r_sym,&symaddr,1)) {
  /* Undefined weak symbol can only be resolved once the compiler has shut down. */
  if ((rel->r_sym->sy_flags&DCC_SYMFLAG_WEAK) &&
      (drt.rt_flags&DRT_FLAG_JOINING)) rel_value = 0;
  else {
   symaddr.sa_sym = rel->r_sym;
#if DCC_TARGET_BIN == DCC_BINARY_PE
   assert(symaddr.sa_sym);
   assert(DCCSym_ISFORWARD(symaddr.sa_sym));
   /* Resolve IAT symbol indirection.
    * >> Even though IAT indirection is disabled when DRT is used,
    *    statically linking against other object files, such as
    *   'crt1.o' may introduce IAT symbol, meaning we must still
    *    resolve them manually here.
    * >> This can easily be done by allocating a reseved address
    *    that we can fill with the IAT base symbol before linking
    *    against said fixed address. */
   if (symaddr.sa_sym->sy_name->k_size >= DCC_COMPILER_STRLEN(ITA_PREFIX) &&
      !memcmp(symaddr.sa_sym->sy_name->k_name,ITA_PREFIX,sizeof(ITA_PREFIX)-sizeof(char))) {
    /* This is probably IAT symbol. */
    struct DCCSym *base_sym = DCCUnit_GetSyms(symaddr.sa_sym->sy_name->k_name+
                                              DCC_COMPILER_STRLEN(ITA_PREFIX));
    if (base_sym &&
       (base_sym->sy_flags&DCC_SYMFLAG_PE_ITA_IND) &&
        base_sym->sy_peind == symaddr.sa_sym &&
       !DCCSym_ISFORWARD(base_sym)) {
     /* Yes. It is a PE indirection symbol. */
     struct DCCSymAddr base_addr;
     target_ptr_t *ind_slot;
     if unlikely(!DCCSym_LoadAddr(base_sym,&base_addr,1)) goto missing_reference;
     assert(base_addr.sa_sym);
     assert(DCCSym_ISDEFINED(base_addr.sa_sym));
     if (DCCSection_ISIMPORT(base_addr.sa_sym->sy_sec)) {
      /* Most likely case: Load an import symbol. */
      void *import_sym = DCCSection_DLImport(base_addr.sa_sym->sy_sec,
                                             base_addr.sa_sym->sy_name->k_name);
      if unlikely(!import_sym) goto missing_reference;
      base_addr.sa_off += (uintptr_t)import_sym;
     } else {
      /* PE indirection against statically linked symbol? - ok... */
      if (!DCCSym_ISSECTION(base_addr.sa_sym))
           base_addr.sa_off += base_addr.sa_sym->sy_addr;
      base_addr.sa_off += (uintptr_t)base_addr.sa_sym->sy_sec->sc_dat.sd_rt.rs_vaddr;
     }
     ind_slot = DRT_AllocPEIndirection();
     if unlikely(!ind_slot) goto missing_reference;
     *ind_slot = base_addr.sa_off;
     DCCSym_Define(symaddr.sa_sym,&DCCSection_Abs,
                  (target_ptr_t)ind_slot,sizeof(target_ptr_t),1);
     goto has_symbol;
    }
   }
#endif /* DCC_BINARY_PE */
missing_reference:
   if (warn_failure) {
    WARN(W_UNRESOLVED_REFERENCE,
         sec,rel->r_addr,
         symaddr.sa_sym->sy_name,
         sec->sc_start.sy_name,
         rel->r_addr);
   }
   return 0; /* Unresolved symbol. */
  }
 } else {
#if DCC_TARGET_BIN == DCC_BINARY_PE
has_symbol:
#endif /* DCC_BINARY_PE */
  if (DCCSection_ISIMPORT(symaddr.sa_sym->sy_sec)) {
   /* Lazily dlopen() import sections. */
   rel_value = (target_ptr_t)DCCSection_DLImport(symaddr.sa_sym->sy_sec,
                                                 symaddr.sa_sym->sy_name->k_name);
   if (!rel_value) goto missing_reference;
   rel_value += symaddr.sa_off;
  } else {
   assert(symaddr.sa_sym);
   assert(DCCSym_ISDEFINED(symaddr.sa_sym));
   rel_value = symaddr.sa_off+(target_ptr_t)symaddr.sa_sym->sy_sec->sc_dat.sd_rt.rs_vaddr;
   if (!DCCSym_ISSECTION(symaddr.sa_sym)) {
    target_ptr_t sym_end;
    rel_value += symaddr.sa_sym->sy_addr;
    sym_end = symaddr.sa_sym->sy_addr+symaddr.sa_sym->sy_size;
    if (rel_value < sym_end) {
     struct DCCRel *rel_iter,*rel_end; size_t relc;
     /* Make sure that the address range from
      * 'rel_value..sym_end' doesn't contain unresolved relocations. */
     rel_iter = DCCSection_GetRel(sec,rel_value,
                                  (target_siz_t)(sym_end-rel_value),
                                  &relc);
     rel_end = rel_iter+relc;
     for (; rel_iter != rel_end; ++rel_iter) {
      if (!DRT_CheckSymbolReferences(rel_iter->r_sym,sec,
       rel_iter->r_addr,
       warn_failure))
       return 0;
     }
    }
   }
  }
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
 case DCC_R_EXT_SIZE:
  rel_value = HOST(int32_t);
  if (symaddr.sa_sym) {
   if (DCCSym_ISSECTION(symaddr.sa_sym))
        rel_value += DCCSection_VSIZE(DCCSym_TOSECTION(symaddr.sa_sym));
   else rel_value += symaddr.sa_sym->sy_size;
  }
  USER(int32_t) = rel_value;
  break;
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
   if (distance < drt.rt_maxsection)
       return result;
   nearest_distance = distance;
  }
 }
 /* Don't associated the address when it is too far away. */
 if (nearest_distance > drt.rt_maxsection*16)
     result = NULL;
 return result;
}

extern void dcc_outf(char const *fmt, ...);

PRIVATE target_siz_t
DCCSection_RawMirrorData(struct DCCSection *__restrict self,
                         target_ptr_t addr,
                         target_siz_t size,
                         uint8_t DRT_USER *utext,
                         uint8_t DRT_HOST *htext, size_t htext_alloc) {
#define PBEGIN  &self->sc_dat.sd_rt.rs_mtext.fd_begin
 struct DCCFreeRange **piter = PBEGIN,*iter,*other;
 target_siz_t mirror_size;
 target_siz_t size_ok = 0;
more:
 while ((iter = *piter) != NULL &&
         iter->fr_addr+iter->fr_size <= addr)
         piter = &iter->fr_next;
 if (iter) {
  if (addr >= iter->fr_addr) {
   /* Skip overlap. */
   mirror_size = (target_siz_t)((iter->fr_addr+iter->fr_size)-addr);
   if (mirror_size >= size) goto done_mirror;
   addr += mirror_size;
   size -= mirror_size;
   assert(addr == iter->fr_addr+iter->fr_size);
   goto more;
  }
  assert(addr < iter->fr_addr);
  mirror_size = iter->fr_addr-addr;
  if (mirror_size > size)
      mirror_size = size;
  if (iter->fr_addr == addr+mirror_size) {
   /* Extend this chunk below. */
   iter->fr_addr -= mirror_size;
   iter->fr_size += mirror_size;
   if ((other = iter->fr_next) != NULL &&
        iter->fr_addr+iter->fr_size == other->fr_addr) {
    /* Merge chunks. */
    iter->fr_size += other->fr_size;
    iter->fr_next  = other->fr_next;
    free(other);
   }
   goto commit_mirror;
  }
 } else {
  mirror_size = size;
 }
 if (piter != PBEGIN) {
  other = (struct DCCFreeRange *)((uintptr_t)piter-
                                   DCC_COMPILER_OFFSETOF(struct DCCFreeRange,fr_next));
  assert(other->fr_next == iter);
  if (other->fr_addr+other->fr_size == addr) {
   /* Extend previous chunk above. */
   other->fr_size += mirror_size;
   assert(!iter || other->fr_addr+other->fr_size < iter->fr_addr);
   goto commit_mirror;
  }
  assert(addr > other->fr_addr+other->fr_size);
 }
 assert(!iter || addr+mirror_size < iter->fr_addr);
 assert(mirror_size == size);
 assert(iter == *piter);

 /* Insert a new chunk. */
 other = (struct DCCFreeRange *)malloc(sizeof(struct DCCFreeRange));
 if unlikely(!other) { DCC_AllocFailed(sizeof(struct DCCFreeRange)); goto done_mirror; }
 other->fr_addr = addr;
 other->fr_size = mirror_size;
 other->fr_next = iter;
 *piter = other;

commit_mirror:
 assert(mirror_size <= size);
 size_ok += mirror_size;
/*
 dcc_outf("MIRROR(%p ... %p)\n",
          utext+addr,utext+addr+(mirror_size-1));
*/
 if (addr+mirror_size > htext_alloc) {
  size_t copy_size = addr >= htext_alloc ? 0u : htext_alloc-addr;
  memcpy(utext+addr,htext+addr,copy_size);
  /* Fill trailing ZERO-memory. */
  memset(utext+addr+copy_size,0,mirror_size-copy_size);
 } else {
  memcpy(utext+addr,htext+addr,mirror_size);
 }
 if (mirror_size != size) {
  addr += mirror_size;
  size -= mirror_size;
  goto more;
 }
done_mirror:
 return size_ok;
#undef PBEGIN
}


/* Copy text data from 'addr...+=size' to 'target'.
 * NOTE: During this operation, data referred to by
 *       relocations is filled in with 'DRT_FAULT_ADDRESS'.
 * NOTE: Any byte of memory is only copied once before being marked as mirrored.
 * NOTE: This function copies whole pages, meaning that more than 'size' bytes may be copied.
 * @return: *: Amount of newly loaded bytes. */
LOCAL void
DCCSection_RTMirrorText(struct DCCSection *__restrict self,
                        target_ptr_t addr,
                        size_t *__restrict prelc_ok, size_t *__restrict prelc_no,
                        size_t *__restrict psize_ok, size_t *__restrict psize_total,
                        int warn_failure, int single_instruction) {
 uint8_t DRT_USER *utext;
 uint8_t DRT_HOST *htext;
 struct DCCTextBuf *text;
 struct DCCRel *rel_iter,*rel_end;
 size_t relc = 0,relc_no = 0;
 size_t relc_ok = 0,size_ok = 0;
 target_siz_t size,msize;
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 text  = self == unit.u_curr ? &unit.u_tbuf : &self->sc_dat.sd_text;
 htext = text->tb_begin;
 size  = (target_siz_t)(text->tb_max-htext);
 msize = (target_siz_t)(text->tb_end-htext);
 if (addr >= size) goto end;
 size -= addr;
 assert(size);
 /* Artificially limit for how much code is ever fetched at once. */
 if (size > DCC_TARGET_PAGESIZE)
     size = DCC_TARGET_PAGESIZE;
 if (single_instruction) {
  uint8_t *hcode = htext+addr;
  assert(self->sc_start.sy_flags&DCC_SYMFLAG_SEC_X);
  /* Only load a single instruction. */
  size = (target_siz_t)(x86_instrlen(hcode)-hcode);
  assert(size);
 }
 utext = DCCSection_RTAlloc(self,addr,size,1);
 if unlikely(utext == DRT_VERROR) goto end;
 utext = self->sc_dat.sd_rt.rs_vaddr;
 /* NOTE: The return value must not indicate
  *       that text is loaded more than once. */
 size_ok = DCCSection_RawMirrorData(self,addr,size,utext,htext,msize);
 /* Now to handle relocations. */
 rel_iter = DCCSection_GetRel(self,addr,size,&relc);
 if (relc) {
  rel_end = rel_iter+relc;
  for (; rel_iter != rel_end; ++rel_iter) {
   if (rel_iter->r_type == DCC_R_NONE) continue;
   if (DRT_ResolveRel(utext+rel_iter->r_addr,
                      htext+rel_iter->r_addr,
                      rel_iter,self,warn_failure)) {
    rel_iter->r_type = DCC_R_NONE;
    ++relc_ok;
   } else {
    DRT_FaultRel(utext+rel_iter->r_addr,rel_iter);
    ++relc_no;
   }
  }
 }
 DCCSection_RTDoneWrite(self,addr,size);
end:
 *prelc_no    = relc_no;
 *psize_total = size;
 *prelc_ok   += relc_ok;
 *psize_ok   += size_ok;
}



INTERN int
DCCSection_RTMirrorData(struct DCCSection *__restrict self,
                        target_ptr_t addr, target_siz_t size,
                        target_siz_t *__restrict psize_ok,
                        int warn_failure) {
 uint8_t DRT_USER *utext;
 uint8_t DRT_HOST *htext;
 struct DCCTextBuf *text;
 target_ptr_t addrend    = addr+size;
 target_ptr_t pa_addr    = (addr)&~(DCC_TARGET_PAGESIZE-1);
 target_ptr_t pa_addrend = (addrend+(DCC_TARGET_PAGESIZE-1))&~(DCC_TARGET_PAGESIZE-1);
 target_siz_t size_ok    = 0;
 target_siz_t sec_size;
 size_t pagei_idx = pa_addr/DCC_TARGET_PAGESIZE;
 size_t pagei_end = pa_addrend/DCC_TARGET_PAGESIZE;
 //if (self == unit.u_string/* && pa_addr >= 0x1000*/)
 //    DCC_BREAKPOINT();
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 text     = self == unit.u_curr ? &unit.u_tbuf : &self->sc_dat.sd_text;
 htext    = text->tb_begin;
 sec_size = (target_siz_t)(text->tb_max-htext);
 if (addr+size >= sec_size) return 0;
 utext = DCCSection_RTAlloc(self,pa_addr,pa_addrend-pa_addr,1);
 if unlikely(utext == DRT_VERROR) return 0;
 utext = self->sc_dat.sd_rt.rs_vaddr;
 assert(pagei_idx <= pagei_end);
 for (; pagei_idx != pagei_end; ++pagei_idx) {
  if (!DCCRTSection_PAGE_ISUSABLE(&self->sc_dat.sd_rt,pagei_idx)) {
   struct DCCRel *rel_iter,*rel_end; size_t relc;
   target_ptr_t page_addr  = pagei_idx*DCC_TARGET_PAGESIZE;
   target_siz_t page_vsize = (target_siz_t)(text->tb_max-htext);
   if (page_addr >= page_vsize) break;
   page_vsize -= page_addr;
   if (page_vsize > DCC_TARGET_PAGESIZE)
       page_vsize = DCC_TARGET_PAGESIZE;
   size_ok += DCCSection_RawMirrorData(self,page_addr,page_vsize,utext,htext,
                                      (size_t)(text->tb_end-htext));
   DCCRTSection_SET_PAGEATTR(&self->sc_dat.sd_rt,pagei_idx,0x1|0x2);
   rel_iter = DCCSection_GetRel(self,page_addr,page_vsize,&relc);
   if (relc) {
    rel_end = rel_iter+relc;
    for (; rel_iter != rel_end; ++rel_iter) {
     if (rel_iter->r_type == DCC_R_NONE) continue;
     /* If any relocation cannot be loaded,
      * we must not set the mirror flag.
      * In that way, DRT data section are separated
      * into pages, where pages must be defined as a
      * whole in order to become usable. */
     if (DRT_ResolveRel(utext+rel_iter->r_addr,
                        htext+rel_iter->r_addr,
                        rel_iter,self,warn_failure)) {
      rel_iter->r_type = DCC_R_NONE;
     } else {
      DCCRTSection_UNSET_PAGEATTR(&self->sc_dat.sd_rt,pagei_idx,0x1);
      goto fail;
     }
    }
   }
  }
 }
 DCCSection_RTDoneWrite(self,pa_addr,pa_addrend-pa_addr);
 *psize_ok = size_ok; /* Intentionally only set here! */
 return 1;
fail:
 DCCSection_RTDoneWrite(self,pa_addr,pa_addrend-pa_addr);
 return 0;
}

PUBLIC int DCC_ATTRIBUTE_FASTCALL DRT_H_Sync(int warn_failure) {
#define EVENT   drt.rt_event
 uint32_t code; int result;
 struct DCCSection *sec;
 assert(drt.rt_flags&DRT_FLAG_STARTED);
 code = EVENT.ue_code;
 MEMORY_BARRIER();
 switch (code) {

 {
  size_t relc_no;
  int single_instruction;
 case DRT_EVENT_MIRROR_TEXT:
  single_instruction = 0;
  sec = DRT_FindUserSection(EVENT.ue_text.te_addr);
  if unlikely(!sec) {
   /* Invalid address. */
   result = DRT_SYNC_FAULT;
   goto post;
  }
load_text:
  DCCSection_RTMirrorText(sec,
                         (target_ptr_t)((uintptr_t)EVENT.ue_text.te_addr-
                                        (uintptr_t)sec->sc_dat.sd_rt.rs_vaddr),
                         &EVENT.ue_text.te_relc_ok,&relc_no,
                         &EVENT.ue_text.te_size_ok,
                         &EVENT.ue_text.te_size_total,
                          warn_failure,single_instruction);
  /* If data was read or everything was already read to begin with, post the results. */
  if (EVENT.ue_text.te_relc_ok ||
      EVENT.ue_text.te_size_ok ||
     (EVENT.ue_text.te_size_total && !relc_no)) {
   result = DRT_SYNC_OK;
   goto post;
  }
  /* Try again, but only attempt to load a single instruction. */
  if (!single_instruction &&
      (sec->sc_start.sy_flags&DCC_SYMFLAG_SEC_X)) {
   single_instruction = 1;
   goto load_text;
  }
unresolved:
  result = DRT_SYNC_UNRESOLVED;
 } break;

 case DRT_EVENT_MIRROR_DATA:
  sec = DRT_FindUserSection(EVENT.ue_data.de_addr);
  if unlikely(!sec) {
   /* Invalid address. */
   EVENT.ue_data.de_size = (size_t)-1;
   result = DRT_SYNC_FAULT;
   goto post;
  }
  if (DCCSection_RTMirrorData(sec,
                             (target_ptr_t)((uintptr_t)EVENT.ue_data.de_addr-
                                            (uintptr_t)sec->sc_dat.sd_rt.rs_vaddr),
                              EVENT.ue_data.de_size,
                             &EVENT.ue_data.de_size,warn_failure)) {
   result = DRT_SYNC_OK;
   goto post;
  }
  goto unresolved;

 {
  struct A2lState s;
  struct DCCSymAddr a2l_addr;
  struct DCCSection *sec_dbgstr;
  char DRT_USER *dbgstr_base;
 case DRT_EVENT_ADDR2LINE:
  sec = DRT_FindUserSection(EVENT.ue_a2l.ae_addr);
  if unlikely(!sec) {
   /* Invalid address. */
   EVENT.ue_a2l.ae_addr = DRT_EVENT_ADDR2LINE_FAULT;
   result = DRT_SYNC_FAULT;
   goto post;
  }
  a2l_addr.sa_sym = &sec->sc_start;
  a2l_addr.sa_off = ((uint8_t DRT_USER *)EVENT.ue_a2l.ae_addr-
                                         sec->sc_dat.sd_rt.rs_vaddr);
  /* Lookup compile-time A2L debug information about the requested address. */
  if (!DCCA2l_LookupAdr(&s,&a2l_addr))
       EVENT.ue_a2l.ae_addr = DRT_EVENT_ADDR2LINE_FAULT;
  else EVENT.ue_a2l.ae_addr = sec->sc_dat.sd_rt.rs_vaddr+s.s_addr;
  sec_dbgstr = unit.u_dbgstr;
  if unlikely(!sec_dbgstr) sec_dbgstr = DCCUnit_GetSecs(A2L_STRING_SECTION);
  dbgstr_base = sec_dbgstr && !DCCSection_ISIMPORT(sec_dbgstr)
              ? (char DRT_USER *)sec_dbgstr->sc_dat.sd_rt.rs_vaddr : NULL;
  /* Translate A2L information into DRT event results. */
  EVENT.ue_a2l.ae_path = NULL;
  EVENT.ue_a2l.ae_file = NULL;
  EVENT.ue_a2l.ae_name = NULL;
  EVENT.ue_a2l.ae_line = 0;
  EVENT.ue_a2l.ae_col  = 0;
  if (s.s_features&A2L_STATE_HASLINE) EVENT.ue_a2l.ae_line = s.s_line+1;
  if (s.s_features&A2L_STATE_HASCOL ) EVENT.ue_a2l.ae_col  = s.s_col+1;
  if (dbgstr_base) {
   if (s.s_features&A2L_STATE_HASPATH) EVENT.ue_a2l.ae_path = dbgstr_base+s.s_path;
   if (s.s_features&A2L_STATE_HASFILE) EVENT.ue_a2l.ae_file = dbgstr_base+s.s_file;
   if (s.s_features&A2L_STATE_HASNAME) EVENT.ue_a2l.ae_name = dbgstr_base+s.s_name;
  }
  /* Post results. */
  result = DRT_SYNC_OK;
  goto post;
 }

 default:
  result = DRT_SYNC_NONE;
  break;
 }
 return result;
post:
 if unlikely(!OK) return DRT_SYNC_UNRESOLVED;
 EVENT.ue_code = DRT_EVENT_NONE;
 MEMORY_BARRIER();
 if (!(drt.rt_flags&DRT_FLAG_JOINING)) {
  ReleaseSemaphore(EVENT.ue_sem,1,NULL);
 }
 return result;
}
#undef EVENT

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
 if (error != DRT_SYNC_UNRESOLVED) {
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
