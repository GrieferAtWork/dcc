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
#ifndef GUARD_DCC_UNIT_SECCOLL_C_INL
#define GUARD_DCC_UNIT_SECCOLL_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/unit.h>

#include <string.h>

DCC_DECL_BEGIN

#define DCCSECTION_ASSERT_NOTANIMPORT(self) \
 assertf(!DCCSection_ISIMPORT(self),\
          "This operation cannot be performed on import-section '%s'",\
         (self)->sc_start.sy_name->k_name)

#ifdef DCC_COMPILER_FLAG_TEXTFLUSH
#define DCCSECTION_ASSERT_TEXT_FLUSHED(self) \
 (DCCSECTION_ASSERT_NOTANIMPORT(self),\
  assertf(!DCCSection_ISCURR(self) || \
         (compiler.c_flags&DCC_COMPILER_FLAG_TEXTFLUSH),\
          "Can't perform text operations without flushing the current text section '%s'",\
         (self)->sc_start.sy_name->k_name))
#else
#define DCCSECTION_ASSERT_TEXT_FLUSHED(self) \
 DCCSECTION_ASSERT_NOTANIMPORT(self)
#endif



PUBLIC void
DCCSection_FreeUnused(struct DCCSection *__restrict self) {
 assert(self),DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 /* Step #1: Clear all old free address ranges. */
 { struct DCCFreeRange *iter,*next;
   iter = self->sc_free.fd_begin;
   self->sc_free.fd_begin = NULL;
   while (iter) {
    next = iter->fr_next;
    free(iter);
    iter = next;
   }
 }
 /* Step #2: Mark all unused address ranges as free. */
 { struct DCCAllocRange *iter;
   target_ptr_t last_range_end;
   target_siz_t section_size;
   target_siz_t unused_size;
   iter           = self->sc_alloc;
   last_range_end = 0;
   section_size   = DCCSection_VSIZE(self);
   while (iter) {
    assert(iter->ar_addr >= last_range_end);
    assert(iter->ar_refcnt != 0);
    unused_size = (target_siz_t)(iter->ar_addr-last_range_end);
    if (last_range_end+unused_size >= section_size) goto done_step2;
    if (unused_size) DCCFreeData_Release(&self->sc_free,last_range_end,unused_size);
    last_range_end = iter->ar_addr+iter->ar_size;
    iter = iter->ar_next;
   }
   /* Mark the free memory between the last
    * range and the section end as unused. */
   assert(section_size >= last_range_end);
   unused_size = section_size-last_range_end;
   if (unused_size) DCCFreeData_Release(&self->sc_free,last_range_end,unused_size);
done_step2:;
 }
}

PUBLIC size_t
DCCSection_MergeSymbols(struct DCCSection *__restrict self) {
 struct DCCSym *sym; size_t result = 0;
 assert(self),DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 if (!(self->sc_start.sy_flags&DCC_SYMFLAG_SEC_M) ||
      (self->sc_start.sy_flags&DCC_SYMFLAG_NOCOLL)) goto end;
 DCCSection_ENUMSYM(sym,self) {
  target_ptr_t new_addr;
  DCCSym_ASSERT(sym);
  assert(sym->sy_sec == self);
  if (!sym->sy_size || (sym->sy_flags&DCC_SYMFLAG_NOCOLL)) continue;
  new_addr = DCCSection_DMerge(self,sym->sy_addr,sym->sy_size,sym->sy_align,0);
  if (new_addr != sym->sy_addr) {
   /* Update the symbol address. */
   DCCSection_DIncref(self,new_addr,sym->sy_size);
   DCCSection_DDecref(self,sym->sy_addr,sym->sy_size);
   sym->sy_addr = new_addr;
   ++result;
  }
 }
end:
 return result;
}

PUBLIC size_t
DCCSection_CollapseSymbols(struct DCCSection *__restrict self) {
 struct DCCSym *sym;
 size_t result = 0;
 assert(self),DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 if (self->sc_start.sy_flags&DCC_SYMFLAG_NOCOLL) goto end;
 /* TODO: This could be further optimized by trying
  *       to collapse larger symbols first! */
 DCCSection_ENUMSYM(sym,self) {
  target_ptr_t below_addr;
  DCCSym_ASSERT(sym);
  assert(sym->sy_sec == self);
  if (!sym->sy_size || (sym->sy_flags&DCC_SYMFLAG_NOCOLL)) continue;
  /* Try to allocate free memory below. */
  /* TODO: This acquire call should consider the memory region
   *      'sym->sy_addr..sym->sy_size' as available.
   *       Otherwise, we can't move large functions just a bit
   *       when a small area of memory becomes available below. */
  below_addr = DCCFreeData_AcquireBelow(&self->sc_free,sym->sy_addr,
                                        sym->sy_size,sym->sy_align,0);
  if (below_addr != DCC_FREEDATA_INVPTR) {
   /* Must collapse this symbol! */
   uint8_t *src_data,*dst_data;
   assert(below_addr < sym->sy_addr);
   src_data = (uint8_t *)DCCSection_GetText(self,sym->sy_addr,sym->sy_size);
   if unlikely(!src_data) continue;
   dst_data = self->sc_text.tb_begin+below_addr;
   assert(dst_data < src_data);
   assert(dst_data+sym->sy_size <= src_data);
   /* Move all relocations to the lower memory address. */
   DCCSection_Movrel(self,below_addr,sym->sy_addr,sym->sy_size);
   DCCA2l_Mov(&self->sc_a2l,below_addr,sym->sy_addr,sym->sy_size);
   /* Copy symbol data to lower memory. */
   memcpy(dst_data,src_data,sym->sy_size);
   /* Update section data reference counters. */
   DCCSection_DIncref(self,below_addr,sym->sy_size);
   DCCSection_DDecref(self,sym->sy_addr,sym->sy_size);
   /* Update the symbol address. */
   sym->sy_addr = below_addr;
   ++result;
  }
 }
end:
 return result;
}

PUBLIC target_siz_t
DCCSection_TrimFree(struct DCCSection *__restrict self) {
 struct DCCFreeRange **plast_range,*last_range;
 target_siz_t result = 0;
 assert(self),DCCSECTION_ASSERT_TEXT_FLUSHED(self);
 last_range = *(plast_range = &self->sc_free.fd_begin);
 /* Without any free ranges, we can't trim anything. */
 if unlikely(!last_range) goto end;
 while (last_range->fr_next) {
  assert(last_range->fr_size);
  assert(last_range->fr_next->fr_addr >
         last_range->fr_addr+
         last_range->fr_size);
  plast_range = &last_range->fr_next;
  last_range = *plast_range;
 }
 assert((target_ptr_t)(last_range->fr_addr+last_range->fr_size) <=
        (target_ptr_t)(self->sc_text.tb_max-self->sc_text.tb_begin));
 if ((target_ptr_t)(last_range->fr_addr+last_range->fr_size) ==
     (target_ptr_t)(self->sc_text.tb_max-self->sc_text.tb_begin)) {
  /* The last free range describes the end of the section.
   * >> We can now trim the section to release data near its end. */
  result = last_range->fr_size;
  self->sc_text.tb_max = self->sc_text.tb_begin+last_range->fr_addr;
  /* Clamp the text pointer at the end. */
  if (self->sc_text.tb_pos > self->sc_text.tb_max)
      self->sc_text.tb_pos = self->sc_text.tb_max;
  /* Delete the last free range. */
  assert(!last_range->fr_next);
  *plast_range = NULL;
  free(last_range);
 }
end:
 return result;
}



DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_SECCOLL_C_INL */
