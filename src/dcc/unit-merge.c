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
#ifndef GUARD_DCC_UNIT_MERGE_C
#define GUARD_DCC_UNIT_MERGE_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/target.h>
#include <dcc/vstack.h>
#include <dcc/binary.h>
#include <dcc/stream.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#ifdef _WIN32
#include <Windows.h>
#else
#include <sys/mman.h>
#endif

DCC_DECL_BEGIN

extern void DCCSection_InsSym(struct DCCSection *__restrict self, struct DCCSym *__restrict sym);
extern void DCCUnit_InsSym(/*ref*/struct DCCSym *__restrict sym);
PRIVATE struct DCCSym *inherit_named_sym(/*ref*/struct DCCSym *__restrict srcsym);
PRIVATE struct DCCSym *inherit_any_sym(/*ref*/struct DCCSym *__restrict srcsym);

PRIVATE void
inherit_sym(/*ref*/struct DCCSym *__restrict srcsym) {
 struct DCCSym *beforesym;
 struct DCCSection *dstsec;
 for (beforesym = srcsym->sy_unit_before;
      beforesym; beforesym = beforesym->sy_unit_before) {
  /* Make sure to inherit all old definitions as well! */
  assert(beforesym->sy_name == srcsym->sy_name);
  DCCSym_ASSERT(beforesym);
  if (beforesym->sy_sec) {
   if ((*beforesym->sy_sec_pself = beforesym->sy_sec_next) != NULL)
         beforesym->sy_sec_next->sy_sec_pself = beforesym->sy_sec_pself;
   assert(beforesym->sy_sec->sc_symc);
   --beforesym->sy_sec->sc_symc;
   dstsec = DCCUnit_GetSec(beforesym->sy_sec->sc_start.sy_name);
   assert(dstsec);
   DCCSection_Incref(dstsec);
   DCCSection_Decref(beforesym->sy_sec);
   beforesym->sy_sec = dstsec; /* Drop & Inherit reference. */
   if (!DCCSection_ISIMPORT(dstsec))
        beforesym->sy_addr += dstsec->sc_merge;
   DCCSection_InsSym(beforesym->sy_sec,beforesym);
  } else if (beforesym->sy_alias) {
   beforesym->sy_alias = inherit_any_sym(beforesym->sy_alias);
   DCCSym_ASSERT(beforesym->sy_alias);
   DCCSym_Incref(beforesym->sy_alias);
  }
 }
 if (srcsym->sy_sec) {
  if (srcsym->sy_sec == &DCCSection_Abs) {
   /* Special case: Symbol from the ABS section. */
   if (srcsym == &DCCSection_Abs.sc_start) {
    /* Special case: This is the ABS section. */
    goto already_inherited;
   }
   goto ins_ndef_sym;
  }
  assert(srcsym->sy_sec_pself);
  dstsec = DCCUnit_GetSec(srcsym->sy_sec->sc_start.sy_name);
  assert(dstsec);
  if (dstsec == srcsym->sy_sec) {
   assert(srcsym->sy_refcnt >= 2);
   --srcsym->sy_refcnt;
   return;
  }
  if ((*srcsym->sy_sec_pself = srcsym->sy_sec_next) != NULL)
        srcsym->sy_sec_next->sy_sec_pself = srcsym->sy_sec_pself;
  assert(srcsym->sy_sec->sc_symc >= 1);
  --srcsym->sy_sec->sc_symc;
  DCCSection_Incref(dstsec);
  DCCSection_Decref(srcsym->sy_sec);
  srcsym->sy_sec   = dstsec; /* Override. */
  if (!DCCSection_ISIMPORT(dstsec))
       srcsym->sy_addr += dstsec->sc_merge; /* Adjust symbol address. */
  DCCSection_InsSym(dstsec,srcsym);
 } else {
  /* Inherit a symbol. */
  assert(!srcsym->sy_sec);
  assert(!srcsym->sy_sec_pself);
  assert(!srcsym->sy_sec_next);
ins_ndef_sym:
  if (srcsym->sy_unit_next ||
      unit.u_nsym == srcsym) {
already_inherited:
   DCCSym_Decref(srcsym);
   return;
  }
  if (srcsym->sy_name != &TPPKeyword_Empty && unit.u_syma) {
   struct DCCSym *inherited_sym = unit.u_symv[srcsym->sy_name->k_id % unit.u_syma];
   while (inherited_sym) {
    if (inherited_sym == srcsym) goto already_inherited;
    inherited_sym = inherited_sym->sy_unit_next;
   }
  }

  assert(!srcsym->sy_unit_next);
  if (srcsym->sy_alias) {
   srcsym->sy_alias = inherit_any_sym(srcsym->sy_alias);
   DCCSym_ASSERT(srcsym->sy_alias);
   DCCSym_Incref(srcsym->sy_alias);
  }
 }
 DCCUnit_InsSym(srcsym); /* Inherit reference. */
}

PRIVATE struct DCCSym *
inherit_named_sym(/*ref*/struct DCCSym *__restrict srcsym) {
 struct DCCSym *dstsym;
 assert(srcsym);
 dstsym = DCCUnit_GetSym(srcsym->sy_name);
 if (srcsym == dstsym) goto drop_srcsym;
 if (dstsym &&
     /* If either symbol was declared as static, allow both declarations. */
   !(dstsym->sy_flags&DCC_SYMFLAG_STATIC) &&
   !(srcsym->sy_flags&DCC_SYMFLAG_STATIC)) {
  /* The same named symbol exists in both units.
   * >> Redeclare the existing symbol as the new one! */
  if (srcsym->sy_alias) {
   struct DCCSym *alias = inherit_any_sym(srcsym->sy_alias);
   srcsym->sy_alias = NULL; /* Inherited above. */
   DCCSym_ASSERT(alias);
   if (DCCSym_ISFORWARD(dstsym) ||
     !(srcsym->sy_flags&DCC_SYMFLAG_WEAK)) {
    DCCSym_Alias(dstsym,alias);
   }
  } else if (srcsym->sy_sec && !DCCSym_ISSECTION(srcsym)) {
   struct DCCSection *dstsec;
   assert(srcsym->sy_sec->sc_start.sy_name);
   assert(srcsym->sy_sec->sc_start.sy_name != &TPPKeyword_Empty);
   if (DCCSym_ISFORWARD(dstsym) ||
     !(srcsym->sy_flags&DCC_SYMFLAG_WEAK)) {
    target_ptr_t merge_addr;
    dstsec = DCCUnit_GetSec(srcsym->sy_sec->sc_start.sy_name);
    assert(dstsec);
    /* Define after the merge location. */
    merge_addr = srcsym->sy_addr;
    if (!DCCSection_ISIMPORT(dstsec))
         merge_addr += dstsec->sc_merge;
    DCCSym_Define(dstsym,dstsec,merge_addr,srcsym->sy_size);
   }
  }
drop_srcsym:
  DCCSym_Decref(srcsym); /* Drop reference (all are inherited below). */
 } else {
  inherit_sym(srcsym);
  dstsym = srcsym;
 }
 return dstsym;
}
PRIVATE struct DCCSym *
inherit_any_sym(/*ref*/struct DCCSym *__restrict srcsym) {
 assert(srcsym);
 if (srcsym->sy_name != &TPPKeyword_Empty) {
  srcsym = inherit_named_sym(srcsym);
 } else {
  inherit_sym(srcsym);
 }
 return srcsym;
}

#if DCC_DEBUG
PRIVATE void
validate_sym(struct DCCSym *__restrict sym) {
 DCCSym_ASSERT(sym);
 if (sym->sy_unit_before) validate_sym(sym->sy_unit_before);
 if (sym->sy_alias) {
  validate_sym(sym->sy_alias);
 } else if (sym->sy_sec) {
  struct DCCSection *sec;
  target_siz_t sec_size;
  DCCUnit_ENUMSEC(sec) if (sec == sym->sy_sec) goto found_sec;
  DCCUnit_ENUMIMP(sec) if (sec == sym->sy_sec) goto found_sec;
  if (sym->sy_sec == &DCCSection_Abs) goto found_sec;
  assertf(0,"Section '%s' of symbol '%s' is not part of the unit",
          sym->sy_sec->sc_start.sy_name->k_name,
          sym->sy_name->k_name);
found_sec:
  if (!DCCSection_ISIMPORT(sym->sy_sec)) {
   DCCSection_TBEGIN(sym->sy_sec);
   sec_size = (target_siz_t)(sym->sy_sec->sc_text.tb_max-
                             sym->sy_sec->sc_text.tb_begin);
   DCCSection_TEND(sym->sy_sec);
   assert(sym->sy_addr+sym->sy_size >= sym->sy_addr);
   assertf(sym->sy_addr+sym->sy_size <= sec_size,
           "Symbol '%s' address range %lu..%lu is out-of-bounds of section '%s' range 0..%lu",
           sym->sy_name->k_name,
          (unsigned long)sym->sy_addr,
          (unsigned long)sym->sy_addr+sym->sy_size,
           sym->sy_sec->sc_start.sy_name->k_name,
          (unsigned long)sec_size);
  }
 }
}
#endif


PUBLIC void
DCCUnit_Merge(struct DCCUnit *__restrict other) {
 struct DCCSection *srcsec,*dstsec;
 struct DCCSym *srcsym,*nextsym;
 struct DCCSym **sym_iter,**sym_end,**psym;
 struct DCCRel *rel_iter,*rel_end,*reldst;
 assert(other);
 assert(other != &unit);
 /* Merge all sections & section data. */
 for (srcsec = other->u_secs; srcsec; srcsec = srcsec->sc_next) {
  target_ptr_t other_sec_base;
  target_siz_t other_sec_size;
  uint8_t *dst_buffer;
  assert(!(srcsec->sc_start.sy_flags&DCC_SYMFLAG_SEC_ISIMPORT));
  dstsec = DCCUnit_NewSec(srcsec->sc_start.sy_name,
                          srcsec->sc_start.sy_flags);
  if unlikely(!dstsec) return;
  if (DCCSection_ISIMPORT(dstsec)) {
   /* Can't merge non-import-section with import-section. */
   WARN(W_LINKER_CANT_MERGE_NONIMPORT_WITH_IMPORT,
        srcsec->sc_start.sy_name,dstsec->sc_start.sy_name);
  } else {
   other_sec_size = DCCSection_VSIZE(srcsec);
   /* Allocate space in the destination section. */
   if (srcsec->sc_start.sy_flags&DCC_SYMFLAG_SEC_U) srcsec->sc_align = 1;
   other_sec_base = DCCSection_DAlloc(dstsec,other_sec_size,srcsec->sc_align,0);
   other_sec_size = DCCSection_MSIZE(srcsec);
   if (other_sec_size) {
    dst_buffer = (uint8_t *)DCCSection_GetText(dstsec,other_sec_base,other_sec_size);
    if unlikely(!dst_buffer) return;
    /* Append raw section memory to the end. */
    memcpy(dst_buffer,srcsec->sc_text.tb_begin,other_sec_size);
   }
   dstsec->sc_merge = other_sec_base;
   /* TODO: Inherit all free data ranges. */
   //srcsec->sc_free.fd_begin;
  }
 }

 /* Merge imports. */
 for (srcsec = other->u_imps; srcsec; srcsec = srcsec->sc_next) {
  assert(srcsec->sc_start.sy_flags&DCC_SYMFLAG_SEC_ISIMPORT);
  dstsec = DCCUnit_NewSec(srcsec->sc_start.sy_name,
                          srcsec->sc_start.sy_flags);
  if unlikely(!dstsec) return;
  if (!DCCSection_ISIMPORT(dstsec)) {
   /* Can't merge import-section with non-import-section. */
   WARN(W_LINKER_CANT_MERGE_IMPORT_WITH_NONIMPORT,
        srcsec->sc_start.sy_name,dstsec->sc_start.sy_name);
  }
 }

 /* Now it's time to move all the symbols. */
 sym_end = (sym_iter = other->u_symv)+other->u_syma;
 for (; sym_iter != sym_end; ++sym_iter) {
  psym = sym_iter;
  while ((srcsym = *psym) != NULL) {
   DCCSym_ASSERT(srcsym);
   assert(srcsym->sy_name != &TPPKeyword_Empty);
   if (!DCCSym_ISSECTION(srcsym)) {
    *psym = srcsym->sy_unit_next;
    srcsym->sy_unit_next = NULL; /* Ensure consistent state. */
    inherit_named_sym(srcsym);
   } else {
    psym = &srcsym->sy_unit_next;
   }
  }
 }

 /* Inherit all unnamed symbols. */
 for (srcsym = other->u_nsym; srcsym; ) {
  nextsym = srcsym->sy_unit_next;
  DCCSym_ASSERT(srcsym);
  srcsym->sy_unit_next = NULL;
  /* No need to check for name matches (they're all unnamed!) */
  inherit_sym(srcsym);
  srcsym = nextsym;
 }
 other->u_nsym = NULL;


 /* Finally, copy all the relocations. */
 for (srcsec = other->u_secs; srcsec; srcsec = srcsec->sc_next) {
  uint8_t *relbase;
  target_ptr_t merge_base;
  if (!srcsec->sc_relc) continue;
  //assertf(!srcsec->sc_symc,"Section '%s' still contains symbols after all were inherited",
  //         srcsec->sc_start.sy_name->k_name);
  dstsec = DCCUnit_GetSec(srcsec->sc_start.sy_name);
  assert(dstsec);
  if (DCCSection_ISIMPORT(dstsec)) {
   /* Can't add relocations to import section. */
   WARN(W_LINKER_CANT_RELOC_LIB_SECTION,dstsec->sc_start.sy_name);
   continue;
  }
  rel_end = (rel_iter = srcsec->sc_relv)+srcsec->sc_relc;
  reldst = DCCSection_Allocrel(dstsec,srcsec->sc_relc);
  merge_base = dstsec->sc_merge;
  relbase = dstsec->sc_text.tb_begin;
  for (; rel_iter != rel_end; ++rel_iter,++reldst) {
   uint8_t *reldata;
   assert(rel_iter->r_sym);
   *reldst = *rel_iter; /* Inherit reference: 'rel_iter->r_sym'. */
   reldst->r_sym = inherit_any_sym(reldst->r_sym);
   DCCSym_Incref(reldst->r_sym);
   reldst->r_addr += merge_base; /* Adjust for merge offset. */
   reldata = relbase+reldst->r_addr;
   assert(reldata >= relbase);
   assert(reldata <  relbase+(dstsec->sc_text.tb_end-
                              dstsec->sc_text.tb_begin));
   /* Adjust disposition relocations (Must subtract 'merge_base'). */
   switch (reldst->r_type) {
#if DCC_TARGET_IA32(386)
    case R_386_PC8:  *(int8_t  *)reldata -= (int8_t )merge_base; break;
    case R_386_PC16: *(int16_t *)reldata -= (int16_t)merge_base; break;
    case R_386_PC32: *(int32_t *)reldata -= (int32_t)merge_base; break;
#elif DCC_TARGET_CPU == DCC_TARGET_X86_64
    case R_X86_64_PC8:  *(int8_t  *)reldata -= (int8_t )merge_base; break;
    case R_X86_64_PC16: *(int16_t *)reldata -= (int16_t)merge_base; break;
    case R_X86_64_PC32: *(int32_t *)reldata -= (int32_t)merge_base; break;
#endif
    default: break;
   }
  }
  free(srcsec->sc_relv); /* Inherit _all_ symbols. */
  /* Ensure consistent state. */
  srcsec->sc_rela = 0;
  srcsec->sc_relc = 0;
  srcsec->sc_relv = NULL;
 }
#if DCC_DEBUG
 { /* Validate all symbol ranges & associations. */
   struct DCCSym *sym;
   DCCUnit_ENUMSYM(sym) validate_sym(sym);
   sym = unit.u_nsym;
   while (sym) validate_sym(sym),sym = sym->sy_unit_next;
 }
#endif
}

PUBLIC void
DCCUnit_Extract(struct DCCUnit *__restrict other) {
 struct DCCSection *iter;
 assert(other);
 assert(other != &unit);
 *other = unit;
 DCCUnit_Init(&unit);
 iter = other->u_secs;
 if (iter) {
  assert(iter->sc_pself == &unit.u_secs);
  iter->sc_pself = &other->u_secs;
  do {
   assert(!DCCSection_ISIMPORT(iter));
   assert(iter->sc_unit == &unit);
   iter->sc_unit = other;
  } while ((iter = iter->sc_next) != NULL);
 }
 iter = other->u_imps;
 if (iter) {
  assert(iter->sc_pself == &unit.u_imps);
  iter->sc_pself = &other->u_imps;
  do {
   assert(DCCSection_ISIMPORT(iter));
   assert(iter->sc_unit == &unit);
   iter->sc_unit = other;
  } while ((iter = iter->sc_next) != NULL);
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_MERGE_C */
