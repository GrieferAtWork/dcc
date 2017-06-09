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
#include <dcc/compiler.h>
#include <dcc/linker.h>
#include <dcc/stream.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/vstack.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#if DCC_TARGET_BIN == DCC_BINARY_PE
#include "linker-pe.h"
#endif


#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

DCC_DECL_BEGIN

INTDEF void DCCSection_InsSym(struct DCCSection *__restrict self, struct DCCSym *__restrict sym);
INTDEF void DCCUnit_InsSym(/*ref*/struct DCCSym *__restrict sym);

#if DCC_DEBUG
PRIVATE void
validate_sym(struct DCCSym *__restrict sym) {
 DCCSym_ASSERT(sym);
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
   assert(DCCSym_ISSECTION(sym) ||
          sym->sy_addr+sym->sy_size >= sym->sy_addr);
   assertf(DCCSym_ISSECTION(sym) ||
           sym->sy_addr+sym->sy_size <= sec_size,
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

INTDEF int
DCCSection_DIncrefN_impl(struct DCCSection *__restrict self,
                         target_ptr_t addr, target_siz_t size,
                         unsigned int n_refcnt);
PRIVATE void DCCSection_InsAlloc(struct DCCSection *__restrict self,
                    /*inherited*/struct DCCAllocRange *range,
                                 target_ptr_t ins_base) {
 struct DCCAllocRange *iter,*next,**pinsert;
 if unlikely(!range) return; /* nothing to inherit! */
 pinsert = &self->sc_alloc;
 while ((iter = *pinsert) != NULL &&
         iter->ar_addr+iter->ar_size <= ins_base)
         pinsert = &iter->ar_next;
 assert(iter == *pinsert);
 if (!iter) {
  /* Simple (and likely) case: We can append the inherited range chain at the end. */
  if (ins_base) {
   for (iter = range; iter; iter = iter->ar_next)
        iter->ar_addr += ins_base;
  }
  *pinsert = range; /* Inherit _all_ */
  if (pinsert != &self->sc_alloc) {
   /* Check if we must merge the last range of 'self' with 'range'. */
   iter = (struct DCCAllocRange *)((uintptr_t)pinsert-
                                   offsetof(struct DCCAllocRange,ar_next));
   if (iter->ar_refcnt == range->ar_refcnt &&
       iter->ar_addr+iter->ar_size == range->ar_addr) {
    /* Extend the last range of 'self' and delete the first from 'range'. */
    iter->ar_size += range->ar_size;
    iter->ar_next  = range->ar_next;
    free(range);
   }
  }
 } else {
  /* Complicated case: Manually merge all references by allocating new ones. */
  iter = range;
  for (;;) {
   assert(iter);
   assert(iter->ar_refcnt);
   DCCSection_DIncrefN_impl(self,
                            iter->ar_addr+ins_base,
                            iter->ar_size,
                            iter->ar_refcnt);
   next = iter->ar_next;
   free(iter);
   if unlikely(!next) break;
   iter = next;
  }
 }
}


PUBLIC void
DCCUnit_Merge(struct DCCUnit *__restrict other) {
 struct DCCSection *srcsec;
 assert(other);
 assert(other != &unit);
 TPPLexer_PushFile(&TPPFile_Merge);
 if (!unit.u_nsymc && !unit.u_symc) {
  /* Special case: Without anything defined, we can simply restore 'other'! */
  assert(!unit.u_secc);
  assert(!unit.u_impc);
  DCCUnit_Restore(other);
  /* Ensure that 'other' is in a valid state. */
  memset(other,0,sizeof(struct DCCUnit));
  return;
 }

 /* Merge all sections & section data. */
 for (srcsec = other->u_secs; srcsec; srcsec = srcsec->sc_next) {
  struct DCCSection *dstsec;
  target_ptr_t other_sec_base;
  target_siz_t other_sec_size;
  uint8_t *dst_buffer;
  assert(!(srcsec->sc_start.sy_flags&DCC_SYMFLAG_SEC_ISIMPORT));
  assert(DCCSym_ISSECTION(&srcsec->sc_start));
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
   if (srcsec->sc_start.sy_flags&DCC_SYMFLAG_SEC_U) srcsec->sc_start.sy_align = 1;
   other_sec_base = DCCSection_DAlloc(dstsec,other_sec_size,
                                      srcsec->sc_start.sy_align,0);
   other_sec_size = DCCSection_MSIZE(srcsec);
   if (other_sec_size) {
    dst_buffer = (uint8_t *)DCCSection_GetText(dstsec,other_sec_base,other_sec_size);
    if unlikely(!dst_buffer) return;
    /* Append raw section memory to the end. */
    memcpy(dst_buffer,srcsec->sc_text.tb_begin,other_sec_size);
   }
   dstsec->sc_merge = other_sec_base;
   /* Inherit all allocated data ranges. */
   DCCSection_InsAlloc(dstsec,srcsec->sc_alloc,other_sec_base);
   srcsec->sc_alloc = NULL;
   /* Inherit all free data ranges. */
   if (srcsec->sc_free.fd_begin) {
    struct DCCFreeRange **pinsert,*ins,*iter,*before_ins;
    /* Update all source free address ranges. */
    iter = srcsec->sc_free.fd_begin;
    do iter->fr_addr += other_sec_base;
    while ((iter = iter->fr_next) != NULL);

    pinsert = &dstsec->sc_free.fd_begin;
    while ((ins = *pinsert) != NULL &&
            ins->fr_addr < other_sec_base)
            pinsert = &ins->fr_next;
    /* Insert all source free ranges into '*pinsert' */
    *pinsert = iter = srcsec->sc_free.fd_begin;
    if (pinsert != &dstsec->sc_free.fd_begin) {
     /* Must check to see if we must merge this free range with the previous one! */
     before_ins = (struct DCCFreeRange *)((uintptr_t)pinsert-
                                          offsetof(struct DCCFreeRange,fr_next));
     if (before_ins->fr_addr+before_ins->fr_size == iter->fr_addr) {
      /* Must merge the first range of the new section with the last of the old. */
      before_ins->fr_size += iter->fr_size;
      before_ins->fr_next = iter->fr_next;
      free(iter);
      iter = before_ins->fr_next;
     }
    }
    srcsec->sc_free.fd_begin = NULL;
    if (ins) {
     /* Must append this portion to the end. */
     while (iter->fr_next) iter = iter->fr_next;
     assert(ins->fr_addr >= iter->fr_addr+iter->fr_size);
     if (iter->fr_addr+iter->fr_size == ins->fr_addr) {
      /* Must merge the last range of the new section
       * with the one following the insertion point. */
      iter->fr_size += ins->fr_size;
      iter->fr_next = ins->fr_next;
      free(ins);
     } else {
      iter->fr_next = ins;
     }
    }
   }
  }
 }

 /* Merge debug informations. */
 if ((linker.l_flags&DCC_LINKER_FLAG_GENDEBUG)) {
  target_ptr_t text_merge;
  if (unit.u_dbgstr) text_merge = unit.u_dbgstr->sc_merge;
  else {
   struct DCCSection *debug_string;
   debug_string = DCCUnit_GetSecs(A2L_STRING_SECTION);
   text_merge = debug_string ? debug_string->sc_merge : 0;
  }
  for (srcsec = other->u_secs; srcsec; srcsec = srcsec->sc_next) {
   struct DCCSection *dstsec;
   dstsec = DCCUnit_GetSec(srcsec->sc_start.sy_name);
   if (dstsec) {
    /* Merge debug informations. */
    DCCA2l_RelocString(&srcsec->sc_a2l,text_merge);
    DCCA2l_Merge(&dstsec->sc_a2l,&srcsec->sc_a2l,dstsec->sc_merge);
   }
  }
 }

 /* Merge imports. */
 for (srcsec = other->u_imps; srcsec; srcsec = srcsec->sc_next) {
  struct DCCSection *dstsec;
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

 {
  struct DCCSym **symv,*iter,*follow,*next;
  struct DCCSym **next_free,**sym_iter,**sym_end,**old_end;
  size_t symc;
  symc = other->u_symc+other->u_nsymc;
  symv = other->u_symv;
  /* Flatten the hash-map into a vector. */
  if (other->u_syma < symc) {
   symv = (struct DCCSym **)DCC_Realloc(symv,symc*sizeof(struct DCCSym *),0);
   if unlikely(!symv) return;
   memset(symv+other->u_syma,0,
         (size_t)(symc-other->u_syma)*
          sizeof(struct DCCSym *));
  }
  next_free = symv;
  old_end = (sym_iter = symv)+other->u_syma;
  sym_end = symv+symc;
  for (; sym_iter != old_end; ++sym_iter) {
   if ((iter = *sym_iter) != NULL) {
    follow = iter->sy_unit_next;
    iter->sy_unit_next = NULL;
    assert(iter->sy_name != &TPPKeyword_Empty);
    while (follow) {
     assert(follow->sy_name != &TPPKeyword_Empty);
     /* Search for the next free slot. */
     while (*next_free) assert(next_free != sym_end),++next_free;
     next = follow->sy_unit_next;
     follow->sy_unit_next = NULL;
     *next_free = follow;
     follow = next;
    }
   }
  }
  other->u_symc = 0;
  other->u_syma = 0;
  other->u_symv = NULL;

  /* Merge unnamed symbols. */
  follow = other->u_nsym;
  other->u_nsymc = 0;
  other->u_nsym  = NULL;
  while (follow) {
   while (*next_free) assert(next_free != sym_end),++next_free;
   assert(follow->sy_name == &TPPKeyword_Empty);
   next = follow->sy_unit_next;
   follow->sy_unit_next = NULL;
   *next_free = follow;
   follow = next;
  }

  /* Some named symbols may still exist between 'symv+symc' and 'old_end'.
   * >> Shift them downwards! */
  for (sym_iter = symv+symc; sym_iter < old_end; ++sym_iter) {
   if ((follow = *sym_iter) != NULL) {
    while (*next_free) assert(next_free != sym_end),++next_free;
    *next_free = follow;
   }
  }

  /* Update to the new symbol vector end. */
  sym_end = symv+symc;

  /* At this point, _ALL_ (named + unnamed) symbols from the old unit are collected. */
  /* Step #1: Fix all section symbols to point into the new section. */
  for (sym_iter = symv; sym_iter != sym_end; ++sym_iter) {
   struct DCCSym *src_sym = *sym_iter;
   /* Inherit all named symbols (And search for new symbol bindings). */
   assert(src_sym);
   assert(!src_sym->sy_unit_next);
   if (src_sym->sy_sec && !DCCSym_ISSECTION(src_sym) &&
       src_sym->sy_sec != &DCCSection_Abs) {
    struct DCCSection *new_section;
    /* Fix section linkage. */
    new_section = DCCUnit_GetSec(src_sym->sy_sec->sc_start.sy_name);
    assertf(new_section,"Section '%s' not properly imported",
            src_sym->sy_sec->sc_start.sy_name->k_name);
    DCCSection_Incref(new_section);
    /* Unlike the symbol from its old section. */
    if ((*src_sym->sy_sec_pself = src_sym->sy_sec_next) != NULL)
          src_sym->sy_sec_next->sy_sec_pself = src_sym->sy_sec_pself;
    assert(src_sym->sy_sec->sc_symc);
    --src_sym->sy_sec->sc_symc;
    DCCSection_Decref(src_sym->sy_sec);
#if DCC_DEBUG
    src_sym->sy_sec_pself = NULL;
    src_sym->sy_sec_next  = NULL;
#endif
    if (!DCCSection_ISIMPORT(new_section)) {
     /* Adjust the symbol address accordingly. */
     src_sym->sy_addr += new_section->sc_merge;
    }
    src_sym->sy_sec = new_section; /* Inherit reference. */
    /* Insert the symbol into its new section. */
    DCCSection_InsSym(new_section,src_sym);
   }
  }

  /* Step #2: Inherit all symbols into the new unit. */
  for (sym_iter = symv; sym_iter != sym_end; ++sym_iter) {
   struct DCCSym *dst_sym,*src_sym = *sym_iter;
   /* Inherit all named symbols (And search for new symbol bindings). */
   assert(src_sym);
   assert(!src_sym->sy_unit_next);
   if (DCCSym_ISSECTION(src_sym)) {
    /* Get rid of section symbols. */
    /* This reference is dropped later after relocations are parsed. */
    //DCCSym_Decref(src_sym);
    *sym_iter = NULL; /* This reference is dropped later. */
   } else {
#if DCC_TARGET_BIN == DCC_BINARY_PE
    if (src_sym->sy_peind) {
     /* Check if the current unit has an ITA symbol with the same name.
      * >> If it does, re-line the PE-IND reference to point to shared ITA symbol. */
     assert(src_sym->sy_peind->sy_name != &TPPKeyword_Empty);
     if ((dst_sym = DCCUnit_GetSym(src_sym->sy_peind->sy_name)) != NULL) {
      /* There is a new ITA symbol! */
      DCCSym_Incref(dst_sym); /* Incref before, in case the symbols already match. */
      DCCSym_Decref(src_sym->sy_peind);
      src_sym->sy_peind = dst_sym; /* Override reference. */
     }
    }
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
    if ((src_sym->sy_flags&DCC_SYMFLAG_STATIC) ||
        (assert(src_sym->sy_name != &TPPKeyword_Empty),
         dst_sym = DCCUnit_GetSym(src_sym->sy_name)) == NULL) {
     /* Load static symbols as though they were unnamed. */
     DCCSym_Incref(src_sym);
     DCCUnit_InsSym(src_sym); /* Inherit reference. */
    } else {
     assert(src_sym != dst_sym);
#if DCC_TARGET_BIN == DCC_BINARY_PE
     if (src_sym->sy_peind) {
      assertf(!dst_sym->sy_peind || dst_sym->sy_peind == src_sym->sy_peind,
              "If this isn't the same IAT symbol, then how come we didn't find it before?");
      if (!dst_sym->sy_peind) {
       dst_sym->sy_peind = src_sym->sy_peind;
       DCCSym_Incref(src_sym->sy_peind);
      }
     }
#endif
     if (DCCSym_ISFORWARD(src_sym)) {
      /* Get rid of undefined symbols. (Since these are named,
       * relocations to the symbol will be fixed later) */
drop_srcsym:
      DCCSym_Decref(src_sym);
      *sym_iter = NULL;
     } else {
      /* NOTE: Technically, we could exchange 'dst_sym' here if its reference counter was ONE(1). */
      assert(src_sym->sy_name != &TPPKeyword_Empty);
      /* At thing point, we know that the symbol is used in
       * the current unit, meaning we can't just replace an
       * existing declaration, but must define a new one.
       * >> Yet at this point, we can already filter symbol re-declaration cases. */
      if (!DCCSym_ISFORWARD(dst_sym)) {
       /* Don't override symbol declaration with a weak symbol.
        * >> Instead, drop the source symbol and let relocations below
        *    link every use of 'src_sym' against the existing symbol. */
       if (src_sym->sy_flags&DCC_SYMFLAG_WEAK) goto drop_srcsym;
      }
     }
    }
   }
  }
  /* Step #3: All unique symbols have been imported.
   *          With that in mind, we must now go through again and
   *          Re-define/alias all remaining symbol declarations. */
  for (sym_iter = symv; sym_iter != sym_end; ++sym_iter) {
   struct DCCSym *src_sym = *sym_iter;
   if (src_sym) {
    struct DCCSym *dst_sym,*old_alias,*alias_target;
    dst_sym = DCCUnit_GetSym(src_sym->sy_name);
    /* Fix an alias declaration for this symbol. */
    if ((old_alias = src_sym->sy_alias) != NULL &&
       !(old_alias->sy_flags&DCC_SYMFLAG_STATIC)) {
     assert(old_alias->sy_name != &TPPKeyword_Empty);
     /* Replace the alias symbol. */
     alias_target = DCCUnit_GetSym(old_alias->sy_name);
     assertf(alias_target,"Missing alias symbol '%s'",alias_target->sy_name->k_name);
     DCCSym_Incref(alias_target);
     DCCSym_Decref(old_alias);
     src_sym->sy_alias = alias_target; /* Inherit reference (both ways). */
    }
    if (dst_sym && dst_sym != src_sym &&
      !(src_sym->sy_flags&DCC_SYMFLAG_STATIC) &&
      !(dst_sym->sy_flags&DCC_SYMFLAG_STATIC)) {
     /* Override an existing symbol, or define it. */
     if (src_sym->sy_sec) {
      /* Section merge adjustments were already made above! */
      DCCSym_Define(dst_sym,src_sym->sy_sec,
                    src_sym->sy_addr,0,
                    src_sym->sy_align);
      /* Really hacky: inherit all data.
       * Since we've already inherited all section references,
       * we must not create new ones when inheriting symbol data!
       * For that reason, we manually inherit the data after initially
       * lying by stating that the symbol wouldn't have a size.
       * >> This way, we prevent an unnecessary (and temporary)
       *    data reference to the source symbol! */
      dst_sym->sy_size = src_sym->sy_size;
      src_sym->sy_size = 0;
      goto merge_symflags;
     } else if (src_sym->sy_alias) {
      /* 'sy_alias' is known to belong to the merged unit. */
      DCCSym_Alias(dst_sym,src_sym->sy_alias,src_sym->sy_addr);
merge_symflags:
      dst_sym->sy_flags |= src_sym->sy_flags;
     }
    }
    /* Drop the vector reference to this symbol.
     * NOTE: If this doesn't kill it, it is likely that relocations below will! */
    DCCSym_Decref(src_sym);
#if DCC_DEBUG
    *sym_iter = NULL; /* Symbolically inherit the reference. */
#endif
   }
  }
  /* And we're done! All symbols are inherited, or have been merged.
   * >> None should be remaining and once relocations have been fixed, everything should be OK! */
  free(symv);

#if DCC_TARGET_BIN == DCC_BINARY_PE && 0
  { struct DCCSym *sym,*basesym;
    /* Must re-link ITA functions. */
    DCCUnit_ENUMSYM(sym) {
     /* ----: Wouldn't it suffice only to relink ITA functions of symbols from 'other'.
      *       >> As in everything symbol from other with 'sy_peind' set? */
     assert(sym);
     if (sym->sy_name->k_size <= DCC_COMPILER_STRLEN(ITA_PREFIX)) continue;
     if (memcmp(sym->sy_name->k_name,ITA_PREFIX,
                DCC_COMPILER_STRLEN(ITA_PREFIX)*
                sizeof(char)) != 0) continue;
     /* This is an ITA symbol. - Try to find the associated base symbol and link them! */
     basesym = DCCUnit_GetSyms(sym->sy_name->k_name+DCC_COMPILER_STRLEN(ITA_PREFIX));
     if unlikely(!basesym) continue;
     DCCSym_Incref(sym);
     DCCSym_XDecref(basesym->sy_peind);
     basesym->sy_peind = sym; /* Inherit reference. */
    }
  }
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */

  /* Don't continue if something went wrong! */
  if unlikely(!OK) return;
 }

 /* Now copy all the relocations. */
 for (srcsec = other->u_secs; srcsec; srcsec = srcsec->sc_next) {
  struct DCCRel *rel_iter,*rel_end,*reldst,*rel_vec;
  struct DCCSection *dstsec;
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
  merge_base = dstsec->sc_merge;
  rel_end = (rel_iter = rel_vec = srcsec->sc_relv)+srcsec->sc_relc;
  reldst = DCCSection_Allocrel(dstsec,srcsec->sc_relc,merge_base);
  srcsec->sc_rela = 0;  /* Ensure consistent state. */
  srcsec->sc_relc = 0;
  srcsec->sc_relv = NULL;

  relbase = dstsec->sc_text.tb_begin;
  for (; rel_iter != rel_end; ++rel_iter,++reldst) {
   uint8_t *reldata; struct DCCSym *relsym;
   relsym = rel_iter->r_sym;
   DCCSym_ASSERT(relsym);
   *reldst = *rel_iter; /* Inherit reference: 'relsym'. */
   if (!(relsym->sy_flags&DCC_SYMFLAG_STATIC)) {
    assertf(relsym->sy_name != &TPPKeyword_Empty,
            "Non-static symbols _must_ have be named");
    /* Search for the new instance of a named symbol.
     * NOTE: We know that the symbol _must_ exist, because it was inherited above! */
    reldst->r_sym = DCCUnit_GetSym(relsym->sy_name);
    assertf(reldst->r_sym,"Symbol '%s' was not imported properly",
            relsym->sy_name->k_name);
    DCCSym_Incref(reldst->r_sym); /* Create reference to the new target. */
    DCCSym_Decref(relsym);        /* Drop reference from the old target. */
   } else {
    /* NOTE: All unnamed/static symbols were inherited above, meaning
     *       that 'reldst->r_sym' is part of the merged unit. */
   }
   reldst->r_addr += merge_base; /* Adjust for merge offset. */
   reldata = relbase+reldst->r_addr;
   assert(reldata >= relbase);
   assert(reldata <  relbase+(dstsec->sc_text.tb_end-
                              dstsec->sc_text.tb_begin));
   /* Adjust disposition relocations (Must subtract 'merge_base'). */
   switch (reldst->r_type) {
#if DCC_TARGET_IA32(386)
    case R_386_PC8:     *(int8_t  *)reldata -= (int8_t )merge_base; break;
    case R_386_PC16:    *(int16_t *)reldata -= (int16_t)merge_base; break;
    case R_386_PC32:    *(int32_t *)reldata -= (int32_t)merge_base; break;
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
    case R_X86_64_PC8:  *(int8_t  *)reldata -= (int8_t )merge_base; break;
    case R_X86_64_PC16: *(int16_t *)reldata -= (int16_t)merge_base; break;
    case R_X86_64_PC32: *(int32_t *)reldata -= (int32_t)merge_base; break;
#endif
    default: break;
   }
  }
  free(rel_vec); /* Inherit _all_ symbols. */
 }
 /* Finally, we must drop the section references we've
  * inherited during symbol inheritance Step #2. */
 for (srcsec = other->u_secs; srcsec;) {
  struct DCCSection *secnext = srcsec->sc_next;
  DCCSection_Decref(srcsec);
  srcsec = secnext;
 }
 for (srcsec = other->u_imps; srcsec;) {
  struct DCCSection *secnext = srcsec->sc_next;
  DCCSection_Decref(srcsec);
  srcsec = secnext;
 }
#if DCC_DEBUG
 { /* Validate all symbol ranges & associations. */
   struct DCCSym *sym;
   DCCUnit_ENUMSYM(sym) validate_sym(sym);
   sym = unit.u_nsym;
   while (sym) validate_sym(sym),sym = sym->sy_unit_next;
 }
#endif
 /* TODO: In sections that allow it, merge all symbols.
  *    >> When two compilation units contain the same string (which happens more often than not),
  *       we can merge those string into one, simply by taking the string symbol
  *      (from which we can tell the address and size), and searching for an identical
  *       string in lower memory addresses.
  * NOTE: This kind of merge-optimization should be given its own function
  */
 assert(TOKEN.t_file == &TPPFile_Merge);
 TOKEN.t_file = TPPFile_Merge.f_prev;
 assert(TPPFile_Merge.f_refcnt >= 2);
 --TPPFile_Merge.f_refcnt;
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

PUBLIC void
DCCUnit_Restore(struct DCCUnit *__restrict other) {
 struct DCCSection *iter;
 assert(other);
 assert(other != &unit);
 unit = *other;
 iter = unit.u_secs;
 if (iter) {
  assert(iter->sc_pself == &other->u_secs);
  iter->sc_pself = &unit.u_secs;
  do {
   assert(!DCCSection_ISIMPORT(iter));
   assert(iter->sc_unit == other);
   iter->sc_unit = &unit;
  } while ((iter = iter->sc_next) != NULL);
 }
 iter = other->u_imps;
 if (iter) {
  assert(iter->sc_pself == &other->u_imps);
  iter->sc_pself = &unit.u_imps;
  do {
   assert(DCCSection_ISIMPORT(iter));
   assert(iter->sc_unit == other);
   iter->sc_unit = &unit;
  } while ((iter = iter->sc_next) != NULL);
 }
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "unit-seccoll.c.inl"
#endif


#endif /* !GUARD_DCC_UNIT_MERGE_C */
