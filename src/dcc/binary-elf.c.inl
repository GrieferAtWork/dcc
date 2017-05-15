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
#ifndef GUARD_DCC_BINARY_ELF_C_INL
#define GUARD_DCC_BINARY_ELF_C_INL 1

#include <dcc/common.h>
#include <dcc/binary.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/stream.h>

#include <stdio.h>
#include <elf.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

DCC_DECL_BEGIN


/* Section group weight
 * - Used to sort sections into groups.
 * - Higher values mean place-after.
 * WARNING: When changing the order in here, the
 *         'group_info' vector must also be adjusted. */
enum{
 SECGP_INTERP,      /* The '.interp' section. */
 SECGP_SHSTR,       /* The '.shstrtab' section. */
 SECGP_DYNAMIC,     /* The '.dynamic' section. */
 SECGP_NOALLOC,     /* All sections not allocated at run-time. */
 SECGP_RELOC,       /* Relocation sections. */
 /* BEGIN: read-only */
 SECGP_R,           /* All sections that are read-only. */
 /* BEGIN: executable */
 SECGP_RX,          /* All sections that are read+execute. */
 /* END: read-only */
 SECGP_RWX,         /* All sections that are read+write+execute. */
 SECGP_WX,          /* All sections that are write+execute. */
 /* END: executable */
 SECGP_RW_FULL,     /* All sections that are read-write (And have no padding, uninitialized data). */
 SECGP_RW,          /* All sections that are read-write. */
 SECGP_OTHER,       /* All sections not falling into a different group. */
 SECGP_COUNT,       /* Amount of groups (not actually a group itself). */
//SECGP_UNUSED   = -1, /* All sections that should not be included in the final binary. */
};

typedef int       secgp_t; /* Section group (One of 'SECGP_*') */
typedef Elf(Word) secty_t; /* Section type (One of 'SHT_*') */

struct gpinfo {
 Elf(Word)	fp_type;			/* == Elf(Phdr)::p_type. */
 Elf(Word)	fp_flags;		/* == Elf(Phdr)::p_flags. */
};

/* Elf(Phdr) values for different groups. */
static struct gpinfo const group_info[SECGP_COUNT] = {
 /* [SECGP_INTERP ] = */{PT_INTERP, PF_R},
 /* [SECGP_SHSTR  ] = */{PT_LOAD,   PF_R},
 /* [SECGP_DYNAMIC] = */{PT_DYNAMIC,PF_R},
 /* [SECGP_NOALLOC] = */{PT_NOTE,   0},
 /* [SECGP_RELOC  ] = */{PT_NULL,   0},
 /* [SECGP_R      ] = */{PT_LOAD,   PF_R},
 /* [SECGP_RX     ] = */{PT_LOAD,   PF_R|PF_X},
 /* [SECGP_RWX    ] = */{PT_LOAD,   PF_R|PF_W|PF_X},
 /* [SECGP_WX     ] = */{PT_LOAD,   PF_W|PF_X},
 /* [SECGP_RW_FULL] = */{PT_LOAD,   PF_R|PF_W},
 /* [SECGP_RW     ] = */{PT_LOAD,   PF_R|PF_W},
 /* [SECGP_OTHER  ] = */{PT_LOAD,   0},
};


/* Section type weight
 * - used for sorting section locations in-file
 * - higher weights mean place-after. */
static uint8_t const sht_weight[SHT_NUM] = {
 /* [SHT_NULL         ] = */0,
 /* [SHT_PROGBITS     ] = */4,
 /* [SHT_SYMTAB       ] = */6,
 /* [SHT_STRTAB       ] = */2,
 /* [SHT_RELA         ] = */18,
 /* [SHT_HASH         ] = */3,
 /* [SHT_DYNAMIC      ] = */5,
 /* [SHT_NOTE         ] = */16,
 /* [SHT_NOBITS       ] = */15,
 /* [SHT_REL          ] = */17,
 /* [SHT_SHLIB        ] = */7,
 /* [SHT_DYNSYM       ] = */1,
 /* [UNUSED12         ] = */11,
 /* [UNUSED13         ] = */12,
 /* [SHT_INIT_ARRAY   ] = */9,
 /* [SHT_FINI_ARRAY   ] = */10,
 /* [SHT_PREINIT_ARRAY] = */8,
 /* [SHT_GROUP        ] = */14,
 /* [SHT_SYMTAB_SHNDX ] = */13,
};

/* Determine the group and type of a given section. */
PRIVATE secgp_t secgp_of(struct DCCSection const *__restrict section);
PRIVATE secty_t secty_of(struct DCCSection const *__restrict section);

struct secinfo {
 struct DCCSection *si_sec;   /*< [1..1] Section associated with this information. */
 struct DCCSection *si_rel;   /*< [0..1] Section used for relocations. */
 secgp_t            si_grp;   /*< Section group. */
 Elf(Shdr)          si_hdr;   /*< ELF header, as later written to file. */
 size_t             si_msize; /*< Allocated size of the section text. */
 target_siz_t       si_vsize; /*< Virtual size of the section text. */
};

/* Return the total sort-order weight of a given section info record. */
#define secinfo_weight(self) (((self)->si_grp*SHT_NUM)+sht_weight[(self)->si_hdr.sh_type])
#define secinfo_cmplo(a,b)   (secinfo_weight(a) < secinfo_weight(b))

struct phinfo {
 struct secinfo *ph_siv; /*< [0..ph_sic] The first section associated with this record.
                          *  NOTE: All records in this sub-vector of 'elf.elf_secv' must share the same grouping!
                          *  NOTE: When NULL, this program header is for the program header table itself! */
 size_t          ph_sic; /*< [(!= 0) == (ph_siv != NULL)] The amount of sections contained within this header. */
 Elf(Phdr)       ph_hdr; /*< ELF header, as later written to file. */
};


struct DCCElfInfo {
 Elf(Half)             elf_type;    /*< ELF output type (One of 'ET_*') */
 size_t                elf_secc;    /*< Amount of sections contained in this ELF binary. */
 struct secinfo       *elf_secv;    /*< [0..pe_secc][owned] Vector of sections, sorted by the type.
                                     *   NOTE: An index in this section is the 'SHN_*' number -1 (aka. 'secinfo->si_ndx == (secinfo-elf.elf_secv)+1') */
 size_t                elf_phdc;    /*< Amount of program headers. */
 size_t                elf_phda;    /*< Allocated amount of program headers. */
 target_ptr_t          elf_base;    /*< ELF base address (Similar to PE's 'pe_imgbase' field, loading the
                                     *  binary at this address will require the least amount of work). */
 struct phinfo        *elf_phdv;    /*< [0..elf_phdc][owned] . */
 struct DCCSection    *elf_interp;  /*< [0..1] '.interp': Interpreter section (if needed). */
 struct DCCSection    *elf_shstr;   /*< [1..1] '.shstrtab': Section name section. */
 struct DCCSection    *elf_dynsym;  /*< [1..1] '.dynsym': Symbol table used for relocations and dynamic linking. */
 struct DCCSection    *elf_dynamic; /*< [1..1] '.dynamic': Dynamic data section. */
 /*ref*/struct DCCSym *elf_entry;   /*< [1..1] ELF Entry point. */
};

PRIVATE struct DCCElfInfo elf;
PRIVATE void elf_mk_entry(void);
PRIVATE void elf_mk_interp(void);
PRIVATE void elf_mk_relsec(void);          /* Generate stub relocation sections. */
PRIVATE void elf_mk_secvec(void);          /* Generate the vector of sections, calculation their groups and basic header data. */
PRIVATE void elf_mk_secnam(void);          /* Allocate section names. */
PRIVATE void elf_mk_secsort(void);         /* Sort sections according to their weight. */
PRIVATE void elf_mk_delsecunused(int all); /* Delete unused sections (Don't delete relocation or internal sections unless 'all' is non-ZERO). */
PRIVATE void elf_mk_dynsym(void);          /* Generate dynamic symbol information. */
PRIVATE void elf_mk_secvaddr(void);        /* Generate section virtual addresses. */
PRIVATE void elf_mk_execdisp(void);        /* Execute DISP relocations that can be resolved at compile-time. */
PRIVATE void elf_mk_execrel(void);         /* Execute all relocations. */
PRIVATE void elf_mk_delnoprel(void);       /* Delete all unused relocations, but set the 'sy_size' field of associated symbols to ZERO to prevent their data from being deallocated. */
PRIVATE void elf_mk_reldat(void);          /* Generate ELF relocations against dynamic symbols. */
PRIVATE void elf_mk_dyndat(void);          /* Fill the '.dynamic' section with information. */
PRIVATE void elf_mk_phdvec(void);          /* Generate program headers and match associated section groups. */
PRIVATE void elf_mk_relabj(void);          /* Adjust generated relocation offsets by section base addresses. */
PRIVATE void elf_mk_secfaddr(void);        /* Generate section file addresses. */
PRIVATE void elf_mk_phdaddr(void);         /* Fill the program header table according to associated section groups. */
PRIVATE void elf_mk_relocbase(void);       /* Execute relocations for fix-address images. */
PRIVATE void elf_mk_outfile(stream_t s);
PRIVATE void elf_fd_paddata(stream_t s, Elf(Off) addr);
PRIVATE void elf_fd_padbytes(stream_t s, size_t n_addr);
PRIVATE struct phinfo *elf_mk_phdr(void);

/* Return the section index of a given section, or 'SHN_UNDEF' if not known. */
PRIVATE Elf(Half) elf_get_secidx(struct DCCSection const *__restrict sec);

#ifndef DCC_TARGET_ELFINTERP
#define DCC_TARGET_ELFINTERP  "/lib/ld-linux.so.2"
#endif





//////////////////////////////////////////////////////////////////////////
PRIVATE secgp_t secgp_of(struct DCCSection const *__restrict section) {
 assert(section);
 if (section == elf.elf_interp) return SECGP_INTERP;
 if (section == elf.elf_shstr) return SECGP_SHSTR;
 if (section == elf.elf_dynamic) return SECGP_DYNAMIC;
 if (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_NOALLOC) return SECGP_NOALLOC;
 switch (section->sc_start.sy_flags&
        (DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X)) {
 case DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X: return SECGP_RWX;
 case DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W:                   return section->sc_text.tb_max == section->sc_text.tb_end ? SECGP_RW_FULL : SECGP_RW;
 case DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_X:                   return SECGP_RX;
 case DCC_SYMFLAG_SEC_R:                                     return SECGP_R;
 case DCC_SYMFLAG_SEC_W|DCC_SYMFLAG_SEC_X:                   return SECGP_WX;
 default: break;
 }
 return SECGP_OTHER;
}

//////////////////////////////////////////////////////////////////////////
PRIVATE secty_t
secty_of(struct DCCSection const *__restrict section) {
 struct TPPKeyword const *section_name;
 assert(section);
 section_name = section->sc_start.sy_name;
#define CHECK_NAME(s) \
   (section_name->k_size == sizeof(s)/sizeof(char)-1 && \
   !memcmp(section_name->k_name,s,sizeof(s)-sizeof(char)))
#define CHECK_ENDSWITH(s) \
   (section_name->k_size >= sizeof(s)/sizeof(char)-1 && \
   !memcmp(section_name->k_name+(section_name->k_size-(sizeof(s)/sizeof(char)-1)),\
           s,sizeof(s)-sizeof(char)))
#define CHECK_STARTSWITH(s) \
   (section_name->k_size >= sizeof(s)/sizeof(char)-1 && \
   !memcmp(section_name->k_name,s,sizeof(s)-sizeof(char)))
 if (CHECK_ENDSWITH("strtab") || CHECK_NAME(".dynstr")) return SHT_STRTAB;
 if (CHECK_ENDSWITH("symtab")) return SHT_SYMTAB;
 if (CHECK_NAME(".rel") || CHECK_STARTSWITH(".rel.")) return SHT_REL;
 if (CHECK_NAME(".rela") || CHECK_STARTSWITH(".rela.")) return SHT_RELA;
 if (CHECK_NAME(".dynsym")) return SHT_DYNSYM;
 if (CHECK_NAME(".dynamic")) return SHT_DYNAMIC;
 if (CHECK_NAME(".hash")) return SHT_HASH;
 if (CHECK_NAME(".preinit_array")) return SHT_PREINIT_ARRAY;
 if (CHECK_NAME(".init_array")) return SHT_INIT_ARRAY;
 if (CHECK_NAME(".fini_array")) return SHT_FINI_ARRAY;
 if (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_NOALLOC) {
  return (section->sc_start.sy_flags&DCC_SYMFLAG_USED) ? SHT_NOTE : SHT_NOBITS;
 }
 return SHT_PROGBITS;
#undef CHECK_STARTSWITH
#undef CHECK_ENDSWITH
#undef CHECK_NAME
}

//////////////////////////////////////////////////////////////////////////
PRIVATE Elf(Half) elf_get_secidx(struct DCCSection const *__restrict sec) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) if (iter->si_sec == sec) {
  return (Elf(Half))(iter-elf.elf_secv);
  //return (Elf(Half))((iter-elf.elf_secv)+1);
 }
 return SHN_UNDEF;
}


PRIVATE void elf_mk_entry(void) {
 elf.elf_entry = DCCUnit_GetSyms("_start");
 if unlikely(!elf.elf_entry) {
  WARN(W_MISSING_ENTRY_POINT,"_start");
  elf.elf_entry = &unit.u_text->sc_start;
 }
 DCCSym_Incref(elf.elf_entry);
}

PRIVATE void elf_mk_interp(void) {
 struct DCCSection *sec;
 if (!(compiler.l_flags&(DCC_LINKER_FLAG_SHARED|
                         DCC_LINKER_FLAG_STATIC))) {
  char *itp = getenv("LD_SO"); /* XXX: Override with commandline-switch. */
  if (!itp) itp = DCC_TARGET_ELFINTERP;
  /* Create a section for the interpreter. */
  sec = elf.elf_interp = DCCUnit_NewSecs(".interp",DCC_SYMFLAG_SEC(1,0,0,0,0,0));
  if (sec) {
   size_t itp_len = strlen(itp);
   DCCSection_DAllocMem(sec,itp,
                       (itp_len+0)*sizeof(char),
                       (itp_len+1)*sizeof(char),
                        1,0);
  }
 }
}
PRIVATE void elf_mk_dynsym(void) {
 if unlikely(!elf.elf_dynsym) return;
 /* TODO: Generate dynamic symbol information. */

}

PRIVATE struct DCCSection *
get_relsec(struct DCCSection *__restrict base,
           int alloc_missing) {
 struct TPPKeyword const *kwd;
 char *buf;
 kwd = base->sc_start.sy_name;
 buf = (char *)alloca((kwd->k_size+4)*sizeof(char));
 buf[0] = '.';
 buf[1] = 'r';
 buf[2] = 'e';
 buf[3] = 'l';
 memcpy(buf+4,kwd->k_name,kwd->k_size*sizeof(char));
 kwd = TPPLexer_LookupKeyword(buf,kwd->k_size+4,alloc_missing);
 return !kwd ? NULL : !alloc_missing ? DCCUnit_GetSec(kwd)
  : DCCUnit_NewSec(kwd,DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_NOALLOC);
}


PRIVATE void elf_mk_relsec(void) {
 /* Allocate relocation sections. */
 struct DCCSection *section;
 DCCUnit_ENUMSEC(section) {
  if (section->sc_relc) get_relsec(section,1);
 }
}

PRIVATE void
elf_mk_secinfo(struct secinfo *__restrict info,
               struct DCCSection *__restrict section) {
 symflag_t secflags;
 info->si_grp         = secgp_of(section);
 secflags             = section->sc_start.sy_flags;
 info->si_rel         = compiler.l_flags&DCC_LINKER_FLAG_NORELOC ? NULL : get_relsec(section,0);
 info->si_hdr.sh_type = secty_of(section);
 info->si_sec         = section;
 if (info->si_hdr.sh_type == SHT_REL ||
     info->si_hdr.sh_type == SHT_RELA
     ) info->si_grp = SECGP_RELOC; /* Not detected by 'secgp_of' */
 {
  uint8_t *effective_end = section->sc_text.tb_end;
  if (effective_end > section->sc_text.tb_max)
      effective_end = section->sc_text.tb_max;
  info->si_msize = (size_t)(effective_end-
                            section->sc_text.tb_begin);
  info->si_vsize = (target_siz_t)(section->sc_text.tb_max-
                                  section->sc_text.tb_begin);
  info->si_hdr.sh_size = info->si_vsize;
  /* Optimization: Remove any trailing ZERO-data,
   * potentially reducing the generated binary size. */
  while (info->si_msize && !effective_end[-1])
       --info->si_msize,--effective_end;
 }
 info->si_hdr.sh_flags = 0;
 if (secflags&DCC_SYMFLAG_SEC_W) info->si_hdr.sh_flags |= SHF_WRITE;
 if (!(secflags&DCC_SYMFLAG_SEC_NOALLOC)) info->si_hdr.sh_flags |= SHF_ALLOC;
 if (secflags&DCC_SYMFLAG_SEC_X) info->si_hdr.sh_flags |= SHF_EXECINSTR;
 if (secflags&DCC_SYMFLAG_SEC_M) info->si_hdr.sh_flags |= SHF_MERGE;
 if (info->si_grp == SECGP_INTERP || info->si_grp == SECGP_SHSTR ||
     info->si_hdr.sh_type == SHT_STRTAB) info->si_hdr.sh_flags |= SHF_STRINGS;
 /* Fill in entry size information for relocation sections. */
      if (info->si_hdr.sh_type == SHT_REL)  info->si_hdr.sh_entsize = sizeof(Elf(Rel));
 else if (info->si_hdr.sh_type == SHT_RELA) info->si_hdr.sh_entsize = sizeof(Elf(Rela));
 //iter->si_hdr.sh_offset = ...; /* Filled later. */
 //iter->si_hdr.sh_size = ...; /* Filled later. */
#if SHN_UNDEF != 0
 info->si_hdr.sh_link = SHN_UNDEF;
#endif
 info->si_hdr.sh_addralign = (Elf(Word))section->sc_align;
}

PRIVATE void elf_mk_secvec(void) {
 struct DCCSection *section;
 struct secinfo *info,*iter;
 if unlikely(!unit.u_secc) {
  /* Shouldn't happen, but better be careful. */
nosec:
  elf.elf_secc = 0;
  elf.elf_secv = NULL;
  return;
 }

 info = (struct secinfo *)DCC_Calloc(unit.u_secc*sizeof(struct secinfo),0);
 if unlikely(!info) goto nosec;
 elf.elf_secv = iter = info;
 DCCUnit_ENUMSEC(section) {
  assert(iter < info+unit.u_secc);
  elf_mk_secinfo(iter,section);
  ++iter;
 }
 elf.elf_secc = (size_t)(iter-info);
 if unlikely(!elf.elf_secc) { free(info); return; }
}
PRIVATE void elf_mk_secnam(void) {
 struct secinfo *shstrtab = NULL;
 struct secinfo *iter,*end;
 /* Execute all relocations. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  struct DCCSection *section = iter->si_sec;
  assert(section); /* Allocate the section name. */
  iter->si_hdr.sh_name = (Elf(Word))DCCSection_DAllocMem(elf.elf_shstr,
                                                         section->sc_start.sy_name->k_name,
                                                        (section->sc_start.sy_name->k_size+0)*sizeof(char),
                                                        (section->sc_start.sy_name->k_size+1)*sizeof(char),
                                                         1,0);
  if (iter->si_sec == elf.elf_shstr) shstrtab = iter;
 }
 if (shstrtab) elf_mk_secinfo(shstrtab,elf.elf_shstr);
}

PRIVATE void elf_mk_execrel(void) {
 struct secinfo *iter,*end;
 /* Execute all relocations. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  DCCSection_Reloc(iter->si_sec);
 }
}
PRIVATE void elf_mk_execdisp(void) {
 struct secinfo *iter,*end;
 /* Execute DISP relocations that can be resolved at compile-time. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  DCCSection_ResolveDisp(iter->si_sec);
 }
}
PRIVATE void elf_mk_delnoprel(void) {
 struct secinfo *iter,*end;
 /* Delete all unused relocations, but set the 'sy_size' field of
  * associated symbols to ZERO to prevent their data from being deallocated. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  struct DCCRel *rel_iter,*rel_end;
  struct DCCSection *sec = iter->si_sec;
  rel_end = (rel_iter = sec->sc_relv)+sec->sc_relc;
  while (rel_iter != rel_end) {
   assert(rel_iter < rel_end);
   if (rel_iter->r_type == DCC_R_NONE) {
    /* Delete this relocation, but inherit the associated symbol's data. */
    assert(rel_iter->r_sym);
    rel_iter->r_sym->sy_size = 0;
    DCCSym_Decref(rel_iter->r_sym);
    assert(sec->sc_relc);
    /* Update relocation information. */
    --rel_end,--sec->sc_relc;
    memmove(rel_iter,rel_iter+1,
           (size_t)(rel_end-rel_iter)*
            sizeof(struct DCCRel));
   } else {
    ++rel_iter;
   }
  }
 }
}
PRIVATE void elf_mk_reldat(void) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) if (iter->si_rel) {
  struct DCCSection *iter_sec = iter->si_sec;
  struct DCCRel *rel_iter,*rel_end;
  target_ptr_t reladdr;
  Elf(Rel)    *reldata;
  size_t rel_count;
  assert(iter_sec);
  rel_count = iter_sec->sc_relc;
  if unlikely(!rel_count) continue;
  rel_end = (rel_iter = iter_sec->sc_relv)+rel_count;
  reladdr = DCCSection_DAlloc(iter->si_rel,rel_count*sizeof(Elf(Rel)),
                              DCC_COMPILER_ALIGNOF(Elf(Rel)),0);
  reldata = (Elf(Rel) *)DCCSection_GetText(iter->si_rel,reladdr,
                                           rel_count*sizeof(Elf(Rel)));
  if unlikely(!reldata) break;
  for (; rel_iter != rel_end; ++rel_iter,++reldata) {
   /* TODO: Use the '.dynsym' symbol index! */
   reldata->r_info    = ELF32_R_INFO(0,rel_iter->r_type);
   reldata->r_offset  = rel_iter->r_addr;
   /* Adjust the relocation address from section-relative to  */
   reldata->r_offset += rel_iter->r_addr;
  }
 }
}

PRIVATE void
elf_mk_dyndat(void) {
 if (elf.elf_dynamic) {
  /* TODO: Put 'DT_*' information */
  /* NOTE: Remember: The information vector for this is terminated by a 'DT_NULL' entry! */
 }
}

PRIVATE int
secinfo_mergesort(struct secinfo *__restrict v, size_t n,
                  struct secinfo *__restrict r) {
 struct secinfo *lb,*rb,*pl,*pr;
 size_t ln,rn; int error = 0;
 /* mergesort */
 switch (n) {
 case 0: return 1;
 case 1: r[0] = v[0]; return 1;
 case 2:
  if (secinfo_cmplo(&v[1],&v[0])) {
   r[0] = v[1];
   r[1] = v[0];
  } else {
   r[0] = v[0];
   r[1] = v[1];
  }
  return 1;
 default: break;
 }
 ln = n/2;
 rn = n-ln;
 lb = (struct secinfo *)malloc(ln*sizeof(struct secinfo));
 if unlikely(!lb) goto end;
 rb = (struct secinfo *)malloc(rn*sizeof(struct secinfo));
 if unlikely(!rb) goto end_lb;
 if (!secinfo_mergesort(v,ln,lb) ||
     !secinfo_mergesort(v+ln,rn,rb)) goto end_rb;
 /* Now merge the two vector together. */
 pl = lb,pr = rb,error = 1;
 while (ln && rn) {
  if (secinfo_cmplo(pr,pl))
       *r++ = *pr++,--rn;
  else *r++ = *pl++,--ln;
 }
 if (ln) {
  assert(!rn);
  memcpy(r,pl,ln*sizeof(struct secinfo));
 } else if (rn) {
  memcpy(r,pr,rn*sizeof(struct secinfo));
 }
end_rb: free(rb);
end_lb: free(lb);
end: return error;
}

PRIVATE void
secinfo_bubblesort(struct secinfo *__restrict v, size_t n) {
 struct secinfo temp,*v_end,*iter,*next;
 v_end = v+n;
 while (n--) {
  iter = v;
  for (;;) {
   next = iter+1;
   if (next == v_end) break;
   if (secinfo_cmplo(next,iter)) {
    temp = *iter;
    *iter = *next;
    *next = temp;
   }
   iter = next;
  }
 }
}

PRIVATE void elf_mk_secsort(void) {
 /* Sort ELF sections according to their weight. */
 struct secinfo *newvec;
 if unlikely(!elf.elf_secc) return;
 newvec = (struct secinfo *)malloc(elf.elf_secc*sizeof(struct secinfo));
 if unlikely(!newvec) goto sort_fallback;
 if (!secinfo_mergesort(elf.elf_secv,elf.elf_secc,newvec)) {
  free(newvec);
sort_fallback:
  /* fallback sorting algorithm */
  secinfo_bubblesort(elf.elf_secv,elf.elf_secc);
 } else {
  free(elf.elf_secv);
  elf.elf_secv = newvec;
 }
}


PRIVATE void elf_mk_delsecunused(int all) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 while (iter != end) {
  assert(iter->si_sec);
  /* Ignore empty section. */
  if (!(iter->si_vsize) &&
      !(iter->si_sec->sc_start.sy_flags&DCC_SYMFLAG_USED) &&
       (all || (iter->si_hdr.sh_type != SHT_REL &&
                iter->si_hdr.sh_type != SHT_RELA &&
                iter->si_sec != elf.elf_shstr &&
                iter->si_sec != elf.elf_dynamic &&
                iter->si_sec != elf.elf_dynsym))) {
   /* Delete this section. */
   assert(elf.elf_secc);
   --end;
   memmove(iter,iter+1,(end-iter)*
           sizeof(struct secinfo));
   --elf.elf_secc;
   continue;
  }
  ++iter;
 }
}


PRIVATE void elf_mk_phdvec(void) {
 struct phinfo *curhdr;
 struct secinfo *iter,*end;
 /* Look at all sections and generate the program header vector. */
 /* NOTE: Also look at the difference between virtual and physical section size.
  *    >> When it is small enough, simply ZERO-extend the section
  *       physically to prevent a program header split. */
 if unlikely(!elf.elf_secc) return;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 do {
  secgp_t hdr_group;
  Elf(Off) hdr_faddr; /* Expected file address for the next section. */
  curhdr = elf_mk_phdr();
  if unlikely(!curhdr) return;
  curhdr->ph_siv = iter;
  hdr_faddr = iter->si_hdr.sh_offset+
              iter->si_hdr.sh_size;
  hdr_group = iter->si_grp;
  ++iter;
  /* Special case: Ignore this group for program headers! */
  if (group_info[hdr_group].fp_type == PT_NULL) goto next;
  while (iter != end && iter->si_grp == hdr_group) {
   /* Make sure the section is written to the expected file address. */
   if (iter->si_hdr.sh_offset != hdr_faddr) {
    if (iter->si_hdr.sh_offset > hdr_faddr) {
     Elf(Off) fdiff = iter->si_hdr.sh_offset-hdr_faddr;
     /* Fix small file adjustments by extension the previous section. */
     if (fdiff < 16) {
      iter[-1].si_hdr.sh_size += fdiff;
      goto next_section;
     }
    }
    break;
   }
next_section:
   /* Update the expected file address. */
   hdr_faddr = iter->si_hdr.sh_offset+
               iter->si_hdr.sh_size;
   ++iter;
  }
  //curhdr->ph_hdr = { ... }; /* Filled later */
  curhdr->ph_sic = (size_t)(iter-curhdr->ph_siv);
next:;
 } while (iter != end);
}


PRIVATE void elf_mk_secvaddr(void) {
 /* Generate virtual section addresses. */
 target_ptr_t addr_base = elf.elf_base;
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  /* Properly align the target address pointer. */
  addr_base +=  (iter->si_hdr.sh_addralign-1);
  addr_base &= ~(iter->si_hdr.sh_addralign-1);
  iter->si_hdr.sh_addr = addr_base;
  /* Advance the address pointer. */
  addr_base += iter->si_vsize;
 }
}

PRIVATE void elf_mk_secfaddr(void) {
 struct secinfo *iter,*end;
 Elf(Off) fileaddr = sizeof(Elf(Ehdr));
 /* At this point, the amount of program
  * headers and sections _must_ be known. */
 fileaddr += elf.elf_phdc*sizeof(Elf(Phdr));
 fileaddr += elf.elf_secc*sizeof(Elf(Shdr));
 /* Generate file section addresses. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  /* Simple enough!
   * WARNING: Other code assumes the file-order of section
   *          matching that of the 'elf_secv' vector. */
  iter->si_hdr.sh_offset = fileaddr;
  fileaddr += iter->si_hdr.sh_size;
 }
}
PRIVATE void elf_mk_phdaddr(void) {
 struct phinfo *iter,*end;
 /* Generate file & virtual program header addresses. */
 end = (iter = elf.elf_phdv)+elf.elf_phdc;
 for (; iter != end; ++iter) {
  struct secinfo *sec_iter,*sec_end;
  assert((iter->ph_siv != NULL) ==
         (iter->ph_sic != 0));
  if ((sec_iter = iter->ph_siv) == NULL) {
   sec_iter->si_hdr.sh_type = PT_PHDR;
   iter->ph_hdr.p_offset    = sizeof(Elf(Ehdr))+
          (iter-elf.elf_phdv)*sizeof(Elf(Phdr));
   iter->ph_hdr.p_paddr = 0; /* ??? */
   iter->ph_hdr.p_vaddr = 0; /* ??? */
   iter->ph_hdr.p_filesz =
   iter->ph_hdr.p_memsz = elf.elf_phdc*sizeof(Elf(Phdr));
   iter->ph_hdr.p_flags = PF_R; /* ??? */
   iter->ph_hdr.p_align = DCC_COMPILER_ALIGNOF(Elf(Phdr));
  } else {
   Elf(Word) min_align = 1;
   sec_end = sec_iter+iter->ph_sic;
   assert(sec_iter >= elf.elf_secv);
   assert(sec_end  <= elf.elf_secv+elf.elf_secc);
   assert(sec_iter != sec_end);
   iter->ph_hdr.p_type = group_info[sec_iter->si_grp].fp_type;
   assert(iter->ph_hdr.p_type != PT_NULL);
   iter->ph_hdr.p_offset = sec_iter->si_hdr.sh_offset;
   iter->ph_hdr.p_flags  = group_info[sec_iter->si_grp].fp_flags;
   iter->ph_hdr.p_vaddr  = sec_iter->si_hdr.sh_addr;
   iter->ph_hdr.p_paddr  = iter->ph_hdr.p_vaddr;
   iter->ph_hdr.p_filesz = 0;
   iter->ph_hdr.p_memsz  = 0;
   for (; sec_iter != sec_end; ++sec_iter) {
    assertf((sec_iter->si_hdr.sh_size == sec_iter->si_vsize) ||
            (sec_iter == sec_end-1),
            "Non-last section '%s' with miss-matching msize %lu and vsize %lu",
            sec_iter->si_sec->sc_start.sy_name->k_name,
            sec_iter->si_hdr.sh_size,sec_iter->si_vsize);
    iter->ph_hdr.p_filesz += sec_iter->si_hdr.sh_size;
    iter->ph_hdr.p_memsz  += sec_iter->si_vsize;
    if (min_align < iter->ph_hdr.p_align)
        min_align = iter->ph_hdr.p_align;
   }
   iter->ph_hdr.p_align = min_align;
  }
 }
}

PRIVATE struct phinfo *elf_mk_phdr(void) {
 struct phinfo *result = elf.elf_phdv;
 if (elf.elf_phdc == elf.elf_phda) {
  size_t newsize = elf.elf_phda;
  if (!newsize) newsize = 1;
  newsize *= 2;
  result = (struct phinfo *)DCC_Realloc(result,newsize*
                                        sizeof(struct phinfo),
                                        0);
  if unlikely(!result) return NULL;
  memset(result+elf.elf_phda,0,
        (newsize-elf.elf_phda)*
         sizeof(struct phinfo));
  elf.elf_phdv = result;
  elf.elf_phda = newsize;
 }
 result += elf.elf_phdc++;
 return result;
}

PRIVATE void
elf_fd_padbytes(stream_t fd, size_t n_bytes) {
 void *buffer = calloc(1,n_bytes);
 if (buffer) {
  s_writea(fd,buffer,n_bytes);
  free(buffer);
 } else while (--n_bytes) {
  static char const zero[1] = {0};
  s_writea(fd,zero,1);
 }
}
PRIVATE void
elf_fd_paddata(stream_t fd, Elf(Off) addr) {
 DWORD ptr = s_seek(fd,0,SEEK_CUR);
 if (ptr >= addr) return;
 elf_fd_padbytes(fd,addr-ptr);
}


PRIVATE void
elf_mk_outfile(stream_t fd) {
 Elf(Ehdr) ehdr;
 memset(&ehdr,0,sizeof(ehdr));

 ehdr.e_phentsize = sizeof(Elf(Phdr));
 ehdr.e_shentsize = sizeof(Elf(Shdr));

 ehdr.e_ident[EI_MAG0] = ELFMAG0;
 ehdr.e_ident[EI_MAG1] = ELFMAG1;
 ehdr.e_ident[EI_MAG2] = ELFMAG2;
 ehdr.e_ident[EI_MAG3] = ELFMAG3;
#if ELF_USE == 32
 ehdr.e_ident[EI_CLASS] = ELFCLASS32;
#else
 ehdr.e_ident[EI_CLASS] = ELFCLASS64;
#endif
#if DCC_TARGET_BYTEORDER == 1234
 ehdr.e_ident[EI_DATA] = ELFDATA2LSB;
#elif DCC_TARGET_BYTEORDER == 4321
 ehdr.e_ident[EI_DATA] = ELFDATA2MSB;
#else
 ehdr.e_ident[EI_DATA] = ELFDATANONE;
#endif
 ehdr.e_ident[EI_VERSION] = EV_CURRENT;
#if DCC_TARGET_OS == DCC_OS_FREEBSD || \
    DCC_TARGET_OS == DCC_OS_FREEBSD_KERNEL
 ehdr.e_ident[EI_OSABI] = ELFOSABI_FREEBSD;
#elif DCC_TARGET_OS == DCC_OS_LINUX
 ehdr.e_ident[EI_OSABI] = ELFOSABI_LINUX;
#else
 ehdr.e_ident[EI_OSABI] = ELFOSABI_SYSV;
#endif
#if DCC_TARGET_IA32(386)
 ehdr.e_machine = EM_386;
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
 ehdr.e_machine = EM_X86_64;
#else
#   error FIXME
#endif
 ehdr.e_type    = elf.elf_type;
 ehdr.e_version = EV_CURRENT;
 ehdr.e_ehsize  = sizeof(Elf(Ehdr));

 {
  struct DCCSymAddr entryaddr;
  assert(elf.elf_entry);
  if (!DCCSym_LoadAddr(elf.elf_entry,&entryaddr,1)) {
   WARN(W_MISSING_ENTRY_POINT,elf.elf_entry->sy_name->k_name);
   /* TODO: What if the text section is empty? */
   entryaddr.sa_sym = &unit.u_text->sc_start;
   entryaddr.sa_off = 0;
  }
  ehdr.e_entry = (Elf(Addr))(entryaddr.sa_sym->sy_addr+entryaddr.sa_off+
                             entryaddr.sa_sym->sy_sec->sc_base);
 }
 ehdr.e_shstrndx = elf_get_secidx(elf.elf_shstr);
 ehdr.e_phnum    = (Elf(Half))elf.elf_phdc;
 ehdr.e_shnum    = (Elf(Half))elf.elf_secc;

 ehdr.e_phoff  = sizeof(ehdr); /* Directly after the E-Header. */
 ehdr.e_shoff  = ehdr.e_phoff; /* Directly after the P-Headers. */
 ehdr.e_shoff += ehdr.e_phnum*sizeof(Elf(Phdr));
 if (!ehdr.e_phnum) ehdr.e_phoff = 0;
 if (!ehdr.e_shnum) ehdr.e_shoff = 0;

 /* Write the E-header. */
 s_writea(fd,&ehdr,sizeof(ehdr));

 { /* Write the program headers. */
   struct phinfo *iter,*end;
   end = (iter = elf.elf_phdv)+elf.elf_phdc;
   for (; iter != end; ++iter) s_writea(fd,&iter->ph_hdr,sizeof(iter->ph_hdr));
 }

 { /* Write the section headers. */
   struct secinfo *iter,*end;
   end = (iter = elf.elf_secv)+elf.elf_secc;
   for (; iter != end; ++iter) s_writea(fd,&iter->si_hdr,sizeof(iter->si_hdr));
 }
 /* >> Write all the sections, according to their addresses. */
 { /* Write the section headers. */
   struct secinfo *iter,*end;
   end = (iter = elf.elf_secv)+elf.elf_secc;
   for (; iter != end; ++iter) {
    size_t text_size,phys_size;
    /* Generate padding until the section data. */
    elf_fd_paddata(fd,iter->si_hdr.sh_offset);
    text_size = (size_t)(iter->si_sec->sc_text.tb_end-
                         iter->si_sec->sc_text.tb_begin);
    phys_size = iter->si_hdr.sh_size;
    if (text_size > phys_size)
        text_size = phys_size;
    assert((size_t)(iter->si_sec->sc_text.tb_end-
                    iter->si_sec->sc_text.tb_begin) >= text_size);
    s_write(fd,iter->si_sec->sc_text.tb_begin,text_size);
    /* Pad a difference between the physical and text size with ZEROes. */
    if (text_size != phys_size)
     elf_fd_padbytes(fd,phys_size-text_size);
   }
 }
 /* And we're done! */
}



PUBLIC void
DCCBin_Generate(stream_t target) {
 memset(&elf,0,sizeof(elf));
 /* TODO: Output object files? */
 if (compiler.l_flags&DCC_LINKER_FLAG_SHARED) {
  elf.elf_base = 0; /* Load shared libraries at offset
                     * ZERO(0), relying on relocations. */
  elf.elf_type = ET_DYN;
 } else {
#ifdef DCC_TARGET_X86
  elf.elf_base = 0x08048000;
#else
#   error FIXME
#endif
  elf.elf_type = ET_EXEC;
 }
 /* Allocate internal ELF sections. */
 elf.elf_shstr   = DCCUnit_NewSecs(".shstrtab",DCC_SYMFLAG_SEC_NOALLOC|DCC_SYMFLAG_SEC(1,0,0,0,1,0));
 elf.elf_dynamic = DCCUnit_NewSecs(".dynamic", DCC_SYMFLAG_SEC_NOALLOC|DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 elf.elf_dynsym  = DCCUnit_NewSecs(".dynsym",  DCC_SYMFLAG_SEC_NOALLOC|DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 if unlikely(!OK) goto end;
 elf_mk_entry();         if unlikely(!OK) goto end;

 /* Delete unused stuff. */
 DCCUnit_ClearUnused();
 DCCUnit_ClearUnusedLibs();

 elf_mk_interp();        if unlikely(!OK) goto end; /* Create and fill the '.interp' section. */
 if (!(compiler.c_flags&DCC_LINKER_FLAG_NORELOC)) {
 elf_mk_relsec();        if unlikely(!OK) goto end; /* Create stub relocation sections. Must be done now
                                                     * because later we can no longer create new sections. */
 }
 elf_mk_secvec();        if unlikely(!OK) goto end; /* Create the ELF section vector. */
 elf_mk_delsecunused(0); if unlikely(!OK) goto end; /* Remove any unused section (except for the stub relocation sections, and internal sections). */
 elf_mk_secnam();        if unlikely(!OK) goto end; /* Allocate section names. */
 elf_mk_secsort();       if unlikely(!OK) goto end; /* Sort section headers, thus greatly improving cache locality
                                                     * and restricting the amount of potential program headers. */
 if (compiler.c_flags&DCC_LINKER_FLAG_NORELOC) {
  elf_mk_dyndat();       if unlikely(!OK) goto end; /* Generate general-purpose dynamic information. */
  elf_mk_secvaddr();     if unlikely(!OK) goto end; /* Generate virtual section addresses. */
  elf_mk_execrel();      if unlikely(!OK) goto end; /* Execute all relocations. */
  /* XXX: commandline switch for generating dynamic symbol information without relocations. */
//elf_mk_dynsym();       if unlikely(!OK) goto end; /* Create dynamic symbol information. */
 } else {
  elf_mk_execdisp();     if unlikely(!OK) goto end; /* Execute and delete compile-time DISP-relocations. */
  elf_mk_dynsym();       if unlikely(!OK) goto end; /* Create dynamic symbol information. */
  elf_mk_delnoprel();    if unlikely(!OK) goto end; /* Delete obsolete relocations, but keep associated symbol data alive. */
  elf_mk_reldat();       if unlikely(!OK) goto end; /* Generate relocation data. */
  elf_mk_dyndat();       if unlikely(!OK) goto end; /* Generate general-purpose dynamic information. */
  /* At this point, all sections are final and won't have to be changed again! */
  elf_mk_secvaddr();     if unlikely(!OK) goto end; /* Generate virtual section addresses. */
 }
 elf_mk_delsecunused(1); if unlikely(!OK) goto end; /* Delete any sections still unused (including any associated relocation section) */

 elf_mk_phdvec();        if unlikely(!OK) goto end; /* Generate program headers suitable for the generated (and sorted) list of sections. */
 elf_mk_secfaddr();      if unlikely(!OK) goto end; /* Generate file addresses for all remaining sections. */
 elf_mk_phdaddr();       if unlikely(!OK) goto end; /* Fill in program header file/virtual addresses. */

 elf_mk_outfile(target);
end:
 DCCSym_XDecref(elf.elf_entry);
 free(elf.elf_phdv);
 free(elf.elf_secv);
}

DCC_DECL_END

#endif /* !GUARD_DCC_BINARY_ELF_C_INL */
