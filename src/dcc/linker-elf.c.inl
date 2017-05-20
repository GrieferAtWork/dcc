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
#ifndef GUARD_DCC_LINKER_ELF_C_INL
#define GUARD_DCC_LINKER_ELF_C_INL 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/linker.h>
#include <dcc/stream.h>

#include <stdio.h>

#include "linker-elf.h"

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
 SECGP_DYNSYM,      /* The '.dynsym' section. */
 SECGP_DYNSTR,      /* The '.dynstr' section. */
 SECGP_HASH,        /* The '.hash' section. */
 SECGP_RELOC,       /* Relocation sections. */
 SECGP_RELOCA,      /* Relocation sections. */
 SECGP_NOALLOC,     /* All sections not allocated at run-time. */
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

#define SECGP_ISX(g) (group_info[g].fp_flags&PF_X)
#define SECGP_ISW(g) (group_info[g].fp_flags&PF_W)
#define SECGP_ISR(g) (group_info[g].fp_flags&PF_R)

typedef int       secgp_t; /* Section group (One of 'SECGP_*') */
typedef Elf(Word) secty_t; /* Section type (One of 'SHT_*') */

struct gpinfo {
 Elf(Word)	fp_type;			/* == Elf(Phdr)::p_type. */
 Elf(Word)	fp_flags;		/* == Elf(Phdr)::p_flags. */
};

/* Elf(Phdr) values for different groups. */
static struct gpinfo const group_info[SECGP_COUNT] = {
 /* [SECGP_INTERP ] = */{PT_INTERP, PF_R},
#if 0
 /* [SECGP_SHSTR  ] = */{PT_NULL,   0},
#else
 /* [SECGP_SHSTR  ] = */{PT_LOAD,   PF_R},
#endif
 /* [SECGP_DYNAMIC] = */{PT_DYNAMIC,PF_R/*|PF_W*/},
#if 1
 /* [SECGP_DYNSYM ] = */{PT_LOAD,   PF_R},
 /* [SECGP_DYNSTR ] = */{PT_LOAD,   PF_R},
 /* [SECGP_HASH   ] = */{PT_LOAD,   PF_R},
#else
 /* [SECGP_DYNSYM ] = */{PT_NULL,   0},
 /* [SECGP_DYNSTR ] = */{PT_NULL,   0},
 /* [SECGP_HASH   ] = */{PT_NULL,   0},
#endif
 /* [SECGP_RELOC  ] = */{PT_LOAD,   PF_R},
 /* [SECGP_RELOCA ] = */{PT_LOAD,   PF_R},
 /* [SECGP_NOALLOC] = */{PT_NOTE,   0},
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
 /* [12               ] = */11, /* UNUSED */
 /* [13               ] = */12, /* UNUSED */
 /* [SHT_INIT_ARRAY   ] = */9,
 /* [SHT_FINI_ARRAY   ] = */10,
 /* [SHT_PREINIT_ARRAY] = */8,
 /* [SHT_GROUP        ] = */14,
 /* [SHT_SYMTAB_SHNDX ] = */13,
};

static uint8_t const pht_weight[PT_NUM] = {
 /* [PT_NULL   ] = */7,
 /* [PT_LOAD   ] = */2,
 /* [PT_DYNAMIC] = */4,
 /* [PT_INTERP ] = */1,
 /* [PT_NOTE   ] = */5,
 /* [PT_SHLIB  ] = */6,
 /* [PT_PHDR   ] = */0,
 /* [PT_TLS    ] = */3,
};

/* Determine the group and type of a given section. */
PRIVATE secgp_t secgp_of(struct DCCSection const *__restrict section);
PRIVATE secty_t secty_of(struct DCCSection const *__restrict section);

struct secinfo {
 struct DCCSection    *si_sec;  /*< [1..1] Section associated with this information. */
 struct DCCSection    *si_rel;  /*< [0..1] Section used for relocations. */
 target_ptr_t          si_rdat; /*< Base address for relocation data in si_rel. */
 target_ptr_t          si_rcnt; /*< Base address for relocation data in si_rel. */
 secgp_t               si_grp;  /*< Section group. */
 Elf(Shdr)             si_hdr;  /*< ELF header, as later written to file. */
};

DCC_LOCAL size_t       secinfo_msize(struct secinfo const *__restrict self);
DCC_LOCAL target_siz_t secinfo_vsize(struct secinfo const *__restrict self);

/* Return the total sort-order weight of a given section info record. */
#define secinfo_weight(self) (((self)->si_grp*SHT_NUM)+sht_weight[(self)->si_hdr.sh_type])
#define secinfo_cmplo(a,b)   (secinfo_weight(a) < secinfo_weight(b))

struct phinfo {
 struct secinfo *ph_siv; /*< [1..ph_sic] The first section associated with this record.
                          *  NOTE: All records in this sub-vector of 'elf.elf_secv' must share the same grouping! */
 size_t          ph_sic; /*< [!0] The amount of sections contained within this header. */
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
 struct DCCSection    *elf_shstr;   /*< [0..1] '.shstrtab': Section name section. */
 struct DCCSection    *elf_dynsym;  /*< [0..1] '.dynsym': Symbol table used for relocations and dynamic linking. */
 struct DCCSection    *elf_dynstr;  /*< [0..1] '.dynstr': Section used for dynamic symbol names. */
 struct DCCSection    *elf_hash;    /*< [0..1] '.hash': Hash section for dynamic symbol data. */
 struct DCCSection    *elf_dynamic; /*< [0..1] '.dynamic': Dynamic data section. */
 /*ref*/struct DCCSym *elf_entry;   /*< [1..1] ELF Entry point. */
 size_t                elf_dync;    /*< Amount of dynamic data entries. */
 target_ptr_t          elf_dynoff;  /*< Offset in 'elf_dynamic': Start of the dynamic data vector. */
};

PRIVATE struct DCCElfInfo elf;
PRIVATE void elf_mk_entry(void);
PRIVATE void elf_mk_interp(void);
PRIVATE void elf_mk_relsec(void);           /* Generate stub relocation sections. */
PRIVATE void elf_mk_secvec(void);           /* Generate the vector of sections, calculation their groups and basic header data. */
PRIVATE void elf_mk_secnam(void);           /* Allocate section names. */
PRIVATE void elf_mk_seclnk(void);           /* Fix section links. */
PRIVATE void elf_mk_secsort(void);          /* Sort sections according to their weight. */
PRIVATE void elf_mk_delsecunused(secgp_t min_gp); /* Delete unused sections with a group >= 'min_gp'. */
PRIVATE void elf_mk_dynsym(void);           /* Generate dynamic symbol information. */
PRIVATE void elf_mk_execdisp(void);         /* Execute DISP relocations that can be resolved at compile-time. */
PRIVATE void elf_mk_execrel(int resolve_weak);/* Execute all relocations. */
PRIVATE void elf_mk_delnoprel(void);        /* Delete all unused relocations, but set the 'sy_size' field of associated symbols to ZERO to prevent their data from being deallocated. */
PRIVATE void elf_mk_reldat(void);           /* Generate ELF relocations against dynamic symbols. */
PRIVATE void elf_mk_dyndat(void);           /* Generate dynamic data entries in the '.dynamic' section. */
PRIVATE void elf_mk_dynfll(void);           /* Fill dynamic data entries previously generated using 'elf_mk_dyndat'. */
PRIVATE void elf_mk_phdvec(void);           /* Generate program headers and match associated section groups. */
PRIVATE void elf_mk_phdsort(void);          /* Sort program headers. */
PRIVATE void elf_mk_reladj(void);           /* Adjust generated relocation offsets by section base addresses. */
PRIVATE void elf_mk_relocbase(void);        /* Execute relocations for fix-address images. */
PRIVATE void elf_mk_outfile(stream_t s);
PRIVATE void elf_fd_paddata(stream_t s, Elf(Off) addr);
PRIVATE void elf_fd_padbytes(stream_t s, size_t n_addr);

PRIVATE void elf_clr_unused(struct DCCSection **psec);
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
 if (section == elf.elf_dynsym) return SECGP_DYNSYM;
 if (section == elf.elf_dynstr) return SECGP_DYNSTR;
 if (section == elf.elf_hash) return SECGP_HASH;
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
DCC_LOCAL size_t secinfo_msize(struct secinfo const *__restrict self) {
 uint8_t *effective_end = self->si_sec->sc_text.tb_end;
 if (effective_end > self->si_sec->sc_text.tb_max)
     effective_end = self->si_sec->sc_text.tb_max;
 return (size_t)(effective_end-
                 self->si_sec->sc_text.tb_begin);
}
//////////////////////////////////////////////////////////////////////////
DCC_LOCAL target_siz_t secinfo_vsize(struct secinfo const *__restrict self) {
 return (target_siz_t)(self->si_sec->sc_text.tb_max-
                       self->si_sec->sc_text.tb_begin);
}

//////////////////////////////////////////////////////////////////////////
PRIVATE Elf(Half) elf_get_secidx(struct DCCSection const *__restrict sec) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) if (iter->si_sec == sec) {
  return (Elf(Half))((iter-elf.elf_secv)+1);
 }
 return SHN_UNDEF;
}


PRIVATE void elf_mk_entry(void) {
 if (elf.elf_type != ET_EXEC) goto use_text;
 elf.elf_entry = DCCUnit_GetSyms("_start");
 if unlikely(!elf.elf_entry) {
  WARN(W_MISSING_ENTRY_POINT,"_start");
use_text:
  elf.elf_entry = &unit.u_text->sc_start;
 }
 DCCSym_Incref(elf.elf_entry);
}

PRIVATE void elf_mk_interp(void) {
 char *itp; size_t itp_len;
 if (!elf.elf_interp) return;
 if (linker.l_elf_interp) {
  /* Use a custom interpreter name. */
  itp     = linker.l_elf_interp->s_text;
  itp_len = linker.l_elf_interp->s_size;
 } else {
  if ((itp = getenv("LD_SO")) == NULL) itp = DCC_TARGET_ELFINTERP;
  /* Create a section for the interpreter. */
  itp_len = strlen(itp);
 }
 DCCSection_DAllocMem(elf.elf_interp,itp,
                     (itp_len+0)*sizeof(char),
                     (itp_len+1)*sizeof(char),
                      1,0);
}

PRIVATE uint8_t const vismap[4] = {
 /* [DCC_SYMFLAG_NONE     ] = */STV_DEFAULT,
 /* [DCC_SYMFLAG_PROTECTED] = */STV_PROTECTED,
 /* [DCC_SYMFLAG_PRIVATE  ] = */STV_HIDDEN,
 /* [DCC_SYMFLAG_INTERNAL ] = */STV_INTERNAL,
};

PRIVATE uint32_t elf_hashof(char const *name) {
 uint32_t h = 0,g;
 while (*name) {
  h = (h << 4) + (unsigned char)*name++;
  g = h & 0xf0000000;
  if (g) h ^= g >> 24;
  h &= ~g;
 }
 return h;
}

PRIVATE void
elf_mk_hash(struct DCCSection *__restrict hashsec,
            struct DCCSection *__restrict symtab,
            struct DCCSection *__restrict strtab,
            size_t bucketcnt) {
 char *strdat; size_t symcnt;
 uint32_t *linkptr,*mapptr,hash;
 Elf(Sym) *sym_iter,*sym_end,*sym_begin;
 assert(hashsec);
 assert(symtab);
 assert(strtab);
 assert(hashsec != symtab);
 assert(hashsec != strtab);
 assert(symtab != strtab);
 symcnt  = (size_t)(symtab->sc_text.tb_max-
                    symtab->sc_text.tb_begin);
 symcnt /= sizeof(Elf(Sym));
 if unlikely(!symcnt) return;
 sym_end = (sym_iter = sym_begin = (Elf(Sym) *)symtab->sc_text.tb_begin)+symcnt;
 strdat = (char *)strtab->sc_text.tb_begin;
 assert(symtab->sc_text.tb_max <=
        symtab->sc_text.tb_end);
 hashsec->sc_text.tb_pos = hashsec->sc_text.tb_begin;
 linkptr = (uint32_t *)DCCSection_TAlloc(hashsec,
                                        (2+bucketcnt+symcnt)*
                                         sizeof(uint32_t));
 if unlikely(!linkptr) return;
 *linkptr++ = bucketcnt;
 *linkptr++ = symcnt;
 mapptr     = linkptr;
 memset(mapptr,0,(bucketcnt+1)*sizeof(uint32_t));
 linkptr   += bucketcnt+1;
 for (; sym_iter != sym_end; ++sym_iter) {
  if (ELF(ST_BIND)(sym_iter->st_info) != STB_LOCAL) {
   hash = elf_hashof(strdat+sym_iter->st_name) % bucketcnt;
   *linkptr = mapptr[hash];
   mapptr[hash] = (uint32_t)(sym_iter-sym_begin);
  } else {
   *linkptr = 0;
  }
  ++linkptr;
 }
}

/* Force index ZERO(0) of .dynsym to be the NULL-symbol. */
#define ELF_HAVE_NULLSYM 1


#if ELF_HAVE_NULLSYM
static Elf(Sym) const null_sym = {
 /* st_name  */0,
 /* st_value */0,
 /* st_size  */0,
 /* st_info  */ELF(ST_INFO)(STB_LOCAL,STT_NOTYPE),
 /* st_other */ELF(ST_VISIBILITY)(STV_DEFAULT),
 /* st_shndx */SHN_UNDEF,
};
#endif /* ELF_HAVE_NULLSYM */

PRIVATE void elf_mk_dynsym(void) {
 struct DCCSym *sym; Elf(Sym) esym;
#if ELF_HAVE_NULLSYM
 uint32_t dynid = 1; /* Index ZERO is the NULL-symbol. */
 int first = 1;
#else /* ELF_HAVE_NULLSYM */
 uint32_t dynid = 0;
#endif /* ELF_HAVE_NULLSYM */
 if unlikely(!elf.elf_dynsym ||
             !elf.elf_dynstr) return;
 /* Generate dynamic symbol information. */
 DCCUnit_ENUMSYM(sym) {
  target_ptr_t entry_addr;
  unsigned char st_bind,st_type;
  struct DCCSymAddr symaddr;
  if (DCCSym_ISSECTION(sym) &&
     ((!(DCCSection_VSIZE(DCCSym_TOSECTION(sym))) &&
       !(sym->sy_flags&DCC_SYMFLAG_USED)) ||
       DCCSection_ISIMPORT(DCCSym_TOSECTION(sym)))) continue;
#if 1 /* Don't export static symbols. */
  if (sym->sy_flags&DCC_SYMFLAG_STATIC) continue;
#endif
#if 0 /* Only export symbols with default visibility? */
  if ((sym->sy_flags&DCC_SYMFLAG_VISIBILITYBASE) != DCC_SYMFLAG_NONE) continue;
#endif
  if (DCCSym_LoadAddr(sym,&symaddr,0)) {
   /* Handle special sections. */
   if (symaddr.sa_sym->sy_sec == &DCCSection_Abs)        esym.st_shndx = SHN_ABS;
   else if (DCCSection_ISIMPORT(symaddr.sa_sym->sy_sec)) esym.st_shndx = SHN_UNDEF;
   else {
    esym.st_shndx = elf_get_secidx(symaddr.sa_sym->sy_sec);
    /* Skip symbols from removed sections (including removed sections themself). */
    if (symaddr.sa_sym->sy_sec && esym.st_shndx == SHN_UNDEF &&
        !DCCSection_ISIMPORT(symaddr.sa_sym->sy_sec)) continue;
   }
  } else {
   esym.st_shndx  = SHN_UNDEF;
   symaddr.sa_off = 0;
   symaddr.sa_sym = sym;
   /* Skip symbols form removed sections. */
   if (sym->sy_sec && !DCCSection_ISIMPORT(sym->sy_sec)) continue;
  }
#if ELF_HAVE_NULLSYM
  if (first) {
   /* The first dynamic symbol (aka. index ZERO(0)) must be the NULL-symbol. */
   DCCSection_TWrite(elf.elf_dynsym,&null_sym,sizeof(Elf(Sym)));
   first = 0;
  }
#endif /* ELF_HAVE_NULLSYM */
  esym.st_name = DCCSection_DAllocMem(elf.elf_dynstr,
                                      sym->sy_name->k_name,
                                     (sym->sy_name->k_size+0)*sizeof(char),
                                     (sym->sy_name->k_size+1)*sizeof(char),
                                      1,0);
  st_bind = (sym->sy_flags&DCC_SYMFLAG_WEAK) ? STB_WEAK :
            (sym->sy_flags&DCC_SYMFLAG_STATIC) ? STB_LOCAL : STB_GLOBAL;
  if (DCCSym_ISSECTION(symaddr.sa_sym)) st_type = STT_SECTION;
  else st_type = STT_NOTYPE; /* All the other types? */
  esym.st_value = symaddr.sa_off+symaddr.sa_sym->sy_addr;
  /*if (symaddr.sa_sym == sym && DCCSym_ISSECTION(sym))
         esym.st_size = DCCSection_VSIZE(DCCSym_TOSECTION(sym));
  else*/ esym.st_size = sym->sy_size;
  esym.st_info  = ELF(ST_INFO)(st_bind,st_type);
  esym.st_other = vismap[sym->sy_flags&DCC_SYMFLAG_VISIBILITYBASE];
  sym->sy_elfid = dynid++; /* Save symbol index. */
  entry_addr = DCCSection_TADDR(elf.elf_dynsym);
  DCCSection_TWrite(elf.elf_dynsym,&esym,sizeof(esym));
  if (symaddr.sa_sym->sy_sec && !DCCSection_ISIMPORT(symaddr.sa_sym->sy_sec)) {
   DCCSection_Putrel(elf.elf_dynsym,entry_addr+offsetof(Elf(Sym),st_value),
                     DCC_R_DATA_PTR,&symaddr.sa_sym->sy_sec->sc_start);
  }
  /* TODO: Shouldn't we add a relocations for undefined symbol aliases?
   *    >> extern int printf(char const *format, ...)
   *    >>     __attribute__((lib("libc.so.6")));
   *    >>
   *    >> // Re-export 'printf' under a different name
   *    >> extern int my_printf(char const *format, ...)
   *    >>     __attribute__((visibility("default"),alias("printf")));
   *    >> 
   * ELF: .dynsym:
   *         #0: 0x00000000 SHN_UNDEF <NULL>
   *         #1: 0x00000000 SHN_UNDEF printf
   *         #2: 0x00000000 SHN_ABS   my_printf
   *      .rel.dynsym:
   *         ADDROF(.dynsym[2]) R_386_32 -> .dynsym[1] (printf)
   */
 }
 if (elf.elf_hash) {
  /* Generate the symbol hash table. */
  elf_mk_hash(elf.elf_hash,
              elf.elf_dynsym,
              elf.elf_dynstr,
             (dynid*2)/3);
  /* XXX: Option to generate GNU_HASH sections instead of HASH section? */
 }
}

PRIVATE struct DCCSection *
get_relsec(struct DCCSection *__restrict base,
           int alloc_missing) {
 struct TPPKeyword const *kwd;
 char *mbuf = NULL,*buf;
 kwd = base->sc_start.sy_name;
 if (kwd->k_size < 64) {
  buf = (char *)alloca((kwd->k_size+4)*sizeof(char));
 } else {
  mbuf = buf = (char *)malloc((kwd->k_size+4)*sizeof(char));
  if unlikely(!mbuf) { TPPLexer_SetErr(); return NULL; }
 }
 buf[0] = '.';
 buf[1] = 'r';
 buf[2] = 'e';
 buf[3] = 'l';
 memcpy(buf+4,kwd->k_name,kwd->k_size*sizeof(char));
 kwd = TPPLexer_LookupKeyword(buf,kwd->k_size+4,alloc_missing);
 free(mbuf);
 return !kwd ? NULL : !alloc_missing ? DCCUnit_GetSec(kwd)
  : DCCUnit_NewSec(kwd,DCC_SYMFLAG_SEC_R/*|DCC_SYMFLAG_SEC_NOALLOC*/);
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
 info->si_rel         = get_relsec(section,0);
 info->si_hdr.sh_type = secty_of(section);
 info->si_sec         = section;
 /* Not detected by 'secgp_of': */
      if (info->si_hdr.sh_type == SHT_REL)  info->si_grp = SECGP_RELOC;
 else if (info->si_hdr.sh_type == SHT_RELA) info->si_grp = SECGP_RELOCA;
 info->si_hdr.sh_flags = 0;
 if (secflags&DCC_SYMFLAG_SEC_W) info->si_hdr.sh_flags |= SHF_WRITE;
 if (!(secflags&DCC_SYMFLAG_SEC_NOALLOC)) info->si_hdr.sh_flags |= SHF_ALLOC;
 if (secflags&DCC_SYMFLAG_SEC_X) info->si_hdr.sh_flags |= SHF_EXECINSTR;
 if (secflags&DCC_SYMFLAG_SEC_M) info->si_hdr.sh_flags |= SHF_MERGE;
 if (info->si_grp == SECGP_INTERP || info->si_grp == SECGP_SHSTR ||
     info->si_hdr.sh_type == SHT_STRTAB) info->si_hdr.sh_flags |= SHF_STRINGS;
 /* Fill in entry size information for relocation sections. */
 switch (info->si_hdr.sh_type) {
 case SHT_REL:     info->si_hdr.sh_entsize = sizeof(Elf(Rel)); break;
 case SHT_RELA:    info->si_hdr.sh_entsize = sizeof(Elf(Rela)); break;
 case SHT_HASH:    info->si_hdr.sh_entsize = sizeof(uint32_t); break;
 case SHT_DYNAMIC: info->si_hdr.sh_entsize = sizeof(Elf(Dyn)); break;
 case SHT_DYNSYM:
 case SHT_SYMTAB:  info->si_hdr.sh_entsize = sizeof(Elf(Sym)); break;
 case SHT_INIT_ARRAY:
 case SHT_FINI_ARRAY:
 case SHT_PREINIT_ARRAY:
  info->si_hdr.sh_entsize = sizeof(Elf(Addr));
  break;
 default: break;
 }
 //iter->si_hdr.sh_offset = ...; /* Filled later. */
 //iter->si_hdr.sh_size = ...; /* Filled later. */
 if (elf.elf_dynsym) elf.elf_dynsym->sc_elflnk = elf.elf_dynstr;
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
 if unlikely(!elf.elf_shstr) return;
 /* Allocate one ZERO-byte at the start of the '.shstrtab'
  * section (Required so that 'SHN_UNDEF' is unnamed). */
 DCCSection_DAllocAt(elf.elf_shstr,0,sizeof(char));
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  struct DCCSection *section = iter->si_sec;
  assert(section); /* Allocate the section name. */
  if (section == elf.elf_shstr) { shstrtab = iter; goto alloc_name; }
  if (!(section->sc_start.sy_flags&DCC_SYMFLAG_USED) && !secinfo_vsize(iter)) continue;
alloc_name:
  iter->si_hdr.sh_name = (Elf(Word))DCCSection_DAllocMem(elf.elf_shstr,
                                                         section->sc_start.sy_name->k_name,
                                                        (section->sc_start.sy_name->k_size+0)*sizeof(char),
                                                        (section->sc_start.sy_name->k_size+1)*sizeof(char),
                                                         1,0);
 }
 if (shstrtab) elf_mk_secinfo(shstrtab,elf.elf_shstr);
}
PRIVATE void elf_mk_seclnk(void) {
 struct secinfo *iter,*end;
 /* Execute all relocations. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  if (iter->si_sec->sc_elflnk) {
   iter->si_hdr.sh_link = elf_get_secidx(iter->si_sec->sc_elflnk);
  }
 }
}

PRIVATE void elf_mk_execrel(int resolve_weak) {
 struct secinfo *iter,*end;
 /* Execute all relocations. */
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) {
  DCCSection_Reloc(iter->si_sec,resolve_weak);
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

/* When compiling with the 'DCC_LINKER_FLAG_NORELOC'
 * flag set, check if 'rel' can be determined at compile-time. */
PRIVATE int elf_wantrel(struct DCCRel const *__restrict rel) {
 assert(rel),assert(rel->r_sym);
 return !rel->r_sym->sy_sec || DCCSection_ISIMPORT(rel->r_sym->sy_sec);
}

PRIVATE void elf_mk_reldat(void) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) if (iter->si_rel) {
  struct DCCSection *iter_sec = iter->si_sec;
  struct DCCRel *rel_iter,*rel_end;
  target_ptr_t reladdr;
  Elf(Rel)    *reldata;
  size_t relcnt;
  assert(iter_sec);
  relcnt = iter_sec->sc_relc;
  if unlikely(!relcnt) continue;
  rel_end = (rel_iter = iter_sec->sc_relv)+relcnt;
  if (linker.l_flags&DCC_LINKER_FLAG_NORELOC) {
   for (; rel_iter != rel_end; ++rel_iter) {
    if (!elf_wantrel(rel_iter)) --relcnt;
   }
   rel_iter = iter_sec->sc_relv;
  }
  reladdr = DCCSection_DAlloc(iter->si_rel,relcnt*sizeof(Elf(Rel)),
                              DCC_COMPILER_ALIGNOF(Elf(Rel)),0);
  reldata = (Elf(Rel) *)DCCSection_GetText(iter->si_rel,reladdr,
                                           relcnt*sizeof(Elf(Rel)));
  if unlikely(!reldata) break;
  /* Link the relocations against the dynamic symbol table. */
  iter->si_rel->sc_elflnk = elf.elf_dynsym;
  iter->si_rdat = reladdr;
  iter->si_rcnt = relcnt;
  if (linker.l_flags&DCC_LINKER_FLAG_NORELOC) {
   for (; rel_iter != rel_end; ++rel_iter) {
    if (!elf_wantrel(rel_iter)) continue;
    /* Use the '.dynsym' symbol index! */
    reldata->r_info    = ELF(R_INFO)(rel_iter->r_sym->sy_elfid,
                                     rel_iter->r_type);
    reldata->r_offset  = rel_iter->r_addr;
    ++reldata;
   }
  } else {
   for (; rel_iter != rel_end; ++rel_iter) {
    /* Use the '.dynsym' symbol index! */
    reldata->r_info    = ELF(R_INFO)(rel_iter->r_sym->sy_elfid,
                                     rel_iter->r_type);
    reldata->r_offset  = rel_iter->r_addr;
    ++reldata;
   }
  }
 }
}
PRIVATE void elf_mk_reladj(void) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 for (; iter != end; ++iter) if (iter->si_rel) {
  struct DCCSection *iter_sec = iter->si_sec;
  uint8_t *text_data; target_ptr_t secbase;
  Elf(Rel)    *reldata,*relend;
  assert(iter_sec);
  if unlikely(!iter->si_rcnt) continue;
  reldata = (Elf(Rel) *)DCCSection_GetText(iter->si_rel,iter->si_rdat,
                                           iter->si_rcnt*sizeof(Elf(Rel)));
  if unlikely(!reldata) break;
  secbase   = iter_sec->sc_base;
  relend    = reldata+iter->si_rcnt;
  text_data = iter->si_sec->sc_text.tb_begin;
  for (; reldata != relend; ++reldata) {
   /* Adjust the relocation address from section-relative to image-relative.
    * WARNING: This can only be done after virtual addresses have been generated. */
   if (ELF(R_SYM)(reldata->r_info)  == STN_UNDEF &&
       ELF(R_TYPE)(reldata->r_info) == DCC_R_DATA_PTR) {
    /* Must convert this relocation. */
    reldata->r_info = ELF(R_INFO)(STN_UNDEF,DCC_R_RELATIVE);
   }
   reldata->r_offset += secbase;
  }
 }
}

PRIVATE void elf_mk_dt(Elf(Sword) dt) {
 Elf(Dyn) dyn;
 dyn.d_tag = dt;
 DCCSection_TWrite(elf.elf_dynamic,&dyn,sizeof(Elf(Dyn)));
}
PRIVATE void elf_mk_dtd(Elf(Sword) dt, Elf(Addr) v) {
 Elf(Dyn) dyn;
 dyn.d_tag      = dt;
 dyn.d_un.d_ptr = v;
 DCCSection_TWrite(elf.elf_dynamic,&dyn,sizeof(Elf(Dyn)));
}


PRIVATE void
elf_mk_dyndat(void) {
 struct secinfo *iter,*end;
 if (elf.elf_dynamic) {
  Elf(Addr) dynflags = 0;
  int has_relsec;
  elf_clr_unused(&elf.elf_dynsym);
  elf_clr_unused(&elf.elf_hash);
  elf.elf_dynoff = DCCSection_TADDR(elf.elf_dynamic);
  /* Put 'DT_*' information */
  if (elf.elf_dynstr) {
   struct DCCSection *dependency;
   DCCUnit_ENUMIMP(dependency) {
    /* Allocate library dependency strings. */
    target_ptr_t depnam = DCCSection_DAllocMem(elf.elf_dynstr,
                                               dependency->sc_start.sy_name->k_name,
                                              (dependency->sc_start.sy_name->k_size+0)*sizeof(char),
                                              (dependency->sc_start.sy_name->k_size+1)*sizeof(char),
                                               1,0);
    elf_mk_dtd(DT_NEEDED,depnam);
   }
  }
  if (linker.l_soname) {
   /* Allocate a custom entry for 'DT_SONAME'. */
   target_ptr_t soname = DCCSection_DAllocMem(elf.elf_dynstr,
                                              linker.l_soname->s_text,
                                             (linker.l_soname->s_size+0)*sizeof(char),
                                             (linker.l_soname->s_size+1)*sizeof(char),
                                              1,0);
   elf_mk_dtd(DT_SONAME,soname);
  }
  elf_clr_unused(&elf.elf_dynstr);
  if (elf.elf_hash)   elf_mk_dt(DT_HASH);
  if (elf.elf_dynsym) elf_mk_dt(DT_SYMTAB),
                      elf_mk_dt(DT_SYMENT);
  if (elf.elf_dynstr) elf_mk_dt(DT_STRTAB),
                      elf_mk_dt(DT_STRSZ);
  has_relsec = 0;
  end = (iter = elf.elf_secv)+elf.elf_secc;
  for (; iter != end; ++iter) {
   if (iter->si_grp == SECGP_RELOC) 
    assert(secinfo_vsize(iter)),has_relsec |= 1;
   if (iter->si_grp == SECGP_RELOCA) assert(secinfo_vsize(iter)),has_relsec |= 2;
   /* Check if there are relocations in non-writable sections. */
   if (iter->si_rel && !SECGP_ISW(iter->si_grp)) has_relsec |= 4;
  }
  /* TODO: 'DT_RUNPATH': With custom library paths, shouldn't we
   *                     include that information in the binary? */
  if (has_relsec&1) elf_mk_dt(DT_REL),
                    elf_mk_dt(DT_RELSZ),
                    elf_mk_dt(DT_RELENT);
  if (has_relsec&2) elf_mk_dt(DT_RELA),
                    elf_mk_dt(DT_RELASZ),
                    elf_mk_dt(DT_RELAENT);
  if (has_relsec&4) elf_mk_dtd(DT_TEXTREL,0),
                    dynflags |= DF_TEXTREL;
  if (dynflags)     elf_mk_dtd(DT_FLAGS,dynflags);
  if (DCCSection_TADDR(elf.elf_dynamic) != elf.elf_dynoff) elf_mk_dt(DT_NULL);
  elf.elf_dync   = (size_t)(DCCSection_TADDR(elf.elf_dynamic)-
                            elf.elf_dynoff)/
                            sizeof(Elf(Dyn));
 }
}

PRIVATE void
elf_mk_dynfll(void) {
 Elf(Dyn) *iter,*end;
 if (!elf.elf_dync) return;
 assert(elf.elf_dynamic);
 iter = (Elf(Dyn) *)DCCSection_GetText(elf.elf_dynamic,elf.elf_dynoff,
                                       elf.elf_dync*sizeof(Elf(Dyn)));
 if unlikely(!iter) return;
 end = iter+elf.elf_dync;
 for (; iter != end; ++iter) {
  switch (iter->d_tag) {
  case DT_HASH:   if (!elf.elf_hash)   goto def; iter->d_un.d_ptr = elf.elf_hash  ->sc_base; break;
  case DT_SYMTAB: if (!elf.elf_dynsym) goto def; iter->d_un.d_ptr = elf.elf_dynsym->sc_base; break;
  case DT_SYMENT: if (!elf.elf_dynsym) goto def; iter->d_un.d_ptr = sizeof(Elf(Sym)); break;
  case DT_STRTAB: if (!elf.elf_dynstr) goto def; iter->d_un.d_ptr = elf.elf_dynstr->sc_base; break;
  case DT_STRSZ:  if (!elf.elf_dynstr) goto def;
   iter->d_un.d_ptr = (Elf(Addr))(elf.elf_dynstr->sc_text.tb_max-
                                  elf.elf_dynstr->sc_text.tb_begin);
   break;
  {
   struct secinfo *sec_iter,*sec_end;
   secgp_t seach_gp; Elf(Addr) val;
   if (DCC_MACRO_FALSE) { case DT_RELA: case DT_RELASZ: seach_gp = SECGP_RELOCA; }
   if (DCC_MACRO_FALSE) { case DT_REL:  case DT_RELSZ:  seach_gp = SECGP_RELOC;  }
   sec_end = (sec_iter = elf.elf_secv)+elf.elf_secc;
   for (; sec_iter != sec_end; ++sec_iter) if (sec_iter->si_grp == seach_gp) goto found_relsec;
   goto def; found_relsec:
   if (iter->d_tag == DT_REL ||
       iter->d_tag == DT_RELA) {
    iter->d_un.d_ptr = sec_iter->si_hdr.sh_addr;
    break;
   }
   val = 0;
   while (sec_iter != sec_end && sec_iter->si_grp == seach_gp)
          val += secinfo_vsize(sec_iter),++sec_iter;
   iter->d_un.d_ptr = val;
  } break;

  case DT_RELENT:
   iter->d_un.d_ptr = sizeof(Elf(Rel));
   break;

  case DT_SONAME:
  case DT_FLAGS:
  case DT_TEXTREL:
  case DT_NEEDED:
   /* Already filled. */
   break;

  default:
def: iter->d_un.d_ptr = 0;
   break;
  }
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


PRIVATE void elf_mk_delsecunused(secgp_t min_gp) {
 struct secinfo *iter,*end;
 end = (iter = elf.elf_secv)+elf.elf_secc;
 while (iter != end) {
  assert(iter->si_sec);
  /* Ignore empty section. */
  if (!secinfo_vsize(iter) &&
      !(iter->si_sec->sc_start.sy_flags&DCC_SYMFLAG_USED) &&
       (iter->si_grp >= min_gp)) {
   /* Delete this section. */
   assert(elf.elf_secc);
   printf("Deleting: '%s'\n",iter->si_sec->sc_start.sy_name->k_name);
   /* Must update dynamic symbol information! */
   if (elf.elf_dynsym) {
    Elf(Section) seci = (Elf(Section))(iter-elf.elf_secv);
    Elf(Sym) *sym_iter,*sym_end;
    sym_iter = (Elf(Sym) *)elf.elf_dynsym->sc_text.tb_begin;
    sym_end  = (Elf(Sym) *)elf.elf_dynsym->sc_text.tb_end;
    for (; sym_iter < sym_end; ++sym_iter) {
     assertf(sym_iter->st_shndx != seci,
             "Attempting to delete section '%s' with dynamic symbols",
             iter->si_sec->sc_start.sy_name->k_name);
     if (sym_iter->st_shndx > seci) --sym_iter->st_shndx;
    }
   }
   --end;
   memmove(iter,iter+1,(end-iter)*
           sizeof(struct secinfo));
   --elf.elf_secc;
   continue;
  }
  ++iter;
 }
}


PRIVATE void elf_clr_unused(struct DCCSection **psec) {
 struct DCCSection *sec;
 assert(psec); sec = *psec;
 if (sec && sec->sc_text.tb_max == sec->sc_text.tb_begin) *psec = NULL;
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
PRIVATE void elf_mk_phdvec(void) {
 struct phinfo *curhdr,*endhdr;
 struct secinfo *iter,*end,*partend;
 target_off_t faddr_align,vaddr_align;
 Elf(Addr) sec_vaddr; /* Current virtual section address. */
 Elf(Off)  sec_faddr; /* Current virtual section address. */
 /* Look at all sections and generate the program header vector. */
 /* NOTE: Also look at the difference between virtual and physical section size.
  *    >> When it is small enough, simply ZERO-extend the section
  *       physically to prevent a program header split. */
 if unlikely(!elf.elf_secc) return;
again:
 end = (iter = elf.elf_secv)+elf.elf_secc;
 sec_vaddr  = elf.elf_base;
 sec_faddr  = sizeof(Elf(Ehdr));
 endhdr = (curhdr = elf.elf_phdv)+elf.elf_phdc;
 if (elf.elf_dynamic) {
  if unlikely(elf.elf_phdc < 2) goto alloc_more;
  /* The first program header describes the program header table. */
  faddr_align = sec_faddr % DCC_TARGET_PAGESIZE;
  vaddr_align = sec_vaddr % DCC_TARGET_PAGESIZE;
  if (faddr_align < vaddr_align) faddr_align += DCC_TARGET_PAGESIZE;
  /* Fix the virtual address. */
  sec_vaddr += (faddr_align-vaddr_align);
  assert((sec_faddr % DCC_TARGET_PAGESIZE) ==
         (sec_vaddr % DCC_TARGET_PAGESIZE));
  /* Fill the first header with the PHDR entry. */
  curhdr->ph_siv          = NULL;
  curhdr->ph_sic          = 0;
  curhdr->ph_hdr.p_type   = PT_PHDR;
  curhdr->ph_hdr.p_offset = sec_faddr;
  curhdr->ph_hdr.p_vaddr  =
  curhdr->ph_hdr.p_paddr  = sec_vaddr;
  curhdr->ph_hdr.p_filesz =
  curhdr->ph_hdr.p_memsz  = elf.elf_phdc*sizeof(Elf(Phdr));
  curhdr->ph_hdr.p_flags  = PF_R|PF_X; /* GCC makes this executable, too. - But why? */
  curhdr->ph_hdr.p_align  = DCC_COMPILER_ALIGNOF(Elf(Phdr));
  ++curhdr;

  sec_faddr += (elf.elf_phdc  )*sizeof(Elf(Phdr))+
               (elf.elf_secc+1)*sizeof(Elf(Shdr));
  sec_vaddr += (elf.elf_phdc  )*sizeof(Elf(Phdr))+
               (elf.elf_secc+1)*sizeof(Elf(Shdr));
  faddr_align = sec_faddr % DCC_TARGET_PAGESIZE;
  vaddr_align = sec_vaddr % DCC_TARGET_PAGESIZE;
  if (faddr_align < vaddr_align) faddr_align += DCC_TARGET_PAGESIZE;
  /* Fix the virtual address. */
  sec_vaddr += (faddr_align-vaddr_align);
  assert((sec_faddr % DCC_TARGET_PAGESIZE) ==
         (sec_vaddr % DCC_TARGET_PAGESIZE));

  /* Fill the second header with a LOAD command loading the E/P/S-HDRs. */
  curhdr->ph_hdr.p_type    = PT_LOAD;
  curhdr->ph_hdr.p_offset  = 0;
  curhdr->ph_hdr.p_vaddr   =
  curhdr->ph_hdr.p_paddr   = sec_vaddr-sec_faddr;
  curhdr->ph_hdr.p_filesz  =
  curhdr->ph_hdr.p_memsz   = sec_faddr;
  curhdr->ph_hdr.p_flags   = PF_R|PF_X; /* GCC makes this executable, too. - But why? */
  curhdr->ph_hdr.p_align   = (DCC_COMPILER_ALIGNOF(Elf(Phdr))+
                              (DCC_TARGET_PAGESIZE-1)) &
                             ~(DCC_TARGET_PAGESIZE-1);
  ++curhdr;

 } else {
  sec_faddr += (elf.elf_phdc  )*sizeof(Elf(Phdr));
  sec_faddr += (elf.elf_secc+1)*sizeof(Elf(Shdr));
 }
 do {
  secgp_t      hdr_group;
  target_siz_t last_overalloc; /* Amount of overallocated ZERO-bytes at the end of the last section.. */
  assert(iter < end);
  assert(secinfo_vsize(iter) >= secinfo_msize(iter));
  hdr_group = iter->si_grp;
  if (group_info[hdr_group].fp_type == PT_NULL) {
   /* Section without program header.
    * >> Still allocate section memory for this one! */
   assert(iter->si_hdr.sh_addr == 0);
   iter->si_hdr.sh_offset = sec_faddr;
   sec_faddr += secinfo_vsize(iter);
   ++iter;
   goto skip_section;
  }
  /* Must start over if we need to increment the header counter. */
  if (curhdr == endhdr) { alloc_more: if unlikely(!elf_mk_phdr()) return; goto again; }
#if 1
  if (group_info[hdr_group].fp_type == PT_LOAD ||
      group_info[hdr_group].fp_type == PT_DYNAMIC) {
   /* Prevent addresses of segments with different attributes from overlapping. */
   sec_vaddr +=  (DCC_TARGET_PAGESIZE-1);
   sec_vaddr &= ~(DCC_TARGET_PAGESIZE-1);
  }
#endif

  /* Align the sub-pagedir virtual address of the program header to its file address. */
  faddr_align = sec_faddr % DCC_TARGET_PAGESIZE;
  vaddr_align = sec_vaddr % DCC_TARGET_PAGESIZE;
  if (faddr_align < vaddr_align) faddr_align += DCC_TARGET_PAGESIZE;
  /* Fix the virtual address. */
  sec_vaddr += (faddr_align-vaddr_align);
  assert((sec_faddr % DCC_TARGET_PAGESIZE) ==
         (sec_vaddr % DCC_TARGET_PAGESIZE));
  /* Fill out initial program header information. */
  curhdr->ph_siv         = iter;
  curhdr->ph_hdr.p_type  = group_info[hdr_group].fp_type;
  curhdr->ph_hdr.p_flags = group_info[hdr_group].fp_flags;
  curhdr->ph_hdr.p_align = 1;
  /* Collect all sections that should be associated with this header.
   * NOTE: Also collect information about alignment requirements. */
  last_overalloc         = 0;
  do {
   /* Don't fix large overallocation. */
   if (last_overalloc > 64) break;
   if (curhdr->ph_hdr.p_align < iter->si_hdr.sh_addralign)
       curhdr->ph_hdr.p_align = iter->si_hdr.sh_addralign;
   last_overalloc = secinfo_vsize(iter)-secinfo_msize(iter);
   ++iter;
   /* Make sure to only include sections from the same groups! */
  } while (iter != end &&
#if 1
           group_info[iter->si_grp].fp_type  == group_info[hdr_group].fp_type &&
           group_info[iter->si_grp].fp_flags == group_info[hdr_group].fp_flags
#else
           iter->si_grp == hdr_group
#endif
           );
  curhdr->ph_sic = (size_t)(iter-curhdr->ph_siv);
  assert(curhdr->ph_sic);
  /* Fix explicit user-alignments. */
  sec_faddr = (sec_faddr+(curhdr->ph_hdr.p_align-1)) & ~(curhdr->ph_hdr.p_align-1);
  sec_vaddr = (sec_vaddr+(curhdr->ph_hdr.p_align-1)) & ~(curhdr->ph_hdr.p_align-1);
  /* Make sure that page-alignment is still correct. */
  assert((sec_faddr % DCC_TARGET_PAGESIZE) ==
         (sec_vaddr % DCC_TARGET_PAGESIZE));
  /* NOTE: Elf requires that PT_LOAD sections be page-aligned! */
  if (curhdr->ph_hdr.p_type == PT_LOAD) {
   curhdr->ph_hdr.p_align +=  (DCC_TARGET_PAGESIZE-1);
   curhdr->ph_hdr.p_align &= ~(DCC_TARGET_PAGESIZE-1);
  }
  /* At this point, we know the file/virtual address pair for this header! */
  curhdr->ph_hdr.p_offset = sec_faddr;
  curhdr->ph_hdr.p_paddr  = sec_vaddr;
  curhdr->ph_hdr.p_vaddr  = sec_vaddr;
  /* Now iterate all associated sections and assign their addresses. */
  partend = iter,iter = curhdr->ph_siv;
  assert(partend == curhdr->ph_siv+curhdr->ph_sic);
  for (; iter != partend; ++iter) {
   target_ptr_t sec_vsize;
   /* Fix per-section alignments. */
   sec_faddr  = (sec_faddr+(iter->si_hdr.sh_addralign-1)) & ~(iter->si_hdr.sh_addralign-1);
   sec_vaddr  = (sec_vaddr+(iter->si_hdr.sh_addralign-1)) & ~(iter->si_hdr.sh_addralign-1);
   iter->si_hdr.sh_offset = sec_faddr;
   iter->si_hdr.sh_addr   = sec_vaddr;
   iter->si_sec->sc_base  = sec_vaddr;
   sec_vsize = secinfo_vsize(iter);
   sec_faddr += sec_vsize;
   sec_vaddr += sec_vsize;
  }
  assert(partend == iter);
  /* Finally, save the program header file/memory size */
  curhdr->ph_hdr.p_filesz = (Elf(Word))(sec_faddr-curhdr->ph_hdr.p_offset);
  curhdr->ph_hdr.p_memsz  = (Elf(Word))(sec_vaddr-curhdr->ph_hdr.p_vaddr);
  assert(curhdr != endhdr);
  /* Continue with the next header. */
  if ((curhdr++)->ph_hdr.p_type == PT_DYNAMIC) {
   /* <sarcasm>Because ELF loaders are smart enough to load dynamic data section
    *          only during runtime-linking, we don't have to declare a secondary
    *          program header describing the same data as the dynamic block.
    * </sarcasm> */
   if (curhdr == endhdr) goto alloc_more;
   *curhdr = curhdr[-1];
   curhdr->ph_hdr.p_type = PT_LOAD;
   curhdr->ph_hdr.p_align +=  (DCC_TARGET_PAGESIZE-1);
   curhdr->ph_hdr.p_align &= ~(DCC_TARGET_PAGESIZE-1);
   ++curhdr;
  }

skip_section:;
 } while (iter != end);
}
PRIVATE void
elf_mk_phdsort(void) {
 /* Sort program headers. */
 struct phinfo temp,*v_end,*iter,*next;
 size_t n = elf.elf_phdc;
 v_end = elf.elf_phdv+n;
 while (n--) {
  iter = elf.elf_phdv;
  for (;;) {
   next = iter+1;
   if (next == v_end) break;
   if (pht_weight[next->ph_hdr.p_type] <
       pht_weight[iter->ph_hdr.p_type]) {
    temp = *iter;
    *iter = *next;
    *next = temp;
   }
   iter = next;
  }
 }
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

 ehdr.e_ident[EI_MAG0]    = ELFMAG0;
 ehdr.e_ident[EI_MAG1]    = ELFMAG1;
 ehdr.e_ident[EI_MAG2]    = ELFMAG2;
 ehdr.e_ident[EI_MAG3]    = ELFMAG3;
 ehdr.e_ident[EI_CLASS]   = DCC_TARGET_ELFCLASS;
 ehdr.e_ident[EI_DATA]    = DCC_TARGET_ELFDATA;
 ehdr.e_ident[EI_VERSION] = DCC_TARGET_ELFVERSION;
 ehdr.e_ident[EI_OSABI]   = DCC_TARGET_ELFOSABI;
 ehdr.e_machine           = DCC_TARGET_ELF_MACHINE;
 ehdr.e_type              = elf.elf_type;
 ehdr.e_version           = DCC_TARGET_ELFVERSION;
 ehdr.e_ehsize            = sizeof(Elf(Ehdr));

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
 ehdr.e_shnum    = (Elf(Half))(elf.elf_secc+1);

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

 { /* Write the NULL-section header. */
   Elf(Shdr) null_hdr;
   memset(&null_hdr,0,sizeof(null_hdr));
   s_writea(fd,&null_hdr,sizeof(null_hdr));
 }

 { /* Write the section headers. */
   struct secinfo *iter,*end;
   end = (iter = elf.elf_secv)+elf.elf_secc;
   for (; iter != end; ++iter) {
    iter->si_hdr.sh_size = secinfo_vsize(iter);
    s_writea(fd,&iter->si_hdr,sizeof(iter->si_hdr));
   }
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
    if (text_size != phys_size) elf_fd_padbytes(fd,phys_size-text_size);
   }
 }
 /* And we're done! */
}



PUBLIC void
DCCLinker_Make(stream_t target) {
 memset(&elf,0,sizeof(elf));

 /* TODO: Output object files? */
 if (linker.l_flags&DCC_LINKER_FLAG_SHARED) {
  elf.elf_base = 0; /* Load shared libraries at offset
                     * ZERO(0), relying on relocations. */
  elf.elf_type = ET_DYN;
 } else {
  /* TODO: Commandline switch: '-fPIC'
   * NOTE: This flag is actually fully implemented & working. */
  //linker.l_flags |= DCC_LINKER_FLAG_NORELOC;

#ifdef DCC_TARGET_X86
  elf.elf_base = 0x08048000;
#else
#   error FIXME
#endif
  elf.elf_type = ET_EXEC;
 }

 /* Allocate internal ELF sections. */
 elf.elf_interp  = DCCUnit_NewSecs(".interp",  DCC_SYMFLAG_SEC_NOALLOC|DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 elf.elf_shstr   = DCCUnit_NewSecs(".shstrtab",DCC_SYMFLAG_SEC_NOALLOC|DCC_SYMFLAG_SEC(1,0,0,0,1,0));
 elf.elf_dynamic = DCCUnit_NewSecs(".dynamic",                         DCC_SYMFLAG_SEC(1,1,0,0,0,0));
 elf.elf_dynsym  = DCCUnit_NewSecs(".dynsym",                          DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 elf.elf_dynstr  = DCCUnit_NewSecs(".dynstr",                          DCC_SYMFLAG_SEC(1,0,0,0,1,0));
 elf.elf_hash    = DCCUnit_NewSecs(".hash",                            DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 if (elf.elf_dynsym) elf.elf_dynsym->sc_elflnk = elf.elf_dynstr;
#define CC(x) { x; if unlikely(!OK) goto end; }
 if unlikely(!OK) goto end;
 CC(elf_mk_entry());

 /* Delete unused stuff. */
 DCCUnit_ClearUnused();
 DCCUnit_ClearUnusedLibs();

 CC(elf_mk_relsec());        /* Create stub relocation sections. Must be done now
                              * because later we can no longer create new sections. */
 CC(elf_mk_secvec());        /* Create the ELF section vector. */
 CC(elf_mk_delsecunused(SECGP_NOALLOC)); /* Remove any unused section. */
 CC(elf_mk_secsort());       /* Sort section headers, thus greatly improving cache locality
                              * and restricting the amount of potential program headers. */
 CC(elf_mk_execdisp());      /* Execute and delete compile-time DISP-relocations. */
 CC(elf_mk_delnoprel());     /* Delete obsolete relocations, but keep associated symbol data alive. */
 CC(elf_mk_dynsym());        /* Create dynamic symbol information. */
 CC(elf_mk_reldat());        /* Generate relocation data. */
 CC(elf_mk_delsecunused(SECGP_RELOC)); /* Delete any sections still unused (including any associated relocation section) */
 CC(elf_mk_dyndat());        /* Generate general-purpose dynamic information. */
 CC(elf_mk_delsecunused(SECGP_DYNAMIC)); /* Delete all unused dynamic sections. */
 elf_clr_unused(&elf.elf_dynamic);
 if (elf.elf_dynamic) {
 /* WARNING: ___NEVER___ generate a '.interp' section for static objects!
  *       >> It took me forever to figure it out, but the linux linker
  *          will just SEGFAULT when you feed it an ELF binary without
  *          relocations. (At least my $h1tty version does...) */
  CC(elf_mk_interp());       /* Create and fill the '.interp' section. */
 }
 CC(elf_mk_secnam());        /* Allocate section names. */
 CC(elf_mk_seclnk());        /* Fix section links. */
 CC(elf_mk_delsecunused(SECGP_INTERP)); /* Delete _ALL_ sections still unused. */

 CC(elf_mk_phdvec());        /* Generate program headers suitable for the generated (and sorted) list of sections.
                              * NOTE: This also generates section virtual and file addresses. */
 CC(elf_mk_phdsort());       /* Sort program headers. */
 /* After this point, no new section data may be written, and no new
  * sections may be created. Only existing data is allowed to be modified! */
 CC(elf_mk_dynfll());        /* Now that everything is known, fill out dynamic header information. */

 CC(elf_mk_execrel(0));      /* Execute all relocations. */
 CC(elf_mk_reladj());        /* Adjust relocation addresses to image-relative. */
 elf_mk_outfile(target);
#undef CC
end:
 DCCSym_XDecref(elf.elf_entry);
 free(elf.elf_phdv);
 free(elf.elf_secv);
}

DCC_DECL_END

#endif /* !GUARD_DCC_LINKER_ELF_C_INL */
