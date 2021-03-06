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
#ifndef GUARD_DCC_LINKER_PE_C_INL
#define GUARD_DCC_LINKER_PE_C_INL 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/linker.h>
#include <dcc/stream.h>
#include <dcc/vstack.h>

#include <stdio.h>

#include "linker-pe.h"

DCC_DECL_BEGIN

/* Template PE header. */
static PE_HEADER const pe_template = {
 /* dhdr            */{
 /* dhdr.e_magic    */IMAGE_DOS_SIGNATURE,
 /* dhdr.e_cblp     */0x0090,
 /* dhdr.e_cp       */0x0003,
 /* dhdr.e_crlc     */0x0000,
 /* dhdr.e_cparhdr  */0x0004,
 /* dhdr.e_minalloc */0x0000,
 /* dhdr.e_maxalloc */0xFFFF,
 /* dhdr.e_ss       */0x0000,
 /* dhdr.e_sp       */0x00B8,
 /* dhdr.e_csum     */0x0000,
 /* dhdr.e_ip       */0x0000,
 /* dhdr.e_cs       */0x0000,
 /* dhdr.e_lfarlc   */0x0040,
 /* dhdr.e_ovno     */0x0000,
 /* dhdr.e_res      */{0,0,0,0},
 /* dhdr.e_oemid    */0x0000,
 /* dhdr.e_oeminfo  */0x0000,
 /* dhdr.e_res2     */{0,0,0,0,0,0,0,0,0,0},
 /* Practically the _only_ important one: Offset from file start: where is the ~real~ header at? */
 /* dhdr.e_lfanew   */(DWORD)(uintptr_t)&((PE_HEADER *)0)->ntsg},
 /* dcod            */{
 /* code... */0x0e,0x1f,0xba,0x0e,0x00,0xb4,0x09,0xcd,0x21,0xb8,0x01,0x4c,0xcd,0x21,
 /* text... */'T','h','i','s',' ','p','r','o','g','r','a','m',' ','c','a','n','n','o','t',' ','b','e',' ',
              'r','u','n',' ','i','n',' ','D','O','S',' ','m','o','d','e','.','\r','\r','\n','$',
 /* padding */0,0,0,0,0,0,0},
 /* ntsg    */IMAGE_NT_SIGNATURE,
 /* fhdr                      */{
 /* fhdr.Machine              */DCC_PE_TARGET_MACHINE,
 /* fhdr.NumberOfSections     */0, /* Filled later. */
 /* fhdr.TimeDateStamp        */0, /* Filled later? */
 /* fhdr.PointerToSymbolTable */0, /* Filled later. */
 /* fhdr.NumberOfSymbols      */0, /* Filled later. */
 /* fhdr.SizeOfOptionalHeader */sizeof(DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER),
 /* fhdr.Characteristics      */(IMAGE_FILE_EXECUTABLE_IMAGE
                                |IMAGE_FILE_LINE_NUMS_STRIPPED
                                |IMAGE_FILE_LOCAL_SYMS_STRIPPED
#if DCC_TARGET_SIZEOF_POINTER > 4
                                |IMAGE_FILE_LARGE_ADDRESS_AWARE
#endif
#if DCC_TARGET_SIZEOF_POINTER == 4
                                |IMAGE_FILE_32BIT_MACHINE
#endif
                                |IMAGE_FILE_DEBUG_STRIPPED)},
 /* ohdr                             */{
 /* ohdr.Magic                       */DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER_MAGIC,
 /* ohdr.MajorLinkerVersion          */0x06,
 /* ohdr.MinorLinkerVersion          */0x00,
 /* ohdr.SizeOfCode                  */0, /* Filled later? */
 /* ohdr.SizeOfInitializedData       */0, /* Filled later? */
 /* ohdr.SizeOfUninitializedData     */0, /* Filled later? */
 /* ohdr.AddressOfEntryPoint         */0, /* Filled later. */
 /* ohdr.BaseOfCode                  */0, /* Filled later. */
#if !(DCC_TARGET_CPUF&DCC_CPUF_X86_64)
 /* ohdr.BaseOfData                  */0, /* Filled later. */
#endif
 /* ohdr.ImageBase                   */0x00100000,
 /* ohdr.SectionAlignment            */4096,
 /* ohdr.FileAlignment               */0x200,
 /* ohdr.MajorOperatingSystemVersion */0x0004,
 /* ohdr.MinorOperatingSystemVersion */0x0000,
 /* ohdr.MajorImageVersion           */0x0000,
 /* ohdr.MinorImageVersion           */0x0000,
 /* ohdr.MajorSubsystemVersion       */0x0004,
 /* ohdr.MinorSubsystemVersion       */0x0000,
 /* ohdr.Win32VersionValue           */0x00000000,
 /* ohdr.SizeOfImage                 */0x00000000,
 /* ohdr.SizeOfHeaders               */0x00000000,
 /* ohdr.CheckSum                    */0x00000000,
 /* ohdr.Subsystem                   */0x0002,
 /* ohdr.DllCharacteristics          */0x0000,
 /* ohdr.SizeOfStackReserve          */0x00100000,
 /* ohdr.SizeOfStackCommit           */0x00001000,
 /* ohdr.SizeOfHeapReserve           */0x00100000,
 /* ohdr.SizeOfHeapCommit            */0x00001000,
 /* ohdr.LoaderFlags                 */0x00000000,
 /* ohdr.NumberOfRvaAndSizes         */0x00000010,
 /* ohdr.DataDirectory               */{{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},
                                        {0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0},{0,0}}
                                       }
};

#define PETYPE_EXE 0 /* Default case: console-based .exe application. */
#define PETYPE_DLL 1 /* Shared library (aka. dll) */
#define PETYPE_GUI 2 /* WinMain(): GUI-application */


/* NOTE: These ids also encode the order of sections! */
#define SECTY_TEXT    0
#define SECTY_DATA    1
#define SECTY_BSS     2
#define SECTY_IDATA   3
#define SECTY_PDATA   4
#define SECTY_OTHER   5
#define SECTY_RSRC    6
#define SECTY_STAB    7
#define SECTY_COMMENT 8
#define SECTY_RELOC   9 /*< Relocations (MUST BE LAST!). */
typedef unsigned int secty_t; /*< One of 'SECTY_*'. */
#define SECTY_COUNT 10 /*< Amount of section types (Not really a value) */
#define SECTY_UNKNOWN ((secty_t)-1) /* Unknown type */
PRIVATE secty_t secty_of(struct DCCSection const *__restrict section);


struct secinfo {
 struct DCCSection   *si_sec;   /*< [1..1] Associated section. */
 secty_t              si_type;  /*< Section type (One of 'SECTY_*'; never 'SECTY_UNKNOWN') */
 target_ptr_t         si_addr;  /*< [>= :pe.pe_imgbase][== si_sec->sc_base]
                                 *   Address where this section must be relocated to.
                                 *   NOTE: Due to how the windows linker works, this is likely the
                                 *         absolute, run-time address of the section.
                                 *         Note though, that the linker is allowed to move the section
                                 *         elsewhere, unless relocation information was stripped. */
 uint8_t             *si_data;  /*< [0..si_msize] Pointer to compile-time pre-initialized data. */
 size_t               si_msize; /*< [<= si_vsize] Size of the compile-time pre-initialized data. */
 target_siz_t         si_vsize; /*< [>= si_msize][!0] Actual (virtual) size of the section (A different between this and 'si_msize' is filled with ZEROes by the runtime-linker). */
 IMAGE_SECTION_HEADER si_hdr;   /*< In-file header for this section. */
};

struct DCCPEInfo {
 uint8_t                pe_type;      /*< One of 'PETYPE_*' */
 uint8_t                pe_subsystem; /*< One of 'IMAGE_SUBSYSTEM_*' */
 DWORD                  pe_filalign;  /*< Minimum alignment for headers within the generated binary. */
 target_siz_t           pe_secalign;  /*< Minimum alignment for sections within the generated binary. */
 target_ptr_t           pe_imgbase;   /*< Image base address. */
                       
 size_t                 pe_secc;      /*< Amount of sections contained in this PE binary. */
 struct secinfo        *pe_secv;      /*< [0..pe_secc][owned][sort(->si_type)] Vector of sections, sorted by the type.
                                       *   NOTE: Empty sections are excluded from this list. */
 size_t                 pe_exportc;   /*< Amount of symbols that will be exported. */
 /*ref*/struct DCCSym **pe_exportv;   /*< [1..1][0..pe_exportc][owned] Vector of exported symbols. */
 /*ref*/struct DCCSym  *pe_entry;     /*< [1..1] PE Entry point. */
 struct DCCSection     *pe_reloc;     /*< [0..1] Section used for relocations. */
 struct DCCSection     *pe_thunk;     /*< [0..1] Section used for thunk data. */
 DWORD                  pe_imp_addr;  /*< In 'pe_thunk': Section address of the import table. */
 DWORD                  pe_imp_size;  /*< In 'pe_thunk': Import table size in bytes (or ZERO(0) when unused). */
 DWORD                  pe_exp_addr;  /*< In 'pe_thunk': Section address of the export table. */
 DWORD                  pe_exp_size;  /*< In 'pe_thunk': Export table size in bytes (or ZERO(0) when unused). */
 DWORD                  pe_iat_addr;  /*< In 'pe_thunk': Section address of the import address table. */
 DWORD                  pe_iat_size;  /*< In 'pe_thunk': import address table size in bytes (or ZERO(0) when unused). */
};

#if DCC_TARGET_HASF(F_X86_64)
#define PE_STDSYM2(name,suffix)     name suffix
#define PE_STDSYM(name,suffix)  "_" name suffix
#else
#define PE_STDSYM2(name,suffix) name
#define PE_STDSYM(name,suffix)  name
#endif

/* Information about the PE file currently being compiled. */
PRIVATE struct DCCPEInfo pe;


PRIVATE void pe_mk_genrt(void);                                  /* Generate PE runtime information. */
PRIVATE void pe_mk_secvec(void);                                 /* Generate the vector of PE section. */
PRIVATE void pe_mk_collect_exports(void);                        /* Collect all functions that should be exported. */
PRIVATE void pe_mk_buildita(void);                               /* Built ITA function wrappers. */
PRIVATE void pe_mk_relocate(void);                               /* Execute relocations. */
PRIVATE void pe_mk_relrva(struct secinfo *__restrict info);      /* Convert RVA relocations within the section associated with the given record. */
PRIVATE void pe_mk_imptab(struct DCCSection *__restrict thunk);  /* Write the import table to the given '.thunk' section. */
PRIVATE void pe_mk_exptab(struct DCCSection *__restrict thunk);  /* Write the export table to the given '.thunk' section. */
PRIVATE void pe_mk_reldat(struct DCCSection *__restrict relocs); /* Generate relocation informations and write them to 'relocs'. */
PRIVATE void pe_mk_writefile(stream_t fd);                       /* Generate the final PE binary by writing it to 'fd'. */

LOCAL   int     pe_symcmp(struct DCCSym const *__restrict a, struct DCCSym const *__restrict b);
LOCAL   int     pe_should_export(struct DCCSym const *__restrict sym);
PRIVATE int     pe_symsort_merge(struct DCCSym **__restrict v, size_t n, struct DCCSym **__restrict r);
PRIVATE void    pe_symsort_bubble(struct DCCSym **__restrict v, size_t n);
PRIVATE void    pe_toupper(char *p, size_t s);
PRIVATE DWORD   pe_checksum(void const *data, size_t s, DWORD sum);
LOCAL   DWORD   pe_alignfile(DWORD n);
LOCAL   target_ptr_t pe_alignsec(target_ptr_t n);
PRIVATE secty_t secty_of(struct DCCSection const *__restrict section);




LOCAL DWORD pe_alignfile(DWORD n) { return (n+(pe.pe_filalign-1)) & ~(pe.pe_filalign-1); }
LOCAL target_ptr_t pe_alignsec(target_ptr_t n) { return (n+(pe.pe_secalign-1)) & ~(pe.pe_secalign-1); }


PRIVATE secty_t
secty_of(struct DCCSection const *__restrict section) {
 struct TPPKeyword const *section_name;
 assert(section);
 section_name = section->sc_start.sy_name;
#define CHECK_NAME(s) \
   (section_name->k_size == sizeof(s)/sizeof(char)-1 && \
   !memcmp(section_name->k_name,s,sizeof(s)-sizeof(char)))
 if (!(section->sc_start.sy_flags&DCC_SYMFLAG_SEC_NOALLOC)) {
  if (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_X) return SECTY_TEXT;
  /* section with actual data inside. */
  if (CHECK_NAME(".rsrc"))  return SECTY_RSRC;
  if (CHECK_NAME(".iedat")) return SECTY_IDATA;
  if (CHECK_NAME(".pdata")) return SECTY_PDATA;
  if ((section->sc_start.sy_flags&DCC_SYMFLAG_SEC_W)/* &&
      (section->sc_dat.sd_text.tb_begin == section->sc_dat.sd_text.tb_end)*/)
       return SECTY_BSS;
  return (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_R) ? SECTY_DATA : SECTY_OTHER;
 } else if (!(section->sc_start.sy_flags&DCC_SYMFLAG_SEC_ISIMPORT)) {
  if (CHECK_NAME(".reloc")) return SECTY_RELOC;
  if (section_name->k_size >= 5 &&
     !memcmp(section_name->k_name,".stab",5*sizeof(char))
     ) return SECTY_STAB;
 }
#undef CHECK_NAME
 /* Ingore any unknown sections, unless they have the USED flag
  * set, in which case they are treated as comment sections. */
 if (section->sc_start.sy_flags&DCC_SYMFLAG_USED) return SECTY_COMMENT;
 return SECTY_UNKNOWN;
}


PRIVATE void
pe_mk_reldat(struct DCCSection *__restrict relocs) {
 struct secinfo *siter,*send;
 struct DCCRel *iter,*end;
 target_ptr_t addr,blockaddr = 0,offset = 0;
 size_t count = 0;
 if unlikely(!(linker.l_flags&DCC_LINKER_FLAG_PIC)) return;
 send = (siter = pe.pe_secv)+pe.pe_secc;
 iter = end = NULL;
 /* Search all sections for relocations. */
 for (;;) {
  if (iter < end) {
   rel_t t = iter->r_type;
   if (iter->r_sym->sy_sec == &DCCSection_Abs &&
       /* Relocation into 'SHN_ABS' */
       t == DCC_R_DISP_32) t = DCC_R_DATA_32;
   addr = iter->r_addr+siter->si_addr;
   ++iter;
   if (t != DCC_R_DATA_PTR) continue;
   if (count == 0) { /* new block */
    blockaddr = DCCSection_DAlloc(relocs,sizeof(IMAGE_BASE_RELOCATION),1,0);
    offset = addr & 0xFFFFFFFF<<12;
   }
   if ((addr -= offset) < (1<<12)) { /* one block spans 4k addresses */
    WORD *wp = (WORD *)DCCSection_TAlloc(relocs,sizeof(WORD));
    if (wp) *wp = (WORD)addr|(IMAGE_REL_BASED_HIGHLOW << 12);
    ++count;
    continue;
   }
   --iter;
  } else if (siter != send) {
   if (siter->si_sec) {
    end = (iter = siter->si_sec->sc_dat.sd_relv)+
                  siter->si_sec->sc_dat.sd_relc;
   }
   ++siter;
   continue;
  }
  if (count) { /* Finish the block */
   PIMAGE_BASE_RELOCATION hdr;
   if (count & 1) DCCSection_DAlloc(relocs,sizeof(WORD),1,0),++count;
   hdr = (PIMAGE_BASE_RELOCATION)DCCSection_GetText(relocs,blockaddr,
                                                    sizeof(IMAGE_BASE_RELOCATION));
   if (hdr) {
    hdr->VirtualAddress = (DWORD)(offset-pe.pe_imgbase);
    hdr->SizeOfBlock    = (DWORD)(count*sizeof(WORD)+sizeof(IMAGE_BASE_RELOCATION));
    count               = 0;
   }
  }
  if (iter >= end) break;
 }
}

PRIVATE void
pe_toupper(char *p, size_t s) {
 char ch,*end = p+s;
 for (; p != end; ++p) {
  ch = *p;
  if (ch >= 'a' && ch <= 'z') {
   *p = ch-('a'-'A');
  }
 }
}

PRIVATE void
pe_mk_imptab(struct DCCSection *__restrict thunk) {
 struct DCCSection *lib;
 PIMAGE_IMPORT_DESCRIPTOR hdr; size_t symbol_count = 0;
 target_ptr_t dll_name,dll_ptr,thunk_ptr,entry_ptr;
 target_siz_t total_size; DWORD thunk_base;
 thunk_base = (DWORD)(DCCSection_BASE(thunk)-pe.pe_imgbase);
 assert(thunk);
 assert(!DCCSection_ISIMPORT(thunk));
 /* Count the total number of import symbols. */
 DCCUnit_ENUMIMP(lib) symbol_count += lib->sc_symc;
 if (!symbol_count) return; /* Nothing to do here! */
 /* NOTE: +1 here is required to a padding ZERO-entry below. */
 pe.pe_imp_size = (unit.u_impc+1)*sizeof(IMAGE_IMPORT_DESCRIPTOR);
 pe.pe_iat_size = (symbol_count+unit.u_impc)*DCC_TARGET_SIZEOF_POINTER;

 /* Allocate memory for the import table & import address table. */
 total_size     = pe.pe_imp_size+2*pe.pe_iat_size;
 dll_ptr        = DCCSection_DAlloc(thunk,total_size,16,0);
 pe.pe_imp_addr = (DWORD)dll_ptr;
 /* The address table is located directly after the import table. */
 pe.pe_iat_addr = (DWORD)(dll_ptr+pe.pe_imp_size);

 /* Allocate the text. */
 if unlikely(!DCCSection_GetText(thunk,dll_ptr,total_size)) return;

 thunk_ptr = pe.pe_iat_addr;
 entry_ptr = pe.pe_iat_addr+pe.pe_iat_size;
 DCCUnit_ENUMIMP(lib) {
  assert(lib != thunk);
  /* Allocate the name for the library within the section. */
  dll_name = DCCSection_DAllocMem(thunk,
                                  lib->sc_start.sy_name->k_name,
                                 (lib->sc_start.sy_name->k_size+0)*sizeof(char),
                                 (lib->sc_start.sy_name->k_size+1)*sizeof(char),
                                  1,0);
  { /* Convert the DLL name to uppercase. */
    char *p = (char *)DCCSection_GetText(thunk,dll_name,lib->sc_start.
                                         sy_name->k_size*sizeof(char));
    if (p) pe_toupper(p,lib->sc_start.sy_name->k_size);
  }
  hdr      = (PIMAGE_IMPORT_DESCRIPTOR)(thunk->sc_dat.sd_text.tb_begin+dll_ptr);
  dll_ptr += sizeof(IMAGE_IMPORT_DESCRIPTOR);
  hdr->FirstThunk         = (DWORD)(thunk_ptr+thunk_base);
  hdr->OriginalFirstThunk = (DWORD)(entry_ptr+thunk_base);
  hdr->Name               = (DWORD)(dll_name+thunk_base);

  { struct DCCSym **biter,**bend,*sym,*next;
    bend = (biter = lib->sc_symv)+lib->sc_syma;
    for (; biter != bend; ++biter) {
     sym = *biter;
     while (sym) {
      target_ptr_t import_addr;
      struct DCCSym *iat_sym;
      struct TPPKeyword const *entry_name;
      uint8_t                 *entry_data;
      target_ptr_t             entry_addr;
      target_siz_t             entry_size;
      next = sym->sy_sec_next;

      /* Generate the import entry. */
      entry_name = sym->sy_name;
      entry_size = sizeof(WORD)+(entry_name->k_size+1)*sizeof(char);
      entry_addr = DCCSection_DAlloc(thunk,entry_size,1,0);
      entry_data = (uint8_t *)DCCSection_GetText(thunk,entry_addr,entry_size-sizeof(char));
      if unlikely(!entry_data) return;
      *(WORD *)entry_data = (WORD)sym->sy_addr;
      entry_data += 2;
      memcpy(entry_data,entry_name->k_name,entry_name->k_size*sizeof(char));
      /* Allow the entry memory to be merged. */
      entry_addr = DCCSection_DMerge(thunk,entry_addr,entry_size,1,1);

      /* Patch the library symbols to point into the thunk. */
      if likely((iat_sym = sym->sy_peind) != NULL) {
       /* 'sy_size' contains the address of the ITA wrapper (s.a.: 'pe_mk_buildita()') */
       import_addr = iat_sym->sy_size;
       DCCSym_Define(iat_sym,thunk,thunk_ptr,0,1); /* Point the ITA symbol into the thunk table. */
       DCCSym_ClrDef(sym);
       DCCSym_Define(sym,linker.l_text,import_addr,0,1); /* Point the library symbol to the ITA code. */
      } else {
       /* We shouldn't really get here because the only way we could would be if
        * the symbol was never accessed, either through PE:ITA-Indirection,
        * or as a wrapper function.
        * Yet if that was the case, this library import should have been deleted.
        * But to prevent future problems, we still kind-of break some rules here
        * because the below code assigns the symbol pointer to the PE object itself. */
       WARN(W_INVALID_PE_SYMBOL_LINKAGE,entry_name);
       DCCSym_ClrDef(sym);
       DCCSym_Define(sym,thunk,thunk_ptr,0,1);
      }
      entry_addr += thunk_base; /* Patch the entry address. */
      *(target_ptr_t *)(thunk->sc_dat.sd_text.tb_begin+thunk_ptr) =
      *(target_ptr_t *)(thunk->sc_dat.sd_text.tb_begin+entry_ptr) = entry_addr;
      thunk_ptr += DCC_TARGET_SIZEOF_POINTER;
      entry_ptr += DCC_TARGET_SIZEOF_POINTER;

      sym = next;
     }
    }
    assert(!lib->sc_symc);
  }
  /* Trailing ZERO-entry. */
  *(target_ptr_t *)(thunk->sc_dat.sd_text.tb_begin+thunk_ptr) =
  *(target_ptr_t *)(thunk->sc_dat.sd_text.tb_begin+entry_ptr) = 0;
  thunk_ptr += DCC_TARGET_SIZEOF_POINTER;
  entry_ptr += DCC_TARGET_SIZEOF_POINTER;
 }
 /* Trailing ZERO-entry. */
 hdr = (IMAGE_IMPORT_DESCRIPTOR*)(thunk->sc_dat.sd_text.tb_begin+dll_ptr);
 hdr->FirstThunk         = 0;
 hdr->OriginalFirstThunk = 0;
 hdr->Name               = 0;
}

LOCAL int
pe_symcmp(struct DCCSym const *__restrict a,
        struct DCCSym const *__restrict b) {
 assert(a);
 assert(b);
 return strcmp(a->sy_name->k_name,
               b->sy_name->k_name);
}

PRIVATE int
pe_symsort_merge(struct DCCSym **__restrict v, size_t n,
              struct DCCSym **__restrict r) {
 struct DCCSym **lb,**rb,**pl,**pr;
 size_t ln,rn; int error = 0;
 /* mergesort */
 switch (n) {
 case 0: return 1;
 case 1: r[0] = v[0]; return 1;
 case 2:
  if (pe_symcmp(v[1],v[0]) < 0) {
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
 lb = (struct DCCSym **)malloc(ln*sizeof(struct DCCSym *));
 if unlikely(!lb) goto end;
 rb = (struct DCCSym **)malloc(rn*sizeof(struct DCCSym *));
 if unlikely(!rb) goto end_lb;
 if (!pe_symsort_merge(v,ln,lb) ||
     !pe_symsort_merge(v+ln,rn,rb)) goto end_rb;
 /* Now merge the two vector together. */
 pl = lb,pr = rb,error = 1;
 while (ln && rn) {
  if (pe_symcmp(*pr,*pl) < 0)
       *r++ = *pr++,--rn;
  else *r++ = *pl++,--ln;
 }
 if (ln) {
  assert(!rn);
  memcpy(r,pl,ln*sizeof(struct DCCSym *));
 } else if (rn) {
  memcpy(r,pr,rn*sizeof(struct DCCSym *));
 }
end_rb: free(rb);
end_lb: free(lb);
end: return error;
}

PRIVATE void
pe_symsort_bubble(struct DCCSym **__restrict v, size_t n) {
 struct DCCSym *temp,**v_end,**iter,**next;
 v_end = v+n;
 while (n--) {
  iter = v;
  for (;;) {
   next = iter+1;
   if (next == v_end) break;
   if (pe_symcmp(*next,*iter) < 0) {
    temp = *iter;
    *iter = *next;
    *next = temp;
   }
   iter = next;
  }
 }
}

LOCAL int
pe_should_export(struct DCCSym const *__restrict sym) {
 if (sym->sy_flags&DCC_SYMFLAG_DLLEXPORT) {
  /* NOTE: We allow symbol forward exporting, as it
   *       is possible through use of an ITA adapter! */
  if (!sym->sy_sec /*|| DCCSection_ISIMPORT(sym->sy_sec)*/) {
   WARN(W_LINKER_PE_DLLEXPORT_NEVER_DEFINED,sym->sy_name);
   return 0;
  }
  /* Make sure not to export unknown sections, as the linker will not assign
   * a base address to them, meaning that later code trying to relocate
   * the export table reference to the section will trigger an assertion
   * when trying to relocate against a section that is missing its base address. */
  if (DCCSym_ISSECTION(sym) && secty_of(DCCSym_TOSECTION(sym)) == SECTY_UNKNOWN) {
   WARN(W_LINKER_PE_CANT_EXPORT_EMPTY_SECTION,sym->sy_name);
   return 0;
  }
do_export:
  /* Warn about weak exports on PE targets not really being weak. */
  if (sym->sy_flags&DCC_SYMFLAG_WEAK)
   WARN(W_LINKER_PE_WEAKSYM_EXPORTED_AS_NORMAL,sym->sy_name);
  return 1;
 }
 /* Extension: Consider ELF visibility. */
 if (!(linker.l_flags&DCC_LINKER_FLAG_PEDYNAMIC) ||
       DCCSym_ISFORWARD(sym)) return 0;
 /* NOTE: Never export symbols of static duration. */
 if ((sym->sy_flags&(DCC_SYMFLAG_VISIBILITY|DCC_SYMFLAG_STATIC)) !=
                     DCC_SYMFLAG_NONE) return 0;
 /* Don't implicitly export section start symbols. */
 if (DCCSym_ISSECTION(sym)) return 0;
 /* Don't dynamically export symbols import from other libraries. */
 if (!(linker.l_flags&DCC_LINKER_FLAG_PEDYNAMIC_FWD) &&
       sym->sy_sec && DCCSection_ISIMPORT(sym->sy_sec)) return 0;
 goto do_export;
}

PRIVATE void
pe_mk_collect_exports(void) {
 size_t exportc,exporta,new_exporta;
 struct DCCSym **exportv,**new_exportv,*sym;
 /* Step #1: Collect all symbols that should be exported. */
 exportc = exporta = 0,exportv = NULL;
 DCCUnit_ENUMSYM(sym) {
  if (pe_should_export(sym)) {
   if (exportc == exporta) {
    new_exporta = exporta;
    if (!new_exporta) new_exporta = 1;
    new_exporta *= 2;
    new_exportv = (struct DCCSym **)realloc(exportv,new_exporta*
                                            sizeof(struct DCCSym *));
    if unlikely(!new_exportv) goto seterr;
    exportv = new_exportv;
    exporta = new_exporta;
   }
   DCCSym_Incref(sym);
   exportv[exportc++] = sym; /* Inherit reference. */
  }
 }
 assert((exportc != 0) == (exportv != NULL));
 assert((exporta != 0) == (exportv != NULL));
 if (!exportc) return; /* Without any exports, we've got nothing to do! */
 new_exportv = (struct DCCSym **)realloc(exportv,exportc*
                                         sizeof(struct DCCSym *));
 if likely(new_exportv) exportv = new_exportv;
 /* Step #2: Sort all the symbols lexicographically by their names. */
 new_exportv = (struct DCCSym **)malloc(exportc*sizeof(struct DCCSym *));
 if unlikely(!new_exportv) goto sort_fallback;
 if (!pe_symsort_merge(exportv,exportc,new_exportv)) {
  free(new_exportv);
sort_fallback:
  /* fallback sorting algorithm */
  pe_symsort_bubble(exportv,exportc);
 } else {
  free(exportv);
  exportv = new_exportv;
 }
 assert(exportc);
 pe.pe_exportc = exportc;
 pe.pe_exportv = exportv;
 return;
seterr:
 TPPLexer_SetErr();
}




PRIVATE void
pe_mk_exptab(struct DCCSection *__restrict thunk) {
 DWORD thunk_base;
 PIMAGE_EXPORT_DIRECTORY expdir_data;
 target_ptr_t            expdir_addr;
 target_ptr_t dll_name = 0; /* Address of the DLL name. */
 target_ptr_t func_addr;    /* Vector of DWORD active as virtual-pointers to entry points. */
 target_ptr_t name_addr;    /* Vector of DWORD acting as virtual-pointers to function name strings. */
 target_ptr_t ord_addr;     /* Vector of WORD identifying function ordinals used as import hints. */
 assert(thunk);
 /* generate the export directory */
 thunk_base = (DWORD)(DCCSection_BASE(thunk)-pe.pe_imgbase);
 /* Allocate section memory for the export directory, function pointers and function IDS.
  * NOTE: Memory for the function names must be allocated dynamically later! */
 expdir_addr = DCCSection_DAlloc(thunk,sizeof(IMAGE_EXPORT_DIRECTORY),16,0);
 func_addr   = DCCSection_DAlloc(thunk,pe.pe_exportc*sizeof(DWORD),sizeof(DWORD),0);
 name_addr   = DCCSection_DAlloc(thunk,pe.pe_exportc*sizeof(DWORD),sizeof(DWORD),0);
 ord_addr    = DCCSection_DAlloc(thunk,pe.pe_exportc*sizeof(WORD),sizeof(WORD),0);
 if (linker.l_soname) {
  /* Allocate memory for the module name. */
  dll_name = DCCSection_DAllocMem(thunk,linker.l_soname->s_text,
                                 (linker.l_soname->s_size+0)*sizeof(char),
                                 (linker.l_soname->s_size+1)*sizeof(char),
                                  1,0);
  dll_name += thunk_base;
 }
 if unlikely(!DCCSection_GetText(thunk,func_addr,pe.pe_exportc*sizeof(DWORD)) ||
             !DCCSection_GetText(thunk,name_addr,pe.pe_exportc*sizeof(DWORD)) ||
             !DCCSection_GetText(thunk,ord_addr,pe.pe_exportc*sizeof(WORD))) goto end;
 expdir_data = (PIMAGE_EXPORT_DIRECTORY)DCCSection_GetText(thunk,expdir_addr,
                                                           sizeof(IMAGE_EXPORT_DIRECTORY));
 if unlikely(!expdir_data) goto end;
 expdir_data->Characteristics        = 0; /* ??? */
 expdir_data->Name                   = (DWORD)dll_name;
 expdir_data->Base                   = 1; /* ??? */
 expdir_data->NumberOfFunctions      = (DWORD)pe.pe_exportc;
 expdir_data->NumberOfNames          = (DWORD)pe.pe_exportc;
 expdir_data->AddressOfFunctions     = (DWORD)(func_addr+thunk_base);
 expdir_data->AddressOfNames         = (DWORD)(name_addr+thunk_base);
 expdir_data->AddressOfNameOrdinals  = (DWORD)(ord_addr+thunk_base);
 /* Enumerate all symbols and write them to the export list. */
 { struct DCCSym **iter,**end; WORD ord = 0;
   end = (iter = pe.pe_exportv)+pe.pe_exportc;
   for (; iter != end; ++iter) {
    DWORD name_address;
    struct TPPKeyword const *sym_name = (*iter)->sy_name;
#if 0
    printf("Exporting: '%s'\n",sym_name->k_name);
#endif
    /* Add a relocation for the export address.
     * >> That way, we don't have to deal with undefined symbols yet, and
     *    later parts of the linker are still able to define export symbols.
     * ALSO: This way, all the exported symbols will have an incremented
     *       reference count, indicating that they are actually used.
     * ALSO#2: This way, we can safely (and implicitly) export aliased symbols. */
    *(DWORD *)(thunk->sc_dat.sd_text.tb_begin+func_addr) = 0;
    DCCSection_Putrel(thunk,func_addr,DCC_R_RELATIVE,*iter);
    func_addr += sizeof(DWORD);
    /* Allocate and store memory for the function name. */
    name_address = (DWORD)DCCSection_DAllocMem(thunk,sym_name->k_name,
                                              (sym_name->k_size+0)*sizeof(char),
                                              (sym_name->k_size+1)*sizeof(char),
                                               1,0);
    *(DWORD *)(thunk->sc_dat.sd_text.tb_begin+name_addr) = name_address+thunk_base;
    name_addr += sizeof(DWORD);
    /* Fill in ordinal information. */
    *(WORD *)(thunk->sc_dat.sd_text.tb_begin+ord_addr) = ord++;
    ord_addr += sizeof(WORD);
   }
 }
 /* NOTE: This is kind-of error-prone, as due to data merging
  *       parts of the data described by the export directory
  *       might be allocated out of this range.
  *       But that can easily be prevented by explicitly allocating a '.thunk' section,
  *       or by simply not marking it as mergeable (which it shouldn't be to begin with) */
 pe.pe_exp_addr  = (DWORD)expdir_addr;
 pe.pe_exp_size  = (DWORD)(thunk->sc_dat.sd_text.tb_max-thunk->sc_dat.sd_text.tb_begin);
 pe.pe_exp_size -= (DWORD)expdir_addr;
end:;
}




/* Assign section addresses. */
PRIVATE void pe_mk_secvec(void) {
 struct DCCSection **sec_order,**iter,**end,*section;
 struct secinfo *info; secty_t section_type;
 target_ptr_t addr;
 /* Create a fallback section for thunk data. */
 DCCUnit_NewSecs(".thunk",DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 if unlikely(!unit.u_secc) {
  /* Shouldn't happen, but better be careful. */
  pe.pe_secc = 0;
  pe.pe_secv = NULL;
  return;
 }
 sec_order = (struct DCCSection **)DCC_Malloc(unit.u_secc*
                                              sizeof(struct DCCSection *),0);
 if unlikely(!sec_order) return;
 iter = sec_order;
 for (section_type = 0;
      section_type < SECTY_COUNT;
      ++section_type) {
  DCCUnit_ENUMSEC(section) {
   if (section_type == secty_of(section)) {
    assert(iter < sec_order+unit.u_secc);
    *iter++ = section;
   }
  }
 }
 pe.pe_secc = (size_t)((end = iter)-sec_order);
 pe.pe_secv = (struct secinfo *)DCC_Calloc(pe.pe_secc*
                                           sizeof(struct secinfo),0);
 if unlikely(!pe.pe_secv) pe.pe_secc = 0;
 else {
  info = pe.pe_secv;
  /* NOTE: If you're ever writing your own PE generator,
   *       the '+1' in the following line is _REQUIRED_ (for some reason)
   *    >> What it does, is to leave the first chunk of address-space
   *       of the PE binary unused, only starting at the next aligned
   *       address (The windows linker might need that space?).
   *       Anyways: This _IS_ a requirement!
   */
  addr = pe.pe_imgbase+1;
  for (iter = sec_order; iter != end; ++iter) {
   assert(info < pe.pe_secv+pe.pe_secc);
   section       = *iter;
   info->si_type = secty_of(section);
   info->si_sec  = section;

   /* Align 'addr' by the minimum section alignment required by PE. */
   addr = pe_alignsec(addr);
   if (!(section->sc_start.sy_flags&DCC_SYMFLAG_SEC_U)) {
    /* Now align it again by the requirements of the section. */
    addr = (addr+(section->sc_start.sy_align-1)) & ~(section->sc_start.sy_align-1);
   }

   if (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_FIXED) {
    info->si_addr = DCCSection_BASE(section);
   } else {
    info->si_addr = addr;
    /* Set the section base address when it isn't a fixed section. */
    DCCSection_SetBaseTo(section,addr);
   }

   /* (Ab-)use the first .data section as thunk for dll imports/exports. */
   if (info->si_type == SECTY_DATA &&
      !pe.pe_thunk) pe.pe_thunk = section;
   if (pe.pe_thunk == section) {
    pe_mk_imptab(section);
    pe_mk_exptab(section);
   }

   /* Define the relocations section. */
   if (info->si_type == SECTY_RELOC &&
      !pe.pe_reloc) pe.pe_reloc = section;
   if (pe.pe_reloc == section) pe_mk_reldat(section);

   /* Figure out the virtual and physical section size. */
   info->si_data = section->sc_dat.sd_text.tb_begin;
   {
    uint8_t *effective_end = section->sc_dat.sd_text.tb_end;
    if (effective_end > section->sc_dat.sd_text.tb_max)
        effective_end = section->sc_dat.sd_text.tb_max;
    info->si_msize = (size_t)(effective_end-
                              section->sc_dat.sd_text.tb_begin);
    info->si_vsize = (target_siz_t)(section->sc_dat.sd_text.tb_max-
                                    section->sc_dat.sd_text.tb_begin);
   }
   if (info->si_vsize) {
#if 0
    if (!(section->sc_start.sy_flags&DCC_SYMFLAG_SEC_FIXED))
          addr += info->si_vsize;
#elif 0
    addr += info->si_vsize;
#else
    addr = DCCSection_BASE(section)+info->si_vsize;
#endif
    ++info;
   }
  }
  assert(info <= pe.pe_secv+pe.pe_secc);
  pe.pe_secc = (size_t)(info-pe.pe_secv);
 }
 free(sec_order);
}


PRIVATE void
pe_mk_relrva(struct secinfo *__restrict info) {
 struct DCCRel *iter,*end; struct DCCSym *relsym;
 struct DCCSection *sec;
 uint8_t *data,*data_end,*ptr; size_t size;
 assert(info);
 sec = info->si_sec;
 assert(sec);
 data = info->si_data;
 size = info->si_msize;
 if (size < DCC_TARGET_SIZEOF_POINTER) return;
 size -= DCC_TARGET_SIZEOF_POINTER;
 end = (iter = sec->sc_dat.sd_relv)+sec->sc_dat.sd_relc;
 data_end = data+size;
 for (; iter != end; ++iter) {
  if (iter->r_type == DCC_R_RELATIVE) {
   target_ptr_t reladdr = 0;
   relsym = iter->r_sym;
   while ((DCCSym_ASSERT(relsym),relsym->sy_alias))
           reladdr += relsym->sy_addr,
           relsym = relsym->sy_alias;
   if (relsym->sy_sec) {
    ptr = data+iter->r_addr;
    if (ptr >= data && ptr <= data_end) {
     target_ptr_t sym_base_offset;
     sym_base_offset = (relsym->sy_addr+reladdr+
                        DCCSection_BASE(relsym->sy_sec))-
                        pe.pe_imgbase;
     *(target_ptr_t *)ptr += sym_base_offset;
    } else {
     /* Shouldn't happen... */
    }
   }
  }
 }
}

/* Relocate all PE sections to simplify all relocations, simply requiring the linker
 * to add a new image-base, offset from 'pe.pe_imgbase' to every absolute pointer! */
PRIVATE void pe_mk_relocate(void) {
 struct secinfo *iter,*end;
 end = (iter = pe.pe_secv)+pe.pe_secc;
 for (; iter != end; ++iter) {
  DCCSection_Reloc(iter->si_sec,1);
  pe_mk_relrva(iter);
 }
}

PRIVATE DWORD
pe_checksum(void const *data, size_t s, DWORD sum) {
 uint16_t *p = (uint16_t *)data;
 while (s) {
  if (s == 1) sum += *(uint8_t *)p,--s;
  else        sum += *p++,s -= 2;
  sum = (sum + (sum >> 16)) & 0xFFFF;
 }
 return sum;
}

PRIVATE void
pe_mk_writefile(stream_t fd) {
#define DEFDIR(id,p,s) \
 (hdr.ohdr.DataDirectory[id].VirtualAddress = (DWORD)(p),\
  hdr.ohdr.DataDirectory[id].Size           = (DWORD)(s))
 PE_HEADER hdr;
 DWORD file_offset;
 struct secinfo *iter,*end;
 hdr = pe_template;
 assert(pe.pe_entry);
 file_offset = pe_alignfile(sizeof(PE_HEADER)+pe.pe_secc*
                            sizeof(IMAGE_SECTION_HEADER));
 hdr.ohdr.SizeOfHeaders    = file_offset;
 hdr.ohdr.SectionAlignment = (DWORD)(pe.pe_secalign);
 hdr.ohdr.FileAlignment    = (DWORD)(pe.pe_filalign);
 end = (iter = pe.pe_secv)+pe.pe_secc;
 for (; iter != end; ++iter) {
  target_ptr_t addr,size;
  assert(iter->si_sec);
  assert(iter->si_sec->sc_start.sy_name);
  addr = iter->si_addr-pe.pe_imgbase;
  size = iter->si_vsize;
  assert(size >= iter->si_msize);
  switch (iter->si_type) {
  case SECTY_TEXT:
   hdr.ohdr.BaseOfCode = (DWORD)addr;
   hdr.ohdr.SizeOfCode = (DWORD)size;
   break;
#if DCC_TARGET_CPUM != DCC_CPU_X86_64
  case SECTY_DATA:
#if !(DCC_TARGET_CPUF&DCC_CPUF_X86_64)
   hdr.ohdr.BaseOfData              = (DWORD)addr;
#endif
   hdr.ohdr.SizeOfInitializedData   = (DWORD)(iter->si_msize);
   hdr.ohdr.SizeOfUninitializedData = (DWORD)(size-iter->si_msize);
   break;
#endif
  case SECTY_RELOC: DEFDIR(IMAGE_DIRECTORY_ENTRY_BASERELOC,addr,size); break;
  case SECTY_RSRC: DEFDIR(IMAGE_DIRECTORY_ENTRY_RESOURCE,addr,size); break;
  case SECTY_PDATA: DEFDIR(IMAGE_DIRECTORY_ENTRY_EXCEPTION,addr,size); break;
  default: break;
  }
  if (pe.pe_thunk == iter->si_sec) {
   if (pe.pe_imp_size) {
    DEFDIR(IMAGE_DIRECTORY_ENTRY_IMPORT,addr+pe.pe_imp_addr,pe.pe_imp_size);
    DEFDIR(IMAGE_DIRECTORY_ENTRY_IAT,   addr+pe.pe_iat_addr,pe.pe_iat_size);
   }
   if (pe.pe_exp_size) {
    DEFDIR(IMAGE_DIRECTORY_ENTRY_EXPORT,addr+pe.pe_exp_addr,pe.pe_exp_size);
   }
  }
  memset(&iter->si_hdr,0,sizeof(IMAGE_SECTION_HEADER));
  { /* Fill in the section name. */
    struct TPPKeyword const *name = iter->si_sec->sc_start.sy_name;
    size_t name_length = name->k_size;
    if (name_length >= IMAGE_SIZEOF_SHORT_NAME)
        name_length = IMAGE_SIZEOF_SHORT_NAME-1;
    memcpy(iter->si_hdr.Name,name->k_name,name_length*sizeof(char));
  }
  { /* Fill in section characteristics. */
    struct DCCSection *sec = iter->si_sec;
    symflag_t section_flags = iter->si_sec->sc_start.sy_flags;
    target_ptr_t sec_align = sec->sc_start.sy_align;
    iter->si_hdr.Characteristics = 0;
#ifndef IMAGE_SCN_TYPE_NOLOAD
#define IMAGE_SCN_TYPE_NOLOAD 0x00000002
#endif
    if (section_flags&DCC_SYMFLAG_SEC_R) iter->si_hdr.Characteristics |= IMAGE_SCN_MEM_READ;
    if (section_flags&DCC_SYMFLAG_SEC_W) iter->si_hdr.Characteristics |= IMAGE_SCN_MEM_WRITE;
    if (section_flags&DCC_SYMFLAG_SEC_X) iter->si_hdr.Characteristics |= IMAGE_SCN_MEM_EXECUTE;
    if (section_flags&DCC_SYMFLAG_SEC_S) iter->si_hdr.Characteristics |= IMAGE_SCN_MEM_SHARED;
    if (section_flags&DCC_SYMFLAG_SEC_NOALLOC) iter->si_hdr.Characteristics |= IMAGE_SCN_TYPE_NOLOAD;
    /* Stab, comment, or relocation sections can be discarded. */
    switch (iter->si_type) {
    case SECTY_STAB:
    case SECTY_RELOC:   iter->si_hdr.Characteristics |= (IMAGE_SCN_MEM_DISCARDABLE); break;
    case SECTY_COMMENT: iter->si_hdr.Characteristics |= (IMAGE_SCN_MEM_DISCARDABLE|IMAGE_SCN_LNK_INFO); break;
    case SECTY_TEXT:    iter->si_hdr.Characteristics |= (IMAGE_SCN_CNT_CODE); break;
    case SECTY_DATA:    iter->si_hdr.Characteristics |= (IMAGE_SCN_CNT_INITIALIZED_DATA); break;
    case SECTY_BSS:     iter->si_hdr.Characteristics |= (IMAGE_SCN_CNT_UNINITIALIZED_DATA); break;
    default: break;
    }

    /* Figure out section alignment. */
    if (section_flags&DCC_SYMFLAG_SEC_U) sec_align = 1;
         if (sec_align >= 8192) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_8192BYTES;
    else if (sec_align >= 4096) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_4096BYTES;
    else if (sec_align >= 2048) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_2048BYTES;
    else if (sec_align >= 1024) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_1024BYTES;
    else if (sec_align >=  512) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_512BYTES;
    else if (sec_align >=  256) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_256BYTES;
    else if (sec_align >=  128) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_128BYTES;
    else if (sec_align >=   64) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_64BYTES;
    else if (sec_align >=   32) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_32BYTES;
    else if (sec_align >=   16) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_16BYTES;
    else if (sec_align >=    8) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_8BYTES;
    else if (sec_align >=    4) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_4BYTES;
    else if (sec_align >=    2) iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_2BYTES;
    else                        iter->si_hdr.Characteristics |= IMAGE_SCN_ALIGN_1BYTES;
  }
  iter->si_hdr.VirtualAddress   = (DWORD)addr;
  iter->si_hdr.Misc.VirtualSize = (DWORD)size;
  {
   DWORD newsize = (DWORD)pe_alignsec(addr+size);
   if (hdr.ohdr.SizeOfImage < newsize)
       hdr.ohdr.SizeOfImage = newsize;
  }
  if (iter->si_msize) {
   /* Allocate pre-initialized section memory. */
   //file_offset = pe_alignfile(file_offset);
   iter->si_hdr.PointerToRawData = file_offset;
   file_offset = pe_alignfile(file_offset+iter->si_msize);
   iter->si_hdr.SizeOfRawData    = file_offset-iter->si_hdr.PointerToRawData;
  }
 }
 /* Figure out the address of the entry point. */
 { struct DCCSymAddr entryaddr;
   if (!DCCSym_LoadAddr(pe.pe_entry,&entryaddr,1) ||
        DCCSection_ISIMPORT(entryaddr.sa_sym->sy_sec)) {
    WARN(W_LINKER_MISSING_ENTRY_POINT,pe.pe_entry->sy_name->k_name);
    /* TODO: What if the text section is empty? */
    entryaddr.sa_sym = &linker.l_text->sc_start;
    entryaddr.sa_off = 0;
   }
   hdr.ohdr.AddressOfEntryPoint = (DWORD)((entryaddr.sa_sym->sy_addr+entryaddr.sa_off+
                                           DCCSection_BASE(entryaddr.sa_sym->sy_sec))-
                                           pe.pe_imgbase);
 }
 hdr.fhdr.NumberOfSections = (WORD)pe.pe_secc;
 hdr.ohdr.ImageBase        = pe.pe_imgbase;
 hdr.ohdr.Subsystem        = pe.pe_subsystem;
 if (linker.l_pe_stacksiz) hdr.ohdr.SizeOfStackReserve = linker.l_pe_stacksiz;
 if (pe.pe_type == PETYPE_DLL) hdr.fhdr.Characteristics |= IMAGE_FILE_DLL;
 if (!pe.pe_reloc) {
  /* Setup the relocs-stripped flag correctly. */
  hdr.fhdr.Characteristics |= IMAGE_FILE_RELOCS_STRIPPED;
 }

 /* Calculate the PE checksum. */
 assert(hdr.ohdr.CheckSum == 0);
 hdr.ohdr.CheckSum = pe_checksum(&hdr,sizeof(hdr),0);
 for (iter = pe.pe_secv; iter != end; ++iter) {
  hdr.ohdr.CheckSum = pe_checksum(&iter->si_hdr,sizeof(IMAGE_SECTION_HEADER),
                                  hdr.ohdr.CheckSum);
 }
 for (iter = pe.pe_secv; iter != end; ++iter) {
  if (iter->si_msize) {
   hdr.ohdr.CheckSum = pe_checksum(iter->si_data,iter->si_msize,
                                   hdr.ohdr.CheckSum);
   file_offset = iter->si_hdr.PointerToRawData+
                 iter->si_hdr.SizeOfRawData;
  }
 }
 hdr.ohdr.CheckSum += file_offset;
 s_writea(fd,&hdr,sizeof(hdr));
 for (iter = pe.pe_secv; iter != end; ++iter) {
  s_writea(fd,&iter->si_hdr,sizeof(IMAGE_SECTION_HEADER));
 }
 DCCStream_PadAddr(fd,hdr.ohdr.SizeOfHeaders);
 for (iter = pe.pe_secv; iter != end; ++iter) {
  if (iter->si_msize) {
   /* Must create padding data before _AND_ after! */
   DCCStream_PadAddr(fd,iter->si_hdr.PointerToRawData);
   s_writea(fd,iter->si_data,iter->si_msize);
   DCCStream_PadAddr(fd,iter->si_hdr.PointerToRawData+
                        iter->si_hdr.SizeOfRawData);
  }
 }
#undef DEFDIR
}

LOCAL int is_sym_defined(char const *__restrict name) {
 struct DCCSym *sym = DCCUnit_GetSyms(name);
 return sym && !DCCSym_ISFORWARD(sym);
}

/* Generate PE runtime information. */
PRIVATE void pe_mk_genrt(void) {
 char const *entry_point;
      if (linker.l_flags&DCC_LINKER_FLAG_SHARED) pe.pe_type = PETYPE_DLL;
 else if (is_sym_defined(PE_STDSYM("WinMain","@16"))) pe.pe_type = PETYPE_GUI;
 else pe.pe_type = PETYPE_EXE;
 entry_point = pe.pe_type == PETYPE_DLL ? PE_STDSYM2("__dllstart","@12") :
               pe.pe_type == PETYPE_GUI ?            "__winstart"
                                        :            "__start";
 if (linker.l_flags&DCC_LINKER_FLAG_NOUNDERSCORE) ++entry_point;
 assert(!pe.pe_entry);
 if (linker.l_entry) {
  pe.pe_entry = DCCUnit_NewSyms(linker.l_entry,DCC_SYMFLAG_NONE);
  if unlikely(!pe.pe_entry) return;
 } else {
  pe.pe_entry = DCCUnit_GetSyms(entry_point);
  if (!pe.pe_entry) {
   WARN(W_LINKER_MISSING_ENTRY_POINT,entry_point);
   pe.pe_entry = &linker.l_text->sc_start;
  }
 }
 /* Keep a reference to the entry point to prevent it
  * from being deleted when unused symbols are all removed. */
 DCCSym_Incref(pe.pe_entry);

 /* NOTE: 'pe_subsystem' can be overwritten with '-Wl,-subsystem=...' */
 pe.pe_subsystem = (pe.pe_type == PETYPE_DLL || pe.pe_type == PETYPE_EXE)
                    ? IMAGE_SUBSYSTEM_WINDOWS_CUI
                    : IMAGE_SUBSYSTEM_WINDOWS_GUI;
 if (linker.l_pe_subsys) pe.pe_subsystem = linker.l_pe_subsys;
 if (pe.pe_subsystem == IMAGE_SUBSYSTEM_NATIVE) {
  pe.pe_secalign = 0x20;
  pe.pe_filalign = 0x20;
 } else {
  pe.pe_secalign = DCC_TARGET_PAGESIZE;
  pe.pe_filalign = 0x200;
 }
 if (linker.l_secalign)    pe.pe_secalign = linker.l_secalign;
 if (linker.l_pe_filalign) pe.pe_secalign = linker.l_pe_filalign;

 if (linker.l_flags&DCC_LINKER_FLAG_PIC)
     DCCUnit_NewSecs(".reloc",DCC_SYMFLAG_SEC_NOALLOC);

 if (pe.pe_type == PETYPE_DLL) {
  /* TODO: Add some randomization here, to
   *       reduce library collisions at runtime. */
  pe.pe_imgbase = 0x10000000; /* Place dlls in high memory. */
 } else {
  pe.pe_imgbase = 0x00400000;
 }
 if (pe.pe_subsystem == IMAGE_SUBSYSTEM_EFI_APPLICATION ||
     pe.pe_subsystem == IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER ||
     pe.pe_subsystem == IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER
     ) pe.pe_imgbase = 0;
 if (linker.l_flags&DCC_LINKER_FLAG_IMGBASE)
     pe.pe_imgbase = linker.l_imgbase;
}


INTERN struct DCCSym *
pe_getitasym(struct DCCSym *__restrict basesym) {
 assert(basesym);
 assert(!basesym->sy_peind);
#if 1 /* Name the symbol for unique linkage between object files. */
 {
  size_t namelen = basesym->sy_name->k_size;
  char *buf,*mbuf; struct DCCSym *result;
  if (namelen < 64) {
   buf = (char *)alloca((namelen+1+DCC_COMPILER_STRLEN(ITA_PREFIX))*sizeof(char));
   mbuf = NULL;
  } else {
   mbuf = buf = (char *)malloc((namelen+1+DCC_COMPILER_STRLEN(ITA_PREFIX))*sizeof(char));
   if unlikely(!mbuf) { TPPLexer_SetErr(); return NULL; }
  }
  memcpy(buf,ITA_PREFIX,DCC_COMPILER_STRLEN(ITA_PREFIX)*sizeof(char));
  buf[DCC_COMPILER_STRLEN(ITA_PREFIX)+namelen] = '\0';
  memcpy(buf+DCC_COMPILER_STRLEN(ITA_PREFIX),
         basesym->sy_name->k_name,namelen*sizeof(char));
  result = DCCUnit_NewSyms(buf,DCC_SYMFLAG_HIDDEN);
  free(mbuf);
  return result;
 }
#else
 return DCCUnit_AllocSym();
#endif
}

PRIVATE void pe_mk_buildita(void) {
 struct DCCSection *lib;
 struct DCCSym *sym;
 DCCUnit_ENUMIMP(lib) {
  DCCSection_ENUMSYM(sym,lib) {
   /* Generate an ITA indirection. */
   target_ptr_t iat_addr;
   uint8_t *iat_code;
   struct DCCSym *iat_sym;
   /* Don't generate an ITA entry if it would go unused! */
   if (sym->sy_refcnt == 1 && /*!sym->sy_peind &&*/
     !(sym->sy_flags&DCC_SYMFLAG_USED)) continue;

   /* TODO: ITA wrappers only work for function symbols.
    *       For any other type of symbol, we must generate startup code
    *       that will manually fix relocations by copying the addresses
    *       of imported symbols everywhere they are needed:
    *    >> extern int lib_x;
    *    >> void foo() {
    *    >>     // 'lib_x' is referenced without PE-indirection,
    *    >>     // but doing so cannot be fixed through use of
    *    >>     // an ITA wrapper because it's not a function.
    *    >>     printf("%d\n",lib_x);
    *    >> }
    *    >> 
    *    >> int __start() {
    *    >>     foo();
    *    >> }
    *    >> 
    *    >> ...
    *    >> 
    *    >> [[lib("mylib.dll")]] extern int lib_x;
    *  FIX:
    *    >> static int __real_start() { // This symbol is unnamed and therefor not addressable
    *    >>     // Copy the address of 'lib_x' from the import table
    *    >>     // and place it into the text reference of 'foo',
    *    >>     // essentially performing the last relocation fix-ups at runtime.
    *    >>     make_writable(.text)
    *    >>     *(uintptr_t *)&foo$ref(lib_x) = &imptab(lib_x);
    *    >>     make_readonly(.text)
    *    >>
    *    >>     __asm__("jmp $__start\n");
    *    >> }
    */

#if DCC_TARGET_HASI(I_X86)
   iat_addr = DCCSection_TADDR(linker.l_text);
   iat_code = (uint8_t *)DCCSection_TAlloc(linker.l_text,6);
   if unlikely(!iat_code) return;
   /* jmp *$(DCCSection_BASE(thunk) + thunk_ptr) */
   *iat_code++           = 0xff;
   *iat_code++           = 0x25;
   *(uint32_t *)iat_code = 0;
#else
#error FIXME
#endif
   if ((iat_sym = sym->sy_peind) == NULL) {
    /* The symbol must be part of the unnamed symbol list! */
    if unlikely((iat_sym = pe_getitasym(sym)) == NULL) return;
    DCCSym_Incref(iat_sym);
    sym->sy_peind = iat_sym; /* Inherit reference. */
   }
   /* - Here, 'iat_addr' is the address of the jump-instruction that
    *   performs the indirection required for dereferencing the ITA
    *   symbol.
    * - 'iat_sym' will later be defined to point into the import-address-table,
    *   meaning that code will eventually call a library function like this:
    * @ASM
    * >> .text
    * >> _start:
    * >>     pushl $0
    * >>     pushl $0
    * >>     pushl $message
    * >>     pushl $0
    * >>     # 'MessageBoxA' can be used like any regular, old symbol
    * >>     call MessageBoxA
    * >>     addl $16, %esp
    * >>     ret
    * >> 
    * >> .section ".thunk"
    * >> iat_MessageBoxA: # This is 'iat_sym'
    * >>     .dword 0
    * >>
    * >> .text
    * >> MessageBoxA: # This is 'sym'
    * >>     # Call the external function
    * >>     jmp *$iat_MessageBoxA
    * >> 
    * >> .section ".string"
    * >> message: .string "Messagebox Text!"
    */
   /* Hacky temporary storage for what will eventually become 'lib_sym->sy_addr'
    * NOTE: Using 'sy_size' here is OK, because the field is unused for IMPORT symbols. */
   iat_sym->sy_size = iat_addr;
   DCCSection_Putrel(linker.l_text,iat_addr+2,DCC_R_DATA_PTR,iat_sym);

   /* These calls must be done later to keep the library symbols inside! */
   // DCCSym_ClrDef(sym);
   // DCCSym_Define(sym,linker.l_text,iat_addr,0);
  }
 }
}

PUBLIC void
DCCLinker_Make(stream_t fd) {
 memset(&pe,0,sizeof(pe));

 if (!linker.l_text) {
  if (unit.u_text)
       linker.l_text = unit.u_text;
  else linker.l_text = DCCUnit_NewSecs(".text",DCC_SYMFLAG_SEC_X|DCC_SYMFLAG_SEC_R);
 }

 /* Determine PE settings, as well as the entry point. */
 pe_mk_genrt();

 /* Collect all symbols that should be exported from the binary. */
 pe_mk_collect_exports();
 if (!OK) goto end;

 /* Everything that is externally visible has been collected,
  * meaning that everything that's still unused can be removed. */
 DCCUnit_ClearUnused();
 DCCUnit_ClearUnusedLibs();
 DCCUnit_CollapseSections();
 DCCUnit_ResolveDisp();
 
 /* Build ITA wrapper functions for all symbols still in use. */
 pe_mk_buildita();
 if (!OK) goto end;

 /* Build the vector of used sections.
  * NOTE: During this process, relocations and
  *       import/export tables are generated as well. */
 pe_mk_secvec();
 if (!OK) goto end;

 /* Execute relocations. */
 pe_mk_relocate();
 if (!OK) goto end;

 /* Write everything to file. */
 pe_mk_writefile(fd);
end:
 {
  struct DCCSym **iter,**end;
  end = (iter = pe.pe_exportv)+pe.pe_exportc;
  for (; iter != end; ++iter) DCCSym_Decref(*iter);
  free(pe.pe_exportv);
 }
 DCCSym_Decref(pe.pe_entry);
 free(pe.pe_secv);
}

DCC_DECL_END

#endif /* !GUARD_DCC_LINKER_PE_C_INL */
