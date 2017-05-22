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
#ifndef GUARD_DCC_UNIT_IMPORT_PE_STATIC_C_INL
#define GUARD_DCC_UNIT_IMPORT_PE_STATIC_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>

#if DCC_LIBFORMAT_PE_STATIC
#include <dcc/unit.h>
#include <dcc/stream.h>

#include "x86_util.h"
#include "linker-pe.h"

#include <stdio.h>

DCC_DECL_BEGIN

#if !DCC_CONFIG_NEED_X86_INSTRLEN
#error "Statically linking PE binaries requires the x86_instrlen() utility"
#endif /* !DCC_CONFIG_NEED_X86_INSTRLEN */

/* Returns the compile-time address of a given image_base-relative address 'rva'.
 * When non-NULL, '*maxsize' is filled with the max amount of valid bytes at that address.
 * @param: minsize: The min amount of bytes the caller is intending to access.
 *                 (Required for accessing uninitialized, aka. '.bss'-data)
 * @return: NULL: Invalid RVA pointer ('*maxsize' is set to ZERO(0))
 * @return: *   : The compile-time memory address described by 'rva' */
PRIVATE void *get_vaddr(target_ptr_t rva, size_t minsize, size_t *maxsize);


PRIVATE int load_pe_sections(PIMAGE_SECTION_HEADER secv,
                             size_t                secc,
                             stream_t              fd) {
 PIMAGE_SECTION_HEADER iter,end;
 struct DCCSection *section;
 struct TPPKeyword *section_name;
 assert(secc);
 end = (iter = secv)+secc;
 for (; iter != end; ++iter) {
  ptrdiff_t    readsiz;
  uint8_t     *sec_data;
  target_ptr_t sec_addr;
  symflag_t    secflags = DCC_SYMFLAG_USED|DCC_SYMFLAG_SEC_FIXED;
  target_siz_t sec_align;
  if (iter->Characteristics&IMAGE_SCN_MEM_SHARED)  secflags |= DCC_SYMFLAG_SEC_S;
  if (iter->Characteristics&IMAGE_SCN_MEM_EXECUTE) secflags |= DCC_SYMFLAG_SEC_X;
  if (iter->Characteristics&IMAGE_SCN_MEM_READ)    secflags |= DCC_SYMFLAG_SEC_R;
  if (iter->Characteristics&IMAGE_SCN_MEM_WRITE)   secflags |= DCC_SYMFLAG_SEC_W;
  switch (iter->Characteristics&IMAGE_SCN_ALIGN_MASK) {
  default                       : sec_align = 1; break;
  case IMAGE_SCN_ALIGN_2BYTES   : sec_align = 2; break;
  case IMAGE_SCN_ALIGN_4BYTES   : sec_align = 4; break;
  case IMAGE_SCN_ALIGN_8BYTES   : sec_align = 8; break;
  case 0                        :
  case IMAGE_SCN_ALIGN_16BYTES  : sec_align = 16; break;
  case IMAGE_SCN_ALIGN_32BYTES  : sec_align = 32; break;
  case IMAGE_SCN_ALIGN_64BYTES  : sec_align = 64; break;
  case IMAGE_SCN_ALIGN_128BYTES : sec_align = 128; break;
  case IMAGE_SCN_ALIGN_256BYTES : sec_align = 256; break;
  case IMAGE_SCN_ALIGN_512BYTES : sec_align = 512; break;
  case IMAGE_SCN_ALIGN_1024BYTES: sec_align = 1024; break;
  case IMAGE_SCN_ALIGN_2048BYTES: sec_align = 2048; break;
  case IMAGE_SCN_ALIGN_4096BYTES: sec_align = 4096; break;
  case IMAGE_SCN_ALIGN_8192BYTES: sec_align = 8192; break;
  }
  sec_align = 1;
  section_name = TPPLexer_LookupKeyword((char *)iter->Name,strnlen(
                                        (char *)iter->Name,IMAGE_SIZEOF_SHORT_NAME),
                                         1);
  if unlikely(!section_name) return 0;
  section = DCCUnit_NewSec(section_name,secflags);
  if unlikely(!section) return 0;
  /* Shouldn't happen, but lets be careful. */
  if (iter->Misc.VirtualSize < iter->SizeOfRawData)
      iter->Misc.VirtualSize = iter->SizeOfRawData;
  /* Allocate section memory. */
  sec_addr = DCCSection_DAlloc(section,iter->Misc.VirtualSize,sec_align,0);
  sec_data = (uint8_t *)DCCSection_GetText(section,sec_addr,iter->SizeOfRawData);
  if unlikely(!sec_data) return 0;
  s_seek(fd,iter->PointerToRawData,SEEK_SET);
  readsiz = s_read(fd,sec_data,iter->SizeOfRawData);
  if (readsiz < 0) return 0;
  /* Fill everything that shouldn't be read with ZEROes (shouldn't happen...) */
  memset(sec_data+readsiz,0,iter->SizeOfRawData-readsiz);
  /* NOTE: 'sc_base' will (hopefully) be fixed during relocations. */
  section->sc_base = section->sc_merge = iter->VirtualAddress;
 }
 return 1;
}


PRIVATE void *get_vaddr(target_ptr_t rva, size_t minsize, size_t *maxsize) {
 struct DCCSection *sec;
 target_ptr_t sec_offset;
 size_t sec_size;
 void *result;
 DCCUnit_ENUMSEC(sec) {
  if (rva < sec->sc_merge) continue;
  sec_offset = rva-sec->sc_merge;
  sec_size   = DCCSection_VSIZE(sec);
  if (sec_offset < sec_size) {
   /* Found the associated section! */
   if (sec_offset+minsize > sec_size) result = NULL;
   else result = DCCSection_GetText(sec,sec_offset,minsize);
   if (maxsize) {
    *maxsize = result ? (size_t)(sec->sc_text.tb_end-
                                (uint8_t *)result)
                      : (size_t)0;
   }
   return result;
  }
 }
 return NULL;
}

INTERN int DCCUNIT_IMPORTCALL
DCCUnit_StaLoadPE(struct DCCLibDef *__restrict def,
                  char const *__restrict file, stream_t fd) {
 LONG header_offset;
 size_t section_count,hdr_size;
 ptrdiff_t read_error;
 NT_HEADER hdr;
 int result = 0;
 assert(def);
 assert(def->ld_flags&DCC_LIBDEF_FLAG_STATIC);
 assert(file);
 assert(!unit.u_symc);
 assert(!unit.u_nsymc);
 assert(!unit.u_secc);
 assert(!unit.u_impc);
 header_offset = (size_t)pe_readhdr(fd);
 if (!header_offset) goto end;
#define HAS_FIELD(f) ((size_t)((&((NT_HEADER *)0)->f)+1) <= hdr_size)
#define HAS_DIR(id)  \
 (HAS_FIELD(ohdr.DataDirectory[id]) && \
 (id) < hdr.ohdr.NumberOfRvaAndSizes && \
  hdr.ohdr.DataDirectory[id].Size)
#define GET_DIR(id)  hdr.ohdr.DataDirectory[id]
 read_error = s_read(fd,&hdr,sizeof(hdr));
 if (read_error < 0) goto end;
 hdr_size = (size_t)read_error;
 if (!HAS_FIELD(fhdr)) goto end;
 if (hdr.ntsg != IMAGE_NT_SIGNATURE) goto end;
 if (hdr.fhdr.Machine != DCC_PE_TARGET_MACHINE) {
  WARN(W_LIB_PE_INVALID_MACHINE,
      (unsigned int)DCC_PE_TARGET_MACHINE,
      (unsigned int)hdr.fhdr.Machine);
  goto end;
 }
 if (HAS_FIELD(fhdr.SizeOfOptionalHeader)) {
  size_t newsize = offsetof(NT_HEADER,ohdr)+
                   hdr.fhdr.SizeOfOptionalHeader;
  if (newsize < hdr_size) hdr_size = newsize;
 }
 /* Used later in a context where ZERO(0) is equal to no-op. */
 if (!HAS_FIELD(ohdr.ImageBase)) hdr.ohdr.ImageBase = 0;

 section_count = HAS_FIELD(fhdr.NumberOfSections) ? hdr.fhdr.NumberOfSections : 0;
 header_offset += hdr_size;
 {
  PIMAGE_SECTION_HEADER sections;
  sections = (PIMAGE_SECTION_HEADER)DCC_Malloc(section_count*
                                               sizeof(IMAGE_SECTION_HEADER),0);
  if unlikely(!sections) goto end;
  s_seek(fd,header_offset,SEEK_SET);
  read_error = s_read(fd,sections,section_count*
                      sizeof(IMAGE_SECTION_HEADER));
  if (read_error < 0) goto end;
  section_count = read_error/sizeof(IMAGE_SECTION_HEADER);
  result = section_count && load_pe_sections(sections,section_count,fd);
  free(sections);
  if (!result) goto end;
  def->ld_dynlib = NULL; /* This is a static loader. */
 }

 /* All the sections have been loaded. - Time to collect all the symbols we can find! */
 if (HAS_DIR(IMAGE_DIRECTORY_ENTRY_IMPORT)) {
  /* TODO: Load imports */
 }
 if (HAS_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT)) {
  PIMAGE_EXPORT_DIRECTORY export_dir;
  size_t export_dir_size,numsym;
  size_t real_numsym,symid;
  DWORD *funvec;   /* void **funvec; */
  DWORD *namevec;  /* char **namevec; */
  /* Load the export directory. */
  export_dir = (PIMAGE_EXPORT_DIRECTORY)get_vaddr(GET_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT).VirtualAddress,
                                                  GET_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT).Size,
                                                 &export_dir_size);
  if (!export_dir) export_dir = (PIMAGE_EXPORT_DIRECTORY)get_vaddr(GET_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT).VirtualAddress,
                                                                   sizeof(IMAGE_EXPORT_DIRECTORY),&export_dir_size);
  if (!export_dir || export_dir_size < sizeof(IMAGE_EXPORT_DIRECTORY)) goto noexpdir;
  /* Safely load all symbol vectors. */
  numsym = export_dir->NumberOfFunctions;
  if (numsym > export_dir->NumberOfNames)
      numsym = export_dir->NumberOfNames;
  numsym *= sizeof(DWORD);
               funvec = (DWORD *)get_vaddr(export_dir->AddressOfFunctions,numsym,&real_numsym);
  if (!funvec) funvec = (DWORD *)get_vaddr(export_dir->AddressOfFunctions,0,&real_numsym);
  if (!funvec) goto noexpdir;
  if (real_numsym < numsym) numsym = real_numsym;
                namevec = (DWORD *)get_vaddr(export_dir->AddressOfNames,numsym,&real_numsym);
  if (!namevec) namevec = (DWORD *)get_vaddr(export_dir->AddressOfNames,0,&real_numsym);
  if (!namevec) goto noexpdir;
  if (real_numsym < numsym) numsym = real_numsym;
  numsym /= sizeof(DWORD);

  /* Iterate all symbols. */
  for (symid = 0; symid < numsym; ++symid) {
   struct DCCSym *newsym; struct TPPKeyword *symkwd;
   struct DCCSection *symsec; size_t symsize; char *symname;
   target_ptr_t symaddr = (target_ptr_t)funvec[symid];
   symname = (char *)get_vaddr(namevec[symid],0,&symsize);
   if unlikely(!symname) { WARN(W_STA_PE_CORRUPT_SYMNAME,namevec[symid],symaddr); continue; } /* Skip corrupt symbols. */
   if unlikely(!symsize || !*symname) continue; /* Skip unnamed symbols? */
   symkwd = TPPLexer_LookupKeyword(symname,strnlen(symname,symsize),1);
   if unlikely(!symkwd) break;
   symsec = dcc_getsec(symaddr);
   if unlikely(!symsec) {
    WARN(W_STA_PE_UNMAPPED_ADDR,symaddr,symkwd);
    symsec = &DCCSection_Abs;
   }
#ifdef DCC_SYMFLAG_DLLEXPORT
   newsym = DCCUnit_NewSym(symkwd,DCCLibDef_EXPFLAGS(def,DCC_SYMFLAG_DLLEXPORT));
#else
   newsym = DCCUnit_NewSym(symkwd,DCCLibDef_EXPFLAGS(def,DCC_SYMFLAG_NONE));
#endif
   if unlikely(!newsym) break;
   /* Adjust the symbol address. */
   if (!DCCSection_ISIMPORT(symsec))
        symaddr -= symsec->sc_base;
   DCCSym_Define(newsym,symsec,symaddr,0);
  }
  /* TODO: After loading relocations, check back and
   *       remove the export directory if is is unused. */
 }
noexpdir:

 /* Load all relocations.
  * NOTE: The ELF representation is accomplished by
  *       creation section-base-relative relocations
  *       using 'DCC_R_DATA_PTR' to add the base
  *       address of a specific section at runtime. */
 if (HAS_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC)) {
  uint8_t *reldat,*reldat_base; size_t relsiz;
  PIMAGE_BASE_RELOCATION relhdr;
  reldat = (uint8_t *)get_vaddr(GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress,
                                GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).Size,
                               &relsiz);
  if (!reldat) reldat = (uint8_t *)get_vaddr(GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress,
                                             sizeof(IMAGE_BASE_RELOCATION),&relsiz);
  if (!reldat) goto norel;
  reldat_base = reldat;
  while (relsiz >= sizeof(IMAGE_BASE_RELOCATION)) {
   DWORD    effective_blocksize; target_ptr_t reladdr,reladj;
   WORD    *relwordv,*relword_end; size_t relwordc;
   uint8_t *region_base; size_t region_size;
   struct DCCSection *region_sec;
   relhdr               = (PIMAGE_BASE_RELOCATION)reldat;
   reldat              += sizeof(IMAGE_BASE_RELOCATION);
   relsiz              -= sizeof(IMAGE_BASE_RELOCATION);
   effective_blocksize  = relhdr->SizeOfBlock;
   if (effective_blocksize < sizeof(IMAGE_BASE_RELOCATION))
   { effective_blocksize = 0; goto next_block; }
   effective_blocksize -= sizeof(IMAGE_BASE_RELOCATION);
   if (effective_blocksize > relsiz) /* Shouldn't happen... */
       effective_blocksize = relsiz;
   relwordc    = effective_blocksize/sizeof(WORD);
   if unlikely(!relwordc) goto next_block;
   relwordv    = (WORD *)reldat;
   reladdr     = (target_ptr_t)relhdr->VirtualAddress;
   relword_end = relwordv+relwordc;
                     region_base = (uint8_t *)get_vaddr(reladdr,0x1000,&region_size);
   if (!region_base) region_base = (uint8_t *)get_vaddr(reladdr,0,&region_size);
   if (!region_base || (region_sec  = dcc_getsec(reladdr)) == NULL) {
    WARN(W_STA_PE_UNMAPPED_RELOC,(target_ptr_t)reladdr,(target_ptr_t)(
         GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress+
        (reldat-sizeof(IMAGE_BASE_RELOCATION))-reldat_base),
        (size_t)relwordc);
    continue;
   }
   assert(reladdr >= region_sec->sc_base);
   /* Figure out the adjustment relocation-region addresses and section addresses. */
   reladj = (reladdr-region_sec->sc_base);
   assert(reladj <= DCCSection_VSIZE(region_sec));
   for (; relwordv != relword_end; ++relwordv) {
    struct DCCRel rel; uint8_t *reldata;
    WORD type,offset,relword; int relwarn;
    relword    = *relwordv;
    type       = relword >> 12;
    offset     = relword & ((1 << 12)-1);
    if (offset >= region_size) {
oob_reloc:
     relwarn = W_STA_PE_OUTOFBOUNDS_RELOC;
ill_reloc:
     WARN(relwarn,
         (unsigned int)*relwordv,reladdr,
          GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress+
        ((uintptr_t)relwordv-(uintptr_t)reldat_base),
          relhdr->VirtualAddress,relhdr->VirtualAddress+region_size,
          GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress+
        ((uintptr_t)relhdr-(uintptr_t)reldat_base));
     continue;
    }
    rel.r_addr = offset+reladj;
    reldata    = region_base+offset;
    switch (type) {
    case IMAGE_REL_BASED_ABSOLUTE: continue; /* Ignore this relocation. */
    {
     uint32_t relvalue;
     struct DCCSection *addrtarget;
    case IMAGE_REL_BASED_HIGHLOW:
     /* i386 ELF call this 'R_386_RELATIVE', but we implement it as 'DCC_R_DATA_PTR' */
     if ((size_t)offset+4 >= region_size) goto oob_reloc;
     /* Adjust the relocation data from image-relative to section-relative. */
     relvalue   = *(uint32_t *)reldata;

     relvalue  -= hdr.ohdr.ImageBase;
     addrtarget = dcc_getsec((target_ptr_t)relvalue);
     if (!addrtarget) {
      WARN(W_STA_PE_UNKNOWN_RELOCATION_TARGET,
          (target_ptr_t)relvalue,
          (unsigned int)*relwordv,reladdr,
           GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress+
         ((uintptr_t)relwordv-(uintptr_t)reldat_base),
           relhdr->VirtualAddress,relhdr->VirtualAddress+region_size,
           GET_DIR(IMAGE_DIRECTORY_ENTRY_BASERELOC).VirtualAddress+
         ((uintptr_t)relhdr-(uintptr_t)reldat_base));
      continue;
     }
     relvalue            -= addrtarget->sc_base;
     rel.r_sym            = &addrtarget->sc_start;
     *(uint32_t *)reldata = relvalue;
     rel.r_type           = DCC_R_DATA_PTR;
    } break;
    default:
     relwarn = W_STA_PE_UNKNOWN_RELOC;
     goto ill_reloc;
    }
    DCCSection_Putrelo(region_sec,&rel);
   }
next_block:
   reldat  += effective_blocksize;
   relsiz  -= effective_blocksize;
  }
  /* What about disp-relocations?
   * Well: The only way any of this can work is if we knew about
   *       those as well, as otherwise we'd be unable to append
   *       new data at the end of sections loaded here.
   * >> The only way this could be done that I can think about, is
   *    to disassemble code from all executable code segments
   *    in search of all relN instructions (such as 'call' or 'jmp'). */
  { struct DCCSection *sec;
    DCCUnit_ENUMSEC(sec) {
     if (sec->sc_start.sy_flags&DCC_SYMFLAG_SEC_X)
         x86_mkrel_textdisp(sec,hdr.ohdr.ImageBase);
    }
  }
  /* With all relocations loaded, we can now
   * delete the section base addresses! */
  { struct DCCSection *sec;
    DCCUnit_ENUMSEC(sec) {
     sec->sc_start.sy_flags &= ~(DCC_SYMFLAG_SEC_FIXED);
     sec->sc_base            = 0;
    }
  }
 }
norel:

end:
 return result;
}

DCC_DECL_END
#endif /* DCC_LIBFORMAT_PE_STATIC */

#endif /* !GUARD_DCC_UNIT_IMPORT_PE_STATIC_C_INL */
