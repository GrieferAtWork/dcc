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
#ifndef GUARD_DCC_UNIT_IMPORT_PE_DYNAMIC_C_INL
#define GUARD_DCC_UNIT_IMPORT_PE_DYNAMIC_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>

#if DCC_LIBFORMAT_PE_DYNAMIC || DCC_LIBFORMAT_PE_STATIC
#include <dcc/unit.h>
#include <dcc/stream.h>

#include <string.h>
#include <stdlib.h>

#include "linker-pe.h"

DCC_DECL_BEGIN

/* Load hint ids when loading dlls, thus allowing
 * them to propagate and generate faster binaries.
 * >> The downside to this is, that hint IDS may
 *    change the next time windows does an update,
 *    so any performance boost gained by this will
 *    become redundant at one point.
 * WARNING: As far as privacy in concerned, someone really
 *          smart might be able to deduce your exact
 *          version of windows if you give him a binary
 *          linked against something like 'ntdll.dll',
 *          with this option enabled.
 */
#ifndef PE_PROCESS_HINTS
#define PE_PROCESS_HINTS 1
#endif

/* Read a PE header and navigate the file pointer to the 32-bit header. */
INTERN LONG pe_readhdr(stream_t s, soff_t start) {
 IMAGE_DOS_HEADER hdr;
 if (!s_reada(s,&hdr,sizeof(IMAGE_DOS_HEADER))) return 0;
 if (hdr.e_magic != IMAGE_DOS_SIGNATURE) return 0;
 s_seek(s,start+hdr.e_lfanew,SEEK_SET);
 return hdr.e_lfanew;
}

INTERN char *
pe_readzstring(stream_t fd, size_t *s) {
 char *result,*newresult,*resend,*readpos;
 size_t cursize,bufsize;
 ptrdiff_t read_error;
 cursize = 0,bufsize = 128;
 result = (char *)malloc(bufsize);
 while (result) {
  readpos = result+cursize;
  read_error = s_read(fd,readpos,bufsize-cursize);
  if (read_error <= 0) break;
  resend = (char *)memchr(readpos,'\0',(size_t)read_error);
  if (resend) { cursize += (size_t)(resend-readpos); break; }
  cursize += read_error;
  if (cursize > bufsize) cursize = bufsize;
  if (cursize == bufsize) {
   bufsize *= 2;
   newresult = (char *)realloc(result,bufsize);
   if unlikely(!newresult) goto ret_null;
   result = newresult;
  }
 }
 if (!cursize) goto ret_null;
 if ((cursize+sizeof(char)) != bufsize) {
  newresult = (char *)realloc(result,cursize+sizeof(char));
  if unlikely(newresult) {
   result = newresult;
   bufsize = cursize;
  }
  assert(bufsize);
  if (bufsize < cursize) cursize = bufsize;
  assert(cursize);
  result[cursize/sizeof(char)] = '\0';
 }
 *s = cursize/sizeof(char);
 return result;
ret_null:
 free(result);
 return NULL;
}

#if DCC_LIBFORMAT_PE_DYNAMIC
INTERN int DCCUNIT_IMPORTCALL
DCCUnit_DynLoadPE(struct DCCLibDef *__restrict def,
                  char const *__restrict file,
                  stream_t fd, soff_t start) {
 NT_HEADER hdr;
 size_t hdr_size;
 size_t section_header_offset;
 size_t section_header_count;
 ptrdiff_t read_error;
 uintptr_t export_file_base;
 struct DCCSection *ressec = NULL;
 PIMAGE_SECTION_HEADER sections = NULL;
 PIMAGE_SECTION_HEADER sec,sec_end;
 assert(def);
 assert(file);
 section_header_offset = (size_t)pe_readhdr(fd,start);
 if (!section_header_offset) goto end;
#define HAS_FIELD(f) ((size_t)((&((NT_HEADER *)0)->f)+1) <= hdr_size)
#define HAS_DIR(id)  \
 (HAS_FIELD(ohdr.DataDirectory[id]) && \
 (id) < hdr.ohdr.NumberOfRvaAndSizes && \
  hdr.ohdr.DataDirectory[id].Size)
#define GET_DIR(id)  hdr.ohdr.DataDirectory[id]
 read_error = s_read(fd,&hdr,sizeof(hdr));
#ifdef __DCC_VERSION__
 printf("#1\n");
#endif
 if (read_error < 0) goto end;
#ifdef __DCC_VERSION__
 printf("#2\n");
#endif
 hdr_size = (size_t)read_error;
 if (!HAS_FIELD(fhdr)) goto end;
#ifdef __DCC_VERSION__
 printf("#3\n");
#endif
 if (hdr.ntsg != IMAGE_NT_SIGNATURE) goto end;
#ifdef __DCC_VERSION__
 printf("#4\n");
#endif
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
 section_header_offset += hdr_size;
 /* Do some optional checks on the headers.
  * But besides that, we know that the library is OK to use at this point! */
 /* Make sure that the binary has relocations, because
  * otherwise it would be impossible to safely link against it. */
#ifdef __DCC_VERSION__
 printf("#5\n");
#endif
 if (hdr.fhdr.Characteristics&IMAGE_FILE_RELOCS_STRIPPED) { WARN(W_LIB_PE_NO_RELOCATIONS,file); if (!OK) goto end; }
 if (!(hdr.fhdr.Characteristics&IMAGE_FILE_DLL)) { WARN(W_LIB_PE_NO_DLL,file); if (!OK) goto end; }
 if (HAS_FIELD(ohdr.Magic) && hdr.ohdr.Magic !=
     DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER_MAGIC) { WARN(W_LIB_PE_INVMAGIC,file); if (!OK) goto end; }
 if (!HAS_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT)) {
no_export_table:
  WARN(W_LIB_PE_NO_EXPORT_TABLE,file);
  goto end;
 }
 section_header_count = HAS_FIELD(fhdr.NumberOfSections)
                      ? hdr.fhdr.NumberOfSections : 0;
 if (!section_header_count) {
  WARN(W_LIB_PE_NO_SECTIONS,file);
  goto end;
 }
#define exptab_addr  GET_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT).VirtualAddress
#define exptab_size  GET_DIR(IMAGE_DIRECTORY_ENTRY_EXPORT).Size
 /* At this point, we know the virtual address of the export table.
  * To get its actual address, we must now iterate all sections in order
  * to find the associated section and know here it exists within the file. */
 sections = (PIMAGE_SECTION_HEADER)DCC_Malloc(section_header_count*
                                              sizeof(IMAGE_SECTION_HEADER),0);
#ifdef __DCC_VERSION__
 printf("#5.5\n");
#endif
 if unlikely(!sections) goto end;
 s_seek(fd,section_header_offset,SEEK_SET);
 read_error = s_read(fd,sections,section_header_count*
                     sizeof(IMAGE_SECTION_HEADER));
#ifdef __DCC_VERSION__
 printf("#6\n");
#endif
 if (read_error < 0) goto end;
#ifdef __DCC_VERSION__
 printf("#7\n");
#endif
 section_header_count = read_error/sizeof(IMAGE_SECTION_HEADER);
 if (!section_header_count) { WARN(W_LIB_PE_NO_SECTIONS,file); goto end; }
 sec_end = (sec = sections)+section_header_count;
 for (; sec != sec_end; ++sec) {
  if (exptab_addr >= sec->VirtualAddress &&
      exptab_addr < sec->VirtualAddress+sec->SizeOfRawData
      ) goto found_section;
 }
 WARN(W_LIB_PE_NO_SECTION_MAPPING,file,exptab_addr);
 goto end;
found_section:
#define exptab_max section_header_count
 exptab_max = (sec->VirtualAddress+sec->SizeOfRawData)-exptab_addr;
 if (exptab_max > exptab_size) exptab_size = exptab_max;
 /* Different subtracted from a pointer to convert a virtual
  * address in the export table to a SEEK_SET-file offset. */
 export_file_base = sec->VirtualAddress-sec->PointerToRawData;
 DCC_Free(sections),sections = NULL;
 if (exptab_size <= sizeof(IMAGE_EXPORT_DIRECTORY)) goto no_export_table;
 {
  IMAGE_EXPORT_DIRECTORY export_dir;
  DWORD i,namepptr,nameptr;
#if PE_PROCESS_HINTS
  WORD hint; DWORD hintptr;
#endif /* PE_PROCESS_HINTS */
  s_seek(fd,start+(exptab_addr-export_file_base),SEEK_SET);
  read_error = s_read(fd,&export_dir,sizeof(IMAGE_EXPORT_DIRECTORY));
  if (read_error < 0) goto end;
  exptab_size = (DWORD)read_error;
  if (!export_dir.NumberOfNames) { WARN(W_LIB_PE_EMPTY_EXPORT_TABLE,file); goto end; }
  /* Here we go! */
  {
   char const *used_libname = def->ld_name;
   size_t      used_libsize = def->ld_size;
   char       *mall_libname = NULL;
   struct TPPKeyword *libkwd;
   if (export_dir.Name && !(def->ld_flags&DCC_LIBDEF_FLAG_USERNAME)) {
    size_t z_libsize; /* Use the name specified inside the library. */
    s_seek(fd,export_dir.Name-export_file_base,SEEK_SET);
    mall_libname = pe_readzstring(fd,&z_libsize);
    if (mall_libname) used_libname = mall_libname,
                      used_libsize = z_libsize;
   }
   libkwd = TPPLexer_LookupKeyword(used_libname,used_libsize,1);
   free(mall_libname);
   if unlikely(!libkwd) goto end;
   ressec = DCCUnit_NewSec(libkwd,DCC_SYMFLAG_SEC_ISIMPORT);
   if unlikely(!ressec) goto end;
  }
  def->ld_dynlib = ressec;

  namepptr = export_dir.AddressOfNames-export_file_base;
#if PE_PROCESS_HINTS
  hintptr  = export_dir.AddressOfNameOrdinals-export_file_base;
#endif /* PE_PROCESS_HINTS */
  for (i = 0; i < export_dir.NumberOfNames; ++i) {
   char *symname; size_t symsize;
   if (!OK) break;
   s_seek(fd,start+namepptr,SEEK_SET),namepptr += sizeof(nameptr);
   if (!s_reada(fd,&nameptr,sizeof(nameptr))) continue;
#if PE_PROCESS_HINTS
   s_seek(fd,start+hintptr,SEEK_SET),hintptr += sizeof(hint);
   if (!s_reada(fd,&hint,sizeof(hint))) continue;
#endif /* PE_PROCESS_HINTS */
   s_seek(fd,start+(nameptr-export_file_base),SEEK_SET);
   symname = pe_readzstring(fd,&symsize);
   if (symname && symsize) {
    struct DCCSym *sym;
    struct TPPKeyword *symname_kwd;
    symname_kwd = TPPLexer_LookupKeyword(symname,symsize,1);
    if (symname_kwd) {
     //printf("DEFINE: '%s' -> '%s' (%d)\n",file,symname,hint);
#ifdef DCC_SYMFLAG_DLLIMPORT
     sym = DCCUnit_NewSym(symname_kwd,DCCLibDef_IMPFLAGS(def,DCC_SYMFLAG_DLLIMPORT));
#else
     sym = DCCUnit_NewSym(symname_kwd,DCCLibDef_IMPFLAGS(def,DCC_SYMFLAG_NONE));
#endif
     if (sym && sym->sy_sec != ressec) { /* Ignore double exports? */
#if PE_PROCESS_HINTS
      DCCSym_Import(sym,ressec,(target_ptr_t)hint);
#else /* PE_PROCESS_HINTS */
      DCCSym_Import(sym,ressec,0);
#endif /* !PE_PROCESS_HINTS */
#ifdef DCC_SYMFLAG_PE_ITA_IND
      /* Since this is a PE symbol, try to use ITA indirection. */
      if (def->ld_impsymfa&DCC_SYMFLAG_PE_ITA_IND)
          sym->sy_flags |= DCC_SYMFLAG_PE_ITA_IND;
#endif
     }
    }
   }
   free(symname);
  }
 }
#undef exptab_size
#undef exptab_addr
#undef GET_DIR
#undef HAS_DIR
#undef HAS_FIELD
end:
 DCC_Free(sections);
 return ressec != NULL;
}

DCC_DECL_END
#endif /* DCC_LIBFORMAT_PE_DYNAMIC */
#endif /* DCC_LIBFORMAT_PE_DYNAMIC || DCC_LIBFORMAT_PE_STATIC */

#endif /* !GUARD_DCC_UNIT_IMPORT_PE_DYNAMIC_C_INL */
