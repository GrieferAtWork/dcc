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
#ifndef GUARD_DCC_UNIT_IMPORT_ELF_C_INL
#define GUARD_DCC_UNIT_IMPORT_ELF_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>

#if DCC_LIBFORMAT_ELF
#include <dcc/unit.h>
#include <dcc/stream.h>
#include <dcc/linker.h>

#include "linker-elf.h"

DCC_DECL_BEGIN



INTERN int DCCUNIT_IMPORTCALL
DCCUnit_LoadELF(struct DCCLibDef *__restrict def,
                char const *__restrict file, stream_t fd) {
 int result = 0;
 int link_statically;
 Elf(Ehdr) ehdr;
 assert(def);
 assert(file);
 if (!s_reada(fd,&ehdr,sizeof(ehdr))) goto end;
 if (ehdr.e_ident[EI_MAG0] != ELFMAG0 ||
     ehdr.e_ident[EI_MAG1] != ELFMAG1 ||
     ehdr.e_ident[EI_MAG2] != ELFMAG2 ||
     ehdr.e_ident[EI_MAG3] != ELFMAG3) goto end;
 result = 1;
 /* Check header settings. */
 if (ehdr.e_ident[EI_CLASS]   != DCC_TARGET_ELFCLASS)    { WARN(W_LIB_ELF_INVALID_CLASS,file,DCC_TARGET_ELFCLASS,ehdr.e_ident[EI_CLASS]); }
 if (ehdr.e_ident[EI_DATA]    != DCC_TARGET_ELFDATA)     { WARN(W_LIB_ELF_INVALID_DATA,file,DCC_TARGET_ELFDATA,ehdr.e_ident[EI_DATA]); result = 0; }
 if (ehdr.e_ident[EI_VERSION] != DCC_TARGET_ELFVERSION)  { WARN(W_LIB_ELF_INVALID_VERSION,file,DCC_TARGET_ELFVERSION,ehdr.e_ident[EI_VERSION]); }
 if (ehdr.e_ident[EI_OSABI]   != DCC_TARGET_ELFOSABI)    { WARN(W_LIB_ELF_INVALID_OSABI,file,DCC_TARGET_ELFOSABI,ehdr.e_ident[EI_OSABI]); }
 if (ehdr.e_machine           != DCC_TARGET_ELF_MACHINE) { WARN(W_LIB_ELF_INVALID_MACHINE,file,DCC_TARGET_ELF_MACHINE,ehdr.e_machine); result = 0; }
 if (ehdr.e_version           != DCC_TARGET_ELFVERSION)  { WARN(W_LIB_ELF_INVALID_VERSION2,file,DCC_TARGET_ELFVERSION,ehdr.e_version); }
 if (!result) goto end;
 if (!OK) { result = 0; goto end; }
 link_statically = (def->ld_flags&DCC_LIBDEF_FLAG_STATIC);
 /* When allowed to, dynamically against an ELF shared library. */
 if (!(def->ld_flags&DCC_LIBDEF_FLAG_NODYN) &&
      (ehdr.e_type == ET_DYN)) link_statically = 0;
 /* Warn about strange (and unlikely to-be intended) linker combinations. */
 if (link_statically) {
  if (ehdr.e_type == ET_DYN) WARN(W_LIB_ELF_STATIC_SHARED,file);
 } else {
  if (ehdr.e_type == ET_EXEC) WARN(W_LIB_ELF_DYNAMIC_EXEC,file);
  if (ehdr.e_type == ET_REL) WARN(W_LIB_ELF_DYNAMIC_RELO,file);
 }
 assert(result);
 /* OK! Let's do this. */
 if (!link_statically) {
  /* >> Simple get a list of all ELF exports
   *    from 'PT_DYNAMIC' program headers. */
  if (!ehdr.e_phnum) { nophdr: WARN(W_LIB_ELF_DYNAMIC_EMPTY,file); goto nodyn; }
  if (!ehdr.e_phentsize) {
   WARN(W_LIB_ELF_DYNAMIC_NOSIZE,file);
   ehdr.e_phentsize = sizeof(Elf(Phdr));
  }
link_dynamic:
  { Elf(Phdr) *phdr_data,*iter,*end;
    size_t phdr_size;
    ptrdiff_t read_error;
    phdr_size = ehdr.e_phnum*sizeof(Elf(Phdr));
    phdr_data = (Elf(Phdr) *)DCC_Malloc(phdr_size,0);
    if unlikely(!phdr_data) goto fail;
    if likely(ehdr.e_phentsize == sizeof(Elf(Phdr))) {
     s_seek(fd,ehdr.e_phoff,SEEK_SET);
     read_error = s_read(fd,phdr_data,phdr_size);
     if (read_error < 0) goto fail;
     if (phdr_size > (size_t)read_error)
         phdr_size = (size_t)read_error;
     phdr_size /= ehdr.e_phentsize;
    } else {
     /* Since the program header size isn't what was expected, we
      * must adjust the read size and manually read each element. */
     size_t i,common_size = ehdr.e_phentsize;
     if (ehdr.e_phentsize > sizeof(Elf(Phdr)))
         memset(phdr_data,0,phdr_size),
         common_size = sizeof(Elf(Phdr));
     assert(common_size);
     phdr_size /= sizeof(Elf(Phdr));
     for (i = 0; i < ehdr.e_phnum; ++i) {
      s_seek(fd,ehdr.e_phoff+i*ehdr.e_phentsize,SEEK_SET);
      read_error = s_read(fd,phdr_data+i,common_size);
      if (read_error < 0) goto fail;
      if (read_error < common_size) { phdr_size = i; break; }
     }
    }
    if unlikely(!phdr_size) goto nophdr;
    end = (iter = phdr_data)+phdr_size;
    for (; iter != end; ++iter) {
     /* TODO: Track all 'PT_LOAD' headers (we need to know a mapping from 'p_vaddr' -> 'p_offset') */
     /* TODO: Collect all 'PT_DYNAMIC' headers (we need to iterate them to figure out visible, exported symbols later) */
    }
    goto fail; /* TODO: Remove me */
    //goto end;
  }
 }
link_static:
 assert(link_statically);
 if (!ehdr.e_shnum) {
  /* If there are no section headers, we can't link statically.
   * TODO: Technically, we could load the 'PT_LOAD' program headers
   *       instead and assign auto-generated sections names to each,
   *       essentially generating a new set of sections.
   *       If the program header then contains a 'DT_DYNAMIC' entry,
   *       and the image itself is relocatable (similar to a PE binary),
   *       we could do as we are doing for PE static linkage and after
   *       generating missing DISP-relocs simply copy everything. */
  WARN(W_LIB_ELF_STATIC_EMPTY,file);
  if (!(def->ld_flags&DCC_LIBDEF_FLAG_NODYN) &&
        ehdr.e_phnum) goto link_dynamic;
  goto fail;
 }
 if (!ehdr.e_shentsize) {
  WARN(W_LIB_ELF_STATIC_NOSIZE,file);
  ehdr.e_phentsize = sizeof(Elf(Shdr));
 }

 assert(def->ld_flags&DCC_LIBDEF_FLAG_STATIC);
 assert(!unit.u_symc);
 assert(!unit.u_nsymc);
 assert(!unit.u_secc);
 assert(!unit.u_impc);

 /* TODO: Link static ELF binary. */
 result         = 0;
 def->ld_dynlib = NULL;


end: return result;
nodyn:
 /* If dynamic linking failed (for whatever reason), try to link statically (if allowed, to). */
 if (def->ld_flags&DCC_LIBDEF_FLAG_STATIC) { link_statically = 1; goto link_static; }
fail: result = 0,def->ld_dynlib = NULL; goto end;
}

DCC_DECL_END
#endif /* DCC_LIBFORMAT_ELF */

#endif /* !GUARD_DCC_UNIT_IMPORT_ELF_C_INL */
