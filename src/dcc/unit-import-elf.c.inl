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
#include "x86_util.h"

#if DCC_TARGET_BIN == DCC_BINARY_PE
#include "linker-pe.h"
#endif

DCC_DECL_BEGIN


/* Sort a given vector of program headers by type, ensuring
 * that same-type headers are adjacent to each other. */
LOCAL void elf_phdr_sort(Elf(Phdr) *__restrict hdrv, size_t hdrc) {
 Elf(Phdr) temp,*v_end,*iter,*next;
 v_end = hdrv+hdrc;
 while (hdrc--) {
  iter = hdrv;
  for (;;) {
   next = iter+1;
   if (next == v_end) break;
   if (next->p_type < iter->p_type) {
    temp = *iter;
    *iter = *next;
    *next = temp;
   }
   iter = next;
  }
 }
}

/* Find the program header associated with the given 'addr' */
LOCAL Elf(Phdr) *elf_findphdr(Elf(Phdr) *__restrict hdrv,
                              size_t hdrc, Elf(Addr) addr) {
 Elf(Phdr) *iter,*end;
 end = (iter = hdrv)+hdrc;
 for (; iter != end; ++iter) {
  if (addr >= iter->p_vaddr &&
      addr <  iter->p_vaddr+iter->p_memsz) return iter;
 }
 return NULL;
}

LOCAL Elf(Off) elf_faddrof(Elf(Phdr) *__restrict hdrv, size_t hdrc,
                           Elf(Addr) addr, size_t *__restrict max_bytes) {
 Elf(Phdr) *iter,*end;
 assert(max_bytes);
 end = (iter = hdrv)+hdrc;
 for (; iter != end; ++iter) {
  if (addr >= iter->p_vaddr &&
      addr <  iter->p_vaddr+iter->p_filesz) {
   /* Found it! */
   *max_bytes = (iter->p_vaddr+iter->p_filesz)-addr;
   return iter->p_offset+(addr-iter->p_vaddr);
  }
 }
 *max_bytes = 0;
 return 0;
}


PRIVATE int
elf_gnuhash_findmaxsym(size_t *__restrict result,
                       Elf(Off) hash_off, size_t hash_siz,
                       stream_t fd, int is64b) {
 Elf(GNUHash) hdr;
 size_t    maxsymp1 = 0; /* Max symbol index +1 */
 uint32_t *bucket_vector,*iter,*end;
 size_t    bucket_offset;
 size_t    bucket_count;
 size_t    indexsize = is64b ? 8 : 4;
 ptrdiff_t read_error;
 if unlikely(hash_siz < sizeof(hdr)) return 0;
 s_seek(fd,hash_off,SEEK_SET);
 if unlikely(!s_reada(fd,&hdr,sizeof(hdr))) return 0;
 bucket_offset = sizeof(Elf(GNUHash))+hdr.gh_bitmask_nwords*indexsize;
 if (bucket_offset > hash_siz) return 0;
 bucket_count = (size_t)(hash_siz-bucket_offset)/4;
 if (bucket_count > (size_t)hdr.gh_nbuckets)
     bucket_count = (size_t)hdr.gh_nbuckets;
 bucket_vector = (uint32_t *)DCC_Malloc(bucket_count*4,0);
 if unlikely(!bucket_vector) return 0;
 s_seek(fd,hash_off+bucket_offset,SEEK_SET);
 read_error = s_read(fd,bucket_vector,bucket_count*4);
 if (read_error < 0) read_error = 0;
 read_error /= 4;
 if (bucket_count > (size_t)read_error)
     bucket_count = (size_t)read_error;
 if unlikely(!bucket_count) { DCC_Free(bucket_vector); return 0; }
 /* We've managed to read the bucket vector. - Now to find the greatest symbol index. */
 end = (iter = bucket_vector)+bucket_count;
 for (; iter != end; ++iter) {
  if (*iter >= maxsymp1) maxsymp1 = *iter+1;
 }
 assert(maxsymp1 > 0);
 *result = maxsymp1;
 DCC_Free(bucket_vector);
 return 1;
}


PRIVATE void
elf_loadsection(Elf(Shdr) const *__restrict hdr,
                struct DCCTextBuf *__restrict text,
                stream_t fd, soff_t start) {
 uint8_t *data;
 ptrdiff_t readsize;
 assert(hdr);
 assert(text);
 data = (uint8_t *)DCC_Malloc(hdr->sh_size,0);
 if unlikely(!data) goto empty;
 s_seek(fd,start+hdr->sh_offset,SEEK_SET);
 readsize = s_read(fd,data,hdr->sh_size);
 if (readsize < 0) readsize = 0;
 if (readsize == 0) { DCC_Free(data); goto empty; }
 if ((size_t)readsize == (size_t)hdr->sh_size)
  text->tb_begin = data;
 else {
  text->tb_begin = (uint8_t *)realloc(data,(size_t)readsize);
  if likely(text->tb_begin)
       data = text->tb_begin;
  else text->tb_begin = data;
 }
 text->tb_end = data+(size_t)readsize;
end: text->tb_max = text->tb_pos = text->tb_end;
 return;
empty: text->tb_begin = text->tb_end = NULL; goto end;
}

PRIVATE symflag_t const elfvismap[] = {
 /* [STV_DEFAULT  ] = */DCC_SYMFLAG_NONE,
 /* [STV_INTERNAL ] = */DCC_SYMFLAG_INTERNAL,
 /* [STV_HIDDEN   ] = */DCC_SYMFLAG_PRIVATE,
 /* [STV_PROTECTED] = */DCC_SYMFLAG_PROTECTED,
};

INTERN int DCCUNIT_IMPORTCALL
DCCUnit_LoadELF(struct DCCLibDef *__restrict def,
                char const *__restrict file,
                stream_t fd, soff_t start) {
 int result = 0;
 int link_statically,not_dynamic = 0;
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
  {
   Elf(Phdr) *phdr_data,*iter,*end;
   Elf(Dyn) *dynvec = NULL;
   size_t phdr_size;
   ptrdiff_t read_error;
   phdr_size = ehdr.e_phnum*sizeof(Elf(Phdr));
   phdr_data = (Elf(Phdr) *)DCC_Malloc(phdr_size,0);
   if unlikely(!phdr_data) goto fail;
   if likely(ehdr.e_phentsize == sizeof(Elf(Phdr))) {
    s_seek(fd,start+ehdr.e_phoff,SEEK_SET);
    read_error = s_read(fd,phdr_data,phdr_size);
    if (read_error < 0) goto fail_dyn;
    if (phdr_size > (size_t)read_error)
        phdr_size = (size_t)read_error;
    phdr_size /= ehdr.e_phentsize;
   } else {
    /* Since the program header size isn't what was expected, we
     * must adjust the read size and manually read each element. */
    size_t i,common_size = sizeof(Elf(Phdr));
    if (ehdr.e_phentsize < sizeof(Elf(Phdr))) {
     memset(phdr_data,0,phdr_size);
     common_size = ehdr.e_phentsize;
    }
    assert(common_size);
    phdr_size /= sizeof(Elf(Phdr));
    for (i = 0; i < ehdr.e_phnum; ++i) {
     s_seek(fd,start+ehdr.e_phoff+i*ehdr.e_phentsize,SEEK_SET);
     if (!s_reada(fd,phdr_data+i,common_size)) { phdr_size = i; break; }
    }
   }
   if unlikely(!phdr_size) goto nophdr;
   /* Sort the program header vector so we can later use
    * sub-vector of all PT_LOAD and PT_DYNAMIC entries. */
   elf_phdr_sort(phdr_data,phdr_size);
   {
    /* Track all 'PT_LOAD' headers (we need to know
     * the mapping from 'p_vaddr' -> 'p_offset') */
    /* Collect all 'PT_DYNAMIC' headers (we need to iterate 
     * them to figure out visible, exported symbols later) */
    Elf(Phdr) *cmd_loadv; size_t cmd_loadc = 0;
    Elf(Phdr) *cmd_dynv;  size_t cmd_dync  = 0;
    end = (iter = phdr_data)+phdr_size;
#if PT_LOAD < PT_DYNAMIC
    while (iter != end && iter->p_type != PT_LOAD) ++iter;
    cmd_loadv = iter;
    while (iter != end && iter->p_type == PT_LOAD) ++iter;
    cmd_loadc = (size_t)(iter-cmd_loadv);
#if PT_DYNAMIC != PT_LOAD+1
    while (iter != end && iter->p_type != PT_DYNAMIC) ++iter;
#endif
    cmd_dynv = iter;
    while (iter != end && iter->p_type == PT_DYNAMIC) ++iter;
    cmd_dync = (size_t)(iter-cmd_dynv);
#else
    while (iter != end && iter->p_type != PT_DYNAMIC) ++iter;
    cmd_dynv = iter;
    while (iter != end && iter->p_type == PT_DYNAMIC) ++iter;
    cmd_dync = (size_t)(iter-cmd_dynv);
#if PT_LOAD != PT_DYNAMIC+1
    while (iter != end && iter->p_type != PT_LOAD) ++iter;
#endif
    cmd_loadv = iter;
    while (iter != end && iter->p_type == PT_LOAD) ++iter;
    cmd_loadc = (size_t)(iter-cmd_loadv);
#endif
    /* Make sure there are both load headers (required for mapping dynamic address headers),
     * as well as dynamic headers (containing information about dynamic (aka. exported) symbols) */
    if unlikely(!cmd_dync) { WARN(W_LIB_ELF_DYNAMIC_NOLOAD,file); goto nodyn; }
    if unlikely(!cmd_loadc) {nodynhdr: WARN(W_LIB_ELF_DYNAMIC_NODYN,file); goto nodyn; }
    /* Now iterate all dynamic headers and parse their commands. */
    end = (iter = cmd_dynv)+cmd_dync;
    /* NOTE: Usually, 'cmd_dync' will be 1 (And the ELF standard might even require that),
     *       but implementation is _very_ lenient and simply defines the first (non-empty)
     *       dynamic header as the only one of interest. */
    def->ld_dynlib = NULL; /* Filled later. */
    for (; iter != end; ++iter) {
     Elf(Dyn) *dyn_iter,*dyn_end;
     size_t dynsiz;
     assert(iter->p_type == PT_DYNAMIC);
     dynsiz = iter->p_filesz;
     if (dynsiz > iter->p_memsz)
         dynsiz = iter->p_memsz;
     /* Align the size to truncate with the size of a dynamic header. */
     dynsiz &= ~(sizeof(Elf(Dyn))-1);
     if unlikely(!dynsiz) continue;
     assert(dynsiz >= sizeof(Elf(Dyn)));
     dynvec = (Elf(Dyn) *)DCC_Malloc(dynsiz,0);
     if unlikely(!dynvec) goto fail_dyn;
     /* XXX: Does ELF guaranty that PT_DYNAMIC contains proper offset information?
      *      Or do we have to find a PT_LOAD header containing the PT_DYNAMIC virtual address? */
     s_seek(fd,start+iter->p_offset,SEEK_SET);
     read_error = s_read(fd,dynvec,dynsiz);
     if unlikely(dynsiz > (size_t)read_error || read_error < 0) {
      if (read_error < 0) dynsiz = 0;
      else                dynsiz = (size_t)read_error;
      dynsiz &= ~(sizeof(Elf(Dyn))-1);
      if unlikely(!dynsiz) goto next_dynhdr;
     }
     assert(dynsiz >= sizeof(Elf(Dyn)));
     {
      /* Iterate the dynamic header commands. */
      Elf(Addr) dt_soname   = 0;                /* DT_SONAME (Optional; Points into 'dt_strtab' defaults to 'def->ld_name') */
      Elf(Addr) dt_hash     = 0;                /* DT_HASH (Optional; Address of dymbol hash table; can be dereferenced for symbol count) */
      Elf(Addr) dt_gnu_hash = 0;                /* DT_GNU_HASH (Optional; Many ELF binaries no longer contain DT_HASH, but only 'DT_GNU_HASH'. - We use the later to figure out how many symbols exist) */
      Elf(Addr) dt_strtab   = 0;                /* DT_STRTAB (Mandatory; Contains the address of the string table) */
      Elf(Addr) dt_symtab   = 0;                /* DT_SYMTAB (Mandatory; Contains the address of the symbol table) */
      Elf(Word) dt_strsz    = 0;                /* DT_STRSZ (Optional; When omited, assume 'abs(dt_symtab-dt_strtab)' and always truncate by PT_LOAD constraints) */
      Elf(Word) dt_syment   = sizeof(Elf(Sym)); /* DT_SYMENT (Optional; size of a single symbol table entry) */
      dynsiz /= sizeof(Elf(Dyn));
      dyn_end = (dyn_iter = dynvec)+dynsiz;
      for (; dyn_iter != dyn_end; ++dyn_iter) {
       /* ELF says that DT_NULL should be able to terminate this list. */
       switch (dyn_iter->d_tag) {
       case DT_NULL:     goto done_dynhdr1;
       case DT_SONAME:   dt_soname   = dyn_iter->d_un.d_ptr; break;
       case DT_HASH:     dt_hash     = dyn_iter->d_un.d_ptr; break;
       case DT_STRTAB:   dt_strtab   = dyn_iter->d_un.d_ptr; break;
       case DT_SYMTAB:   dt_symtab   = dyn_iter->d_un.d_ptr; break;
       case DT_STRSZ:    dt_strsz    = dyn_iter->d_un.d_val; break;
       case DT_SYMENT:   dt_syment   = dyn_iter->d_un.d_val; break;
       case DT_GNU_HASH: dt_gnu_hash = dyn_iter->d_un.d_ptr; break;
       default: break; /* Ignore unknown entries. */
       }
      }
#define FADDR(vaddr,pmax_size) elf_faddrof(cmd_loadv,cmd_loadc,vaddr,pmax_size)
done_dynhdr1:
      {
       Elf(Off) strtab_off; size_t strtab_siz; char *strtab;
       Elf(Off) symtab_off; size_t symtab_siz; Elf(Sym) *symtab;

       /* Make sure all the required sections were loaded. */
       if unlikely(!dt_strtab) { WARN(W_LIB_ELF_DYNAMIC_NO_DT_STRTAB,(size_t)(iter-cmd_dynv),file); goto next_dynhdr; }
       if unlikely(!dt_symtab) { WARN(W_LIB_ELF_DYNAMIC_NO_DT_SYMTAB,(size_t)(iter-cmd_dynv),file); goto next_dynhdr; }
       /* If the string table size was omitted, assume its size by looking at the string/symbol table addresses. */
       if (!dt_strsz && dt_strtab < dt_symtab) dt_strsz = dt_symtab-dt_strtab;
       /* Skip impossible constraints for the string/symbol tables. */
       if unlikely(!dt_strsz || !dt_syment) goto next_dynhdr;
       /* Lookup the file-offset of the string/symbol table. */
       if unlikely((strtab_off = FADDR(dt_strtab,&strtab_siz)) == 0) { WARN(W_LIB_ELF_DYNAMIC_UNMAPPED_STRTAB,(size_t)(iter-cmd_dynv),file,(target_ptr_t)dt_strtab); goto next_dynhdr; }
       if unlikely((symtab_off = FADDR(dt_symtab,&symtab_siz)) == 0) { WARN(W_LIB_ELF_DYNAMIC_UNMAPPED_SYMTAB,(size_t)(iter-cmd_dynv),file,(target_ptr_t)dt_symtab); goto next_dynhdr; }
       if (dt_hash) {
        Elf(Off) hash_off; size_t hash_siz; uint32_t hashcnt;
        /* Try to load the start of the hash-table to figure out the exact symbol count.
         * The symbol count is located at OFFSET '4' in a 4-BYTE unsigned integer. */
        if unlikely((hash_off = FADDR(dt_hash,&hash_siz)) == 0) {
         WARN(W_LIB_ELF_DYNAMIC_UNMAPPED_HASH,(size_t)(iter-cmd_dynv),file,(target_ptr_t)dt_symtab);
         goto no_dynhash;
        }
        if (hash_siz < 8) goto no_dynhash;
        s_seek(fd,start+hash_off+4,SEEK_SET);
        read_error = s_read(fd,&hashcnt,4);
        if (read_error < 4) goto no_dynhash;
        /* Clamp the symbol table size using the value read from the hash table. */
        hashcnt *= dt_syment;
        if (symtab_siz > hashcnt)
            symtab_siz = hashcnt;
       } else if (dt_gnu_hash) {
        Elf(Off) gnuhash_off; size_t gnuhash_siz;
        /* The GNU hash is a bit more complicated because it only
         * contains information about how many symbols exist implicitly.
         * Instead, we must load the entire table and search for the greatest symbol index.
         * A better explaination of this can be found here:
         * >> http://deroko.phearless.org/dt_gnu_hash.txt */
        if unlikely((gnuhash_off = FADDR(dt_gnu_hash,&gnuhash_siz)) == 0) {
         WARN(W_LIB_ELF_DYNAMIC_UNMAPPED_STRTAB,
             (size_t)(iter-cmd_dynv),file,
             (target_ptr_t)dt_gnu_hash);
         goto no_dynhash;
        }
        if (!elf_gnuhash_findmaxsym(&symtab_siz,start+gnuhash_off,gnuhash_siz,fd,
                                     ehdr.e_ident[EI_CLASS] == ELFCLASS64)
             ) goto no_dynhash;
       } else {
no_dynhash:
        /* TODO: Try to load the section list and locate the section associated
         *       with 'symtab_off'. Then, using that section's 'sh_size' field,
         *       we can truncate the max amount of symbols.
         * >> When that fails too (such as when there are no sections), emit a warning
         *    and perform special checks when iterating the symbol vector, stopping
         *    at the first symbol describing invalid data (e.g.: an invalid section id, or a corrupt name) */
        ;
       }
       /* Constrict the max symbol table size using the PT_DYNAMIC header. */
       if (dt_strsz < strtab_siz) strtab_siz = dt_strsz;
       /* Allocate & read the string table. */
       strtab = (char *)DCC_Malloc(strtab_siz+1,0);
       if unlikely(!strtab) goto next_dynhdr;
       /* Make sure the string table is ZERO-terminated,
        * so we can safely use strlen on its elements. */
       strtab[strtab_siz] = '\0';
       s_seek(fd,start+strtab_off,SEEK_SET);
       read_error = s_read(fd,strtab,strtab_siz);
       if unlikely(read_error < 0) goto end_free_strtab;
       if (strtab_siz > (size_t)read_error)
           strtab_siz = (size_t)read_error;
       strtab_siz /= sizeof(char);
       { /* The string table has been read! Now to read the symbol table */
        size_t symcnt = (symtab_siz/dt_syment)*sizeof(Elf(Sym));
        symtab = (Elf(Sym) *)DCC_Malloc(symcnt,0);
        if unlikely(!symtab) goto end_free_strtab;
        if likely(dt_syment == sizeof(Elf(Sym))) {
         /* Likely case: We can read the symbol table as-is. */
         s_seek(fd,start+symtab_off,SEEK_SET);
         read_error = s_read(fd,symtab,symcnt);
         if (read_error < 0) goto end_free_symtab;
         if (symcnt > (size_t)read_error)
             symcnt = (size_t)read_error;
         symcnt /= sizeof(Elf(Sym));
        } else {
         size_t i,common_size = sizeof(Elf(Sym));
         /* Difficult case: Must read each entry individually. */
         if (dt_syment < sizeof(Elf(Sym))) {
          memset(symtab,0,symcnt);
          common_size = dt_syment;
         }
         symcnt /= sizeof(Elf(Sym));
         for (i = 0; i < symcnt; ++i) {
          s_seek(fd,start+symtab_off+i*dt_syment,SEEK_SET);
          if (!s_reada(fd,&symtab[i],dt_syment)) { symcnt = i; break; }
         }
        }
        /* Store the symbol table element count as its size. */
        symtab_siz = symcnt;
       }
#define STR(off)    ((off) >= strtab_siz ? NULL : strtab+(off))
       if likely(symtab_siz) {
        char const *used_soname = def->ld_name;
        size_t      used_sosize = def->ld_size;
        struct TPPKeyword *sokwd;
        struct DCCSection *libsec;
        Elf(Sym) *sym_iter,*sym_end;
        /* OK! We've got the symbol table in 'symtab' and its element count in 'symtab_siz'.
         * And we've got the string table in 'strtab' and its size in bytes in 'strtab_siz'. */
        if (dt_soname && !(def->ld_flags&DCC_LIBDEF_FLAG_USERNAME)) {
         /* Use a custom SO-name. */
         char *cname = STR(dt_soname);
         if (cname && *cname) {
          used_soname = cname;
          used_sosize = strlen(cname);
         }
        }
        sokwd = TPPLexer_LookupKeyword(used_soname,used_sosize,1);
        if unlikely(!sokwd) goto end_free_symtab;
        libsec = DCCUnit_NewSec(sokwd,DCC_SYMFLAG_SEC_ISIMPORT);
        if unlikely(!libsec) goto end_free_symtab;
        if (!DCCSection_ISIMPORT(libsec)) { /* TODO: What now? */ }
        /* Define the initial library section. */
        if (!def->ld_dynlib) def->ld_dynlib = libsec;
        sym_end = (sym_iter = symtab)+symtab_siz;
        for (; sym_iter != sym_end; ++sym_iter) {
         struct DCCSym *sym;
         char *symname;
         symflag_t flags;
#ifdef DCC_SYMFLAG_DLLIMPORT
         flags = DCC_SYMFLAG_DLLIMPORT;
#else
         flags = DCC_SYMFLAG_NONE;
#endif
         switch (ELF32_ST_BIND(sym_iter->st_info)) {
         case STB_LOCAL: continue; /* Skip local symbol definitions. */
         case STB_WEAK: flags |= DCC_SYMFLAG_WEAK; break; /* Weak symbol. */
         default: break;
         }
         /* Check symbol visibility. */
         if (ELF(ST_VISIBILITY)(sym_iter->st_other) != STV_DEFAULT) continue;
         if ((symname = STR(sym_iter->st_name)) == NULL) {
          WARN(W_LIB_ELF_DYNAMIC_SYMNAME_CORRUPT,
              (size_t)(iter-cmd_dynv),file,
              (size_t)(sym_iter-symtab),
              (size_t)sym_iter->st_name,
              (size_t)strtab_siz);
          continue;
         }
         sym = DCCUnit_NewSyms(symname,DCCLibDef_EXPFLAGS(def,flags));
         /* NOTE: Only (re-)define a symbol when it is forward,
          *       or when the library definition isn't weak. */
         if (sym && sym->sy_sec != libsec &&
            (!(flags&DCC_SYMFLAG_WEAK) && DCCSym_ISFORWARD(sym))) {
#if 1 /* Don't include symbol value/size information (cannot rely on either) */
          DCCSym_Define(sym,libsec,0,0);
#else
          DCCSym_Define(sym,libsec,sym_iter->st_value,sym_iter->st_size);
#endif
         }
        }
       }
#undef STR
end_free_symtab: DCC_Free(symtab);
end_free_strtab: DCC_Free(strtab);
      }
#undef FADDR
     }
next_dynhdr:
     DCC_Free(dynvec);
     dynvec = NULL;
    }
   }
   /* Make sure at least one non-empty dynamic header was found. */
   if (!def->ld_dynlib) goto nodynhdr;
   DCC_Free(phdr_data);
   goto end;
fail_dyn:
   DCC_Free(dynvec);
   DCC_Free(phdr_data);
   goto fail;
  }
 }
link_static:
 assert(link_statically);
 if (!ehdr.e_shnum) {
nosechdr:
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
        ehdr.e_phnum && !not_dynamic) goto link_dynamic;
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
 def->ld_dynlib = NULL;
 /* Link static ELF binary/object file. */
 {
  Elf(Shdr) *secv,*iter,*end;
  size_t secc = ehdr.e_shnum;
  ptrdiff_t read_error;
  secv = (Elf(Shdr) *)DCC_Malloc(secc*sizeof(Elf(Shdr)),0);
  if unlikely(!secv) goto fail;
  if (ehdr.e_shentsize == sizeof(Elf(Shdr))) {
   s_seek(fd,start+ehdr.e_shoff,SEEK_SET);
   read_error = s_read(fd,secv,secc*sizeof(Elf(Shdr)));
   if (read_error < 0) read_error = 0;
   read_error /= sizeof(Elf(Shdr));
   if (secc < (size_t)read_error)
       secc = (size_t)read_error;
  } else {
   size_t i,common_size = sizeof(Elf(Shdr));
   if (ehdr.e_shentsize < sizeof(Elf(Shdr))) {
    memset(secv,0,secc*sizeof(Elf(Shdr)));
    common_size = ehdr.e_shentsize;
   }
   for (i = 0; i < secc; ++i) {
    s_seek(fd,start+ehdr.e_shoff+i*ehdr.e_shentsize,SEEK_SET);
    if (!s_reada(fd,&secv[i],common_size)) { secc = i; break; }
   }
  }
  if unlikely(!secc) { DCC_Free(secv); goto nosechdr; }
  /* First step: Validate and find the '.shstrtab' section. */
  if (ehdr.e_shstrndx >= secc) {
   size_t i;
   WARN(W_LIB_ELF_STATIC_UNMAPPED_SHSTR,file,ehdr.e_shstrndx,secc);
   for (i = 0; i < secc; ++i) {
    if (secv[i].sh_type == SHT_STRTAB) {
     ehdr.e_shstrndx = (Elf(Half))i;
     goto found_shstr;
    }
   }
   WARN(W_LIB_ELF_STATIC_NOSHSTR,file);
  } else if (secv[ehdr.e_shstrndx].sh_type != SHT_STRTAB) {
   WARN(W_LIB_ELF_STATIC_WRONG_SHSTR_TYPE,
        file,ehdr.e_shstrndx,
       (unsigned long)SHT_STRTAB,
       (unsigned long)secv[ehdr.e_shstrndx].sh_type);
  }
found_shstr:
   /* Load the section name section so we can figure out what everything's called!
    * Only once we did that, can we start creating the sections. */
#define SEC_DCCSEC(shdr) (*(struct DCCSection **)&(shdr)->sh_addralign)
  {
   struct DCCTextBuf shstr_text;
   elf_loadsection(&secv[ehdr.e_shstrndx],&shstr_text,fd,start);
   /* With the section names loaded, we can generate all the section! */
   end = (iter = secv)+secc;
#define SHSTR(off) (char *)(shstr_text.tb_begin+(off) >= shstr_text.tb_end ? NULL : shstr_text.tb_begin+(off))
   for (; iter != end; ++iter) {
    char *name; struct DCCSection *sec;
    symflag_t secflags = DCC_SYMFLAG_NONE;
    /* Don't load sections not marked as ALLOC. */
    switch (iter->sh_type) {
    case SHT_NULL        : /* Don't load these sections. */
    case SHT_SYMTAB      :
    case SHT_STRTAB      :
    case SHT_RELA        :
    case SHT_HASH        :
    case SHT_DYNAMIC     :
    case SHT_REL         :
    case SHT_DYNSYM      :
    case SHT_GROUP       :
    case SHT_SYMTAB_SHNDX:
    case SHT_DCC_SYMFLG  :
sec_unused: SEC_DCCSEC(iter) = NULL;
     continue;
    default:
     /* Ignore all unrecognized, user-defined section types. */
     if (iter->sh_type >= SHT_LOUSER &&
         iter->sh_type <= SHT_HIUSER)
         continue;
    case SHT_DCC_IMPSEC:
     break; /* Link any other section. */
    }
    name = SHSTR(iter->sh_name);
    if unlikely(!name) {
     WARN(W_LIB_ELF_STATIC_SECNAME_CORRUPT,
         (size_t)(iter-secv),file,(size_t)iter->sh_name,
         (size_t)(shstr_text.tb_end-shstr_text.tb_begin));
     goto sec_unused;
    }
    if (iter->sh_flags&SHF_ALLOC)     secflags |= DCC_SYMFLAG_SEC_R;
    if (iter->sh_flags&SHF_WRITE)     secflags |= DCC_SYMFLAG_SEC_W;
    if (iter->sh_flags&SHF_EXECINSTR) secflags |= DCC_SYMFLAG_SEC_X;
    if (iter->sh_flags&SHF_MERGE)     secflags |= DCC_SYMFLAG_SEC_M;
    if (iter->sh_type == SHT_DCC_IMPSEC) {
     /* TODO: According to my specs, we must use a custom
      *       filename obtained from 'sh_link[sh_info]' */
     secflags |= DCC_SYMFLAG_SEC_ISIMPORT;
    }
    sec = DCCUnit_NewSecs(name,secflags);
    if unlikely(!sec) goto sec_unused;
    if (!(secflags&DCC_SYMFLAG_SEC_ISIMPORT)) {
     if (sec->sc_text.tb_begin != sec->sc_text.tb_end)
         WARN(W_LIB_ELF_STATIC_SECNAME_REUSED,file,name);
     free(sec->sc_text.tb_begin);
     elf_loadsection(iter,&sec->sc_text,fd,start);
     sec->sc_align = iter->sh_addralign;
     sec->sc_base  = iter->sh_addr;
    }
    /* Save the section in the header (very hacky; don't look). */
    SEC_DCCSEC(iter) = sec;
   }
   /* Clean up section names. */
   free(shstr_text.tb_begin);
#undef SHSTR
  }
  {
   /* Hacky way to re-use symbol section headers
    * for tracking all DCC symbols contained. */
#define SEC_SYMVEC(shdr) (*(struct DCCSym ***)&(shdr)->sh_addr)
#define SEC_SYMCNT(shdr) (*(size_t *)&(shdr)->sh_size)

#define SEC_DCCSECI(i) ((i) < secc ? SEC_DCCSEC(&secv[i]) : NULL)
#define SEC_SYMVECI(i) ((i) < secc ? SEC_SYMVEC(&secv[i]) : NULL)
#define SEC_SYMCNTI(i) ((i) < secc ? SEC_SYMCNT(&secv[i]) : NULL)

   /* All sections of interest have been loaded.
    * Now, we must load all symbol tables. */
   for (iter = secv; iter != end; ++iter) {
    Elf(Sym) *symvec,*sym_iter,*sym_end; size_t symcnt;
    struct DCCSym **symdef; struct DCCTextBuf symstr_text;
    if (iter->sh_type != SHT_SYMTAB) continue;
    if (!iter->sh_entsize) iter->sh_entsize = sizeof(Elf(Sym));
    symcnt = iter->sh_size/iter->sh_entsize;
    if unlikely(!symcnt) {null_symvec: symvec = NULL; goto done_symvec; }
    symvec = (Elf(Sym) *)DCC_Malloc(symcnt*sizeof(Elf(Sym)),0);
    if (iter->sh_entsize == sizeof(Elf(Sym))) {
     symcnt *= sizeof(Elf(Sym));
     s_seek(fd,start+iter->sh_offset,SEEK_SET);
     read_error = s_read(fd,symvec,symcnt);
     if (read_error < 0) read_error = 0;
     if (symcnt > (size_t)read_error)
         symcnt = (size_t)read_error;
     symcnt /= sizeof(Elf(Sym));
    } else {
     size_t i,common_size = sizeof(Elf(Sym));
     /* Difficult case: Must read each entry individually. */
     if (iter->sh_entsize < sizeof(Elf(Sym))) {
      memset(symvec,0,symcnt*sizeof(Elf(Sym)));
      common_size = iter->sh_entsize;
     }
     for (i = 0; i < symcnt; ++i) {
      s_seek(fd,start+iter->sh_offset+i*iter->sh_entsize,SEEK_SET);
      if (!s_reada(fd,&symvec[i],common_size)) { symcnt = i; break; }
     }
    }
    if unlikely(!symcnt) {free_null_symvec: DCC_Free(symvec); goto null_symvec; }
    /* Load symbol names from the linked section into 'symstr_text' */
    if (iter->sh_link >= secc) {
     WARN(W_LIB_ELF_STATIC_SYMTAB_INVSTRID,file,
         (size_t)iter->sh_link,(size_t)(iter-secv),secc);
     goto free_null_symvec;
    }
    elf_loadsection(&secv[iter->sh_link],&symstr_text,fd,start);
#define SYMSTR(off) (char *)(symstr_text.tb_begin+(off) >= symstr_text.tb_end ? NULL : symstr_text.tb_begin+(off))

    /* All symbols and string have been read. - Time to iterate them! */
    symdef = (struct DCCSym **)symvec;
    sym_end = (sym_iter = symvec)+symcnt;
    for (; sym_iter != sym_end; ++sym_iter,++symdef) {
     char *name; struct DCCSection *sec;
     symflag_t symflags; struct DCCSym *sym;
     switch (ELF(ST_TYPE)(sym_iter->st_info)) {
      /* Ignore these symbol types (DCC doesn't use them; yet?) */
     case STT_NOTYPE:
      if (!sym_iter->st_name &&
          !sym_iter->st_value &&
           sym_iter->st_shndx == SHN_UNDEF) {
       *symdef = NULL;
       continue; /* Ignore the NULL-symbol. */
      }
      break;
     case STT_FILE:
skip_symdef:
      *symdef = NULL;
      continue;
     case STT_SECTION: /* Special handling for section symbols. */
      sec = SEC_DCCSECI(sym_iter->st_shndx);
      if (sec) { *symdef = &sec->sc_start; continue; }
      break;
     default: break;
     }
     name = SYMSTR(sym_iter->st_name);
     if unlikely(!name) {
      WARN(W_LIB_ELF_STATIC_SYMNAME_CORRUPT,
          (size_t)(sym_iter-symvec),file,(size_t)sym_iter->st_name,
          (size_t)(symstr_text.tb_end-symstr_text.tb_begin));
      goto skip_symdef;
     }
     /* Don't warn is this symbol ends up unused. */
     symflags = DCC_SYMFLAG_UNUSED;
     symflags |= elfvismap[ELF(ST_VISIBILITY)(sym_iter->st_other)];
     switch (ELF(ST_BIND)(sym_iter->st_info)) {
     case STB_LOCAL: symflags |= DCC_SYMFLAG_STATIC; break;
     case STB_WEAK : symflags |= DCC_SYMFLAG_WEAK; break;
     default       : break;
     }
     if (*name) {
      sym = DCCUnit_NewSyms(name,symflags);
      if unlikely(!sym) goto skip_symdef;
     } else {
      sym = DCCUnit_AllocSym();
      if unlikely(!sym) goto skip_symdef;
      sym->sy_flags |= symflags;
     }
     if (sym_iter->st_shndx == SHN_ABS) sec = &DCCSection_Abs;
     else sec = SEC_DCCSECI(sym_iter->st_shndx);
     if (sec) DCCSym_Define(sym,sec,sym_iter->st_value,sym_iter->st_size);
     else sym->sy_size = sym_iter->st_value; /* Temporary storage for alias offsets. */
     *symdef = sym;
    }
    assert(symdef <= (struct DCCSym **)symvec+symcnt);
    symcnt = (size_t)(symdef-(struct DCCSym **)symvec);
    if unlikely(!symcnt) goto free_null_symvec;
    free(symstr_text.tb_begin);
#undef SYMSTR
done_symvec:
    SEC_SYMVEC(iter) = (struct DCCSym **)symvec;
    SEC_SYMCNT(iter) = symcnt;
   }
#ifdef SHT_DCC_SYMFLG
   /* What about alias symbols? - This is what's about that!
    * ELF doesn't appear to be able to represent these, meaning
    * that when the times comes for DCC to generate object files,
    * I'll have to create an extension, or a new format all-together (which I don't want to):
    * 
    * source_a.c:
    * >> int add(int x, int y) {
    * >>     return x+y;
    * >> }
    * 
    * source_b.c:
    * >> [[alias("add")]] int my_add(int x, int y);
    * >> 
    * >> int _start() {
    * >>     int z = my_add(10,20);
    * >>     return z;
    * >> }
    * 
    * $dcc -c source_a.c
    * $dcc -c source_b.c # GCC fails to compile this, but DCC's supposed to be able to
    * $dcc -o app source_a.o source_b.o
    */
   /* Load all 'SHT_DCC_SYMFLG' extension sections. */
   for (iter = secv; iter != end; ++iter) {
    Elf(DCCSymFlg) *flgv,*flg_iter,*flg_end; size_t flgc;
    struct DCCSym **symv,**sym_iter; size_t symc;
    if (iter->sh_type != SHT_DCC_SYMFLG) continue;
    /* Parse extended symbol flags. */
    if (!iter->sh_entsize) iter->sh_entsize = sizeof(Elf(DCCSymFlg));
    if unlikely((flgc = iter->sh_size/iter->sh_entsize) == 0) continue;
    if unlikely((symc = SEC_SYMCNTI(iter->sh_link)) == 0) continue;
    if unlikely((symv = SEC_SYMVECI(iter->sh_link)) == NULL) continue;
    if (symc < flgc) flgc = symc;
    flgv = (Elf(DCCSymFlg) *)DCC_Malloc(flgc*sizeof(Elf(DCCSymFlg)),0);
    if unlikely(!flgv) continue;
    if (iter->sh_entsize == sizeof(Elf(DCCSymFlg))) {
     flgc *= sizeof(Elf(DCCSymFlg));
     s_seek(fd,start+iter->sh_offset,SEEK_SET);
     read_error = s_read(fd,flgv,flgc);
     if (read_error < 0) read_error = 0;
     if (flgc > (size_t)read_error)
         flgc = (size_t)read_error;
     flgc /= sizeof(Elf(DCCSymFlg));
    } else {
     size_t i,common_size = sizeof(Elf(DCCSymFlg));
     /* Difficult case: Must read each entry individually. */
     if (iter->sh_entsize < sizeof(Elf(DCCSymFlg))) {
      memset(flgv,0,flgc*sizeof(Elf(DCCSymFlg)));
      common_size = iter->sh_entsize;
     }
     for (i = 0; i < flgc; ++i) {
      s_seek(fd,start+iter->sh_offset+i*iter->sh_entsize,SEEK_SET);
      if (!s_reada(fd,&flgv[i],common_size)) { flgc = i; break; }
     }
    }
    flg_end = (flg_iter = flgv)+flgc;
    sym_iter = symv;
    for (; flg_iter != flg_end; ++flg_iter,++sym_iter) {
     struct DCCSym *sym = *sym_iter;
     uint8_t  flags = ELF(DCC_SYMFLAG_FLAGS)(flg_iter->sf_info);
     if unlikely(!sym) continue;
#if ELF_DCC_SYMFLAG_F_USED   == DCC_SYMFLAG_USED && \
    ELF_DCC_SYMFLAG_F_UNUSED == DCC_SYMFLAG_UNUSED
     sym->sy_flags |= flags&(ELF_DCC_SYMFLAG_F_USED|
                             ELF_DCC_SYMFLAG_F_UNUSED);
#else
     if (flags&ELF_DCC_SYMFLAG_F_USED)   sym->sy_flags |= DCC_SYMFLAG_USED;
     if (flags&ELF_DCC_SYMFLAG_F_UNUSED) sym->sy_flags |= DCC_SYMFLAG_UNUSED;
#endif
     if (flags&ELF_DCC_SYMFLAG_F_ALIAS) {
      uint32_t symid = ELF(DCC_SYMFLAG_SYM)(flg_iter->sf_info);
      struct DCCSym *alias_target;
      if unlikely(symid >= symc) continue;
      if unlikely((alias_target = symv[symid]) == NULL) continue;
      /* Define 'sym' as an alias for 'symv[symid]' */
      DCCSym_Alias(sym,alias_target,sym->sy_size);
      sym->sy_size = 0;
     }
    }
    DCC_Free(flgv);
   }
#endif /* SHT_DCC_SYMFLG */
#if DCC_TARGET_BIN == DCC_BINARY_PE
   /* Bind all ITA symbols. */
   for (iter = secv; iter != end; ++iter) {
    struct DCCSym **sym_iter,**sym_end,*sym,*basesym;
    if (iter->sh_type != SHT_SYMTAB) continue;
    sym_end = (sym_iter = SEC_SYMVEC(iter))+SEC_SYMCNT(iter);
    for (; sym_iter != sym_end; ++sym_iter) {
     if ((sym = *sym_iter) == NULL) continue;
     if (sym->sy_name->k_size <= 4) continue;
     if (memcmp(sym->sy_name->k_name,ITA_PREFIX,
                DCC_COMPILER_STRLEN(ITA_PREFIX)*
                sizeof(char)) != 0) continue;
     /* This is an ITA symbol. - Try to find the associated base symbol and link them! */
     basesym = DCCUnit_GetSyms(sym->sy_name->k_name+
                               DCC_COMPILER_STRLEN(ITA_PREFIX));
     if unlikely(!basesym || basesym->sy_peind) continue;
     DCCSym_Incref(sym);
     basesym->sy_peind = sym; /* Inherit reference. */
    }
   }
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
   /* Load relocations. Symbols can be addressed by-index
    * through 'SEC_SYMVEC' of the associated section. */
   for (iter = secv; iter != end; ++iter) {
    struct DCCSection *relo_section;
    struct DCCTextBuf relo_text;
    struct DCCSym **relsymv;
    size_t          relsymc,relcnt,final_relcnt;
    target_ptr_t    relo_sec_size;
    Elf(Rel) *rel_iter,*rel_end;
    struct DCCRel *dcc_rel;
    /* TODO: What about SHT_RELA? */
    if (iter->sh_type != SHT_REL) continue;
    if (iter->sh_info >= secc) {
     WARN(W_LIB_ELF_STATIC_REL_INVSECID,file,
         (size_t)iter->sh_info,
         (size_t)(iter-secv),secc);
    }
    relo_section = SEC_DCCSECI(iter->sh_info);
    if (!relo_section) {
     if (iter->sh_info < secc) {
      WARN(W_LIB_ELF_STATIC_REL_UNUSECID,file,
          (size_t)iter->sh_info,
          (size_t)(iter-secv));
     }
     continue;
    }
    if (iter->sh_link >= secc) {
     WARN(W_LIB_ELF_STATIC_REL_INVSYMID,file,
         (size_t)iter->sh_link,
         (size_t)(iter-secv),secc);
     continue;
    }
    if (secv[iter->sh_link].sh_type != SHT_SYMTAB) {
     WARN(W_LIB_ELF_STATIC_REL_NOSYMTAB,file,
         (size_t)iter->sh_link,
         (size_t)(iter-secv));
     continue;
    }
    /* Lookup the symbol vector of the associated symbol section. */
    relsymv = SEC_SYMVEC(&secv[iter->sh_link]);
    relsymc = SEC_SYMCNT(&secv[iter->sh_link]);
    /* Load the relocation text. */
    if (iter->sh_entsize == sizeof(Elf(Rel))) {
     elf_loadsection(iter,&relo_text,fd,start);
    } else if (!iter->sh_entsize) {
     continue; /* TODO: Warning? */
    } else {
     size_t i,common_size = sizeof(Elf(Rel));
     /* Adjust relocation data in 'relo_text'. */
     relcnt = iter->sh_size/iter->sh_entsize;
     relo_text.tb_begin = (uint8_t *)DCC_Malloc(relcnt*sizeof(Elf(Rel)),0);
     if unlikely(!relo_text.tb_begin) {
      memset(&relo_text,0,sizeof(struct DCCTextBuf));
      goto reloc_loaded;
     }
     if (iter->sh_entsize < common_size) {
      memset(relo_text.tb_begin,0,relcnt*sizeof(Elf(Rel)));
      common_size = iter->sh_entsize;
     }
     for (i = 0; i < relcnt; ++i) {
      s_seek(fd,start+iter->sh_offset+i*iter->sh_entsize,SEEK_SET);
      if (!s_reada(fd,relo_text.tb_begin+(i*sizeof(Elf(Rel))),common_size)) break;
     }
     if (i != relcnt) {
      relo_text.tb_end = (uint8_t *)DCC_Realloc(relo_text.tb_begin,
                                                i*sizeof(Elf(Rel)),0);
      if likely(relo_text.tb_end) relo_text.tb_begin = relo_text.tb_end;
     }
     relo_text.tb_max = relo_text.tb_pos =
     relo_text.tb_end = relo_text.tb_begin+i;
    }
reloc_loaded:
    /* At this point, we know all we need to load the relocations:
     *  - The ELF-compatible relocation text is stored in 'relo_text'
     *  - The symbol vector referred to is stored in 'relsymv|relsymc'
     *  - The section we're supposed to write the relocations to is stored in 'relo_section' */
    relcnt  = (size_t)(relo_text.tb_end-relo_text.tb_begin);
    relcnt /= sizeof(Elf(Rel));
    relo_sec_size = (target_ptr_t)(relo_section->sc_text.tb_max-
                                   relo_section->sc_text.tb_begin);
    if (relcnt &&
       (dcc_rel = (struct DCCRel *)DCCSection_Allocrel(relo_section,relcnt,0)) != NULL) {
     rel_end = (rel_iter = (Elf(Rel) *)relo_text.tb_begin)+relcnt;
     for (; rel_iter != rel_end; ++rel_iter) {
      rel_t relty; uint32_t symid; struct DCCSym *rsym;
      relty = ELF(R_TYPE)(rel_iter->r_info);
      symid = ELF(R_SYM)(rel_iter->r_info);
      if (rel_iter->r_offset >= relo_sec_size) {
invrelo: /* Warning: Invalid relocation */
       WARN(W_LIB_ELF_STATIC_INVRELOC,file,
           (unsigned int)relty,
           (unsigned int)symid,relo_section->sc_start.sy_name->k_name,
           (target_ptr_t)rel_iter->r_offset,
           (target_ptr_t)relo_sec_size);
       continue;
      }
      rsym = symid >= relsymc ? NULL : relsymv[symid];
      dcc_rel->r_addr = rel_iter->r_offset;
      if unlikely(!rsym) {
       if (!symid) goto absrel; /* symbol id ZERO(0) is interpreted as SHN_ABS::start. */
       if (relty != DCC_R_RELATIVE) goto invrelo;
       /* TODO: Determine which section this relocation points into,
        *       then convert it into a 'DCC_R_DATA_PTR' relocation
        *       against the start symbol of that section.
        *    >> If we fail to do this, we won't be able to
        *       safely append data to any section. */
absrel:
       dcc_rel->r_type = relty;
       dcc_rel->r_sym  = &DCCSection_Abs.sc_start;
      } else {
       dcc_rel->r_type = relty;
       dcc_rel->r_sym  = rsym;
      }
      DCCSym_Incref(dcc_rel->r_sym);
      ++dcc_rel;
     }
     /* Fix unused (aka. invalid) relocations. */
     final_relcnt = (size_t)(dcc_rel-relo_section->sc_relv);
     assert(final_relcnt <= relcnt);
     assert(relo_section->sc_relc >= relcnt);
     if (relcnt != final_relcnt) {
      memmove(relo_section->sc_relv+final_relcnt,
              relo_section->sc_relv+relcnt,
             (relo_section->sc_relc-relcnt)*
              sizeof(struct DCCRel));
      relo_section->sc_relc = final_relcnt;
     }
    }
    free(relo_text.tb_begin);
   }
   /* Free all symbol vectors previously allocated. */
   for (iter = secv; iter != end; ++iter) {
    if (iter->sh_type != SHT_SYMTAB) continue;
    free(SEC_SYMVEC(iter));
   }
#undef SEC_SYMCNTI
#undef SEC_SYMVECI
#undef SEC_DCCSECI
#undef SEC_SYMCNT
#undef SEC_SYMVEC
#undef SEC_DCCSEC
  }
  DCC_Free(secv);
 }

 if (ehdr.e_type != ET_REL) {
#if DCC_LIBFORMAT_ELF_STATIC
  struct DCCSection *sec;
  /* Linking against non-relocatable binary.
   * NOTE: If there were no relocations at all, we wouldn't have gotten here.
   *       This means that the binary is relocatable ('-fPIC'), but only as a whole,
   *       meaning that we must reverse engineer inter-section DISP relocations. */
  DCCUnit_ENUMSEC(sec) {
   if (sec->sc_start.sy_flags&DCC_SYMFLAG_SEC_X)
       x86_mkrel_textdisp(sec,0); /* ELF doesn't use whole-image base addresses! */
  }
  /* With all relocations loaded, we can now
   * delete the section base addresses! */
  DCCUnit_ENUMSEC(sec) {
   sec->sc_start.sy_flags &= ~(DCC_SYMFLAG_SEC_FIXED);
   sec->sc_base            = 0;
  }
#endif
  /* TODO: Load dynamic library dependencies ('DT_NEEDED' program header entires) */
 }

end: return result;
nodyn: not_dynamic = 1;
 /* If dynamic linking failed (for whatever reason), try to link statically (if allowed, to). */
 if (def->ld_flags&DCC_LIBDEF_FLAG_STATIC) { link_statically = 1; goto link_static; }
fail: result = 0,def->ld_dynlib = NULL; goto end;
}

DCC_DECL_END
#endif /* DCC_LIBFORMAT_ELF */

#endif /* !GUARD_DCC_UNIT_IMPORT_ELF_C_INL */
