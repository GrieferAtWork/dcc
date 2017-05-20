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


INTERN int DCCUNIT_IMPORTCALL
DCCUnit_LoadELF(struct DCCLibDef *__restrict def,
                char const *__restrict file, stream_t fd) {
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
    s_seek(fd,ehdr.e_phoff,SEEK_SET);
    read_error = s_read(fd,phdr_data,phdr_size);
    if (read_error < 0) goto fail_dyn;
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
     if (read_error < 0) goto fail_dyn;
     if ((size_t)read_error < common_size) { phdr_size = i; break; }
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
     /* TODO: Does ELF guaranty that PT_DYNAMIC contains proper offset information?
      *       Or do we have to find a PT_LOAD header containing the PT_DYNAMIC virtual address? */
     s_seek(fd,iter->p_offset,SEEK_SET);
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
        s_seek(fd,hash_off+4,SEEK_SET);
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
        if (!elf_gnuhash_findmaxsym(&symtab_siz,gnuhash_off,gnuhash_siz,fd,
                                     ehdr.e_ident[EI_CLASS] == ELFCLASS64)
             ) goto no_dynhash;
       } else {
no_dynhash:;
        /* TODO: Try to load the section list and locate the section associated
         *       with 'symtab_off'. Then, using that section's 'sh_size' field,
         *       we can truncate the max amount of symbols.
         * >> When that fails too (such as when there are no sections), emit a warning
         *    and perform special checks when iterating the symbol vector, stopping
         *    at the first symbol describing invalid data (e.g.: an invalid section id, or a corrupt name) */
       }
       /* Constrict the max symbol table size using the PT_DYNAMIC header. */
       if (dt_strsz < strtab_siz) strtab_siz = dt_strsz;
       /* Allocate & read the string table. */
       strtab = (char *)DCC_Malloc(strtab_siz+1,0);
       if unlikely(!strtab) goto next_dynhdr;
       /* Make sure the string table is ZERO-terminated,
        * so we can safely use strlen on its elements. */
       strtab[strtab_siz] = '\0';
       s_seek(fd,strtab_off,SEEK_SET);
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
         s_seek(fd,symtab_off,SEEK_SET);
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
          s_seek(fd,symtab_off+i*dt_syment,SEEK_SET);
          read_error = s_read(fd,&symtab[i],dt_syment);
          if (read_error < (ptrdiff_t)dt_syment) { symcnt = i; break; }
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
              (size_t)symtab_siz);
          continue;
         }
         sym = DCCUnit_NewSyms(symname,DCCLibDef_EXPFLAGS(def,flags));
         /* NOTE: Only (re-)define a symbol when it is forward,
          *       or when the library definition isn't weak. */
         if (sym && (DCCSym_ISFORWARD(sym) || !(flags&DCC_SYMFLAG_WEAK))) {
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

 /* TODO: Link static ELF binary. */
 result         = 0;
 def->ld_dynlib = NULL;


end: return result;
nodyn: not_dynamic = 1;
 /* If dynamic linking failed (for whatever reason), try to link statically (if allowed, to). */
 if (def->ld_flags&DCC_LIBDEF_FLAG_STATIC) { link_statically = 1; goto link_static; }
fail: result = 0,def->ld_dynlib = NULL; goto end;
}

DCC_DECL_END
#endif /* DCC_LIBFORMAT_ELF */

#endif /* !GUARD_DCC_UNIT_IMPORT_ELF_C_INL */
