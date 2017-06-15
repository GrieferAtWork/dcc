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
#ifndef GUARD_DCC_UNIT_EXPORT_ELF_C_INL
#define GUARD_DCC_UNIT_EXPORT_ELF_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/stream.h>
#include <dcc/linker.h>

#include <string.h>

#include "unit-export.h"
#include "linker-elf.h"

DCC_DECL_BEGIN

struct elf_shdr {
 /*ref*/struct DCCSection *s_sec; /*< [0..1] DCC-section associated with this header.
                                   *   - The import section for import headers.
                                   *   - NULL for relocation/internal sections. */
 struct DCCTextBuf         s_buf; /*< [owned_if(s_sec == NULL)] Text buffer for this section. */
#ifdef __INTELLISENSE__
 Elf32_Shdr                s_hdr; /*< Representation of the section header. */
#else
 Elf(Shdr)                 s_hdr; /*< Representation of the section header. */
#endif
 target_ptr_t              s_oldaddr; /* Save 's_sec->sc_start.sy_addr' value, as the
                                       * field is (ab-)used for mapping sections
                                       * to their header indices.
                                       * NOTE: 'sy_elfid' cannot be used, as the
                                       *       section must also has a symbol index! */
};

INTDEF void *
DCCTextBuf_TAlloc_intern(struct DCCTextBuf *__restrict self,
                         target_siz_t size);

PRIVATE Elf(Word)
DCCTextBuf_AllocData(struct DCCTextBuf *__restrict self,
                     void const *__restrict p, size_t s) {
 void *buf = DCCTextBuf_TAlloc_intern(self,s);
 if unlikely(!buf) return 0;
 memcpy(buf,p,s);
 return (Elf(Word))((uint8_t *)buf-self->tb_begin);
}
PRIVATE Elf(Word)
DCCTextBuf_AllocString(struct DCCTextBuf *__restrict self,
                       char const *__restrict str, size_t len) {
 void *buf = DCCTextBuf_TAlloc_intern(self,(len+1)*sizeof(char));
 if unlikely(!buf) return 0;
 memcpy(buf,str,len*sizeof(char));
 ((char *)buf)[len] = '\0';
 return (Elf(Word))((uint8_t *)buf-self->tb_begin);
}

#define REL_PREFIX  ".rel"
#define A2L_PREFIX  ".DCC.a2l"

LOCAL Elf(Word)
DCCTextBuf_AllocRelString(struct DCCTextBuf *__restrict self,
                          char const *__restrict str, size_t len) {
 char *buf = (char *)DCCTextBuf_TAlloc_intern(self,(len*sizeof(char))+sizeof(REL_PREFIX));
 if unlikely(!buf) return 0;
 memcpy(buf,REL_PREFIX,sizeof(REL_PREFIX)-sizeof(char));
 memcpy(buf+DCC_COMPILER_STRLEN(REL_PREFIX),str,len*sizeof(char));
 buf[len+DCC_COMPILER_STRLEN(REL_PREFIX)] = '\0';
 return (Elf(Word))((uint8_t *)buf-self->tb_begin);
}

LOCAL Elf(Word)
DCCTextBuf_AllocA2lString(struct DCCTextBuf *__restrict self,
                          char const *__restrict str, size_t len) {
 char *buf = (char *)DCCTextBuf_TAlloc_intern(self,(len*sizeof(char))+sizeof(A2L_PREFIX));
 if unlikely(!buf) return 0;
 memcpy(buf,A2L_PREFIX,sizeof(A2L_PREFIX)-sizeof(char));
 memcpy(buf+DCC_COMPILER_STRLEN(A2L_PREFIX),str,len*sizeof(char));
 buf[len+DCC_COMPILER_STRLEN(A2L_PREFIX)] = '\0';
 return (Elf(Word))((uint8_t *)buf-self->tb_begin);
}


#if (DCC_SYMFLAG_NONE      == STV_DEFAULT) && \
    (DCC_SYMFLAG_INTERNAL  == STV_INTERNAL) && \
    (DCC_SYMFLAG_HIDDEN    == STV_HIDDEN) && \
    (DCC_SYMFLAG_PROTECTED == STV_PROTECTED)
#define VISMAP(x) x
#else
#define VISMAP(x) vismap[x]
PRIVATE uint8_t const vismap[4] = {
 /* [DCC_SYMFLAG_NONE     ] = */STV_DEFAULT,
 /* [DCC_SYMFLAG_INTERNAL ] = */STV_INTERNAL,
 /* [DCC_SYMFLAG_HIDDEN   ] = */STV_HIDDEN,
 /* [DCC_SYMFLAG_PROTECTED] = */STV_PROTECTED,
};
#endif


INTERN void DCCUNIT_EXPORTCALL
DCCUnit_ExportElf(struct DCCExpDef *__restrict def,
                  stream_t fd) {
 struct DCCSection *sec;
 struct DCCSym *sym;
 size_t shdr_regc; /* Amount of regular section headers. */
 size_t shdr_impc; /* Amount of import section headers. */
 size_t shdr_relc; /* Amount of relocation section headers. */
 size_t shdr_a2lc; /* Amount of Addr2Line section headers. */
 size_t shdr_intc; /* Amount of internal section headers. */
 size_t shdrc;     /* Sum of 'shdr_regc+shdr_relc'. */
 /* Vector for section headers. Layout:
  * 0:                                                    NULL section.
  * 1..shdr_regc:                                         Regular sections.
  * 1+shdr_regc..shdr_regc+shdr_impc:                     Import sections.
  * 1+shdr_regc+shdr_impc..shdr_regc+shdr_impc+shdr_relc: Relocation sections.
  * 1+shdr_regc+shdr_impc+shdr_relc..shdrc:               Internal sections.
  */
 struct elf_shdr *shdrv;
 struct elf_shdr *shdr_regv; /* == shdrv+1 */
 struct elf_shdr *shdr_impv; /* == shdrv+1+shdr_regc */
 struct elf_shdr *shdr_relv; /* == shdrv+1+shdr_regc+shdr_impc */
 struct elf_shdr *shdr_a2lv; /* == shdrv+1+shdr_regc+shdr_impc+shdr_relc */
 struct elf_shdr *shdr_intv; /* == shdrv+1+shdr_regc+shdr_impc+shdr_relc+shdr_a2lc */
 struct elf_shdr *shdr_a2l_iter,*shdr_iter,*shdr_end;
#define SHDR_SHSTRTAB   (&shdr_intv[0])
#define SHDR_SYMTAB     (&shdr_intv[1])
#define SHDR_STRTAB     (&shdr_intv[2])
#define SHDR_SYMFLG     (&shdr_intv[3])

#define ELF_SYMIDX(sym) ((sym)->sy_elfid) /* Return the symbol's '.strtab' index */
#define ELF_SHDR_IDX(h) ((Elf(Section))((h)-shdrv))
 assert(def);
 shdr_regc = unit.u_secc,shdr_relc = 0;
 DCCUnit_ENUMSEC(sec) if (sec->sc_relc) ++shdr_relc;
 assert(shdr_relc <= shdr_regc);
 shdr_a2lc = 0;
 shdr_intc = 3; /* '.shstrtab', '.symtab', '.strtab' */
 if (!(def->ed_flags&DCC_EXPFLAG_ELF_NOEXT)) {
  ++shdr_intc; /* '.DCC.symflg' */
  DCCUnit_ENUMSEC(sec) if (sec->sc_a2l.d_chunkc) ++shdr_a2lc; /* '.DCC.a2l.<BASE_SECTION>' */
 }

 shdr_impc = (def->ed_flags&DCC_EXPFLAG_ELF_NOEXT) ? 0 : unit.u_impc; /* Import section headers. */
 shdrc = 1+shdr_regc+shdr_impc+shdr_relc+shdr_a2lc+shdr_intc;
 shdrv = (struct elf_shdr *)DCC_Calloc(shdrc*sizeof(struct elf_shdr),0);
 if unlikely(!shdrv) return;
 shdr_regv = shdrv+1;
 shdr_impv = shdr_regv+shdr_regc;
 shdr_relv = shdr_impv+shdr_impc;
 shdr_a2lv = shdr_relv+shdr_relc;
 shdr_intv = shdr_a2lv+shdr_a2lc;

 /* Allocate 1 ZERO-byte at the star of '.shstrtab' and '.strtab' */
 DCCTextBuf_AllocData(&SHDR_SHSTRTAB->s_buf,"",1);
 DCCTextBuf_AllocData(&SHDR_STRTAB->s_buf,"",1);

 /* Define special section settings. */
 SHDR_SHSTRTAB->s_hdr.sh_name      = DCCTextBuf_AllocString(&SHDR_SHSTRTAB->s_buf,".shstrtab",9);
 SHDR_SHSTRTAB->s_hdr.sh_type      = SHT_STRTAB;
 SHDR_SHSTRTAB->s_hdr.sh_flags     = SHF_STRINGS;
 SHDR_SHSTRTAB->s_hdr.sh_addralign = 1;

 SHDR_SYMTAB->s_hdr.sh_name        = DCCTextBuf_AllocString(&SHDR_SHSTRTAB->s_buf,".symtab",7);
 SHDR_SYMTAB->s_hdr.sh_type        = SHT_SYMTAB;
 SHDR_SYMTAB->s_hdr.sh_link        = ELF_SHDR_IDX(SHDR_STRTAB);
 SHDR_SYMTAB->s_hdr.sh_info        = ELF_SHDR_IDX(SHDR_SHSTRTAB); /* I don't really get this one, but this should be standard-conforming. */
 SHDR_SYMTAB->s_hdr.sh_entsize     = sizeof(Elf(Sym));
 SHDR_SYMTAB->s_hdr.sh_addralign   = DCC_COMPILER_ALIGNOF(Elf(Sym));

 SHDR_STRTAB->s_hdr.sh_name        = DCCTextBuf_AllocString(&SHDR_SHSTRTAB->s_buf,".strtab",7);
 SHDR_STRTAB->s_hdr.sh_type        = SHT_STRTAB;
 SHDR_STRTAB->s_hdr.sh_flags       = SHF_STRINGS;
 SHDR_STRTAB->s_hdr.sh_addralign   = 1;

 /* Load import sections. */
 if (!(def->ed_flags&DCC_EXPFLAG_ELF_NOEXT)) {
  shdr_iter = shdr_impv;
  DCCUnit_ENUMIMP(sec) {
   assert(shdr_iter < shdr_impv+shdr_impc);
   shdr_iter->s_oldaddr = sec->sc_start.sy_addr;
   shdr_iter->s_hdr.sh_name = DCCTextBuf_AllocString(&SHDR_SHSTRTAB->s_buf,
                                                     sec->sc_start.sy_name->k_name,
                                                     sec->sc_start.sy_name->k_size);
   /* Declare an 'SHT_DCC_IMPSEC' section. */
   shdr_iter->s_hdr.sh_type = SHT_DCC_IMPSEC;
   /* Currently, we simply re-use the section name as dependency name. */
   shdr_iter->s_hdr.sh_link = ELF_SHDR_IDX(SHDR_SHSTRTAB);
   shdr_iter->s_hdr.sh_info = shdr_iter->s_hdr.sh_name;
   sec->sc_start.sy_addr = ELF_SHDR_IDX(shdr_iter);
   ++shdr_iter;
  }
  assert(shdr_iter == shdr_impv+shdr_impc);
 }

 /* Load regular sections. */
 shdr_iter = shdr_regv;
 shdr_a2l_iter = shdr_a2lv;
 DCCUnit_ENUMSEC(sec) {
  symflag_t secflags = sec->sc_start.sy_flags;
  assert(shdr_iter < shdr_regv+shdr_regc);
  shdr_iter->s_sec              = sec;
  shdr_iter->s_buf              = sec->sc_text;
  shdr_iter->s_oldaddr          = sec->sc_start.sy_addr;
  shdr_iter->s_hdr.sh_name      = DCCTextBuf_AllocString(&SHDR_SHSTRTAB->s_buf,
                                                         sec->sc_start.sy_name->k_name,
                                                         sec->sc_start.sy_name->k_size);
  shdr_iter->s_hdr.sh_type      = !(secflags&DCC_SYMFLAG_SEC_NOALLOC) ? SHT_PROGBITS :
                                   (secflags&DCC_SYMFLAG_USED) ? SHT_NOTE : SHT_NOBITS;
  if (secflags&DCC_SYMFLAG_SEC_W) shdr_iter->s_hdr.sh_flags |= SHF_WRITE;
  if (!(secflags&DCC_SYMFLAG_SEC_NOALLOC)) shdr_iter->s_hdr.sh_flags |= SHF_ALLOC;
  if (secflags&DCC_SYMFLAG_SEC_X) shdr_iter->s_hdr.sh_flags |= SHF_EXECINSTR;
  if (secflags&DCC_SYMFLAG_SEC_M) shdr_iter->s_hdr.sh_flags |= SHF_MERGE;
  shdr_iter->s_hdr.sh_addralign = sec->sc_start.sy_align;
  sec->sc_start.sy_addr = ELF_SHDR_IDX(shdr_iter);
  if (sec->sc_a2l.d_chunkc &&
    !(def->ed_flags&DCC_EXPFLAG_ELF_NOEXT)) {
   a2l_op_t *a2l_code; size_t a2l_size;
   /* Must generate an A2L section. */
   a2l_code = DCCA2l_Link(&sec->sc_a2l,&a2l_size);
   if unlikely(!a2l_code) a2l_size = 0;
   /* Directly inherit linked A2L code. */
   shdr_a2l_iter->s_buf.tb_begin     = (uint8_t *)a2l_code;
   shdr_a2l_iter->s_buf.tb_pos       =
   shdr_a2l_iter->s_buf.tb_max       =
   shdr_a2l_iter->s_buf.tb_end       = (uint8_t *)a2l_code+a2l_size;
   /* Fill in ELF header information. */
   shdr_a2l_iter->s_hdr.sh_name      = DCCTextBuf_AllocA2lString(&SHDR_SHSTRTAB->s_buf,
                                                                 sec->sc_start.sy_name->k_name,
                                                                 sec->sc_start.sy_name->k_size);
   shdr_a2l_iter->s_hdr.sh_type      = SHT_DCC_ADDR2LINE;
   shdr_a2l_iter->s_hdr.sh_size      = a2l_size;
   shdr_a2l_iter->s_hdr.sh_link      = ELF_SHDR_IDX(shdr_iter);
   shdr_a2l_iter->s_hdr.sh_addralign = DCC_COMPILER_ALIGNOF(a2l_op_t);
   shdr_a2l_iter->s_hdr.sh_entsize   = sizeof(a2l_op_t);
   ++shdr_a2l_iter;
  }
  ++shdr_iter;
 }
 assert(shdr_a2l_iter == shdr_a2lv+shdr_a2lc);
 assert(shdr_iter == shdr_regv+shdr_regc);

 {
  uint32_t symid = 1; Elf(Sym) symhdr;
  uint32_t need_symflags = 0;
  /* Allocate the NULL-symbol at index ZERO. */
  void *null_sym = DCCTextBuf_TAlloc_intern(&SHDR_SYMTAB->s_buf,
                                            sizeof(Elf(Sym)));
  if (null_sym) memset(null_sym,0,sizeof(Elf(Sym)));
  /* Load all symbols into 'SHDR_SYMTAB' (Both those named & those unnamed!) */
  DCCUnit_ENUMALLSYM(sym) {
   struct DCCSymAddr symaddr;
   unsigned char bind,type;
   if (DCCSym_ISSECTION(sym)) type = STT_SECTION;
   else                       type = STT_NOTYPE;
        if (sym->sy_flags&DCC_SYMFLAG_STATIC) bind = STB_LOCAL;
   else if (sym->sy_flags&DCC_SYMFLAG_WEAK)   bind = STB_WEAK;
   else                                       bind = STB_GLOBAL;
   if (sym->sy_name == &TPPKeyword_Empty)
        symhdr.st_name = 0;
   else symhdr.st_name = DCCTextBuf_AllocString(&SHDR_STRTAB->s_buf,
                                                sym->sy_name->k_name,
                                                sym->sy_name->k_size);
   symhdr.st_info  = ELF(ST_INFO)(bind,type);
   symhdr.st_other = ELF(ST_VISIBILITY)(VISMAP(sym->sy_flags&DCC_SYMFLAG_VISIBILITY));
   sym->sy_elfid   = symid++;
   if (!(def->ed_flags&DCC_EXPFLAG_ELF_NOEXT) ||
       !DCCSym_LoadAddr(sym,&symaddr,0)) {
    symaddr.sa_off = 0;
    symaddr.sa_sym = sym;
   }
   symhdr.st_value = symaddr.sa_off+symaddr.sa_sym->sy_addr;
   symhdr.st_size  = symaddr.sa_sym->sy_size;
   if ((sym->sy_flags&(DCC_SYMFLAG_USED|DCC_SYMFLAG_UNUSED|DCC_SYMFLAG_NOCOLL)) ||
       (sym->sy_align > 1)) need_symflags = sym->sy_elfid;
   if (symaddr.sa_sym->sy_sec) {
    if (symaddr.sa_sym->sy_sec == &DCCSection_Abs)
     symhdr.st_shndx = SHN_ABS;
    else if (!(def->ed_flags&DCC_EXPFLAG_ELF_NOEXT) ||
             /* Allow import indices only when ELF extensions are allowed. */
             !DCCSection_ISIMPORT(symaddr.sa_sym->sy_sec)) {
     symhdr.st_shndx = (Elf(Section))symaddr.sa_sym->sy_sec->sc_start.sy_addr;
    } else {
     assert(symaddr.sa_sym->sy_name != &TPPKeyword_Empty);
     symhdr.st_shndx = SHN_UNDEF;
    }
   } else {
    if (symaddr.sa_sym->sy_alias) {
     /* XXX: warn if ELF extensions are disabled. */
     need_symflags = sym->sy_elfid;
    } else if (symaddr.sa_sym->sy_name == &TPPKeyword_Empty) {
     /* An unnamed, undefined symbol appears in the object file.
      * >> This is bad and probably means that some internal symbol wasn't defined... */
     WARN(W_EXPORT_UNDEFINED_UNNAMED_SYMBOL,sym->sy_elfid);
    }
    symhdr.st_shndx = SHN_UNDEF;
   }
   DCCTextBuf_AllocData(&SHDR_SYMTAB->s_buf,&symhdr,sizeof(symhdr));
  }
#ifdef SHT_DCC_SYMFLG
  if (need_symflags && !(def->ed_flags&DCC_EXPFLAG_ELF_NOEXT)) {
   Elf(DCCSymFlg) *buf;
   SHDR_SYMFLG->s_hdr.sh_name      = DCCTextBuf_AllocString(&SHDR_SHSTRTAB->s_buf,".DCC.symflg",11);
   SHDR_SYMFLG->s_hdr.sh_type      = SHT_DCC_SYMFLG;
   SHDR_SYMFLG->s_hdr.sh_link      = ELF_SHDR_IDX(SHDR_SYMTAB);
   SHDR_SYMFLG->s_hdr.sh_entsize   = sizeof(Elf(DCCSymFlg));
   SHDR_SYMFLG->s_hdr.sh_addralign = DCC_COMPILER_ALIGNOF(Elf(DCCSymFlg));
   buf = (Elf(DCCSymFlg) *)DCCTextBuf_TAlloc_intern(&SHDR_SYMFLG->s_buf,
                                                   (need_symflags+1)*
                                                    sizeof(Elf(DCCSymFlg)));
   if likely(buf) {
    memset(buf,0,need_symflags*sizeof(Elf(DCCSymFlg)));
    ++buf; /* Skip the first entry, which refers to the NULL-symbol */
    DCCUnit_ENUMALLSYM(sym) {
#if ELF_DCC_SYMFLAG_F_NOCOLL == DCC_SYMFLAG_NOCOLL && \
    ELF_DCC_SYMFLAG_F_USED   == DCC_SYMFLAG_USED && \
    ELF_DCC_SYMFLAG_F_UNUSED == DCC_SYMFLAG_UNUSED
     buf->sf_info = (sym->sy_flags&(DCC_SYMFLAG_USED|DCC_SYMFLAG_UNUSED|DCC_SYMFLAG_NOCOLL));
#else
     if (flags&DCC_SYMFLAG_NOCOLL) buf->sf_info |= ELF_DCC_SYMFLAG_F_NOCOLL;
     if (flags&DCC_SYMFLAG_USED)   buf->sf_info |= ELF_DCC_SYMFLAG_F_USED;
     if (flags&DCC_SYMFLAG_UNUSED) buf->sf_info |= ELF_DCC_SYMFLAG_F_UNUSED;
#endif
     buf->sf_align = (Elf(Word))sym->sy_align;
     if (sym->sy_alias) { /* Generate an alias descriptor. */
      buf->sf_info |= ELF(DCC_SYMFLAG)(sym->sy_alias->sy_elfid,
                                       ELF_DCC_SYMFLAG_F_ALIAS);
     }
     if (!--need_symflags) goto done_symflg;
     ++buf;
    }
   }
done_symflg:;
  }
#endif
 }

 /* Generate relocation sections. */
 shdr_iter = shdr_relv;
 DCCUnit_ENUMSEC(sec) {
  Elf(Rel) *rel_data;
  struct DCCRel *rel_iter,*rel_end;
  if (!sec->sc_relc) continue;
  assert(shdr_iter < shdr_relv+shdr_relc);
  shdr_iter->s_hdr.sh_name = DCCTextBuf_AllocRelString(&SHDR_SHSTRTAB->s_buf,
                                                       sec->sc_start.sy_name->k_name,
                                                       sec->sc_start.sy_name->k_size);
  shdr_iter->s_hdr.sh_type      = SHT_REL;
  shdr_iter->s_hdr.sh_flags     = SHF_INFO_LINK;
  shdr_iter->s_hdr.sh_link      = ELF_SHDR_IDX(SHDR_SYMTAB);
  assert(sec->sc_start.sy_addr < (shdr_regc+1));
  shdr_iter->s_hdr.sh_info      = (Elf(Word))sec->sc_start.sy_addr;
  shdr_iter->s_hdr.sh_entsize   = sizeof(Elf(Rel));
  shdr_iter->s_hdr.sh_addralign = DCC_COMPILER_ALIGNOF(Elf(Rel));
  rel_data = (Elf(Rel) *)DCCTextBuf_TAlloc_intern(&shdr_iter->s_buf,
                                                  sec->sc_relc*
                                                  sizeof(Elf(Rel)));
  if unlikely(!rel_data) continue;
  rel_end = (rel_iter = sec->sc_relv)+sec->sc_relc;
  for (; rel_iter != rel_end; ++rel_iter,++rel_data) {
   uint32_t symid;
   if (rel_iter->r_sym == &DCCSection_Abs.sc_start) {
    symid = 0; /* ??? */
   } else {
    symid = (assert(rel_iter->r_sym),ELF_SYMIDX(rel_iter->r_sym));
    assertf(symid != 0,"Unmapped reference to '%s' in '%s'",
            rel_iter->r_sym->sy_name->k_name,
            sec->sc_start.sy_name->k_name);
   }
   rel_data->r_info   = ELF(R_INFO)(symid,rel_iter->r_type);
   rel_data->r_offset = rel_iter->r_addr;
  }
  ++shdr_iter;
 }
 assert(shdr_iter == shdr_relv+shdr_relc);
#define EHDR_ADDR  0
#define SHDR_ADDR  ((sizeof(Elf(Ehdr))+(DCC_COMPILER_ALIGNOF(Elf(Shdr))-1)) & \
                                      ~(DCC_COMPILER_ALIGNOF(Elf(Shdr))-1))
#define DATA_ADDR  (SHDR_ADDR+(shdrc*sizeof(Elf(Shdr))))

 { /* Assign section file offsets. */
   Elf(Off) fileaddr = DATA_ADDR; /* Start after the sections headers. */
   shdr_end = (shdr_iter = shdr_regv)+(shdrc-1);
   for (; shdr_iter != shdr_end; ++shdr_iter) {
    /* Optional: Align the file pointer by the alignment of a section header. */
    shdr_iter->s_hdr.sh_offset = fileaddr;
    shdr_iter->s_hdr.sh_size   = (Elf32_Word)(shdr_iter->s_buf.tb_max-
                                              shdr_iter->s_buf.tb_begin);
    fileaddr += shdr_iter->s_hdr.sh_size;
   }
 }

 if (OK) /* Don't write the file if something went wrong! */
 { /* Write everything to file. */
   Elf(Ehdr) ehdr;
   memset(&ehdr,0,sizeof(ehdr));
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
   ehdr.e_type              = ET_REL;
   ehdr.e_version           = DCC_TARGET_ELFVERSION;
   ehdr.e_shoff             = SHDR_ADDR;
   ehdr.e_ehsize            = sizeof(Elf(Ehdr));
   ehdr.e_shentsize         = sizeof(Elf(Shdr));
   ehdr.e_shnum             = (Elf(Half))shdrc;
   ehdr.e_shstrndx          = ELF_SHDR_IDX(SHDR_SHSTRTAB);
   s_writea(fd,&ehdr,sizeof(ehdr));

   /* Write all section headers. */
   if DCC_MACRO_COND(SHDR_ADDR != sizeof(Elf(Ehdr))) DCCStream_PadAddr(fd,SHDR_ADDR);
   shdr_end = (shdr_iter = shdrv)+shdrc;
   for (; shdr_iter != shdr_end; ++shdr_iter) {
    s_writea(fd,&shdr_iter->s_hdr,sizeof(Elf(Shdr)));
   }
   /* Write all sections. */
   for (shdr_iter = shdrv; shdr_iter != shdr_end; ++shdr_iter) {
    uint8_t *alloc_end;
    /* Skip empty sections (e.g.: import sections) */
    if (shdr_iter->s_buf.tb_begin == shdr_iter->s_buf.tb_max) continue;
    DCCStream_PadAddr(fd,shdr_iter->s_hdr.sh_offset);
    alloc_end = shdr_iter->s_buf.tb_end;
    if (alloc_end > shdr_iter->s_buf.tb_max)
        alloc_end = shdr_iter->s_buf.tb_max;
    s_writea(fd,shdr_iter->s_buf.tb_begin,
            (size_t)(alloc_end-shdr_iter->s_buf.tb_begin));
    assert(alloc_end <= shdr_iter->s_buf.tb_max);
    if (alloc_end != shdr_iter->s_buf.tb_max)
        DCCStream_PadSize(fd,(size_t)(shdr_iter->s_buf.tb_max-alloc_end));
   }
 }

 /* Cleanup/restore */
 shdr_impv = (shdr_iter = shdrv)+shdrc;
 for (; shdr_iter != shdr_impv; ++shdr_iter) {
  if (shdr_iter->s_sec)
      shdr_iter->s_sec->sc_start.sy_addr = shdr_iter->s_oldaddr;
  else free(shdr_iter->s_buf.tb_begin);
 }
 DCC_Free(shdrv);
#undef ELF_SYMIDX
#undef ELF_SHDR_IDX
#undef SHDR_STRTAB
#undef SHDR_SYMTAB
#undef SHDR_SHSTRTAB
}

DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_EXPORT_ELF_C_INL */
