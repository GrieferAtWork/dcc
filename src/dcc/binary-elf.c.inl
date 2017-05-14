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

DCC_DECL_BEGIN


/* NOTE: These ids also encode the order of sections! */
#define SECTY_INTERP    0 /* '.interp' */
#define SECTY_DYNSYM    1 /* '.dynsym' */
#define SECTY_STRTAB    2 /* '.dynstr' / '*strtab' */
#define SECTY_HASH      3 /* '.hash' */
#define SECTY_RELOC     4 /* '.rel' / '.rel.*' */
#define SECTY_OTHER     5 /* Anything else. */
#define SECTY_NOBITS    6 /* Anything that isn't allocated. */
#define SECTY_W_DYNSYM  7 /* WRITEABLE: SECTY_DYNSYM */
#define SECTY_W_STRTAB  8 /* WRITEABLE: SECTY_STRTAB */
#define SECTY_W_HASH    9 /* WRITEABLE: SECTY_HASH */
#define SECTY_W_RELOC  10 /* WRITEABLE: SECTY_RELOC */
#define SECTY_W_OTHER  11 /* WRITEABLE: SECTY_OTHER */
typedef unsigned int secty_t; /*< One of 'SECTY_*'. */
#define SECTY_COUNT    13 /*< Amount of section types (Not really a value) */
#define SECTY_UNKNOWN ((secty_t)-1) /* Unknown type */
PRIVATE secty_t secty_of(struct DCCSection const *__restrict section);


struct secinfo {
 struct DCCSection *si_sec;  /*< [1..1] Section associated with this information. */
 secty_t            si_type; /*< Section type (One of 'SECTY_*'; never 'SECTY_UNKNOWN') */
 Elf(Shdr)          si_hdr;  /*< ELF header, as later written to file. */
};

struct DCCElfInfo {
 size_t          elf_secc;   /*< Amount of sections contained in this ELF binary. */
 struct secinfo *elf_secv;   /*< [0..pe_secc][owned][sort(->si_type)] Vector of sections, sorted by the type.
                              *   NOTE: Empty sections are excluded from this list. */
 struct secinfo *elf_interp; /*< [0..1] Interpreter section (if needed). */
};

PRIVATE struct DCCElfInfo elf;
PRIVATE void elf_mk_interp(void);


#ifndef DCC_TARGET_ELFINTERP
#define DCC_TARGET_ELFINTERP  "/lib/ld-linux.so.2"
#endif







PRIVATE secty_t
secty_of(struct DCCSection const *__restrict section) {
 secty_t result;
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
 if (CHECK_NAME(".interp")) return SECTY_INTERP;
 if (CHECK_NAME(".dynsym")) { result = SECTY_DYNSYM; goto found; }
 if (CHECK_NAME(".dynstr") || CHECK_ENDSWITH("strtab")) { result = SECTY_STRTAB; goto found; }
 if (CHECK_NAME(".hash")) { result = SECTY_HASH; goto found; }
 if (CHECK_NAME(".rel") || CHECK_STARTSWITH(".rel.")) { result = SECTY_RELOC; goto found; }
 /* Ignore empty/unused sections. */
 if (section->sc_text.tb_max == section->sc_text.tb_begin) return SECTY_UNKNOWN;
 if (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_NOALLOC) return SECTY_NOBITS;
 result = SECTY_OTHER;
found:
 if (section->sc_start.sy_flags&DCC_SYMFLAG_SEC_W) result += 6;
 return result;
#undef CHECK_STARTSWITH
#undef CHECK_ENDSWITH
#undef CHECK_NAME
}




PRIVATE void elf_mk_interp(void) {
 struct DCCSection *sec;
 if (!(compiler.l_flags&(DCC_LINKER_FLAG_SHARED|
                         DCC_LINKER_FLAG_STATIC))) {
  char *itp = getenv("LD_SO"); /* TODO: Override with commandline-switch. */
  if (!itp) itp = DCC_TARGET_ELFINTERP;
  /* Create a section for the interpreter. */
  sec = DCCUnit_NewSecs(".interp",DCC_SYMFLAG_SEC(1,0,0,0,0,0));
  if (sec) {
   size_t itp_len = strlen(itp);
   DCCSection_DAllocMem(sec,itp,
                       (itp_len+0)*sizeof(char),
                       (itp_len+1)*sizeof(char),
                        1,0);
  }
 }
}


PUBLIC void
DCCBin_Generate(stream_t target) {
 memset(&elf,0,sizeof(elf));
 elf_mk_interp();

 (void)target;
}

DCC_DECL_END

#endif /* !GUARD_DCC_BINARY_ELF_C_INL */
