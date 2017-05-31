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
#ifndef GUARD_DCC_LINKER_H
#define GUARD_DCC_LINKER_H 1

#include "common.h"
#include "target.h"
#include "stream.h"
#include "unit.h"

DCC_DECL_BEGIN

/* ELF extensions used in DCC object files. */

/* Used for dynamic linking in DCC ELF object files:
 *  - The 'Elf(Shdr)' fields are defined as follows:
 *      'sh_name':      The link-time name of the library hook (offset into the '.shstrtab' section)
 *      'sh_type':      <SHT_DCC_IMPSEC>
 *      'sh_flags':     <0>
 *      'sh_addr':      Undefined
 *      'sh_offset':    Undefined
 *      'sh_size':      <0>
 *      'sh_link':      Index of a '.strtab' section containing the dependency name.
 *      'sh_info':      String table offset within 'sh_link', pointing at the dependency name.
 *      'sh_addralign': Undefined
 *      'sh_entsize':   Undefined
 *  - Symbols linked against a section of type 'SHT_DCC_IMPSEC'
 *    are imported from the associated dynamic library.
 *  - Note that this section type should only appear in binaries of type 'ET_REL'
 *  - Note that 'SHT_DCC_IMPSEC'-typed sections are optional and mainly
 *    used to implemented '__attribute__((lib(...)))' in object files.
 *    NOTE: Use in executable files will lead to run-time undefined behavior,
 *          as the linker will not understand that a symbol located inside a
 *          SHT_DCC_IMPSEC-section must be located the same way it must be
 *          if its 'st_shndx' member was set to 'SHN_UNDEF'.
 *  - Unlike other symbols imported from dynamic libraries, that have 'SHN_UNDEF'
 *    set as their associated section, setting a section of type 'SHT_DCC_IMPSEC'
 *    will exclude the symbol from being considered as undefined.
 */
#define SHT_DCC_IMPSEC (SHT_LOUSER+0x0305)

/* Used for symbol aliasing & extended flags in DCC ELF object files:
 *  - The 'Elf(Shdr)' fields are defined as follows:
 *      'sh_name':      Offset in '.shstrtab' - ".DCC.symflg"
 *      'sh_type':      <SHT_DCC_SYMFLG>
 *      'sh_flags':     <0>
 *      'sh_addr':      <0>
 *      'sh_offset':    Absolute file-offset of the symbol flag table.
 *      'sh_size':      Size of the section's data (in bytes).
 *      'sh_link':      Index of a '.symtab' section, who's symbols are extended by this section.
 *      'sh_info':      Undefined
 *      'sh_addralign': DCC_COMPILER_ALIGNOF(Elf(DCCSymFlg))
 *      'sh_entsize':   sizeof(Elf(DCCSymFlg))
 *  - Using this extension, DCC is able to retain dependency tree information
 *    caused by symbol aliasing, as well as less important symbol flags,
 *    such as 'DCC_SYMFLAG_USED'
 *  - NOTE: The original symbol value acts as an alias offset
 */
#define SHT_DCC_SYMFLG (SHT_LOUSER+0x0306)

typedef struct {
  Elf32_Word sf_info;  /*< Extended symbol information. */
  Elf32_Word sf_align; /*< Minimum alignment required by this symbol. */
} Elf32_DCCSymFlg;

typedef struct {
  Elf64_Word sf_info;  /*< Extended symbol information. */
  Elf64_Word sf_align; /*< Minimum alignment required by this symbol. */
} Elf64_DCCSymFlg;

#define ELF_DCC_SYMFLAG_F_ALIAS  0x01 /*< The symbol is aliasing another symbol retrievable through 'ELF(32|64)_DCC_SYMFLAG_SYM(...)'.
                                       *  The symbol id being aliased is apart of the same symbol table extended by the associated section. */
#define ELF_DCC_SYMFLAG_F_NOCOLL 0x20 /*< Don't allow this symbol to be collapsed during late symbol merging. */
#define ELF_DCC_SYMFLAG_F_USED   0x40 /*< The symbol is used and shall not be removed, even when it appears unused. */
#define ELF_DCC_SYMFLAG_F_UNUSED 0x80 /*< The symbol is intended to be unused and no warning shall be emit when it is removed. */

/* Helper macros describing how to insert/extract data from 'sf_info' */
#define ELF32_DCC_SYMFLAG_SYM(info)   ((info) >> 8)
#define ELF32_DCC_SYMFLAG_FLAGS(info) ((uint8_t)(info))
#define ELF32_DCC_SYMFLAG(sym,flags)  ((sym) << 8 | (uint8_t)(flags))

#define ELF64_DCC_SYMFLAG_SYM   ELF32_DCC_SYMFLAG_SYM
#define ELF64_DCC_SYMFLAG_FLAGS ELF32_DCC_SYMFLAG_FLAGS
#define ELF64_DCC_SYMFLAG       ELF32_DCC_SYMFLAG




struct DCCLibPaths {
 struct DCCLibPaths       *lp_prev;  /*< [0..1][owned] Previous list of library paths. */
 size_t                    lp_patha; /*< Allocated amount of library paths. */
 size_t                    lp_pathc; /*< Amount of elements in the vector below (may be empty). */
 /*ref*/struct TPPString **lp_pathv; /*< [1..1][0..il_pathc][owned] Vector of sanitized library paths. */
};


#define DCC_LINKER_FLAG_SHARED       0x00000001 /*< Create shared libraries. */
#define DCC_LINKER_FLAG_NOSTDLIB     0x00000002 /*< Don't include standard libraries. */
#define DCC_LINKER_FLAG_NOUNDERSCORE 0x00000004 /*< Don't prepend leading underscores. */
#define DCC_LINKER_FLAG_PIC          0x00000008 /*< Generate relocations, meaning that a generated image will not be position-independent. */
#define DCC_LINKER_FLAG_SYMBOLIC     0x00000010 /*< Set if '-Wl,-Bsymbolic' was passed on the commandline. */
#define DCC_LINKER_FLAG_IMGBASE      0x00000020 /*< Set when an explicit image base has been stored in 'l_imgbase'. */
#define DCC_LINKER_FLAG_LIBSYMREDEF  0x00000040 /*< When linking against multiple libraries exporting the same symbol, still allow redefinition of use the newest version.
                                                 *  This option is not normally set, as it usually just widens the scope of library dependencies, such as when linking against
                                                 * 'ntdll.dll' on windows, which happens to export its own version of 'strlen' (which is probably already linked against 'msvcrt.dll')
                                                 *  NOTE: This flag is implicitly set for the definition of a symbol with a [[lib(...)]] attribute.
                                                 *  NOTE: Depending on this flag an import-redefinition emits 'W_SYMBOL_ALREADY_DEFINED_IMP_IMP/W_SYMBOL_ALREADY_DEFINED_IMP_IMP_NOT'. */
#define DCC_LINKER_FLAG_O_NOMERGESYM 0x00010000 /*< OPTIMIZATION: Don't merge symbols during compilation (e.g.: same-string optimizations). */
#define DCC_LINKER_FLAG_O_CLRUNUSED  0x00020000 /*< OPTIMIZATION: Clear unused symbols during the last phases of linking (whole-program optimization). */
#define DCC_LINKER_FLAG_O_COLLSEC    0x00040000 /*< OPTIMIZATION: Collapse & merge section symbols to reclaim unused memory. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_LINKER_FLAG_PEDYNAMIC     0x10000000 /*< On PE binary targets: Consider C/ELF visibility when generating an export table.
                                                  *  >> When this flag is set, any non-static definition matching '__attribute__((visibility("default")))'
                                                  *     will be exported, in addition to symbols marked with '__attribute__((dllexport))'. */
#define DCC_LINKER_FLAG_PEDYNAMIC_FWD 0x20000000 /*< On PE binary targets: Force enable dynamic export for symbols imported from other libraries, creating proxy declarations.
                                                  *  NOTE: Proxy declarations can always be created by declaring a symbol with both 'dllimport' and 'dllexport'!
                                                  *  WARNING: You probably don't want to enable this flag, because something as simple as '#pragma comment(lib,"ntdll.dll")'
                                                  *           will cause _ALL_ symbols from ntdll to be both imported _AND_ exported from your application (It'll work, but it's a damn stupid thing to do...) */
#endif
#if DCC_LIBFORMAT_PE_STATIC
#define DCC_LINKER_FLAG_LINKPE_KEEPEXPORT 0x08000000 /*< When statically linking against a PE binary, the old export table
                                                      *  is kept even though this will cause a copy of it to be created.
                                                      *  NOTE: The export table is always kept when a relocation pointing inside is detected. */
#endif
typedef uint32_t DCC(lflag_t); /*< Set of 'DCC_LINKER_FLAG_*'. */

struct DCCLinker {
 DCC(lflag_t)          l_flags;       /*< Current linker-related flags. */
 struct DCCLibPaths    l_intpaths;    /*< List of internal library paths (may be empty). */
 struct DCCLibPaths    l_paths;       /*< List of effective library paths (may be empty). */
 struct TPPString     *l_soname;      /*< [0..1] When non-NULL, a module name included in the binary (if appropriate). */
 struct DCCSection    *l_text;        /*< [0..1] When non-NULL, the section used for wrapper code such as ITA functions.
                                       *         When not defined, a target-specific default section
                                       *         is used, and when not needed, this field is ignored. */
 char                 *l_entry;       /*< [0..1][owned] Command-line option for '-Wl,entry=...'. */
 char                 *l_init;        /*< [0..1][owned] Command-line option for '-Wl,init=...'. */
 char                 *l_fini;        /*< [0..1][owned] Command-line option for '-Wl,fini=...'. */
 target_ptr_t          l_imgbase;     /*< Used when 'DCC_LINKER_FLAG_IMGBASE' has been set: An explicit image base address. */
 target_siz_t          l_secalign;    /*< [if(!=0)] Command-line override for section/program header alignment. */
 DCC(symflag_t)        l_visdefault;  /*< Default symbol visibility inherited when a new compiler is created. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
 uint8_t               l_pe_subsys;   /*< [if(!=0)] Command-line override for PE subsystem. */
 uint32_t              l_pe_filalign; /*< [if(!=0)] Command-line override for PE file alignment. */
 target_siz_t          l_pe_stacksiz; /*< [if(!=0)] Command-line override for PE stack size. */
#endif
#if DCC_TARGET_BIN == DCC_BINARY_ELF
 struct TPPString     *l_elf_interp; /*< [0..1] When non-NULL, the linked interpreter name. */
#endif
};

DCCDAT struct DCCLinker DCCLinker_Current; /* The current linker. */

DCCFUN void DCCLinker_Init(struct DCCLinker *__restrict self);
DCCFUN void DCCLinker_Quit(struct DCCLinker *__restrict self);

/* Clear any preallocated memory reachable from 'self'
 * NOTE: Some flush operations are always performed:
 *        - Clear unused relocation.
 * @param: flags: A set of 'DCCUNIT_FLUSHFLAG_*' (Unknown flags are ignored) */
DCCFUN void DCCLinker_Flush(struct DCCLinker *__restrict self, uint32_t flags);
#define DCCLINKER_FLUSHFLAG_NONE     0x00000000
#define DCCLINKER_FLUSHFLAG_LIBPATHS 0x00000200 /*< Clear unused library path vector entries. */

#define DCCLinker_Pushf() \
do{ DCC(lflag_t) const _old_lflags = DCCLinker_Current.l_flags
#define DCCLinker_Popf() \
    DCCLinker_Current.l_flags = _old_lflags;\
}while(DCC_MACRO_FALSE)


/* Adds the given path to the list of effective library paths.
 * Upon failure, a lexer error is set and 0 is returned.
 * NOTE: When the 'EXT_CANONICAL_LIB_PATHS' extension is enabled,
 *       the given path is fixed before being added.
 * WARNING: The given path may be modified.
 * @return: 0: [DCCLinker_AddLibPath] Not enough available memory (A lexer error was set).
 * @return: 1: [DCCLinker_AddLibPath] The given path was successfully added.
 * @return: 2: [DCCLinker_AddLibPath] The given path had already been added before.
 * @return: 0: [DCCLinker_DelLibPath] The given path was not found.
 * @return: 1: [DCCLinker_DelLibPath] The given path was successfully removed. */
DCCFUN int DCCLinker_AddLibPath(char *__restrict path, size_t pathsize);
DCCFUN int DCCLinker_DelLibPath(char *__restrict path, size_t pathsize);

/* Add a given list of paths separated by 'DCCLINKER_PATHS_SEP'
 * @return: 0 : Either no paths were added, or a lexer error was set.
 * @return: * : The amount of added library paths. */
DCCFUN size_t DCCLinker_AddLibPaths(char *__restrict list, size_t listsize);
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
#   define DCCLINKER_PATHS_SEP ';'
#else
#   define DCCLINKER_PATHS_SEP ':'
#endif

/* Add system library paths.
 * @param: outfile_or_basefile: The name of the output file (e.g.: 'a.out')
 *                              or '__BASE_FILE__' of the input source file. */
DCCFUN void DCCLinker_AddSysPaths(char const *__restrict outfile_or_basefile);

/* Push/pop the current list of library paths.
 * NOTE: [DCCLinker_LibPathPop] If no old list is available, emit a warning. */
DCCFUN void DCCLinker_LibPathPush(void);
DCCFUN void DCCLinker_LibPathPop(void);

/* Output generated code as a target binary (PE/ELF).
 * >> This is the most high-level function of all!
 * NOTES:
 *  - Relocations not compatible with the target are warned about.
 *  - To be able to safely call this function, only 'DCCLinker_Current',
 *    'DCCUnit_Current' and 'TPPLexer_Current' must be initialized.
 *    'DCCCompiler_Current' is unused and its state can be undefined. */
DCCFUN void DCCLinker_Make(DCC(stream_t) target);

#if DCC_TARGET_BIN == DCC_BINARY_PE
struct DCCStackValue;
/* Fix PE import symbols to use the indirection of the ITA address,
 * instead of generating an indirect call to an ITA wrapper function.
 * >> In addition, this function is a no-op if 'self'
 *    is anything that doesn't apply to the above. */
DCCFUN void DCCLinker_PEIndImport(struct DCCStackValue *__restrict self);
#endif




#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
DCCDAT struct DCCLinker linker;
#else
#define linker  DCCLinker_Current
#endif

/* push/pop the current unit c-flags. */
#define lpushf      DCCLinker_Pushf
#define lpopf       DCCLinker_Popf
#define lsetf(f)   (void)(linker.l_flags|= (f))
#define lunsetf(f) (void)(linker.l_flags&=~(f))

#endif

DCC_DECL_END

#endif /* !GUARD_DCC_LINKER_H */
