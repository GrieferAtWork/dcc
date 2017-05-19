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

DCC_DECL_BEGIN

#define DCC_LINKER_FLAG_SHARED       0x00000001 /*< Create shared libraries. */
#define DCC_LINKER_FLAG_NOSTDLIB     0x00000002 /*< Don't include standard libraries. */
#define DCC_LINKER_FLAG_NOUNDERSCORE 0x00000004 /*< Don't prepend leading underscores. */
#define DCC_LINKER_FLAG_STATIC       0x00000008 /*< Perform static linking. */
#define DCC_LINKER_FLAG_NORELOC      0x00000010 /*< Don't generate relocations, meaning that a generated image will not be position-independent.
                                                 *  WARNING: Despite the name, this flag does _NOT_ disable import relocations on ELF targets (because that'd just break dynamic linking...) */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_LINKER_FLAG_PEDYNAMIC     0x10000000 /*< On PE binary targets: Consider C/ELF visibility when generating an export table.
                                                  *  >> When this flag is set, any non-static definition matching '__attribute__((visibility("default")))'
                                                  *     will be exported, in addition to symbols marked with '__attribute__((dllexport))'. */
#define DCC_LINKER_FLAG_PEDYNAMIC_FWD 0x20000000 /*< On PE binary targets: Force enable dynamic export for symbols imported from other libraries, creating proxy declarations.
                                                  *  NOTE: Proxy declarations can always be created by declaring a symbol with both 'dllimport' and 'dllexport'!
                                                  *  WARNING: You probably don't want to enable this flag, because something as simple as '#pragma comment(lib,"ntdll.dll")'
                                                  *           will cause _ALL_ symbols from ntdll to be both imported _AND_ exported from your application (It'll work, but it's a damn stupid thing to do...) */
#endif
#if DCC_LIBFORMAT_STA_PE
#define DCC_LINKER_FLAG_LINKPE_KEEPEXPORT 0x08000000 /*< When statically linking against a PE binary, the old export table
                                                      *  is kept even though this will cause a copy of it to be created.
                                                      *  NOTE: The export table is always kept when a relocation pointing inside is detected. */
#endif
typedef uint32_t DCC(lflag_t); /*< Set of 'DCC_LINKER_FLAG_*'. */

struct DCCLibPaths {
 struct DCCLibPaths       *lp_prev;  /*< [0..1][owned] Previous list of library paths. */
 size_t                    lp_patha; /*< Allocated amount of library paths. */
 size_t                    lp_pathc; /*< Amount of elements in the vector below (may be empty). */
 /*ref*/struct TPPString **lp_pathv; /*< [1..1][0..il_pathc][owned] Vector of sanitized library paths. */
};

struct DCCLinker {
 DCC(lflag_t)       l_flags; /*< Current linker-related flags. */
 struct DCCLibPaths l_paths; /*< List of effective library paths (may be empty). */
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
#if DCC_HOST_OS == DCC_OS_WINDOWS
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
 *  - The caller should call 'DCCSection_ResolveDisp' on all sections beforehand.
 *  - To be able to safely call this function, only 'DCCLinker_Current',
 *    'DCCUnit_Current' and 'TPPLexer_Current' must be initialized.
 *    'DCCCompiler_Current' is unused and its state can be undefined. */
DCCFUN void DCCLinker_Make(DCC(stream_t) target);

#if DCC_TARGET_BIN == DCC_BINARY_PE
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
