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
#ifndef GUARD_DCC_UNIT_IMPORT_H
#define GUARD_DCC_UNIT_IMPORT_H 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/stream.h>

DCC_DECL_BEGIN


/* Generic library loader prototype:
 * @param: def: The library definition (s.a.: documentation of 'DCCLibDef')
 * @return: 0: Data form the given stream 'fd' does not describe a binary. (No lexer error was set)
 *             A critical error occurred while parsing the given stream 'fd'. (A lexer error was set)
 * @return: 1: Successfully loaded a binary into the current unit (either statically, or dynamically) */
typedef int (DCCUNIT_IMPORTCALL *p_libloader)(struct DCCLibDef *__restrict def,
                                              char const *__restrict file, stream_t fd);
typedef void (DCCUNIT_IMPORTCALL *p_srcloader)(struct DCCLibDef *__restrict def);

#define LIBLOADER_MAXMAGIC  8

#define LIBLOADER_FLAG_NONE  0x00000000
#define LIBLOADER_FLAG_DYN   0x00000010 /* The lib loader can load dynamic libraries. */
#define LIBLOADER_FLAG_STA   0x00000020 /* The lib loader can load static libraries. */
#define LIBLOADER_MASK_MSIZE 0x0000000f /* Mask for magic size */
#define LIBLOADER_FLAGS(d,s,msiz) (((d)?LIBLOADER_FLAG_DYN:0)|((s)?LIBLOADER_FLAG_STA:0)|((msiz)&LIBLOADER_MASK_MSIZE))

struct LibLoaderDef {
 p_libloader lld_func;  /*< [1..1] Loader entry point (NULL used for list terminator). */
 uint32_t    lld_flags; /*< Flags and magic size (Set of 'LIBLOADER_FLAG_*' or'd with any value matching 'LIBLOADER_MASK_MSIZE'). */
 uint8_t     lld_magic[LIBLOADER_MAXMAGIC]; /*< [ld_msize] Magic header for quickly identifying this kind of loader. */
};



#define SRCLOADER_MAXEXT    4
#define SRCLOADER_FLAG_NONE 0x00000000
#define SRCLOADER_FLAG_PP   0x00000001 /*< TPP macro preprocessing must be enabled
                                        *  while parsing source files of this type. */

/* When a source loader is executed, TPP will be in the following state:
 *  - The source file has been pushed on the #include-stack.
 *  - The #include-stack is limited to the current source file.
 *  - The current token is EOF and no tokens have been parsed yet.
 *  - The configuration for enabled token ('l_extokens') is in an undefined state.
 *  - Lexer flags ('l_flags') have been cleared, except
 *    for the following flags, which are inherited (unchanged):
 *    All flags are cleared except:
 *      - TPPLEXER_FLAG_TERMINATE_STRING_LF
 *      - TPPLEXER_FLAG_MESSAGE_LOCATION
 *      - TPPLEXER_FLAG_MESSAGE_NOLINEFEED
 *      - TPPLEXER_FLAG_WERROR
 *      - TPPLEXER_FLAG_WSYSTEMHEADERS
 *      - TPPLEXER_FLAG_NO_DEPRECATED
 *      - TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
 *      - TPPLEXER_FLAG_MERGEMASK (All flags masked)
 *    The following flags are set when the loader
 *    hasn't been declared with 'SRCLOADER_FLAG_PP':
 *      - TPPLEXER_FLAG_NO_MACROS
 *      - TPPLEXER_FLAG_NO_DIRECTIVES
 *      - TPPLEXER_FLAG_NO_BUILTIN_MACROS
 * Upon return of the source loader, the
 * following TPP options will be restored:
 *  - 'CURRENT.l_flags'
 *  - 'CURRENT.l_extokens'
 *  - 'CURRENT.l_eof_file'
 *  - All files still on-stack after the original source file,
 *    as well as the original source file itself are removed.
 * NOTE: 'def->ld_dynlib' was already set to NULL, meaning
 *        that the source file loader is not required to
 *        update the value to mirror static linkage behavior.
 */
struct SrcLoaderDef {
 p_srcloader sld_func;  /*< [1..1] The function to call for parsing. */
 uint32_t    sld_flags; /*< A set of 'SRCLOADER_FLAG_*' */
 uint32_t    sld_type;  /*< A set of 'DCC_LIBDEF_*_SOURCE' of all source types handled by this loaders or'd together. */
 char        sld_ext[SRCLOADER_MAXEXT]; /*< File extension applicable to this loader. */
};


extern struct SrcLoaderDef const dcc_srcloaders[];


#if DCC_LIBFORMAT_PE_DYNAMIC
INTERN int DCCUNIT_IMPORTCALL DCCUnit_DynLoadPE(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_PE_DYNAMIC */
#if DCC_LIBFORMAT_PE_STATIC
INTERN int DCCUNIT_IMPORTCALL DCCUnit_StaLoadPE(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_PE_STATIC */
#if DCC_LIBFORMAT_DEF_DYNAMIC
INTERN int DCCUNIT_IMPORTCALL DCCUnit_DynLoadDEF(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
INTERN int DCCUNIT_IMPORTCALL DCCUnit_DynLoadDEF2(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */
#if DCC_LIBFORMAT_ELF
INTERN int DCCUNIT_IMPORTCALL DCCUnit_LoadELF(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_ELF */

/* Source code loaders. */
INTERN void DCCUNIT_IMPORTCALL DCCUnit_LoadSrc_C(struct DCCLibDef *__restrict def);
INTERN void DCCUNIT_IMPORTCALL DCCUnit_LoadSrc_ASM(struct DCCLibDef *__restrict def);



DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_IMPORT_H */
