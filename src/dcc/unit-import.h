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

#define LIBLOADER_MAXMAGIC  8

#define LIBLOADER_FLAG_NONE  0x00000000
#define LIBLOADER_FLAG_DYN   0x00000001 /* The lib loader can load dynamic libraries. */
#define LIBLOADER_FLAG_STA   0x00000002 /* The lib loader can load static libraries. */
#define LIBLOADER_MASK_MSIZE 0x0000000f /* Mask for magic size */
#define LIBLOADER_FLAGS(d,s,msiz) (((d)?LIBLOADER_FLAG_DYN:0)|((s)?LIBLOADER_FLAG_STA:0)|((msiz)&LIBLOADER_MASK_MSIZE))

struct PLibLoaderDef {
 p_libloader lld_func;  /*< [1..1] Loader entry point (NULL used for list terminator). */
 uint32_t    lld_flags; /*< Flags and magic size (Set of 'LIBLOADER_FLAG_*' or'd with any value matching 'LIBLOADER_MASK_MSIZE'). */
 uint8_t     lld_magic[LIBLOADER_MAXMAGIC]; /*< [ld_msize] Magic header for quickly identifying this kind of loader. */
};

extern struct PLibLoaderDef const dcc_libloaders[];


#if DCC_LIBFORMAT_DYN_PE
INTERN int DCCUNIT_IMPORTCALL DCCUnit_DynLoadPE(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_DYN_PE */
#if DCC_LIBFORMAT_STA_PE
INTERN int DCCUNIT_IMPORTCALL DCCUnit_StaLoadPE(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_STA_PE */
#if DCC_LIBFORMAT_DYN_DEF
INTERN int DCCUNIT_IMPORTCALL DCCUnit_DynImportDEF(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
INTERN int DCCUNIT_IMPORTCALL DCCUnit_DynImportDEF2(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_DYN_DEF */
#if DCC_LIBFORMAT_ELF
INTERN int DCCUNIT_IMPORTCALL DCCUnit_LoadELF(struct DCCLibDef *__restrict def, char const *__restrict file, stream_t fd);
#endif /* DCC_LIBFORMAT_ELF */


DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_IMPORT_H */
