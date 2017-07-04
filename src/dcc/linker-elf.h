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
#ifndef GUARD_DCC_LINKER_ELF_H
#define GUARD_DCC_LINKER_ELF_H 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/lexer.h>
#include "../../lib/include/elf.h"

DCC_DECL_BEGIN

#ifdef DCC_PRIVATE_API
#define DCC_TARGET_ELF_SIZE  DCC_TARGET_SIZEOF_POINTER
#if DCC_TARGET_ELF_SIZE == 4
#   define ELF_USE 32
#   define Elf(x)  Elf32_##x
#   define ELF(x)  ELF32_##x
#elif DCC_TARGET_ELF_SIZE == 8
#   define ELF_USE 64
#   define Elf(x)  Elf64_##x
#   define ELF(x)  ELF64_##x
#else
#   error FIXME
#endif
#endif



#ifndef DCC_TARGET_ELFCLASS
#if ELF_USE == 32
#   define DCC_TARGET_ELFCLASS ELFCLASS32
#else
#   define DCC_TARGET_ELFCLASS ELFCLASS64
#endif
#endif /* !DCC_TARGET_ELFCLASS */

#ifndef DCC_TARGET_ELFDATA
#if DCC_TARGET_BYTEORDER == 1234
#   define DCC_TARGET_ELFDATA ELFDATA2LSB
#elif DCC_TARGET_BYTEORDER == 4321
#   define DCC_TARGET_ELFDATA ELFDATA2MSB
#else
#   define DCC_TARGET_ELFDATA ELFDATANONE
#endif
#endif /* !DCC_TARGET_ELFDATA */

#ifndef DCC_TARGET_ELFVERSION
#   define DCC_TARGET_ELFVERSION EV_CURRENT
#endif /* DCC_TARGET_ELFVERSION */

#ifndef DCC_TARGET_ELFOSABI
#if DCC_TARGET_OS == DCC_OS_FREEBSD || \
    DCC_TARGET_OS == DCC_OS_FREEBSD_KERNEL
#   define DCC_TARGET_ELFOSABI ELFOSABI_FREEBSD
#elif DCC_TARGET_OS == DCC_OS_F_UNIX
#   define DCC_TARGET_ELFOSABI ELFOSABI_LINUX
#else
#   define DCC_TARGET_ELFOSABI ELFOSABI_SYSV
#endif
#endif /* !DCC_TARGET_ELFOSABI */

#ifndef DCC_TARGET_ELF_MACHINE
#if DCC_TARGET_HASM(M_I386)
#   define DCC_TARGET_ELF_MACHINE EM_386
#elif DCC_TARGET_HASF(F_X86_64)
#   define DCC_TARGET_ELF_MACHINE EM_X86_64
#else
#   error FIXME
#endif
#endif /* !DCC_TARGET_ELF_MACHINE */


/* Header for data segment pointed to by 'DT_GNU_HASH'. */
typedef struct {
    Elf32_Word gh_nbuckets;
    Elf32_Word gh_symbase;
    Elf32_Word gh_bitmask_nwords;
    Elf32_Word gh_gnushift;
    /* Variable-length vector of 32-bit integers ('gh_bitmask_nwords' elements long) */
    /* array of gnu buckets as 32-bit integers ('gh_nbuckets' elements long) */
} Elf32_GNUHash;

typedef struct {
    Elf64_Word gh_nbuckets;
    Elf64_Word gh_symbase;
    Elf64_Word gh_bitmask_nwords;
    Elf64_Word gh_gnushift;
    /* Variable-length vector of 64-bit integers ('gh_bitmask_nwords' elements long) */
    /* array of gnu buckets as 32-bit integers ('gh_nbuckets' elements long) */
} Elf64_GNUHash;


DCC_DECL_END

#endif /* !GUARD_DCC_LINKER_ELF_H */
