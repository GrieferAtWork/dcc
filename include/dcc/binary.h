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
#ifndef GUARD_DCC_BINARY_H
#define GUARD_DCC_BINARY_H 1

#include "common.h"
#include "target.h"
#include "lexer.h"

#include <elf.h>

DCC_DECL_BEGIN

#ifdef DCC_PRIVATE_API
#if DCC_TARGET_SIZEOF_POINTER == 4
#define ELF_USE 32
#define Elf(x) Elf32_##x
#define ELF(x) ELF32_##x
#elif DCC_TARGET_SIZEOF_POINTER == 8
#define ELF_USE 64
#define Elf(x) Elf64_##x
#define ELF(x) ELF64_##x
#endif
#endif


/* Output generated code as a target binary (PE/ELF).
 * >> This is the most high-level function of all!
 * NOTE: Relocations not compatible with the target are warned about
 * NOTE: The caller should call 'DCCSection_ResolveDisp' on all sections beforehand. */
DCCFUN void DCCBin_Generate(DCC(stream_t) target);

#if DCC_TARGET_BIN == DCC_BINARY_PE
/* Fix PE import symbols to use the indirection of the ITA address,
 * instead of generating an indirect call to an ITA wrapper function.
 * >> In addition, this function is a no-op if 'self'
 *    is anything that doesn't apply to the above. */
DCCFUN void DCCBin_PEIndImport(struct DCCStackValue *__restrict self);
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_BINARY_H */
