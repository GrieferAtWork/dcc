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
#ifndef GUARD_DCC_UNIT_EXPORT_C
#define GUARD_DCC_UNIT_EXPORT_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/stream.h>

#include "unit-export.h"

DCC_DECL_BEGIN

PUBLIC void DCCUNIT_EXPORTCALL
DCCUnit_Export(struct DCCExpDef *__restrict def,
               char const *__restrict filename) {
 stream_t fd = s_openw(filename);
 if (fd == TPP_STREAM_INVALID)
      WARN(W_EXPORT_CANNOT_OPEN,filename);
 else DCCUnit_ExportStream(def,fd);
 s_close(fd);
}

PUBLIC void DCCUNIT_EXPORTCALL
DCCUnit_ExportStream(struct DCCExpDef *__restrict def,
                     stream_t fd) {
 assert(def);
 /* XXX: Format switch? */
 DCCUnit_ExportElf(def,fd);
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "unit-export-elf.c.inl"
#endif

#endif /* !GUARD_DCC_UNIT_EXPORT_C */
