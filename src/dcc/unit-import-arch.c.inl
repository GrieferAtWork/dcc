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
#ifndef GUARD_DCC_UNIT_IMPORT_ARCH_C_INL
#define GUARD_DCC_UNIT_IMPORT_ARCH_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>

#if DCC_LIBFORMAT_ARCH
#include <dcc/byteorder.h>
#include <dcc/unit.h>
#include <dcc/stream.h>
#include <dcc/linker.h>

#include "unit-import.h"
#include "../../lib/include/ar.h"

#include <string.h>

DCC_DECL_BEGIN

struct ArchHeader {
 char          mag[SARMAG];
 struct ar_hdr hdr;
};


INTERN void DCCUNIT_IMPORTCALL
DCCUnit_LoadCOFFSymtab(struct DCCLibDef *__restrict def,
                       char const *__restrict file,
                       stream_t fd, soff_t start,
                       size_t size) {
 char *end,*data = (char *)DCC_Malloc(size+sizeof(char),0);
 soff_t    segstart;
 ptrdiff_t read_error;
 uint32_t  i,symcnt;
 uint32_t *ar_index;
 char     *ar_names,*name;
 int bound;
 if unlikely(!data) return;
 s_seek(fd,start,SEEK_SET);
 read_error = s_read(fd,data,size);
 if (read_error < 0) read_error = 0;
 if (size > (size_t)read_error)
     size = (size_t)read_error;
 if (size < 4) return;
 data[size] = '\0';
 end = data+size;
 symcnt = DCC_BE2H32(*(uint32_t *)data);
 ar_index = (uint32_t *)(data+4);
 ar_names = (char *)(ar_index+symcnt);
 do {
  name  = ar_names;
  bound = 0;
  for (i = 0; i < symcnt; i++) {
   struct DCCSym *sym;
   if (name >= end) break;
   sym = DCCUnit_GetSyms(name);
   if (sym && DCCSym_ISFORWARD(sym)) {
    struct DCCLibDef subdef = *def;
    subdef.ld_flags &= ~(DCC_LIBDEF_FLAG_SOURCE|DCC_LIBDEF_FLAG_NOWARNMISSING);
    /* Load a sub-library. */
    segstart = DCC_BE2H32(ar_index[i])+(start-SARMAG*sizeof(char));
    s_seek(fd,segstart,SEEK_SET);
    if (DCCUnit_ImportStream(&subdef,file,fd,segstart)) ++bound;
    if (!OK) break;
    if (!def->ld_dynlib) def->ld_dynlib = subdef.ld_dynlib;
   }
   name += strlen(name)+1;
  }
 } while(bound);
 DCC_Free(data);
}


INTERN int DCCUNIT_IMPORTCALL
DCCUnit_LoadARCH(struct DCCLibDef *__restrict def,
                 char const *__restrict file,
                 stream_t fd, soff_t start) {
 struct ArchHeader hdr;
 int in_ill_hdr = 0,result = 0;
 if unlikely(!s_reada(fd,&hdr,sizeof(hdr))) goto end;
 /* Validate all the magic fields. */
 if unlikely(memcmp(hdr.mag,ARMAG,SARMAG*sizeof(char)) != 0) goto end;
 start += SARMAG*sizeof(char);
 def->ld_dynlib = NULL;
 for (;;) {
#define fmt_name  hdr.hdr.ar_name
  unsigned long size;
  if unlikely(memcmp(hdr.hdr.ar_fmag,ARFMAG,sizeof(hdr.hdr.ar_fmag)) != 0) {
   /* NOTE: Apparently, archive headers are 2-byte aligned, but 
    *       size members are not required to reflect this.
    *       With that in mind: Don't warn about illegal magic when we're not aligned. */
   if (!in_ill_hdr && !(start&1)) {
    WARN(W_LIB_ARCH_INVALID_MAGIC,file,
        (unsigned long)(start+DCC_COMPILER_OFFSETOF(struct ar_hdr,ar_fmag)));
    in_ill_hdr = 1;
   }
   /* Try again at the next byte.
    * NOTE: This might seem hacky, but this does seem
    *       to fix some [broken?] windows libraries. */
   ++start;
   goto next_hdr;
  }
  in_ill_hdr = 0;
  start += sizeof(hdr.hdr);
  /* NOTE: The hacky zero-termination below is only OK because
   *       there are still members where we're writing to. */
  hdr.hdr.ar_size[sizeof(hdr.hdr.ar_size)/sizeof(char)] = '\0'; /* Force ZERO-termination by writing past the end. */
  size = strtoul(hdr.hdr.ar_size,NULL,0);
  hdr.hdr.ar_name[sizeof(hdr.hdr.ar_name)/sizeof(char)] = '\0';
  { char *end = strchr(fmt_name,' '); if (end) *end = '\0'; } /* Truncate the string. */
  if (!strcmp(fmt_name,"/")) {
   if (!(def->ld_flags&DCC_LIBDEF_FLAG_NODYN)) {
    /* TODO: This kind of symbol loading is designed to
     *       be performed during the last stages of linking.
     *       Mainly since is looks at all the symbols actually
     *       used, this kind of linking will only include symbols
     *       actually in use (which they probably won't be, yet!) */
    /* COFF symbol table */
    DCCUnit_LoadCOFFSymtab(def,file,fd,start,(size_t)size);
    if (!OK) goto end;
    result = 1;
   }
  } else if (!strcmp(fmt_name,"//") ||
             !strcmp(fmt_name,"__.SYMDEF") ||
             !strcmp(fmt_name,"__.SYMDEF/") ||
             !strcmp(fmt_name,"ARFILENAMES/")) {
   /* Skip symbol table & archive name entries. */
  } else if (*fmt_name == '/') {
   /* '/<INT>' (e.g.: '/0', '/92')
    * I don't know that these are for, but windows seems to have them...
    * ... I guess they're section names, probably meaning
    *     that this format could be used for static linkage... */
  } else {
   /* Fallback: Load the sub-section as an automatic library. */
   struct DCCLibDef subdef = *def;
   subdef.ld_flags &= ~(DCC_LIBDEF_FLAG_SOURCE);
   subdef.ld_flags |=  (DCC_LIBDEF_FLAG_NOWARNMISSING);
   if (!DCCUnit_ImportStream(&subdef,file,fd,start))
        WARN(W_LIB_ARCH_UNKNOWN_FORMAT,fmt_name,start,file);
   else result = 1;
   if (!OK) goto end;
   if (!def->ld_dynlib) def->ld_dynlib = subdef.ld_dynlib;
  }
#undef fmt_name
  /* Read the next header. */
  start += size;
next_hdr:
  s_seek(fd,start,SEEK_SET);
  if unlikely(!s_reada(fd,&hdr.hdr,sizeof(hdr.hdr))) break;
 }
end:
 if (!result) def->ld_dynlib = NULL;
 else if (!def->ld_dynlib) {
  /* We may not have done anything, but we must still
   * indicate that we didn't include a static link. */
  def->ld_dynlib = &DCCSection_Abs;
 }
 return result;
}


DCC_DECL_END
#endif /* DCC_LIBFORMAT_ARCH */

#endif /* !GUARD_DCC_UNIT_IMPORT_ARCH_C_INL */
