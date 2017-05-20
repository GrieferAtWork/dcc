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
#ifndef GUARD_DCC_UNIT_IMPORT_C
#define GUARD_DCC_UNIT_IMPORT_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/stream.h>
#include <dcc/linker.h>

#include "unit-import.h"

#include <stdio.h>
#include <string.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#if DCC_LIBFORMAT_ELF
#include <elf.h>
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */

DCC_DECL_BEGIN

#define LOADDEF(d,s,f,msiz,...) \
 {f,LIBLOADER_FLAGS(d,s,msiz),__VA_ARGS__}

INTERN struct PLibLoaderDef const dcc_libloaders[] = {
#if DCC_LIBFORMAT_PE_STATIC /* '*.exe/*.dll' PE binaries (static mode). */
 LOADDEF(0,1,&DCCUnit_StaLoadPE,2,{'M','Z'}),
#endif /* DCC_LIBFORMAT_PE_STATIC */
#if DCC_LIBFORMAT_PE_DYNAMIC /* '*.exe/*.dll' PE binaries (dynamic mode). */
 LOADDEF(1,0,&DCCUnit_DynLoadPE,2,{'M','Z'}),
#endif /* DCC_LIBFORMAT_PE_DYNAMIC */
#if DCC_LIBFORMAT_ELF /* ELF binary/library files. */
 LOADDEF(1,1,&DCCUnit_LoadELF,SELFMAG,{ELFMAG0,ELFMAG1,ELFMAG2,ELFMAG3}),
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */
#if DCC_LIBFORMAT_DEF_DYNAMIC /* '*.def' library files. */
 LOADDEF(1,0,&DCCUnit_DynLoadDEF2,7,{'L','I','B','R','A','R','Y'}),
 LOADDEF(1,0,&DCCUnit_DynLoadDEF,0,{0}),
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */
 {NULL,0,{0}},
};



PRIVATE int DCCUNIT_IMPORTCALL
DCCUnit_DoImportStream(struct DCCLibDef *__restrict def,
                       char const *__restrict filename,
                       stream_t fd) {
 uint8_t magic[LIBLOADER_MAXMAGIC];
 ptrdiff_t max_magic; int result = 0;
 struct PLibLoaderDef const *iter;
 uint32_t reqflags;
 assert(def);
 /* Do some initial assertions about the unit state during static import. */
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || !unit.u_symc);
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || !unit.u_nsymc);
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || !unit.u_secc);
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || !unit.u_impc);
 reqflags = (def->ld_flags&DCC_LIBDEF_FLAG_STATIC)
  ? (LIBLOADER_FLAG_DYN|LIBLOADER_FLAG_STA)
  : (LIBLOADER_FLAG_DYN);
 if (def->ld_flags&DCC_LIBDEF_FLAG_NODYN)
     reqflags &= ~(LIBLOADER_FLAG_DYN);
 max_magic = s_read(fd,magic,LIBLOADER_MAXMAGIC);
 if (max_magic < 0) max_magic = 0;
 if (max_magic) s_seek(fd,-max_magic,SEEK_CUR);
 for (iter = dcc_libloaders; iter->lld_func; ++iter) {
  if ((iter->lld_flags&reqflags) &&
      (iter->lld_flags&LIBLOADER_MASK_MSIZE) &&
      (iter->lld_flags&LIBLOADER_MASK_MSIZE) <= (size_t)max_magic &&
      !memcmp(iter->lld_magic,magic,
             (iter->lld_flags&LIBLOADER_MASK_MSIZE))) {
   /* Got a magic match. */
   result = (*iter->lld_func)(def,filename,fd);
   if (result || !OK) goto done;
  }
 }
 /* Check for magic-less formats. */
 for (iter = dcc_libloaders; iter->lld_func; ++iter) {
  if ((iter->lld_flags&reqflags) &&
     !(iter->lld_flags&LIBLOADER_MASK_MSIZE)) {
   result = (*iter->lld_func)(def,filename,fd);
   if (result || !OK) goto done;
  }
 }
done:
 return result;
}


PRIVATE int DCCUNIT_IMPORTCALL
DCCUnit_ImportWithFilename(struct DCCLibDef *__restrict def,
                           char const *__restrict filename) {
 int result; stream_t s = s_openr(filename);
 //printf("Checking: '%s'\n",filename);
 if (s == TPP_STREAM_INVALID) return NULL;
 result = DCCUnit_DoImportStream(def,filename,s);
 s_close(s);
 return result;
}


#define MAX_EXTLEN 8
struct path_extension { char ext[MAX_EXTLEN]; };

static struct path_extension const
search_extensions[] = {
 {""    },
 {".dll"},
 {".exe"},
 {".def"},
};
PRIVATE int DCCUNIT_IMPORTCALL
DCCUnit_ImportWithPath(struct TPPString *__restrict path,
                       struct DCCLibDef *__restrict def) {
 struct path_extension const *ext_iter,*ext_end;
 size_t pathlen,buflen;
 char *buf,*mbuf,*ext; int result;
 assert(def);
 assert(path);
 pathlen = path->s_size;
 while (pathlen && (path->s_text[pathlen-1] == '/' ||
                    path->s_text[pathlen-1] == '\\')
        ) --pathlen;
 buflen = pathlen+def->ld_size+MAX_EXTLEN+2;
 if (buflen < 128) {
  buf = (char *)alloca(buflen);
  mbuf = NULL;
 } else {
  buf = mbuf = (char *)DCC_Malloc(buflen,0);
  if unlikely(!mbuf) return 0;
 }
 ext = buf;
 memcpy(ext,path->s_text,pathlen*sizeof(char));
 ext += pathlen;
#if DCC_HOST_OS == DCC_OS_WINDOWS
 if (pathlen) *ext++ = '\\';
#else
 if (pathlen) *ext++ = '/';
#endif
 memcpy(ext,def->ld_name,def->ld_size*sizeof(char));
 ext += def->ld_size;
 ext[MAX_EXTLEN] = '\0';
 ext_end = (ext_iter = search_extensions)+(sizeof(search_extensions)/
                                           sizeof(search_extensions[0]));
 assert(ext_iter != ext_end);
 do {
  memcpy(ext,ext_iter->ext,MAX_EXTLEN*sizeof(char));
  result = DCCUnit_ImportWithFilename(def,buf);
  if (result || !OK) break;
 } while (++ext_iter != ext_end);
 DCC_Free(mbuf);
 return result;
}




PUBLIC int DCCUNIT_IMPORTCALL
DCCUnit_Import(struct DCCLibDef *__restrict def) {
 size_t i; int result = 0;
 assert(def);
 if (def->ld_flags&DCC_LIBDEF_FLAG_NOSEARCHSTD) {
  /* Using an empty string as path, we
   * can search the given 'def' as-is. */
  struct TPPString *empty = TPPString_NewSized(0);
  assert(empty);
  result = DCCUnit_ImportWithPath(empty,def);
  assert(empty->s_refcnt > 1);
  --empty->s_refcnt;
 } else for (i = 0; i < linker.l_paths.lp_pathc; ++i) {
  /* Search user-defined library paths. */
  result = DCCUnit_ImportWithPath(linker.l_paths.lp_pathv[i],def);
  if (result || !OK) goto end;
 }
 if (!(def->ld_flags&DCC_LIBDEF_FLAG_NOWARNMISSING))
       WARN(W_LIB_NOT_FOUND,def->ld_name,def->ld_size);
end:
 return result;
}

PUBLIC int DCCUNIT_IMPORTCALL
DCCUnit_ImportStream(struct DCCLibDef *__restrict def,
                     char const *__restrict filename,
                     stream_t fd) {
 int result = DCCUnit_DoImportStream(def,filename,fd);
 if (!result && !(def->ld_flags&DCC_LIBDEF_FLAG_NOWARNMISSING)
     ) WARN(W_LIB_NOT_FOUND,filename,(size_t)strlen(filename));
 return result;
}


DCC_DECL_END

#ifndef __INTELLISENSE__
#include "unit-import-def-dynamic.c.inl"
#include "unit-import-elf.c.inl"
#include "unit-import-pe-dynamic.c.inl"
#include "unit-import-pe-static.c.inl"
#endif

#endif /* !GUARD_DCC_UNIT_IMPORT_C */
