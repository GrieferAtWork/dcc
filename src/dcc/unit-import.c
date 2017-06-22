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
#include <dcc/compiler.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/stream.h>
#include <dcc/linker.h>
#include <dcc/preprocessor.h>

#include "unit-import.h"

#include <stdio.h>
#include <string.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#if DCC_LIBFORMAT_ELF
#include "../../lib/include/elf.h"
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */

DCC_DECL_BEGIN

#define LOADDEF(d,s,f,msiz,...) \
 {f,LIBLOADER_FLAGS(d,s,msiz),__VA_ARGS__}

INTERN struct LibLoaderDef const dcc_libloaders[] = {
#if DCC_LIBFORMAT_PE_STATIC /* '*.exe|*.dll' PE binaries (static mode). */
 LOADDEF(0,1,&DCCUnit_StaLoadPE,2,{'M','Z'}),
#endif /* DCC_LIBFORMAT_PE_STATIC */
#if DCC_LIBFORMAT_PE_DYNAMIC /* '*.exe|*.dll' PE binaries (dynamic mode). */
 LOADDEF(1,0,&DCCUnit_DynLoadPE,2,{'M','Z'}),
#endif /* DCC_LIBFORMAT_PE_DYNAMIC */
#if DCC_LIBFORMAT_ELF /* ELF binary/library files. */
 LOADDEF(1,1,&DCCUnit_LoadELF,SELFMAG,{ELFMAG0,ELFMAG1,ELFMAG2,ELFMAG3}),
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */
#if DCC_LIBFORMAT_DEF_DYNAMIC /* '*.def' library files. */
 LOADDEF(1,0,&DCCUnit_DynLoadDEF2,7,{'L','I','B','R','A','R','Y'}),
 LOADDEF(1,0,&DCCUnit_DynLoadDEF,0,{0}),
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */
#if DCC_LIBFORMAT_ARCH
 LOADDEF(1,1,&DCCUnit_LoadARCH,8,{'!','<','a','r','c','h','>','\n'}),
#endif /* DCC_LIBFORMAT_ARCH */
 {NULL,0,{0}},
};


INTERN struct SrcLoaderDef const dcc_srcloaders[] = {
 /* NOTE: This order mirrors GCC behavior, as documented here:
  * >> http://labor-liber.org/en/gnu-linux/development/extensions */
 {&DCCUnit_LoadSrc_C,SRCLOADER_FLAG_PP,DCC_LIBDEF_FLAG_C_SOURCE,{'c','\0'}},
 {&DCCUnit_LoadSrc_C,SRCLOADER_FLAG_NONE,DCC_LIBDEF_FLAG_C_SOURCE,{'i','\0'}},
 {&DCCUnit_LoadSrc_ASM,SRCLOADER_FLAG_PP,DCC_LIBDEF_FLAG_ASM_SOURCE,{'S','\0'}},
 {&DCCUnit_LoadSrc_ASM,SRCLOADER_FLAG_NONE,DCC_LIBDEF_FLAG_ASM_SOURCE,{'s','\0'}},

 /* Fallback load handling when no extension is present. */
 {&DCCUnit_LoadSrc_C,SRCLOADER_FLAG_PP,DCC_LIBDEF_FLAG_C_SOURCE,{'\0'}},
 {&DCCUnit_LoadSrc_ASM,SRCLOADER_FLAG_PP,DCC_LIBDEF_FLAG_ASM_SOURCE,{'\0'}},
 {NULL,SRCLOADER_FLAG_NONE,0,{'\0'}},
};



PRIVATE void DCCUNIT_IMPORTCALL
srcloader_exec(struct SrcLoaderDef const *__restrict loader,
               struct DCCLibDef *__restrict def,
               char const *__restrict filename,
               stream_t fd) {
 struct TPPFile *tpp_file;
 struct DCCCompiler old_compiler;
 uint32_t old_tpp_flags;
 uint32_t old_tpp_extokens;
 struct TPPFile *old_tpp_eof_file;
 struct TPPFile *prev_file;

 /* Make sure the loading a source file
  * behaves like loading a static library does. */
 def->ld_dynlib = NULL;

 /* Initialize a new compiler. */
 old_compiler = compiler;
 DCCCompiler_Init(&compiler);

 /* Re-configure TPP */
 old_tpp_flags    = CURRENT.l_flags;
 old_tpp_extokens = CURRENT.l_extokens;
 old_tpp_eof_file = CURRENT.l_eof_file;
 TPPLexer_Current->l_flags &= (TPPLEXER_FLAG_TERMINATE_STRING_LF|
                               TPPLEXER_FLAG_MESSAGE_LOCATION|
                               TPPLEXER_FLAG_MESSAGE_NOLINEFEED|
                               TPPLEXER_FLAG_WERROR|
                               TPPLEXER_FLAG_WSYSTEMHEADERS|
                               TPPLEXER_FLAG_NO_DEPRECATED|
                               TPPLEXER_FLAG_MSVC_MESSAGEFORMAT|
                               TPPLEXER_FLAG_MERGEMASK);
 if (!(loader->sld_flags&SRCLOADER_FLAG_PP)) {
  TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_NO_MACROS|
                                TPPLEXER_FLAG_NO_DIRECTIVES|
                                TPPLEXER_FLAG_NO_BUILTIN_MACROS);
 }
 tpp_file = TPPFile_OpenStream(fd,filename);
 if unlikely(!tpp_file) { TPPLexer_SetErr(); goto end; }
 assert(!tpp_file->f_textfile.f_usedname);
 if (preproc.p_srcname) {
  tpp_file->f_textfile.f_usedname = TPPString_New(preproc.p_srcname,
                                                  strlen(preproc.p_srcname));
 }
 /* Output the base file as a dependency. */
 if (preproc.p_depfd != TPP_STREAM_INVALID)
     DCCPreprocessor_DepPrint(tpp_file->f_name,tpp_file->f_namesize);
 /* Manipulate the previous file to re-parse the last token. */
 prev_file = TOKEN.t_file;
 assert(prev_file);
 assert(TOKEN.t_begin >= prev_file->f_begin);
 assert(TOKEN.t_begin <= prev_file->f_end);
 prev_file->f_pos = TOKEN.t_begin;

 /* Configure TPP to use a sub-set of its
  * #include-stack, only consisting of the new files. */
 TPPLexer_PushFileInherited(tpp_file);
 CURRENT.l_eof_file = tpp_file;

 /* Execute the loader. */
 (*loader->sld_func)(def);

 assert(CURRENT.l_eof_file == tpp_file);
 CURRENT.l_eof_file = old_tpp_eof_file;
 /* Manually pop all files until the original is found. */
 while (TOKEN.t_file != prev_file) TPPLexer_PopFile();
 /* Restore the old token. */
 YIELD();

 if (!(def->ld_flags&DCC_LIBDEF_FLAG_NO_RESET_TPP)) {
  TPPLexer_Reset(TPPLexer_Current,
                 TPPLEXER_RESET_MACRO|TPPLEXER_RESET_ASSERT|
                 TPPLEXER_RESET_KWDFLAGS|TPPLEXER_RESET_COUNTER|
                 TPPLEXER_RESET_FONCE);
 }
 CURRENT.l_flags   &= TPPLEXER_FLAG_MERGEMASK;
 CURRENT.l_flags   |= old_tpp_flags;
 CURRENT.l_extokens = old_tpp_extokens;
 /* XXX: Shouldn't we update all symbols according
  *      to 'def->ld_expsymfa' and 'def->ld_expsymfo', as well
  *      as 'def->ld_impsymfa' and 'def->ld_impsymfo'? */
end:
 DCCCompiler_Quit(&compiler);
 compiler = old_compiler;
}

PRIVATE int DCCUNIT_IMPORTCALL
staloader_exec(p_libloader fun,
               struct DCCLibDef *__restrict def,
               char const *__restrict file,
               stream_t fd, soff_t start) {
 int result;
 if (def->ld_flags&DCC_LIBDEF_FLAG_AUTOMERGE) {
  DCCUnit_Push();
  result = (*fun)(def,file,fd,start);
  DCCUnit_Pop(result && OK); /* Merge if successfully loaded & OK */
 } else {
  result = (*fun)(def,file,fd,start);
 }
 return result;
}


PRIVATE int DCCUNIT_IMPORTCALL
DCCUnit_DoImportStream(struct DCCLibDef *__restrict def,
                       char const *__restrict filename,
                       stream_t fd, soff_t start) {
 int result = 0;
 uint32_t reqflags;
 assert(def);
 /* Do some initial assertions about the unit state during static import. */
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || (def->ld_flags&DCC_LIBDEF_FLAG_AUTOMERGE) || !unit.u_symc);
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || (def->ld_flags&DCC_LIBDEF_FLAG_AUTOMERGE) || !unit.u_nsymc);
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || (def->ld_flags&DCC_LIBDEF_FLAG_AUTOMERGE) || !unit.u_secc);
 assert(!(def->ld_flags&DCC_LIBDEF_FLAG_STATIC) || (def->ld_flags&DCC_LIBDEF_FLAG_AUTOMERGE) || !unit.u_impc);
 reqflags = (def->ld_flags&DCC_LIBDEF_FLAG_STATIC)
  ? (LIBLOADER_FLAG_DYN|LIBLOADER_FLAG_STA)
  : (LIBLOADER_FLAG_DYN);
 if (def->ld_flags&DCC_LIBDEF_FLAG_NODYN)
     reqflags &= ~(LIBLOADER_FLAG_DYN);
 if (!(def->ld_flags&DCC_LIBDEF_FLAG_ONLY_SOURCE)) {
  uint8_t magic[LIBLOADER_MAXMAGIC];
  struct LibLoaderDef const *iter;
  ptrdiff_t max_magic;
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
    result = staloader_exec(iter->lld_func,def,filename,fd,start);
    if (result || !OK) goto done;
   }
  }
  /* Check for magic-less formats. */
  for (iter = dcc_libloaders; iter->lld_func; ++iter) {
   if ((iter->lld_flags&reqflags) &&
      !(iter->lld_flags&LIBLOADER_MASK_MSIZE)) {
    result = staloader_exec(iter->lld_func,def,filename,fd,start);
    if (result || !OK) goto done;
   }
  }
 }
 if ((def->ld_flags&DCC_LIBDEF_FLAG_STATIC) &&
     (def->ld_flags&DCC_LIBDEF_FLAG_SOURCE)) {
  /* load source files. */
  struct SrcLoaderDef const *loader = dcc_srcloaders;
  size_t filename_len = strlen(filename);
  for (; loader->sld_func; ++loader) {
   size_t extlen;
   /* Make sure that at least one source
    * type handled by this loader is enabled. */
   if (!(def->ld_flags&loader->sld_type)) continue;
   /* Check for a matching extension. */
   extlen = strnlen(loader->sld_ext,SRCLOADER_MAXEXT);
   if (extlen >= filename_len ||
       filename[filename_len-(extlen+1)] != '.' ||
       memcmp(filename+(filename_len-extlen),
              loader->sld_ext,extlen) != 0) continue;
   srcloader_exec(loader,def,filename,fd);
   result = OK;
   goto done;
  }
 }
done:
 return result;
}


PRIVATE int DCCUNIT_IMPORTCALL
DCCUnit_ImportWithFilename(struct DCCLibDef *__restrict def,
                           char const *__restrict filename) {
 int result; stream_t s;
 if (!strcmp(filename,"-")) {
  struct DCCLibDef newdef = *def;
  /* Don't allow anything but source files to be read from <stdin>! */
  newdef.ld_flags |= DCC_LIBDEF_FLAG_ONLY_SOURCE;
  /* Read from stdin. */
  result = DCCUnit_DoImportStream(&newdef,"<stdin>",
                                  DCC_STREAM_STDIN,0);
  def->ld_dynlib = newdef.ld_dynlib;
 } else {
  s = s_openr(filename);
  //printf("Checking: '%s'\n",filename);
  if (s == TPP_STREAM_INVALID) {
#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
   /* TODO: maybe retry with fixed slashes? ('filename.replace("/","\\")') */
#endif
   return 0;
  }
  result = DCCUnit_DoImportStream(def,filename,s,0);
  s_close(s);
 }
 return result;
}


#define MAX_EXTLEN 4
struct path_extension { char ext[MAX_EXTLEN]; };

static struct path_extension const
search_extensions[] = {
 {{0}},
 {{'.','o'}},
 {{'.','a'}},
 {{'.','l','i','b'}},
#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
 {{'.','d','l','l'}},
 {{'.','e','x','e'}},
#endif
#if !!(DCC_HOST_OS&DCC_OS_F_UNIX)
 {{'.','s','o'}},
#endif
 {{'.','d','e','f'}},
};

PRIVATE int DCCUNIT_IMPORTCALL
DCCUnit_ImportWithPath(struct TPPString *__restrict path,
                       struct DCCLibDef *__restrict def) {
 struct path_extension const *ext_iter,*ext_end;
 char *buf,*mbuf,*ext; int result;
 size_t pathlen,buflen;
 assert(def);
 assert(path);
 pathlen = path->s_size;
 if (pathlen == 1 && path->s_text[0] == '.') pathlen = 0;
 while (pathlen && path->s_text[pathlen-1] == '/') --pathlen;
 buflen = pathlen+def->ld_size+(MAX_EXTLEN+DCC_COMPILER_STRLEN(DCC_CONFIG_LIBPREFIX))+2;
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
 if (pathlen) *ext++ = '/';
 memcpy(ext,def->ld_name,def->ld_size*sizeof(char));
 ext += def->ld_size;
 if (def->ld_flags&DCC_LIBDEF_FLAG_NOSEARCHEXT) {
  ext[0] = '\0';
  result = DCCUnit_ImportWithFilename(def,buf);
 } else {
  ext[MAX_EXTLEN] = '\0';
  ext_end = (ext_iter = search_extensions)+(sizeof(search_extensions)/
                                            sizeof(search_extensions[0]));
  assert(ext_iter != ext_end);
  do {
   memcpy(ext,ext_iter->ext,MAX_EXTLEN*sizeof(char));
   result = DCCUnit_ImportWithFilename(def,buf);
   if (result || !OK) goto done;
  } while (++ext_iter != ext_end);
  /* Retry with 'lib' prefixed before the filename. */
  ext -= def->ld_size;
  memcpy(ext,DCC_CONFIG_LIBPREFIX,DCC_COMPILER_STRLEN(DCC_CONFIG_LIBPREFIX)*sizeof(char));
  ext += DCC_COMPILER_STRLEN(DCC_CONFIG_LIBPREFIX);
  memcpy(ext,def->ld_name,def->ld_size*sizeof(char));
  ext[def->ld_size] = '\0';
  ext_iter = search_extensions;
  do {
   memcpy(ext,ext_iter->ext,MAX_EXTLEN*sizeof(char));
   result = DCCUnit_ImportWithFilename(def,buf);
   if (result || !OK) goto done;
  } while (++ext_iter != ext_end);
 }
done:
 DCC_Free(mbuf);
 return result;
}

PUBLIC int DCCUNIT_IMPORTCALL
DCCUnit_Import(struct DCCLibDef *__restrict def) {
 struct DCCLibPaths *pathlist;
 size_t i; int result = 0;
 assert(def);
 if (def->ld_flags&DCC_LIBDEF_FLAG_INTERN) {
  pathlist = &linker.l_intpaths;
  goto search_pathlist;
 } else if (def->ld_flags&DCC_LIBDEF_FLAG_NOSEARCHSTD) {
  /* Using an empty string as path, we
   * can search the given 'def' as-is. */
  result = DCCUnit_ImportWithPath(TPPFile_Empty.f_text,def);
  if (result || !OK) goto end;
 } else {
  pathlist = &linker.l_paths;
search_pathlist:
  i = pathlist->lp_pathc;
  while (i-- != 0) {
   /* Search user-defined library paths. */
   result = DCCUnit_ImportWithPath(pathlist->lp_pathv[i],def);
   if (result || !OK) goto end;
  }
 }
 if (!(def->ld_flags&DCC_LIBDEF_FLAG_NOWARNMISSING))
       WARN(W_LIB_NOT_FOUND,def->ld_name,def->ld_size);
end:
 return result;
}

PUBLIC int DCCUNIT_IMPORTCALL
DCCUnit_ImportStream(struct DCCLibDef *__restrict def,
                     char const *__restrict filename,
                     stream_t fd, soff_t start) {
 int result = DCCUnit_DoImportStream(def,filename,fd,start);
 if (!result && !(def->ld_flags&DCC_LIBDEF_FLAG_NOWARNMISSING))
      WARN(W_LIB_NOT_FOUND,filename,(size_t)strlen(filename));
 return result;
}


DCC_DECL_END

#ifndef __INTELLISENSE__
#include "unit-import-arch.c.inl"
#include "unit-import-def-dynamic.c.inl"
#include "unit-import-elf.c.inl"
#include "unit-import-pe-dynamic.c.inl"
#include "unit-import-pe-static.c.inl"
#include "unit-import-source.c.inl"
#endif

#endif /* !GUARD_DCC_UNIT_IMPORT_C */
