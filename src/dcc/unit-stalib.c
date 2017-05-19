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
#ifndef GUARD_DCC_UNIT_STALIB_C
#define GUARD_DCC_UNIT_STALIB_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>
#include <dcc/stream.h>

#include <stdio.h>

DCC_DECL_BEGIN

/* Generic library loader:
 * @return: 0: Data form the given stream 's' does not describe a binary. (No lexer error was set)
 *             A critical error occurred while parsing the given stream 's'. (A lexer error was set)
 * @return: 1: Successfully loaded a binary into the current unit (statically) */
typedef int (*PLibLoader)(char *__restrict filename,
                          char *__restrict name,
                          stream_t s);

#define LIBLOADER_MAXMAGIC  8
struct PLibLoaderDecl {
 PLibLoader  ld_loader;                    /*< [1..1] Loader entry point (NULL used for list terminator). */
 size_t      ld_msize;                     /*< Size of the magic header 'ld_magic' (Or ZERO(0) if loading should always be attempted). */
 uint8_t     ld_magic[LIBLOADER_MAXMAGIC]; /*< [ld_msize] Magic header for quickly identifying this kind of loader. */
};

#if DCC_STAFORMAT_PE
INTERN int DCCUnit_StaLoadPE(char *__restrict filename,
                             char *__restrict name, stream_t fd);
#endif /* DCC_STAFORMAT_PE */

static struct PLibLoaderDecl const lib_loaders[] = {
#if DCC_STAFORMAT_PE
 {&DCCUnit_StaLoadPE,2,{'M','Z'}}, /* '*.exe/*.dll' PE binaries. */
#endif /* DCC_STAFORMAT_PE */
 {NULL,0,{0}},
};




PUBLIC int
DCCUnit_StaLoadStream(char *__restrict filename,
                      char *__restrict name,
                      stream_t s, int warn_unknown) {
 uint8_t magic[LIBLOADER_MAXMAGIC];
 ptrdiff_t max_magic;
 int result = 0;
 struct PLibLoaderDecl const *iter;
 max_magic = s_read(s,magic,LIBLOADER_MAXMAGIC);
 if (max_magic < 0) max_magic = 0;
 if (max_magic) s_seek(s,-max_magic,SEEK_CUR);
 for (iter = lib_loaders; iter->ld_loader; ++iter) {
  if (iter->ld_msize && iter->ld_msize <= (size_t)max_magic &&
     !memcmp(iter->ld_magic,magic,iter->ld_msize)) {
   /* Got a magic match. */
   result = (*iter->ld_loader)(filename,name,s);
   if (result || !OK) goto done;
  }
 }
 /* Check for magic-less formats. */
 for (iter = lib_loaders; iter->ld_loader; ++iter) {
  if (!iter->ld_msize) {
   result = (*iter->ld_loader)(filename,name,s);
   if (result || !OK) goto done;
  }
 }
 if (warn_unknown) WARN(W_LIB_NOT_FOUND,name);
done:
 return result;
}



PRIVATE int
DCCUnit_StaLoadWithPath2(char *__restrict filename,
                         char *__restrict name) {
 stream_t s = s_openr(filename);
 int result;
 //printf("Checking: '%s'\n",filename);
 if (s == TPP_STREAM_INVALID) return NULL;
 result = DCCUnit_StaLoadStream(filename,name,s,0);
 s_close(s);
 return result;
}

static char const *const search_format[] = {
 "%s",
 "%s.dll",
 "%s.exe",
 NULL,
};
PRIVATE int
DCCUnit_StaLoadWithPath(char const *__restrict path,
                        char *__restrict name) {
 int result = 0;
 char buffer[1024],*bufpos; size_t bufsize;
 char const *const *format_iter;
 bufsize = strnlen(path,sizeof(buffer));
 memcpy(buffer,path,bufsize*sizeof(char));
 bufpos = buffer+bufsize;
 bufsize = sizeof(buffer)-bufsize;
 if (bufsize && bufpos != buffer && bufpos[-1] != '\\')
    *bufpos++ = '\\',--bufsize;
 for (format_iter = search_format; *format_iter; ++format_iter) {
  snprintf(bufpos,bufsize,*format_iter,name);
  result = DCCUnit_StaLoadWithPath2(buffer,name);
  if (result || !OK) break;
 }
 return result;
}




static char const *const builtin_paths[] = {
 "",                        /* Search without directory. */
 "C:\\Windows\\System32\\", /* Placeholder for finding windows-dlls. */
 NULL,
};

PUBLIC int
DCCUnit_StaLoad(char *__restrict name, int warn_unknown) {
 char const *const *builtin_iter;
 int result = 0;
 /* TODO: For compatibility with windows dll-path resolution,
  *       the following locations must be searched by DCC (in that order):
  *    1. <output-directory-of-executable> (binary-output-mode)
  *    OR <directory-of-__BASE_FILE__>     (immediate-execution-mode)
  *    2. GetSystemDirectoryA();
  *    3. GetWindowsDirectoryA();
  *    4. %PATH%
  * NOTE: Before #2, windows actually searches the CWD of the calling process,
  *       but since that can't be predicted by the compiler (and the best
  *       prediction being what #1 already does), that step is skipped.
  */
 for (builtin_iter = builtin_paths; *builtin_iter; ++builtin_iter) {
  result = DCCUnit_StaLoadWithPath(*builtin_iter,name);
  if (result || !OK) goto end;
 }
 /* TODO: Search user-defined library paths. */
 if (warn_unknown) WARN(W_LIB_NOT_FOUND,name);
end:
 return result;
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "unit-stalib-pe.c.inl"
#endif

#endif /* !GUARD_DCC_UNIT_STALIB_C */
