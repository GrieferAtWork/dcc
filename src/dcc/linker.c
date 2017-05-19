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
#ifndef GUARD_DCC_LINKER_C
#define GUARD_DCC_LINKER_C 1

#include <dcc/common.h>
#include <dcc/linker.h>
#include <dcc/target.h>

#include <string.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#ifdef _WIN32
#include <Windows.h>
#endif

#ifndef PATH_MAX
#ifdef PATHMAX
#   define PATH_MAX  PATHMAX
#elif defined(MAX_PATH)
#   define PATH_MAX  MAX_PATH
#elif defined(MAXPATH)
#   define PATH_MAX  MAXPATH
#elif DCC_HOST_OS == DCC_OS_WINDOWS
#   define PATH_MAX  256
#else
#   define PATH_MAX  1024
#endif
#endif


DCC_DECL_BEGIN

PUBLIC struct DCCLinker DCCLinker_Current = {0};

PUBLIC void DCCLinker_Init(struct DCCLinker *__restrict self) {
 assert(self);
 memset(self,0,sizeof(struct DCCLinker));
}
PUBLIC void DCCLinker_Quit(struct DCCLinker *__restrict self) {
 assert(self);
 { struct TPPString **iter,**end;
   struct DCCLibPaths *next,*piter = &self->l_paths;
   do {
    /* Clear allocated library paths. */
    end = (iter = piter->lp_pathv)+piter->lp_pathc;
    for (; iter != end; ++iter) TPPString_Decref(*iter);
    free(piter->lp_pathv);
    next = piter->lp_prev;
    if (piter != &self->l_paths) free(piter);
   } while ((piter = next) != NULL);
 }
}

PUBLIC void DCCLinker_LibPathPush(void) {
 struct DCCLibPaths *backup;
 struct TPPString **iter,**end,**dst;
 backup = (struct DCCLibPaths *)DCC_Malloc(sizeof(struct DCCLibPaths),0);
 if unlikely(!backup) return;
 backup->lp_pathv = (struct TPPString **)DCC_Malloc(linker.l_paths.lp_pathc*
                                                    sizeof(struct TPPString *),0);
 if unlikely(!backup->lp_pathv) return;
 backup->lp_patha =
 backup->lp_pathc = linker.l_paths.lp_pathc;
 dst = backup->lp_pathv;
 end = (iter = linker.l_paths.lp_pathv)+linker.l_paths.lp_pathc;
 for (; iter != end; ++iter,++dst) *dst = *iter,assert(*dst),
                                      TPPString_Incref(*dst);
 backup->lp_prev        = linker.l_paths.lp_prev; /* Inherit data. */
 linker.l_paths.lp_prev = backup; /* Inherit data. */
}
PUBLIC void DCCLinker_LibPathPop(void) {
 struct DCCLibPaths *backup;
 struct TPPString **iter,**end;
 backup = linker.l_paths.lp_prev;
 if unlikely(!backup) {
  WARN(W_PRAGMA_LIBPATH_NOTHING_TO_POP);
  return;
 }
 end = (iter = linker.l_paths.lp_pathv)+linker.l_paths.lp_pathc;
 for (; iter != end; ++iter) assert(*iter),TPPString_Decref(*iter);
 free(linker.l_paths.lp_pathv);
 memcpy(&linker.l_paths,backup,sizeof(struct DCCLibPaths));
 free(backup);
}

extern char *fix_filename(char *filename, size_t *pfilename_size); /* From 'tcc.c' */

PRIVATE int
DCCLinker_DoAddLibPath(char const *__restrict path, size_t pathsize) {
 struct TPPString **iter,**end,*elem;
 /* Handle special case: empty path & remove trailing slashes. */
 while (pathsize && path[pathsize-1] == '/') --pathsize;
 if unlikely(!pathsize) path = ".",pathsize = 1;
 /* Make sure that the path doesn't already exists. */
 end = (iter = linker.l_paths.lp_pathv)+
               linker.l_paths.lp_pathc;
 for (; iter != end; ++iter) {
  if ((elem = *iter)->s_size == pathsize &&
     !memcmp(elem->s_text,path,pathsize*sizeof(char))
     ) return 2; /* Path already exists. */
 }
 elem = TPPString_New(path,pathsize);
 if unlikely(!elem) return 0;
 iter = linker.l_paths.lp_pathv;
 if (linker.l_paths.lp_pathc == linker.l_paths.lp_patha) {
  size_t newalloc = linker.l_paths.lp_patha;
  if unlikely(!newalloc) newalloc = 1;
  newalloc *= 2;
  iter = (struct TPPString **)DCC_Realloc(linker.l_paths.lp_pathv,newalloc*
                                          sizeof(struct TPPString *),
                                          DCCLINKER_FLUSHFLAG_LIBPATHS);
  if unlikely(!iter) { TPPString_Decref(elem); return 0; }
  linker.l_paths.lp_pathv = iter;
  linker.l_paths.lp_patha = newalloc;
 }
 iter[linker.l_paths.lp_pathc++] = elem; /*< Inherit reference. */
 return 1;
}
PUBLIC int
DCCLinker_AddLibPath(char *__restrict path, size_t pathsize) {
 /* Normalize & fix the given path as best as possible. */
 if (HAS(EXT_CANONICAL_LIB_PATHS)) fix_filename(path,&pathsize);
 return DCCLinker_DoAddLibPath(path,pathsize);
}
PUBLIC int
DCCLinker_DelLibPath(char *__restrict path, size_t pathsize) {
 struct TPPString **iter,**end,*elem;
 if (HAS(EXT_CANONICAL_LIB_PATHS)) fix_filename(path,&pathsize);
 while (pathsize && path[-1] == '/') --pathsize;
 if unlikely(!pathsize) path = ".",pathsize = 1;
 /* Make sure that the path doesn't already exists. */
 end = (iter = linker.l_paths.lp_pathv)+
               linker.l_paths.lp_pathc;
 for (; iter != end; ++iter) {
  if ((elem = *iter)->s_size == pathsize &&
     !memcmp(elem->s_text,path,pathsize*sizeof(char))) {
   /* Found it! */
   memmove(iter,iter+1,((end-iter)-1)*sizeof(struct TPPString *));
   --linker.l_paths.lp_pathc;
   TPPString_Decref(elem);
   return 1;
  }
 }
 return 0;
}


PRIVATE void linker_add_dirof(char const *__restrict filename) {
 size_t dirbase; char *buf,*mbuf;
 assert(filename);
 dirbase = strlen(filename);
 while (dirbase && (filename[dirbase] != '/' && filename[dirbase] != '\\')) --dirbase;
 while (dirbase && (filename[dirbase] == '/' || filename[dirbase] == '\\')) --dirbase;
 if (!dirbase) { char p[2]; p[0] = '.'; p[1] = '\0'; DCCLinker_AddLibPath(p,1); return; }
 if (dirbase < 64) {
  buf = (char *)alloca((dirbase+1)*sizeof(char));
  mbuf = NULL;
 } else {
  mbuf = buf = (char *)DCC_Malloc((dirbase+1)*sizeof(char),0);
  if unlikely(!buf) return;
 }
 memcpy(buf,filename,dirbase*sizeof(char));
 buf[dirbase] = '\0';
 DCCLinker_AddLibPath(buf,dirbase);
 DCC_Free(mbuf);
}

#if DCC_HOST_OS == DCC_OS_WINDOWS
PRIVATE void linker_add_win32_systemdirectory(void) {
 UINT reqsize; char *p,buffer[PATH_MAX];
again:
 reqsize = GetSystemDirectoryA((p = buffer),sizeof(buffer));
 if (reqsize > sizeof(buffer)) {
  UINT newreqsize;
  p = (char *)DCC_Malloc(reqsize*sizeof(char),0);
  if unlikely(!p) return;
  newreqsize = GetSystemDirectoryA(p,reqsize);
  if (newreqsize != reqsize) { DCC_Free(p); goto again; }
 }
 DCCLinker_AddLibPath(p,reqsize);
 if (p != buffer) DCC_Free(p);
}
PRIVATE void linker_add_win32_windowsdirectory(void) {
 UINT reqsize; char *p,buffer[PATH_MAX];
again:
 reqsize = GetWindowsDirectoryA((p = buffer),sizeof(buffer));
 if (reqsize > sizeof(buffer)) {
  UINT newreqsize;
  p = (char *)DCC_Malloc(reqsize*sizeof(char),0);
  if unlikely(!p) return;
  newreqsize = GetWindowsDirectoryA(p,reqsize);
  if (newreqsize != reqsize) { DCC_Free(p); goto again; }
 }
 DCCLinker_AddLibPath(p,reqsize-sizeof(char));
 if (p != buffer) DCC_Free(p);
}
#endif

PUBLIC void
DCCLinker_AddSysPaths(char const *__restrict outfile_or_basefile) {
#if DCC_HOST_OS == DCC_OS_WINDOWS
 /* NOTE: The order here is important, and
  *       mirrors LoadLibrary path resolution. */
 linker_add_dirof(outfile_or_basefile);
 linker_add_win32_systemdirectory();
 linker_add_win32_windowsdirectory();
#else
#   error TODO
#endif
}


PUBLIC void DCCLinker_Flush(struct DCCLinker *__restrict self, uint32_t flags) {
 assert(self);
 if (flags&DCCLINKER_FLUSHFLAG_LIBPATHS) {
  struct DCCLibPaths *piter = &self->l_paths;
  do {
   assert(piter->lp_patha >= piter->lp_pathc);
   if (piter->lp_patha != piter->lp_pathc) {
    if (!piter->lp_pathc) {
     free(piter->lp_pathv);
     piter->lp_pathv = NULL;
     piter->lp_patha = 0;
    } else {
     struct TPPString **newvec;
     newvec = (struct TPPString **)realloc(piter->lp_pathv,
                                           piter->lp_pathc*
                                           sizeof(struct TPPString *));
     if likely(newvec) {
      piter->lp_pathv = newvec;
      piter->lp_patha = piter->lp_pathc;
     }
    }
   }
  } while ((piter = piter->lp_prev) != NULL);
 }
}

DCC_DECL_END


#if DCC_TARGET_BIN == DCC_BINARY_PE
#   include "linker-pe.c.inl"
#elif DCC_TARGET_BIN == DCC_BINARY_ELF
#   include "linker-elf.c.inl"
#else
#   error FIXME
#endif

#endif /* !GUARD_DCC_LINKER_C */
