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
#include <dcc/unit.h>

#include <string.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#if DCC_HOST_OS == DCC_OS_WINDOWS
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
#   define PATH_MAX  260
#else
#   define PATH_MAX  1024
#endif
#endif


DCC_DECL_BEGIN

LOCAL void
DCCLibPaths_Quit(struct DCCLibPaths *__restrict self) {
 struct TPPString **iter,**end;
 struct DCCLibPaths *next,*piter = self;
 do {
  /* Clear allocated library paths. */
  end = (iter = piter->lp_pathv)+piter->lp_pathc;
  for (; iter != end; ++iter) TPPString_Decref(*iter);
  free(piter->lp_pathv);
  next = piter->lp_prev;
  if (piter != self) free(piter);
 } while ((piter = next) != NULL);
}

LOCAL void
DCCLibPaths_Push(struct DCCLibPaths *__restrict self) {
 struct DCCLibPaths *backup;
 struct TPPString **iter,**end,**dst;
 backup = (struct DCCLibPaths *)DCC_Malloc(sizeof(struct DCCLibPaths),0);
 if unlikely(!backup) return;
 backup->lp_pathv = (struct TPPString **)DCC_Malloc(self->lp_pathc*
                                                    sizeof(struct TPPString *),0);
 if unlikely(!backup->lp_pathv) return;
 backup->lp_patha =
 backup->lp_pathc = self->lp_pathc;
 dst = backup->lp_pathv;
 end = (iter = self->lp_pathv)+self->lp_pathc;
 for (; iter != end; ++iter,++dst) *dst = *iter,assert(*dst),
                                      TPPString_Incref(*dst);
 backup->lp_prev = self->lp_prev; /* Inherit data. */
 self->lp_prev   = backup; /* Inherit data. */
}

LOCAL void
DCCLibPaths_Pop(struct DCCLibPaths *__restrict self) {
 struct DCCLibPaths *backup;
 struct TPPString **iter,**end;
 backup = self->lp_prev;
 if unlikely(!backup) {
  WARN(W_PRAGMA_LIBRARY_PATH_NOTHING_TO_POP);
  return;
 }
 end = (iter = self->lp_pathv)+self->lp_pathc;
 for (; iter != end; ++iter) assert(*iter),TPPString_Decref(*iter);
 free(self->lp_pathv);
 memcpy(self,backup,sizeof(struct DCCLibPaths));
 free(backup);
}

INTDEF char *fix_filename(char *filename, size_t *pfilename_size); /* From 'tcc.c' */

LOCAL int
DCCLibPaths_DoAddLibPathNow(struct DCCLibPaths *__restrict self,
                            char const *__restrict path, size_t pathsize) {
 struct TPPString **vec,*elem;
 elem = TPPString_New(path,pathsize);
 if unlikely(!elem) return 0;
 vec = self->lp_pathv;
 if (self->lp_pathc == self->lp_patha) {
  size_t newalloc = self->lp_patha;
  if unlikely(!newalloc) newalloc = 1;
  newalloc *= 2;
  vec = (struct TPPString **)DCC_Realloc(self->lp_pathv,newalloc*
                                         sizeof(struct TPPString *),
                                         DCCLINKER_FLUSHFLAG_LIBPATHS);
  if unlikely(!vec) { TPPString_Decref(elem); return 0; }
  self->lp_pathv = vec;
  self->lp_patha = newalloc;
 }
 vec[self->lp_pathc++] = elem; /*< Inherit reference. */
 return 1;
}
LOCAL int
DCCLibPaths_DoAddLibPath(struct DCCLibPaths *__restrict self,
                         char const *__restrict path, size_t pathsize) {
 struct TPPString **iter,**end,*elem;
 /* Handle special case: empty path & remove trailing slashes. */
 while (pathsize && path[pathsize-1] == '/') --pathsize;
 if unlikely(!pathsize) path = ".",pathsize = 1;
 /* Make sure that the path doesn't already exists. */
 end = (iter = self->lp_pathv)+
               self->lp_pathc;
 for (; iter != end; ++iter) {
  if ((elem = *iter)->s_size == pathsize &&
      !memcmp(elem->s_text,path,pathsize*sizeof(char)))
      return 2; /* Path already exists. */
 }
 return DCCLibPaths_DoAddLibPathNow(self,path,pathsize);
}
LOCAL int
DCCLibPaths_AddLibPath(struct DCCLibPaths *__restrict self,
                       char *__restrict path, size_t pathsize) {
 /* Normalize & fix the given path as best as possible. */
 if (HAS(EXT_CANONICAL_LIB_PATHS)) fix_filename(path,&pathsize);
 return DCCLibPaths_DoAddLibPath(self,path,pathsize);
}
LOCAL int
DCCLibPaths_DelLibPath(struct DCCLibPaths *__restrict self,
                       char *__restrict path, size_t pathsize) {
 struct TPPString **iter,**end,*elem;
 if (HAS(EXT_CANONICAL_LIB_PATHS)) fix_filename(path,&pathsize);
 while (pathsize && path[-1] == '/') --pathsize;
 if unlikely(!pathsize) path = ".",pathsize = 1;
 /* Make sure that the path doesn't already exists. */
 end = (iter = self->lp_pathv)+
               self->lp_pathc;
 for (; iter != end; ++iter) {
  if ((elem = *iter)->s_size == pathsize &&
     !memcmp(elem->s_text,path,pathsize*sizeof(char))) {
   /* Found it! */
   memmove(iter,iter+1,((end-iter)-1)*sizeof(struct TPPString *));
   --self->lp_pathc;
   TPPString_Decref(elem);
   return 1;
  }
 }
 return 0;
}

LOCAL size_t
DCCLibPaths_AddLibPaths(struct DCCLibPaths *__restrict self,
                        char *__restrict list, size_t listsize) {
 size_t result = 0;
 while (listsize) {
  char *sep = (char *)memchr(list,DCCLINKER_PATHS_SEP,listsize);
  if unlikely(!sep) sep = list+listsize;
  DCCLibPaths_AddLibPath(self,list,(size_t)(sep-list));
  listsize -= (sep-list);
  if unlikely(!listsize) break;
  --listsize,list = sep+1;
 }
 return result;
}



PUBLIC struct DCCLinker DCCLinker_Current = {0};
PUBLIC void DCCLinker_Init(struct DCCLinker *__restrict self) {
 assert(self);
 memset(self,0,sizeof(struct DCCLinker));
}
PUBLIC void DCCLinker_Quit(struct DCCLinker *__restrict self) {
 assert(self);
 DCCLibPaths_Quit(&self->l_intpaths);
 DCCLibPaths_Quit(&self->l_paths);
 DCC_Free(self->l_entry);
 DCC_Free(self->l_init);
 DCC_Free(self->l_fini);
}

PUBLIC void DCCLinker_LibPathPush(void) { DCCLibPaths_Push(&linker.l_paths); }
PUBLIC void DCCLinker_LibPathPop(void) { DCCLibPaths_Pop(&linker.l_paths); }
PUBLIC int DCCLinker_AddLibPath(char *__restrict path, size_t pathsize) { return DCCLibPaths_AddLibPath(&linker.l_paths,path,pathsize); }
PUBLIC int DCCLinker_DelLibPath(char *__restrict path, size_t pathsize) { return DCCLibPaths_DelLibPath(&linker.l_paths,path,pathsize); }
PUBLIC size_t DCCLinker_AddLibPaths(char *__restrict list, size_t listsize) { return DCCLibPaths_AddLibPaths(&linker.l_paths,list,listsize); }

LOCAL void linker_add_dirof(char const *__restrict filename) {
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
typedef UINT (WINAPI *SYSDIR_FUN)(LPSTR lpBuffer, UINT uSize);
typedef BOOL (WINAPI *LPISWOW64PROCESS)(HANDLE hProcess, PBOOL Wow64Process);


LOCAL void linker_add_win32_systemdirectory(void) {
 UINT reqsize; char *p,buffer[PATH_MAX];
#if !defined(_WIN64) && !defined(WIN64) && \
    (DCC_TARGET_SIZEOF_POINTER == 4)
 /* When running through emulation on a 64-bit version of windows,
  * we must use 'GetSystemWow64DirectoryA' to determine this directory. */
 SYSDIR_FUN get_sysdir;
 LPISWOW64PROCESS iswow64_fun;
 static HMODULE k32 = NULL;
 if unlikely(!k32 && (k32 = LoadLibraryA("Kernel32.dll")) == NULL) goto no_wow;
 get_sysdir  = (SYSDIR_FUN)GetProcAddress(k32,"GetSystemWow64DirectoryA");
 if unlikely(!get_sysdir) goto no_wow;
 iswow64_fun = (LPISWOW64PROCESS)GetProcAddress(k32,"IsWow64Process");
 if unlikely(!iswow64_fun) goto no_wow;
 {
  BOOL iswow = FALSE;
  if (!(*iswow64_fun)(GetCurrentProcess(),&iswow)) iswow = FALSE;
  if (!iswow) goto no_wow;
 }
 if (DCC_MACRO_FALSE) { no_wow: get_sysdir = &GetSystemDirectoryA; }
#else
#define get_sysdir  GetSystemDirectoryA
#endif
again:
 reqsize = get_sysdir((p = buffer),sizeof(buffer));
 if (reqsize > sizeof(buffer)) {
  UINT newreqsize;
  p = (char *)DCC_Malloc(reqsize*sizeof(char),0);
  if unlikely(!p) return;
  newreqsize = get_sysdir(p,reqsize);
  if (newreqsize != reqsize) { DCC_Free(p); goto again; }
 }
 DCCLinker_AddLibPath(p,reqsize);
 if (p != buffer) DCC_Free(p);
#ifdef get_sysdir
#undef get_sysdir
#endif
}

LOCAL void linker_add_win32_windowsdirectory(void) {
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
 DCCLinker_AddLibPath(p,reqsize);
 if (p != buffer) DCC_Free(p);
}

#define INTPATH_MAXTRAIL 16

#define LIBINT_TRIAL_COUNT 2
#define INCINT_TRIAL_COUNT 2
struct inttrail { char t[INTPATH_MAXTRAIL]; };
static struct inttrail const
libint_trails[LIBINT_TRIAL_COUNT+1] = {
 {{'/','l','i','b','\0'}},
 {{'/','l','i','b','s','\0'}},
 {{'\0'}},
};

static struct inttrail const
incint_trails[INCINT_TRIAL_COUNT+1] = {
 {{'/','f','i','x','i','n','c','l','u','d','e','\0'}},
 {{'/','i','n','c','l','u','d','e','\0'}},
 {{'\0'}},
};


LOCAL void linker_add_intpath_self(void) {
 char *mbuf = NULL,buf[1024],*path = buf,*iter,*lastslash;
 DWORD newbuflen,buflen = sizeof(buf);
 struct inttrail const *trail_iter,*inc_trail_iter;
 struct TPPString **inc_dst;
 for (;;) {
  /* XXX: Add support for 'DllMain' */
  newbuflen  = GetModuleFileNameA(NULL,path,buflen);
  newbuflen += (INTPATH_MAXTRAIL*2);
  if (newbuflen < buflen) break;
  mbuf = path = (char *)realloc(mbuf,(buflen *= 2)*sizeof(char));
  if unlikely(!mbuf) {nomem: TPPLexer_SetErr(); return; }
 }
 for (iter = path,lastslash = NULL; *iter; ++iter) {
  if (*iter == '\\') *iter = '/';
  if (*iter == '/') lastslash = iter;
 }
 if (lastslash) {
  while (lastslash != path &&
         lastslash[-1] == '/') --lastslash;
 } else {
  lastslash = path+(newbuflen-(INTPATH_MAXTRAIL*2));
 }
 if ((lastslash-path) > 4 &&
    !memcmp(lastslash-4,"/bin",4*sizeof(char))
     ) lastslash -= 4;
 buflen = lastslash-path;
 inc_dst = (struct TPPString **)realloc(TPPLexer_Current->l_syspaths.il_pathv,
                                       (TPPLexer_Current->l_syspaths.il_pathc+
                                       (LIBINT_TRIAL_COUNT*INCINT_TRIAL_COUNT))*
                                        sizeof(struct TPPString *));
 if unlikely(!inc_dst) goto nomem;
 TPPLexer_Current->l_syspaths.il_pathv  = inc_dst;
 inc_dst += TPPLexer_Current->l_syspaths.il_pathc;
 TPPLexer_Current->l_syspaths.il_pathc += (LIBINT_TRIAL_COUNT*INCINT_TRIAL_COUNT);
 for (trail_iter = libint_trails; trail_iter->t[0]; ++trail_iter) {
  size_t path_len = buflen+strlen(trail_iter->t);
  memcpy(lastslash,trail_iter->t,sizeof(trail_iter->t));
  DCCLibPaths_DoAddLibPathNow(&linker.l_intpaths,path,path_len);
  /* Add internal include paths. */
  for (inc_trail_iter = incint_trails;
       inc_trail_iter->t[0]; ++inc_trail_iter,++inc_dst) {
   memcpy(path+path_len,inc_trail_iter->t,sizeof(inc_trail_iter->t));
   *inc_dst = TPPString_New(path,path_len+strlen(inc_trail_iter->t));
   if (!*inc_dst) { *inc_dst = TPPFile_Empty.f_text; TPPString_Incref(TPPFile_Empty.f_text); }
  }
 }
 assert(inc_dst == TPPLexer_Current->l_syspaths.il_pathv+
                   TPPLexer_Current->l_syspaths.il_pathc);
 free(mbuf);
}
#endif

PUBLIC void
DCCLinker_AddSysPaths(char const *__restrict outfile_or_basefile) {
#if DCC_HOST_OS == DCC_OS_WINDOWS && DCC_TARGET_OS == DCC_OS_WINDOWS
 /* For compatibility with windows dll-path resolution,
  * the following locations must be searched by DCC (in that order):
  *    1. <output-directory-of-executable> (binary-output-mode)
  *    OR <directory-of-__BASE_FILE__>     (immediate-execution-mode)
  *    2. GetSystemDirectoryA();
  *    3. GetWindowsDirectoryA();
  *    4. %PATH%
  * NOTE: Before #2, windows actually searches the CWD of the calling process,
  *       but since that can't be predicted by the compiler (and the best
  *       prediction being what #1 already does), that step is skipped.
  */
 linker_add_dirof(outfile_or_basefile);
 linker_add_win32_systemdirectory();
 linker_add_win32_windowsdirectory();
 { /* Add %PATH% */
   char *pathcopy,*path = getenv("PATH");
   if (path && (pathcopy = strdup(path)) != NULL) {
    DCCLinker_AddLibPaths(pathcopy,strlen(pathcopy));
    free(pathcopy);
   }
 }
 /* Setup the default list of internal library paths. */
 linker_add_intpath_self();
#else
 /* TODO */
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
