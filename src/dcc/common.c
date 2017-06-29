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
#ifndef GUARD_DCC_TPP_COMMON_C
#define GUARD_DCC_TPP_COMMON_C 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/compiler.h>
#include <dcc/unit.h>
#include <dcc/linker.h>
#include <dcc/stream.h>

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#if DCC_DEBUG && !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#include <dcc_winmin.h>
#endif
#if DCC_DEBUG
#ifdef _MSC_VER
#include <malloc.h>
#endif
#endif

DCC_DECL_BEGIN

#ifdef __DCC_VERSION__
#pragma DCC __sleep(0)
#endif

#if DCC_DEBUG
#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
INTERN void dcc_voutf(char const *fmt, va_list args) {
 char buffer[4096];
 vsnprintf(buffer,sizeof(buffer),fmt,args);
#ifndef __DCC_VERSION__
 OutputDebugStringA(buffer);
#endif
#ifdef _MSC_VER
 /* Try not to use locking IO because of the following:
  * input.c:
  * >> #include <stdio.h>
  * >> int main() {
  * >>     fprintf(stderr,"Hello world\n");
  * >> #warning "Compiler warning message"
  * >>     return 0;
  * >> }
  * When running the above in DRT mode, a data sync may be triggered
  * while DRT execution in inside 'printf()', following printf() trying
  * to access the potentially unsynchronized string "Hello World\n".
  * If that happens, DRT will hold onto the the 'stderr' file lock
  * in a way that could potentially result in a deadlock if the
  * compiler thread attempts to acquire the same lock.
  *
  * TODO: A more permanent solution for this problem is to never
  *       make use of stdio anywhere within the compiler, meaning
  *       that all existing uses should probably be replaced
  *       with lower-level APIs such as dprintf/write/WriteFile.
  */
 _fwrite_nolock(buffer,sizeof(char),strlen(buffer),stderr);
#else
 fwrite(buffer,sizeof(char),strlen(buffer),stderr);
#endif
}
INTERN void dcc_outf(char const *fmt, ...) {
 va_list args;
 va_start(args,fmt);
 dcc_voutf(fmt,args);
 va_end(args);
}
#else
#undef dcc_voutf
INTERN void dcc_voutf(char const *fmt, va_list args) { vfprintf(stderr,fmt,args); }
#undef dcc_outf
INTERN void dcc_outf(char const *fmt, ...) {
 va_list args;
 va_start(args,fmt);
 vfprintf(stderr,fmt,args);
 va_end(args);
}
#define dcc_voutf(fmt,args) vfprintf(stderr,fmt,args)
#define dcc_outf(...)       fprintf(stderr,__VA_ARGS__)
#endif


PUBLIC
#ifdef DCC_NO_BREAKPOINT
       DCC_ATTRIBUTE_NORETURN
#endif
void dcc_assertion_failed(char const *expr, char const *file, int line) {
 dcc_assertion_failedf(expr,file,line,NULL);
}
PUBLIC
#ifdef DCC_NO_BREAKPOINT
       DCC_ATTRIBUTE_NORETURN
#endif
void dcc_assertion_failedf(char const *expr, char const *file,
                           int line, char const *fmt, ...) {
 static int in_assert = 0;
#ifdef _MSC_VER
 int msvc_mode = !TPPLexer_Current || !!(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT);
#else
 int msvc_mode = TPPLexer_Current && !!(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT);
#endif
 dcc_outf(msvc_mode ? "%s(%d) : " : "%s:%d: ",file,line);
 dcc_outf("Assertion failed: '%s'\n",expr);
 if (fmt) {
  va_list args;
  va_start(args,fmt);
  dcc_voutf(fmt,args);
  va_end(args);
  dcc_outf("\n");
 }
 if (!in_assert) {
  in_assert = 1;
  /* Here, so we print some text indicating where the error happened. */
  WARN(W_OUT_OF_MEMORY,0);
  in_assert = 0;
 }
#ifdef DCC_NO_BREAKPOINT
 _exit(1);
#endif
}
#endif /* DCC_DEBUG */


#define CM_MAX_LEVEL 6
static uint32_t const cm_level_flags[CM_MAX_LEVEL][3] = {
 /* level #0 */{DCCCOMPILER_FLUSHFLAG_NONE,DCCUNIT_FLUSHFLAG_NONE,DCCLINKER_FLUSHFLAG_NONE},
 /* level #1 */{DCCCOMPILER_FLUSHFLAG_PACKSTACK|DCCCOMPILER_FLUSHFLAG_VISISTACK,
                DCCUNIT_FLUSHFLAG_NONE,DCCLINKER_FLUSHFLAG_LIBPATHS},
 /* level #2 */{DCCCOMPILER_FLUSHFLAG_VSTACK,DCCUNIT_FLUSHFLAG_NONE,DCCLINKER_FLUSHFLAG_NONE},
 /* level #3 */{DCCCOMPILER_FLUSHFLAG_NONE,DCCUNIT_FLUSHFLAG_SECMEM,DCCLINKER_FLUSHFLAG_NONE},
 /* level #4 */{DCCCOMPILER_FLUSHFLAG_DECLTAB,DCCUNIT_FLUSHFLAG_SYMTAB,DCCLINKER_FLUSHFLAG_NONE},
 /* level #5 */{DCCCOMPILER_FLUSHFLAG_DECLTAB|DCCCOMPILER_FLUSHFLAG_TABMIN,
                DCCUNIT_FLUSHFLAG_SYMTAB|DCCUNIT_FLUSHFLAG_TABMIN,DCCLINKER_FLUSHFLAG_NONE}
};

PRIVATE void DCC_ClearMem(int level, uint32_t flush_exclude) {
 assert(level >= 0 && level < CM_MAX_LEVEL);
 /* Flush global cache objects. */
 DCCCompiler_ClearCache();
 DCCUnit_ClearCache();
 if (level == 0) return;
 /* Flush compiler/unit memory. */
 DCCCompiler_Flush(&compiler,cm_level_flags[level][0]&~flush_exclude);
 DCCUnit_Flush(&unit,cm_level_flags[level][1]&~flush_exclude);
 DCCLinker_Flush(&linker,cm_level_flags[level][2]&~flush_exclude);
}

#undef DCC_Malloc
PUBLIC void *DCC_Malloc(size_t s, uint32_t flush_exclude) {
 void *result; int level = 0;
again:
 if ((result = malloc(s)) == NULL) {
  if (level == CM_MAX_LEVEL) DCC_AllocFailed(s);
  else { DCC_ClearMem(level++,flush_exclude); goto again; }
 }
 return result;
}
#undef DCC_Calloc
PUBLIC void *DCC_Calloc(size_t s, uint32_t flush_exclude) {
 void *result; int level = 0;
again:
 if ((result = calloc(1,s)) == NULL) {
  if (level == CM_MAX_LEVEL) DCC_AllocFailed(s);
  else { DCC_ClearMem(level++,flush_exclude); goto again; }
 }
 return result;
}
#undef DCC_Realloc
PUBLIC void *DCC_Realloc(void *p, size_t s, uint32_t flush_exclude) {
 void *result; int level = 0;
again:
 if ((result = realloc(p,s)) == NULL) {
  if (level == CM_MAX_LEVEL) DCC_AllocFailed(s);
  else { DCC_ClearMem(level++,flush_exclude); goto again; }
 }
 return result;
}
#undef DCC_Free
PUBLIC void DCC_Free(void *p) {
 free(p);
}
#if DCC_DEBUG
PUBLIC void *
DCC_dbg_realloc(void *p, size_t s, char const *f, int l) {
 /* A debug version of realloc() that forces
  * allocation into a different pointer. */
 void *result; size_t old_size = (size_t)-1;
 (void)f,(void)l;
#ifdef _MSC_VER
 if (p) {
#ifdef _CRTDBG_MAP_ALLOC
  old_size = _msize_dbg(p,_NORMAL_BLOCK);
#else
  old_size = _msize(p);
#endif
 }
#endif
#ifdef _CRTDBG_MAP_ALLOC
 result = _realloc_dbg(p,s,_NORMAL_BLOCK,f,l);
#else
 result = realloc(p,s);
#endif
 if (result) {
  if (result == p) {
#ifdef _CRTDBG_MAP_ALLOC
   void *newresult = _malloc_dbg(s,_NORMAL_BLOCK,f,l);
#else
   void *newresult = malloc(s);
#endif
   if (newresult) {
    memcpy(newresult,result,s);
    free(result);
    result = newresult;
   }
  }
  if (s > old_size) {
   memset((void *)((uintptr_t)result+old_size),
           0xac,s-old_size);
  }
 }
 return result;
}
#endif


PUBLIC void DCC_AllocFailed(size_t s) {
 WARN(W_OUT_OF_MEMORY,s);
}


PUBLIC int DCCStream_PadSize(stream_t fd, size_t n_bytes) {
 void *buffer;
 if ((buffer = calloc(1,n_bytes)) != NULL) {
  int result = s_writea(fd,buffer,n_bytes);
  free(buffer);
  return result;
 } else while (--n_bytes) {
  static char const zero[1] = {0};
  if (!s_writea(fd,zero,1)) return 0;
 }
 return 1;
}
PUBLIC int DCCStream_PadAddr(stream_t fd, DCC(soff_t) offset) {
 DCC(soff_t) ptr = s_seek(fd,0,SEEK_CUR);
 if (ptr >= offset) return 0;
 return DCCStream_PadSize(fd,offset-ptr);
}


DCC_DECL_END

#endif /* !GUARD_DCC_TPP_COMMON_C */
