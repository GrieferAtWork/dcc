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

#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef _WIN32
#include <Windows.h>
#endif

DCC_DECL_BEGIN

#if DCC_DEBUG
#ifdef _WIN32
PRIVATE void dcc_voutf(char const *fmt, va_list args) {
 char buffer[4096];
 vsnprintf(buffer,sizeof(buffer),fmt,args);
 OutputDebugStringA(buffer);
 fwrite(buffer,sizeof(char),strlen(buffer),stderr);
}
PRIVATE void dcc_outf(char const *fmt, ...) {
 va_list args;
 va_start(args,fmt);
 dcc_voutf(fmt,args);
 va_end(args);
}
#else
#define dcc_voutf(fmt,args) vfprintf(stderr,fmt,args)
#define dcc_outf(...)       fprintf(stderr,__VA_ARGS__)
#endif


PUBLIC
#ifdef DCC_NO_BREAKPOINT
       DCC_ATTRIBUTE_NORETURN
#endif
void dcc_assertion_failed(char const *expr, char const *file,
                          int line, char const *fmt, ...) {
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
#ifdef DCC_NO_BREAKPOINT
 _exit(1);
#endif
}
#endif /* DCC_DEBUG */


#define CM_MAX_LEVEL 6
static uint32_t const cm_level_flags[CM_MAX_LEVEL][2] = {
 /* level #0 */{DCCCOMPILER_FLUSHFLAG_NONE,DCCUNIT_FLUSHFLAG_NONE},
 /* level #1 */{DCCCOMPILER_FLUSHFLAG_PACKSTACK|
                DCCCOMPILER_FLUSHFLAG_VISISTACK,DCCUNIT_FLUSHFLAG_NONE},
 /* level #2 */{DCCCOMPILER_FLUSHFLAG_VSTACK,DCCUNIT_FLUSHFLAG_NONE},
 /* level #3 */{DCCCOMPILER_FLUSHFLAG_NONE,DCCUNIT_FLUSHFLAG_SECMEM},
 /* level #4 */{DCCCOMPILER_FLUSHFLAG_DECLTAB,DCCUNIT_FLUSHFLAG_SYMTAB},
 /* level #5 */{DCCCOMPILER_FLUSHFLAG_DECLTAB|DCCCOMPILER_FLUSHFLAG_TABMIN,
                DCCUNIT_FLUSHFLAG_SYMTAB|DCCUNIT_FLUSHFLAG_TABMIN}
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
PUBLIC void DCC_AllocFailed(size_t s) {
 WARN(W_OUT_OF_MEMORY,s);
}


DCC_DECL_END

#endif /* !GUARD_DCC_TPP_COMMON_C */
