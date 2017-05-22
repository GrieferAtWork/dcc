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
#ifndef GUARD_DCC_LEXER_C
#define GUARD_DCC_LEXER_C 1

#include <dcc/common.h>
#include <dcc/unit.h>
#include <dcc/lexer.h>
#include <dcc/linker.h>
#include <dcc/compiler.h>

#include "lexer-priv.h"

#include "cmd.h"

#include <string.h>
#include <stdlib.h>
#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

DCC_DECL_BEGIN

#ifndef __INTELLISENSE__
PUBLIC struct TPPKeyword_Empty_struct
TPPKeyword_Empty_data = {
 /* k_next  */NULL,
 /* k_macro */NULL,
 /* k_rare  */NULL,
 /* k_id    */TOK_KEYWORD_BEGIN,
 /* k_size  */0,
 /* k_hash  */1,
 /* k_zero  */0,
};
#endif


LEXPRIV struct TPPKeyword *DCC_PARSE_CALL tok_without_underscores(void) {
 struct TPPKeyword *result = NULL;
 if (TPP_ISKEYWORD(TOK)) {
  result = TOKEN.t_kwd;
  if (result->k_name[0] == '_' ||
      result->k_name[result->k_size-1] == '_') {
   char const *begin,*end;
   /* Keyword has leading/terminating underscores.
    * >> Remove them and use that keyword instead! */
   end = (begin = result->k_name)+result->k_size;
   while (begin != end && *begin == '_') ++begin;
   while (end != begin && end[-1] == '_') --end;
   /* NOTE: Don't create the keyword if it doesn't exist!
    *    >> Callers only use this function to unify attribute names & arguments! */
   result = TPPLexer_LookupKeyword(begin,(size_t)(end-begin),0);
  }
 }
 return result;
}

extern char *skip_whitespace_and_comments(char *iter, char *end);

LEXPRIV char *DCC_PARSE_CALL
peek_next_token(struct TPPFile **tok_file) {
 char *result = TOKEN.t_end,*end,*file_begin;
 struct TPPFile *curfile = TOKEN.t_file;
again:
 end = curfile->f_end;
 result = skip_whitespace_and_comments(result,end);
 if (result == end) {
  int extend_error;
  file_begin = curfile->f_begin;
  /* Special case: Must extend the file. */
  extend_error = TPPFile_NextChunk(curfile,TPPFILE_NEXTCHUNK_FLAG_EXTEND);
  if (curfile == TOKEN.t_file) {
   TOKEN.t_begin = curfile->f_begin+(TOKEN.t_begin-file_begin);
   TOKEN.t_end   = curfile->f_begin+(TOKEN.t_end-file_begin);
  }
  result = curfile->f_begin+(result-file_begin);
  /* If the file was extended, search for the next token again. */
  if likely(extend_error) goto again;
  /* Continue searching through the include-stack. */
  if (curfile->f_prev) {
   curfile = curfile->f_prev;
   result = curfile->f_pos;
   goto again;
  }
 }
 if (tok_file) *tok_file = curfile;
 return result;
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_WarnAllocaInLoop(void) {
 /* Warn if used inside a loop construct */
 if (compiler.c_flags&DCC_COMPILER_FLAG_INLOOP
     ) WARN(W_BUILTIN_ALLOCA_IN_LOOP);
}

PRIVATE void DCCLinker_LibPathClear(void) {
 struct TPPString **iter,**end;
 end = (iter = linker.l_paths.lp_pathv)+
               linker.l_paths.lp_pathc;
 for (; iter != end; ++iter)
      assert(*iter),
      TPPString_Decref(*iter);
 linker.l_paths.lp_pathc = 0;
}

PUBLIC int DCCParse_Pragma(void) {
 switch (TOK) {

 { /* #pragma weak foo */
   /* #pragma weak foo = bar */
  struct DCCSym *weaksym,*alias;
 case KWD_weak:
  /* declare a given symbol as weak. */
  YIELD();
  if (!TPP_ISKEYWORD(TOK)) { WARN(W_PRAGMA_WEAK_EXPECTES_KEYWORD); break; }
  weaksym = DCCUnit_NewSym(TOKEN.t_kwd,DCC_SYMFLAG_NONE);
  if unlikely(!weaksym) break;
  YIELD();
  if (weaksym->sy_flags&DCC_SYMFLAG_WEAK)
      WARN(W_PRAGMA_WEAK_ALREADY_WEAK,weaksym->sy_name);
  weaksym->sy_flags |= DCC_SYMFLAG_WEAK;
  if (TOK == '=') {
   YIELD();
   if (!TPP_ISKEYWORD(TOK)) { WARN(W_PRAGMA_WEAK_EXPECTES_KEYWORD_AFTER_EQUAL); break; }
   alias = DCCUnit_NewSym(TOKEN.t_kwd,DCC_SYMFLAG_NONE);
   if unlikely(!alias) break;
   /* Define 'weaksym' to be an alias for 'alias' */
   DCCSym_Alias(weaksym,alias,0);
   YIELD();
  }
 } break;

 { /* DCC Pragma namespace. */
 case KWD_DCC:
  YIELD();
  switch (TOK) {
  { /* Configure library search paths. */
  case KWD_library_path:
   YIELD();
   if unlikely(TOK != '(') TPPLexer_Warn(W_EXPECTED_LPAREN);
   else YIELD();
   while (TOK != ')') {
    switch (TOK) {
     { /* push/pop the library-path vector. */
      if (DCC_MACRO_FALSE) { case KWD_clear: DCCLinker_LibPathClear(); }
      if (DCC_MACRO_FALSE) { case KWD_push:  DCCLinker_LibPathPush(); }
      if (DCC_MACRO_FALSE) { case KWD_pop:   DCCLinker_LibPathPop(); }
      YIELD();
     } break;
     {
      int mode,error;
      struct TPPConst path_string;
     case '+': YIELD();
     default:  mode = 1;
      if (DCC_MACRO_FALSE) { case '-': mode = 0; YIELD(); }
      if unlikely(!TPPLexer_Eval(&path_string)) return 0;
      if (path_string.c_kind != TPP_CONST_STRING) {
       TPPLexer_Warn(W_PRAGMA_LIBRARY_PATH_EXPECTED_STRING,&path_string);
      } else {
       char *path; size_t size = path_string.c_data.c_string->s_size;
       if (path_string.c_data.c_string->s_refcnt == 1) {
        path = path_string.c_data.c_string->s_text;
       } else {
        path = (char *)DCC_Malloc((size+1)*sizeof(char),0);
        if unlikely(!path) return 0;
        memcpy(path,path_string.c_data.c_string->s_text,
              (size+1)*sizeof(char));
       }
       error = mode ? DCCLinker_AddLibPath(path,size)
                    : DCCLinker_DelLibPath(path,size);
       if unlikely(mode && !error) return 0;
       if unlikely(error == 2) error = 0;
       if unlikely(!error) {
        WARN(mode ? W_PRAGMA_LIBRARY_PATH_ALREADY_EXISTS
                  : W_PRAGMA_LIBRARY_PATH_UNKNOWN,
             path,size);
       }
       if (path != path_string.c_data.c_string->s_text) DCC_Free(path);
       TPPString_Decref(path_string.c_data.c_string);
      }
      break;
     }
    }
    if (TOK != ',') break;
    YIELD();
   }
   if unlikely(TOK != ')') TPPLexer_Warn(W_EXPECTED_RPAREN);
   else YIELD();
  } break;
  default: break; /* Reserved for future use (don't warn about unknown dcc-pragma). */
  }
 } break;

 { /* #pragma pack(8) */
   /* #pragma pack(push) */
   /* #pragma pack(pop) */
 case KWD_pack:
  YIELD();
  if (TOK != '(') WARN(W_EXPECTED_LPAREN); else YIELD();
  while (TOK != ')') {
        if (TOK == KWD_push) DCCCompiler_PackPush();
   else if (TOK == KWD_pop) DCCCompiler_PackPop();
   else {
    target_siz_t packval = (target_siz_t)DCCParse_CExpr(1);
    if (packval & (packval-1))
        WARN(W_PRAGMA_PACK_EXPECTED_POWER_OF_TWO,packval);
    compiler.c_pack.ps_pack = packval;
    goto skip_yield_after_pack;
   }
   YIELD();
skip_yield_after_pack:
   if (TOK != ',') break;
   YIELD();
  }
  if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
 } break;

 { /* #pragma comment(lib,"foo.dll") */
  tok_t comment_group;
  struct TPPString *comment_string;
 case KWD_comment:
  /* Emit a comment for the compiler/linker. */
  YIELD();
  if (TOK != '(') WARN(W_EXPECTED_LPAREN); else YIELD();
  comment_group = TOK;
  if (!TPP_ISKEYWORD(comment_group)) WARN(W_PRAGMA_COMMENT_EXPECTED_KEYWORD);
  else if (comment_group != KWD_lib &&
           comment_group != KWD_compiler &&
           comment_group != KWD_linker) WARN(W_PRAGMA_COMMENT_UNKNOWN);
  YIELD();
  if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
  if (TOK != TOK_STRING)
   comment_string = NULL,
   WARN(W_PRAGMA_COMMENT_EXPECTED_STRING);
  else comment_string = TPPLexer_ParseString();
  if (comment_string) {
   if (comment_group == KWD_lib) {
    struct DCCLibDef def;
    def.ld_flags    = DCC_LIBDEF_FLAG_NONE;
    def.ld_name     = comment_string->s_text;
    def.ld_size     = comment_string->s_size;
    def.ld_expsymfa = (symflag_t)-1;
    def.ld_expsymfo = (symflag_t) 0;
    def.ld_impsymfa = (symflag_t)-1;
    def.ld_impsymfo = (symflag_t) 0;
    DCCUnit_Import(&def); /* Load a new library. */
   } else if (comment_group == KWD_compiler) {
    DCCCmd_ExecString(OPG_grp_main,
                      comment_string->s_text,
                      comment_string->s_size);
   } else if (comment_group == KWD_linker) {
    DCCCmd_ExecString(OPG_grp_Wl,
                      comment_string->s_text,
                      comment_string->s_size);
   }
   TPPString_Decref(comment_string);
  }
  if (!OK) break;
  if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
 } break;

 {
 default:
  WARN(W_PRAGMA_UNKNOWN);
 } break;
 }

 return 1; /* Never re-emit pragmas. */
}

PUBLIC int DCCParse_GCCPragma(void) {
 switch (TOK) {

 { /* #pragma GCC visibility push("visibility") */
   /* #pragma GCC visibility pop */
 case KWD_visibility:
  YIELD();
  if (TOK == KWD_push) {
   struct TPPString *vis_text;
   DCCCompiler_VisibilityPush();
   YIELD();
   if (TOK != '(') WARN(W_EXPECTED_LPAREN); else YIELD();
   if (TOK != TOK_STRING) WARN(W_PRAGMA_GCC_VISIBILITY_EXPECTED_STRING);
   else if ((vis_text = TPPLexer_ParseString()) != NULL) {
    symflag_t newvis = DCCSymflag_FromString(vis_text);
    if (newvis == (symflag_t)-1)
     WARN(W_PRAGMA_GCC_VISIBILITY_UNKNOWN_VISIBILITY,vis_text->s_text);
    else compiler.c_visibility.vs_viscur = newvis;
    TPPString_Decref(vis_text);
   }
   if (TOK != ')') WARN(W_EXPECTED_LPAREN); else YIELD();
  } else if (TOK == KWD_pop) {
   DCCCompiler_VisibilityPop();
   YIELD();
  } else {
   WARN(W_PRAGMA_GCC_VISIBILITY_EXPECTED_PUSH_OR_POP);
  }
 } break;

 {
 default: ;
 } break;
 }
 return 1; /* Never re-emit pragmas. */
}


PUBLIC int
DCCParse_InsComment(struct TPPString *__restrict comment) {
 struct DCCSection *sec;
 sec = DCCUnit_NewSecs(".comment",
                       DCC_SYMFLAG_SEC(1,0,0,0,0,0));
 if (sec) {
  /* Allocate memory into the '.comment' section,
   * and keep the reference dangling. */
  DCCSection_DAllocMem(sec,comment->s_text,
                      (comment->s_size+0)*sizeof(char),
                      (comment->s_size+1)*sizeof(char),
                       1,0);
 }
 return OK;
}


LEXPRIV /*ref*/struct TPPString *DCC_PARSE_CALL
DCCParse_GetFunction(void) {
 char const *name; size_t size;
 if (!compiler.c_fun) {
  WARN(W_EXPR_FUNC_OUTSIDE_OF_FUNCTION);
  name = NULL;
  size = 0;
 } else {
  struct TPPKeyword const *name_kwd;
  name_kwd = compiler.c_fun->d_name;
  assert(name_kwd);
  name = name_kwd->k_name;
  size = name_kwd->k_size;
 }
 return TPPString_New(name,size);
}
LEXPRIV /*ref*/struct TPPString *DCC_PARSE_CALL
DCCParse_GetPrettyFunction(void) {
 if (!compiler.c_fun) {
  WARN(W_EXPR_FUNC_OUTSIDE_OF_FUNCTION);
  TPPString_Incref(TPPFile_Empty.f_text);
  return TPPFile_Empty.f_text;
 }
 return DCCType_ToTPPString(&compiler.c_fun->d_type,
                             compiler.c_fun->d_name);
}

LEXPRIV /*ref*/struct TPPString *DCC_PARSE_CALL
DCCParse_OneStringExt(void) {
 struct TPPString *result;
 if (TOK == TOK_STRING) {
  result = TPPLexer_ParseString();
 } else {
  result = TOK == KWD___PRETTY_FUNCTION__
   ? DCCParse_GetPrettyFunction()
   : DCCParse_GetFunction();
  YIELD();
 }
 return result;
}

PUBLIC /*ref*/struct TPPString *
DCCParse_String(void) {
 struct TPPString *result;
 assert(TPP_ISSTRING(TOK));
 if (!DCC_LX_HAS(EXT_FUNCTION_STRING_LITERALS)) {
  /* Simple case: Only parse a regular, old c-string with concatation. */
  return TPPLexer_ParseString();
 }
 result = DCCParse_OneStringExt();
 while (TPP_ISSTRING(TOK)) {
  struct TPPString *append;
  append = DCCParse_OneStringExt();
  result = TPPString_Cat(result,append);
  if unlikely(!result) break;
 }
 return result;
}

#if DCC_TARGET_OS == DCC_OS_WINDOWS
#define INVALID_PATH_CHARACTERS "/\\:?*;"
#elif !!(DCC_TARGET_OS&DCC_OS_UNIX)
#define INVALID_PATH_CHARACTERS "/\\:?*"
#endif

PUBLIC struct TPPKeyword *
DCCParse_GetLibname(char const *__restrict name, size_t size) {
 struct TPPKeyword *result;
 char *mbuf,*buf,*iter,*end;
 /* Don't perform any formating if not requested, to. */
 if (!HAS(EXT_CANONICAL_LIB_NAMES))
  return TPPLexer_LookupKeyword(name,size,1);
 /* Strip surrounding whitespace. */
 while (size && tpp_isspace(name[0])) ++name,--size;
 while (size && tpp_isspace(name[size-1])) --size;
 if (size < 64) {
  buf = (char *)alloca((size+1)*sizeof(char));
  mbuf = NULL;
 } else {
  buf = mbuf = (char *)malloc((size+1)*sizeof(char));
  if unlikely(!mbuf) { TPPLexer_SetErr(); return NULL; }
 }
#if DCC_TARGET_OS == DCC_OS_WINDOWS && 0 /* Don't do this... */
 {
  char *dst,ch;
  end = (iter = (char *)name)+size;
  for (dst = buf; iter != end; ) {
   ch = *iter++;
   if (ch >= 'a' && ch <= 'z') ch -= 'a'-'A';
   *dst++ = ch;
  }
 }
#else
 memcpy(buf,name,size*sizeof(char));
#endif
 end = (iter = buf)+size;
 for (; iter != end; ++iter) {
#ifdef INVALID_PATH_CHARACTERS
  if (strchr(INVALID_PATH_CHARACTERS,*iter))
   WARN(W_INVALID_LIBRARY_CHARACTER,*iter,name,size);
#endif
#if DCC_TARGET_OS == DCC_OS_WINDOWS
  if (*iter == '/') *iter = '\\';
#elif !!(DCC_TARGET_OS&DCC_OS_UNIX)
  if (*iter == '\\') *iter = '/';
#endif
 }
 result = TPPLexer_LookupKeyword(buf,size,1);
 free(mbuf);
 return result;
}


DCC_DECL_END

#ifndef __INTELLISENSE__
#include "lexer-asm.c.inl"
#include "lexer-attributes.c.inl"
#include "lexer-builtins.c.inl"
#include "lexer-ctype.c.inl"
#include "lexer-decl.c.inl"
#include "lexer-expr.c.inl"
#include "lexer-init.c.inl"
#include "lexer-stmt.c.inl"
#endif


#endif /* !GUARD_DCC_LEXER_C */
