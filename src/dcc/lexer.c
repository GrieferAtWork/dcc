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
#define _BSD_SOURCE 1 /* Enable usleep() */

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/linker.h>
#include <dcc/unit.h>

#if DCC_DEBUG
#include <drt/drt.h>
#endif

#include "lexer-priv.h"

#include "cmd.h"

#include <string.h>
#include <stdlib.h>
#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#include <dcc_winmin.h> /* Sleep() */
#define dcc_sleep(msecs) Sleep((DWORD)(msecs))
#else
#include <unistd.h> /* sleep() */
#define dcc_sleep(msecs) usleep((useconds_t)(msecs)*1000)
#endif

DCC_DECL_BEGIN

struct TPPStringEmpty {
 refcnt_t s_refcnt;
 size_t   s_size;
 char     s_text[1];
};

INTDEF struct TPPStringEmpty tpp_empty_string;
#define empty_string  ((struct TPPString *)&tpp_empty_string)
#define SPECIAL_FILE(name,hash) \
{\
 /* f_refcnt                 */0x80000000,\
 /* f_kind                   */TPPFILE_KIND_TEXT,\
 /* f_prev                   */NULL,\
 /* f_name                   */(char *)(name),\
 /* f_namesize               */DCC_COMPILER_STRLEN(name),\
 /* f_namehash               */hash,\
 /* f_text                   */empty_string,\
 /* f_begin                  */empty_string->s_text,\
 /* f_end                    */empty_string->s_text,\
 /* f_pos                    */empty_string->s_text,{{\
 /* f_textfile.f_cacheentry  */NULL,\
 /* f_textfile.f_usedname    */NULL,\
 /* f_textfile.f_lineoff     */0,\
 /* f_textfile.f_stream      */TPP_STREAM_INVALID,\
 /* f_textfile.f_ownedstream */TPP_STREAM_INVALID,\
 /* f_textfile.f_guard       */NULL,\
 /* f_textfile.f_cacheinc    */0,\
 /* f_textfile.f_rdata       */0,\
 /* f_textfile.f_prefixdel   */0,\
 /* f_textfile.f_flags       */TPP_TEXTFILE_FLAG_NOGUARD|TPP_TEXTFILE_FLAG_INTERNAL,\
 /* f_textfile.f_encoding    */TPP_ENCODING_UTF8,\
 /* f_textfile.f_padding     */{0},\
 /* f_textfile.f_newguard    */NULL}}\
}

PUBLIC struct TPPFile TPPFile_Cmd    = SPECIAL_FILE("CMD",22846089ul);
#if __SIZEOF_SIZE_T__ == 4
PUBLIC struct TPPFile TPPFile_Merge  = SPECIAL_FILE("MERGE",147485999ul);
PUBLIC struct TPPFile TPPFile_Linker = SPECIAL_FILE("LINKER",2160376288ul);
#elif __SIZEOF_SIZE_T__ == 8
PUBLIC struct TPPFile TPPFile_Merge  = SPECIAL_FILE("MERGE",1627940091183ull);
PUBLIC struct TPPFile TPPFile_Linker = SPECIAL_FILE("LINKER",426909024696800ull);
#else
#   error FIXME
#endif
#if DCC_CONFIG_HAVE_DRT
PUBLIC struct TPPFile TPPFile_DRT    = SPECIAL_FILE("DRT",22916589ul);
#endif /* DCC_CONFIG_HAVE_DRT */


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

INTDEF char *skip_whitespace_and_comments(char *iter, char *end);

LEXPRIV char *DCC_PARSE_CALL
peek_next_token(struct TPPFile **tok_file) {
 if (tok_file) *tok_file = TOKEN.t_file;
 return peek_next_advance(TOKEN.t_end,tok_file);
}

LEXPRIV char *DCC_PARSE_CALL
peek_next_advance(char *p, struct TPPFile *__restrict *tok_file) {
 char *result = p,*end,*file_begin;
 struct TPPFile *curfile;
 if (tok_file) {
  curfile = *tok_file;
  assert(curfile);
 } else {
  curfile = TOKEN.t_file;
 }
 assert(p >= curfile->f_begin &&
        p <= curfile->f_end);
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


INTDEF struct TPPKeyword *
lookup_escaped_keyword(char const *name, size_t namelen,
                       size_t unescaped_size, int create_missing);

LEXPRIV struct TPPKeyword *DCC_PARSE_CALL
peek_keyword(struct TPPFile *__restrict tok_file,
             char *__restrict tok_begin, int create_missing) {
 struct TPPKeyword *kwd_entry;
 size_t name_escapesize,name_size = 1;
 uint8_t chflags = CH_ISALPHA;
 char *iter = tok_begin,*end;
 assert(tok_file);
 assert(tok_begin);
 assert(tok_begin >= tok_file->f_begin);
 assert(tok_begin <= tok_file->f_end);
 end = tok_file->f_end;
 while (SKIP_WRAPLF(iter,end));
 if (iter == end) return NULL; /* EOF */
 /* Set the ANSI flag if we're supporting those characters. */
 if (HAS(EXT_EXTENDED_IDENTS)) chflags |= CH_ISANSI;
 if (!(chrattr[*iter]&chflags) ||
    (!HAS(EXT_DOLLAR_IS_ALPHA) && *iter == '$'))
      return NULL; /* Not-a-keyword. */
 /* All non-first characters are allowed to be digits as well. */
 chflags |= CH_ISDIGIT;
 ++iter;
 /* keyword: scan until a non-alnum character is found. */
 if (HAS(EXT_DOLLAR_IS_ALPHA)) for (;;) {
  while (SKIP_WRAPLF(iter,end));
  if (!(chrattr[*iter]&chflags)) break;
  ++iter,++name_size;
 } else for (;;) {
  while (SKIP_WRAPLF(iter,end));
  if (!(chrattr[*iter]&chflags) || *iter == '$') break;
  ++iter,++name_size;
 }
 /* Lookup/generate the token id of this keyword. */
 name_escapesize = (size_t)(iter-tok_begin);
 if (name_size == name_escapesize) {
  kwd_entry = TPPLexer_LookupKeyword(tok_begin,name_size,create_missing);
 } else {
  kwd_entry = lookup_escaped_keyword(tok_begin,name_escapesize,name_size,create_missing);
 }
 return kwd_entry;
}

LEXPRIV struct TPPKeyword *DCC_PARSE_CALL
peek_next_keyword(int create_missing) {
 struct TPPFile *tok_file;
 char *tok_begin = peek_next_token(&tok_file);
 return peek_keyword(tok_file,tok_begin,create_missing);
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
  TPPLexer_Current->l_flags &= ~(TPPLEXER_FLAG_NO_MACROS|
                                 TPPLEXER_FLAG_NO_DIRECTIVES|
                                 TPPLEXER_FLAG_NO_BUILTIN_MACROS);
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
   TPPLexer_Current->l_flags &= ~(TPPLEXER_FLAG_NO_MACROS|
                                  TPPLEXER_FLAG_NO_DIRECTIVES|
                                  TPPLEXER_FLAG_NO_BUILTIN_MACROS);
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
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
       WARN(W_PRAGMA_LIBRARY_PATH_EXPECTED_STRING,&path_string);
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
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  } break;

  default:
   /* Reserved for future use (don't warn about unknown dcc-pragma). */
#if DCC_DEBUG
   if (TPP_ISKEYWORD(TOKEN.t_id) &&
      !strcmp(TOKEN.t_kwd->k_name,"__sleep")) {
    struct TPPConst val;
    /* Let the compiler sleep for a while.
     * >> Used to simulate pauses in input streams &
     *    aiding in the debugging of DRT symbol waiting. */
    YIELD();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN); else YIELD();
    if (TPPLexer_Eval(&val)) {
     TPPConst_ToInt(&val);
     DRT_Sync();
     dcc_sleep(val.c_data.c_int);
    }
    if (TOK != ')') WARN(W_EXPECTED_LPAREN); else YIELD();
   }
#endif
   break;
  }
 } break;

 { /* #pragma pack(8) */
   /* #pragma pack(push) */
   /* #pragma pack(pop) */
 case KWD_pack:
  YIELD();
  TPPLexer_Current->l_flags &= ~(TPPLEXER_FLAG_NO_MACROS|
                                 TPPLEXER_FLAG_NO_DIRECTIVES|
                                 TPPLEXER_FLAG_NO_BUILTIN_MACROS);
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
  TPPLexer_Current->l_flags &= ~(TPPLEXER_FLAG_NO_MACROS|
                                 TPPLEXER_FLAG_NO_DIRECTIVES|
                                 TPPLEXER_FLAG_NO_BUILTIN_MACROS);
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
  TPPLexer_Current->l_flags &= ~(TPPLEXER_FLAG_NO_MACROS|
                                 TPPLEXER_FLAG_NO_DIRECTIVES|
                                 TPPLEXER_FLAG_NO_BUILTIN_MACROS);
  YIELD();
  if (TOK == KWD_push) {
   struct TPPString *vis_text;
   DCCCompiler_VisibilityPush();
   YIELD();
   if (TOK != '(') WARN(W_EXPECTED_LPAREN); else YIELD();
   if (TOK != TOK_STRING) {
         if (TOK == KWD_default)   compiler.c_visibility.vs_viscur = DCC_SYMFLAG_NONE;
    else if (TOK == KWD_hidden)    compiler.c_visibility.vs_viscur = DCC_SYMFLAG_HIDDEN;
    else if (TOK == KWD_protected) compiler.c_visibility.vs_viscur = DCC_SYMFLAG_PROTECTED;
    else if (TOK == KWD_internal)  compiler.c_visibility.vs_viscur = DCC_SYMFLAG_INTERNAL;
    else WARN(W_PRAGMA_GCC_VISIBILITY_EXPECTED_STRING);
    if (TPP_ISKEYWORD(TOK)) YIELD();
   } else if ((vis_text = TPPLexer_ParseString()) != NULL) {
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
  return TPPString_NewEmpty();
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

#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
#   define INVALID_PATH_CHARACTERS "/\\:?*;"
#elif !!(DCC_TARGET_OS&DCC_OS_F_UNIX)
#   define INVALID_PATH_CHARACTERS "/\\:?*"
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
 memcpy(buf,name,size*sizeof(char));
 end = (iter = buf)+size;
#if defined(INVALID_PATH_CHARACTERS) || \
  !!(DCC_TARGET_OS&(DCC_OS_F_WINDOWS|DCC_OS_F_UNIX))
 for (; iter != end; ++iter) {
#ifdef INVALID_PATH_CHARACTERS
  if (strchr(INVALID_PATH_CHARACTERS,*iter))
      WARN(W_INVALID_LIBRARY_CHARACTER,*iter,name,size);
#endif
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
  if (*iter == '/') *iter = '\\';
#elif !!(DCC_TARGET_OS&DCC_OS_F_UNIX)
  if (*iter == '\\') *iter = '/';
#endif
 }
#endif /* ... */
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
#include "lexer-ctype-guess.c.inl"
#include "lexer-ctype-struct.c.inl"
#include "lexer-decl.c.inl"
#include "lexer-expr.c.inl"
#include "lexer-init.c.inl"
#include "lexer-stmt.c.inl"
#endif


#endif /* !GUARD_DCC_LEXER_C */
