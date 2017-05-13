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
#include <dcc/compiler.h>

#include "lexer-priv.h"

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

extern uint8_t const chrattr[256];

extern char *skip_whitespace_and_comments(char *iter, char *end);

LEXPRIV char *DCC_PARSE_CALL peek_next_token(void) {
 char *result,*end,*file_begin;
again:
 end = TOKEN.t_file->f_end;
 result = skip_whitespace_and_comments(TOKEN.t_end,end);
 if (result == end) {
  int extend_error;
  file_begin = TOKEN.t_file->f_begin;
  /* Special case: Must extend the file. */
  extend_error = TPPFile_NextChunk(TOKEN.t_file,TPPFILE_NEXTCHUNK_FLAG_EXTEND);
  TOKEN.t_begin = TOKEN.t_file->f_begin+(TOKEN.t_begin-file_begin);
  TOKEN.t_end   = TOKEN.t_file->f_begin+(TOKEN.t_end-file_begin);
  result        = TOKEN.t_file->f_begin+(result-file_begin);
  /* If the file was extended, search for the next token again. */
  if likely(extend_error) goto again;
 }
 return result;
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_WarnAllocaInLoop(void) {
 /* Warn if used inside a loop construct */
 if (compiler.c_flags&DCC_COMPILER_FLAG_INLOOP
     ) WARN(W_BUILTIN_ALLOCA_IN_LOOP);
}

PUBLIC int DCCParse_Pragma(void) {
 switch (TOK) {

 {
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

 { /* Emit a comment for the compiler/linker. */
  tok_t comment_group;
  struct TPPString *comment_string;
 case KWD_comment:
  YIELD();
  if (TOK != '(') WARN(W_EXPECTED_LPAREN); else YIELD();
  comment_group = TOK;
  if (!TPP_ISKEYWORD(comment_group)) WARN(W_PRAGMA_COMMENT_EXPECTED_KEYWORD);
  else if (comment_group != KWD_lib) WARN(W_PRAGMA_COMMENT_UNKNOWN);
  YIELD();
  if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
  if (TOK != TOK_STRING)
   comment_string = NULL,
   WARN(W_PRAGMA_COMMENT_EXPECTED_STRING);
  else comment_string = TPPLexer_ParseString();
  if (comment_string) {
   if (comment_group == KWD_lib) {
    DCCUnit_DynLoad(comment_string->s_text,1); /* Load a new library. */
   }
   TPPString_Decref(comment_string);
  }
  if (TOK != ')') WARN(W_EXPECTED_RPAREN); else YIELD();
 } break;

 default:
  WARN(W_PRAGMA_UNKNOWN);
  break;
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
  return TPPString_NewSized(0);
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
