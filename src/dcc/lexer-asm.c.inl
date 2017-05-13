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
#ifndef GUARD_DCC_LEXER_ASM_C_INL
#define GUARD_DCC_LEXER_ASM_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/unit.h>
#include <dcc/assembler.h>
#include <dcc/compiler.h>

#include "lexer-priv.h"

#include <string.h>

DCC_DECL_BEGIN

LEXPRIV void DCC_PARSE_CALL
DCCAsm_ExecStringInherited(struct TPPString *__restrict asm_text) {
 struct TPPFile *asm_file;
 asm_file = TPPFile_NewExplicitInherited(asm_text);
 if unlikely(!asm_file) TPPString_Decref(asm_text);
 else {
  struct TPPFile *old_eob;
  /* Setup the last file in a way that will cause the
   * current token (likely a ';') to be parsed again. */
  assert(TOKEN.t_begin >= TOKEN.t_file->f_begin &&
         TOKEN.t_begin <= TOKEN.t_file->f_end);
  TOKEN.t_file->f_pos = TOKEN.t_begin;
  TPPLexer_PushFileInherited(asm_file);
  /* Set the EOB file as the asm text, thus ensuring
   * that nothing past it will be parsed as assembly. */
  old_eob = TPPLexer_Current->l_eob_file;
  TPPLexer_Current->l_eob_file = asm_file;
  DCCParse_AsmBegin();
  /* Disable macros inside of assembly strings. */
  TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_NO_MACROS|
                                TPPLEXER_FLAG_NO_DIRECTIVES|
                                TPPLEXER_FLAG_NO_BUILTIN_MACROS);
  YIELD(); /* Yield the initial token using ASM configurations. */
  /* Parse assembly directives. */
  while (TOK > 0) DCCParse_AsmInstr();
  DCCParse_AsmEnd();
  TPPLexer_Current->l_eob_file = old_eob;
  YIELD(); /* Yield the next token after the assembly (which should be a ';'). */
 }
}


/* input/output assembly operands. */
struct asm_operand {
 /*ref*/struct TPPString *ao_constraints; /*< [1..1] User-string of assembly constraints. */
 struct TPPKeyword       *ao_name;        /*< [0..1] Optional operand name. */
#define ASMOP_FLAG_IN     0x0001          /*< Set for input operands. */
#define ASMOP_FLAG_OUT    0x0002          /*< Set for output operands. */
#define ASMOP_FLAG_RW     0x0004          /*< '+'. */
 uint16_t                 ao_flags;       /*< Set of 'ASMOP_FLAG_*'. */
 rc_t                     ao_reg;         /*< Explicit register in which the operand must life, register class, 'DCC_RC_CONST' for memory locations. */
 struct DCCStackValue    *ao_value;       /*< [1..1] stack value of this operand. */
 char                     ao_repr[16];    /*< Zero-terminated string for the representation within the assembly string. */
};

LEXPRIV void DCC_PARSE_CALL
DCCParse_AsmOp(struct asm_operand *__restrict self) {
 struct TPPString *constraints;
 assert(self);
 memset(self,0,sizeof(struct asm_operand));
 if (TOK == '[') {
  YIELD(); /* Named operand value. */
  if (!TPP_ISKEYWORD(TOK)) WARN(W_IASM_EXPECTED_KEYWORD_FOR_NAMED_OPERAND);
  else { self->ao_name = TOKEN.t_kwd; YIELD(); }
  if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
 }
 if (TPP_ISSTRING(TOK)) {
  constraints = DCCParse_String();
 } else {
  WARN(W_IASM_EXPECTED_STRING_FOR_CONSTRAINTS);
  constraints = NULL;
 }
 if (!constraints) constraints = TPPString_NewSized(0);
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 DCCParse_ParPairEnd();
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_AsmConstraint(struct TPPString *asm_text) {


 DCCAsm_ExecStringInherited(asm_text);
}


LEXPRIV void DCC_PARSE_CALL
DCCParse_AsmStmt(int global) {
 struct TPPString *asm_text;
 assert(TOK == KWD_asm ||
        TOK == KWD___asm ||
        TOK == KWD___asm__);
 YIELD();
 if (TOK == KWD_volatile ||
     TOK == KWD___volatile ||
     TOK == KWD___volatile__) YIELD();
 else if (TOK == KWD_goto && !global) {
  YIELD(); /* TODO: goto-mode. */
 }
 DCCParse_ParPairBegin();
 if (!TPP_ISSTRING(TOK)) WARN(W_STMT_ASM_EXPECTED_STRING),asm_text = NULL;
 else asm_text = DCCParse_String();
 if (!global && TOK == ':') {
  /* TODO: Assembly constraints & input/output arguments. */
  DCCParse_AsmConstraint(asm_text);
 } else if (asm_text) {
  DCCAsm_ExecStringInherited(asm_text);
 }
 DCCParse_ParPairEnd();
}


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_ASM_C_INL */
