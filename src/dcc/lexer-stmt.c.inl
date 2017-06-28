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
#ifndef GUARD_DCC_LEXER_STMT_C_INL
#define GUARD_DCC_LEXER_STMT_C_INL 1

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>
#include <dcc/lexer.h>
#include <dcc/unit.h>
#include <dcc/vstack.h>

#include <string.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

LEXPRIV void DCC_PARSE_CALL DCCParse_Scope(pflag_t f) {
 assert(TOK == '{');
 YIELD();
 if (TOK == '}') vpushv();
 else {
  pushscope_local();
  DCCParse_ScopeText(f);
  popscope_local();
 }
 if (TOK != '}') WARN(W_EXPECTED_RBRACE);
 else YIELD();
}

LEXPRIV void DCC_PARSE_CALL DCCParse_ScopeText(pflag_t f) {
 unsigned long tok_num;
 pflag_t pflags = DCC_PFLAG_NONE;
 for (;;) {
#if DCC_DEBUG
  size_t old_vsize = vsize;
#endif
  tok_num = TOKEN.t_num;
  if (!DCCParse_Stmt(pflags)) pflags |= DCC_PFLAG_WARNDECL;
  else if (pflags == DCC_PFLAG_WARNDECL) {
   pflags &= ~(DCC_PFLAG_WARNDECL);
   pflags |=  (DCC_PFLAG_WARNDECL << 1);
  }
#if DCC_DEBUG
  assertf(vsize == old_vsize+1,"Invalid v-stack size: %lu %s",
         (unsigned long)(vsize <= old_vsize ? (old_vsize+1)-vsize : vsize-(old_vsize+1)),
                        (vsize <= old_vsize ? "too few" : "too many"));
#endif
  if (TOK <= 0 || TOK == '}' ||
      tok_num == TOKEN.t_num) break;
  vpop(1);
 }
 /* If the last statement value is an l-value that doesn't refer
  * to a constant symbol, we must load it, because otherwise that
  * would mean it's referring to a location variable, which may
  * be part of the scope we're going to delete.
  */
 if ((f&DCC_PFLAG_USED) &&
     (vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
     (vbottom->sv_reg != DCC_RC_CONST)
      ) DCCStackValue_Load(vbottom);
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_Typedef(struct DCCType *__restrict base, struct DCCAttrDecl *__restrict attr) {
 struct DCCType type;
 struct TPPKeyword *ty_name;
 struct DCCDecl *ty_decl;
 /* A type prefix was parsed. */
 for (;;) {
  DCCType_InitCopy(&type,base);
  ty_name = DCCParse_CTypeSuffix(&type,attr);
  type.t_type &= ~(DCCTYPE_STOREMASK);
  ty_decl = NULL;
  if (ty_name != &TPPKeyword_Empty) {
   /* Local type declaration. */
   ty_decl = DCCCompiler_NewLocalDecl(ty_name,DCC_NS_LOCALS);
  }
  if likely(ty_decl) {
   if (ty_decl->d_kind != DCC_DECLKIND_NONE) {
    if (!(ty_decl->d_kind&DCC_DECLKIND_TYPE)) {
     WARN(W_DECL_ALREADY_DEFINED,ty_decl);
    } else if (!DCCType_IsCompatible(&ty_decl->d_type,&type,0)) {
     WARN(W_DECL_ALREADY_DEFINED,ty_decl);
     /* Redefine a typedef (used for by DCC fixed system headers to
      * redefine potentially wrong typedefs from system definitions).
      * NOTE: Using '#pragma GCC system_header', those
      *       headers will suppress the warning above. */
     DCCType_Quit(&ty_decl->d_type);
     ty_decl->d_type.t_base = NULL;
     /* Also delete old attributes. */
     if (ty_decl->d_attr) {
      DCCAttrDecl_Quit(ty_decl->d_attr);
      free(ty_decl->d_attr);
      ty_decl->d_attr = NULL;
     }
     goto deftype;
    }
   } else {
deftype:
    ty_decl->d_kind = DCC_DECLKIND_TYPE;
    DCCType_InitCopy(&ty_decl->d_type,&type);
    /* Apply symbol attributes. */
    DCCDecl_SetAttr(ty_decl,attr);
   }
  }
  DCCType_Quit(&type);
  if (TOK == '=') {
   YIELD();

  }
  if (TOK != ',') break;
  YIELD();
 }
}

LEXPRIV void DCC_PARSE_CALL DCCParse_DoWhileStmt(void) {
 struct DCCSym *b_label,*c_label,*start_label;
 assert(TOK == KWD_do);
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_INLOOP;
 YIELD();
 if unlikely((start_label = DCCUnit_AllocSym()) == NULL) return;
 if unlikely((b_label = DCCUnit_AllocSym()) == NULL) return;
 if unlikely((c_label = DCCUnit_AllocSym()) == NULL) return;
 push_bc();
 /* Setup the active break/continue labels. */
 compiler.c_bsym = b_label;
 compiler.c_csym = c_label;
 t_defsym(start_label);

 /* Parse the do ... while-block. */
 DCCParse_Stmt(DCC_PFLAG_NONE);
 vpop(1);

 if (TOK != KWD_while) WARN(W_EXPECTED_WHILE_AFTER_DO); else YIELD();

 t_defsym(c_label); /* 'continue' jumps to the condition. */

 /* Parse the condition expression. */
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 DCCParse_ParPairEnd();

 vpushs(start_label);
 vgen1('&');
 /* jump to the start_label label if the condition was met.
  * NOTE: 'vjcc' automatically performs constant-optimizations! */
 vjcc(0);
 t_defsym(b_label); /* 'break' jumps after the block. */
 pop_bc();
 /* TODO: propagate dead flags if 'b_label' has no relocations. */
 popf();
}

LEXPRIV void DCC_PARSE_CALL DCCParse_WhileStmt(void) {
 struct DCCSym *b_label,*c_label;
 assert(TOK == KWD_while);
 YIELD();
 if unlikely((b_label = DCCUnit_AllocSym()) == NULL) return;
 if unlikely((c_label = DCCUnit_AllocSym()) == NULL) return;
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_INLOOP;
 push_bc();
 /* Setup the active break/continue labels. */
 compiler.c_bsym = b_label;
 compiler.c_csym = c_label;
 t_defsym(c_label); /* 'continue' jumps before the condition. */

 /* Parse the condition expression. */
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 DCCParse_ParPairEnd();
 if (visconst_bool() && !vgtconst_bool()) {
  /* Special case: constant false. */
  vpop(1);
  DCCParse_DeadStmt(b_label);
  vpop(1);
 } else {
  vpushs(b_label);
  vgen1('&');
  /* jump to the break label if the condition wasn't met.
   * NOTE: 'vjcc' automatically performs constant-optimizations! */
  vjcc(1);

  /* Parse the while-block. */
  DCCParse_Stmt(DCC_PFLAG_NONE);
  vpop(1);

  vpushs(c_label);
  vgen1('&');
  vjmp(); /* jump back to the condition at the end. */
 }

 t_defsym(b_label); /* 'break' jumps after the block. */
 pop_bc();
 /* TODO: propagate dead flags if 'b_label' has no relocations. */
 popf();
}

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#pragma warning(disable: 4703)
#endif

LEXPRIV void DCC_PARSE_CALL
DCCParse_DeadStmt(struct DCCSym *dead_jmp) {
 compiler.c_flags |= (DCC_COMPILER_FLAG_DEAD|DCC_COMPILER_FLAG_NOCGEN);
 if (!compiler.c_deadjmp) compiler.c_deadjmp = dead_jmp;
 DCCParse_Stmt(DCC_PFLAG_NONE);
}


LEXPRIV void DCC_PARSE_CALL DCCParse_IfStmt(void) {
 struct DCCSym *jmp_sym,*jmp_sym2;
 assert(TOK == KWD_if);
 YIELD();
 pushscope_local();
 DCCParse_ParPairBegin();
 if (DCCParse_DeclOrExpr()) WARN(W_DECLARATION_IN_IF_CONDITION);
 DCCParse_ParPairEnd();
 jmp_sym = DCCUnit_AllocSym();
 if unlikely(!jmp_sym) goto end;
 if (visconst_bool()) {
  int is_true = vgtconst_bool();
  int is_alive = 0;
  vpop(1);
  jmp_sym2 = NULL;
  if (!is_true) {
   pushf();
   DCCParse_DeadStmt(jmp_sym);
   if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD)) is_alive = 1;
   vpop(1);
   if (TOK == KWD_else) {
    /* If there is an else-block, and the true block contained
     * a label, we must generate a jump across else. */
    if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD)) {
     jmp_sym2 = DCCUnit_AllocSym();
     if unlikely(!jmp_sym2) goto end;
     vpushs(jmp_sym2),vgen1('&'),vjmp();
    }
   }
   popf();
  } else {
   DCCParse_Stmt(DCC_PFLAG_NONE);
   vpop(1);
  }
  t_defsym(jmp_sym);
  if (TOK == KWD_else) {
   YIELD();
   if (is_true) {
    if (!jmp_sym2) {
     jmp_sym2 = DCCUnit_AllocSym();
     if unlikely(!jmp_sym2) goto end;
    }
    pushf();
    DCCParse_DeadStmt(jmp_sym2);
    if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD)) is_alive = 1;
    popf();
   } else {
    DCCParse_Stmt(DCC_PFLAG_NONE);
   }
   vpop(1);
  }
  /* When either if-statement is alive, the if-statement it, too.
   * >> switch (TOK) {
   * >> {
   * >>     char const *name;
   * >>     // Each of these if-statements is alive!
   * >>     if (0) { case '+': name = "ADD"; }
   * >>     if (0) { case '-': name = "SUB"; }
   * >>     if (0) { case '*': name = "MUL"; }
   * >>     if (0) { case '/': name = "DIV"; }
   * >>     return name;
   * >> }
   * >> default: break;
   * >> }
   */
  if (is_alive) compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|
                                      DCC_COMPILER_FLAG_DEAD);
  if (jmp_sym2) t_defsym(jmp_sym2);
 } else {
  int is_dead = 0;
  int is_alive = 0;
  /* We've not using the return value of the blocks.
   * a statement-style 'if' always returns 'void'. */
  vpushs(jmp_sym); /* Push the jump target used when the condition fails. */
  vgen1('&');      /* Use the symbol address, instead of its value. */
  vjcc(1);         /* Jmp to else/after the if if the condition was false. */
  pushf();
  /* If the expression is a constant false, don't generate code for the true-block. */
  DCCParse_Stmt(DCC_PFLAG_NONE);
  vpop(1);
  if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) is_dead |= 1;
  else                                         is_alive = 1;
  if (TOK == KWD_else) {
   jmp_sym2 = DCCUnit_AllocSym();
   if unlikely(!jmp_sym2) goto end;
   /* Define a jump across the else block, if we got here from 'TRUE'. */
   vpushs(jmp_sym2);
   vgen1('&');
   vjmp();
  }
  popf();
  if (TOK == KWD_else) {
   YIELD();
   /* Define the jump target if TRUE was skipped. */
   t_defsym(jmp_sym);
   /* Setup the second symbol as target below. */
   jmp_sym = jmp_sym2;
   pushf();
   /* If the expression is a constant true, don't generate code for the false-block. */
   DCCParse_Stmt(DCC_PFLAG_NONE);
   vpop(1);
   if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD) is_dead |= 2;
   else                                         is_alive = 1;
   popf();
  }
  if (is_dead == (1|2)) {
   /* Both branches lead to dead code (e.g.: both contain a return statement)
    * >> Considering that, we know that this if-statement is _always_ dead. */
   compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  } else if (is_alive) {
   /* If either branch is alive, the statement itself is, too. */
   compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  }
  /* Define the jump target. */
  t_defsym(jmp_sym);
 }
end:
 popscope_local();
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_ForStmt(void) {
 struct DCCSym *b_label,*c_label;
 uint32_t old_target_flags;
 assert(TOK == KWD_for);
 YIELD();
 pushscope_local();
 DCCParse_ParPairBegin();
 /* Parse the initializer. */
 if (TOK != ';') {
  if (DCCParse_DeclOrExpr()) WARN(W_DECLARATION_IN_FOR_INITIALIZER);
  vpop(1);
 }
 if (TOK == ':') { /* todo: for-each style loop extensions? */ }
 if (TOK != ';') WARN(W_EXPECTED_SEMICOLON); else YIELD();
 if unlikely((b_label = DCCUnit_AllocSym()) == NULL) goto end2;
 if unlikely((c_label = DCCUnit_AllocSym()) == NULL) goto end2;
 old_target_flags = compiler.c_flags;
 compiler.c_flags |= DCC_COMPILER_FLAG_INLOOP;
 t_defsym(c_label); /* Condition label. */
 if (TOK != ';') {
  DCCParse_Expr();
  vpushs(b_label);
  vgen1('&');
  vjcc(1); /* jump over the block when the expression is false. */
 }
 if (TOK != ';') WARN(W_EXPECTED_SEMICOLON); else YIELD();
 if (DCCParse_IsExpr()) {
  struct DCCSym *adv_label,*skipadv_label;
  if unlikely((adv_label = DCCUnit_AllocSym()) == NULL) goto end;
  if unlikely((skipadv_label = DCCUnit_AllocSym()) == NULL) goto end;
  /* Skip advance when we come from the condition. */
  vpushs(skipadv_label),vgen1('&'),vjmp();
  t_defsym(adv_label); /* Advance label. */
  pushf();
  DCCParse_Expr();
  vpop(1);
  popf();
  /* Jump to the condition. */
  vpushs(c_label),vgen1('&'),vjmp();
  /* Use the advance-label as continue! */
  c_label = adv_label;
  t_defsym(skipadv_label); /* Advance label. */
 }
 DCCParse_ParPairEnd();
 push_bc();
 compiler.c_bsym = b_label;
 compiler.c_csym = c_label;
 /* Parse the block statement block. */
 DCCParse_Stmt(DCC_PFLAG_NONE);
 vpop(1);
 /* Jump to the condition/advance. */
 vpushs(c_label),vgen1('&'),vjmp();
 t_defsym(b_label); /* break label. */
 pop_bc();
end:  compiler.c_flags = old_target_flags;
end2: popscope_local();
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif


PUBLIC int DCC_PARSE_CALL DCCParse_Stmt(pflag_t f) {
 int result = 0;
 DCCUnit_MkDebugLC(DCCUNIT_DEBUGLC_STMT);
again:
 switch (TOK) {

 /* WARNING: Unlike deemon, DCC returns void (aka. none) for if-statements.
  *       >> This change had to be made to prevent a loss in performance. */
 case KWD_if:    DCCParse_IfStmt(); goto pushv;
 case KWD_while: DCCParse_WhileStmt(); goto pushv;
 case KWD_do:    DCCParse_DoWhileStmt(); goto pushv_semi;
 case KWD_for:   DCCParse_ForStmt(); goto pushv;

 { /* break/continue a loop. */
  struct DCCSym *target;
  if (DCC_MACRO_FALSE) { case KWD_break:    target = compiler.c_bsym; }
  if (DCC_MACRO_FALSE) { case KWD_continue: target = compiler.c_csym; }
  YIELD();
  if unlikely(!target) { WARN(W_BREAK_CONTINUE_NOT_ALLOWED); goto pushv_semi; }
  vpushs(target),vgen1('&'),vjmp();
  /* Make the control flow as dead. */
  goto dead_pushv_semi;
 } break;

 {
  int suppress_goto_warnings;
  struct TPPKeyword *next_keyword;
  struct TPPFile *tok_file; char *tok_begin;
  /* Special handling to allow '__extension__ goto *get_addr();' */
 case KWD___extension__:
  tok_begin = peek_next_token(&tok_file);
  if (*tok_begin == '(') {
   /* Handle special case: '__extension__({ foo(); });' */
   DCCParse_Expr();
   break;
  }
  next_keyword = peek_keyword(tok_file,tok_begin,0);
  if (!next_keyword ||
       next_keyword->k_id != KWD_goto ||
       next_keyword->k_macro) goto default_kwd;
  suppress_goto_warnings = 1;
  YIELD();
  if unlikely(TOK != KWD_goto) goto default_case;

  /* Jump to another label. */
  if (DCC_MACRO_FALSE) { case KWD_goto: suppress_goto_warnings = 0; }
  YIELD();
  if (DCC_MACRO_FALSE) { parse_goto: suppress_goto_warnings = 0; }
  if (TOK == KWD___extension__)
      suppress_goto_warnings = 1,
      YIELD();
  if (TOK == '*') {
   if (!suppress_goto_warnings)
        WARN(W_EXT_GOTO_EXPRESSIONS);

   /* GCC extension: label-expressions. */
   YIELD();
   DCCParse_Expr();
   /* NOTE: No need to do 'vgen1('*')' because it'd
    *       just cancel out the 'vgen1('&')' below! */
   goto gen_goto;
  } else if (TPP_ISKEYWORD(TOK)) {
   struct DCCDecl *goto_label;
   if (compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) YIELD();
   else {
    goto_label = DCCCompiler_NewLabel(TOKEN.t_kwd);
    YIELD();
    vpushd(goto_label);
    vgen1('&');
gen_goto:
    vjmp();
   }
  } else {
   WARN(W_EXPECTED_KEYWORD_AFTER_GOTO);
  }
  /* Make the control flow as dead. */
  goto dead_pushv_semi;
 } break;

 {
  struct DCCAttrDecl attr;
 case KWD___label__:
  for (;;) {
   memset(&attr,0,sizeof(attr));
   DCCParse_Attr(&attr);
   if (TPP_ISKEYWORD(TOK)) {
    struct DCCDecl *decl;
    /* Forward declare an empty symbol.
     * With this in place, any following label-related use of this symbol
     * will choose this one, unless another, closer one will still be declared. */
    decl = DCCCompiler_NewLocalDecl(TOKEN.t_kwd,DCC_NS_LABELS);
    YIELD();
    DCCParse_Attr(&attr);
    if (decl) {
     DCCDecl_SetAttr(decl,&attr);
     if (decl->d_kind == DCC_DECLKIND_NONE) {
      assert(decl->d_type.t_type         == DCCTYPE_INT);
      assert(decl->d_type.t_base         == NULL);
      assert(decl->d_mdecl.md_scope      == 0);
      assert(decl->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
      assert(decl->d_mdecl.md_loc.ml_off == 0);
      assert(decl->d_mdecl.md_loc.ml_sym == NULL);
      decl->d_kind        = DCC_DECLKIND_MLOC;
      decl->d_type.t_type = DCCTYPE_VOID;
     }
    }
   }
   DCCAttrDecl_Quit(&attr);
   if (TOK != ',') break;
  }
 } break;

 case '{':
  /* Parse a new scope. */
  DCCParse_Scope(f);
  break;

 { /* Empty statement. */
 case TOK_EOF:
  WARN(W_UNEXPECTED_EOF_IN_STATEMENT);
  /* fallthrough. */
 case ';':
  do YIELD(); while (TOK == ';'); /* Skip more empty statements. */
pushv:
  vpushv();
 } break;

 {
  struct DCCDecl *funty_decl;
 case KWD_return:
  YIELD();
  funty_decl = compiler.c_fun ? compiler.c_fun->d_type.t_base : NULL;
  if (TOK != ';') {
   /* By killing the EAX register here, we increase the chance
    * that the register allocator will already choose the correct
    * register when generating the return expression. */
   DCCVStack_GetRegExact(DCC_RR_XAX);
   DCCParse_Expr(); /* x */
   DCCVStack_PushReturn(funty_decl);
   vswap();   /* return, x */
   vstore(1); /* return=x */
   vpop(1);
  } else if (funty_decl &&
            (funty_decl->d_kind == DCC_DECLKIND_FUNCTION ||
             funty_decl->d_kind == DCC_DECLKIND_OLDFUNCTION) &&
            !DCCTYPE_ISBASIC(funty_decl->d_type.t_type,DCCTYPE_VOID)) {
   /* Warn amount missing return expressions. */
   WARN(W_MISSING_RETURN_EXPRESSION,compiler.c_fun,&funty_decl->d_type);
  }
  if (compiler.c_return) {
   /* Jump to a common return code address.
    * >> Used for stack cleanup! */
   vpushs(compiler.c_return);
   vgen1('&');
   vjmp();
  } else {
   DCCDisp_Ret();
  }
dead_pushv_semi:
  /* The control flow is dead for now. */
  compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  goto pushv_semi;
 } break;

 {
  struct DCCSym *break_sym,*old_break_sym,*casejmp;
  struct DCCSym *default_case,*old_default_case,*old_casejmp;
  struct DCCStackValue old_switch;
 case KWD_switch:
  if unlikely((break_sym = DCCUnit_AllocSym()) == NULL) break;
  if unlikely((default_case = DCCUnit_AllocSym()) == NULL) break;
  if unlikely((casejmp = DCCUnit_AllocSym()) == NULL) break;
  YIELD();
  DCCParse_ParPairBegin();
  DCCParse_Expr();
  DCCParse_ParPairEnd();
  /* Prefer loading the switch expression into a register. */
  if (/*(vbottom->sv_reg == DCC_RC_CONST) ||*/
      (vbottom->sv_flags&DCC_SFLAG_LVALUE)
      ) DCCStackValue_Load(vbottom);
  /*assert(vbottom->sv_reg != DCC_RC_CONST);*/
  /* Setup the switch register. */
  old_switch = compiler.c_sexpr;
  compiler.c_sexpr = *vbottom;
  vpop(0);
  /* Setup the break/default symbols. */
  old_break_sym      = compiler.c_bsym;
  old_default_case   = compiler.c_defsym;
  old_casejmp        = compiler.c_casejmp;
  compiler.c_bsym    = break_sym;
  compiler.c_defsym  = default_case;
  compiler.c_casejmp = casejmp; /* case-jumper. */
  /* Jump to the first case. */
  vpushs(casejmp),vgen1('&'),vjmp();
  /* Until a case is reached, all code inside a
   * switch is unreachable and therefor dead. */
  pushf();
  if (compiler.c_flags&DCC_COMPILER_FLAG_DEAD)
      compiler.c_flags |= DCC_COMPILER_FLAG_SDEAD;
  compiler.c_flags |= (DCC_COMPILER_FLAG_DEAD|
                       DCC_COMPILER_FLAG_NOCGEN);
  /* Parse the switch block. */
  DCCParse_Stmt(DCC_PFLAG_NONE);

  /* Reload the last case jumper. */
  casejmp            = compiler.c_casejmp;
  compiler.c_casejmp = old_casejmp;
  compiler.c_bsym    = old_break_sym;
  compiler.c_defsym  = old_default_case;
  /* The last case jump leads to the default case. */
  assert(DCCSym_ISFORWARD(casejmp));
  DCCSym_Alias(casejmp,default_case,0);
  /* if forward defined, set the default case to jump after the switch. */
  if (DCCSym_ISFORWARD(default_case))
      DCCSym_Alias(default_case,break_sym,0);
  /* When nothing used the break label, and the current
   * control flow is dead, it should stay that way! */
  if (break_sym->sy_refcnt == 1 &&
     (compiler.c_flags&DCC_COMPILER_FLAG_DEAD)) {
   compiler.c_flags  = _old_flags;
   compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                        DCC_COMPILER_FLAG_DEAD);
   goto def_break;
  }
  popf();
def_break:
  t_defsym(break_sym);
  compiler.c_sexpr = old_switch;
 } break;

 { /* Label statements. */
  struct DCCSym *label_sym;

 { /* Switch integral case. */
  int double_case;
  struct DCCSym *old_casejmp;
 case KWD_case:
  YIELD();
  if (!compiler.c_casejmp) {
   WARN(W_CASE_NOT_ALLOWED_HERE);
ignore_case_label:
   pushf();
   compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
   DCCParse_Expr();
   vpop(1);
   if (TOK == TOK_DOTS) {
    WARN(W_CASE_RANGES);
    YIELD(),DCCParse_Expr(),vpop(1);
   }
   if (TOK != ':') WARN(W_EXPECTED_COLON_AFTER_CASE); else YIELD();
   popf();
   goto again_after_missing_label;
  }
  /* If the switch statement was dead, ignore this case label! */
  if (compiler.c_flags&DCC_COMPILER_FLAG_SDEAD) goto ignore_case_label;
  if unlikely((label_sym = DCCUnit_AllocSym()) == NULL) break;
  if (compiler.c_deadjmp && /* Special case: re-enable code within a dead branch. */
     (compiler.c_flags&(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD)) ==
                     (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD)) {
   compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
   /* Must jump to a given target to skip dead code above. */
   vpushs(compiler.c_deadjmp);
   compiler.c_deadjmp = NULL;
   vgen1('&');
   vjmp();
   /* Preserve old flags for the pushf() below! */
   compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD;
  }
  /* Jump across the case expression (This generates no code if this is the first, dead case). */
  vpushs(label_sym),vgen1('&'),vjmp();
  /* Define the current last case jump and allocate a new one. */
  old_casejmp = compiler.c_casejmp;
  t_defsym(old_casejmp);
  if unlikely((compiler.c_casejmp = DCCUnit_AllocSym()) == NULL) break;
  /* Parse the case expression. */
  pushf();
  compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  DCCParse_Expr();
  if (!visconst_xval()) WARN(W_EXPECTED_CONSTANT_FOR_CASE);
  double_case = 0;
  if (TOK == TOK_DOTS) {
   double_case = 1;
   YIELD();
   DCCParse_Expr();
   if (!visconst_xval()) WARN(W_EXPECTED_CONSTANT_FOR_CASE);
   if (DCCVStack_IsSame(0)) { double_case = 0; vpop(1); }
  }
  /* lower limit is now stored in 'vbottom[1]', and the higher is in 'vbottom[0]' */
  if (!double_case) {
   /* one expression --> Compile as a simple compare. */
   vpush(&compiler.c_sexpr); /* expr, switch */
   vgen2(TOK_NOT_EQUAL);     /* expr != switch */
  } else {
   /* This is where it gets a bit more complicated! */
   vswap();                /* hi, lo */
   vpush(&compiler.c_sexpr); /* hi, lo, switch */
   vgen2(TOK_GREATER);     /* hi, lo > switch */
   /* Don't generate code for the second comparison
    * if the first one already indicated non-matching.
    * NOTE: This is also required for proper
    *       constant-optimization & dead-branch detection. */
   if (!visconst_bool() || !vgtconst_bool()) {
    vpushs(compiler.c_casejmp),vgen1('&'),vjcc(0); /* Jump to the next case if lo > switch. */
    vpush(&compiler.c_sexpr); /* hi, switch */
    vgen2(TOK_LOWER);       /* hi < switch */
   } else {
    vswap(); /* true, hi */
    vpop(1); /* true */
   }
  }
  if (TOK != ':') WARN(W_EXPECTED_COLON_AFTER_CASE); else YIELD();
  /* The check above is inverted, meaning that a constant
   * true here indicates a compile-time miss-match! */
  if (visconst_bool() && vgtconst_bool()) {
   /* Constant optimization for dead cases. */
   /* >> Simply restore the old c-flags & handle the case as a dead label.
    * NOTE: Don't set the dead/no-cgen flags, because that
    *       would interfere with fall-through cases!
    * >> Instead, we simply inherit the dead-code
    *    configuration of previous code. */
#if 1
   /* This is a bit strange, but because we've already defined the compiler's case-jump
    * as pointing to the expression that turned out to evaluate to a constant FALSE,
    * we must now either do some hacky re-directing to point our own case-jump to ourself
    * in order to still allow for fallthrough labeling, as well as prevent undefined local symbols
    * from appearing in object files (essentially creating a jmp instruction that does nothing):
    * >> switch ((int)(x == 0)) {      // cmpl $0, x; jmp <case 0>;
    * >> case 0:  handle_not(); break; // jge <default>; call handle_not; jmp <break>;
    * >> default: handle_def();        // call 
    * >> // Because 'bool' (0|1) can never be equal to 2, this case is dead
    * >> case 2:  handle_2();          // jmp <case 2> call handle_2; jmp <break>;
    * >>          break;
    * >> }
    */
   if (DCCSym_ClrDef(old_casejmp)) {
    t_defsym(compiler.c_casejmp);
    compiler.c_casejmp = old_casejmp;
   } else
#endif
   {
    /* The other possibility would be to generate an indirect jump.
     * Technically, we'd always have to do that in case the case expression
     * has side-effects, but we choose not to do so, as case expressions
     * should technically always be constant.
     * >> switch ((int)(x == 0)) {      // cmpl $0, x; jmp <case 0>;
     * >> case 0:  handle_not(); break; // jge <case 2>; call handle_not; jmp <break>;
     * >> default: handle_def();        // call 
     * >> // Because 'bool' (0|1) can never be equal to 2, this case is dead
     * >> case 2:  handle_2();          // jmp <case 2>; jmp <default>; call handle_2; jmp <break>;
     * >>          break;
     * >> }
     */
    vpushs(compiler.c_casejmp),vgen1('&'),vjmp();
   }
   compiler.c_flags = _old_flags;
   vpop(1);
   goto again_after_dead_label;
  } else {
   /* Jump to the next case when the case wasn't met. */
   vpushs(compiler.c_casejmp),vgen1('&'),vjcc(0);
   /* Special case: If the label declaration below will generate a dead-jmp,
    *               we must make sure that the case walks after that jump! */
   if (compiler.c_deadjmp) vpushs(label_sym),vgen1('&'),vjmp();
  }
  popf();
  /* Continue parsing afterwards. */
  if (compiler.c_flags&DCC_COMPILER_FLAG_SDEAD)
   goto again_after_dead_label;
  else goto again_after_label;
 } break;

 { /* Switch default case. */
 case KWD_default:
  YIELD();
  if (TOK != ':') WARN(W_EXPECTED_COLON_AFTER_DEFAULT); else YIELD();
  if ((label_sym = compiler.c_defsym) == NULL) WARN(W_DEFAULT_NOT_ALLOWED_HERE);
  else if (!DCCSym_ISFORWARD(label_sym)) WARN(W_DEFAULT_ALREADY_DEFINED);
  else if (compiler.c_flags&DCC_COMPILER_FLAG_SDEAD) goto again_after_dead_label;
  else goto again_after_label;
  goto again_after_missing_label;
 } break;

 {
  /* Parse AT&T-style inline-assembly. */
 case KWD_asm:
  if (!HAS(EXT_SHORT_EXT_KEYWORDS)) break;
 case KWD___asm:
 case KWD___asm__:
  DCCParse_AsmStmt(0);
pushv_semi:
  vpushv();
  goto check_semi;
 } break;

 case KWD__Static_assert:
  DCCParse_StaticAssert();
  goto pushv_semi;

 default:
default_case:
  if (TPP_ISKEYWORD(TOK)) default_kwd: {
   /* WARNING: Due to how this is implemented, a label name may
    *          not be separated from its associated ':' by anything other than
    *          comments (this includes macros, preprocessor directives, or other files)!
    * >> I know this isn't fully STD-C compliant, but it's the best one
    *    can do without implementing some sort of token-return system.
    */
   struct TPPFile *label_colon_file;
   char *label_colon = peek_next_token(&label_colon_file);
   if (*label_colon == ':') {
    struct DCCDecl *label_decl;
    int parse_goto_next,found_alias;
    /* Declare a c function-local label. */
    label_decl = DCCCompiler_NewLabel(TOKEN.t_kwd);
    label_colon_file->f_pos = label_colon+1;
    assert(label_colon_file->f_pos >= label_colon_file->f_begin);
    assert(label_colon_file->f_pos <= label_colon_file->f_end);
    /* Read the next token after the ':' */
    YIELD();
    { /* Parse label attributes. */
      struct DCCAttrDecl label_attr = DCCATTRDECL_INIT;
      DCCParse_Attr(&label_attr);
      if (label_decl) DCCDecl_SetAttr(label_decl,&label_attr);
      DCCAttrDecl_Quit(&label_attr);
    }
    if unlikely(!label_decl || label_decl->d_kind != DCC_DECLKIND_MLOC) goto def_expr;
    label_sym  = label_decl->d_mdecl.md_loc.ml_sym;
    if unlikely(!label_sym) goto def_expr;
again_after_label:
    parse_goto_next = 0,found_alias = 0;
    if (label_sym) {
     /* Optimization: define alias labels:
      * >>    printf("a\n");
      * >>    goto a;
      * >> a: goto b; // Alias label 'a' to point to 'b'
      * >> b: printf("b\n");
      * NOTE: Remember to also mark the control flow as dead!
      */
     if (TOK == KWD_break) {
      DCCSym_Alias(label_sym,compiler.c_bsym,0);
      found_alias = 1;
     } else if (TOK == KWD_continue) {
      DCCSym_Alias(label_sym,compiler.c_csym,0);
      found_alias = 1;
     } else if (TOK == KWD_goto) {
      /* This one's a bit more complicated... */
      YIELD();
      /* Set to parse a goto expression next. */
      parse_goto_next = 1;
      if (TPP_ISKEYWORD(TOK)) {
       struct DCCDecl *alias_decl = DCCCompiler_NewLabel(TOKEN.t_kwd);
       /* If goto is followed by a keyword, it's the name of the alias label. */
       if (alias_decl && alias_decl->d_kind == DCC_DECLKIND_MLOC &&
           alias_decl->d_mdecl.md_loc.ml_reg == DCC_RC_CONST &&
           alias_decl->d_mdecl.md_loc.ml_sym) {
        DCCSym_Alias(label_sym,alias_decl->d_mdecl.md_loc.ml_sym,0);
        found_alias = 1;
       }
      }
     }
    }
    if (label_sym && /* Special case: re-enable code within a dead branch. */
       (compiler.c_flags&(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD)) ==
                         (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD)) {
     compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
     if (compiler.c_deadjmp) {
      /* Must jump to a given target to skip dead code above. */
      vpushs(compiler.c_deadjmp);
      compiler.c_deadjmp = NULL;
      vgen1('&');
      vjmp();
     }
    }
    /* Special case: During forwarding, we parsed the 'goto' keyword. */
    if (parse_goto_next) {
     assert(label_sym);
     if (!found_alias) t_defsym(label_sym);
     goto parse_goto;
    }
    /* Don't define the symbol again we've already aliased it! */
    if (found_alias) goto again_after_missing_label;
again_after_dead_label:
    /* Finally, define the label to jump to. */
    if (label_sym) t_defsym(label_sym);
again_after_missing_label:
    /* Parse a statement after the label. */
    if (TOK == '}') { WARN(W_LABEL_WITHOUT_STATEMENT); goto pushv; }
    goto again;
   }
  }
def_expr:
  /* Parse a variable declaration, or expression. */
  result = DCCParse_DeclOrExpr();
  if (result && (f&DCC_PFLAG_WARNDECL)) WARN(W_MIXED_DECLARATIONS);
  if (result != 2) {
check_semi:
   if (TOK != ';') WARN(W_EXPECTED_SEMICOLON);
   else YIELD();
  }
 } break;
 }
 return result;
}

PUBLIC void DCC_PARSE_CALL
DCCParse_StaticAssert(void) {
 struct DCCSymExpr val;
 struct TPPString *message;
 assert(TOK == KWD__Static_assert);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_CExpr2(1,&val);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (TPP_ISSTRING(TOK)) message = DCCParse_String();
 else WARN(W_STATIC_ASSERT_EXPECTED_STRING),message = NULL;
 /* Check to confirm that the asserted condition is true. */
 if (!val.e_int && !val.e_sym)
      WARN(W_STATIC_ASSERT_FAILED,message ? message->s_text : (char *)"");
 if (message) TPPString_Decref(message);
 DCCParse_ParPairEnd();
}

PUBLIC int DCC_PARSE_CALL DCCParse_DeclOrExpr(void) {
 int result = DCCParse_LocalDecl();
 if (!result) DCCParse_Expr();
 return result;
}

PUBLIC void DCC_PARSE_CALL DCCParse_Global(void) {
 switch (TOK) {

 { /* Global assembly statement. */
 case KWD_asm:
  if (!HAS(EXT_SHORT_EXT_KEYWORDS)) break;
 case KWD___asm:
 case KWD___asm__:
  DCCUnit_MkDebugLC(DCCUNIT_DEBUGLC_STMT);
  DCCParse_AsmStmt(1);
pushv_semi:
  vpushv();
  goto check_semi;
 } break;

 case KWD__Static_assert:
  DCCParse_StaticAssert();
  goto pushv_semi;


 { /* Parse a declaration. */
  int decl_error;
 default:
  decl_error = DCCParse_GlobalDecl();
  if (decl_error != 2) {
check_semi:
   if (TOK != ';') WARN(W_EXPECTED_SEMICOLON);
   else YIELD();
  }
 } break;
 }
}

PUBLIC void DCC_PARSE_CALL
DCCParse_AllGlobal(void) {
 unsigned long toknum;
 pushf();
 compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|
                     DCC_COMPILER_FLAG_DEAD);
 while (TOK > 0) {
  toknum = TOKEN.t_num;
  DCCParse_Global();
  vpop(1);
  /* Make sure the vstack is empty. */
  assertf(compiler.c_vstack.v_bottom ==
          compiler.c_vstack.v_end,
          "Invalid vstack size: %lu too many",
          (unsigned long)(compiler.c_vstack.v_end-
                          compiler.c_vstack.v_bottom));
  if (toknum == TOKEN.t_num) {
   if (TOK <= 0) break;
   /* Forceably yield one token. */
   WARN(W_PARSE_FAILED_TO_PARSE_ANYTHING);
   YIELD();
  }
 }
 popf();
}



DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_STMT_C_INL */
