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
#include <dcc/gen.h>

#include "lexer-priv.h"
#include "x86_util.h"

#include <string.h>

DCC_DECL_BEGIN

LEXPRIV void DCC_PARSE_CALL
DCCAsm_ExecStringInherited(struct TPPString *__restrict asmtext) {
 struct TPPFile *asm_file;
 asm_file = TPPFile_NewExplicitInherited(asmtext);
 if unlikely(!asm_file) TPPString_Decref(asmtext);
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
  while (TOK > 0) {
   unsigned long old_num = TOKEN.t_num;
   DCCParse_AsmInstr();
   if (old_num == TOKEN.t_num) YIELD();
  }
  DCCParse_AsmEnd();
  TPPLexer_Current->l_eob_file = old_eob;
  YIELD(); /* Yield the next token after the assembly (which should be a ';'). */
 }
}

struct DCCIAsmOperand {
 struct DCCIAsmOperand    *ao_next;        /*< [0..1][->ao_priority >= this->ao_priority] Next operand. */
 /*ref*/struct TPPString  *ao_constraints; /*< [1..1] User-string of assembly constraints. */
 struct TPPKeyword        *ao_name;        /*< [0..1] Optional operand name. */
#define ASMIOP_FLAG_RW     0x00000001      /*< read/write operand. */
#define ASMIOP_FLAG_LLOCAL 0x00000002      /*< The associated stack-value requires additional indirection, for which ':ao_reg' shall be used. */
#define ASMIOP_FLAG_TEST   0x00000004      /*< Used for output operands to return the value of a test as 0/1. */
#define ASMIOP_MASK_TEST   0xf0000000      /*< Mask for what kind of test should be returned. */
#define ASMIOP_SHFT_TEST   28              /*< Shift for the test kind, used to convert the value into a 'test_t'. */
#define ASMIOP_MK_TEST(t) (((t) << ASMIOP_SHFT_TEST)|ASMIOP_FLAG_TEST)
#define ASMIOP_GT_TEST(f) ((f) >> ASMIOP_SHFT_TEST)
 uint32_t                  ao_flags;       /*< Set of 'ASMIOP_FLAG_*'. */
 uint8_t                   ao_priority;    /*< Operand priority (Lower priority means execute-before). */
 uint8_t                   ao_padding;     /*< Padding data. */
 int8_t                    ao_reg;         /*< Explicit register operand, or -1 when unused. */
 int8_t                    ao_reg2;        /*< A secondary register used for double-wide operands, or '-1' when unused. */
 struct DCCIAsmOperand    *ao_ref;         /*< [0..1][->ao_input == this] Referenced for input operand, or NULL. */
 struct DCCIAsmOperand    *ao_input;       /*< [0..1][->ao_ref == this] Referenced input operand, or NULL. */
 struct DCCStackValue     *ao_value;       /*< [1..1][== vbottom+((:ao_opv+(:ao_opc-1))-this)] Associated stack-value. */
};

/* A set of registers that should be saved using push/pop across assembly. */
#ifdef IA32_PROTECTED_REGISTERS
#define DCC_IASM_RCSET_SAVE (DCC_RCSET(DCC_ASMREG_EBX)|\
                             DCC_RCSET(DCC_ASMREG_ESI)|\
                             DCC_RCSET(DCC_ASMREG_EDI))
#else
#define DCC_IASM_RCSET_SAVE   0
#endif

struct DCCIAsmOps {
 size_t                 ao_out;   /*< Amount of output operands (index 0..ao_out-1). */
 size_t                 ao_opc;   /*< Amount of operands. */
 size_t                 ao_opa;   /*< Allocated amount of operands. */
 struct DCCIAsmOperand *ao_first; /*< [0..1] Operand with the lowest priority of all. */
 struct DCCIAsmOperand *ao_opv;   /*< [0..ao_opc] Vector of operands. */
#define DCCIASMOPS_FLAG_NONE           0x00000000
#define DCCIASMOPS_FLAG_CLOBBER_MEMORY 0x00000001 /*< All active registers must be killed beforehand. */
#define DCCIASMOPS_FLAG_CLOBBER_EFLAGS 0x00000002 /*< All tests must be killed beforehand. */
 uint32_t               ao_flags; /*< Assembly flags (Set of 'DCCIASMOPS_FLAG_*'). */
 rcset_t                ao_clob;  /*< ASM clobber registers. */
 rcset_t                ao_alloc; /*< Registers that are allocated by the assembly. */
 int8_t                 ao_rin;   /*< Temporary register used for input indirection, or -1 when unused. */
 int8_t                 ao_rout;  /*< Temporary register used for output indirection, or -1 when unused. */
};
#define DCCIAsmOps_Init(self) memset(self,0,sizeof(struct DCCIAsmOps))
LEXDECL void DCCIAsmOps_Quit(struct DCCIAsmOps *__restrict self);
LEXDECL struct DCCIAsmOperand *DCCIAsmOps_GetOperand(struct DCCIAsmOps *__restrict self, char const *__restrict name, char const **name_end);
LEXDECL void DCCIAsmOps_Prepare(struct DCCIAsmOps *__restrict self);
LEXDECL void DCC_PARSE_CALL DCCAsmOperand_Parse(struct DCCIAsmOperand *__restrict self, int is_output);
LEXDECL void DCC_PARSE_CALL DCCIAsmOps_ParseOne(struct DCCIAsmOps *__restrict self, int is_output);
LEXDECL void DCC_PARSE_CALL DCCIAsmOps_ParseAll(struct DCCIAsmOps *__restrict self, int is_output);
LEXDECL void DCC_PARSE_CALL DCCIAsmOps_ParseClobber(struct DCCIAsmOps *__restrict self);
LEXDECL /*ref*/struct TPPString *DCCIAsmOps_Format(struct DCCIAsmOps *__restrict self, struct TPPString *__restrict str);
LEXDECL void DCCIAsmOps_Load(struct DCCIAsmOps *__restrict self); /* Generate load setup. */
LEXDECL void DCCIAsmOps_Store(struct DCCIAsmOps *__restrict self); /* Generate store teardown. */
#if DCC_IASM_RCSET_SAVE != DCC_RCSET_EMPTY
LEXDECL void DCCIAsmOps_PushSave(struct DCCIAsmOps *__restrict self);
LEXDECL void DCCIAsmOps_PopSave(struct DCCIAsmOps *__restrict self);
#else
#define DCCIAsmOps_PushSave(self) (void)0
#define DCCIAsmOps_PopSave(self)  (void)0
#endif
LEXDECL void DCC_PARSE_CALL DCCParse_AsmWithConstraints(/*ref*/struct TPPString *asmtext, int has_paren);

INTDEF void DCC_VSTACK_CALL
DCCStackValue_Store(struct DCCStackValue *__restrict self,
                    struct DCCStackValue *__restrict target,
                    int initial_store);


LOCAL const char *c_skipmods(const char *p);
/* Return the priority of the given constraint string. */
LOCAL uint8_t c_priority(const char *str);


LEXPRIV void
DCCIAsmOps_Quit(struct DCCIAsmOps *__restrict self) {
 struct DCCIAsmOperand *iter,*end;
 end = (iter = self->ao_opv)+self->ao_opc;
 for (; iter != end; ++iter) TPPString_Decref(iter->ao_constraints);
 free(self->ao_opv);
}

/* Find and return the operator named 'name'.
 * When given, '*name_end' will be set to the end of the operand name in 'name'.
 * @return: NULL: Invalid/unknown operand ('*name_end' is still be set if given) */
LEXPRIV struct DCCIAsmOperand *
DCCIAsmOps_GetOperand(struct DCCIAsmOps *__restrict self,
                      char const *__restrict name,
                      char const **name_end) {
 struct DCCIAsmOperand *result = NULL;
 char ch;
 assert(self);
 assert(name);
 if ((ch = *name,tpp_isdigit(ch))) {
  size_t index = 0;
  do {
   index = (index*10)+(ch-'0');
   ch = *++name;
  } while (tpp_isdigit(ch));
  if (index < self->ao_opc)
      result = &self->ao_opv[index];
 } else if (ch == '[') {
  struct TPPKeyword *nk;
  char const *e = (++name,strchr(name,']'));
  if likely(e) {
   if likely((nk = TPPLexer_LookupKeyword(name,(size_t)(e-name),0)) != NULL) {
    /* Search for a named operator with this identifier. */
    struct DCCIAsmOperand *iter,*end;
    end = (iter = self->ao_opv)+self->ao_opc;
    for (; iter != end; ++iter) {
     if (iter->ao_name == nk) { result = iter; break; }
    }
   }
   name = e+1;
  }
 }
 if (name_end) *name_end = name;
 return result;
}

LOCAL const char *c_skipmods(const char *p) {
 while (*p == '=' || *p == '&' || *p == '+' || *p == '%') ++p;
 return p;
}
/* Return the priority of the given constraint string. */
LOCAL uint8_t c_priority(const char *str) {
 uint8_t result,new_result;
 result = 0;
 for (;;) {
  switch (*str++) {
  case '\0': goto done;
  case 'A': new_result = 0; break;
  case 'a': case 'b': case 'c':
  case 'd': case 'S':
  case 'D': new_result = 1; break;
  case 'q': new_result = 2; break;
  case 'r': new_result = 3; break;
  case 'N': case 'M': case 'I':
  case 'i': case 'm':
  case 'g': new_result = 4; break;
  default: new_result = 0; break;
  }
  /* Use the greatest priority. */
  if (result < new_result)
      result = new_result;
 }
done:
 return result;
}

LOCAL test_t parse_test(char const **__restrict pctext) {
 test_t result;
 char const *iter = *pctext;
 switch (*iter++) {
 case 'a': result = DCC_TEST_A; if (*iter == 'e') ++iter,result = DCC_TEST_AE; break;
 case 'b': result = DCC_TEST_B; if (*iter == 'e') ++iter,result = DCC_TEST_BE; break;
 case 'c': result = DCC_TEST_C; break;
 case 'e': result = DCC_TEST_E; break;
 case 'z': result = DCC_TEST_Z; break;
 case 'g': result = DCC_TEST_G; if (*iter == 'e') ++iter,result = DCC_TEST_GE; break;
 case 'l': result = DCC_TEST_L; if (*iter == 'e') ++iter,result = DCC_TEST_LE; break;
 case 'o': result = DCC_TEST_O; break;
 case 'p': result = DCC_TEST_P; break;
 case 's': result = DCC_TEST_S; break;
 default: --iter; WARN(W_IASM_INVALID_TEST); result = DCC_TEST_E; break;
 }
 *pctext = iter;
 return result;
}


LEXPRIV void
DCCIAsmOps_Prepare(struct DCCIAsmOps *__restrict self) {
 /* With all operands and associated values known, prepare them. */
#define REGMASK_IN  0x01
#define REGMASK_OUT 0x02
 struct DCCIAsmOperand *iter,*end;
 uint8_t i,alloc_regs[8];
 struct DCCStackValue *vvalue;
 assert(self);
 self->ao_rin = -1;
 self->ao_rout = -1;

 /* Figure out allocated registers. */
 memset(alloc_regs,0,sizeof(alloc_regs));
 DCC_RCSET_FOREACH(self->ao_clob,i)
  alloc_regs[i] = (REGMASK_IN|REGMASK_OUT);
 /* Preserve ESP and EBP (TODO: Make both of these usable as well) */
 alloc_regs[DCC_ASMREG_ESP] = (REGMASK_IN|REGMASK_OUT);
 alloc_regs[DCC_ASMREG_EBP] = (REGMASK_IN|REGMASK_OUT);

 end = (iter = self->ao_opv)+self->ao_opc;
 assert(vsize >= self->ao_opc);
 vvalue = vbottom+self->ao_opc;
 for (; iter != end; ++iter) {
  char const *ctext;
  /* Automatically link stack-values. */
  iter->ao_value = --vvalue;
  ctext = c_skipmods(iter->ao_constraints->s_text);
  assert(!iter->ao_input);
  if (tpp_isdigit(*ctext) || *ctext == '[') {
   char const *cend;
   /* Reference to another constraint. */
   iter->ao_ref = DCCIAsmOps_GetOperand(self,ctext,&cend);
   if (!iter->ao_ref) {
    WARN(W_IASM_INVALID_OPERAND_REFERENCE,
         ctext,(size_t)(cend-ctext));
   } else if (iter->ao_ref->ao_input) {
    WARN(W_IASM_DOUBLE_OPERAND_REFERENCE,
         ctext,(size_t)(cend-ctext));
    iter->ao_ref = NULL;
   } else {
    iter->ao_ref->ao_input = iter;
   }
   iter->ao_priority = 5;
  } else {
   iter->ao_priority = c_priority(ctext);
  }
 }
 /* With the priority of all operands determined, sort them. */
 for (iter = self->ao_opv; iter != end; ++iter) {
  struct DCCIAsmOperand *inselem,**pinsert = &self->ao_first;
  uint8_t curr_prio = iter->ao_priority;
  while ((inselem = *pinsert) != NULL &&
         (inselem->ao_priority < curr_prio))
          pinsert = &inselem->ao_next;
  /* Insert this segment into '*pinsert' */
  assert(inselem == *pinsert);
  iter->ao_next = inselem;
  *pinsert = iter;
 }
 for (iter = self->ao_first; iter; iter = iter->ao_next) {
  char const *ctext; uint8_t mask; int8_t reg;
  /* Skip reference operands. */
  if (iter->ao_ref) continue;
  ctext = iter->ao_constraints->s_text;
  /* Determine the debug register usage mask. */
  if (iter->ao_input) {
   mask = (REGMASK_IN|REGMASK_OUT);
  } else if (iter < self->ao_opv+self->ao_out) {
   mask = (REGMASK_OUT);
  } else {
   mask = (REGMASK_IN);
  }
cnext:
  switch (*ctext++) {
  case '=':
   /* TODO: Don't output operands require either '=' or '+' in their constraints?
    *       Shouldn't we warn if they don't? */
   if (ctext[0] == '@' && ctext[1] == 'c' && ctext[2] == 'c') {
    test_t test = 0;
    ctext += 3; /* Test-output operand. */
    while (*ctext == 'n') ++ctext,test ^= DCC_TEST_NBIT;
    test ^= parse_test(&ctext);
    iter->ao_flags |= ASMIOP_MK_TEST(test);
    mask = (REGMASK_OUT);
    break;
   }
   goto cnext;
  case '+':
   iter->ao_flags |= ASMIOP_FLAG_RW;
  case '&':
   if (iter >= self->ao_opv+self->ao_out)
       WARN(W_IASM_MODIFIER_ONLY_VALID_FOR_OUTPUT_OPERANDS,(int)ctext[-1]);
   mask = (REGMASK_IN|REGMASK_OUT);
   goto cnext;
  case 'A':
   /* Allocate EAX and EDX (long-long register pair) */
   if ((alloc_regs[DCC_ASMREG_EAX]&mask) ||
       (alloc_regs[DCC_ASMREG_EDX]&mask)) goto cnext;
   iter->ao_reg                = DCC_RR_XAX;
   iter->ao_reg2               = DCC_RR_XDX;
   alloc_regs[DCC_ASMREG_EAX] |= mask;
   alloc_regs[DCC_ASMREG_EDX] |= mask;
   break;

   /* Explicit register allocation. */
   if (DCC_MACRO_FALSE) { case 'a': reg = DCC_ASMREG_EAX; }
   if (DCC_MACRO_FALSE) { case 'b': reg = DCC_ASMREG_EBX; }
   if (DCC_MACRO_FALSE) { case 'c': reg = DCC_ASMREG_ECX; }
   if (DCC_MACRO_FALSE) { case 'd': reg = DCC_ASMREG_EDX; }
   if (DCC_MACRO_FALSE) { case 'S': reg = DCC_ASMREG_ESI; }
   if (DCC_MACRO_FALSE) { case 'D': reg = DCC_ASMREG_EDI; }
   if (alloc_regs[reg]&mask) goto cnext;
ralloc:
   assert(iter->ao_reg == -1);
   assert(iter->ao_reg2 == -1);
   iter->ao_reg     = reg;
   alloc_regs[reg] |= mask;
   break;

  case 'q': /* Any register addressible as 'Rh' */
  case 'Q': /* Any register addressible as 'Rl' */
   /* TODO: Prefer registers from 'iter->ao_value'. */
   for (reg = 0; reg < 4; ++reg) {
    if (!(alloc_regs[reg]&mask)) goto ralloc;
   }
   goto cnext;
  case 'r':
  case 'R':
   /* Any general purpose register. */
   /* TODO: Prefer registers from 'iter->ao_value'. */
   for (reg = 0; reg < 8; ++reg) {
    if (!(alloc_regs[reg]&mask)) goto ralloc;
   }
   goto cnext;

  case 'i':
  case 'n':
#if !DCC_TARGET_ASMF(F_X86_64)
  case 'Z': /* uint32_t */
#endif /* F_X86_64 */
   /* Check for constant (xval) values. */
   if (!DCCSTACKVALUE_ISCONST_XVAL(iter->ao_value))
        goto cnext;
   break;
  {
   int_t valmin,valmax;
#define RANGE(c,mi,ma) if (DCC_MACRO_FALSE) { case c: valmin = mi,valmax = ma; }
   RANGE('I',0,31);
   RANGE('J',0,63);
   RANGE('K',INT8_MIN,INT8_MAX);
   RANGE('M',0,3);
   RANGE('N',0x00,0xff);
#undef RANGE
   /* Check for constant (int) values. */
   if (!DCCSTACKVALUE_ISCONST_INT(iter->ao_value))
        goto cnext;
   /* Check the valid range. */
   if (iter->ao_value->sv_const.it < valmin ||
       iter->ao_value->sv_const.it > valmax) goto cnext;
  } break;
  case 'L': /* 0xff or 0xffff */
   if (!DCCSTACKVALUE_ISCONST_INT(iter->ao_value))
        goto cnext;
   if (iter->ao_value->sv_const.it != 0xff ||
       iter->ao_value->sv_const.it != 0xffff)
       goto cnext;
   break;

  case 'g':
   if (iter < self->ao_opv+self->ao_out) {
  case 'm':
  case 'o':
    /* And l-value to l-value case. */
    if ((iter->ao_value->sv_flags&DCC_SFLAG_LVALUE) &&
        DCCTYPE_GROUP(iter->ao_value->sv_ctype.t_type) == DCCTYPE_LVALUE) {
     if (self->ao_rin == -1) {
      /* Search for a free register. */
      for (reg = 0; reg < 8; ++reg) {
       if (!(alloc_regs[reg]&REGMASK_IN)) goto found_llocal;
      }
      goto cnext;
found_llocal:
      self->ao_rin = reg;
     }
     iter->ao_reg     = reg;
     iter->ao_flags  |= ASMIOP_FLAG_LLOCAL;
     alloc_regs[reg] |= REGMASK_IN;
    }
   }
   break;
  case 'X': break;

  case '\0':
   WARN(W_IASM_UNFULFILLED_CONTRAINT,
        iter->ao_constraints->s_text);
   break;
  default:
   if (tpp_isspace(ctext[-1])) goto cnext;
   WARN(W_IASM_INVALID_CONSTRAINT_MODIFIER,(int)ctext[-1]);
   goto cnext;
  }
  /* Duplicate reference operators. */
  if (iter->ao_input) {
   iter->ao_input->ao_reg   = iter->ao_reg;
   iter->ao_input->ao_reg2  = iter->ao_reg2;
   iter->ao_input->ao_flags = iter->ao_flags&~(ASMIOP_FLAG_LLOCAL);
  }
 }
 self->ao_rout = -1;
 /* Allocate an additional register for llocal indirection during output. */
 for (iter = self->ao_opv; iter != end; ++iter) {
  if (iter->ao_reg >= 0 &&
     (iter->ao_value->sv_flags&DCC_SFLAG_LVALUE) &&
      DCCTYPE_GROUP(iter->ao_value->sv_ctype.t_type) == DCCTYPE_LVALUE &&
     !(iter->ao_flags&ASMIOP_FLAG_LLOCAL)) {
   int8_t reg;
   if (self->ao_rin != -1) reg = self->ao_rin;
   else {
    for (reg = 0; reg < 8; ++reg) {
     if (!(alloc_regs[reg]&REGMASK_OUT)) goto found_llocal2;
    }
    WARN(W_IASM_MISSING_RELOAD_REGISTER);
   }
found_llocal2:
   self->ao_rout = reg;
   break;
  }
 }
 /* Figure out all the registers that are being allocated. */
 self->ao_alloc = self->ao_clob;
 if (self->ao_rin >= 0) self->ao_alloc |= (1 << self->ao_rin);
 if (self->ao_rout >= 0) self->ao_alloc |= (1 << self->ao_rout);
 for (iter = self->ao_opv; iter != end; ++iter) {
  if (iter->ao_reg >= 0) self->ao_alloc |= (1 << iter->ao_reg);
  if (iter->ao_reg2 >= 0) self->ao_alloc |= (1 << iter->ao_reg2);
 }
}


LEXPRIV void DCC_PARSE_CALL
DCCAsmOperand_Parse(struct DCCIAsmOperand *__restrict self, int is_output) {
 struct TPPString *constraints;
 assert(self);
 memset(self,0,sizeof(struct DCCIAsmOperand));
 self->ao_reg  = -1;
 self->ao_reg2 = -1;
 if (TOK == '[') {
  /* Named operand value. */
#if 1
  /* Don't allow macros for the assembly name.
   * While not really standard-conforming (if there even is one for a GCC feature),
   * it does make it much easier to choose any name without having to worry about
   * a macro with the same name, especially when considering that no macro expansion
   * happens within the assembly string (Not even once it'll finally get compiled). */
  TPPLexer_YieldPP();
#else
  YIELD();
#endif
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
 if (!constraints) {
  constraints = TPPFile_Empty.f_text;
  TPPString_Incref(TPPFile_Empty.f_text);
 }
 self->ao_constraints = constraints;
 DCCParse_ParPairBegin();
 DCCParse_Expr();
 /* Warn if output operands aren't l-values. */
 if (is_output && !(vbottom->sv_flags&DCC_SFLAG_LVALUE))
     WARN(W_IASM_EXPECTED_LVALUE_FOR_OUTPUT);
 DCCParse_ParPairEnd();
}

LEXPRIV void DCC_PARSE_CALL
DCCIAsmOps_ParseOne(struct DCCIAsmOps *__restrict self, int is_output) {
 struct DCCIAsmOperand *opv;
 assert(self);
 assert(self->ao_opc <= self->ao_opa);
 if (self->ao_opc == self->ao_opa) {
  size_t newsize = self->ao_opa;
  if (!newsize) newsize = 1;
  newsize *= 2;
  opv = (struct DCCIAsmOperand *)realloc(self->ao_opv,newsize*
                                         sizeof(struct DCCIAsmOperand));
  if unlikely(!opv) { DCC_AllocFailed(newsize*sizeof(struct DCCIAsmOperand)); return; }
  self->ao_opv = opv;
  self->ao_opa = newsize;
 }
 opv  = self->ao_opv;
 opv += self->ao_opc++;
 DCCAsmOperand_Parse(opv,is_output);
}

LEXPRIV void DCC_PARSE_CALL
DCCIAsmOps_ParseAll(struct DCCIAsmOps *__restrict self, int is_output) {
 for (;;) {
  DCCIAsmOps_ParseOne(self,is_output);
  if (TOK != ',') break;
  YIELD();
 }
}

LEXPRIV void DCC_PARSE_CALL
DCCIAsmOps_ParseClobber(struct DCCIAsmOps *__restrict self) {
 for (;;) {
  struct TPPString *clobber;
  if (TOK != TOK_STRING) {
   WARN(W_IASM_EXPECTED_STRING_FOR_CLOBBER);
   clobber = NULL;
  } else {
   clobber = TPPLexer_ParseString();
  }
#define CLOBBER(x) \
       (clobber->s_size == DCC_COMPILER_STRLEN(x) && \
       !memcmp(clobber->s_text,x,sizeof(x)-sizeof(char)))
  if (clobber) {
        if (CLOBBER("memory")) self->ao_flags |= DCCIASMOPS_FLAG_CLOBBER_MEMORY;
   else if (CLOBBER("cc"))     self->ao_flags |= DCCIASMOPS_FLAG_CLOBBER_EFLAGS;
   else {
    /* Handle register clobber. */
    struct TPPKeyword *register_clobber;
    char const *register_name;
    size_t      register_size;
    tok_t       register_id;
    register_name = clobber->s_text;
    register_size = clobber->s_size;
    while (register_size && *register_name == '%')
           --register_size,++register_name;
    register_clobber = TPPLexer_LookupKeyword(register_name,register_size,0);
    register_id = register_clobber ? register_clobber->k_id : 0;
    if (register_id >= KWD_al &&
#if DCC_TARGET_ASMF(F_X86_64)
        register_id <= KWD_rdi
#else /* F_X86_64 */
        register_id <= KWD_edi
#endif /* !F_X86_64 */
        ) {
     self->ao_clob |= (1 << ((register_id-KWD_al) % 8));
    } else {
     WARN(W_IASM_UNKNOWN_CLOBBER,clobber->s_text);
    }
   }
#undef CLOBBER
   TPPString_Decref(clobber);
  }
  if (TOK != ',') break;
  YIELD();
 }
}

#if DCC_IASM_RCSET_SAVE != DCC_RCSET_EMPTY
LEXPRIV void
DCCIAsmOps_PushSave(struct DCCIAsmOps *__restrict self) {
 rc_t rc; uint8_t ri; rcset_t save; assert(self);
 save = DCC_RCSET_COM(self->ao_alloc,DCC_IASM_RCSET_SAVE);
 rc = DCC_RC_I16;
 if (!(compiler.c_flags&DCC_COMPILER_FLAG_CODE16)) rc |= DCC_RC_I32;
 DCC_RCSET_FOREACH(save,ri) DCCDisp_RegPush(rc|ri);
}
LEXPRIV void
DCCIAsmOps_PopSave(struct DCCIAsmOps *__restrict self) {
 rc_t rc; uint8_t ri; rcset_t save; assert(self);
 save = DCC_RCSET_COM(self->ao_alloc,DCC_IASM_RCSET_SAVE);
 rc = DCC_RC_I16;
 if (!(compiler.c_flags&DCC_COMPILER_FLAG_CODE16)) rc |= DCC_RC_I32;
 DCC_RCSET_RFOREACH(save,ri) DCCDisp_PopReg(rc|ri);
}
#endif

struct stringwriter {
 size_t            sw_stra; /*< Allocated amount of characters. */
 size_t            sw_strc; /*< Used amount of characters (soon-to-be 'sw_strv->s_size'). */
 struct TPPString *sw_strv; /*< [0..1][owned] Allocated string buffer (only 's_text' is (partially) initialized). */
};

PRIVATE void
stringwriter_write(struct stringwriter *__restrict self,
                   char const *__restrict str, size_t len) {
 size_t avail;
 assert(self);
 avail = self->sw_stra-self->sw_strc;
 if (len > avail) {
  struct TPPString *new_str;
  size_t new_alloc = self->sw_stra*2;
  assert(new_alloc);
  while (self->sw_strc+len > new_alloc) new_alloc *= 2;
  new_str = (struct TPPString *)realloc(self->sw_strv,
                                        offsetof(struct TPPString,s_text)+
                                       (new_alloc+1)*sizeof(char));
  if unlikely(!new_str) {
   DCC_AllocFailed(offsetof(struct TPPString,s_text)+
                  (new_alloc+1)*sizeof(char));
   return;
  }
  self->sw_strv = new_str;
  self->sw_stra = new_alloc;
 }
 memcpy(self->sw_strv->s_text+
        self->sw_strc,
        str,len*sizeof(char));
 self->sw_strc += len;
}

PRIVATE void
stringwriter_writeint(struct stringwriter *__restrict self, int_t it) {
 char buf[64];
 stringwriter_write(self,buf,(size_t)(TPP_Itos(buf,it)-buf));
}
PRIVATE void
stringwriter_writereg(struct stringwriter *__restrict self, rc_t rr) {
 static char const reg_chrs[4] = {'a','c','d','b'};
 static char const reg_high[4][2] = {{'s','p'},{'b','p'},{'s','i'},{'d','i'}};
 char buf[16],*iter = buf;
 assert((rr&DCC_RC_MASK) != 0);
#ifdef DCC_RC_I64
 if (rr&DCC_RC_I64) *iter++ = 'r';
 else
#endif
      if (rr&DCC_RC_I32) *iter++ = 'e';
 else if (!(rr&DCC_RC_I16)) {
  /* Special case: 8-bit registers. */
  *iter++ = reg_chrs[rr&3];
  *iter++ = (rr&4) ? 'h' : 'l';
  goto done;
 }
 if (rr&4) {
  /* High-order special/pointer registers. */
  *iter++ = reg_high[rr&3][0];
  *iter++ = reg_high[rr&3][1];
 } else {
  *iter++ = reg_chrs[rr&3];
  *iter++ = 'x';
 }
done:
 stringwriter_write(self,buf,(size_t)(iter-buf));
}




LEXPRIV /*ref*/struct TPPString *
DCCIAsmOps_Format(struct DCCIAsmOps *__restrict self,
                  struct TPPString *__restrict str) {
 struct stringwriter writer; char mod;
 char const *iter,*end,*flush_start;
 unsigned int dialect_depth = 0;
 assert(self);
 assert(str);
 /* Start out with a buffer that is very likely to already be sufficient. */
 writer.sw_strc = 0;
 writer.sw_stra = (str->s_size*3)/2;
 if unlikely(!writer.sw_stra) {
return_empty:
  TPPString_Incref(TPPFile_Empty.f_text);
  return TPPFile_Empty.f_text;
 }
 /* Allocate an initial buffer. */
 writer.sw_strv = (struct TPPString *)malloc(offsetof(struct TPPString,s_text)+
                                            (writer.sw_stra+1)*sizeof(char));
 end = (flush_start = iter = str->s_text)+str->s_size;
 assert(iter != end);
 assert(*end == '\0');
next:
 assert(iter <= end);
 switch (*iter++) {
 case '\0':
  if (iter-1 != end) goto next;
  --iter;
  break;

 case '{':
  ++dialect_depth;
 case '|':
 case '}':
  /* Begin a new dialect block.
   * NOTE: Since DCC implements AT&T syntax, we
   *       always parse only the first dialect block. */
  if (flush_start != iter-1) {
   stringwriter_write(&writer,flush_start,
                     (size_t)((iter-1)-flush_start));
   flush_start = iter;
  }
  if (iter[-1] == '{') goto next;
  if (dialect_depth) {
   if (iter[-1] == '|') {
    unsigned int recursion = 0;
    /* Scan until the next '|' or '}', recursively resolving '{' ... '}' pairs */
    for (;;) {
     char ch = *iter++;
          if (ch == '{') ++recursion;
     else if (ch == '}') { if (!recursion) { --dialect_depth; break; } --recursion; }
     else if (ch == '|') { if (!recursion) { break; } }
     else if (!ch && iter-1 == end) goto done;
    }
    flush_start = iter;
    goto next;
   } else {
    --dialect_depth;
   }
  } else {
   WARN(W_IASM_UNESCAPED_SPECIAL_CHARACTER,(int)iter[-1]);
  }
  goto next;


 case '%':
  /* */
  if (flush_start != iter-1) {
   stringwriter_write(&writer,flush_start,
                     (size_t)((iter-1)-flush_start));
  }
  mod = *iter; /* Parse operand reference. */
  /* Special, escaped characters. */
  if (mod == '%' || mod == '{' ||
      mod == '|' || mod == '}') {
   /* Same-character escape. */
   flush_start = iter++;
   goto next;
  }
  if (mod == '=') {
   /* Expand to a unique integer. */
   stringwriter_writeint(&writer,compiler.c_iasm_unique);
   ++compiler.c_iasm_unique;
   goto done_special;
  }


  {
   char const *new_iter;
   struct DCCIAsmOperand *operand;
   if (mod == 'c' || mod == 'n' ||
       mod == 'b' || mod == 'w' ||
       mod == 'h' || mod == 'k' ||
       mod == 'z'
#if DCC_TARGET_ASMF(DCC_CPUF_X86_64)
    || mod == 'q'
#endif
       ) ++iter;
   else mod = 0;
   operand = DCCIAsmOps_GetOperand(self,iter,&new_iter);
   if (!operand) {
    WARN(W_IASM_UNKNOWN_OPERAND,iter,(size_t)(new_iter-iter));
    operand = new_iter != iter ? self->ao_opv : NULL;
   }
   if (operand) {
    struct DCCStackValue val;
    val = *operand->ao_value;
    if (mod == 'z') {
     /* Special case: write the opcode suffix associated with
      *               the register class of the current value. */
     val.sv_reg &= DCC_RC_MASK;
     if (val.sv_reg == DCC_RC_CONST)
         val.sv_reg = DCC_RC_FORTYPE(&val.sv_ctype);
#ifdef DCC_RC_I64
     if (val.sv_reg&DCC_RC_I64) mod = 'q';
     else
#endif
          if (val.sv_reg&DCC_RC_I32) mod = 'l';
     else if (val.sv_reg&DCC_RC_I16) mod = 'w';
     else                            mod = 'b';
     stringwriter_write(&writer,&mod,1);
     goto done_op;
    }
    if (operand->ao_reg >= 0) {
     val.sv_reg = operand->ao_reg;
     if (mod == 'b')
      val.sv_reg |= DCC_RC_I8;
     else if (mod == 'h') {
      if (val.sv_reg&4) {
       static char const *const high_names[4] = {"esp","ebp","esi","edi"};
       WARN(W_IASM_NO_HIGH_REGISTER,high_names[val.sv_reg&3]);
      }
      val.sv_reg |= 4|DCC_RC_I8;
     } else if (mod == 'w') {
      if (val.sv_reg < 4) val.sv_reg |= DCC_RC_I8;
      val.sv_reg |= DCC_RC_I16;
     }
#if DCC_TARGET_ASMF(DCC_CPUF_X86_64)
     else if (mod == 'q')
      val.sv_reg |= DCC_RC_I3264|DCC_RC_I16|DCC_RC_I8;
#endif
     else val.sv_reg |= DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;

     if (!(operand->ao_flags&ASMIOP_FLAG_LLOCAL)) {
      /* Fix explicit register operands. */
      val.sv_const.it = 0;
      val.sv_sym      = NULL;
      val.sv_flags   &= ~(DCC_SFLAG_LVALUE);
     }
    } else if (mod == 'h') {
     /* Really hacky; don't look: Increase offset. */
     ++val.sv_const.it;
    }
#if 0
    if (operand->ao_reg2 >= 0) {
     val.sv_reg2  = operand->ao_reg2;
     val.sv_reg2 |= operand->ao_reg2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
    }
#endif
    /* Write the representation of this stack-value. */
    if (val.sv_reg == DCC_RC_CONST) {
     if (!(val.sv_flags&DCC_SFLAG_LVALUE) &&
          (mod != 'c' && mod != 'n'))
          stringwriter_write(&writer,"$",1);
     if (mod == 'n') val.sv_const.it = -val.sv_const.it;
     if (val.sv_sym) {
      /* Symbol offset. */
      stringwriter_write(&writer,
                         val.sv_sym->sy_name->k_name,
                         val.sv_sym->sy_name->k_size);
      if (!val.sv_const.it) goto done_op;
      if (val.sv_const.it > 0)
          stringwriter_write(&writer,"+",1);
     }
     stringwriter_writeint(&writer,val.sv_const.it);
    } else if (val.sv_flags&DCC_SFLAG_LVALUE) {
     /* Indirection w/ register offset. */
     if (val.sv_const.it || val.sv_sym) {
      if (val.sv_sym) {
       /* Symbol offset. */
       stringwriter_write(&writer,
                          val.sv_sym->sy_name->k_name,
                          val.sv_sym->sy_name->k_size);
       if (!val.sv_const.it) goto register_indirection;
       if (val.sv_const.it > 0)
           stringwriter_write(&writer,"+",1);
      }
      stringwriter_writeint(&writer,val.sv_const.it);
     }
register_indirection:
     stringwriter_write(&writer,"(%",2);
     stringwriter_writereg(&writer,val.sv_reg);
     stringwriter_write(&writer,")",1);
    } else {
     assertf(!val.sv_const.it,"Register offsets cannot be represented in assembly");
     assertf(!val.sv_sym,     "Register offsets cannot be represented in assembly");
     stringwriter_write(&writer,"%",1);
     stringwriter_writereg(&writer,val.sv_reg);
    }
   }
done_op:
   iter = new_iter;
  }
done_special:
  flush_start = iter;
  goto next;
 default: goto next;
 }
done:
 assert(iter == end);
 /* Cleanup the dialect options stack. */
 if (dialect_depth) WARN(W_IASM_UNCLOSED_DIALECT_STACK_BRACE);

 /* Flush any remainder. */
 if (flush_start != iter) {
  stringwriter_write(&writer,flush_start,
                    (size_t)(iter-flush_start));
 }

 /* Free up unused memory. */
 if (writer.sw_strc != writer.sw_stra) {
  struct TPPString *new_str;
  if unlikely(!writer.sw_strc) { free(writer.sw_strv); goto return_empty; }
  new_str = (struct TPPString *)realloc(writer.sw_strv,
                                        offsetof(struct TPPString,s_text)+
                                       (writer.sw_strc+1)*sizeof(char));
  if (new_str) writer.sw_strv = new_str;
 }
 /* Initialize the string header. */
 writer.sw_strv->s_refcnt = 1;
 writer.sw_strv->s_size   = writer.sw_strc;
 writer.sw_strv->s_text[writer.sw_strc] = '\0';
 return writer.sw_strv;
}


LEXPRIV void
DCCIAsmOps_Load(struct DCCIAsmOps *__restrict self) {
 struct DCCIAsmOperand *iter,*end;
 assert(self);
 end = (iter = self->ao_opv)+self->ao_opc;
 for (; iter != end; ++iter) {
  struct DCCStackValue *sval = iter->ao_value;
  /* Make sure to fix register offsets, as
   * those can't be represented in assembly. */
  if (!(sval->sv_flags&DCC_SFLAG_LVALUE))
        DCCStackValue_FixRegOffset(sval);
  if (iter->ao_reg >= 0) {
   if ((iter->ao_flags&ASMIOP_FLAG_LLOCAL) &&
       (sval->sv_flags&DCC_SFLAG_LVALUE)) {
    struct DCCType *type_iter = &sval->sv_ctype;
    struct DCCMemLoc ml;
    ml.ml_off = sval->sv_const.offset;
    ml.ml_reg = sval->sv_reg;
    ml.ml_sym = sval->sv_sym;
    while (DCCTYPE_GROUP(type_iter->t_type) == DCCTYPE_LVALUE) {
     assert(type_iter->t_base);
     DCCDisp_MemMovReg(&ml,iter->ao_reg|DCC_RC_PTRX);
     type_iter = &type_iter->t_base->d_type;
     ml.ml_off = 0;
     ml.ml_reg = iter->ao_reg;
     ml.ml_sym = NULL;
    }
   } else if (iter >= self->ao_opv+self->ao_out ||
             (iter->ao_flags&ASMIOP_FLAG_RW)) {
    struct DCCStackValue register_target;
    /* Load register value. */
    register_target.sv_ctype    = sval->sv_ctype;
    register_target.sv_flags    = DCC_SFLAG_NONE;
    register_target.sv_const.it = 0;
    register_target.sv_reg      = DCC_RC_I16|DCC_RC_I3264|iter->ao_reg;
    if (iter->ao_reg2 >= 0) {
     register_target.sv_reg2 = DCC_RC_I16|DCC_RC_I3264|iter->ao_reg2;
    } else {
     register_target.sv_reg2 = DCC_RC_CONST;
    }
    register_target.sv_sym = NULL;
    DCCStackValue_Store(sval,&register_target,1);
   }
  }
#if 0 /* Turns out this isn't something GCC actually does... */
  else if (iter >= self->ao_opv+self->ao_out &&
         !(iter->ao_flags&ASMIOP_FLAG_RW)) {
   /* Make sure to copy input-only l-value operands.
    * NOTE: If assembly code made use of vstack operations
    *       it would be sufficient to only mark the stack-value
    *       for copy, knowing that it will only be copied for
    *       real upon its first use. - But that can't be done here.
    * NOTE: We could theoretically scan the assembly text and
    *       try to figure out where, and how this operand is used
    *      (if it is used at all), and only copy it when it is
    *       used as the target operator of a modifying operation, 
    *       meaning that it may be changed.
    */
   if (sval->sv_flags&DCC_SFLAG_LVALUE) {
    sval->sv_flags |= DCC_SFLAG_COPY;
    DCCStackValue_Cow(sval);
   }
  }
#endif
 }
}
LEXPRIV void
DCCIAsmOps_Store(struct DCCIAsmOps *__restrict self) {
 struct DCCIAsmOperand *iter,*end;
 assert(self);
 end = (iter = self->ao_opv)+self->ao_out;
 for (; iter != end; ++iter) {
  if (iter->ao_reg >= 0) {
   struct DCCStackValue *sval = iter->ao_value;
   if (sval->sv_flags&DCC_SFLAG_LVALUE) {
    if (!(iter->ao_flags&ASMIOP_FLAG_LLOCAL)) {
     struct DCCType *type_iter = &sval->sv_ctype;
     struct DCCMemLoc ml; size_t type_size;
     type_size = DCCType_Sizeof(type_iter,NULL,1);
     if (type_size) {
      ml.ml_off = sval->sv_const.offset;
      ml.ml_reg = sval->sv_reg;
      ml.ml_sym = sval->sv_sym;
      while (DCCTYPE_GROUP(type_iter->t_type) == DCCTYPE_LVALUE) {
       assert(type_iter->t_base);
       DCCDisp_MemMovReg(&ml,iter->ao_reg|DCC_RC_PTRX);
       type_iter = &type_iter->t_base->d_type;
       ml.ml_off = 0;
       ml.ml_reg = iter->ao_reg;
       ml.ml_sym = NULL;
      }
           if (type_size == 1) DCCDisp_RegMovMem(iter->ao_reg|DCC_RC_I8,&ml);
      else if (type_size == 2) DCCDisp_RegMovMem(iter->ao_reg|DCC_RC_I16,&ml);
      else if (type_size == 4) DCCDisp_RegMovMem(iter->ao_reg|DCC_RC_I32,&ml);
#ifdef DCC_RC_I64
      else if (type_size == 8) DCCDisp_RegMovMem(iter->ao_reg|DCC_RC_I64,&ml);
#endif
      else if (iter->ao_reg2 >= 0) {
       DCCDisp_RegsBinMems('=',
                           iter->ao_reg|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8,
                           iter->ao_reg2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8,
                           &ml,type_size,1);
      } else {
       DCCDisp_RegMovMems(iter->ao_reg|DCC_RC_FORSIZE(type_size),
                          &ml,type_size,1);
      }
     }
    }
   } else if (iter >= self->ao_opv+self->ao_out ||
             (iter->ao_flags&ASMIOP_FLAG_RW)) {
    struct DCCStackValue register_target;
    /* Load register value. */
    register_target.sv_ctype    = sval->sv_ctype;
    register_target.sv_flags    = DCC_SFLAG_NONE;
    register_target.sv_const.it = 0;
    register_target.sv_reg      = DCC_RC_I16|DCC_RC_I3264|iter->ao_reg;
    if (iter->ao_reg2 >= 0) {
     register_target.sv_reg2 = DCC_RC_I16|DCC_RC_I3264|iter->ao_reg2;
    } else {
     register_target.sv_reg2 = DCC_RC_CONST;
    }
    register_target.sv_sym = NULL;
    DCCStackValue_Store(&register_target,sval,0);
   }
  } else if (iter->ao_flags&ASMIOP_FLAG_TEST) {
   /* Store the result of a test. */
   struct DCCStackValue test_value;
   test_value.sv_ctype.t_base = NULL;
   test_value.sv_ctype.t_type = DCCTYPE_BOOL;
   test_value.sv_flags        = DCC_SFLAG_MKTEST(ASMIOP_GT_TEST(iter->ao_flags));
   test_value.sv_const.it     = 0;
   test_value.sv_reg          = DCC_RC_CONST;
   test_value.sv_reg2         = DCC_RC_CONST;
   test_value.sv_sym          = NULL;
   DCCStackValue_Store(&test_value,iter->ao_value,0);
  }
 }
}



LEXPRIV void DCC_PARSE_CALL
DCCParse_AsmWithConstraints(/*ref*/struct TPPString *asmtext, int has_paren) {
 struct TPPString *new_asmtext;
#define HAS_OP() (TOK != ':' && (has_paren ? TOK != ')' : (TOK == TOK_STRING || TOK == '[')))
 struct DCCIAsmOps ops;
 size_t n_values;
 DCCIAsmOps_Init(&ops);
 if (TOK == ':') {
  YIELD();
  if (HAS_OP()) DCCIAsmOps_ParseAll(&ops,1);
  ops.ao_out = ops.ao_opc;
  if (TOK == ':') {
   YIELD();
   if (HAS_OP()) DCCIAsmOps_ParseAll(&ops,0);
   if (TOK == ':') {
    YIELD();
    if (has_paren ? TOK != ')' : (TOK == TOK_STRING))
        DCCIAsmOps_ParseClobber(&ops);
   }
  }
 }
 DCCIAsmOps_Prepare(&ops);
 /* Kill vstack entries according to constraints. */
 if (ops.ao_flags&DCCIASMOPS_FLAG_CLOBBER_EFLAGS) DCCVStack_KillTst();
 if (ops.ao_flags&DCCIASMOPS_FLAG_CLOBBER_MEMORY) DCCVStack_KillAll(0);
 else if (ops.ao_alloc) DCCVStack_KillInt(ops.ao_alloc); /* Always kill used registers. */
 DCCIAsmOps_PushSave(&ops);
 DCCIAsmOps_Load(&ops);
 /* Format input assembly text */
 new_asmtext = DCCIAsmOps_Format(&ops,asmtext);
 if (new_asmtext) TPPString_Decref(asmtext);
 else new_asmtext = asmtext;
 DCCAsm_ExecStringInherited(new_asmtext);
 DCCIAsmOps_Store(&ops);
 DCCIAsmOps_PopSave(&ops);
 /* Cleanup... */
 n_values = ops.ao_opc;
 while (n_values--) vpop(1);
 DCCIAsmOps_Quit(&ops);
}


LEXPRIV void DCC_PARSE_CALL
DCCParse_AsmStmt(int global) {
 struct TPPString *asmtext;
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
 if (!TPP_ISSTRING(TOK)) WARN(W_STMT_ASM_EXPECTED_STRING),asmtext = NULL;
 else asmtext = DCCParse_String();
 if (/*!global && */TOK == ':') {
  DCCParse_AsmWithConstraints(asmtext,_has_paren);
 } else if (asmtext) {
  DCCAsm_ExecStringInherited(asmtext);
 }
 DCCParse_ParPairEnd();
}


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_ASM_C_INL */
