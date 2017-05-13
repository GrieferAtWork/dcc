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
#ifndef GUARD_DCC_ASSEMBLER_C
#define GUARD_DCC_ASSEMBLER_C 1

#include <dcc/assembler.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/target.h>
#include <dcc/gen.h>
#include <dcc/byteorder.h>

#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#include "x86_util.h"

DCC_DECL_BEGIN

PRIVATE uint8_t asm_parse_register(void) {
 uint8_t result;
 if (TOK != '%') goto exp_reg;
 YIELD();
#if DCC_TARGET_CPU == DCC_TARGET_X86_64
 if (TOK >= KWD_ax && TOK <= KWD_rdi)
#else
 if (TOK >= KWD_ax && TOK <= KWD_edi)
#endif
 {
  result = (TOK-KWD_ax) % 8;
  YIELD();
 } else {
exp_reg:
  WARN(W_ASM_EXPECTED_REGISTER_NAME);
  result = 0;
 }
 return result;
}
PRIVATE uint8_t DCCParse_AsmShift(void) {
 struct TPPConst val; uint8_t result;
 if (!TPPLexer_Eval(&val)) return 0;
 TPPConst_ToInt(&val);
 switch (val.c_data.c_int) {
 case 1: result = 0; break;
 case 2: result = 1; break;
 case 4: result = 2; break;
 case 8: result = 3; break;
 default:
  TPPLexer_Warn(W_ASM_INVALID_SHIFT,&val);
  result = 0;
  break;
 }
 return result;
}

PUBLIC void
DCCParse_AsmOperand(struct DCCAsmOperand *op) {
 op->ao_type       = 0;
 op->ao_reg        = -1;
 op->ao_reg2       = -1;
 op->ao_val.sa_sym = NULL;
 if (TOK == '*') YIELD(),op->ao_type |= DCC_ASMOP_IND;
 if (TOK == '%') {
  /* Register name. */
  YIELD();
  if (TOK >= KWD_al && TOK <= KWD_dr7) {
   /* Regular registers. */
   int reg_id = TOK-KWD_al;
   op->ao_type |= 1 << (reg_id >> 3); /* ffs(8) == 3 */
   assert((op->ao_type&0xffff) >= DCC_ASMOP_R_8 &&
          (op->ao_type&0xffff) <= DCC_ASMOP_R_DR);
   op->ao_reg = reg_id & DCC_BITS(3);
   /* Check for special registers used during overloading. */
   if ((op->ao_type & (DCC_ASMOP_REG)) &&
       (op->ao_reg == 0)) op->ao_type |= DCC_ASMOP_EAX;
   else if (op->ao_type == DCC_ASMOP_R_8 && op->ao_reg == DCC_ASMREG_CL) op->ao_type |= DCC_ASMOP_CL;
   else if (op->ao_type == DCC_ASMOP_R_16 && op->ao_reg == DCC_ASMREG_DX) op->ao_type |= DCC_ASMOP_DX;
  } else if (TOK >= KWD_es && TOK <= KWD_gs) {
   op->ao_type |= DCC_ASMOP_R_SEG;
   op->ao_reg   = (uint8_t)(TOK-KWD_es);
   switch (op->ao_reg) {
   /* push/pop have special overloads for each of these (except for 'pop %cs') */
   case DCC_ASMREG_ES: op->ao_type |= DCC_ASMOP_ES; break;
   case DCC_ASMREG_DS: op->ao_type |= DCC_ASMOP_DS; break;
   case DCC_ASMREG_FS: op->ao_type |= DCC_ASMOP_FS; break;
   case DCC_ASMREG_GS: op->ao_type |= DCC_ASMOP_GS; break;
   case DCC_ASMREG_CS: op->ao_type |= DCC_ASMOP_CS; break;
   case DCC_ASMREG_SS: op->ao_type |= DCC_ASMOP_SS; break;
   default: break;
   }
  } else if (TOK == KWD_st) {
   YIELD();
   op->ao_type |= DCC_ASMOP_R_ST;
   op->ao_reg   = 0;
   if (TOK == '(') {
    struct TPPConst val;
    YIELD();
    if unlikely(!TPPLexer_Eval(&val)) val.c_data.c_int = 0;
    TPPConst_ToInt(&val);
    if (val.c_data.c_int < 0 || val.c_data.c_int >= 8)
     WARN(W_ASM_INVALID_ST_REGISTER,(int)val.c_data.c_int);
    op->ao_reg = (uint8_t)(val.c_data.c_int & BITS(3));
    if (TOK == ')') YIELD();
    else WARN(W_EXPECTED_RPAREN);
   }
   if (!op->ao_reg) op->ao_type |= DCC_ASMOP_ST0;
   goto done_register;
  } else {
   WARN(W_ASM_EXPECTED_REGISTER_NAME);
   goto fallback;
  }
  YIELD();
done_register:;
 } else if (TOK == '$') {
  YIELD();
  DCCParse_AsmExpr(&op->ao_val);
  if (op->ao_val.sa_sym) goto def_imm;
  if (op->ao_val.sa_off == (int8_t)op->ao_val.sa_off) {
   op->ao_type |= DCC_ASMOP_IMM_8S;
   /* Check for sub-case: immediate value equal to ONE(1). */
   if (op->ao_val.sa_off == 1) op->ao_type |= DCC_ASMOP_ONE;
  } else if (op->ao_val.sa_off == (target_off_t)(uint8_t)op->ao_val.sa_off) op->ao_type |= DCC_ASMOP_IMM_8;
  else if (op->ao_val.sa_off == (target_off_t)(uint16_t)op->ao_val.sa_off) op->ao_type |= DCC_ASMOP_IMM_16;
  else if (op->ao_val.sa_off == (target_off_t)(uint32_t)op->ao_val.sa_off) op->ao_type |= DCC_ASMOP_IMM_32;
  else def_imm: op->ao_type |= DCC_PP_CAT(DCC_ASMOP_IMM_,DCC_MUL8(DCC_TARGET_SIZEOF_POINTER));
 } else {
  op->ao_type |= DCC_ASMOP_EA;
  op->ao_shift = 0;
  /* TODO: segment-offset pair. */
  op->ao_val.sa_off = 0;
  if (TOK != '(') DCCParse_AsmExpr(&op->ao_val);
  if (!op->ao_val.sa_sym && TOK == '(') {
   YIELD();
   if (TOK != ',') op->ao_reg = asm_parse_register();
   if (TOK == ',') {
    YIELD();
    if (TOK != ',') op->ao_reg2 = asm_parse_register();
    if (TOK == ',') {
     YIELD();
     /* Not available in '.code16'. */
     if (compiler.c_flags&DCC_COMPILER_FLAG_CODE16)
         WARN(W_ASM_386_RM_SHIFT_IN_CODE16);
     op->ao_shift = DCCParse_AsmShift();
    }
   }
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  }
  if (op->ao_reg == -1 && op->ao_reg2 == -1) {
   /* Without any registers, this is merely an ADDR operand. */
   op->ao_type |= DCC_ASMOP_ADDR;
  }
 }
 return;
fallback:
 op->ao_type |= DCC_ASMOP_R_32;
 op->ao_reg   = DCC_ASMREG_EAX;
}

PUBLIC int
DCCAsm_OpCompatible(struct DCCAsmOperand const *op, uint16_t op_flags, uint16_t type) {
 uint32_t type_flags,flags;
 /* Make sure that the type matches. */
 type_flags = (uint32_t)1 << (type&0x1f);
 if (!(op->ao_type&type_flags)) {
  /* SPecial case: Allow a memory location if the EA flag is set. */
  if (type&DCC_ASMOPT_EA && op->ao_type&(DCC_ASMOP_EA|DCC_ASMOP_ADDR)) goto check_flags;
  /* Special case: Both sides require immediate values.
   *               If we can upcast the immediate, we are still compatible. */
  if ((op->ao_type&DCC_ASMOP_IMM) && (type_flags&DCC_ASMOP_IMM) && 
      (op->ao_type&DCC_ASMOP_IMM) <  (type_flags&DCC_ASMOP_IMM)) goto check_flags;
  return 0;
 }
check_flags:
 /* Make sure that all type flags as set. */
 flags = (uint32_t)(type&0x3f00) << 16;
 if ((op->ao_type&(flags|DCC_ASMOP_IND)) != flags) {
  if ((flags&DCC_ASMOP_EA) && (
      /* Allow EA/ADDR operands for EA arguments. */
      (op->ao_type&(DCC_ASMOP_ADDR|DCC_ASMOP_EA)) ||
      /* Check if registers are allowed in addition to E/A. */
      ((type_flags&DCC_ASMOP_REG) && (op->ao_type&DCC_ASMOP_REG)))) {
   flags &= ~(DCC_ASMOP_EA);
   if ((op->ao_type&flags) == flags) goto done_flags;
  }
  return 0;
 }
 /* Make sure special register requirements are all met if specified. */
 if ((type&DCC_ASMOPT_REGSPEC) &&
    ((uint32_t)(op->ao_type&(DCC_ASMOPT_REGSPEC << 16)) !=
     (uint32_t)((type&DCC_ASMOPT_REGSPEC) << 16))) {
  return 0;
 }
done_flags:
 if (type_flags&DCC_ASMOP_ADDR) {
  /* Make sure that the associated address can fit. */
  size_t minbytes,maxbytes = DCC_ASMOPT_ASIZ(type);
  struct DCCSym *op_sym;
  int_t op_val = op->ao_val.sa_off;
  /* Make sure that this overload is of sufficient size for this address. */
  if ((op_sym = op->ao_val.sa_sym) != NULL) {
   while ((DCCSym_ASSERT(op_sym),op_sym->sy_alias)) op_sym = op_sym->sy_alias;
   if (op_flags&DCC_ASMOPC_DISP &&
       DCCSym_SECTION(op_sym) == unit.u_curr) {
    size_t op_length = maxbytes; /* Size used by the immediate address. */
    /* Predict a worst-case situation for the
     * amount of bytes required by the opcode.
     * NOTE: This is basically just guess-work, but
     *       '16' should always be sufficient! */
    op_length += 16;
    /* Reverse displacement within the same section
     * >> We can predict the exact value here! */
    op_val += op_sym->sy_addr-(t_addr+op_length);
    goto def_minbytes;
   } else {
    /* An unknown symbol is contained
     * >> must assume worst case, or extern linkage,
     *    meaning we will require _all_ 'dem bytes. */
    minbytes = DCC_TARGET_SIZEOF_POINTER;
   }
  } else {
def_minbytes:
   minbytes = 0;
   while (op_val&1) ++minbytes,op_val >>= 1;
   minbytes = (minbytes+7)/8;
  }
  /* The address/disp is too large. */
  if (minbytes > maxbytes) return 0;
 }
 return 1;
}

PRIVATE void
asm_parse_expr_unary(struct DCCSymAddr *v) {
 tok_t mode;
 if (v) {
  v->sa_off = 0;
  v->sa_sym = NULL;
 }
 switch (TOK) {

 case '.':
  YIELD();
  /* Current instruction address. */
  if (v) {
   v->sa_off = (int_t)t_addr;
   v->sa_sym = &unit.u_curr->sc_start;
  }
  break;

 case TOK_INT:
  if (TOKEN.t_end[-1] == 'b' ||
      TOKEN.t_end[-1] == 'f' && v) {
   struct TPPKeyword *label_kwd;
   struct DCCSym *sym;
   int backwards = TOKEN.t_end[-1] == 'b';
   /* Forward/backward label reference. */
   label_kwd = TPPLexer_LookupEscapedKeyword(TOKEN.t_begin,
                                            (TOKEN.t_end-TOKEN.t_begin)-1,
                                             1);
   if unlikely(!label_kwd) break;
   if (backwards) {
    /* Search in reverse for the last instance of this symbol. */
    sym = DCCUnit_GetSym(label_kwd);
    if unlikely(!sym) WARN(W_ASM_UNKNOWN_LOCAL_LABEL,label_kwd);
    if (DCCSym_ISFORWARD(sym) && sym->sy_unit_before) sym = sym->sy_unit_before;
   } else {
    /* Create a new forward-label. */
    sym = DCCUnit_NewForwardSym(label_kwd,DCC_SYMFLAG_STATIC);
    if unlikely(!sym) break;
   }
   v->sa_sym = sym;
   YIELD();
   break;
  }
  /* fallthrough */
 case TOK_CHAR:
  if (v) { int_t val; TPP_Atoi(&val); v->sa_off = (target_off_t)val; }
  YIELD();
  break;

 case '+':
 case '-':
 case '~':
  mode = TOK;
  YIELD();
  asm_parse_expr_unary(v);
  if (v) {
   if (v->sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
        if (mode == '-') v->sa_off = -v->sa_off;
   else if (mode == '~') v->sa_off = ~v->sa_off;
  }
  break;

 case '(':
  YIELD();
  DCCParse_AsmExpr(v);
  if (TOK != ')') WARN(W_EXPECTED_RPAREN);
  else YIELD();
  break;

 case TOK_STRING:
  if (HAS(EXT_ASM_IMM_STRINGS)) {
   if (v) {
    struct TPPString *s;
    /* Immediate strings. */
    s = TPPLexer_ParseString();
    if unlikely(!s) break;
    DCCSection_TBEGIN(unit.u_str);
    v->sa_sym = DCCSection_DAllocSym(unit.u_str,s->s_text,
                                    (s->s_size)*sizeof(char),
                                    (s->s_size+1)*sizeof(char),1,0);
    DCCSection_TEND(unit.u_str);
    TPPString_Decref(s);
   } else {
    do YIELD();
    while (TOK == TOK_STRING);
   }
   break;
  }
  // fallthrough
 default:
  if (TPP_ISKEYWORD(TOK)) {
   /* Find/create a label using this keyword. */
   if (v) v->sa_sym = DCCUnit_NewSym(TOKEN.t_kwd,DCC_SYMFLAG_NONE);
   YIELD();
  } else if (v) {
   WARN(W_ASM_EXPECTED_EXPR);
  }
  break;
 }
}
PRIVATE void
asm_parse_expr_prod(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_unary(v);
 for (;;) {
  tok_t mode = TOK;
  if (mode != '*' && mode != '/' && mode != '%') break;
  YIELD();
  if (v) {
   asm_parse_expr_unary(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   switch (mode) {
   case '*': v->sa_off *= rhs.sa_off; break;
   case '/': if (rhs.sa_off) v->sa_off /= rhs.sa_off; break;
   default:  if (rhs.sa_off) v->sa_off %= rhs.sa_off; break;
   }
  } else {
   asm_parse_expr_unary(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_sum(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 assert(v);
 asm_parse_expr_prod(v);
 for (;;) {
  tok_t mode = TOK;
  if (mode != '+' && mode != '-') break;
  YIELD();
  if (v) {
   asm_parse_expr_prod(&rhs);
   if (mode == '+') {
    v->sa_off += rhs.sa_off;
         if (!v->sa_sym) v->sa_sym = rhs.sa_sym;
    else if (rhs.sa_sym) WARN(W_LINKER_CANNOT_RELOCATE_SYMPLUSSYM); /* symbol+symbol */
   } else {
    assert(mode == '-');
    v->sa_off -= rhs.sa_off;
    /* Special case: Difference between two symbols. */
    if (v->sa_sym && rhs.sa_sym) {
     if (v->sa_sym == rhs.sa_sym) {
      /* Always succeeds: Difference between the same symbol. */
     } else if (DCCSym_SECTION(v->sa_sym) &&
                DCCSym_SECTION(v->sa_sym) ==
                DCCSym_SECTION(rhs.sa_sym)) {
      /* Special case: Difference between two defined symbols from the same section. */
      v->sa_off += v ->sa_sym->sy_addr-
                  rhs.sa_sym->sy_addr;
      v->sa_sym  = NULL;
     } else {
      /* Special case: undefined symbols, or symbols from different sections.
       * TODO: Store two symbols in an expression to work around this! */
      WARN(W_LINKER_CANNOT_RELOCATE_SYMMINUSSYM);
     }
    }
   }
  } else {
   asm_parse_expr_prod(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_shift(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_sum(v);
 for (;;) {
  tok_t mode = TOK;
  if (mode != TOK_SHL && mode != TOK_SHR) break;
  YIELD();
  if (v) {
   asm_parse_expr_sum(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   if (mode == TOK_SHL) v->sa_off <<= rhs.sa_off;
   else                 v->sa_off >>= rhs.sa_off;
  } else {
   asm_parse_expr_sum(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_cmp(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_shift(v);
 for (;;) {
  tok_t mode = TOK;
  if (mode != TOK_LOWER &&
      mode != TOK_LOWER_EQUAL &&
      mode != TOK_GREATER &&
      mode != TOK_GREATER_EQUAL) break;
  YIELD();
  if (v) {
   asm_parse_expr_shift(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   switch (mode) {
   case TOK_LOWER:       v->sa_off = v->sa_off <  rhs.sa_off; break;
   case TOK_LOWER_EQUAL: v->sa_off = v->sa_off <= rhs.sa_off; break;
   case TOK_GREATER:     v->sa_off = v->sa_off >  rhs.sa_off; break;
   default:              v->sa_off = v->sa_off >= rhs.sa_off; break;
   }
  } else {
   asm_parse_expr_shift(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_cmp_eq(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_cmp(v);
 for (;;) {
  tok_t mode = TOK;
  if (mode != TOK_EQUAL &&
      mode != TOK_NOT_EQUAL &&
      mode != TOK_LOGT) break;
  YIELD();
  if (v) {
   asm_parse_expr_cmp(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   if (mode == TOK_EQUAL) v->sa_off = v->sa_off == rhs.sa_off;
   else                   v->sa_off = v->sa_off != rhs.sa_off;
  } else {
   asm_parse_expr_cmp(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_and(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_cmp_eq(v);
 while (TOK == '&') {
  YIELD();
  if (v) {
   asm_parse_expr_cmp_eq(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   v->sa_off &= rhs.sa_off;
  } else {
   asm_parse_expr_cmp_eq(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_xor(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_and(v);
 while (TOK == '^') {
  YIELD();
  if (v) {
   asm_parse_expr_and(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   v->sa_off ^= rhs.sa_off;
  } else {
   asm_parse_expr_and(NULL);
  }
 }
}
PRIVATE void
asm_parse_expr_or(struct DCCSymAddr *v) {
 struct DCCSymAddr rhs;
 asm_parse_expr_xor(v);
 while (TOK == '|') {
  YIELD();
  if (v) {
   asm_parse_expr_xor(&rhs);
   if (v->sa_sym || rhs.sa_sym) WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   v->sa_off |= rhs.sa_off;
  } else {
   asm_parse_expr_xor(NULL);
  }
 }
}

PUBLIC void
DCCParse_AsmExpr(struct DCCSymAddr *v) {
 /* TODO: Logic operators: '||', '&&', '^^', '?:' */
 asm_parse_expr_or(v);
}

PUBLIC int_t DCCParse_AsmExprI(void) {
 struct DCCSymAddr v;
 DCCParse_AsmExpr(&v);
 if (v.sa_sym) WARN(W_ASM_EXPECTED_INTEGER_EXPRESSION,v.sa_sym->sy_name);
 return v.sa_off;
}





#define MAX_OPERANDS 3

struct x86_opcode {
 uint32_t o_code;    /*< opcode number. */
 uint16_t o_flags;   /*< Set of 'DCC_ASMOPC_*'. */
 uint16_t o_argv[MAX_OPERANDS]; /*< [o_argc] Set of 'DCC_ASMOPT_*'. */
};
#define X86_OPCODE_ISEMPTY(x) (!((uint32_t *)(x))[0] && !((uint32_t *)(x))[1])

#define OVERLOAD_SENTINAL                            {0,0,{0,0,0}}
#define OVERLOAD(argc,opcode,opsize,group,flags,...) \
 {(opcode),flags|DCC_ASMOPC_SIZE(opsize)\
                |DCC_ASMOPC_GROUP(group)\
                |DCC_ASMOPC_ARGC(argc),\
 {__VA_ARGS__}},
#define OVERLOAD_0(opcode,opsize,group,flags)         OVERLOAD(0,opcode,opsize,group,flags,0,0,0)
#define OVERLOAD_1(opcode,opsize,group,flags,a)       OVERLOAD(1,opcode,opsize,group,flags,a,0,0)
#define OVERLOAD_2(opcode,opsize,group,flags,a,b)     OVERLOAD(2,opcode,opsize,group,flags,a,b,0)
#define OVERLOAD_3(opcode,opsize,group,flags,a,b,c)   OVERLOAD(3,opcode,opsize,group,flags,a,b,c)
#define DEF_OPCODE(name,...) PRIVATE struct x86_opcode const op_##name[] = __VA_ARGS__;
#include <dcc/def-asm.inl>
#undef DEF_OPCODE
#undef OVERLOAD_SENTINAL
#undef OVERLOAD_3
#undef OVERLOAD_2
#undef OVERLOAD_1
#undef OVERLOAD_0
#undef OVERLOAD


PRIVATE struct x86_opcode const *x86_opcodes[] = {
#define DEF_OPCODE(name,...)  op_##name,
#include <dcc/def-asm.inl>
#undef DEF_OPCODE
};
#define X86_OPCODE_COUNT   (sizeof(x86_opcodes)/sizeof(*x86_opcodes))

LOCAL void
asm_gen_modrm(int8_t reg, struct DCCAsmOperand const *op) {
 int mod,reg1,reg2,sib_reg1;
 if (op->ao_type & (DCC_ASMOP_REG|DCC_ASMOP_R_MMX|DCC_ASMOP_R_SSE)) {
  /* Regular register: mod == '11'. */
  t_putb(MODRM_REGISTER(reg,op->ao_reg));
 } else if (op->ao_reg == -1 && op->ao_reg2 == -1) {
  /* displacement only */
  if (compiler.c_flags&DCC_COMPILER_FLAG_CODE16) {
   t_putb(MODRM_DISP16(reg));
   DCCDisp_SymAddr(&op->ao_val,2);
  } else {
   t_putb(MODRM_DISP32(reg));
   DCCDisp_SymAddr(&op->ao_val,4);
  }
 } else {
  sib_reg1 = op->ao_reg;
  /* Select displacement encoding. */
  if (sib_reg1 == -1) sib_reg1 = DCC_ASMREG_EBP,mod = MODRM_MOD(B(00));
  else if (!op->ao_val.sa_off  && !op->ao_val.sa_sym && sib_reg1 != DCC_ASMREG_EBP) mod = MODRM_MOD(B(00)); /* No displacement. */
  else if (op->ao_val.sa_off == (int8_t)op->ao_val.sa_off && !op->ao_val.sa_sym) mod = MODRM_MOD(B(01)); /* 8-bit, signed displacement. */
  else mod = MODRM_MOD(B(10)); /* 32-bit displacement. */
  /* Figure out if an sib byte is needed. */
  reg1 = op->ao_reg;
  if (op->ao_reg2 != -1) reg1 = MODRM_SIBREGISTER;
  if (!(compiler.c_flags&DCC_COMPILER_FLAG_CODE16)) {
   t_putb((uint8_t)(mod|MODRM_REG(reg)|MODRM_RM(reg1)));
   if (reg1 == MODRM_SIBREGISTER) {
    /* Add the sib byte. */
    reg2 = op->ao_reg2;
    if (reg2 == -1) reg2 = MODRM_SIBREGISTER; /* No index */
    t_putb((uint8_t)(MODRM_MOD(op->ao_shift)|
                       MODRM_REG(reg2)|
                       MODRM_RM(sib_reg1)));
   }
  } else {
   /* Convert 32-bit R/M register ids to 16-bit:
    * === 32-bit
    * 0: [EAX]+disp32
    * 1: [ECX]+disp32
    * 2: [EDX]+disp32
    * 3: [EBX]+disp32
    * 4: [--][--]+disp32
    * 5: [EBP]+disp32
    * 6: [ESI]+disp32
    * 7: [EDI]+disp32
    */
        if (reg1 == DCC_ASMREG_ESI) reg1 = MODRM_B16_SI; /* 6: [ESI]+disp32 --> 4: [SI]+disp16 */
   else if (reg1 == DCC_ASMREG_EDI) reg1 = MODRM_B16_DI; /* 7: [EDI]+disp32 --> 5: [DI]+disp16 */
   else if (reg1 == DCC_ASMREG_EBX) reg1 = MODRM_B16_BX; /* 3: [EBX]+disp32 --> 7: [BX]+disp16 */
   else if (reg1 == DCC_ASMREG_EBP) reg1 = MODRM_B16_BP; /* 5: [EBP]+disp32 --> 6: [BP]+disp16 */
   else if (reg1 == DCC_ASMREG_ESP) {
    reg2 = op->ao_reg2;
         if (sib_reg1 == DCC_ASMREG_EBX && reg2 == DCC_ASMREG_ESI) reg1 = MODRM_B16_BXSI; /* bx+si+offset --> 0: [BX+SI]+disp16 */
    else if (sib_reg1 == DCC_ASMREG_EBX && reg2 == DCC_ASMREG_EDI) reg1 = MODRM_B16_BXDI; /* bx+di+offset --> 1: [BX+DI]+disp16 */
    else if (sib_reg1 == DCC_ASMREG_EBP && reg2 == DCC_ASMREG_ESI) reg1 = MODRM_B16_BPSI; /* bp+si+offset --> 2: [BP+SI]+disp16 */
    else if (sib_reg1 == DCC_ASMREG_EBP && reg2 == DCC_ASMREG_EDI) reg1 = MODRM_B16_BPDI; /* bp+di+offset --> 3: [BP+DI]+disp16 */
    else reg1 = 0,WARN(W_ASM_INVALID_EFFECTIVE_ADDRESS);
   } else {
    WARN(W_ASM_INVALID_REGISTER);
   }
   /* mod = 0, reg = 6 in 16-bit mode is a 16-bit displacement.
    * There is no displacement-less variant of REG 6 (BP) */
   if (!op->ao_val.sa_off && reg1 != MODRM_B16_DISP16) mod = 0;
   t_putb((uint8_t)(mod|MODRM_REG(reg)|MODRM_RM(reg1)));
  }
  /* add offset */
       if (mod == MODRM_MOD(B(01))) t_putb((uint8_t)op->ao_val.sa_off);
  else if (mod == MODRM_MOD(B(10)) || op->ao_reg == -1) {
   DCCDisp_SymAddr(&op->ao_val,(compiler.c_flags&DCC_COMPILER_FLAG_CODE16) ? 2 : 4);
  }
 }
}


PRIVATE void
asm_gen_op(struct x86_opcode const *op,
           struct DCCAsmOperand const *argv) {
 size_t i,j;
 uint8_t temp,argc;
 uint32_t opcode;
 assert(op);
 assert(argv);
 opcode = op->o_code;
 argc = (op->o_flags&DCC_ASMOPC_ARGCMASK) >> DCC_ASMOPC_ARGCSHIFT;
 /* Opcodes that require special handling. */
 if (opcode == 0xcd) { /* int $imm8 */
  assert(argc == 1);
  assert(argv[0].ao_type&(DCC_ASMOP_IMM_8S|DCC_ASMOP_IMM_8));
  /* Check for $imm8-style arguments. */
  if (argv[0].ao_val.sa_off == 3) {
   /* 'int $3' --> '.byte 0xcc' */
   t_putb(0xcc);
   return;
  }
 }

 /* Add the first register index if need be. */
 if (op->o_flags & DCC_ASMOPC_REG) {
  for (i = 0; i < argc; ++i) {
   if (argv[i].ao_type & (DCC_ASMOP_REG|DCC_ASMOP_R_ST) &&
     !(op->o_argv[i]&0x0f00)) { /* No special register. */
    opcode += argv[i].ao_reg;
    break;
   }
  }
 }
 if (op->o_flags&DCC_ASMOPC_D16) t_putb(0x66);
 if ((temp = (uint8_t)(opcode >> 24)) != 0) t_putb(temp);
 if ((temp = (uint8_t)(opcode >> 16)) != 0) t_putb(temp);
 if ((temp = (uint8_t)(opcode >>  8)) != 0) t_putb(temp);
 t_putb((uint8_t)opcode);
 if (op->o_flags&DCC_ASMOPC_MODRM) {
  struct DCCAsmOperand const *modrm_op;
  int8_t reg = (op->o_flags & DCC_ASMOPC_GROUPMASK) >> DCC_ASMOPC_GROUPSHIFT;
  /* Look for an E/A operand. */
  for (i = 0; i < argc; ++i) {
   if (op->o_argv[i]&DCC_ASMOPT_EA) {
    modrm_op = &argv[i];
    goto got_modrm_op;
   }
  }
  /* Look for a register/indirection operand. */
  for (i = 0; i < argc; ++i) {
   if (argv[i].ao_type&(DCC_ASMOP_REG|DCC_ASMOP_R_MMX|
                        DCC_ASMOP_R_SSE|DCC_ASMOP_IND)) {
    modrm_op = &argv[i];
    goto got_modrm_op;
   }
  }
  goto emit_args;
got_modrm_op:
  if (op->o_flags&DCC_ASMOPC_GROUP_REG) {
   /* If an operand other than the mod/rm operand uses
    * a register, use that register's id as group instead. */
   for (j = 0; j < argc; ++j) {
    if (j != i && 
        argv[j].ao_type&(DCC_ASMOP_REG|DCC_ASMOP_R_MMX|DCC_ASMOP_R_SSE|
                         DCC_ASMOP_R_CR|DCC_ASMOP_R_TR|DCC_ASMOP_R_DB|
                         DCC_ASMOP_R_SEG)) {
     reg = argv[j].ao_reg;
     break;
    }
   }
  }
  asm_gen_modrm(reg,modrm_op);
 }

emit_args:


 /* Emit immediate arguments. */
 for (i = 0; i < argc; ++i) {
  uint16_t t = op->o_argv[i];
  if ((t&DCC_ASMOPT_REGSPEC) != DCC_ASMOPT_ONE) {
   switch (t&0x1f) {
   case DCC_ASMOPT_IMM_8S:
   case DCC_ASMOPT_IMM_8:expr8:   DCCDisp_SymAddr(&argv[i].ao_val,1); break;
   case DCC_ASMOPT_IMM_16:expr16: DCCDisp_SymAddr(&argv[i].ao_val,2); break;
   case DCC_ASMOPT_IMM_32:expr32: DCCDisp_SymAddr(&argv[i].ao_val,4); break;
#ifdef DCC_ASMOPT_IMM_64
   case DCC_ASMOPT_IMM_64:expr64: DCCDisp_SymAddr(&argv[i].ao_val,8); break;
#endif
   case DCC_ASMOPT_ADDR:
    if (op->o_flags&DCC_ASMOPC_DISP) {
     switch (t&DCC_ASMOPT_AMASK) {
      /* Displacement (operand size is based on the 'DCC_ASMOPT_A(8|16|32|64)' flags) */
     default: DCCDisp_SymDisp(&argv[i].ao_val,1); break;
     case DCC_ASMOPT_A16: DCCDisp_SymDisp(&argv[i].ao_val,2); break;
     case DCC_ASMOPT_A32: DCCDisp_SymDisp(&argv[i].ao_val,4); break;
#ifdef DCC_ASMOPT_A64
     case DCC_ASMOPT_A64: DCCDisp_SymDisp(&argv[i].ao_val,8); break;
#endif
     }
    } else {
     switch (t&DCC_ASMOPT_AMASK) {
     default: goto expr8;
     case DCC_ASMOPT_A16: goto expr16;
     case DCC_ASMOPT_A32: goto expr32;
#ifdef DCC_ASMOPT_A64
     case DCC_ASMOPT_A64: goto expr64;
#endif
     }
    }
    break;
   default: break;
   }
  }
 }
}

PRIVATE void
asm_parse_op(struct x86_opcode const *ops, size_t size_override) {
 struct DCCAsmOperand argv[MAX_OPERANDS];
 struct x86_opcode const *op,*resop;
 size_t i,argc = 0,shifted_argc;
 assert(ops);
 (void)size_override; // Use to resolve ambiguity
 /* Parse arguments. */
 if (!(ops->o_flags&DCC_ASMOPC_PREFIX)) {
  while (TOK > 0 && TOK != '\n' &&
         TOK != ';' && argc != MAX_OPERANDS) {
   DCCParse_AsmOperand(&argv[argc++]);
   if (TOK != ',') break;
   YIELD();
  }
 }
 /* Find a suitable overload. */
 shifted_argc = argc << DCC_ASMOPC_ARGCSHIFT;
 for (op = ops,resop = NULL; !X86_OPCODE_ISEMPTY(op); ++op) {
  if ((size_t)(op->o_flags&DCC_ASMOPC_ARGCMASK) == shifted_argc) {
   size_t opsize;
   if (size_override &&
      (opsize = (size_t)((op->o_flags&DCC_ASMOPC_SIZEMASK) >> DCC_ASMOPC_SIZESHIFT),
       opsize && (opsize = 1 << (opsize-1),1)) &&
      (opsize != size_override)) continue;
   /* Make sure that all operands are compatible. */
   for (i = 0; i != argc; ++i) {
    if (!DCCAsm_OpCompatible(&argv[i],op->o_flags,op->o_argv[i])) goto next_op;
   }
   if (resop) {
    for (i = 0; i != argc; ++i) {
     uint16_t old_flag = resop->o_argv[i];
     uint16_t new_flag = op   ->o_argv[i];
     uint8_t old_type = old_flag&0xff;
     uint8_t new_type = new_flag&0xff;
     /* If the ambiguity arises because we're using a register-specific
      * operator, don't warn about ambiguity and simply use the specific opcode. */
     if ((old_flag&DCC_ASMOPT_REGSPEC) !=
         (new_flag&DCC_ASMOPT_REGSPEC)) {
      if (new_flag&DCC_ASMOPT_REGSPEC) goto select_resop;
      goto next_op;
     }
     /* If the ambiguity has arisen due to immediate value
      * range, choose the smaller $imm and don't warn. */
     if (old_type != new_type) {
      if (old_type >= DCC_ASMOPT_IMM_8S &&
#ifdef DCC_ASMOPT_IMM_64
          old_type <= DCC_ASMOPT_IMM_64 &&
#else
          old_type <= DCC_ASMOPT_IMM_32 &&
#endif
          new_type >= DCC_ASMOPT_IMM_8S &&
#ifdef DCC_ASMOPT_IMM_64
          new_type <= DCC_ASMOPT_IMM_64
#else
          new_type <= DCC_ASMOPT_IMM_32
#endif
          ) {
       /* Ambiguity has arisen due to differing immediate value ranges.
        * >> Select the smaller range to reduce code size, and don't warn about ambiguity. */
       if (new_type < old_type) goto select_resop;
       goto next_op;
      }
     } else if (old_type == DCC_ASMOPT_ADDR) {
      uint16_t old_size,new_size;
      /* Both types are 'DCC_ASMOPT_ADDR'
       * >> Select the opcode with the smaller address range,
       *    but don't do so if both opcodes have identical ranges.
       */
      old_size = old_flag&DCC_ASMOPT_AMASK;
      new_size = new_flag&DCC_ASMOPT_AMASK;
      if (old_size != new_size) {
       if (new_size < old_size) goto select_resop;
       goto next_op;
      }
     }
    }
    WARN(W_ASM_AMBIGUOUS_INSTR);
   } else {
select_resop:
    resop = op;
   }
   /* No need to continue searching if the size modifier is explicit. */
   if (size_override) break;
  }
next_op:;
 }
 if (!resop) goto no_overload;
 /* Generate the actual code. */
 asm_gen_op(resop,argv);
 return;
no_overload:
 WARN(W_ASM_NO_SUCH_OVERLOAD);
}

PRIVATE struct TPPKeyword *
lookup_asm_opkwd(char const *__restrict name, size_t size) {
 struct TPPKeyword *result;
 result = TPPLexer_LookupKeyword(name,size,0);
 if ((!result || TPP_ISUSERKEYWORD(result->k_id)) &&
       HAS(EXT_ASM_CASE_INSENSITIVE)) {
  /* Search again, but with a lower-case opcode string. */
  char ch,*dst,*buf = (char *)alloca(size*sizeof(char));
  char const *src,*end;
  dst = buf;
  end = (src = name)+size;
  for (; src != end; ++src,++dst) {
   ch = *src;
   if (ch >= 'A' && ch <= 'Z') ch += ('a'-'A');
   *dst = ch;
  }
  result = TPPLexer_LookupKeyword(buf,size,0);
 }
 return result;
}

PUBLIC void DCCParse_AsmInstr(void) {
 struct x86_opcode const *ops;
 size_t size_override = 0;
 tok_t instr_name;
 struct TPPKeyword *instr_kwd;
 struct DCCSym *sym;
again:
 if (TOK == ';' || TOK == '\n') goto yield_and_return;
 instr_name = TOK;
 instr_kwd = TOK == TOK_INT
  /* Special handling for local assembly labels (e.g.: '1: jmp 1b'). */
  ? TPPLexer_LookupEscapedKeyword(TOKEN.t_begin,
                                 (size_t)(TOKEN.t_end-TOKEN.t_begin),
                                  1)
  : TPP_ISKEYWORD(instr_name) ? TOKEN.t_kwd : NULL;
 YIELD();
 if (instr_name == '.') {
  /* Parse an assembly directive. */
  DCCParse_AsmDirective();
  ops = NULL;
  goto done_instr;
 }
 if (instr_kwd) {
  if (TOK == ':' || TOK == '=') {
   /* Assembly label. */
   sym = (instr_name == TOK_INT)
    ? DCCUnit_NewForwardSym(instr_kwd,DCC_SYMFLAG_NONE)
    : DCCUnit_NewSym(instr_kwd,DCC_SYMFLAG_NONE);
   if unlikely(!sym) return;
   if (TOK == '=') {
    struct DCCSymAddr v;
    struct DCCSection *sym_section = &DCCSection_Abs;
    YIELD();
    /* Define an absolute symbol. */
    DCCParse_AsmExpr(&v);
    if (v.sa_sym) {
     if (DCCSym_SECTION(v.sa_sym)) {
      /* Offset from a known symbol. */
      v.sa_off   += v.sa_sym->sy_addr;
      sym_section = DCCSym_SECTION(v.sa_sym);
     } else {
      WARN(W_ASM_UNKNOWN_SYMBOL_IN_ABSOLUTE_LABEL,v.sa_sym->sy_name);
     }
    }
    DCCSym_Define(sym,sym_section,(target_ptr_t)v.sa_off,0);
    goto again;
   } else {
    t_defsym(sym);
   }
   YIELD();
   goto again;
  }
 }
 size_override = 0;
check_instr_name:
 if (instr_name >= DCC_OPCODE_FIRST &&
     instr_name < DCC_OPCODE_FIRST+X86_OPCODE_COUNT) {
  ops = x86_opcodes[instr_name-DCC_OPCODE_FIRST];
 } else if (instr_kwd && !size_override) { /* Make sure this is the first pass. */
  /* TODO: Extension for case-insensitive asm instructions. */
  size_t suffix_length = 1;
  char last_ch = instr_kwd->k_name[instr_kwd->k_size-1];
       if (last_ch == 'b') size_override = 1;
  else if (last_ch == 'w') size_override = 2;
  else if (last_ch == 'l' || last_ch == 'd') size_override = 4;
  else if (last_ch == 'q') size_override = 8;
  else if (HAS(EXT_ASM_FIXED_LENGTH)) {
   /* Extension: fixed-length suffix (e.g.: 'mov8' --> 'movb', 'movI' --> 'movl/q') */
        if (last_ch == 'I') size_override = DCC_TARGET_SIZEOF_POINTER;
   else if (last_ch == '8') size_override = 1;
   else if (last_ch >= '0' && last_ch <= '9' &&
            instr_kwd->k_size >= 2 && !((last_ch-'0')%2)) {
    char const *instr_end = instr_kwd->k_name+(instr_kwd->k_size-2);
    ++suffix_length;
    /* Last character is a digit divisible by 2. */
         if (*instr_end == '1' && last_ch == '6') size_override = 2;
    else if (*instr_end == '3' && last_ch == '2') size_override = 4;
    else if (*instr_end == '6' && last_ch == '4') size_override = 8;
    else if (*instr_end == '2' && last_ch == '8' &&
             instr_kwd->k_size >= 3 && instr_end[-1] == '1'
             ) ++suffix_length,size_override = 16;
   }
  }

  if unlikely(!size_override) goto unknown_instr;
  /* Check again after removing the last character. */
  assert(suffix_length);
  assert(instr_kwd->k_size >= suffix_length);
  instr_kwd = lookup_asm_opkwd(instr_kwd->k_name,
                               instr_kwd->k_size-suffix_length);
  if unlikely(!instr_kwd) goto unknown_instr;
  /* Load the instruction name. */
  instr_name = instr_kwd->k_id;
  goto check_instr_name;
 } else {
unknown_instr:
  WARN(W_ASM_EXPECTED_INSTR);
  ops = NULL;
 }
 if (ops) asm_parse_op(ops,size_override);
done_instr:
 if (TOK == ';' || TOK == '\n' || TOK <= 0) yield_and_return: YIELD();
 else if (!ops || !(ops->o_flags&DCC_ASMOPC_PREFIX)) {
  WARN(W_ASM_JUNK_AFTER_INSTR);
  do YIELD();
  while (TOK > 0 && TOK != '\n' && TOK != ';');
 }
}


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif
PUBLIC void DCCParse_AsmDirective(void) {
 tok_t mode = TOK;
 switch (mode) {

 {
  size_t v;
  int filler;
 case KWD_align:
 case KWD_skip:
 case KWD_space:
  YIELD();
  v = (size_t)DCCParse_AsmExprI();
  if (mode == KWD_align && v) {
   target_ptr_t aligned_addr;
   if (v & (v-1)) WARN(W_ASM_INVALID_ALIGNMENT,v);
   else if (DCCCompiler_ISCGEN()) {
    /* Make sure the current text section can sustain this alignment. */
    if (v > unit.u_curr->sc_align) unit.u_curr->sc_align = v;
   }
   aligned_addr = t_addr;
   aligned_addr = (aligned_addr+(v-1))&~(v-1);
   if (aligned_addr < t_addr) aligned_addr = t_addr;
   v = aligned_addr-t_addr;
  }
  filler = 0;
  if (TOK == ',') {
   YIELD();
   filler = (int)DCCParse_AsmExprI();
  }
fill_data:
  {
   void *dat = t_alloc(v);
   if (dat) memset(dat,filler,v);
  }
  break;

 { target_ptr_t new_origin;
 case KWD_org:
 case '=': /* Support for '. = 42' */
  new_origin = (target_ptr_t)DCCParse_AsmExprI();
  if (new_origin > t_addr) {
   /* Skip ahead. */
   v = new_origin-t_addr;
   filler = 0;
   goto fill_data;
  } else {
   /* Move backwards. */
   unit.u_tbuf.tb_pos = unit.u_tbuf.tb_begin+new_origin;
  }
 } break;
 }

 { /* Emit integral constants. */
  size_t csize; int_t cval;
  if (DCC_MACRO_FALSE) { case KWD_byte:  csize = 1; }
  if (DCC_MACRO_FALSE) { case KWD_word:
                         case KWD_hword:
                         case KWD_octa:  csize = 2; }
  if (DCC_MACRO_FALSE) { case KWD_quad:  csize = 8; }
  if (DCC_MACRO_FALSE) { case KWD_long:  csize = DCC_TARGET_SIZEOF_LONG; }
  if (DCC_MACRO_FALSE) { case KWD_int:   csize = DCC_TARGET_SIZEOF_INT; }
  if (DCC_MACRO_FALSE) { case KWD_short: csize = DCC_TARGET_SIZEOF_SHORT; }
  do {
   YIELD();
   cval = DCCParse_AsmExprI();
#if DCC_HOST_BYTEORDER != DCC_TARGET_BYTEORDER
   switch (csize) {
   case 2: cval = (int_t)DCC_H2T16((uint16_t)cval); break;
   case 4: cval = (int_t)DCC_H2T32((uint32_t)cval); break;
   case 8: cval = (int_t)DCC_H2T64((uint64_t)cval); break;
   default: break;
   }
#endif
   t_write(&cval,csize);
  } while (TOK == ',');
 } break;

 {
  size_t frepeat,fsize; int_t fval;
  uint8_t *buffer;
 case KWD_fill:
  YIELD();
  frepeat = (size_t)DCCParse_AsmExprI();
  fsize = 1,fval = 0;
  if (TOK == ',') {
   YIELD();
   fsize = (size_t)DCCParse_AsmExprI();
   if (fsize > 8) fsize = 8;
   if (TOK == ',') {
    YIELD();
    fval = DCCParse_AsmExprI();
   }
  }
#if DCC_HOST_BYTEORDER != DCC_TARGET_BYTEORDER
  switch (fsize) {
  case 2: fval = (int_t)DCC_H2T16((uint16_t)fval); break;
  case 4: fval = (int_t)DCC_H2T32((uint32_t)fval); break;
  case 8: fval = (int_t)DCC_H2T64((uint64_t)fval); break;
  default: break;
  }
#endif
  buffer = (uint8_t *)t_alloc(frepeat*fsize);
  if likely(buffer) while (frepeat--) {
   memcpy(buffer,&fval,fsize);
   buffer += fsize;
  }
 } break;

 { /* Define symbol visibility. */
  uint16_t new_vis;
 case KWD_extern: case KWD_weak:
  if (DCC_MACRO_FALSE) { case KWD_global: case KWD_globl: new_vis = DCC_SYMFLAG_NONE; }
  if (DCC_MACRO_FALSE) { case KWD_protected: new_vis = DCC_SYMFLAG_PROTECTED; }
  if (DCC_MACRO_FALSE) { case KWD_hidden:    new_vis = DCC_SYMFLAG_PRIVATE; }
  if (DCC_MACRO_FALSE) { case KWD_internal:  new_vis = DCC_SYMFLAG_INTERNAL; }
  do {
   struct DCCSym *sym;
   YIELD();
   if (!TPP_ISKEYWORD(TOK)) {
    WARN(W_ASM_DIRECTIVE_VISIBILITY_EXPECTED_KEYWORD);
    break;
   }
   sym = DCCUnit_NewSym(TOKEN.t_kwd,new_vis);
   if unlikely(!sym) break;
   if (mode == KWD_weak) sym->sy_flags |= DCC_SYMFLAG_WEAK;
   else if (mode != KWD_extern) {
    sym->sy_flags &= ~(DCC_SYMFLAG_VISIBILITYBASE);
    sym->sy_flags |=   new_vis;
   }
   YIELD();
  } while (TOK == ',');
 } break;

 { /* Emit a string/character sequence. */
  struct TPPString *text;
  register size_t write_plus;
 case KWD_string:
 case KWD_ascii:
 case KWD_asciz:
  write_plus = TOK != KWD_ascii;
  do {
   YIELD();
   if (TOK != TOK_STRING) {
    WARN(W_ASM_DIRECTIVE_STRING_EXPECTED_STRING);
    break;
   }
   text = TPPLexer_ParseString();
   t_write(text->s_text,
                       text->s_size+write_plus);
   TPPString_Decref(text);
  } while (TOK == ',');
 } break;

 { /* Select the current section. */
 case KWD_text:
 case KWD_data:
 case KWD_bss:
  YIELD();
  DCCUnit_SetCurr(mode == KWD_text ? unit.u_text :
                  mode == KWD_data ? unit.u_data :
                                     unit.u_bss);
 } break;


 { /* Select the current section by name. */
  struct TPPString *section_name,*section_opts;
  struct TPPKeyword *section_kwd;
  uint32_t section_type; char const *iter;
  struct DCCSection *section;
 case KWD_section:
  YIELD();
  if (TOK == TOK_STRING) {
   section_name = TPPLexer_ParseString();
   if unlikely(!section_name) break;
   if (TOK == ',') {
    /* Parse options */
    YIELD();
    section_type = 0;
    section_opts = TPPLexer_ParseString();
    if (section_opts) {
     iter = section_opts->s_text;
     for (; *iter; ++iter) switch (*iter) {
     case 'b': section_type = DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W; break;
     case 'n': section_type = 0; break;
     case 'r': section_type |= DCC_SYMFLAG_SEC_R; break;
     case 'w': section_type |= DCC_SYMFLAG_SEC_W; break;
     case 'x': section_type |= DCC_SYMFLAG_SEC_X; break;
     case 'm': section_type |= DCC_SYMFLAG_SEC_M; break;
     case 'u': section_type |= DCC_SYMFLAG_SEC_U; break;
#ifdef DCC_SYMFLAG_SEC_S
     case 's': section_type |= DCC_SYMFLAG_SEC_S; break;
#endif
     case 'a': break;
     default: WARN(W_ASM_DIRECTIVE_SECTION_UNKNOWN_FLAG,*iter); break;
     }
     TPPString_Decref(section_opts);
    }
   } else {
    section_type = DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W;
   }
   section_kwd = TPPLexer_LookupKeyword(section_name->s_text,section_name->s_size,1);
   section = section_kwd ? DCCUnit_NewSec(section_kwd,section_type) : NULL;
   TPPString_Decref(section_name);
   if unlikely(!section) break;
   DCCUnit_SetCurr(section);
  } else {
   WARN(W_ASM_DIRECTIVE_SECTION_EXPECTED_STRING);
  }
 } break;

 case KWD_previous:
  if (unit.u_prev) DCCUnit_SetCurr(unit.u_prev);
  else WARN(W_ASM_DIRECTIVE_PREV_NO_PREVIOUS_SECTION);
  YIELD();
  break;

#if DCC_TARGET_IA32(0)
 { /* Set the current code size. */
  if (DCC_MACRO_FALSE) { case KWD_code16: compiler.c_flags |=  (DCC_COMPILER_FLAG_CODE16); }
  if (DCC_MACRO_FALSE) { case KWD_code32: compiler.c_flags &= ~(DCC_COMPILER_FLAG_CODE16); }
  YIELD();
 } break;
#endif
#if DCC_TARGET_CPU == DCC_TARGET_X86_64
 case code64: YIELD(); break;
#endif

 { /* '.set sym, expr'. Same as: 'sym = expr' */
  struct TPPKeyword *sym_kwd;
  struct DCCSymAddr v;
  struct DCCSym *sym;
  struct DCCSection *sym_sec;
 case KWD_set:
  YIELD();
  /* Define an absolute symbol. */
  if (!TPP_ISKEYWORD(TOK)) {
   WARN(W_ASM_DIRECTIVE_SET_EXPECTED_KEYWORD);
   sym_kwd = NULL;
  } else {
   sym_kwd = TOKEN.t_kwd;
   YIELD();
  }
  if (TOK != ',') WARN(W_EXPECTED_COMMA);
  else YIELD();
  DCCParse_AsmExpr(&v);
  if unlikely(!sym_kwd) break;
  sym = DCCUnit_NewSym(sym_kwd,DCC_SYMFLAG_NONE);
  if unlikely(!sym) break;
  sym_sec = &DCCSection_Abs;
  if (v.sa_sym) {
   if (DCCSym_SECTION(v.sa_sym)) {
    /* Offset from a known symbol. */
    v.sa_off += v.sa_sym->sy_addr;
    sym_sec   = DCCSym_SECTION(v.sa_sym);
   } else {
    WARN(W_ASM_UNKNOWN_SYMBOL_IN_ABSOLUTE_LABEL,v.sa_sym->sy_name);
   }
  }
  DCCSym_Define(sym,sym_sec,(target_ptr_t)v.sa_off,0);
 } break;

 {
  struct TPPString *include_filename;
  struct TPPFile *include_file;
 case KWD_include:
  YIELD();
  if (TOK == TOK_STRING) {
   include_filename = TPPLexer_ParseString();
  } else {
   WARN(W_ASM_DIRECTIVE_INCLUDE_EXPECTED_STRING);
   include_filename = NULL;
  }
  while (TOK > 0 && TOK != '\n' && TOK != ';') YIELD();
  if unlikely(!include_filename) break;
  include_file = TPPLexer_OpenFile(TPPLEXER_OPENFILE_MODE_NORMAL,
                                   include_filename->s_text,
                                   include_filename->s_size,NULL);
  if unlikely(!include_file) WARN(W_FILE_NOT_FOUND,include_filename->s_text);
  include_file = TPPFile_CopyForInclude(include_file);
  if unlikely(!include_file) break;

  TPPString_Decref(include_filename);
  if (include_file) {
   TPPLexer_PushFileInherited(include_file);
   /* Yield the first token from the file. */
   YIELD();
  }
 } break;

 {
  struct TPPString *bin_filename;
  struct TPPFile *bin_file;
  size_t skip_before,max_read;
  /* Paste all data from a given binary, (mostly) equivalent to:
   * >> .ascii __TPP_LOAD_FILE("binary")
   * >> .incbin "binary"
   */
 case KWD_incbin:
  YIELD();
  if (TOK == TOK_STRING) {
   bin_filename = TPPLexer_ParseString();
  } else {
   WARN(W_ASM_DIRECTIVE_INCLUDE_EXPECTED_STRING);
   break;
  }
  if unlikely(!bin_filename) break;
  bin_file = TPPLexer_OpenFile(TPPLEXER_OPENFILE_MODE_NORMAL,
                               bin_filename->s_text,
                               bin_filename->s_size,NULL);
  TPPString_Decref(bin_filename);
  if unlikely(!bin_file) break;
  skip_before = 0,max_read = (size_t)-1;
  if (TOK == ',') {
   YIELD();
   skip_before = (size_t)DCCParse_AsmExprI();
   if (TOK == ',') {
    YIELD();
    max_read = (size_t)DCCParse_AsmExprI();
   }
  }
  bin_file = TPPFile_CopyForInclude(bin_file);
  if unlikely(!bin_file) break;
  /* Data the file in binary-mode, writing data to the generated assembly. */
  while (TPPFile_NextChunk(bin_file,TPPFILE_NEXTCHUNK_FLAG_BINARY)) {
   char *copy_begin = bin_file->f_begin;
   char *copy_end = bin_file->f_end;
   size_t copy_size;
   if (skip_before) {
    if (copy_begin+skip_before <= copy_end) {
     copy_begin += skip_before;
     skip_before = 0;
    } else {
     skip_before -= (size_t)(copy_end-copy_begin);
     continue;
    }
   }
   copy_size = (size_t)(copy_end-copy_begin);
   if (copy_size > max_read) copy_size = max_read;
   t_write(copy_begin,copy_size);
   assert(copy_size <= max_read);
   max_read -= copy_size;
   if (!max_read) break;
  }
  TPPFile_Decref(bin_file);
 } break;

 default:
  WARN(W_ASM_UNKNOWN_DIRECTIVE);
  /* Directives not implemented/ignored. */
 case KWD_file:
 case KWD_ident:
 case KWD_size:
 case KWD_type:
 case KWD_lflags:
 case KWD_line:
 case KWD_ln:
  do YIELD();
  while (TOK > 0 && TOK != '\n' && TOK != ';');
  break;
 }
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif



DCC_DECL_END

#endif /* !GUARD_DCC_ASSEMBLER_C */
