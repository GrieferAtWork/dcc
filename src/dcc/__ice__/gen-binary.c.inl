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
#ifndef GUARD_DCC_GEN_BINARY_C_INL
#define GUARD_DCC_GEN_BINARY_C_INL 1

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/code.h>

#include "x86_util.h"

DCC_DECL_BEGIN

struct binary_operator {
  tok_t    bo_tok;     /*< Opcode token ID. */
  uint8_t  bo_r_rm8;   /*< The opcode for the '* r8, r/m8' overload. */
//uint8_t  bo_rm8_r;   /*< The opcode for the '* r/m8, r8' overload (Always 'bo_r_rm8+2'). */
//uint8_t  bo_i8_al;   /*< The opcode for the '* $imm8, %al' overload (Always 'bo_r_rm8+4'). */
  uint8_t  bo_im_grp;  /*< The group id for '* $immN, %r/mM' overloads. */
  uint16_t bo_padding; /*< Padding. */
};

PRIVATE struct binary_operator const bin_ops[] = {
 {'+',    0x00,0},
 {'|',    0x08,1},
 {TOK_INC,0x10,2},
 {TOK_DEC,0x18,3},
 {'&',    0x20,4},
 {'-',    0x28,5},
 {'^',    0x30,6},
 {'?',    0x38,7},
 {0},
};

PRIVATE uint8_t get_shift_group(tok_t op) {
 switch (op) {
 case TOK_SHR:     return 5; /* shr */
 case TOK_RANGLE3: return 7; /* sar */
 default:          return 4; /* shl */
 }
}

PUBLIC void DCCGen_BinReg(tok_t op, rc_t src, rc_t dst, int src_unsigned) {
 rc_t c_src,c_dst;
 struct binary_operator const *bin_op;
 /* Special optimizations for binary operation on the same register. */
 if (src == dst) {
  switch (op) {
   /* Special case: 'add %reg, %reg' --> 'shl $1, %reg' */
  case '+':
   asm_putb(0xd0);
   gen_modreg(4,dst&7);
   return;
   /* Special case: 'sub %reg, %reg' --> 'xor %reg, %reg' */
  case '-': op = '^'; break;
   /* Special case: 'or/and %reg, %reg' --> *nothing* */
  case '|':
  case '&': return;

#if 1
   /* What about divide-by-zero?
    * Could the user really intend an exception when '%reg == 0'?
    */
  {
   struct DCCSymExpr value;
  case '/':
  case '%':
   /* Special case: %reg / %reg --> %reg = 1. */
   /* Special case: %reg % %reg --> %reg = 0. */
   value.e_int = op == '/' ? 1 : 0;
   value.e_sym = NULL;
   DCCGen_CstMov(&value,dst);
   return;
  } break;
#endif

  default: break;
  }
 }
 c_src = src&DCC_RC_MASK;
 c_dst = dst&DCC_RC_MASK;
 bin_op = bin_ops;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  /* Special opcodes: '*', '/', '%', TOK_SHL, TOK_SHR, TOK_LANGLE3, TOK_RANGLE3 */
  src = DCCVStack_CastReg(src,src_unsigned,c_dst);
  switch (op) {

  {
  case '*':
   if (!(c_dst&(DCC_RC_I16|DCC_RC_I3264))) {
    rc_t temp_dst; /* 8-bit dst */
    temp_dst = DCCVStack_GetReg(DCC_RC_I16,1);
    DCCGen_MovReg(dst,temp_dst,1);
    /* imulw %src, %temp_dst */
    asm_putb(0x66);
    asm_putb(0x0f);
    asm_putb(0xaf);
    gen_modreg(src&7,temp_dst&7);
    DCCGen_MovReg(temp_dst,dst,1);
    return;
   } else {
    /* imulw/l %src, %dst */
    if (!(c_dst&DCC_RC_I3264)) asm_putb(0x66);
    asm_putb(0x0f);
    asm_putb(0xaf);
    goto modreg;
   }
  } break;

  { /* Divide/Modulo */
   rc_t eax,edx,original_src;
  case '%':
  case '/':
   /* Reserve AX & DX for the same storage class as 'src'. */
   eax = DCC_ASMREG_EAX|(src&DCC_RC_MASK);
   edx = DCC_ASMREG_EDX|(src&DCC_RC_MASK);
   original_src = src;
   /* If 'src' is either 'EAX' or 'EDX', it needs to be stored elsewhere! */
   if ((src&7) == DCC_ASMREG_EAX ||
       (src&7) == DCC_ASMREG_EDX) {
    /* We can either use ECX or EBX as fallback for src.
     * NOTE: Though we have to make sure not to use that of 'dst'*/
    rc_t temp;
    if ((dst&7) != DCC_ASMREG_ECX &&
        (temp = (src&DCC_RC_MASK)|DCC_ASMREG_ECX,
        !DCCVStack_GetRegInuse(temp))); /* ECX */
    else if ((dst&7) != DCC_ASMREG_EBX &&
             (temp = (src&DCC_RC_MASK)|DCC_ASMREG_EBX,
             !DCCVStack_GetRegInuse(temp))); /* EBX */
    else if ((dst&7) != DCC_ASMREG_ECX) {
     /* ECX (w/kill) */
     temp = DCCVStack_GetRegExact(DCC_ASMREG_ECX|(src&DCC_RC_MASK));
    } else {
     /* EBX (w/kill) */
     temp = DCCVStack_GetRegExact(DCC_ASMREG_EBX|(src&DCC_RC_MASK));
    }
    assert((src&DCC_RC_MASK) == (temp&DCC_RC_MASK));
    /* Move the source into our fallback register. */
    DCCGen_MovReg(src,temp,1);
    src = temp;
   }
   /* Sanity check: 'src' must not be either of these! */
   assert((src&7) != DCC_ASMREG_EAX);
   assert((src&7) != DCC_ASMREG_EDX);
   /* No need to allocate these registers if they've already been. */
   if ((original_src&7) != DCC_ASMREG_EAX && (dst&7) != DCC_ASMREG_EAX) eax = DCCVStack_GetRegExact(eax);
   if ((original_src&7) != DCC_ASMREG_EDX && (dst&7) != DCC_ASMREG_EDX) edx = DCCVStack_GetRegExact(edx);
   /* Setup the register to where 'dst' resides in EAX and EDX is filled with ZERO(0). */
   DCCGen_MovReg(dst,eax,1);
   DCCGen_BinReg('^',edx,edx,1);
   /* Generate the div/idiv instruction. */
   if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xf6+!!(src&DCC_RC_I16));
   gen_modreg(src_unsigned ? 6 : 7,src&7);
   /* Either move the Quotient, or the Remained back into dst */
   DCCGen_MovReg(op == '/' ? eax : edx,dst,1);
  } break;

  { /* Shift operations. */
   rc_t cl,used_dst;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   used_dst = dst;
   if ((dst&7) == DCC_ASMREG_CL) {
    /* The destination register overlaps with CL.
     * With that in mind, we need to move it elsewhere. */
    used_dst = dst&DCC_RI_MASK;
    used_dst = DCCVStack_GetRegOf(used_dst,
                                ~((1 << DCC_ASMREG_CL)|
                                  (1 << (src&7))));
    DCCGen_MovReg(dst,used_dst,1);
   }
   cl = DCC_ASMREG_CL|DCC_RC_I8;
   if ((src&7) != DCC_ASMREG_CL) {
    cl = DCCVStack_GetRegExact(cl);
    DCCGen_MovReg(src,cl,src_unsigned);
   }
   /* Generate the shift instruction. */
   if ((used_dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xd2+!!(used_dst&DCC_RC_I16));
   gen_modreg(get_shift_group(op),used_dst&7);
   DCCGen_MovReg(used_dst,dst,1);
  } break;

  { /* test */
  case 't':
   /* TODO: src/dst of different length. */
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0x84+!!(dst&DCC_RC_I16));
   gen_modreg(src&7,dst&7);
  } break;

  default: break;
  }
  return;
 }
 if (c_src == c_dst) {
  /* operation in the same storage class. */
  if ((c_src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
  asm_putb(bin_op->bo_r_rm8+!!(c_src&DCC_RC_I16));
  goto modreg;
 }
 if (c_src > c_dst) {
  /* Truncate: Simply copy from a lower order register,
   *           but apply a mask for EDI/ESI. */
  if (c_dst&(DCC_RC_I16|DCC_RC_I3264)) {
   /* Destination class isn't 8-bit, meaning a lower-order register is always available. */
   if (!(c_dst&DCC_RC_I3264)) asm_putb(0x66);
   asm_putb(bin_op->bo_r_rm8+1);
  } else if (!(src&4)) {
   /* 8-bit mode, but the source register has an 8-bit equivalent. */
   asm_putb(bin_op->bo_r_rm8);
  } else {
   src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I16|DCC_RC_I8);
   /* *op %src, %dst */
   asm_putb(bin_op->bo_r_rm8);
   goto modreg;
  }
 } else {
  /* At this point, we know that src is either 16, or 8-bit, and that c_dst is 16/32-bit.
   * >> In both cases, we must use a temporary register to extend 'src' to 16/32-bit. */
  src = DCCVStack_CastReg(src,src_unsigned,c_dst);
  /* *op %used_src, %dst */
  if (!(c_dst&DCC_RC_I32)) asm_putb(0x66);
  asm_putb(bin_op->bo_r_rm8+1);
  goto modreg;
 }
modreg:
 gen_modreg(src&7,dst&7);
}


PUBLIC void
DCCGen_IndBin(tok_t op, target_off_t offset,
              rc_t src, rc_t dst, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
  case '*':
   if (dst&(DCC_RC_I32|DCC_RC_I16)) {
    /* 16/32-bit mul. */
    if (!(dst&DCC_RC_I32)) asm_putb(0x66);
    asm_putb(0x0f);
    asm_putb(0xaf);
    goto modrm;
   }
   /* 8-bit mul. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCGen_MovReg(dst,temp,1);
   /* imul offset(%src), %temp */
   asm_putb(0x0f);
   asm_putb(0xaf);
   gen_modrm(temp&7,offset,src&7);
   /* Store the multiplication result. */
   DCCGen_MovReg(temp,dst,1);
  } break;

  { /* Divide/Modulo */
   rc_t eax,edx;
  case '/':
  case '%':
   eax = DCC_ASMREG_EAX|(dst&DCC_RC_MASK);
   edx = DCC_ASMREG_EDX|(dst&DCC_RC_MASK);
   if ((src&7) == DCC_ASMREG_EAX ||
       (src&7) == DCC_ASMREG_EDX) {
    /* We can either use ECX or EBX as fallback for src.
     * NOTE: Though we have to make sure not to use that of 'dst'*/
    rc_t temp;
    if ((dst&7) != DCC_ASMREG_ECX &&
        (temp = (dst&DCC_RC_MASK)|DCC_ASMREG_ECX,
        !DCCVStack_GetRegInuse(temp))); /* ECX */
    else if ((dst&7) != DCC_ASMREG_EBX &&
             (temp = (dst&DCC_RC_MASK)|DCC_ASMREG_EBX,
             !DCCVStack_GetRegInuse(temp))); /* EBX */
    else if ((dst&7) != DCC_ASMREG_ECX) {
     /* ECX (w/kill) */
     temp = DCCVStack_GetRegExact(DCC_ASMREG_ECX|(dst&DCC_RC_MASK));
    } else {
     /* EBX (w/kill) */
     temp = DCCVStack_GetRegExact(DCC_ASMREG_EBX|(dst&DCC_RC_MASK));
    }
    assert((dst&DCC_RC_MASK) == (temp&DCC_RC_MASK));
    /* Generate a regular, register operation.
     * >> The alternative here would be to move the source base pointer
     *    into an unused register and simply offset from that register.
     *    Yet that is kind-of pointless, because we can also simply
     *    move the source data from memory into that temporary register,
     *    meaning that one less move operation comes into play.
     */
    DCCGen_IndMov(offset,src,temp);
    DCCGen_BinReg(op,temp,dst,1);
    return;
   }
   /* Allocate EAX and EDX. */
   if ((dst&7) != DCC_ASMREG_EAX) eax = DCCVStack_GetRegExact(eax);
   if ((dst&7) != DCC_ASMREG_EDX) edx = DCCVStack_GetRegExact(edx);
   /* Move dst into eax. */
   DCCGen_MovReg(dst,eax,1);
   DCCGen_BinReg('^',edx,edx,1);
   /* Generate the div/idiv instruction. */
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xf6+!!(dst&DCC_RC_I16));
   gen_modrm(src_unsigned ? 6 : 7,offset,src&7);
   /* Either move the Quotient, or the Remained back into dst */
   DCCGen_MovReg(op == '/' ? eax : edx,dst,1);
  } break;

  { /* Shift operations. */
   rc_t cl;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   cl = DCCVStack_GetRegExact(DCC_ASMREG_CL|DCC_RC_I8);
   DCCGen_IndMov(offset,src,cl);
   DCCGen_BinReg(op,cl,dst,src_unsigned);
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op offset(%src), %dst */
 if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
 asm_putb(bin_op->bo_r_rm8+2+!!(dst&DCC_RC_I16));
modrm:
 gen_modrm(dst&7,offset,src&7);
}

PUBLIC void
DCCGen_BinInd(tok_t op, rc_t src, target_off_t offset,
              rc_t dst, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
  case '*':
   if (src&(DCC_RC_I32|DCC_RC_I16)) {
    /* 16/32-bit mul. */
    if (!(src&DCC_RC_I32)) asm_putb(0x66);
    asm_putb(0x0f);
    asm_putb(0xaf);
    goto modrm;
   }
   /* 8-bit mul. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCGen_BinReg('^',temp,temp,1); /* Clear the 16-bit temp register. */
   DCCGen_IndMov(offset,dst,temp&~(DCC_RC_I16));
   /* imul %src, %temp */
   asm_putb(0x0f);
   asm_putb(0xaf);
   gen_modreg(temp&7,src&7);
   /* Store the multiplication result. */
   DCCGen_MovInd(temp&~(DCC_RC_I16),offset,src);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Prefer storing the 'dst' in the correct output register. */
   temp = op == '/' ? DCC_ASMREG_EAX : DCC_ASMREG_EDX;
   if ((src&7) == temp || (dst&7) == temp) {
    /* Either 'src', or 'dst' are already using the true output register.
     * So instead, we must allocate a new register of the same class.
     * WARNING: Having to do this may result in less optimal code. */
    temp = DCCVStack_GetReg(src&DCC_RC_MASK,1);
   } else {
    temp |= (src&DCC_RC_MASK);
    DCCVStack_GetRegExact(temp);
   }
   DCCGen_IndMov(offset,dst,temp);
   DCCGen_BinReg(op,src,temp,src_unsigned);
   DCCGen_MovInd(temp,offset,dst);
   break;
  }

  { /* Shift operations. */
   rc_t cl,used_dst;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   cl = DCC_ASMREG_CL|DCC_RC_I8;
   used_dst = dst;
   if ((dst&7) == DCC_ASMREG_CL) {
    used_dst = DCCVStack_GetReg(DCC_RC_PTR,1);
    DCCGen_MovReg(dst,used_dst,1);
   }
   if ((src&7) != DCC_ASMREG_CL) {
    cl = DCCVStack_GetRegExact(cl);
    DCCGen_MovReg(src,cl,src_unsigned);
   }
   /* Generate the shift instruction. */
   if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xd2+!!(src&DCC_RC_I16));
   gen_modrm(get_shift_group(op),offset,used_dst&7);
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op %src, offset(%dst) */
 if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
 asm_putb(bin_op->bo_r_rm8+!!(dst&DCC_RC_I16));
modrm:
 gen_modrm(src&7,offset,dst&7);
}
PUBLIC void
DCCGen_SymBin(tok_t op, struct DCCSymExpr const *__restrict symaddr,
              rc_t dst, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
  case '*':
   if (dst&(DCC_RC_I32|DCC_RC_I16)) {
    /* 16/32-bit mul. */
    if (!(dst&DCC_RC_I32)) asm_putb(0x66);
    asm_putb(0x0f);
    asm_putb(0xaf);
    goto moddisp;
   }
   /* 8-bit mul. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCGen_MovReg(dst,temp,1);
   /* imulw symaddr, %temp */
   asm_putb(0x0f);
   asm_putb(0xaf);
   asm_putb(MODRM_DISP32(temp&7));
   DCCTarget_TextExpr32(symaddr);
   /* Store the multiplication result. */
   DCCGen_MovReg(temp,dst,1);
  } break;

  { /* Divide/Modulo */
   rc_t eax,edx;
  case '/':
  case '%':
   eax = DCC_ASMREG_EAX|(dst&DCC_RC_MASK);
   edx = DCC_ASMREG_EDX|(dst&DCC_RC_MASK);
   /* Allocate EAX and EDX. */
   if ((dst&7) != DCC_ASMREG_EAX) eax = DCCVStack_GetRegExact(eax);
   if ((dst&7) != DCC_ASMREG_EDX) edx = DCCVStack_GetRegExact(edx);
   /* Move dst into eax. */
   DCCGen_MovReg(dst,eax,1);
   DCCGen_BinReg('^',edx,edx,1);
   /* Generate the div/idiv instruction. */
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xf6+!!(dst&DCC_RC_I16));
   asm_putb(MODRM_DISP32(src_unsigned ? 6 : 7));
   DCCTarget_TextExpr32(symaddr);
   /* Either move the Quotient, or the Remained back into dst */
   DCCGen_MovReg(op == '/' ? eax : edx,dst,1);
  } break;

  { /* Shift operations. */
   rc_t cl;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   cl = DCCVStack_GetRegExact(DCC_ASMREG_CL|DCC_RC_I8);
   DCCGen_SymMov(symaddr,cl);
   /* Generate the shift instruction. */
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xd2+!!(dst&DCC_RC_I16));
   gen_modreg(get_shift_group(op),dst&7);
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op symaddr, %dst */
 if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
 asm_putb(bin_op->bo_r_rm8+2+!!(dst&DCC_RC_I16));
moddisp:
 asm_putb(MODRM_DISP32(dst&7));
 DCCTarget_TextExpr32(symaddr);
}
PUBLIC void
DCCGen_BinSym(tok_t op, rc_t src,
              struct DCCSymExpr const *__restrict symaddr,
              int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
  case '*':
   if (src&(DCC_RC_I32|DCC_RC_I16)) {
    /* 16/32-bit mul. */
    if (!(src&DCC_RC_I32)) asm_putb(0x66);
    asm_putb(0x0f);
    asm_putb(0xaf);
    goto modrm;
   }
   /* 8-bit mul. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCGen_BinReg('^',temp,temp,1); /* Clear the 16-bit temp register. */
   DCCGen_SymMov(symaddr,temp&~(DCC_RC_I16));
   /* imul %src, %temp */
   asm_putb(0x0f);
   asm_putb(0xaf);
   gen_modreg(temp&7,src&7);
   /* Store the multiplication result. */
   DCCGen_MovSym(temp&~(DCC_RC_I16),symaddr);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Prefer storing the 'dst' in the correct output register. */
   temp = op == '/' ? DCC_ASMREG_EAX : DCC_ASMREG_EDX;
   if ((src&7) == temp) {
    /* Either 'src', or 'dst' are already using the true output register.
     * So instead, we must allocate a new register of the same class.
     * WARNING: Having to do this may result in less optimal code. */
    temp = DCCVStack_GetReg(src&DCC_RC_MASK,1);
   } else {
    temp |= (src&DCC_RC_MASK);
    DCCVStack_GetRegExact(temp);
   }
   DCCGen_SymMov(symaddr,temp);
   DCCGen_BinReg(op,src,temp,src_unsigned);
   DCCGen_MovSym(temp,symaddr);
   break;
  }

  { /* Shift operations. */
   rc_t cl;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   cl = DCC_ASMREG_CL|DCC_RC_I8;
   if ((src&7) != DCC_ASMREG_CL) {
    cl = DCCVStack_GetRegExact(cl);
    DCCGen_MovReg(src,cl,src_unsigned);
   }
   /* Generate the shift instruction. */
   if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   asm_putb(0xd2+!!(src&DCC_RC_I16));
   asm_putb(MODRM_DISP32(get_shift_group(op)));
   DCCTarget_TextExpr32(symaddr);
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op %src, symaddr */
 if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
 asm_putb(bin_op->bo_r_rm8+!!(src&DCC_RC_I16));
modrm:
 asm_putb(MODRM_DISP32(src&7));
 DCCTarget_TextExpr32(symaddr);
}

PUBLIC void
DCCGen_CstBin(tok_t op, struct DCCSymExpr const *__restrict symaddr,
              rc_t reg, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
  case '*':
   if (reg&(DCC_RC_I32|DCC_RC_I16)) {
    /* 16/32-bit mul. */
    if (!(reg&DCC_RC_I32)) asm_putb(0x66);
    if (!symaddr->e_sym && symaddr->e_int == (int8_t)symaddr->e_int) {
     /* imul $imm8, %r16/32 */
     asm_putb(0x6b);
     reg &= ~(DCC_RC_I3264|DCC_RC_I16);
    } else {
     /* imul $imm16/32, %r16/32 */
     asm_putb(0x69);
    }
    gen_modreg(reg&7,reg&7);
    goto put_expr;
   }
   /* 8-bit mul w/ immediate value. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCGen_MovReg(reg,temp,1);
   /* imulw $symaddr, %temp */
   DCCGen_CstBin('*',symaddr,temp,src_unsigned);
   /* Store the multiplication result. */
   DCCGen_MovReg(temp,reg,1);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Allocate a temporary register for the source constant.
    * NOTE: Make sure not to allocate the destination register,
    *       or the fixed registers that will be required for the opcode.
    */
   temp = DCCVStack_GetRegOf(reg&DCC_RC_MASK,
                           ~((1 << DCC_ASMREG_EAX)|
                             (1 << DCC_ASMREG_EDX)|
                             (1 << (reg&7))));
   DCCGen_CstMov(symaddr,temp);
   DCCGen_BinReg(op,temp,reg,src_unsigned);
   break;
  }

  { /* Shift operations. */
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   /* Generate the shift instruction. */
   if ((reg&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
   if (!symaddr->e_sym && symaddr->e_int == 1) {
    /* Shift by 1. */
    asm_putb(0xd0+!!(reg&DCC_RC_I16));
    gen_modreg(get_shift_group(op),reg&7);
   } else {
    asm_putb(0xc0+!!(reg&DCC_RC_I16));
    gen_modreg(get_shift_group(op),reg&7);
    DCCTarget_TextExpr8(symaddr);
   }
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op symaddr, %reg */
 if ((reg&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) asm_putb(0x66);
 if ((reg&7) == DCC_ASMREG_EAX) {
  asm_putb(bin_op->bo_r_rm8+4+!!(reg&DCC_RC_I16));
 } else {
  if (!(reg&(DCC_RC_I16|DCC_RC_I3264))) {
   /* 8-bit immediate value. */
   asm_putb(0x80);
  } else if (!symaddr->e_sym && symaddr->e_int == (int8_t)symaddr->e_int) {
   /* 7-bit immediate value. */
   asm_putb(0x83);
   reg &= ~(DCC_RC_I16|DCC_RC_I3264);
  } else {
   /* 16/32-bit immediate value. */
   asm_putb(0x81);
  }
  gen_modreg(bin_op->bo_im_grp,reg&7);
 }
put_expr:
      if (reg&DCC_RC_I32) DCCTarget_TextExpr32(symaddr);
 else if (reg&DCC_RC_I16) DCCTarget_TextExpr16(symaddr);
 else                     DCCTarget_TextExpr8(symaddr);
}

PUBLIC void
DCCGen_CstBinInd(tok_t op, struct DCCSymExpr const *__restrict symaddr,
                 target_off_t offset, rc_t dst, size_t n, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
   rc_t temp;
  case '*':
   temp = DCCVStack_GetReg(n == 4 ? DCC_RC_I32 : DCC_RC_I16,n != 1);
   assertf(n != 1 || temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   if (n == 1) {
    DCCGen_BinReg('^',temp,temp,1);
    DCCGen_IndMov(offset,dst,temp&~(DCC_RC_I16));
    DCCGen_CstBin(op,symaddr,temp,src_unsigned);
    temp &= ~(DCC_RC_I16);
   } else {
    /* imulw/l $imm8/16/32, offset(%dst), %temp */
    if (n == 2) asm_putb(0x66);
    if (!symaddr->e_sym && symaddr->e_int == (int8_t)symaddr->e_int) asm_putb(0x6b),n = 1;
    else                                                             asm_putb(0x69);
    gen_modrm(temp&7,offset,dst&7);
         if (n == 4) DCCTarget_TextExpr32(symaddr);
    else if (n == 2) DCCTarget_TextExpr16(symaddr);
    else             DCCTarget_TextExpr8(symaddr);
   }
   /* Store the multiplication result. */
   DCCGen_MovInd(temp,offset,dst);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Allocate a temporary register for the source constant.
    * NOTE: Make sure not to allocate the destination register,
    *       or the fixed registers that will be required for the opcode.
    */
   temp = DCCVStack_GetRegOf(DCC_RC_FORSIZE(n),
                           ~((1 << DCC_ASMREG_EAX)|
                             (1 << DCC_ASMREG_EDX)|
                             (1 << (dst&7))));
   DCCGen_CstMov(symaddr,temp);
   DCCGen_BinInd(op,temp,offset,dst,src_unsigned);
   break;
  }

  { /* Shift operations. */
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   /* Generate the shift instruction. */
   if (n == 2) asm_putb(0x66);
   if (!symaddr->e_sym && symaddr->e_int == 1) {
    /* Shift by 1. */
    asm_putb(0xd0+(n >= 2));
    gen_modrm(get_shift_group(op),offset,dst&7);
   } else {
    asm_putb(0xc0+(n >= 2));
    gen_modrm(get_shift_group(op),offset,dst&7);
    DCCTarget_TextExpr8(symaddr);
   }
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op symaddr, offset(%dst) */
 if (n == 2) asm_putb(0x66);
 if (n == 1) {
  /* 8-bit immediate value. */
  asm_putb(0x80);
 } else if (!symaddr->e_sym && symaddr->e_int == (int8_t)symaddr->e_int) {
  /* 7-bit immediate value. */
  asm_putb(0x83);
  n = 1;
 } else {
  /* 16/32-bit immediate value. */
  asm_putb(0x81);
 }
 gen_modrm(bin_op->bo_im_grp,offset,dst&7);
      if (n == 4) DCCTarget_TextExpr32(symaddr);
 else if (n == 2) DCCTarget_TextExpr16(symaddr);
 else             DCCTarget_TextExpr8(symaddr);
}
PUBLIC void
DCCGen_CstBinSym(tok_t op, struct DCCSymExpr const *__restrict symaddr,
                 struct DCCSymExpr const *__restrict target, size_t n, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  {
   rc_t temp;
  case '*':
   temp = DCCVStack_GetReg(n == 4 ? DCC_RC_I32 : DCC_RC_I16,n != 1);
   assertf(n != 1 || temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   if (n == 1) {
    DCCGen_BinReg('^',temp,temp,1);
    DCCGen_SymMov(target,temp&~(DCC_RC_I16));
    DCCGen_CstBin(op,symaddr,temp,src_unsigned);
    temp &= ~(DCC_RC_I16);
   } else {
    /* imulw/l $imm8/16/32, offset(%dst), %temp */
    if (n == 2) asm_putb(0x66);
    if (!symaddr->e_sym && symaddr->e_int == (int8_t)symaddr->e_int) asm_putb(0x6b),n = 1;
    else                                                             asm_putb(0x69);
    asm_putb(MODRM_DISP32(temp&7));
    DCCTarget_TextExpr32(target);
         if (n == 4) DCCTarget_TextExpr32(symaddr);
    else if (n == 2) DCCTarget_TextExpr16(symaddr);
    else             DCCTarget_TextExpr8(symaddr);
   }
   /* Store the multiplication result. */
   DCCGen_MovSym(temp,target);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Allocate a temporary register for the source constant.
    * NOTE: Make sure not to allocate the the fixed
    *       registers that will be required for the opcode.
    */
   temp = DCCVStack_GetRegOf(DCC_RC_FORSIZE(n),(uint8_t)
                           ~((1 << DCC_ASMREG_EAX)|
                             (1 << DCC_ASMREG_EDX)));
   DCCGen_CstMov(symaddr,temp);
   DCCGen_BinSym(op,temp,target,src_unsigned);
   break;
  }

  { /* Shift operations. */
  case TOK_SHL:
  case TOK_SHR:
  case TOK_LANGLE3:
  case TOK_RANGLE3:
   /* Generate the shift instruction. */
   if (n == 2) asm_putb(0x66);
   if (!symaddr->e_sym && symaddr->e_int == 1) {
    /* Shift by 1. */
    asm_putb(0xd0+(n >= 2));
    asm_putb(MODRM_DISP32(get_shift_group(op)));
    DCCTarget_TextExpr32(target);
   } else {
    asm_putb(0xc0+(n >= 2));
    asm_putb(MODRM_DISP32(get_shift_group(op)));
    DCCTarget_TextExpr32(target);
    DCCTarget_TextExpr8(symaddr);
   }
  } break;

  { /* test */
  case 't': /* TODO */;
  } break;

  default: break;
  }
  return;
 }
 /* *op symaddr, target */
 if (n == 2) asm_putb(0x66);
 if (n == 1) {
  /* 8-bit immediate value. */
  asm_putb(0x80);
 } else if (!symaddr->e_sym && symaddr->e_int == (int8_t)symaddr->e_int) {
  /* 7-bit immediate value. */
  asm_putb(0x83);
  n = 1;
 } else {
  /* 16/32-bit immediate value. */
  asm_putb(0x81);
 }
 asm_putb(MODRM_DISP32(bin_op->bo_im_grp));
 DCCTarget_TextExpr32(target);
      if (n == 4) DCCTarget_TextExpr32(symaddr);
 else if (n == 2) DCCTarget_TextExpr16(symaddr);
 else             DCCTarget_TextExpr8(symaddr);
}

DCC_DECL_END

#endif /* !GUARD_DCC_GEN_BINARY_C_INL */
