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
#ifndef GUARD_DCC_GEN_BIN_C_INL
#define GUARD_DCC_GEN_BIN_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#include <string.h>

#include "x86_util.h"

DCC_DECL_BEGIN

PUBLIC void
DCCDisp_MemsBinReg(tok_t op, struct DCCMemLoc const *__restrict src,
                   target_siz_t src_bytes, rc_t dst, int src_unsigned) {
 rc_t temp;
 target_siz_t dst_size;
 if (op == '=') { DCCDisp_MemsMovReg(src,src_bytes,dst,src_unsigned); return; }
 dst_size = DCC_RC_SIZE(dst);
 if (src_bytes >= dst_size) {
  if (op == '?' && src_bytes != dst_size) {
   /* TODO: We need to know if 'dst' is signed or unsigned! */
  }
  DCCDisp_MemBinReg(op,src,dst,src_unsigned);
  return;
 }
 temp = DCCVStack_GetReg(DCC_RC_FORSIZE(src_bytes),1);
 DCCDisp_MemMovReg(src,temp);
 DCCDisp_RegBinReg(op,temp,dst,src_unsigned);
}
PUBLIC void
DCCDisp_RegBinMems(tok_t op, rc_t src, struct DCCMemLoc const *__restrict dst,
                   target_siz_t dst_bytes, int src_unsigned) {
 struct DCCMemLoc newdst;
 target_siz_t src_siz;
 if (op == '=') { DCCDisp_RegMovMems(src,dst,dst_bytes,src_unsigned); return; }
 src_siz = DCC_RC_SIZE(src); assert(dst);
 if (src_siz >= dst_bytes) {
  assert(dst_bytes <= DCC_TARGET_SIZEOF_IMM_MAX);
  switch (dst_bytes) {
  case 0: return;
  case 1: src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I8); break;
  case 2: src &= ~(DCC_RC_I3264); break;
  {
   struct DCCSymAddr cst;
  case 3:
   DCCDisp_RegBinMem(op,src&~(DCC_RC_I32),dst,1);
   cst.sa_off = 16;
   cst.sa_sym = NULL;
   DCCDisp_CstBinReg(TOK_SHR,&cst,src,1);
   src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I8);
   assert(!(src&(DCC_RC_I3264|DCC_RC_I16)));
   newdst         = *dst;
   newdst.ml_off += 2;
   DCCDisp_RegBinMem(op,src,&newdst,src_unsigned);
   return;
  } break;
  default: break;
  }
  DCCDisp_RegBinMem(op,src,dst,src_unsigned);
  return;
 }
 DCCDisp_RegBinMem(op,src,dst,src_unsigned);
 newdst         = *dst;
 newdst.ml_off += src_siz;
 dst_bytes     -= src_siz;
 if (src_unsigned) {
  DCCDisp_BytBinMem(op,0,dst_bytes,&newdst,dst_bytes,1);
 } else {
  DCCDisp_SignMirrorReg(src);
  DCCDisp_ByrBinMem(op,src,dst_bytes,&newdst,dst_bytes,1);
 }
}

PUBLIC void
DCCDisp_MemsBinRegs(tok_t op, struct DCCMemLoc const *__restrict src,
                    target_siz_t src_bytes, rc_t dst, rc_t dst2, int src_unsigned) {
 struct DCCMemLoc new_src;
 if (!DCC_RC_ISCONST(dst2) && IS_LARGE_OP(op)) {
  DCCDisp_LargeMemsBinRegs(op,src,src_bytes,dst,dst2,src_unsigned);
  return;
 }
 if (!DCC_RC_ISCONST(dst2)) {
  target_siz_t s = DCC_RC_SIZE(dst);
  struct DCCSym *jsym = NULL;
  if (s > src_bytes) s = src_bytes;
  new_src = *src;
  if (src->ml_reg != DCC_RC_CONST &&
     (src->ml_reg&DCC_RI_MASK) == (dst&DCC_RI_MASK)) {
   /* Special case: The source value is offset from a register that overlaps with 'dst'.
    * Based on the opcode, we must either update 'src' to use an additional register,
    * or simply invert the register operation order. */
   if (op == '=' || op == '|' || op == '&' || op == '^') {
    /* We can invert the operation order for these operands. */
    DCCDisp_MemsBinReg(op,src,s,dst,src_unsigned);
    new_src.ml_off += s;
    if (src_bytes == s && !src_unsigned) {
     /* Must sign-extend the source location. */
     rc_t temp = DCCVStack_GetRegOf(dst2&DCC_RC_MASK,(uint8_t)~(
                                   (DCC_RC_ISCONST(src->ml_reg) ? 0 : (1 << (src->ml_reg&DCC_RI_MASK)))|
                                   (1 << (dst&DCC_RI_MASK))|
                                   (1 << (dst2&DCC_RI_MASK))));
     --new_src.ml_off;
     DCCDisp_MemMovReg(&new_src,temp);
     DCCDisp_SignMirrorReg(temp);
     DCCDisp_RegBinReg(op,temp,dst2,0);
    } else {
     DCCDisp_MemsBinReg(op,&new_src,src_bytes-s,dst2,src_unsigned);
    }
    return;
   }
   /* Allocate an additional register to use instead of that from 'src'. */
   new_src.ml_reg = DCCVStack_GetRegOf(src->ml_reg&DCC_RC_MASK,(uint8_t)~(
                                      (1 << (dst&DCC_RI_MASK))|
                                      (1 << (dst2&DCC_RI_MASK))));
   DCCDisp_RegMovReg(src->ml_reg,new_src.ml_reg,1);
  }
  DCCDisp_MemsBinReg(op,&new_src,s,dst,src_unsigned);
  if (op == '?' &&
     (jsym = DCCUnit_AllocSym()) != NULL)
      DCCDisp_SymJcc(DCC_TEST_NE,jsym);
  new_src.ml_off += s;
  src_bytes -= s;
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
  if (!src_bytes) {
   rc_t temp = DCCVStack_GetRegOf(dst2&DCC_RC_MASK,(uint8_t)~(
                                 (DCC_RC_ISCONST(src->ml_reg) ? 0 : (1 << (src->ml_reg&DCC_RI_MASK)))|
                                 (1 << (dst&DCC_RI_MASK))|
                                 (1 << (dst2&DCC_RI_MASK))));
   --new_src.ml_off;
   DCCDisp_MemMovReg(&new_src,temp);
   DCCDisp_SignMirrorReg(temp);
   DCCDisp_RegBinReg(op,temp,dst2,0);
  } else {
   DCCDisp_MemsBinReg(op,&new_src,src_bytes,dst2,src_unsigned);
  }
  if (jsym) t_defsym(jsym);
 } else {
  DCCDisp_MemsBinReg(op,src,src_bytes,dst,src_unsigned);
 }
}
PUBLIC void
DCCDisp_RegsBinMems(tok_t op, rc_t src, rc_t src2,
                    struct DCCMemLoc const *__restrict dst,
                    target_siz_t dst_bytes, int src_unsigned) {
 struct DCCMemLoc dst2;
 if unlikely(!dst_bytes) return;
 if (dst_bytes > DCC_TARGET_SIZEOF_ARITH_MAX && IS_LARGE_OP(op)) {
  DCCDisp_LargeRegsBinMems(op,src,src2,dst,dst_bytes,src_unsigned);
  return;
 }
#ifdef DCC_RC_I64
 if (dst_bytes < 8) src &= ~(DCC_RC_I64);
#endif
 if (dst_bytes < 4) src &= ~(DCC_RC_I3264);
 if (dst_bytes < 2) src &= ~(DCC_RC_I16);
 DCCDisp_RegBinMem(op,src,dst,src_unsigned);
 dst2 = *dst;
#ifdef DCC_RC_I64
      if (src&DCC_RC_I64) dst_bytes -= 8,dst2.ml_off += 8;
 else
#endif
      if (src&DCC_RC_I32) dst_bytes -= 4,dst2.ml_off += 4;
 else if (src&DCC_RC_I16) dst_bytes -= 2,dst2.ml_off += 2;
 else                     dst_bytes -= 1,dst2.ml_off += 1;
 if (dst_bytes) {
  struct DCCSym *jsym = NULL;
  /* Fill additional memory. */
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
  else if (op == '?') {
   /* Special handling for compare operations. */
   if ((jsym = DCCUnit_AllocSym()) != NULL)
        DCCDisp_SymJcc(DCC_TEST_NE,jsym);
  }
  if (!DCC_RC_ISCONST(src2)) {
#ifdef DCC_RC_I64
   if (dst_bytes < 8) src2 &= ~(DCC_RC_I64);
#endif
   if (dst_bytes < 4) src2 &= ~(DCC_RC_I3264);
   if (dst_bytes < 2) src2 &= ~(DCC_RC_I16);
   DCCDisp_RegBinMem(op,src2,&dst2,src_unsigned);
#ifdef DCC_RC_I64
        if (src2&DCC_RC_I64) dst_bytes -= 8,dst2.ml_off += 8;
   else
#endif
        if (src2&DCC_RC_I32) dst_bytes -= 4,dst2.ml_off += 4;
   else if (src2&DCC_RC_I16) dst_bytes -= 2,dst2.ml_off += 2;
   else                      dst_bytes -= 1,dst2.ml_off += 1;
  }
  if (dst_bytes) {
   if (src_unsigned) {
    DCCDisp_BytBinMem(op,0,dst_bytes,&dst2,
                      dst_bytes,src_unsigned);
   } else {
    DCCDisp_SignMirrorReg(src);
    DCCDisp_ByrBinMem(op,src,dst_bytes,&dst2,dst_bytes,0);
   }
  }
  if (jsym) t_defsym(jsym);
 }
}

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


PUBLIC void
DCCDisp_MemBinReg(tok_t op, struct DCCMemLoc const *__restrict src,
                  rc_t dst, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  case '=': DCCDisp_MemMovReg(src,dst); break;
  {
  case '*':
   if (dst&(DCC_RC_I32|DCC_RC_I16)) {
    /* 16/32-bit mul. */
    DCCDisp_X86Segp(src->ml_reg);
    if (!(dst&DCC_RC_I32)) t_putb(0x66);
    t_putb(0x0f);
    t_putb(0xaf);
    goto modrm;
   }
   /* 8-bit mul. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCDisp_RegMovReg(dst,temp,1);
   /* imul offset(%src), %temp */
   DCCDisp_X86Segp(src->ml_reg);
   t_putb(0x0f);
   t_putb(0xaf);
   asm_modmem(temp&DCC_RI_MASK,src);
   /* Store the multiplication result. */
   DCCDisp_RegMovReg(temp,dst,1);
  } break;

  { /* Divide/Modulo */
   rc_t eax,edx;
  case '/':
  case '%':
   eax = DCC_ASMREG_EAX|(dst&DCC_RC_MASK);
   edx = DCC_ASMREG_EDX|(dst&DCC_RC_MASK);
   if (src->ml_reg != DCC_RC_CONST &&
      (src->ml_reg&DCC_RI_MASK) == DCC_ASMREG_EAX ||
      (src->ml_reg&DCC_RI_MASK) == DCC_ASMREG_EDX) {
    /* We can either use ECX or EBX as fallback for src.
     * NOTE: Though we have to make sure not to use that of 'dst'*/
    rc_t temp;
    if ((dst&DCC_RI_MASK) != DCC_ASMREG_ECX &&
        (temp = (dst&DCC_RC_MASK)|DCC_ASMREG_ECX,
        !DCCVStack_GetRegInuse(temp))); /* ECX */
    else if ((dst&DCC_RI_MASK) != DCC_ASMREG_EBX &&
             (temp = (dst&DCC_RC_MASK)|DCC_ASMREG_EBX,
             !DCCVStack_GetRegInuse(temp))); /* EBX */
    else if ((dst&DCC_RI_MASK) != DCC_ASMREG_ECX) {
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
    DCCDisp_MemMovReg(src,temp);
    DCCDisp_RegBinReg(op,temp,dst,1);
    return;
   }
   /* Allocate EAX and EDX. */
   if ((dst&DCC_RI_MASK) != DCC_ASMREG_EAX) eax = DCCVStack_GetRegExact(eax);
   if ((dst&DCC_RI_MASK) != DCC_ASMREG_EDX) edx = DCCVStack_GetRegExact(edx);
   /* Move dst into eax. */
   DCCDisp_RegMovReg(dst,eax,1);
   DCCDisp_IntMovReg(0,edx);
   /* Generate the div/idiv instruction. */
   DCCDisp_X86Segp(src->ml_reg);
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0xf6+!!(dst&DCC_RC_I16));
   asm_modmem(src_unsigned ? 6 : 7,src);
   /* Either move the Quotient, or the Remained back into dst */
   DCCDisp_RegMovReg(op == '/' ? eax : edx,dst,1);
  } break;

  { /* Shift operations. */
   rc_t temp;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_RANGLE3:
   if ((dst&DCC_RI_MASK) == DCC_ASMREG_CL) {
    /* Special handling when the destination operand is %CL. */
    temp = DCCVStack_GetRegOf(dst&DCC_RC_MASK,(uint8_t)~(1 << DCC_ASMREG_CL));
    DCCDisp_RegMovReg(dst,temp,1);
    DCCDisp_MemMovReg(src,DCC_ASMREG_CL|DCC_RC_I8);
    DCCDisp_RegBinReg(op,DCC_ASMREG_CL|DCC_RC_I8,temp,src_unsigned);
    DCCDisp_RegMovReg(temp,dst,1);
   } else {
    temp = DCCVStack_GetRegExact(DCC_ASMREG_CL|DCC_RC_I8);
    DCCDisp_MemMovReg(src,temp);
    DCCDisp_RegBinReg(op,temp,dst,src_unsigned);
   }
  } break;

  { /* test */
  case 't':
   DCCDisp_X86Segp(src->ml_reg);
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0x84+!!(dst&DCC_RC_I16));
   asm_modmem(dst&DCC_RI_MASK,src);
  } break;

  default: break;
  }
  return;
 }
 /* *op !src, %dst */
 DCCDisp_X86Segp(src->ml_reg);
 if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
 t_putb(bin_op->bo_r_rm8+2+!!(dst&DCC_RC_I16));
modrm:
 asm_modmem(dst&DCC_RI_MASK,src);
}


PRIVATE uint8_t get_shift_group(tok_t op) {
 switch (op) {
 case TOK_SHR:     return 5; /* shr */
 case TOK_RANGLE3: return 7; /* sar */
 default:          return 4; /* shl */
 }
}

PUBLIC void
DCCDisp_RegBinMem(tok_t op, rc_t src, struct DCCMemLoc const *__restrict dst, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  case '=': DCCDisp_RegMovMem(src,dst); break;
  {
   uint8_t skip_set;
  case '*':
   skip_set = (uint8_t)(1 << (src&DCC_RI_MASK));
   if (dst->ml_reg != DCC_RC_CONST)
       skip_set |= (uint8_t)(1 << (dst->ml_reg&DCC_RI_MASK));
   temp = DCCVStack_GetRegOf(src&DCC_RC_MASK,(uint8_t)~skip_set);
   DCCDisp_MemMovReg(dst,temp);
   DCCDisp_RegBinReg(op,src,temp,src_unsigned);
   DCCDisp_RegMovMem(temp,dst);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Prefer storing the 'dst' in the correct output register. */
   temp = op == '/' ? DCC_ASMREG_EAX : DCC_ASMREG_EDX;
   if ((src&DCC_RI_MASK) == temp ||
      (dst->ml_reg != DCC_RC_CONST &&
      (dst->ml_reg&DCC_RI_MASK) == temp)) {
    /* Either 'src', or 'dst' are already using the true output register.
     * So instead, we must allocate a new register of the same class.
     * WARNING: Having to do this may result in less optimal code. */
    temp = DCCVStack_GetReg(src&DCC_RC_MASK,1);
   } else {
    temp |= (src&DCC_RC_MASK);
    DCCVStack_GetRegExact(temp);
   }
   DCCDisp_MemMovReg(dst,temp);
   DCCDisp_RegBinReg(op,src,temp,src_unsigned);
   DCCDisp_RegMovMem(temp,dst);
  } break;

  { /* Shift operations. */
   rc_t cl; struct DCCMemLoc used_dst;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_RANGLE3:
   cl = DCC_ASMREG_CL|DCC_RC_I8;
   used_dst = *dst;
   if ((used_dst.ml_reg != DCC_RC_CONST) &&
       (used_dst.ml_reg&DCC_RI_MASK) == DCC_ASMREG_CL) {
    used_dst.ml_reg  =  DCCVStack_GetReg(DCC_RC_PTR,1);
    used_dst.ml_reg |= (dst->ml_reg&DCC_RC_MASK_86SEGP);
    DCCDisp_RegMovReg(dst->ml_reg,used_dst.ml_reg,1);
   }
   if ((src&DCC_RI_MASK) != DCC_ASMREG_CL) {
    cl = DCCVStack_GetRegExact(cl);
    DCCDisp_RegMovReg(src,cl,src_unsigned);
   }
   /* Generate the shift instruction. */
   DCCDisp_X86Segp(used_dst.ml_reg);
   if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0xd2+!!(src&DCC_RC_I16));
   asm_modmem(get_shift_group(op),&used_dst);
  } break;

  { /* test */
  case 't':
   DCCDisp_MemBinReg('t',dst,src,1);
  } break;

  default: break;
  }
  return;
 }
 /* *op %src, !dst */
 DCCDisp_X86Segp(dst->ml_reg);
 if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
 t_putb(bin_op->bo_r_rm8+!!(src&DCC_RC_I16));
 asm_modmem(src&DCC_RI_MASK,dst);
}
PUBLIC void
DCCDisp_RegBinReg(tok_t op, rc_t src, rc_t dst, int src_unsigned) {
 rc_t c_src,c_dst;
 struct binary_operator const *bin_op;
 /* Special optimizations for binary operation on the same register. */
 if (src == dst) {
  switch (op) {
  case '=': return;
   /* Special case: 'add %reg, %reg' --> 'shl $1, %reg' */
  case '+':
   if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0xd0+!!(dst&DCC_RC_I16));
   asm_modreg(4,dst&DCC_RI_MASK);
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
  case '/':
  case '%':
   /* Special case: %reg / %reg --> %reg = 1. */
   /* Special case: %reg % %reg --> %reg = 0. */
   DCCDisp_IntMovReg(op == '/' ? 1 : 0,dst);
   return;
#endif

  default: break;
  }
 }
 c_src = src&DCC_RC_MASK;
 c_dst = dst&DCC_RC_MASK;
 bin_op = bin_ops;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  /* Special opcodes: '*', '/', '%', TOK_SHL, TOK_SHR, TOK_RANGLE3 */
  switch (op) {
  case '=': DCCDisp_RegMovReg(src,dst,src_unsigned); break;

  {
  case '*':
   src = DCCVStack_CastReg(src,src_unsigned,c_dst);
   if (!(c_dst&(DCC_RC_I16|DCC_RC_I3264))) {
    rc_t temp_dst; /* 8-bit dst */
    temp_dst = DCCVStack_GetReg(DCC_RC_I16,1);
    DCCDisp_RegMovReg(dst,temp_dst,1);
    /* imulw %src, %temp_dst */
    t_putb(0x66);
    t_putb(0x0f);
    t_putb(0xaf);
    /* NOTE: Operands must be written reverse of the usual order. */
    asm_modreg(temp_dst&DCC_RI_MASK,src&DCC_RI_MASK);
    DCCDisp_RegMovReg(temp_dst,dst,1);
   } else {
    /* imulw/l %src, %dst */
    if (!(c_dst&DCC_RC_I3264)) t_putb(0x66);
    t_putb(0x0f);
    t_putb(0xaf);
    /* NOTE: Operands must be written reverse of the usual order. */
    asm_modreg(dst&DCC_RI_MASK,src&DCC_RI_MASK);
   }
  } break;

  { /* Divide/Modulo */
   rc_t eax,edx,original_src;
  case '%':
  case '/':
   src = DCCVStack_CastReg(src,src_unsigned,c_dst);
   /* Reserve AX & DX for the same storage class as 'src'. */
   eax = DCC_ASMREG_EAX|(src&DCC_RC_MASK);
   edx = DCC_ASMREG_EDX|(src&DCC_RC_MASK);
   original_src = src;
   /* If 'src' is either 'EAX' or 'EDX', it needs to be stored elsewhere! */
   if ((src&DCC_RI_MASK) == DCC_ASMREG_EAX ||
       (src&DCC_RI_MASK) == DCC_ASMREG_EDX) {
    /* We can either use ECX or EBX as fallback for src.
     * NOTE: Though we have to make sure not to use that of 'dst'*/
    rc_t temp;
    if ((dst&DCC_RI_MASK) != DCC_ASMREG_ECX &&
        (temp = (src&DCC_RC_MASK)|DCC_ASMREG_ECX,
        !DCCVStack_GetRegInuse(temp))); /* ECX */
    else if ((dst&DCC_RI_MASK) != DCC_ASMREG_EBX &&
             (temp = (src&DCC_RC_MASK)|DCC_ASMREG_EBX,
             !DCCVStack_GetRegInuse(temp))); /* EBX */
    else if ((dst&DCC_RI_MASK) != DCC_ASMREG_ECX) {
     /* ECX (w/kill) */
     temp = DCCVStack_GetRegExact(DCC_ASMREG_ECX|(src&DCC_RC_MASK));
    } else {
     /* EBX (w/kill) */
     temp = DCCVStack_GetRegExact(DCC_ASMREG_EBX|(src&DCC_RC_MASK));
    }
    assert((src&DCC_RC_MASK) == (temp&DCC_RC_MASK));
    /* Move the source into our fallback register. */
    DCCDisp_RegMovReg(src,temp,1);
    src = temp;
   }
   /* Sanity check: 'src' must not be either of these! */
   assert((src&DCC_RI_MASK) != DCC_ASMREG_EAX);
   assert((src&DCC_RI_MASK) != DCC_ASMREG_EDX);
   /* No need to allocate these registers if they've already been. */
   if ((original_src&DCC_RI_MASK) != DCC_ASMREG_EAX &&
       (dst&DCC_RI_MASK) != DCC_ASMREG_EAX) eax = DCCVStack_GetRegExact(eax);
   if ((original_src&DCC_RI_MASK) != DCC_ASMREG_EDX &&
       (dst&DCC_RI_MASK) != DCC_ASMREG_EDX) edx = DCCVStack_GetRegExact(edx);
   /* Setup the register to where 'dst' resides in EAX and EDX is filled with ZERO(0). */
   DCCDisp_RegMovReg(dst,eax,1);
   DCCDisp_IntMovReg(0,edx);
   /* Generate the div/idiv instruction. */
   if ((src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0xf6+!!(src&DCC_RC_I16));
   asm_modreg(src_unsigned ? 6 : 7,src&DCC_RI_MASK);
   /* Either move the Quotient, or the Remained back into dst */
   DCCDisp_RegMovReg(op == '/' ? eax : edx,dst,1);
  } break;

  { /* Shift operations. */
   rc_t cl,used_dst;
  case TOK_SHL:
  case TOK_SHR:
  case TOK_RANGLE3:
   used_dst = dst;
   if ((dst&DCC_RI_MASK) == DCC_ASMREG_CL) {
    /* The destination register overlaps with CL.
     * With that in mind, we need to move it elsewhere. */
    used_dst = DCCVStack_GetRegOf(dst&DCC_RC_MASK,
                                ~((1 << DCC_ASMREG_CL)|
                                  (1 << (src&DCC_RI_MASK))));
    DCCDisp_RegMovReg(dst,used_dst,1);
   }
   cl = DCC_ASMREG_CL|DCC_RC_I8;
   if ((src&DCC_RI_MASK) != DCC_ASMREG_CL) {
    cl = DCCVStack_GetRegExact(cl);
    DCCDisp_RegMovReg(src,cl,src_unsigned);
   }
   /* Generate the shift instruction. */
   if ((used_dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0xd2+!!(used_dst&DCC_RC_I16));
   asm_modreg(get_shift_group(op),used_dst&DCC_RI_MASK);
   DCCDisp_RegMovReg(used_dst,dst,1);
  } break;

  { /* test */
  case 't':
   /* Cast 'src' to the same length as 'dst'. */
   src = DCCVStack_CastReg(src,src_unsigned,c_dst);
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   t_putb(0x84+!!(dst&DCC_RC_I16));
   asm_modreg(src&DCC_RI_MASK,dst&DCC_RI_MASK);
  } break;

  default: break;
  }
  return;
 }
 if (c_src == c_dst) {
  /* operation in the same storage class. */
  if ((c_src&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
  t_putb(bin_op->bo_r_rm8+!!(c_src&DCC_RC_I16));
  goto modreg;
 }
 if (c_src > c_dst) {
  /* Truncate: Simply copy from a lower order register,
   *           but apply a mask for EDI/ESI. */
  if (c_dst&(DCC_RC_I16|DCC_RC_I3264)) {
   /* Destination class isn't 8-bit, meaning a lower-order register is always available. */
   if (!(c_dst&DCC_RC_I3264)) t_putb(0x66);
   t_putb(bin_op->bo_r_rm8+1);
  } else if (!(src&4)) {
   /* 8-bit mode, but the source register has an 8-bit equivalent. */
   t_putb(bin_op->bo_r_rm8);
  } else {
   src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I16|DCC_RC_I8);
   /* *op %src, %dst */
   t_putb(bin_op->bo_r_rm8);
   goto modreg;
  }
 } else {
  /* At this point, we know that src is either 16, or 8-bit, and that c_dst is 16/32-bit.
   * >> In both cases, we must use a temporary register to extend 'src' to 16/32-bit. */
  src = DCCVStack_CastReg(src,src_unsigned,c_dst);
  /* *op %used_src, %dst */
  if (!(c_dst&DCC_RC_I32)) t_putb(0x66);
  t_putb(bin_op->bo_r_rm8+1);
  goto modreg;
 }
modreg:
 asm_modreg(src&DCC_RI_MASK,dst&DCC_RI_MASK);
}

#if DCC_TARGET_SIZEOF_ARITH_MAX < 8
PUBLIC void
DCCDisp_CstBinRegs(tok_t op, struct DCCSymExpr const *__restrict val,
                   rc_t dst, rc_t dst2, int src_unsigned) {
 struct DCCSymAddr aval;
 if (dst2 != DCC_RC_CONST && IS_LARGE_OP(op)) {
  DCCDisp_LargeCstBinRegs(op,val,dst,dst2,src_unsigned);
  return;
 }
 if (op == '?' && dst2 != DCC_RC_CONST) {
  /* Compare constant with 64-bit integer. */
  struct DCCMemLoc jmp;
  jmp.ml_sym = DCCUnit_AllocSym();
  if unlikely(!jmp.ml_sym) return;
  jmp.ml_reg = DCC_RC_CONST;
  jmp.ml_off = 0;
  aval.sa_off = (target_off_t)val->e_int;
  aval.sa_sym = val->e_sym;
  DCCDisp_CstBinReg('?',&aval,dst,src_unsigned);
  DCCDisp_LocJcc(DCC_TEST_NE,&jmp);
  aval.sa_off = (target_off_t)(val->e_int >> 32);
  aval.sa_sym = NULL;
  DCCDisp_CstBinReg('?',&aval,dst2,src_unsigned);
  t_defsym(jmp.ml_sym);
 } else {
  aval.sa_off = (target_off_t)val->e_int;
  aval.sa_sym = val->e_sym;
  DCCDisp_CstBinReg(op,&aval,dst,src_unsigned);
  if (dst2 != DCC_RC_CONST) {
        if (op == '+') op = TOK_INC;
   else if (op == '-') op = TOK_DEC;
   aval.sa_off = (target_off_t)(val->e_int >> 32);
   aval.sa_sym = NULL;
   DCCDisp_CstBinReg(op,&aval,dst2,src_unsigned);
  }
 }
}
#endif
PUBLIC void
DCCDisp_RegsBinRegs(tok_t op, rc_t src, rc_t src2,
                    rc_t dst, rc_t dst2, int src_unsigned) {
 if (dst2 != DCC_RC_CONST && IS_LARGE_OP(op)) {
  DCCDisp_LargeRegsBinRegs(op,src,src2,dst,dst2,src_unsigned);
  return;
 }
 if (op == '=' && dst2 != DCC_RC_CONST && src2 != DCC_RC_CONST) {
  if (src == dst) { DCCDisp_RegMovReg(src2,dst2,src_unsigned); return; }
  if (src2 == dst2) { DCCDisp_RegMovReg(src,dst,src_unsigned); return; }
  /* Special handling for criss-cross assignment. */
  if ((src&DCC_RI_MASK) == (dst2&DCC_RI_MASK) &&
      (src2&DCC_RI_MASK) == (dst&DCC_RI_MASK)) {
   rc_t common_rc = (src&DCC_RC_MASK)|(src2&DCC_RC_MASK);
   src  = DCCVStack_CastReg(src,src_unsigned,common_rc);
   src2 = DCCVStack_CastReg(src2,src_unsigned,common_rc);
   assert((src&DCC_RC_MASK)  == common_rc);
   assert((src2&DCC_RC_MASK) == common_rc);
   DCCDisp_RegXchReg(src,src2);
   DCCDisp_RegMovReg(src2,dst,src_unsigned);
   DCCDisp_RegMovReg(src,dst2,src_unsigned);
   return;
  }
 }
 DCCDisp_RegBinReg(op,src,dst,src_unsigned);
 if (dst2 != DCC_RC_CONST) {
  struct DCCSym *jsym = NULL;
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
  else if (op == '?') {
   /* Special handling for compare operations. */
   if ((jsym = DCCUnit_AllocSym()) != NULL)
        DCCDisp_SymJcc(DCC_TEST_NE,jsym);
  }
  if (src2 != DCC_RC_CONST) {
   DCCDisp_RegBinReg(op,src2,dst2,src_unsigned);
  } else {
   struct DCCSymAddr val = {0,NULL};
   DCCDisp_CstBinReg(op,&val,dst2,src_unsigned);
  }
  if (jsym) t_defsym(jsym);
 }
}

PUBLIC void
DCCDisp_CstBinReg(tok_t op, struct DCCSymAddr const *__restrict val,
                  rc_t dst, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops; rc_t temp;
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  case '=': DCCDisp_CstMovReg(val,dst); break;
  {
  case '*':
   if (dst&(DCC_RC_I32|DCC_RC_I16)) {
    DCCDisp_ProbeSym(val,1);
    /* 16/32-bit mul. */
    if (!(dst&DCC_RC_I32)) t_putb(0x66);
    if (!val->sa_sym && val->sa_off == (int8_t)val->sa_off) {
     /* imul $imm8, %r16/32 */
     t_putb(0x6b);
     dst &= ~(DCC_RC_I3264|DCC_RC_I16);
    } else {
     /* imul $imm16/32, %r16/32 */
     t_putb(0x69);
    }
    asm_modreg(dst&DCC_RI_MASK,dst&DCC_RI_MASK);
    goto put_expr;
   }
   /* 8-bit mul w/ immediate value. */
   temp = DCCVStack_GetReg(DCC_RC_I16,0);
   assertf(temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   DCCDisp_RegMovReg(dst,temp,1);
   /* imulw $symaddr, %temp */
   DCCDisp_CstBinReg('*',val,temp,src_unsigned);
   /* Store the multiplication result. */
   DCCDisp_RegMovReg(temp,dst,1);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Allocate a temporary register for the source constant.
    * NOTE: Make sure not to allocate the destination register,
    *       or the fixed registers that will be required for the opcode.
    */
#ifdef IA32_PROTECTED_REGISTERS
   if ((dst&DCC_RI_MASK) == DCC_ASMREG_ECX) {
    /* Since 'EBX' is protected, ECX is the only register we could use.
     * But as it turns out, that one's already in use by the destination.
     * >> Now we have to push/pop EBX... */
    temp = (dst&DCC_RC_MASK)|DCC_ASMREG_EBX;
    DCCDisp_RegPush(temp);
    DCCDisp_CstMovReg(val,temp);
    DCCDisp_RegBinReg(op,temp,dst,src_unsigned);
    DCCDisp_PopReg(temp);
   } else
#endif
   {
    temp = DCCVStack_GetRegOf(dst&DCC_RC_MASK,
                            ~((1 << DCC_ASMREG_EAX)|
                              (1 << DCC_ASMREG_EDX)|
                              (1 << (dst&DCC_RI_MASK))));
    DCCDisp_CstMovReg(val,temp);
    DCCDisp_RegBinReg(op,temp,dst,src_unsigned);
   }
   break;
  }

  { /* Shift operations. */
  case TOK_SHL:
  case TOK_SHR:
  case TOK_RANGLE3:
   DCCDisp_ProbeSym(val,1);
   /* Generate the shift instruction. */
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   if (!val->sa_sym && val->sa_off == 1) {
    /* Shift by 1. */
    t_putb(0xd0+!!(dst&DCC_RC_I16));
    asm_modreg(get_shift_group(op),dst&DCC_RI_MASK);
   } else {
    t_putb(0xc0+!!(dst&DCC_RC_I16));
    asm_modreg(get_shift_group(op),dst&DCC_RI_MASK);
    DCCDisp_SymAddr8(val);
   }
  } break;

  { /* test */
  case 't':
   DCCDisp_ProbeSym(val,1);
   if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
   if ((dst&DCC_RI_MASK) == DCC_ASMREG_EAX) {
    t_putb(0xa8+!!(dst&DCC_RC_I16));
   } else {
    t_putb(0xf6+!!(dst&DCC_RC_I16));
    asm_modreg(0,dst&DCC_RI_MASK);
   }
        if (dst&DCC_RC_I3264) DCCDisp_SymAddr32(val);
   else if (dst&DCC_RC_I16)   DCCDisp_SymAddr16(val);
   else                       DCCDisp_SymAddr8 (val);
  } break;

  default: break;
  }
  return;
 }
 /* *op symaddr, %reg */
 DCCDisp_ProbeSym(val,1);
 if ((dst&(DCC_RC_I16|DCC_RC_I3264)) == DCC_RC_I16) t_putb(0x66);
 if ((dst&DCC_RI_MASK) == DCC_ASMREG_EAX) {
  t_putb(bin_op->bo_r_rm8+4+!!(dst&DCC_RC_I16));
 } else {
  if (!(dst&(DCC_RC_I16|DCC_RC_I3264))) {
   t_putb(0x80); /* 8-bit immediate value. */
  } else if (!val->sa_sym && val->sa_off == (int8_t)val->sa_off) {
   /* 7-bit immediate value. */
   t_putb(0x83);
   dst &= ~(DCC_RC_I16|DCC_RC_I3264);
  } else {
   /* 16/32-bit immediate value. */
   t_putb(0x81);
  }
  asm_modreg(bin_op->bo_im_grp,dst&DCC_RI_MASK);
 }
put_expr:
#ifdef DCC_RC_I64
      if (dst&DCC_RC_I64) DCCDisp_SymAddr64(val);
 else
#endif
      if (dst&DCC_RC_I32) DCCDisp_SymAddr32(val);
 else if (dst&DCC_RC_I16) DCCDisp_SymAddr16(val);
 else                     DCCDisp_SymAddr8(val);
}

PUBLIC void
DCCDisp_CstBinMem(tok_t op,
                  struct DCCSymAddr const *__restrict val,
                  struct DCCMemLoc  const *__restrict dst,
                  width_t width, int src_unsigned) {
 struct binary_operator const *bin_op = bin_ops;
 assert(CHECK_WIDTH(width));
 while (bin_op->bo_tok && bin_op->bo_tok != op) ++bin_op;
 if (!bin_op->bo_tok) {
  switch (op) {
  case '=': DCCDisp_CstMovMem(val,dst,width); break;
  {
   rc_t temp;
  case '*':
   temp = DCCVStack_GetReg(width == 4 ? DCC_RC_I32 : DCC_RC_I16,width != 1);
   assertf(width != 1 || temp&DCC_RC_I8,"Must always be the case for non-pointer classes.");
   if (width == 1) {
    DCCDisp_IntMovReg(0,temp);
    DCCDisp_MemMovReg(dst,temp&~(DCC_RC_I16));
    DCCDisp_CstBinReg(op,val,temp,src_unsigned);
    temp &= ~(DCC_RC_I16);
   } else {
    /* imulw/l $imm8/16/32, offset(%dst), %temp */
    DCCDisp_ProbeSym(val,1);
    DCCDisp_X86Segp(dst->ml_reg);
    if (width == 2) t_putb(0x66);
    if (!val->sa_sym && val->sa_off == (int8_t)val->sa_off) t_putb(0x6b),width = 1;
    else                                                    t_putb(0x69);
    /* NOTE: Operands must be written reverse of the usual order. */
    asm_modmem(temp&DCC_RI_MASK,dst);
    DCCDisp_SymAddr(val,width);
   }
   /* Store the multiplication result. */
   DCCDisp_RegMovMem(temp,dst);
  } break;

  { /* Divide/Modulo */
   rc_t temp;
  case '/':
  case '%':
   /* Allocate a temporary register for the source constant.
    * NOTE: Make sure not to allocate the destination register,
    *       or the fixed registers that will be required for the opcode.
    */
#ifdef IA32_PROTECTED_REGISTERS
   if (dst->ml_reg != DCC_RC_CONST &&
      (dst->ml_reg&DCC_RI_MASK) == DCC_ASMREG_ECX) {
    /* Since 'EBX' is protected, ECX is the only register we could use.
     * But as it turns out, that one's already in use by the destination.
     * >> Now we have to push/pop EBX... */
    temp = DCC_RC_FORSIZE(width)|DCC_ASMREG_EBX;
    DCCDisp_RegPush(temp);
    DCCDisp_CstMovReg(val,temp);
    DCCDisp_RegBinMem(op,temp,dst,src_unsigned);
    DCCDisp_PopReg(temp);
   } else
#endif
   {
    temp = DCCVStack_GetRegOf(DCC_RC_FORSIZE(width),
                            ~((1 << DCC_ASMREG_EAX)|
                              (1 << DCC_ASMREG_EDX)|
                              (dst->ml_reg != DCC_RC_CONST ? (1 << (dst->ml_reg&7)) : 0)
                             ));
    DCCDisp_CstMovReg(val,temp);
    DCCDisp_RegBinMem(op,temp,dst,src_unsigned);
   }
   break;
  }

  { /* Shift operations. */
  case TOK_SHL:
  case TOK_SHR:
  case TOK_RANGLE3:
   /* Generate the shift instruction. */
   DCCDisp_ProbeSym(val,1);
   DCCDisp_X86Segp(dst->ml_reg);
   if (width == 2) t_putb(0x66);
   if (!val->sa_sym && val->sa_off == 1) {
    /* Shift by 1. */
    t_putb(0xd0+(width >= 2));
    asm_modmem(get_shift_group(op),dst);
   } else {
    t_putb(0xc0+(width >= 2));
    asm_modmem(get_shift_group(op),dst);
    DCCDisp_SymAddr8(val);
   }
  } break;

  { /* test */
  case 't':
   DCCDisp_ProbeSym(val,1);
   DCCDisp_X86Segp(dst->ml_reg);
   if (width == 2) t_putb(0x66);
   t_putb(0xf6+(width >= 2));
   asm_modmem(0,dst);
   DCCDisp_SymAddr(val,width);
  } break;

  default: break;
  }
  return;
 }
 /* *op symaddr, offset(%dst) */
 DCCDisp_ProbeSym(val,1);
 DCCDisp_X86Segp(dst->ml_reg);
 if (width == 2) t_putb(0x66);
 if (width == 1) {
  /* 8-bit immediate value. */
  t_putb(0x80);
 } else if (!val->sa_sym && val->sa_off == (int8_t)val->sa_off) {
  /* 7-bit immediate value. */
  t_putb(0x83);
  width = 1;
 } else {
  /* 16/32-bit immediate value. */
  t_putb(0x81);
 }
 asm_modmem(bin_op->bo_im_grp,dst);
 DCCDisp_SymAddr(val,width);
}


PRIVATE void
DCCDisp_StaBinStaWidth(tok_t op,
                       void const *__restrict src,
                       void       *__restrict dst,
                       int src_unsigned, width_t width,
                       target_ptr_t *eflags) {
 target_off_t vsrc = readw(src,width,src_unsigned);
 target_off_t vdst = readw(dst,width,1);
 target_ptr_t newflags = 0;
 switch (op) {
 case '+': case TOK_INC: newflags = (vdst+vsrc < vdst); vdst += (vsrc+*eflags); break;
 case '-': case TOK_DEC: newflags = (vdst-vsrc > vdst); vdst -= (vsrc+*eflags); break;
 case '&': vdst &= vsrc; break;
 case '|': vdst |= vsrc; break;
 case '^': vdst ^= vsrc; break;
 /* TODO: Special handling for these? */
 case '*': vdst *= vsrc; break;
 case '/': vdst /= vsrc; break;
 case '%': vdst %= vsrc; break;
 case TOK_SHL:     *(target_ptr_t *)&vdst <<= vsrc; break;
 case TOK_SHR:     *(target_ptr_t *)&vdst >>= vsrc; break;
 case TOK_RANGLE3:                   vdst >>= vsrc; break;
 default: break;
 }
 writew(dst,vdst,width);
 *eflags = newflags;
}
PRIVATE void
DCCDisp_BytBinStaWidth(tok_t op, target_off_t src,
                       void       *__restrict dst,
                       width_t width, unsigned int *eflags) {
 target_off_t vdst = readw(dst,width,1);
 target_ptr_t newflags = 0;
 switch (op) {
 case '+': case TOK_INC: newflags = (vdst+src < vdst); vdst += (src+*eflags); break;
 case '-': case TOK_DEC: newflags = (vdst-src > vdst); vdst -= (src+*eflags); break;
 case '&': vdst &= src; break;
 case '|': vdst |= src; break;
 case '^': vdst ^= src; break;
 /* TODO: Special handling for these? */
 case '*': vdst *= src; break;
 case '/': vdst /= src; break;
 case '%': vdst %= src; break;
 case TOK_SHL:     *(target_ptr_t *)&vdst <<= src; break;
 case TOK_SHR:     *(target_ptr_t *)&vdst >>= src; break;
 case TOK_RANGLE3:                   vdst >>= src; break;
 default: break;
 }
 writew(dst,vdst,width);
 *eflags = newflags;
}
PRIVATE void
DCCDisp_StaBinSta(tok_t op,
                  void const *__restrict src, target_siz_t src_bytes,
                  void       *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 target_ptr_t flags = 0;
 target_off_t filler;
 target_siz_t common_size,iter_size;
 iter_size = common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 while (iter_size >= DCC_TARGET_SIZEOF_ARITH_MAX) {
  DCCDisp_StaBinStaWidth(op,src,dst,src_unsigned,
                         DCC_TARGET_SIZEOF_ARITH_MAX,
                        &flags);
  *(uintptr_t *)&src += DCC_TARGET_SIZEOF_ARITH_MAX;
  *(uintptr_t *)&dst += DCC_TARGET_SIZEOF_ARITH_MAX;
  iter_size          -= DCC_TARGET_SIZEOF_ARITH_MAX;
 }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 if (iter_size&4) {
  DCCDisp_StaBinStaWidth(op,src,dst,src_unsigned,4,&flags);
  *(uintptr_t *)&src += 4;
  *(uintptr_t *)&dst += 4;
 }
#endif
 if (iter_size&2) {
  DCCDisp_StaBinStaWidth(op,src,dst,src_unsigned,2,&flags);
  *(uintptr_t *)&src += 2;
  *(uintptr_t *)&dst += 2;
 }
 if (iter_size&1) {
  DCCDisp_StaBinStaWidth(op,src,dst,src_unsigned,1,&flags);
  *(uintptr_t *)&src += 1;
  *(uintptr_t *)&dst += 1;
 }
 filler = 0;
 if (src_bytes && !src_unsigned && ((uint8_t *)src)[-1]&0x80) {
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
  filler = 0xffffffffffffffffll;
#else
  filler = 0xffffffffl;
#endif
 }
 assert(dst_bytes >= common_size);
 common_size = dst_bytes-common_size;
 while (common_size > DCC_TARGET_SIZEOF_ARITH_MAX) {
  DCCDisp_BytBinStaWidth(op,filler,dst,DCC_TARGET_SIZEOF_ARITH_MAX,&flags);
  *(uintptr_t *)&dst += DCC_TARGET_SIZEOF_ARITH_MAX;
  common_size        -= DCC_TARGET_SIZEOF_ARITH_MAX;
 }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 if (common_size&4) {
  DCCDisp_BytBinStaWidth(op,filler,dst,4,&flags);
  *(uintptr_t *)&src += 4;
  *(uintptr_t *)&dst += 4;
 }
#endif
 if (common_size&2) {
  DCCDisp_BytBinStaWidth(op,filler,dst,2,&flags);
  *(uintptr_t *)&src += 2;
  *(uintptr_t *)&dst += 2;
 }
 if (common_size&1) {
  DCCDisp_BytBinStaWidth(op,filler,dst,1,&flags);
 }
}
PRIVATE void
DCCDisp_BytBinSta(tok_t op, int          src, target_siz_t src_bytes,
                  void       *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 target_off_t extended_src;
 target_ptr_t flags = 0;
 target_off_t filler;
 target_siz_t common_size,iter_size;
 extended_src = (target_off_t)(src & 0xff);
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 extended_src *= 0x0101010101010101ll;
#else
 extended_src *= 0x01010101;
#endif

 iter_size = common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 while (iter_size >= DCC_TARGET_SIZEOF_ARITH_MAX) {
  DCCDisp_BytBinStaWidth(op,extended_src,dst,DCC_TARGET_SIZEOF_ARITH_MAX,&flags);
  *(uintptr_t *)&dst += DCC_TARGET_SIZEOF_ARITH_MAX;
  iter_size          -= DCC_TARGET_SIZEOF_ARITH_MAX;
 }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 if (iter_size&4) {
  DCCDisp_BytBinStaWidth(op,extended_src,dst,4,&flags);
  *(uintptr_t *)&dst += 4;
 }
#endif
 if (iter_size&2) {
  DCCDisp_BytBinStaWidth(op,extended_src,dst,2,&flags);
  *(uintptr_t *)&dst += 2;
 }
 if (iter_size&1) {
  DCCDisp_BytBinStaWidth(op,extended_src,dst,1,&flags);
  *(uintptr_t *)&dst += 1;
 }
 filler = 0;
 if (src_bytes && !src_unsigned && (src&0x80)) {
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
  filler = 0xffffffffffffffffll;
#else
  filler = 0xffffffff;
#endif
 }
 common_size = dst_bytes-common_size;
 while (common_size > DCC_TARGET_SIZEOF_ARITH_MAX) {
  DCCDisp_BytBinStaWidth(op,filler,dst,DCC_TARGET_SIZEOF_ARITH_MAX,&flags);
  *(uintptr_t *)&dst += DCC_TARGET_SIZEOF_ARITH_MAX;
 }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 if (common_size&4) {
  DCCDisp_BytBinStaWidth(op,filler,dst,4,&flags);
  *(uintptr_t *)&src += 4;
  *(uintptr_t *)&dst += 4;
 }
#endif
 if (common_size&2) {
  DCCDisp_BytBinStaWidth(op,filler,dst,2,&flags);
  *(uintptr_t *)&src += 2;
  *(uintptr_t *)&dst += 2;
 }
 if (common_size&1) {
  DCCDisp_BytBinStaWidth(op,filler,dst,1,&flags);
 }
}


PRIVATE void
DCCDisp_AXCmpDI(width_t max_width, target_siz_t n_bytes,
                struct DCCMemLoc const *__restrict nejmp) {
 assert(CHECK_WIDTH(max_width));
 if (n_bytes <= DCC_TARGET_SIZEOF_ARITH_MAX*2) {
  /* Special case for small scas. */
  if (max_width >= 4 && n_bytes >= 4) for (;;) {
   asm_op_scasl(); n_bytes -= 4;
   if (n_bytes) DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
   if (n_bytes < 4) break;
  }
  if (max_width >= 2 && n_bytes >= 2) for (;;) {
   asm_op_scasw(); n_bytes -= 2;
   if (n_bytes) DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
   if (n_bytes < 2) break;
  }
  if (n_bytes) for (;;) {
   asm_op_scasb();
   if (!n_bytes) break;
   DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  }
  return;
 }
 DCCVStack_GetRegExact(DCC_RR_XCX);
 if (n_bytes % max_width) {
  /* Special cases for situations in which the given byte count is unaligned! */
  if (!(n_bytes % 2) && n_bytes <= DCC_TARGET_SIZEOF_ARITH_MAX*8) {
use_scasw:
   /* Use 'repe stosw' */
   DCCDisp_IntMovReg((target_off_t)(n_bytes / 2),DCC_RR_XCX),n_bytes %= 2;
   asm_op_cld();
   asm_op_repe();
   asm_op_scasw();
  } else if (n_bytes <= DCC_TARGET_SIZEOF_ARITH_MAX*4) {
use_scasb:
   /* Use 'repe stosb' */
   DCCDisp_IntMovReg((target_off_t)n_bytes,DCC_RR_XCX),n_bytes = 0;
   asm_op_cld();
   asm_op_repe();
   asm_op_scasb();
  } else {
   goto use_scasauto;
  }
 } else {
use_scasauto:
  if (max_width == 1) goto use_scasb;
  if (max_width == 2) goto use_scasw;
  assert(max_width == 4);
  DCCDisp_IntMovReg((target_off_t)(n_bytes / 4),DCC_RR_XCX),n_bytes %= 4;
  asm_op_repe();
  asm_op_scasl();
 }
 if (n_bytes) {
  assert(n_bytes < DCC_TARGET_SIZEOF_ARITH_MAX);
  DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  /* Fix any overflow still remaining. */
  if (n_bytes >= 2 && max_width >= 2) {
   asm_op_scasw(),n_bytes -= 2;
   if (n_bytes) DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  }
  if (n_bytes) for (;;) {
   asm_op_scasb();
   if (!--n_bytes) break;
   DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  }
 }
}

PRIVATE void
DCCDisp_DoRepAXCmpMem_impl(width_t max_width,
                           struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                           struct DCCMemLoc const *__restrict nejmp) {
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_RegPush(DCC_RR_XDI);
#else
 DCCVStack_GetRegExact(DCC_RR_XDI);
#endif
 /* Load the target address into 'EDI' */
 DCCDisp_LeaReg(dst,DCC_RR_XDI);
 DCCDisp_AXCmpDI(max_width,dst_bytes,nejmp);
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_PopReg(DCC_RR_XDI);
#endif
}
PRIVATE void
DCCDisp_DoRepBytCmpMem_impl(int src, struct DCCMemLoc const *__restrict dst,
                            target_siz_t dst_bytes, struct DCCMemLoc const *__restrict nejmp) {
 rc_t ax; width_t max_width;
 target_off_t filler;
#ifdef DCC_RC_I64
      if (dst_bytes >= 8) max_width = 8,ax = DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_RAX;
 else
#endif
      if (dst_bytes >= 4) max_width = 4,ax = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
 else if (dst_bytes >= 2) max_width = 2,ax = DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_AX;
 else                     max_width = 1,ax = DCC_RC_I8|DCC_ASMREG_AL;
 ax = DCCVStack_GetRegExact(ax);
 filler = (target_off_t)(src & 0xff);
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 filler *= 0x0101010101010101ll;
#else
 filler *= 0x01010101;
#endif
 DCCDisp_IntMovReg(filler,ax); /* Load the filler byte into 'AX'. */
 DCCDisp_DoRepAXCmpMem_impl(max_width,dst,dst_bytes,nejmp);
}
PRIVATE void
DCCDisp_DoRepByrCmpMem_impl(rc_t src, width_t max_width, struct DCCMemLoc const *__restrict dst,
                            target_siz_t dst_bytes, struct DCCMemLoc const *__restrict nejmp) {
 rc_t ax;
#ifdef DCC_RC_I64
      if (max_width >= 8) ax = DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_RAX;
 else
#endif
      if (max_width >= 4) ax = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
 else if (max_width >= 2) ax = DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_AX;
 else                     ax = DCC_RC_I8|DCC_ASMREG_AL;
 if ((src&7) != DCC_ASMREG_EAX) ax = DCCVStack_GetRegExact(ax);
 /* Load the source register into 'AX'. */
 DCCDisp_RegMovReg(src,ax,1);
 DCCDisp_DoRepAXCmpMem_impl(max_width,dst,dst_bytes,nejmp);
}


/* Compare 'dst_bytes' of memory from 'dst' with 'src' in reverse, jumping
 * to 'nejmp' once data is found in 'dst' that's not equal to 'src'.
 * NOTE: The last compare operation will _not_ generate a jump! */
PRIVATE void
DCCDisp_BytCmpMem_impl(int src, struct DCCMemLoc const *__restrict dst,
                       target_siz_t dst_bytes, struct DCCMemLoc const *__restrict nejmp) {
 struct DCCMemLoc dst_iter = *dst;
 struct DCCSymAddr filler;
 unsigned int score = 3;
 if unlikely(!dst_bytes) return;
 if (!DCCVStack_GetRegInuse(DCC_RR_XAX)) --score;
 if ((dst->ml_reg&DCC_RI_MASK) == DCC_ASMREG_EDI) score -= (!dst->ml_off && !dst->ml_sym) ? 2 : 1;
 /* Special optimization for _very_ large compare: use 'rep scasb/w/l' */
 if (dst_bytes >= REPMOV_THRESHOLD(score)) {
  DCCDisp_DoRepBytCmpMem_impl(src,dst,dst_bytes,nejmp);
  return;
 }
 dst_iter.ml_off += dst_bytes;
 filler.sa_sym = NULL;
 filler.sa_off = (target_off_t)(src & 0xff);
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 filler.sa_off *= 0x0101010101010101ll;
#else
 filler.sa_off *= 0x01010101;
#endif
 for (;;) {
  target_siz_t part;
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
       if (dst_bytes >= 8) part = 8;
  else
#endif
       if (dst_bytes >= 4) part = 4;
  else if (dst_bytes >= 2) part = 2;
  else                     part = 1;
  dst_iter.ml_off -= part;
  DCCDisp_CstBinMem('?',&filler,&dst_iter,part,1);
  dst_bytes -= part;
  if (!dst_bytes) break;
  DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
 }
}
PRIVATE void /* Same as 'DCCDisp_BytCmpMem_impl', but 'src' is a register only known at runtime. */
DCCDisp_ByrCmpMem_impl(rc_t src, width_t max_width, struct DCCMemLoc const *__restrict dst,
                       target_siz_t dst_bytes, struct DCCMemLoc const *__restrict nejmp) {
 struct DCCMemLoc dst_iter = *dst;
 unsigned int score = 3;
 if unlikely(!dst_bytes) return;
 if ((src&DCC_RI_MASK) == DCC_ASMREG_EAX) --score;
 if ((dst->ml_reg&DCC_RI_MASK) == DCC_ASMREG_EDI) score -= (!dst->ml_off && !dst->ml_sym) ? 2 : 1;
 /* Special optimization for _very_ large compare: use 'rep scasb/w/l' */
 if (dst_bytes >= REPMOV_THRESHOLD(score)) {
  DCCDisp_DoRepByrCmpMem_impl(src,max_width,dst,dst_bytes,nejmp);
  return;
 }
 dst_iter.ml_off += dst_bytes;
 for (;;) {
  target_siz_t part = max_width;
  if (part > dst_bytes) {
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
   if (dst_bytes <= 7 &&
       dst_bytes >= 5) part = 4;
   else
#endif
   if (dst_bytes == 3) part = 2;
   else                part = dst_bytes;
  }
#ifdef DCC_RC_I64
       if (part < 8) src &= ~(DCC_RC_I64);
  else
#endif
       if (part < 4) src &= ~(DCC_RC_I32);
  else if (part < 2) src &= ~(DCC_RC_I16);
  dst_iter.ml_off -= part;
  DCCDisp_RegBinMem('?',src,&dst_iter,1);
  dst_bytes -= part;
  if (!dst_bytes) break;
  DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
 }
}

PRIVATE void
DCCDisp_SICmpDI(target_siz_t n_bytes,
                struct DCCMemLoc const *__restrict nejmp) {
 if (n_bytes <= DCC_TARGET_SIZEOF_ARITH_MAX*2) {
  /* Special case for small compares. */
  while (n_bytes >= 4) {
   asm_op_cmpsl();
   n_bytes -= 4;
   if (n_bytes) DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  }
  if (n_bytes & 2) {
   asm_op_cmpsw();
   if (n_bytes & 1) DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  }
  if (n_bytes & 1) asm_op_cmpsb();
  return;
 }
 DCCVStack_GetRegExact(DCC_RR_XCX);
 if (n_bytes % 4) {
  /* Special cases for situations in which the given byte count is unaligned! */
  if (!(n_bytes % 2) && n_bytes <= DCC_TARGET_SIZEOF_ARITH_MAX*8) {
   /* Use 'repe cmpsw' */
   DCCDisp_IntMovReg((target_off_t)(n_bytes / 2),DCC_RR_XCX),n_bytes %= 2;
   asm_op_cld();
   asm_op_repe();
   asm_op_cmpsw();
  } else if (n_bytes <= DCC_TARGET_SIZEOF_ARITH_MAX*4) {
   /* Use 'repe cmpsb' */
   DCCDisp_IntMovReg((target_off_t)n_bytes,DCC_RR_XCX),n_bytes = 0;
   asm_op_cld();
   asm_op_repe();
   asm_op_cmpsb();
  } else {
   goto use_movsl;
  }
 } else {
use_movsl:
  DCCDisp_IntMovReg((target_off_t)(n_bytes / 4),DCC_RR_XCX),n_bytes %= 4;
  asm_op_repe();
  asm_op_cmpsl();
 }
 /* Compare any overflow still remaining. */
 if (n_bytes & 2) {
  DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  asm_op_cmpsw();
 }
 if (n_bytes & 1) {
  DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  asm_op_cmpsb();
 }
}
PRIVATE void
DCCDisp_DoRepMemCmpMem_fixed(struct DCCMemLoc const *__restrict src,
                             struct DCCMemLoc const *__restrict dst,
                             target_siz_t n_bytes,
                             struct DCCMemLoc const *__restrict nejmp) {
 assert(n_bytes);
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_RegPush(DCC_RR_XDI);
 if ((src->ml_reg&DCC_RI_MASK) != DCC_ASMREG_ESI) DCCDisp_RegPush(DCC_RR_XSI);
#else
 DCCVStack_GetRegExact(DCC_RR_XDI);
 DCCVStack_GetRegExact(DCC_RR_XSI);
#endif
 DCCDisp_LeaReg(dst,DCC_RR_XDI);
 DCCDisp_LeaReg(src,DCC_RR_XSI);
 DCCDisp_SICmpDI(n_bytes,nejmp);
#ifdef IA32_PROTECTED_REGISTERS
 if ((src->ml_reg&DCC_RI_MASK) != DCC_ASMREG_ESI) DCCDisp_PopReg(DCC_RR_XSI);
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_PopReg(DCC_RR_XDI);
#endif
}
PRIVATE void
DCCDisp_MemCmpMem_fixed(struct DCCMemLoc const *__restrict src,
                        struct DCCMemLoc const *__restrict dst,
                        target_siz_t n_bytes, struct DCCMemLoc const *__restrict nejmp) {
 rc_t temp_register; target_siz_t part;
 struct DCCMemLoc src_iter = *src,dst_iter = *dst;
 unsigned int score = 4;
 if unlikely(!n_bytes) return;
 if ((src->ml_reg&DCC_RI_MASK) == DCC_ASMREG_ESI) score -= (!src->ml_off && !src->ml_sym) ? 2 : 1;
 if ((dst->ml_reg&DCC_RI_MASK) == DCC_ASMREG_EDI) score -= (!dst->ml_off && !dst->ml_sym) ? 2 : 1;
 /* Special optimization for _very_ large moves: use 'rep movsb/w/l' */
 if (n_bytes >= REPMOV_THRESHOLD(score)) {
  DCCDisp_DoRepMemCmpMem_fixed(src,dst,n_bytes,nejmp);
  return;
 }
      if (n_bytes >= 4) temp_register = DCC_RC_I32;
 else if (n_bytes >= 2) temp_register = DCC_RC_I16;
 else                   temp_register = DCC_RC_I8;
 temp_register = DCCVStack_GetReg(temp_register,!(n_bytes&1));
 for (;;) {
       if (n_bytes >= 4) part = 4;
  else if (n_bytes >= 2) part = 2,temp_register &= ~(DCC_RC_I32);
  else                   part = 1,temp_register &= ~(DCC_RC_I32|DCC_RC_I16);
  /* Load memory from 'src' and compare it with 'dst'. */
  DCCDisp_MemMovReg(&src_iter,temp_register);
  DCCDisp_RegBinMem('?',temp_register,&dst_iter,0);
  n_bytes -= part;
  if (!n_bytes) break;
  /* If the compared memory is non-equal, jump to the end of the full comparison. */
  DCCDisp_LocJcc(DCC_TEST_NE,nejmp);
  src_iter.ml_off += part;
  dst_iter.ml_off += part;
 }
}


PUBLIC void
DCCDisp_MemBinMem(tok_t op,
                  struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 target_siz_t common_size,part; void const *csrc;
 rc_t temp,curr; struct DCCMemLoc curr_src,curr_dst;
 if (op == '=') { DCCDisp_MemMovMem(src,src_bytes,dst,dst_bytes,src_unsigned); return; }

 curr_src = *src,curr_dst = *dst;
 /* Special case: The source operand is known at compile-time. */
 if ((csrc = DCCMemLoc_CompilerData(&curr_src,src_bytes)) != NULL) {
  DCCDisp_VecBinMem(op,csrc,src_bytes,dst,dst_bytes,src_unsigned);
  return;
 }
 if (dst_bytes > DCC_TARGET_SIZEOF_ARITH_MAX && IS_LARGE_OP(op)) {
  DCCDisp_LargeMemBinMem(op,src,src_bytes,
                            dst,dst_bytes,
                         src_unsigned);
  return;
 }
 common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 if (op == '?' && (dst_bytes != common_size || !CHECK_WIDTH(common_size))) {
  /* Special handling for compare operations.
   * >> Required for handling unaligned, or very large comparisons. */
  struct DCCMemLoc jmp;
  jmp.ml_sym = DCCUnit_AllocSym();
  if unlikely(!jmp.ml_sym) return;
  jmp.ml_reg = DCC_RC_CONST;
  jmp.ml_off = 0;
  dst_bytes -= common_size;
  /* Generate compare across the shared region of memory. */
  DCCDisp_MemCmpMem_fixed(src,dst,common_size,&jmp);
  /* Generate the missing jump for overflow memory. */
  if (common_size && dst_bytes) DCCDisp_LocJcc(DCC_TEST_NE,&jmp);
  if (!src_bytes)
   DCCDisp_BytCmpMem_impl(0,dst,dst_bytes,&jmp);
  else if (dst_bytes) {
   struct DCCMemLoc high_dst = *dst;
   /* 'dst >= src', meaning that we must compare the higher regions of 'dst'
    * with the zero-/sign-extension of the last byte from 'src'. */
   high_dst.ml_off += common_size;
   if (src_unsigned) {
    /* Simple case: Just compare the higher regions with '0' */
    DCCDisp_BytCmpMem_impl(0,&high_dst,dst_bytes,&jmp);
   } else {
    target_siz_t max_width = 1;
    rc_t sign_extension;
    struct DCCMemLoc high_src = *dst;
    high_src.ml_off += (src_bytes-1);
         if (dst_bytes >= 16) max_width = 4;
    else if (dst_bytes >= 4)  max_width = 2;
         if (max_width >= 4) sign_extension = DCC_RC_I32;
    else if (max_width >= 2) sign_extension = DCC_RC_I16;
    else                     sign_extension = DCC_RC_I8;
    sign_extension = DCCVStack_GetReg(sign_extension,0);
    DCCDisp_MemSignExtendReg(&high_src,sign_extension);
    DCCDisp_ByrCmpMem_impl(sign_extension,max_width,&high_dst,dst_bytes,&jmp);
   }
  }
  /* Declare the jump target used when non-equal  */
  t_defsym(jmp.ml_sym);
  return;
 }
 temp = DCCVStack_GetReg(DCC_RC_FORSIZE(common_size),
                      !!(common_size != 1));
 dst_bytes -= common_size;
 while (common_size) {
  part = common_size >= DCC_TARGET_SIZEOF_ARITH_MAX ? DCC_TARGET_SIZEOF_ARITH_MAX :
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
         common_size >= 4 ? 4 :
#endif
         common_size >= 2 ? 2 :
                          1;
  curr = temp;
       if (part == 1) curr &= ~(DCC_RC_I3264|DCC_RC_I16);
  else if (part == 2) curr &= ~(DCC_RC_I3264);
#if DCC_RC_I64
  else if (part == 4) curr &= ~(DCC_RC_I64);
#endif
  DCCDisp_MemMovReg(&curr_src,curr);
  DCCDisp_RegBinMem(op,curr,&curr_dst,src_unsigned);
  curr_src.ml_off += part;
  curr_dst.ml_off += part;
  common_size     -= part;
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
 }
 /* Handle the rest. */
 DCCDisp_BytBinMem(op,0,dst_bytes,&curr_dst,dst_bytes,1);
}
PUBLIC void
DCCDisp_VecBinMem(tok_t op,   void const *__restrict src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 void *cdst; struct DCCSymAddr cst;
 struct DCCMemLoc dst_iter;
 struct DCCSym *jsym = NULL;
 target_siz_t common_size;
 /* Special case: Static, compile-time operation. */
 if ((cdst = DCCMemLoc_CompilerAddrUpdate(dst,(void **)&src,dst_bytes)) != NULL) {
  DCCDisp_StaBinSta(op,src,src_bytes,cdst,dst_bytes,src_unsigned);
  return;
 }
 if (op == '=') { DCCDisp_VecMovMem(src,src_bytes,dst,dst_bytes,src_unsigned); return; }
 if (!src_bytes) { DCCDisp_BytBinMem(op,0,dst_bytes,dst,dst_bytes,src_unsigned); return; }
 if (dst_bytes > DCC_TARGET_SIZEOF_ARITH_MAX && IS_LARGE_OP(op)) {
  DCCDisp_LargeVecBinMem(op,src,src_bytes,
                            dst,dst_bytes,
                         src_unsigned);
  return;
 }
 common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 dst_bytes -= common_size;
 cst.sa_sym = NULL;
 dst_iter = *dst;
 while (common_size) {
  width_t width;
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
       if (common_size >= 8) width = 8,cst.sa_off = (target_off_t)*(int64_t *)src;
  else
#endif
       if (common_size >= 4) width = 4,cst.sa_off = (target_off_t)*(int32_t *)src;
  else if (common_size >= 2) width = 2,cst.sa_off = (target_off_t)*(int16_t *)src;
  else                       width = 1,cst.sa_off = (target_off_t)*(int8_t *)src;
  DCCDisp_CstBinMem(op,&cst,&dst_iter,width,src_unsigned);
  dst_iter.ml_off    += width;
  *(uintptr_t *)&src += width;
  common_size        -= width;
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
  else if (op == '?' && common_size) {
   /* Special handling for compare operations. */
   if (jsym || (jsym = DCCUnit_AllocSym()) != NULL)
        DCCDisp_SymJcc(DCC_TEST_NE,jsym);
  }
 }
 /* Handle the remainder. */
 DCCDisp_BytBinMem(op,0,dst_bytes,dst,dst_bytes,src_unsigned);
 if (jsym) t_defsym(jsym);
}
PUBLIC void
DCCDisp_BytBinMem(tok_t op, int                      src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 void *cdst;
 struct DCCSymAddr cst;
 struct DCCMemLoc dst_iter;
 struct DCCSym *jsym = NULL;
 target_siz_t common_size;
 if (!dst_bytes) return;
 if (!src_bytes) {
  src_bytes    = dst_bytes;
  src          = 0;
  src_unsigned = 1;
 }
 /* Special case: Static, compile-time operation. */
 if ((cdst = DCCMemLoc_CompilerAddr(dst,dst_bytes)) != NULL) {
  DCCDisp_BytBinSta(op,src,src_bytes,cdst,dst_bytes,src_unsigned);
  return;
 }
 if (op == '=') { DCCDisp_BytMovMem(src,src_bytes,dst,dst_bytes,src_unsigned); return; }
 if (dst_bytes > DCC_TARGET_SIZEOF_ARITH_MAX && IS_LARGE_OP(op)) {
  DCCDisp_LargeBytBinMem(op,src,src_bytes,
                            dst,dst_bytes,
                         src_unsigned);
  return;
 }
 common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 dst_bytes -= common_size;
 cst.sa_sym = NULL;
 cst.sa_off = (target_off_t)(src & 0xff);
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
 cst.sa_off *= 0x0101010101010101ll;
#else
 cst.sa_off *= 0x01010101;
#endif
 dst_iter = *dst;
 while (common_size) {
  width_t width;
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
       if (common_size >= 8) width = 8;
  else
#endif
       if (common_size >= 4) width = 4;
  else if (common_size >= 2) width = 2;
  else                       width = 1;
  cst.sa_off = 0;
  DCCDisp_CstBinMem(op,&cst,&dst_iter,width,src_unsigned);
  dst_iter.ml_off += width;
  common_size     -= width;
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
  else if (op == '?' && common_size) {
   /* Special handling for compare operations. */
   if (jsym || (jsym = DCCUnit_AllocSym()) != NULL)
        DCCDisp_SymJcc(DCC_TEST_NE,jsym);
  }
 }
 /* Handle the remainder. */
 src = src_unsigned || !(src&0x80) ? 0 : 0xff;
 DCCDisp_BytBinMem(op,src,dst_bytes,&dst_iter,dst_bytes,src_unsigned);
 if (jsym) t_defsym(jsym);
}

PRIVATE void
DCCDisp_ByrBinMem_fixed(tok_t op, rc_t src,
                        struct DCCMemLoc const *__restrict dst,
                        target_siz_t n_bytes) {
 struct DCCMemLoc dst_iter = *dst;
 assert(!(src&(DCC_RC_I16|DCC_RC_I3264)));
 while (n_bytes) {
  DCCDisp_RegBinMem(op,src,&dst_iter,1);
  ++dst_iter.ml_off;
  --n_bytes;
 }
}

PUBLIC void
DCCDisp_ByrBinMem(tok_t op, rc_t                     src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 target_siz_t common_size;
 struct DCCMemLoc new_dst;
 struct DCCSym *jsym = NULL;
 if (op == '=') { DCCDisp_ByrMovMem(src,src_bytes,dst,dst_bytes,src_unsigned); return; }
 if (dst_bytes > DCC_TARGET_SIZEOF_ARITH_MAX && IS_LARGE_OP(op)) {
  DCCDisp_LargeByrBinMem(op,src,src_bytes,
                            dst,dst_bytes,
                         src_unsigned);
  return;
 }
 /* Fallback. */
 src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I8);
 common_size = dst_bytes < src_bytes ? dst_bytes : src_bytes;
 if (common_size) {
  DCCDisp_ByrBinMem_fixed(op,src,dst,common_size);
       if (op == '+') op = TOK_INC;
  else if (op == '-') op = TOK_DEC;
  else if (op == '?' && common_size != dst_bytes) {
   /* Special handling for compare operations. */
   if ((jsym = DCCUnit_AllocSym()) != NULL)
        DCCDisp_SymJcc(DCC_TEST_NE,jsym);
  }
 }

 /* zero-/sign-extend the remainder. */
 new_dst         = *dst;
 new_dst.ml_off += common_size;
 common_size = dst_bytes-common_size;
 if (common_size) {
  if (!src_unsigned) {
   /* sign-extend: src = (uint8_t)sign_extend(src); */
   DCCDisp_SignMirrorReg(src);
   DCCDisp_ByrBinMem(op,src,common_size,&new_dst,common_size,1);
  } else {
   DCCDisp_BytBinMem(op,0,common_size,&new_dst,common_size,1);
  }
 }
 if (jsym) t_defsym(jsym);
}



PUBLIC void
DCCDisp_AddReg(struct DCCSymAddr const *__restrict val,
               rc_t dst) {
 target_off_t i;
 assert(val);
 if (val->sa_sym) goto add_def;
 i = val->sa_off;
      if (i == 1)  DCCDisp_UnaryReg(TOK_INC,dst);
 else if (i == -1) DCCDisp_UnaryReg(TOK_DEC,dst);
 else if (i < 0) {
  struct DCCSymAddr neg_val;
  neg_val.sa_sym = val->sa_sym;
  neg_val.sa_off = -val->sa_off;
  DCCDisp_CstBinReg('-',&neg_val,dst,1);
 } else if (i > 0) {
add_def:
  DCCDisp_CstBinReg('+',val,dst,1);
 }
}
PUBLIC void
DCCDisp_AddMem(struct DCCSymAddr const *__restrict val,
               struct DCCMemLoc const *__restrict dst,
               width_t width) {
 target_off_t i;
 assert(val);
 if (val->sa_sym) goto add_def;
 i = val->sa_off;
      if (i == 1)  DCCDisp_UnaryMem('+',dst,width);
 else if (i == -1) DCCDisp_UnaryMem('-',dst,width);
 else if (i < 0) {
  struct DCCSymAddr neg_val;
  neg_val.sa_sym = val->sa_sym;
  neg_val.sa_off = -val->sa_off;
  DCCDisp_CstBinMem('-',&neg_val,dst,width,1);
 } else if (i > 0) {
add_def:
  DCCDisp_CstBinMem('+',val,dst,width,1);
 }
}


PUBLIC void
DCCDisp_LeaReg(struct DCCMemLoc const *__restrict addr,
               rc_t dst) {
      if (addr->ml_reg == DCC_RC_CONST) DCCDisp_CstMovReg(&addr->ml_sad,dst); /* Without a register, simply move the constant address. */
#if 0 /* LEA is used to prevent CPU-flags side-effects. - So 'add' can't be used here! */
 else if (addr->ml_reg == dst) DCCDisp_AddReg(&addr->ml_sad,dst); /* Special case: Same register. */
#endif
 else if (!addr->ml_off) DCCDisp_RegMovReg(addr->ml_reg,dst,1); /* Special case: No offset. */
 else if (!(dst&(DCC_RC_I3264|DCC_RC_I16))) {
  /* 8-bit lea. */
  DCCDisp_RegMovReg(addr->ml_reg,dst,1);
  DCCDisp_AddReg(&addr->ml_sad,dst);
 } else {
  DCCDisp_Probe(addr,1);
  /*DCCDisp_X86Segp(addr->ml_reg);*/
  if (!(dst&DCC_RC_I3264)) t_putb(0x66);
  t_putb(0x8d);
  asm_modmem(dst&DCC_RI_MASK,addr);
 }
}

PUBLIC rc_t
DCCDisp_AddProtReg(struct DCCSymAddr const *__restrict val, rc_t dst) {
 struct DCCMemLoc src;
 rc_t result = dst;
 if ((dst&(DCC_RC_I3264|DCC_RC_I16))&&
      DCC_ASMREG_ISSPTR(dst&DCC_RI_MASK)) {
  /* Protected register. */
  result = DCCVStack_GetReg(result&DCC_RC_MASK,1);
 }
 src.ml_reg = dst;
 src.ml_sad = *val;
 DCCDisp_LeaReg(&src,result);
 return result;
}


PUBLIC void
DCCDisp_LeaMem(struct DCCMemLoc const *__restrict addr,
               struct DCCMemLoc const *__restrict dst,
               DCC(width_t) width) {
 assert(CHECK_WIDTH(width));
      if (addr->ml_reg == DCC_RC_CONST) DCCDisp_CstMovMem(&addr->ml_sad,dst,width); /* Without a register, simply move the constant address. */
 else if (addr->ml_reg == dst->ml_reg) DCCDisp_AddMem(&addr->ml_sad,dst,width); /* Special case: Same register. */
 else if (!addr->ml_off) DCCDisp_RegMovMem(addr->ml_reg,dst); /* Special case: No offset. */
 else {
  rc_t temp = DCCVStack_GetReg(DCC_RC_FORSIZE(width),1);
  DCCDisp_LeaReg(addr,temp);
  DCCDisp_RegMovMem(temp,dst);
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_GEN_MOV_C_INL */
