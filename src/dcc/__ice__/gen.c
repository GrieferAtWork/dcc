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
#ifndef GUARD_DCC_GEN_C
#define GUARD_DCC_GEN_C 1

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/code.h>

#include <stdio.h>
#include <string.h>

#include "x86_util.h"

DCC_DECL_BEGIN

#if DCC_TARGET_CPU == DCC_TARGET_X86_64
#define VALIDATE_N(n) ((n) == 1 || (n) == 2 || (n) == 4 || (n) == 8)
#else
#define VALIDATE_N(n) ((n) == 1 || (n) == 2 || (n) == 4)
#endif

#define gen_modreg(group,reg) asm_putb(MODRM_REGISTER(group,reg))
#ifndef GUARD_DCC_DISP_C
PRIVATE void gen_modrm(int8_t reg, target_off_t offset, int8_t rm_reg) {
 int mod;
 /* Select displacement encoding. */
 if (!offset && rm_reg != DCC_ASMREG_EBP) mod = MODRM_MOD(B(00)); /* No displacement. */
 else if (offset == (int8_t)offset) mod = MODRM_MOD(B(01)); /* 8-bit, signed displacement. */
 else mod = MODRM_MOD(B(10)); /* 32-bit displacement. */
 /* Figure out if an sib byte is needed. */
 asm_putb((uint8_t)(mod|MODRM_REG(reg)|MODRM_RM(rm_reg)));
 if (rm_reg == MODRM_SIBREGISTER) {
  /* Add the sib byte. */
  asm_putb((uint8_t)(MODRM_MOD(0)| /* shift */
                     MODRM_REG(MODRM_SIBREGISTER)| /* No index */
                     MODRM_RM(rm_reg)));
 }
 /* add offset */
      if (mod == MODRM_MOD(B(01))) asm_putb((uint8_t)offset);
 else if (mod == MODRM_MOD(B(10))) asm_putl((uint32_t)offset);
}
#endif



DCCFUN void DCCGen_IndLea(DCC(target_off_t) offset, DCC(rc_t) src, DCC(rc_t) dst); /* lea offset(src), %dst */
#define     DCCGen_SymLea DCCGen_CstMov                                            /* lea symaddr, %dst */

DCCFUN void DCCGen_MovReg(DCC(rc_t) src, DCC(rc_t) dst, int mov_unsigned);             /* mov %src, %dst (NOTE: Any size-difference between classes is automatically truncated/zero-filled, or sign-extended based on 'mov_unsigned') */
DCCFUN void DCCGen_IndMov(DCC(target_off_t) offset, DCC(rc_t) src, DCC(rc_t) dst);     /* mov offset(src), %dst */
DCCFUN void DCCGen_MovInd(DCC(rc_t) src, DCC(target_off_t) offset, DCC(rc_t) dst);     /* mov %src, offset(dst) */
DCCFUN void DCCGen_SymMov(struct DCCSymExpr const *__restrict symaddr, DCC(rc_t) dst); /* mov symaddr, %dst (with indirection) */
DCCFUN void DCCGen_MovSym(DCC(rc_t) src, struct DCCSymExpr const *__restrict symaddr); /* mov %src, symaddr (with indirection) */
DCCFUN void DCCGen_CstMov(struct DCCSymExpr const *__restrict symaddr, DCC(rc_t) reg); /* mov $symaddr, %reg */
DCCFUN void DCCGen_CstMovInd(struct DCCSymExpr const *__restrict symaddr, DCC(target_off_t) offset, DCC(rc_t) dst, size_t n_bytes);    /* mov $symaddr, offset(%reg) */
DCCFUN void DCCGen_CstMovSym(struct DCCSymExpr const *__restrict symaddr, struct DCCSymExpr const *__restrict target, size_t n_bytes); /* mov $symaddr, symaddr (with indirection) */


//////////////////////////////////////////////////////////////////////////
// Generate memcpy-style code, assuming that the specified src/dst memory location don't overlap.
DCCFUN void DCCGen_IndMovInd(DCC(target_off_t) src_offset, DCC(rc_t) src, DCC(target_off_t) dst_offset, DCC(rc_t) dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_IndMovSym(DCC(target_off_t) src_offset, DCC(rc_t) src, struct DCCSymExpr const *__restrict dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_SymMovInd(struct DCCSymExpr const *__restrict src, DCC(target_off_t) dst_offset, DCC(rc_t) dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_SymMovSym(struct DCCSymExpr const *__restrict src, struct DCCSymExpr const *__restrict dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_VecMovInd(void const *__restrict data, DCC(target_off_t) dst_offset, DCC(rc_t) dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_VecMovSym(void const *__restrict data, struct DCCSymExpr const *__restrict dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_BytMovInd(int byte, DCC(target_off_t) dst_offset, DCC(rc_t) dst, target_ptr_t n_bytes);
DCCFUN void DCCGen_BytMovSym(int byte, struct DCCSymExpr const *__restrict dst, target_ptr_t n_bytes);


//////////////////////////////////////////////////////////////////////////
// Perform a binary operation 'op' between two operands:
// NOTE: '+' and '-' don't perform any special-case optimization!
//  -  '+':         dst += src;
//  -  '|':         dst |= src;
//  -  TOK_INC:     dst += (src + CARRY);
//  -  TOK_DEC:     dst -= (src + CARRY);
//  -  '&':         dst &= src;
//  -  '-':         dst -= src;
//  -  '^':         dst ^= src;
//  -  '?':         dst ? src; // compare (set EFLAGS accordingly)
//  -  '*':         dst *= src;
//  -  '%':         dst %= src;
//  -  '/':         dst /= src;
//  -  TOK_SHL:     dst <<= src;
//  -  TOK_SHR:     dst >>= src;
//  -  TOK_LANGLE3: dst <<= src; // dst: signed
//  -  TOK_RANGLE3: dst >>= src; // dst: signed
//  -  't':         dst & src; // bitwise test (set eflags accordingly)
DCCFUN void DCCGen_BinReg(DCC(tok_t) op, DCC(rc_t) src, DCC(rc_t) dst, int src_unsigned);         /* *op %src, %dst (NOTE: Any size-difference between classes is automatically truncated/zero-filled) */
DCCFUN void DCCGen_IndBin(DCC(tok_t) op, DCC(target_off_t) offset, DCC(rc_t) src, DCC(rc_t) dst, int src_unsigned); /* *op offset(src), %dst */
DCCFUN void DCCGen_BinInd(DCC(tok_t) op, DCC(rc_t) src, DCC(target_off_t) offset, DCC(rc_t) dst, int src_unsigned); /* *op %src, offset(dst) */
DCCFUN void DCCGen_SymBin(DCC(tok_t) op, struct DCCSymExpr const *__restrict symaddr, DCC(rc_t) dst, int src_unsigned); /* *op symaddr, %dst (with indirection) */
DCCFUN void DCCGen_BinSym(DCC(tok_t) op, DCC(rc_t) src, struct DCCSymExpr const *__restrict symaddr, int src_unsigned); /* *op %src, symaddr (with indirection) */
DCCFUN void DCCGen_CstBin(DCC(tok_t) op, struct DCCSymExpr const *__restrict symaddr, DCC(rc_t) reg, int src_unsigned); /* *op $symaddr, %reg */
DCCFUN void DCCGen_CstBinInd(DCC(tok_t) op, struct DCCSymExpr const *__restrict symaddr, DCC(target_off_t) offset, DCC(rc_t) dst, size_t n, int src_unsigned); /* *op $symaddr, offset(%reg) */
DCCFUN void DCCGen_CstBinSym(DCC(tok_t) op, struct DCCSymExpr const *__restrict symaddr, struct DCCSymExpr const *__restrict target, size_t n, int src_unsigned);    /* *op $symaddr, target (with indirection) */

//////////////////////////////////////////////////////////////////////////
// Recognized values for 'op':
//  -  '-':     dst = -dst;
//  -  '~':     dst = ~dst;
//  -  '(':   (*dst)(...);    // CALL (With indirection)
//  -  TOK_INC: dst = dst+1;
//  -  TOK_DEC: dst = dst-1;
DCCFUN void DCCGen_UnaryReg(DCC(tok_t) op, DCC(rc_t) dst);
DCCFUN void DCCGen_UnaryInd(DCC(tok_t) op, DCC(target_off_t) offset, DCC(rc_t) dst, size_t n);
DCCFUN void DCCGen_UnarySym(DCC(tok_t) op, struct DCCSymExpr const *__restrict symaddr, size_t n);
DCCFUN void DCCGen_CallSym(struct DCCSymExpr const *__restrict symaddr);

//////////////////////////////////////////////////////////////////////////
// Add 'i' to the given register/memory location, implementing
// 'inc/dec', as well as 'add/sub' for constant operands.
// WARNING: Remember that inc/dec won't touch the carry flag!
//          If carry is important, 'DCCGen_CstBin*('+',...)' instead.
DCCFUN void DCCGen_AddReg(DCC(int_t) i, DCC(rc_t) dst);
DCCFUN void DCCGen_AddInd(DCC(int_t) i, DCC(target_off_t) offset, DCC(rc_t) dst, size_t n);
DCCFUN void DCCGen_AddSym(DCC(int_t) i, struct DCCSymExpr const *__restrict symaddr, size_t n);

DCCFUN void DCCGen_PushInd(DCC(target_off_t) offset, DCC(rc_t) reg, target_ptr_t n);
DCCFUN void DCCGen_PushCst(struct DCCSymExpr const *__restrict symaddr, target_ptr_t n); /* push(b/w/l) $symaddr */
DCCFUN void DCCGen_PushSym(struct DCCSymExpr const *__restrict symaddr, target_ptr_t n); /* [run-time] push 'n' bytes of memory, starting at 'symaddr' */

DCCFUN void DCCGen_JmpInd(DCC(target_off_t) offset, DCC(rc_t) reg, size_t n);
DCCFUN void DCCGen_JmpSym(struct DCCSymExpr const *__restrict symaddr, size_t n);
DCCFUN void DCCGen_SccInd(test_t t, DCC(target_off_t) offset, DCC(rc_t) reg, size_t n);
DCCFUN void DCCGen_SccSym(test_t t, struct DCCSymExpr const *__restrict symaddr, size_t n);




#if DCC_DEBUG
PRIVATE void *
DCCGen_CompiletimeAddress(struct DCCSymExpr const *__restrict target,
                          size_t n_bytes);
#else
PRIVATE void *
#define DCCGen_CompiletimeAddress(target,n_bytes) DCCGen_CompiletimeAddress_fast(target)
DCCGen_CompiletimeAddress_fast(struct DCCSymExpr const *__restrict target);
#endif


PUBLIC void DCCGen_IndLea(target_off_t offset, rc_t src, rc_t dst) {
 /* Special case: No offset. */
 if (!offset) { DCCGen_MovReg(src,dst,1); return; }
 /* Special case: Same register. */
 if (src == dst) { DCCGen_AddReg(offset,dst); return; }
 if (!(dst&(DCC_RC_I3264|DCC_RC_I16))) {
  /* 8-bit lea. */
  DCCGen_MovReg(src,dst,1);
  DCCGen_AddReg(offset,dst);
  return;
 }
 if (!(dst&DCC_RC_I32)) asm_putb(0x66);
 asm_putb(0x8d);
 gen_modrm(dst&7,offset,src&7);
}
#ifndef DCCGen_SymLea
PUBLIC void DCCGen_SymLea(struct DCCSymExpr const *__restrict symaddr, rc_t dst) {
 if (!(dst&(DCC_RC_I3264|DCC_RC_I16))) {
  /* 8-bit lea. */
  DCCGen_CstMov(symaddr,dst);
  return;
 }
 if (!(dst&DCC_RC_I32)) asm_putb(0x66);
 asm_putb(0x8d);
 asm_putb(MODRM_DISP32(dst&7));
 DCCTarget_TextExpr32(symaddr);
}
#endif


/* Generates instructions for moving 'n_bytes' from 'ESI' to EDI */
PRIVATE void gen_sidi_mov(target_ptr_t n_bytes) {
 struct DCCSymExpr val;
 if (n_bytes <= 16) {
write_small:
  /* Inline opcodes for small memory sizes. */
  while (n_bytes >= 4) asm_putb(0xa5),n_bytes -= 4;
  if (n_bytes&2) asm_putb(0x66),asm_putb(0xa5);
  if (n_bytes&1) asm_putb(0xa4);
 } else {
  rc_t cnt_reg = DCCVStack_GetRegExact(DCC_RR_XCX);
  val.e_sym = NULL;
  if (n_bytes >= 256) {
   val.e_int = n_bytes/4;
   DCCGen_CstMov(&val,cnt_reg);
   /* 'rep movl' */
   asm_putb(0xf3);
   asm_putb(0xa5);
   n_bytes %= 4;
   goto write_small;
  }
  if (n_bytes >= 64) {
   val.e_int = n_bytes/2;
   DCCGen_CstMov(&val,cnt_reg);
   /* 'rep movw' */
   asm_putb(0xf3);
   asm_putb(0x66);
   asm_putb(0xa5);
   n_bytes %= 2;
   goto write_small;
  }
  val.e_int = n_bytes;
  DCCGen_CstMov(&val,cnt_reg);
  /* 'rep movb' */
  asm_putb(0xf3);
  asm_putb(0xa4);
 }
}

#  define GET_REPMOV_THRESHOLD(score) ((score)*(DCC_TARGET_SIZEOF_POINTER*2)) /*< Use 'rep mov' for transfer of data. */
#  define MOVE_MEMSET_THRESHOLD                (8*DCC_TARGET_SIZEOF_POINTER)  /*< Use memset-style semantics. */
//#define MOVE_DATVEC_THRESHOLD                (8*DCC_TARGET_SIZEOF_POINTER)  /*< Copy data from '.data' individually (very inefficient). */
#  define PUSH_INLVEC_THRESHOLD                (4*DCC_TARGET_SIZEOF_POINTER)  /*< (ab-)use push/pop for data transfer. */

PUBLIC void
DCCGen_IndMovInd(target_off_t src_offset, rc_t src,
                 target_off_t dst_offset, rc_t dst,
                 target_ptr_t n_bytes) {
 rc_t temp_register;
 int pushpop_restore;
 unsigned int score = 3;
 target_ptr_t max_block;
 if unlikely(!n_bytes) return;
 /* TODO: Special handling for overlapping code? */
 if ((src&7) == DCC_ASMREG_ESI) --score;
 if ((dst&7) == DCC_ASMREG_EDI) --score;
 if (!src_offset && !dst_offset) --score;
 if (n_bytes >= GET_REPMOV_THRESHOLD(score)) {
  /* Use ESI/EDI. */
#ifdef IA32_PROTECTED_REGISTERS
  if (src != DCC_RR_XSI) DCCDisp_RegPush(DCC_RR_XSI);
  if (dst != DCC_RR_XDI) DCCDisp_RegPush(DCC_RR_XDI);
#else /* IA32_PROTECTED_REGISTERS */
  if (src != DCC_RR_XSI) DCCVStack_GetRegExact(DCC_RR_XSI);
  if (dst != DCC_RR_XDI) DCCVStack_GetRegExact(DCC_RR_XDI);
#endif /* !IA32_PROTECTED_REGISTERS */
  DCCGen_IndLea(src_offset,src,DCC_RR_XSI);
  DCCGen_IndLea(dst_offset,dst,DCC_RR_XDI);
  gen_sidi_mov(n_bytes);
#ifdef IA32_PROTECTED_REGISTERS
  if (dst != DCC_RR_XDI) DCCDisp_PopReg(DCC_RR_XDI);
  if (src != DCC_RR_XSI) DCCDisp_PopReg(DCC_RR_XSI);
#endif /* IA32_PROTECTED_REGISTERS */
  return;
 }
 /* Use regular, old indirection mov. */
 max_block = n_bytes;
 if (max_block >= DCC_TARGET_SIZEOF_POINTER)
  max_block = DCC_TARGET_SIZEOF_POINTER;
#if DCC_TARGET_SIZEOF_POINTER > 4
 else if (max_block >= 4) max_block = 4;
#endif
 else if (max_block >= 2) max_block = 2;
 else if (max_block >= 1) max_block = 1;
 pushpop_restore = 0;
 temp_register = DCCVStack_GetReg(DCC_RC_FORSIZE(max_block),
                                  2|(int)!(n_bytes&1));
 if (!temp_register) {
  temp_register = DCC_ASMREG_EAX;
  while ((src&7) == temp_register ||
         (dst&7) == temp_register ||
          temp_register == DCC_ASMREG_ESP ||
          temp_register == DCC_ASMREG_EBP) {
   ++temp_register;
   temp_register %= 8;
  }
  pushpop_restore = 1;
  DCCDisp_RegPush(temp_register);
 }

 while (n_bytes >= max_block) {
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
  src_offset += max_block;
  dst_offset += max_block;
  n_bytes    -= max_block;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  temp_register &= ~(DCC_RC_I64);
  assert(temp_register&DCC_RC_I32);
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
  src_offset += 4;
  dst_offset += 4;
 }
#endif
 if (n_bytes&2) {
  temp_register &= ~(DCC_RC_I3264);
  assert(temp_register&DCC_RC_I16);
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
  src_offset += 2;
  dst_offset += 2;
 }
 if (n_bytes&1) {
  temp_register &= ~(DCC_RC_I3264|DCC_RC_I16);
  assert(temp_register&DCC_RC_I8);
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
 }
 if (pushpop_restore) {
  DCCDisp_PopReg(temp_register);
 }
}

PUBLIC void
DCCGen_IndMovSym(target_off_t src_offset, rc_t src,
                 struct DCCSymExpr const *__restrict dst,
                 target_ptr_t n_bytes) {
 struct DCCSymExpr temp;
 target_ptr_t max_block;
 unsigned int score = 3;
 rc_t temp_register;
 if unlikely(!n_bytes) return;
 if ((src&7) == DCC_ASMREG_ESI) --score;
 if (n_bytes >= GET_REPMOV_THRESHOLD(score)) {
  /* Use ESI/EDI. */
#ifdef IA32_PROTECTED_REGISTERS
  if (src != DCC_RR_XSI) DCCDisp_RegPush(DCC_RR_XSI);
  DCCDisp_RegPush(DCC_RR_XDI);
#else /* IA32_PROTECTED_REGISTERS */
  if (src != DCC_RR_XSI) DCCVStack_GetRegExact(DCC_RR_XSI);
  DCCVStack_GetRegExact(DCC_RR_XDI);
#endif /* !IA32_PROTECTED_REGISTERS */
  DCCGen_IndLea(src_offset,src,DCC_RR_XSI);
  DCCGen_SymLea(dst,DCC_RR_XDI);
  gen_sidi_mov(n_bytes);
#ifdef IA32_PROTECTED_REGISTERS
  DCCDisp_PopReg(DCC_RR_XDI);
  if (src != DCC_RR_XSI) DCCDisp_PopReg(DCC_RR_XSI);
#endif /* IA32_PROTECTED_REGISTERS */
  return;
 }
 /* Use regular, old indirection mov. */
 memcpy(&temp,dst,sizeof(struct DCCSymExpr));
 max_block = n_bytes;
 if (max_block >= DCC_TARGET_SIZEOF_POINTER)
  max_block = DCC_TARGET_SIZEOF_POINTER;
#if DCC_TARGET_SIZEOF_POINTER > 4
 else if (max_block >= 4) max_block = 4;
#endif
 else if (max_block >= 2) max_block = 2;
 else if (max_block >= 1) max_block = 1;
 temp_register = DCCVStack_GetReg(DCC_RC_FORSIZE(max_block),!(n_bytes&1));
 while (n_bytes >= max_block) {
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovSym(temp_register,&temp);
  src_offset += max_block;
  temp.e_int += max_block;
  n_bytes    -= max_block;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  temp_register &= ~(DCC_RC_I64);
  assert(temp_register&DCC_RC_I32);
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovSym(temp_register,&temp);
  src_offset += 4;
  temp.e_int += 4;
 }
#endif
 if (n_bytes&2) {
  temp_register &= ~(DCC_RC_I3264);
  assert(temp_register&DCC_RC_I16);
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovSym(temp_register,&temp);
  src_offset += 2;
  temp.e_int += 2;
 }
 if (n_bytes&1) {
  temp_register &= ~(DCC_RC_I3264|DCC_RC_I16);
  assert(temp_register&DCC_RC_I8);
  DCCGen_IndMov(src_offset,src,temp_register);
  DCCGen_MovSym(temp_register,&temp);
 }
}
PUBLIC void
DCCGen_SymMovInd(struct DCCSymExpr const *__restrict src,
                 target_off_t dst_offset, rc_t dst,
                 target_ptr_t n_bytes) {
 struct DCCSymExpr temp;
 target_ptr_t max_block; 
 unsigned int score = 3;
 rc_t temp_register;
 if unlikely(!n_bytes) return;
 if ((dst&7) == DCC_ASMREG_EDI) --score;
 if (n_bytes >= GET_REPMOV_THRESHOLD(score)) {
  /* Use ESI/EDI. */
#ifdef IA32_PROTECTED_REGISTERS
  DCCDisp_RegPush(DCC_RR_XSI);
  if (dst != DCC_RR_XDI) DCCDisp_RegPush(DCC_RR_XDI);
#else
  DCCVStack_GetRegExact(DCC_RR_XSI);
  if (dst != DCC_RR_XDI) DCCVStack_GetRegExact(DCC_RR_XDI);
#endif
  DCCGen_SymLea(src,DCC_RR_XSI);
  DCCGen_IndLea(dst_offset,dst,DCC_RR_XDI);
  gen_sidi_mov(n_bytes);
#ifdef IA32_PROTECTED_REGISTERS
  if (dst != DCC_RR_XDI) DCCDisp_PopReg(DCC_RR_XDI);
  DCCDisp_PopReg(DCC_RR_XSI);
#endif
  return;
 }
 /* Use regular, old indirection mov. */
 memcpy(&temp,src,sizeof(struct DCCSymExpr));
 max_block = n_bytes;
 if (max_block >= DCC_TARGET_SIZEOF_POINTER)
  max_block = DCC_TARGET_SIZEOF_POINTER;
#if DCC_TARGET_SIZEOF_POINTER > 4
 else if (max_block >= 4) max_block = 4;
#endif
 else if (max_block >= 2) max_block = 2;
 else if (max_block >= 1) max_block = 1;
 temp_register = DCCVStack_GetReg(DCC_RC_FORSIZE(max_block),!(n_bytes&1));
 while (n_bytes >= max_block) {
  DCCGen_SymMov(&temp,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
  temp.e_int += max_block;
  dst_offset += max_block;
  n_bytes    -= max_block;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  temp_register &= ~(DCC_RC_I64);
  assert(temp_register&DCC_RC_I32);
  DCCGen_SymMov(&temp,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
  temp.e_int += 4;
  dst_offset += 4;
 }
#endif
 if (n_bytes&2) {
  temp_register &= ~(DCC_RC_I3264);
  assert(temp_register&DCC_RC_I16);
  DCCGen_SymMov(&temp,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
  temp.e_int += 2;
  dst_offset += 2;
 }
 if (n_bytes&1) {
  temp_register &= ~(DCC_RC_I3264|DCC_RC_I16);
  assert(temp_register&DCC_RC_I8);
  DCCGen_SymMov(&temp,temp_register);
  DCCGen_MovInd(temp_register,dst_offset,dst);
 }
}
PUBLIC void
DCCGen_SymMovSym(struct DCCSymExpr const *__restrict src,
                 struct DCCSymExpr const *__restrict dst,
                 target_ptr_t n_bytes) {
 struct DCCSymExpr src_temp,dst_temp;
 target_ptr_t max_block; rc_t temp_register;
 void *saddr,*daddr;
 /* Special case: Nothing must be copied. */
 if unlikely(!n_bytes) return;
 /* Special case: source and destination don't differ. */
 if (src->e_int == dst->e_int &&
     src->e_sym == dst->e_sym) return;
 /* Special case: compile-time assignment. */
 if ((daddr = DCCGen_CompiletimeAddress(dst,n_bytes)) != NULL &&
     (saddr = DCCGen_CompiletimeAddress(src,n_bytes)) != NULL) {
  if (daddr != saddr) memcpy(daddr,saddr,n_bytes);
  return;
 }
 if (n_bytes >= GET_REPMOV_THRESHOLD(3)) {
  /* Use ESI/EDI. */
#ifdef IA32_PROTECTED_REGISTERS
  DCCDisp_RegPush(DCC_RR_XSI);
  DCCDisp_RegPush(DCC_RR_XDI);
#else
  DCCVStack_GetRegExact(DCC_RR_XSI);
  DCCVStack_GetRegExact(DCC_RR_XDI);
#endif
  DCCGen_SymLea(src,DCC_RR_XSI);
  DCCGen_SymLea(dst,DCC_RR_XDI);
  gen_sidi_mov(n_bytes);
#ifdef IA32_PROTECTED_REGISTERS
  DCCDisp_PopReg(DCC_RR_XDI);
  DCCDisp_PopReg(DCC_RR_XSI);
#endif
  return;
 }
 /* Use regular, old indirection mov. */
 memcpy(&src_temp,src,sizeof(struct DCCSymExpr));
 memcpy(&dst_temp,dst,sizeof(struct DCCSymExpr));
 max_block = n_bytes;
 if (max_block >= DCC_TARGET_SIZEOF_POINTER)
  max_block = DCC_TARGET_SIZEOF_POINTER;
#if DCC_TARGET_SIZEOF_POINTER > 4
 else if (max_block >= 4) max_block = 4;
#endif
 else if (max_block >= 2) max_block = 2;
 else if (max_block >= 1) max_block = 1;
 temp_register = DCCVStack_GetReg(DCC_RC_FORSIZE(max_block),!(n_bytes&1));
 while (n_bytes >= max_block) {
  DCCGen_SymMov(&src_temp,temp_register);
  DCCGen_MovSym(temp_register,&dst_temp);
  src_temp.e_int += max_block;
  dst_temp.e_int += max_block;
  n_bytes        -= max_block;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  temp_register &= ~(DCC_RC_I64);
  assert(temp_register&DCC_RC_I32);
  DCCGen_SymMov(&src_temp,temp_register);
  DCCGen_MovSym(temp_register,&dst_temp);
  src_temp.e_int += 4;
  dst_temp.e_int += 4;
 }
#endif
 if (n_bytes&2) {
  temp_register &= ~(DCC_RC_I3264);
  assert(temp_register&DCC_RC_I16);
  DCCGen_SymMov(&src_temp,temp_register);
  DCCGen_MovSym(temp_register,&dst_temp);
  src_temp.e_int += 2;
  dst_temp.e_int += 2;
 }
 if (n_bytes&1) {
  temp_register &= ~(DCC_RC_I3264|DCC_RC_I16);
  assert(temp_register&DCC_RC_I8);
  DCCGen_SymMov(&src_temp,temp_register);
  DCCGen_MovSym(temp_register,&dst_temp);
 }
}

PUBLIC void
DCCGen_VecMovInd(void const *__restrict data,
                 target_off_t dst_offset, rc_t dst, target_ptr_t n_bytes) {
 struct DCCSymExpr data_expr;
#ifdef MOVE_DATVEC_THRESHOLD
 if (n_bytes >= MOVE_DATVEC_THRESHOLD) {
  /* Don't use immediate data, but copy from '.data' instead! */
  data_expr.e_int = 0;
  data_expr.e_sym = DCCTarget_AllocData(data,n_bytes,DCC_MIN_ALIGNMENT);
  DCCGen_SymMovInd(&data_expr,dst_offset,dst,n_bytes);
  return;
 }
#endif
 if (n_bytes >= PUSH_INLVEC_THRESHOLD) {
  rc_t temp_register;
  /* (ab-)use the stack pointer ESP to quickly write immediate data to memory.
   *  The only disadvantage of this, is that we need
   *  a temporary register to backup the original ESP.
   */
  temp_register = DCCVStack_GetReg(DCC_RC_PTR,1);
  DCCGen_MovReg(DCC_RR_XSP,temp_register,1);
  /* Since the stack grows downwards, need to adjust the destination offset. */
  dst_offset += n_bytes;
  /* Align for the first push operation. */
       if (n_bytes >= 4) dst_offset -= 4;
  else if (n_bytes >= 2) dst_offset -= 2;
  else                   dst_offset -= 1;
  DCCGen_IndLea(dst_offset,dst,DCC_RR_XSP);
  DCCDisp_VecPush(data,n_bytes);
  DCCGen_MovReg(temp_register,DCC_RR_XSP,1);
  return;
 }
 /* Fallback: Using immediate values, directly write data into the destination. */
 data_expr.e_sym = NULL;
 while (n_bytes >= DCC_TARGET_SIZEOF_POINTER) {
  data_expr.e_int = (int_t)*(uint32_t *)data;
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,DCC_TARGET_SIZEOF_POINTER);
  *(uintptr_t *)&data += DCC_TARGET_SIZEOF_POINTER;
  dst_offset          += DCC_TARGET_SIZEOF_POINTER;
  n_bytes             -= DCC_TARGET_SIZEOF_POINTER;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  data_expr.e_int = (int_t)*(uint32_t *)data;
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,4);
  *(uintptr_t *)&data += 4;
  dst_offset          += 4;
 }
#endif
 if (n_bytes&2) {
  data_expr.e_int = (int_t)*(uint16_t *)data;
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,2);
  *(uintptr_t *)&data += 2;
  dst_offset          += 2;
 }
 if (n_bytes&1) {
  data_expr.e_int = (int_t)*(uint8_t *)data;
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,1);
 }
}

#if DCC_DEBUG
PRIVATE void *
DCCGen_CompiletimeAddress(struct DCCSymExpr const *__restrict target,
                          size_t n_bytes)
#else
PRIVATE void *
DCCGen_CompiletimeAddress_fast(struct DCCSymExpr const *__restrict target)
#endif
{
 if ((dcc_current->t_flags&DCC_TARGET_FLAG_SINIT) && target->e_sym) {
  struct DCCSym *target_sym = target->e_sym;
  while ((assert(target_sym->s_kind == DCC_SYMTYPE_LABEL) &&
          target_sym->s_label.sl_alias)) target_sym = target_sym->s_label.sl_alias;
  if (target_sym->s_label.sl_sec) {
   struct DCCSection *target_sec;
   struct DCCTextBuf *target_text;
   uint8_t *target_data;
   /* Directly write to target memory (at compile-time).
    * >> This is used for static initializer of global data. */
   assert(!target_sym->s_label.sl_alias);
   target_sec = target_sym->s_label.sl_sec;
   /* Make sure to use the correct text buffer. */
   target_text = target_sec == dcc_curr
    ? &dcc_current->t_currbuf
    : &target_sec->s_text;
   target_data  = target_text->tb_begin;
   target_data += target_sym->s_label.sl_addr;
   target_data += target->e_int;
#if DCC_DEBUG
   assertf(target_data         >= target_text->tb_begin &&
           target_data+n_bytes <= target_text->tb_pos,
           "Target pointer %lu..%lu is out-of-bounds of 0..%lu",
          (unsigned long)(target_data-target_text->tb_begin),
          (unsigned long)((target_data-target_text->tb_begin)+n_bytes),
          (unsigned long)(target_text->tb_pos-target_text->tb_begin));
#endif
   return target_data;
  }
 }
 return NULL;
}


PUBLIC void
DCCGen_VecMovSym(void const *__restrict data,
                 struct DCCSymExpr const *__restrict dst,
                 target_ptr_t n_bytes) {
 struct DCCSymExpr data_expr,dst_expr;
 void *cdata = DCCGen_CompiletimeAddress(dst,n_bytes);
 if (cdata) { memcpy(cdata,data,n_bytes); return; }

#ifdef MOVE_DATVEC_THRESHOLD
 if (n_bytes >= MOVE_DATVEC_THRESHOLD) {
  /* Don't use immediate data, but copy from '.data' instead! */
  data_expr.e_int = 0;
  data_expr.e_sym = DCCTarget_AllocData(data,n_bytes,DCC_MIN_ALIGNMENT);
  DCCGen_SymMovSym(&data_expr,dst,n_bytes);
  return;
 }
#endif
 if (n_bytes >= PUSH_INLVEC_THRESHOLD) {
  rc_t temp_register;
  /* (ab-)use the stack pointer ESP to quickly write immediate data to memory.
   *  The only disadvantage of this, is that we need
   *  a temporary register to backup the original ESP.
   */
  dst_expr.e_sym  = dst->e_sym;
  dst_expr.e_int  = dst->e_int;
  /* Since the stack grows downwards, need to adjust the destination offset. */
  dst_expr.e_int += n_bytes;
  /* Align for the first push operation. */
       if (n_bytes >= 4) dst_expr.e_int -= 4;
  else if (n_bytes >= 2) dst_expr.e_int -= 2;
  else                   dst_expr.e_int -= 1;
  temp_register = DCCVStack_GetReg(DCC_RC_PTR,1);
  DCCGen_MovReg(DCC_RR_XSP,temp_register,1);
  DCCGen_SymLea(&dst_expr,DCC_RR_XSP);
  DCCDisp_VecPush(data,n_bytes);
  DCCGen_MovReg(temp_register,DCC_RR_XSP,1);
  return;
 }
 /* Fallback: Using immediate values, directly write data into the destination. */
 dst_expr.e_sym  = dst->e_sym;
 dst_expr.e_int  = dst->e_int;
 data_expr.e_sym = NULL;
 while (n_bytes >= DCC_TARGET_SIZEOF_POINTER) {
  data_expr.e_int = (int_t)*(uint32_t *)data;
  DCCGen_CstMovSym(&data_expr,&dst_expr,DCC_TARGET_SIZEOF_POINTER);
  *(uintptr_t *)&data += DCC_TARGET_SIZEOF_POINTER;
  dst_expr.e_int      += DCC_TARGET_SIZEOF_POINTER;
  n_bytes             -= DCC_TARGET_SIZEOF_POINTER;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  data_expr.e_int = (int_t)*(uint32_t *)data;
  DCCGen_CstMovSym(&data_expr,&dst_expr,4);
  *(uintptr_t *)&data += 4;
  dst_expr.e_int      += 4;
 }
#endif
 if (n_bytes&2) {
  data_expr.e_int = (int_t)*(uint16_t *)data;
  DCCGen_CstMovSym(&data_expr,&dst_expr,2);
  *(uintptr_t *)&data += 2;
  dst_expr.e_int      += 2;
 }
 if (n_bytes&1) {
  data_expr.e_int = (int_t)*(uint8_t *)data;
  DCCGen_CstMovSym(&data_expr,&dst_expr,1);
 }
}


PRIVATE void
DCCGen_DoMemset(int byte, target_ptr_t n_bytes) {
 struct DCCSymExpr temp; rc_t eax;
 target_ptr_t part_size;
 eax = DCC_RR_XAX;
#ifdef DCC_RC_I64
 if (n_bytes <= 4) eax &= ~(DCC_RC_I64);
#endif
 if (n_bytes <= 2) eax &= ~(DCC_RC_I32);
 if (n_bytes <= 1) eax &= ~(DCC_RC_I16);
 DCCVStack_GetRegExact(eax);
 temp.e_sym = NULL;
#if DCC_TARGET_SIZEOF_POINTER >= 8
 if (n_bytes >= 8) temp.e_int = 0x0101010101010101ll*byte;
 else
#endif
      if (n_bytes >= 4) temp.e_int = 0x01010101*byte;
 else if (n_bytes >= 2) temp.e_int = 0x0101*byte;
 else                   temp.e_int = byte;
 /* Fill the lowest-order EAX register with the repeated filler byte. */
 /* mov $..., %eax */
 DCCGen_CstMov(&temp,eax);
 if (n_bytes >= 4) {
  /* mov $(n_bytes/4), %ecx */
  /* rep stosl */
  /* Allocate ECX for the size. */
  DCCVStack_GetRegExact(DCC_RR_XCX);
  part_size = n_bytes/4,n_bytes %= 4;
  temp.e_int = (int_t)part_size;
  /* Fill ECX with the partial size. */
  DCCGen_CstMov(&temp,DCC_RR_XCX);
  asm_putb(0xf3); /* rep */
  asm_putb(0xab); /* stosl */
 }
 assert(n_bytes < 4);
 /* At this point, we must still fill any overflow from 'n_bytes'. */
 if (n_bytes&2) asm_putb(0x66),asm_putb(0xab); /* stosw */
 if (n_bytes&1) asm_putb(0xaa); /* stosb */
}

PRIVATE void
DCCGen_MemsetInd(int byte, target_off_t dst_offset, rc_t dst, target_ptr_t n_bytes) {
 if unlikely(!n_bytes) return;
 /* Allocate the EDI register if dst isn't already it. */
 if ((dst&7) != DCC_ASMREG_EDI) DCCVStack_GetRegExact(DCC_RR_XDI);
 /* Load the effective address into EDI */
 DCCGen_IndLea(dst_offset,dst,DCC_RR_XDI);
 /* Do the common part. */
 DCCGen_DoMemset(byte,n_bytes);
}
PRIVATE void
DCCGen_MemsetSym(int byte, struct DCCSymExpr const *__restrict dst, target_ptr_t n_bytes) {
 if unlikely(!n_bytes) return;
 /* Load the destination symbol address into EDI. */
 DCCVStack_GetRegExact(DCC_RR_XDI);
 DCCGen_CstMov(dst,DCC_RR_XDI);
 /* Do the common part. */
 DCCGen_DoMemset(byte,n_bytes);
}


PUBLIC void
DCCGen_BytMovInd(int byte, target_off_t dst_offset, rc_t dst, target_ptr_t n_bytes) {
 struct DCCSymExpr data_expr;
 if (n_bytes >= MOVE_MEMSET_THRESHOLD) {
  DCCGen_MemsetInd(byte,dst_offset,dst,n_bytes);
  return;
 }
 if (n_bytes >= PUSH_INLVEC_THRESHOLD) {
  rc_t temp_register;
  /* (ab-)use the stack pointer ESP to quickly write immediate data to memory.
   *  The only disadvantage of this, is that we need
   *  a temporary register to backup the original ESP.
   */
  temp_register = DCCVStack_GetReg(DCC_RC_PTR,1);
  DCCGen_MovReg(DCC_RR_XSP,temp_register,1);
  /* Since the stack grows downwards, need to adjust the destination offset. */
  dst_offset += n_bytes;
  /* Align for the first push operation. */
       if (n_bytes >= 4) dst_offset -= 4;
  else if (n_bytes >= 2) dst_offset -= 2;
  else                   dst_offset -= 1;
  DCCGen_IndLea(dst_offset,dst,DCC_RR_XSP);
  DCCDisp_BytPush(byte,n_bytes);
  DCCGen_MovReg(temp_register,DCC_RR_XSP,1);
  return;
 }
 /* Fallback: Using immediate values, directly write data into the destination. */
 data_expr.e_sym = NULL;
#if DCC_TARGET_SIZEOF_POINTER == 8
 data_expr.e_int = 0x0101010101010101ull*byte;
#elif DCC_TARGET_SIZEOF_POINTER == 4
 data_expr.e_int = 0x01010101ull*byte;
#else
#   error FIXME
#endif
 while (n_bytes >= DCC_TARGET_SIZEOF_POINTER) {
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,DCC_TARGET_SIZEOF_POINTER);
  dst_offset += DCC_TARGET_SIZEOF_POINTER;
  n_bytes    -= DCC_TARGET_SIZEOF_POINTER;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,4);
  dst_offset += 4;
 }
#endif
 if (n_bytes&2) {
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,2);
  dst_offset += 2;
 }
 if (n_bytes&1) {
  DCCGen_CstMovInd(&data_expr,dst_offset,dst,1);
 }
}
PUBLIC void
DCCGen_BytMovSym(int byte, struct DCCSymExpr const *__restrict dst, target_ptr_t n_bytes) {
 struct DCCSymExpr data_expr,dst_expr;
 void *cdata = DCCGen_CompiletimeAddress(dst,n_bytes);
 if (cdata) { memset(cdata,byte,n_bytes); return; }
 if (n_bytes >= MOVE_MEMSET_THRESHOLD) {
  DCCGen_MemsetSym(byte,dst,n_bytes);
  return;
 }
 if (n_bytes >= PUSH_INLVEC_THRESHOLD) {
  rc_t temp_register;
  /* (ab-)use the stack pointer ESP to quickly write immediate data to memory.
   *  The only disadvantage of this, is that we need
   *  a temporary register to backup the original ESP.
   */
  dst_expr.e_sym  = dst->e_sym;
  dst_expr.e_int  = dst->e_int;
  /* Since the stack grows downwards, need to adjust the destination offset. */
  dst_expr.e_int += n_bytes;
  /* Align for the first push operation. */
       if (n_bytes >= 4) dst_expr.e_int -= 4;
  else if (n_bytes >= 2) dst_expr.e_int -= 2;
  else                   dst_expr.e_int -= 1;
  temp_register = DCCVStack_GetReg(DCC_RC_PTR,1);
  DCCGen_MovReg(DCC_RR_XSP,temp_register,1);
  DCCGen_SymLea(&dst_expr,DCC_RR_XSP);
  DCCDisp_BytPush(byte,n_bytes);
  DCCGen_MovReg(temp_register,DCC_RR_XSP,1);
  return;
 }
 /* Fallback: Using immediate values, directly write data into the destination. */
 dst_expr.e_sym  = dst->e_sym;
 dst_expr.e_int  = dst->e_int;
 data_expr.e_sym = NULL;
#if DCC_TARGET_SIZEOF_POINTER == 8
 data_expr.e_int = 0x0101010101010101ull*byte;
#elif DCC_TARGET_SIZEOF_POINTER == 4
 data_expr.e_int = 0x01010101ull*byte;
#else
#   error FIXME
#endif
 while (n_bytes >= DCC_TARGET_SIZEOF_POINTER) {
  DCCGen_CstMovSym(&data_expr,&dst_expr,DCC_TARGET_SIZEOF_POINTER);
  dst_expr.e_int += DCC_TARGET_SIZEOF_POINTER;
  n_bytes        -= DCC_TARGET_SIZEOF_POINTER;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  DCCGen_CstMovSym(&data_expr,&dst_expr,4);
  dst_expr.e_int += 4;
 }
#endif
 if (n_bytes&2) {
  DCCGen_CstMovSym(&data_expr,&dst_expr,2);
  dst_expr.e_int += 2;
 }
 if (n_bytes&1) {
  DCCGen_CstMovSym(&data_expr,&dst_expr,1);
 }
}




PUBLIC void DCCGen_MovReg(rc_t src, rc_t dst, int mov_unsigned) {
 rc_t c_src,c_dst;
 c_src = src&DCC_RC_MASK;
 c_dst = dst&DCC_RC_MASK;
 if (c_src&DCC_RC_I16) c_src |= DCC_RC_I8;
 if (c_dst&DCC_RC_I16) c_dst |= DCC_RC_I8;
 if (c_src == c_dst) {
  /* Check for special case: dst&src are identical. */
  if ((dst&7) == (src&7)) return;
  /* Move in same storage class. */
  if ((c_src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
  if (c_src&DCC_RC_I16) asm_putb(0x89);
  else                  asm_putb(0x88);
  goto modreg;
 }
 if (c_src > c_dst) {
  /* Truncate: Simply copy from a lower order register,
   *           but apply a mask for EDI/ESI. */
  if (c_dst&(DCC_RC_I16|DCC_RC_I3264)) {
   /* Special case: Move to lower-order storage class. */
   if ((dst&7) == (src&7)) return;
   /* Destination class isn't 8-bit, meaning a lower-order register is always available. */
   if (!(c_dst&DCC_RC_I3264)) asm_putb(0x66);
   asm_putb(0x89);
  } else {
   /* Special case: 'dst' is simply a lower-order register for 'src' */
   if ((dst&3) == (src&3)) return;
   if (!(src&4)) {
    /* 8-bit mode, but the source register has an 8-bit equivalent. */
    asm_putb(0x88);
   } else {
    int8_t temp_register;
    /* (very) difficult case:
     * The source register cannot be copied directly into the destination. */
    /* Choose a lower-order, general purpose 8-bit register, that doesn't overlap with 'dst'. */
    temp_register = (1 << DCC_ASMREG_AL)
                   |(1 << DCC_ASMREG_CL)
                   |(1 << DCC_ASMREG_DL)
                   |(1 << DCC_ASMREG_BL);
    temp_register &= ~(dst&3);
    temp_register = (int8_t)(DCCVStack_GetRegOf(DCC_RC_I8,temp_register)&7);

    /* movw %src, %temp_register */
    asm_putb(0x66);
    asm_putb(0x89);
    gen_modreg(src&7,temp_register);

    /* movb %temp_register, %dst */
    asm_putb(0x88);
    gen_modreg(temp_register,dst&7);
    return;
   }
  }
 } else if (c_dst&DCC_RC_I32) {
  rc_t temp;
  asm_putb(0x0f);
  /* Expand: Use movzx / movsx */
  if (mov_unsigned) {
   if (c_src&DCC_RC_I16) asm_putb(0xb7);
   else upcast1632_u:    asm_putb(0xb6);
  } else {
   if (c_src&DCC_RC_I16) asm_putb(0xbf);
   else upcast1632_s:    asm_putb(0xbe);
  }
  /* Must swap registers because movzx uses the second operand as r/m argument. */
  temp = src,src = dst,dst = temp;
 } else {
  asm_putb(0x66);
  asm_putb(0x0f);
  if (mov_unsigned) goto upcast1632_u;
  else              goto upcast1632_s;
 }
modreg:
 gen_modreg(src&7,dst&7);
}

PUBLIC void DCCGen_IndMov(target_off_t offset, rc_t src, rc_t dst) {
 /* mov offset(src), %dst */
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 if (dst&DCC_RC_I16) asm_putb(0x8b);
 else                asm_putb(0x8a);
 gen_modrm(dst&7,offset,src&7);
}
PUBLIC void DCCGen_MovInd(rc_t src, target_off_t offset, rc_t dst) {
 /* mov %src, offset(%dst) */
 if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 if (src&DCC_RC_I16) asm_putb(0x89);
 else                asm_putb(0x88);
 gen_modrm(src&7,offset,dst&7);
}
PUBLIC void DCCGen_SymMov(struct DCCSymExpr const *__restrict symaddr, rc_t dst) {
 /* mov symaddr, %dst (with indirection) */
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 if (dst&DCC_RC_I16) asm_putb(0x8b);
 else                asm_putb(0x8a);
 asm_putb(MODRM_DISP32(dst&7));
 DCCTarget_TextExpr32(symaddr);
}
PUBLIC void DCCGen_MovSym(rc_t src, struct DCCSymExpr const *__restrict symaddr) {
 /* mov %src, symaddr (with indirection) */
 if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 if (src&DCC_RC_I16) asm_putb(0x89);
 else                asm_putb(0x88);
 asm_putb(MODRM_DISP32(src&7));
 DCCTarget_TextExpr32(symaddr);
}
PUBLIC void DCCGen_CstMov(struct DCCSymExpr const *__restrict symaddr, rc_t reg) {
 uint8_t opno;
 if ((reg&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 if (!symaddr->e_int) {
  if (!symaddr->e_sym) {
   /* Special case: mov $0, %reg --> xor %reg, %reg */
   DCCGen_BinReg('^',reg,reg,0);
   return;
  } else if (symaddr->e_int == 1) {
   /* Special case: movl $1, %reg --> xor %reg, %reg; inc %reg */
   DCCGen_BinReg('^',reg,reg,0);
   DCCGen_UnaryReg(TOK_INC,reg);
   return;
  } else if (symaddr->e_int == reg_max(reg)) {
   /* Special case: movl $0xffffffff, %reg --> xor %reg, %reg; dec %reg */
   DCCGen_BinReg('^',reg,reg,0);
   DCCGen_UnaryReg(TOK_DEC,reg);
   return;
  }
 }
 /* mov $symaddr, %reg */
 opno = (reg&DCC_RC_I16) ? 0xb8 : 0xb0;
 opno += reg&7;
 asm_putb(opno);
      if (reg&DCC_RC_I32) DCCTarget_TextExpr32(symaddr);
 else if (reg&DCC_RC_I16) DCCTarget_TextExpr16(symaddr);
 else                     DCCTarget_TextExpr8(symaddr);
}
PUBLIC void DCCGen_CstMovInd(struct DCCSymExpr const *__restrict symaddr,
                             target_off_t offset, rc_t dst, size_t n) {
 /* mov $symaddr, offset(%reg) */
 assert(VALIDATE_N(n));
 if (n == 2) asm_putb(0x66);
 if (n == 1) asm_putb(0xc6);
 else        asm_putb(0xc7);
 gen_modrm(0,offset,dst&7);
 switch (n) {
 case 4:  DCCTarget_TextExpr32(symaddr); break;
 case 2:  DCCTarget_TextExpr16(symaddr); break;
 default: DCCTarget_TextExpr8(symaddr); break;
 }
}


LOCAL int_t
def_reloc(struct DCCSymExpr const *__restrict expr,
          struct DCCSection *__restrict target_section,
          target_ptr_t target_offset, rel_t rel_size) {
 struct DCCSym *xsym; int_t xval;
 assert(expr);
 assert(dcc_current);
 xval = expr->e_int;
 if ((xsym = expr->e_sym) != NULL) {
  while ((assert(xsym->s_kind == DCC_SYMTYPE_LABEL),
          xsym->s_label.sl_alias)) xsym = xsym->s_label.sl_alias;
#if 1
  if (xsym->s_label.sl_sec && DCCSection_HASBASE(xsym->s_label.sl_sec)) {
   /* The associated section has a fixed base associated with it.
    * >> We don't need to emit a relocation, because
    *    we can simply add the base value here!
    */
   xval += (int_t)xsym->s_label.sl_addr;
   xval += (int_t)(target_ptr_t)xsym->s_label.sl_sec->s_base;
  } else
#endif
  {
   /* Generate a relocation at the current address.
    * >> This relocation must later add its base to the symbol. */
   DCCSection_PutRelocAt(target_section,xval
                         ? DCC_REL_OPADD|rel_size
                         : DCC_REL_OPSET|rel_size,
                         xsym,target_offset);
  }
 }
 return xval;
}


PUBLIC void
DCCGen_CstMovSym(struct DCCSymExpr const *__restrict symaddr,
                 struct DCCSymExpr const *__restrict target,
                 size_t n) {
 assert(symaddr);
 assert(target);
 VALIDATE_N(n);
 /* Handle special case: static initializer with relocation. */
 if ((dcc_current->t_flags&DCC_TARGET_FLAG_SINIT) && target->e_sym) {
  struct DCCSym *target_sym = target->e_sym;
  while ((assert(target_sym->s_kind == DCC_SYMTYPE_LABEL) &&
          target_sym->s_label.sl_alias)) target_sym = target_sym->s_label.sl_alias;
  if (target_sym->s_label.sl_sec) {
   struct DCCSection *target_sec;
   struct DCCTextBuf *target_text;
   uint8_t *target_data;
   target_ptr_t target_offset;
   /* Directly write to target memory (at compile-time).
    * >> This is used for static initializer of global data. */
   assert(!target_sym->s_label.sl_alias);
   target_sec = target_sym->s_label.sl_sec;
   target_text = target_sec == dcc_curr
    ? &dcc_current->t_currbuf
    : &target_sec->s_text;
   target_data    = target_text->tb_begin;
   target_offset  = target_sym->s_label.sl_addr;
   target_offset += (target_ptr_t)target->e_int;
   target_data   += target_offset;
#if DCC_DEBUG
   assertf(target_data   >= target_text->tb_begin &&
           target_data+n <= target_text->tb_pos,
           "Target pointer %lu..%lu is out-of-bounds of 0..%lu",
          (unsigned long)(target_data-target_text->tb_begin),
          (unsigned long)((target_data-target_text->tb_begin)+n),
          (unsigned long)(target_text->tb_pos-target_text->tb_begin));
#endif
   {
    /* Add a relocation and store the symbol. */
    rel_t rel_size = n == 8 ? DCC_REL_SIZE64 :
                     n == 4 ? DCC_REL_SIZE32 :
                     n == 2 ? DCC_REL_SIZE16 :
                              DCC_REL_SIZE8;
    int_t v = def_reloc(symaddr,target_sec,target_offset,rel_size);
    /* Store the initial value inside the target data. */
         if (n == 8) *(int64_t *)target_data = (int64_t)v;
    else if (n == 4) *(int32_t *)target_data = (int32_t)v;
    else if (n == 2) *(int16_t *)target_data = (int16_t)v;
    else             *(int8_t  *)target_data = (int8_t)v;
   }
   return;
  }
 }
 /* mov $symaddr, target */
 assert(VALIDATE_N(n));
 if (n == 2) asm_putb(0x66);
 if (n == 1) asm_putb(0xc6);
 else        asm_putb(0xc7);
 asm_putb(MODRM_DISP32(0));
 DCCTarget_TextExpr32(target);
 switch (n) {
 case 4:  DCCTarget_TextExpr32(symaddr); break;
 case 2:  DCCTarget_TextExpr16(symaddr); break;
 default: DCCTarget_TextExpr8(symaddr); break;
 }
}



PUBLIC void DCCGen_UnaryReg(tok_t op, rc_t dst) {
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 switch (op) {
 case '-':
 case '~':
  if (dst&DCC_RC_I16) asm_putb(0xf7);
  else                asm_putb(0xf6);
  gen_modreg((op == '-' ? 3 : 2),dst&7);
  break;
 case TOK_INC:
  if (dst&(DCC_RC_I16|DCC_RC_I3264)) {
   asm_putb(0x40+(dst&7));
  } else {
   asm_putb(0xfe);
   gen_modreg(0,dst&7);
  }
  break;
 case TOK_DEC:
  if (dst&(DCC_RC_I16|DCC_RC_I3264)) {
   asm_putb(0x48+(dst&7));
  } else {
   asm_putb(0xfe);
   gen_modreg(1,dst&7);
  }
  break;
 case '(':
  if (!(dst&(DCC_RC_I3264|DCC_RC_I16)))
   dst = DCCVStack_CastReg(dst,1,DCC_RC_I16);
  /* call *%dst */
  if (!(dst&DCC_RC_I32)) asm_putb(0x66);
  asm_putb(0xff);
  gen_modreg(2,dst&7);
  break;
 default: break;
 }
}
PUBLIC void DCCGen_UnaryInd(tok_t op, target_off_t offset, rc_t dst, size_t n) {
 if (n == 2) asm_putb(0x66);
 switch (op) {
 case '-':
 case '~':
  if (n == 4) asm_putb(0xf7);
  else        asm_putb(0xf6);
  gen_modrm(op == '-' ? 3 : 2,offset,dst&7);
  break;
 case TOK_INC: asm_putb(0xfe+(n != 1)); gen_modrm(0,offset,dst&7); break;
 case TOK_DEC: asm_putb(0xfe+(n != 1)); gen_modrm(1,offset,dst&7); break;
 case '(':
  if (n == 1) {
   /* 8-bit call. */
   rc_t temp = DCCVStack_GetReg(DCC_RC_I16,1);
   DCCGen_IndMov(offset,dst,temp);
   DCCGen_UnaryReg(op,temp);
  } else {
   /* call *offset(%dst) */
   if (n == 2) asm_putb(0x66);
   asm_putb(0xff);
   gen_modrm(2,offset,dst&7);
  }
  break;
 default: break;
 }
}
PUBLIC void
DCCGen_UnarySym(tok_t op, struct DCCSymExpr const *__restrict symaddr, size_t n) {
 if (n == 2) asm_putb(0x66);
 switch (op) {
 case '-':
 case '~':
  if (n == 4) asm_putb(0xf7);
  else        asm_putb(0xf6);
  asm_putb(MODRM_DISP32(op == '-' ? 3 : 2));
  DCCTarget_TextExpr32(symaddr);
  break;
 case TOK_INC: asm_putb(0xfe+(n != 1)); asm_putb(MODRM_DISP32(0)); DCCTarget_TextExpr32(symaddr); break;
 case TOK_DEC: asm_putb(0xfe+(n != 1)); asm_putb(MODRM_DISP32(1)); DCCTarget_TextExpr32(symaddr); break;
 case '(':
  if (n == 1) {
   /* 8-bit call. */
   rc_t temp = DCCVStack_GetReg(DCC_RC_I16,1);
   DCCGen_SymMov(symaddr,temp);
   DCCGen_UnaryReg(op,temp);
  } else {
   /* call *$symaddr */
   if (n == 2) asm_putb(0x66);
   asm_putb(0xff);
   asm_putb(MODRM_DISP32(2));
   DCCTarget_TextExpr32(symaddr);
  }
  break;
 default: break;
 }
}
PUBLIC void DCCGen_CallSym(struct DCCSymExpr const *__restrict symaddr) {
 /* call symaddr */
 asm_putb(0xe8);
 DCCTarget_TextDisp32(symaddr);
}



PUBLIC void DCCGen_AddReg(int_t i, rc_t dst) {
 if (i == (int_t)-1) return; /* Special case: With all bits set, nothing would change. */
 if (i && (dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) asm_putb(0x66);
 if (i == 1) {
  /* inc %dst */
  if (dst&DCC_RC_I16) asm_putb(0x40+dst&7);
  else asm_putb(0xfe),gen_modreg(0,dst&7);
 } else if (i == -1) {
  /* dec %dst */
  if (dst&DCC_RC_I16) asm_putb(0x48+dst&7);
  else asm_putb(0xfe),gen_modreg(1,dst&7);
 } else if (i < 0) {
  /* sub $-i, %dst */
  if ((dst&7) == DCC_ASMREG_EAX) {
   asm_putb(0x2c+!!(dst&DCC_RC_I16));
  } else {
   asm_putb(0x80+!!(dst&DCC_RC_I16));
   gen_modreg(5,dst&7);
  }
  i = -i;
  goto put_imm;
 } else if (i > 0) {
  /* add $i, %dst */
  if ((dst&7) == DCC_ASMREG_EAX) {
   asm_putb(0x04+!!(dst&DCC_RC_I16));
  } else {
   asm_putb(0x80+!!(dst&DCC_RC_I16));
   gen_modreg(0,dst&7);
  }
put_imm:
       if (dst&DCC_RC_I32) asm_putl((uint32_t)(int32_t)i);
  else if (dst&DCC_RC_I16) asm_putw((uint16_t)(int16_t)i);
  else                     asm_putb((uint8_t)(int8_t)i);
 }
}
PUBLIC void
DCCGen_AddInd(int_t i, target_off_t offset, rc_t dst, size_t n) {
 if (n == 2) asm_putb(0x66);
 if (i == 1) {
  /* inc offset(%dst) */
  asm_putb(0xfe+(n != 1));
  gen_modrm(0,offset,dst&7);
 } else if (i == -1) {
  /* dec offset(%dst) */
  asm_putb(0xfe+(n != 1));
  gen_modrm(1,offset,dst&7);
 } else if (i < 0) {
  /* sub $-i, %dst */
  asm_putb(0x80+(n != 1));
  gen_modrm(5,offset,dst&7);
  i = -i;
  goto put_imm;
 } else if (i > 0) {
  /* add $i, %dst */
  asm_putb(0x80+(n != 1));
  gen_modrm(0,offset,dst&7);
put_imm:
       if (dst&DCC_RC_I32) asm_putl((uint32_t)(int32_t)i);
  else if (dst&DCC_RC_I16) asm_putw((uint16_t)(int16_t)i);
  else                     asm_putb((uint8_t)(int8_t)i);
 }
}
PUBLIC void
DCCGen_AddSym(int_t i, struct DCCSymExpr const *__restrict symaddr, size_t n) {
 if (n == 2) asm_putb(0x66);
 if (i == 1) {
  /* inc symaddr */
  asm_putb(0xfe+(n != 1));
  asm_putb(MODRM_DISP32(0));
  goto gen_symaddr;
 } else if (i == -1) {
  /* dec symaddr */
  asm_putb(0xfe+(n != 1));
  asm_putb(MODRM_DISP32(1));
gen_symaddr:
  DCCTarget_TextExpr32(symaddr);
 } else if (i < 0) {
  /* sub $-i, symaddr */
  asm_putb(0x80+(n != 1));
  asm_putb(MODRM_DISP32(5));
  i = -i;
  goto put_imm;
 } else if (i > 0) {
  /* add $i, symaddr */
  asm_putb(0x80+(n != 1));
  asm_putb(MODRM_DISP32(0));
put_imm:
  DCCTarget_TextExpr32(symaddr);
       if (n == 4) asm_putl((uint32_t)(int32_t)i);
  else if (n == 2) asm_putw((uint16_t)(int16_t)i);
  else             asm_putb((uint8_t)(int8_t)i);
 }
}


PUBLIC void
DCCGen_PushInd(target_off_t offset, rc_t reg, target_ptr_t n_bytes) {
 if (n_bytes == 1) {
  /* Special handling for 8-bit push. */
  rc_t temp_reg = DCCVStack_GetReg(DCC_RC_I8,0);
  DCCGen_UnaryReg(TOK_DEC,DCC_RR_XSP);
  DCCGen_IndMov(offset,reg,temp_reg);
  DCCGen_MovInd(temp_reg,0,DCC_RR_XSP);
  return;
 }
 if (n_bytes == 2 || n_bytes == 4) {
  if (n_bytes == 2) asm_putb(0x66);
  asm_putb(0xff);
  gen_modrm(6,offset,reg&7);
  return;
 }
#ifdef MOVE_DATVEC_THRESHOLD
 if (n_bytes > MOVE_DATVEC_THRESHOLD) {
  struct DCCSymExpr temp;
  temp.e_int = (int_t)n_bytes;
  temp.e_sym = NULL;
  /* memcpy the block of data onto the stack. */
  DCCGen_CstBin('-',&temp,DCC_RR_XSP);
  DCCGen_IndMovInd(offset,reg,0,DCC_RR_XSP,n_bytes);
  return;
 }
#endif
 /* FALLBACK: Push each value individually. */
 while (n_bytes >= 4) {
  DCCGen_PushInd(offset,reg,4);
  offset += 4,n_bytes -= 4;
 }
 if (n_bytes&2) {
  DCCGen_PushInd(offset,reg,2);
  offset += 2,n_bytes -= 2;
 }
 if (n_bytes&1) {
  DCCGen_PushInd(offset,reg,1);
 }
}
PUBLIC void
DCCGen_PushCst(struct DCCSymExpr const *__restrict symaddr, target_ptr_t n) {
 assert(symaddr);
 if (n == 8 && !symaddr->e_sym) {
  struct DCCSymExpr temp;
  /* Special case: push 64-bit immediate value. */
  temp.e_sym = NULL;
  temp.e_int = ((uint32_t *)&symaddr->e_int)[1];
  DCCGen_PushCst(&temp,4);
  temp.e_int = ((uint32_t *)&symaddr->e_int)[0];
  DCCGen_PushCst(&temp,4);
  return;
 }
 if (n == 2) asm_putb(0x66);
 if (n == 1) asm_putb(0x6a);
 else        asm_putb(0x68);
      if (n == 4) DCCTarget_TextExpr32(symaddr);
 else if (n == 2) DCCTarget_TextExpr16(symaddr);
 else             DCCTarget_TextExpr8(symaddr);
}
PUBLIC void
DCCGen_PushSym(struct DCCSymExpr const *__restrict symaddr,
               target_ptr_t n_bytes) {
 struct DCCSymExpr temp;
 switch (n_bytes) {
 { /* Special case for 1-byte push. */
  rc_t temp_reg;
 case 1:
  temp_reg = DCCVStack_GetReg(DCC_RC_I8,0);
  DCCGen_UnaryReg(TOK_DEC,DCC_RR_XSP);
  DCCGen_SymMov(symaddr,temp_reg);
  DCCGen_MovInd(temp_reg,0,DCC_RR_XSP);
  return;
 } break;

 case 2: asm_putb(0x66); /* fallthrough */
 case 4:
  /* pushw/l symaddr */
  asm_putb(0xff);
  asm_putb(MODRM_DISP32(6));
  DCCTarget_TextExpr32(symaddr);
  return;
 default: break;
 }
#ifdef MOVE_DATVEC_THRESHOLD
 if (n_bytes > MOVE_DATVEC_THRESHOLD) {
  temp.e_int = (int_t)n_bytes;
  temp.e_sym = NULL;
  /* memcpy the block of data onto the stack. */
  DCCGen_CstBin('-',&temp,DCC_RR_XSP);
  DCCGen_SymMovInd(symaddr,0,DCC_RR_XSP,n_bytes);
  return;
 }
#endif
 /* FALLBACK: Push each symbol value individually. */
 temp.e_int  = symaddr->e_int;
 temp.e_sym  = symaddr->e_sym;
 temp.e_int += n_bytes;
 while (n_bytes >= 4) {
  temp.e_int -= 4,n_bytes -= 4;
  DCCGen_PushSym(&temp,4);
 }
 if (n_bytes&2) {
  temp.e_int -= 2,n_bytes -= 2;
  DCCGen_PushSym(&temp,2);
 }
 if (n_bytes&1) {
  temp.e_int -= 1;
  DCCGen_PushSym(&temp,1);
 }
}

PUBLIC void DCCGen_JmpInd(target_off_t offset, rc_t reg, size_t n) {
 if (n == 1) {
  rc_t temp_reg;
  /* Need at least 16-bit register. */
  temp_reg = DCCVStack_GetReg(DCC_RC_I16,0);
  DCCGen_BinReg('^',temp_reg,temp_reg,1);
  DCCGen_IndMov(offset,reg,temp_reg&~(DCC_RC_I16));
  DCCDisp_RegJmp(temp_reg);
  return;
 }
 if (n == 2) asm_putb(0x66);
 asm_putb(0xff);
 gen_modrm(4,offset,reg&7);
}
PUBLIC void DCCGen_JmpSym(struct DCCSymExpr const *__restrict symaddr, size_t n) {
 if (n == 1) {
  rc_t temp_reg;
  /* Need at least 16-bit address. */
  temp_reg = DCCVStack_GetReg(DCC_RC_I16,0);
  DCCGen_BinReg('^',temp_reg,temp_reg,1);
  DCCGen_SymMov(symaddr,temp_reg&~(DCC_RC_I16));
  DCCDisp_RegJmp(temp_reg);
  return;
 }
 if (n == 2) asm_putb(0x66);
 asm_putb(0xff);
 asm_putb(MODRM_DISP32(4));
 DCCTarget_TextDisp32(symaddr);
}


PUBLIC void
DCCGen_SccInd(test_t t, target_off_t offset,
              rc_t reg, size_t n) {
 assert(t >= 0 && t <= 0xf);
 if unlikely(!n) return;
 /* setcc offset(%reg) */
 asm_putb(0x0f);
 asm_putb(0x90+t);
 gen_modrm(0,offset,reg&7);
 /* Fill the rest with ZERO. */
 DCCGen_BytMovInd(0,offset+1,reg,n-1);
}
PUBLIC void
DCCGen_SccSym(test_t t,
              struct DCCSymExpr const *__restrict symaddr,
              size_t n) {
 assert(t >= 0 && t <= 0xf);
 if unlikely(!n) return;
 /* setcc symaddr */
 asm_putb(0x0f);
 asm_putb(0x90+t);
 asm_putb(MODRM_DISP32(0));
 DCCTarget_TextExpr32(symaddr);
 if (n > 1) {
  /* Fill the rest with ZERO. */
  struct DCCSymExpr temp;
  temp.e_int = symaddr->e_int+1;
  temp.e_sym = symaddr->e_sym;
  DCCGen_BytMovSym(0,&temp,n-1);
 }
}


DCC_DECL_END

#ifndef __INTELLISENSE__
#include "gen-binary.c.inl"
#endif


#endif /* !GUARD_DCC_GEN_C */
