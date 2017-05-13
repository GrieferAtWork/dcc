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
#ifndef GUARD_DCC_GEN_MOV_C_INL
#define GUARD_DCC_GEN_MOV_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#include <string.h>

#include "x86_util.h"

DCC_DECL_BEGIN

PUBLIC void
DCCDisp_MemMovReg(struct DCCMemLoc const *__restrict src,
                  rc_t dst) {
 /* mov !src, %dst */
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
 if (dst&DCC_RC_I16) t_putb(0x8b);
 else                t_putb(0x8a);
 asm_modmem(dst&7,src);
}

PUBLIC void
DCCDisp_RegMovMem(rc_t src,
                  struct DCCMemLoc const *__restrict dst) {
 /* mov %src, offset(%dst) */
 if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
 if (src&DCC_RC_I16) t_putb(0x89);
 else                t_putb(0x88);
 asm_modmem(src&7,dst);
}

PUBLIC void
DCCDisp_MemsMovReg(struct DCCMemLoc const *__restrict src,
                   target_siz_t src_bytes, rc_t dst,
                   int src_unsigned) {
 target_siz_t dst_siz = DCC_RC_SIZE(dst); assert(src);
 if unlikely(!src_bytes) { DCCDisp_RegBinReg('^',dst,dst,1); return; }
 if (src_bytes >= dst_siz) { DCCDisp_MemMovReg(src,dst); return; } /* Simply copy from src. */
 assert(src_bytes < DCC_TARGET_SIZEOF_POINTER);
 /* use movsx/movzx to extend the source memory location into 'dst'. */
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) {
  assert(src_bytes == 1); /* movsx/movzx m8, r16 */
  t_putb(0x66);
  t_putb(0x0f);
  t_putb((uint8_t)(src_unsigned ? 0xb6 : 0xbe));
  asm_modmem(dst&7,src);
 } else if (src_bytes == 1) {
  assert(dst&DCC_RC_I32); /* movsx/movzx m8, r32 */
  t_putb(0x0f);
  t_putb((uint8_t)(src_unsigned ? 0xb6 : 0xbe));
  asm_modmem(dst&7,src);
 } else {
  assert(dst&DCC_RC_I32); /* movsx/movzx m16, r32 */
  t_putb(0x0f);
  t_putb((uint8_t)(src_unsigned ? 0xb7 : 0xbf));
  asm_modmem(dst&7,src);
 }
}
PUBLIC void
DCCDisp_RegMovMems(rc_t src, struct DCCMemLoc const *__restrict dst,
                   target_siz_t dst_bytes, int src_unsigned) {
 struct DCCMemLoc newdst;
 target_siz_t src_siz = DCC_RC_SIZE(src); assert(dst);
 if (src_siz >= dst_bytes) {
  assert(dst_bytes <= DCC_TARGET_SIZEOF_POINTER);
  switch (dst_bytes) {
  case 0: return;
  case 1: src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I8); break;
  case 2: src &= ~(DCC_RC_I3264); break;
  {
   struct DCCSymAddr cst;
  case 3:
   DCCDisp_RegMovMem(src&~(DCC_RC_I32),dst);
   cst.sa_off = 16;
   cst.sa_sym = NULL;
   DCCDisp_CstBinReg(TOK_RANGLE3,&cst,src,1);
   src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I8);
   assert(!(src&(DCC_RC_I3264|DCC_RC_I16)));
   newdst         = *dst;
   newdst.ml_off += 2;
   DCCDisp_RegMovMem(src,&newdst);
   return;
  } break;
  default: break;
  }
  DCCDisp_RegMovMem(src,dst);
  return;
 }
 DCCDisp_RegMovMem(src,dst);
 newdst         = *dst;
 newdst.ml_off += src_siz;
 dst_bytes     -= src_siz;
 if (src_unsigned) {
  DCCDisp_BytMovMem(0,dst_bytes,&newdst,dst_bytes,1);
 } else {
  DCCDisp_SignExtendReg(src);
  DCCDisp_ByrMovMem(src,dst_bytes,&newdst,dst_bytes,1);
 }
}


PRIVATE void
DCCDisp_SIMovDI(target_siz_t n_bytes) {
 if (n_bytes <= DCC_TARGET_SIZEOF_POINTER*2) {
  /* Special case for small movs. */
  while (n_bytes >= 4) asm_op_movsl(),n_bytes -= 4;
  if (n_bytes & 2) asm_op_movsw();
  if (n_bytes & 1) asm_op_movsb();
  return;
 }
 DCCVStack_GetRegExact(DCC_RR_XCX);
 if (n_bytes % 4) {
  /* Special cases for situations in which the given byte count is unaligned! */
  if (!(n_bytes % 2) && n_bytes <= DCC_TARGET_SIZEOF_POINTER*8) {
   /* Use 'rep movsw' */
   DCCDisp_IntMovReg((target_off_t)(n_bytes / 2),DCC_RR_XCX),n_bytes %= 2;
   asm_op_cld();
   asm_op_rep();
   asm_op_movsw();
  } else if (n_bytes <= DCC_TARGET_SIZEOF_POINTER*4) {
   /* Use 'rep movsb' */
   DCCDisp_IntMovReg((target_off_t)n_bytes,DCC_RR_XCX),n_bytes = 0;
   asm_op_cld();
   asm_op_rep();
   asm_op_movsb();
  } else {
   goto use_movsl;
  }
 } else {
use_movsl:
  DCCDisp_IntMovReg((target_off_t)(n_bytes / 4),DCC_RR_XCX),n_bytes %= 4;
  asm_op_rep();
  asm_op_movsl();
 }
 /* Fix any overflow still remaining. */
 if (n_bytes & 2) asm_op_movsw();
 if (n_bytes & 1) asm_op_movsb();
}
PRIVATE void
DCCDisp_AXMovDI(width_t max_width, target_siz_t n_bytes) {
 assert(CHECK_WIDTH(max_width));
 if (n_bytes <= DCC_TARGET_SIZEOF_POINTER*2) {
  /* Special case for small stos. */
  if (max_width >= 4) while (n_bytes >= 4) asm_op_stosl(),n_bytes -= 4;
  if (max_width >= 2) while (n_bytes >= 2) asm_op_stosw(),n_bytes -= 2;
  while (n_bytes) asm_op_stosb(),--n_bytes;
  return;
 }
 DCCVStack_GetRegExact(DCC_RR_XCX);
 if (n_bytes % max_width) {
  /* Special cases for situations in which the given byte count is unaligned! */
  if (!(n_bytes % 2) && n_bytes <= DCC_TARGET_SIZEOF_POINTER*8) {
use_stosw:
   /* Use 'rep stosw' */
   DCCDisp_IntMovReg((target_off_t)(n_bytes / 2),DCC_RR_XCX),n_bytes %= 2;
   asm_op_cld();
   asm_op_rep();
   asm_op_stosw();
  } else if (n_bytes <= DCC_TARGET_SIZEOF_POINTER*4) {
use_stosb:
   /* Use 'rep stosb' */
   DCCDisp_IntMovReg((target_off_t)n_bytes,DCC_RR_XCX),n_bytes = 0;
   asm_op_cld();
   asm_op_rep();
   asm_op_stosb();
  } else {
   goto use_stosauto;
  }
 } else {
use_stosauto:
  if (max_width == 1) goto use_stosb;
  if (max_width == 2) goto use_stosw;
  assert(max_width == 4);
  DCCDisp_IntMovReg((target_off_t)(n_bytes / 4),DCC_RR_XCX),n_bytes %= 4;
  asm_op_rep();
  asm_op_stosl();
 }
 /* Fix any overflow still remaining. */
 if (max_width >= 2) while (n_bytes >= 2) asm_op_stosw(),n_bytes -= 2;
 while (n_bytes) asm_op_stosb(),--n_bytes;
}


PRIVATE void
DCCDisp_BytMovMem_fixed(int src,
                        struct DCCMemLoc const *__restrict dst,
                        target_siz_t n_bytes);
PRIVATE void
DCCDisp_DoRepMemMovMem(struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 target_siz_t common_size;
 if unlikely(!dst_bytes) return;
 if unlikely(!src_bytes) { DCCDisp_BytMovMem_fixed(0,dst,dst_bytes); return; }
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_RegPush(DCC_RR_XDI);
 if ((src->ml_reg&DCC_RI_MASK) != DCC_ASMREG_ESI) DCCDisp_RegPush(DCC_RR_XSI);
#else
 DCCVStack_GetRegExact(DCC_RR_XSI);
 DCCVStack_GetRegExact(DCC_RR_XDI);
#endif
 DCCDisp_LeaReg(dst,DCC_RR_XDI);
 DCCDisp_LeaReg(src,DCC_RR_XSI);
 common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 DCCDisp_SIMovDI(common_size);
#ifdef IA32_PROTECTED_REGISTERS
 if ((src->ml_reg&DCC_RI_MASK) != DCC_ASMREG_ESI) DCCDisp_PopReg(DCC_RR_XSI);
#endif
 if (common_size < dst_bytes) {
  /* Must fill remainder, with zero-/sign-extension of last src byte. */
  dst_bytes -= common_size;
  if (src_unsigned && CHECK_WIDTH(dst_bytes)) {
   struct DCCMemLoc dst_end;
   dst_end.ml_reg = DCC_RR_XDI;
   dst_end.ml_off = 0;
   dst_end.ml_sym = NULL;
   /* Special case: Fill remainder destination with one byte. */
   DCCDisp_CstMovMem(0,&dst_end,dst_bytes);
  } else {
   struct DCCMemLoc src_last = *src;
   src_last.ml_off += common_size-1;
   rc_t ax;
        if (dst_bytes >= 4) ax = DCC_RC_I32|DCC_ASMREG_EAX;
   else if (dst_bytes >= 2) ax = DCC_RC_I16|DCC_ASMREG_AX;
   else                     ax = DCC_RC_I8|DCC_ASMREG_AL;
   ax = DCCVStack_GetRegExact(ax);
   if (src_unsigned) DCCDisp_RegBinReg('^',ax,ax,1);
   else DCCDisp_MemSignExtendReg(&src_last,ax);
   DCCDisp_AXMovDI(4,dst_bytes);
  }
 }
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_PopReg(DCC_RR_XDI);
#endif
}
PRIVATE void
DCCDisp_DoRepBytMovMem(int                                src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 target_siz_t common_size;
 target_off_t used_src;
 rc_t ax;
 if unlikely(!dst_bytes) return;
 if unlikely(!src_bytes) {
  /* No source is the same as a zero-filled source. */
  src_bytes    = dst_bytes;
  src          = 0;
  src_unsigned = 1;
 }
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_RegPush(DCC_RR_XDI);
#else
 DCCVStack_GetRegExact(DCC_RR_XDI);
#endif
 if (!src || (src == 0xff && !src_unsigned)) {
  /* Special case: The overflow byte is equal to the filler byte. */
  common_size = dst_bytes;
 } else {
  common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 }
 used_src = src;
      if (common_size >= 4) ax = DCC_RC_I32|DCC_ASMREG_EAX,used_src *= 0x01010101;
 else if (common_size >= 2) ax = DCC_RC_I16|DCC_ASMREG_AX,used_src *= 0x0101;
 else                       ax = DCC_RC_I8|DCC_ASMREG_AL;
 ax = DCCVStack_GetRegExact(ax);
 DCCDisp_LeaReg(dst,DCC_RR_XDI); /* Load the destination into 'EDI' */
 DCCDisp_IntMovReg(used_src,ax); /* Load the filler byte into '(E)AX' */
 DCCDisp_AXMovDI(4,common_size); /* Fill the destination with '(E)AX' */
 dst_bytes -= common_size;
 if (dst_bytes) {
  /* Fill the remainder. */
  used_src = (src_unsigned || !(src&0x80)) ? 0 : -1;
  if (CHECK_WIDTH(dst_bytes)) {
   /* Small remainder: Fill with immediate value. */
   struct DCCMemLoc used_dst;
   struct DCCSymAddr filler;
   used_dst.ml_reg = DCC_RR_XDI;
   used_dst.ml_off = 0;
   used_dst.ml_sym = NULL;
   filler.sa_off = used_src;
   filler.sa_sym = NULL;
   DCCDisp_CstMovMem(&filler,&used_dst,dst_bytes);
  } else {
   DCCDisp_IntMovReg(used_src,ax); /* Load the overflow byte into '(E)AX' */
   DCCDisp_AXMovDI(4,common_size); /* Fill the destination with '(E)AX' */
  }
 }
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_PopReg(DCC_RR_XDI);
#endif
}
PRIVATE void
DCCDisp_DoRepByrMovMem(rc_t                               src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 target_siz_t common_size;
 width_t max_width; rc_t ax;
 if unlikely(!dst_bytes) return;
 if unlikely(!src_bytes) {
  /* No source is the same as a zero-filled source. */
  DCCDisp_DoRepBytMovMem(0,dst_bytes,dst,dst_bytes,1);
  return;
 }
 if (!(src&DCC_RC_I8)) src = DCCVStack_CastReg(src,1,DCC_RC_I8);
 else src &= ~(DCC_RC_I16|DCC_RC_I3264);
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_RegPush(DCC_RR_XDI);
#else
 DCCVStack_GetRegExact(DCC_RR_XDI);
#endif
 common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 max_width = 1;
 /* Determine an appropriate filler byte width. */
      if (common_size >= 16) max_width = 4;
 else if (common_size >= 4)  max_width = 2;
      if (max_width >= 4) ax = DCC_RC_I32|DCC_ASMREG_EAX;
 else if (max_width >= 2) ax = DCC_RC_I16|DCC_ASMREG_AX;
 else                     ax = DCC_RC_I8|DCC_ASMREG_AL;
 if ((src&7) != DCC_ASMREG_EAX) {
  ax = DCCVStack_GetRegExact(ax);
  DCCDisp_RegMovReg(src,ax,1);
 }
 if (max_width >= 2) {
  /* Clone 8-bit to 16-bit. */
  DCCDisp_RegMovReg(DCC_RC_I8|DCC_ASMREG_AL,
                    DCC_RC_I8|DCC_ASMREG_AH,1);
 }
 DCCDisp_LeaReg(dst,DCC_RR_XDI);         /* Load the destination into 'EDI' */
 if (max_width >= 4) {
  struct DCCSymAddr val;
  rc_t temp16 = DCCVStack_GetRegOf(DCC_RC_I16,(uint8_t)~((1 << DCC_ASMREG_EAX)|
                                                         (1 << DCC_ASMREG_EDI)));
  val.sa_off = 16,val.sa_sym = NULL;
  /* Clone 16-bit to 32-bit. */
  DCCDisp_RegMovReg(DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_AX,temp16,1);                   /* mov %ax, %temp */
  DCCDisp_CstBinReg(TOK_SHL,&val,DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX,1); /* shl $16, %eax */
  DCCDisp_RegMovReg(temp16,DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_AX,1);                   /* mov %temp, %ax */
 }
 DCCDisp_AXMovDI(max_width,common_size); /* Fill the destination with '(E)AX' */
 dst_bytes -= common_size;
 if (dst_bytes) {
  if (src_unsigned) {
   ax |= DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
   max_width = 4;
   DCCDisp_RegBinReg('^',ax,ax,1);
  } else {
   DCCDisp_SignExtendReg(ax);
  }
  DCCDisp_AXMovDI(max_width,dst_bytes); /* Fill the remainder... */
 }
#ifdef IA32_PROTECTED_REGISTERS
 if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDI) DCCDisp_PopReg(DCC_RR_XDI);
#endif
}

PRIVATE void
DCCDisp_DoMemMovMem(struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                    struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                    int src_unsigned) {
 rc_t temp_register; int must_pop_temp;
 struct DCCMemLoc new_dst,src_iter = *src,dst_iter = *dst;
 target_siz_t common_size,size_iter,max_block;
 unsigned int score = 5;
 if (src_unsigned) --score;
 if ((src->ml_reg&7) == DCC_ASMREG_ESI) score -= (!src->ml_off && !src->ml_sym) ? 2 : 1;
 if ((dst->ml_reg&7) == DCC_ASMREG_EDI) score -= (!dst->ml_off && !dst->ml_sym) ? 2 : 1;
 /* Special optimization for _very_ large moves: use 'rep movsb/w/l' */
 if ((src_bytes > dst_bytes ? src_bytes : dst_bytes) >= REPMOV_THRESHOLD(score)) {
  DCCDisp_DoRepMemMovMem(src,src_bytes,dst,dst_bytes,src_unsigned);
  return;
 }
 common_size = src_bytes < dst_bytes ? src_bytes : dst_bytes;
 max_block = size_iter = common_size;
 /* The mov must be performed at run-time. */
 if (max_block >= DCC_TARGET_SIZEOF_POINTER)
  max_block = DCC_TARGET_SIZEOF_POINTER;
#if DCC_TARGET_SIZEOF_POINTER > 4
 else if (max_block >= 4) max_block = 4;
#endif
 else if (max_block >= 2) max_block = 2;
 else if (max_block >= 1) max_block = 1;
 must_pop_temp = 0;
 temp_register = DCCVStack_GetReg(DCC_RC_FORSIZE(max_block),
                                  2|(int)(!src_unsigned && !(common_size&1)));
 if (!temp_register) {
  temp_register = DCC_ASMREG_EAX;
  while ((src_iter.ml_reg != DCC_RC_CONST && (src_iter.ml_reg&7) == temp_register) ||
         (dst_iter.ml_reg != DCC_RC_CONST && (dst_iter.ml_reg&7) == temp_register) ||
          DCC_ASMREG_ISSPTR(temp_register)) {
   ++temp_register;
   temp_register %= 8;
  }
  must_pop_temp = 1;
  temp_register |= DCC_RC_PTRX;
  DCCDisp_RegPush(temp_register);
 }

 while (size_iter >= max_block) {
  DCCDisp_MemMovReg(&src_iter,temp_register);
  DCCDisp_RegMovMem(temp_register,&dst_iter);
  src_iter.ml_off += max_block;
  dst_iter.ml_off += max_block;
  size_iter       -= max_block;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (size_iter&4) {
  temp_register &= ~(DCC_RC_I64);
  assert(temp_register&DCC_RC_I32);
  DCCDisp_MemMovReg(&src_iter,temp_register);
  DCCDisp_RegMovMem(temp_register,&dst_iter);
  src_iter.ml_off += 4;
  dst_iter.ml_off += 4;
 }
#endif
 if (size_iter&2) {
  temp_register &= ~(DCC_RC_I3264);
  assert(temp_register&DCC_RC_I16);
  DCCDisp_MemMovReg(&src_iter,temp_register);
  DCCDisp_RegMovMem(temp_register,&dst_iter);
  src_iter.ml_off += 2;
  dst_iter.ml_off += 2;
 }
 if (size_iter&1) {
  temp_register &= ~(DCC_RC_I3264|DCC_RC_I16);
  assert(temp_register&DCC_RC_I8);
  DCCDisp_MemMovReg(&src_iter,temp_register);
  DCCDisp_RegMovMem(temp_register,&dst_iter);
 }
 /* zero-/sign-extend the remainder. */
 new_dst         = *dst;
 new_dst.ml_off += common_size;
 common_size = dst_bytes-common_size;
 if (common_size && !src_unsigned) {
  /* sign-extend: temp_register = (uint8_t)sign_extend(temp_register); */
  DCCDisp_SignExtendReg(temp_register);
  DCCDisp_ByrMovMem(temp_register,common_size,&new_dst,common_size,1);
  if (must_pop_temp) DCCDisp_PopReg(temp_register);
 } else {
  if (must_pop_temp) DCCDisp_PopReg(temp_register);
  DCCDisp_BytMovMem(0,common_size,&new_dst,common_size,1);
 }
}

PUBLIC void
DCCDisp_MemMovMem(struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 void const *csrc;
 target_siz_t common_size;
 assert(src);
 assert(dst);
 /* Special case: src & dst are equal -> nothing to do here! */
 if (DCCMemLoc_Equal(src,dst)) return;
 /* Special cases: empty source/destination. */
 if (!src_bytes) {
  /* Fill 'dst' with ZERO. */
  DCCDisp_BytMovMem(0,dst_bytes,dst,dst_bytes,1);
  return;
 }
 if (!dst_bytes) return; /* Do nothing for empty destination. */
 common_size = dst_bytes < src_bytes ? dst_bytes : src_bytes;
 /* Special case: Source is known at compile-time in this context. */
 if ((csrc = DCCMemLoc_CompilerData(src,src_bytes)) != NULL) {
  DCCDisp_VecMovMem(csrc,src_bytes,dst,dst_bytes,src_unsigned);
  return;
 }
 /* Fallback. */
 DCCDisp_DoMemMovMem(src,src_bytes,dst,dst_bytes,src_unsigned);
}


PRIVATE void
DCCDisp_DoCstMovMem(struct DCCSymAddr const *__restrict val,
                    struct DCCMemLoc const *__restrict dst,
                    width_t width) {
 /* mov $symaddr, offset(%reg) */
 assert(CHECK_WIDTH(width));
 if (width == 2) t_putb(0x66);
 if (width == 1) t_putb(0xc6);
 else            t_putb(0xc7);
 asm_modmem(0,dst);
 DCCDisp_SymAddr(val,width);
}

PRIVATE void
DCCDisp_VecMovMem_fixed(void const *__restrict src,
                        struct DCCMemLoc const *__restrict dst,
                        target_siz_t n_bytes) {
 struct DCCSymAddr cst;
 struct DCCMemLoc dst_iter = *dst;
 cst.sa_sym = DCC_RC_CONST;
 while (n_bytes >= DCC_TARGET_SIZEOF_POINTER) {
  cst.sa_off = *(target_off_t *)src;
  DCCDisp_DoCstMovMem(&cst,&dst_iter,DCC_TARGET_SIZEOF_POINTER);
  *(uintptr_t *)&src += DCC_TARGET_SIZEOF_POINTER;
  dst_iter.ml_off    += DCC_TARGET_SIZEOF_POINTER;
  n_bytes            -= DCC_TARGET_SIZEOF_POINTER;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  cst.sa_off = (target_off_t)*(uint32_t *)src;
  DCCDisp_DoCstMovMem(&cst,&dst_iter,4);
  *(uintptr_t *)&src += 4;
  dst_iter.ml_off    += 4;
 }
#endif
 if (n_bytes&2) {
  cst.sa_off = (target_off_t)*(uint16_t *)src;
  DCCDisp_DoCstMovMem(&cst,&dst_iter,2);
  *(uintptr_t *)&src += 2;
  dst_iter.ml_off    += 2;
 }
 if (n_bytes&1) {
  cst.sa_off = (target_off_t)*(uint8_t *)src;
  DCCDisp_DoCstMovMem(&cst,&dst_iter,1);
 }
}
PRIVATE void
DCCDisp_BytMovMem_fixed(int src,
                        struct DCCMemLoc const *__restrict dst,
                        target_siz_t n_bytes) {
 struct DCCSymAddr cst;
 struct DCCMemLoc dst_iter = *dst;
 cst.sa_sym = NULL;
#if DCC_TARGET_SIZEOF_POINTER == 8
 cst.sa_off = 0x0101010101010101ull*src;
#else
 cst.sa_off = 0x01010101*src;
#endif
 while (n_bytes >= DCC_TARGET_SIZEOF_POINTER) {
  DCCDisp_DoCstMovMem(&cst,&dst_iter,DCC_TARGET_SIZEOF_POINTER);
  dst_iter.ml_off += DCC_TARGET_SIZEOF_POINTER;
  n_bytes         -= DCC_TARGET_SIZEOF_POINTER;
 }
#if DCC_TARGET_SIZEOF_POINTER > 4
 if (n_bytes&4) {
  DCCDisp_DoCstMovMem(&cst,&dst_iter,4);
  dst_iter.ml_off += 4;
 }
#endif
 if (n_bytes&2) {
  DCCDisp_DoCstMovMem(&cst,&dst_iter,2);
  dst_iter.ml_off += 2;
 }
 if (n_bytes&1) {
  DCCDisp_DoCstMovMem(&cst,&dst_iter,1);
 }
}
PRIVATE void
DCCDisp_ByrMovMem_fixed(rc_t src,
                        struct DCCMemLoc const *__restrict dst,
                        target_siz_t n_bytes) {
 struct DCCMemLoc dst_iter = *dst;
 assert(!(src&(DCC_RC_I16|DCC_RC_I3264)));
 while (n_bytes) {
  DCCDisp_RegMovMem(src,&dst_iter);
  ++dst_iter.ml_off;
  --n_bytes;
 }
}


PUBLIC void
DCCDisp_VecMovMem(void             const *__restrict src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 void *cdst; target_siz_t common_size;
 struct DCCMemLoc new_dst; int filler = 0;
 assert(dst);
 if (!src_bytes) {
  /* Fill 'dst' with ZERO. */
  DCCDisp_BytMovMem(0,dst_bytes,dst,dst_bytes,1);
  return;
 }
 if (!dst_bytes) return; /* Do nothing for empty destination. */
 common_size = dst_bytes < src_bytes ? dst_bytes : src_bytes;
 /* Special case: Move at compile-time. */
 if ((cdst = DCCMemLoc_CompilerAddr(dst,dst_bytes)) != NULL) {
  memmove(cdst,src,common_size);
  if (!src_unsigned && ((uint8_t *)src)[src_bytes-1]&0x80) filler = 0xff; /* sign-extend. */
  *(uintptr_t *)&cdst += common_size;
  common_size = dst_bytes-common_size;
  memset(cdst,filler,common_size);
  return;
 }
 /* Fallback. */
 DCCDisp_VecMovMem_fixed(src,dst,common_size);
 /* zero-/sign-extend the remainder. */
 new_dst         = *dst;
 new_dst.ml_off += common_size;
 dst_bytes -= common_size;
 assert(src_bytes);
 filler = src_unsigned || !(((uint8_t *)src)[src_bytes-1]&0x80) ? 0 : 0xff;
 DCCDisp_BytMovMem(filler,dst_bytes,&new_dst,dst_bytes,1);
}

PUBLIC void
DCCDisp_BytMovMem(int                                src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 void *cdst; target_siz_t common_size;
 struct DCCMemLoc new_dst;
 unsigned int score,filler = 0;
 assert(dst);
 if (!dst_bytes) return; /* Do nothing for empty destination. */
 if (!src_bytes) {
  /* Fill 'dst' with ZERO. */
  DCCDisp_BytMovMem(0,dst_bytes,dst,dst_bytes,1);
  return;
 }
 common_size = dst_bytes < src_bytes ? dst_bytes : src_bytes;
 /* Special case: Move at compile-time. */
 if ((cdst = DCCMemLoc_CompilerAddr(dst,dst_bytes)) != NULL) {
  memset(cdst,src,common_size);
  if (!src_unsigned && (src&0x80)) filler = 0xff; /* sign-extend. */
  new_dst = *dst;
  new_dst.ml_off += common_size;
  common_size = dst_bytes-common_size;
  DCCDisp_BytMovMem(filler,common_size,&new_dst,common_size,1);
  return;
 }
 score = 3;
 if (!DCCVStack_GetRegInuse(DCC_RR_XAX)) --score;
 if ((dst->ml_reg&7) == DCC_ASMREG_EDI) score -= (!dst->ml_off && !dst->ml_sym) ? 2 : 1;
 /* Special optimization for _very_ large moves: use 'rep stosb/w/l' */
 if ((dst_bytes > src_bytes ? dst_bytes : src_bytes) >= REPMOV_THRESHOLD(score)) {
  DCCDisp_DoRepBytMovMem(src,src_bytes,dst,dst_bytes,src_unsigned);
  return;
 }
 /* Fallback. */
 DCCDisp_BytMovMem_fixed(src,dst,common_size);
 /* zero-/sign-extend the remainder. */
 new_dst         = *dst;
 new_dst.ml_off += common_size;
 common_size = dst_bytes-common_size;
 if (common_size && !src_unsigned && (src&0x80)
     ) filler = 0xff; /* sign-extend. */
 /* Fill the remainder. */
 DCCDisp_BytMovMem(filler,common_size,&new_dst,common_size,1);
}

PUBLIC void
DCCDisp_ByrMovMem(rc_t                               src, target_siz_t src_bytes,
                  struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                  int src_unsigned) {
 target_siz_t common_size; unsigned int score;
 struct DCCMemLoc new_dst;
 assert(dst);
 if (!src_bytes) {
  /* Fill 'dst' with ZERO. */
  DCCDisp_BytMovMem(0,dst_bytes,dst,dst_bytes,1);
  return;
 }
 if (!dst_bytes) return; /* Do nothing for empty destination. */
 /* Make sure that 'src' is an 8-bit register class. */
 src = DCCVStack_CastReg(src,src_unsigned,DCC_RC_I8);
 common_size = dst_bytes < src_bytes ? dst_bytes : src_bytes;
 score = 3;
 if ((src&7) == DCC_ASMREG_EAX) --score;
 if ((dst->ml_reg&7) == DCC_ASMREG_EDI) score -= (!dst->ml_off && !dst->ml_sym) ? 2 : 1;
 /* Special optimization for _very_ large moves: use 'rep stosb/w/l' */
 if ((dst_bytes > src_bytes ? dst_bytes : src_bytes) >= REPMOV_THRESHOLD(score)) {
  DCCDisp_DoRepByrMovMem(src,src_bytes,dst,dst_bytes,src_unsigned);
  return;
 }
 /* Fallback. */
 DCCDisp_ByrMovMem_fixed(src,dst,common_size);
 /* zero-/sign-extend the remainder. */
 new_dst         = *dst;
 new_dst.ml_off += common_size;
 common_size = dst_bytes-common_size;
 if (common_size && !src_unsigned) {
  /* sign-extend: src = (uint8_t)sign_extend(src); */
  DCCDisp_SignExtendReg(src);
  DCCDisp_ByrMovMem(src,common_size,&new_dst,common_size,1);
 } else {
  DCCDisp_BytMovMem(0,common_size,&new_dst,common_size,1);
 }
}




PUBLIC void
DCCDisp_RegMovReg(rc_t src, rc_t dst, int src_unsigned) {
 rc_t c_src,c_dst;
 c_src = src&DCC_RC_MASK;
 c_dst = dst&DCC_RC_MASK;
 if (c_src&DCC_RC_I16) c_src |= DCC_RC_I8;
 if (c_dst&DCC_RC_I16) c_dst |= DCC_RC_I8;
 if (c_src == c_dst) {
  /* Check for special case: dst&src are identical. */
  if ((dst&7) == (src&7)) return;
  /* Move in same storage class. */
  if ((c_src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
  t_putb(0x88+!!(c_src&DCC_RC_I16));
  goto modreg;
 }
 if (c_src > c_dst) {
  /* Truncate: Simply copy from a lower order register,
   *           but apply a mask for EDI/ESI. */
  if (c_dst&(DCC_RC_I16|DCC_RC_I3264)) {
   /* Special case: Move to lower-order storage class. */
   if ((dst&7) == (src&7)) return;
   /* Destination class isn't 8-bit, meaning a lower-order register is always available. */
   if (!(c_dst&DCC_RC_I3264)) t_putb(0x66);
   t_putb(0x89);
  } else {
   /* Special case: 'dst' is simply a lower-order register for 'src' */
   if ((dst&3) == (src&3)) return;
   if (!(src&4)) {
    /* 8-bit mode, but the source register has an 8-bit equivalent. */
    t_putb(0x88);
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
    t_putb(0x66);
    t_putb(0x89);
    asm_modreg(src&7,temp_register);

    /* movb %temp_register, %dst */
    t_putb(0x88);
    asm_modreg(temp_register,dst&7);
    return;
   }
  }
 } else if (c_dst&DCC_RC_I32) {
  rc_t temp;
  t_putb(0x0f);
  /* Expand: Use movzx / movsx */
  if (src_unsigned) {
   if (c_src&DCC_RC_I16) t_putb(0xb7);
   else upcast1632_u:    t_putb(0xb6);
  } else {
   if (c_src&DCC_RC_I16) t_putb(0xbf);
   else upcast1632_s:    t_putb(0xbe);
  }
  /* Must swap registers because movzx uses the second operand as r/m argument. */
  temp = src,src = dst,dst = temp;
 } else {
  t_putb(0x66);
  t_putb(0x0f);
  if (src_unsigned) goto upcast1632_u;
  else              goto upcast1632_s;
 }
modreg:
 asm_modreg(src&7,dst&7);
}


PUBLIC void
DCCDisp_LocMovReg(struct DCCMemLoc const *__restrict val, rc_t dst) {
 assert(val);
 if (val->ml_reg == DCC_RC_CONST) {
  DCCDisp_CstMovReg(&val->ml_sad,dst);
 } else {
  DCCDisp_RegMovReg(val->ml_reg,dst,1);
  DCCDisp_AddReg(&val->ml_sad,dst);
 }
}
PUBLIC void
DCCDisp_CstMovRegRaw(struct DCCSymAddr const *__restrict val, rc_t dst) {
 uint8_t opno;
 assert(val);
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
 /* mov $symaddr, %reg */
 opno = (dst&DCC_RC_I16) ? 0xb8 : 0xb0;
 opno += dst&7;
 t_putb(opno);
      if (dst&DCC_RC_I32) DCCDisp_SymAddr32(val);
 else if (dst&DCC_RC_I16) DCCDisp_SymAddr16(val);
 else                     DCCDisp_SymAddr8 (val);
}
PUBLIC void
DCCDisp_CstMovReg(struct DCCSymAddr const *__restrict val, rc_t dst) {
 assert(val);
 if (!val->sa_off) {
  if (!val->sa_sym) {
   /* Special case: mov $0, %reg --> xor %reg, %reg */
   DCCDisp_RegBinReg('^',dst,dst,1);
   return;
#if 0
  } else if (val->sa_off == 1) {
   /* Special case: movl $1, %reg --> xor %reg, %reg; inc %reg */
   DCCDisp_RegBinReg('^',dst,dst,1);
   DCCDisp_UnaryReg(TOK_INC,dst);
   return;
  } else if (val->sa_off == reg_max(dst)) {
   /* Special case: movl $0xffffffff, %reg --> xor %reg, %reg; dec %reg */
   DCCDisp_RegBinReg('^',dst,dst,1);
   DCCDisp_UnaryReg(TOK_DEC,dst);
   return;
#endif
  }
 }
 /* mov $symaddr, %reg */
 DCCDisp_CstMovRegRaw(val,dst);
}
LOCAL target_off_t
def_reloc(struct DCCSymAddr const *__restrict expr,
          struct DCCSection *__restrict target_section,
          target_ptr_t target_offset, rel_t rel) {
 struct DCCSym *xsym; target_off_t xval;
 assert(expr);
 xval = expr->sa_off;
 if ((xsym = expr->sa_sym) != NULL) {
  while ((DCCSym_ASSERT(xsym),xsym->sy_alias)) xsym = xsym->sy_alias;
#if 1
  if (DCCSym_SECTION(xsym) && DCCSection_HASBASE(DCCSym_SECTION(xsym))) {
   /* The associated section has a fixed base associated with it.
    * >> We don't need to emit a relocation, because
    *    we can simply add the base value here!
    */
   xval += (target_off_t)xsym->sy_addr;
   xval += (target_off_t)(target_ptr_t)DCCSym_SECTION(xsym)->sc_base;
  } else
#endif
  {
   /* Generate a relocation at the current address.
    * >> This relocation must later add its base to the symbol. */
   DCCSection_Putrel(target_section,target_offset,rel,xsym);
  }
 }
 return xval;
}




PUBLIC void
DCCDisp_CstMovMem(struct DCCSymAddr const *__restrict val,
                  struct DCCMemLoc  const *__restrict dst,
                  width_t width) {
 assert(val),assert(dst);
 assert(CHECK_WIDTH(width));
 if ((compiler.c_flags&DCC_COMPILER_FLAG_SINIT) && dst->ml_sym) {
  struct DCCSym *target_sym = dst->ml_sym;
  while ((DCCSym_ASSERT(target_sym),target_sym->sy_alias)) target_sym = target_sym->sy_alias;
  if (DCCSym_SECTION(target_sym)) {
   struct DCCSection *target_sec;
   uint8_t *target_data;
   target_ptr_t target_offset;
   /* Directly write to target memory (at compile-time).
    * >> This is used for static initializer of global data. */
   assert(!target_sym->sy_alias);
   target_sec = DCCSym_SECTION(target_sym);
   target_offset  = target_sym->sy_addr;
   target_offset += (target_ptr_t)dst->ml_off;
   DCCSection_TBEGIN(target_sec);
   target_data = (uint8_t *)DCCSection_GetText(target_sec,target_offset,width);
   DCCSection_TEND(target_sec);
   if unlikely(!target_data) return;
   {
    /* Add a relocation and store the symbol. */
    rel_t rel =
#if DCC_TARGET_SIZEOF_POINTER >= 8
                width == 8 ? DCC_R_DATA_64 :
#endif
                width == 4 ? DCC_R_DATA_32 :
                width == 2 ? DCC_R_DATA_16 :
                             DCC_R_DATA_8;
    target_off_t v = def_reloc(val,target_sec,target_offset,rel);
    /* Store the initial value inside the target data. */
#if DCC_TARGET_SIZEOF_POINTER >= 8
         if (width == 8) *(int64_t *)target_data = (int64_t)v;
    else
#endif
         if (width == 4) *(int32_t *)target_data = (int32_t)v;
    else if (width == 2) *(int16_t *)target_data = (int16_t)v;
    else                 *(int8_t  *)target_data = (int8_t)v;
   }
   return;
  }
 }
 DCCDisp_DoCstMovMem(val,dst,width);
}




PUBLIC void
DCCDisp_LocPush(struct DCCMemLoc const *__restrict addr) {
 assert(addr);
 if (addr->ml_reg)
  DCCDisp_RegPush(DCCDisp_AddProtReg(&addr->ml_sad,addr->ml_reg));
 else DCCDisp_CstPush(&addr->ml_sad,DCC_TARGET_SIZEOF_POINTER);
}
PUBLIC void
DCCDisp_CstPush(struct DCCSymAddr const *__restrict val,
                width_t width) {
 assert(val);
 assert(CHECK_WIDTH(width));
 if (width == 8 && !val->sa_sym) {
  struct DCCSymAddr temp;
  /* Special case: push 64-bit immediate value. */
  temp.sa_sym = NULL;
  temp.sa_off = (target_off_t)((int32_t *)&val->sa_off)[1];
  DCCDisp_CstPush(&temp,4);
  temp.sa_off = (target_off_t)((int32_t *)&val->sa_off)[0];
  DCCDisp_CstPush(&temp,4);
  return;
 }
 if (width == 2) t_putb(0x66);
 if (width == 1) t_putb(0x6a);
 else            t_putb(0x68);
 DCCDisp_SymAddr(val,width);
}
PUBLIC void
DCCDisp_MemPush(struct DCCMemLoc const *__restrict src,
                target_siz_t n_bytes) {
 struct DCCMemLoc temp;
 if (n_bytes == 1) {
  /* Special handling for 8-bit push. */
  temp.ml_off = 0;  
  temp.ml_sym = NULL;  
  temp.ml_reg = DCC_RR_XSP;  
  DCCDisp_UnaryReg(TOK_DEC,DCC_RR_XSP);
  DCCDisp_MemMovMem(src,1,&temp,1,1);
  return;
 }
 if (n_bytes == 2 || n_bytes == 4) {
  if (n_bytes == 2) t_putb(0x66);
  t_putb(0xff);
  asm_modmem(6,src);
  return;
 }
#ifdef MOVE_DATVEC_THRESHOLD
 if (n_bytes > MOVE_DATVEC_THRESHOLD) {
  struct DCCSymAddr sizeval;
  sizeval.sa_off = (target_off_t)n_bytes;
  sizeval.sa_sym = NULL;
  temp.ml_off    = 0;  
  temp.ml_sym    = NULL;  
  temp.ml_reg    = DCC_RR_XSP;  
  /* memcpy the block of data onto the stack. */
  DCCDisp_CstBinReg('-',&sizeval,DCC_RR_XSP,1);
  DCCDisp_MemMovMem(src,n_bytes,&temp,n_bytes,1);
  return;
 }
#endif
 temp = *src; /* FALLBACK: Push each value individually. */
 while (n_bytes >= 4) { DCCDisp_MemPush(&temp,4); temp.ml_off += 4,n_bytes -= 4; }
 if    (n_bytes &  2) { DCCDisp_MemPush(&temp,2); temp.ml_off += 2; }
 if    (n_bytes &  1) { DCCDisp_MemPush(&temp,1); }
}
PUBLIC void
DCCDisp_MemRevPush(struct DCCMemLoc const *__restrict src,
                   target_siz_t n_bytes) {
 struct DCCMemLoc temp;
 if (n_bytes == 1) {
  /* Special handling for 8-bit push. */
  temp.ml_off = 0;  
  temp.ml_sym = NULL;  
  temp.ml_reg = DCC_RR_XSP;  
  DCCDisp_UnaryReg(TOK_DEC,DCC_RR_XSP);
  DCCDisp_MemMovMem(src,1,&temp,1,1);
  return;
 }
 if (n_bytes == 2 || n_bytes == 4) {
  if (n_bytes == 2) t_putb(0x66);
  t_putb(0xff);
  asm_modmem(6,src);
  return;
 }
#ifdef MOVE_DATVEC_THRESHOLD
 if (n_bytes > MOVE_DATVEC_THRESHOLD) {
  struct DCCSymAddr sizeval;
  sizeval.sa_off = (target_off_t)n_bytes;
  sizeval.sa_sym = NULL;
  temp.ml_off    = 0;  
  temp.ml_sym    = NULL;  
  temp.ml_reg    = DCC_RR_XSP;  
  /* memcpy the block of data onto the stack. */
  DCCDisp_CstBinReg('-',&sizeval,DCC_RR_XSP,1);
  DCCDisp_MemMovMem(src,n_bytes,&temp,n_bytes,1);
  return;
 }
#endif
 temp = *src; /* FALLBACK: Push each value individually. */
 temp.ml_off += n_bytes;
 /* Push data in reverse order. (Required to fix the downwards-growing stack) */
#if DCC_TARGET_BYTEORDER == 1234
 if    (n_bytes &  1) { temp.ml_off -= 1; DCCDisp_MemPush(&temp,1); }
 if    (n_bytes &  2) { temp.ml_off -= 2; DCCDisp_MemPush(&temp,2); }
 while (n_bytes >= 4) { temp.ml_off -= 4; DCCDisp_MemPush(&temp,4); n_bytes -= 4; }
#else
 while (n_bytes >= 4) { temp.ml_off -= 4; DCCDisp_MemPush(&temp,4); n_bytes -= 4; }
 if    (n_bytes &  2) { temp.ml_off -= 2; DCCDisp_MemPush(&temp,2); }
 if    (n_bytes &  1) { temp.ml_off -= 1; DCCDisp_MemPush(&temp,1); }
#endif
}
PUBLIC void
DCCDisp_MemPushs(struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                 target_siz_t n_bytes, int src_unsigned) {
 if (src_bytes >= n_bytes) DCCDisp_MemPush(src,n_bytes);
 else {
  struct DCCMemLoc stack_target;
  stack_target.ml_reg = DCC_RR_XSP;
  stack_target.ml_sym = NULL;
  stack_target.ml_off = -(target_off_t)n_bytes;
  DCCDisp_AddReg(&stack_target.ml_sad,DCC_RR_XSP);
  stack_target.ml_off = 0;
  DCCDisp_MemMovMem(src,src_bytes,&stack_target,
                    n_bytes,src_unsigned);
 }
}

PUBLIC void
DCCDisp_VecPush(void const *__restrict src, target_siz_t n_bytes) {
 uint8_t const *iter;
 if (n_bytes >= PUSHMEM_MOV_THRESHOLD) {
  /* Optimization for _very_ large sizes. */
  struct DCCMemLoc stack_target;
  stack_target.ml_reg = DCC_RR_XSP;
  stack_target.ml_sym = NULL;
  stack_target.ml_off = -(target_off_t)n_bytes;
  DCCDisp_AddReg(&stack_target.ml_sad,DCC_RR_XSP);
  stack_target.ml_off = 0;
  DCCDisp_VecMovMem(src,n_bytes,&stack_target,n_bytes,1);
  return;
 }
 iter = (uint8_t const *)src+n_bytes;
 while (n_bytes >= 4) {
  iter    -= 4;
  n_bytes -= 4;
  t_putb(0x68);
  t_putl(*(uint32_t *)iter);
 }
 if (n_bytes&2) {
  iter    -= 2;
  n_bytes -= 2;
  t_putb(0x66);
  t_putb(0x68);
  t_putw(*(uint16_t *)iter);
 }
 if (n_bytes&1) {
  iter -= 1;
  t_putb(0x6a);
  t_putb(*(uint8_t *)iter);
 }
}
PUBLIC void
DCCDisp_BytPush(int src, target_siz_t n_bytes) {
 uint8_t data[4];
 if (n_bytes >= PUSHMEM_MOV_THRESHOLD) {
  /* Optimization for _very_ large sizes. */
  struct DCCSymAddr temp;
  struct DCCMemLoc esp_target;
  temp.sa_off = (target_off_t)n_bytes;
  temp.sa_sym = NULL;
  /* Allocate stack memory */
  DCCDisp_CstBinReg('-',&temp,DCC_RR_XSP,1);
  esp_target.ml_reg = DCC_RR_XSP;
  esp_target.ml_off = 0;
  esp_target.ml_sym = NULL;
  DCCDisp_BytMovMem(src,n_bytes,&esp_target,n_bytes,1);
  return;
 }
 data[0] = data[1] = data[2] = data[3] = (uint8_t)src;
 while (n_bytes >= 4) {
  n_bytes -= 4;
  t_putb(0x68);
  t_putl(*(uint32_t *)data);
 }
 if (n_bytes&2) {
  t_putb(0x66);
  t_putb(0x68);
  t_putw(*(uint16_t *)data);
 }
 if (n_bytes&1) {
  t_putb(0x6a);
  t_putb(*(uint8_t *)data);
 }
}
PUBLIC void
DCCDisp_ByrPush(rc_t src, target_siz_t n_bytes) {
 struct DCCSymAddr temp;
 struct DCCMemLoc esp_target;
 temp.sa_off = (target_off_t)n_bytes;
 temp.sa_sym = NULL;
 /* Allocate stack memory */
 DCCDisp_CstBinReg('-',&temp,DCC_RR_XSP,1);
 esp_target.ml_reg = DCC_RR_XSP;
 esp_target.ml_off = 0;
 esp_target.ml_sym = NULL;
 DCCDisp_ByrMovMem(src,n_bytes,&esp_target,n_bytes,1);
}
PUBLIC void
DCCDisp_RegPush(rc_t src) {
 if (!(src&DCC_RC_I16)) {
  struct DCCMemLoc dst;
  assert(src&DCC_RC_I8);
  /* Special handling for 8-bit push. */
  DCCDisp_UnaryReg(TOK_DEC,DCC_RR_XSP);
  dst.ml_reg = DCC_RR_XSP;
  dst.ml_off = 0;
  dst.ml_sym = NULL;
  DCCDisp_RegMovMem(src,&dst);
  return;
 }
 assert((src&DCC_RC_MASK) != DCC_RC_I8);
 if (!(src&DCC_RC_I32)) t_putb(0x66);
 t_putb(0x50+(src&7));
}
PUBLIC void
DCCDisp_PopMem(struct DCCMemLoc const *__restrict dst,
               width_t width) {
 assert(dst);
 assert(CHECK_WIDTH(width));
 if (width == 1) {
  struct DCCMemLoc src;
  /* Special handling for 1-byte pop. */
  src.ml_reg = DCC_RR_XSP;
  src.ml_sym = NULL;
  src.ml_off = 0;
  DCCDisp_MemMovMem(&src,1,dst,1,1);
  DCCDisp_UnaryReg('+',DCC_RR_XSP);
  return;
 }
 if (width == 2) t_putb(0x66);
 t_putb(0x8f);
 asm_modmem(0,dst);
}
PUBLIC void
DCCDisp_PopReg(rc_t dst) {
 if (!(dst&DCC_RC_I16)) {
  /* Special handling for 8-bit pop. */
  struct DCCMemLoc src;
  src.ml_reg = DCC_RR_XSP;
  src.ml_off = 0;
  src.ml_sym = NULL;
  DCCDisp_MemMovReg(&src,dst);
  DCCDisp_UnaryReg(TOK_INC,DCC_RR_XSP);
  return;
 }
 assert((dst&DCC_RC_MASK) != DCC_RC_I8);
 if (!(dst&DCC_RC_I32)) t_putb(0x66);
 t_putb(0x58+(dst&7));
}


PUBLIC void
DCCDisp_RegCMovReg(test_t t, rc_t src,
                   rc_t dst, int src_unsigned) {
 rc_t used_src = src;
 assert(t <= 0xf);
 if ((src&DCC_RC_MASK) != (dst&DCC_RC_MASK)) {
  /* mov between different classes. */
  used_src = DCCVStack_GetRegOf(dst&DCC_RC_MASK,(uint8_t)~
                               ((1 << (src&7))|(1 << (dst&7))));
  DCCDisp_RegMovReg(src,used_src,src_unsigned);
 }
 if (!(dst&(DCC_RC_I16|DCC_RC_I3264))) {
  t_putb(0x70+(t^DCC_TEST_NBIT)); /* Special case: 8-bit conditional mov */
  t_putb(2); /* Exact byte count from next byte until end of mov instruction. */
  t_putb(0x88); /* Place a normal register-jump. */
  asm_modreg(src&7,dst&7);
  return;
 }
 /* cmovcc %src, %dst */
 if (!(dst&DCC_RC_I32)) t_putb(0x66);
 t_putb(0x0f);
 t_putb(0x40+t);
 asm_modreg(dst&7,src&7);
}
PUBLIC void
DCCDisp_MemCMovReg(test_t t, struct DCCMemLoc const *__restrict src,
                   rc_t dst) {
 assert(t <= 0xf);
 if (!(dst&(DCC_RC_I16|DCC_RC_I3264))) {
  struct DCCMemLoc jmp;
  jmp.ml_sym = DCCUnit_AllocSym();
  if unlikely(!jmp.ml_sym) return;
  DCCDisp_LocJcc(t^DCC_TEST_NBIT,&jmp);
  DCCDisp_MemMovReg(src,dst);
  t_defsym(jmp.ml_sym);
  return;
 }
 /* cmovcc !src, %dst */
 if (!(dst&DCC_RC_I32)) t_putb(0x66);
 t_putb(0x0f);
 t_putb(0x40+t);
 asm_modmem(dst&7,src);
}


DCC_DECL_END

#endif /* !GUARD_DCC_GEN_MOV_C_INL */