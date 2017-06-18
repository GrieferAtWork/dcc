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
#ifndef GUARD_DCC_GEN_MISC_C_INL
#define GUARD_DCC_GEN_MISC_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#include "x86_util.h"

DCC_DECL_BEGIN

PUBLIC void
DCCDisp_RegXchReg(rc_t a, rc_t b) {
 rc_t temp;
 if unlikely(a == b) return;
 assert((a&(DCC_RC_MASK&~(DCC_RC_I8))) ==
        (b&(DCC_RC_MASK&~(DCC_RC_I8))));
 if ((a&(DCC_RC_I3264|DCC_RC_I16)) == DCC_RC_I16) t_putb(0x66);
 if ((b&DCC_RI_MASK) == DCC_ASMREG_EAX) { temp = a,a = b,b = temp; }
 if ((a&DCC_RI_MASK) == DCC_ASMREG_EAX) { t_putb(0x90+(b&7)); return; } /* Special case: Swap EAX. */
 /* fallback: swap any register. */
 t_putb(0x86+!!(a&DCC_RC_I16));
 asm_modreg(a&7,b&7);
}

PUBLIC void
DCCDisp_RegXchMem(rc_t a, struct DCCMemLoc const *__restrict b) {
 if ((a&(DCC_RC_I3264|DCC_RC_I16)) == DCC_RC_I16) t_putb(0x66);
 t_putb(0x86+!!(a&DCC_RC_I16));
 asm_modmem(a&7,b);
}

PUBLIC void
DCCDisp_MemXchMem(struct DCCMemLoc const *__restrict a,
                  struct DCCMemLoc const *__restrict b,
                  target_siz_t n_bytes) {
 rc_t temp;
 struct DCCMemLoc a_iter,b_iter;
#ifdef DCC_RC_I64
      if (n_bytes >= 8) temp = DCC_RC_I64;
 else
#endif
      if (n_bytes >= 4) temp = DCC_RC_I32;
 else if (n_bytes >= 2) temp = DCC_RC_I16;
 else if (n_bytes >= 1) temp = DCC_RC_I8;
 else return;
 temp = DCCVStack_GetReg(temp,!(n_bytes&1));
 a_iter = *a,b_iter = *b;
 for (;;) {
  target_siz_t part;
  if (n_bytes >= DCC_TARGET_SIZEOF_ARITH_MAX)
      part = DCC_TARGET_SIZEOF_ARITH_MAX;
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
  else if (n_bytes >= 4) part = 4,temp &= ~(DCC_RC_I64);
#endif
  else if (n_bytes >= 2) part = 2,temp &= ~(DCC_RC_I32);
  else if (n_bytes >= 1) part = 1,temp &= ~(DCC_RC_I16);
  else break;
  DCCDisp_MemMovReg(&a_iter,temp);
  DCCDisp_RegXchMem(temp,&b_iter);
  DCCDisp_RegMovMem(temp,&a_iter);
  a_iter.ml_off += part;
  b_iter.ml_off += part;
  n_bytes       -= part;
 }
}


PUBLIC void DCCDisp_Fence(tok_t kind) {
 switch (kind) {
 case DCC_FENCE_BREAKPOINT: t_putb(0xcc); break; /* int $3 */
 case DCC_FENCE_UNREACHABLE:
  if (!(compiler.c_flags&DCC_COMPILER_FLAG_NOUNREACH)) {
 case DCC_FENCE_TRAP: t_putb(0x0f),t_putb(0x0b); /* ud2 */
  }
  /* Setup the flags to mirror the dead/unreachable state. */
  compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  break;
#if DCC_TARGET_HASF(F_SSE2)
 case DCC_FENCE_SFENCE: t_putb(0x0f),t_putb(0xae),asm_modreg(7,0); break; /* sfence */
 case DCC_FENCE_LFENCE: t_putb(0x0f),t_putb(0xae),asm_modreg(5,0); break; /* lfence */
 case DCC_FENCE_MFENCE: t_putb(0x0f),t_putb(0xae),asm_modreg(6,0); break; /* mfence */
#else
 case DCC_FENCE_SFENCE: break;
 case DCC_FENCE_LFENCE: break;
 case DCC_FENCE_MFENCE: break;
#endif
 default: break;
 }
}

PUBLIC void
DCCDisp_BSwapReg(rc_t dst) {
 if (dst&DCC_RC_I32) {
  /* swap 32-bit register. */
  /* bswap %dst */
  t_putb(0x0f);
  t_putb(0xc8+(dst&7));
 } else if (dst&DCC_RC_I16) {
  /* swap 16-bit register. */
  if (dst&4) {
   rc_t temp = DCCVStack_GetReg(DCC_RC_I16,0);
   /* TODO: 'rorw $8, %dst' */
   assert(!(temp&4));
   DCCDisp_RegMovReg(dst,temp,1);
   DCCDisp_BSwapReg(temp);
   DCCDisp_RegMovReg(temp,dst,1);
  } else {
   assert(dst&DCC_RC_I8);
   assert(dst&DCC_RC_I16);
   /* The lo/hi bytes of the 4 basic general-purpose registers can be
    * addressed individually, meaning that if 'dst' is one of them,
    * we can simply use 'xchg' to swap them. */
   DCCDisp_RegXchReg(dst&~(DCC_RC_I16),DCC_RC_I8|((dst+4)&7));
  }
 }
}

PUBLIC void
DCCDisp_BSwapMem(struct DCCMemLoc const *__restrict dst,
                 target_siz_t n_bytes) {
 struct DCCMemLoc a,b;
 rc_t temp;
 /* Check for small/special cases. */
 switch (n_bytes) {
 case 0: case 1: return;
  if (DCC_MACRO_FALSE) { case 2: temp = DCC_RC_I16; }
  if (DCC_MACRO_FALSE) { case 4: temp = DCC_RC_I32; }
  temp = DCCVStack_GetReg(temp,1);
  DCCDisp_MemMovReg(dst,temp);
  DCCDisp_BSwapReg(temp);
  DCCDisp_RegMovMem(temp,dst);
  return;
 default: break;
 }
 b = a = *dst;
 b.ml_off += n_bytes;
 temp = DCCVStack_GetReg(DCC_RC_PTR,1);
 while (n_bytes >= 2) {
  target_siz_t part = n_bytes/2;
  if (part > DCC_TARGET_SIZEOF_ARITH_MAX)
      part = DCC_TARGET_SIZEOF_ARITH_MAX;
       if (part < 2) temp &= ~(DCC_RC_I3264|DCC_RC_I16);
  else if (part < 4) temp &= ~(DCC_RC_I3264);
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
  else if (part < 8) temp &= ~(DCC_RC_I64);
#endif
  b.ml_off -= part;
  DCCDisp_MemMovReg(&a,temp);  /* mov   !a, %temp */
  DCCDisp_BSwapReg(temp);      /* bswap %temp     */
  DCCDisp_RegXchMem(temp,&b);  /* xchg  %temp, !b */
  DCCDisp_BSwapReg(temp);      /* bswap %temp     */
  DCCDisp_RegMovMem(temp,&a);  /* mov   %temp, !a */
  a.ml_off += part;
  n_bytes  -= part*2;
 }
}

LOCAL void DCCDisp_BsfReg(int reverse, rc_t src, rc_t dst) {
 if (dst&(DCC_RC_I16|DCC_RC_I32)) {
  src = DCCVStack_CastReg(src,1,dst&DCC_RC_MASK);
  if (!(dst&DCC_RC_I32)) t_putb(0x66);
  t_putb(0x0f);
  t_putb(0xbc+!!reverse);
  asm_modreg(dst&7,src&7);
 } else {
  rc_t temp = DCCVStack_GetReg(DCC_RC_I16,1);
  DCCDisp_BsfReg(reverse,src,temp);
  DCCDisp_RegMovReg(temp,dst,1);
 }
}
LOCAL void DCCDisp_BsfMem(int reverse, struct DCCMemLoc const *__restrict src,
                          rc_t dst, width_t width) {
 if (width == 2 || width == 4) {
  rc_t used_dst = dst;
  if (width == 2) {
   /* Use an intermediate register for non-overlapping 16/32->8 bit scan. */
   if (!(dst&(DCC_RC_I16|DCC_RC_I3264))
       ) used_dst = DCCVStack_GetReg(width == 2 ? DCC_RC_I16 : DCC_RC_I32,0);
   t_putb(0x66);
  }
  t_putb(0x0f);
  t_putb(0xbc+!!reverse);
  asm_modmem(used_dst&7,src);
  if ((used_dst&7) != (dst&7)) DCCDisp_RegMovReg(used_dst,dst,1);
 } else {
  rc_t temp = DCCVStack_GetReg(DCC_RC_I16,1);
  DCCDisp_BsfMem(reverse,src,temp,width);
  DCCDisp_RegMovReg(temp,dst,1);
 }
}

PRIVATE void
DCCDisp_FFSMem_fixed(struct DCCMemLoc const *__restrict src,
                     rc_t dst, width_t width) {
 rc_t temp;
 /*    mov   $-1, %temp */
 /*    bsf   %src, %dst */
 /*    cmovz %temp, %dst f */
 /*    inc   %dst */
 temp = DCCVStack_GetRegOf(dst&DCC_RC_MASK,(uint8_t)~(1 << (dst&7)));
 DCCDisp_IntMovReg(-1,temp);
 DCCDisp_BsfMem(0,src,dst,width);
 DCCDisp_RegCMovReg(DCC_TEST_Z,temp,dst,1);
 DCCDisp_UnaryReg(TOK_INC,dst);
}
PUBLIC void
DCCDisp_FFSReg(rc_t src, rc_t dst) {
 rc_t temp;
 /*    mov   $-1, %temp */
 /*    bsf   %src, %dst */
 /*    cmovz %temp, %dst f */
 /*    inc   %dst */
 temp = DCCVStack_GetRegOf(dst&DCC_RC_MASK,(uint8_t)~
                          ((1 << (src&7))|(1 << (dst&7))));
 DCCDisp_IntMovReg(-1,temp);
 DCCDisp_BsfReg(0,src,dst);
 DCCDisp_RegCMovReg(DCC_TEST_Z,temp,dst,1);
 DCCDisp_UnaryReg(TOK_INC,dst);
}
PUBLIC void
DCCDisp_FFSRegs(rc_t src, rc_t src2, rc_t dst) {
 if (src2 == DCC_RC_CONST) {
  DCCDisp_FFSReg(src,dst);
 } else {
  struct DCCMemLoc jmp;
  /*    bsf %src, %dst */
  /*    jnz 1f */
  /*    bsf %src2, %dst */
  /*    jnz 1f */
  /*    mov -1, %dst */
  /* 1: inc %dst */
  jmp.ml_sym = DCCUnit_AllocSym();
  if unlikely(!jmp.ml_sym) return;
  jmp.ml_reg = DCC_RC_CONST;
  jmp.ml_off = 0;
  DCCDisp_BsfReg(0,src,dst);
  DCCDisp_LocJcc(DCC_TEST_NZ,&jmp);
  DCCDisp_BsfReg(0,src2,dst);
  DCCDisp_LocJcc(DCC_TEST_NZ,&jmp);
  DCCDisp_IntMovReg(-1,dst);
  t_defsym(jmp.ml_sym);
  DCCDisp_UnaryReg(TOK_INC,dst);
 }
}
PUBLIC void
DCCDisp_FFSMem(struct DCCMemLoc const *__restrict src,
               rc_t dst, target_siz_t n_bytes) {
 /* Special optimizations for small/empty sources. */
 switch (n_bytes) {
 case 0: DCCDisp_IntMovReg(0,dst); return;
 case 1: case 2: case 4: DCCDisp_FFSMem_fixed(src,dst,n_bytes); return;
 default: break;
 }
 {
  struct DCCMemLoc jmp,src_iter = *src;
  jmp.ml_sym = DCCUnit_AllocSym();
  if unlikely(!jmp.ml_sym) return;
  jmp.ml_reg = DCC_RC_CONST;
  jmp.ml_off = 0;
  /*    bsf 0(%src), %dst */
  /*    jnz 1f */
  /*    bsf 4(%src2), %dst */
  /*    jnz 1f */
  /*    bsf 8(%src2), %dst */
  /*    jnz 1f */
  /*    ... */
  /*    mov -1, %dst */
  /* 1: inc %dst */
  while (n_bytes) {
   target_siz_t part;
        if (n_bytes >= 4) part = 4;
   else if (n_bytes >= 2) part = 2;
   else                   part = 1;
   DCCDisp_BsfMem(0,&src_iter,dst,part);
   DCCDisp_LocJcc(DCC_TEST_NZ,&jmp);
   src_iter.ml_off += part;
   n_bytes         -= part;
  }
  DCCDisp_IntMovReg(-1,dst);
  t_defsym(jmp.ml_sym);
  DCCDisp_UnaryReg(TOK_INC,dst);
 }
}


PRIVATE void
DCCDisp_CLZMem_fixed(struct DCCMemLoc const *__restrict src,
                     rc_t dst, width_t width) {
 struct DCCSymAddr val;
 /*    bsr   !src, %dst */
 /*    xor   $(width*8-1), %dst */
 DCCDisp_BsfMem(1,src,dst,width);
 val.sa_off = (target_off_t)(width*8-1);
 val.sa_sym = NULL;
 DCCDisp_CstBinReg('^',&val,dst,1);
}
PUBLIC void
DCCDisp_CLZReg(rc_t src, rc_t dst) {
 struct DCCSymAddr val;
 /*    bsr   %src, %dst */
 /*    xor   $(width*8-1), %dst */
 DCCDisp_BsfReg(1,src,dst);
 val.sa_sym = NULL;
#ifdef DCC_RC_I64
      if (src&DCC_RC_I64) val.sa_off = 63;
 else
#endif
      if (src&DCC_RC_I32) val.sa_off = 31;
 else if (src&DCC_RC_I16) val.sa_off = 15;
 else                     val.sa_off = 7;
 DCCDisp_CstBinReg('^',&val,dst,1);
}
PUBLIC void
DCCDisp_CLZRegs(rc_t src, rc_t src2, rc_t dst) {
 if (src2 == DCC_RC_CONST) {
  DCCDisp_CLZReg(src,dst);
 } else {
  struct DCCSymAddr val;
  struct DCCMemLoc jmp;
  /*    bsr %src, %dst */
  /*    jnz 1f */
  /*    bsr %src2, %dst */
  /*    add $32, %dst */
  /* 1: xor $31, %dst */
  jmp.ml_sym = DCCUnit_AllocSym();
  if unlikely(!jmp.ml_sym) return;
  jmp.ml_reg = DCC_RC_CONST;
  jmp.ml_off = 0;
  DCCDisp_BsfReg(1,src,dst);
  DCCDisp_LocJcc(DCC_TEST_NZ,&jmp);
  DCCDisp_BsfReg(1,src2,dst);
  val.sa_sym = NULL,val.sa_off = 32;
  DCCDisp_CstBinReg('+',&val,dst,1);
  t_defsym(jmp.ml_sym);
  val.sa_sym = NULL,val.sa_off = 31;
  DCCDisp_CstBinReg('^',&val,dst,1);
 }
}
PUBLIC void
DCCDisp_CLZMem(struct DCCMemLoc const *__restrict src,
               rc_t dst, target_siz_t n_bytes) {
 /* Special optimizations for small/empty sources. */
 switch (n_bytes) {
 case 0: DCCDisp_IntMovReg(0,dst); return;
 case 1: case 2: case 4: DCCDisp_CLZMem_fixed(src,dst,n_bytes); return;
 default: break;
 }
 {
  struct DCCSymAddr val; target_off_t start_offset;
  struct DCCMemLoc jmp,jmp_end,src_iter = *src;
  if unlikely((jmp.ml_sym = DCCUnit_AllocSym()) == NULL) return;
  if unlikely((jmp_end.ml_sym = DCCUnit_AllocSym()) == NULL) return;
  jmp.ml_reg = jmp_end.ml_reg = DCC_RC_CONST;
  jmp.ml_off = jmp_end.ml_off = 0;
  /*    bsr n+8(%src), %dst */
  /*    jz 1f */
  /*    xor 31, %dst */
  /*    jmp 2f */
  /* 1: bsr n+4(%src2), %dst */
  /*    jz 1f */
  /*    xor 31, %dst */
  /*    add $32, %dst*/
  /*    jmp 2f */
  /* 1: bsr n(%src2), %dst */
  /*    jz 1f */
  /*    xor 31, %dst */
  /*    add $64, %dst*/
  /*    jmp 2f */
  /*    ... */
  /* 1: bsr 0(%src2), %dst */
  /*    xor 31, %dst */
  /*    add $(n*8), %dst*/
  /* 2: */
  src_iter.ml_off += n_bytes;
  val.sa_sym = NULL;
  start_offset = 0;
  for (;;) {
   target_siz_t part;
        if (n_bytes >= 4) part = 4;
   else if (n_bytes >= 2) part = 2;
   else                   part = 1;
   n_bytes -= part;
   src_iter.ml_off -= part;
   DCCDisp_BsfMem(1,&src_iter,dst,part);
   if (n_bytes) DCCDisp_LocJcc(DCC_TEST_Z,&jmp);
   val.sa_off = (part*8)-1;
   DCCDisp_CstBinReg('^',&val,dst,1);
   if (start_offset) {
    val.sa_off = start_offset*8;
    DCCDisp_CstBinReg('+',&val,dst,1);
   }
   if (!n_bytes) break; /* check if this is the end. */
   DCCDisp_LocJmp(&jmp_end);
   start_offset += part;
   t_defsym(jmp.ml_sym);

   if unlikely((jmp.ml_sym = DCCUnit_AllocSym()) == NULL) return;
  }
  t_defsym(jmp_end.ml_sym);
 }
}


PUBLIC void
DCCDisp_AtomicRegCmpXchMemAX(rc_t src, struct DCCMemLoc const *__restrict dst) {
 assert(dst);
 /* lock cmpxchg %src, !dst */
 t_putb(0xf0);
 if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
 t_putb(0x0f);
 t_putb(0xb0+!!(src&DCC_RC_I16));
 asm_modmem(src&7,dst);
}


PUBLIC rc_t
DCCDisp_AtomicBin(tok_t op, rc_t src, rc_t temp,
                  struct DCCMemLoc const *__restrict dst) {
 struct DCCMemLoc jmp;
 rc_t eax;
 assert((temp&DCC_RI_MASK) != DCC_ASMREG_EAX);
 if ((src&DCC_RI_MASK) == DCC_ASMREG_EAX) {
  rc_t newsrc;
  uint8_t wanted_set = 0xff & ~((1 << (src&7))|
                                (1 << (temp&7))|
                                (1 << DCC_ASMREG_EAX));
  if (dst->ml_reg != DCC_RC_CONST) wanted_set &= ~(1 << (dst->ml_reg&7));
  newsrc = DCCVStack_GetRegOf((src&DCC_RC_MASK),wanted_set);
  DCCDisp_RegMovReg(src,newsrc,1);
  src = newsrc;
 }
 eax = (temp&DCC_RC_MASK)|DCC_ASMREG_EAX;
 if unlikely((jmp.ml_sym = DCCUnit_AllocSym()) == NULL) return eax;
 jmp.ml_reg = DCC_RC_CONST;
 jmp.ml_off = 0;
 t_defsym(jmp.ml_sym);
 /* >> 1:  mov !dst, %eax
  * >>     mov %eax, %temp
  * >>     :op %src, %temp
  * >>     lock cmpxchg %temp, !dst
  * >>     jne 1b
  */
 DCCDisp_MemMovReg(dst,eax);
 DCCDisp_RegMovReg(eax,temp,1);
 if (op == '~') {
  /* Special operation: nand */
  DCCDisp_RegBinReg('&',src,temp,1);
  DCCDisp_UnaryReg('~',temp);
 } else {
  DCCDisp_RegBinReg(op,src,temp,1);
 }
 DCCDisp_AtomicRegCmpXchMemAX(temp,dst);
 DCCDisp_LocJcc(DCC_TEST_NE,&jmp);
 return eax;
}

PUBLIC rc_t
DCCDisp_AtomicUnary(tok_t op, rc_t temp,
                    struct DCCMemLoc const *__restrict dst) {
 struct DCCMemLoc jmp; rc_t eax;
 assert((temp&DCC_RI_MASK) != DCC_ASMREG_EAX);
 eax = (temp&DCC_RC_MASK)|DCC_ASMREG_EAX;
 if unlikely((jmp.ml_sym = DCCUnit_AllocSym()) == NULL) return eax;
 jmp.ml_reg = DCC_RC_CONST;
 jmp.ml_off = 0;
 t_defsym(jmp.ml_sym);
 /* >> 1:  mov !dst, %eax
  * >>     mov %eax, %temp
  * >>     :op %temp
  * >>     lock cmpxchg %temp, !dst
  * >>     jne 1b
  */
 DCCDisp_MemMovReg(dst,eax);
 DCCDisp_RegMovReg(eax,temp,1);
 DCCDisp_UnaryReg(op,temp);
 DCCDisp_AtomicRegCmpXchMemAX(temp,dst);
 DCCDisp_LocJcc(DCC_TEST_NE,&jmp);
 return eax;
}


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif
PUBLIC void
DCCDisp_AtomicRegBinMem(tok_t op, int fetch_mode, rc_t src,
                        struct DCCMemLoc const *__restrict dst) {
 rc_t eax,temp;
 uint8_t wanted_set = 0xff & ~((1 << (src&7))|
                               (1 << DCC_ASMREG_EAX));
 if (dst->ml_reg != DCC_RC_CONST) wanted_set &= ~(1 << (dst->ml_reg&7));
 if (op == '=') {
  if (fetch_mode == DCCDISP_ATOMICBIN_FETCHAFTER) {
   temp = DCCVStack_GetRegOf(src&DCC_RC_MASK,wanted_set);
   DCCDisp_RegMovReg(src,temp,1);
  }
  /* lock xchg !dst, %src */
  t_putb(0xf0);
  if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
  t_putb(0x86+!!(src&DCC_RC_I16));
  asm_modmem(src&7,dst);
  /* Restore the source backup if the new value is requested. */
  if (fetch_mode == DCCDISP_ATOMICBIN_FETCHAFTER)
   DCCDisp_RegMovReg(temp,src,1);
  return;
 }
 if (fetch_mode == DCCDISP_ATOMICBIN_FETCHNEVER && op != '~') {
  /* Optimization: All allowed atomic operations (except for nand) can
   * be prefixed with 'lock', so long as the old value doesn't matter. */
  t_putb(0xf0);
  DCCDisp_RegBinMem(op,src,dst,1);
  return;
 }
#if DCC_TARGET_HASM(M_I486)
 if (op == '+') {
  /* Optimizations for '+': use 'lock xadd' */
  if (fetch_mode == DCCDISP_ATOMICBIN_FETCHAFTER) {
   temp = DCCVStack_GetRegOf(src&DCC_RC_MASK,wanted_set);
   DCCDisp_RegMovReg(src,temp,1);
  }
  /* lock xadd !dst, %src */
  t_putb(0xf0);
  if ((src&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
  t_putb(0x0f);
  t_putb(0xc0+!!(src&DCC_RC_I16));
  asm_modmem(src&7,dst);
  /* Add the source backup to the result if the new value is requested. */
  if (fetch_mode == DCCDISP_ATOMICBIN_FETCHAFTER)
   DCCDisp_RegBinReg('+',temp,src,1);
  return;
 }
#endif
 temp = DCCVStack_GetRegOf(src&DCC_RC_MASK,wanted_set);
 eax = DCCDisp_AtomicBin(op,src,temp,dst);
 if (fetch_mode == DCCDISP_ATOMICBIN_FETCHBEFORE) {
  DCCDisp_RegMovReg(eax,src,1);
 } else if (fetch_mode == DCCDISP_ATOMICBIN_FETCHAFTER) {
  DCCDisp_RegMovReg(temp,src,1);
 }
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif
PUBLIC void
DCCDisp_AtomicUnaryMem(tok_t op, int fetch_mode, rc_t result,
                       struct DCCMemLoc const *__restrict dst) {
 rc_t eax,temp;
 if (fetch_mode == DCCDISP_ATOMICBIN_FETCHNEVER) {
  /* Optimization: All allowed atomic operations can be prefixed
   * with 'lock', so long as the old value doesn't matter. */
  t_putb(0xf0);
  DCCDisp_UnaryMem(op,dst,
#ifdef DCC_RC_I64
                          (result&DCC_RC_I64) ? 8 :
#endif
                          (result&DCC_RC_I32) ? 4 :
                          (result&DCC_RC_I16) ? 2 : 1);
  return;
 }
 if (fetch_mode != DCCDISP_ATOMICBIN_FETCHNEVER) {
  if ((result&DCC_RI_MASK) != DCC_ASMREG_EAX) temp = result;
  else {
   uint8_t wanted_set = 0xff & ~((1 << DCC_ASMREG_EAX)|
                                 (1 << (result&7)));
   if (dst->ml_reg != DCC_RC_CONST) wanted_set &= ~(1 << (dst->ml_reg&7));
   temp = DCCVStack_GetRegOf(result&DCC_RC_MASK,wanted_set);
  }
 } else {
  uint8_t wanted_set = 0xff & ~(1 << DCC_ASMREG_EAX);
  if (dst->ml_reg != DCC_RC_CONST) wanted_set &= ~(1 << (dst->ml_reg&7));
  temp = DCCVStack_GetRegOf(result&DCC_RC_MASK,wanted_set);
 }
 eax = DCCDisp_AtomicUnary(op,temp,dst);
 if (fetch_mode == DCCDISP_ATOMICBIN_FETCHBEFORE) {
  if (result != eax) DCCDisp_RegMovReg(eax,result,1);
 } else if (fetch_mode == DCCDISP_ATOMICBIN_FETCHAFTER) {
  if (result != temp) DCCDisp_RegMovReg(temp,result,1);
 }
}





DCC_DECL_END

#endif /* !GUARD_DCC_GEN_MISC_C_INL */
