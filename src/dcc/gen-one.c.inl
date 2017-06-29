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
#ifndef GUARD_DCC_GEN_ONE_C_INL
#define GUARD_DCC_GEN_ONE_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>

#include "x86_util.h"

DCC_DECL_BEGIN

PUBLIC void
DCCDisp_UnaryReg(tok_t op, rc_t dst) {
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
 switch (op) {
 case '-':
 case '~':
  if (dst&DCC_RC_I16) t_putb(0xf7);
  else                t_putb(0xf6);
  asm_modreg((op == '-' ? 3 : 2),dst&DCC_RI_MASK);
  break;
 case TOK_INC:
  if (dst&(DCC_RC_I16|DCC_RC_I3264)) {
   t_putb(0x40+(dst&DCC_RI_MASK));
  } else {
   t_putb(0xfe);
   asm_modreg(0,dst&DCC_RI_MASK);
  }
  break;
 case TOK_DEC:
  if (dst&(DCC_RC_I16|DCC_RC_I3264)) {
   t_putb(0x48+(dst&DCC_RI_MASK));
  } else {
   t_putb(0xfe);
   asm_modreg(1,dst&DCC_RI_MASK);
  }
  break;
 case '(':
  if (!(dst&(DCC_RC_I3264|DCC_RC_I16))) {
   dst = DCCVStack_CastReg(dst,1,DCC_RC_I16)|
                          (dst&DCC_RC_MASK_86SEGP);
  }
  /* call *%dst */
  DCCDisp_X86Segp(dst);
  if (!(dst&DCC_RC_I32)) t_putb(0x66);
  t_putb(0xff);
  asm_modreg(2,dst&DCC_RI_MASK);
  break;
 default: break;
 }
}

PRIVATE void
DCCDisp_UnaryMemWidth(tok_t op, struct DCCMemLoc const *__restrict dst,
                      width_t width) {
 assert(CHECK_WIDTH(width));
 switch (op) {
 case '-':
 case '~':
  DCCDisp_X86Segp(dst->ml_reg);
  if (width == 2) t_putb(0x66);
  if (width == 4) t_putb(0xf7);
  else            t_putb(0xf6);
  asm_modmem(op == '-' ? 3 : 2,dst);
  break;
 case TOK_INC:
 case TOK_DEC:
  DCCDisp_X86Segp(dst->ml_reg);
  if (width == 2) t_putb(0x66);
  t_putb((0xfe)+(width != 1));
  asm_modmem(op == TOK_DEC ? 1 : 0,dst);
  break;
 case '(':
  if (width == 1) {
   /* 8-bit call. */
   rc_t temp = DCCVStack_GetReg(DCC_RC_I16,0);
   rc_t high_order = (temp&~(DCC_RC_I16))+4;
   DCCDisp_IntMovReg(0,high_order);
   DCCDisp_MemMovReg(dst,temp&~(DCC_RC_I16));
   DCCDisp_UnaryReg(op,temp);
  } else {
   /* call *offset(%dst) */
   DCCDisp_X86Segp(dst->ml_reg);
   if (width == 2) t_putb(0x66);
   t_putb(0xff);
   asm_modmem(2,dst);
  }
  break;
 default: break;
 }
}

PRIVATE void
DCCDisp_UnaryStaWidth(tok_t op, void *__restrict dst,
                      width_t width) {
 target_off_t v;
 assert(op != '(');
 assert(CHECK_WIDTH(width));
 v = readw(dst,width,0);
 switch (op) {
 case '-': v = -v; break;
 case '~': v = ~v; break;
 case TOK_INC: ++v; break;
 case TOK_DEC: --v; break;
 default: break;
 }
 writew(dst,v,width);
}
PRIVATE void
DCCDisp_UnarySta(tok_t op, void *__restrict dst,
                 target_siz_t dst_bytes) {
 assert(op != '(');
 while (dst_bytes >= DCC_TARGET_SIZEOF_ARITH_MAX) {
  DCCDisp_UnaryStaWidth(op,dst,DCC_TARGET_SIZEOF_ARITH_MAX);
  *(uintptr_t *)&dst += DCC_TARGET_SIZEOF_ARITH_MAX;
  dst_bytes          -= DCC_TARGET_SIZEOF_ARITH_MAX;
 }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 if (dst_bytes&4) { DCCDisp_UnaryStaWidth(op,dst,4); *(uintptr_t *)&dst += 4; }
#endif
 if (dst_bytes&2) { DCCDisp_UnaryStaWidth(op,dst,2); *(uintptr_t *)&dst += 2; }
 if (dst_bytes&1) { DCCDisp_UnaryStaWidth(op,dst,1); }
}


PUBLIC void
DCCDisp_UnaryMem(tok_t op, struct DCCMemLoc const *__restrict dst,
                 target_siz_t dst_bytes) {
 struct DCCMemLoc dst_iter; void *cdst;
 if (!dst_bytes) return;
 if (op != '(' &&
    (cdst = DCCMemLoc_CompilerAddr(dst,dst_bytes)) != NULL) {
  DCCDisp_UnarySta(op,cdst,dst_bytes);
  return;
 }
 dst_iter = *dst;
 if (dst_bytes > DCC_TARGET_SIZEOF_ARITH_MAX) {
  if (op == TOK_INC || op == TOK_DEC) {
   /* inc/dec for out-of-band memory arguments. */
   struct DCCSymAddr src_val = {1,NULL};
   DCCDisp_CstBinMem(op == TOK_INC ? '+' : '-',&src_val,
                     &dst_iter,DCC_TARGET_SIZEOF_ARITH_MAX,1);
   dst_iter.ml_off += DCC_TARGET_SIZEOF_ARITH_MAX;
   dst_bytes       -= DCC_TARGET_SIZEOF_ARITH_MAX;
   src_val.sa_off = 0;
   while (dst_bytes >= DCC_TARGET_SIZEOF_ARITH_MAX) {
    DCCDisp_CstBinMem(op,&src_val,&dst_iter,DCC_TARGET_SIZEOF_ARITH_MAX,1);
    dst_iter.ml_off += DCC_TARGET_SIZEOF_ARITH_MAX;
    dst_bytes       -= DCC_TARGET_SIZEOF_ARITH_MAX;
   }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
   if (dst_bytes&4) { DCCDisp_CstBinMem(op,&src_val,&dst_iter,4,1); dst_iter.ml_off += 4; }
#endif
   if (dst_bytes&2) { DCCDisp_CstBinMem(op,&src_val,&dst_iter,2,1); dst_iter.ml_off += 2; }
   if (dst_bytes&1) { DCCDisp_CstBinMem(op,&src_val,&dst_iter,1,1); }
   return;
  } else if (op == '-') {
   /* -x --> ~(x-1) */
   struct DCCSymAddr src_val = {1,NULL};
   DCCDisp_CstBinMem('-',&src_val,&dst_iter,dst_bytes,1);
   op = '~'; /* Generate a bit-wise inversion below. */
  }
 }
 while (dst_bytes >= DCC_TARGET_SIZEOF_ARITH_MAX) {
  DCCDisp_UnaryMemWidth(op,&dst_iter,DCC_TARGET_SIZEOF_ARITH_MAX);
  dst_iter.ml_off += DCC_TARGET_SIZEOF_ARITH_MAX;
  dst_bytes       -= DCC_TARGET_SIZEOF_ARITH_MAX;
 }
#if DCC_TARGET_SIZEOF_ARITH_MAX > 4
 if (dst_bytes&4) { DCCDisp_UnaryMemWidth(op,&dst_iter,4); dst_iter.ml_off += 4; }
#endif
 if (dst_bytes&2) { DCCDisp_UnaryMemWidth(op,&dst_iter,2); dst_iter.ml_off += 2; }
 if (dst_bytes&1) { DCCDisp_UnaryMemWidth(op,&dst_iter,1); }
}

DCC_DECL_END

#endif /* !GUARD_DCC_GEN_ONE_C_INL */
