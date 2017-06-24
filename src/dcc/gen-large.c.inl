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
#ifndef GUARD_DCC_GEN_LARGE_C_INL
#define GUARD_DCC_GEN_LARGE_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include "x86_util.h"

/* Special handling for special, large unary/binary operations, that
 * can't be handled using regular code (NOTE: only handles 'IS_LARGE_OP' opcodes!)
 * >> Using the generic code produced in here, DCC dynamically implements
 *    arithmetic operations for types of (theoretically) infinite size.
 *    Some operations on such types will be compiled in-line as they can
 *    be stack to infinity, yet other operations will generate calls to
 *    external symbols, leaving the responsibility of implementing
 *    the required functions to library creators. */
DCC_DECL_BEGIN

PRIVATE target_ptr_t
DCCMemLoc_MinAlign(struct DCCMemLoc const *__restrict self) {
 target_ptr_t offset;
 target_ptr_t result = 16;
 int i; assert(self);
 offset = self->ml_off;
 if (self->ml_sym) {
  if (DCCSym_ISDEFINED(self->ml_sym))
      return self->ml_sym->sy_align;
  result = 1;
 }
 if (offset) {
  i = 0;
  while (!(offset&1)) ++i,offset >>= 1;
  result = (1 << i);
 }
 return result;
}


#define REQSIZE(op,dst_bytes) \
 (((op) == TOK_SHL || (op) == TOK_SHR || (op) == TOK_RANGLE3) \
  ? DCC_TARGET_SIZEOF_INT : dst_bytes)
#define REQALIGN(op,dst) \
 (((op) == TOK_SHL || (op) == TOK_SHR || (op) == TOK_RANGLE3) \
  ? DCC_TARGET_SIZEOF_INT : DCCMemLoc_MinAlign(dst))


#define LARGEMEM_STACK_LLONG 8

#ifdef LARGEMEM_STACK_LLONG
PRIVATE void
DCCDisp_LargeBinLLong(tok_t op, int src_unsigned) {
 struct DCCSym *sym;
 struct DCCSymAddr cleanup;
 assert(IS_LARGE_OP(op));
 /* Call an external symbol. */
#define CRT_SYMFLAG (DCC_SYMFLAG_HIDDEN)
 cleanup.sa_off = 2*DCC_TARGET_SIZEOF_LONG_LONG;
 switch (op) { /* Use GCC names for binary compatibility. */
 case TOK_SHL:     sym = DCCUnit_NewSyms("__ashlti3",CRT_SYMFLAG); goto cleanup_adj;
 case TOK_SHR:     sym = DCCUnit_NewSyms("__lshrti3",CRT_SYMFLAG); goto cleanup_adj;
 case TOK_RANGLE3: sym = DCCUnit_NewSyms("__ashrti3",CRT_SYMFLAG);
cleanup_adj: cleanup.sa_off -= (DCC_TARGET_SIZEOF_LONG_LONG-DCC_TARGET_SIZEOF_INT); break;
 case '/':         sym = DCCUnit_NewSyms(src_unsigned ? "__udivti3" : "__divti3",CRT_SYMFLAG); break;
 case '%':         sym = DCCUnit_NewSyms(src_unsigned ? "__umodti3" : "__modti3",CRT_SYMFLAG); break;
 default:          sym = DCCUnit_NewSyms("__multi3",CRT_SYMFLAG); break;
 }
 DCCDisp_SymCll(sym);
 cleanup.sa_sym = NULL;
 DCCDisp_AddReg(&cleanup,DCC_RR_XSP);
}
#endif /* LARGEMEM_STACK_LLONG */

PRIVATE void
DCCDisp_LargeMemBinMem_fixed(tok_t op,
                             struct DCCMemLoc const *__restrict src,
                             struct DCCMemLoc const *__restrict dst,
                             target_siz_t n_bytes, int src_unsigned) {
 uint8_t kill_mask;
 struct DCCSym *funsym;
 struct DCCSymAddr cleanup;
 assert(IS_LARGE_OP(op));
 assert(n_bytes > DCC_TARGET_SIZEOF_ARITH_MAX);
 assert(src),assert(dst);
 kill_mask = 0xff;
 if (!DCC_RC_ISCONST(src->ml_reg)) kill_mask &= ~(1 << (src->ml_reg&DCC_RI_MASK));
 if (!DCC_RC_ISCONST(dst->ml_reg)) kill_mask &= ~(1 << (dst->ml_reg&DCC_RI_MASK));

 DCCVStack_KillInt(kill_mask);
#ifdef LARGEMEM_STACK_LLONG
 if (n_bytes == LARGEMEM_STACK_LLONG) {
  if (!DCC_RC_ISCONST(dst->ml_reg) &&
      !DCC_ASMREG_ISPROTECTED(dst->ml_reg&DCC_RI_MASK)) {
   /* Must safe the destination register across the call. */
   if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EAX &&
       (dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDX) {
    DCCDisp_RegPush(dst->ml_reg);
    DCCDisp_MemPush(src,REQSIZE(op,LARGEMEM_STACK_LLONG));
    DCCDisp_MemPush(dst,LARGEMEM_STACK_LLONG);
    DCCDisp_LargeBinLLong(op,src_unsigned);
    DCCDisp_PopReg(dst->ml_reg);
    DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,dst,LARGEMEM_STACK_LLONG,1);
   } else {
    struct DCCMemLoc new_dst = *dst;
    DCCDisp_RegPush(new_dst.ml_reg);
    DCCDisp_MemPush(src,REQSIZE(op,LARGEMEM_STACK_LLONG));
    DCCDisp_MemPush(dst,LARGEMEM_STACK_LLONG);
    DCCDisp_LargeBinLLong(op,src_unsigned);
    new_dst.ml_reg = DCCVStack_GetRegOf(dst->ml_reg&DCC_RC_MASK,kill_mask);
    DCCDisp_PopReg(new_dst.ml_reg);
    DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,&new_dst,LARGEMEM_STACK_LLONG,1);
    DCCDisp_RegMovReg(new_dst.ml_reg,dst->ml_reg,1);
   }
  } else {
   DCCDisp_MemPush(src,REQSIZE(op,LARGEMEM_STACK_LLONG));
   DCCDisp_MemPush(dst,LARGEMEM_STACK_LLONG);
   DCCDisp_LargeBinLLong(op,src_unsigned);
   DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,dst,LARGEMEM_STACK_LLONG,1);
  }
  return;
 }
#endif
 /* Call an external symbol. */
 DCCDisp_LocPush(src);
 DCCDisp_LocPush(dst);
 n_bytes *= DCC_TARGET_BITPERBYTE;
 switch (op) { /* Special functions for variadic integral size. */
 case TOK_SHL:     funsym = DCCUnit_NewSymf(DCC_SYMFLAG_NONE,"__xshl%lu",(unsigned long)n_bytes); break;
 case TOK_SHR:     funsym = DCCUnit_NewSymf(DCC_SYMFLAG_NONE,"__xshr%lu",(unsigned long)n_bytes); break;
 case TOK_RANGLE3: funsym = DCCUnit_NewSymf(DCC_SYMFLAG_NONE,"__xsar%lu",(unsigned long)n_bytes); break;
 case '/':         funsym = DCCUnit_NewSymf(DCC_SYMFLAG_NONE,src_unsigned ? "__xudiv%lu" : "__xdiv%lu",(unsigned long)n_bytes); break;
 case '%':         funsym = DCCUnit_NewSymf(DCC_SYMFLAG_NONE,src_unsigned ? "__xumod%lu" : "__xdiv%lu",(unsigned long)n_bytes); break;
 default:          funsym = DCCUnit_NewSymf(DCC_SYMFLAG_NONE,"__xmul%lu",(unsigned long)n_bytes); break;
 }
 DCCDisp_SymCll(funsym);
 cleanup.sa_off = 2*DCC_TARGET_SIZEOF_POINTER;
 cleanup.sa_sym = NULL;
 DCCDisp_AddReg(&cleanup,DCC_RR_XSP);
}

PRIVATE void
DCCDisp_LargeMemBinMem(tok_t op,
                       struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 struct DCCMemLoc used_src = *src;
 target_siz_t reqbytes = REQSIZE(op,dst_bytes);
#ifdef LARGEMEM_STACK_LLONG
 if (dst_bytes == LARGEMEM_STACK_LLONG) {
  uint8_t kill_mask = 0xff & ~(1 << (dst->ml_reg&DCC_RI_MASK));
  if (!DCC_RC_ISCONST(src->ml_reg)) kill_mask &= ~(1 << (src->ml_reg&DCC_RI_MASK));
  DCCVStack_KillInt(kill_mask);
  if (!DCC_RC_ISCONST(dst->ml_reg) &&
      !DCC_ASMREG_ISPROTECTED(dst->ml_reg&DCC_RI_MASK)) {
   /* Must safe the destination register across the call. */
   if ((dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EAX &&
       (dst->ml_reg&DCC_RI_MASK) != DCC_ASMREG_EDX) {
    DCCDisp_RegPush(dst->ml_reg);
    DCCDisp_MemPushs(src,src_bytes,reqbytes,src_unsigned);
    DCCDisp_MemPush (dst,LARGEMEM_STACK_LLONG);
    DCCDisp_LargeBinLLong(op,src_unsigned);
    DCCDisp_PopReg(dst->ml_reg);
    DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,dst,dst_bytes,1);
   } else {
    struct DCCMemLoc new_dst = *dst;
    DCCDisp_RegPush(new_dst.ml_reg);
    DCCDisp_MemPushs(src,src_bytes,reqbytes,src_unsigned);
    DCCDisp_MemPush (dst,LARGEMEM_STACK_LLONG);
    DCCDisp_LargeBinLLong(op,src_unsigned);
    new_dst.ml_reg = DCCVStack_GetRegOf(dst->ml_reg&DCC_RC_MASK,kill_mask);
    DCCDisp_PopReg(new_dst.ml_reg);
    DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,&new_dst,dst_bytes,1);
    DCCDisp_RegMovReg(new_dst.ml_reg,dst->ml_reg,1);
   }
  } else {
   DCCDisp_MemPushs(src,src_bytes,reqbytes,src_unsigned);
   DCCDisp_MemPush (dst,LARGEMEM_STACK_LLONG);
   DCCDisp_LargeBinLLong(op,src_unsigned);
   DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,dst,dst_bytes,1);
  }
  return;
 }
#endif
 if (src_bytes < reqbytes) {
  used_src.ml_sym = NULL;
  used_src.ml_reg = DCC_RR_XBP;
  used_src.ml_off = DCCCompiler_HWStackAlloc(reqbytes,REQALIGN(op,dst),0);
  DCCDisp_MemMovMem(src,src_bytes,&used_src,reqbytes,src_unsigned);
 }
 DCCDisp_LargeMemBinMem_fixed(op,&used_src,dst,dst_bytes,src_unsigned);
}


PRIVATE void
DCCDisp_LargeMemsBinRegs(tok_t op, struct DCCMemLoc const *__restrict src,
                         target_siz_t src_bytes, rc_t dst,
                         rc_t dst2, int src_unsigned) {
#if defined(LARGEMEM_STACK_LLONG) && \
   (LARGEMEM_STACK_LLONG == DCC_TARGET_SIZEOF_GP_REGISTER*2)
 assert(dst2 != DCC_RC_CONST);
 DCCDisp_MemPushs(src,src_bytes,REQSIZE(op,LARGEMEM_STACK_LLONG),src_unsigned);
 DCCDisp_RegPush(dst2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCDisp_RegPush(dst|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCVStack_KillInt(~(uint8_t)((1 << (dst&DCC_RI_MASK))|
                              (1 << (dst2&DCC_RI_MASK))));
 DCCDisp_LargeBinLLong(op,src_unsigned);
 DCCDisp_RegsBinRegs('=',DCC_RR_XAX,DCC_RR_XDX,dst,dst2,1);
#else
 struct DCCMemLoc used_dst;
 assert(dst2 != DCC_RC_CONST);
 used_dst.ml_sym = NULL;
 used_dst.ml_reg = DCC_RR_XBP;
 used_dst.ml_off = DCCCompiler_HWStackAlloc(DCC_TARGET_SIZEOF_GP_REGISTER*2,
                                            DCC_TARGET_SIZEOF_GP_REGISTER,0);
 DCCDisp_RegMovMem(dst,&used_dst);  used_dst.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 DCCDisp_RegMovMem(dst2,&used_dst); used_dst.ml_off -= DCC_TARGET_SIZEOF_GP_REGISTER;
 DCCDisp_LargeMemBinMem(op,src,src_bytes,&used_dst,DCC_TARGET_SIZEOF_GP_REGISTER*2,src_unsigned);
 DCCDisp_MemMovReg(&used_dst,dst);  used_dst.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 DCCDisp_MemMovReg(&used_dst,dst2);
#endif
}
PRIVATE void
DCCDisp_LargeRegsBinMems(tok_t op, rc_t src, rc_t src2,
                         struct DCCMemLoc const *__restrict dst,
                         target_siz_t dst_bytes, int src_unsigned) {
 struct DCCMemLoc used_src;
 target_siz_t reqbytes = REQSIZE(op,dst_bytes);
#ifdef LARGEMEM_STACK_LLONG
 if (dst_bytes == LARGEMEM_STACK_LLONG) {
  if (REQSIZE(op,LARGEMEM_STACK_LLONG) > 4) {
   if (src2 != DCC_RC_CONST) {
    DCCDisp_RegPush(src2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
   } else {
    struct DCCSymAddr temp = {0,NULL};
    DCCDisp_CstPush(&temp,4);
   }
  }
  DCCDisp_RegPush(src|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
  DCCDisp_MemPush(dst,LARGEMEM_STACK_LLONG);
  DCCVStack_KillInt(~(uint8_t)((1 << (src&DCC_RI_MASK))|
                               (DCC_RC_ISCONST(src2) ? 0 :
                                1 << (src2&DCC_RI_MASK))));
  DCCDisp_LargeBinLLong(op,src_unsigned);
  DCCDisp_RegsBinMems('=',DCC_RR_XAX,DCC_RR_XDX,dst,LARGEMEM_STACK_LLONG,1);
  return;
 }
#endif
 used_src.ml_sym = NULL;
 used_src.ml_reg = DCC_RR_XBP;
 used_src.ml_off = DCCCompiler_HWStackAlloc(reqbytes,REQALIGN(op,dst),0);
 DCCDisp_RegsBinMems('=',src,src2,&used_src,reqbytes,src_unsigned);
 DCCDisp_LargeMemBinMem_fixed(op,&used_src,dst,dst_bytes,src_unsigned);
}
PRIVATE void
DCCDisp_LargeVecBinMem(tok_t op,
                       void             const *__restrict src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 struct DCCMemLoc used_src;
 void *fixed_source_data;
 target_siz_t reqbytes = REQSIZE(op,dst_bytes);
 fixed_source_data = (void *)src;
 if (src_bytes > reqbytes) src_bytes = reqbytes;
 else if (src_bytes != reqbytes) {
  int filler = 0;
  fixed_source_data = malloc(reqbytes);
  if unlikely(!fixed_source_data) goto seterr;
  memcpy(fixed_source_data,src,reqbytes);
  if (src_bytes && !src_unsigned && ((uint8_t *)src)[src_bytes-1]&0x80) filler = 0xff;
  memset((uint8_t *)fixed_source_data+src_bytes,
          filler,reqbytes-src_bytes);
 }
 used_src.ml_off = 0;
 used_src.ml_reg = DCC_RC_CONST;
 DCCSection_TBEGIN(unit.u_data);
 used_src.ml_sym = DCCSection_DAllocSym(unit.u_data,fixed_source_data,
                                        reqbytes,reqbytes,REQALIGN(op,dst),0);
 DCCSection_TEND(unit.u_data);
 if (fixed_source_data != src) free(fixed_source_data);
 DCCDisp_LargeMemBinMem_fixed(op,&used_src,dst,dst_bytes,src_unsigned);
 return;
seterr: TPPLexer_SetErr();
}
PRIVATE void
DCCDisp_LargeBytBinMem(tok_t op, int                      src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 struct DCCMemLoc used_src; int filler;
 void *fixed_source_data;
 target_siz_t reqbytes = REQSIZE(op,dst_bytes);
 if (src_bytes > reqbytes) src_bytes = reqbytes;
 fixed_source_data = malloc(reqbytes);
 if unlikely(!fixed_source_data) goto seterr;
 memset(fixed_source_data,src,reqbytes);
 filler = 0;
 if (src_bytes && !src_unsigned && src&0x80) filler = 0xff;
 memset((uint8_t *)fixed_source_data+src_bytes,
         filler,reqbytes-src_bytes);
 used_src.ml_off = 0;
 used_src.ml_reg = DCC_RC_CONST;
 DCCSection_TBEGIN(unit.u_data);
 used_src.ml_sym = DCCSection_DAllocSym(unit.u_data,fixed_source_data,
                                        reqbytes,reqbytes,REQALIGN(op,dst),0);
 DCCSection_TEND(unit.u_data);
 free(fixed_source_data);
 DCCDisp_LargeMemBinMem_fixed(op,&used_src,dst,dst_bytes,src_unsigned);
 return;
seterr: TPPLexer_SetErr();
}
PRIVATE void
DCCDisp_LargeByrBinMem(tok_t op, rc_t                     src, target_siz_t src_bytes,
                       struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                       int src_unsigned) {
 struct DCCMemLoc used_src;
 target_siz_t reqbytes = REQSIZE(op,dst_bytes);
 used_src.ml_sym = NULL;
 used_src.ml_reg = DCC_RR_XBP;
 used_src.ml_off = DCCCompiler_HWStackAlloc(reqbytes,REQALIGN(op,dst),0);
 DCCDisp_ByrMovMem(src,src_bytes,&used_src,reqbytes,src_unsigned);
 DCCDisp_LargeMemBinMem_fixed(op,&used_src,dst,dst_bytes,src_unsigned);
}
PRIVATE void
DCCDisp_LargeRegsBinRegs(tok_t op, rc_t src, rc_t src2,
                         rc_t dst, rc_t dst2, int src_unsigned) {
#if defined(LARGEMEM_STACK_LLONG) && \
   (LARGEMEM_STACK_LLONG == DCC_TARGET_SIZEOF_GP_REGISTER*2)
 assert(dst2 != DCC_RC_CONST);
 if (REQSIZE(op,LARGEMEM_STACK_LLONG) > 4) {
  if (src2 != DCC_RC_CONST) {
   DCCDisp_RegPush(src2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
  } else {
   struct DCCSymAddr temp = {0,NULL};
   DCCDisp_CstPush(&temp,4);
  }
 }
 DCCDisp_RegPush(src|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCDisp_RegPush(dst2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCDisp_RegPush(dst|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCVStack_KillInt(~(uint8_t)((1 << (src&DCC_RI_MASK))|
                              (1 << (dst&DCC_RI_MASK))|
                              (1 << (dst2&DCC_RI_MASK))));
 DCCDisp_LargeBinLLong(op,src_unsigned);
 DCCDisp_RegsBinRegs('=',DCC_RR_XAX,DCC_RR_XDX,dst,dst2,1);
#else
 struct DCCMemLoc src_buf,dst_buf;
 target_siz_t reqbytes = REQSIZE(op,DCC_TARGET_SIZEOF_GP_REGISTER*2);
 src_buf.ml_sym = dst_buf.ml_sym = NULL;
 src_buf.ml_reg = dst_buf.ml_reg = DCC_RR_XBP;
 src_buf.ml_off = DCCCompiler_HWStackAlloc(reqbytes,reqbytes,0);
 dst_buf.ml_off = DCCCompiler_HWStackAlloc(DCC_TARGET_SIZEOF_GP_REGISTER*2,
                                           DCC_TARGET_SIZEOF_GP_REGISTER,0);
 DCCDisp_RegsBinMems('=',src,src2,&src_buf,reqbytes,src_unsigned);
 DCCDisp_RegsBinMems('=',dst,dst2,&dst_buf,DCC_TARGET_SIZEOF_GP_REGISTER*2,1);
 DCCDisp_LargeMemBinMem_fixed(op,&src_buf,&src_buf,DCC_TARGET_SIZEOF_GP_REGISTER*2,src_unsigned);
 DCCDisp_MemsBinRegs('=',&dst_buf,DCC_TARGET_SIZEOF_GP_REGISTER*2,dst,dst2,1);
#endif
}

#if DCC_TARGET_SIZEOF_GP_REGISTER < 8
PRIVATE void
DCCDisp_LargeCstBinRegs(tok_t op, struct DCCSymExpr const *__restrict val,
                        rc_t dst, rc_t dst2, int src_unsigned) {
#if defined(LARGEMEM_STACK_LLONG) && \
   (LARGEMEM_STACK_LLONG == DCC_TARGET_SIZEOF_GP_REGISTER*2)
 struct DCCSymAddr addr;
#if (DCC_TARGET_BYTEORDER == 1234) ^ DCC_TARGET_STACKDOWN
 addr.sa_off = (target_off_t)val->e_int;
 addr.sa_sym = val->e_sym;
 DCCDisp_CstPush(&addr,DCC_TARGET_SIZEOF_GP_REGISTER);
 addr.sa_off = (target_off_t)(val->e_int >> 32);
 addr.sa_sym = NULL;
 DCCDisp_CstPush(&addr,DCC_TARGET_SIZEOF_GP_REGISTER);
#else
 addr.sa_off = (target_off_t)(val->e_int >> 32);
 addr.sa_sym = NULL;
 DCCDisp_CstPush(&addr,DCC_TARGET_SIZEOF_GP_REGISTER);
 addr.sa_off = (target_off_t)val->e_int;
 addr.sa_sym = val->e_sym;
 DCCDisp_CstPush(&addr,DCC_TARGET_SIZEOF_GP_REGISTER);
#endif
 DCCDisp_RegPush(dst2|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCDisp_RegPush(dst|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8);
 DCCVStack_KillInt(~(uint8_t)((1 << (dst&DCC_RI_MASK))|
                              (1 << (dst2&DCC_RI_MASK))));
 DCCDisp_LargeBinLLong(op,src_unsigned);
 DCCDisp_RegsBinRegs('=',DCC_RR_XAX,DCC_RR_XDX,dst,dst2,1);
#else
 struct DCCMemLoc srcval; struct DCCSymAddr aval;
 target_siz_t reqbytes; uint8_t *data;
 reqbytes = REQSIZE(op,DCC_TARGET_SIZEOF_GP_REGISTER*2);
 srcval.ml_reg = DCC_RC_CONST;
 srcval.ml_off = DCCSection_TADDR(unit.u_data);
 srcval.ml_sym = &unit.u_data->sc_start;
 data = (uint8_t *)DCCSection_AAlloc(unit.u_data,
                                     reqbytes,reqbytes);
 if (data) {
  /* Store the initial value inside the target data. */
  aval.sa_sym = val->e_sym;
  if (reqbytes >= 4) {
   aval.sa_off           = (target_off_t)val->e_int;
   *(target_off_t *)data = def_reloc(&aval,unit.u_data,srcval.ml_off,DCC_REL_SIZE32);
   data                 += sizeof(target_off_t);
   reqbytes             -= 4;
  }
  if (reqbytes >= 4) {
   aval.sa_off           = (target_off_t)(val->e_int >> 32);
   *(target_off_t *)data = def_reloc(&aval,unit.u_data,srcval.ml_off,DCC_REL_SIZE32);
   data                 += sizeof(target_off_t);
   reqbytes             -= 4;
  }
  if (reqbytes) {
   int filler = 0;
   if (!src_unsigned && val->e_int < 0) filler = 0xff;
   memset(data,filler,reqbytes);
  }
 }
 DCCDisp_LargeMemsBinRegs(op,&srcval,DCC_TARGET_SIZEOF_GP_REGISTER*2,
                          dst,dst2,src_unsigned);
#endif
}
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_GEN_LARGE_C_INL */
