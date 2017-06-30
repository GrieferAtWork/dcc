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
#ifndef GUARD_DCC_GEN_CMP_C_INL
#define GUARD_DCC_GEN_CMP_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

DCC_DECL_BEGIN

PRIVATE struct DCCSymAddr const symaddr_zero = {0,NULL};

PUBLIC test_t
DCCDisp_MemIcmpMem(test_t test,
                   struct DCCMemLoc const *__restrict src, target_siz_t src_bytes, int src_unsigned,
                   struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes, int dst_unsigned) {
 target_siz_t uncommon_size,common_size;
 target_siz_t max_part_size,rc_part_size,part_size;
 struct DCCMemLoc used_src,used_dst;
 struct DCCSym *done_sym = NULL; test_t utest;
 rc_t temp_storage,uncommon_sign;
again:
 assert(src);
 assert(dst);
 if (dst_bytes < src_bytes && dst_unsigned) {
  struct DCCMemLoc const *temp_mloc;
  target_siz_t temp_bytes;
  test = DCC_TEST_MIRROR(test);
  temp_mloc = src,src = dst,dst = temp_mloc;
  temp_bytes = src_bytes,src_bytes = dst_bytes,dst_bytes = temp_bytes;
  dst_unsigned = src_unsigned,src_unsigned = 1;
  goto again;
 }

 /* TODO: Compile-time optimizations? */
 if (src_bytes >= dst_bytes) {
  uncommon_size = src_bytes-dst_bytes;
  common_size   = dst_bytes;
 } else {
  uncommon_size = dst_bytes-src_bytes;
  common_size   = src_bytes;
 }
 max_part_size = uncommon_size;
 if (max_part_size < common_size)
     max_part_size = common_size;
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
      if (max_part_size >= 8) rc_part_size = 8;
 else
#endif
      if (max_part_size >= 4) rc_part_size = 4;
 else if (max_part_size >= 2) rc_part_size = 2;
 else                         rc_part_size = 1;
 used_src = *src,used_src.ml_off += (common_size+uncommon_size);
 used_dst = *dst,used_dst.ml_off += (common_size+uncommon_size);
 temp_storage = DCC_RC_CONST;
 uncommon_sign = DCC_RC_CONST;
 if (src_bytes && dst_bytes) {
  int uncommon_sign_kind = 0;
  /* Allocate a temporary transport register. */
  rcset_t wanted_set = DCC_RCSET_FULL;
  rc_t part_class = DCC_RC_FORSIZE(rc_part_size);
  if (uncommon_size) {
   /* Load the sign bit. */
   if (src_bytes < dst_bytes) { if (src_bytes && !src_unsigned) uncommon_sign_kind = 1; }
   else if ((assert(src_bytes > dst_bytes),dst_bytes && !dst_unsigned)) uncommon_sign_kind = 2;
  }
  if (!DCC_RC_ISCONST(used_src.ml_reg)) wanted_set &= ~DCC_RCSET(used_src.ml_reg&DCC_RI_MASK);
  if (!DCC_RC_ISCONST(used_dst.ml_reg)) wanted_set &= ~DCC_RCSET(used_dst.ml_reg&DCC_RI_MASK);
  if (uncommon_sign_kind && part_class&(DCC_RC_I16|DCC_RC_I3264))
      wanted_set &= ~(DCC_RCSET(DCC_ASMREG_SI)|DCC_RCSET(DCC_ASMREG_DI));
  temp_storage = DCCVStack_GetRegOf(part_class,wanted_set);
  if (uncommon_sign_kind) {
   assert(uncommon_size >= 1);
   uncommon_sign = temp_storage;
#ifdef DCC_RC_I64
   if (uncommon_size < 8) uncommon_sign &= ~(DCC_RC_I64);
#endif
   if (uncommon_size < 4) uncommon_sign &= ~(DCC_RC_I32);
   if (uncommon_size < 2) uncommon_sign &= ~(DCC_RC_I16);
   assert((uncommon_sign&DCC_RC_MASK) != 0);
   if (uncommon_sign_kind == 1) {
    --used_src.ml_off;
    DCCDisp_MemSignExtendReg(&used_src,uncommon_sign);
    ++used_src.ml_off;
   } else {
    assert(uncommon_sign_kind == 2);
    --used_dst.ml_off;
    DCCDisp_MemSignExtendReg(&used_dst,uncommon_sign);
    ++used_dst.ml_off;
   }
  }
 }
 while (uncommon_size) {
  part_size = uncommon_size;
  if (part_size > rc_part_size)
      part_size = rc_part_size;
  else if (part_size&(part_size-1)) {
#if DCC_TARGET_SIZEOF_ARITH_MAX > 8
#error FIXME
#elif DCC_TARGET_SIZEOF_ARITH_MAX > 4
   if (part_size == 7 || part_size == 6 || part_size == 5)
       part_size = 4;
   else
#endif
   {
    assert(part_size == 3);
    part_size = 2;
   }
  }
  used_src.ml_off -= part_size;
  used_dst.ml_off -= part_size;
  uncommon_size   -= part_size;
  if (uncommon_sign != DCC_RC_CONST) {
   rc_t effective_sign = uncommon_sign;
#ifdef DCC_RC_I64
   if (part_size < 8) effective_sign &= ~(DCC_RC_I64);
#endif
   if (part_size < 4) effective_sign &= ~(DCC_RC_I32);
   if (part_size < 2) effective_sign &= ~(DCC_RC_I16);
   assert((effective_sign&DCC_RC_MASK) != 0);
   if (src_bytes < dst_bytes) {
    DCCDisp_RegBinMem('?',effective_sign,&used_dst,1);
   } else {
    DCCDisp_MemBinReg('?',&used_src,effective_sign,1);
   }
  } else {
   /* NOTE: The case for 'sizeof(dst) < sizeof(src) && sizeof(dst) && !is_signed(dst)'
    *       is handled explicitly at the beginning, meaning that we can only ever get
    *       here when a sign-register has been set. */
   assert(src_bytes </*=*/ dst_bytes);
   DCCDisp_CstBinMem('?',&symaddr_zero,&used_dst,part_size,1);
  }
  if (uncommon_size || common_size) {
   if (!done_sym && (done_sym = DCCUnit_AllocSym()) == NULL) goto end;
   utest = DCC_TEST_UNSIGNED(test);
   DCCDisp_SccTst(test,utest),test = utest;
   DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
  }
 }
 assert(!uncommon_size);
 while (common_size) {
  rc_t effective_comm = temp_storage;
  part_size = common_size;
  if (part_size > rc_part_size)
      part_size = rc_part_size;
  else if (part_size&(part_size-1)) {
#if DCC_TARGET_SIZEOF_ARITH_MAX > 8
#error FIXME
#elif DCC_TARGET_SIZEOF_ARITH_MAX > 4
   if (part_size == 7 || part_size == 6 || part_size == 5)
       part_size = 4;
   else
#endif
   {
    assert(part_size == 3);
    part_size = 2;
   }
  }
#ifdef DCC_RC_I64
  if (part_size < 8) effective_comm &= ~(DCC_RC_I64);
#endif
  if (part_size < 4) effective_comm &= ~(DCC_RC_I32);
  if (part_size < 2) effective_comm &= ~(DCC_RC_I16);
  assert((effective_comm&DCC_RC_MASK) != 0);
  used_src.ml_off -= part_size;
  used_dst.ml_off -= part_size;
  common_size     -= part_size;
  DCCDisp_MemMovReg(&used_src,effective_comm);
  DCCDisp_RegBinMem('?',effective_comm,&used_dst,0);
  if (common_size) {
   if (!done_sym && (done_sym = DCCUnit_AllocSym()) == NULL) goto end;
   utest = DCC_TEST_UNSIGNED(test);
   DCCDisp_SccTst(test,utest),test = utest;
   DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
  }
 }
 assert(!memcmp(&used_dst,dst,sizeof(struct DCCMemLoc)));
 assert(!memcmp(&used_src,src,sizeof(struct DCCMemLoc)));
 if (done_sym) t_defsym(done_sym);
end:
 return test;
}

PUBLIC test_t
DCCDisp_MemsIcmpRegs(test_t test, struct DCCMemLoc const *__restrict src,
                     target_siz_t src_bytes, int src_unsigned,
                     rc_t dst, rc_t dst2, int dst_unsigned) {
 struct DCCMemLoc used_src; rc_t temp;
 struct DCCSym *done_sym = NULL;
 target_siz_t dst_bytes;
 used_src = *src;
again_compare:
 dst_bytes = DCC_RC_SIZE(dst);
 if (DCC_RC_ISCONST(dst2)) {
  if (src_bytes == dst_bytes) {
   DCCDisp_MemBinReg('?',&used_src,dst,1);
  } else if (src_bytes < dst_bytes) {
   temp = DCCVStack_GetReg(dst&DCC_RC_MASK,1);
   DCCDisp_MemsMovReg(&used_src,src_bytes,temp,src_unsigned);
   DCCDisp_RegBinReg('?',temp,dst,1);
  } else if (src_bytes <= DCC_TARGET_SIZEOF_GP_REGISTER) {
   /* int8_t == int32_t */
   dst = DCCVStack_CastReg(dst,dst_unsigned,DCC_RC_FORSIZE(src_bytes));
   DCCDisp_MemBinReg('?',&used_src,dst,1);
  } else if (!dst_unsigned) {
   /* int8_t == int128_t */
   target_siz_t diff_size;
   target_siz_t part_size;
signed_upper_cmp:
   assert(!done_sym);
   diff_size = src_bytes-dst_bytes;
   done_sym = DCCUnit_AllocSym();
   if unlikely(!done_sym) goto end;
   assert(diff_size);
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
        if (diff_size >= 8) part_size = 8;
   else
#endif
        if (diff_size >= 4) part_size = 4;
   else if (diff_size >= 2) part_size = 2;
   else                     part_size = 1;
   temp = DCCVStack_GetReg(DCC_RC_FORSIZE(part_size),1);
   DCCDisp_RegMovReg(dst2 != DCC_RC_CONST ? dst2 : dst,temp,1);
   DCCDisp_SignMirrorReg(temp);
   used_src.ml_off += src_bytes;
   do {
    test_t utest = DCC_TEST_UNSIGNED(test);
    used_src.ml_off -= part_size;
    src_bytes   -= part_size;
    /* Compare high memory against sign-extended integer register. */
    DCCDisp_MemBinReg('?',&used_src,temp,1);
    DCCDisp_SccTst(test,utest),test = utest;
    DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
   } while (src_bytes > dst_bytes);
   used_src.ml_off -= src_bytes;
   goto again_compare;
  } else {
   /* uint8_t == uint128_t */
   target_siz_t diff_size;
   target_siz_t part_size;
   struct DCCSymAddr cmp_val;
unsigned_upper_cmp:
   assert(!done_sym);
   diff_size = src_bytes-dst_bytes;
   done_sym = DCCUnit_AllocSym();
   if unlikely(!done_sym) goto end;
   assert(diff_size);
#if DCC_TARGET_SIZEOF_ARITH_MAX >= 8
        if (diff_size >= 8) part_size = 8;
   else
#endif
        if (diff_size >= 4) part_size = 4;
   else if (diff_size >= 2) part_size = 2;
   else                     part_size = 1;
   used_src.ml_off += src_bytes;
   test = DCC_TEST_MIRROR(test);
   cmp_val.sa_off = 0;
   cmp_val.sa_sym = NULL;
   do {
    test_t utest = DCC_TEST_UNSIGNED(test);
    used_src.ml_off -= part_size;
    src_bytes   -= part_size;
    /* Compare high memory against sign-extended integer register. */
    DCCDisp_CstBinMem('?',&cmp_val,&used_src,part_size,1);
    DCCDisp_SccTst(test,utest),test = utest;
    DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
   } while (src_bytes > dst_bytes);
   used_src.ml_off -= src_bytes;
   goto again_compare;
  }
 } else {
  target_siz_t dst1_size,dst2_size;
  dst1_size = dst_bytes;
  dst2_size = DCC_RC_SIZE(dst2);
  dst_bytes += dst2_size;
  assert(dst1_size == DCC_RC_SIZE(dst));
  if (src_bytes == dst_bytes) {
   used_src.ml_off += dst1_size;
   DCCDisp_MemBinReg('?',&used_src,dst2,1);
   used_src.ml_off -= dst1_size;

  } else if (src_bytes < dst_bytes) {
   temp = DCCVStack_GetReg(dst&DCC_RC_MASK,1);
   DCCDisp_MemsMovReg(&used_src,src_bytes,temp,src_unsigned);
   DCCDisp_RegBinReg('?',temp,dst,1);
  } else if (src_bytes <= DCC_TARGET_SIZEOF_GP_REGISTER) {
   /* int8_t == int32_t */
   dst = DCCVStack_CastReg(dst,dst_unsigned,DCC_RC_FORSIZE(src_bytes));
   DCCDisp_MemBinReg('?',&used_src,dst,1);
  } else if (!dst_unsigned) {
   goto signed_upper_cmp;
  } else {
   goto unsigned_upper_cmp;
  }
 }
 if (done_sym) t_defsym(done_sym);
end:
 return test;
}

PUBLIC test_t
DCCDisp_RegsIcmpMems(test_t test, rc_t src, rc_t src2,
                     int src_unsigned, struct DCCMemLoc const *__restrict dst,
                     target_siz_t dst_bytes, int dst_unsigned) {
 /* TODO: This can be done better... */
 return DCCDisp_MemsIcmpRegs(DCC_TEST_MIRROR(test),
                             dst,dst_bytes,dst_unsigned,
                             src,src2,src_unsigned);
}

PUBLIC test_t
DCCDisp_RegsIcmpRegs(test_t test,
                     rc_t src, rc_t src2, int src_unsigned,
                     rc_t dst, rc_t dst2, int dst_unsigned) {
 assert(!DCC_RC_ISCONST(src));
 assert(!DCC_RC_ISCONST(dst));
again:
 if (src2 == DCC_RC_CONST) {
  if (dst2 == DCC_RC_CONST) {
   DCCDisp_RegBinReg('?',src,dst,0);
  } else if (!src_unsigned) {
   /* int64_t == int32_t */
   struct DCCSym *done_sym; test_t utest;
   done_sym = DCCUnit_AllocSym();
   if unlikely(!done_sym) goto end;
   utest = DCC_TEST_UNSIGNED(test);
   /* Compare high memory against sign-extended integer register. */
   DCCDisp_RegPush(src);
   src = DCCVStack_CastReg(src,0,dst2&DCC_RC_MASK);
   DCCDisp_SignMirrorReg(src);
   DCCDisp_RegBinReg('?',src,dst2,1);
   DCCDisp_PopReg(src);
   DCCDisp_SccTst(test,utest),test = utest;
   DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
   DCCDisp_RegBinReg('?',src,dst,0);
   t_defsym(done_sym);
  } else {
   /* uint64_t == uint32_t */
   struct DCCSymAddr val;
   struct DCCSym *done_sym;
   test_t utest;
   done_sym = DCCUnit_AllocSym();
   if unlikely(!done_sym) goto end;
   utest = DCC_TEST_UNSIGNED(test);
   /* Compare high memory against sign-extended integer register. */
   val.sa_off = 0;
   val.sa_sym = NULL;
   DCCDisp_CstBinReg('?',&val,dst2,1);
   DCCDisp_SccTst(test,utest),test = utest;
   DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
   DCCDisp_RegBinReg('?',src,dst,0);
   t_defsym(done_sym);
  }
 } else if (dst2 == DCC_RC_CONST) {
  if (!dst_unsigned) {
   /* int32_t == int64_t */
   struct DCCSym *done_sym; test_t utest;
   done_sym = DCCUnit_AllocSym();
   if unlikely(!done_sym) goto end;
   utest = DCC_TEST_UNSIGNED(test);
   /* Compare high memory against sign-extended integer register. */
   DCCDisp_RegPush(dst);
   dst = DCCVStack_CastReg(dst,0,src2&DCC_RC_MASK);
   DCCDisp_SignMirrorReg(dst);
   DCCDisp_RegBinReg('?',src2,dst,1);
   DCCDisp_PopReg(dst);
   DCCDisp_SccTst(test,utest),test = utest;
   DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
   DCCDisp_RegBinReg('?',src,dst,0);
   t_defsym(done_sym);
  } else {
   rc_t temp;
   /* uint32_t == uint64_t */
   test = DCC_TEST_MIRROR(test);
   temp = src,src = dst,dst = temp;
   temp = src2,src2 = dst2,dst2 = temp;
   goto again;
  }
 } else {
  struct DCCSym *done_sym;
  test_t utest;
  done_sym = DCCUnit_AllocSym();
  if unlikely(!done_sym) goto end;
  utest = DCC_TEST_UNSIGNED(test);
  DCCDisp_RegBinReg('?',src2,dst2,1);
  DCCDisp_SccTst(test,utest),test = utest;
  DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
  DCCDisp_RegBinReg('?',src,dst,0);
  t_defsym(done_sym);
 }
end:
 return test;
}

#if DCC_TARGET_SIZEOF_ARITH_MAX < 8
PUBLIC test_t
DCCDisp_CstIcmpRegs(test_t test, struct DCCSymExpr const *__restrict val,
                    int src_unsigned, rc_t dst, rc_t dst2) {
 struct DCCSymAddr val32;
 test_t utest; struct DCCSym *done_sym;
 if (dst2 == DCC_RC_CONST) {
  val32.sa_off = (target_off_t)val->e_int;
  val32.sa_sym = val->e_sym;
  DCCDisp_CstBinReg('?',&val32,dst,1);
  return test;
 }
 if (src_unsigned) val32.sa_off = (target_off_t)((uint_least64_t)val->e_int >> 32);
 else              val32.sa_off = (target_off_t)(( int_least64_t)val->e_int >> 32);
 val32.sa_sym = NULL;
 done_sym = DCCUnit_AllocSym();
 if unlikely(!done_sym) goto end;
 utest = DCC_TEST_UNSIGNED(test);
 DCCDisp_CstBinReg('?',&val32,dst2,1);
 DCCDisp_SccTst(test,utest),test = utest;
 DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
 val32.sa_off = (target_off_t)val->e_int;
 val32.sa_sym = val->e_sym;
 DCCDisp_CstBinReg('?',&val32,dst,1);
 t_defsym(done_sym);
end:
 return test;
}
#endif

DCC_DECL_END

#endif /* !GUARD_DCC_GEN_CMP_C_INL */
