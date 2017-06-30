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
#ifndef GUARD_DCC_VSTACK_CMP_C_INL
#define GUARD_DCC_VSTACK_CMP_C_INL 1

#include <dcc/common.h>
#include <dcc/vstack.h>
#include <dcc/gen.h>
#include <dcc/compiler.h>

DCC_DECL_BEGIN

LOCAL int DCC_VSTACK_CALL DCCStackValue_IsUnsignedReg(struct DCCStackValue const *__restrict self);
LOCAL int DCC_VSTACK_CALL DCCStackValue_IsUnsignedOrPtr(struct DCCStackValue const *__restrict self);

PRIVATE test_t DCC_VSTACK_CALL
DCCStackValue_CmpMem(struct DCCStackValue *__restrict self,
                     struct DCCMemLoc const *__restrict target,
                     target_siz_t n, int target_unsigned, test_t test) {
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc src;
  src.ml_reg = self->sv_reg;
  src.ml_sym = self->sv_sym;
  src.ml_off = self->sv_const.offset;
  test = DCCDisp_MemIcmpMem(test,&src,
                            DCCType_Sizeof(&self->sv_ctype,NULL,1),
                            DCCStackValue_IsUnsignedOrPtr(self),
                            target,n,target_unsigned);
 } else if (self->sv_reg == DCC_RC_CONST) {
  struct DCCSymAddr temp;
  struct DCCMemLoc used_target;
  struct DCCSym *done_sym = NULL;
  /* TODO: Optimization:
   * >> long long x = 42;
   * >> x >= 0; --> !(((uint8_t *)&x)[7]&0x80)
   * >> x <  0; -->  (((uint8_t *)&x)[7]&0x80)
   * >> ...
   */

  target_siz_t src_siz = DCCType_Sizeof(&self->sv_ctype,NULL,1);
  if (src_siz > sizeof(int_t)) src_siz = sizeof(int_t);
  while (n > DCC_TARGET_SIZEOF_ARITH_MAX || n&(n-1)) {
   test_t utest;
   target_siz_t part_size = DCC_TARGET_SIZEOF_ARITH_MAX;
   if (part_size > n) {
#if DCC_TARGET_SIZEOF_ARITH_MAX > 8
#error FIXME
#elif DCC_TARGET_SIZEOF_ARITH_MAX > 4
    if (n == 7 || n == 6 || n == 5)
        part_size = 4;
    else
#endif
    {
     assert(n == 3);
     part_size = 2;
    }
   }
   n -= part_size;
   temp.sa_off = 0;
   temp.sa_sym = NULL;
   if (n < src_siz) {
    temp.sa_off = (target_off_t)((target_ptr_t)self->sv_const.it >> (n*8));
   }
   used_target = *target;
   used_target.ml_off += n;
   if (!done_sym && (done_sym = DCCUnit_AllocSym()) == NULL) goto end;
   utest = DCC_TEST_UNSIGNED(test);
   DCCDisp_CstBinMem('?',&temp,&used_target,part_size,1);
   DCCDisp_SccTst(test,utest),test = utest;
   DCCDisp_SymJcc(DCC_DISP_ICMP_JCC(test),done_sym);
  }
  temp.sa_off = (target_off_t)self->sv_const.it;
  temp.sa_sym = self->sv_sym;
  DCCDisp_CstBinMem('?',&temp,target,n,1);
  if (done_sym) t_defsym(done_sym);
 } else {
  DCCStackValue_FixRegOffset(self);
  assert(!self->sv_sym);
  assert(!self->sv_const.it);
  assert(!DCC_RC_ISCONST(self->sv_reg));
  test = DCCDisp_RegsIcmpMems(test,self->sv_reg,self->sv_reg2,
                              DCCStackValue_IsUnsignedOrPtr(self),
                              target,n,target_unsigned);
 }
end:
 return test;
}
PRIVATE test_t DCC_VSTACK_CALL
DCCStackValue_CmpReg(struct DCCStackValue *__restrict self,
                     rc_t dst, rc_t dst2, int dst_unsigned, test_t test) {
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc src;
  src.ml_reg = self->sv_reg;
  src.ml_sym = self->sv_sym;
  src.ml_off = self->sv_const.offset;
  test = DCCDisp_MemsIcmpRegs(test,&src,
                              DCCType_Sizeof(&self->sv_ctype,NULL,1),
                              DCCStackValue_IsUnsignedOrPtr(self),
                              dst,dst2,dst_unsigned);
 } else if (self->sv_reg == DCC_RC_CONST) {
#if DCC_TARGET_SIZEOF_ARITH_MAX < 8
  struct DCCSymExpr temp;
  temp.e_int = self->sv_const.it;
  temp.e_sym = self->sv_sym;
  test = DCCDisp_CstIcmpRegs(test,&temp,DCCStackValue_IsUnsignedOrPtr(self),dst,dst2);
#else
  struct DCCSymAddr temp;
  temp.sa_off = self->sv_const.it;
  temp.sa_sym = self->sv_sym;
  DCCDisp_CstBinReg('?',&temp,dst,DCCStackValue_IsUnsignedOrPtr(self));
  /* NOTE: 'dst2' should always be 'DCC_RC_CONST' here... */
#endif
 } else {
  DCCStackValue_FixRegOffset(self);
  assert(!self->sv_sym);
  assert(!self->sv_const.it);
  assert(!DCC_RC_ISCONST(self->sv_reg));
  test = DCCDisp_RegsIcmpRegs(test,self->sv_reg,self->sv_reg2,
                              DCCStackValue_IsUnsignedOrPtr(self),
                              dst,dst2,dst_unsigned);
 }
 return test;
}

PRIVATE test_t DCC_VSTACK_CALL
DCCStackValue_Compare(struct DCCStackValue *__restrict self,
                      struct DCCStackValue *__restrict target,
                      test_t test) {
 /* Kill all tests.
  * This needs to be done for cases like this:
  * >> assert((a == b) == (c == d));
  */
 DCCVStack_KillTst();
 assert(!(target->sv_flags&DCC_SFLAG_TEST) || (compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN));
 assert(!(self->sv_flags&DCC_SFLAG_TEST)   || (compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN));
 DCCStackValue_LoadLValue(self);
 DCCStackValue_LoadLValue(target);
 DCCStackValue_FixBitfield(self);
 DCCStackValue_FixBitfield(target);
 if (target->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc mloc;
  mloc.ml_off = target->sv_const.offset;
  mloc.ml_reg = target->sv_reg;
  mloc.ml_sym = target->sv_sym;
  test = DCCStackValue_CmpMem(self,&mloc,DCCType_Sizeof(&target->sv_ctype,NULL,1),
                              DCCStackValue_IsUnsignedOrPtr(target),test);
 } else {
  if (target->sv_reg == DCC_RC_CONST)
       DCCStackValue_Load(target);
  else DCCStackValue_FixRegOffset(target);
  assert(!target->sv_sym);
  assert(!target->sv_const.it);
  assert(!DCC_RC_ISCONST(target->sv_reg));
  test = DCCStackValue_CmpReg(self,target->sv_reg,target->sv_reg2,
                              DCCStackValue_IsUnsignedOrPtr(target),test);
 }
 return test;
}

DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_CMP_C_INL */
