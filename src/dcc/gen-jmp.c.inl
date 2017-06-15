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
#ifndef GUARD_DCC_GEN_JMP_C_INL
#define GUARD_DCC_GEN_JMP_C_INL 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>

#include <string.h>

#include "x86_util.h"


DCC_DECL_BEGIN

struct jmpop { uint16_t r8,r1632; };
LOCAL void DCCDisp_TargetJmp(struct jmpop const *__restrict op,
                             struct DCCSymAddr const *__restrict addr);

/* Must handle special cases. e.g.:
 * >> jcc %reg
 * Compile as:
 * >>     jnc 1f
 * >>     jmp %reg
 * >> 1:
 */
PUBLIC void DCCDisp_RegJcc(test_t t, rc_t reg) {
 if (!(reg&(DCC_RC_I16|DCC_RC_I3264))) {
  /* Need at least 16-bit register. */
  reg = DCCVStack_CastReg(reg,1,DCC_RC_I16);
 }
 /* inverted, conditional jump with 8-bit disposition. */
 t_putb(0x70+(t^DCC_TEST_NBIT));
 /* Exact byte count from next byte until end of register-jump. */
 t_putb((!(reg&DCC_RC_I3264)) ? 3 : 2);
 /* Place a normal register-jump. */
 if (!(reg&DCC_RC_I3264)) t_putb(0x66);
 t_putb(0xff);
 asm_modreg(4,reg&DCC_RI_MASK);
}
PUBLIC void
DCCDisp_MemJcc(test_t t, struct DCCMemLoc const *__restrict src,
               target_siz_t n) {
 struct DCCMemLoc target_addr;
 struct DCCSym *temp_sym = DCCUnit_AllocSym();
 if unlikely(!temp_sym) return;
 target_addr.ml_reg = DCC_RC_CONST;
 target_addr.ml_off = 0;
 target_addr.ml_sym = temp_sym;
 /* Skip the jmp if the condition isn't met. */
 DCCDisp_LocJcc(t^DCC_TEST_NBIT,&target_addr);
 DCCDisp_MemJmp(src,n);
 t_defsym(temp_sym);
}

PUBLIC void
DCCDisp_LocJcc(test_t t, struct DCCMemLoc const *__restrict addr) {
 assert(addr);
 assertf(t >= 0 && t <= 0xf,"t = %x",t);
 if (addr->ml_reg != DCC_RC_CONST) {
  rc_t preg = DCCDisp_AddProtReg(&addr->ml_sad,
                                  addr->ml_reg);
  DCCDisp_RegJcc(t,preg);
 } else {
  struct jmpop op;
  op.r8    = (uint16_t)(0x70+t);
  op.r1632 = (uint16_t)(0x0f80+t);
  DCCDisp_TargetJmp(&op,&addr->ml_sad);
 }
}
PUBLIC void
DCCDisp_SymJcc(DCC(test_t) t, struct DCCSym *__restrict sym) {
 struct DCCMemLoc addr;
 addr.ml_reg = DCC_RC_CONST;
 addr.ml_off = 0;
 addr.ml_sym = sym;
 DCCDisp_LocJcc(t,&addr);
}
PUBLIC void
DCCDisp_LocJmp(struct DCCMemLoc const *__restrict addr) {
 if (addr->ml_reg != DCC_RC_CONST) {
  rc_t preg = DCCDisp_AddProtReg(&addr->ml_sad,
                                  addr->ml_reg);
  DCCDisp_RegJmp(preg);
 } else {
  static struct jmpop const jcc_jmp = {0xeb,0xe9};
  DCCDisp_TargetJmp(&jcc_jmp,&addr->ml_sad);
 }
}

LOCAL void
DCCDisp_TargetJmp(struct jmpop const *__restrict op,
                  struct DCCSymAddr const *__restrict addr) {
 struct DCCSymAddr used_addr;
 size_t minbytes; uint8_t temp;
 target_off_t off_iter;
 assert(op);
 assert(addr);
 used_addr = *addr;
 if (used_addr.sa_sym) {
  struct DCCSymAddr symaddr;
  if (DCCSym_LoadAddr(used_addr.sa_sym,&symaddr,0)) {
   if (DCCSym_SECTION(symaddr.sa_sym) == unit.u_curr) {
    off_iter = symaddr.sa_off;
    off_iter += symaddr.sa_sym->sy_addr-
     (t_addr+/* Worst cast for immediate value */DCC_TARGET_SIZEOF_POINTER+
             /* Leaway for opcode bytes. */      4);
    goto def_minbytes;
   }
  }
  /* Must assume worst-case for symbols with unknown relation. */
  minbytes = DCC_TARGET_SIZEOF_POINTER;
 } else {
  off_iter = used_addr.sa_off;
def_minbytes:
  minbytes = 0;
  if (off_iter < 0) off_iter = -off_iter,++minbytes;
  while (off_iter) ++minbytes,off_iter >>= 1;
  minbytes = (minbytes+7)/8;
 }
 assert(minbytes <= DCC_TARGET_SIZEOF_POINTER);
 if (minbytes >= 2) {
  /* 16-bit disposition. */
  //if (minbytes < 4) t_putb(0x66);
  /* 16/32-bit disposition. */
  temp = (uint8_t)(op->r1632 >> 8);
  if (temp) t_putb(temp);
  t_putb((uint8_t)op->r1632);
  /*if (minbytes < 4) DCCDisp_SymDisp16(&used_addr);
  else              */DCCDisp_SymDisp32(&used_addr);
 } else {
  /* 8-bit disposition. */
  temp = (uint8_t)(op->r8 >> 8);
  if (temp) t_putb(temp);
  t_putb((uint8_t)op->r8);
  DCCDisp_SymDisp8(&used_addr);
 }
}

PUBLIC void DCCDisp_RegJmp(rc_t reg) {
 if (!(reg&(DCC_RC_I16|DCC_RC_I3264))) {
  /* Need at least 16-bit register. */
  reg = DCCVStack_CastReg(reg,1,DCC_RC_I16);
 }
 if (!(reg&DCC_RC_I3264)) t_putb(0x66);
 t_putb(0xff);
 asm_modreg(4,reg&DCC_RI_MASK);
}
PUBLIC void
DCCDisp_MemJmp(struct DCCMemLoc const *__restrict src,
               target_siz_t n) {
 if (n == 1) {
  rc_t temp_reg;
  /* Need at least 16-bit register. */
  temp_reg = DCCVStack_GetReg(DCC_RC_I16,0);
  DCCDisp_IntMovReg(0,temp_reg);
  DCCDisp_MemMovReg(src,temp_reg&~(DCC_RC_I16));
  DCCDisp_RegJmp(temp_reg);
 } else if (n) {
  if (n < 4) t_putb(0x66);
  t_putb(0xff);
  asm_modmem(4,src);
 } else {
  rc_t temp = DCCVStack_GetReg(DCC_RC_PTR,1);
  DCCDisp_IntMovReg(0,temp);
  DCCDisp_RegJmp(temp);
 }
}






PUBLIC void
DCCDisp_SccReg(test_t t, rc_t reg) {
 rc_t used_reg = reg;
 assert(t >= 0 && t <= 0xf);
 if (used_reg&(DCC_RC_I16|DCC_RC_I3264) &&
     used_reg&4) {
  used_reg = DCCVStack_GetReg(DCC_RC_I8,0);
 }
 used_reg &= DCC_RI_MASK;
 /* setcc %reg */
 t_putb(0x0f);
 t_putb(0x90+t);
 asm_modreg(0,(uint8_t)used_reg);
 /* Make sure to fill the whole requested width. */
 DCCDisp_RegMovReg(used_reg|DCC_RC_I8,reg,1);
}
PUBLIC void
DCCDisp_SccMem(test_t t, struct DCCMemLoc const *__restrict dst,
               target_siz_t n) {
 assert(t >= 0 && t <= 0xf);
 if unlikely(!n) return;
 /* setcc offset(%reg) */
 t_putb(0x0f);
 t_putb(0x90+t);
 asm_modmem(0,dst);
 if (--n) {
  /* TODO: Optimize for special 'n' values:
   * n == 2: >> setcc %temp8
   *         >> movzx %temp8, %temp16
   *         >> mov %temp16, !dst
   * n == 4: >> setcc %temp8
   *         >> movzx %temp8, %temp32
   *         >> mov %temp32, !dst
   * >> That way, memory must only be written once!
   */
  struct DCCMemLoc rest = *dst;
  ++rest.ml_off;
  /* Fill the rest with ZERO. */
  DCCDisp_BytMovMem(0,n,&rest,n,1);
 }
}


PUBLIC void
DCCDisp_LocCll(struct DCCMemLoc const *__restrict addr) {
 if (addr->ml_reg) {
  rc_t preg = DCCDisp_AddProtReg(&addr->ml_sad,
                                  addr->ml_reg);
  DCCDisp_UnaryReg('(',preg);
 } else {
  /* call symaddr */
  t_putb(0xe8);
  DCCDisp_SymDisp32(&addr->ml_sad);
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_GEN_JMP_C_INL */
