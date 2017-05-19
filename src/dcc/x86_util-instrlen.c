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
#ifndef GUARD_DCC_X86_UTIL_INSTRLEN_C
#define GUARD_DCC_X86_UTIL_INSTRLEN_C 1

#include <dcc/common.h>
#include <dcc/target.h>

#include "x86_util.h"

#if DCC_CONFIG_NEED_X86_INSTRLEN

#include <stdio.h>

DCC_DECL_BEGIN



#if INSTRLEN_DEBUG
INTERN size_t    instrlen_offset = 0;
INTERN uint8_t  *instrlen_base   = 0;
#define ADDR(x) (instrlen_offset+(size_t)((x)-instrlen_base))
#endif /* INSTRLEN_DEBUG */

uint8_t const *x86_instrlen(uint8_t const *p) {
#if INSTRLEN_DEBUG
 uint8_t const *start = p;
#endif
 uint8_t opcode;
 unsigned int flags = 0;
 /* TODO: Everything starting at 'fcomi' is _NOT_ implemented! */
 size_t suffix = 0;
#define F(x) (!!(flags&(x)))
#define F_16B 0x01
 for (;;) {
  opcode = *p++;
  switch (opcode) {
  case 0x64: case 0x65: /* FS/GS prefix */
  case 0x36:            /* SS prefix */
  case 0x66: if (flags&F_16B) goto done_prefix; flags |= F_16B; break;
  case 0x67:
  case 0xf0: /* lock */
  case 0xf2: /* repne */
  case 0xf3:
  case 0x2e: /* CS prefix */
  case 0x3e: /* DS prefix */
  case 0x9b: /* fwait */
   break;
  default: goto done_prefix;
  }
 }
done_prefix:
 switch (opcode) {

  /* === Simple 1-byte opcodes. */
 case 0x37: /* aaa */
 case 0x3f: /* aas */
 case 0x98: /* cbw / cwde */
 case 0xf8: /* clc */
 case 0xfc: /* cld */
 case 0xfa: /* cli */
 case 0xf5: /* cmc */
 case 0xa6: /* cmpsb */
 case 0xa7: /* cmpsw / cmpsl */
 case 0x99: /* cwd / cdq */
 case 0x27: /* daa */
 case 0x2f: /* das */
 case 0x48: case 0x49: case 0x4a: case 0x4b: /* dec */
 case 0x4c: case 0x4d: case 0x4e: case 0x4f: /* ... */
 case 0xf4: /* hlt */
 case 0xec: /* in %dx, %al */
 case 0xed: /* in %dx, %ax / in %dx, %eax */
 case 0x40: case 0x41: case 0x42: case 0x43: /* inc */
 case 0x44: case 0x45: case 0x46: case 0x47: /* ... */
 case 0x6c: /* insb */
 case 0x6d: /* insw / insl */
 case 0xcc: /* int $3 */
 case 0xce: /* into */
 case 0xcf: /* iret */
 case 0x9f: /* lahf */
 case 0xc9: /* leave */
 case 0xac: /* lodsb */
 case 0xad: /* lodsw / lodsl */
 case 0xa4: /* movsb */
 case 0xa5: /* movsw / movsl */
 case 0x90: /* nop */
 case 0xee: /* out %dx, %al */
 case 0xef: /* out %dx, %ax / out %dx, %eax */
 case 0x6e: /* outsb */
 case 0x6f: /* outsw / outsl */
 case 0x58: case 0x59: case 0x5a: case 0x5b: /* pop %r16 / pop %r32 */
 case 0x5c: case 0x5d: case 0x5e: case 0x5f: /* *ditto* */
 case 0x1f: /* pop %ds */
 case 0x07: /* pop %es */
 case 0x17: /* pop %ss */
 case 0x61: /* popa / popad / popaw */
 case 0x9d: /* popf / popfd / popfw */
 case 0x50: case 0x51: case 0x52: case 0x53: /* push %r16 / push %r32 */
 case 0x54: case 0x55: case 0x56: case 0x57: /* *ditto* */
 case 0x1e: /* push %ds */
 case 0x06: /* push %es */
 case 0x0e: /* push %cs */
 case 0x16: /* push %ss */
 case 0x60: /* pusha / pushad / pushaw */
 case 0x9c: /* pushf / pushfd / pushfw */
 case 0xc3: /* ret */
 case 0x9b: /* sahf */
 case 0xae: /* scasb */
 case 0xaf: /* scasw / scasl */
 case 0xf9: /* stc */
 case 0xfd: /* std */
 case 0xfb: /* sti */
 case 0xaa: /* stosb */
 case 0xab: /* stosw / stosl */
            case 0x91: case 0x92: case 0x93: /* xchg %r16, %ax / xchg %r32, %ax */
 case 0x94: case 0x95: case 0x96: case 0x97: /* *ditto* */
 case 0xd7: /* xlat */
  break;

  /* === 1-byte opcodes with 1-byte immediate argument. */
 case 0xd5: /* aad */
 case 0x14: /* adc $imm8, %al */
 case 0x04: /* add $imm8, %al */
 case 0x24: /* and $imm8, %al */
 case 0x3c: /* cmp $imm8, %al */
 case 0xe4: /* in $imm8, %al */
 case 0xe5: /* in $imm8, %ax / in $imm8, %eax */
 case 0xcd: /* int $imm8 */
 case 0xe3: /* jcxz rel8 / jecxz rel8 */
 case 0x77: /* ja rel8 */
 case 0x73: /* jae rel8 */
 case 0x72: /* jb rel8 */
 case 0x76: /* jbe rel8 */
 case 0x74: /* je rel8 */
 case 0x7f: /* jg rel8 */
 case 0x7d: /* jge rel8 */
 case 0x7c: /* jl rel8 */
 case 0x7e: /* jle rel8 */
 case 0x75: /* jne rel8 */
 case 0x71: /* jno rel8 */
 case 0x7b: /* jnp rel8 */
 case 0x79: /* jns rel8 */
 case 0x70: /* jo rel8 */
 case 0x7a: /* jp rel8 */
 case 0x78: /* js rel8 */
 case 0xeb: /* jmp rel8 */
 case 0xa0: /* mov rel8, %al */
 case 0xa2: /* mov %al, rel8 */
 case 0xb0: case 0xb1: case 0xb2: case 0xb3: /* mov $imm8, r8 */
 case 0xb4: case 0xb5: case 0xb6: case 0xb7: /* *ditto* */
 case 0x0c: /* or $imm8, %al */
 case 0xe6: /* out $imm8, %al */
 case 0xe7: /* out $imm8, %ax / out $imm8, %eax */
 case 0x6a: /* push $imm8 */
 case 0x1c: /* sbb $imm8, %al */
 case 0x2c: /* sub $imm8, %al */
 case 0xa8: /* test $imm8, %al */
 case 0x34: /* xor $imm8, %al */
done_p1: p += 1;
  break;

  /* === 1-byte opcodes with 2-byte immediate argument. */
 case 0xc2: /* ret $imm16 */
  goto done_p2;

  /* === 1-byte opcodes with 2/4-byte immediate argument. */
 case 0x15: /* adc $imm16, %ax / adc $imm32, %eax */
 case 0x05: /* add $imm16, %ax / adc $imm32, %eax */
 case 0x25: /* and $imm16, %ax / adc $imm32, %eax */
 case 0xe8: /* call rel16 / call rel32 */
 case 0x3d: /* cmp $imm16, %ax / cmd $imm32, %eax */
 case 0xe9: /* jmp rel16 / jmp rel32 */
 case 0xa1: /* mov rel16, %ax / mov rel32, %eax */
 case 0xa3: /* mov %ax, rel16 / mov %eax, rel32 */
 case 0xb8: case 0xb9: case 0xba: case 0xbb: /* mov imm16, r16 / mov imm32, r32 */
 case 0xbc: case 0xbd: case 0xbe: case 0xbf: /* *ditto* */
 case 0x0d: /* or $imm16, %ax / or $imm32, %eax */
 case 0x68: /* push $imm16 / push $imm32 */
 case 0x1d: /* sbb $imm16, %ax / sbb $imm32, %eax */
 case 0x2d: /* sub $imm16, %ax / sub $imm32, %eax */
 case 0xa9: /* test $imm16, %ax / test $imm32, %ax */
 case 0x35: /* xor $imm16, %ax / xor $imm32, %eax */
done_2or4:
  if (F(F_16B)) done_p2: p += 2;
  else          done_p4: p += 4;
  break;

  /* === 1-byte opcodes with 1-byte immediate value & modr/m operand. */
 case 0x80: /* adc $imm8, r/m8 */
 case 0x83: /* adc $imm8s, r/m16 / adc $imm8s, r/m32 */
 case 0x6b: /* imul $imm8, r/m16, %r16 / imul $imm8, r/m32, %r32 */
 case 0xc6: /* mov $imm8, r/m8 */
 case 0xc0: /* rcl $imm8, r/m8 / rcr $imm8, r/m8
             * rol $imm8, r/m8 / ror $imm8, r/m8
             * sal $imm8, r/m8 / sar $imm8, r/m8
             * shl $imm8, r/m8 / shr $imm8, r/m8 */
 case 0xc1: /* rcl $imm8, r/m16 / rcl $imm8, r/m32 / rcr $imm8, r/m16 / rcr $imm8, r/m32
             * rol $imm8, r/m16 / rol $imm8, r/m32 / ror $imm8, r/m16 / ror $imm8, r/m32
             * sal $imm8, r/m16 / sal $imm8, r/m32 / sar $imm8, r/m16 / sar $imm8, r/m32
             * shl $imm8, r/m16 / shl $imm8, r/m32 / shr $imm8, r/m16 / shr $imm8, r/m32 */
  ++suffix;
  goto modrm;

  /* === 1-byte opcodes with 2/4-byte immediate value & modr/m operand. */
 case 0x81: /* adc $imm16, r/m16 / adc $imm32, r/m32 */
 case 0x69: /* imul $imm16, r/m16, %r16 / imul $imm32, r/m32, %r32 */
 case 0xc7: /* mov imm16, r/m16 / mov imm32, r/m32 */
  if (F(F_16B)) suffix += 2;
  else          suffix += 4;
  goto modrm;

  /* === 1-byte opcodes with modr/m operand. */
 case 0x10: /* adc r8, r/m8 */
 case 0x11: /* adc r16, r/m16 / adc r32, r/m32 */
 case 0x12: /* adc r/m8, r8 */
 case 0x13: /* adc r/m16, r16 / adc r/m32, r32 */
 case 0x00: /* add r8, r/m8 */
 case 0x01: /* add r16, r/m16 / add r32, r/m32 */
 case 0x02: /* add r/m8, r8 */
 case 0x03: /* add r/m16, r16 / add r/m32, r32 */
 case 0x20: /* and r8, r/m8 */
 case 0x21: /* and r16, r/m16 / and r32, r/m32 */
 case 0x22: /* and r/m8, r8 */
 case 0x23: /* and r/m16, r16 / and r/m32, r32 */
 case 0x63: /* arpl r16, r/m16 */
 case 0x62: /* bound m16, r16 / bound m32, r32 */
 case 0xff: /* call *r/m16 / call *r/m32 / dec r/m16 / dec r/m32 / inc r/m16 / inc r/m32
             * jmp *r/m16 / jmp *r/m32 / ljmp m16 / ljmp m32 */
 case 0x38: /* cmp r8, r/m8 */
 case 0x39: /* cmp r16, r/m16 / cmp r32, r/m32 */
 case 0x3a: /* cmp r/m8, r8 */
 case 0x3b: /* cmp r/m16, r16 / cmp r/m32, r32 */
 case 0xfe: /* dec r/m8 / inc r/m8 */
  /* Floating point (st(i)-operations) prefix bytes */
 case 0xd8: case 0xd9: case 0xda: case 0xdb:
 case 0xdc: case 0xdd: case 0xde: case 0xdf:
 case 0xc5: /* lds m16, r16 / lds m32, r32 */
 case 0xc4: /* les m16, r16 / les m32, r32 */
 case 0x8d: /* lea m, r16 / lea m, r32 */
 case 0x88: /* mov r8, r/m8 */
 case 0x89: /* mov r16, r/m16 / mov r32, r/m32 */
 case 0x8a: /* mov r/m8, r8 */
 case 0x8b: /* mov r/m16, r16 / mov r/m32, r32 */
 case 0x8c: /* mov Sreg, r/m16 / mov Sreg, r/m32 */
 case 0x8e: /* mov r/m16, Sreg / mov r/m32, Sreg */
 case 0x08: /* or r8, r/m8 */
 case 0x09: /* or r16, r/m16 / or r32, r/m32 */
 case 0x0a: /* or r/m8, r8 */
 case 0x0b: /* or r/m16, r16 / or r/m32, r32 */
 case 0x8f: /* pop r/m16 / pop r/m32 */
 case 0xd0: /* rcl r/m8 / rcr r/m8
             * rol r/m8 / ror r/m8
             * sal r/m8 / sar r/m8
             * shl r/m8 / shr r/m8 */
 case 0xd1: /* rcl r/m16 / rcl r/m32 / rcr r/m16 / rcr r/m32
             * rol r/m16 / rol r/m32 / ror r/m16 / ror r/m32
             * sal r/m16 / sal r/m32 / sar r/m16 / sar r/m32
             * shl r/m16 / shl r/m32 / shr r/m16 / shr r/m32 */
 case 0xd2: /* rcl %cl, r/m8 / rcr %cl, r/m8
             * rol %cl, r/m8 / ror %cl, r/m8
             * sal %cl, r/m8 / sar %cl, r/m8
             * shl %cl, r/m8 / shr %cl, r/m8 */
 case 0xd3: /* rcl %cl, r/m16 / rcl %cl, r/m32 / rcr %cl, r/m16 / rcr %cl, r/m32
             * rol %cl, r/m16 / rol %cl, r/m32 / ror %cl, r/m16 / ror %cl, r/m32
             * sal %cl, r/m16 / sal %cl, r/m32 / sar %cl, r/m16 / sar %cl, r/m32
             * shl %cl, r/m16 / shl %cl, r/m32 / shr %cl, r/m16 / shr %cl, r/m32 */
 case 0x18: /* sbb r8, r/m8 */
 case 0x19: /* sbb r16, r/m16 / sbb r32, r/m32 */
 case 0x1a: /* sbb r/m8, r8 */
 case 0x1b: /* sbb r/m16, r16 / sbb r/m32, r32 */
 case 0x28: /* sub r8, r/m8 */
 case 0x29: /* sub r16, r/m16 / sub r32, r/m32 */
 case 0x2a: /* sub r/m8, r8 */
 case 0x2b: /* sub r/m16, r16 / sub r/m32, r32 */
 case 0x84: /* test r8, r/m8 */
 case 0x85: /* test r16, r/m16 / test r32, r/m32 */
 case 0x86: /* xchg %r8, r/m8 / xchg r/m8, %r8 */
 case 0x87: /* xchg %r16, r/m16 / xchg r/m16, %r16
             * xchg %r32, r/m32 / xchg r/m32, %r32 */
 case 0x30: /* xor r8, r/m8 */
 case 0x31: /* xor r16, r/m16 / xor r32, r/m32 */
 case 0x32: /* xor r/m8, r8 */
 case 0x33: /* xor r/m16, r16 / xor r/m32, r32 */
  goto modrm;

  /* Special arithmetic opcodes. */
 case 0xf6: /* div r/m8 / idiv r/m8 / mul r/m8 / imul r/m8 / neg r/m8 / not r/m8 */
 case 0xf7: /* div r/m16 / div r/m32 / idiv r/m16 / idiv r/m32
             * mul r/m16 / mul r/m32 / imul r/m16 / imul r/m32
             * neg r/m16 / neg r/m32 / not r/m16 / not r/m32 */
  opcode = *p++;
  /* 'test' (with a r/m group of '0' ('MODRM_REG_MASK') has 8/16/32 bits of immediate data) */
  if ((opcode&MODRM_REG_MASK) == 0) {
   if (p[-2] == 0xf6) suffix += 1;
   else if (F(F_16B)) suffix += 2;
   else               suffix += 4;
  }
  goto modrm_fetched;

  /* === 1-byte opcodes with 3-byte operand. */
 case 0xc8: /* enter $imm16, $imm8 */
  p += 3;
  break;

  /* === 1-byte opcodes with 4/6-byte operand. */
 case 0x9a: /* lcall ptr16,16 / lcall ptr16,32 */
 case 0xea: /* ljmp ptr16,16 / ljmp ptr16,32 */
  p += 2;
  goto done_2or4;

 case 0x0f: /* Prefix byte: 0x0f */
  opcode = *p++;
  switch (opcode) {

   /* === 2-byte opcodes with modr/m operand. */
  case 0x58: /* addpd xmm2/m128, xmm1 */
  case 0xd0: /* addsubpd xmm2/m128, xmm1 */
  case 0x54: /* andpd xmm2/m128, xmm1 */
  case 0x55: /* andnpd xmm2/m128, xmm1 */
  case 0xbc: /* bsf r/m16, r16 */
  case 0xbd: /* bsr r/m16, r16 */
  case 0xa3: /* bt r16, r/m16 / bt r32, r/m32 */
  case 0xbb: /* btc r16, r/m16 / btc r32, r/m32 */
  case 0xb3: /* btr r16, r/m16 / btr r32, r/m32 */
  case 0xab: /* bts r16, r/m16 / bts r32, r/m32 */
  case 0xae: /* clflush m8 / fxrstor m512byte / fxsave m512byte
              * ldmxcsr m32 / lfence / mfence / sfence
              * stmxcsr m32 */
  case 0x47: /* cmova r/m16, r16 */
  case 0x43: /* cmovae r/m16, r16 */
  case 0x42: /* cmovb r/m16, r16 */
  case 0x46: /* cmovbe r/m16, r16 */
  case 0x44: /* cmove r/m16, r16 */
  case 0x4f: /* cmovg r/m16, r16 */
  case 0x4d: /* cmovge r/m16, r16 */
  case 0x4c: /* cmovl r/m16, r16 */
  case 0x4e: /* cmovle r/m16, r16 */
  case 0x45: /* cmovne r/m16, r16 */
  case 0x41: /* cmovno r/m16, r16 */
  case 0x4b: /* cmovnp r/m16, r16 */
  case 0x49: /* cmovns r/m16, r16 */
  case 0x40: /* cmovo r/m16, r16 */
  case 0x4a: /* cmovp r/m16, r16 */
  case 0x48: /* cmovs r/m16, r16 */
  case 0xb0: /* cmpxch r8, r/m8 */
  case 0xb1: /* cmpxch r16, r/m16 / cmpxch r32, r/m32 */
  case 0xc7: /* cmpxchg8b m64 */
  case 0x2f: /* comisd xmm2/m64, xmm1 */
  case 0xe6: /* cvtdq2pd xmm2/m64, xmm1 */
  case 0x5b: /* cvtdq2ps xmm2/m128, xmm1 */
  case 0x5a: /* cvtpd2ps xmm2/m128, xmm1 / cvtsd2ss xmm2/m64, xmm1 */
  case 0x2a: /* cvtpi2pd mm/m64, xmm */
  case 0x2d: /* cvtps2pi xmm/m64, mm */
  case 0x2c: /* cvttpd2pi xmm/m128, mm */
  case 0x5e: /* divpd xmm2/m128, xmm1 */
  case 0x7c: /* haddpd xmm2/m128, xmm1 / haddps xmm2/m128, xmm1 */
  case 0x7d: /* hsubpd xmm2/m128, xmm1 / hsubps xmm2/m128, xmm1 */
  case 0xaf: /* imul r/m16, %r16 / imul r/m32, %r32 */
  case 0x01: /* invlpg m / lgdt m48 / lidt m48 / lmsw r/m16
              * monitor / mwait / sgdt m48 / sidt m48
              * smsw r/m16 / smsw r32/m16 */
  case 0x02: /* lar r/m16, r16 / lar r/m32, r32 */
  case 0xf0: /* lddqu m128, xmm */
  case 0xb2: /* lss m16, r16 / lss m32, r32 */
  case 0xb4: /* lfs m16, r16 / lfs m32, r32 */
  case 0xb5: /* lgs m16, r16 / lgs m32, r32 */
  case 0x00: /* lldt r/m16 / ltr r/m16 / sldt r/m16 / str r/m16
              * verr r/m16 / verw r/m16 */
  case 0x03: /* lsl r/m16, %r16 / lsl r/m32, %r32 */
  case 0xf7: /* maskmovdqu xmm2, xmm1 / maskmovq mm2, mm1 */
  case 0x5f: /* maxpd xmm2/m128, xmm1 / maxps xmm2/m128, xmm1
              * maxpd xmm2/m64, xmm1 / maxps xmm2/m64, xmm1 */
  case 0x5d: /* minpd xmm2/m128, xmm1 / minps xmm2/m128, xmm1
              * minpd xmm2/m64, xmm1 / minps xmm2/m64, xmm1 */
  case 0x20: /* mov crx, r32 */
  case 0x21: /* mov drx, r32 */
  case 0x24: /* mov trx, r32 */
  case 0x22: /* mov r32, crx */
  case 0x23: /* mov r32, drx */
  case 0x26: /* mov r32, trx */
  case 0x28: /* movapd xmm1, xmm2/m128 / movaps xmm1, xmm2/m128 */
  case 0x29: /* movapd xmm2/m128, xmm1 / movaps xmm2/m128, xmm1 */
  case 0x6e: /* movd r/m32, mm / movd r/m32, xmm */
  case 0x7e: /* movd mm, r/m32  / movd xmm, r/m32
              * movq xmm/m64, xmm */
  case 0x12: /* movddup xmm2/m64, xmm1 / movhlps xmm2, xmm1
              * movlpd m64, xmm / movlps m64, xmm / movsldup xmm2/m128, xmm1 */
  case 0x13: /* movlpd xmm, m64 / movlps xmm, m64 */
  case 0x6f: /* movdqa xmm2/m128, xmm1 / movdqu xmm2/m128, xmm1
              * movq mm/m64, mm */
  case 0x7f: /* movdqa xmm1, xmm2/m128 / movdqu xmm1, xmm2/m128
              * movq mm, mm/m64 */
  case 0xd6: /* movdq2q xmm, mm / movq xmm, xmm/m64 / movq2dq mm, xmm */
  case 0x16: /* movhpd m64, xmm / movhps m64, xmm / movlhps xmm2, xmm1 / movshdup xmm2/m128, xmm1 */
  case 0x17: /* movhpd xmm, m64 / movhps xmm, m64 */
  case 0x50: /* movmskpd xmm, r32 / movmskps xmm, r32 */
  case 0xe7: /* movntdq xmm, m128 / movntq mm, m64 */
  case 0xc3: /* movnti r32, m32 */
  case 0x2b: /* movntpd xmm, m128 / movntps xmm, m128 */
  case 0x10: /* movsd xmm2/m64, xmm1 / movss xmm2/m32, xmm1
              * movapd xmm2/m128, xmm1 / movups xmm2/m128, xmm1 */
  case 0x11: /* movsd xmm1, xmm2/m64 / movss xmm1, xmm2/m32
              * movapd xmm1, xmm2/m128 / movups xmm1, xmm2/m128 */
  case 0xbe: /* movsx r/m8, r16 / movsx r/m8, r32 */
  case 0xbf: /* movsx r/m16, r32 */
  case 0xb6: /* movzx r/m8, r16 / movzx r/m8, r32 */
  case 0xb7: /* movzx r/m16, r32 */
  case 0x59: /* mulpd xmm2/m128, xmm1 / mulps xmm2/m128, xmm1
              * mulsd xmm2/m64, xmm1 / mulss xmm2/m64, xmm1 */
  case 0x56: /* orpd xmm2/m128, xmm1 / orps xmm2/m128, xmm1 */
  case 0x63: /* packsswb mm2/m64, mm1 / packsswb xmm2/m128, xmm1 */
  case 0x6b: /* packssdw mm2/m64, mm1 / packssdw xmm2/m128, xmm1 */
  case 0x67: /* packuswb mm2/m64, mm1 / packuswb xmm2/m128, xmm1 */
  case 0xfc: /* paddb mm2/m64, mm1 / paddb xmm2/m128, xmm1 */
  case 0xfd: /* paddw mm2/m64, mm1 / paddw xmm2/m128, xmm1 */
  case 0xfe: /* paddl mm2/m64, mm1 / paddl xmm2/m128, xmm1 */
  case 0xd4: /* paddq mm2/m64, mm1 / paddq xmm2/m128, xmm1 */
  case 0xec: /* paddsb mm2/m64, mm1 / paddsb xmm2/m128, xmm1 */
  case 0xed: /* paddsw mm2/m64, mm1 / paddsw xmm2/m128, xmm1 */
  case 0xdc: /* paddusb mm2/m64, mm1 / paddusb xmm2/m128, xmm1 */
  case 0xdd: /* paddusw mm2/m64, mm1 / paddusw xmm2/m128, xmm1 */
  case 0xdb: /* pand mm2/m64, mm1 / pand xmm2/m128, xmm1 */
  case 0xdf: /* pandn mm2/m64, mm1 / pandn xmm2/m128, xmm1 */
  case 0x90: /* pause */
  case 0xe0: /* pavgbb mm2/m64, mm1 / pavgbb xmm2/m128, xmm1 */
  case 0xe3: /* pavgbw mm2/m64, mm1 / pavgbw xmm2/m128, xmm1 */
  case 0x74: /* pcmpeqb mm2/m64, mm1 / pcmpeqb xmm2/m128, xmm1 */
  case 0x75: /* pcmpeqw mm2/m64, mm1 / pcmpeqw xmm2/m128, xmm1 */
  case 0x76: /* pcmpeql mm2/m64, mm1 / pcmpeql xmm2/m128, xmm1 */
  case 0x64: /* pcmpgtb mm2/m64, mm1 / pcmpgtb xmm2/m128, xmm1 */
  case 0x65: /* pcmpgtw mm2/m64, mm1 / pcmpgtw xmm2/m128, xmm1 */
  case 0x66: /* pcmpgtl mm2/m64, mm1 / pcmpgtl xmm2/m128, xmm1 */
  case 0xf5: /* pmaddwd mm2/m64, mm1 / pmaddwd xmm2/m128, xmm1 */
  case 0xee: /* pmaxsw mm2/m64, mm1 / pmaxsw xmm2/m128, xmm1 */
  case 0xde: /* pmaxub mm2/m64, mm1 / pmaxub xmm2/m128, xmm1 */
  case 0xea: /* pminsw mm2/m64, mm1 / pminsw xmm2/m128, xmm1 */
  case 0xda: /* pminub mm2/m64, mm1 / pminub xmm2/m128, xmm1 */
  case 0xd7: /* pmovmskb r32, mm1 / pmovmskb r32, xmm1 */
  case 0xe4: /* pmulhuw mm2/m64, mm1 / pmulhuw xmm2/m128, xmm1 */
  case 0xe5: /* pmulhw mm2/m64, mm1 / pmulhw xmm2/m128, xmm1 */
  case 0xd5: /* pmullw mm2/m64, mm1 / pmullw xmm2/m128, xmm1 */
  case 0xf4: /* pmuludq mm2/m64, mm1 / pmuludq xmm2/m128, xmm1 */
  case 0xeb: /* por mm2/m64, mm1 / por xmm2/m128, xmm1 */
  case 0x18: /* prefetcht(0|1|2) m8 / prefetchnta m8 */
  case 0xf6: /* psadbw mm2/m64, mm1 / psadbw xmm2/m128, xmm1 */
  case 0xf1: /* psllw mm2/m64, mm1 / psllw xmm2/m128, xmm1 */
  case 0xf2: /* pslld mm2/m64, mm1 / pslld xmm2/m128, xmm1 */
  case 0xf3: /* psllq mm2/m64, mm1 / psllq xmm2/m128, xmm1 */
  case 0xe1: /* psraw mm2/m64, mm1 / psraw xmm2/m128, xmm1 */
  case 0xe2: /* psrad mm2/m64, mm1 / psrad xmm2/m128, xmm1 */
  case 0xd1: /* psrlw mm2/m64, mm1 / psrlw xmm2/m128, xmm1 */
  case 0xd2: /* psrld mm2/m64, mm1 / psrld xmm2/m128, xmm1 */
  case 0xd3: /* psrlq mm2/m64, mm1 / psrlq xmm2/m128, xmm1 */
  case 0xf8: /* psubb mm2/m64, mm1 / psubb xmm2/m128, xmm1 */
  case 0xf9: /* psubw mm2/m64, mm1 / psubw xmm2/m128, xmm1  */
  case 0xfa: /* psubd mm2/m64, mm1 / psubd xmm2/m128, xmm1  */
  case 0xfb: /* psubq mm2/m64, mm1 / psubq xmm2/m128, xmm1  */
  case 0xe8: /* psubsb mm2/m64, mm1 / psubsb xmm2/m128, xmm1 */
  case 0xe9: /* psubsw mm2/m64, mm1 / psubsw xmm2/m128, xmm1 */
  case 0xd8: /* psubusb mm2/m64, mm1 / psubusb xmm2/m128, xmm1 */
  case 0xd9: /* psubusw mm2/m64, mm1 / psubusw xmm2/m128, xmm1 */
  case 0x68: /* punpckhbw mm2/m64, mm1 / punpckhbw xmm2/m128, xmm1 */
  case 0x69: /* punpckhbd mm2/m64, mm1 / punpckhbd xmm2/m128, xmm1 */
  case 0x6a: /* punpckhbq mm2/m64, mm1 / punpckhbq xmm2/m128, xmm1 */
  case 0x6d: /* punpckhqdq xmm2/m128, xmm1 */
  case 0x60: /* punpcklbw mm2/m64, mm1 / punpcklbw xmm2/m128, xmm1 */
  case 0x61: /* punpcklbd mm2/m64, mm1 / punpcklbd xmm2/m128, xmm1 */
  case 0x62: /* punpcklbq mm2/m64, mm1 / punpcklbq xmm2/m128, xmm1 */
  case 0x6c: /* punpcklqdq xmm2/m128, xmm1 */
  case 0xef: /* pxor mm2/m64, mm1 / pxor xmm2/m128, xmm1 */
  case 0x53: /* rcpps xmm2/m128, xmm1 / rcpss xmm2/m32, xmm1 */
  case 0x52: /* rsqrtps xmm2/m128, xmm1 / rsqrtss xmm2/m128, xmm1 */
  case 0x97: /* seta r/m8 */
  case 0x93: /* setae r/m8 */
  case 0x92: /* setb r/m8 */
  case 0x96: /* setbe r/m8 */
  case 0x94: /* sete r/m8 */
  case 0x9f: /* setg r/m8 */
  case 0x9d: /* setge r/m8 */
  case 0x9c: /* setl r/m8 */
  case 0x9e: /* setle r/m8 */
  case 0x95: /* setne r/m8 */
  case 0x91: /* setno r/m8 */
  case 0x9b: /* setnp r/m8 */
  case 0x99: /* setns r/m8 */
  case 0x9a: /* setp r/m8 */
  case 0x98: /* sets r/m8 */
  case 0xa5: /* shld %cl, %r16, r/m16 / shld %cl, %r32, r/m32 */
  case 0xad: /* shrd %cl, %r16, r/m16 / shrd %cl, %r32, r/m32 */
  case 0x51: /* sqrtpd xmm2/m128, xmm1 / sqrtps xmm2/m128, xmm1
              * sqrtsd xmm2/m64, xmm1 / sqrtss xmm2/m32, xmm1 */
  case 0x5c: /* subpd xmm2/m128, xmm1 / subps xmm2/m128, xmm1
              * subsd xmm2/m64, xmm1 / subss xmm2/m32, xmm1 */
  case 0x2e: /* ucomisd xmm2/m64, xmm1 / ucomiss xmm2/m64, xmm1 */
  case 0x15: /* unpckhpd xmm2/m128, xmm1 / unpckhps xmm2/m128, xmm1 */
  case 0x14: /* unpcklpd xmm2/m128, xmm1 / unpcklps xmm2/m128, xmm1 */
#if DCC_TARGET_IA32(486)
  case 0xc0: /* xadd %r8, r/m8 */
  case 0xc1: /* xadd %r16, r/m16 / xadd %r32, r/m32 */
#endif
  case 0x57: /* xorpd xmm2/m128, xmm1 / xorps xmm2/m128, xmm1 */
   goto modrm;

   /* === 2-byte opcodes with 8-bit & modr/m operands. */
  case 0xc5: /* pextrw $imm8, mm, r32 / pextrw $imm8, xmm, r32 */
  case 0xc4: /* pinsrw $imm8, r/m32, mm / pinsrw $imm8, r/m32, xmm */
  case 0x70: /* pshufw $imm8, mm2/m64, mm1 / pshufd $imm8, xmm2/m128, xmm1
              * pshufhw $imm8, xmm2/m128, xmm1 / pshuflw $imm8, xmm2/m128, xmm1 */
  case 0x71: /* psllw $imm8, mm / psllw $imm8, xmm
              * psraw $imm8, mm1 / psraw $imm8, xmm1
              * psrlw $imm8, mm / psrlw $imm8, xmm */
  case 0x72: /* pslld $imm8, mm1 / pslld $imm8, xmm1
              * psrad $imm8, mm1 / psrad $imm8, xmm1
              * psrld $imm8, mm1 / psrld $imm8, xmm1 */
  case 0x73: /* psllq $imm8, mm1 / psllq $imm8, xmm1
              * pslldq $imm8, xmm1 / psrldq $imm8, xmm1
              * psrlq $imm8, mm1 / psrlq $imm8, xmm1 */
  case 0xa4: /* shld $imm8, %r16, r/m16 / shld $imm8, %r32, r/m32 */
  case 0xac: /* shrd $imm8, %r16, r/m16 / shrd $imm8, %r32, r/m32 */
  case 0xc6: /* shufpd $imm8, xmm2/m128, xmm1 / shufps $imm8, xmm2/m128, xmm1 */
   ++suffix;
   goto modrm;

   /* === 2-byte opcodes with 16/32-bit immediate operand. */
  case 0x87: /* ja rel32 */
  case 0x83: /* jae rel32 */
  case 0x82: /* jb rel32 */
  case 0x86: /* jbe rel32 */
  case 0x84: /* je rel32 */
  case 0x8f: /* jg rel32 */
  case 0x8d: /* jge rel32 */
  case 0x8c: /* jl rel32 */
  case 0x8e: /* jle rel32 */
  case 0x85: /* jne rel32 */
  case 0x81: /* jno rel32 */
  case 0x8b: /* jnp rel32 */
  case 0x89: /* jns rel32 */
  case 0x80: /* jo rel32 */
  case 0x8a: /* jp rel32 */
  case 0x88: /* js rel32 */
   goto done_2or4;

   /* === 2-byte opcodes with 8-bit immediate & modr/m operand. */
  case 0xba: /* bt $imm8, r/m16 / bt $imm8, r/m32
              * btc $imm8, r/m16 / btc $imm8, r/m32
              * bts $imm8, r/m16 / bts $imm8, r/m32
              * btr $imm8, r/m16 / btr $imm8, r/m32 */
  case 0xc2: /* cmppd $imm8, xmm2/m128, xmm1 */
   ++suffix;
   goto modrm;

   /* === 2-byte opcodes. */
  case 0xc8: case 0xc9: case 0xca: case 0xcb: /* bswap r32 */
  case 0xcc: case 0xcd: case 0xce: case 0xcf: /* ... */
  case 0x06: /* clts */
  case 0xa2: /* cpuid */
  case 0x77: /* emms */
  case 0x08: /* invd */
  case 0xa1: /* pop %fs */
  case 0xa9: /* pop %gs */
  case 0xa0: /* push %fs */
  case 0xa8: /* push %gs */
  case 0x32: /* rdmsr */
  case 0x33: /* rdpmc */
  case 0x31: /* rdtsc */
  case 0xaa: /* rsm */
  case 0x34: /* sysenter */
  case 0x35: /* sysexit */
  case 0x0b: /* ud2 */
  case 0x09: /* wbinvd */
  case 0x30: /* wrmsr */
   goto done;

  default: goto unknown;
  }
  break;

 default:unknown:
#if INSTRLEN_DEBUG
  printf("ADDR(%8x): Unknown opcode %2x\n",ADDR(start),opcode);
#endif /* INSTRLEN_DEBUG */
  break;
 }
done:
 p += suffix;
 return p;
modrm:
 opcode = *p++;
modrm_fetched:
 if (opcode >= 0xC0) goto done; /* Register operand. */
 /* memory access */
 if (MODRM_GETRM(opcode) == MODRM_SIBREGISTER) {
  /* instruction with SIB byte */
  uint8_t sib = *p++;
  if ((sib & 0x7) == 0x05) {
   if ((opcode & 0xC0) == 0x40) goto done_p1;
   goto done_p4;
  }
 }
 switch (opcode & 0xC0) {
 case 0x0: /* 0/4 byte displacement */
  if ((opcode & 0x07) == 0x05) goto done_p4;
  goto done; /* 0-length offset */
 case 0x80: goto done_p4; /* 4 byte offset */
 default: goto done_p1;   /* one byte offset */
 }
}

DCC_DECL_END
#endif /* DCC_CONFIG_NEED_X86_INSTRLEN */

#endif /* !GUARD_DCC_X86_UTIL_INSTRLEN_C */
