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

#ifdef __INTELLISENSE__
#define F_COM     0x0 /* Common opcode (no operand data). */
#define F_PFX16   0x1 /* Prefix opcode ('0x66'). */
#define F_PFX     0x2 /* Prefix opcode. */
#define F_1B      0x3 /* 1 byte immediate. */
#define F_24B     0x4 /* 2/4 byte immediate (based on F_PFX16 prefix). */
#define F_46B     0x5 /* 4/6 byte immediate (based on F_PFX16 prefix). */
#define F_RM_1B   0x6 /* modr/m operand + 1 byte immediate. */
#define F_RM_24B  0x7 /* modr/m operand + 2/4 byte immediate (based on F_PFX16 prefix). */
#define F_RM      0x8 /* modr/m operand. */
#define F_2B      0x9 /* 2 byte immediate. */
#define F_3B      0xa /* 3 byte immediate. */
#define F_SPEC1   0xb /* Special opcode #1 (Special arithmetic opcodes). */
#define F_SPEC2   0xc /* Special opcode #2 (Prefix byte: '0x0f'). */
#if INSTRLEN_DEBUG
#define F_COM     0xf /* Common opcode (no operand data). */
#else
#define F_NK     0x0 /* Unknown opcode (Syntactically the same as 'F_COMMON'). */
#endif

#define BEGIN(mode)
#define END
#define O(...)      case (__VA_ARGS__):
typedef unsigned char opflag_t;
#define BEGIN_GROUP(name) \
static opflag_t const name[256]; \
void __intellisense_group_##name(int x) {switch (x) {
#define END_GROUP(name)                ;}}
#endif

/* Undocumented opcode. */
#define NDC(x) x

BEGIN_GROUP(x86_asm_common)

BEGIN(F_PFX)
   O(0x2e) /* CS prefix / Branch not taken */
   O(0x36) /* SS prefix */
   O(0x3e) /* DS prefix / Branch taken */
   O(0x26) /* ES prefix */
   O(0x64) /* FS prefix */
   O(0x65) /* GS prefix */
   O(0x67) /* Address-size prefix. */
   O(0xf0) /* lock */
   O(0xf2) /* repne */
   O(0xf3)
   O(0x9b) /* fwait */
END

BEGIN(F_PFX16)
   O(0x66)
END

/* === Simple 1-byte opcodes. */
BEGIN(F_COM)
    O(0x37) /* aaa */
    O(0x3f) /* aas */
    O(0x98) /* cbw / cwde */
    O(0xf8) /* clc */
    O(0xfc) /* cld */
    O(0xfa) /* cli */
    O(0xf5) /* cmc */
    O(0xa6) /* cmpsb */
    O(0xa7) /* cmpsw / cmpsl */
    O(0x99) /* cwd / cdq */
    O(0x27) /* daa */
    O(0x2f) /* das */
    O(0x48,0x49,0x4a,0x4b) /* dec */
    O(0x4c,0x4d,0x4e,0x4f) /* ... */
    O(0xf4) /* hlt */
    O(0xec) /* in %dx, %al */
    O(0xed) /* in %dx, %ax / in %dx, %eax */
    O(0x40,0x41,0x42,0x43) /* inc */
    O(0x44,0x45,0x46,0x47) /* ... */
    O(0x6c) /* insb */
    O(0x6d) /* insw / insl */
    O(0xcc) /* int $3 */
    O(0xce) /* into */
    O(0xcf) /* iret */
    O(0x9f) /* lahf */
    O(0xc9) /* leave */
    O(0xac) /* lodsb */
    O(0xad) /* lodsw / lodsl */
    O(0xa4) /* movsb */
    O(0xa5) /* movsw / movsl */
    O(0xee) /* out %dx, %al */
    O(0xef) /* out %dx, %ax / out %dx, %eax */
    O(0x6e) /* outsb */
    O(0x6f) /* outsw / outsl */
    O(0x58,0x59,0x5a,0x5b) /* pop %r16 / pop %r32 */
    O(0x5c,0x5d,0x5e,0x5f) /* *ditto* */
    O(0x1f) /* pop %ds */
    O(0x07) /* pop %es */
    O(0x17) /* pop %ss */
    O(0x61) /* popa / popad / popaw */
    O(0x9d) /* popf / popfd / popfw */
    O(0x50,0x51,0x52,0x53) /* push %r16 / push %r32 */
    O(0x54,0x55,0x56,0x57) /* *ditto* */
    O(0x1e) /* push %ds */
    O(0x06) /* push %es */
    O(0x0e) /* push %cs */
    O(0x16) /* push %ss */
    O(0x60) /* pusha / pushad / pushaw */
    O(0x9c) /* pushf / pushfd / pushfw */
    O(0xc3) /* ret */
    O(0xcb) /* lret */
    O(0x9e) /* sahf */
    O(0xae) /* scasb */
    O(0xaf) /* scasw / scasl */
    O(0xf9) /* stc */
    O(0xfd) /* std */
    O(0xfb) /* sti */
    O(0xaa) /* stosb */
    O(0xab) /* stosw / stosl */
    O(0x90) /* nop */
    O(     0x91,0x92,0x93) /* xchg %r16, %ax / xchg %r32, %ax */
    O(0x94,0x95,0x96,0x97) /* *ditto* */
    O(0xd7) /* xlat */
NDC(O(0xd6) /* salc */)
NDC(O(0xf1) /* icebp */)
END

/* === 1-byte opcodes with 1-byte immediate argument. */
BEGIN(F_1B)
    O(0xd5) /* aad [$imm8] */
    O(0xd4) /* aam [$imm8] */
    O(0x14) /* adc $imm8, %al */
    O(0x04) /* add $imm8, %al */
    O(0x24) /* and $imm8, %al */
    O(0x3c) /* cmp $imm8, %al */
    O(0xe4) /* in $imm8, %al */
    O(0xe5) /* in $imm8, %ax / in $imm8, %eax */
    O(0xcd) /* int $imm8 */
    O(0xe3) /* jcxz rel8 / jecxz rel8 */
    O(0x77) /* ja rel8 */
    O(0x73) /* jae rel8 */
    O(0x72) /* jb rel8 */
    O(0x76) /* jbe rel8 */
    O(0x74) /* je rel8 */
    O(0x7f) /* jg rel8 */
    O(0x7d) /* jge rel8 */
    O(0x7c) /* jl rel8 */
    O(0x7e) /* jle rel8 */
    O(0x75) /* jne rel8 */
    O(0x71) /* jno rel8 */
    O(0x7b) /* jnp rel8 */
    O(0x79) /* jns rel8 */
    O(0x70) /* jo rel8 */
    O(0x7a) /* jp rel8 */
    O(0x78) /* js rel8 */
    O(0xeb) /* jmp rel8 */
    O(0xe0) /* loopne/loopnz rel8 */
    O(0xe1) /* loope/loopz rel8 */
    O(0xe2) /* loop rel8 */
    O(0xa0) /* mov rel8, %al */
    O(0xa2) /* mov %al, rel8 */
    O(0xb0,0xb1,0xb2,0xb3) /* mov $imm8, r8 */
    O(0xb4,0xb5,0xb6,0xb7) /* *ditto* */
    O(0x0c) /* or $imm8, %al */
    O(0xe6) /* out $imm8, %al */
    O(0xe7) /* out $imm8, %ax / out $imm8, %eax */
    O(0x6a) /* push $imm8 */
    O(0xca) /* lret $imm16 */
    O(0x1c) /* sbb $imm8, %al */
    O(0x2c) /* sub $imm8, %al */
    O(0xa8) /* test $imm8, %al */
    O(0x34) /* xor $imm8, %al */
END

/* === 1-byte opcodes with 2-byte immediate argument. */
BEGIN(F_2B)
    O(0xc2) /* ret $imm16 */
END

/* === 1-byte opcodes with 2/4-byte immediate argument. */
BEGIN(F_24B)
    O(0x15) /* adc $imm16, %ax / adc $imm32, %eax */
    O(0x05) /* add $imm16, %ax / adc $imm32, %eax */
    O(0x25) /* and $imm16, %ax / adc $imm32, %eax */
    O(0xe8) /* call rel16 / call rel32 */
    O(0x3d) /* cmp $imm16, %ax / cmd $imm32, %eax */
    O(0xe9) /* jmp rel16 / jmp rel32 */
    O(0xa1) /* mov rel16, %ax / mov rel32, %eax */
    O(0xa3) /* mov %ax, rel16 / mov %eax, rel32 */
    O(0xb8,0xb9,0xba,0xbb) /* mov imm16, r16 / mov imm32, r32 */
    O(0xbc,0xbd,0xbe,0xbf) /* *ditto* */
    O(0x0d) /* or $imm16, %ax / or $imm32, %eax */
    O(0x68) /* push $imm16 / push $imm32 */
    O(0x1d) /* sbb $imm16, %ax / sbb $imm32, %eax */
    O(0x2d) /* sub $imm16, %ax / sub $imm32, %eax */
    O(0xa9) /* test $imm16, %ax / test $imm32, %ax */
    O(0x35) /* xor $imm16, %ax / xor $imm32, %eax */
END

/* === 1-byte opcodes with 1-byte immediate value & modr/m operand. */
BEGIN(F_RM_1B)
    O(0x80) /* adc $imm8, r/m8 */
    O(0x82) /* http://lists.llvm.org/pipermail/llvm-commits/Week-of-Mon-20141229/250249.html */
    O(0x83) /* adc $imm8s, r/m16 / adc $imm8s, r/m32 */
    O(0x6b) /* imul $imm8, r/m16, %r16 / imul $imm8, r/m32, %r32 */
    O(0xc6) /* mov $imm8, r/m8 */
    O(0xc0) /* rcl $imm8, r/m8 / rcr $imm8, r/m8
             * rol $imm8, r/m8 / ror $imm8, r/m8
             * sal $imm8, r/m8 / sar $imm8, r/m8
             * shl $imm8, r/m8 / shr $imm8, r/m8 */
    O(0xc1) /* rcl $imm8, r/m16 / rcl $imm8, r/m32 / rcr $imm8, r/m16 / rcr $imm8, r/m32
             * rol $imm8, r/m16 / rol $imm8, r/m32 / ror $imm8, r/m16 / ror $imm8, r/m32
             * sal $imm8, r/m16 / sal $imm8, r/m32 / sar $imm8, r/m16 / sar $imm8, r/m32
             * shl $imm8, r/m16 / shl $imm8, r/m32 / shr $imm8, r/m16 / shr $imm8, r/m32 */
END

/* === 1-byte opcodes with 2/4-byte immediate value & modr/m operand. */
BEGIN(F_RM_24B)
    O(0x81) /* adc $imm16, r/m16 / adc $imm32, r/m32 */
    O(0x69) /* imul $imm16, r/m16, %r16 / imul $imm32, r/m32, %r32 */
    O(0xc7) /* mov imm16, r/m16 / mov imm32, r/m32 */
END

/* === 1-byte opcodes with modr/m operand. */
BEGIN(F_RM)
    O(0x10) /* adc r8, r/m8 */
    O(0x11) /* adc r16, r/m16 / adc r32, r/m32 */
    O(0x12) /* adc r/m8, r8 */
    O(0x13) /* adc r/m16, r16 / adc r/m32, r32 */
    O(0x00) /* add r8, r/m8 */
    O(0x01) /* add r16, r/m16 / add r32, r/m32 */
    O(0x02) /* add r/m8, r8 */
    O(0x03) /* add r/m16, r16 / add r/m32, r32 */
    O(0x20) /* and r8, r/m8 */
    O(0x21) /* and r16, r/m16 / and r32, r/m32 */
    O(0x22) /* and r/m8, r8 */
    O(0x23) /* and r/m16, r16 / and r/m32, r32 */
    O(0x63) /* arpl r16, r/m16 */
    O(0x62) /* bound m16, r16 / bound m32, r32 */
    O(0xff) /* call *r/m16 / call *r/m32 / dec r/m16 / dec r/m32 / inc r/m16 / inc r/m32
             * jmp *r/m16 / jmp *r/m32 / ljmp m16 / ljmp m32 */
    O(0x38) /* cmp r8, r/m8 */
    O(0x39) /* cmp r16, r/m16 / cmp r32, r/m32 */
    O(0x3a) /* cmp r/m8, r8 */
    O(0x3b) /* cmp r/m16, r16 / cmp r/m32, r32 */
    O(0xfe) /* dec r/m8 / inc r/m8 */
    /* Floating point (st(i)-operations) prefix bytes */
    O(0xd8,0xd9,0xda,0xdb)
    O(0xdc,0xdd,0xde,0xdf)
    O(0xc5) /* lds m16, r16 / lds m32, r32 */
    O(0xc4) /* les m16, r16 / les m32, r32 */
    O(0x8d) /* lea m, r16 / lea m, r32 */
    O(0x88) /* mov r8, r/m8 */
    O(0x89) /* mov r16, r/m16 / mov r32, r/m32 */
    O(0x8a) /* mov r/m8, r8 */
    O(0x8b) /* mov r/m16, r16 / mov r/m32, r32 */
    O(0x8c) /* mov Sreg, r/m16 / mov Sreg, r/m32 */
    O(0x8e) /* mov r/m16, Sreg / mov r/m32, Sreg */
    O(0x08) /* or r8, r/m8 */
    O(0x09) /* or r16, r/m16 / or r32, r/m32 */
    O(0x0a) /* or r/m8, r8 */
    O(0x0b) /* or r/m16, r16 / or r/m32, r32 */
    O(0x8f) /* pop r/m16 / pop r/m32 */
    O(0xd0) /* rcl r/m8 / rcr r/m8
             * rol r/m8 / ror r/m8
             * sal r/m8 / sar r/m8
             * shl r/m8 / shr r/m8 */
    O(0xd1) /* rcl r/m16 / rcl r/m32 / rcr r/m16 / rcr r/m32
             * rol r/m16 / rol r/m32 / ror r/m16 / ror r/m32
             * sal r/m16 / sal r/m32 / sar r/m16 / sar r/m32
             * shl r/m16 / shl r/m32 / shr r/m16 / shr r/m32 */
    O(0xd2) /* rcl %cl, r/m8 / rcr %cl, r/m8
             * rol %cl, r/m8 / ror %cl, r/m8
             * sal %cl, r/m8 / sar %cl, r/m8
             * shl %cl, r/m8 / shr %cl, r/m8 */
    O(0xd3) /* rcl %cl, r/m16 / rcl %cl, r/m32 / rcr %cl, r/m16 / rcr %cl, r/m32
             * rol %cl, r/m16 / rol %cl, r/m32 / ror %cl, r/m16 / ror %cl, r/m32
             * sal %cl, r/m16 / sal %cl, r/m32 / sar %cl, r/m16 / sar %cl, r/m32
             * shl %cl, r/m16 / shl %cl, r/m32 / shr %cl, r/m16 / shr %cl, r/m32 */
    O(0x18) /* sbb r8, r/m8 */
    O(0x19) /* sbb r16, r/m16 / sbb r32, r/m32 */
    O(0x1a) /* sbb r/m8, r8 */
    O(0x1b) /* sbb r/m16, r16 / sbb r/m32, r32 */
    O(0x28) /* sub r8, r/m8 */
    O(0x29) /* sub r16, r/m16 / sub r32, r/m32 */
    O(0x2a) /* sub r/m8, r8 */
    O(0x2b) /* sub r/m16, r16 / sub r/m32, r32 */
    O(0x84) /* test r8, r/m8 */
    O(0x85) /* test r16, r/m16 / test r32, r/m32 */
    O(0x86) /* xchg %r8, r/m8 / xchg r/m8, %r8 */
    O(0x87) /* xchg %r16, r/m16 / xchg r/m16, %r16
             * xchg %r32, r/m32 / xchg r/m32, %r32 */
    O(0x30) /* xor r8, r/m8 */
    O(0x31) /* xor r16, r/m16 / xor r32, r/m32 */
    O(0x32) /* xor r/m8, r8 */
    O(0x33) /* xor r/m16, r16 / xor r/m32, r32 */
END

/* Special arithmetic opcodes. */
BEGIN(F_SPEC1)
    O(0xf6) /* div r/m8 / idiv r/m8 / mul r/m8 / imul r/m8 / neg r/m8 / not r/m8 */
    O(0xf7) /* div r/m16 / div r/m32 / idiv r/m16 / idiv r/m32
             * mul r/m16 / mul r/m32 / imul r/m16 / imul r/m32
             * neg r/m16 / neg r/m32 / not r/m16 / not r/m32 */
END

/* === 1-byte opcodes with 3-byte operand. */
BEGIN(F_3B)
    O(0xc8) /* enter $imm16, $imm8 */
END

/* === 1-byte opcodes with 4/6-byte operand. */
BEGIN(F_46B)
    O(0x9a) /* lcall ptr16,16 / lcall ptr16,32 */
    O(0xea) /* ljmp ptr16,16 / ljmp ptr16,32 */
END

BEGIN(F_SPEC2)
    O(0x0f) /* Prefix byte: 0x0f */
END
END_GROUP(x86_asm_common)

/* === 2-byte opcodes with modr/m operand. */
BEGIN_GROUP(x86_asm_0f)

BEGIN(F_RM)
    O(0x58) /* addpd xmm2/m128, xmm1 */
    O(0xd0) /* addsubpd xmm2/m128, xmm1 */
    O(0x54) /* andpd xmm2/m128, xmm1 */
    O(0x55) /* andnpd xmm2/m128, xmm1 */
    O(0xbc) /* bsf r/m16, r16 */
    O(0xbd) /* bsr r/m16, r16 */
    O(0xa3) /* bt r16, r/m16 / bt r32, r/m32 */
    O(0xbb) /* btc r16, r/m16 / btc r32, r/m32 */
    O(0xb3) /* btr r16, r/m16 / btr r32, r/m32 */
    O(0xab) /* bts r16, r/m16 / bts r32, r/m32 */
    O(0xae) /* clflush m8 / fxrstor m512byte / fxsave m512byte
             * ldmxcsr m32 / lfence / mfence / sfence
             * stmxcsr m32 */
    O(0x47) /* cmova r/m16, r16 */
    O(0x43) /* cmovae r/m16, r16 */
    O(0x42) /* cmovb r/m16, r16 */
    O(0x46) /* cmovbe r/m16, r16 */
    O(0x44) /* cmove r/m16, r16 */
    O(0x4f) /* cmovg r/m16, r16 */
    O(0x4d) /* cmovge r/m16, r16 */
    O(0x4c) /* cmovl r/m16, r16 */
    O(0x4e) /* cmovle r/m16, r16 */
    O(0x45) /* cmovne r/m16, r16 */
    O(0x41) /* cmovno r/m16, r16 */
    O(0x4b) /* cmovnp r/m16, r16 */
    O(0x49) /* cmovns r/m16, r16 */
    O(0x40) /* cmovo r/m16, r16 */
    O(0x4a) /* cmovp r/m16, r16 */
    O(0x48) /* cmovs r/m16, r16 */
    O(0xb0) /* cmpxch r8, r/m8 */
    O(0xb1) /* cmpxch r16, r/m16 / cmpxch r32, r/m32 */
    O(0xc7) /* cmpxchg8b m64 */
    O(0x2f) /* comisd xmm2/m64, xmm1 */
    O(0xe6) /* cvtdq2pd xmm2/m64, xmm1 */
    O(0x5b) /* cvtdq2ps xmm2/m128, xmm1 */
    O(0x5a) /* cvtpd2ps xmm2/m128, xmm1 / cvtsd2ss xmm2/m64, xmm1 */
    O(0x2a) /* cvtpi2pd mm/m64, xmm */
    O(0x2d) /* cvtps2pi xmm/m64, mm */
    O(0x2c) /* cvttpd2pi xmm/m128, mm */
    O(0x5e) /* divpd xmm2/m128, xmm1 */
    O(0x7c) /* haddpd xmm2/m128, xmm1 / haddps xmm2/m128, xmm1 */
    O(0x7d) /* hsubpd xmm2/m128, xmm1 / hsubps xmm2/m128, xmm1 */
    O(0xaf) /* imul r/m16, %r16 / imul r/m32, %r32 */
    O(0x01) /* invlpg m / lgdt m48 / lidt m48 / lmsw r/m16
             * monitor / mwait / sgdt m48 / sidt m48
             * smsw r/m16 / smsw r32/m16 */
    O(0x02) /* lar r/m16, r16 / lar r/m32, r32 */
    O(0xf0) /* lddqu m128, xmm */
    O(0xb2) /* lss m16, r16 / lss m32, r32 */
    O(0xb4) /* lfs m16, r16 / lfs m32, r32 */
    O(0xb5) /* lgs m16, r16 / lgs m32, r32 */
    O(0x00) /* lldt r/m16 / ltr r/m16 / sldt r/m16 / str r/m16
             * verr r/m16 / verw r/m16 */
    O(0x03) /* lsl r/m16, %r16 / lsl r/m32, %r32 */
    O(0xf7) /* maskmovdqu xmm2, xmm1 / maskmovq mm2, mm1 */
    O(0x5f) /* maxpd xmm2/m128, xmm1 / maxps xmm2/m128, xmm1
             * maxpd xmm2/m64, xmm1 / maxps xmm2/m64, xmm1 */
    O(0x5d) /* minpd xmm2/m128, xmm1 / minps xmm2/m128, xmm1
             * minpd xmm2/m64, xmm1 / minps xmm2/m64, xmm1 */
    O(0x20) /* mov crx, r32 */
    O(0x21) /* mov drx, r32 */
    O(0x24) /* mov trx, r32 */
    O(0x22) /* mov r32, crx */
    O(0x23) /* mov r32, drx */
    O(0x26) /* mov r32, trx */
    O(0x28) /* movapd xmm1, xmm2/m128 / movaps xmm1, xmm2/m128 */
    O(0x29) /* movapd xmm2/m128, xmm1 / movaps xmm2/m128, xmm1 */
    O(0x6e) /* movd r/m32, mm / movd r/m32, xmm */
    O(0x7e) /* movd mm, r/m32  / movd xmm, r/m32
             * movq xmm/m64, xmm */
    O(0x12) /* movddup xmm2/m64, xmm1 / movhlps xmm2, xmm1
             * movlpd m64, xmm / movlps m64, xmm / movsldup xmm2/m128, xmm1 */
    O(0x13) /* movlpd xmm, m64 / movlps xmm, m64 */
    O(0x6f) /* movdqa xmm2/m128, xmm1 / movdqu xmm2/m128, xmm1
             * movq mm/m64, mm */
    O(0x7f) /* movdqa xmm1, xmm2/m128 / movdqu xmm1, xmm2/m128
             * movq mm, mm/m64 */
    O(0xd6) /* movdq2q xmm, mm / movq xmm, xmm/m64 / movq2dq mm, xmm */
    O(0x16) /* movhpd m64, xmm / movhps m64, xmm / movlhps xmm2, xmm1 / movshdup xmm2/m128, xmm1 */
    O(0x17) /* movhpd xmm, m64 / movhps xmm, m64 */
    O(0x50) /* movmskpd xmm, r32 / movmskps xmm, r32 */
    O(0xe7) /* movntdq xmm, m128 / movntq mm, m64 */
    O(0xc3) /* movnti r32, m32 */
    O(0x2b) /* movntpd xmm, m128 / movntps xmm, m128 */
    O(0x10) /* movsd xmm2/m64, xmm1 / movss xmm2/m32, xmm1
             * movapd xmm2/m128, xmm1 / movups xmm2/m128, xmm1 */
    O(0x11) /* movsd xmm1, xmm2/m64 / movss xmm1, xmm2/m32
             * movapd xmm1, xmm2/m128 / movups xmm1, xmm2/m128 */
    O(0xbe) /* movsx r/m8, r16 / movsx r/m8, r32 */
    O(0xbf) /* movsx r/m16, r32 */
    O(0xb6) /* movzx r/m8, r16 / movzx r/m8, r32 */
    O(0xb7) /* movzx r/m16, r32 */
    O(0x59) /* mulpd xmm2/m128, xmm1 / mulps xmm2/m128, xmm1
             * mulsd xmm2/m64, xmm1 / mulss xmm2/m64, xmm1 */
    O(0x56) /* orpd xmm2/m128, xmm1 / orps xmm2/m128, xmm1 */
    O(0x63) /* packsswb mm2/m64, mm1 / packsswb xmm2/m128, xmm1 */
    O(0x6b) /* packssdw mm2/m64, mm1 / packssdw xmm2/m128, xmm1 */
    O(0x67) /* packuswb mm2/m64, mm1 / packuswb xmm2/m128, xmm1 */
    O(0xfc) /* paddb mm2/m64, mm1 / paddb xmm2/m128, xmm1 */
    O(0xfd) /* paddw mm2/m64, mm1 / paddw xmm2/m128, xmm1 */
    O(0xfe) /* paddl mm2/m64, mm1 / paddl xmm2/m128, xmm1 */
    O(0xd4) /* paddq mm2/m64, mm1 / paddq xmm2/m128, xmm1 */
    O(0xec) /* paddsb mm2/m64, mm1 / paddsb xmm2/m128, xmm1 */
    O(0xed) /* paddsw mm2/m64, mm1 / paddsw xmm2/m128, xmm1 */
    O(0xdc) /* paddusb mm2/m64, mm1 / paddusb xmm2/m128, xmm1 */
    O(0xdd) /* paddusw mm2/m64, mm1 / paddusw xmm2/m128, xmm1 */
    O(0xdb) /* pand mm2/m64, mm1 / pand xmm2/m128, xmm1 */
    O(0xdf) /* pandn mm2/m64, mm1 / pandn xmm2/m128, xmm1 */
    O(0x90) /* pause */
    O(0xe0) /* pavgbb mm2/m64, mm1 / pavgbb xmm2/m128, xmm1 */
    O(0xe3) /* pavgbw mm2/m64, mm1 / pavgbw xmm2/m128, xmm1 */
    O(0x74) /* pcmpeqb mm2/m64, mm1 / pcmpeqb xmm2/m128, xmm1 */
    O(0x75) /* pcmpeqw mm2/m64, mm1 / pcmpeqw xmm2/m128, xmm1 */
    O(0x76) /* pcmpeql mm2/m64, mm1 / pcmpeql xmm2/m128, xmm1 */
    O(0x64) /* pcmpgtb mm2/m64, mm1 / pcmpgtb xmm2/m128, xmm1 */
    O(0x65) /* pcmpgtw mm2/m64, mm1 / pcmpgtw xmm2/m128, xmm1 */
    O(0x66) /* pcmpgtl mm2/m64, mm1 / pcmpgtl xmm2/m128, xmm1 */
    O(0xf5) /* pmaddwd mm2/m64, mm1 / pmaddwd xmm2/m128, xmm1 */
    O(0xee) /* pmaxsw mm2/m64, mm1 / pmaxsw xmm2/m128, xmm1 */
    O(0xde) /* pmaxub mm2/m64, mm1 / pmaxub xmm2/m128, xmm1 */
    O(0xea) /* pminsw mm2/m64, mm1 / pminsw xmm2/m128, xmm1 */
    O(0xda) /* pminub mm2/m64, mm1 / pminub xmm2/m128, xmm1 */
    O(0xd7) /* pmovmskb r32, mm1 / pmovmskb r32, xmm1 */
    O(0xe4) /* pmulhuw mm2/m64, mm1 / pmulhuw xmm2/m128, xmm1 */
    O(0xe5) /* pmulhw mm2/m64, mm1 / pmulhw xmm2/m128, xmm1 */
    O(0xd5) /* pmullw mm2/m64, mm1 / pmullw xmm2/m128, xmm1 */
    O(0xf4) /* pmuludq mm2/m64, mm1 / pmuludq xmm2/m128, xmm1 */
    O(0xeb) /* por mm2/m64, mm1 / por xmm2/m128, xmm1 */
    O(0x18) /* prefetcht(0|1|2) m8 / prefetchnta m8 */
    O(0xf6) /* psadbw mm2/m64, mm1 / psadbw xmm2/m128, xmm1 */
    O(0xf1) /* psllw mm2/m64, mm1 / psllw xmm2/m128, xmm1 */
    O(0xf2) /* pslld mm2/m64, mm1 / pslld xmm2/m128, xmm1 */
    O(0xf3) /* psllq mm2/m64, mm1 / psllq xmm2/m128, xmm1 */
    O(0xe1) /* psraw mm2/m64, mm1 / psraw xmm2/m128, xmm1 */
    O(0xe2) /* psrad mm2/m64, mm1 / psrad xmm2/m128, xmm1 */
    O(0xd1) /* psrlw mm2/m64, mm1 / psrlw xmm2/m128, xmm1 */
    O(0xd2) /* psrld mm2/m64, mm1 / psrld xmm2/m128, xmm1 */
    O(0xd3) /* psrlq mm2/m64, mm1 / psrlq xmm2/m128, xmm1 */
    O(0xf8) /* psubb mm2/m64, mm1 / psubb xmm2/m128, xmm1 */
    O(0xf9) /* psubw mm2/m64, mm1 / psubw xmm2/m128, xmm1  */
    O(0xfa) /* psubd mm2/m64, mm1 / psubd xmm2/m128, xmm1  */
    O(0xfb) /* psubq mm2/m64, mm1 / psubq xmm2/m128, xmm1  */
    O(0xe8) /* psubsb mm2/m64, mm1 / psubsb xmm2/m128, xmm1 */
    O(0xe9) /* psubsw mm2/m64, mm1 / psubsw xmm2/m128, xmm1 */
    O(0xd8) /* psubusb mm2/m64, mm1 / psubusb xmm2/m128, xmm1 */
    O(0xd9) /* psubusw mm2/m64, mm1 / psubusw xmm2/m128, xmm1 */
    O(0x68) /* punpckhbw mm2/m64, mm1 / punpckhbw xmm2/m128, xmm1 */
    O(0x69) /* punpckhbd mm2/m64, mm1 / punpckhbd xmm2/m128, xmm1 */
    O(0x6a) /* punpckhbq mm2/m64, mm1 / punpckhbq xmm2/m128, xmm1 */
    O(0x6d) /* punpckhqdq xmm2/m128, xmm1 */
    O(0x60) /* punpcklbw mm2/m64, mm1 / punpcklbw xmm2/m128, xmm1 */
    O(0x61) /* punpcklbd mm2/m64, mm1 / punpcklbd xmm2/m128, xmm1 */
    O(0x62) /* punpcklbq mm2/m64, mm1 / punpcklbq xmm2/m128, xmm1 */
    O(0x6c) /* punpcklqdq xmm2/m128, xmm1 */
    O(0xef) /* pxor mm2/m64, mm1 / pxor xmm2/m128, xmm1 */
    O(0x53) /* rcpps xmm2/m128, xmm1 / rcpss xmm2/m32, xmm1 */
    O(0x52) /* rsqrtps xmm2/m128, xmm1 / rsqrtss xmm2/m128, xmm1 */
    O(0x97) /* seta r/m8 */
    O(0x93) /* setae r/m8 */
    O(0x92) /* setb r/m8 */
    O(0x96) /* setbe r/m8 */
    O(0x94) /* sete r/m8 */
    O(0x9f) /* setg r/m8 */
    O(0x9d) /* setge r/m8 */
    O(0x9c) /* setl r/m8 */
    O(0x9e) /* setle r/m8 */
    O(0x95) /* setne r/m8 */
    O(0x91) /* setno r/m8 */
    O(0x9b) /* setnp r/m8 */
    O(0x99) /* setns r/m8 */
    O(0x9a) /* setp r/m8 */
    O(0x98) /* sets r/m8 */
    O(0xa5) /* shld %cl, %r16, r/m16 / shld %cl, %r32, r/m32 */
    O(0xad) /* shrd %cl, %r16, r/m16 / shrd %cl, %r32, r/m32 */
    O(0x51) /* sqrtpd xmm2/m128, xmm1 / sqrtps xmm2/m128, xmm1
             * sqrtsd xmm2/m64, xmm1 / sqrtss xmm2/m32, xmm1 */
    O(0x5c) /* subpd xmm2/m128, xmm1 / subps xmm2/m128, xmm1
             * subsd xmm2/m64, xmm1 / subss xmm2/m32, xmm1 */
    O(0x2e) /* ucomisd xmm2/m64, xmm1 / ucomiss xmm2/m64, xmm1 */
    O(0x15) /* unpckhpd xmm2/m128, xmm1 / unpckhps xmm2/m128, xmm1 */
    O(0x14) /* unpcklpd xmm2/m128, xmm1 / unpcklps xmm2/m128, xmm1 */
    O(0xc0) /* xadd %r8, r/m8 */
    O(0xc1) /* xadd %r16, r/m16 / xadd %r32, r/m32 */
    O(0x57) /* xorpd xmm2/m128, xmm1 / xorps xmm2/m128, xmm1 */
END

/* === 2-byte opcodes with 8-bit immediate & modr/m operands. */
BEGIN(F_RM_1B)
    O(0xc5) /* pextrw $imm8, mm, r32 / pextrw $imm8, xmm, r32 */
    O(0xc4) /* pinsrw $imm8, r/m32, mm / pinsrw $imm8, r/m32, xmm */
    O(0x70) /* pshufw $imm8, mm2/m64, mm1 / pshufd $imm8, xmm2/m128, xmm1
             * pshufhw $imm8, xmm2/m128, xmm1 / pshuflw $imm8, xmm2/m128, xmm1 */
    O(0x71) /* psllw $imm8, mm / psllw $imm8, xmm
             * psraw $imm8, mm1 / psraw $imm8, xmm1
             * psrlw $imm8, mm / psrlw $imm8, xmm */
    O(0x72) /* pslld $imm8, mm1 / pslld $imm8, xmm1
             * psrad $imm8, mm1 / psrad $imm8, xmm1
             * psrld $imm8, mm1 / psrld $imm8, xmm1 */
    O(0x73) /* psllq $imm8, mm1 / psllq $imm8, xmm1
             * pslldq $imm8, xmm1 / psrldq $imm8, xmm1
             * psrlq $imm8, mm1 / psrlq $imm8, xmm1 */
    O(0xa4) /* shld $imm8, %r16, r/m16 / shld $imm8, %r32, r/m32 */
    O(0xac) /* shrd $imm8, %r16, r/m16 / shrd $imm8, %r32, r/m32 */
    O(0xc6) /* shufpd $imm8, xmm2/m128, xmm1 / shufps $imm8, xmm2/m128, xmm1 */
    O(0xba) /* bt $imm8, r/m16 / bt $imm8, r/m32
             * btc $imm8, r/m16 / btc $imm8, r/m32
             * bts $imm8, r/m16 / bts $imm8, r/m32
             * btr $imm8, r/m16 / btr $imm8, r/m32 */
    O(0xc2) /* cmppd $imm8, xmm2/m128, xmm1 */
END

/* === 2-byte opcodes with 16/32-bit immediate operand. */
BEGIN(F_24B)
    O(0x87) /* ja rel32 */
    O(0x83) /* jae rel32 */
    O(0x82) /* jb rel32 */
    O(0x86) /* jbe rel32 */
    O(0x84) /* je rel32 */
    O(0x8f) /* jg rel32 */
    O(0x8d) /* jge rel32 */
    O(0x8c) /* jl rel32 */
    O(0x8e) /* jle rel32 */
    O(0x85) /* jne rel32 */
    O(0x81) /* jno rel32 */
    O(0x8b) /* jnp rel32 */
    O(0x89) /* jns rel32 */
    O(0x80) /* jo rel32 */
    O(0x8a) /* jp rel32 */
    O(0x88) /* js rel32 */
END

/* === 2-byte opcodes. */
BEGIN(F_COM)
    O(0xc8,0xc9,0xca,0xcb) /* bswap r32 */
    O(0xcc,0xcd,0xce,0xcf) /* ... */
    O(0x06) /* clts */
    O(0xa2) /* cpuid */
    O(0x77) /* emms */
    O(0x08) /* invd */
    O(0xa1) /* pop %fs */
    O(0xa9) /* pop %gs */
    O(0xa0) /* push %fs */
    O(0xa8) /* push %gs */
    O(0x32) /* rdmsr */
    O(0x33) /* rdpmc */
    O(0x31) /* rdtsc */
    O(0xaa) /* rsm */
    O(0x34) /* sysenter */
    O(0x35) /* sysexit */
    O(0x0b) /* ud2 */
    O(0x09) /* wbinvd */
    O(0x30) /* wrmsr */
NDC(O(0x04) /* Unknown mnemonic */)
NDC(O(0x05) /* loadall */)
NDC(O(0x07) /* loadalld */)
NDC(O(0xb9) /* ud1 */)
END

END_GROUP(x86_asm_0f)

#undef END_GROUP
#undef BEGIN_GROUP
#undef O
#undef END
#undef BEGIN
