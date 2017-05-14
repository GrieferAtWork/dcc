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

#define DCC_OPCODE_FIRST   KWD_aaa

/* >> OVERLOAD_[n](opcode,opsize,group,flags,[n*(args)])
 * OPCODE: In big endian: 1-4 successive operator bytes.
 * OPSIZE: 0, 8, 16, 32, 64, 128: Preferred size suffix (used to resolve ambiguity) Set to '0' if no other is apropriate
 * GROUP:  'r', 0..7: Argument for mod/rm opcodes (set to 'r' for '/r' opcodes; aka when using a register index)
 * flags:  A set of 'DCC_ASMOPC_*'
 * args:   Repeated 'n' times: overload constraints for the argument (uses AT&T operand order)
 */

DEF_OPCODE(aaa,{ /* aaa */OVERLOAD_0(0x37,8,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(aad,{
    /* aad       */OVERLOAD_0(0xd50a,8,0,0)
    /* aad $imm8 */OVERLOAD_1(0xd5,  8,0,0,DCC_ASMOPT_IMM_8) // EXTENSION
    OVERLOAD_SENTINAL
})
DEF_OPCODE(aas,{ /* aas */OVERLOAD_0(0x3f,8,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(adc,{
    /* adc $imm8,  %al   */OVERLOAD_2(0x14,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* adc $imm16, %ax   */OVERLOAD_2(0x15,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* adc $imm32, %eax  */OVERLOAD_2(0x15,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* adc $imm8,  r/m8  */OVERLOAD_2(0x80,8, 2,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* adc $imm16, r/m16 */OVERLOAD_2(0x81,16,2,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* adc $imm32, r/m32 */OVERLOAD_2(0x81,32,2,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* adc $imm8s, r/m16 */OVERLOAD_2(0x83,16,2,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* adc $imm8s, r/m32 */OVERLOAD_2(0x83,32,2,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* adc r8,     r/m8  */OVERLOAD_2(0x10,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* adc r16,    r/m16 */OVERLOAD_2(0x11,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* adc r32,    r/m32 */OVERLOAD_2(0x11,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* adc r/m8,   r8    */OVERLOAD_2(0x12,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* adc r/m16,  r16   */OVERLOAD_2(0x13,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* adc r/m32,  r32   */OVERLOAD_2(0x13,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})


DEF_OPCODE(add,{
    /* add $imm8,  %al   */OVERLOAD_2(0x04,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* add $imm16, %ax   */OVERLOAD_2(0x05,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* add $imm32, %eax  */OVERLOAD_2(0x05,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* add $imm8,  r/m8  */OVERLOAD_2(0x80,8, 0,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* add $imm16, r/m16 */OVERLOAD_2(0x81,16,0,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* add $imm32, r/m32 */OVERLOAD_2(0x81,32,0,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* add $imm8s, r/m16 */OVERLOAD_2(0x83,8, 0,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* add $imm8s, r/m32 */OVERLOAD_2(0x83,8, 0,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* add r8,     r/m8  */OVERLOAD_2(0x00,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* add r16,    r/m16 */OVERLOAD_2(0x01,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* add r32,    r/m32 */OVERLOAD_2(0x01,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* add r/m8,   r8    */OVERLOAD_2(0x02,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* add r/m16,  r16   */OVERLOAD_2(0x03,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* add r/m32,  r32   */OVERLOAD_2(0x03,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(addpd,{/* addpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f58,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(addps,{/* addps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f58,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(addsd,{/* addsd xmm2/m64,  xmm1 */OVERLOAD_2(0xf20f58,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(addss,{/* addss xmm2/m32,  xmm1 */OVERLOAD_2(0xf30f58,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(addsubpd,{/* addsubpd xmm2/m128, xmm1 */OVERLOAD_2(0x660fd0,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(addsubps,{/* addsubps xmm2/m128, xmm1 */OVERLOAD_2(0xf20fd0,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(and,{
    /* and $imm8,  %al   */OVERLOAD_2(0x24,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* and $imm16, %ax   */OVERLOAD_2(0x25,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* and $imm32, %eax  */OVERLOAD_2(0x25,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* and $imm8,  r/m8  */OVERLOAD_2(0x80,8, 4,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* and $imm16, r/m16 */OVERLOAD_2(0x81,16,4,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* and $imm32, r/m32 */OVERLOAD_2(0x81,32,4,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* and $imm8s, r/m16 */OVERLOAD_2(0x83,16,4,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* and $imm8s, r/m32 */OVERLOAD_2(0x83,32,4,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* and r8,     r/m8  */OVERLOAD_2(0x20,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* and r16,    r/m16 */OVERLOAD_2(0x21,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* and r32,    r/m32 */OVERLOAD_2(0x21,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* and r/m8,   r8    */OVERLOAD_2(0x22,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* and r/m16,  r16   */OVERLOAD_2(0x23,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* and r/m32,  r32   */OVERLOAD_2(0x23,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(andpd,{/* andpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f54,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(andps,{/* andps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f54,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(andnpd,{/* andnpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f55,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(andnps,{/* andnps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f55,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(arpl,{/* arpl r16, r/m16 */OVERLOAD_2(0x63,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(bound,{
    /* bound m16, r16 */OVERLOAD_2(0x62,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* bound m32, r32 */OVERLOAD_2(0x62,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(bsf,{
    /* bsf r/m16, r16 */OVERLOAD_2(0x0fbc,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* bsf r/m32, r32 */OVERLOAD_2(0x0fbc,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(bsr,{
    /* bsr r/m16, r16 */OVERLOAD_2(0x0fbd,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* bsr r/m32, r32 */OVERLOAD_2(0x0fbd,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(bswap,{
    /* HINT: You can do a 16-bit bswap using 'xch %al, %ah' (at least for %ax, %cx, %dx and %bx) */
    /* bswap r32 */OVERLOAD_1(0x0fc8,32,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(bt,{
    /* bt r16,   r/m16 */OVERLOAD_2(0x0fa3,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* bt r32,   r/m32 */OVERLOAD_2(0x0fa3,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* bt $imm8, r/m16 */OVERLOAD_2(0x0fba,16,4,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* bt $imm8, r/m32 */OVERLOAD_2(0x0fba,32,4,  DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(btc,{
    /* btc r16,   r/m16 */OVERLOAD_2(0x0fbb,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* btc r32,   r/m32 */OVERLOAD_2(0x0fbb,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* btc $imm8, r/m16 */OVERLOAD_2(0x0fba,16,7,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* btc $imm8, r/m32 */OVERLOAD_2(0x0fba,32,7,  DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(btr,{
    /* btr r16,   r/m16 */OVERLOAD_2(0x0fb3,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* btr r32,   r/m32 */OVERLOAD_2(0x0fb3,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* btr $imm8, r/m16 */OVERLOAD_2(0x0fba,16,6,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* btr $imm8, r/m32 */OVERLOAD_2(0x0fba,32,6,  DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(call,{
    /* For some reason, 16-bit disp-call will truncate the resulting address to the lower 16 bits.
     * Saying that, you might think that makes sense, but let me tell you why it doesn't, and why
     * the rel16 call instruction cannot be used for that reason:
     * 1. The equivalent jmp instruction does _not_ do this
     *    It does EIP+rel16, in 32-bit mode, while call does IP+rel16
     * 2. This makes the opcode completely useless, as it would only ever
     *    work when the runtime linker decided to put the code into the
     *    lower 64Kib of memory (which it would _never_ choose to do)
     * EDIT: OK! I get it now! It's because of the return address that's saved on the stack!
     *    >> Truncating the return address early allows the processor to use a 16-bit bus
     *       for transportation of the original address to both a full-adder, adding the
     *       16-bit offset provided as immediate argument, as well as the stack on which
     *       only the 16 lower bits of the caller's EIP are stored.
     *       With that in mind, the processor lazily enforces a caller origin with
     *       the first 64Kib (1 << 16) bytes of memory by ignoring all higher bits.
     *       The reason jmp behaves differently quickly simply lies in the fact
     *       that it doesn't need to push a return address.
     * WARNING: call *r/m16 probably behaves the same way, but because of the fact
     *          that the size must be specified explicitly due to the ambiguity with
     *          call *r/m32, the user should already be aware of the special behaviour.
     */
    //* call rel16  */OVERLOAD_1(0xe8,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
    /* call rel32  */OVERLOAD_1(0xe8,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
    /* call *r/m16 */OVERLOAD_1(0xff,16,2,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA|DCC_ASMOPT_IND)
    /* call *r/m32 */OVERLOAD_1(0xff,32,2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA|DCC_ASMOPT_IND)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lcall,{
    /* lcall ptr16,16 */OVERLOAD_2(0x9a,16,0,DCC_ASMOPC_D16,                 DCC_ASMOPT_IMM_16,DCC_ASMOPT_IMM_16)
    /* lcall ptr16,32 */OVERLOAD_2(0x9a,32,0,0,                              DCC_ASMOPT_IMM_16,DCC_ASMOPT_IMM_32)
    /* lcall m16      */OVERLOAD_1(0xff,16,3,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA)
    /* lcall m32      */OVERLOAD_1(0xff,32,3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(cbw,{ /* cbw */OVERLOAD_0(0x98,16,0,DCC_ASMOPC_D16) OVERLOAD_SENTINAL })
DEF_OPCODE(cwde,{ /* cwde */OVERLOAD_0(0x98,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(clc,{ /* clc */OVERLOAD_0(0xf8,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(cld,{ /* cld */OVERLOAD_0(0xfc,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(clflush,{ /* clflush m8 */OVERLOAD_1(0x0fae,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(cli,{ /* cli */OVERLOAD_0(0xfa,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(clts,{ /* clts */OVERLOAD_0(0x0f06,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(cmc,{ /* cmc */OVERLOAD_0(0xf5,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(cmova,  { /* cmova r/m16, r16   */OVERLOAD_2(0x0f47,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if above (CF=0 and ZF=0). */
                     /* cmova r/m32, r32   */OVERLOAD_2(0x0f47,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovae, { /* cmovae r/m16, r16  */OVERLOAD_2(0x0f43,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if above or equal (CF=0). */
                     /* cmovae r/m32, r32  */OVERLOAD_2(0x0f43,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovb,  { /* cmovb r/m16, r16   */OVERLOAD_2(0x0f42,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if below (CF=1). */
                     /* cmovb r/m32, r32   */OVERLOAD_2(0x0f42,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL }) 
DEF_OPCODE(cmovbe, { /* cmovbe r/m16, r16  */OVERLOAD_2(0x0f46,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if below or equal (CF=1 or ZF=1). */
                     /* cmovbe r/m32, r32  */OVERLOAD_2(0x0f46,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovc,  { /* cmovc r/m16, r16   */OVERLOAD_2(0x0f42,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if carry (CF=1). */
                     /* cmovc r/m32, r32   */OVERLOAD_2(0x0f42,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmove,  { /* cmove r/m16, r16   */OVERLOAD_2(0x0f44,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if equal (ZF=1). */
                     /* cmove r/m32, r32   */OVERLOAD_2(0x0f44,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovg,  { /* cmovg r/m16, r16   */OVERLOAD_2(0x0f4f,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if greater (ZF=0 and SF=OF). */
                     /* cmovg r/m32, r32   */OVERLOAD_2(0x0f4f,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovge, { /* cmovge r/m16, r16  */OVERLOAD_2(0x0f4d,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if greater or equal (SF=OF). */
                     /* cmovge r/m32, r32  */OVERLOAD_2(0x0f4d,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovl,  { /* cmovl r/m16, r16   */OVERLOAD_2(0x0f4c,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if less (SF<>OF). */
                     /* cmovl r/m32, r32   */OVERLOAD_2(0x0f4c,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovle, { /* cmovle r/m16, r16  */OVERLOAD_2(0x0f4e,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if less or equal (ZF=1 or SF<>OF). */
                     /* cmovle r/m32, r32  */OVERLOAD_2(0x0f4e,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovna, { /* cmovna r/m16, r16  */OVERLOAD_2(0x0f46,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not above (CF=1 or ZF=1). */
                     /* cmovna r/m32, r32  */OVERLOAD_2(0x0f46,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnae,{ /* cmovnae r/m16, r16 */OVERLOAD_2(0x0f42,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not above or equal (CF=1). */
                     /* cmovnae r/m32, r32 */OVERLOAD_2(0x0f42,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnb, { /* cmovnb r/m16, r16  */OVERLOAD_2(0x0f43,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not below (CF=0). */
                     /* cmovnb r/m32, r32  */OVERLOAD_2(0x0f43,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnbe,{ /* cmovnbe r/m16, r16 */OVERLOAD_2(0x0f47,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not below or equal (CF=0 and ZF=0). */
                     /* cmovnbe r/m32, r32 */OVERLOAD_2(0x0f47,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnc, { /* cmovnc r/m16, r16  */OVERLOAD_2(0x0f43,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not carry (CF=0). */
                     /* cmovnc r/m32, r32  */OVERLOAD_2(0x0f43,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovne, { /* cmovne r/m16, r16  */OVERLOAD_2(0x0f45,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not equal (ZF=0). */
                     /* cmovne r/m32, r32  */OVERLOAD_2(0x0f45,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovng, { /* cmovng r/m16, r16  */OVERLOAD_2(0x0f4e,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not greater (ZF=1 or SF<>OF). */
                     /* cmovng r/m32, r32  */OVERLOAD_2(0x0f4e,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnge,{ /* cmovnge r/m16, r16 */OVERLOAD_2(0x0f4c,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not greater or equal (SF<>OF). */
                     /* cmovnge r/m32, r32 */OVERLOAD_2(0x0f4c,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnl, { /* cmovnl r/m16, r16  */OVERLOAD_2(0x0f4d,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not less (SF=OF). */
                     /* cmovnl r/m32, r32  */OVERLOAD_2(0x0f4d,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnle,{ /* cmovnle r/m16, r16 */OVERLOAD_2(0x0f4f,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not less or equal (ZF=0 and SF=OF). */
                     /* cmovnle r/m32, r32 */OVERLOAD_2(0x0f4f,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovno, { /* cmovno r/m16, r16  */OVERLOAD_2(0x0f41,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not overflow (OF=0). */
                     /* cmovno r/m32, r32  */OVERLOAD_2(0x0f41,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnp, { /* cmovnp r/m16, r16  */OVERLOAD_2(0x0f4b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not parity (PF=0). */
                     /* cmovnp r/m32, r32  */OVERLOAD_2(0x0f4b,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovns, { /* cmovns r/m16, r16  */OVERLOAD_2(0x0f49,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not sign (SF=0). */
                     /* cmovns r/m32, r32  */OVERLOAD_2(0x0f49,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovnz, { /* cmovnz r/m16, r16  */OVERLOAD_2(0x0f45,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if not zero (ZF=0). */
                     /* cmovnz r/m32, r32  */OVERLOAD_2(0x0f45,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovo,  { /* cmovo r/m16, r16   */OVERLOAD_2(0x0f40,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if overflow (OF=1). */
                     /* cmovo r/m32, r32   */OVERLOAD_2(0x0f40,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovp,  { /* cmovp r/m16, r16   */OVERLOAD_2(0x0f4a,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if parity (PF=1). */
                     /* cmovp r/m32, r32   */OVERLOAD_2(0x0f4a,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovpe, { /* cmovpe r/m16, r16  */OVERLOAD_2(0x0f4a,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if parity even (PF=1). */
                     /* cmovpe r/m32, r32  */OVERLOAD_2(0x0f4a,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovpo, { /* cmovpo r/m16, r16  */OVERLOAD_2(0x0f4b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if parity odd (PF=0). */
                     /* cmovpo r/m32, r32  */OVERLOAD_2(0x0f4b,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovs,  { /* cmovs r/m16, r16   */OVERLOAD_2(0x0f48,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if sign (SF=1). */
                     /* cmovs r/m32, r32   */OVERLOAD_2(0x0f48,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cmovz,  { /* cmovz r/m16, r16   */OVERLOAD_2(0x0f44,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16) /* Move if zero (ZF=1). */
                     /* cmovz r/m32, r32   */OVERLOAD_2(0x0f44,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })

DEF_OPCODE(cmp,{
    /* cmp $imm8, %al    */OVERLOAD_2(0x3c,8, 0,0,             DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* cmp $imm16, %ax   */OVERLOAD_2(0x3d,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* cmp $imm32, %eax  */OVERLOAD_2(0x3d,32,0,0,             DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* cmp $imm8, r/m8   */OVERLOAD_2(0x80,8, 7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* cmp $imm16, r/m16 */OVERLOAD_2(0x81,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* cmp $imm32, r/m32 */OVERLOAD_2(0x81,32,7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* cmp $imm8, r/m16  */OVERLOAD_2(0x83,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* cmp $imm8, r/m32  */OVERLOAD_2(0x83,16,7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* cmp r8, r/m8      */OVERLOAD_2(0x38,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* cmp r16, r/m16    */OVERLOAD_2(0x39,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* cmp r32, r/m32    */OVERLOAD_2(0x39,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* cmp r/m8, r8      */OVERLOAD_2(0x3a,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_8)
    /* cmp r/m16, r16    */OVERLOAD_2(0x3b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* cmp r/m32, r32    */OVERLOAD_2(0x3b,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(cmppd,{/* cmppd imm8, xmm2/m128, xmm1 */OVERLOAD_3(0x660fc2,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cmpps,{/* cmpps imm8, xmm2/m128, xmm1 */OVERLOAD_3(  0x0fc2,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

DEF_OPCODE(cmps,{
    /* cmpsb */OVERLOAD_0(0xa6,8, 0,0)
    /* cmpsw */OVERLOAD_0(0xa7,16,0,DCC_ASMOPC_D16)
    /* cmpsl */OVERLOAD_0(0xa7,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(scmp,{
    /* scmpb */OVERLOAD_0(0xa6,8, 0,0)
    /* scmpw */OVERLOAD_0(0xa7,16,0,DCC_ASMOPC_D16)
    /* scmpl */OVERLOAD_0(0xa7,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(cmpsd,{ /* cmpsd imm8, xmm2/m64, xmm1 */OVERLOAD_3(0xf20fc2,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cmpss,{ /* cmpss imm8, xmm2/m64, xmm1 */OVERLOAD_3(0xf30fc2,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cmpxch,{
    /* cmpxch r8, r/m8   */OVERLOAD_2(0x0fb0,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* cmpxch r16, r/m16 */OVERLOAD_2(0x0fb1,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* cmpxch r32, r/m32 */OVERLOAD_2(0x0fb1,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(cmpxchg8b,{ /* cmpxchg8b m64 */OVERLOAD_1(0x0fc7,64,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(comisd,{/* comisd xmm2/m64, xmm1 */OVERLOAD_2(0x660f2f,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(comiss,{/* comiss xmm2/m32, xmm1 */OVERLOAD_2(  0x0f2f,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cpuid,{/* cpuid */OVERLOAD_0(0x0fa2,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtdq2pd,{/* cvtdq2pd xmm2/m64, xmm1 */OVERLOAD_2(0xf30fe6,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtdq2ps,{/* cvtdq2ps xmm2/m128, xmm1 */OVERLOAD_2(0x0f5b,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtpd2dq,{/* cvtpd2dq xmm2/m128, xmm1 */OVERLOAD_2(0xf20fe6,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtpd2pi,{/* cvtpd2pi xmm/m128, mm */OVERLOAD_2(0xf20fe6,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtpd2ps,{/* cvtpd2ps xmm2/m128, xmm1 */OVERLOAD_2(0x660f5a,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtpi2pd,{/* cvtpi2pd mm/m64, xmm */OVERLOAD_2(0x660f2a,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtpi2ps,{/* cvtpi2ps xmm, mm/m64 */OVERLOAD_2(0x0f2a,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtps2dq,{/* cvtps2dq xmm2/m128, xmm1 */OVERLOAD_2(0x660f5b,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtps2pd,{/* cvtps2pd xmm2/m64, xmm1 */OVERLOAD_2(0x0f5a,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtps2pi,{/* cvtps2pi xmm/m64, mm */OVERLOAD_2(0x0f2d,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtsd2si,{/* cvtsd2si xmm/m64, r32 */OVERLOAD_2(0xf20f2d,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtsd2ss,{/* cvtsd2ss xmm2/m64, xmm1 */OVERLOAD_2(0xf20f51,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtsi2sd,{/* cvtsi2sd r/m32, xmm */OVERLOAD_2(0xf20f2a,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtsi2ss,{/* cvtsi2ss r/m32, xmm */OVERLOAD_2(0xf30f2a,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtss2sd,{/* cvtss2sd xmm2/m32, xmm1 */OVERLOAD_2(0xf30f5a,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvtss2si,{/* cvtss2si xmm/m32, r32 */OVERLOAD_2(0xf30f2d,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cvttpd2pi,{/* cvttpd2pi xmm/m128, mm */OVERLOAD_2(0x660f2c,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX) OVERLOAD_SENTINAL })
DEF_OPCODE(cvttpd2dq,{/* cvttpd2dq xmm2/m128, xmm1 */OVERLOAD_2(0x660fe6,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvttps2dq,{/* cvttps2dq xmm2/m128, xmm1 */OVERLOAD_2(0xf30f5b,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(cvttps2pi,{/* cvttps2pi xmm/m64, mm */OVERLOAD_2(0x0f2c,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX) OVERLOAD_SENTINAL })
DEF_OPCODE(cvttsd2si,{/* cvttsd2si xmm/m64, r32 */OVERLOAD_2(0xf20f2c,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cvttss2si,{/* cvttss2si xmm/m32, r32 */OVERLOAD_2(0xf30f2c,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(cwd,{/* cwd */OVERLOAD_0(0x99,0,0,DCC_ASMOPC_D16) OVERLOAD_SENTINAL })
DEF_OPCODE(cdq,{/* cdq */OVERLOAD_0(0x99,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(daa,{/* daa */OVERLOAD_0(0x27,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(das,{/* das */OVERLOAD_0(0x2f,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(dec,{
    /* dec r16   */OVERLOAD_1(0x48,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16)
    /* dec r32   */OVERLOAD_1(0x48,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32)
    /* dec r/m8  */OVERLOAD_1(0xfe,8, 1,DCC_ASMOPC_MODRM,                 DCC_ASMOPT_R_8|   DCC_ASMOPT_EA)
    /* dec r/m16 */OVERLOAD_1(0xff,16,1,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,/*DCC_ASMOPT_R_16|*/DCC_ASMOPT_EA)  /* NOTE: Don't use these for registers to prevent ambiguity. */
    /* dec r/m32 */OVERLOAD_1(0xff,32,1,DCC_ASMOPC_MODRM,               /*DCC_ASMOPT_R_32|*/DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(div,{
    /* div r/m8  */OVERLOAD_1(0xf6,8, 6,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* div r/m16 */OVERLOAD_1(0xf7,16,6,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* div r/m32 */OVERLOAD_1(0xf7,32,6,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(divpd,{/* divpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f5e,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(divps,{/* divps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f5e,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(divsd,{/* addsd xmm2/m64,  xmm1 */OVERLOAD_2(0xf20f5e,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(divss,{/* addss xmm2/m32,  xmm1 */OVERLOAD_2(0xf30f5e,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(emms,{/* emms */OVERLOAD_0(0x0f77,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(enter,{/* enter $imm16, $imm8 */OVERLOAD_2(0xc8,0,0,0,DCC_ASMOPT_IMM_16,DCC_ASMOPT_IMM_8) OVERLOAD_SENTINAL })
DEF_OPCODE(f2xm1,{/* f2xm1 */OVERLOAD_0(0xd9f0,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fabs,{/* fabs */OVERLOAD_0(0xd9e1,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(fadd,{
    /* fadd m32fp          */OVERLOAD_1(0xd8,32,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fadd m64fp          */OVERLOAD_1(0xdc,64,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fadd %st(i), %st(0) */OVERLOAD_2(0xd8c0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fadd %st(0), %st(i) */OVERLOAD_2(0xdcc0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(faddp,{
    /* faddp %st(i)         */OVERLOAD_1(0xdec0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* faddp %st(0), %st(i) */OVERLOAD_2(0xdec0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    /* faddp %st(i), %st(0) */OVERLOAD_2(0xdec0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* faddp                */OVERLOAD_0(0xdec1,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fiadd,{
    /* fiadd m32int */OVERLOAD_1(0xda,32,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fiadd m64int */OVERLOAD_1(0xde,64,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fadds,{ /* fadds m32fp */OVERLOAD_1(0xd8,32,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fiadds,{ /* fiadds m64int */OVERLOAD_1(0xde,64,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fbld,{ /* fbld m80 */OVERLOAD_1(0xdf,0,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fbstp,{ /* fbstp m80 */OVERLOAD_1(0xdf,0,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fchs,{ /* fchs */OVERLOAD_0(0xd9e0,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fclex,{ /* fclex */OVERLOAD_0(0x9bdbe2,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fnclex,{ /* fnclex */OVERLOAD_0(0xdbe2,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(fcmovb,{ /* fcmovb %st(i), %st(0) */OVERLOAD_2(0xdac0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if below (CF=1). */
DEF_OPCODE(fcmove,{ /* fcmove %st(i), %st(0) */OVERLOAD_2(0xdac8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if equal (ZF=1). */
DEF_OPCODE(fcmovbe,{ /* fcmovbe %st(i), %st(0) */OVERLOAD_2(0xdad0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if below or equal (CF=1 or ZF=1). */
DEF_OPCODE(fcmovu,{ /* fcmovu %st(i), %st(0) */OVERLOAD_2(0xdad8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if unordered (PF=1). */
DEF_OPCODE(fcmovnb,{ /* fcmovnb %st(i), %st(0) */OVERLOAD_2(0xdbc0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if not below (CF=0). */
DEF_OPCODE(fcmovne,{ /* fcmovne %st(i), %st(0) */OVERLOAD_2(0xdbc8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if not equal (ZF=0). */
DEF_OPCODE(fcmovnbe,{ /* fcmovnbe %st(i), %st(0) */OVERLOAD_2(0xdbd0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if not below or equal (CF=0 and ZF=0). */
DEF_OPCODE(fcmovnu,{ /* fcmovnu %st(i), %st(0) */OVERLOAD_2(0xdbd8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL }) /* Move if not unordered (PF=0). */

DEF_OPCODE(fcom,{
    /* fcom m32fp  */OVERLOAD_1(0xd8,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fcom m64fp  */OVERLOAD_1(0xdc,64,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fcom %st(i) */OVERLOAD_1(0xd8d0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* fcom        */OVERLOAD_0(0xd8d1,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fcomp,{
    /* fcomp m32fp  */OVERLOAD_1(0xd8,32,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fcomp m64fp  */OVERLOAD_1(0xdc,64,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fcomp %st(i) */OVERLOAD_1(0xd8d8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* fcomp        */OVERLOAD_0(0xd8d9,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fcompp,{
    /* fcompp */OVERLOAD_0(0xded9,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fcomi,{ /* fcomi %st(i), %st(0) */OVERLOAD_2(0xdbf0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL })
DEF_OPCODE(fcomip,{ /* fcomip %st(i), %st(0) */OVERLOAD_2(0xdff0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL })
DEF_OPCODE(fucomi,{ /* fucomi %st(i), %st(0) */OVERLOAD_2(0xdbe8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL })
DEF_OPCODE(fucomip,{ /* fucomip %st(i), %st(0) */OVERLOAD_2(0xdfe8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0) OVERLOAD_SENTINAL })
DEF_OPCODE(fcos,{ /* fcos */OVERLOAD_0(0xd9ff,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fdecstp,{ /* fdecstp */OVERLOAD_0(0xd9f6,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fdiv,{
    /* fdiv m32fp          */OVERLOAD_1(0xd8,32,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fdiv m64fp          */OVERLOAD_1(0xdc,64,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fdiv %st(i), %st(0) */OVERLOAD_2(0xd8f0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fdiv %st(0), %st(i) */OVERLOAD_2(0xdcf8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fdivp,{
    /* fdivp %st(0), %st(i) */OVERLOAD_2(0xdef8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    /* fdivp                */OVERLOAD_0(0xdef9,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fidiv,{
    /* fidiv m32fp */OVERLOAD_1(0xda,32,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fidiv m64fp */OVERLOAD_1(0xde,64,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fdivr,{
    /* fdivr m32fp          */OVERLOAD_1(0xd8,32,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fdivr m64fp          */OVERLOAD_1(0xdc,64,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fdivr %st(i), %st(0) */OVERLOAD_2(0xd8f8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fdivr %st(0), %st(i) */OVERLOAD_2(0xdcf0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fdivrp,{
    /* fdivrp %st(0), %st(i) */OVERLOAD_2(0xdef0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    /* fdivrp                */OVERLOAD_0(0xdef1,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fidivr,{
    /* fidivr m32fp */OVERLOAD_1(0xda,32,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fidivr m64fp */OVERLOAD_1(0xde,64,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ffree,{
    /* ffree %st(i) */OVERLOAD_1(0xddc0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ficom,{
    /* ficom m16int */OVERLOAD_1(0xde,16,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* ficom m32int */OVERLOAD_1(0xda,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ficomp,{
    /* ficomp m16int */OVERLOAD_1(0xde,16,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* ficomp m32int */OVERLOAD_1(0xda,32,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fild,{
    /* fild m16int */OVERLOAD_1(0xdf,16,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fild m32int */OVERLOAD_1(0xdb,32,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fild m64int */OVERLOAD_1(0xdf,64,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fincstp,{ /* fincstp */OVERLOAD_0(0xd9f7,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(finit,{ /* finit */OVERLOAD_0(0x9bdbe3,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fninit,{ /* fninit */OVERLOAD_0(0xdbe3,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fist,{
    /* fistw m16int */OVERLOAD_1(0xdf,16,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fistl m32int */OVERLOAD_1(0xdb,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fistp,{
    /* fistpw m16int */OVERLOAD_1(0xdf,16,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fistpl m32int */OVERLOAD_1(0xdb,32,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fistpq m64int */OVERLOAD_1(0xdf,64,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fisttp,{
    /* fisttp m16int */OVERLOAD_1(0xdf,16,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fisttp m32int */OVERLOAD_1(0xdb,32,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fisttp m64int */OVERLOAD_1(0xdd,64,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fld,{
    /* fld m64fp  */OVERLOAD_1(0xdd,32,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fld %st(i) */OVERLOAD_1(0xd9c0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(flds,{
    /* flds m16fp */OVERLOAD_1(0xd9,16,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fldt,{
    /* fldt m80fp */OVERLOAD_1(0xdb,0,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fld1,{ /* fld1 */OVERLOAD_0(0xd9e8,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldl2t,{ /* fldl2t */OVERLOAD_0(0xd9e9,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldl2e,{ /* fldl2e */OVERLOAD_0(0xd9ea,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldpi,{ /* fldpi */OVERLOAD_0(0xd9eb,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldlg2,{ /* fldlg2 */OVERLOAD_0(0xd9ec,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldln2,{ /* fldln2 */OVERLOAD_0(0xd9ed,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldz,{ /* fldz */OVERLOAD_0(0xd9ee,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fldcw,{ /* fldcw m2byte */OVERLOAD_1(0xd9,8,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fldenv,{ /* fldenv m14/28byte */OVERLOAD_1(0xd9,0,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fmul,{
    /* fmul m32fp          */OVERLOAD_1(0xd8,32,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fmul m64fp          */OVERLOAD_1(0xdc,64,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fmul %st(i), %st(0) */OVERLOAD_2(0xd8c8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fmul %st(0), %st(i) */OVERLOAD_2(0xdcc8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fmulp,{
    /* fmulp %st(0), %st(i) */OVERLOAD_2(0xdec8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    /* fmulp                */OVERLOAD_0(0xdec9,0,0,DCC_ASMOPC_REG)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fimul,{
    /* fimul m32int */OVERLOAD_1(0xda,32,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fimul m16int */OVERLOAD_1(0xde,16,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fnop,{ /* fnop */OVERLOAD_0(0xd9d0,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fpatan,{ /* fpatan */OVERLOAD_0(0xd9f3,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fprem,{ /* fprem */OVERLOAD_0(0xd9f8,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fprem1,{ /* fprem1 */OVERLOAD_0(0xd9f5,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fptan,{ /* fptan */OVERLOAD_0(0xd9f2,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(frndint,{ /* frndint */OVERLOAD_0(0xd9fc,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(frstor,{ /* frstor m94/108byte */OVERLOAD_1(0xdd,0,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fsave,{ /* fsave m94/108byte */OVERLOAD_1(0x9bdd,0,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fnsave,{ /* fnsave m94/108byte */OVERLOAD_1(0xdd,0,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fscale,{ /* fscale */OVERLOAD_0(0xd9fd,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fsin,{ /* fsin */OVERLOAD_0(0xd9fe,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fsincos,{ /* fsincos */OVERLOAD_0(0xd9fb,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fsqrt,{ /* fsqrt */OVERLOAD_0(0xd9fa,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fst,{
    /* fst m64fp  */OVERLOAD_1(0xdd,64,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fst %st(i) */OVERLOAD_1(0xddd0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fsts,{
    /* fsts m32fp */OVERLOAD_1(0xd9,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fstp,{
    /* fstp m64fp  */OVERLOAD_1(0xdd,64,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fstp %st(i) */OVERLOAD_1(0xddd8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fstps,{
    /* fstp m32fp  */OVERLOAD_1(0xd9,0,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fstpt,{
    /* fstpt m80fp */OVERLOAD_1(0xdb,0,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fstcw,{ /* fstcw m2byte */OVERLOAD_1(0x9bd9,16,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fnstcw,{ /* fnstcw m2byte */OVERLOAD_1(0xd9,16,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fstenv,{ /* fstenv m14/28byte */OVERLOAD_1(0x9bd9,0,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fnstenv,{ /* fnstenv m14/28byte */OVERLOAD_1(0xd9,0,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })

DEF_OPCODE(fsub,{
    /* fsub m32fp          */OVERLOAD_1(0xd8,32,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fsub m64fp          */OVERLOAD_1(0xdc,64,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fsub %st(i), %st(0) */OVERLOAD_2(0xd8e0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fsub %st(0), %st(i) */OVERLOAD_2(0xdce0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fsubp,{
    /* fsubp %st(i)         */OVERLOAD_1(0xdee0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* fsubp %st(0), %st(i) */OVERLOAD_2(0xdee0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    /* fsubp %st(i), %st(0) */OVERLOAD_2(0xdee0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fsubp                */OVERLOAD_0(0xdee1,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fisub,{
    /* fisub m32int */OVERLOAD_1(0xda,32,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fisub m64int */OVERLOAD_1(0xde,64,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fsubs,{ /* fsubs m32fp */OVERLOAD_1(0xd8,32,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fisubs,{ /* fisubs m64int */OVERLOAD_1(0xde,64,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })

DEF_OPCODE(fsubr,{
    /* fsubr m32fp          */OVERLOAD_1(0xd8,32,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fsubr m64fp          */OVERLOAD_1(0xdc,64,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fsubr %st(i), %st(0) */OVERLOAD_2(0xd8e8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0)
    /* fsubr %st(0), %st(i) */OVERLOAD_2(0xdce0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fsubrp,{
    /* fsubrp %st(0), %st(i) */OVERLOAD_2(0xdee0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST|DCC_ASMOPT_ST0,DCC_ASMOPT_R_ST)
    /* fsubrp                */OVERLOAD_0(0xdee1,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fisubr,{
    /* fisubr m32fp */OVERLOAD_1(0xda,32,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    /* fisubr m64fp */OVERLOAD_1(0xde,64,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ftst,{ /* ftst */OVERLOAD_0(0xd9e4,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fucom,{
    /* fucom %st(i) */OVERLOAD_1(0xdde0,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* fucom        */OVERLOAD_0(0xdde1,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fucomp,{
    /* fucomp %st(i) */OVERLOAD_1(0xdde8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* fucomp        */OVERLOAD_0(0xdde9,0,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fucompp,{ /* fucompp */OVERLOAD_0(0xdae9,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fxam,{ /* fxam */OVERLOAD_0(0xd9e5,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fxch,{
    /* fxch %st(i) */OVERLOAD_1(0xd9c8,0,0,DCC_ASMOPC_REG,DCC_ASMOPT_R_ST)
    /* fxch        */OVERLOAD_0(0xd9c9,0,0,DCC_ASMOPC_REG)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(fxrstor,{ /* fxrstor m512byte */OVERLOAD_1(0x0fae,0,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fxsave, { /* fxsave  m512byte */OVERLOAD_1(0x0fae,0,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(fxtract,{ /* fxtract */OVERLOAD_0(0xd9f4,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fyl2x,  { /* fyl2x */OVERLOAD_0(0xd9f1,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fyl2xp1,{ /* fyl2xp1 */OVERLOAD_0(0xd9f9,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(haddpd, { /* haddpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f7c,128,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(haddps, { /* haddps xmm2/m128, xmm1 */OVERLOAD_2(0xf20f7c,128,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(hlt,    { /* hlt */OVERLOAD_0(0xf4,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(hsubpd, { /* hsubpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f7d,128,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(hsubps, { /* hsubps xmm2/m128, xmm1 */OVERLOAD_2(0xf20f7d,128,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(idiv,{
    /* idiv r/m8  */OVERLOAD_1(0xf6,8, 7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8)
    /* idiv r/m16 */OVERLOAD_1(0xf7,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16)
    /* idiv r/m32 */OVERLOAD_1(0xf7,32,7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(imul,{
    /* imul r/m8                */OVERLOAD_1(0xf6,8, 5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8)
    /* imul r/m16               */OVERLOAD_1(0xf7,16,5,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16)
    /* imul r/m32               */OVERLOAD_1(0xf7,32,5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32)
    /* imul r/m16, %r16         */OVERLOAD_2(0x0faf,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* imul r/m32, %r32         */OVERLOAD_2(0x0faf,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    /* imul $imm8, r/m16, %r16  */OVERLOAD_3(0x6b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* imul $imm8, r/m32, %r32  */OVERLOAD_3(0x6b,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* imul $imm8, %r16         */OVERLOAD_2(0x6b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16)
    /* imul $imm8, %r32         */OVERLOAD_2(0x6b,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16)
    /* imul $imm16, r/m16, %r16 */OVERLOAD_3(0x69,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* imul $imm32, r/m32, %r32 */OVERLOAD_3(0x69,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* imul $imm16, %r16        */OVERLOAD_2(0x69,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16)
    /* imul $imm32, %r32        */OVERLOAD_2(0x69,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(in,{
    /* in $imm8, %al  */OVERLOAD_2(0xe4,8, 0,0,             DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* in $imm8, %ax  */OVERLOAD_2(0xe5,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* in $imm8, %eax */OVERLOAD_2(0xe5,32,0,0,             DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* in %dx, %al    */OVERLOAD_2(0xec,8, 0,0,             DCC_ASMOPT_R_16|DCC_ASMOPT_DX,DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* in %dx, %ax    */OVERLOAD_2(0xed,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_DX,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* in %dx, %eax   */OVERLOAD_2(0xed,32,0,0,             DCC_ASMOPT_R_16|DCC_ASMOPT_DX,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(inc,{
    /* inc r16   */OVERLOAD_1(0x40,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16)
    /* inc r32   */OVERLOAD_1(0x40,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32)
    /* inc r/m8  */OVERLOAD_1(0xfe,8, 0,DCC_ASMOPC_MODRM,                 DCC_ASMOPT_R_8|   DCC_ASMOPT_EA)
    /* inc r/m16 */OVERLOAD_1(0xff,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,/*DCC_ASMOPT_R_16|*/DCC_ASMOPT_EA)  /* NOTE: Don't use these for registers to prevent ambiguity. */
    /* inc r/m32 */OVERLOAD_1(0xff,32,0,DCC_ASMOPC_MODRM,               /*DCC_ASMOPT_R_32|*/DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ins,{
    /* insb */OVERLOAD_0(0x6c,8, 0,0)
    /* insw */OVERLOAD_0(0x6d,16,0,DCC_ASMOPC_D16)
    /* insl */OVERLOAD_0(0x6d,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(int,{ /* int $imm8 */OVERLOAD_1(0xcd,0,0,0,DCC_ASMOPT_IMM_8) OVERLOAD_SENTINAL })
DEF_OPCODE(into,{ /* into */OVERLOAD_0(0xce,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(invd,{ /* invd */OVERLOAD_0(0x0f08,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(invlpg,{ /* invlpg m */OVERLOAD_1(0x0f08,0,7,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(iret,{ /* iret */OVERLOAD_0(0xcf,0,0,0) OVERLOAD_SENTINAL })

/* === Conditional jump opcodes === */
DEF_OPCODE(jcxz, { /* jcxz  rel8 */OVERLOAD_1(0xe3,8,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL }) /* Jump short if CX register is 0. */
DEF_OPCODE(jecxz,{ /* jecxz rel8 */OVERLOAD_1(0xe3,8,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL }) /* Jump short if ECX register is 0. */

/*[[[deemon
local ops = list {
    { "ja", 0x77, 0x0f87, "Jump if above (CF=0 and ZF=0)." },
    { "jae", 0x73, 0x0f83, "Jump if above or equal (CF=0)." },
    { "jb", 0x72, 0x0f82, "Jump if below (CF=1)." },
    { "jbe", 0x76, 0x0f86, "Jump if below or equal (CF=1 or ZF=1)." },
    { "jc", 0x72, 0x0f82, "Jump if carry (CF=1)." },
    { "je", 0x74, 0x0f84, "Jump if equal (ZF=1)." },
    { "jg", 0x7f, 0x0f8f, "Jump if greater (ZF=0 and SF=OF)." },
    { "jge", 0x7d, 0x0f8d, "Jump if greater or equal (SF=OF)." },
    { "jl", 0x7c, 0x0f8c, "Jump if less (SF<>OF)." },
    { "jle", 0x7e, 0x0f8e, "Jump if less or equal (ZF=1 or SF<>OF)." },
    { "jna", 0x76, 0x0f86, "Jump if not above (CF=1 or ZF=1)." },
    { "jnae", 0x72, 0x0f82, "Jump if not above or equal (CF=1)." },
    { "jnb", 0x73, 0x0f83, "Jump if not below (CF=0)." },
    { "jnbe", 0x77, 0x0f87, "Jump if not below or equal (CF=0 and ZF=0)." },
    { "jnc", 0x73, 0x0f83, "Jump if not carry (CF=0)." },
    { "jne", 0x75, 0x0f85, "Jump if not equal (ZF=0)." },
    { "jng", 0x7e, 0x0f8e, "Jump if not greater (ZF=1 or SF<>OF)." },
    { "jnge", 0x7c, 0x0f8c, "Jump if not greater or equal (SF<>OF)." },
    { "jnl", 0x7d, 0x0f8d, "Jump if not less (SF=OF)." },
    { "jnle", 0x7f, 0x0f8f, "Jump if not less or equal (ZF=0 and SF=OF)." },
    { "jno", 0x71, 0x0f81, "Jump if not overflow (OF=0)." },
    { "jnp", 0x7b, 0x0f8b, "Jump if not parity (PF=0)." },
    { "jns", 0x79, 0x0f89, "Jump if not sign (SF=0)." },
    { "jnz", 0x75, 0x0f85, "Jump if not zero (ZF=0)." },
    { "jo", 0x70, 0x0f80, "Jump if overflow (OF=1)." },
    { "jp", 0x7a, 0x0f8a, "Jump if parity (PF=1)." },
    { "jpe", 0x7a, 0x0f8a, "Jump if parity even (PF=1)." },
    { "jpo", 0x7b, 0x0f8b, "Jump if parity odd (PF=0)." },
    { "js", 0x78, 0x0f88, "Jump if sign (SF=1)." },
    { "jz", 0x74, 0x0f84, "Jump if 0 (ZF=1)." },
};
for (local name,oa,ob,comment: ops) {
    local prefix = "              "+(" "*#name);
    print "DEF_OPCODE("+name+",{ /" "* "+name+" rel8  *" "/OVERLOAD_1("+("0x%.2x" % oa)+",  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)";
    print prefix[:#prefix-1]+"//"       "* "+name+" rel16 *" "/OVERLOAD_1("+("0x%.4x" % ob)+  ",16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)";
    print prefix+"/"       "* "+name+" rel32 *" "/OVERLOAD_1("+("0x%.4x" % ob)+  ",32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)";
    print prefix+"OVERLOAD_SENTINAL }) /" "* "+comment+" *" "/";
}
]]]*/
DEF_OPCODE(ja,{ /* ja rel8  */OVERLOAD_1(0x77,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* ja rel16 */OVERLOAD_1(0x0f87,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* ja rel32 */OVERLOAD_1(0x0f87,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if above (CF=0 and ZF=0). */
DEF_OPCODE(jae,{ /* jae rel8  */OVERLOAD_1(0x73,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jae rel16 */OVERLOAD_1(0x0f83,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jae rel32 */OVERLOAD_1(0x0f83,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if above or equal (CF=0). */
DEF_OPCODE(jb,{ /* jb rel8  */OVERLOAD_1(0x72,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jb rel16 */OVERLOAD_1(0x0f82,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jb rel32 */OVERLOAD_1(0x0f82,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if below (CF=1). */
DEF_OPCODE(jbe,{ /* jbe rel8  */OVERLOAD_1(0x76,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jbe rel16 */OVERLOAD_1(0x0f86,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jbe rel32 */OVERLOAD_1(0x0f86,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if below or equal (CF=1 or ZF=1). */
DEF_OPCODE(jc,{ /* jc rel8  */OVERLOAD_1(0x72,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jc rel16 */OVERLOAD_1(0x0f82,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jc rel32 */OVERLOAD_1(0x0f82,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if carry (CF=1). */
DEF_OPCODE(je,{ /* je rel8  */OVERLOAD_1(0x74,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* je rel16 */OVERLOAD_1(0x0f84,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* je rel32 */OVERLOAD_1(0x0f84,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if equal (ZF=1). */
DEF_OPCODE(jg,{ /* jg rel8  */OVERLOAD_1(0x7f,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jg rel16 */OVERLOAD_1(0x0f8f,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jg rel32 */OVERLOAD_1(0x0f8f,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if greater (ZF=0 and SF=OF). */
DEF_OPCODE(jge,{ /* jge rel8  */OVERLOAD_1(0x7d,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jge rel16 */OVERLOAD_1(0x0f8d,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jge rel32 */OVERLOAD_1(0x0f8d,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if greater or equal (SF=OF). */
DEF_OPCODE(jl,{ /* jl rel8  */OVERLOAD_1(0x7c,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jl rel16 */OVERLOAD_1(0x0f8c,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jl rel32 */OVERLOAD_1(0x0f8c,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if less (SF<>OF). */
DEF_OPCODE(jle,{ /* jle rel8  */OVERLOAD_1(0x7e,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jle rel16 */OVERLOAD_1(0x0f8e,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jle rel32 */OVERLOAD_1(0x0f8e,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if less or equal (ZF=1 or SF<>OF). */
DEF_OPCODE(jna,{ /* jna rel8  */OVERLOAD_1(0x76,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jna rel16 */OVERLOAD_1(0x0f86,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jna rel32 */OVERLOAD_1(0x0f86,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not above (CF=1 or ZF=1). */
DEF_OPCODE(jnae,{ /* jnae rel8  */OVERLOAD_1(0x72,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                 //* jnae rel16 */OVERLOAD_1(0x0f82,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                  /* jnae rel32 */OVERLOAD_1(0x0f82,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                  OVERLOAD_SENTINAL }) /* Jump if not above or equal (CF=1). */
DEF_OPCODE(jnb,{ /* jnb rel8  */OVERLOAD_1(0x73,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jnb rel16 */OVERLOAD_1(0x0f83,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jnb rel32 */OVERLOAD_1(0x0f83,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not below (CF=0). */
DEF_OPCODE(jnbe,{ /* jnbe rel8  */OVERLOAD_1(0x77,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                 //* jnbe rel16 */OVERLOAD_1(0x0f87,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                  /* jnbe rel32 */OVERLOAD_1(0x0f87,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                  OVERLOAD_SENTINAL }) /* Jump if not below or equal (CF=0 and ZF=0). */
DEF_OPCODE(jnc,{ /* jnc rel8  */OVERLOAD_1(0x73,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jnc rel16 */OVERLOAD_1(0x0f83,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jnc rel32 */OVERLOAD_1(0x0f83,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not carry (CF=0). */
DEF_OPCODE(jne,{ /* jne rel8  */OVERLOAD_1(0x75,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jne rel16 */OVERLOAD_1(0x0f85,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jne rel32 */OVERLOAD_1(0x0f85,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not equal (ZF=0). */
DEF_OPCODE(jng,{ /* jng rel8  */OVERLOAD_1(0x7e,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jng rel16 */OVERLOAD_1(0x0f8e,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jng rel32 */OVERLOAD_1(0x0f8e,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not greater (ZF=1 or SF<>OF). */
DEF_OPCODE(jnge,{ /* jnge rel8  */OVERLOAD_1(0x7c,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                 //* jnge rel16 */OVERLOAD_1(0x0f8c,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                  /* jnge rel32 */OVERLOAD_1(0x0f8c,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                  OVERLOAD_SENTINAL }) /* Jump if not greater or equal (SF<>OF). */
DEF_OPCODE(jnl,{ /* jnl rel8  */OVERLOAD_1(0x7d,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jnl rel16 */OVERLOAD_1(0x0f8d,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jnl rel32 */OVERLOAD_1(0x0f8d,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not less (SF=OF). */
DEF_OPCODE(jnle,{ /* jnle rel8  */OVERLOAD_1(0x7f,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                 //* jnle rel16 */OVERLOAD_1(0x0f8f,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                  /* jnle rel32 */OVERLOAD_1(0x0f8f,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                  OVERLOAD_SENTINAL }) /* Jump if not less or equal (ZF=0 and SF=OF). */
DEF_OPCODE(jno,{ /* jno rel8  */OVERLOAD_1(0x71,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jno rel16 */OVERLOAD_1(0x0f81,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jno rel32 */OVERLOAD_1(0x0f81,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not overflow (OF=0). */
DEF_OPCODE(jnp,{ /* jnp rel8  */OVERLOAD_1(0x7b,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jnp rel16 */OVERLOAD_1(0x0f8b,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jnp rel32 */OVERLOAD_1(0x0f8b,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not parity (PF=0). */
DEF_OPCODE(jns,{ /* jns rel8  */OVERLOAD_1(0x79,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jns rel16 */OVERLOAD_1(0x0f89,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jns rel32 */OVERLOAD_1(0x0f89,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not sign (SF=0). */
DEF_OPCODE(jnz,{ /* jnz rel8  */OVERLOAD_1(0x75,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jnz rel16 */OVERLOAD_1(0x0f85,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jnz rel32 */OVERLOAD_1(0x0f85,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if not zero (ZF=0). */
DEF_OPCODE(jo,{ /* jo rel8  */OVERLOAD_1(0x70,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jo rel16 */OVERLOAD_1(0x0f80,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jo rel32 */OVERLOAD_1(0x0f80,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if overflow (OF=1). */
DEF_OPCODE(jp,{ /* jp rel8  */OVERLOAD_1(0x7a,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jp rel16 */OVERLOAD_1(0x0f8a,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jp rel32 */OVERLOAD_1(0x0f8a,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if parity (PF=1). */
DEF_OPCODE(jpe,{ /* jpe rel8  */OVERLOAD_1(0x7a,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jpe rel16 */OVERLOAD_1(0x0f8a,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jpe rel32 */OVERLOAD_1(0x0f8a,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if parity even (PF=1). */
DEF_OPCODE(jpo,{ /* jpo rel8  */OVERLOAD_1(0x7b,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
                //* jpo rel16 */OVERLOAD_1(0x0f8b,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                 /* jpo rel32 */OVERLOAD_1(0x0f8b,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                 OVERLOAD_SENTINAL }) /* Jump if parity odd (PF=0). */
DEF_OPCODE(js,{ /* js rel8  */OVERLOAD_1(0x78,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* js rel16 */OVERLOAD_1(0x0f88,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* js rel32 */OVERLOAD_1(0x0f88,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if sign (SF=1). */
DEF_OPCODE(jz,{ /* jz rel8  */OVERLOAD_1(0x74,  8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
               //* jz rel16 */OVERLOAD_1(0x0f84,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
                /* jz rel32 */OVERLOAD_1(0x0f84,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
                OVERLOAD_SENTINAL }) /* Jump if 0 (ZF=1). */
//[[[end]]]
DEF_OPCODE(jmp,{
    /* jmp rel8   */OVERLOAD_1(0xeb,8, 0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
   //* jmp rel16  */OVERLOAD_1(0xe9,16,0,DCC_ASMOPC_DISP|DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
    /* jmp rel32  */OVERLOAD_1(0xe9,32,0,DCC_ASMOPC_DISP,               DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
    /* jmp *r/m16 */OVERLOAD_1(0xff,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA|DCC_ASMOPT_IND)
    /* jmp *r/m32 */OVERLOAD_1(0xff,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA|DCC_ASMOPT_IND)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ljmp,{
    /* ljmp ptr16,16 */OVERLOAD_2(0xea,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,DCC_ASMOPT_IMM_16)
    /* ljmp ptr16,32 */OVERLOAD_2(0xea,32,0,0,             DCC_ASMOPT_IMM_16,DCC_ASMOPT_IMM_32)
    /* ljmp m16      */OVERLOAD_1(0xff,16,5,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA)
    /* ljmp m32      */OVERLOAD_1(0xff,32,5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lahf,{ /* lahf */OVERLOAD_0(0x9f,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(lar,{
    /* lar r/m16, r16 */OVERLOAD_2(0x0f02,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lar r/m32, r32 */OVERLOAD_2(0x0f02,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lddqu,{ /* lddqu m128, xmm */OVERLOAD_2(0xf20ff0,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(ldmxcsr,{ /* ldmxcsr m32 */OVERLOAD_1(0x0fae,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(lds,{
    /* lds m16, r16 */OVERLOAD_2(0xc5,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lds m32, r32 */OVERLOAD_2(0xc5,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lss,{
    /* lss m16, r16 */OVERLOAD_2(0x0fb2,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lss m32, r32 */OVERLOAD_2(0x0fb2,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(les,{
    /* les m16, r16 */OVERLOAD_2(0xc4,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* les m32, r32 */OVERLOAD_2(0xc4,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lfs,{
    /* lfs m16, r16 */OVERLOAD_2(0x0fb4,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lfs m32, r32 */OVERLOAD_2(0x0fb4,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lgs,{
    /* lgs m16, r16 */OVERLOAD_2(0x0fb5,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lgs m32, r32 */OVERLOAD_2(0x0fb5,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lea,{
    /* lea m, r16 */OVERLOAD_2(0x8d,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lea m, r32 */OVERLOAD_2(0x8d,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(leave,{ /* leave */OVERLOAD_0(0xc9,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(lfence,{ /* lfence */OVERLOAD_0(0x0fae,0,5,DCC_ASMOPC_MODRM) OVERLOAD_SENTINAL })
DEF_OPCODE(lgdt,{ /* lgdt m48 */OVERLOAD_1(0x0f01,0,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(lidt,{ /* lidt m48 */OVERLOAD_1(0x0f01,0,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(lldt,{ /* lldt r/m16 */OVERLOAD_1(0x0f00,16,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(lmsw,{ /* lmsw r/m16 */OVERLOAD_1(0x0f01,16,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(lock,{ /* lock */OVERLOAD_0(0xf0,0,0,DCC_ASMOPC_PREFIX) OVERLOAD_SENTINAL })
DEF_OPCODE(lods,{
    /* lodsb */OVERLOAD_0(0xac,8, 0,0)
    /* lodsw */OVERLOAD_0(0xad,16,0,DCC_ASMOPC_D16)
    /* lodsl */OVERLOAD_0(0xad,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(loop,  { /* loop rel8   */OVERLOAD_1(0xe2,8,0,DCC_ASMOPC_DISP,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL })
DEF_OPCODE(loope, { /* loope rel8  */OVERLOAD_1(0xe1,8,0,DCC_ASMOPC_DISP,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL })
DEF_OPCODE(loopz, { /* loopz rel8  */OVERLOAD_1(0xe1,8,0,DCC_ASMOPC_DISP,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL })
DEF_OPCODE(loopne,{ /* loopne rel8 */OVERLOAD_1(0xe0,8,0,DCC_ASMOPC_DISP,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL })
DEF_OPCODE(loopnz,{ /* loopnz rel8 */OVERLOAD_1(0xe0,8,0,DCC_ASMOPC_DISP,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8) OVERLOAD_SENTINAL })
DEF_OPCODE(lsl,{
    /* lsl r/m16, %r16 */OVERLOAD_2(0x0f03,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* lsl r/m32, %r32 */OVERLOAD_2(0x0f03,16,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ltr,{ /* ltr r/m16 */OVERLOAD_1(0x0f00,16,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(maskmovdqu,{/* maskmovdqu xmm2, xmm1 */OVERLOAD_2(0x660ff7,0,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(maskmovq,{/* maskmovq mm2, mm1 */OVERLOAD_2(0x660ff7,0,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX,DCC_ASMOPT_R_MMX) OVERLOAD_SENTINAL })
DEF_OPCODE(maxpd,{/* maxpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f5f,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(maxps,{/* maxps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f5f,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(maxsd,{/* maxpd xmm2/m64, xmm1 */OVERLOAD_2(0xf20f5f,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(maxss,{/* maxps xmm2/m64, xmm1 */OVERLOAD_2(0xf30f5f,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(mfence,{/* mfence */OVERLOAD_0(0x0fae,0,6,DCC_ASMOPC_MODRM) OVERLOAD_SENTINAL })
DEF_OPCODE(minpd,{/* minpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f5d,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(minps,{/* minps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f5d,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(minsd,{/* minpd xmm2/m64, xmm1 */OVERLOAD_2(0xf20f5d,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(minss,{/* minps xmm2/m64, xmm1 */OVERLOAD_2(0xf30f5d,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(monitor,{/* monitor */OVERLOAD_0(0x0f01c8,0,0,0) OVERLOAD_SENTINAL })


DEF_OPCODE(mov,{
    /* mov rel8, %al    */OVERLOAD_2(0xa0,8, 0,0,             DCC_ASMOPT_ADDR|DCC_ASMOPT_A8,DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* mov rel16, %ax   */OVERLOAD_2(0xa1,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* mov rel32, %eax  */OVERLOAD_2(0xa1,32,0,0,             DCC_ASMOPT_ADDR|DCC_ASMOPT_A32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* mov %al, rel8    */OVERLOAD_2(0xa2,8, 0,0,             DCC_ASMOPT_R_8|DCC_ASMOPT_EAX,DCC_ASMOPT_ADDR|DCC_ASMOPT_A8)
    /* mov %ax, rel16   */OVERLOAD_2(0xa3,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX,DCC_ASMOPT_ADDR|DCC_ASMOPT_A16)
    /* mov %eax, rel32  */OVERLOAD_2(0xa3,32,0,0,             DCC_ASMOPT_R_32|DCC_ASMOPT_EAX,DCC_ASMOPT_ADDR|DCC_ASMOPT_A32)
    /* mov r8,  r/m8    */OVERLOAD_2(0x88,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* mov r16, r/m16   */OVERLOAD_2(0x89,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* mov r32, r/m32   */OVERLOAD_2(0x89,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* mov r/m8, r8     */OVERLOAD_2(0x8a,8, 'r',DCC_ASMOPC_MODRM,               /*DCC_ASMOPT_R_8|*/DCC_ASMOPT_EA,DCC_ASMOPT_R_8)
    /* mov r/m16, r16   */OVERLOAD_2(0x8b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,/*DCC_ASMOPT_R_16|*/DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* mov r/m32, r32   */OVERLOAD_2(0x8b,32,'r',DCC_ASMOPC_MODRM,               /*DCC_ASMOPT_R_32|*/DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    /* mov Sreg, r/m16  */OVERLOAD_2(0x8c,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_SEG,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* mov Sreg, r/m32  */OVERLOAD_2(0x8c,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_SEG,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* mov r/m16, Sreg  */OVERLOAD_2(0x8e,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_SEG)
    /* mov r/m32, Sreg  */OVERLOAD_2(0x8e,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_SEG)
    /* mov imm8, r8     */OVERLOAD_2(0xb0,8, 0,DCC_ASMOPC_REG,               DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_8)
    /* mov imm16, r16   */OVERLOAD_2(0xb8,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16)
    /* mov imm32, r32   */OVERLOAD_2(0xb8,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32)
    /* mov imm8, r/m8   */OVERLOAD_2(0xc6,8, 0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8, /*DCC_ASMOPT_R_8|*/DCC_ASMOPT_EA) /* NOTE: Don't use these for registers to prevent ambiguity. */
    /* mov imm16, r/m16 */OVERLOAD_2(0xc7,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,/*DCC_ASMOPT_R_16|*/DCC_ASMOPT_EA)
    /* mov imm32, r/m32 */OVERLOAD_2(0xc7,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_32,/*DCC_ASMOPT_R_32|*/DCC_ASMOPT_EA)
    /* mov crx, r32     */OVERLOAD_2(0x0f20,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_CR,DCC_ASMOPT_R_32)
    /* mov drx, r32     */OVERLOAD_2(0x0f21,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_DR,DCC_ASMOPT_R_32)
    /* mov trx, r32     */OVERLOAD_2(0x0f24,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_TR,DCC_ASMOPT_R_32)
    /* mov r32, crx     */OVERLOAD_2(0x0f22,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32,DCC_ASMOPT_R_CR)
    /* mov r32, drx     */OVERLOAD_2(0x0f23,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32,DCC_ASMOPT_R_DR)
    /* mov r32, trx     */OVERLOAD_2(0x0f26,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32,DCC_ASMOPT_R_TR)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movapd,{
    /* movapd xmm1, xmm2/m128 */OVERLOAD_2(0x660f28,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    /* movapd xmm2/m128, xmm1 */OVERLOAD_2(0x660f29,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movaps,{
    /* movaps xmm1, xmm2/m128 */OVERLOAD_2(0x0f28,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    /* movaps xmm2/m128, xmm1 */OVERLOAD_2(0x0f29,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movd,{
    /* movd r/m32, mm */OVERLOAD_2(0x0f6e,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* movd mm, r/m32 */OVERLOAD_2(0x0f7e,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* movd r/m32, xmm */OVERLOAD_2(0x660f6e,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* movd xmm, r/m32 */OVERLOAD_2(0x660f7e,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movddup,{
    /* movddup xmm2/m64, xmm1 */OVERLOAD_2(0xf20f12,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movdqa,{
    /* movdqa xmm2/m128, xmm1 */OVERLOAD_2(0x660f6f,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movdqa xmm1, xmm2/m128 */OVERLOAD_2(0x660f7f,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movdqu,{
    /* movdqu xmm2/m128, xmm1 */OVERLOAD_2(0xf30f6f,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movdqu xmm1, xmm2/m128 */OVERLOAD_2(0xf30f7f,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movdq2q,{
    /* movdq2q xmm, mm */OVERLOAD_2(0xf20fd6,0,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_MMX)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movhlps,{
    /* movhlps xmm2, xmm1 */OVERLOAD_2(0x0f12,0,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movhpd,{
    /* movhpd m64, xmm */OVERLOAD_2(0x660f16,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movhpd xmm, m64 */OVERLOAD_2(0x660f17,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movhps,{
    /* movhps m64, xmm */OVERLOAD_2(0x0f16,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movhps xmm, m64 */OVERLOAD_2(0x0f17,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movlhps,{
    /* movlhps xmm2, xmm1 */OVERLOAD_2(0x0f16,0,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movlpd,{
    /* movlpd m64, xmm */OVERLOAD_2(0x660f12,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movlpd xmm, m64 */OVERLOAD_2(0x660f13,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movlps,{
    /* movlps m64, xmm */OVERLOAD_2(0x0f12,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movlps xmm, m64 */OVERLOAD_2(0x0f13,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movmskpd,{ /* movmskpd xmm, r32 */OVERLOAD_2(0x660f50,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(movmskps,{ /* movmskps xmm, r32 */OVERLOAD_2(  0x0f50,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_32) OVERLOAD_SENTINAL })
DEF_OPCODE(movntdq,{ /* movntdq xmm, m128 */OVERLOAD_2(0x660fe7,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(movnti,{ /* movnti r32, m32 */OVERLOAD_2(0x0fc3,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(movntpd,{ /* movntpd xmm, m128 */OVERLOAD_2(0x660f2b,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(movntps,{ /* movntps xmm, m128 */OVERLOAD_2(  0x0f2b,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(movntq,{ /* movntq mm, m64 */OVERLOAD_2(0x0fe7,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(movshdup,{ /* movshdup xmm2/m128, xmm1 */OVERLOAD_2(0xf30f16,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(movsldup,{ /* movsldup xmm2/m128, xmm1 */OVERLOAD_2(0xf30f12,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(movq,{
    /* movq mm/m64, mm   */OVERLOAD_2(0x0f6f,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* movq mm, mm/m64   */OVERLOAD_2(0x0f7f,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA)
    /* movq xmm/m64, xmm */OVERLOAD_2(0xf30f7e,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movq xmm, xmm/m64 */OVERLOAD_2(0x660fd6,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movq2dq,{ /* movq2dq mm, xmm */OVERLOAD_2(0xf30fd6,0,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(movs,{
    /* movsb */OVERLOAD_0(0xa4,8, 0,0)
    /* movsw */OVERLOAD_0(0xa5,16,0,DCC_ASMOPC_D16)
    /* movsl */OVERLOAD_0(0xa5,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movsd,{
    /* movsd xmm2/m64, xmm1 */OVERLOAD_2(0xf20f10,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movsd xmm1, xmm2/m64 */OVERLOAD_2(0xf20f11,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movss,{
    /* movss xmm2/m32, xmm1 */OVERLOAD_2(0xf30f10,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movss xmm1, xmm2/m32 */OVERLOAD_2(0xf30f11,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movsx,{
    /* movsx r/m8,  r16 */OVERLOAD_2(0x0fbe,8, 'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* movsx r/m8,  r32 */OVERLOAD_2(0x0fbe,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    /* movsx r/m16, r32 */OVERLOAD_2(0x0fbf,16,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movupd,{
    /* movapd xmm2/m128, xmm1 */OVERLOAD_2(0x660f10,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movapd xmm1, xmm2/m128 */OVERLOAD_2(0x660f11,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movups,{
    /* movups xmm2/m128, xmm1 */OVERLOAD_2(0x0f10,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* movups xmm1, xmm2/m128 */OVERLOAD_2(0x0f11,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(movzx,{
    /* movzx r/m8,  r16 */OVERLOAD_2(0x0fb6,8, 'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* movzx r/m8,  r32 */OVERLOAD_2(0x0fb6,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    /* movzx r/m16, r32 */OVERLOAD_2(0x0fb7,16,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(mul,{
    /* mul r/m8  */OVERLOAD_1(0xf6,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* mul r/m16 */OVERLOAD_1(0xf7,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* mul r/m32 */OVERLOAD_1(0xf7,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(mulpd,{ /* mulpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f59,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(mulps,{ /* mulps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f59,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(mulsd,{ /* mulsd xmm2/m64, xmm1 */OVERLOAD_2(0xf20f59,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(mulss,{ /* mulss xmm2/m64, xmm1 */OVERLOAD_2(0xf30f59,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(mwait,{ /* mwait */OVERLOAD_0(0x0f01c9,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(neg,{
    /* neg r/m8  */OVERLOAD_1(0xf6,8, 3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* neg r/m16 */OVERLOAD_1(0xf7,16,3,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* neg r/m32 */OVERLOAD_1(0xf7,32,3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(nop,{ /* nop */OVERLOAD_0(0x90,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(not,{
    /* not r/m8  */OVERLOAD_1(0xf6,8, 2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* not r/m16 */OVERLOAD_1(0xf7,16,2,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* not r/m32 */OVERLOAD_1(0xf7,32,2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(or,{
    /* or $imm8,  %al   */OVERLOAD_2(0x0c,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* or $imm16, %ax   */OVERLOAD_2(0x0d,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* or $imm32, %eax  */OVERLOAD_2(0x0d,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* or $imm8,  r/m8  */OVERLOAD_2(0x80,8, 1,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* or $imm16, r/m16 */OVERLOAD_2(0x81,16,1,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* or $imm32, r/m32 */OVERLOAD_2(0x81,32,1,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* or $imm8s, r/m16 */OVERLOAD_2(0x83,8, 1,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* or $imm8s, r/m32 */OVERLOAD_2(0x83,8, 1,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* or r8,     r/m8  */OVERLOAD_2(0x08,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* or r16,    r/m16 */OVERLOAD_2(0x09,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* or r32,    r/m32 */OVERLOAD_2(0x09,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* or r/m8,   r8    */OVERLOAD_2(0x0a,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* or r/m16,  r16   */OVERLOAD_2(0x0b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* or r/m32,  r32   */OVERLOAD_2(0x0b,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(orpd,{ /* orpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f56,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(orps,{ /* orps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f56,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(out,{
    /* out $imm8, %al  */OVERLOAD_2(0xe6,8, 0,0,             DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* out $imm8, %ax  */OVERLOAD_2(0xe7,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* out $imm8, %eax */OVERLOAD_2(0xe7,32,0,0,             DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* out %dx, %al    */OVERLOAD_2(0xee,8, 0,0,             DCC_ASMOPT_R_16|DCC_ASMOPT_DX,DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* out %dx, %ax    */OVERLOAD_2(0xef,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_DX,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* out %dx, %eax   */OVERLOAD_2(0xef,32,0,0,             DCC_ASMOPT_R_16|DCC_ASMOPT_DX,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(outs,{
    /* outsb */OVERLOAD_0(0x6e,8, 0,0)
    /* outsw */OVERLOAD_0(0x6f,16,0,DCC_ASMOPC_D16)
    /* outsl */OVERLOAD_0(0x6f,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(packsswb,{
    /* packsswb mm2/m64, mm1    */OVERLOAD_2(  0x0f63,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* packsswb xmm2/m128, xmm1 */OVERLOAD_2(0x660f63,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(packssdw,{
    /* packssdw mm2/m64, mm1    */OVERLOAD_2(  0x6b,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* packssdw xmm2/m128, xmm1 */OVERLOAD_2(0x0f6b,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(packuswb,{
    /* packuswb mm2/m64, mm1    */OVERLOAD_2(  0x0f67,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* packuswb xmm2/m128, xmm1 */OVERLOAD_2(0x660f67,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(padd,{
    /* paddb mm2/m64, mm1    */OVERLOAD_2(  0x0ffc,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddb xmm2/m128, xmm1 */OVERLOAD_2(0x660ffc,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* paddw mm2/m64, mm1    */OVERLOAD_2(  0x0ffd,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddw xmm2/m128, xmm1 */OVERLOAD_2(0x660ffd,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* paddl mm2/m64, mm1    */OVERLOAD_2(  0x0ffe,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddl xmm2/m128, xmm1 */OVERLOAD_2(0x660ffe,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* paddq mm2/m64, mm1    */OVERLOAD_2(  0x0fd4,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddq xmm2/m128, xmm1 */OVERLOAD_2(0x660fd4,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(padds,{
    /* paddsb mm2/m64, mm1    */OVERLOAD_2(  0x0fec,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddsb xmm2/m128, xmm1 */OVERLOAD_2(0x660fec,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* paddsw mm2/m64, mm1    */OVERLOAD_2(  0x0fed,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddsw xmm2/m128, xmm1 */OVERLOAD_2(0x660fed,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(paddus,{
    /* paddusb mm2/m64, mm1    */OVERLOAD_2(  0x0fdc,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddusb xmm2/m128, xmm1 */OVERLOAD_2(0x660fdc,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* paddusw mm2/m64, mm1    */OVERLOAD_2(  0x0fdd,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* paddusw xmm2/m128, xmm1 */OVERLOAD_2(0x660fdd,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pand,{
    /* pand mm2/m64, mm1    */OVERLOAD_2(  0x0fdb,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pand xmm2/m128, xmm1 */OVERLOAD_2(0x660fdb,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pandn,{
    /* pandn mm2/m64, mm1    */OVERLOAD_2(  0x0fdf,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pandn xmm2/m128, xmm1 */OVERLOAD_2(0x660fdf,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pause,{ /* pause */OVERLOAD_0(0xf390,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(pavgb,{
    /* pavgbb mm2/m64, mm1    */OVERLOAD_2(  0x0fe0,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pavgbb xmm2/m128, xmm1 */OVERLOAD_2(0x660fe0,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* pavgbw mm2/m64, mm1    */OVERLOAD_2(  0x0fe3,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pavgbw xmm2/m128, xmm1 */OVERLOAD_2(0x660fe3,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pcmpeq,{
    /* pcmpeqb mm2/m64, mm1    */OVERLOAD_2(  0x0f74,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pcmpeqb xmm2/m128, xmm1 */OVERLOAD_2(0x660f74,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* pcmpeqw mm2/m64, mm1    */OVERLOAD_2(  0x0f75,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pcmpeqw xmm2/m128, xmm1 */OVERLOAD_2(0x660f75,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* pcmpeql mm2/m64, mm1    */OVERLOAD_2(  0x0f76,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pcmpeql xmm2/m128, xmm1 */OVERLOAD_2(0x660f76,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pcmpgt,{
    /* pcmpgtb mm2/m64, mm1    */OVERLOAD_2(  0x0f64,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pcmpgtb xmm2/m128, xmm1 */OVERLOAD_2(0x660f64,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* pcmpgtw mm2/m64, mm1    */OVERLOAD_2(  0x0f65,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pcmpgtw xmm2/m128, xmm1 */OVERLOAD_2(0x660f65,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* pcmpgtl mm2/m64, mm1    */OVERLOAD_2(  0x0f66,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pcmpgtl xmm2/m128, xmm1 */OVERLOAD_2(0x660f66,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pextrw,{
    /* pextrw $imm8, mm, r32  */OVERLOAD_3(  0x0fc5,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX,DCC_ASMOPT_R_32)
    /* pextrw $imm8, xmm, r32 */OVERLOAD_3(0x660fc5,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pinsrw,{
    /* pinsrw $imm8, r/m32, mm  */OVERLOAD_3(  0x0fc4,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pinsrw $imm8, r/m32, xmm */OVERLOAD_3(0x660fc4,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmaddwd,{
    /* pmaddwd mm2/m64, mm1    */OVERLOAD_2(  0x0ff5,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmaddwd xmm2/m128, xmm1 */OVERLOAD_2(0x660ff5,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmaxsw,{
    /* pmaxsw mm2/m64, mm1    */OVERLOAD_2(  0x0fee,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmaxsw xmm2/m128, xmm1 */OVERLOAD_2(0x660fee,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmaxub,{
    /* pmaxub mm2/m64, mm1    */OVERLOAD_2(  0x0fde,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmaxub xmm2/m128, xmm1 */OVERLOAD_2(0x660fde,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pminsw,{
    /* pminsw mm2/m64, mm1    */OVERLOAD_2(  0x0fea,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pminsw xmm2/m128, xmm1 */OVERLOAD_2(0x660fea,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pminub,{
    /* pminub mm2/m64, mm1    */OVERLOAD_2(  0x0fda,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pminub xmm2/m128, xmm1 */OVERLOAD_2(0x660fda,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmovmskb,{
    /* pmovmskb r32, mm1  */OVERLOAD_2(  0x0fd7,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32,DCC_ASMOPT_R_MMX)
    /* pmovmskb r32, xmm1 */OVERLOAD_2(0x660fd7,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_32,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmulhuw,{
    /* pmulhuw mm2/m64, mm1    */OVERLOAD_2(  0x0fe4,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmulhuw xmm2/m128, xmm1 */OVERLOAD_2(0x660fe4,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmulhw,{
    /* pmulhw mm2/m64, mm1    */OVERLOAD_2(  0x0fe5,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmulhw xmm2/m128, xmm1 */OVERLOAD_2(0x660fe5,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmullw,{
    /* pmullw mm2/m64, mm1    */OVERLOAD_2(  0x0fd5,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmullw xmm2/m128, xmm1 */OVERLOAD_2(0x660fd5,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pmuludq,{
    /* pmuludq mm2/m64, mm1    */OVERLOAD_2(  0x0ff4,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pmuludq xmm2/m128, xmm1 */OVERLOAD_2(0x660ff4,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pop,{
    /* pop %r16  */OVERLOAD_1(0x58,  16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16)
    /* pop %r32  */OVERLOAD_1(0x58,  32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32)
    /* pop r/m16 */OVERLOAD_1(0x8f,  16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,/*DCC_ASMOPT_R_16|*/DCC_ASMOPT_EA)
    /* pop r/m32 */OVERLOAD_1(0x8f,  32,0,DCC_ASMOPC_MODRM,               /*DCC_ASMOPT_R_32|*/DCC_ASMOPT_EA)
    /* pop %ds   */OVERLOAD_1(0x1f,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_DS)
    /* pop %es   */OVERLOAD_1(0x07,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_ES)
    /* pop %ss   */OVERLOAD_1(0x17,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_SS)
    /* pop %fs   */OVERLOAD_1(0x0fa1,16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_FS)
    /* pop %gs   */OVERLOAD_1(0x0fa9,16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_GS)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(popa,{ /* popa */OVERLOAD_0(0x61,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(popad,{ /* popad */OVERLOAD_0(0x61,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(popaw,{ /* popaw */OVERLOAD_0(0x61,16,0,DCC_ASMOPC_D16) OVERLOAD_SENTINAL })
DEF_OPCODE(popf,{ /* popf */OVERLOAD_0(0x9d,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(popfd,{ /* popfd */OVERLOAD_0(0x9d,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(popfw,{ /* popfw */OVERLOAD_0(0x9d,16,0,DCC_ASMOPC_D16) OVERLOAD_SENTINAL })
DEF_OPCODE(por,{
    /* por mm2/m64, mm1    */OVERLOAD_2(  0x0feb,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* por xmm2/m128, xmm1 */OVERLOAD_2(0x660feb,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(prefetcht0,{ /* prefetcht0 m8 */OVERLOAD_1(0x0f18,0,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(prefetcht1,{ /* prefetcht1 m8 */OVERLOAD_1(0x0f18,0,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(prefetcht2,{ /* prefetcht2 m8 */OVERLOAD_1(0x0f18,0,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(prefetchnta,{ /* prefetchnta m8 */OVERLOAD_1(0x0f18,0,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(psadbw,{
    /* psadbw mm2/m64, mm1    */OVERLOAD_2(  0x0ff6,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psadbw xmm2/m128, xmm1 */OVERLOAD_2(0x660ff6,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pshuf,{
    /* pshufw $imm8, mm2/m64, mm1    */OVERLOAD_3(  0x0f70,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pshufd $imm8, xmm2/m128, xmm1 */OVERLOAD_3(0x660f70,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) 
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pshufhw,{ /* pshufhw $imm8, xmm2/m128, xmm1 */OVERLOAD_3(0xf30f70,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(pshuflw,{ /* pshuflw $imm8, xmm2/m128, xmm1 */OVERLOAD_3(0xf20f70,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(pslldq,{ /* pslldq $imm8, xmm1 */OVERLOAD_2(0x660f73,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(psll,{
    /* psllw mm2/m64, mm1    */OVERLOAD_2(  0x0ff1,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psllw xmm2/m128, xmm1 */OVERLOAD_2(0x660ff1,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psllw $imm8, mm       */OVERLOAD_2(  0x0f71,16,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psllw $imm8, xmm      */OVERLOAD_2(0x660f71,16,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    /* pslld mm2/m64, mm1    */OVERLOAD_2(  0x0ff2,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pslld xmm2/m128, xmm1 */OVERLOAD_2(0x660ff2,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* pslld $imm8, mm1      */OVERLOAD_2(  0x0f72,32,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* pslld $imm8, xmm1     */OVERLOAD_2(0x660f72,32,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    /* psllq mm2/m64, mm1    */OVERLOAD_2(  0x0ff3,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psllq xmm2/m128, xmm1 */OVERLOAD_2(0x660ff3,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psllq $imm8, mm1      */OVERLOAD_2(  0x0f73,64,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psllq $imm8, xmm1     */OVERLOAD_2(0x660f73,64,6,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(psra,{
    /* psraw mm2/m64, mm1    */OVERLOAD_2(  0x0fe1,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psraw xmm2/m128, xmm1 */OVERLOAD_2(0x660fe1,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psraw $imm8, mm1      */OVERLOAD_2(  0x0f71,16,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psraw $imm8, xmm1     */OVERLOAD_2(0x660f71,16,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    /* psrad mm2/m64, mm1    */OVERLOAD_2(  0x0fe2,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psrad xmm2/m128, xmm1 */OVERLOAD_2(0x660fe2,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psrad $imm8, mm1      */OVERLOAD_2(  0x0f72,32,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psrad $imm8, xmm1     */OVERLOAD_2(0x660f72,32,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(psrldq,{
    /* psrldq $imm8, xmm1 */OVERLOAD_2(0x660f73,128,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(psrl,{
    /* psrlw mm2/m64, mm1    */OVERLOAD_2(  0x0fd1,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psrlw xmm2/m128, xmm1 */OVERLOAD_2(0x660fd1,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psrlw $imm8, mm       */OVERLOAD_2(  0x0f71,16,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psrlw $imm8, xmm      */OVERLOAD_2(0x660f71,16,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    /* psrld mm2/m64, mm1    */OVERLOAD_2(  0x0fd2,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psrld xmm2/m128, xmm1 */OVERLOAD_2(0x660fd2,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psrld $imm8, mm1      */OVERLOAD_2(  0x0f72,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psrld $imm8, xmm1     */OVERLOAD_2(0x660f72,32,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    /* psrlq mm2/m64, mm1    */OVERLOAD_2(  0x0fd3,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psrlq xmm2/m128, xmm1 */OVERLOAD_2(0x660fd3,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psrlq $imm8, mm1      */OVERLOAD_2(  0x0f73,64,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_MMX)
    /* psrlq $imm8, xmm1     */OVERLOAD_2(0x660f73,64,2,DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(psub,{
    /* psubb mm2/m64, mm1    */OVERLOAD_2(  0x0ff8,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubb xmm2/m128, xmm1 */OVERLOAD_2(0x660ff8,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psubw mm2/m64, mm1    */OVERLOAD_2(  0x0ff9,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubw xmm2/m128, xmm1 */OVERLOAD_2(0x660ff9,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psubd mm2/m64, mm1    */OVERLOAD_2(  0x0ffa,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubd xmm2/m128, xmm1 */OVERLOAD_2(0x660ffa,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psubq mm2/m64, mm1    */OVERLOAD_2(  0x0ffb,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubq xmm2/m128, xmm1 */OVERLOAD_2(0x660ffb,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(psubs,{
    /* psubsb mm2/m64, mm1    */OVERLOAD_2(  0x0fe8,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubsb xmm2/m128, xmm1 */OVERLOAD_2(0x660fe8,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psubsw mm2/m64, mm1    */OVERLOAD_2(  0x0fe9,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubsw xmm2/m128, xmm1 */OVERLOAD_2(0x660fe9,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(psubus,{
    /* psubusb mm2/m64, mm1    */OVERLOAD_2(  0x0fd8,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubusb xmm2/m128, xmm1 */OVERLOAD_2(0x660fd8,8, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* psubusw mm2/m64, mm1    */OVERLOAD_2(  0x0fd9,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* psubusw xmm2/m128, xmm1 */OVERLOAD_2(0x660fd9,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(punpckhb,{
    /* punpckhbw mm2/m64, mm1    */OVERLOAD_2(  0x0f68,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* punpckhbw xmm2/m128, xmm1 */OVERLOAD_2(0x660f68,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* punpckhbd mm2/m64, mm1    */OVERLOAD_2(  0x0f69,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* punpckhbd xmm2/m128, xmm1 */OVERLOAD_2(0x660f69,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* punpckhbq mm2/m64, mm1    */OVERLOAD_2(  0x0f6a,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* punpckhbq xmm2/m128, xmm1 */OVERLOAD_2(0x660f6a,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(punpckhqdq,{
    /* punpckhqdq xmm2/m128, xmm1 */OVERLOAD_2(0x660f6d,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(punpcklb,{
    /* punpcklbw mm2/m64, mm1    */OVERLOAD_2(  0x0f60,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* punpcklbw xmm2/m128, xmm1 */OVERLOAD_2(0x660f60,16,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* punpcklbd mm2/m64, mm1    */OVERLOAD_2(  0x0f61,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* punpcklbd xmm2/m128, xmm1 */OVERLOAD_2(0x660f61,32,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    /* punpcklbq mm2/m64, mm1    */OVERLOAD_2(  0x0f62,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* punpcklbq xmm2/m128, xmm1 */OVERLOAD_2(0x660f62,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(punpcklqdq,{
    /* punpcklqdq xmm2/m128, xmm1 */OVERLOAD_2(0x660f6c,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(push,{
    /* push %r16   */OVERLOAD_1(0x50,  16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16)
    /* push %r32   */OVERLOAD_1(0x50,  32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32)
    /* push r/m16  */OVERLOAD_1(0xff,  16,6,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,/*DCC_ASMOPT_R_16|*/DCC_ASMOPT_EA)
    /* push r/m32  */OVERLOAD_1(0xff,  32,6,DCC_ASMOPC_MODRM,               /*DCC_ASMOPT_R_32|*/DCC_ASMOPT_EA)
    /* push $imm8  */OVERLOAD_1(0x6a,  8, 0,0,             DCC_ASMOPT_IMM_8)
    /* push $imm16 */OVERLOAD_1(0x68,  16,0,DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16)
    /* push $imm32 */OVERLOAD_1(0x68,  32,0,0,             DCC_ASMOPT_IMM_32)
    /* push %ds    */OVERLOAD_1(0x1e,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_DS)
    /* push %es    */OVERLOAD_1(0x06,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_ES)
    /* push %fs    */OVERLOAD_1(0x0fa0,16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_FS)
    /* push %gs    */OVERLOAD_1(0x0fa8,16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_GS)
    /* push %cs    */OVERLOAD_1(0x0e,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_CS)
    /* push %ss    */OVERLOAD_1(0x16,  16,0,0,DCC_ASMOPT_R_SEG|DCC_ASMOPT_SS)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(pusha,{ /* pusha */OVERLOAD_0(0x60,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(pushad,{ /* pushad */OVERLOAD_0(0x60,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(pushaw,{ /* pushaw */OVERLOAD_0(0x60,16,0,DCC_ASMOPC_D16) OVERLOAD_SENTINAL })
DEF_OPCODE(pushf,{ /* pushf */OVERLOAD_0(0x9c,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(pushfd,{ /* pushfd */OVERLOAD_0(0x9c,32,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(pushfw,{ /* pushfw */OVERLOAD_0(0x9c,16,0,DCC_ASMOPC_D16) OVERLOAD_SENTINAL })
DEF_OPCODE(pxor,{
    /* pxor mm2/m64, mm1    */OVERLOAD_2(  0x0fef,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_MMX|DCC_ASMOPT_EA,DCC_ASMOPT_R_MMX)
    /* pxor xmm2/m128, xmm1 */OVERLOAD_2(0x660fef,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(rcl,{
    /* rcl r/m8         */OVERLOAD_1(0xd0,8, 2,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcl $1, r/m8     */OVERLOAD_2(0xd0,8, 2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcl %cl, r/m8    */OVERLOAD_2(0xd2,8, 2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcl $imm8, r/m8  */OVERLOAD_2(0xc0,8, 2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcl r/m16        */OVERLOAD_1(0xd1,16,2,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcl $1, r/m16    */OVERLOAD_2(0xd1,16,2,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcl %cl, r/m16   */OVERLOAD_2(0xd3,16,2,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcl $imm8, r/m16 */OVERLOAD_2(0xc1,16,2,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcl r/m32        */OVERLOAD_1(0xd1,32,2,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rcl $1, r/m32    */OVERLOAD_2(0xd1,32,2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rcl %cl, r/m32   */OVERLOAD_2(0xd3,32,2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rcl $imm8, r/m32 */OVERLOAD_2(0xc1,32,2,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(rcr,{
    /* rcr r/m8         */OVERLOAD_1(0xd0,8, 3,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcr $1, r/m8     */OVERLOAD_2(0xd0,8, 3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcr %cl, r/m8    */OVERLOAD_2(0xd2,8, 3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcr $imm8, r/m8  */OVERLOAD_2(0xc0,8, 3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rcr r/m16        */OVERLOAD_1(0xd1,16,3,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcr $1, r/m16    */OVERLOAD_2(0xd1,16,3,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcr %cl, r/m16   */OVERLOAD_2(0xd3,16,3,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcr $imm8, r/m16 */OVERLOAD_2(0xc1,16,3,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rcr r/m32        */OVERLOAD_1(0xd1,32,3,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rcr $1, r/m32    */OVERLOAD_2(0xd1,32,3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rcr %cl, r/m32   */OVERLOAD_2(0xd3,32,3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rcr $imm8, r/m32 */OVERLOAD_2(0xc1,32,3,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(rol,{
    /* rol r/m8         */OVERLOAD_1(0xd0,8, 0,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rol $1, r/m8     */OVERLOAD_2(0xd0,8, 0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rol %cl, r/m8    */OVERLOAD_2(0xd2,8, 0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rol $imm8, r/m8  */OVERLOAD_2(0xc0,8, 0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* rol r/m16        */OVERLOAD_1(0xd1,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rol $1, r/m16    */OVERLOAD_2(0xd1,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rol %cl, r/m16   */OVERLOAD_2(0xd3,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rol $imm8, r/m16 */OVERLOAD_2(0xc1,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* rol r/m32        */OVERLOAD_1(0xd1,32,0,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rol $1, r/m32    */OVERLOAD_2(0xd1,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rol %cl, r/m32   */OVERLOAD_2(0xd3,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* rol $imm8, r/m32 */OVERLOAD_2(0xc1,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(ror,{
    /* ror r/m8         */OVERLOAD_1(0xd0,8, 1,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* ror $1, r/m8     */OVERLOAD_2(0xd0,8, 1,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* ror %cl, r/m8    */OVERLOAD_2(0xd2,8, 1,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* ror $imm8, r/m8  */OVERLOAD_2(0xc0,8, 1,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* ror r/m16        */OVERLOAD_1(0xd1,16,1,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* ror $1, r/m16    */OVERLOAD_2(0xd1,16,1,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* ror %cl, r/m16   */OVERLOAD_2(0xd3,16,1,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* ror $imm8, r/m16 */OVERLOAD_2(0xc1,16,1,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* ror r/m32        */OVERLOAD_1(0xd1,32,1,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* ror $1, r/m32    */OVERLOAD_2(0xd1,32,1,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* ror %cl, r/m32   */OVERLOAD_2(0xd3,32,1,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* ror $imm8, r/m32 */OVERLOAD_2(0xc1,32,1,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(rcpps,{ /* rcpps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f53,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(rcpss,{ /* rcpss xmm2/m32, xmm1  */OVERLOAD_2(0xf30f53,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(rdmsr,{ /* rdmsr */OVERLOAD_0(0x0f32,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(rdpmc,{ /* rdpmc */OVERLOAD_0(0x0f33,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(rdtsc,{ /* rdtsc */OVERLOAD_0(0x0f31,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(rep,  { /* rep   */OVERLOAD_0(0xf3,0,0,DCC_ASMOPC_PREFIX) OVERLOAD_SENTINAL })
DEF_OPCODE(repe, { /* repe  */OVERLOAD_0(0xf3,0,0,DCC_ASMOPC_PREFIX) OVERLOAD_SENTINAL })
DEF_OPCODE(repne,{ /* repne */OVERLOAD_0(0xf2,0,0,DCC_ASMOPC_PREFIX) OVERLOAD_SENTINAL })

DEF_OPCODE(ret,{
    /* ret        */OVERLOAD_0(0xc3,8,0,0)
    /* ret $imm16 */OVERLOAD_1(0xc2,16,0,0,DCC_ASMOPT_IMM_16)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(lret,{
    /* lret        */OVERLOAD_0(0xcb,8,0,0)
    /* lret $imm16 */OVERLOAD_1(0xca,16,0,0,DCC_ASMOPT_IMM_16)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(rsm,{ /* rsm */OVERLOAD_0(0x0faa,0,0,DCC_ASMOPC_PREFIX) OVERLOAD_SENTINAL })
DEF_OPCODE(rsqrtps,{ /* rsqrtps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f52,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(rsqrtss,{ /* rsqrtss xmm2/m128, xmm1 */OVERLOAD_2(0xf30f52,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(sahf,{ /* sahf */OVERLOAD_0(0x9e,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(sal,{
    /* sal r/m8         */OVERLOAD_1(0xd0,8, 4,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sal $1, r/m8     */OVERLOAD_2(0xd0,8, 4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sal %cl, r/m8    */OVERLOAD_2(0xd2,8, 4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sal $imm8, r/m8  */OVERLOAD_2(0xc0,8, 4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sal r/m16        */OVERLOAD_1(0xd1,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sal $1, r/m16    */OVERLOAD_2(0xd1,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sal %cl, r/m16   */OVERLOAD_2(0xd3,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sal $imm8, r/m16 */OVERLOAD_2(0xc1,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sal r/m32        */OVERLOAD_1(0xd1,32,4,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sal $1, r/m32    */OVERLOAD_2(0xd1,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sal %cl, r/m32   */OVERLOAD_2(0xd3,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sal $imm8, r/m32 */OVERLOAD_2(0xc1,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(sar,{
    /* sar r/m8         */OVERLOAD_1(0xd0,8, 7,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sar $1, r/m8     */OVERLOAD_2(0xd0,8, 7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sar %cl, r/m8    */OVERLOAD_2(0xd2,8, 7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sar $imm8, r/m8  */OVERLOAD_2(0xc0,8, 7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sar r/m16        */OVERLOAD_1(0xd1,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sar $1, r/m16    */OVERLOAD_2(0xd1,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sar %cl, r/m16   */OVERLOAD_2(0xd3,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sar $imm8, r/m16 */OVERLOAD_2(0xc1,16,7,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sar r/m32        */OVERLOAD_1(0xd1,32,7,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sar $1, r/m32    */OVERLOAD_2(0xd1,32,7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sar %cl, r/m32   */OVERLOAD_2(0xd3,32,7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sar $imm8, r/m32 */OVERLOAD_2(0xc1,32,7,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(shl,{
    /* shl r/m8         */OVERLOAD_1(0xd0,8, 4,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shl $1, r/m8     */OVERLOAD_2(0xd0,8, 4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shl %cl, r/m8    */OVERLOAD_2(0xd2,8, 4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shl $imm8, r/m8  */OVERLOAD_2(0xc0,8, 4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shl r/m16        */OVERLOAD_1(0xd1,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shl $1, r/m16    */OVERLOAD_2(0xd1,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shl %cl, r/m16   */OVERLOAD_2(0xd3,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shl $imm8, r/m16 */OVERLOAD_2(0xc1,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shl r/m32        */OVERLOAD_1(0xd1,32,4,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shl $1, r/m32    */OVERLOAD_2(0xd1,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shl %cl, r/m32   */OVERLOAD_2(0xd3,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shl $imm8, r/m32 */OVERLOAD_2(0xc1,32,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(shr,{
    /* shr r/m8         */OVERLOAD_1(0xd0,8, 5,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shr $1, r/m8     */OVERLOAD_2(0xd0,8, 5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shr %cl, r/m8    */OVERLOAD_2(0xd2,8, 5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shr $imm8, r/m8  */OVERLOAD_2(0xc0,8, 5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* shr r/m16        */OVERLOAD_1(0xd1,16,5,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,                                DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shr $1, r/m16    */OVERLOAD_2(0xd1,16,5,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shr %cl, r/m16   */OVERLOAD_2(0xd3,16,5,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shr $imm8, r/m16 */OVERLOAD_2(0xc1,16,5,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shr r/m32        */OVERLOAD_1(0xd1,32,5,DCC_ASMOPC_MODRM,                                               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shr $1, r/m32    */OVERLOAD_2(0xd1,32,5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8|DCC_ASMOPT_ONE,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shr %cl, r/m32   */OVERLOAD_2(0xd3,32,5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,   DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shr $imm8, r/m32 */OVERLOAD_2(0xc1,32,5,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(sbb,{
    /* sbb $imm8,  %al   */OVERLOAD_2(0x1c,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* sbb $imm16, %ax   */OVERLOAD_2(0x1d,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* sbb $imm32, %eax  */OVERLOAD_2(0x1d,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* sbb $imm8,  r/m8  */OVERLOAD_2(0x80,8, 3,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sbb $imm16, r/m16 */OVERLOAD_2(0x81,16,3,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sbb $imm32, r/m32 */OVERLOAD_2(0x81,32,3,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sbb $imm8s, r/m16 */OVERLOAD_2(0x83,8, 3,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sbb $imm8s, r/m32 */OVERLOAD_2(0x83,8, 3,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sbb r8,     r/m8  */OVERLOAD_2(0x18,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sbb r16,    r/m16 */OVERLOAD_2(0x19,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sbb r32,    r/m32 */OVERLOAD_2(0x19,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sbb r/m8,   r8    */OVERLOAD_2(0x1a,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* sbb r/m16,  r16   */OVERLOAD_2(0x1b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* sbb r/m32,  r32   */OVERLOAD_2(0x1b,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(scas,{
    /* scasb */OVERLOAD_0(0xae,8, 0,0)
    /* scasw */OVERLOAD_0(0xaf,16,0,DCC_ASMOPC_D16)
    /* scasl */OVERLOAD_0(0xaf,32,0,0)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(seta,  { /* seta r/m8   */OVERLOAD_1(0x0f97,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if above (CF=0 and ZF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setae, { /* setae r/m8  */OVERLOAD_1(0x0f93,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if above or equal (CF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setb,  { /* setb r/m8   */OVERLOAD_1(0x0f92,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if below (CF=1). */ OVERLOAD_SENTINAL }) 
DEF_OPCODE(setbe, { /* setbe r/m8  */OVERLOAD_1(0x0f96,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if below or equal (CF=1 or ZF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setc,  { /* setc r/m8   */OVERLOAD_1(0x0f92,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if carry (CF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(sete,  { /* sete r/m8   */OVERLOAD_1(0x0f94,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if equal (ZF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setg,  { /* setg r/m8   */OVERLOAD_1(0x0f9f,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if greater (ZF=0 and SF=OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setge, { /* setge r/m8  */OVERLOAD_1(0x0f9d,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if greater or equal (SF=OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setl,  { /* setl r/m8   */OVERLOAD_1(0x0f9c,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if less (SF<>OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setle, { /* setle r/m8  */OVERLOAD_1(0x0f9e,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if less or equal (ZF=1 or SF<>OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setna, { /* setna r/m8  */OVERLOAD_1(0x0f96,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not above (CF=1 or ZF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnae,{ /* setnae r/m8 */OVERLOAD_1(0x0f92,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not above or equal (CF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnb, { /* setnb r/m8  */OVERLOAD_1(0x0f93,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not below (CF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnbe,{ /* setnbe r/m8 */OVERLOAD_1(0x0f97,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not below or equal (CF=0 and ZF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnc, { /* setnc r/m8  */OVERLOAD_1(0x0f93,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not carry (CF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setne, { /* setne r/m8  */OVERLOAD_1(0x0f95,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not equal (ZF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setng, { /* setng r/m8  */OVERLOAD_1(0x0f9e,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not greater (ZF=1 or SF<>OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnge,{ /* setnge r/m8 */OVERLOAD_1(0x0f9c,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not greater or equal (SF<>OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnl, { /* setnl r/m8  */OVERLOAD_1(0x0f9d,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not less (SF=OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnle,{ /* setnle r/m8 */OVERLOAD_1(0x0f9f,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not less or equal (ZF=0 and SF=OF). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setno, { /* setno r/m8  */OVERLOAD_1(0x0f91,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not overflow (OF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnp, { /* setnp r/m8  */OVERLOAD_1(0x0f9b,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not parity (PF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setns, { /* setns r/m8  */OVERLOAD_1(0x0f99,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not sign (SF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setnz, { /* setnz r/m8  */OVERLOAD_1(0x0f95,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if not zero (ZF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(seto,  { /* seto r/m8   */OVERLOAD_1(0x0f90,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if overflow (OF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setp,  { /* setp r/m8   */OVERLOAD_1(0x0f9a,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if parity (PF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setpe, { /* setpe r/m8  */OVERLOAD_1(0x0f9a,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if parity even (PF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setpo, { /* setpo r/m8  */OVERLOAD_1(0x0f9b,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if parity odd (PF=0). */ OVERLOAD_SENTINAL })
DEF_OPCODE(sets,  { /* sets r/m8   */OVERLOAD_1(0x0f98,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if sign (SF=1). */ OVERLOAD_SENTINAL })
DEF_OPCODE(setz,  { /* setz r/m8   */OVERLOAD_1(0x0f94,8,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_8|DCC_ASMOPT_EA) /* Set byte if zero (ZF=1). */ OVERLOAD_SENTINAL })

DEF_OPCODE(sfence,{ /* sfence */OVERLOAD_0(0x0fae,0,7,DCC_ASMOPC_MODRM) OVERLOAD_SENTINAL })
DEF_OPCODE(sgdt,{ /* sgdt m48 */OVERLOAD_1(0x0f01,0,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })

DEF_OPCODE(shld,{
    /* shld $imm8, %r16, r/m16 */OVERLOAD_3(0x0fa4,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,            DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shld %cl, %r16, r/m16   */OVERLOAD_3(0x0fa5,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shld $imm8, %r32, r/m32 */OVERLOAD_3(0x0fa4,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,            DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shld %cl, %r32, r/m32   */OVERLOAD_3(0x0fa5,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(shrd,{
    /* shrd $imm8, %r16, r/m16 */OVERLOAD_3(0x0fac,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_8,            DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shrd %cl, %r16, r/m16   */OVERLOAD_3(0x0fad,16,0,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_8|DCC_ASMOPT_CL,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* shrd $imm8, %r32, r/m32 */OVERLOAD_3(0x0fac,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8,            DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* shrd %cl, %r32, r/m32   */OVERLOAD_3(0x0fad,32,0,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_CL,DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(shufpd,{ /* shufpd $imm8, xmm2/m128, xmm1 */OVERLOAD_3(0x660fc6,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(shufps,{ /* shufps $imm8, xmm2/m128, xmm1 */OVERLOAD_3(  0x0fc6,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_IMM_8,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

DEF_OPCODE(sidt,{ /* sidt m48 */OVERLOAD_1(0x0f01,0,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(sldt,{ /* sldt r/m16 */OVERLOAD_1(0x0f00,16,0,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })

DEF_OPCODE(smsw,{
    /* smsw r/m16   */OVERLOAD_1(0x0f01,16,4,DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* smsw r32/m16 */OVERLOAD_1(0x0f01,16,4,DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(sqrtpd,{ /* sqrtpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f51,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(sqrtps,{ /* sqrtps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f51,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(sqrtsd,{ /* sqrtsd xmm2/m64, xmm1  */OVERLOAD_2(0xf20f51,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(sqrtss,{ /* sqrtss xmm2/m32, xmm1  */OVERLOAD_2(0xf30f51,32, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

DEF_OPCODE(stc,{ /* stc */OVERLOAD_0(0xf9,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(std,{ /* std */OVERLOAD_0(0xfd,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(sti,{ /* sti */OVERLOAD_0(0xfb,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(stmxcsr,{ /* stmxcsr m32 */OVERLOAD_1(0x0fae,32,3,DCC_ASMOPC_MODRM,DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(stos,{
    /* stosb */OVERLOAD_0(0xaa,8, 0,0)
    /* stosw */OVERLOAD_0(0xab,16,0,DCC_ASMOPC_D16)
    /* stosl */OVERLOAD_0(0xab,32,0,0)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(str,{ /* str r/m16 */OVERLOAD_1(0x0f00,16,1,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })

DEF_OPCODE(sub,{
    /* sub $imm8,  %al   */OVERLOAD_2(0x2c,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* sub $imm16, %ax   */OVERLOAD_2(0x2d,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* sub $imm32, %eax  */OVERLOAD_2(0x2d,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* sub $imm8,  r/m8  */OVERLOAD_2(0x80,8, 5,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sub $imm16, r/m16 */OVERLOAD_2(0x81,16,5,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sub $imm32, r/m32 */OVERLOAD_2(0x81,32,5,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sub $imm8s, r/m16 */OVERLOAD_2(0x83,8, 5,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sub $imm8s, r/m32 */OVERLOAD_2(0x83,8, 5,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sub r8,     r/m8  */OVERLOAD_2(0x28,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* sub r16,    r/m16 */OVERLOAD_2(0x29,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* sub r32,    r/m32 */OVERLOAD_2(0x29,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* sub r/m8,   r8    */OVERLOAD_2(0x2a,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* sub r/m16,  r16   */OVERLOAD_2(0x2b,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* sub r/m32,  r32   */OVERLOAD_2(0x2b,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(subpd,{ /* subpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f5c,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(subps,{ /* subps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f5c,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(subsd,{ /* subsd xmm2/m64, xmm1  */OVERLOAD_2(0xf20f5c,64, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(subss,{ /* subss xmm2/m32, xmm1  */OVERLOAD_2(0xf30f5c,32, 'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

DEF_OPCODE(sysenter,{ /* sysenter */OVERLOAD_0(0x0f34,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(sysexit, { /* sysexit  */OVERLOAD_0(0x0f35,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(test,{
    /* test $imm8, %al    */OVERLOAD_2(0xa8,8, 0,0,             DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EAX)
    /* test $imm16, %ax   */OVERLOAD_2(0xa9,16,0,DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* test $imm32, %eax  */OVERLOAD_2(0xa9,32,0,0,             DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* test $imm8, r/m8   */OVERLOAD_2(0xf6,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* test $imm16, r/m16 */OVERLOAD_2(0xf7,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* test $imm32, r/m32 */OVERLOAD_2(0xf7,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* test r8, r/m8      */OVERLOAD_2(0x84,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* test r16, r/m16    */OVERLOAD_2(0x85,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* test r32, r/m32    */OVERLOAD_2(0x85,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})

DEF_OPCODE(ucomisd,{ /* ucomisd xmm2/m64, xmm1 */OVERLOAD_2(0x660f2e,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(ucomiss,{ /* ucomiss xmm2/m64, xmm1 */OVERLOAD_2(  0x0f2e,64,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

DEF_OPCODE(ud2,{ /* ud2 */OVERLOAD_0(0x0f0b,0,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(unpckhpd,{ /* unpckhpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f15,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(unpckhps,{ /* unpckhps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f15,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(unpcklpd,{ /* unpcklpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f14,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(unpcklps,{ /* unpcklps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f14,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

DEF_OPCODE(verr,{ /* verr r/m16 */OVERLOAD_1(0x0f00,16,4,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })
DEF_OPCODE(verw,{ /* verw r/m16 */OVERLOAD_1(0x0f00,16,5,DCC_ASMOPC_MODRM,DCC_ASMOPT_R_16|DCC_ASMOPT_EA) OVERLOAD_SENTINAL })

DEF_OPCODE(wait,{ /* wait */OVERLOAD_0(0x9b,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(fwait,{ /* fwait */OVERLOAD_0(0x9b,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(wbinvd,{ /* wbinvd */OVERLOAD_0(0x0f09,0,0,0) OVERLOAD_SENTINAL })
DEF_OPCODE(wrmsr,{ /* wrmsr */OVERLOAD_0(0x0f30,0,0,0) OVERLOAD_SENTINAL })
#if DCC_TARGET_IA32(486)
DEF_OPCODE(xadd,{
    /* xadd %r8, r/m8   */OVERLOAD_2(0x0fc0,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* xadd %r16, r/m16 */OVERLOAD_2(0x0fc1,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* xadd %r32, r/m32 */OVERLOAD_2(0x0fc1,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    OVERLOAD_SENTINAL
})
#endif
DEF_OPCODE(xchg,{
    /* xchg %r16, %ax   */OVERLOAD_2(0x90,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* xchg %ax, %r16   */OVERLOAD_2(0x90,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* xchg %r32, %eax  */OVERLOAD_2(0x90,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* xchg %eax, %r32  */OVERLOAD_2(0x90,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* xchg %r8, r/m8   */OVERLOAD_2(0x86,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* xchg r/m8, %r8   */OVERLOAD_2(0x86,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_8)
    /* xchg %r16, r/m16 */OVERLOAD_2(0x87,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* xchg r/m16, %r16 */OVERLOAD_2(0x87,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* xchg %r32, r/m32 */OVERLOAD_2(0x87,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* xchg r/m32, %r32 */OVERLOAD_2(0x87,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
#if 1 /* Screw you 'xchg' (Y u so unnatural? - Why not be dis?) */
DEF_OPCODE(xch,{
    /* xch %r16, %ax   */OVERLOAD_2(0x90,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* xch %ax, %r16   */OVERLOAD_2(0x90,16,0,DCC_ASMOPC_REG|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* xch %r32, %eax  */OVERLOAD_2(0x90,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* xch %eax, %r32  */OVERLOAD_2(0x90,32,0,DCC_ASMOPC_REG,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* xch %r8, r/m8   */OVERLOAD_2(0x86,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8,DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* xch r/m8, %r8   */OVERLOAD_2(0x86,8, 'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_8|DCC_ASMOPT_EA,DCC_ASMOPT_R_8)
    /* xch %r16, r/m16 */OVERLOAD_2(0x87,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* xch r/m16, %r16 */OVERLOAD_2(0x87,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* xch %r32, r/m32 */OVERLOAD_2(0x87,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* xch r/m32, %r32 */OVERLOAD_2(0x87,32,'r',DCC_ASMOPC_MODRM,               DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
#endif

DEF_OPCODE(xlat,{ /* xlat */OVERLOAD_0(0xd7,8,0,0) OVERLOAD_SENTINAL })

DEF_OPCODE(xor,{
    /* xor $imm8,  %al   */OVERLOAD_2(0x34,8, 0,  0,                               DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8| DCC_ASMOPT_EAX)
    /* xor $imm16, %ax   */OVERLOAD_2(0x35,16,0,  DCC_ASMOPC_D16,                  DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EAX)
    /* xor $imm32, %eax  */OVERLOAD_2(0x35,32,0,  0,                               DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EAX)
    /* xor $imm8,  r/m8  */OVERLOAD_2(0x80,8, 6,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8, DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* xor $imm16, r/m16 */OVERLOAD_2(0x81,16,6,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_16,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* xor $imm32, r/m32 */OVERLOAD_2(0x81,32,6,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_32,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* xor $imm8s, r/m16 */OVERLOAD_2(0x83,8, 6,  DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* xor $imm8s, r/m32 */OVERLOAD_2(0x83,8, 6,  DCC_ASMOPC_MODRM,                DCC_ASMOPT_IMM_8S,DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* xor r8,     r/m8  */OVERLOAD_2(0x30,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8,   DCC_ASMOPT_R_8|DCC_ASMOPT_EA)
    /* xor r16,    r/m16 */OVERLOAD_2(0x31,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16,  DCC_ASMOPT_R_16|DCC_ASMOPT_EA)
    /* xor r32,    r/m32 */OVERLOAD_2(0x31,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32,  DCC_ASMOPT_R_32|DCC_ASMOPT_EA)
    /* xor r/m8,   r8    */OVERLOAD_2(0x32,8, 'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_8|DCC_ASMOPT_EA, DCC_ASMOPT_R_8)
    /* xor r/m16,  r16   */OVERLOAD_2(0x33,16,'r',DCC_ASMOPC_MODRM|DCC_ASMOPC_D16, DCC_ASMOPT_R_16|DCC_ASMOPT_EA,DCC_ASMOPT_R_16)
    /* xor r/m32,  r32   */OVERLOAD_2(0x33,32,'r',DCC_ASMOPC_MODRM,                DCC_ASMOPT_R_32|DCC_ASMOPT_EA,DCC_ASMOPT_R_32)
    OVERLOAD_SENTINAL
})
DEF_OPCODE(xorpd,{ /* xorpd xmm2/m128, xmm1 */OVERLOAD_2(0x660f57,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })
DEF_OPCODE(xorps,{ /* xorps xmm2/m128, xmm1 */OVERLOAD_2(  0x0f57,128,'r',DCC_ASMOPC_MODRM,DCC_ASMOPT_R_SSE|DCC_ASMOPT_EA,DCC_ASMOPT_R_SSE) OVERLOAD_SENTINAL })

