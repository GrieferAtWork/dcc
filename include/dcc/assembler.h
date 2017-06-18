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
#ifndef GUARD_DCC_ASSEMBLER_H
#define GUARD_DCC_ASSEMBLER_H 1

#include "common.h"
#include "lexer.h"

#include <stddef.h>
#include <stdint.h>
#include <dcc/target.h>

DCC_DECL_BEGIN

struct DCCSymAddr;

#if DCC_TARGET_ASMI(I_X86)
/* DCC_ASMOP_R_8: 8-bit registers. */
#define DCC_ASMREG_AL     (0 << 0)
#define DCC_ASMREG_CL     (1 << 0)
#define DCC_ASMREG_DL     (2 << 0)
#define DCC_ASMREG_BL     (3 << 0)
#define DCC_ASMREG_AH  (4|(0 << 0))
#define DCC_ASMREG_CH  (4|(1 << 0))
#define DCC_ASMREG_DH  (4|(2 << 0))
#define DCC_ASMREG_BH  (4|(3 << 0))

/* Returns true for SI/DI pointer registers. */
#define DCC_ASMREG_ISPTR(x)  ((x) >= 6)

/* Returns true for SP/BP pointer registers. */
#define DCC_ASMREG_ISSPTR(x) ((x) == 4 || (x) == 5)

/* DCC_ASMOP_R_16: 16-bit registers. */
#define DCC_ASMREG_AX   0 /*< Accumulator. */
#define DCC_ASMREG_CX   1 /*< Counter register. */
#define DCC_ASMREG_DX   2 /*< General purpose d-register. */
#define DCC_ASMREG_BX   3 /*< General purpose b-register. */
#define DCC_ASMREG_SP   4 /*< Stack pointer. */
#define DCC_ASMREG_BP   5 /*< Stack base pointer. */
#define DCC_ASMREG_SI   6 /*< Source pointer. */
#define DCC_ASMREG_DI   7 /*< Destination pointer. */

/* DCC_ASMOP_R_32: 32-bit registers. */
#define DCC_ASMREG_EAX  0 /*< Accumulator. */
#define DCC_ASMREG_ECX  1 /*< Counter register. */
#define DCC_ASMREG_EDX  2 /*< General purpose d-register. */
#define DCC_ASMREG_EBX  3 /*< General purpose b-register. */
#define DCC_ASMREG_ESP  4 /*< Stack pointer. */
#define DCC_ASMREG_EBP  5 /*< Stack base pointer. */
#define DCC_ASMREG_ESI  6 /*< Source pointer. */
#define DCC_ASMREG_EDI  7 /*< Destination pointer. */

#if DCC_TARGET_HASF(F_X86_64)
/* DCC_ASMOP_R_64: 64-bit registers. */
#define DCC_ASMREG_RAX  0 /*< Accumulator. */
#define DCC_ASMREG_RCX  1 /*< Counter register. */
#define DCC_ASMREG_RDX  2 /*< General purpose d-register. */
#define DCC_ASMREG_RBX  3 /*< General purpose b-register. */
#define DCC_ASMREG_RSP  4 /*< Stack pointer. */
#define DCC_ASMREG_RBP  5 /*< Stack base pointer. */
#define DCC_ASMREG_RSI  6 /*< Source pointer. */
#define DCC_ASMREG_RDI  7 /*< Destination pointer. */
#endif /* F_X86_64 */

#if DCC_TARGET_ASMF(F_MMX)
/* DCC_ASMOP_R_MMX: MMX registers. */
#define DCC_ASMREG_MM0  0
#define DCC_ASMREG_MM1  1
#define DCC_ASMREG_MM2  2
#define DCC_ASMREG_MM3  3
#define DCC_ASMREG_MM4  4
#define DCC_ASMREG_MM5  5
#define DCC_ASMREG_MM6  6
#define DCC_ASMREG_MM7  7
#endif /* F_MMX */

#if DCC_TARGET_ASMF(F_SSE)
/* DCC_ASMOP_R_SSE: SSE registers. */
#define DCC_ASMREG_XMM0 0
#define DCC_ASMREG_XMM1 1
#define DCC_ASMREG_XMM2 2
#define DCC_ASMREG_XMM3 3
#define DCC_ASMREG_XMM4 4
#define DCC_ASMREG_XMM5 5
#define DCC_ASMREG_XMM6 6
#define DCC_ASMREG_XMM7 7
#endif /* F_SSE */

/* DCC_ASMOP_R_CR: Control registers. */
#define DCC_ASMREG_CR0  0
#define DCC_ASMREG_CR1  1
#define DCC_ASMREG_CR2  2
#define DCC_ASMREG_CR3  3
#define DCC_ASMREG_CR4  4
#define DCC_ASMREG_CR5  5
#define DCC_ASMREG_CR6  6
#define DCC_ASMREG_CR7  7

/* DCC_ASMOP_R_TR: Test registers. */
#define DCC_ASMREG_TR0  0
#define DCC_ASMREG_TR1  1
#define DCC_ASMREG_TR2  2
#define DCC_ASMREG_TR3  3
#define DCC_ASMREG_TR4  4
#define DCC_ASMREG_TR5  5
#define DCC_ASMREG_TR6  6
#define DCC_ASMREG_TR7  7

/* DCC_ASMOP_R_DB: Debug registers. */
#define DCC_ASMREG_DB0  0
#define DCC_ASMREG_DB1  1
#define DCC_ASMREG_DB2  2
#define DCC_ASMREG_DB3  3
#define DCC_ASMREG_DB4  4
#define DCC_ASMREG_DB5  5
#define DCC_ASMREG_DB6  6
#define DCC_ASMREG_DB7  7

/* DCC_ASMOP_R_DR: Debug registers. */
#define DCC_ASMREG_DR0  0
#define DCC_ASMREG_DR1  1
#define DCC_ASMREG_DR2  2
#define DCC_ASMREG_DR3  3
#define DCC_ASMREG_DR4  4
#define DCC_ASMREG_DR5  5
#define DCC_ASMREG_DR6  6
#define DCC_ASMREG_DR7  7

/* DCC_ASMOP_R_SEG: Segment registers. */
#define DCC_ASMREG_ES   0
#define DCC_ASMREG_CS   1
#define DCC_ASMREG_SS   2
#define DCC_ASMREG_DS   3
#define DCC_ASMREG_FS   4
#define DCC_ASMREG_GS   5

/* DCC_ASMOP_R_ST: Floating point st(i) registers. */
#define DCC_ASMREG_ST(i) ((i)&DCC_BITS(3))
#else /* I_X86 */
/* ... */
#endif /* !I_X86 */


/* Opcode encoding flags. */
#if DCC_TARGET_ASMI(I_X86)
#define DCC_ASMOPC_REG         0x0001 /*< May only be used if at least one operand is a register:
                                       *  Add the index of the first operand's register to the opcode. */
#define DCC_ASMOPC_MODRM       0x0002 /*< "r/m(8|16|32)":  Use mod/rm opcode encoding. */
#define DCC_ASMOPC_ARGCSHIFT   2      /*< Shift for argument count. */
#define DCC_ASMOPC_ARGCMASK    0x000C /*< Mask for argument count. */
#define DCC_ASMOPC_ARGC(c)  (((c)&DCC_BITS(2)) << DCC_ASMOPC_ARGCSHIFT)
#define DCC_ASMOPC_GROUP_REG   0x0010 /*< Use the first non-mod/rm register as  mod/rm group. */
#define DCC_ASMOPC_GROUPSHIFT  5      /*< Shift for default mod/rm register. */
#define DCC_ASMOPC_GROUPMASK   0x00E0 /*< Mask for default mod/rm register. */
#define DCC_ASMOPC_GROUP(i) \
 ((i) == 'r' ? DCC_ASMOPC_GROUP_REG : \
 ((i)&DCC_BITS(3)) << DCC_ASMOPC_GROUPSHIFT)
#define DCC_ASMOPC_D16         0x0100 /*< Generate data16 prefix. */
#define DCC_ASMOPC_DISP        0x0200 /*< Immediate arguments are generated as displacements to the next instruction. */
#define DCC_ASMOPC_PREFIX      0x0400 /*< The instruction can be written in-line with the next one (NOTE: Only checked for the first overload). */
#define DCC_ASMOPC_SIZE(bits) \
 ((bits) == 8 ? 0x2000 : \
  (bits) == 16 ? 0x4000 : \
  (bits) == 32 ? 0x6000 : \
  (bits) == 64 ? 0x8000 : \
  (bits) == 128 ? 0xa000 : \
  (bits) == 256 ? 0xc000 : \
  (bits) == 512 ? 0xe000 : 0)
#define DCC_ASMOPC_SIZESHIFT   13     /*< Shift for explicit operand size. */
#define DCC_ASMOPC_SIZEMASK    0xe000 /*< Mask for explicit operand size. */
#else /* I_X86 */
/* ... */
#endif /* !I_X86 */

#if DCC_TARGET_ASMI(I_X86)
/* Operand types (for use in 'asm.inl' operand types) */
/* NOTE: These registers must be ordered to those in 'keywords.def'. */
#define DCC_ASMOPT_R_8    0x0000 /*< 8-bit register. */
#define DCC_ASMOPT_R_16   0x0001 /*< 16-bit register. */
#define DCC_ASMOPT_R_32   0x0002 /*< 32-bit register. */
#if DCC_TARGET_HASF(F_X86_64)
#define DCC_ASMOPT_R_64   0x0003 /*< 64-bit register. */
#define DCC_PRIVATE_ASMOPT_R_NEXT1 0x0004 /*< MMX register. */
#else /* F_X86_64 */
#define DCC_PRIVATE_ASMOPT_R_NEXT1 0x0003 /*< MMX register. */
#endif /* !F_X86_64 */
#if DCC_TARGET_ASMF(F_MMX)
#define DCC_ASMOPT_R_MMX  DCC_PRIVATE_ASMOPT_R_NEXT1 /*< MMX register. */
#define DCC_PRIVATE_ASMOPT_R_NEXT2 DCC_PRIVATE_ASMOPT_R_NEXT1+1
#else /* F_MMX */
#define DCC_PRIVATE_ASMOPT_R_NEXT2 DCC_PRIVATE_ASMOPT_R_NEXT1
#endif /* !F_MMX */
#if DCC_TARGET_ASMF(F_SSE)
#define DCC_ASMOPT_R_SSE  (DCC_PRIVATE_ASMOPT_R_NEXT2+1) /*< SSE register. */
#define DCC_PRIVATE_ASMOPT_R_NEXT3 DCC_PRIVATE_ASMOPT_R_NEXT2+1
#else /* F_SSE */
#define DCC_PRIVATE_ASMOPT_R_NEXT3 DCC_PRIVATE_ASMOPT_R_NEXT2
#endif /* !F_SSE */
#define DCC_ASMOPT_R_CR   (DCC_PRIVATE_ASMOPT_R_NEXT3+0) /*< CR* register. */
#define DCC_ASMOPT_R_TR   (DCC_PRIVATE_ASMOPT_R_NEXT3+1) /*< TR* register. */
#define DCC_ASMOPT_R_DB   (DCC_PRIVATE_ASMOPT_R_NEXT3+2) /*< DB* register. */
#define DCC_ASMOPT_R_DR   (DCC_PRIVATE_ASMOPT_R_NEXT3+3) /*< DR* register. */

#define DCC_ASMOPT_R_SEG  0x000a /*< Segment register. */
#define DCC_ASMOPT_R_ST   0x000b /*< st(i) register. */
#define DCC_ASMOPT_IMM_8S 0x000c /*< 8-bit, signed immediate value, or 7-bit unsigned value. */
#define DCC_ASMOPT_IMM_8  0x000d /*< 8-bit, unsigned immediate value. */
#define DCC_ASMOPT_IMM_16 0x000e /*< 16-bit, unsigned immediate value. */
#define DCC_ASMOPT_IMM_32 0x000f /*< 32-bit, unsigned immediate value. */
#if DCC_TARGET_HASF(F_X86_64)
#define DCC_ASMOPT_IMM_64 0x0010 /*< 64-bit, unsigned immediate value. */
#endif /* F_X86_64 */
#define DCC_ASMOPT_ADDR   0x0011 /*< An absolute memory location. */
/* Special register flags. */
#define DCC_ASMOPT_EAX    0x0100 /*< Any accumulator register ('%al', '%ax', '%eax' or '%rax'). */
#define DCC_ASMOPT_ST0    0x0200 /*< The '%st(0)' register. */
#define DCC_ASMOPT_CL     0x0300 /*< The '%cl' register. */
#define DCC_ASMOPT_DX     0x0400 /*< The '%dx' register. */
#define DCC_ASMOPT_DS     0x0500 /*< The '%ds' segment register. */
#define DCC_ASMOPT_ES     0x0600 /*< The '%es' segment register. */
#define DCC_ASMOPT_FS     0x0700 /*< The '%fs' segment register. */
#define DCC_ASMOPT_GS     0x0800 /*< The '%gs' segment register. */
#define DCC_ASMOPT_CS     0x0900 /*< The '%cs' segment register. */
#define DCC_ASMOPT_SS     0x0a00 /*< The '%ss' segment register. */
#define DCC_ASMOPT_ONE    0x0b00 /*< Any immediate value equal to 1 (NOTE: Will prevent the value from being emit). */
#define DCC_ASMOPT_REGSPEC 0x0f00 /*< Mask for register-specific flags. */
/* Optional, additional flags. */
#define DCC_ASMOPT_IND    0x1000 /*< The operand is an indirection (e.g.: '*%eax') */
#define DCC_ASMOPT_EA     0x2000 /*< A memory location potentially offset from a register. */
#define DCC_ASMOPT_A8     0x0000 /*< Used with 'DCC_ASMOPT_ADDR': 8-bit address/disp. */
#define DCC_ASMOPT_A16    0x4000 /*< Used with 'DCC_ASMOPT_ADDR': 16-bit address/disp. */
#define DCC_ASMOPT_A32    0x8000 /*< Used with 'DCC_ASMOPT_ADDR': 32-bit address/disp. */
#if DCC_TARGET_HASF(F_X86_64)
#define DCC_ASMOPT_A64    0xc000 /*< Used with 'DCC_ASMOPT_ADDR': 64-bit address/disp. */
#endif /* F_X86_64 */
#define DCC_ASMOPT_ASHIFT 14
#define DCC_ASMOPT_AMASK  0xc000
#define DCC_ASMOPT_ASIZ(t) (1 << (((t)&DCC_ASMOPT_AMASK) >> DCC_ASMOPT_ASHIFT))


#define DCC_ASMOP_R_8    (1 << DCC_ASMOPT_R_8)    /*< 8-bit general-purpose register. */
#define DCC_ASMOP_R_16   (1 << DCC_ASMOPT_R_16)   /*< 16-bit general-purpose register. */
#define DCC_ASMOP_R_32   (1 << DCC_ASMOPT_R_32)   /*< 32-bit general-purpose register. */
#ifdef DCC_ASMOPT_R_64
#define DCC_ASMOP_R_64   (1 << DCC_ASMOPT_R_64)   /*< [x86_64] 64-bit general-purpose register. */
#endif
#ifdef DCC_ASMOPT_R_MMX
#define DCC_ASMOP_R_MMX  (1 << DCC_ASMOPT_R_MMX)  /*< MMX register. */
#endif
#ifdef DCC_ASMOPT_R_SSE
#define DCC_ASMOP_R_SSE  (1 << DCC_ASMOPT_R_SSE)  /*< SSE register. */
#endif
#define DCC_ASMOP_R_CR   (1 << DCC_ASMOPT_R_CR)   /*< CR* register. */
#define DCC_ASMOP_R_TR   (1 << DCC_ASMOPT_R_TR)   /*< TR* register. */
#define DCC_ASMOP_R_DB   (1 << DCC_ASMOPT_R_DB)   /*< DB* register. */
#define DCC_ASMOP_R_DR   (1 << DCC_ASMOPT_R_DR)   /*< DR* register. */
#define DCC_ASMOP_R_SEG  (1 << DCC_ASMOPT_R_SEG)  /*< Segment register. */
#define DCC_ASMOP_R_ST   (1 << DCC_ASMOPT_R_ST)   /*< st(i) register. */
#define DCC_ASMOP_IMM_8S (1 << DCC_ASMOPT_IMM_8S) /*< 8-bit, signed immediate value, or 7-bit unsigned value. */
#define DCC_ASMOP_IMM_8  (1 << DCC_ASMOPT_IMM_8)  /*< 8-bit, unsigned immediate value. */
#define DCC_ASMOP_IMM_16 (1 << DCC_ASMOPT_IMM_16) /*< 16-bit, unsigned immediate value. */
#define DCC_ASMOP_IMM_32 (1 << DCC_ASMOPT_IMM_32) /*< 32-bit, unsigned immediate value. */
#ifdef DCC_ASMOPT_IMM_64
#define DCC_ASMOP_IMM_64 (1 << DCC_ASMOPT_IMM_64) /*< [x86_64] 64-bit, unsigned immediate value. */
#endif
#define DCC_ASMOP_ADDR   (1 << DCC_ASMOPT_ADDR)   /*< An absolute memory location. */
#define DCC_ASMOP_EAX    (DCC_ASMOPT_EAX  << 16)  /*< Any accumulator register ('%al', '%ax', '%eax' or '%rax'). . */
#define DCC_ASMOP_ST0    (DCC_ASMOPT_ST0  << 16)  /*< The '%st(0)' register. */
#define DCC_ASMOP_CL     (DCC_ASMOPT_CL   << 16)  /*< The '%cl' register. */
#define DCC_ASMOP_DX     (DCC_ASMOPT_DX   << 16)  /*< The '%dx' register. */
#define DCC_ASMOP_DS     (DCC_ASMOPT_DS   << 16)  /*< The '%ds' segment register. */
#define DCC_ASMOP_ES     (DCC_ASMOPT_ES   << 16)  /*< The '%es' segment register. */
#define DCC_ASMOP_FS     (DCC_ASMOPT_FS   << 16)  /*< The '%fs' segment register. */
#define DCC_ASMOP_GS     (DCC_ASMOPT_GS   << 16)  /*< The '%gs' segment register. */
#define DCC_ASMOP_CS     (DCC_ASMOPT_CS   << 16)  /*< The '%ss' segment register. */
#define DCC_ASMOP_SS     (DCC_ASMOPT_SS   << 16)  /*< The '%ss' segment register. */
#define DCC_ASMOP_ONE    (DCC_ASMOPT_ONE  << 16)  /*< Any immediate value equal to 1 (NOTE: Will prevent the value from being emit). */
#define DCC_ASMOP_IND    (DCC_ASMOPT_IND  << 16)  /*< The operand is an indirection (e.g.: '*%eax') */
#define DCC_ASMOP_EA     (DCC_ASMOPT_EA   << 16)  /*< A memory location potentially offset from a register. */
#ifdef DCC_ASMOP_R_64
#define DCC_ASMOP_REG    (DCC_ASMOP_R_8|DCC_ASMOP_R_16|DCC_ASMOP_R_32|DCC_ASMOP_R_64)
#else
#define DCC_ASMOP_REG    (DCC_ASMOP_R_8|DCC_ASMOP_R_16|DCC_ASMOP_R_32)
#endif
#ifdef DCC_ASMOP_IMM_64
#define DCC_ASMOP_IMM    (DCC_ASMOP_IMM_8S|DCC_ASMOP_IMM_8|DCC_ASMOP_IMM_16|DCC_ASMOP_IMM_32|DCC_ASMOP_IMM_64)
#else
#define DCC_ASMOP_IMM    (DCC_ASMOP_IMM_8S|DCC_ASMOP_IMM_8|DCC_ASMOP_IMM_16|DCC_ASMOP_IMM_32)
#endif
#else /* I_X86 */
/* ... */
#endif /* !I_X86 */


struct DCCAsmOperand {
#if DCC_TARGET_ASMI(I_X86)
 uint32_t          ao_type;    /*< Set of 'DCC_ASMOP_*'. */
 int8_t            ao_reg;     /*< Register id or -1 if not used. (depends on 'ao_type'). */
 int8_t            ao_reg2;    /*< Second register id or -1 if not used. (depends on 'ao_type'). */
 uint8_t           ao_shift;   /*< Register shift. */
 uint8_t           ao_padding; /*< Padding... */
 struct DCCSymAddr ao_val;     /*< Immediate value, offset, or symbol. */
#else /* I_X86 */
/* ... */
#endif /* !I_X86 */
};


//////////////////////////////////////////////////////////////////////////
// Returns non-ZERO(0) if the given operand 'op' is compatible with 'type' and 'op_flags'
DCCFUN int DCCAsm_OpCompatible(struct DCCAsmOperand const *op, uint16_t op_flags, uint16_t type);
DCCFUN void DCCParse_AsmOperand(struct DCCAsmOperand *op);

//////////////////////////////////////////////////////////////////////////
// Parse and compile one assembly instruction/directive from TPP.
// NOTE: This function assumes that LF tokens are enabled.
DCCFUN void DCCParse_AsmInstr(void);

//////////////////////////////////////////////////////////////////////////
// Parse an assembly directive.
DCCFUN void DCCParse_AsmDirective(void);

//////////////////////////////////////////////////////////////////////////
// Parse an assembly expression
// NOTE: This function does not generate floating point constants in 'v'
// HINT: Pass NULL for 'v' to skip an expression without emitting warnings.
DCCFUN void DCCParse_AsmExpr(struct DCCSymAddr *v);

//////////////////////////////////////////////////////////////////////////
// Parse an integer assembly expression.
DCCFUN DCC(int_t) DCCParse_AsmExprI(void);


/* Reconfigure the target and lexer for assembly mode. */
#define DCCParse_AsmBegin() \
do{ uint32_t const _old_flags = DCCCompiler_Current.c_flags;\
    uint32_t const _old_tpp_flags = TPPLexer_Current->l_flags;\
    uint32_t const _old_tpp_tokens = TPPLexer_Current->l_extokens;\
    TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_WANTLF|TPPLEXER_FLAG_ASM_COMMENTS|\
                                  TPPLEXER_FLAG_DIRECTIVE_NOOWN_LF|\
                                  TPPLEXER_FLAG_COMMENT_NOOWN_LF);\
    TPPLexer_Current->l_extokens |= (TPPLEXER_TOKEN_DOLLAR|TPPLEXER_TOKEN_LOGT);\
    DCCCompiler_Current.c_flags |= DCC_COMPILER_FLAG_INASM
#define DCCParse_AsmEnd() \
    TPPLexer_Current->l_extokens = _old_tpp_tokens;\
    TPPLexer_Current->l_flags = _old_tpp_flags;\
    DCCCompiler_Current.c_flags = _old_flags;\
}while(DCC_MACRO_FALSE)




DCC_DECL_END

#endif /* !GUARD_DCC_ASSEMBLER_H */
