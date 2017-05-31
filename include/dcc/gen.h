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
#ifndef GUARD_DCC_GEN_H
#define GUARD_DCC_GEN_H 1

#include "common.h"
#include "assembler.h"
#include "vstack.h"
#include "lexer.h"
#include "target.h"

DCC_DECL_BEGIN


/* One of '1', '2', '4' or (if DCC_TARGET_SIZEOF_IMM_MAX >= 8) '8'
 * >> A multiple of '2' <= 'DCC_TARGET_SIZEOF_IMM_MAX' */
typedef unsigned int DCC(width_t);
struct DCCMemLoc;

DCCFUN void DCCDisp_SymAddr(struct DCCSymAddr const *__restrict expr, DCC(width_t) width);
DCCFUN void DCCDisp_SymDisp(struct DCCSymAddr const *__restrict expr, DCC(width_t) width);

/* If known, translate the memory location 'l' into its compile-time
 * counterpart, returning a pointer to the section data.
 * @return: NULL: - The section location could not be determined (e.g.: forward-symbols/no symbol)
 *                - Compile-time addressing is disabled (The 'DCC_COMPILER_FLAG_SINIT' flag wasn't set)
 *                - [DCCMemLoc_CompilerData] Relocations were present inside the text are (use 'DCCMemLoc_CompilerText' instead)
 * 'DCCMemLoc_CompilerData' does the same, but only for constant data that may be used for
 * compile-time optimizations where operands are static constants that are not allowed to be changed. */
DCCFUN void       *DCCMemLoc_CompilerAddr(struct DCCMemLoc const *__restrict l, size_t n_bytes);
DCCFUN void const *DCCMemLoc_CompilerData(struct DCCMemLoc const *__restrict l, size_t n_bytes);

/* Same as the two function above, but accept an additional argument 'update_ptr' that describes
 * any kind of compile-time pointer (doesn't even have to be part of a section), that will be
 * updated in the even that in order to access section data, the section buffer must be re-allocated.
 * >> This is a very crude work-around for such cases, but required for
 *    compile-time implementation of something like 'DCCDisp_VecMovMem()'. */
DCCFUN void       *DCCMemLoc_CompilerAddrUpdate(struct DCCMemLoc const *__restrict l, void **__restrict update_ptr, size_t n_bytes);
DCCFUN void const *DCCMemLoc_CompilerDataUpdate(struct DCCMemLoc const *__restrict l, void **__restrict update_ptr, size_t n_bytes);

struct DCCCompilerText {
 struct DCCSection *ct_sec;  /*< [0..1] Section that the text is located inside of. */
 DCC(target_ptr_t)  ct_base; /*< Text base address (The offset of the text in 'ct_sec' and start of relocations). */
 size_t             ct_relc; /*< Amount of relocations affecting the text. */
 struct DCCRel     *ct_relv; /*< [?..ct_relc][weak(ct_sec->sc_relv)] Vector of (sorted) relocations affective the text. */
};


/* A function offering extended functionality compared to 'DCCMemLoc_CompilerData',
 * adding the ability to access compile-time data containing relocations, such as
 * is required when cloning static initializers containing dynamic relocations.
 * @return: NULL: Failed to query compile-time information about the given memory location.
 * @return: * : The compile-time text address of the given memory location. */
DCCFUN void const *DCCMemLoc_CompilerText(struct DCCMemLoc const *__restrict l,
                                          struct DCCCompilerText *__restrict text,
                                          size_t n_bytes);

/* mov !src, %dst (size depends on register) */
DCCFUN void DCCDisp_MemMovReg(struct DCCMemLoc const *__restrict src, DCC(rc_t) dst);

/* mov %src, !dst (size depends on register) */
DCCFUN void DCCDisp_RegMovMem(DCC(rc_t) src, struct DCCMemLoc const *__restrict dst);
DCCFUN void DCCDisp_MemsMovReg(struct DCCMemLoc const *__restrict src,
                               DCC(target_siz_t) src_bytes, DCC(rc_t) dst,
                               int src_unsigned);
DCCFUN void DCCDisp_RegMovMems(DCC(rc_t) src, struct DCCMemLoc const *__restrict dst,
                               DCC(target_siz_t) dst_bytes, int src_unsigned);

/* Move 'n_bytes' of memory from 'src' to 'dst'.
 * - If both 'src' and 'dst' are known at compile-time, and static
 *   initialization is enabled, memory will be copied at compile-time.
 * - If a relation exists between 'src' and 'dst' that can be determined
 *   at compile-time (e.g.: both offsets from the same register), special
 *   optimizations may be performed if it could be determined that there
 *   is no overlap. */
DCCFUN void DCCDisp_MemMovMem(struct DCCMemLoc const *__restrict src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
/* Similar to 'DCCDisp_MemMovMem', but the
 * initialization data is given as a vector 'src' */
DCCFUN void DCCDisp_VecMovMem(void             const *__restrict src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
/* Same as a call to 'DCCDisp_VecMovMem' with a vector filled with 'src' */
DCCFUN void DCCDisp_BytMovMem(int                                src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
/* Fill 'dst_bytes' of memory at 'dst', using an imaginary vector
 * of 'src_bytes' filled with the 8-bit equivalent of 'src'. */
DCCFUN void DCCDisp_ByrMovMem(DCC(rc_t)                          src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);

/* mov %src, %dst (Automatically performs trunc/zero-/sign-extension based on 'src_unsigned') */
DCCFUN void DCCDisp_RegMovReg(DCC(rc_t) src, DCC(rc_t) dst, int src_unsigned);

/* lea !val, %dst (can also used for loading constants into registers) */
DCCFUN void DCCDisp_LocMovReg(struct DCCMemLoc const *__restrict val, DCC(rc_t) dst);
DCCFUN void DCCDisp_CstMovReg(struct DCCSymAddr const *__restrict val, DCC(rc_t) dst);
DCCFUN void DCCDisp_CstMovRegRaw(struct DCCSymAddr const *__restrict val, DCC(rc_t) dst);

/* lea !val, !dst (Set 'n' bytes of memory at 'dst' to an address from 'val')
 * NOTE: If static initialization is enabled, memory may be moved at compile-time. */
DCCFUN void DCCDisp_CstMovMem(struct DCCSymAddr const *__restrict val,
                              struct DCCMemLoc  const *__restrict dst,
                              DCC(width_t) width);

/* Perform a unary in-place operation.
 * @param: op: One of:
 *  - '-':     dst = -dst;
 *  - '~':     dst = ~dst;
 *  - '(':   (*dst)(...);    // CALL (With indirection)
 *  - TOK_INC: dst = dst+1;
 *  - TOK_DEC: dst = dst-1;
 */
DCCFUN void DCCDisp_UnaryReg(DCC(tok_t) op, DCC(rc_t) dst);
DCCFUN void DCCDisp_UnaryMem(DCC(tok_t) op, struct DCCMemLoc const *__restrict dst,
                             DCC(target_siz_t) dst_bytes);

/* Perform binary operations.
 * Semantically, these functions are the same as the 'mov' operations,
 * even going as far as to accept '=' for 'op' to do the same job.
 * Yet these functions will always perform the inplace-equivalent of the
 * given 'op' instruction on 'dst', using 'src'/'val' as an immutable operand.
 * WARNING: No optimizations are every performed by these functions, only generating pure code.
 * WARNING: Some operations may generate calls to external symbols, which must be resolved later.
 * @param: op: One of:
 *  - '='          dst = src;
 *  - '+'          dst = dst + src;
 *  - '|'          dst = dst | src;
 *  - TOK_INC      dst = dst + (src + CARRY);
 *  - TOK_DEC      dst = dst - (src + CARRY);
 *  - '&'          dst = dst & src;
 *  - '-'          dst = dst - src;
 *  - '^'          dst = dst ^ src;
 *  - '?'          set_eflags(dst - src); // compare (set EFLAGS accordingly)
 *  - '*':         dst = dst * src;
 *  - '%':         dst = dst % src;
 *  - '/':         dst = dst / src;
 *  - TOK_SHL:     dst = dst << src;
 *  - TOK_SHR:     dst = dst >> src;
 *  - TOK_RANGLE3: dst = dst >> src; // dst: signed
 *  - 't':         dst & src;        // bitwise test (set eflags accordingly)
 */
DCCFUN void DCCDisp_MemBinReg(DCC(tok_t) op, struct DCCMemLoc const *__restrict src, DCC(rc_t) dst, int src_unsigned);
DCCFUN void DCCDisp_RegBinMem(DCC(tok_t) op, DCC(rc_t) src, struct DCCMemLoc const *__restrict dst, int src_unsigned);
DCCFUN void DCCDisp_MemsBinReg(DCC(tok_t) op, struct DCCMemLoc const *__restrict src,
                               DCC(target_siz_t) src_bytes, DCC(rc_t) dst, int src_unsigned);
DCCFUN void DCCDisp_RegBinMems(DCC(tok_t) op, DCC(rc_t) src, struct DCCMemLoc const *__restrict dst,
                               DCC(target_siz_t) dst_bytes, int src_unsigned);
DCCFUN void DCCDisp_MemsBinRegs(DCC(tok_t) op, struct DCCMemLoc const *__restrict src,
                                DCC(target_siz_t) src_bytes, DCC(rc_t) dst,
                                DCC(rc_t) dst2, int src_unsigned);
DCCFUN void DCCDisp_RegsBinMems(DCC(tok_t) op, DCC(rc_t) src, DCC(rc_t) src2,
                                struct DCCMemLoc const *__restrict dst,
                                DCC(target_siz_t) dst_bytes, int src_unsigned);
DCCFUN void DCCDisp_MemBinMem(DCC(tok_t) op,
                              struct DCCMemLoc const *__restrict src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
DCCFUN void DCCDisp_VecBinMem(DCC(tok_t) op,
                              void             const *__restrict src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
DCCFUN void DCCDisp_BytBinMem(DCC(tok_t) op, int                 src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
DCCFUN void DCCDisp_ByrBinMem(DCC(tok_t) op, DCC(rc_t)           src, DCC(target_siz_t) src_bytes,
                              struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) dst_bytes,
                              int src_unsigned);
DCCFUN void DCCDisp_RegBinReg(DCC(tok_t) op, DCC(rc_t) src, DCC(rc_t) dst, int src_unsigned);
DCCFUN void DCCDisp_CstBinReg(DCC(tok_t) op, struct DCCSymAddr const *__restrict val,
                              DCC(rc_t) dst, int src_unsigned);
DCCFUN void DCCDisp_RegsBinRegs(DCC(tok_t) op, DCC(rc_t) src, DCC(rc_t) src2,
                                DCC(rc_t) dst, DCC(rc_t) dst2, int src_unsigned);
#if DCC_TARGET_SIZEOF_ARITH_MAX < 8
DCCFUN void DCCDisp_CstBinRegs(DCC(tok_t) op, struct DCCSymExpr const *__restrict val,
                               DCC(rc_t) dst, DCC(rc_t) dst2, int src_unsigned);
#endif
DCCFUN void DCCDisp_CstBinMem(DCC(tok_t) op,
                              struct DCCSymAddr const *__restrict val,
                              struct DCCMemLoc const *__restrict dst,
                              DCC(width_t) width, int src_unsigned);


/* Add the given 'val' to 'dst'
 * NOTE: Special optimizations are performed when the
 *       absolute value of 'val' is known at compile-time. */
DCCFUN void DCCDisp_AddReg(struct DCCSymAddr const *__restrict val, DCC(rc_t) dst);
DCCFUN void DCCDisp_AddMem(struct DCCSymAddr const *__restrict val,
                           struct DCCMemLoc const *__restrict dst,
                           DCC(width_t) width);

/* Same as 'DCCDisp_AddReg', but protected registers will not be modified,
 * but instead create a copy that will be the sum of 'val' and 'dst' */
DCCFUN DCC(rc_t) DCCDisp_AddProtReg(struct DCCSymAddr const *__restrict val, DCC(rc_t) dst);

/* Push/pop constants/addresses/memory-locations/registers onto the run-time %ESP-stack. */
DCCFUN void DCCDisp_CstPush(struct DCCSymAddr const *__restrict val, DCC(width_t) width); /* NOTE: Always accepts '8' for 'width' */
DCCFUN void DCCDisp_LocPush(struct DCCMemLoc const *__restrict addr);
DCCFUN void DCCDisp_MemPush(struct DCCMemLoc const *__restrict src, DCC(target_siz_t) n_bytes);
DCCFUN void DCCDisp_MemRevPush(struct DCCMemLoc const *__restrict src, DCC(target_siz_t) n_bytes);
DCCFUN void DCCDisp_MemPushs(struct DCCMemLoc const *__restrict src, DCC(target_siz_t) src_bytes,
                             DCC(target_siz_t) n_bytes, int src_unsigned);
DCCFUN void DCCDisp_VecPush(void const *__restrict src, DCC(target_siz_t) n_bytes);
DCCFUN void DCCDisp_BytPush(int src, DCC(target_siz_t) n_bytes);
DCCFUN void DCCDisp_ByrPush(DCC(rc_t) src, DCC(target_siz_t) n_bytes);
DCCFUN void DCCDisp_RegPush(DCC(rc_t) src);
#define     DCCDisp_NdfPush(n_bytes) DCCDisp_BytPush(0,n_bytes)
DCCFUN void DCCDisp_PopMem(struct DCCMemLoc const *__restrict dst, DCC(width_t) width);
DCCFUN void DCCDisp_PopReg(DCC(rc_t) dst);

/* Sign-extend the given register into itself, essentially meaning
 * that all bits will be set to a copy of the 8'th bit. */
DCCFUN void DCCDisp_SignExtendReg(DCC(rc_t) dst);
DCCFUN void DCCDisp_SignExtendMem(struct DCCMemLoc const *__restrict dst, DCC(target_siz_t) n_bytes);

/* Load one byte from 'src', filling 'dst' with the sign-extension of it. */
DCCFUN void DCCDisp_MemSignExtendReg(struct DCCMemLoc const *__restrict src, DCC(rc_t) dst);

/* Load the effective address of 'addr' into the given 'dst' */
DCCFUN void DCCDisp_LeaReg(struct DCCMemLoc const *__restrict addr, DCC(rc_t) dst);
DCCFUN void DCCDisp_LeaMem(struct DCCMemLoc const *__restrict addr,
                           struct DCCMemLoc const *__restrict dst, DCC(width_t) width);

typedef uint8_t DCC(test_t);

#define DCC_TEST_NBIT     1
#define DCC_TEST_NOT(x) ((x)^DCC_TEST_NBIT)

#define DCC_TEST_O    0x0 /* test: overflow (OF=1). */
#define DCC_TEST_NO   0x1 /* test: not overflow (OF=0). */
#define DCC_TEST_B    0x2 /* test: below (CF=1). */
#define DCC_TEST_C    0x2 /* test: carry (CF=1). */
#define DCC_TEST_NAE  0x2 /* test: not above or equal (CF=1). */
#define DCC_TEST_AE   0x3 /* test: above or equal (CF=0). */
#define DCC_TEST_NB   0x3 /* test: not below (CF=0). */
#define DCC_TEST_NC   0x3 /* test: not carry (CF=0). */
#define DCC_TEST_E    0x4 /* test: equal (ZF=1). */
#define DCC_TEST_Z    0x4 /* test: zero (ZF=1). */
#define DCC_TEST_NE   0x5 /* test: not equal (ZF=0). */
#define DCC_TEST_NZ   0x5 /* test: not zero (ZF=0). */
#define DCC_TEST_BE   0x6 /* test: below or equal (CF=1 or ZF=1). */
#define DCC_TEST_NA   0x6 /* test: not above (CF=1 or ZF=1). */
#define DCC_TEST_A    0x7 /* test: above (CF=0 and ZF=0). */
#define DCC_TEST_NBE  0x7 /* test: not below or equal (CF=0 and ZF=0). */
#define DCC_TEST_S    0x8 /* test: sign (SF=1). */
#define DCC_TEST_NS   0x9 /* test: not sign (SF=0). */
#define DCC_TEST_P    0xa /* test: parity (PF=1). */
#define DCC_TEST_PE   0xa /* test: parity even (PF=1). */
#define DCC_TEST_NP   0xb /* test: not parity (PF=0). */
#define DCC_TEST_PO   0xb /* test: parity odd (PF=0). */
#define DCC_TEST_L    0xc /* test: less (SF<>OF). */
#define DCC_TEST_NGE  0xc /* test: not greater or equal (SF<>OF). */
#define DCC_TEST_GE   0xd /* test: greater or equal (SF=OF). */
#define DCC_TEST_NL   0xd /* test: not less (SF=OF). */
#define DCC_TEST_LE   0xe /* test: less or equal (ZF=1 or SF<>OF). */
#define DCC_TEST_NG   0xe /* test: not greater (ZF=1 or SF<>OF). */
#define DCC_TEST_G    0xf /* test: greater (ZF=0 and SF=OF). */
#define DCC_TEST_NLE  0xf /* test: not less or equal (ZF=0 and SF=OF). */


/* Unconditionally jump to/call the given
 * register/memory-location/address-read-from-memory. */
DCCFUN void DCCDisp_RegJmp(DCC(rc_t) reg);
DCCFUN void DCCDisp_MemJmp(struct DCCMemLoc const *__restrict src, DCC(target_siz_t) n);
DCCFUN void DCCDisp_LocJmp(struct DCCMemLoc const *__restrict addr);
#define     DCCDisp_RegCll(reg)   DCCDisp_UnaryReg('(',reg)
#define     DCCDisp_MemCll(src,n) DCCDisp_UnaryMem('(',src,n)
DCCFUN void DCCDisp_LocCll(struct DCCMemLoc const *__restrict addr);

/* Generate a conditional jump (according to EFLAGS) */
DCCFUN void DCCDisp_RegJcc(DCC(test_t) t, DCC(rc_t) reg);
DCCFUN void DCCDisp_MemJcc(DCC(test_t) t, struct DCCMemLoc const *__restrict src, DCC(target_siz_t) n);
DCCFUN void DCCDisp_LocJcc(DCC(test_t) t, struct DCCMemLoc const *__restrict addr);

/* Conditionally move 'src' into 'dst'. */
DCCFUN void DCCDisp_RegCMovReg(DCC(test_t) t, DCC(rc_t) src, DCC(rc_t) dst, int src_unsigned);
DCCFUN void DCCDisp_MemCMovReg(DCC(test_t) t, struct DCCMemLoc const *__restrict src, DCC(rc_t) dst);

/* Set a given register/memory location to 0/1, based on the given test.
 * NOTE: For best results, only pass 1-byte registers/memory
 *       areas, as any overflow must manually be set to ZERO(0). */
DCCFUN void DCCDisp_SccReg(DCC(test_t) t, DCC(rc_t) reg);
DCCFUN void DCCDisp_SccMem(DCC(test_t) t, struct DCCMemLoc const *__restrict dst,
                           DCC(target_siz_t) n);

/* mov $cst, %dst (Simply a proxy for 'DCCDisp_CstMovReg') */
DCC_LOCAL void DCCDisp_IntMovReg(DCC(target_off_t) cst, DCC(rc_t) dst);

DCC_LOCAL void
DCCDisp_IntMovReg(DCC(target_off_t) cst, DCC(rc_t) dst) {
 struct DCCSymAddr l;
 l.sa_off = cst;
 l.sa_sym = NULL;
 DCCDisp_CstMovReg(&l,dst);
}



struct DCCDispFunction { DCC(target_ptr_t) df_proaddr; };
DCCFUN void DCCDisp_FunProlog(struct DCCDispFunction *__restrict info);
DCCFUN void DCCDisp_GenProlog(struct DCCDispFunction *__restrict info);
DCCFUN void DCCDisp_FunEpilog(void);
DCCFUN void DCCDisp_Ret(void);

/* Fill 'n_bytes' of memory starting at 'p' with fast, executable code that does nothing.
 * WARNING: If the skip block becomes too large, a relative jump may be placed at the start,
 *          meaning that it is unsafe to enter this block of code anywhere else that at the start.
 * >> The naive implementation would be a memset with 'DCCGEN_NOPBYTE', yet this function is a bit smarter. */
DCCFUN void DCCDisp_FillNop(void *__restrict p, size_t n_bytes);

#define DCCGEN_NOPBYTE  0x90 /* Byte used for NOPs */

/* offset/register constants for accessing the return frame/address:
 * Access the parent frame:
 * >> mov DCCDISP_RETURNFRAME_OFF(%DCCDISP_RETURNFRAME_REG), %eax // parent #1
 * >> mov DCCDISP_RETURNFRAME_OFF(%eax), %eax                     // parent #2
 * >> mov DCCDISP_RETURNFRAME_OFF(%eax), %eax                     // parent #...
 * Access the return address:
 * >> mov DCCDISP_RETURNADDR_OFF(%DCCDISP_RETURNFRAME_REG), %eax  // parent #1
 * >> mov DCCDISP_RETURNFRAME_OFF(%DCCDISP_RETURNFRAME_REG), %eax
 * >> mov DCCDISP_RETURNADDR_OFF(%eax), %eax                      // parent #2
 * >> mov DCCDISP_RETURNFRAME_OFF(%DCCDISP_RETURNFRAME_REG), %eax
 * >> mov DCCDISP_RETURNFRAME_OFF(%eax), %eax
 * >> mov DCCDISP_RETURNADDR_OFF(%eax), %eax                      // parent #...
 */
#define DCCDISP_RETURNFRAME_OFF 0
#define DCCDISP_RETURNFRAME_REG DCC_RR_XBP
#define DCCDISP_RETURNADDR_OFF  DCC_TARGET_SIZEOF_POINTER
#define DCCDISP_RETURNADDR_REG  DCC_RR_XBP



/* Generate a CPU-fence that performs different functionality. */
DCCFUN void DCCDisp_Fence(DCC(tok_t) kind);
#define DCC_FENCE_BREAKPOINT    DCC(KWD___builtin_breakpoint)
#define DCC_FENCE_TRAP          DCC(KWD___builtin_trap)
#define DCC_FENCE_UNREACHABLE   DCC(KWD___builtin_unreachable)
#define DCC_FENCE_SFENCE        DCC(KWD___builtin_sfence)
#define DCC_FENCE_LFENCE        DCC(KWD___builtin_lfence)
#define DCC_FENCE_MFENCE        DCC(KWD___builtin_mfence)


/* Swap the byte-order within the given register/memory location.
 * NOTE: no-op for 1-byte register/size
 * NOTE: 'n_bytes' is actually allowed to be anything!
 *       If it is an uneven number, the swap will not modify the center-most byte. */
DCCFUN void DCCDisp_BSwapReg(DCC(rc_t) dst);
DCCFUN void DCCDisp_BSwapMem(struct DCCMemLoc const *__restrict dst,
                             DCC(target_siz_t) n_bytes);


/* Exchange two registers/memory-locations.
 * WARNING: The caller is responsible for ensuring that
 *          both registers are of equal class & size. */
DCCFUN void DCCDisp_RegXchReg(DCC(rc_t) a, DCC(rc_t) b);
DCCFUN void DCCDisp_RegXchMem(DCC(rc_t) a, struct DCCMemLoc const *__restrict b);
#define     DCCDisp_MemXchReg(a,b) DCCDisp_RegXchMem(b,a)
DCCFUN void DCCDisp_MemXchMem(struct DCCMemLoc const *__restrict a,
                              struct DCCMemLoc const *__restrict b,
                              DCC(target_siz_t) n_bytes);

/* Compute the value of '__builtin_ffs(src)' and store it in 'dst'
 * NOTE: When both are registers, 'src' and 'dst' are allowed to overlap! */
DCCFUN void DCCDisp_FFSReg(DCC(rc_t) src, DCC(rc_t) dst);
DCCFUN void DCCDisp_FFSRegs(DCC(rc_t) src, DCC(rc_t) src2, DCC(rc_t) dst);
DCCFUN void DCCDisp_FFSMem(struct DCCMemLoc const *__restrict src,
                           DCC(rc_t) dst, DCC(target_siz_t) n_bytes);

DCCFUN void DCCDisp_CLZReg(DCC(rc_t) src, DCC(rc_t) dst);
DCCFUN void DCCDisp_CLZRegs(DCC(rc_t) src, DCC(rc_t) src2, DCC(rc_t) dst);
DCCFUN void DCCDisp_CLZMem(struct DCCMemLoc const *__restrict src,
                           DCC(rc_t) dst, DCC(target_siz_t) n_bytes);


/* atomic: if (!dst == EAX) { ZF = 1; !dst = src; } else { ZF = 0; EAX = !dst; } */
DCCFUN void DCCDisp_AtomicRegCmpXchMemAX(DCC(rc_t) src, struct DCCMemLoc const *__restrict dst);

/* Perform a binary operation atomically.
 * NOTE: These function support two additional operation:
 *  op = '~': nand - (dst = ~(dst & src))
 *  op = '=': xchg - (dst = src)
 * @param: fetch_mode: One of 'DCCDISP_ATOMICBIN_FETCH*' */
DCCFUN void
DCCDisp_AtomicRegBinMem(DCC(tok_t) op, int fetch_mode, DCC(rc_t) src,
                        struct DCCMemLoc const *__restrict dst);
#define DCCDISP_ATOMICBIN_FETCHNEVER  0 /* Never fetch any value. */
#define DCCDISP_ATOMICBIN_FETCHBEFORE 1 /* The the old value, storing it in 'src' upon completion. */
#define DCCDISP_ATOMICBIN_FETCHAFTER  2 /* The the new value, storing it in 'src' upon completion. */

/* Perform a unary, atomic operation.
 * NOTE: when 'fetch_mode' is 'DCCDISP_ATOMICBIN_FETCHNEVER',
 *      'result' is still used for the memory width. */
DCCFUN void
DCCDisp_AtomicUnaryMem(DCC(tok_t) op, int fetch_mode, DCC(rc_t) result,
                       struct DCCMemLoc const *__restrict dst);


DCC_DECL_END

#endif /* !GUARD_DCC_GEN_H */
