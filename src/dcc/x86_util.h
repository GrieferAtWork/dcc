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
#ifndef GUARD_DCC_X86_UTIL_H
#define GUARD_DCC_X86_UTIL_H 1

#include <dcc/common.h>
#include <dcc/assembler.h>
#include <dcc/target.h>

#include <stddef.h>
#include <stdint.h>

DCC_DECL_BEGIN

/* IA-32 makes is illegal to clobber %EBX, %EDI and %ESI upon function return.
 * Something needs to be done about this, because until that happens, DCC may
 * work just fine on its own, but will run into troubles when trying to
 * interface with code generated by other compilers...
 * Enabling this option causes DCC to never use those
 * registers, or to restore them if they've been used.
 */
#define IA32_PROTECTED_REGISTERS

#ifdef IA32_PROTECTED_REGISTERS
#define DCC_ASMREG_ISPROTECTED(x) ((x) >= 3) /* EBX, ESP, EBP, EDI, ESI */
#else
#define DCC_ASMREG_ISPROTECTED      DCC_ASMREG_ISSPTR /* ESP, EBP */
#endif


#define MODRM_MOD_MASK   B(11000000)
#define MODRM_REG_MASK   B(00111000)
#define MODRM_RM_MASK    B(00000111)
#define MODRM_MOD_SHIFT  6
#define MODRM_REG_SHIFT  3
#define MODRM_RM_SHIFT   0

#define MODRM_MOD(x)    (((x) << MODRM_MOD_SHIFT)/*&MODRM_MOD_MASK*/)
#define MODRM_REG(x)    (((x) << MODRM_REG_SHIFT)/*&MODRM_REG_MASK*/)
#define MODRM_RM(x)     (((x) << MODRM_RM_SHIFT)/*&MODRM_RM_MASK*/)
#define MODRM_GETMOD(x) (((x)&MODRM_MOD_MASK) >> MODRM_MOD_SHIFT)
#define MODRM_GETREG(x) (((x)&MODRM_REG_MASK) >> MODRM_REG_SHIFT)
#define MODRM_GETRM(x)  (((x)&MODRM_RM_MASK) >> MODRM_RM_SHIFT)


#define MODRM_REGISTER(reg,rm) ((uint8_t)(MODRM_MOD(B(11))|MODRM_REG(reg)|MODRM_RM(rm)))
#define MODRM_DISP16(reg)      ((uint8_t)(MODRM_MOD(B(00))|MODRM_REG(reg)|MODRM_RM(B(110)))) /* Uses what would otherwise be displacement from BP. */
#define MODRM_DISP32(reg)      ((uint8_t)(MODRM_MOD(B(00))|MODRM_REG(reg)|MODRM_RM(B(101))))
#define MODRM_SIBREGISTER       DCC_ASMREG_ESP /* When used with MOD 00, 01 or 10, an SIB byte follows. */
#define MODRM_RM_SIB            MODRM_RM(MODRM_SIBREGISTER) /* When used with MOD 00, 01 or 10, an SIB byte follows. */
#if MODRM_SIBREGISTER != B(100)
#error "The SIB register must be 0b100"
#endif


                           /* MOD:   | 00 |    01 |     10 */
#define MODRM_B16_BXSI   0 /* bx+si+ |  0 | disp8 | disp16 */
#define MODRM_B16_BXDI   1 /* bx+di+ |  0 | disp8 | disp16 */
#define MODRM_B16_BPSI   2 /* bp+si+ |  0 | disp8 | disp16 */
#define MODRM_B16_BPDI   3 /* bp+di+ |  0 | disp8 | disp16 */
#define MODRM_B16_SI     4 /* si+    |  0 | disp8 | disp16 */
#define MODRM_B16_DI     5 /* di+    |  0 | disp8 | disp16 */
#define MODRM_B16_BP     6 /* bp+    |  * | disp8 | disp16 | *: raw, disp16 */
#define MODRM_B16_BX     7 /* bx+    |  0 | disp8 | disp16 */
#define MODRM_B16_DISP16 6 /* May only be used with MOD = 00 */

#ifdef X86_UTIL_GENERATOR_SOURCE
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
#define CHECK_WIDTH(w) ((w) == 1 || (w) == 2 || (w) == 4 || (w) == 8)
#else
#define CHECK_WIDTH(w) ((w) == 1 || (w) == 2 || (w) == 4)
#endif
                           
LOCAL target_off_t DCCDisp_PutAddrRel(struct DCCSymAddr const *__restrict addr, rel_t rel_size);
LOCAL target_off_t DCCDisp_PutDispRel(struct DCCSymAddr const *__restrict addr, rel_t rel_size);

PRIVATE void DCCDisp_SymAddr8(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp8(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymAddr16(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp16(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymAddr32(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp32(struct DCCSymAddr const *__restrict expr);
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
PRIVATE void DCCDisp_SymAddr64(struct DCCSymAddr const *__restrict expr);
#ifdef DCC_R_DISP_64
PRIVATE void DCCDisp_SymDisp64(struct DCCSymAddr const *__restrict expr);
#define      DCCDisp_SymDispI  DCCDisp_SymDisp64
#endif
#define      DCCDisp_SymAddrI  DCCDisp_SymAddr64
#else
#define      DCCDisp_SymAddrI  DCCDisp_SymAddr32
#define      DCCDisp_SymDispI  DCCDisp_SymDisp32
#endif

LOCAL target_off_t readw(void const *__restrict p, width_t width, int is_unsigned);
LOCAL void writew(void const *__restrict p, target_off_t v, width_t width);
LOCAL target_off_t def_reloc(struct DCCSymAddr const *__restrict expr,
                             struct DCCSection *__restrict target_section,
                             target_ptr_t target_offset, rel_t rel_size);

PRIVATE void asm_modmem(uint8_t group, struct DCCMemLoc const *__restrict ml);
PRIVATE void asm_modreg(uint8_t group, uint8_t reg);

/* Special opcodes that are complied as external function calls for large operand sizes. */
#define IS_LARGE_OP(op) ((op) == TOK_SHL || (op) == TOK_SHR || \
                         (op) == TOK_RANGLE3 || (op) == '*' || \
                         (op) == '/' || (op) == '%')

/* Functions for handling large binary operations. */
PRIVATE void DCCDisp_LargeMemsBinRegs(tok_t op, struct DCCMemLoc const *__restrict src,
                                      target_siz_t src_bytes, rc_t dst,
                                      rc_t dst2, int src_unsigned);
PRIVATE void DCCDisp_LargeRegsBinMems(tok_t op, rc_t src, rc_t src2,
                                      struct DCCMemLoc const *__restrict dst,
                                      target_siz_t dst_bytes, int src_unsigned);
PRIVATE void DCCDisp_LargeMemBinMem(tok_t op,
                                    struct DCCMemLoc const *__restrict src, target_siz_t src_bytes,
                                    struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                                    int src_unsigned);
PRIVATE void DCCDisp_LargeVecBinMem(tok_t op,
                                    void             const *__restrict src, target_siz_t src_bytes,
                                    struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                                    int src_unsigned);
PRIVATE void DCCDisp_LargeBytBinMem(tok_t op, int                 src, target_siz_t src_bytes,
                                    struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                                    int src_unsigned);
PRIVATE void DCCDisp_LargeByrBinMem(tok_t op, rc_t           src, target_siz_t src_bytes,
                                    struct DCCMemLoc const *__restrict dst, target_siz_t dst_bytes,
                                    int src_unsigned);
PRIVATE void DCCDisp_LargeRegsBinRegs(tok_t op, rc_t src, rc_t src2,
                                      rc_t dst, rc_t dst2, int src_unsigned);
#if DCC_TARGET_SIZEOF_GP_REGISTER < 8
PRIVATE void DCCDisp_LargeCstBinRegs(tok_t op, struct DCCSymExpr const *__restrict val,
                                     rc_t dst, rc_t dst2, int src_unsigned);
#endif

#endif /* X86_UTIL_GENERATOR_SOURCE */


/* Use 'rep mov' / 'rep stos' / 'rep cmps' for transfer/store/compare of data. */
#define REPMOV_THRESHOLD(score) ((score)*(DCC_TARGET_SIZEOF_ARITH_MAX*2))

/* Allocate memory & copy data into it, instead of pushing data individually. */
#define PUSHMEM_MOV_THRESHOLD (DCC_TARGET_SIZEOF_ARITH_MAX*4)


#define asm_op_cld()  ((compiler.c_flags&DCC_COMPILER_FLAG_NODFLAG)?(void)0:t_putb(0xfc))
#define asm_op_std()  ((compiler.c_flags&DCC_COMPILER_FLAG_NODFLAG)?(void)0:t_putb(0xfd))
#define asm_op_rep()    t_putb(0xf3)
#define asm_op_repe()   t_putb(0xf3)
#define asm_op_cmpsb()  t_putb(0xa6)
#define asm_op_cmpsw() (t_putb(0x66),t_putb(0xa7))
#define asm_op_cmpsl()  t_putb(0xa7)
#define asm_op_scasb()  t_putb(0xae)
#define asm_op_scasw() (t_putb(0x66),t_putb(0xaf))
#define asm_op_scasl()  t_putb(0xaf)
#define asm_op_movsb()  t_putb(0xa4)
#define asm_op_movsw() (t_putb(0x66),t_putb(0xa5))
#define asm_op_movsl()  t_putb(0xa5)
#define asm_op_stosb()  t_putb(0xaa)
#define asm_op_stosw() (t_putb(0x66),t_putb(0xab))
#define asm_op_stosl()  t_putb(0xab)


#define DCC_CONFIG_NEED_X86_INSTRLEN  \
  (DCC_LIBFORMAT_PE_STATIC|DCC_LIBFORMAT_ELF_STATIC)


#if DCC_CONFIG_NEED_X86_INSTRLEN
#define INSTRLEN_DEBUG 0
INTDEF uint8_t const *x86_instrlen(uint8_t const *p);
#if INSTRLEN_DEBUG
INTDEF size_t    instrlen_offset;
INTDEF uint8_t  *instrlen_base;
#endif

/* Reverse engineer disp relocations by decompiling
 * the assembly from the given 'text_section' and
 * inserting the necessary relocations everywhere
 * an inter-section relocation takes place.
 * NOTE: This function assumes that all sections within the
 *       current unit have a base address assigned that
 *       is equal to the sections runtime address when
 *      'image_base' is added. */
INTDEF void
x86_mkrel_textdisp(struct DCCSection *__restrict text_section,
                   target_ptr_t image_base);

/* Returns the section containing a given RVA address.
 * Similar to 'dcc_mkrel_textdisp', this function assumes
 * that all sections have been assigned a unique base address. */
INTDEF struct DCCSection *dcc_getsec(target_ptr_t rva);
#endif

DCC_DECL_END

#endif /* !GUARD_DCC_X86_UTIL_H */
