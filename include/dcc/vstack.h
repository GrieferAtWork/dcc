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
#ifndef GUARD_DCC_VSTACK_H
#define GUARD_DCC_VSTACK_H 1

#include "common.h"
#include "target.h"
#include "type.h"
#include "lexer.h"
#include "assembler.h"

#include <stdint.h>

DCC_DECL_BEGIN

/* For the sake of optimization, the vstack makes the following assumptions:
 *   - No section except for '&DCCSection_Abs' can be loaded at address ZERO
 *   - No (meaningful) symbol can be loaded at address ZERO
 *     This assumption is made to allow code like this to be optimized:
 *     >> #include <stddef.h>
 *     >> #include <stdio.h>
 *     >> 
 *     >> extern int value;
 *     >> 
 *     >> int main()
 *     >>     int    argc;
 *     >>     char **argv;
 *     >> {
 *     >>     if (&value != NULL) {
 *     >>         printf("");
 *     >>     }
 *     >>     return 0;
 *     >> }
 *     >> 
 *     >> // Above, the compiler will have assumed that the
 *     >> // address of 'value' can never be NULL, even though
 *     >> // we can now break that assumption by doing this:
 *     >> __asm__("value = 0"); // ups...
 *     >> 
 *   - The %esp and %ebp registers are never NULL
 */


struct DCCSym;
struct TPPString;

#ifdef DCC_TARGET_X86
/* Register classes (or'd together with 'DCC_ASMREG_*' constants from "assembler.h")
 * NOTE: The RC_F* classes are or'd together to express overlapp.
 * e.g.: the first 4 16-bit register have 2 associated 8-bit registers.
 *    >> %eax is expressed as: (DCC_ASMREG_AX|DCC_RC_I8|DCC_RC_I16|DCC_RC_I32)
 *    >> %esp is expressed as: (DCC_ASMREG_SP|DCC_RC_I16|DCC_RC_I32) // There is no 8-bit variant of SP
 * */
#   define DCC_RC_I8     0x0008 /*< 8-bit register: AL, CL, DL, BL, AH, CH, DH, BH. */
#   define DCC_RC_I16    0x0010 /*< 16-bit register: AX, CX, DX, BX, SI, DI. */
#   define DCC_RC_I32    0x0020 /*< 32-bit register: EAX, ECX, EDX, EBX, ESI, EDI. */
#if DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_RC_I64    0x0040 /*< 64-bit register: RAX, RCX, RDX, RBX, RSI, RDI. */
#   define DCC_RC_I3264 (DCC_RC_I32|DCC_RC_I64)
#   define DCC_RC_PTR    DCC_RC_I64
#   define DCC_RC_PTRX  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8)
#else
#   define DCC_RC_I3264  DCC_RC_I32
#   define DCC_RC_PTR    DCC_RC_I32
#   define DCC_RC_PTRX  (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8)
#endif
#   define DCC_RC_I     (DCC_RC_I8|DCC_RC_I16|DCC_RC_I3264)
#   define DCC_RC_MMX    0x0800 /*< MMX register: MM(0-7) */
#   define DCC_RC_SSE    0x0900 /*< SSE register: XMM(0-7) */
#   define DCC_RC_CR     0x0a00 /*< CR* register: CR(0-7) */
#   define DCC_RC_TR     0x0b00 /*< TR* register: TR(0-7) */
#   define DCC_RC_DB     0x0c00 /*< DB* register: DB(0-7) */
#   define DCC_RC_DR     0x0d00 /*< DR* register: DR(0-7)*/
#   define DCC_RC_SEG    0x0e00 /*< Segment registers. */
#   define DCC_RC_ST     0x0f00 /*< ST(i) registers. */
#else
#   error FIXME
#endif

#define DCC_PRIVATE_RC_IN1 DCC_RC_I8
#define DCC_PRIVATE_RC_IN2 DCC_RC_I16
#define DCC_PRIVATE_RC_IN4 DCC_RC_I32
#ifdef DCC_RC_I64
#define DCC_PRIVATE_RC_IN8 DCC_RC_I64
#endif
#define DCC_PRIVATE_RC_IN(bytes) DCC_PRIVATE_RC_IN##bytes
#define DCC_RC_IN(bytes) DCC_PRIVATE_RC_IN(bytes)

#define DCC_RI_MASK 0x0007 /*< Mask for the register id (One of 'DCC_ASMREG_*'). */
#define DCC_RC_MASK 0xfff8 /*< Mask for the register class (See above). */

/* Special register classes that describe things different than registers. */
#define DCC_RC_CONST 0x0000 /*< Constant value. ('sv_const' contains its value, or offset when 'sv_sym' is non-NULL). */

/* NOTE: Local variables are implemented as follow:
 *    >> int x = 42;
 *    >> printf("%d\n",x);
 *       Internal:
 *    >> struct DCCStackValue local_x,init_x;
 *    >> local_x.sv_ctype.t_type = DCCTYPE_INT;
 *    >> local_x.sv_ctype.t_base = NULL;
 *    >> local_x.sv_flags        = DCC_SFLAG_LVALUE;
 *    >> local_x.sv_reg          = DCC_RR_XBP;
 *    >> local_x.sv_reg2         = DCC_RC_CONST;
 *    >> local_x.sv_const.it     = DCCCompiler_HWStackAlloc(DCC_TARGET_SIZEOF_INT,
 *    >>                                                    DCC_TARGET_SIZEOF_INT,0);
 *    >> local_x.sv_sym         = NULL;
 *    >> init_x.sv_ctype.t_type = DCCTYPE_INT;
 *    >> init_x.sv_ctype.t_base = NULL;
 *    >> init_x.sv_flags        = DCC_SFLAG_RVALUE;
 *    >> init_x.sv_reg          = DCC_RC_CONST;
 *    >> init_x.sv_reg2         = DCC_RC_CONST;
 *    >> init_x.sv_const.it     = 42;
 *    >> init_x.sv_sym          = NULL;
 *    >>
 *    >> vpush(&local_x);
 *    >> vpush(&init_x);
 *    >> vstore(1);
 */

/* NOTE: Global variables are implemented as follows:
 *    >> static int x = 42;
 *    >> printf("%d\n",x);
 *       Internal:
 *    >> struct DCCStackValue static_x,init_x;
 *    >> struct DCCSection *symsec;
 *    >> target_ptr_t       x_addr;
 *    >> struct DCCSym *sym = DCCUnit_NewSyms("x",DCC_SYMFLAG_STATIC);
 *    >> if (DCCSym_ISDEFINED(sym)) return X_IS_ALREADY_DEFINED;
 *    >> 
 *    >> // If 'x' was declared as const, this would be 'unit.u_data'
 *    >> // In both cases, the used section can be overwritten with __attribute__((section(...)))
 *    >> symsec = unit.u_bss;
 *    >>
 *    >> x_addr = DCCSection_DAlloc(symsec,DCC_TARGET_SIZEOF_INT,
 *    >>                                   DCC_TARGET_SIZEOF_INT,
 *    >>                                   0); 
 *    >> DCCSym_Define(sym,symsec,x_addr,DCC_TARGET_SIZEOF_INT);
 *    >> init_x.sv_ctype.t_type = DCCTYPE_INT;
 *    >> init_x.sv_ctype.t_base = NULL;
 *    >> init_x.sv_flags        = DCC_SFLAG_LVALUE;
 *    >> init_x.sv_reg          = DCC_RC_CONST;
 *    >> init_x.sv_reg2         = DCC_RC_CONST;
 *    >> // This could be used as a compile-time constant offset to 'sym'
 *    >> // e.g.: When 'x' is a structure, this might be the offset to a member.
 *    >> init_x.sv_const.it     = 0;
 *    >> static_x.sv_sym        = sym;
 *    >>
 *    >> init_x.sv_ctype.t_type = DCCTYPE_INT;
 *    >> init_x.sv_ctype.t_base = NULL;
 *    >> init_x.sv_flags        = DCC_SFLAG_RVALUE;
 *    >> init_x.sv_reg          = DCC_RC_CONST;
 *    >> init_x.sv_reg2         = DCC_RC_CONST;
 *    >> init_x.sv_const.it     = 42;
 *    >> init_x.sv_sym          = NULL;
 *    >>
 *    >> pushf();
 *    >> // Allow static initializers to directly write to section memory.
 *    >> // If this flag wasn't set, the calls below would
 *    >> // generate code that assigns '42' to 'x' at runtime.
 *    >> setf(DCC_COMPILER_FLAG_SINIT);
 *    >> vpush(&local_x);
 *    >> vpush(&init_x);
 *    >> vstore(1);
 *    >> popf();
 */

#ifdef DCC_TARGET_X86
/* Register descriptor + class for base address of local variables. */
#if DCC_TARGET_CPU == DCC_CPU_X86_64
#define DCC_RR_XAX  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_RAX)
#define DCC_RR_XCX  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_RCX)
#define DCC_RR_XDX  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_RDX)
#define DCC_RR_XBX  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_RBX)
#define DCC_RR_XBP  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_RBP)
#define DCC_RR_XSP  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_RSP)
#define DCC_RR_XSI  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_RSI)
#define DCC_RR_XDI  (DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_RDI)
#else
#define DCC_RR_XAX  (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX)
#define DCC_RR_XCX  (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_ECX)
#define DCC_RR_XDX  (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EDX)
#define DCC_RR_XBX  (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EBX)
#define DCC_RR_XBP  (DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_EBP)
#define DCC_RR_XSP  (DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_ESP)
#define DCC_RR_XSI  (DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_ESI)
#define DCC_RR_XDI  (DCC_RC_I32|DCC_RC_I16|DCC_ASMREG_EDI)
#endif
#endif

#define DCC_VSTACK_CALL  DCC_ATTRIBUTE_FASTCALL

DCCFUN DCC(tyid_t) DCC_VSTACK_CALL DCC_RC_GETTYPE(DCC(rc_t) rc);
DCCFUN DCC(target_siz_t) DCC_VSTACK_CALL DCC_RC_SIZE(DCC(rc_t) rc);
DCCFUN DCC(rc_t) DCC_VSTACK_CALL DCC_RC_FORTYPE(struct DCCType const *__restrict t);
DCC_LOCAL DCC(rc_t) DCC_RC_FORSIZE(size_t n) {
 switch (n) {
#ifdef DCC_RC_I64
  case 4:  return DCC_RC_I8|DCC_RC_I16|DCC_RC_I32;
#endif
  case 2:  return DCC_RC_I8|DCC_RC_I16;
  case 1:  return DCC_RC_I8;
  default: break;
 }
#ifdef DCC_RC_I64
 return DCC_RC_I8|DCC_RC_I16|DCC_RC_I32|DCC_RC_I64;
#else
 return DCC_RC_I8|DCC_RC_I16|DCC_RC_I32;
#endif
}



union DCCConstValue {
 int8_t  s8;  uint8_t  u8;
 int16_t s16; uint16_t u16;
 int32_t s32; uint32_t u32;
 int64_t s64; uint64_t u64;
 int  i; unsigned int ui;
 long l; unsigned long ul;
 long long ll; unsigned long long ull;
 int32_t s64_2[2]; uint32_t u64_2[2];
 signed s; unsigned u;
 float f; double d; long double ld;
 void *p;
#ifdef __INTELLISENSE__
 int_t it;
 float_t flt;
 target_off_t offset; /* Used As offset by symbols. */
 target_ptr_t ptr;
#else
 DCC(int_t) it;
 DCC(float_t) flt;
 DCC(target_off_t) offset; /* Used As offset by symbols. */
 DCC(target_ptr_t) ptr;
#endif
};

#define DCC_SFLAG_NONE           0x00000000
#define DCC_SFLAG_LVALUE         0x00000001 /*< The stack value is an l-value.
                                             *  When 'sv_reg' describes a register, 'sv_const' is used as
                                             *  an offset from that register, to use in r/m-style operands. */
#define DCC_SFLAG_RVALUE         0x00000100 /*< The stack value is an r-value. (TODO: Currently not implemented)
                                             *  Unlike 'DCC_SFLAG_LVALUE', this flag does _NOT_ change the code
                                             *  generated by using this stack-value, but simply enables additional
                                             *  warnings when code attempts to assign to this value, or performs
                                             *  in-place operations while the 'DCC_SFLAG_COPY' flag isn't set. */
#define DCC_SFLAG_XOFFSET        0x00000002 /*< Used for register+offset pairs: The specified offset (when non-ZERO) must be added to the register before the value is popped. */
#define DCC_SFLAG_XREGISTER      0x00000004 /*< Set for explicit register references stated by the user. - The stack value should not be kill if at all possible! */
#define DCC_SFLAG_COPY           0x00000008 /*< The associated value must be copied before being written to (aka. COW-semantics).
                                             *  Since all stack-value operations are in-place, this flag is very important for implementing
                                             *  the difference between 'x+y' and 'x += y' (Simply set this flag for 'x' in 'x+y') */

#define DCC_SFLAG_TEST           0x00000010 /*< The stack value is the result of a test. */
#define DCC_SFLAG_TEST_MASK      0xf0000000 /*< Mask for test ids (One of 'DCC_TEST_*'). */
#define DCC_SFLAG_TEST_SHIFT     28         /*< Shift for test ids. */
#define DCC_SFLAG_MKTEST(t)     (DCC_SFLAG_TEST|(((t) << DCC_SFLAG_TEST_SHIFT)&DCC_SFLAG_TEST_MASK))
#define DCC_SFLAG_GTTEST(f)     (((f)&DCC_SFLAG_TEST_MASK) >> DCC_SFLAG_TEST_SHIFT)

#define DCC_SFLAG_BITFLD         0x00000020 /*< The stack value describes a bit-field. (both off and siz are 0..63) */
#define DCC_SFLAG_BITOFF_SHIFT   26         /*< Shift for accessing the bitfield offset (aka. bit-shift). */
#define DCC_SFLAG_BITOFF_MASK    0xfc000000 /*< Mask for accessing the bitfield offset (aka. bit-shift). */
#define DCC_SFLAG_BITSIZ_SHIFT   20         /*< Shift for accessing the bitfield size. */
#define DCC_SFLAG_BITSIZ_MASK    0x03f00000 /*< Mask for accessing the bitfield size. */
#define DCC_SFLAG_MKBITFLD(o,s) (DCC_SFLAG_BITFLD|(((DCC(sflag_t))(o) << DCC_SFLAG_BITOFF_SHIFT)&DCC_SFLAG_BITOFF_MASK)\
                                                 |(((DCC(sflag_t))(s) << DCC_SFLAG_BITSIZ_SHIFT)&DCC_SFLAG_BITSIZ_MASK))
#define DCC_SFLAG_GTBITSIZ(f) (((f)&DCC_SFLAG_BITSIZ_MASK) >> DCC_SFLAG_BITSIZ_SHIFT)
#define DCC_SFLAG_GTBITOFF(f) (((f)&DCC_SFLAG_BITOFF_MASK) >> DCC_SFLAG_BITOFF_SHIFT)

#define DCC_SFLAG_NO_WSIGN       0x00000040 /*< Don't emit warnings about loss of sign in a constant during signed->unsigned casts.
                                             *  Normally, a warning is emit when the sign-bit of a constant is set, yet consider
                                             *  the following case:
                                             *  >> unsigned int x = ~(0x1|0x2); // This constant has the sign bit set, but it wouldn't make sense to warn here...
                                             *  >> unsigned int y = -4;         // Now here, we should warn about the assignment.
                                             *  NOTE: You should realize that 'x' and 'y' are assigned the same value... */
#define DCC_SFLAG_DO_WUNUSED     0x00000080 /*< Emit warnings when the stack-value is unused.
                                             *  A use qualifies as the value being:
                                             *   - The source operand in a binary/store operation
                                             *   - The target operand in a unary/binary/store operation (Only if the 'DCC_SFLAG_COPY|DCC_SFLAG_RVALUE' flags aren't set)
                                             *   - An argument operand during a function call. */

typedef uint32_t DCC(sflag_t); /*< Stack value flag (Set of 'DCC_SFLAG_*'). */

struct DCCStackValue {
 /* A value on the stack. */
 struct DCCType        sv_ctype; /*< C Type of this value. */
#ifdef __INTELLISENSE__
 sflag_t               sv_flags; /*< Stack-value flags. */
 rc_t                  sv_reg;   /*< First associated register, or location (One of 'DCC_ASMREG_*').
                                  *  This value determines what 'sv_const' and 'sv_sym' are used for. */
 rc_t                  sv_reg2;  /*< Second register, or 'DCC_RC_CONST' when not used. */
#else
 DCC(sflag_t)          sv_flags; /*< Stack-value flags. */
 DCC(rc_t)             sv_reg;   /*< First associated register, or location (One of 'DCC_ASMREG_*').
                                  *  This value determines what 'sv_const' and 'sv_sym' are used for. */
 DCC(rc_t)             sv_reg2;  /*< Second register, or 'DCC_RC_CONST' when not used. */
#endif
 union DCCConstValue   sv_const; /*< Constant value, offset from a symbol/register, or stack location offset.
                                  *  The meaning of this value depends on 'sv_reg&DCC_RC_MASK'. */
 /*ref*/struct DCCSym *sv_sym;   /*< [0..1] Symbol used as base address for offsets.
                                  *   NOTE: Must only be a reference within the vstack. */
};

#define DCCStackValue_ISCONST(self) \
 ((self)->sv_reg == DCC_RC_CONST && !((self)->sv_flags&DCC_SFLAG_LVALUE))
#define DCCStackValue_MUSTCOPY(self) \
  (((self)->sv_flags&DCC_SFLAG_COPY) || /* Must copy protected registers ESP/EBP. */\
 (!((self)->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_XREGISTER)) \
  && DCCStackValue_ISPROTECTED(self)))
#define DCCStackValue_ISPROTECTED(self) \
 ((((self)->sv_reg &(DCC_RC_I3264|DCC_RC_I16)) && DCC_ASMREG_ISSPTR((self)->sv_reg &DCC_RI_MASK)) ||\
  (((self)->sv_reg2&(DCC_RC_I3264|DCC_RC_I16)) && DCC_ASMREG_ISSPTR((self)->sv_reg2&DCC_RI_MASK)))


/* Kill a given stack value by saving it on the stack,
 * then redirecting it to point to that saved value.
 * >> // Before
 * >> int x = 10+20;
 * >> int y = x+30;
 * >> // After application to '10' and 'x'
 * >> int const a = 10;
 * >> int x = a+20;
 * >> int &const _x = x; // May not be proper c/c++, but mirrors the internal representation
 * >> int y = _x+30; */
DCCFUN void DCC_VSTACK_CALL DCCStackValue_Kill(struct DCCStackValue *__restrict self);
DCCFUN void DCC_VSTACK_CALL DCCStackValue_Cow(struct DCCStackValue *__restrict self);

/* The opposite of 'DCCStackValue_Kill': Load the value into a register.
 * After a call to this function, 'self' is never an lvalue! */
DCCFUN void DCC_VSTACK_CALL DCCStackValue_Load(struct DCCStackValue *__restrict self);
DCCFUN void DCC_VSTACK_CALL DCCStackValue_LoadClass(struct DCCStackValue *__restrict self, DCC(rc_t) rc, int allow_ptr_regs);
DCCFUN void DCC_VSTACK_CALL DCCStackValue_LoadExplicit(struct DCCStackValue *__restrict self, DCC(rc_t) rcr);

/* Load the value of a bitfield, either into a register, or onto the stack. */
DCCFUN void DCC_VSTACK_CALL DCCStackValue_FixBitfield(struct DCCStackValue *__restrict self);

/* Load the boolean result of a test, either into a register, or onto the stack. */
DCCFUN void DCC_VSTACK_CALL DCCStackValue_FixTest(struct DCCStackValue *__restrict self);

/* Convert an array type to pointer-to-array-base, and function to pointer-to-function */
DCCFUN void DCC_VSTACK_CALL DCCStackValue_Promote(struct DCCStackValue *__restrict self);
DCCFUN void DCC_VSTACK_CALL DCCStackValue_PromoteFunction(struct DCCStackValue *__restrict self);

/* Perform integer promotion, as required by the C standard in practically any unary/binary operation.
 * Note, that v-stack API functions normally will _NOT_ do this, leaving the
 * task of calling this function on the appropriate stack-entires to the user. */
#if (DCC_TARGET_SIZEOF_CHAR < DCC_TARGET_SIZEOF_INT) || \
    (DCC_TARGET_SIZEOF_SHORT < DCC_TARGET_SIZEOF_INT) || \
    (DCC_TARGET_SIZEOF_LONG_LONG < DCC_TARGET_SIZEOF_INT)
DCCFUN void DCC_VSTACK_CALL DCCStackValue_PromoteInt(struct DCCStackValue *__restrict self);
#else
#define DCCStackValue_PromoteInt(self) (void)0
#endif

/* Resolve all 'self->sv_ctype' lvalue-style references. */
DCCFUN void DCC_VSTACK_CALL DCCStackValue_LoadLValue(struct DCCStackValue *__restrict self);

/* Check if the given stack value 'value' can implicitly be cast to the given type 'type'.
 * @return: 0 : The cast is allowed.
 * @return: * : One of 'W_CAST_*' */
DCCFUN int DCC_VSTACK_CALL
DCCStackValue_AllowCast(struct DCCStackValue const *__restrict value,
                        struct DCCType const *__restrict type,
                        int explicit_cast);


struct DCCVStack {
 /* A downwards-growing virtual stack of C operands.
  * >> Using this, DCC simplifies abstract register
  *    operations into something much more comprehensive:
  * >> vpush(&x);  // x
  * >> vpush(&x);  // x, x
  * >> vdup(1);    // x, x, dx
  * >> vswap();    // x, dx, x
  * >> vpop(1);     // x, dx
  * >> vpushi(42); // x, dx, 42
  * >> vgen2('+'); // x, dx+42
  * >> vstore();   // x
  * >> vpop(1);     // .
  * May generate something like this:
  * >> mov -8(%ebp), %eax
  * >> add $42, %eax
  * >> mov %eax, -8(%ebp)
  * HINT: DCC also uses the vstack to track
  *       what CPU registers are allocated.
  */
 struct DCCStackValue *v_begin;  /*< [0..1][<= v_bottom][owned] Lowest, valid stack entry. */
 struct DCCStackValue *v_end;    /*< [0..1][>= v_bottom] First invalid vstack address, following the bottom-most entry. */
 struct DCCStackValue *v_bottom; /*< [0..1][>= v_begin && <= v_end] Current position (aka. bottom of the stack). */
};


/* VStack commands.
 * NOTE: All unary/binary operations behave as in-place. */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Pop(int del);                                        /* -1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Push(struct DCCStackValue const *__restrict sval);   /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushInt(DCC(tyid_t) type, DCC(int_t) v);             /* +1 ('type': 'DCCTYPE_*') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushFlt(DCC(tyid_t) type, DCC(float_t) v);           /* +1 ('type': 'DCCTYPE_*') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushCst(struct DCCType const *__restrict type, DCC(int_t) v); /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushSym(struct DCCSym *__restrict sym);              /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushSymt(struct DCCType const *__restrict type, struct DCCSym *__restrict sym); /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushDecl(struct DCCDecl *__restrict decl);           /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushStr(char const *__restrict p, size_t s);         /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushReg(DCC(rc_t) reg);                              /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushRegs(DCC(rc_t) reg, DCC(rc_t) reg2);             /* +1 (When 'reg2 != DCC_RC_CONST', push a register pair as 64-bit integer) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushXReg(DCC(rc_t) reg);                             /* +1 (Same as 'DCCVStack_PushReg', but push as explicit, allowing for questionable operations such as '%ebp += 42') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushAddr(struct DCCSection *__restrict sec, DCC(target_ptr_t) addr); /* +1  (Push a section-relative address as a void-pointer) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushSizeof(struct DCCType const *__restrict t);      /* +1 (Also pushes the correct size for VLA types) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PushReturn(struct DCCDecl const *funty_decl);        /* +1 (Pushes the register/memory location used to store the return type of a function type 'funty_decl'. When 'fundecl' is NULL or not a function type, the default register is pushed as 'int') */
#define                     DCCVStack_PushVoid() DCCVStack_PushInt(DCCTYPE_VOID,0)         /* +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Dup(int copy);                                       /* -1, +2 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_ReplaceCopy(void);                                   /* -1, +1 (Same as calling 'vdup(1),vswap(),vpop(1)') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Swap(void);                                          /* -2, +2 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Unary(DCC(tok_t) op);                                /* -1, +1 ('*', '&', '!', '-', '~') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Binary(DCC(tok_t) op);                               /* -2, +1 ('+', '-', '|', '&', '^', TOK_INC, TOK_DEC, '*', '/', '%', TOK_SHL, TOK_SHR, '?') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Cast(struct DCCType const *__restrict t, int explicit_case); /* -1, +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Store(int initial_store);                            /* -2, +1 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Call(size_t n_args);                                 /* -n_args, -1, +1 (NOTE: Args are popped first, and in reverse, meaning that the last argument should be in 'vbottom') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Jcc(int invert);                                     /* -2 (Jump to 'vbottom[0]' if 'vbottom[1]^invert' is true) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Jmp(void);                                           /* -1 (Jump to 'vbottom[0]') */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Bitfldf(DCC(sflag_t) flags);                         /* -1, +1 */
#define                     DCCVStack_Bitfld(shift,size) DCCVStack_Bitfldf(DCC_SFLAG_MKBITFLD(shift,size)) /* -1, +1 */
DCCFUN int  DCC_VSTACK_CALL DCCVStack_IsSame(int same_declaration);                        /* -2, +2 (Return non-ZERO if 'vbottom[0]' and 'vbottom[1]' contain the same value) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_Subscript(struct TPPKeyword const *__restrict name); /* -1, +1 (Access the a member 'name' in the current structure) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_PromInt2(void);                                      /* -2, +2 (Promote integer types between the top 2 stack values, conforming to STD-C conventions for binary operations) */

/* Rotate 'n' stack entries left (towards to v_bottom) by 1 slots,
 * placing the previously bottom-most entry at index 'n'
 * WARNING: The stack layout notation used everything describes
 *          the opposite as it denotes an upwards-growing stack.
 *          Simply invert left/right when going by those notations!
 * e.g.: >> vpush(a); // a
 *       >> vpush(b); // a, b
 *       >> vpush(c); // a, b, c
 *       >> vlrot(3); // c, a, b  // Notation-wise, the stack was moved right (c fell
 *       >>           //          // off the end), but because it growns downwards, we
 *       >>           //          // must actually move left to achieve the same result.
 *       >> vpop(1);  // c, a
 *       >> vpop(1);  // c
 *       >> vpop(1);  // .
 */
DCCFUN void DCC_VSTACK_CALL DCCVStack_LRot(size_t n); /* -n, +n */

/* Rotate 'n' stack entries right (away from v_bottom) by 1 slots,
 * placing the previously n'th entry at the bottom.
 * WARNING: The stack layout notation used everything describes
 *          the opposite as it denotes an upwards-growing stack.
 *          Simply invert left/right when going by those notations!
 * e.g.: <see above> */
DCCFUN void DCC_VSTACK_CALL DCCVStack_RRot(size_t n); /* -n, +n */


/* Return a register of class 'rc' to the caller.
 * The current value stack is searched for unused registers of the given
 * register class, and a suitable unused register is returned, or a
 * previous use of the register is stored on the stack.
 * WARNING: Remember that some higher-order registers don't imply all
 *          underlying classes (s.a.: '%esp is expressed as' above)
 * @param: rc: The register class to look for.
 *             Additionally, you may or a register id to this
 *             in order to specify a preferred register.
 * @param: allow_ptr_regs: When ONE(1), allow %EDI and %ESI registers
 *                         Note that DI/SI can't take on the role of an 8-bit register!
 * @return: * : The fully capable register id + class + optional implied classes, matching 'rc'. */
DCCFUN DCC(rc_t) DCC_VSTACK_CALL DCCVStack_GetReg(DCC(rc_t) rc, int allow_ptr_regs);

/* Similar to 'DCCVStack_GetReg', but instead of allocating any register
 * from a given register class, this function will allocate the _exact_
 * specified register, meaning that it will always return 'rcr', but
 * with any higher-order registers also filling in lower-order ones.
 * WARNING: Remember that some higher-order registers don't imply all
 *          underlying classes (s.a.: '%esp is expressed as' above)
 * @param: rcr: A _single_ register class, or'd together with one 'DCC_ASMREG_*' value.
 * @return: * : The fully capable register id + class + optional implied classes, matching 'rc'. */
DCCFUN DCC(rc_t) DCC_VSTACK_CALL DCCVStack_GetRegExact(DCC(rc_t) rcr);

/* Similar to 'DCCVStack_GetReg', but only allow registers set in 'wanted_set':
 * >> rc_t reg;
 * >> uint8_t set = 0;
 * >> set |= (1 << DCC_ASMREG_EAX);
 * >> set |= (1 << DCC_ASMREG_EDX);
 * >> reg = DCCVStack_GetRegOf(DCC_RC_I32,set);
 * >> // Reg is now either EAX, or EDX */
DCCFUN DCC(rc_t) DCC_VSTACK_CALL DCCVStack_GetRegOf(DCC(rc_t) rc, uint8_t wanted_set);

/* Returns true if the given register is in use. */
DCCFUN int DCC_VSTACK_CALL DCCVStack_GetRegInuse(DCC(rc_t) rcr);

/* Kill all non-explicit (aka. without the 'DCC_SFLAG_XREGISTER' flag)
 * stack values that are referencing the specified register/class 'rcr'
 *  - This function is called to free registers when the 
 *    user specifies an explicit register variable:
 * >> %eax = %eax+10; // The second reference to 'EAX' will not
 * >>                 // kill the first one, though internal
 * >>                 // uses of EAX will have already been
 * >>                 // killed by the first reference.
 * @return: * : Simply returns 'rcr' (Symbolic for the freed state of the register).
 */
DCCFUN DCC(rc_t) DCC_VSTACK_CALL DCCVStack_KillXNon(DCC(rc_t) rcr);

/* Cast the given register 'reg' to a register of class 'new_class',
 * in the process moving the contents of 'reg' to that new register
 * while truncating or zero-/sign extending the original value.
 * @return: * : The register+class of the new value. */
DCCFUN DCC(rc_t) DCC_VSTACK_CALL DCCVStack_CastReg(DCC(rc_t) reg, int reg_unsigned, DCC(rc_t) new_class);

/* Kill all registers. (Performed before calling a function) */
DCCFUN void DCC_VSTACK_CALL DCCVStack_KillAll(void);
DCCFUN void DCC_VSTACK_CALL DCCVStack_KillTst(void);


#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
extern struct DCCVStack      vstack;
extern size_t                vsize;
extern struct DCCStackValue *vbottom;
#else
#define vstack             compiler.c_vstack
#define vsize     (size_t)(compiler.c_vstack.v_end-compiler.c_vstack.v_bottom)
#define vbottom            compiler.c_vstack.v_bottom 
#endif

/* Shorthand names for vstack operations. */
#define vpop       DCCVStack_Pop
#define vpush      DCCVStack_Push
#define vpushi     DCCVStack_PushInt
#define vpushf     DCCVStack_PushFlt
#define vpushc     DCCVStack_PushCst
#define vpushr     DCCVStack_PushReg
#define vpushxr    DCCVStack_PushXReg
#define vpushs     DCCVStack_PushSym
#define vpushst    DCCVStack_PushSymt
#define vpushd     DCCVStack_PushDecl
#define vpushv     DCCVStack_PushVoid
#define vpushstr   DCCVStack_PushStr
#define vdup       DCCVStack_Dup
#define vrcopy     DCCVStack_ReplaceCopy
#define vrval()   (void)(vbottom->sv_flags |= DCC_SFLAG_RVALUE)
#define vlrot      DCCVStack_LRot
#define vrrot      DCCVStack_RRot
#define vswap      DCCVStack_Swap
#define vgen1      DCCVStack_Unary
#define vgen2      DCCVStack_Binary
#define vcast      DCCVStack_Cast
#define vstore     DCCVStack_Store
#define vcall      DCCVStack_Call
#define vjcc       DCCVStack_Jcc
#define vjmp       DCCVStack_Jmp
#define vbitfld    DCCVStack_Bitfld
#define vbitfldf   DCCVStack_Bitfldf
#define vsubscript DCCVStack_Subscript

#define vprom()    DCCStackValue_Promote(vbottom)
#define vpromi()   DCCStackValue_PromoteInt(vbottom)
#define vpromi2()  DCCVStack_PromInt2()

#define vcast_pt(id,explicit_cast) \
 vcast((id)&DCCTYPE_CONST ? &DCCType_BuiltinConstPointers[(id)&15]\
                          : &DCCType_BuiltinPointers     [(id)&15]\
       ,explicit_cast)

DCC_LOCAL void vcast_t(DCC(tyid_t) id, int explicit_cast) {
 struct DCCType t;
 t.t_type = id,t.t_base = NULL;
 vcast(&t,explicit_cast);
}

/* Query the is-const state of vbottom.
 * NOTE: Due to relocation, there is a difference
 *       between a constant bool and a constant it!
 */
#define visconst_bool() \
 (!(vbottom->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST|DCC_SFLAG_BITFLD)) && \
   (vbottom->sv_reg == DCC_RC_CONST))
#define vgtconst_bool() (vbottom->sv_sym ? 1 : !!vbottom->sv_const.it)

#define visconst_int() \
 (!(vbottom->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST|DCC_SFLAG_BITFLD)) && \
   (vbottom->sv_reg == DCC_RC_CONST) && \
  !(vbottom->sv_sym))
#define vgtconst_int() (vbottom->sv_const.it)

/* Check if vbottom is a constant expression value (symbol+offset) */
#define visconst_xval() \
 (!(vbottom->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST|DCC_SFLAG_BITFLD)) && \
   (vbottom->sv_reg == DCC_RC_CONST))

#endif /* DCC_PRIVATE_API */


DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_H */
