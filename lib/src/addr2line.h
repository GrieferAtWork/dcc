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
#ifndef GUARD_DCC_LIB_SRC_ADDR2LINE_H
#define GUARD_DCC_LIB_SRC_ADDR2LINE_H 1

#include <stddef.h>
#include <stdint.h>
#include <string.h>

#ifdef DCC_PRIVATE_API
#include <dcc/common.h>
#include <dcc/target.h>
#endif


#ifdef DCC_BUILDING_A2L_RUNTIME
#define A2L_NAME(x) __dcc_dbg_##x
#define A2L_TYPE(x) x
#define A2L_DECL    [[visibility("hidden")]]
#define A2L_IMPL    [[visibility("hidden")]]
#else
#define A2L_NAME(x) DCC_##x
#define A2L_TYPE    DCC
#define A2L_DECL    DCCFUN
#define A2L_IMPL    PUBLIC
#endif


#ifdef __cplusplus
extern "C" {
#endif

#define A2L_STRING_SECTION   ".string"

typedef uint8_t              A2L_TYPE(a2l_op_t);
#ifdef DCC_BUILDING_A2L_RUNTIME
/* Configure for host-mode. */
typedef uintptr_t            A2L_TYPE(a2l_addr_t);
#define A2L_SIZEOF_ADDR      __SIZEOF_POINTER__
#else
/* Configure for target-mode. */
typedef target_ptr_t         A2L_TYPE(a2l_addr_t);
#define A2L_SIZEOF_ADDR      DCC_TARGET_SIZEOF_POINTER
#endif

/* NOTE: The 'a2l_arg_t' type is both signed & unsigned at the same time! */
#if A2L_SIZEOF_ADDR <= 4
#define A2L_ARG_MAXBYTES     5 /* ceildiv((4*8),7) */
typedef uint32_t             A2L_TYPE(a2l_arg_t);
typedef int32_t              A2L_TYPE(a2l_sarg_t);
#else
#define A2L_ARG_MAXBYTES    10 /* ceildiv((8*8),7) */
typedef uint64_t             A2L_TYPE(a2l_arg_t);
typedef int64_t              A2L_TYPE(a2l_sarg_t);
#endif

typedef A2L_TYPE(a2l_sarg_t) A2L_TYPE(a2l_line_t);
typedef A2L_TYPE(a2l_sarg_t) A2L_TYPE(a2l_col_t);
typedef A2L_TYPE(a2l_arg_t)  A2L_TYPE(a2l_path_t);
typedef A2L_TYPE(a2l_arg_t)  A2L_TYPE(a2l_file_t);
typedef A2L_TYPE(a2l_arg_t)  A2L_TYPE(a2l_name_t);


/* A sign-byte 'A2L_A_NEG' must be prefixed and 'A2L_ARG_NEGATE(a)' must be written. */
#define A2L_ARG_NEEDSIGN(a) \
   ((A2L_TYPE(a2l_sarg_t))(a) < 0 && \
  (-(A2L_TYPE(a2l_sarg_t))(a) <= ((A2L_TYPE(a2l_arg_t))1 << (A2L_ARG_MAXBYTES-1)*7)))
#define A2L_ARG_NEGATE(a) (A2L_TYPE(a2l_arg_t))(-(A2L_TYPE(a2l_sarg_t))(a))


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4201)
#endif
struct A2lState {
 /* WARNING: The order of the following memory is important! */
union{struct{
 A2L_TYPE(a2l_line_t) s_line;
 A2L_TYPE(a2l_col_t)  s_col;
 A2L_TYPE(a2l_path_t) s_path;
 A2L_TYPE(a2l_file_t) s_file;
 A2L_TYPE(a2l_name_t) s_name;
}; 
 A2L_TYPE(a2l_arg_t)  s_data[5];
};
 /* ---end of order-dependency--- */
#define A2L_STATE_HASLINE 0x01
#define A2L_STATE_HASCOL  0x02
#define A2L_STATE_HASPATH 0x04 /*< 's_path' is filled with a meaningful value. */
#define A2L_STATE_HASFILE 0x08 /*< 's_file' is filled with a meaningful value. */
#define A2L_STATE_HASNAME 0x10 /*< 's_name' is filled with a meaningful value. */
 uint32_t             s_features; /*< Currently available features (set of 'A2L_STATE_*') */
 A2L_TYPE(a2l_addr_t) s_addr;
};
#ifdef _MSC_VER
#pragma warning(pop)
#endif

#define A2lState_RESET(self)      memset((self),0,sizeof(struct A2lState))
#define A2lState_SETF(self,f)   ((self)->s_features |=  (f))
#define A2lState_UNSETF(self,f) ((self)->s_features &= ~(f))

#define A2lState_DEL_L(self)     ((self)->s_line = 0,A2lState_UNSETF(self,A2L_STATE_HASLINE))
#define A2lState_DEL_C(self)     ((self)->s_col  = 0,A2lState_UNSETF(self,A2L_STATE_HASCOL))
#define A2lState_DEL_P(self)     ((self)->s_path = 0,A2lState_UNSETF(self,A2L_STATE_HASPATH))
#define A2lState_DEL_F(self)     ((self)->s_file = 0,A2lState_UNSETF(self,A2L_STATE_HASFILE))
#define A2lState_DEL_N(self)     ((self)->s_name = 0,A2lState_UNSETF(self,A2L_STATE_HASNAME))
#define A2lState_DEL_LCPFN(self) ((self)->s_line = 0,(self)->s_col = 0,\
                                  (self)->s_path = 0,(self)->s_file = 0,\
                                  (self)->s_name = 0,\
                                   A2lState_UNSETF(self,A2L_STATE_HASLINE|A2L_STATE_HASCOL|\
                                                        A2L_STATE_HASPATH|A2L_STATE_HASFILE|\
                                                        A2L_STATE_HASNAME))

/* Operand count flags. */
#define A2L_OPC_0     0x00
#define A2L_OPC_1     0x40
#define A2L_OPC_2     0x80
#define A2L_OPC_3     0xc0
#define A2L_OPCOUNT   4

/* Helper macros for get/set of operand count. */
#define A2L_GETOPC(op)   ((A2L_TYPE(a2l_op_t))(op) >> 6)
#define A2L_SETOPC(op,c) ((A2L_TYPE(a2l_op_t))(op) | (A2L_TYPE(a2l_op_t))((c)&3) << 6)

/* A2L predefined opcodes. */
#define A2L_O_EOF        0x00 /* >> found = (s->s_addr == capture); STOP();
                               * Terminate the A2L command chain.
                               * NOTE: _MUST_ Always be ZERO(0)! */
/* Delete opcodes (May be or'd together, or used as stand-alone) */
#define A2L_O_DEL_L      0x01 /* A2lState_DEL_L(s); */
#define A2L_O_DEL_C      0x02 /* A2lState_DEL_C(s); */
#define A2L_O_DEL_P      0x04 /* A2lState_DEL_P(s); */
#define A2L_O_DEL_F      0x08 /* A2lState_DEL_F(s); */
#define A2L_O_DEL_N      0x10 /* A2lState_DEL_N(s); */
/*                       0x20 */
/*                       .... */
/*                       0x3d */
#define A2L_O_SET_L      0x30 /* A2lState_SETF(s); */
#define A2L_O_RESET      0x3e /* A2lState_RESET(s); */
#define A2L_O_NOP        0x3f /* no-op */

#define A2L_O_IA         0x40 /* [CAPTURE] s->s_addr += ARG(0); */
#define A2L_O_IL         0x41 /*           s->s_line += ARG(0); A2lState_SETF(s,A2L_STATE_HASLINE); A2lState_DEL_C(s); */
#define A2L_O_IC         0x42 /*           s->s_col  += ARG(0); A2lState_SETF(s,A2L_STATE_HASCOL); */
/*                       0x43 */
/*                       .... */
/*                       0x4a */
/* WARNING: These opcodes encode the member order in 'struct A2lState' */
#define A2L_O_SL         0x4b /*           s->s_line  = ARG(0); A2lState_DEL_C(s); */
#define A2L_O_SC         0x4c /*           s->s_col   = ARG(0); A2lState_SETF(s,A2L_STATE_HASCOL); */
#define A2L_O_SP         0x4d /*           s->s_path  = ARG(0); A2lState_SETF(s,A2L_STATE_HASPATH); */
#define A2L_O_SF         0x4e /*           s->s_file  = ARG(0); A2lState_SETF(s,A2L_STATE_HASFILE); */
#define A2L_O_SN         0x4f /*           s->s_name  = ARG(0); A2lState_SETF(s,A2L_STATE_HASNAME); */

#define A2L_ISSTROP(o) ((o) >= A2L_O_SP && (o) <= A2L_O_SN)

#define A2L_O_NCLO_IA    0x50 /* [CAPTURE] s->s_col += 1+(op-A2L_O_NCLO_IA); A2lState_SETF(s,A2L_STATE_HASCOL); s->s_addr += ARG(0); */
#define A2L_O_NCHI_IA    0x5f /* ... */
#define A2L_MAX_NCIA    (1+(A2L_O_NCHI_IA-A2L_O_NCLO_IA))

#define A2L_O_NLLO_IA    0x60 /* [CAPTURE] s->s_line += 1+(op-A2L_O_NLLO_IA); A2lState_SETF(s,A2L_STATE_HASLINE); A2lState_DEL_C(s); s->s_addr += ARG(0); */
#define A2L_O_NLHI_IA    0x7f /* ... */
#define A2L_MAX_NLIA    (1+(A2L_O_NLHI_IA-A2L_O_NLLO_IA))

#define A2L_O_IL_IA      0x80 /* [CAPTURE] s->s_line += ARG(0); A2lState_SETF(s,A2L_STATE_HASLINE); A2lState_DEL_C(s); s->s_addr += ARG(1); */
#define A2L_O_IC_IA      0x81 /* [CAPTURE] s->s_col  += ARG(0); A2lState_SETF(s,A2L_STATE_HASCOL); s->s_addr += ARG(1); */
/*                       0x82 */
/*                       .... */
/*                       0x8f */
#define A2L_O_NLLO_SC_IA 0x90 /* [CAPTURE] s->s_line += 1+(op-A2L_O_NLLO_SC_IA); s->s_col = ARG(0); A2lState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr += ARG(1); */
#define A2L_O_NLHI_SC_IA 0xbf /* ... */
#define A2L_MAX_NLSCIA  (1+(A2L_O_NLHI_SC_IA-A2L_O_NLLO_SC_IA))

#define A2L_O_IL_SC_IA   0xc0 /* [CAPTURE] s->s_line += ARG(0); s->s_col = ARG(1); A2lState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr += ARG(2); */
/*                       0xc1 */
/*                       .... */
/*                       0xff */


#define A2L_A_CON 0x80 /*< [flag] When set, the next byte is still part of this operand. */
#define A2L_A_NEG 0x80 /*< Each leading byte equal to this causes the result to be negated. */
#define A2L_A_MAX 0x7f /*< The max value of a single-byte operand. */
#define A2L_A_SFT    7 /*< Bit-shift between operand bytes. */

A2L_DECL A2L_TYPE(a2l_arg_t)
A2L_NAME(a2l_getarg)(A2L_TYPE(a2l_op_t) const **__restrict pcode);

/* Execute code from a given addr2line state,
 * trying to capture the given address 'capture'.
 * @param: s_begin: [1..1][IN]:  The initial A2L state (usually 'A2lState_RESET()')
 *                        [OUT]: Filled with the closest A2L state before 'capture',
 *                               or when not found, the last state before EOF.
 * @param: s_end: [0..1][OUT]: When non-NULL, filled with the closest following A2L state.
 * @param: capture: The address of capture, or (when available) 'A2L_CAPTURE_ONE'
 *                  to stop at the next capture point after '*pcode'.
 * NOTE: When 's_begin' and 's_end' are equal, 's_begin' is either
 *       filled with the EOF state, or the same state 's_end' would
 *       have been filled with upon success.
 * @return: 0: The given address 'capture' has no debug information associated.
 * @return: 1: The given state 's' now contains the description for 'capture' */
A2L_DECL int
A2L_NAME(a2l_exec)(struct A2lState *s_begin,
                   struct A2lState *s_end,
                   A2L_TYPE(a2l_op_t) const **__restrict pcode,
                   A2L_TYPE(a2l_addr_t) capture);

#ifndef DCC_BUILDING_A2L_RUNTIME
#define A2L_CAPTURE_ONE ((A2L_TYPE(a2l_addr_t))-1)
#endif


#ifdef __cplusplus
}
#endif

#endif /* !GUARD_DCC_LIB_SRC_ADDR2LINE_H */
