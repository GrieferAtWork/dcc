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
typedef int32_t              A2L_TYPE(a2l_line_t);
typedef int32_t              A2L_TYPE(a2l_col_t);
#ifdef DCC_BUILDING_A2L_RUNTIME
/* Configure for host-mode. */
typedef uintptr_t            A2L_TYPE(a2l_addr_t);
#define A2L_SIZEOF_ADDR      __SIZEOF_POINTER__
#else
/* Configure for target-mode. */
typedef target_ptr_t         A2L_TYPE(a2l_addr_t);
#define A2L_SIZEOF_ADDR DCC_TARGET_SIZEOF_POINTER
#endif
typedef A2L_TYPE(a2l_addr_t) A2L_TYPE(a2l_path_t);
typedef A2L_TYPE(a2l_addr_t) A2L_TYPE(a2l_file_t);
typedef A2L_TYPE(a2l_addr_t) A2L_TYPE(a2l_name_t);

#if A2L_SIZEOF_ADDR <= 4
#define A2L_ARG_MAXBYTES     5 /* ceildiv((4*8),7) */
typedef uint32_t             A2L_TYPE(a2l_arg_t);
#else
#define A2L_ARG_MAXBYTES    10 /* ceildiv((8*8),7) */
typedef uint64_t             A2L_TYPE(a2l_arg_t);
#endif


struct A2LState {
 A2L_TYPE(a2l_addr_t) s_addr;
 A2L_TYPE(a2l_line_t) s_line;
 A2L_TYPE(a2l_col_t)  s_col;
 A2L_TYPE(a2l_path_t) s_path;
 A2L_TYPE(a2l_file_t) s_file;
 A2L_TYPE(a2l_name_t) s_name;
#define A2L_STATE_HASLINE 0x01
#define A2L_STATE_HASCOL  0x02
#define A2L_STATE_HASPATH 0x04 /*< 's_path' is filled with a meaningful value. */
#define A2L_STATE_HASFILE 0x08 /*< 's_file' is filled with a meaningful value. */
#define A2L_STATE_HASNAME 0x10 /*< 's_name' is filled with a meaningful value. */
 uint32_t   s_features; /*< Currently available features (set of 'A2L_STATE_*') */
};

#define A2LState_RESET(self)      memset(&(self)->s_addr,0,sizeof(struct A2LState)-offsetof(struct A2LState,s_addr))
#define A2LState_SETF(self,f)   ((self)->s_features |=  (f))
#define A2LState_UNSETF(self,f) ((self)->s_features &= ~(f))

#define A2LState_DEL_L(self)     ((self)->s_line = 0,A2LState_UNSETF(self,A2L_STATE_HASLINE))
#define A2LState_DEL_C(self)     ((self)->s_col  = 0,A2LState_UNSETF(self,A2L_STATE_HASCOL))
#define A2LState_DEL_P(self)     ((self)->s_path = 0,A2LState_UNSETF(self,A2L_STATE_HASPATH))
#define A2LState_DEL_F(self)     ((self)->s_file = 0,A2LState_UNSETF(self,A2L_STATE_HASFILE))
#define A2LState_DEL_N(self)     ((self)->s_name = 0,A2LState_UNSETF(self,A2L_STATE_HASNAME))
#define A2LState_DEL_LCPFN(self) ((self)->s_line = 0,(self)->s_col = 0,\
                                  (self)->s_path = 0,(self)->s_file = 0,\
                                  (self)->s_name = 0,\
                                   A2LState_UNSETF(self,A2L_STATE_HASLINE|A2L_STATE_HASCOL|\
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
#define A2L_O_EOF      0x00 /* Terminate the A2L command chain. NOTE: _MUST_ Always be ZERO(0)! */
#define A2L_O_RESET    0x20 /* addr = 0,line = 0,col = 0,path = 0,file = 0,has_line = 0,has_col = 0; */
#define A2L_O_NOP      0x21 /* no-op */

/* Delete opcodes (May be or'd together, or used as stand-alone) */
#define A2L_O_DEL_L    0x01 /* A2LState_DEL_L(s); */
#define A2L_O_DEL_C    0x02 /* A2LState_DEL_C(s); */
#define A2L_O_DEL_F    0x04 /* A2LState_DEL_P(s); */
#define A2L_O_DEL_P    0x08 /* A2LState_DEL_F(s); */
#define A2L_O_DEL_N    0x10 /* A2LState_DEL_N(s); */

/* TODO: Redo this and get rid of any way of decrementing the address. (Other than a reset?) */

#define A2L_O_IA       0x40 /* [CAPTURE] s->s_addr += ARG(0); */
#define A2L_O_DA       0x41 /* [CAPTURE] s->s_addr -= ARG(0); */
#define A2L_O_IL       0x42 /*           s->s_line += ARG(0); A2LState_DEL_C(s); */
#define A2L_O_DL       0x43 /*           s->s_line -= ARG(0); A2LState_DEL_C(s); */
#define A2L_O_IC       0x44 /*           s->s_col  += ARG(0); A2LState_SETF(s,A2L_STATE_HASCOL); */
#define A2L_O_DC       0x45 /*           s->s_col  -= ARG(0); A2LState_SETF(s,A2L_STATE_HASCOL); */

#define A2L_O_SL       0x47 /*           s->s_line  = ARG(0); A2LState_DEL_C(s); */
#define A2L_O_SC       0x48 /*           s->s_col   = ARG(0); A2LState_SETF(s,A2L_STATE_HASCOL); */
#define A2L_O_SP       0x49 /*           s->s_path  = ARG(0); A2LState_SETF(s,A2L_STATE_HASPATH); */
#define A2L_O_SF       0x4a /*           s->s_file  = ARG(0); A2LState_SETF(s,A2L_STATE_HASFILE); */
#define A2L_O_SN       0x4b /*           s->s_name  = ARG(0); A2LState_SETF(s,A2L_STATE_HASNAME); */

#define A2L_O_IL_IA    0x80 /* [CAPTURE] s->s_line += ARG(0); A2LState_SETF(s,A2L_STATE_HASLINE); A2LState_DEL_C(s); s->s_addr += ARG(1); */
#define A2L_O_DL_IA    0x81 /* [CAPTURE] s->s_line -= ARG(0); A2LState_SETF(s,A2L_STATE_HASLINE); A2LState_DEL_C(s); s->s_addr += ARG(1); */
#define A2L_O_SL_IA    0x82 /* [CAPTURE] s->s_line  = ARG(0); A2LState_SETF(s,A2L_STATE_HASLINE); A2LState_DEL_C(s); s->s_addr += ARG(1); */
#define A2L_O_IL_DA    0x83 /* [CAPTURE] s->s_line += ARG(0); A2LState_SETF(s,A2L_STATE_HASLINE); A2LState_DEL_C(s); s->s_addr -= ARG(1); */
#define A2L_O_DL_DA    0x84 /* [CAPTURE] s->s_line -= ARG(0); A2LState_SETF(s,A2L_STATE_HASLINE); A2LState_DEL_C(s); s->s_addr -= ARG(1); */
#define A2L_O_SL_DA    0x85 /* [CAPTURE] s->s_line  = ARG(0); A2LState_SETF(s,A2L_STATE_HASLINE); A2LState_DEL_C(s); s->s_addr -= ARG(1); */

#define A2L_O_IL_SC_IA 0xc0 /* [CAPTURE] s->s_line += ARG(0); s->s_col = ARG(1); A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr += ARG(2); */
#define A2L_O_DL_SC_IA 0xc1 /* [CAPTURE] s->s_line -= ARG(0); s->s_col = ARG(1); A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr += ARG(2); */
#define A2L_O_SL_SC_IA 0xc2 /* [CAPTURE] s->s_line  = ARG(0); s->s_col = ARG(1); A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr += ARG(2); */
#define A2L_O_IL_SC_DA 0xc3 /* [CAPTURE] s->s_line += ARG(0); s->s_col = ARG(1); A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr -= ARG(2); */
#define A2L_O_DL_SC_DA 0xc4 /* [CAPTURE] s->s_line -= ARG(0); s->s_col = ARG(1); A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr -= ARG(2); */
#define A2L_O_SL_SC_DA 0xc5 /* [CAPTURE] s->s_line  = ARG(0); s->s_col = ARG(1); A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL); s->s_addr -= ARG(2); */

#define A2L_A_CON 0x80 /*< [flag] When set, the next byte is still part of this operand. */
#define A2L_A_MAX 0x7f /*< The max value of a single-byte operand. */
#define A2L_A_SFT    7 /*< Bit-shift between operand bytes. */

A2L_DECL A2L_TYPE(a2l_arg_t)
A2L_NAME(a2l_getarg)(A2L_TYPE(a2l_op_t) const **__restrict pcode);

/* Execute code from a given addr2line state,
 * trying to capture the given address 'capture'.
 * @return: 0: The given address 'capture' has no debug information associated.
 * @return: 1: The given state 's' now contains the description for 'capture' */
A2L_DECL int
A2L_NAME(a2l_exec)(struct A2LState *__restrict s,
                   A2L_TYPE(a2l_op_t) const **__restrict pcode,
                   A2L_TYPE(a2l_addr_t) capture);

#ifdef __cplusplus
}
#endif

#endif /* !GUARD_DCC_LIB_SRC_ADDR2LINE_H */
