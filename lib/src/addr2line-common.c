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
#ifndef GUARD_DCC_LIB_SRC_ADDR2LINE_COMMON_C
#define GUARD_DCC_LIB_SRC_ADDR2LINE_COMMON_C 1

#include "addr2line.h"

#ifndef DCC_BUILDING_A2L_RUNTIME
#include <dcc/common.h>
#endif

#ifndef likely
#define likely(x)   (__builtin_expect(!!(x),1))
#define unlikely(x) (__builtin_expect(!!(x),0))
#endif

#ifndef DCC_MACRO_FALSE
#define DCC_MACRO_FALSE 0
#endif

#ifdef __cplusplus
extern "C" {
#endif

A2L_IMPL a2l_arg_t
A2L_NAME(a2l_getarg)(a2l_op_t const **__restrict pcode) {
 a2l_arg_t result = 0;
 a2l_op_t byte;
 a2l_op_t const *iter = *pcode;
 int neg_result = 0;
 for (;;) {
  byte   = *iter++;
  result = (result << A2L_A_SFT)|(byte&A2L_A_MAX);
  if (!(byte&A2L_A_CON)) break;
  /* when 'result' is ZERO at this point, 'byte' must be equal
   * to 'A2L_A_CON', which in turn is equal to 'A2L_A_NEG'.
   * With that in mind, we must trigger a result inversion. */
  if (!result) neg_result ^= 1;
 }
 *pcode = iter;
 if (neg_result) result = A2L_ARG_NEGATE(result);
 return result;
}

#if defined(__DCC_VERSION__) && 0
#define HAVE_LOG 1
extern int printf(char const *,...);
#define LOG(x) printf x
#else
#define LOG(x) (void)0
#endif

/* Execute code from a given addr2line state,
 * trying to capture the given address 'capture'.
 * @return: 0: The given address 'capture' has no debug information associated.
 * @return: 1: The given state 's' now contains the description for 'capture' */
A2L_IMPL int
A2L_NAME(a2l_exec)(struct A2lState *__restrict s,
                   A2L_TYPE(a2l_op_t) const **__restrict pcode,
                   A2L_TYPE(a2l_addr_t) capture) {
#define ARG() A2L_NAME(a2l_getarg)(&code)
 a2l_op_t const *code = *pcode;
 int result = 0;
 if likely(code) for (;;) {
  a2l_op_t op = *code++;
  switch (op) {
  case A2L_O_EOF: LOG(("EOF\n")); goto done;
  case A2L_O_RESET: LOG(("RESET\n")); A2lState_RESET(s); break;

  {
   a2l_addr_t old_addr,new_addr;
  case A2L_O_IA:
read_addr:
   old_addr  = s->s_addr;
   s->s_addr = new_addr = old_addr+ARG();
   LOG(("ADDR = %p\n",new_addr));
   if (new_addr >= old_addr) {
    LOG(("CHECK_ADDR(%p in %p...%p)\n",capture,old_addr,new_addr));
    if (capture > old_addr && capture <= new_addr) goto found;
   } else {
    LOG(("CHECK_ADDR(%p in %p...%p)\n",capture,new_addr,old_addr));
    if (capture > new_addr && capture <= old_addr) goto found;
   }
  } break;

  case A2L_O_IL: s->s_line += ARG(); A2lState_DEL_C(s); LOG(("LINE = %d\n",s->s_line)); break;
  case A2L_O_IC: s->s_col  += ARG(); A2lState_SETF(s,A2L_STATE_HASCOL); LOG(("COL = %d\n",s->s_col)); break;

  {
  case A2L_O_SL: case A2L_O_SC: case A2L_O_SP:
  case A2L_O_SF: case A2L_O_SN:
   *(a2l_arg_t *)((uintptr_t)s+(op-A2L_O_SL)*sizeof(a2l_arg_t)) = ARG();
#if HAVE_LOG
   if (op == A2L_O_SL) LOG(("LINE = %d\n",s->s_line));
   if (op == A2L_O_SC) LOG(("COL  = %d\n",s->s_col));
   if (op == A2L_O_SP) LOG(("PATH = %d\n",s->s_path));
   if (op == A2L_O_SF) LOG(("FILE = %d\n",s->s_file));
   if (op == A2L_O_SN) LOG(("NAME = %d\n",s->s_name));
#endif
  } break;

  {
  case A2L_O_IL_IA:
   s->s_line += ARG();
after_il_ia:
   A2lState_SETF(s,A2L_STATE_HASLINE);
   A2lState_DEL_C(s);
   LOG(("LINE = %d\n",s->s_line));
   goto read_addr;
  } break;

  {
  case A2L_O_IC_IA:
   s->s_col += ARG();
after_ic_ia:
   A2lState_SETF(s,A2L_STATE_HASCOL);
   LOG(("COL = %d\n",s->s_col));
   goto read_addr;
  } break;

  {
  case A2L_O_IL_SC_IA:
   s->s_line += ARG();
after_il_sc_ia:
   LOG(("LINE = %d\n",s->s_line));
   s->s_col = ARG();
   A2lState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL);
   LOG(("COL = %d\n",s->s_col));
   goto read_addr;
  } break;

  {
  default:
   if (op >= A2L_O_DEL_L && op <= A2L_O_DEL_N) {
#if (A2L_STATE_HASLINE == A2L_O_DEL_L) && \
    (A2L_STATE_HASCOL  == A2L_O_DEL_C) && \
    (A2L_STATE_HASPATH == A2L_O_DEL_P) && \
    (A2L_STATE_HASFILE == A2L_O_DEL_F) && \
    (A2L_STATE_HASNAME == A2L_O_DEL_N)
    s->s_features &= ~(uint32_t)(op);
    if (op&A2L_O_DEL_L) s->s_line = 0,LOG(("DELETE(LINE)\n"));
    if (op&A2L_O_DEL_C) s->s_col  = 0,LOG(("DELETE(COL)\n"));
    if (op&A2L_O_DEL_F) s->s_file = 0,LOG(("DELETE(FILE)\n"));
    if (op&A2L_O_DEL_P) s->s_path = 0,LOG(("DELETE(PATH)\n"));
    if (op&A2L_O_DEL_N) s->s_name = 0,LOG(("DELETE(NAME)\n"));
#else
    if (op&A2L_O_DEL_L) A2lState_DEL_L(s),LOG(("DELETE(LINE)\n"));
    if (op&A2L_O_DEL_C) A2lState_DEL_C(s),LOG(("DELETE(COL)\n"));
    if (op&A2L_O_DEL_F) A2lState_DEL_F(s),LOG(("DELETE(FILE)\n"));
    if (op&A2L_O_DEL_P) A2lState_DEL_P(s),LOG(("DELETE(PATH)\n"));
    if (op&A2L_O_DEL_N) A2lState_DEL_N(s),LOG(("DELETE(NAME)\n"));
#endif
   } else {
    unsigned int opc;
    /* Check for special opcode ranges used for short line/col jumps. */
    if (op >= A2L_O_NLLO_IA && op <= A2L_O_NLHI_IA) { s->s_line += op-(A2L_O_NLLO_IA-1); goto after_il_ia; }
    if (op >= A2L_O_NCLO_IA && op <= A2L_O_NCHI_IA) { s->s_col += op-(A2L_O_NLLO_IA-1); goto after_ic_ia; }
    if (op >= A2L_O_NLLO_SC_IA && op <= A2L_O_NLHI_SC_IA) { s->s_line += op-(A2L_O_NLLO_SC_IA-1); goto after_il_sc_ia; }
    /* Unknown opcode (Ignore). */
    opc = A2L_GETOPC(op);
    LOG((op == A2L_O_NOP ? "" : "<unknown: %d>\n",(int)op));
    while (opc--) ARG();
   }
  } break;
  }
 }
done: *pcode = code;
 return result;
found: result = 1; goto done;
#undef ARG
}

#ifdef __cplusplus
}
#endif

#endif /* !GUARD_DCC_LIB_SRC_COMMON_ADDR2LINE_C */
