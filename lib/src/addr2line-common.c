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

#if defined(__DCC_VERSION__) && 0
extern char const __dcc_dbg_strtab[] [[weak]] __asm__(A2L_STRING_SECTION);
extern int printf(char const *,...);
#if 1
#define LOG(x)         (void)0
#define LOG_CAPTURE(x)  printf x
#else
#define HAVE_LOG 1
#define LOG(x)          printf x
#define LOG_CAPTURE(x)  printf x
#endif
#else
#define LOG(x)         (void)0
#define LOG_CAPTURE(x) (void)0
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

/* Execute code from a given addr2line state,
 * trying to capture the given address 'capture'.
 * @return: 0: The given address 'capture' has no debug information associated.
 * @return: 1: The given state 's' now contains the description for 'capture' */
A2L_IMPL int
A2L_NAME(a2l_exec)(struct A2lState *s_begin,
                   struct A2lState *s_end,
                   A2L_TYPE(a2l_op_t) const **__restrict pcode,
                   A2L_TYPE(a2l_addr_t) capture) {
#define ARG() A2L_NAME(a2l_getarg)(&code)
 struct A2lState state;
 int result = 0;
 a2l_op_t const *code = *pcode;
 if likely(code) {
  state = *s_begin;
  LOG(("BEGIN: %p (%x)\n",code,*code));
  for (;;) {
   a2l_op_t op = *code++;
   switch (op) {
   case A2L_O_EOF: LOG(("[%x] EOF\n",op)); goto done;
   case A2L_O_RESET: LOG(("[%x] RESET\n",op)); A2lState_RESET(&state); break;

   {
    a2l_addr_t old_addr,new_addr;
   case A2L_O_IA:
read_addr:
    old_addr     = state.s_addr;
    state.s_addr = new_addr = old_addr+ARG();
    LOG(("[%x] ADDR = %p\n",op,new_addr));
#ifdef A2L_CAPTURE_ONE
    if (capture == A2L_CAPTURE_ONE) goto found;
#endif
    if (new_addr >= old_addr) {
     LOG_CAPTURE(("%s/%s(%d,%d) : [%x] CHECK_ADDR(%p in %p...%p)\n",
                  state.s_features&A2L_STATE_HASPATH ? __dcc_dbg_strtab+state.s_path : "??" "?",
                  state.s_features&A2L_STATE_HASFILE ? __dcc_dbg_strtab+state.s_file : "??" "?",
                  state.s_features&A2L_STATE_HASLINE ? state.s_line+1 : 0,
                  state.s_features&A2L_STATE_HASCOL  ? state.s_col+1 : 0,
                  op,capture,old_addr,new_addr));
     if (capture >= old_addr && capture < new_addr) goto found;
    } else {
     LOG_CAPTURE(("%s/%s(%d,%d) : [%x] CHECK_ADDR(%p in %p...%p)\n",
                  state.s_features&A2L_STATE_HASPATH ? __dcc_dbg_strtab+state.s_path : "??" "?",
                  state.s_features&A2L_STATE_HASFILE ? __dcc_dbg_strtab+state.s_file : "??" "?",
                  state.s_features&A2L_STATE_HASLINE ? state.s_line+1 : 0,
                  state.s_features&A2L_STATE_HASCOL  ? state.s_col+1 : 0,
                  op,capture,new_addr,old_addr));
     if (capture > new_addr && capture <= old_addr) goto found;
    }
    *s_begin = state;
   } break;

   case A2L_O_IL: state.s_line += ARG(); A2lState_DEL_C(&state); LOG(("[%x] LINE = %d\n",op,state.s_line)); break;
   case A2L_O_IC: state.s_col += ARG(); A2lState_SETF(&state,A2L_STATE_HASCOL); LOG(("[%x] COL = %d\n",op,state.s_col)); break;

   {
   case A2L_O_SL: case A2L_O_SC: case A2L_O_SP:
   case A2L_O_SF: case A2L_O_SN:
    op -= A2L_O_SL;
    state.s_data[op]  = ARG();
#ifndef __DCC_VERSION__
    if (op == A2L_O_SP-A2L_O_SL &&
        state.s_data[op] == 0x000000E3)
        for (;;);
#endif
    state.s_features |= 1 << op;
#if HAVE_LOG
    if (op == A2L_O_SL-A2L_O_SL) LOG(("[%x] LINE = %d\n",op+A2L_O_SL,state.s_line));
    if (op == A2L_O_SC-A2L_O_SL) LOG(("[%x] COL  = %d\n",op+A2L_O_SL,state.s_col));
    if (op == A2L_O_SP-A2L_O_SL) LOG(("[%x] PATH = %d (%s)\n",op+A2L_O_SL,state.s_path,__dcc_dbg_strtab+state.s_path));
    if (op == A2L_O_SF-A2L_O_SL) LOG(("[%x] FILE = %d (%s)\n",op+A2L_O_SL,state.s_file,__dcc_dbg_strtab+state.s_file));
    if (op == A2L_O_SN-A2L_O_SL) LOG(("[%x] NAME = %d (%s)\n",op+A2L_O_SL,state.s_name,__dcc_dbg_strtab+state.s_name));
#endif
   } break;

   {
   case A2L_O_IL_IA:
    state.s_line += ARG();
after_il_ia:
    A2lState_SETF(&state,A2L_STATE_HASLINE);
    A2lState_DEL_C(&state);
    LOG(("[%x] LINE = %d\n",op,state.s_line));
    goto read_addr;
   } break;

   {
   case A2L_O_IC_IA:
    state.s_col += ARG();
after_ic_ia:
    A2lState_SETF(&state,A2L_STATE_HASCOL);
    LOG(("[%x] COL = %d\n",op,state.s_col));
    goto read_addr;
   } break;

   {
   case A2L_O_IL_SC_IA:
    state.s_line += ARG();
after_il_sc_ia:
    LOG(("[%x] LINE = %d\n",op,state.s_line));
    state.s_col = ARG();
    A2lState_SETF(&state,A2L_STATE_HASLINE|A2L_STATE_HASCOL);
    LOG(("[%x] COL = %d\n",op,state.s_col));
    goto read_addr;
   } break;

   {
   default:
    if (op >= A2L_O_DEL_L && op <= (A2L_O_DEL_L|A2L_O_DEL_C|
                                    A2L_O_DEL_P|A2L_O_DEL_F|
                                    A2L_O_DEL_N)) {
#if (A2L_STATE_HASLINE == A2L_O_DEL_L) && \
    (A2L_STATE_HASCOL  == A2L_O_DEL_C) && \
    (A2L_STATE_HASPATH == A2L_O_DEL_P) && \
    (A2L_STATE_HASFILE == A2L_O_DEL_F) && \
    (A2L_STATE_HASNAME == A2L_O_DEL_N)
     state.s_features &= ~(uint32_t)(op);
     if (op&A2L_O_DEL_L) state.s_line = 0,LOG(("[%x] DELETE(LINE)\n",op));
     if (op&A2L_O_DEL_C) state.s_col  = 0,LOG(("[%x] DELETE(COL)\n",op));
     if (op&A2L_O_DEL_F) state.s_file = 0,LOG(("[%x] DELETE(FILE)\n",op));
     if (op&A2L_O_DEL_P) state.s_path = 0,LOG(("[%x] DELETE(PATH)\n",op));
     if (op&A2L_O_DEL_N) state.s_name = 0,LOG(("[%x] DELETE(NAME)\n",op));
#else
     if (op&A2L_O_DEL_L) A2lState_DEL_L(&state),LOG(("[%x] DELETE(LINE)\n",op));
     if (op&A2L_O_DEL_C) A2lState_DEL_C(&state),LOG(("[%x] DELETE(COL)\n",op));
     if (op&A2L_O_DEL_F) A2lState_DEL_F(&state),LOG(("[%x] DELETE(FILE)\n",op));
     if (op&A2L_O_DEL_P) A2lState_DEL_P(&state),LOG(("[%x] DELETE(PATH)\n",op));
     if (op&A2L_O_DEL_N) A2lState_DEL_N(&state),LOG(("[%x] DELETE(NAME)\n",op));
#endif
    } else {
     unsigned int opc;
     /* Check for special opcode ranges used for short line/col jumps. */
     if (op >= A2L_O_NLLO_IA && op <= A2L_O_NLHI_IA) { state.s_line += op-(A2L_O_NLLO_IA-1); goto after_il_ia; }
     if (op >= A2L_O_NCLO_IA && op <= A2L_O_NCHI_IA) { state.s_col += op-(A2L_O_NCLO_IA-1); goto after_ic_ia; }
     if (op >= A2L_O_NLLO_SC_IA && op <= A2L_O_NLHI_SC_IA) { state.s_line += op-(A2L_O_NLLO_SC_IA-1); goto after_il_sc_ia; }
     /* Unknown opcode (Ignore). */
     opc = A2L_GETOPC(op);
     LOG((op == A2L_O_NOP ? "" : "<unknown: %d>\n",(int)op));
     while (opc--) ARG();
    }
   } break;
   }
  }
done:
  /* Check if the last address is the requested capture point. */
  if (capture == state.s_addr)
      goto found_last;
end_update:
  *pcode = code;
 }
 return result;
found_last:
 *s_begin = state;
found:
 if (s_end) *s_end = state;
 result = 1;
 goto end_update;
#undef ARG
}

#ifdef __cplusplus
}
#endif

#endif /* !GUARD_DCC_LIB_SRC_COMMON_ADDR2LINE_C */
