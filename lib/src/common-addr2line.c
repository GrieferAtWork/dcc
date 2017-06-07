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
#ifndef GUARD_DCC_LIB_SRC_COMMON_ADDR2LINE_C
#define GUARD_DCC_LIB_SRC_COMMON_ADDR2LINE_C 1

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

A2L_IMPL a2l_arg_t A2L_NAME(a2l_getarg)(a2l_op_t **pcode) {
 a2l_arg_t result = 0;
 a2l_op_t byte,*iter = *pcode;
 for (;;) {
  byte = *iter++;
  result = (result << A2L_A_SFT)|(byte&A2L_A_MAX);
  if (!(byte&A2L_A_CON)) break;
 }
 *pcode = iter;
 return result;
}

#ifdef __DCC_VERSION__
extern int printf(char const *,...);
#define LOG(x) printf x
#else
#define LOG(x) (void)0
#endif

/* Execute code from a given addr2line state,
 * trying to capture the given address 'capture'.
 * @return: 0: The given address 'capture' has no debug information associated.
 * @return: 1: The given state 's' now contains the description for 'capture' */
A2L_IMPL int A2L_NAME(a2l_exec)(struct A2LState *s, a2l_addr_t capture) {
#define ARG() A2L_NAME(a2l_getarg)(&code)
 a2l_op_t *code = s->s_code;
 int result = 0;
 if likely(code) for (;;) {
  a2l_op_t op = *code++;
  switch (op) {
  case A2L_O_EOF: LOG(("EOF\n")); goto done;
  case A2L_O_RESET: LOG(("RESET\n")); A2LState_RESET(s); break;
  case A2L_O_NOP: break;

  {
   a2l_addr_t old_addr,new_addr;
   if (DCC_MACRO_FALSE) { case A2L_O_IA:cap_inc: new_addr = (old_addr = s->s_addr)+ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_DA:cap_dec: new_addr = (old_addr = s->s_addr)-ARG(); }
   LOG(("ADDR = %p\n",new_addr));
   s->s_addr = new_addr;
   if (new_addr >= old_addr) {
    LOG(("CHECK_ADDR(%p in %p...%p)\n",capture,old_addr,new_addr));
    if (capture > old_addr && capture <= new_addr) goto found;
   } else {
    LOG(("CHECK_ADDR(%p in %p...%p)\n",capture,new_addr,old_addr));
    if (capture > new_addr && capture <= old_addr) goto found;
   }
  } break;

  case A2L_O_IL: s->s_line += ARG(); A2LState_DEL_C(s); LOG(("LINE = %d\n",s->s_line)); break;
  case A2L_O_DL: s->s_line -= ARG(); A2LState_DEL_C(s); LOG(("LINE = %d\n",s->s_line)); break;
  case A2L_O_IC: s->s_col  += ARG(); A2LState_SETF(s,A2L_STATE_HASCOL); LOG(("COL = %d\n",s->s_col)); break;
  case A2L_O_DC: s->s_col  -= ARG(); A2LState_SETF(s,A2L_STATE_HASCOL); LOG(("COL = %d\n",s->s_col)); break;

  case A2L_O_SL: s->s_line  = ARG(); A2LState_DEL_C(s); LOG(("LINE = %d\n",s->s_line)); break;
  case A2L_O_SC: s->s_col   = ARG(); A2LState_SETF(s,A2L_STATE_HASCOL); LOG(("COL = %d\n",s->s_col)); break;
  case A2L_O_SP: s->s_path  = ARG(); A2LState_SETF(s,A2L_STATE_HASPATH); LOG(("PATH = %d\n",s->s_path)); break;
  case A2L_O_SF: s->s_file  = ARG(); A2LState_SETF(s,A2L_STATE_HASFILE); LOG(("FILE = %d\n",s->s_file)); break;

  {
   if (DCC_MACRO_FALSE) { case A2L_O_IL_IA: s->s_line += ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_DL_IA: s->s_line -= ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_SL_IA: s->s_line  = ARG(); }
   A2LState_SETF(s,A2L_STATE_HASLINE);
   A2LState_DEL_C(s);
   LOG(("LINE = %d\n",s->s_line));
   goto cap_inc;
  } break;

  {
   if (DCC_MACRO_FALSE) { case A2L_O_IL_DA: s->s_line += ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_DL_DA: s->s_line -= ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_SL_DA: s->s_line  = ARG(); }
   A2LState_SETF(s,A2L_STATE_HASLINE);
   A2LState_DEL_C(s);
   LOG(("LINE = %d\n",s->s_line));
   goto cap_dec;
  } break;

  {
   if (DCC_MACRO_FALSE) { case A2L_O_IL_SC_IA: case A2L_O_IL_SC_DA: s->s_line += ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_DL_SC_IA: case A2L_O_DL_SC_DA: s->s_line -= ARG(); }
   if (DCC_MACRO_FALSE) { case A2L_O_SL_SC_IA: case A2L_O_SL_SC_DA: s->s_line  = ARG(); }
   LOG(("LINE = %d\n",s->s_line));
   s->s_col = ARG();
   A2LState_SETF(s,A2L_STATE_HASLINE|A2L_STATE_HASCOL);
   LOG(("COL = %d\n",s->s_col));
   if (op <= 0xc2) goto cap_inc;
   goto cap_dec;
  } break;

  {
  default:
   if (op >= A2L_O_DEL_L && op <= A2L_O_DEL_P) {
    if (op&A2L_O_DEL_L) A2LState_DEL_L(s),LOG(("DELETE(LINE)\n"));
    if (op&A2L_O_DEL_C) A2LState_DEL_C(s),LOG(("DELETE(COL)\n"));
    if (op&A2L_O_DEL_F) A2LState_DEL_F(s),LOG(("DELETE(FILE)\n"));
    if (op&A2L_O_DEL_P) A2LState_DEL_P(s),LOG(("DELETE(PATH)\n"));
   } else {
    /* Unknown opcode (Ignore). */
    unsigned int opc = A2L_GETOPC(op);
    LOG(("<unknown: %d>\n",(int)op));
    while (opc--) ARG();
   }
  } break;
  }
 }
done: s->s_code = code;
 return result;
found: result = 1; goto done;
#undef ARG
}

#ifdef __cplusplus
}
#endif

#endif /* !GUARD_DCC_LIB_SRC_COMMON_ADDR2LINE_C */
