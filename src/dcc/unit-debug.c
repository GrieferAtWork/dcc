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
#ifndef GUARD_DCC_UNIT_DEBUG_C
#define GUARD_DCC_UNIT_DEBUG_C 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/unit.h>
#include <dcc/linker.h>

DCC_DECL_BEGIN

/* The user-space structure layout for section information.
 * NOTE: This must always remain compatible with the
 *       structure found in '/lib/src/addr2line.c'! */
#define SECINFO_ALIGNOF        (DCC_TARGET_SIZEOF_POINTER)
#define SECINFO_SIZEOF         (DCC_TARGET_SIZEOF_POINTER*3+DCC_TARGET_SIZEOF_SIZE_T)
#define SECINFO_OFFSETOF_NEXT  (0)
#define SECINFO_OFFSETOF_ADDR  (DCC_TARGET_SIZEOF_POINTER)
#define SECINFO_OFFSETOF_SIZE  (DCC_TARGET_SIZEOF_POINTER*2)
#define SECINFO_OFFSETOF_A2L   (DCC_TARGET_SIZEOF_POINTER*2+DCC_TARGET_SIZEOF_SIZE_T)

#define SECTIONFOR_SECINFO (unit.u_data)
#define SECTIONFOR_A2LCMD  (unit.u_data)

#if A2L_O_EOF != 0
#error "A2L_O_EOF must be defined as ZERO(0)"
#endif

PRIVATE struct DCCSym * /* Allocate and define a symbol for the A2l data of the given writer. */
DCCA2lWrite_AllocData(struct DCCA2lWriter *__restrict writer) {
 struct DCCSym *result = DCCUnit_AllocSym();
 if likely(result) {
  void        *a2l_data;
  target_ptr_t a2l_addr;
  size_t       a2l_size;
  /* Try to optimize the A2L code to impact on binary size. */
  DCCA2lWriter_Optimize(writer);
  a2l_size = (size_t)((writer->w_state.s_code-writer->w_cbegin)*sizeof(a2l_op_t));
  /* NOTE: Allocate 1 additional opcode that is implicitly initialized as 'A2L_O_EOF' */
  a2l_addr = DCCSection_DAlloc(SECTIONFOR_A2LCMD,a2l_size+sizeof(a2l_op_t),
                               DCC_COMPILER_ALIGNOF(a2l_op_t),0);
  DCCSym_Define(result,SECTIONFOR_A2LCMD,a2l_addr,
                a2l_size+sizeof(a2l_op_t),
                DCC_COMPILER_ALIGNOF(a2l_op_t));
  /* Copy the A2L data into the allocated section memory. */
  a2l_data = DCCSection_GetText(SECTIONFOR_A2LCMD,a2l_addr,a2l_size);
  if (a2l_data) memcpy(a2l_data,writer->w_cbegin,a2l_size);
 }
 return result;
}


PUBLIC void DCCUnit_MkDebugSym(void) {
 struct DCCSym *secinfo_sym;
 struct DCCSection *sec;
 /* Don't do anything when no debug informations should be generated. */
 if (!(linker.l_flags&DCC_LINKER_FLAG_GENDEBUG)) return;
 secinfo_sym = DCCUnit_NewSyms("__dcc_dbg_secinfo",DCC_SYMFLAG_PRIVATE);
 if unlikely(!secinfo_sym || !DCCSym_ISFORWARD(secinfo_sym)) return;
 DCCUnit_ENUMSEC(sec) {
  struct DCCSym *a2l_sym; void *data;
  target_ptr_t secinfo_addr;
  if (sec->sc_a2l.w_state.s_code == sec->sc_a2l.w_cbegin) continue;
  a2l_sym = DCCA2lWrite_AllocData(&sec->sc_a2l);
  if unlikely(!a2l_sym) break;
  /* Found a section containing debug informations! */
  secinfo_addr = DCCSection_DAlloc(SECTIONFOR_SECINFO,SECINFO_SIZEOF,SECINFO_ALIGNOF,0);
  data = DCCSection_GetText(SECTIONFOR_SECINFO,secinfo_addr,SECINFO_SIZEOF);
  if unlikely(!data) break;
  memset(data,0,SECINFO_SIZEOF);
  DCCSym_Define(secinfo_sym,SECTIONFOR_SECINFO,secinfo_addr,SECINFO_SIZEOF,SECINFO_ALIGNOF);
  if unlikely((secinfo_sym = DCCUnit_AllocSym()) == NULL) return;
  /* Generate relocations to initialize the fields of the section information entry. */
  DCCSection_Putrel(SECTIONFOR_SECINFO,secinfo_addr+SECINFO_OFFSETOF_NEXT,DCC_R_DATA_PTR,secinfo_sym);
  DCCSection_Putrel(SECTIONFOR_SECINFO,secinfo_addr+SECINFO_OFFSETOF_ADDR,DCC_R_DATA_PTR,&sec->sc_start);
  DCCSection_Putrel(SECTIONFOR_SECINFO,secinfo_addr+SECINFO_OFFSETOF_SIZE,DCC_R_SIZE,&sec->sc_start);
  DCCSection_Putrel(SECTIONFOR_SECINFO,secinfo_addr+SECINFO_OFFSETOF_A2L,DCC_R_DATA_PTR,a2l_sym);
 }

 /* Define as a symbol pointing to NULL, thereby
  * terminating the linked list of sections. */
 DCCSym_Define(secinfo_sym,&DCCSection_Abs,0,0,1);
}

PUBLIC void DCCUnit_MkDebugL(void) {
 if (!(linker.l_flags&DCC_LINKER_FLAG_GENDEBUG)) return;
 assert(unit.u_text);
 /* Put a debug addr2line entry. */
 DCCA2lWriter_PutL(&unit.u_text->sc_a2l,t_addr);
}
PUBLIC void DCCUnit_MkDebugLC(void) {
 if (!(linker.l_flags&DCC_LINKER_FLAG_GENDEBUG)) return;
 assert(unit.u_text);
 /* Put a debug addr2line entry. */
 DCCA2lWriter_PutLC(&unit.u_text->sc_a2l,t_addr);
}


DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_DEBUG_C */
