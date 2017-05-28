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
#ifndef GUARD_DCC_UNIT_IMPORT_SOURCE_C_INL
#define GUARD_DCC_UNIT_IMPORT_SOURCE_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/assembler.h>

#include "unit-import.h"

DCC_DECL_BEGIN

PRIVATE void load_std_sections(void) {
 unit.u_text = DCCUnit_NewSecs(".text",DCC_SYMFLAG_SEC_X|DCC_SYMFLAG_SEC_R);
 unit.u_data = DCCUnit_NewSecs(".data",DCC_SYMFLAG_SEC_R);
 unit.u_bss  = DCCUnit_NewSecs(".bss",DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W);
 unit.u_str  = DCCUnit_NewSecs(".string",DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_M);
}


INTERN void DCCUNIT_IMPORTCALL
DCCUnit_LoadSrc_C(struct DCCLibDef *__restrict def) {
 (void)def;
 compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN);
 CURRENT.l_flags  |= (TPPLEXER_FLAG_TERMINATE_STRING_LF);

 /* Load standard sections. */
 load_std_sections();
 CURRENT.l_extokens &= (TPPLEXER_TOKEN_EQUALBINOP);
 CURRENT.l_extokens |= (TPPLEXER_TOKEN_LANG_C|
                        TPPLEXER_TOKEN_TILDETILDE);
 /* Yield the initial token. */
 TPPLexer_Yield();

 /* Select the text section and begin compiling. */
 DCCUnit_SetCurr(unit.u_text);
 DCCParse_AllGlobal();
 DCCUnit_SetCurr(NULL);
}


INTERN void DCCUNIT_IMPORTCALL
DCCUnit_LoadSrc_ASM(struct DCCLibDef *__restrict def) {
 (void)def;
 CURRENT.l_flags |= (TPPLEXER_FLAG_ASM_COMMENTS|
                     TPPLEXER_FLAG_TERMINATE_STRING_LF|
                     TPPLEXER_FLAG_COMMENT_NOOWN_LF|
                     TPPLEXER_FLAG_WANTLF);
 CURRENT.l_extokens = TPPLEXER_TOKEN_LANG_ASM;
 compiler.c_flags |= DCC_COMPILER_FLAG_INASM;
 load_std_sections();

 /* Yield the initial token. */
 TPPLexer_Yield();

 /* Select the text section and begin compiling. */
 DCCUnit_SetCurr(unit.u_text);

 while (TOK > 0) {
  unsigned long old_num = TOKEN.t_num;
  DCCParse_AsmInstr();
  if (old_num == TOKEN.t_num) YIELD();
 }
 DCCUnit_SetCurr(NULL);
}


DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_IMPORT_SOURCE_C_INL */
