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
#ifndef GUARD_DCC_COMPILER_STD_C_INL
#define GUARD_DCC_COMPILER_STD_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>

DCC_DECL_BEGIN

PUBLIC struct DCCStdName const DCCCompiler_Std[] = {

 {"dcc",DCC_COMPILER_STD_DCC},

 {"kr",DCC_COMPILER_STD_KR},
 {"k-r",DCC_COMPILER_STD_KR},
 {"k&r",DCC_COMPILER_STD_KR},

 {"ansi",DCC_COMPILER_STD_C90},
 {"c90",DCC_COMPILER_STD_C90},
 {"c89",DCC_COMPILER_STD_C90},
 {"iso9899:1990",DCC_COMPILER_STD_C90},

 {"c99",DCC_COMPILER_STD_C99},
 {"c9x",DCC_COMPILER_STD_C99},
 {"iso9899:1999",DCC_COMPILER_STD_C99},
 {"iso9899:199x",DCC_COMPILER_STD_C99},

 {"c11",DCC_COMPILER_STD_C11},
 {"c1x",DCC_COMPILER_STD_C11},
 {"iso9899:2011",DCC_COMPILER_STD_C11},

 {NULL,0},
};

PUBLIC void DCCCompiler_SetStd(int std) {
 if (!DCC_COMPILER_STD_KNOWN(std) ||
     (std == DCC_COMPILER_STD_CURRENT)) return;
 /* Reset the lexer state to start with the same basis. */
 TPPLexer_Reset(TPPLexer_Current,
                TPPLEXER_RESET_ESTATE|
                TPPLEXER_RESET_WSTATE);
 /* std=dcc is simply the default. */
 if (std == DCC_COMPILER_STD_DCC) return;
 
 /* Disable extensions that may interfere with STD symtax. */
 TPPLexer_DisableExtension(EXT_CXX11_ATTRIBUTE);
 TPPLexer_DisableExtension(EXT_ASM_REGISTERS);
 TPPLexer_DisableExtension(EXT_ASM_ADDRESS);
 TPPLexer_DisableExtension(EXT_VOID_ARITHMETIC);
 TPPLexer_DisableExtension(EXT_STRUCT_COMPATIBLE);
 TPPLexer_DisableExtension(EXT_AUTO_FOR_AUTOTYPE);
 TPPLexer_DisableExtension(EXT_SHORT_EXT_KEYWORDS);
 //TPPLexer_DisableExtension(EXT_GCC_EXPRSTMT); /* TODO: usercode should be able to override this with '__extension__' */
 //TPPLexer_DisableExtension(EXT_GCC_LABEL_EXPR); /* TODO: usercode should be able to override this with '__extension__' */
 
 switch (std) {
 case DCC_COMPILER_STD_KR:
  /* Enable traditional macros & tokens, and disable old-style warnings. */
  TPPLexer_EnableExtension(EXT_TRADITIONAL_MACRO);
  CURRENT.l_extokens |= TPPLEXER_TOKEN_EQUALBINOP;
  TPPLexer_SetWarningGroup(WG_OLD_FUNCTION_DECL,WSTATE_DISABLED);
  TPPLexer_SetWarningGroup(WG_OLD_VARIABLE_INIT,WSTATE_DISABLED);
  break;
 case DCC_COMPILER_STD_C90:
  /* Disable C++-style comments. */
  TPPLexer_Current->l_extokens &= ~(TPPLEXER_TOKEN_CPP_COMMENT);
  break;
 case DCC_COMPILER_STD_C99:
  /* Disable warnings about C99-specific syntax. */
  TPPLexer_SetWarningGroup(WG_C99,WSTATE_DISABLED);
  break;
 case DCC_COMPILER_STD_C11:
  /* Disable warnings about C99/C11-specific syntax. */
  TPPLexer_SetWarningGroup(WG_C99,WSTATE_DISABLED);
  TPPLexer_SetWarningGroup(WG_C11,WSTATE_DISABLED);
  break;
 default: break;
 }

}


DCC_DECL_END

#endif /* !GUARD_DCC_COMPILER_STD_C_INL */
