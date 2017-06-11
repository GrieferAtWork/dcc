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
#ifndef GUARD_DCC_FUNDECL_H
#define GUARD_DCC_FUNDECL_H 1

#include "common.h"
#include "unit.h"
#include "gen.h"

DCC_DECL_BEGIN

#define DCC_FUNCTIONFRAME_FLAG_NONE    0x00000000
#define DCC_FUNCTIONFRAME_FLAG_NAKED   0x00000001 /*< Naked function declaration (aka. '__attribute__((naked))'). */
struct DCCFunctionFrame {
 struct DCCDecl        *ff_old_fun;     /*< [0..1] Old value for 'compiler.c_fun'. */
 struct DCCSym         *ff_old_funname; /*< [0..1] Old value for 'compiler.c_funname'. */
 struct DCCSym         *ff_old_bsym;    /*< [0..1] Old value for 'compiler.c_bsym'. */
 struct DCCSym         *ff_old_csym;    /*< [0..1] Old value for 'compiler.c_csym'. */
 struct DCCSym         *ff_old_return;  /*< [0..1] Old value for 'compiler.c_return'. */
 uint32_t               ff_old_flags;   /*< Old value for 'compiler.c_flags'. */
 struct DCCVStack       ff_old_vstack;  /*< Old v-stack. */
 struct DCCSection     *ff_old_section; /*< [0..1] Old value for 'unit.u_curr'. */
 struct DCCSection     *ff_new_section; /*< [1..1] The section in which this function is being declared. */
 struct DCCSym         *ff_jmpsym;      /*< [0..1] Target for an optional jmp instruction prior to the function,
                                         *         and used to jump across when embedded inside another declaration. */
 struct DCCDispFunction ff_dispinfo;    /*< Function dispense information. */
 uint32_t               ff_flags;       /*< Set of 'DCC_FUNCTIONFRAME_FLAG_*' */
 struct DCCSym         *ff_funsym;      /*< [1..1] Entry-symbol for the function described by this frame. */
};

/* Enter/Leave, as well as initialize/dispose a give function frame.
 * >> These functions should be called after a function scope was created
 *    using the 'DCCUnit_(Push|Pop)FunctionScope' macros from "unit.h" */
DCCFUN void DCCFunctionFrame_Enter(struct DCCFunctionFrame *__restrict self,
                                   struct DCCDecl *fun_decl,
                                   struct DCCSym *__restrict fun_sym,
                                   struct DCCAttrDecl const *__restrict fun_attr);
DCCFUN void DCCFunctionFrame_Leave(struct DCCFunctionFrame *__restrict self);

DCC_DECL_END

#endif /* !GUARD_DCC_FUNDECL_H */
