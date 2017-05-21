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
#ifndef GUARD_DCC_FUNDECL_C
#define GUARD_DCC_FUNDECL_C 1

#include <dcc/common.h>
#include <dcc/fundecl.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>

#include <string.h>

DCC_DECL_BEGIN

PUBLIC void
DCCFunctionFrame_Enter(struct DCCFunctionFrame *__restrict self,
                       struct DCCDecl *fun_decl,
                       struct DCCSym *__restrict fun_sym,
                       struct DCCAttrDecl const *__restrict fun_attr) {
 assert(self);
 assert(fun_sym);
 assert(fun_attr);
 self->ff_old_fun     = compiler.c_fun;
 self->ff_old_bsym    = compiler.c_bsym;
 self->ff_old_csym    = compiler.c_csym;
 self->ff_old_return  = compiler.c_return;
 self->ff_old_flags   = compiler.c_flags;
 self->ff_old_vstack  = compiler.c_vstack;
 self->ff_old_section = unit.u_curr;
 self->ff_new_section = fun_attr->a_section;
 if (self->ff_new_section &&
    (!(fun_attr->a_flags&DCC_ATTRFLAG_SECTION) ||
       DCCSection_ISIMPORT(self->ff_new_section))
     ) self->ff_new_section = NULL; /* Make sure this isn't an import section. */
 self->ff_jmpsym      = NULL;
 self->ff_funsym      = fun_sym;
 self->ff_flags       = DCC_FUNCTIONFRAME_FLAG_NONE;
 memset(&compiler.c_vstack,0,sizeof(compiler.c_vstack));
 if (!self->ff_new_section) self->ff_new_section = unit.u_curr;
 if (!self->ff_new_section) self->ff_new_section = unit.u_text;
 assert(self->ff_new_section);
 if (fun_attr->a_flags&DCC_ATTRFLAG_NAKED)
     self->ff_flags |= DCC_FUNCTIONFRAME_FLAG_NAKED;
 /* Generate a jump across the function code if code generation was enabled. 
  * >> This is required for nested function declarations. */
 if (compiler.c_fun && 
   !(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN)) {
  if (fun_decl) WARN(W_DECL_NESTED_FUNCTION_DECLARATION,fun_decl->d_name,compiler.c_fun);
  assert(unit.u_curr);
  self->ff_jmpsym = DCCUnit_AllocSym();
  if (self->ff_jmpsym) vpushs(self->ff_jmpsym),vgen1('&'),vjmp();
 }
 compiler.c_fun    = fun_decl;
 compiler.c_bsym   = NULL;
 compiler.c_csym   = NULL;
 compiler.c_return = NULL;
 compiler.c_flags &= ~(DCC_COMPILER_FLAG_SINIT|DCC_COMPILER_FLAG_INLOOP);
 /* Respect the dead-flag to optimize away function declarations in dead branches. */
 if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD))
       compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN);
 /* Select the section in which this function shall be declared. */
 if (self->ff_new_section != unit.u_curr)
     DCCUnit_SetCurr(self->ff_new_section);
 if (fun_decl) {
  /* TODO: Check if the old flags are compatible with the new ones. 
   *    >> Emit warnings like: 'redeclaration as static after previously declaration was extern' */
  fun_sym->sy_flags |= DCCDecl_CalculateSymflags(fun_decl);
  /* Warn if the function declaration isn't a function type. */
  if (DCCTYPE_GROUP(fun_decl->d_type.t_type) != DCCTYPE_FUNCTION
      ) WARN(W_DECL_EXPECTED_FUNCTION_TYPE_FOR_CODE_INITIALIZER,fun_decl);
  /* Warn if the function section isn't executable. */
  if (!(unit.u_curr->sc_start.sy_flags&DCC_SYMFLAG_SEC_X)) {
   WARN(W_DECL_FUNCTION_SECTION_NOT_EXECUTABLE,
        fun_decl,unit.u_curr);
  }
 }

 /* Align the text address. */
 if (fun_attr) {
  target_ptr_t align = 1;
  if (fun_attr->a_flags&DCC_ATTRFLAG_PACKED) {
   align = (fun_attr->a_flags&DCC_ATTRFLAG_FIXEDALIGN) ? fun_attr->a_align : 1;
  } else if (fun_attr->a_flags&DCC_ATTRFLAG_FIXEDALIGN && fun_attr->a_align > align) {
   align = fun_attr->a_align;
  }
  /* Align the text pointer. */
  if (align >= 2) DCCUnit_TAlign(align,0);
 }
 /* Define the location for the function symbol. */
 t_defsym(fun_sym);
 if (!(self->ff_flags&DCC_FUNCTIONFRAME_FLAG_NAKED)) {
  DCCDisp_FunProlog(&self->ff_dispinfo);
  /* Allocate a new return symbol. */
  compiler.c_return = DCCUnit_AllocSym();
 }
}
PUBLIC void
DCCFunctionFrame_Leave(struct DCCFunctionFrame *__restrict self) {
 assert(self);
 if (compiler.c_fun) {
  if (DCCTYPE_GROUP(compiler.c_fun->d_type.t_type) == DCCTYPE_FUNCTION &&
    !(compiler.c_flags&DCC_COMPILER_FLAG_DEAD) &&
     (assert(compiler.c_fun->d_type.t_base),
      compiler.c_fun->d_type.t_base->d_kind == DCC_DECLKIND_FUNCTION ||
      compiler.c_fun->d_type.t_base->d_kind == DCC_DECLKIND_OLDFUNCTION) &&
    !(self->ff_flags&DCC_FUNCTIONFRAME_FLAG_NAKED)) {
   struct DCCType *funtype = &compiler.c_fun->d_type.t_base->d_type;
   if (!DCCTYPE_ISBASIC(funtype->t_type,DCCTYPE_VOID)) {
    /* Warn if a function not returning void reaches the end of its control flow. */
    WARN(W_FUNCTION_MISSING_RETURN,compiler.c_fun,funtype);
   }
  }
  /* Warn if the function ends in a different section than where it starts. */
  if (unit.u_curr != self->ff_new_section) {
   WARN(W_FUNCTION_EXIT_IN_DIFFERENT_SECTION,
        compiler.c_fun,unit.u_curr,self->ff_new_section);
  }
  if (self->ff_flags&DCC_FUNCTIONFRAME_FLAG_NAKED) {
   if (compiler.c_hwstack.hws_maxoffset) WARN(W_ATTRIBUTE_NAKED_USES_STACK,compiler.c_fun);
   if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD)) WARN(W_ATTRIBUTE_NAKED_RETURNS_NORMALLY,compiler.c_fun);
  }
 }
 if (!(self->ff_flags&DCC_FUNCTIONFRAME_FLAG_NAKED)) {
  compiler.c_flags &= ~(DCC_COMPILER_FLAG_SINIT|DCC_COMPILER_FLAG_INLOOP);
  /* Generate the prolog/epilog in respect to the old dead-flag.
   * (Aka. Don't generate anything when the surrounding code is dead) */
  if (!(self->ff_old_flags&DCC_COMPILER_FLAG_DEAD))
   compiler.c_flags &= ~(DCC_COMPILER_FLAG_NOCGEN|DCC_COMPILER_FLAG_DEAD);
  /* Generate the prolog. */
  DCCDisp_GenProlog(&self->ff_dispinfo);
  /* Only need to generate an epilog symbol if the function
   * contained 'return' statements, or reaches its end. */
  if (compiler.c_return &&
     (compiler.c_return->sy_refcnt > 1 ||
     (compiler.c_return->sy_flags&DCC_SYMFLAG_USED) ||
     !(compiler.c_flags&DCC_COMPILER_FLAG_DEAD))) {
   t_defsym(compiler.c_return);
   DCCDisp_FunEpilog();
  }
 }

 /* Place a NOP byte after the function end to ease assembly readability. */
 if (!(compiler.c_flags&DCC_COMPILER_FLAG_NOFUNNOP) &&
     !(self->ff_flags&DCC_FUNCTIONFRAME_FLAG_NAKED)
     ) t_putb(DCCGEN_NOPBYTE); /* nop */

 assert(self->ff_funsym);
 if (unit.u_curr == self->ff_new_section) {
  /* Fill in the actual size of this function.
   * >> This is required for data association and cleanup of unused functions. */
  self->ff_funsym->sy_size = t_addr-self->ff_funsym->sy_addr;
 }
 
 /* Restore all the things... */
 if (self->ff_old_section != unit.u_curr)
     DCCUnit_SetCurr(self->ff_old_section);
 compiler.c_flags  = self->ff_flags;
 assertf(compiler.c_vstack.v_bottom == 
         compiler.c_vstack.v_end,
         "Invalid v-stack size: %lu too many",
        (unsigned long)(compiler.c_vstack.v_end-
                        compiler.c_vstack.v_bottom));
 free(compiler.c_vstack.v_begin); /* Cleanup the old v-stack. */
 compiler.c_vstack = self->ff_old_vstack;
 compiler.c_return = self->ff_old_return;
 compiler.c_csym   = self->ff_old_csym;
 compiler.c_bsym   = self->ff_old_bsym;
 compiler.c_fun    = self->ff_old_fun;
 if (self->ff_jmpsym) t_defsym(self->ff_jmpsym);
}


DCC_DECL_END

#endif /* !GUARD_DCC_FUNDECL_C */
