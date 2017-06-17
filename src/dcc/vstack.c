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
#ifndef GUARD_DCC_VSTACK_C
#define GUARD_DCC_VSTACK_C 1

#include <dcc/byteorder.h>
#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>
#include <dcc/lexer.h>
#include <dcc/linker.h>
#include <dcc/unit.h>
#include <dcc/vstack.h>

#include <stdlib.h>
#include <string.h>

#include "lexer-priv.h"
#include "x86_util.h"

/* When allocating 64-bit register pairs, prefer
 * EAX:EDX as div/mod requires less code, then. */
#define DCC_R64_PREFLO DCC_ASMREG_EAX
#define DCC_R64_PREFHI DCC_ASMREG_EDX


DCC_DECL_BEGIN

typedef uint_least64_t uint_t;

#if DCC_DEBUG && 0
INTDEF void dcc_outf(char const *fmt, ...);
#define HAVE_VLOG 1
#define VLOG(add,x) \
 (dcc_outf("%s(%d,%d) : VSTACK(%lu -> %lu) ",\
           TPPLexer_FILE(NULL),\
          (int)(TPPLexer_LINE()+1),\
          (int)(TPPLexer_COLUMN()+1),\
          (unsigned long)((ptrdiff_t)vsize),\
          (unsigned long)((ptrdiff_t)vsize+(add))),\
  dcc_outf x)
#else
#define VLOG(add,x) (void)0
#endif

/* Internal generator functions. */
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Store(struct DCCStackValue *__restrict self, struct DCCStackValue *__restrict target, int initial_store); /* mov self, target */
PRIVATE void DCC_VSTACK_CALL DCCStackValue_BinMem(struct DCCStackValue *__restrict self, tok_t op, struct DCCMemLoc const *__restrict target, size_t n);  /* *op self, target */
PRIVATE void DCC_VSTACK_CALL DCCStackValue_BinReg(struct DCCStackValue *__restrict self, tok_t op, rc_t dst, rc_t dst2);                             /* *op self, %dst; mov self, %dst2 */
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Unary(struct DCCStackValue *__restrict self, tok_t op);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Binary(struct DCCStackValue *__restrict self, struct DCCStackValue *__restrict target, tok_t op);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Cast(struct DCCStackValue *__restrict self, struct DCCType const *__restrict type);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Call(struct DCCStackValue *__restrict self);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Jcc(struct DCCStackValue *__restrict cond, struct DCCStackValue *__restrict target, int invert);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Jmp(struct DCCStackValue *__restrict target);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_AddOffset(struct DCCStackValue *__restrict self, int_t off);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Subscript(struct DCCStackValue *__restrict self, struct TPPKeyword const *__restrict member_name);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Dup(struct DCCStackValue *__restrict self);
PRIVATE int  DCC_VSTACK_CALL DCCStackValue_IsDuplicate(struct DCCStackValue *__restrict self, struct DCCStackValue *__restrict duplicate);
PRIVATE int  DCC_VSTACK_CALL DCCStackValue_IsCopyDuplicate(struct DCCStackValue *__restrict self, struct DCCStackValue *__restrict duplicate);

PRIVATE void DCC_VSTACK_CALL DCCStackValue_LodTest(struct DCCStackValue *__restrict self); /* The opposite of 'DCCStackValue_FixTest': Force 'self' to be a test. */

PUBLIC tyid_t DCC_VSTACK_CALL DCC_RC_GETTYPE(rc_t rc) {
 tyid_t result;
#ifdef DCC_RC_I64
 if (rc&DCC_RC_I64) result = DCCTYPE_INT64; else
#endif
      if (rc&DCC_RC_I32) result = DCCTYPE_INT32;
#ifdef DCCTYPE_INT16
 else if (rc&DCC_RC_I16) result = DCCTYPE_INT16;
#else
 else if (rc&DCC_RC_I16) result = DCCTYPE_INTPTR;
#endif
 else if (rc&DCC_RC_I8) result = DCCTYPE_INT8;
 else {
  result = 0; /* TODO: Floating point registers? */
 }
 return result;
}

PUBLIC DCC(target_siz_t) DCC_VSTACK_CALL
DCC_RC_SIZE(rc_t rc) {
 size_t result;
#ifdef DCC_RC_I64
 if (rc&DCC_RC_I64) result = 8;
 else
#endif
      if (rc&DCC_RC_I32) result = 4;
 else if (rc&DCC_RC_I16) result = 2;
 else if (rc&DCC_RC_I8) result = 1;
 else switch (rc&DCC_RC_MASK) {
 case DCC_RC_SEG: result = 2; break;
  /* Fallback case: applies to most other registers. */
 default: result = DCC_TARGET_SIZEOF_GP_REGISTER; break;
 }
 return result;
}
PUBLIC rc_t DCC_VSTACK_CALL
DCC_RC_FORTYPE(struct DCCType const *__restrict t) {
 rc_t result;
#if 0
 if (DCCTYPE_GROUP(t->t_type) == DCCTYPE_BUILTIN) {
  switch (DCCTYPE_BASIC(t->t_type)) {
  case DCCTYPE_FLOAT:
  case DCCTYPE_DOUBLE:
  case DCCTYPE_LDOUBLE:
   goto end;
  default: break;
  }
 }
#endif
 switch (DCCType_Sizeof(t,NULL,1)) {
 case 1: result = DCC_RC_I8;  break;
 case 2: result = DCC_RC_I16|DCC_RC_I8; break;
#if DCC_TARGET_CPU == DCC_CPU_X86_64
 case 4: result = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8; break;
 default: result = DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8; break;
#else
 default: result = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8; break;
#endif
 }
//end:
 return result;
}


PUBLIC void DCC_VSTACK_CALL
DCCStackValue_GetReturn(struct DCCStackValue *__restrict self,
                        struct DCCDecl const *funty_decl) {
 assert(self);
 self->sv_ctype.t_type = DCCTYPE_INT;
 self->sv_ctype.t_base = NULL;
 self->sv_flags        = DCC_SFLAG_NONE;
 self->sv_const.it     = 0;
 self->sv_sym          = NULL;
 self->sv_reg2         = DCC_RC_CONST;
 if (!funty_decl || (funty_decl->d_kind != DCC_DECLKIND_FUNCTION &&
                     funty_decl->d_kind != DCC_DECLKIND_OLDFUNCTION)) {
push_default:
  self->sv_reg = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
 } else {
  /* Use the return type of a function type declaration. */
  self->sv_ctype = funty_decl->d_type;
  if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_BUILTIN) {
   /* TODO: floating-point registers. */
   if (DCCTYPE_ISSIGNLESSBASIC(self->sv_ctype.t_type,DCCTYPE_INT64)) {
#ifdef DCC_RC_I64
    self->sv_reg  = DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
#else
    self->sv_reg  = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
    self->sv_reg2 = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EDX;
#endif
   } else {
    self->sv_reg = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
   }
  } else if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_POINTER ||
             DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_LVALUE) {
   self->sv_reg = DCC_RC_PTRX|DCC_ASMREG_EAX;
  } else {
   /* TODO: structure types. */
   goto push_default;
  }
 }
}
LOCAL int DCC_VSTACK_CALL
DCCStackValue_IsUnsignedReg(struct DCCStackValue const *__restrict self) {
 /* Special case: A lower-order register apart of a signed larger type is considered unsigned:
  *            >> int x = (int)%al; // Even though 'al' is typed as 'int', don't sign-extend it! */
 if (!(self->sv_flags&DCC_SFLAG_LVALUE) &&
       self->sv_reg != DCC_RC_CONST) {
  rc_t ty_rc = DCC_RC_FORTYPE(&self->sv_ctype);
  if (DCC_RC_SIZE(self->sv_reg) < DCC_RC_SIZE(ty_rc)) return 1;
 }
 return 0;
}
LOCAL int DCC_VSTACK_CALL
DCCStackValue_IsUnsignedOrPtr(struct DCCStackValue const *__restrict self) {
 if (DCCTYPE_ISUNSIGNED_OR_PTR(self->sv_ctype.t_type)) return 1;
 return DCCStackValue_IsUnsignedReg(self);
}


LOCAL void DCC_VSTACK_CALL
DCCStackValue_SetMemDecl(struct DCCStackValue *__restrict slot,
                         struct DCCMemDecl const *__restrict decl) {
 scopeid_t scope_diff;
 assert(slot),assert(decl);
 assertf(decl->md_scope <= compiler.c_scope.s_id,
         "How did you get a symbol from a future scope? "
         "This should have been invisible!");
 scope_diff        = compiler.c_scope.s_id-decl->md_scope;
 slot->sv_reg      = decl->md_loc.ml_reg;
 slot->sv_const.it = decl->md_loc.ml_off;
 slot->sv_sym      = decl->md_loc.ml_sym;
 /* Dereference frame pointers to access variables from other scopes! */
 if (scope_diff) {
  if (slot->sv_reg == DCCDISP_RETURNFRAME_REG) {
   struct DCCMemLoc src;
   rc_t temp_register = DCCVStack_GetReg(DCC_RC_PTR,1);
   src.ml_reg = DCCDISP_RETURNFRAME_REG;
   src.ml_off = DCCDISP_RETURNFRAME_OFF;
   src.ml_sym = NULL;
   /* Load the first parent frame. */
   DCCDisp_MemMovReg(&src,temp_register);
   src.ml_reg = temp_register;
   /* Dereference additional frame pointers. */
   while (--scope_diff) DCCDisp_MemMovReg(&src,temp_register);
   slot->sv_reg = temp_register;
  } else {
   WARN(W_INVALID_FRAME_INDIRECTION);
  }
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCStackValue_Kill(struct DCCStackValue *__restrict self) {
 target_ptr_t s,a; int was_lvalue;
 struct DCCStackValue local_target;
 assert(self);
 /* NOTE: Since the copy-flag will be lost, we are forced
  *       perform a copy now, even though that may not be
  *       required based on what will eventually be done
  *       with the value.
  *       But if we don't do this, the value may be modified
  *       directly even though it was originally marked for COW. */
 was_lvalue = (self->sv_flags&DCC_SFLAG_LVALUE) &&
             !(self->sv_flags&DCC_SFLAG_COPY);
 /* Allocate local stack-space. */
 if (was_lvalue) {
  /* To kill an l-value, we must generate a additional
   * indirection by saving to an l-value typed local. */
  s = a = DCC_TARGET_SIZEOF_POINTER;
  local_target.sv_ctype = self->sv_ctype;
  DCCDecl_XIncref(local_target.sv_ctype.t_base);
  DCCType_MkLValue(&local_target.sv_ctype);
  //self->sv_flags &= ~(DCC_SFLAG_LVALUE);
 } else {
  /* Figure out how much space is required. */
  s = DCCType_Sizeof(&self->sv_ctype,&a,1);
  local_target.sv_ctype = self->sv_ctype;
 }

 local_target.sv_flags    = DCC_SFLAG_LVALUE;
 local_target.sv_reg2     = DCC_RC_CONST;
 local_target.sv_const.it = 0;
 if (compiler.c_flags&DCC_COMPILER_FLAG_SINIT) {
  /* During static initialization, allocate section memory instead!
   * NOTE: Due to some low-level constructs, assignment to
   *       section memory during static initialization is
   *       performed at compile-time. */
  struct DCCSection *target_section;
  target_ptr_t       target_ptr;
  struct DCCSym     *target_sym;
  target_section = DCCTYPE_STATICWRITABLE(self->sv_ctype.t_type)
                                          ? unit.u_bss
                                          : unit.u_data;
  WARN(W_IMPLICIT_SECTION_ALLOCATION,
       target_section->sc_start.sy_name->k_name,s);
  target_sym = DCCUnit_AllocSym();
  if unlikely(!target_sym) goto alloc_stack;
  target_ptr = DCCSection_DAlloc(target_section,s,a,0);
  DCCSym_Define(target_sym,target_section,target_ptr,s,a);
  local_target.sv_reg = DCC_RC_CONST;
  local_target.sv_sym = target_sym;
  DCCSym_Incref(target_sym);
 } else {
alloc_stack:
  local_target.sv_reg          = DCC_RR_XBP;
  local_target.sv_const.offset = DCCCompiler_HWStackAlloc(s,a,0);
  local_target.sv_sym          = NULL;
 }
 /* Generate the store. */
 DCCStackValue_Store(self,&local_target,1);
 if (was_lvalue) DCCDecl_XDecref(self->sv_ctype.t_base);
 DCCSym_XDecref(self->sv_sym);
 *self = local_target;
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_Load(struct DCCStackValue *__restrict self) {
 struct DCCStackValue local_target;
 assert(self);
 /* Figure out how much space is required. */
 local_target.sv_ctype = self->sv_ctype;
 local_target.sv_flags = DCC_SFLAG_NONE;
#if DCC_TARGET_SIZEOF_GP_REGISTER < 8
 if (DCCTYPE_ISSIGNLESSBASIC(self->sv_ctype.t_type,DCCTYPE_INT64)) {
  local_target.sv_reg  = DCCVStack_GetReg(DCC_RC_I32|DCC_R64_PREFLO,1);
  local_target.sv_reg2 = DCCVStack_GetReg(DCC_RC_I32|DCC_R64_PREFHI,1);
  if unlikely(local_target.sv_reg2 == local_target.sv_reg) {
   /* Make sure not to use the same register twice! */
   local_target.sv_reg2 = DCCVStack_GetRegOf(DCC_RC_I32,
                                            (uint8_t)~(1 << (local_target.sv_reg&7)));
  }
 } else
#endif
 {
  local_target.sv_reg  = DCCVStack_GetReg(DCC_RC_FORTYPE(&self->sv_ctype),1);
  local_target.sv_reg2 = DCC_RC_CONST;
 }
 local_target.sv_const.it = 0;
 local_target.sv_sym      = NULL;
 /* Generate the store. */
 DCCStackValue_Store(self,&local_target,1);
 if (self->sv_sym) DCCSym_Decref(self->sv_sym);
 memcpy(self,&local_target,sizeof(struct DCCStackValue));
}
PUBLIC void DCC_VSTACK_CALL
DCCStackValue_LoadClass(struct DCCStackValue *__restrict self,
                        rc_t rc, int allow_ptr_regs) {
 struct DCCStackValue local_target;
#ifdef DCC_RC_I64
      if (rc&DCC_RC_I64) rc |= DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 else
#endif
      if (rc&DCC_RC_I32) rc |= DCC_RC_I16|DCC_RC_I8;
 else if (rc&DCC_RC_I16) rc |= DCC_RC_I8;
 /* Check if the value already matches the given classification. */
 if (!(self->sv_flags&DCC_SFLAG_LVALUE) &&
      (self->sv_reg&DCC_RC_MASK) == rc &&
      (allow_ptr_regs || !DCC_ASMREG_ISPTR(self->sv_reg&7))
       ) return;
 local_target.sv_ctype    = self->sv_ctype;
 local_target.sv_flags    = DCC_SFLAG_NONE;
 local_target.sv_reg      = DCCVStack_GetReg(rc,allow_ptr_regs);
 local_target.sv_reg2     = DCC_RC_CONST;
 local_target.sv_const.it = 0;
 local_target.sv_sym      = NULL;
 /* Generate the store. */
 DCCStackValue_Store(self,&local_target,1);
 if (self->sv_sym) DCCSym_Decref(self->sv_sym);
 *self = local_target;
}
PUBLIC void DCC_VSTACK_CALL
DCCStackValue_LoadExplicit(struct DCCStackValue *__restrict self, rc_t rcr) {
 struct DCCStackValue local_target;
 assert(self);
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 if (self->sv_reg == rcr) return;
 local_target.sv_ctype    = self->sv_ctype;
 local_target.sv_flags    = DCC_SFLAG_NONE;
 local_target.sv_reg      = rcr;
 local_target.sv_reg2     = DCC_RC_CONST;
 local_target.sv_const.it = 0;
 local_target.sv_sym      = NULL;
 /* Generate the store. */
 DCCStackValue_Store(self,&local_target,1);
 if (self->sv_sym) DCCSym_Decref(self->sv_sym);
 memcpy(self,&local_target,sizeof(struct DCCStackValue));
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Store(struct DCCStackValue *__restrict self,
                    struct DCCStackValue *__restrict target,
                    int initial_store) {
 rc_t target_reg;
 assert(self);
 assert(target);
 DCCStackValue_LoadLValue(self);
 DCCStackValue_FixBitfield(self);
 if (target->sv_flags&DCC_SFLAG_BITFLD) {
  /* Special case: The target is a bit-field. */
  struct DCCStackValue val;
  unsigned int off,siz;
  off = DCC_SFLAG_GTBITOFF(target->sv_flags);
  siz = DCC_SFLAG_GTBITSIZ(target->sv_flags);
  /* Special case: Empty bitfield. */
  if unlikely(!siz) return;
  /* Remove the bitfield flags to prevent infinite recursion. */
  target->sv_flags &= ~(DCC_SFLAG_BITFLD|
                        DCC_SFLAG_BITSIZ_MASK|
                        DCC_SFLAG_BITOFF_MASK);
  val.sv_ctype.t_type = DCCTYPE_INT;
  val.sv_ctype.t_base = NULL;
  val.sv_flags        = DCC_SFLAG_NONE;
  val.sv_reg          = DCC_RC_CONST;
  val.sv_reg2         = DCC_RC_CONST;
  val.sv_sym          = NULL;
  val.sv_const.it     = (((int_t)1 << siz)-1) << off;
  if (self->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST)) {
   DCCStackValue_Load(self);
  } else if (self->sv_reg == DCC_RC_CONST) {
   int_t full_mask;
   /* Special case: the source var is constant. */
   if (self->sv_sym) goto gen_binary_self;
   if (target->sv_reg == DCC_RC_CONST &&
      (target->sv_flags&DCC_SFLAG_LVALUE)) {
    /* Operate on compile-time memory (such as during static initialization) */
    struct DCCMemLoc ml; uint8_t *cdata; size_t cbytes;
    ml.ml_reg  = DCC_RC_CONST;
    ml.ml_off  = target->sv_const.offset;
    ml.ml_sym  = target->sv_sym;
    ml.ml_off += off/DCC_TARGET_BITPERBYTE;
    cbytes     = (siz+(DCC_TARGET_BITPERBYTE-1))/DCC_TARGET_BITPERBYTE;
    if ((cdata = (uint8_t *)DCCMemLoc_CompilerAddr(&ml,cbytes)) != NULL) {
     off %= DCC_TARGET_BITPERBYTE;
     val.sv_const.it = self->sv_const.it;
     while (cbytes) {
      uint8_t byteval,bytemsk,bytesiz;
      bytesiz = (uint8_t)siz;
      if (bytesiz > DCC_TARGET_BITPERBYTE)
          bytesiz = DCC_TARGET_BITPERBYTE;
      bytemsk = (0xff & ((1 << bytesiz)-1)) << off;
      byteval = (uint8_t)val.sv_const.it;
      byteval = (byteval & ((1 << bytesiz)-1)) << off;
      *cdata  = (*cdata & ~bytemsk) | byteval;
      siz              -= DCC_TARGET_BITPERBYTE;
      off               = 0;
      val.sv_const.it >>= DCC_TARGET_BITPERBYTE;
      --cbytes,++cdata;
     }
     return;
    }
   }

   /* Mask the source value with the bit-mask. */
   full_mask        = val.sv_const.it;
   val.sv_const.it &= (self->sv_const.it << off);
   /* Apply a constant to the target. */
   if (val.sv_const.it != full_mask) {
    val.sv_const.it  = ~full_mask;
    DCCStackValue_Binary(&val,target,'&');
    val.sv_const.it &= (self->sv_const.it << off);
   }
   if (val.sv_const.it != 0) {
    DCCStackValue_Binary(&val,target,'|');
   }
   return;
  } else {
   DCCStackValue_Dup(self);
gen_binary_self:
   DCCVStack_KillTst();
   DCCStackValue_Binary(&val,self,'&');
   goto apply_masks;
  }
  DCCVStack_KillTst();
apply_masks:
  val.sv_const.it = ~val.sv_const.it;
  DCCStackValue_Binary(&val,target,'&');
  DCCStackValue_Binary(self,target,'|');
  return;
 }
 if (initial_store) {
  struct DCCType *iter = &target->sv_ctype;
  /* During the initial assignment, l-values themself are initialized.
   * Yet during any other assignment, what the l-value points to is modified instead. */
  while (DCCTYPE_GROUP(iter->t_type) == DCCTYPE_LVALUE) {
   assert(iter->t_base);
   assert(iter->t_base->d_kind&DCC_DECLKIND_TYPE);
   DCCStackValue_Unary(self,'&');
   assert(iter != &iter->t_base->d_type);
   iter = &iter->t_base->d_type;
  }
 } else {
  DCCStackValue_LoadLValue(target);
  assert(DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE);
 }
 DCCStackValue_Cow(target);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);

 target_reg = target->sv_reg;
 if ((target->sv_flags&DCC_SFLAG_LVALUE) || target_reg == DCC_RC_CONST) {
  struct DCCMemLoc dest;
  if (!(target->sv_flags&DCC_SFLAG_LVALUE))
   WARN(W_EXPECTED_LVALUE_IN_STORE,&target->sv_ctype);
  dest.ml_off = target->sv_const.offset;
  dest.ml_sym = target->sv_sym;
  dest.ml_reg = target_reg;
  /* Store to global variable. */
  if (self->sv_flags&DCC_SFLAG_TEST) {
   DCCDisp_SccMem((test_t)DCC_SFLAG_GTTEST(self->sv_flags),&dest,
                  DCCType_Sizeof(&target->sv_ctype,NULL,1));
  } else {
   DCCStackValue_BinMem(self,'=',&dest,DCCType_Sizeof(&target->sv_ctype,NULL,1));
  }
 } else {
  /* Store in regular, old register. */
  if (self->sv_flags&DCC_SFLAG_TEST) {
   DCCDisp_SccReg((test_t)DCC_SFLAG_GTTEST(self->sv_flags),target_reg);
   if (target->sv_reg2 != DCC_RC_CONST) DCCDisp_IntMovReg(0,target->sv_reg2);
  } else {
   DCCStackValue_BinReg(self,'=',target->sv_reg,target->sv_reg2);
  }
 }
}

LOCAL void DCC_VSTACK_CALL
DCCStackValue_FixRegister(struct DCCStackValue *__restrict self) {
 rc_t new_register;
 /* todo: shouldn't we technically also check 'self->sv_reg2' ? */
 if ((self->sv_reg&(DCC_RC_I3264|DCC_RC_I16)) &&
     DCC_ASMREG_ISSPTR(self->sv_reg&DCC_RI_MASK) &&
    !(self->sv_flags&DCC_SFLAG_XREGISTER)) {
  self->sv_flags &= ~(DCC_SFLAG_COPY);
  /* Special case: Must create a copy of the SP/BP registers!
   *            >> We could technically use them directly, but
   *               that would break the stack/stack-frame. */
  new_register = DCCVStack_GetReg(self->sv_reg&DCC_RC_MASK,1);
  if (!self->sv_const.offset) {
   /* mov %self->sv_reg, %new_register */
   DCCDisp_RegMovReg(self->sv_reg,new_register,
                     DCCStackValue_IsUnsignedOrPtr(self));
  } else {
   /* lea self->sv_const.it(%self->sv_reg), %new_register */
   struct DCCMemLoc src;
   src.ml_off = self->sv_const.offset;
   src.ml_reg = self->sv_reg;
   src.ml_sym = self->sv_sym;
   DCCDisp_LeaReg(&src,new_register);
   self->sv_const.it = 0;
   DCCSym_XDecref(self->sv_sym);
   self->sv_sym = NULL;
  }
  self->sv_reg = new_register;
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_FixRegOffset(struct DCCStackValue *__restrict self) {
 assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
 if (self->sv_const.it) {
  DCCStackValue_FixRegister(self);
  if (self->sv_const.it) {
   struct DCCSymAddr temp;
   temp.sa_off = (target_off_t)(self->sv_const.it);
   temp.sa_sym = self->sv_sym;
   if (self->sv_reg2 == DCC_RC_CONST) {
    /* Must use 'lea' instead of 'add' when the v-stack includes tests. */
    if (DCCVStack_HasTst()) {
     struct DCCMemLoc mloc;
     if (!temp.sa_sym) {
      if (temp.sa_off == +1) { DCCDisp_UnaryReg(TOK_INC,self->sv_reg); goto done; }
      if (temp.sa_off == -1) { DCCDisp_UnaryReg(TOK_DEC,self->sv_reg); goto done; }
     }
     /* Use 'lea' to fix the register. */
     mloc.ml_sad = temp;
     mloc.ml_reg = self->sv_reg;
     DCCDisp_LeaReg(&mloc,self->sv_reg);
    } else {
     DCCDisp_AddReg(&temp,self->sv_reg);
    }
   } else {
    DCCVStack_KillTst(); /* Maybe try not to do this? */
    if (temp.sa_off < 0)
     temp.sa_off = -temp.sa_off,
     DCCDisp_CstBinReg('-',&temp,self->sv_reg,0);
    else DCCDisp_CstBinReg('+',&temp,self->sv_reg,0);
    temp.sa_off = (target_off_t)(self->sv_const.it >> 32);
    if (temp.sa_off < 0)
     /* NOTE: This must be '~' instead of '-' because of the carry-extension.
      *       Consider a small number '-5': 0xfffffffffffffffb
      *       Its high memory is equal to '0xffffffff',
      *       which when negated would equal '1'.
      *       That '1' is the carry bit that can simply be
      *       ignored by using '~', which will result in '0'. */
     temp.sa_off = ~temp.sa_off,
     DCCDisp_CstBinReg(TOK_DEC,&temp,self->sv_reg2,0);
    else DCCDisp_CstBinReg(TOK_INC,&temp,self->sv_reg2,0);
   }
done:
   self->sv_const.it = 0;
   self->sv_sym      = NULL;
  }
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_FixTest(struct DCCStackValue *__restrict self) {
 assert(self);
 if (self->sv_flags&DCC_SFLAG_TEST) {
  test_t t = (test_t)((self->sv_flags&DCC_SFLAG_TEST_MASK) >> DCC_SFLAG_TEST_SHIFT);
  self->sv_reg   = DCCVStack_GetReg(DCC_RC_I8,1);
  self->sv_reg2  = DCC_RC_CONST;
  self->sv_flags = DCC_SFLAG_NONE;
  DCCDisp_SccReg(t,self->sv_reg);
 }
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_LodTest(struct DCCStackValue *__restrict self) {
 if (!(self->sv_flags&DCC_SFLAG_TEST)) {
  if ((self->sv_flags&DCC_SFLAG_LVALUE) ||
      (self->sv_reg == DCC_RC_CONST)
      ) DCCStackValue_Load(self);
  assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
  DCCStackValue_FixRegOffset(self);
  assert(!self->sv_const.it);
  assert(!self->sv_sym);
  if (self->sv_reg2) { /* TODO: 64-bit test. */ }
  DCCDisp_RegBinReg('t',self->sv_reg,self->sv_reg,1);
  self->sv_flags = DCC_SFLAG_MKTEST(DCC_TEST_NZ);
  self->sv_reg   = DCC_RC_CONST;
  self->sv_reg2  = DCC_RC_CONST;
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCStackValue_FixBitfield(struct DCCStackValue *__restrict self) {
 assert(self);
 if (self->sv_flags&DCC_SFLAG_BITFLD) {
  struct DCCStackValue val;
  unsigned int off,siz,bits = 32;
  off = DCC_SFLAG_GTBITOFF(self->sv_flags);
  siz = DCC_SFLAG_GTBITSIZ(self->sv_flags);
  /* Remove the bitfield flags to prevent infinite recursion. */
  self->sv_flags &= ~(DCC_SFLAG_BITFLD|
                      DCC_SFLAG_BITSIZ_MASK|
                      DCC_SFLAG_BITOFF_MASK);
  if (self->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST)) {
load_reg: /* Make sure we're operating on a register. */
   DCCStackValue_Load(self);
  } else if (self->sv_reg != DCC_RC_CONST) {
   /* Fix potential register offsets. */
   DCCStackValue_FixRegOffset(self);
  } else {
   if (self->sv_sym) goto load_reg;
   /* Constant optimization. */
   self->sv_const.it <<= (bits-(off+siz));
   self->sv_const.it >>= (bits-siz);
   return;
  }

  assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
  assert(self->sv_reg != DCC_RC_CONST);
  if (!siz) {
   /* Special case: Empty byte-mask. */
   DCCStackValue_Binary(self,self,'^');
   return;
  }
  val.sv_ctype.t_type = DCCTYPE_INT;
  val.sv_ctype.t_base = NULL;
  val.sv_flags        = DCC_SFLAG_NONE;
  val.sv_reg          = DCC_RC_CONST;
  val.sv_reg2         = DCC_RC_CONST;
  val.sv_sym          = NULL;
  if (!off) {
   /* Special case: Can generate a read using a bit-mask. */
   val.sv_const.it     = ((int_t)1 << siz)-1;
   DCCStackValue_Binary(&val,self,'&');
  } else {
   val.sv_const.it     = bits-(off+siz);
   DCCStackValue_Binary(&val,self,TOK_SHL);
   val.sv_const.it     = bits-siz;
   DCCStackValue_Binary(&val,self,DCCStackValue_IsUnsignedOrPtr(self) ? TOK_SHR : TOK_RANGLE3);
  }
 }
}



#if DCC_TARGET_BIN == DCC_BINARY_PE
INTDEF struct DCCSym *pe_getitasym(struct DCCSym *__restrict basesym);
PUBLIC void
DCCLinker_PEIndImport(struct DCCStackValue *__restrict self) {
 struct DCCSym *pesym,*iat_sym;
 struct DCCSection *symsec;
 assert(self);
 //if (!(self->sv_flags&DCC_SFLAG_LVALUE)) return;
 if (self->sv_reg != DCC_RC_CONST) return;
 if ((pesym = self->sv_sym) == NULL) return;
 /* Force ITA indirection on symbols declared as '__attribute__((dllimport))' */
 if (pesym->sy_flags&DCC_SYMFLAG_DLLIMPORT) goto force_iat;
 if (!(pesym->sy_flags&DCC_SYMFLAG_PE_ITA_IND)) return;
 if ((symsec = pesym->sy_sec) == NULL) return;
 if (!DCCSection_ISIMPORT(symsec)) return;
 /* Fix this symbol through PE indirection. */
force_iat:
 if ((iat_sym = pesym->sy_peind) == NULL) {
  /* Don't generate missing ITA functions when no code should be generated. */
  if (compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) return;
  /* The symbol must be part of the unnamed symbol list! */
  if unlikely((iat_sym = pe_getitasym(pesym)) == NULL) return;
  DCCSym_Incref(iat_sym);
  pesym->sy_peind = iat_sym; /* Inherit reference. */
 }
 DCCSym_Incref(iat_sym);
 DCCSym_Decref(self->sv_sym);
 self->sv_sym = iat_sym; /* Inherit reference. */
 /* Do something similar to what 'DCCStackValue_Unary(self,'*')' would do! */
 DCCStackValue_Promote(self);
 DCCStackValue_FixBitfield(self);
 DCCStackValue_FixTest(self);
 if (self->sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(self);
 self->sv_flags |= DCC_SFLAG_LVALUE;
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 assert(!(self->sv_flags&DCC_SFLAG_BITFLD));
}
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */


PUBLIC void DCC_VSTACK_CALL
DCCStackValue_LoadLValue(struct DCCStackValue *__restrict self) {
 assert(self);
#if DCC_TARGET_BIN == DCC_BINARY_PE
 DCCLinker_PEIndImport(self);
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 while (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_LVALUE) {
  if (self->sv_flags&DCC_SFLAG_LVALUE) {
   struct DCCMemLoc addr; rc_t new_register;
   DCCStackValue_FixTest(self);
   DCCStackValue_FixBitfield(self);
   assert(!(self->sv_flags&DCC_SFLAG_TEST));
   assert(!(self->sv_flags&DCC_SFLAG_BITFLD));
   addr.ml_reg = new_register = self->sv_reg;
   addr.ml_off = self->sv_const.offset;
   addr.ml_sym = self->sv_sym;
   self->sv_const.it = 0;
   if (new_register == DCC_RC_CONST ||
     !(self->sv_reg&DCC_RC_PTR) ||
     ((self->sv_flags&DCC_SFLAG_COPY) || /* Must copy protected registers ESP/EBP. */
    (!(self->sv_flags&DCC_SFLAG_XREGISTER) &&
       DCCStackValue_ISPROTECTED(self)))) {
    new_register = DCCVStack_GetReg(DCC_RC_PTR,1);
    self->sv_flags &= ~(DCC_SFLAG_COPY);
   }
   DCCDisp_MemMovReg(&addr,new_register);
   self->sv_reg = new_register;
  } else {
   self->sv_flags |= DCC_SFLAG_LVALUE;
   assert(!(self->sv_flags&DCC_SFLAG_TEST));
   assert(!(self->sv_flags&DCC_SFLAG_BITFLD));
  }
  DCCType_MkBase(&self->sv_ctype);
 }
}

PRIVATE int
DCCStackValue_HasDuplicate(struct DCCStackValue *__restrict self) {
 struct DCCStackValue *iter,*end;
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if (iter != self && DCCStackValue_IsDuplicate(self,iter)) return 1;
 }
 return 0;
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Dup(struct DCCStackValue *__restrict self) {
 struct DCCStackValue copy;
 struct DCCType *source_type;
 int is_basic_type;
 assert(vsize >= 1);
 if ((!(self->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_XREGISTER)) &&
       /* Must still _always_ copy restricted pointers (e.g.: '%esp') */
       !DCCStackValue_ISPROTECTED(self) &&
       /* No need to copy stack-values without duplicates! */
       !DCCStackValue_HasDuplicate(self)) ||
      /* No need to duplicate constant expressions. */
     (!(self->sv_flags&DCC_SFLAG_LVALUE) &&
        self->sv_reg == DCC_RC_CONST &&
        self->sv_reg2 == DCC_RC_CONST)) {
  /* The caller expects this flag to be gone by the time we return. */
  self->sv_flags &= ~(DCC_SFLAG_COPY);
  /* Simply a constant expression (no need for special handling). */
  return;
 }


 /* Figure out the underlying type. */
 source_type = &self->sv_ctype;
 while (DCCTYPE_GROUP(source_type->t_type) == DCCTYPE_LVALUE) {
  assert(source_type->t_base);
  assert(source_type->t_base->d_kind&DCC_DECLKIND_TYPE);
  source_type = &source_type->t_base->d_type;
 }
 is_basic_type = DCCTYPE_GROUP(source_type->t_type) == DCCTYPE_BUILTIN ||
                 DCCTYPE_GROUP(source_type->t_type) == DCCTYPE_POINTER;
 copy.sv_ctype    = *source_type; /* Inherit data. */
 copy.sv_reg2     = DCC_RC_CONST;
 copy.sv_const.it = 0;
 copy.sv_sym      = NULL;
 if (!is_basic_type) {
  target_ptr_t s,a;
  /* Non-basic type. --> Must create a copy on the stack. */
  s = DCCType_Sizeof(source_type,&a,1);
  copy.sv_flags        = DCC_SFLAG_LVALUE;
  copy.sv_reg          = DCC_RR_XBP;
  copy.sv_const.offset = DCCCompiler_HWStackAlloc(s,a,0);
 } else {
  copy.sv_flags = DCC_SFLAG_NONE;
  copy.sv_reg   = DCCVStack_GetReg(DCC_RC_FORTYPE(source_type),1);
#if DCC_TARGET_SIZEOF_GP_REGISTER < 8
  if (DCCTYPE_ISSIGNLESSBASIC(source_type->t_type,DCCTYPE_INT64)) {
   /* Need a second 32-bit register for this. */
   copy.sv_reg2 = DCCVStack_GetRegOf(DCC_RC_I32,
                                    (uint8_t)~(1 << (copy.sv_reg&DCC_RI_MASK)));
  }
#endif
 }
 /* Generate a regular, old store. */
 DCCStackValue_Store(self,&copy,1);
 copy.sv_flags |= (self->sv_flags&DCC_SFLAG_RVALUE);
 DCCSym_XDecref(self->sv_sym);
 *self = copy; /* Inherit data. */
 assert(!(self->sv_flags&DCC_SFLAG_COPY));
}

PRIVATE int DCC_VSTACK_CALL
DCCStackValue_IsDuplicate(struct DCCStackValue *__restrict self,
                          struct DCCStackValue *__restrict duplicate) {
#define FMASK (DCC_SFLAG_LVALUE|DCC_SFLAG_TEST|DCC_SFLAG_TEST_MASK)
 assert(self);
 assert(duplicate);
 /* If 'self' is identical to 'duplicate', remove the COPY flag from 'duplicate' */
 return self->sv_reg == duplicate->sv_reg &&
        self->sv_sym == duplicate->sv_sym &&
        self->sv_const.it == duplicate->sv_const.it &&
      ((self->sv_flags&FMASK) == (duplicate->sv_flags&FMASK));
#undef FMASK
}
PRIVATE int DCC_VSTACK_CALL
DCCStackValue_IsCopyDuplicate(struct DCCStackValue *__restrict self,
                              struct DCCStackValue *__restrict duplicate) {
#define FMASK (DCC_SFLAG_LVALUE|DCC_SFLAG_TEST|DCC_SFLAG_TEST_MASK)
 assert(self);
 assert(duplicate);
 /* If 'self' is identical to 'duplicate', remove the COPY flag from 'duplicate' */
 return (duplicate->sv_flags&DCC_SFLAG_COPY) &&
         self->sv_reg == duplicate->sv_reg &&
         self->sv_sym == duplicate->sv_sym &&
         self->sv_const.it == duplicate->sv_const.it &&
       ((self->sv_flags&FMASK) == (duplicate->sv_flags&FMASK));
#undef FMASK
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_Cow(struct DCCStackValue *__restrict self) {
 if (DCCStackValue_MUSTCOPY(self)) {
  /* Unset the copy flag of adjacent stack values that were equivalent before. */
  DCCStackValue_Dup(self);
 } else {
  /* Check to see if there is a duplicate of this stack-value nearby,
   * and if there is, and it has the copy-flag set, copy it instead.
   * This is required for situations like this:
   * >> vpushr(EAX);    // EAX
   * >> vdup(1);        // EAX, EAXd
   * >> vswap();        // EAXd, EAX
   * >> vgen1(TOK_INC); // EAXd, EAX+1 // EAXd must be copied through COW here, even though it's not the operand!
   * >> vpop();         // EAXd
   * >> // EAX was increated, and vbottom contains its old value
   */
  /* NOTE: Technically, we'd have to search the entire vstack
   *       for duplicates, but this is sufficient for now! */
  if (self != compiler.c_vstack.v_bottom &&
      DCCStackValue_IsCopyDuplicate(self,self-1)) --self;
  else if (self != compiler.c_vstack.v_end &&
           DCCStackValue_IsCopyDuplicate(self,self+1)) ++self;
  else goto done;
  DCCStackValue_Dup(self);
 }
done:

 assert(!(self->sv_flags&DCC_SFLAG_COPY));
}

PRIVATE int DCC_VSTACK_CALL
DCCStackValue_ConstUnary(struct DCCStackValue *__restrict self, tok_t op) {
 int_t iv;
 assert(self);
 assert(self->sv_reg == DCC_RC_CONST);
 iv = self->sv_const.it;
 switch (op) {
 case '!':
  iv = !(iv || self->sv_sym);
  DCCSym_XDecref(self->sv_sym);
  self->sv_sym = NULL;
  break;
 case '~':
 case TOK_TILDE_TILDE:
  /* While there are other means of accidentally setting the sign-bit, this
   * is the only way accepted that will prevent warnings from being emit. */
  self->sv_flags |= DCC_SFLAG_NO_WSIGN;
 case '-':
  if (self->sv_sym) return 0;
  if (op == '-') iv = -iv;
  else           iv = ~iv;
  break;
 case TOK_INC: WARN(W_ASM_CONSTEXPR_INVALID_OPERATION); ++iv; break;
 case TOK_DEC: WARN(W_ASM_CONSTEXPR_INVALID_OPERATION); --iv; break;
 default: break;
 }
 self->sv_const.it = iv;
 DCCStackValue_ClampConst(self,0);
 return 1;
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Unary(struct DCCStackValue *__restrict self, tok_t op) {
 assert(self);
 if (op != '&' && op != '*') {
  /* Warn if an arithmetic operation is used with a non-arithmetic structure type. */
  if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_STRUCTURE &&
     (!self->sv_ctype.t_base->d_attr ||
      !(self->sv_ctype.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)
      )) WARN(W_STRUCTURE_ARITHMETIC,&self->sv_ctype);
 }

 switch (op) {

 { /* Inversion operator. */
 case '!':
  /* Simple case: Just invert the test. */
  if (self->sv_flags&DCC_SFLAG_TEST) {
   /* Simply invert an existing test. */
   self->sv_flags ^= (DCC_TEST_NBIT << DCC_SFLAG_TEST_SHIFT);
   goto end_exclaim;
  }
  /* Special optimization for '!%esp' and '!%ebp'
   * >> DCC assumes that there is always a stack available,
   *    meaning that both of the above expressions are
   *    evaluated to 'false' at compile-time.
   * NOTE: Remember: 8-bit register ids for ESP/EBP describe the
   *                 high-order bytes of AX, CX, DX and BX. For that
   *                 reason, we explicitly check that the register
   *                 isn't 8-bit (as (E)SP/(E)BP can't be either)
   */
  if (!(self->sv_flags&DCC_SFLAG_LVALUE) &&
       (DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE) &&
       (self->sv_reg&(DCC_RC_I&~(DCC_RC_I8))) &&
      ((self->sv_reg&DCC_RI_MASK) == DCC_ASMREG_ESP ||
       (self->sv_reg&DCC_RI_MASK) == DCC_ASMREG_EBP)) {
   /* Generate constant false. */
   self->sv_const.it = 0;
   self->sv_reg      = DCC_RC_CONST;
   self->sv_reg2     = DCC_RC_CONST;
   self->sv_sym      = NULL;
   goto end_exclaim;
  }
  DCCStackValue_LoadLValue(self);
  assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);

  /* Generate a test. */
  if (self->sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(self);
  else if (self->sv_reg == DCC_RC_CONST) {
   /* Constant expression case. */
   self->sv_const.it = !self->sv_const.it;
   goto end_exclaim;
  }
  assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
  assert(self->sv_reg != DCC_RC_CONST);
  DCCStackValue_FixRegOffset(self);
  assert(!self->sv_const.it);
  assert(!self->sv_sym);
  /* Test the register against itself:
   * >> 42&42 --> ZF == 0
   * >> 0&0   --> ZF == 1
   * -> !0 --> 1 (Test must mirror the ZF flag)
   */
  DCCDisp_RegBinReg('t',self->sv_reg,self->sv_reg,1);
  self->sv_flags = DCC_SFLAG_MKTEST(DCC_TEST_Z);
  self->sv_reg   = DCC_RC_CONST;
  self->sv_reg2  = DCC_RC_CONST;
  goto end_exclaim;
 } break;

#if 1
 case TOK_INC:
 case TOK_DEC:
  if (!(self->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_BITFLD)) &&
       (DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE)) {
   /* Special case: Add/Sub offset from register. */
   if (op == TOK_INC) self->sv_const.it += 1;
   else               self->sv_const.it -= 1;
   /* Set the X-offset flag to ensure the offset is added during pop. */
   self->sv_flags |= DCC_SFLAG_XOFFSET;
   return;
  }
  break;
#endif

 case '*':
  /* Indirection/Dereference. */
  DCCStackValue_FixTest(self);
  DCCStackValue_FixBitfield(self);
  DCCStackValue_Promote(self);
  if (self->sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(self);
  if (!(self->sv_ctype.t_type&DCCTYPE_POINTER))
   WARN(W_EXPECTED_POINTER_FOR_DEREF,&self->sv_ctype);
  else DCCType_MkBase(&self->sv_ctype);
  self->sv_flags |=   DCC_SFLAG_LVALUE;
  /* Make sure to delete the COW flag, as it was
   * only meant for the underlying pointer value. */
  self->sv_flags &= ~(DCC_SFLAG_COPY);
  assert(!(self->sv_flags&DCC_SFLAG_TEST));
  assert(!(self->sv_flags&DCC_SFLAG_BITFLD));
  return;

 case '&':
  /* Reference. */
  if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_LVALUE) {
   /* Simple case: Just turn an lvalue type into a pointer! */
   DCCType_MkBase(&self->sv_ctype);
   DCCType_MkPointer(&self->sv_ctype);
  } else if (DCCTYPE_ISARRAY(self->sv_ctype.t_type)) {
   /* Referencing an array simply promotes it to the associated pointer type. */
   DCCStackValue_Promote(self);
  } else if (self->sv_flags&DCC_SFLAG_LVALUE) {
   self->sv_flags &= ~(DCC_SFLAG_LVALUE);
   DCCType_MkPointer(&self->sv_ctype);
  } else {
   WARN(W_EXPECTED_LVALUE_FOR_REFERENCE,&self->sv_ctype);
   /* Force an lvalue on the stack, then reference that lvalue. */
   DCCStackValue_Kill(self);
   assert(self->sv_flags&DCC_SFLAG_LVALUE);
   self->sv_flags &= ~(DCC_SFLAG_LVALUE);
  }
  return;

 case '~':
  /* Optimization: '~(%REG - 1)' --> '-%REG' */
  if (!(self->sv_flags&DCC_SFLAG_LVALUE) &&
      !self->sv_sym && self->sv_const.it == -1) {
   self->sv_const.it = 0;
   op = '-';
  }
  break;

 default: break;
 }

 if (self->sv_flags&DCC_SFLAG_BITFLD) {
  /* Really hacky way to handle bitfields. */
  struct DCCStackValue temp = *self;
  DCCStackValue_FixBitfield(&temp);
  DCCStackValue_Unary(&temp,op);
  DCCStackValue_Store(&temp,self,0);
  *self = temp;
  return;
 }

 /* Kill all tests. */
 DCCVStack_KillTst();
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 /* Check for special case: LValue indirection. */
 DCCStackValue_LoadLValue(self);
 /* Invoke copy-on-write semantics. 
  * HINT: These are not invoked for '&' or '*' (ref/deref) */
 DCCStackValue_Cow(self);
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc temp;
  assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
  temp.ml_off = self->sv_const.offset;
  temp.ml_sym = self->sv_sym;
  temp.ml_reg = self->sv_reg;
  DCCDisp_UnaryMem(op,&temp,DCCType_Sizeof(&self->sv_ctype,NULL,1));
  return;
 }

 /* Check for special case: constant stack entry. */
 if (self->sv_reg == DCC_RC_CONST) {
  if (DCCStackValue_ConstUnary(self,op)) return;
  DCCStackValue_Load(self);
 }

 /* Regular register-operation. */
 DCCStackValue_FixRegOffset(self);
 if (self->sv_reg2 == DCC_RC_CONST) {
  DCCDisp_UnaryReg(op,self->sv_reg);
 } else if (op == '~') {
  DCCDisp_UnaryReg(op,self->sv_reg);
  DCCDisp_UnaryReg(op,self->sv_reg2);
 } else if (op == TOK_INC || op == TOK_DEC) {
  struct DCCSymAddr rhs_val;
  rhs_val.sa_off = 1;
  rhs_val.sa_sym = NULL;
  /* inc/dec don't affect the carry flag!
   * >> Instead, we must use 'add/sub' with an immediate value of '1' */
  DCCDisp_CstBinReg(op == TOK_INC ? '+' : '-',&rhs_val,self->sv_reg,1);
  rhs_val.sa_off = 0;
  DCCDisp_CstBinReg(op,&rhs_val,self->sv_reg2,1); /* add/sub w/ carry/borrow. */
 } else {
  /* Add more/new emulated 64-bit unary operations here. */
 }
 assert(op != '!');
 return;
end_exclaim:
 assert(op == '!');
 /* Make sure that 'operator !' returns a boolean. */
 DCCType_Quit(&self->sv_ctype);
 self->sv_ctype.t_type = DCCTYPE_BUILTIN|DCCTYPE_BOOL;
 self->sv_ctype.t_base = NULL;
}


PRIVATE void DCC_VSTACK_CALL
DCCStackValue_BinMem(struct DCCStackValue *__restrict self, tok_t op,
                     struct DCCMemLoc const *__restrict target, size_t n) {
 struct DCCSymAddr temp;
 assert(self);
 assert(target);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 /* Check for lvalue type. */
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc src;
  src.ml_reg = self->sv_reg;
  src.ml_off = self->sv_const.offset;
  src.ml_sym = self->sv_sym;
  DCCDisp_MemBinMem(op,&src,DCCType_Sizeof(&self->sv_ctype,NULL,1),
                    target,n,DCCStackValue_IsUnsignedOrPtr(self));
  return;
 }

 if (self->sv_reg == DCC_RC_CONST) {
  /* Store a regular constant in 'target'. */
  temp.sa_sym = self->sv_sym;
  switch (n) {
  case 1: temp.sa_off = (target_off_t)self->sv_const.u8; goto integral_common;
  case 2: temp.sa_off = (target_off_t)DCC_H2T16(self->sv_const.u16); goto integral_common;
  case 4: temp.sa_off = (target_off_t)DCC_H2T32(self->sv_const.u32);
integral_common: DCCDisp_CstBinMem(op,&temp,target,n,DCCStackValue_IsUnsignedOrPtr(self)); break;
#ifdef DCC_RC_I64
  case 8: temp.sa_off = (target_off_t)DCC_H2T64(self->sv_const.u64); goto integral_common;
#endif
  default:
   DCCDisp_VecBinMem(op,&self->sv_const,8,target,n,
                     DCCStackValue_IsUnsignedOrPtr(self));
   break;
  }
  return;
 }
 /* mov %reg, symaddr */
 DCCStackValue_FixRegOffset(self);
 DCCDisp_RegsBinMems(op,self->sv_reg,self->sv_reg2,target,
                     n,DCCStackValue_IsUnsignedOrPtr(self));
}
PRIVATE void DCC_VSTACK_CALL
DCCStackValue_BinReg(struct DCCStackValue *__restrict self,
                     tok_t op, rc_t dst, rc_t dst2) {
 rc_t source_reg;
 assert(self);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 source_reg = self->sv_reg;
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc src;
  src.ml_reg = self->sv_reg;
  src.ml_sym = self->sv_sym;
  src.ml_off = self->sv_const.offset;
  DCCDisp_MemsBinRegs(op,&src,DCCType_Sizeof(&self->sv_ctype,NULL,1),
                      dst,dst2,DCCStackValue_IsUnsignedOrPtr(self));
 } else if (source_reg == DCC_RC_CONST) {
#if DCC_TARGET_SIZEOF_ARITH_MAX < 8
  struct DCCSymExpr temp;
  temp.e_int = self->sv_const.it;
  temp.e_sym = self->sv_sym;
  DCCDisp_CstBinRegs(op,&temp,dst,dst2,
                     DCCStackValue_IsUnsignedOrPtr(self));
#else
  struct DCCSymAddr temp;
  temp.sa_off = self->sv_const.it;
  temp.sa_sym = self->sv_sym;
  DCCDisp_CstBinReg(op,&temp,dst,DCCStackValue_IsUnsignedOrPtr(self));
  if (dst2 != DCC_RC_CONST) DCCDisp_IntMovReg(0,dst2);
#endif
 } else {
  /* *op %source_reg, target */
  DCCStackValue_FixRegOffset(self);
  DCCDisp_RegsBinRegs(op,self->sv_reg,self->sv_reg2,dst,dst2,
                      DCCStackValue_IsUnsignedOrPtr(self));
 }
}

PRIVATE struct cmppair {
 tok_t a,b;
} const compare_pairs[] = {
 {TOK_EQUAL,        TOK_EQUAL},
 {TOK_NOT_EQUAL,    TOK_NOT_EQUAL},
 {TOK_LOWER,        TOK_GREATER},
 {TOK_GREATER,      TOK_LOWER},
 {TOK_LOWER_EQUAL,  TOK_GREATER_EQUAL},
 {TOK_GREATER_EQUAL,TOK_LOWER_EQUAL},
};

PRIVATE int DCC_VSTACK_CALL
DCCStackValue_ConstBinary(struct DCCStackValue *__restrict self, tok_t op,
                          struct DCCSymExpr const *__restrict exprval,
                          struct DCCStackValue *__restrict other) {
 int_t iv,rhsv;
 assert(self);
 assert(other);
 assert(self->sv_reg == DCC_RC_CONST);
 assert(self->sv_reg2 == DCC_RC_CONST);
 assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
 assert(exprval);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 /* TODO: Floating point? */
 iv   = self->sv_const.it;
 rhsv = exprval->e_int;
 switch (op) {

 case '=':
  /* Re-assign a stack-value. */
  iv = rhsv;
assign_sym:
  DCCSym_XIncref(exprval->e_sym);
  DCCSym_XDecref(self->sv_sym);
  self->sv_sym = exprval->e_sym;
  break;

 case '+':
 case TOK_INC:
  iv += rhsv;
  if (exprval->e_sym) {
   if (self->sv_sym) return 0;
   self->sv_sym = exprval->e_sym;
   DCCSym_Incref(exprval->e_sym);
  }
  break;

 case '-':
 case TOK_DEC:
  iv -= rhsv;
  /* Special case: Difference between two symbols. */
  if (self->sv_sym && exprval->e_sym) {
   if (self->sv_sym == exprval->e_sym) {
    /* Always succeeds: Difference between the same symbol. */
    goto del_target_sym;
   } else if (DCCSym_SECTION(self->sv_sym) &&
              DCCSym_SECTION(self->sv_sym) ==
              DCCSym_SECTION(exprval->e_sym)) {
    /* Special case: Difference between two defined symbols from the same section. */
    iv += self->sv_sym->sy_addr-exprval->e_sym->sy_addr;
del_target_sym:
    DCCSym_Decref(self->sv_sym);
    self->sv_sym = NULL;
    break;
   } else {
    /* Undefined symbols, or symbols from different sections. */
    return 0;
   }
  }
  goto assign_sym;

 case TOK_SHL:
 case TOK_SHR:
 case TOK_RANGLE3:
  if (rhsv < 0) WARN(W_NEGATIVE_SHIFT_OPERAND);
 case '&':
 case '|':
 case '^':
 case '*':
 case '/':
 case '%':
  if (self->sv_sym || exprval->e_sym) {
   WARN(W_UNSUPPORTED_SYMOBL_ARITHMETIC,(int)op);
   DCCSym_XDecref(self->sv_sym);
   self->sv_sym = NULL;
  }
       if (op == '&') iv &= rhsv;
  else if (op == '|') iv |= rhsv;
  else if (op == '^') iv ^= rhsv;
  else if (op == '*') iv *= rhsv;
  else if (op == TOK_SHL) *(uint_t *)&iv <<= rhsv;
  else if (op == TOK_SHR) *(uint_t *)&iv >>= rhsv;
  else if (op == TOK_RANGLE3) iv >>= rhsv;
  else if (!rhsv) WARN(W_DIVIDE_BY_ZERO);
  else if (op == '/') iv /= rhsv;
  else                iv %= rhsv;
  break;

 case TOK_LOWER:
 case TOK_LOWER_EQUAL:
 case TOK_EQUAL:
 case TOK_NOT_EQUAL:
 case TOK_GREATER:
 case TOK_GREATER_EQUAL:
  /* Special handling for compare operations. */
  if (self->sv_sym && exprval->e_sym) {
   /* Symbols are given on both sides. */
   if (DCCSym_SECTION(self->sv_sym) &&
       DCCSym_SECTION(self->sv_sym) ==
       DCCSym_SECTION(exprval->e_sym)) {
    /* Symbols are located within the same section. */
    iv   += self->sv_sym->sy_addr;
    rhsv += exprval->e_sym->sy_addr;
   } else {
    if ((op == TOK_EQUAL || op == TOK_NOT_EQUAL) &&
         /* Make sure that both symbols are defined in sections */
         DCCSym_SECTION(self->sv_sym)   && !(self->sv_sym->sy_flags&DCC_SYMFLAG_WEAK) &&
         DCCSym_SECTION(exprval->e_sym) && !(self->sv_sym->sy_flags&DCC_SYMFLAG_WEAK) &&
         /* Make sure that the constant symbol offsets indicate non-overlapping
          * (both offsets are identical, meaning that they cannot extend beyond
          *  the associated section, or one of the offsets)
          * TODO: This could be further optimized by checking if one of the offsets
          *       doesn't point out-of-bounds of the associated section
          *
          */
        (iv == rhsv || !iv || !rhsv)) {
     /* Special case: Symbols from different sections:
      * >> int a __attribute__((section(".a")));
      * >> int b __attribute__((section(".b")));
      * >> ...
      * >> 
      * >> int x = (&a == &b); // Must always be false.
      * >> int x = (&a != &b); // Must always be true.
      * >> int y = (&a <  &b); // Cannot be determined at compile-time.
      */
     iv = op == TOK_EQUAL ? 0 : 1;
     break;
    }
    /* Symbols from different sections (can't generate constant expression) */
    return 0;
   }
  } else if (self->sv_sym || exprval->e_sym) {
   /* Only one side has a symbol:
    * >> extern int x;
    * >> if (&x == NULL) { ... }
    */
   if ((!self->sv_sym || !(self->sv_sym->sy_flags&DCC_SYMFLAG_WEAK)) &&
       (!exprval->e_sym || !(exprval->e_sym->sy_flags&DCC_SYMFLAG_WEAK))) {
    int common_unsigned;
    tok_t cmp_op = op;
    /* Negate any overflow within the left operand on the right-hand-side:
     * #1: if (&x+10 >= 10) // Subtrace '10' on both sides
     * #2: if (&x >= 0) // Always true
     * #3: if (1)
     */
    if (exprval->e_sym) {
     /* Swap the operation. */
     struct cmppair const *pair = compare_pairs;
     int_t temp = iv; iv = rhsv; rhsv = temp;
     for (;;) { if (pair->a == cmp_op) { cmp_op = pair->b; break; } ++pair; }
    }
    rhsv -= iv;
#if DCC_DEBUG
    iv = 0;
#endif
    /* Optimizations like '&x < 0' --> 'false' and the like. */
    common_unsigned = DCCStackValue_IsUnsignedOrPtr(self) ||
                      DCCStackValue_IsUnsignedOrPtr(other);
    switch (cmp_op) {
#if DCC_TARGET_SIZEOF_POINTER == 8
#define PM ((int_t)0xffffffffffffffffll)
#else
#define PM ((int_t)0xffffffffl)
#endif
    case TOK_LOWER:       if (rhsv <  0) { if (common_unsigned) break; iv = 0; goto success; } /* &x < -1 */
                          if (rhsv == 0) { iv = 0; goto success; } /* &x < 0 */
                          if (rhsv > PM) { iv = 1; goto success; } /* &x < (uintmax_t)((uintptr_t)-1)+1 */
                          break;
    case TOK_LOWER_EQUAL: if (rhsv == -1) { iv = common_unsigned ? 1 : 0; goto success; } /* &x <= -1 */
                          if (rhsv <  0)  { if (common_unsigned) break; iv = 0; goto success; } /* &x <= -2 */
                          if (rhsv == 0 && DCC_VSTACK_ASSUME_SYMADDR_NONNULL) { iv = 0; goto success; } /* &x <= 0 */
                          if (rhsv >= PM) { iv = 1; goto success; } /* &x <= (uintptr_t)-1 */
                          break;
    case TOK_EQUAL:       if (rhsv <  0)  { iv = 0; goto success; } /* &x == -1 */
                          if (rhsv == 0 && DCC_VSTACK_ASSUME_SYMADDR_NONNULL) { iv = 0; goto success; } /* &x == 0 */
                          if (rhsv >  PM) { iv = 0; goto success; } /* &x == (uintmax_t)-1 */
                          break;
    case TOK_NOT_EQUAL:   if (rhsv <  0)  { iv = 1; goto success; } /* &x != -1 */
                          if (rhsv == 0 && DCC_VSTACK_ASSUME_SYMADDR_NONNULL) { iv = 1; goto success; } /* &x != 0 */
                          if (rhsv >  PM) { iv = 1; goto success; } /* &x != (uintmax_t)-1 */
                          break;
    case TOK_GREATER:     if (rhsv == -1) { iv = common_unsigned ? 0 : 1; goto success; } /* &x > -1 */
                          if (rhsv <   0) { if (common_unsigned) break; iv = 1; goto success; } /* &x > -2 */
                          if (rhsv ==  0 && DCC_VSTACK_ASSUME_SYMADDR_NONNULL) { iv = 1; goto success; } /* &x > 0 */
                          if (rhsv >= PM) { iv = 0; goto success; } /* &x > (uintptr_t)-1 */
                          break;
    default:              if (rhsv <  0) { if (common_unsigned) break; iv = 1; goto success; } /* &x >= -1 */
                          if (rhsv == 0) { iv = 1; goto success; } /* &x >= 0 */
                          if (rhsv > PM) { iv = 0; goto success; } /* &x >= (uintmax_t)((uintptr_t)-1)+1 */
                          break;
#undef PM
    }
   }
   return 0;
  }

  if (DCCStackValue_IsUnsignedOrPtr(self) ||
      DCCStackValue_IsUnsignedOrPtr(other)) {
   switch (op) {
    case TOK_LOWER:         iv = (uint_t)iv <  (uint_t)rhsv; break;
    case TOK_LOWER_EQUAL:   iv = (uint_t)iv <= (uint_t)rhsv; break;
    case TOK_GREATER:       iv = (uint_t)iv >  (uint_t)rhsv; break;
    case TOK_GREATER_EQUAL: iv = (uint_t)iv >= (uint_t)rhsv; break;
    default: goto signed_compare;
   }
  } else {
signed_compare:
   switch (op) {
    case TOK_LOWER:       iv = iv <  rhsv; break;
    case TOK_LOWER_EQUAL: iv = iv <= rhsv; break;
    case TOK_EQUAL:       iv = iv == rhsv; break;
    case TOK_NOT_EQUAL:   iv = iv != rhsv; break;
    case TOK_GREATER:     iv = iv >  rhsv; break;
    default:              iv = iv >= rhsv; break;
   }
  }
  /* Delete a potential symbol within the target-value. */
  if (self->sv_sym) {
   DCCSym_Decref(self->sv_sym);
   self->sv_sym = NULL;
  }
  break;

 default: return 0;
 }
success:
 self->sv_const.it = iv;
 DCCStackValue_ClampConst(self,W_TRUNC_INTEGRAL_OPERATION);
 return 1;
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Swap(struct DCCStackValue *__restrict a,
                   struct DCCStackValue *__restrict b) {
 struct DCCStackValue temp;
 temp = *a;
 *a = *b;
 *b = temp;
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Binary(struct DCCStackValue *__restrict self,
                     struct DCCStackValue *__restrict target,
                     tok_t op) {
 assert(self);
 assert(target);
 if (op != '=') {
  /* Warn if an arithmetic operation is used with a non-arithmetic structure type. */
  if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_STRUCTURE &&
     (!self->sv_ctype.t_base->d_attr ||
      !(self->sv_ctype.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)
      )) WARN(W_STRUCTURE_ARITHMETIC,&self->sv_ctype);
  if (DCCTYPE_GROUP(target->sv_ctype.t_type) == DCCTYPE_STRUCTURE &&
     (!target->sv_ctype.t_base->d_attr ||
      !(target->sv_ctype.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)
      )) WARN(W_STRUCTURE_ARITHMETIC,&target->sv_ctype);
 }
 switch (op) {

 {
 case TOK_LOWER:
 case TOK_LOWER_EQUAL:
 case TOK_EQUAL:
 case TOK_NOT_EQUAL:
 case TOK_GREATER:
 case TOK_GREATER_EQUAL:
#define lhs  target
#define rhs  self
  if (!(lhs->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST)) &&
       (lhs->sv_reg == DCC_RC_CONST)) {
   struct cmppair const *iter = compare_pairs;
   /* Lhs is a constant operand.
    * >> Swap the arguments to simply stuff further below! */
   DCCStackValue_Swap(lhs,rhs);
   for (;; ++iter) {
    if (op == iter->a) { op = iter->b; break; }
   }
  }
  if (!(lhs->sv_flags&DCC_SFLAG_LVALUE) &&
      !(rhs->sv_flags&DCC_SFLAG_LVALUE)) {
   if (rhs->sv_reg == DCC_RC_CONST) {
    if ((lhs->sv_flags&DCC_SFLAG_TEST) &&
        !lhs->sv_sym && !lhs->sv_const.it) {
     /* The lhs operand is a test, and the rhs operand is a constant.
      * Do some special optimizations for situation like this. */
#define A_FF  0 /* if (0) */
#define A_TT  1 /* if (1) */
#define A_NOP 2 /* if (x == 10) */
#define A_INV 3 /* if (!(x == 10)) */
#define ACTION(tok,zero,one,two) ((tok) << 16 | (zero) << 4 | (one) << 2 | (two))
     static int const actions[] = {
      /* if ((x == 10) < 0)       --> if (0)
       * if ((x == 10) < 1)       --> if (!(x == 10))
       * if ((x == 10) < *n>=2*)  --> if (1) */
      ACTION(TOK_LOWER,A_FF,A_INV,A_TT),
      /* if ((x == 10) <= 0)      --> if (!(x == 10))
       * if ((x == 10) <= 1)      --> if (1)
       * if ((x == 10) <= *n>=2*) --> if (1) */
      ACTION(TOK_LOWER_EQUAL,A_INV,A_TT,A_TT),
      /* if ((x == 10) == 0)      --> if (!(x == 10))
       * if ((x == 10) == 1)      --> if (x == 10)
       * if ((x == 10) == *n>=2*) --> if (0) */
      ACTION(TOK_EQUAL,A_INV,A_NOP,A_FF),
      /* if ((x == 10) != 0)      --> if (x == 10)
       * if ((x == 10) != 1)      --> if (!(x == 10))
       * if ((x == 10) != *n>=2*) --> if (1) */
      ACTION(TOK_NOT_EQUAL,A_NOP,A_INV,A_TT),
      /* if ((x == 10) > 0)      --> if (x == 10)
       * if ((x == 10) > 1)      --> if (0)
       * if ((x == 10) > *n>=2*) --> if (0) */
      ACTION(TOK_GREATER,A_NOP,A_FF,A_FF),
      /* if ((x == 10) >= 0)      --> if (1)
       * if ((x == 10) >= 1)      --> if (x == 10)
       * if ((x == 10) >= *n>=2*) --> if (0) */
      ACTION(TOK_GREATER_EQUAL,A_TT,A_NOP,A_FF),
     };
     int action = 0; /* Find the action associated with the operation. */
     while ((tok_t)(actions[action] >> 16) != op) ++action;
     action = actions[action] & 0x3f;
          if (rhs->sv_sym || rhs->sv_const.it >= 2); /* two */
     else if (rhs->sv_const.it == 1) action >>= 2;   /* one */
     else                            action >>= 4;   /* zero */
     action &= 3; /* Select the action mask. */
     /* 'action' is not one of 'A_*' */
     assert(lhs->sv_flags&DCC_SFLAG_TEST);
     switch (action) {
      case A_FF:
      case A_TT:
       /* Turn the test into a constant value. */
       lhs->sv_flags    = DCC_SFLAG_NONE;
       lhs->sv_reg      = DCC_RC_CONST;
       lhs->sv_reg2     = DCC_RC_CONST;
       lhs->sv_sym      = NULL;
       lhs->sv_const.it = action == A_TT ? 1 : 0;
       break;
      case A_INV:
       /* Invert the test. */
       lhs->sv_flags ^= (DCC_TEST_NBIT << DCC_SFLAG_TEST_SHIFT);
       break;
      default: break;
     }
     goto end_cmp;
#undef ACTION
#undef A_INV
#undef A_KEEP
#undef A_TT
#undef A_FF
    } else if (lhs->sv_reg == DCC_RC_CONST) {
     struct DCCSymExpr temp;
     /* Constant expression on both sides. */
     assert(!(lhs->sv_flags&DCC_SFLAG_LVALUE));
     assert(!(rhs->sv_flags&DCC_SFLAG_LVALUE));
     temp.e_int = rhs->sv_const.it;
     temp.e_sym = rhs->sv_sym;
     if (DCCStackValue_ConstBinary(lhs,op,&temp,rhs)) goto end_cmp;
    }
   } else if (lhs->sv_reg == rhs->sv_reg) {
    struct DCCSymExpr temp;
#if DCC_DEBUG
    int cresult;
#endif
    /* Special case: Neither side is an lvalue, meaning both operands
     *               are offsets, as well as share their base register.
     *               This case represents a special optimization
     *               for situations like this:
     *            >> int x,y;
     *            >> if (&x < &y) {
     *            >>   ...
     *            >> }
     */
    assert(!(lhs->sv_flags&DCC_SFLAG_LVALUE));
    assert(!(rhs->sv_flags&DCC_SFLAG_LVALUE));
    temp.e_int   = rhs->sv_const.it;
    temp.e_sym   = rhs->sv_sym;
    lhs->sv_reg  = DCC_RC_CONST;
    lhs->sv_reg2 = DCC_RC_CONST;
    /* The following must always succeed... */
#if DCC_DEBUG
    cresult = DCCStackValue_ConstBinary(lhs,op,&temp,rhs);
    assert(cresult);
#else
    DCCStackValue_ConstBinary(lhs,op,&temp,rhs);
#endif
    goto end_cmp;
   }
  }
  /* Kill all tests.
   * This needs to be done for cases like this:
   * >> assert((a == b) == (c == d));
   */
  DCCVStack_KillTst();
  assert(!(lhs->sv_flags&DCC_SFLAG_TEST));
  assert(!(rhs->sv_flags&DCC_SFLAG_TEST));
  /* Generate a compare operation. */
  DCCStackValue_Binary(self,target,'?');
  {
   int test;
   static struct cmpid {
    uint16_t tok,cmp,ucmp;
   } const compare_ids[] = {
    {TOK_LOWER,        DCC_TEST_L, DCC_TEST_B},
    {TOK_LOWER_EQUAL,  DCC_TEST_LE,DCC_TEST_BE},
    {TOK_EQUAL,        DCC_TEST_E, DCC_TEST_E},
    {TOK_NOT_EQUAL,    DCC_TEST_NE,DCC_TEST_NE},
    {TOK_GREATER,      DCC_TEST_G, DCC_TEST_A},
    {TOK_GREATER_EQUAL,DCC_TEST_GE,DCC_TEST_AE},
   };
   test = 0;
   while (compare_ids[test].tok != op) ++test;
   if (DCCStackValue_IsUnsignedOrPtr(self) ||
       DCCStackValue_IsUnsignedOrPtr(target))
        test = compare_ids[test].ucmp;
   else test = compare_ids[test].cmp;
   /* Setup the flags according to the test that should take place. */
   target->sv_flags   = DCC_SFLAG_MKTEST(test);
  }
  target->sv_reg      = DCC_RC_CONST;
  target->sv_reg2     = DCC_RC_CONST;
  target->sv_const.it = 0;
end_cmp:
  DCCType_Quit(&target->sv_ctype);
  /* Compare operations always return a boolean. */
  target->sv_ctype.t_type = DCCTYPE_BOOL;
  target->sv_ctype.t_base = NULL;
#undef rhs
#undef lhs
  return;
 } break;

 default: break;
 }

 if (target->sv_flags&DCC_SFLAG_BITFLD) {
  if (op == '?' ||
     (target->sv_flags&DCC_SFLAG_COPY))
   /* Simply fix bitfields for compare operations. */
   DCCStackValue_FixBitfield(target);
  else {
   /* Really hacky way to handle bitfields. */
   struct DCCStackValue temp = *target;
   DCCStackValue_FixBitfield(&temp);
   DCCStackValue_Binary(self,&temp,op);
   DCCStackValue_Store(&temp,target,0);
   *target = temp;
   return;
  }
 }
 DCCStackValue_FixBitfield(self);

 if (target->sv_reg == DCC_RC_CONST &&
     /* Make sure the caller doesn't expect to modify 'target' itself. */
    (target->sv_flags&DCC_SFLAG_COPY) &&
   !(target->sv_flags&DCC_SFLAG_LVALUE)) {
  /* NOTE: two-sided constant binary is handled later. */
  if (self->sv_reg == DCC_RC_CONST &&
    !(self->sv_flags&DCC_SFLAG_LVALUE)) goto default_binary;
  if ((op == '+' || op == '*' || op == '&' ||
       op == '|' || op == '^'/* || op == '?'*/) &&
      DCCTYPE_GROUP(self  ->sv_ctype.t_type) != DCCTYPE_LVALUE &&
      DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE) {
#ifdef __INTELLISENSE__
   /* This is how you write offsetof in a way that intellisense can understand!
    * >> How da fuq did you not realize that, Microsoft? You're the DEV of both! */
#undef offsetof
#define offsetof(s,m) (size_t)&((s *)0)->m
#endif
   target_siz_t reg_siz = DCC_RC_SIZE(self->sv_reg);
   if (self->sv_reg2 != DCC_RC_CONST) reg_siz += DCC_RC_SIZE(self->sv_reg);
   /* We must not allow the operands to be swapped
    * if it would cause one of them to be truncated. */
   if (reg_siz >= DCCType_Sizeof(&target->sv_ctype,NULL,1)) {
    char temp[sizeof(struct DCCStackValue)-offsetof(struct DCCStackValue,sv_flags)];
    /* Handle optimizations for commutative operators in rhs.
     * >> This not only simplifies optimization, but is kind-of
     *    a requirement to prevent assembly code like:
     * >> int x = 10+y;
     *    mov $10, %eax
     *    mov y, %ecx
     *    add %ecx, %eax
     *    mov %eax, x
     * >> // Instead of:
     *    mov y, %eax
     *    add $10, %eax
     *    mov %eax, x
     */
    memcpy(temp,&self->sv_flags,sizeof(temp));
    memcpy(&self->sv_flags,&target->sv_flags,sizeof(temp));
    memcpy(&target->sv_flags,temp,sizeof(temp));
    /* With the operands swapped, 'self' now contains
     * a value that the caller does not expect to modify.
     * With that in mind, we simply mark 'self' for COW,
     * so-as to ensure it will not be modified:
     * >> int x = 42;
     * >> int y = 10+x; // Otherwise, this may compile as 'y = (x += 10);'
     */
    target->sv_flags |= DCC_SFLAG_COPY;
    goto constant_rhs;
   }
  }
  if (!target->sv_sym) {
   switch (op) {
   case '-':
    if (!target->sv_const.it) {
     /* '0-foo()' --> '-foo()' */
     DCCStackValue_Swap(self,target);
     DCCStackValue_Unary(self,'-');
     return;
    }
    break;

   case TOK_SHR:
   case TOK_RANGLE3:
    if (target->sv_const.it == 1) {
     /* '1 >> foo()' --> '!foo()' */
     DCCStackValue_Swap(self,target);
     DCCStackValue_Unary(self,'!');
     return;
    }
   case '/':
   case '%':
   case TOK_SHL:
    if (!target->sv_const.it) {
     /* '0/foo()' --> '0' */
     /* '0%foo()' --> '0' */
     /* '0 << foo()' --> '0' */
     /* '0 >> foo()' --> '0' */
     return;
    }
    break;
   default: break;
   }
  }
  DCCStackValue_Load(target);
 }
 if (self->sv_reg == DCC_RC_CONST) {
constant_rhs:
  if (!(self->sv_sym) &&
      !(self->sv_flags&(DCC_SFLAG_TEST|DCC_SFLAG_LVALUE))) {
   assert(self->sv_reg == DCC_RC_CONST);
   switch (op) {

   case '+':
   case '-':
    /* 'foo -= 0' --> 'foo()' */
    /* 'foo += 0' --> 'foo()' */
    if (!self->sv_const.it) return;
    break;

   {
    int_t n,iv; int shift;
   case '*':
    iv = self->sv_const.it;
    if (!iv) {
     struct DCCStackValue zero;
set_zero:
     /* 'foo *= 0' --> 'foo = 0' */
     memset(&zero,0,sizeof(struct DCCStackValue));
     DCCStackValue_Store(&zero,target,0);
     return;
    }
    if (iv == 1) return; /* 'foo *= 1' --> 'foo()' */
    /* Special optimization for 2^n integers: compile as shift operations instead. */
    n = 2,shift = 1;
    while (n < iv && !(n&((int_t)1 << ((sizeof(int_t)*8)-1)))) n <<= 1,++shift;
    if (n == iv) {
     /* 'foo *= 2' --> 'foo() <<= 1'
      * 'foo *= 4' --> 'foo() <<= 2'
      * ...
      */
     self->sv_const.it = (int_t)shift;
     op = TOK_SHL;
    }
   } break;

   {
    int_t n,iv; int shift;
   case '/':
    iv = self->sv_const.it;
    if (!iv) { WARN(W_DIVIDE_BY_ZERO); break; }
    if (iv == 1) return;
    n = 2,shift = 1;
    while (n < iv && !(n&((int_t)1 << ((sizeof(int_t)*8)-1)))) n <<= 1,++shift;
    if (n == iv) {
     /* 'foo /= 2' --> 'foo >>= 1'
      * 'foo /= 4' --> 'foo >>= 2'
      * ...
      */
     self->sv_const.it = (int_t)shift;
     op = DCCStackValue_IsUnsignedOrPtr(target) ? TOK_SHR : TOK_RANGLE3;
    }
   } break;

   {
    int_t n,iv;
   case '%':
    iv = self->sv_const.it;
    if (!iv) { WARN(W_DIVIDE_BY_ZERO); break; }
    if (iv == 1) goto set_zero;
    n = 2;
    while (n < iv && !(n&((int_t)1 << ((sizeof(int_t)*8)-1)))) n <<= 1;
    if (n == iv) {
     /* 'foo %= 2' --> 'foo &= 1'
      * 'foo %= 4' --> 'foo &= 3'
      * ...
      */
     self->sv_const.it = n-1;
     op = TOK_AND;
    }
   } break;

   case TOK_SHL:
   case TOK_SHR:
   case TOK_RANGLE3:
    /* 'foo <<= 0' --> 'foo' */
    /* 'foo >>= 0' --> 'foo' */
    if (!self->sv_const.it) return;
    /* TODO: Special optimizations for bit shifting:
     * >> %ax >> 8; // '%ah'
     * >> %ax << 8; // 'mov %al, %ah'
     * >> int x; short y;
     * >> 'x >> 16'   --> 'movw x+2, %ax'
     * >> 'y >> 8'    --> 'movb y+1, %al'
     */
    break;

   case TOK_AND:
    /* 'foo &= 0' --> 'foo ^= foo' */
    /* 'foo &= -1' --> 'foo' */
    if (!self->sv_const.it) goto set_zero;
    /* TODO: Check the size of 'self' and if all bits of
     *       'iv' are set for that size, don't do anything. */
    if (self->sv_const.it == -1) return;
    /* TODO: Special optimizations for bit masking:
     * >> %eax & 0xffffffff; // '%eax' (no-op)
     * >> %eax & 0xffff; // '%ax'
     * >> %eax & 0xff; // '%al'
     * >> int x;
     * >> 'x & 0xff'   --> 'movb x, %al'
     * >> 'x & 0xff00' --> 'mozxb x+1, %ax'
     */
    break;

   case TOK_OR:
    /* 'foo |= 0' --> 'foo' */
    /* 'foo |= -1' --> 'foo = -1' */
    if (!self->sv_const.it) return;
    /* TODO: Check the size of 'self' and if no bits of
     *       'iv' are set for that size, don't do anything. */
    if (self->sv_const.it == -1) { DCCStackValue_Store(self,target,0); return; }
    break;

   case TOK_XOR:
    /* 'foo ^= 0' --> 'foo' */
    /* 'foo ^= -1' --> 'foo = ~foo' */
    if (!self->sv_const.it) return;
    if (self->sv_const.it == -1) {
     DCCStackValue_Unary(target,'~');
     return;
    }
    break;

   default: break;
   }
  }
 }
default_binary:

 DCCStackValue_LoadLValue(self);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 DCCStackValue_LoadLValue(target);

 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 assert(DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE);
 if ((op == '+' ||
      /* NOTE: When subtracting, the rhs operand must not be offset from a symbol,
       *       as the code below is only capable of _ADDING_ one symbol, but not
       *       subtracting it! */
     (op == '-' && !self->sv_sym)) &&
     (self->sv_reg == DCC_RC_CONST) &&
    (!(self->sv_flags&(DCC_SFLAG_TEST|DCC_SFLAG_LVALUE))) &&
    (!(target->sv_flags&DCC_SFLAG_TEST)) &&
     /* Can only do offset-arithmetic with one symbol at compile-time. */
    (!target->sv_sym || !self->sv_sym)) {
  int_t old_val;
  assert(self->sv_reg2 == DCC_RC_CONST);
  /* Make sure to load l-value stack values. */
  if (target->sv_flags&DCC_SFLAG_LVALUE)
      DCCStackValue_Load(target);
  assert(!(target->sv_flags&DCC_SFLAG_LVALUE));

  /* Special case: Add/Sub offset. */
  if (self->sv_sym) assert(!target->sv_sym),
                    target->sv_sym = self->sv_sym,
                    DCCSym_Incref(target->sv_sym);
  old_val = target->sv_const.it;
  if (op == '+') target->sv_const.it += self->sv_const.it;
  else           target->sv_const.it -= self->sv_const.it;
  if (((op == '+') ^ (self->sv_const.it < 0))
      ? (old_val > target->sv_const.it)
      : (old_val < target->sv_const.it)) {
   /* Under-/Over-flow during constant addition/subtraction. */
   WARN(op == '+' ? W_INTEGER_OVERFLOW_DURING_ADDITION
                  : W_INTEGER_UNDERFLOW_DURING_SUBTRACTION,
        target->sv_const.it);
  }
  /* Clamp the resulting constant to its proper typing. */
  DCCStackValue_ClampConst(target,W_TRUNC_INTEGRAL_ADDITION);
  /* Set the X-offset flag to ensure the offset
   * is added even when the value isn't used. */
  target->sv_flags |= DCC_SFLAG_XOFFSET;
  return;
 }

 /* Kill all tests. */
 DCCVStack_KillTst();
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 assert(!(target->sv_flags&DCC_SFLAG_TEST));

 /* Invoke copy-on-write for the target. */
 if (op != '?') DCCStackValue_Cow(target);

exec_mem_or_reg:
 if ((target->sv_flags&DCC_SFLAG_LVALUE) ||
      target->sv_reg == DCC_RC_CONST) {
  struct DCCMemLoc dest;
  if (!(target->sv_flags&DCC_SFLAG_LVALUE)) {
   /* Propagate the no-sign warnings flag. */
   target->sv_flags |= (self->sv_flags&DCC_SFLAG_NO_WSIGN);
   if (target->sv_reg == DCC_RC_CONST) {
    if (self->sv_reg == DCC_RC_CONST &&
        self->sv_reg2 == DCC_RC_CONST &&
      !(self->sv_flags&DCC_SFLAG_LVALUE)) {
     struct DCCSymExpr temp;
     /* Binary operation on constant expression. */
     temp.e_int = self->sv_const.it;
     temp.e_sym = self->sv_sym;
     if (DCCStackValue_ConstBinary(target,op,&temp,self)) return;
    } else {
     /* Load a constant expression into register storage. */
     DCCStackValue_Load(target);
    }
   } else if (op != '?') {
    WARN(W_EXPECTED_LVALUE_FOR_BINARY_OP,&target->sv_ctype);
   }
   assert(target->sv_reg != DCC_RC_CONST);
   goto exec_mem_or_reg;
  }
  /* Memory location. */
  dest.ml_reg = target->sv_reg;
  dest.ml_off = target->sv_const.offset;
  dest.ml_sym = target->sv_sym;
  /* Operate with a global variable. */
  DCCStackValue_BinMem(self,op,&dest,
                       DCCType_Sizeof(&target->sv_ctype,NULL,1));
 } else {
  /* Operate on a regular, old register. */
  DCCStackValue_FixRegOffset(target);
  DCCStackValue_BinReg(self,op,target->sv_reg,target->sv_reg2);
 }
}

PRIVATE rc_t const cls_size[] = {
 /*[DCCTYPE_INT]   = */DCC_TARGET_SIZEOF_INT,
 /*[DCCTYPE_BYTE]  = */1,
 /*[DCCTYPE_WORD]  = */DCC_TARGET_SIZEOF_SHORT,
 /*[DCCTYPE_INT64] = */8
};

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Cast(struct DCCStackValue *__restrict self,
                   struct DCCType const *__restrict type) {
 struct DCCDecl *base_decl; int group;
 assert(self);
 assert(type);
 /* Special case: Explicit cast to void. */
 if (DCCTYPE_ISBASIC(type->t_type,DCCTYPE_VOID)) {
  /* Anything can be cast to void. */
  assert(!type->t_base);
  DCCSym_XDecref(vbottom->sv_sym);
  DCCType_Quit(&vbottom->sv_ctype);
  vbottom->sv_reg      = DCC_RC_CONST;
  vbottom->sv_reg2     = DCC_RC_CONST;
  vbottom->sv_flags    = DCC_SFLAG_RVALUE;
  vbottom->sv_const.it = 0;
  vbottom->sv_sym      = NULL;
  vbottom->sv_ctype    = *type;
  return;
 }

 group = DCCTYPE_GROUP(self->sv_ctype.t_type);
 if (group == DCCTYPE_LVALUE) {
  /* Special case: lvalue --> lvalue cast. (is simply allowed) */
  if (DCCTYPE_GROUP(type->t_type) == DCCTYPE_LVALUE) goto done;
  /* Must load lvalues before casting to non-lvalue:
   * >> static int x = 42;
   * >> int &get_x() { return x; }
   * >> int main() {
   * >>   int v = (int)get_x();
   * >> }
   */
  DCCStackValue_Promote(self);
  DCCStackValue_LoadLValue(self);
 } else if (DCCTYPE_ISARRAY(type->t_type)) {
  /* Cast to array/vararray type. */
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  goto done;
 } else {
  DCCStackValue_Promote(self);
 }
 group = DCCTYPE_GROUP(self->sv_ctype.t_type);
 if (DCCTYPE_GROUP(type->t_type) == DCCTYPE_LVALUE) {
  assert(type->t_base);
  assert(group != DCCTYPE_LVALUE);
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  type = &type->t_base->d_type;
  goto done;
 }

 if (DCCTYPE_ISBASIC(type->t_type,DCCTYPE_BOOL)) {
  /* Cast to boolean: Force into test case for non-zero (aka. 0/1) */
  DCCStackValue_Unary(self,'!');
  DCCStackValue_Unary(self,'!');
  assert(!self->sv_ctype.t_base);
  assert(self->sv_ctype.t_type == DCCTYPE_BOOL);
  return;
 }
 assert(group != DCCTYPE_LVALUE);
 if (DCCTYPE_GROUP(type->t_type) == DCCTYPE_LVALUE) {
  /* Cast r-value to l-value: If necessary, kill the value to force it onto the stack. */
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  assert(self->sv_flags&DCC_SFLAG_LVALUE);
  self->sv_flags &= ~(DCC_SFLAG_LVALUE);
  goto done;
 }
 /* Pointer/Builtin types. */
 if (DCCTYPE_GROUP(type->t_type) == DCCTYPE_BUILTIN ||
     DCCTYPE_GROUP(type->t_type) == DCCTYPE_POINTER ||
     DCCTYPE_GROUP(type->t_type) == DCCTYPE_FUNCTION) {
  /* Cast between builtin types. */
  static rc_t const cls_regs[] = {
   /*[DCCTYPE_INT]   = */DCC_RC_I32|DCC_RC_I16|DCC_RC_I8,
   /*[DCCTYPE_BYTE]  = */DCC_RC_I8,
   /*[DCCTYPE_WORD]  = */DCC_RC_I16|DCC_RC_I8,
#ifdef DCC_RC_I64
   /*[DCCTYPE_INT64] = */DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8
#else
   /*[DCCTYPE_INT64] = */DCC_RC_I32|DCC_RC_I16|DCC_RC_I8
#endif
  };
  rc_t new_class;
  int to,tn,was_unsigned;
  if (group == DCCTYPE_POINTER || group == DCCTYPE_FUNCTION)
       to = DCCTYPE_INTPTR,was_unsigned = 1;
  else to = DCCTYPE_BASIC(self->sv_ctype.t_type),
       was_unsigned = (to&DCCTYPE_UNSIGNED) || (to == DCCTYPE_BOOL);
  if (DCCTYPE_GROUP(type->t_type) == DCCTYPE_POINTER ||
      DCCTYPE_GROUP(type->t_type) == DCCTYPE_FUNCTION)
       tn = DCCTYPE_INTPTR;
  else tn = DCCTYPE_BASIC(type->t_type);
  if (DCCTYPE_ISFLOAT(to) || DCCTYPE_ISFLOAT(tn)) {
   /* TODO: Cast to/from/between floating point types. */
  }
  /* Handle any other types (such as 'void' or 'auto') as int. */
  if (to >= DCCTYPE_FLOAT) to = DCCTYPE_INT;
  if (tn >= DCCTYPE_FLOAT) tn = DCCTYPE_INT;
  to &= ~(DCCTYPE_UNSIGNED);
  tn &= ~(DCCTYPE_UNSIGNED);
  /* Special case: No change in size. */
  if (to == tn) goto done;
  /* Fix tests. */
  DCCStackValue_FixTest(self);
  if (self->sv_flags&DCC_SFLAG_LVALUE) {
   /* Special case: Cast an l-value.
    * When the type's size increases, we must load the
    * value into a register, as not to access memory
    * past the end of the l-value.
    * Yet if the size is lowered, we can simply re-use
    * the l-value without having to change anything or
    * generating code. */
   if (cls_size[tn] <= cls_size[to]) goto done;
   DCCStackValue_Load(self); /* Load the value if the size increases. */
  }
  if (self->sv_reg == DCC_RC_CONST) {
   int_t mask = (((int_t)1 << cls_size[tn]*DCC_TARGET_BITPERBYTE)-1);
   /* Cast a constant operand. */
   /* Check the sign-bit and sign-extend if necessary. */
   if (!was_unsigned && self->sv_const.it&((mask+1)>>1))
        self->sv_const.it |= ~(mask);
   else self->sv_const.it &=  (mask);
   goto done;
  }
  if (to == DCCTYPE_BYTE && (self->sv_reg&4)) {
   /* High-order 8-bit register (Need to move into the low-order one) */
   DCCVStack_GetRegExact(DCC_RC_I8|(self->sv_reg&3));
  }
  assert(tn >= 0 && tn <= 3);
  new_class = cls_regs[tn];
  if ((self->sv_reg&4) &&
      (new_class&(DCC_RC_I16|DCC_RC_I3264))
      ) new_class &= ~(DCC_RC_I8);
  assert(new_class != 0);
  new_class |= self->sv_reg&DCC_RI_MASK;
  /* Move the old register into the new one.
   * NOTE: 'DCCDisp_RegMovReg' will automatically determine if a mov is really required. */
  DCCDisp_RegMovReg(self->sv_reg,new_class,was_unsigned);
  self->sv_reg  = new_class;
  self->sv_reg2 = DCC_RC_CONST;
#ifndef DCC_RC_I64
  if (tn == DCCTYPE_INT64) {
   assert(DCCTYPE_GROUP(type->t_type) != DCCTYPE_POINTER &&
          DCCTYPE_GROUP(type->t_type) != DCCTYPE_FUNCTION);
   self->sv_reg2 = DCCVStack_GetReg(DCC_RC_I32,1);
   if (!DCCTYPE_ISUNSIGNED_OR_PTR(type->t_type)) {
    /* sign-extend 'self->sv_reg' into 'self->sv_reg2' */
#if 0
    DCCStackValue_FixRegOffset(self);
#else
    if (self->sv_const.it || self->sv_sym) {
     struct DCCMemLoc off;
     off.ml_reg = self->sv_reg;
     off.ml_off = (target_off_t)self->sv_const.it;
     off.ml_sym = self->sv_sym;
     DCCDisp_LeaReg(&off,self->sv_reg2);
    } else
#endif
    {
     DCCDisp_RegMovReg(self->sv_reg,self->sv_reg2,1);
    }
    DCCDisp_SignMirrorReg(self->sv_reg2);
   } else {
    DCCDisp_IntMovReg(0,self->sv_reg2);
   }
  }
#endif
  goto done;
 }
 /* TODO: Cast structures? */

done:
 /* Set the new C-type. */
 base_decl = type->t_base;
 if (base_decl) DCCDecl_Incref(base_decl);
 self->sv_ctype.t_type = type->t_type;
 if (self->sv_ctype.t_base) DCCDecl_Decref(self->sv_ctype.t_base);
 self->sv_ctype.t_base = base_decl; /* Inherit reference. */
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Call(struct DCCStackValue *__restrict self) {
 struct DCCMemLoc src;
 assert(self);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 assert(!(self->sv_flags&DCC_SFLAG_BITFLD));
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 src.ml_reg = self->sv_reg;
 src.ml_off = self->sv_const.offset;
 src.ml_sym = self->sv_sym;
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  target_ptr_t pointer_size = DCCType_Sizeof(&self->sv_ctype,NULL,1);
  if (pointer_size > DCC_TARGET_SIZEOF_POINTER)
      pointer_size = DCC_TARGET_SIZEOF_POINTER;
  DCCDisp_UnaryMem('(',&src,pointer_size);
 } else {
  DCCDisp_LocCll(&src);
 }
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Jmp(struct DCCStackValue *__restrict self) {
 struct DCCMemLoc target_addr;
 assert(self);
 DCCStackValue_LoadLValue(self);
 DCCStackValue_Promote(self);
 DCCStackValue_FixBitfield(self);
 DCCStackValue_FixTest(self);
 target_addr.ml_reg = self->sv_reg;
 target_addr.ml_off = self->sv_const.offset;
 target_addr.ml_sym = self->sv_sym;
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  if (self->sv_reg2 != DCC_RC_CONST) WARN(W_JMP_TARGET_TRUNCATED);
  DCCDisp_MemJmp(&target_addr,DCCType_Sizeof(&self->sv_ctype,NULL,1));
 } else {
  DCCDisp_LocJmp(&target_addr);
 }
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Jcc(struct DCCStackValue *__restrict cond,
                  struct DCCStackValue *__restrict target,
                  int invert) {
 test_t gen_test;
 assert(cond);
 assert(target);
 /* Fix condition/target states. */
 DCCStackValue_LoadLValue(cond);
 DCCStackValue_Promote(cond);
 if (!(cond->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST)) &&
       cond->sv_reg == DCC_RC_CONST) {
  int cond_istrue;
  /* Special case: The condition is know at compile-time. */
  assert(cond->sv_reg2 == DCC_RC_CONST);
  cond_istrue = cond->sv_sym || cond->sv_const.it;
  cond_istrue ^= invert;
  if (cond_istrue) DCCStackValue_Jmp(target);
  return;
 }

 DCCStackValue_LodTest(cond);
 assert(!(cond->sv_flags&DCC_SFLAG_LVALUE));
 assert(cond->sv_flags&DCC_SFLAG_TEST);
 assert(cond->sv_reg == DCC_RC_CONST);
 assert(cond->sv_reg2 == DCC_RC_CONST);
 gen_test = DCC_SFLAG_GTTEST(cond->sv_flags);
 if (invert) gen_test ^= DCC_TEST_NBIT;
 assertf(gen_test <= 0xf,"gen_test = %x",gen_test);
 if (!(target->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_TEST|DCC_SFLAG_BITFLD)) &&
       DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE) {
  struct DCCMemLoc target_addr;
  /* Most likely case: Jump to constant address. */
  target_addr.ml_reg = target->sv_reg;
  target_addr.ml_off = target->sv_const.offset;
  target_addr.ml_sym = target->sv_sym;
  DCCDisp_LocJcc(gen_test,&target_addr);
 } else {
  struct DCCMemLoc target_addr;
  struct DCCSym *temp_sym;
  /* Must handle special cases. e.g.:
   * >> jcc %reg
   * Compile as:
   * >>     jnc 1f
   * >>     jmp %reg
   * >> 1:
   */
  temp_sym = DCCUnit_AllocSym();
  if unlikely(!temp_sym) return;
  target_addr.ml_reg = DCC_RC_CONST;
  target_addr.ml_off = 0;
  target_addr.ml_sym = temp_sym;
  /* Skip the jmp if the condition isn't met. */
  DCCDisp_LocJcc(gen_test^DCC_TEST_NBIT,&target_addr);
  DCCStackValue_Jmp(target);
  t_defsym(temp_sym);
 }
}


PRIVATE void DCC_VSTACK_CALL
DCCStackValue_AddOffset(struct DCCStackValue *__restrict self,
                        int_t off) {
 assert(self);
 /* Special case: No offset (don't do anything). */
 if unlikely(!off) return;
 /* Make sure this is an l-value. */
 if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
 assert(self->sv_flags&DCC_SFLAG_LVALUE);
 /* Simply add the offset here! */
 self->sv_const.it += off;
}

LEXPRIV struct DCCStructField *
DCCDecl_FindStructField(struct DCCDecl const *__restrict self,
                        struct TPPKeyword const *__restrict member_name,
                        target_off_t *path_offset) {
 struct DCCStructField *iter,*end,*result;
 struct DCCDecl *field_decl;
 assert(self);
 assert(member_name);
 assert(self->d_kind == DCC_DECLKIND_STRUCT ||
        self->d_kind == DCC_DECLKIND_UNION);
 end = (iter = self->d_tdecl.td_fieldv)+
               self->d_tdecl.td_size;
 for (; iter != end; ++iter) {
  field_decl = iter->sf_decl;
  assert(field_decl);
  if (field_decl->d_name == member_name) return iter;
  if (field_decl->d_name == &TPPKeyword_Empty &&
      DCCTYPE_GROUP(field_decl->d_type.t_type) == DCCTYPE_STRUCTURE) {
   assert(field_decl->d_type.t_base);
   assert(field_decl->d_type.t_base->d_kind == DCC_DECLKIND_STRUCT ||
          field_decl->d_type.t_base->d_kind == DCC_DECLKIND_UNION);
   /* Recursively search unnamed (aka. inlined) structures/unions. */
   result = DCCDecl_FindStructField(field_decl->d_type.t_base,member_name,path_offset);
   if (result) { if (path_offset) *path_offset += iter->sf_off; return result; }
  }
 }
 return NULL;
}

INTDEF size_t /* From 'tpp.c' */
fuzzy_match(char const *__restrict a, size_t alen,
            char const *__restrict b, size_t blen);

LEXPRIV struct DCCStructField *
DCCDecl_FindLikelyStructField(struct DCCDecl const *__restrict self,
                              struct TPPKeyword const *__restrict member_name,
                              size_t *pscore) {
 struct DCCStructField *iter,*end,*newfield;
 struct DCCStructField *result = NULL;
 struct DCCDecl *field_decl;
 size_t new_score,score = (size_t)-1;
 assert(self);
 assert(member_name);
 assert(self->d_kind == DCC_DECLKIND_STRUCT ||
        self->d_kind == DCC_DECLKIND_UNION);
 end = (iter = self->d_tdecl.td_fieldv)+
               self->d_tdecl.td_size;
 for (; iter != end; ++iter) {
  field_decl = iter->sf_decl;
  assert(field_decl);
  if (field_decl->d_name != &TPPKeyword_Empty) {
   new_score = fuzzy_match(field_decl->d_name->k_name,
                           field_decl->d_name->k_size,
                           member_name->k_name,
                           member_name->k_size);
   if (new_score < score) { result = iter; score = new_score; }
  } else if (DCCTYPE_GROUP(field_decl->d_type.t_type) == DCCTYPE_STRUCTURE) {
   assert(field_decl->d_type.t_base);
   assert(field_decl->d_type.t_base->d_kind == DCC_DECLKIND_STRUCT ||
          field_decl->d_type.t_base->d_kind == DCC_DECLKIND_UNION);
   /* Recursively search unnamed (aka. inlined) structures/unions. */
   newfield = DCCDecl_FindLikelyStructField(field_decl->d_type.t_base,
                                            member_name,&new_score);
   if (new_score < score) { result = newfield; score = new_score; }
  }
 }
 if (pscore) *pscore = score;
 return result;
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Subscript(struct DCCStackValue *__restrict self,
                        struct TPPKeyword const *__restrict member_name) {
 struct DCCStructField *field;
 struct DCCDecl *field_type_base;
 target_off_t path_offset = 0;
 assert(self);
 assert(member_name);
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 DCCStackValue_LoadLValue(self);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 if (DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_STRUCTURE) goto err;
 field = DCCDecl_FindStructField(self->sv_ctype.t_base,member_name,&path_offset);
 if unlikely(!field) goto err;
 DCCStackValue_AddOffset(self,field->sf_off+path_offset);
 /* Inherit the typing of this field. */
 field_type_base = field->sf_decl->d_type.t_base;
 if (field_type_base) DCCDecl_Incref(field_type_base);
 self->sv_ctype.t_type &= DCCTYPE_QUAL; /* Keep const/volatile qualifiers from the struct type. */
 self->sv_ctype.t_type |= field->sf_decl->d_type.t_type;
 assert(self->sv_ctype.t_base);
 DCCDecl_Decref(self->sv_ctype.t_base);
 self->sv_ctype.t_base = field_type_base;
 assert(self->sv_flags&DCC_SFLAG_LVALUE);
 /* Check for a bitfield modifier. */
 if (field->sf_bitfld) {
  DCCStackValue_FixTest(self);
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_FixRegOffset(self);
  DCCStackValue_FixBitfield(self);
  self->sv_flags |= field->sf_bitfld;
  assert(!(self->sv_flags&DCC_SFLAG_TEST));
  /* Quickly fix constant bitfields. */
  if (!(self->sv_flags&DCC_SFLAG_LVALUE) &&
       (self->sv_reg == DCC_RC_CONST)
       ) DCCStackValue_FixBitfield(self);
 }
 return;
err:
 /* Search for a likely match for the given member name. */
 if (DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_STRUCTURE) field = NULL;
 else field = DCCDecl_FindLikelyStructField(self->sv_ctype.t_base,member_name,NULL);
 assert(!field || field->sf_decl);
 assert(!field || field->sf_decl->d_name);
 assert(!field || field->sf_decl->d_name != &TPPKeyword_Empty);
 WARN(W_TYPE_UNKNOWN_FIELD,&self->sv_ctype,member_name,
      field ? field->sf_decl->d_name->k_name : "??" "?");
}


PUBLIC void DCC_VSTACK_CALL
DCCStackValue_Promote(struct DCCStackValue *__restrict self) {
 assert(self);
 switch (DCCTYPE_GROUP(self->sv_ctype.t_type)) {
 {
  int is_vla;
 case DCCTYPE_ARRAY:
 case DCCTYPE_VARRAY:
  /* An array should always reside on the stack,
   * but let's not be too stingy about this... */
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  /* NOTE: The array memory location is currently denoted
   *       by 'self->sv_reg+self->sv_const.offset'.
   *       Neither must be modified, as without the LVALUE flag,
   *       the 'self->sv_reg' and 'self->sv_const.offset' will
   *       describe a register & offset to add to that register,
   *       which in turn results in the logical address of the
   *       original array.
   *    >> The rest of what is happening below simply converts
   *       the array type into a pointer of its base.
   */
  assert(self->sv_flags&DCC_SFLAG_LVALUE);
  assert(self->sv_ctype.t_base);
  assert(self->sv_ctype.t_base->d_kind == DCC_DECLKIND_ARRAY ||
         self->sv_ctype.t_base->d_kind == DCC_DECLKIND_VLA);
  is_vla = self->sv_ctype.t_base->d_kind == DCC_DECLKIND_VLA;
  DCCType_MkBase(&self->sv_ctype);
  DCCType_MkPointer(&self->sv_ctype);
  /* This part is still _very_ important:
   * >> int array[42];
   * >> *(int **)array; // When 'DCC_SFLAG_LVALUE' is set.
   * >> (int *)array;   // When 'DCC_SFLAG_LVALUE' isn't set.
   * With this, we actually turn the array into the proper pointer.
   * NOTE: Since this is just a pointer now, we don't need to copy it either!
   */
  if (is_vla) {
   /* We mustn't allow users to write to the VLA pointer. */
   self->sv_ctype.t_type |= DCCTYPE_CONST;
   /* The VLA is actually a pointer offset from EBP. */
   assert(self->sv_flags&DCC_SFLAG_LVALUE);
  } else {
   self->sv_flags &= ~(DCC_SFLAG_LVALUE/*|DCC_SFLAG_COPY*/);
  }
 } break;
 case DCCTYPE_FUNCTION:
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  DCCType_MkPointer(&self->sv_ctype);
  self->sv_flags &= ~(DCC_SFLAG_LVALUE/*|DCC_SFLAG_COPY*/);
  break;
 {
  struct DCCType *lv_base;
 case DCCTYPE_LVALUE:
  assert(self->sv_ctype.t_base);
  lv_base = &self->sv_ctype.t_base->d_type;
  if (DCCTYPE_GROUP(lv_base->t_type) == DCCTYPE_ARRAY ||
      DCCTYPE_GROUP(lv_base->t_type) == DCCTYPE_FUNCTION) {
   /* Promote lvalue to array/function --> pointer/pointer-to-function. */
   DCCType_MkBase(&self->sv_ctype);
   DCCType_MkBase(&self->sv_ctype);
   DCCType_MkPointer(&self->sv_ctype);
  }
 } break;
 default: break;
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_PromoteFunction(struct DCCStackValue *__restrict self) {
 assert(self);
 /* Only promote function types. */
 if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_FUNCTION) {
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  DCCType_MkPointer(&self->sv_ctype);
  self->sv_flags &= ~(DCC_SFLAG_LVALUE|DCC_SFLAG_COPY);
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_ClampConst(struct DCCStackValue *__restrict self, int wid) {
 size_t size;
 int_t mask;
 int is_unsigned;
 assert(self);
 /* In l-value stack-values, the constant field is a pointer offset. */
 if (self->sv_flags&DCC_SFLAG_LVALUE) return;
 /* In an other kind of stack-value, the constant field is a value
  * either used as a stack-alone, or as an addend to the final value. */
 if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_POINTER ||
     DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_FUNCTION) {
  /* Clamp builtin types. */
  size = DCC_TARGET_SIZEOF_POINTER;
  is_unsigned = 1;
 } else if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_BUILTIN) {
  size = DCCTYPE_BASIC(self->sv_ctype.t_type);
       if (size == DCCTYPE_BOOL) size = 1;
  else if (size >  DCCTYPE_BOOL) size = DCC_TARGET_SIZEOF_INT;
  else if (size >= DCCTYPE_FLOAT) return;
  is_unsigned = (size&DCCTYPE_UNSIGNED);
  size = cls_size[size&~(DCCTYPE_UNSIGNED)];
 } else return;
 mask = (((int_t)1 << size*DCC_TARGET_BITPERBYTE)-1);
 /* Check the sign-bit and sign-extend if necessary. */
 if (!is_unsigned && self->sv_const.it&((mask+1)>>1)) self->sv_const.it |= ~(mask);
 else if (self->sv_const.it&~mask) {
  /* Don't emit a warning if the stack-value depends on a symbol or register. */
  if (wid && self->sv_reg == DCC_RC_CONST && !self->sv_sym)
      WARN(wid,self->sv_const.it);
  self->sv_const.it &= mask;
 }
}

#if (DCC_TARGET_SIZEOF_CHAR < DCC_TARGET_SIZEOF_INT) || \
    (DCC_TARGET_SIZEOF_SHORT < DCC_TARGET_SIZEOF_INT) || \
    (DCC_TARGET_SIZEOF_LONG_LONG < DCC_TARGET_SIZEOF_INT)
PUBLIC void DCC_VSTACK_CALL
DCCStackValue_PromoteInt(struct DCCStackValue *__restrict self) {
 tyid_t tid;
 assert(self);
 /* Make sure never */
 tid = self->sv_ctype.t_type;
 if (DCCTYPE_GROUP(tid) == DCCTYPE_BUILTIN) {
  assert(!self->sv_ctype.t_base);
  tid &= DCCTYPE_BASICMASK;
  switch (tid) {
  case DCCTYPE_BOOL:
  case DCCTYPE_AUTO:
#if DCC_TARGET_SIZEOF_CHAR < DCC_TARGET_SIZEOF_INT
  case DCCTYPE_BYTE:
  case DCCTYPE_UNSIGNED|DCCTYPE_BYTE:
#endif
#if DCC_TARGET_SIZEOF_SHORT < DCC_TARGET_SIZEOF_INT
  case DCCTYPE_WORD:
  case DCCTYPE_UNSIGNED|DCCTYPE_WORD:
#endif
#if 8 < DCC_TARGET_SIZEOF_INT
  case DCCTYPE_IB8:
  case DCCTYPE_UNSIGNED|DCCTYPE_IB8:
#endif
   if (self->sv_flags&DCC_SFLAG_LVALUE) {
    /* Must load the stack value into a register.
     * >> This must be done to conform to the new storage rules. */
    DCCStackValue_Load(self);
   }
   /* Nothing else to do here.
    * The actual effect of this is handled once the value is used!
    * NOTE: After loading l-value operands, we must also
    *       remove c/v qualifiers to prevent warnings. */
   self->sv_ctype.t_type &= ~(DCCTYPE_BASICMASK|DCCTYPE_ALTMASK|DCCTYPE_QUAL);
#if DCCTYPE_INT != 0
   self->sv_ctype.t_type |=   DCCTYPE_INT;
#endif
   break;
  default:
   break;
  }
 }
}
#endif

PUBLIC rc_t DCC_VSTACK_CALL
DCCVStack_GetReg(rc_t rc, int allow_ptr_regs) {
 struct DCCStackValue *iter,*end;
 uint8_t r,in_use,pref; /* Bitset of registers already in use. */
 int second_pass;
#ifdef DCC_RC_I64
 if (rc&DCC_RC_I64) rc |= DCC_RC_I32;
#endif
 if (rc&DCC_RC_I32) rc |= DCC_RC_I16;
 if (rc&DCC_RC_I16) rc |= DCC_RC_I8;
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) return rc;
 pref = rc&7,rc &= ~7;
again:
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 in_use = 0;
 (void)allow_ptr_regs;
 if (rc&(DCC_RC_I16|DCC_RC_I3264)) {
  in_use |= (1 << DCC_ASMREG_SP)
           |(1 << DCC_ASMREG_BP)
#ifdef IA32_PROTECTED_REGISTERS
           |(1 << DCC_ASMREG_BX)
           |(1 << DCC_ASMREG_DI)
           |(1 << DCC_ASMREG_SI)
#endif /* IA32_PROTECTED_REGISTERS */
           ;
#ifndef IA32_PROTECTED_REGISTERS
  /* Mask SI/DI as in-use if we're not allowed to allocate them! */
  if (!allow_ptr_regs)
   in_use |= (1 << DCC_ASMREG_SI)|(1 << DCC_ASMREG_DI);
#endif /* !IA32_PROTECTED_REGISTERS */
#ifdef IA32_PROTECTED_REGISTERS
 } else if (rc&DCC_RC_I8) {
  in_use |= (1 << DCC_ASMREG_BL)
           |(1 << DCC_ASMREG_BH);
#endif /* IA32_PROTECTED_REGISTERS */
 } else if (rc == DCC_RC_SEG) {
  in_use |= (1 << 6)|(1 << 7); /* There is no 6'th, or 7'th segment register. */
 }
 if (rc&DCC_RC_I) {
  /* Special case: Allocate integer register. */
  for (; iter != end; ++iter) {
   if (iter->sv_reg&DCC_RC_I) {
    r = iter->sv_reg&DCC_RI_MASK;
    /* Convert ah, ch, dh and bh --> al, cl, dl and bl. */
    if (!(iter->sv_reg&(DCC_RC_I16|DCC_RC_I3264)) &&
         (rc&(DCC_RC_I16|DCC_RC_I3264))) r &= ~4;
    in_use |= (1 << r);
   }
   if (iter->sv_reg2&DCC_RC_I) {
    r = iter->sv_reg2&DCC_RI_MASK;
    /* Convert ah, ch, dh and bh --> al, cl, dl and bl. */
    if (!(iter->sv_reg2&(DCC_RC_I16|DCC_RC_I3264)) &&
         (rc&(DCC_RC_I16|DCC_RC_I3264))) r &= ~4;
    in_use |= (1 << r);
   }
  }
 } else for (; iter != end; ++iter) {
  if ((iter->sv_reg&DCC_RC_MASK) == rc) in_use |= (1 << (iter->sv_reg&DCC_RI_MASK));
  if ((iter->sv_reg2&DCC_RC_MASK) == rc) in_use |= (1 << (iter->sv_reg2&DCC_RI_MASK));
 }
 /* Check if unused registers are available. */
 if (in_use != 0xff) {
  /* Check if the preferred register is free. */
  if (!(in_use&(1 << pref))) r = pref;
  else {
   /* Search for the first unused register.
    * HINT: We already know there are some! */
   r = 0;
   while (in_use&(1 << r)) ++r;
   assert(r < 8);
   /* Return the combined register class. */
  }
  if (rc != DCC_RC_I8 && r&4) rc &= ~(DCC_RC_I8);
  return rc|r;
 }
 if (allow_ptr_regs&2) return 0;
 /* Special case: Must kill a register by saving it on the stack. */
 iter = compiler.c_vstack.v_bottom;
 for (second_pass = 0; second_pass < 2; ++second_pass) {
  /* NOTE: Must iterate the stack in reverse as not to kill registers of
   *       close-by stack-values (such as those used by binary operation pairs). */
  do {
   assert(iter != end);
   --end;
   /* Only kill explicit register values on a second pass (aka, when we're forced, to!) */
   if ((!(end->sv_flags&DCC_SFLAG_XREGISTER) || second_pass) &&
       (((rc&DCC_RC_I) && ((end->sv_reg&DCC_RC_I) ||
                           (end->sv_reg2&DCC_RC_I))) ||
        ((end->sv_reg&DCC_RC_MASK) == rc) ||
        ((end->sv_reg2&DCC_RC_MASK) == rc))) {
#ifdef IA32_PROTECTED_REGISTERS
    if (end->sv_reg&(DCC_RC_I16|DCC_RC_I3264)) {
     if ((end->sv_reg&4) ||
         (end->sv_reg&7) == DCC_ASMREG_BX) continue;
    } else {
     if ((end->sv_reg&7) == DCC_ASMREG_BL ||
         (end->sv_reg&7) == DCC_ASMREG_BH) continue;
    }
#else /* IA32_PROTECTED_REGISTERS */
    /* Make sure to skip pointer registers if those aren't allowed. */
    if (!allow_ptr_regs && (
       (end->sv_reg&(DCC_RC_I16|DCC_RC_I3264) && DCC_ASMREG_ISSPTR(end->sv_reg&7)) ||
       (end->sv_reg2&(DCC_RC_I16|DCC_RC_I3264) && DCC_ASMREG_ISSPTR(end->sv_reg2&7))
       )) continue;
#endif /* !IA32_PROTECTED_REGISTERS */
    /* Kill the stack value. */
    DCCStackValue_Kill(end);
    /* Start over.
     * NOTE: Must search again for a case like 'al'+'ah' were
     *       both in use, but we only killed one of them. */
    goto again;
   }
  } while (end != iter);
  end = compiler.c_vstack.v_end;
 }
 assertf(0,"Should never get here!");
 return rc;
}
PUBLIC rc_t DCC_VSTACK_CALL
DCCVStack_GetRegExact(rc_t rcr) {
 struct DCCStackValue *iter,*end;
#ifdef DCC_RC_I64
 if (rcr&DCC_RC_I64) rcr |= DCC_RC_I32;
#endif
 if (rcr&DCC_RC_I32) rcr |= DCC_RC_I16;
 if (rcr&DCC_RC_I16 && !(rcr&4)) rcr |= DCC_RC_I8;
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) return rcr;
again:
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if (iter->sv_reg == rcr || iter->sv_reg2 == rcr) {
   /* Register is already in use! */
   DCCStackValue_Kill(iter);
   goto again;
  }
 }
 return rcr;
}
PUBLIC rc_t DCC_VSTACK_CALL
DCCVStack_GetRegOf(rc_t rc, uint8_t wanted_set) {
 struct DCCStackValue *iter,*end;
 uint8_t r,in_use; /* Bitset of registers already in use. */
 int second_pass;
 assertf(!(rc&DCC_RI_MASK),"The register part of RC must be ZERO");
#ifdef DCC_RC_I64
 if (rc&DCC_RC_I64) rc |= DCC_RC_I32;
#endif
 if (rc&DCC_RC_I32) rc |= DCC_RC_I16;
 if (rc&DCC_RC_I16) rc |= DCC_RC_I8;
again:
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 if (rc&(DCC_RC_I16|DCC_RC_I3264)) {
  wanted_set &= ~((1 << DCC_ASMREG_SP)
                 |(1 << DCC_ASMREG_BP)
#ifdef IA32_PROTECTED_REGISTERS
                 |(1 << DCC_ASMREG_BX)
                 |(1 << DCC_ASMREG_DI)
                 |(1 << DCC_ASMREG_SI))
#endif /* IA32_PROTECTED_REGISTERS */
                 ;
#ifdef IA32_PROTECTED_REGISTERS
 } else if (rc&DCC_RC_I8) {
  wanted_set &= ~((1 << DCC_ASMREG_BL)
                 |(1 << DCC_ASMREG_BH));
#endif /* IA32_PROTECTED_REGISTERS */
 } else if (rc == DCC_RC_SEG) {
  wanted_set &= ~((1 << 6)|(1 << 7)); /* There is no 6'th, or 7'th segment register. */
 }
 assertf(wanted_set != 0,"An empty or protected wanted-set was given");
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) {
  rc_t rid = 0; /* Fake-allocate the first register when no code should be generated. */
  while (!(wanted_set&1)) wanted_set >>= 1,++rid;
  return rc|rid;
 }
 /* Don't consider any registers not part of the wanted-set. */
 in_use = ~(wanted_set);
 if (rc&DCC_RC_I) {
  /* Special case: Allocate integer register. */
  for (; iter != end; ++iter) {
   if (iter->sv_reg&DCC_RC_I) {
    r = iter->sv_reg&DCC_RI_MASK;
    /* Convert ah, ch, dh and bh --> al, cl, dl and bl. */
    if (!(iter->sv_reg&(DCC_RC_I16|DCC_RC_I3264)) &&
         (rc&(DCC_RC_I16|DCC_RC_I3264))) r &= ~4;
    in_use |= (1 << r);
   }
   if (iter->sv_reg2&DCC_RC_I) {
    r = iter->sv_reg2&DCC_RI_MASK;
    /* Convert ah, ch, dh and bh --> al, cl, dl and bl. */
    if (!(iter->sv_reg2&(DCC_RC_I16|DCC_RC_I3264)) &&
         (rc&(DCC_RC_I16|DCC_RC_I3264))) r &= ~4;
    in_use |= (1 << r);
   }
  }
 } else for (; iter != end; ++iter) {
  if ((iter->sv_reg&DCC_RC_MASK) == rc) in_use |= (1 << (iter->sv_reg&DCC_RI_MASK));
  if ((iter->sv_reg2&DCC_RC_MASK) == rc) in_use |= (1 << (iter->sv_reg2&DCC_RI_MASK));
 }
 if (in_use != 0xff) {
  /* Unused registers are available. */
  r = 0;
  while (in_use&(1 << r)) ++r;
  assert(r < 8);
  /* Return the combined register class. */
  if (rc != DCC_RC_I8 && r&4) rc &= ~(DCC_RC_I8);
  assert(wanted_set&(1 << r));
  return rc|r;
 }
 /* Special case: Must kill a register by saving it on the stack. */
 iter = compiler.c_vstack.v_bottom;
 for (second_pass = 0; second_pass < 2; ++second_pass) {
  /* NOTE: Must iterate the stack in reverse as not to kill registers of
   *       close-by stack-values (such as those used by binary operation pairs). */
  do {
   assert(iter != end);
   --end;
   /* Only kill explicit register values on a second pass (aka, when we're forced, to!) */
   if ((!(end->sv_flags&DCC_SFLAG_XREGISTER) || second_pass) &&
       (((rc&DCC_RC_I) && ((end->sv_reg&DCC_RC_I) ||
                           (end->sv_reg2&DCC_RC_I))) ||
        ((end->sv_reg&DCC_RC_MASK) == rc) ||
        ((end->sv_reg2&DCC_RC_MASK) == rc))) {
    /* Make sure to skip pointer registers if those aren't allowed. */
    if (!(wanted_set&(1 << (end->sv_reg&7))) &&
        (end->sv_reg2 == DCC_RC_CONST ||
        !(wanted_set&(1 << (end->sv_reg2&7)))))
        continue;
    /* Kill the stack value. */
    DCCStackValue_Kill(end);
    assert(end->sv_flags&DCC_SFLAG_LVALUE);
    assert(end->sv_reg == DCC_RR_XBP);
    assert(end->sv_reg2 == DCC_RC_CONST);
    /* Start over.
     * NOTE: Must search again for a case like 'al'+'ah' were
     *       both in use, but we only killed one of them. */
    goto again;
   }
  } while (end != iter);
  end = compiler.c_vstack.v_end;
 }
 assertf(0,"Should never get here!");
 return rc;
}
PUBLIC int DCC_VSTACK_CALL
DCCVStack_GetRegInuse(rc_t rcr) {
 struct DCCStackValue *iter,*end;
#ifdef DCC_RC_I64
 if (rcr&DCC_RC_I64) rcr |= DCC_RC_I32;
#endif
 if (rcr&DCC_RC_I32) rcr |= DCC_RC_I16;
 if (rcr&DCC_RC_I16 && !(rcr&4)) rcr |= DCC_RC_I8;
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if (iter->sv_reg == rcr ||
      iter->sv_reg2 == rcr) return 1;
 }
 return 0;
}
PUBLIC rc_t DCC_VSTACK_CALL
DCCVStack_KillXNon(rc_t rcr) {
 struct DCCStackValue *iter,*end;
#ifdef DCC_RC_I64
 if (rcr&DCC_RC_I64) rcr |= DCC_RC_I32;
#endif
 if (rcr&DCC_RC_I32) rcr |= DCC_RC_I16;
 if (rcr&DCC_RC_I16 && !(rcr&4)) rcr |= DCC_RC_I8;
again:
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if ((iter->sv_reg == rcr || iter->sv_reg2 == rcr) &&
     !(iter->sv_flags&DCC_SFLAG_XREGISTER)) {
   /* Register is already in use! */
   DCCStackValue_Kill(iter);
   goto again;
  }
 }
 return rcr;
}
PUBLIC rc_t DCC_VSTACK_CALL
DCCVStack_CastReg(rc_t reg, int reg_unsigned,
                  rc_t new_class) {
 rc_t result;
 /* Special case: Already the correct storage class. */
 if ((reg&DCC_RC_MASK) == new_class) return reg;
 /* Special case: The new class is implicitly part of the current (e.g.: '%eax' -> '%ax'). */
 if ((new_class&DCC_RC_I8) && (reg&DCC_RC_I8) &&
     (reg&DCC_RC_MASK) >= new_class) return (reg&~(DCC_RC_MASK))|new_class;
 /* TODO: Optimization for 'DCCVStack_CastReg(%AL,1,DCC_RC_I16)' --> 'xor %ah, %ah' + return %AX; */
 result = DCCVStack_GetReg(new_class,1);
 DCCDisp_RegMovReg(reg,result,reg_unsigned);
 return result;
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_KillAll(size_t n_skip) {
 struct DCCStackValue *iter,*end;
 assert(n_skip <= vsize);
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) return;
 end  = compiler.c_vstack.v_end;
 iter = compiler.c_vstack.v_bottom+n_skip;
 for (; iter != end; ++iter) {
  if (((iter->sv_reg&DCC_RC_I) && !DCC_ASMREG_ISPROTECTED(iter->sv_reg&DCC_RI_MASK)) ||
      ((iter->sv_reg2&DCC_RC_I) && !DCC_ASMREG_ISPROTECTED(iter->sv_reg2&DCC_RI_MASK))) {
   /* Kill this stack value. */
   DCCStackValue_Kill(iter);
   assert(iter->sv_flags&DCC_SFLAG_LVALUE);
  }
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_KillInt(uint8_t mask) {
 struct DCCStackValue *iter,*end;
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) return;
 end   = compiler.c_vstack.v_end;
 iter  = compiler.c_vstack.v_bottom;
 mask &= ~((1 << DCC_ASMREG_EBP)
          |(1 << DCC_ASMREG_ESP)
#ifdef IA32_PROTECTED_REGISTERS
          |(1 << DCC_ASMREG_EBX)
          |(1 << DCC_ASMREG_ESI)
          |(1 << DCC_ASMREG_EDI)
#endif
          );
 if (mask) for (; iter != end; ++iter) {
  if (((iter->sv_reg&DCC_RC_I)  && (mask&(1 << (iter->sv_reg&DCC_RI_MASK)))) ||
      ((iter->sv_reg2&DCC_RC_I) && (mask&(1 << (iter->sv_reg2&DCC_RI_MASK))))) {
   /* Kill this stack value. */
   DCCStackValue_Kill(iter);
   assert(iter->sv_flags&DCC_SFLAG_LVALUE);
  }
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_KillTst(void) {
 struct DCCStackValue *iter,*end;
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) return;
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if (iter->sv_flags&DCC_SFLAG_TEST) {
   /* Kill this test. */
   DCCStackValue_FixTest(iter);
   assert(!(iter->sv_flags&DCC_SFLAG_TEST));
  }
 }
}

PUBLIC int DCC_VSTACK_CALL
DCCVStack_HasTst(void) {
 struct DCCStackValue *iter,*end;
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if (iter->sv_flags&DCC_SFLAG_TEST) return 1;
 }
 return 0;
}




























//////////////////////////////////////////////////////////////////////////
// 
//  PUSH HELPERS
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushInt(tyid_t type, int_t v) {
 struct DCCStackValue slot;
 slot.sv_ctype.t_type = type;
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_RVALUE;
 slot.sv_reg          = DCC_RC_CONST;
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_const.it     = v;
 slot.sv_sym          = NULL; /* If set, this would could be used for a relocation added to 'v'. */
 vpush(&slot);
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushFlt(tyid_t type, float_t v) {
 struct DCCStackValue slot;
 slot.sv_ctype.t_type = type;
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_RVALUE;
 slot.sv_reg          = DCC_RC_CONST;
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_const.flt    = v;
 slot.sv_sym          = NULL; /* If set, this would could be used for a relocation added to 'v'. */
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushCst(struct DCCType const *__restrict type, int_t v) {
 struct DCCStackValue slot;
 slot.sv_ctype    = *type;
 slot.sv_flags    = DCC_SFLAG_RVALUE;
 slot.sv_reg      = DCC_RC_CONST;
 slot.sv_reg2     = DCC_RC_CONST;
 slot.sv_const.it = v;
 slot.sv_sym      = NULL; /* If set, this would could be used for a relocation added to 'v'. */
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushSym(struct DCCSym *__restrict sym) {
 struct DCCStackValue slot;
 assert(sym);
 slot.sv_reg          = DCC_RC_CONST;
 slot.sv_const.it     = 0;
 slot.sv_sym          = sym;
 slot.sv_flags        = DCC_SFLAG_LVALUE;
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_ctype.t_base = NULL;
 slot.sv_ctype.t_type = DCCTYPE_VOID;
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushMloc(struct DCCType const *__restrict type,
                   struct DCCMemLoc *__restrict loc) {
 struct DCCStackValue slot;
 assert(type);
 assert(loc);
 slot.sv_reg      = loc->ml_reg;
 slot.sv_const.it = (int_t)loc->ml_off;
 slot.sv_sym      = loc->ml_sym;
 slot.sv_flags    = DCC_SFLAG_LVALUE;
 slot.sv_reg2     = DCC_RC_CONST;
 slot.sv_ctype    = *type;
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushSymt(struct DCCType const *__restrict type,
                   struct DCCSym *__restrict sym) {
 struct DCCStackValue slot;
 assert(type);
 assert(sym);
 slot.sv_reg      = DCC_RC_CONST;
 slot.sv_const.it = 0;
 slot.sv_sym      = sym;
 slot.sv_flags    = DCC_SFLAG_LVALUE;
 slot.sv_reg2     = DCC_RC_CONST;
 slot.sv_ctype    = *type;
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushDecl(struct DCCDecl *__restrict decl) {
 struct DCCStackValue slot;
 assert(decl);
 if (!(decl->d_kind&DCC_DECLKIND_MLOC)) {
  vpushi(DCCTYPE_VOID,0);
  return;
 }
 if (decl->d_mdecl.md_loc.ml_reg != DCC_RC_CONST) {
  DCCStackValue_SetMemDecl(&slot,&decl->d_mdecl);
 } else {
  struct DCCSym *sym = decl->d_mdecl.md_loc.ml_sym;
  slot.sv_reg      = decl->d_mdecl.md_loc.ml_reg;
  slot.sv_const.it = decl->d_mdecl.md_loc.ml_off;
  if (sym && sym->sy_sec == &DCCSection_Abs &&
    !(sym->sy_flags&DCC_SYMFLAG_WEAK)) {
   /* Inline an absolute symbol value. */
   slot.sv_const.it += sym->sy_addr;
   sym               = NULL; /* Set NULL for 'sv_sym' below. */
  }
  slot.sv_sym = sym;
 }
 slot.sv_flags = (decl->d_kind&(DCC_DECLKIND_MREF&~(DCC_DECLKIND_MLOC))) ? 0 : DCC_SFLAG_LVALUE;
 slot.sv_reg2  = DCC_RC_CONST;
 slot.sv_ctype = decl->d_type;
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushSizeof(struct DCCType const *__restrict t) {
 struct DCCStackValue slot;
 assert(t);
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_ctype.t_type = DCCTYPE_SIZE|DCCTYPE_UNSIGNED;
 slot.sv_ctype.t_base = NULL;
 slot.sv_sym          = NULL;
 if (DCCType_ISVLA(t)) {
  struct DCCMemDecl offset;
  /* Special case: VLA offset. */
  slot.sv_flags        = DCC_SFLAG_LVALUE|DCC_SFLAG_RVALUE;
  offset.md_loc.ml_reg = DCC_RR_XBP;
  offset.md_loc.ml_off = t->t_base->d_tdecl.td_vlaoff;
  offset.md_loc.ml_sym = NULL;
  offset.md_scope      = t->t_base->d_tdecl.td_vlascope;
  DCCStackValue_SetMemDecl(&slot,&offset);
 } else {
  slot.sv_reg      = DCC_RC_CONST;
  slot.sv_flags    = DCC_SFLAG_RVALUE;
  slot.sv_const.it = (int_t)DCCType_Sizeof(t,NULL,1);
 }
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushReturn(struct DCCDecl const *funty_decl) {
 struct DCCStackValue slot;
 DCCStackValue_GetReturn(&slot,funty_decl);
 vpush(&slot);
}


PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushStr(char const *__restrict p, size_t s) {
 struct DCCStackValue slot;
 struct DCCSym *str_sym;
 /* Place a data symbol for the string. */
 str_sym = DCCSection_DAllocSym(unit.u_string,p,s*sizeof(char),
                               (s+1)*sizeof(char),1,0);
 if unlikely(!str_sym) { vpushi(DCCTYPE_INT,0); return; }
 slot.sv_ctype.t_base = NULL;
 /* TODO: 'DCCTYPE_CONST' should somehow be configurable. */
 slot.sv_ctype.t_type = DCCTYPE_BUILTIN|DCCTYPE_CHAR|DCCTYPE_CONST;
 if (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED)
     slot.sv_ctype.t_type |= DCCTYPE_UNSIGNED;
 /* Use a character array as typing for the string. */
 DCCType_MkArray(&slot.sv_ctype,(s+1));
 assert(slot.sv_ctype.t_base);
 slot.sv_flags    = DCC_SFLAG_LVALUE|DCC_SFLAG_RVALUE;
 slot.sv_reg      = DCC_RC_CONST;
 slot.sv_reg2     = DCC_RC_CONST;
 slot.sv_const.it = 0;
 slot.sv_sym      = str_sym;
 vpush(&slot);
 assert(slot.sv_ctype.t_base);
 DCCDecl_Decref(slot.sv_ctype.t_base);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushReg(rc_t reg) {
 struct DCCStackValue slot;
 slot.sv_ctype.t_type = DCC_RC_GETTYPE(reg);
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_NONE;
 slot.sv_reg          = reg;
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_const.it     = 0;
 slot.sv_sym          = NULL;
 vpush(&slot);
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushRegs(rc_t reg, rc_t reg2) {
 struct DCCStackValue slot;
 if (reg2 != DCC_RC_CONST) {
  slot.sv_ctype.t_type = DCCTYPE_INT64;
 } else {
  slot.sv_ctype.t_type = DCC_RC_GETTYPE(reg);
 }
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_NONE;
 slot.sv_reg          = reg;
 slot.sv_reg2         = reg2;
 slot.sv_const.it     = 0;
 slot.sv_sym          = NULL;
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushXReg(rc_t reg) {
 struct DCCStackValue slot;
 slot.sv_ctype.t_type = DCC_RC_GETTYPE(reg);
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_XREGISTER;
 slot.sv_reg          = reg;
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_const.it     = 0;
 slot.sv_sym          = NULL;
 vpush(&slot);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushAddr(struct DCCSection *__restrict sec,
                   target_ptr_t addr) {
 struct DCCStackValue slot;
 assert(sec);
 slot.sv_ctype.t_type = DCCTYPE_VOID;
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_LVALUE;
 slot.sv_reg          = DCC_RC_CONST;
 slot.sv_reg2         = DCC_RC_CONST;
 slot.sv_const.it     = (int_t)addr;
 slot.sv_sym          = &sec->sc_start;
 vpush(&slot);
}








//////////////////////////////////////////////////////////////////////////
// 
//  PUSH
// 
#ifdef HAVE_VLOG
PRIVATE void vlog_regnam(rc_t r) {
 dcc_outf("%");
#ifdef DCC_RC_I64
      if (r&DCC_RC_I64) dcc_outf("r");
 else
#endif
      if (r&DCC_RC_I32) dcc_outf("e");
 else if (r&DCC_RC_I16);
 else if (r&DCC_RC_I8) {
  switch (r&7) {
   case DCC_ASMREG_AL: dcc_outf("al"); break;
   case DCC_ASMREG_CL: dcc_outf("cl"); break;
   case DCC_ASMREG_DL: dcc_outf("dl"); break;
   case DCC_ASMREG_BL: dcc_outf("bl"); break;
   case DCC_ASMREG_AH: dcc_outf("ah"); break;
   case DCC_ASMREG_CH: dcc_outf("ch"); break;
   case DCC_ASMREG_DH: dcc_outf("dh"); break;
   case DCC_ASMREG_BH: dcc_outf("bh"); break;
  }
  return;
 }
 switch (r&7) {
  case DCC_ASMREG_AX: dcc_outf("ax"); break;
  case DCC_ASMREG_CX: dcc_outf("cx"); break;
  case DCC_ASMREG_DX: dcc_outf("dx"); break;
  case DCC_ASMREG_BX: dcc_outf("bx"); break;
  case DCC_ASMREG_SP: dcc_outf("sp"); break;
  case DCC_ASMREG_BP: dcc_outf("bp"); break;
  case DCC_ASMREG_SI: dcc_outf("si"); break;
  case DCC_ASMREG_DI: dcc_outf("di"); break;
 }
}
#endif
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Push(struct DCCStackValue const *__restrict sval) {
 struct DCCStackValue *dst_slot;
 assert(sval);
 /* Make sure that the given stack value is in a consistent state. */
 assertf((sval->sv_reg&DCC_RC_I16) ? ((sval->sv_reg&DCC_RC_I8) || (sval->sv_reg&4)) : 1,
         "16-bit registers AX, CX, DX and BX must imply 8-bit");
 assertf((sval->sv_reg2&DCC_RC_I16) ? ((sval->sv_reg2&DCC_RC_I8) || (sval->sv_reg2&4)) : 1,
         "16-bit registers AX, CX, DX and BX must imply 8-bit");

 assert((sval->sv_reg&DCC_RC_I32) ? (sval->sv_reg&DCC_RC_I16) : 1);
 assert((sval->sv_reg2&DCC_RC_I32) ? (sval->sv_reg2&DCC_RC_I16) : 1);
#ifdef DCC_RC_I64
 assert((sval->sv_reg&DCC_RC_I64) ? (sval->sv_reg&DCC_RC_I32) : 1);
 assert((sval->sv_reg2&DCC_RC_I64) ? (sval->sv_reg2&DCC_RC_I32) : 1);
#endif
 assert((sval->sv_flags&(DCC_SFLAG_TEST|DCC_SFLAG_LVALUE)) != (DCC_SFLAG_TEST|DCC_SFLAG_LVALUE));
 assert((sval->sv_flags&(DCC_SFLAG_BITFLD|DCC_SFLAG_LVALUE)) != (DCC_SFLAG_BITFLD|DCC_SFLAG_LVALUE));
 DCCSym_XASSERT(sval->sv_sym);
 DCCType_ASSERT(&sval->sv_ctype);
#ifdef HAVE_VLOG
 if DCC_MACRO_COND(HAVE_VLOG) {
  struct TPPString *tynam;
  if (sval->sv_flags&(DCC_SFLAG_XOFFSET|DCC_SFLAG_XREGISTER|
                      DCC_SFLAG_TEST|DCC_SFLAG_BITFLD))
      goto default_vlog;
  if (DCCTYPE_ISBASIC(sval->sv_ctype.t_type,DCCTYPE_VOID)) {
   if (!(sval->sv_flags&DCC_SFLAG_LVALUE)) { VLOG(1,("vpushv()\n")); goto done_vlog; }
   if (sval->sv_sym) { VLOG(1,("vpushs(*(")); goto vlog_sym; }
  }
  if (sval->sv_flags == DCC_SFLAG_RVALUE && !sval->sv_sym) {
   VLOG(1,("vpush%c(",sval->sv_ctype.t_base ? 'c' : 'i'));
   if (sval->sv_ctype.t_type != DCCTYPE_INT) {
    tynam = DCCType_ToTPPString(&sval->sv_ctype,NULL);
    dcc_outf("[%s],",tynam->s_text);
    TPPString_Decref(tynam);
   }
   goto vlog_cst;
  }
  if (sval->sv_reg == DCC_RC_CONST && sval->sv_sym) {
   if (sval->sv_flags&DCC_SFLAG_LVALUE) {
    if (DCCTYPE_ISARRAY(sval->sv_ctype.t_type)) {
     assert(sval->sv_ctype.t_base);
     if (sval->sv_ctype.t_base->d_kind == DCC_DECLKIND_ARRAY &&
        (DCCTYPE_ISBASIC(sval->sv_ctype.t_base->d_type.t_type,DCCTYPE_CHAR) ||
         DCCTYPE_ISBASIC(sval->sv_ctype.t_base->d_type.t_type,DCCTYPE_CHAR|DCCTYPE_UNSIGNED))) {
      struct DCCMemLoc ml; char *data;
      size_t size = sval->sv_ctype.t_base->d_tdecl.td_size*DCC_TARGET_SIZEOF_CHAR;
      ml.ml_off = (target_off_t)sval->sv_const.it;
      ml.ml_reg = DCC_RC_CONST;
      ml.ml_sym = sval->sv_sym;
      data = (char *)DCCMemLoc_CompilerData(&ml,size);
      if (data) {
       char *buf; size_t bufsize;
       bufsize = TPP_SizeofEscape(data,size);
       buf = (char *)malloc((bufsize+1)*sizeof(char));
       if (buf) {
        *TPP_Escape(buf,data,size) = '\0';
        VLOG(1,("vpushstr(\"%s\")\n",buf));
        free(buf);
        goto done_vlog;
       }
      }
     }
    }
   } else {
    VLOG(1,("vpushst("));
    tynam = DCCType_ToTPPString(&sval->sv_ctype,NULL);
    dcc_outf("[%s],",tynam->s_text);
    TPPString_Decref(tynam);
    goto vlog_sym;
   }
  }
  if (sval->sv_reg != DCC_RC_CONST &&
     !sval->sv_ctype.t_base &&
      sval->sv_ctype.t_type == DCC_RC_GETTYPE(sval->sv_reg)) {
   VLOG(1,("vpushr("));
   goto vlog_reg;
  }
default_vlog:
  VLOG(1,("vpush("));
  tynam = DCCType_ToTPPString(&sval->sv_ctype,NULL);
  dcc_outf("[%s],",tynam->s_text);
  TPPString_Decref(tynam);
  if (sval->sv_flags&DCC_SFLAG_COPY) dcc_outf("copy");
  if (sval->sv_flags&DCC_SFLAG_RVALUE) dcc_outf("%srvalue",(sval->sv_flags&DCC_SFLAG_COPY)?"|":"");
  if (sval->sv_flags&DCC_SFLAG_XOFFSET) dcc_outf("%sxoffset",(sval->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE))?"|":"");
  if (sval->sv_flags&DCC_SFLAG_XREGISTER) dcc_outf("%sxregister",(sval->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE|DCC_SFLAG_XOFFSET))?"|":"");
  if (sval->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE|DCC_SFLAG_XOFFSET|DCC_SFLAG_XREGISTER)) dcc_outf(",");
  if (sval->sv_flags&DCC_SFLAG_TEST) {
   dcc_outf("test(");
   switch ((sval->sv_flags&DCC_SFLAG_TEST_MASK) >> DCC_SFLAG_TEST_SHIFT) {
    case DCC_TEST_O : dcc_outf("OF=1"); break;       /* test: overflow (OF=1). */
    case DCC_TEST_NO: dcc_outf("OF=0"); break;       /* test: not overflow (OF=0). */
    case DCC_TEST_B : dcc_outf("CF=1"); break;       /* test: below (CF=1). */
    case DCC_TEST_AE: dcc_outf("CF=0"); break;       /* test: above or equal (CF=0). */
    case DCC_TEST_E : dcc_outf("=="); break;         /* test: equal (ZF=1). */
    case DCC_TEST_NE: dcc_outf("!="); break;         /* test: not equal (ZF=0). */
    case DCC_TEST_BE: dcc_outf("CF=1||ZF=1"); break; /* test: below or equal (CF=1 or ZF=1). */
    case DCC_TEST_A : dcc_outf("CF=0&&ZF=0"); break; /* test: above (CF=0 and ZF=0). */
    case DCC_TEST_S : dcc_outf("SF=1"); break;       /* test: sign (SF=1). */
    case DCC_TEST_NS: dcc_outf("SF=0"); break;       /* test: not sign (SF=0). */
    case DCC_TEST_PE: dcc_outf("PF=1"); break;       /* test: parity even (PF=1). */
    case DCC_TEST_PO: dcc_outf("PF=0"); break;       /* test: parity odd (PF=0). */
    case DCC_TEST_L : dcc_outf("<"); break;          /* test: less (SF<>OF). */
    case DCC_TEST_GE: dcc_outf(">="); break;         /* test: greater or equal (SF=OF). */
    case DCC_TEST_LE: dcc_outf("<="); break;         /* test: less or equal (ZF=1 or SF<>OF). */
    case DCC_TEST_G : dcc_outf(">"); break;          /* test: greater (ZF=0 and SF=OF). */
    default: break;
   }
   dcc_outf(")");
  }
  if (sval->sv_flags&DCC_SFLAG_BITFLD) {
   dcc_outf("bitfld(%d,%d),",
           (int)DCC_SFLAG_GTBITOFF(sval->sv_flags),
           (int)DCC_SFLAG_GTBITSIZ(sval->sv_flags));
  }
  if (sval->sv_flags&DCC_SFLAG_LVALUE) dcc_outf("*(");
  if (sval->sv_reg != DCC_RC_CONST) {
vlog_reg:
   dcc_outf("%%");
   vlog_regnam(sval->sv_reg);
   if (sval->sv_reg2 != DCC_RC_CONST) {
    dcc_outf(":%%");
    vlog_regnam(sval->sv_reg2);
   }
   if (sval->sv_const.it ||
       sval->sv_sym) dcc_outf("+");
  }
  if (sval->sv_sym) {
vlog_sym:
   if (sval->sv_sym->sy_name != &TPPKeyword_Empty) {
    dcc_outf("%s",sval->sv_sym->sy_name->k_name);
   } else if (sval->sv_sym->sy_alias) {
    char const *alias_name = sval->sv_sym->sy_alias->sy_name->k_name;
    if (!*alias_name) alias_name = "<UNNAMED>";
    if (sval->sv_sym->sy_addr)
         dcc_outf("ALIAS(%s+%#lx)",alias_name,(unsigned long)sval->sv_sym->sy_addr);
    else dcc_outf("ALIAS(%s)",alias_name);
   } else if (sval->sv_sym->sy_sec) {
    if (sval->sv_sym->sy_addr)
         dcc_outf("%s+%#lx",sval->sv_sym->sy_sec->sc_start.sy_name->k_name,
                           (unsigned long)sval->sv_sym->sy_addr);
    else dcc_outf("%s",     sval->sv_sym->sy_sec->sc_start.sy_name->k_name);
   } else {
    dcc_outf("UNDEFINED");
   }
   if (sval->sv_const.it) dcc_outf("+");
  }
  if (sval->sv_const.it ||
     (!sval->sv_sym && sval->sv_reg == DCC_RC_CONST)) {
vlog_cst:
   if (sval->sv_const.it&15) {
#if TPP_HAVE_LONGLONG
    dcc_outf("%#lld",(long long)sval->sv_const.it);
#else
    dcc_outf("%#ld",(long)sval->sv_const.it);
#endif
   } else {
#if TPP_HAVE_LONGLONG
    dcc_outf("%#llx",(unsigned long long)sval->sv_const.it);
#else
    dcc_outf("%#lx",(unsigned long)sval->sv_const.it);
#endif
   }
  }
  if (sval->sv_flags&DCC_SFLAG_LVALUE) dcc_outf(")");
  dcc_outf(")\n");
 }
done_vlog:
#endif

 dst_slot = vstack.v_bottom;
 if (dst_slot-- == vstack.v_begin) {
  size_t new_size,new_slots;
  /* Must allocate more stack slots below. */
  new_size = (size_t)(vstack.v_end-vstack.v_begin);
  new_slots = new_size;
  new_size = new_size ? new_size*2 : (new_slots = 16);
  dst_slot = (struct DCCStackValue *)realloc(vstack.v_begin,new_size*
                                             sizeof(struct DCCStackValue));
  if unlikely(!dst_slot) goto seterr;
  /* Shift existing entries upwards. */
  memmove(dst_slot+new_slots,dst_slot,
         (new_size-new_slots)*sizeof(struct DCCStackValue));
  vstack.v_begin = dst_slot;
  vstack.v_end   = dst_slot+new_size;
  dst_slot += (new_slots-1);
 }
 assert(dst_slot >= vstack.v_begin);
 assert(dst_slot <  vstack.v_end);
 memcpy(dst_slot,sval,sizeof(struct DCCStackValue));
 if (dst_slot->sv_ctype.t_base) DCCDecl_Incref(dst_slot->sv_ctype.t_base);
 if (dst_slot->sv_sym) {
  if (DCCSym_SECTION(dst_slot->sv_sym) == &DCCSection_Abs &&
    !(dst_slot->sv_sym->sy_flags&DCC_SYMFLAG_WEAK)) {
   /* Special case: The symbol is part of the global section.
    *               To simplify optimization code, we convert it to a constant value here. 
    * NOTE: We have to make sure not to do so for weak symbols, as those may be overwritten again! */
   dst_slot->sv_const.it += dst_slot->sv_sym->sy_addr;
   dst_slot->sv_sym       = NULL;
  } else {
   DCCSym_Incref(dst_slot->sv_sym);
  }
 }
 vstack.v_bottom = dst_slot;
 return;
seterr:
 TPPLexer_SetErr();
}








//////////////////////////////////////////////////////////////////////////
// 
//  DUP
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_ReplaceCopy(void) {
 assert(vsize >= 1);
 VLOG(0,("vrcopy()\n"));
 /* Nothing else really needs to take place for this to happen... */
 vbottom->sv_flags |= (DCC_SFLAG_COPY|DCC_SFLAG_RVALUE);
}
PUBLIC void DCC_VSTACK_CALL DCCVStack_Dup(int copy) {
 struct DCCStackValue *dst_slot,*src_slot;
 assert(vsize >= 1);
 VLOG(1,("vdup(%s)\n",copy ? "copy" : ""));
 dst_slot = src_slot = vstack.v_bottom;
 if (dst_slot-- == vstack.v_begin) {
  size_t new_size,new_slots;
  /* Must allocate more stack slots below. */
  new_size = (size_t)(vstack.v_end-vstack.v_begin);
  new_slots = new_size;
  new_size = new_size ? new_size*2 : (new_slots = 16);
  src_slot = (struct DCCStackValue *)realloc(vstack.v_begin,new_size*
                                              sizeof(struct DCCStackValue));
  if unlikely(!src_slot) goto seterr;
  /* Shift existing entries upwards. */
  memmove(src_slot+new_slots,src_slot,
         (new_size-new_slots)*sizeof(struct DCCStackValue));
  vstack.v_begin = src_slot;
  vstack.v_end   = src_slot+new_size;
  src_slot += new_slots;
  dst_slot  = src_slot-1;
 }
 assert(dst_slot >= vstack.v_begin);
 assert(src_slot <  vstack.v_end);
 assert(src_slot == dst_slot+1);
 memcpy(dst_slot,src_slot,sizeof(struct DCCStackValue));
 if (dst_slot->sv_ctype.t_base) DCCDecl_Incref(dst_slot->sv_ctype.t_base);
 if (dst_slot->sv_sym) DCCSym_Incref(dst_slot->sv_sym);
 if (copy) dst_slot->sv_flags |= DCC_SFLAG_COPY;
 vstack.v_bottom = dst_slot;
 return;
seterr:
 TPPLexer_SetErr();
}








//////////////////////////////////////////////////////////////////////////
// 
//  POP
// 
PUBLIC void DCC_VSTACK_CALL DCCVStack_Pop(int del) {
 VLOG(-1,("vpop(%s)\n",del ? "delete" : ""));
 assert(vsize != 0);
 if (!del) goto done;
 /* Warn about an unused value.
  * NOTE: Don't warn about unused values when no code should be generated.
  *    >> Within those regions, we don't care, as someone
  *       probably just want's to know the type or size. */
 if ((vbottom->sv_flags&DCC_SFLAG_DO_WUNUSED) &&
    !(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN))
      WARN(W_UNUSED_VALUE,&vbottom->sv_ctype);
 if (/* Only perform destruction if we're supposed to. */
    (vbottom->sv_flags&(DCC_SFLAG_XOFFSET|DCC_SFLAG_LVALUE|DCC_SFLAG_COPY)) == DCC_SFLAG_XOFFSET &&
    (vbottom->sv_reg != DCC_RC_CONST) && (vbottom->sv_const.it != 0)) {
  struct DCCSymAddr rhs_val;
  rhs_val.sa_sym = vbottom->sv_sym;
  rhs_val.sa_off = (target_off_t)vbottom->sv_const.offset;
  /* Need to add the offset, as it was explicitly stated!
   * This can happen if the user wrote something like '%eax += 42;' */
  if (vbottom->sv_reg2 != DCC_RC_CONST) {
   /* Always to use 'add', as 'inc' don't set the carry flag. */
   DCCDisp_CstBinReg('+',&rhs_val,vbottom->sv_reg,0);
   /* Add more data to the second operand (NOTE: with carry). */
   rhs_val.sa_off = (target_off_t)(vbottom->sv_const.it >> 32);
   DCCDisp_CstBinReg(TOK_INC,&rhs_val,vbottom->sv_reg,0);
  } else {
   /* Use optimized add. */
   DCCDisp_AddReg(&rhs_val,vbottom->sv_reg);
  }
 }
done:
 DCCType_Quit(&vbottom->sv_ctype);
 DCCSym_XDecref(vbottom->sv_sym);
 ++vbottom;
}








//////////////////////////////////////////////////////////////////////////
// 
//  ROTATE
// 
PUBLIC void DCC_VSTACK_CALL DCCVStack_Swap(void) {
 struct DCCStackValue temp;
 assert(vsize >= 2);
 VLOG(0,("vswap()\n"));
 temp = vbottom[0];
 vbottom[0] = vbottom[1];
 vbottom[1] = temp;
}

PUBLIC void DCC_VSTACK_CALL DCCVStack_LRot(size_t n) {
 struct DCCStackValue temp;
 VLOG(0,("vlrot(%lu)\n",(unsigned long)n));
 assert(n),--n;
 assert(vsize >= n);
 memcpy(&temp,vbottom,sizeof(struct DCCStackValue));
 memmove(vbottom,vbottom+1,n*sizeof(struct DCCStackValue));
 memcpy(vbottom+n,&temp,sizeof(struct DCCStackValue));
}

PUBLIC void DCC_VSTACK_CALL DCCVStack_RRot(size_t n) {
 struct DCCStackValue temp;
 VLOG(0,("vrrot(%lu)\n",(unsigned long)n));
 assert(n),--n;
 assert(vsize >= n);
 memcpy(&temp,vbottom+n,sizeof(struct DCCStackValue));
 memmove(vbottom+1,vbottom,n*sizeof(struct DCCStackValue));
 memcpy(vbottom,&temp,sizeof(struct DCCStackValue));
}








//////////////////////////////////////////////////////////////////////////
// 
//  UNARY
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Unary(tok_t op) {
 struct DCCType *target_type;
 assert(vsize >= 1);
#ifdef HAVE_VLOG
 if DCC_MACRO_COND(HAVE_VLOG) {
  char buf[2] = {(char)op,'\0'};
  char const *name = buf;
  switch (op) {
  case TOK_INC: name = "++"; break;
  case TOK_DEC: name = "--"; break;
  default: break;
  }
  VLOG(0,("vgen1('%s')\n",name));
 }
#endif
 target_type = NULL;
 /* Update WUNUSED flag. */
 if (!(vbottom->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE)))
       vbottom->sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
 if (op != '!' && op != '*' && !(vbottom->sv_flags&DCC_SFLAG_COPY)) {
  target_type = DCCType_Effective(&vbottom->sv_ctype);
  if (op != '&' && (target_type->t_type&DCCTYPE_CONST))
      WARN(W_UNARY_CONSTANT_TYPE,&vbottom->sv_ctype);
  if ((vbottom->sv_flags&(DCC_SFLAG_RVALUE|DCC_SFLAG_COPY)) == DCC_SFLAG_RVALUE &&
      (DCCTYPE_GROUP(vbottom->sv_ctype.t_type) != DCCTYPE_LVALUE))
      WARN(W_UNARY_RVALUE_TYPE,&vbottom->sv_ctype);
 }
 switch (op) {
 case '-':
  if (!target_type) target_type = DCCType_Effective(&vbottom->sv_ctype);
  if (target_type->t_type&DCCTYPE_POINTER) {
   WARN(W_UNARY_NEG_ON_POINTER_TYPE,&vbottom->sv_ctype);
  } else if (DCCTYPE_ISUNSIGNED(target_type->t_type)) {
   WARN(W_UNARY_NEG_ON_UNSIGNED_TYPE,&vbottom->sv_ctype);
  }
  break;
 case TOK_INC:
 case TOK_DEC:
  if (!target_type) target_type = DCCType_Effective(&vbottom->sv_ctype);
  if (target_type->t_type&DCCTYPE_POINTER) {
   struct DCCStackValue multiplier;
   /* Pointer arithmetic: Add/Sub the size of the pointer base. */
   assert(target_type->t_base);
   assert(target_type->t_base->d_kind&DCC_DECLKIND_TYPE);
   multiplier.sv_const.it = (int_t)DCCType_Sizeof(&target_type->t_base->d_type,NULL,0);
   if (!multiplier.sv_const.it) {
    if (HAS(EXT_VOID_ARITHMETIC)) multiplier.sv_const.it = 1;
    else { WARN(W_POINTER_ARITHMETIC_VOID,&vbottom->sv_ctype.t_base->d_type); return; }
   }
   if (multiplier.sv_const.it == 1) goto gen_unary; /* Compile as a regular inc/dec. */
   multiplier.sv_ctype.t_type = DCCTYPE_SIZE|DCCTYPE_UNSIGNED;
   multiplier.sv_ctype.t_base = NULL;
   multiplier.sv_flags        = DCC_SFLAG_NONE;
   multiplier.sv_reg          = DCC_RC_CONST;
   multiplier.sv_reg2         = DCC_RC_CONST;
   multiplier.sv_sym          = NULL;
   /* Make sure the multiplier base is of proper (and sufficient) typing. */
   DCCStackValue_Binary(&multiplier,vbottom,op == TOK_INC ? '+' : '-');
   /* Make sure 'gen2' didn't swap the operands (which it shouldn't) */
   assert(!multiplier.sv_sym);
   assert(!multiplier.sv_ctype.t_base);
   goto update_flags;
  }
  break;
 default: break;
 }
gen_unary:
 DCCStackValue_Unary(vbottom,op);
update_flags:
 if (op == '!' || op == '&') {
  vbottom->sv_flags |=  (DCC_SFLAG_RVALUE);
 } else {
  vbottom->sv_flags &= ~(DCC_SFLAG_RVALUE);
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Bitfldf(sflag_t flags) {
 VLOG(0,("vbitfld(%d,%d)\n",
        (int)DCC_SFLAG_GTBITOFF(flags),
        (int)DCC_SFLAG_GTBITSIZ(flags)));
 assert(vsize >= 1);
 DCCStackValue_FixTest(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE))
  DCCStackValue_FixRegOffset(vbottom);
 DCCStackValue_FixBitfield(vbottom);
 vbottom->sv_flags |= flags;
 assert(!(vbottom->sv_flags&DCC_SFLAG_TEST));
 /* Quickly fix constant bitfields. */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
      (vbottom->sv_reg == DCC_RC_CONST)
      ) DCCStackValue_FixBitfield(vbottom);
}


//////////////////////////////////////////////////////////////////////////
// 
//  BINARY
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Binary(tok_t op) {
 int is_cmp_op;
 struct DCCType *target_type;
 assert(vsize >= 2);
#ifdef HAVE_VLOG
 if DCC_MACRO_COND(HAVE_VLOG) {
  char buf[2] = {(char)op,'\0'};
  char const *name = buf;
  switch (op) {
  case TOK_LOWER_EQUAL:   name = "<="; break;
  case TOK_EQUAL:         name = "=="; break;
  case TOK_NOT_EQUAL:     name = "!="; break;
  case TOK_GREATER_EQUAL: name = ">="; break;
  case TOK_SHL:           name = "<<"; break;
  case TOK_SHR:           name = ">>"; break;
  case TOK_RANGLE3:       name = ">>>"; break;
  default: break;
  }
  VLOG(-1,("vgen2('%s')\n",name));
 }
#endif
 /* Update the WUNUSED flags. */
 vbottom->sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
 if (!(vbottom[1].sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE)))
       vbottom[1].sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
 /* Promote array types. */
 DCCStackValue_Promote(vbottom+1);
 vprom();
 is_cmp_op = (op == '?' ||
              op == TOK_LOWER ||
              op == TOK_LOWER_EQUAL ||
              op == TOK_EQUAL ||
              op == TOK_NOT_EQUAL ||
              op == TOK_GREATER ||
              op == TOK_GREATER_EQUAL);
 target_type = DCCType_Effective(&vbottom[1].sv_ctype);
 if (!is_cmp_op && !(vbottom[1].sv_flags&DCC_SFLAG_COPY)) {
  if (target_type->t_type&DCCTYPE_CONST)
      WARN(W_BINARY_CONSTANT_TYPE,&vbottom->sv_ctype,&vbottom[1].sv_ctype);
  if ((vbottom[1].sv_flags&(DCC_SFLAG_RVALUE|DCC_SFLAG_COPY)) == DCC_SFLAG_RVALUE &&
      (DCCTYPE_GROUP(vbottom[1].sv_ctype.t_type) != DCCTYPE_LVALUE))
       WARN(W_BINARY_RVALUE_TYPE,&vbottom->sv_ctype,&vbottom[1].sv_ctype);
 }
 switch (op) {
 case '+':
 case '-':
  if (target_type->t_type&DCCTYPE_POINTER) {
   struct DCCType *source_type;
   struct DCCType *pointer_base;
   struct DCCStackValue multiplier;
   assert(target_type->t_base);
   assert(target_type->t_base->d_kind&DCC_DECLKIND_TYPE);
   pointer_base = &target_type->t_base->d_type;
   /* Pointer arithmetic: Multiply the rhs (vbottom[0]) operand
    *                     by the size of the pointer base. */
   multiplier.sv_ctype.t_type = DCCTYPE_SIZE|DCCTYPE_UNSIGNED;
   multiplier.sv_ctype.t_base = NULL;
   multiplier.sv_flags        = DCC_SFLAG_NONE;
   multiplier.sv_reg          = DCC_RC_CONST;
   multiplier.sv_reg2         = DCC_RC_CONST;
   multiplier.sv_sym          = NULL;
   /* Check the pointer base for being a complete type. */
   if ((multiplier.sv_const.it = (int_t)DCCType_Sizeof(pointer_base,NULL,0)) == 0) {
    if (DCCTYPE_GROUP(pointer_base->t_type) == DCCTYPE_FUNCTION ||
        DCCTYPE_ISBASIC(pointer_base->t_type,DCCTYPE_VOID)) {
     if (HAS(EXT_VOID_ARITHMETIC)) multiplier.sv_const.ptr = 1;
     else WARN(W_POINTER_ARITHMETIC_VOID,pointer_base);
    } else if (!DCCType_IsComplete(pointer_base)) {
     WARN(W_POINTER_ARITHMETIC_INCOMPLETE,pointer_base);
    }
   }
   source_type = DCCType_Effective(&vbottom[0].sv_ctype);
   
   if (op == '-' && (source_type->t_type&DCCTYPE_POINTER)) {
    static struct DCCType const ty_ptrdiff = {DCCTYPE_PTRDIFF,NULL};
    /* Pointer/pointer difference >> (a-b)/sizeof(*a). */
    int compatible = DCCType_IsCompatible(pointer_base,&source_type->t_base->d_type,1);
    if (!compatible) WARN(W_POINTER_ARITHMETIC_INCOMPATIBLE_DIFF,
                          &vbottom[1].sv_ctype,&vbottom[0].sv_ctype);
    if (!(vbottom[1].sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE)))
          vbottom[1].sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
    /* Calculate the difference between the operands. */
    DCCStackValue_Binary(vbottom,vbottom+1,'-');
    /* Divide the result by the multiplier size (size of the pointer base). */
    DCCStackValue_Binary(&multiplier,vbottom+1,'/');
    /* Cast the result to 'ptrdiff_t'. */
    DCCStackValue_Cast(vbottom+1,&ty_ptrdiff);
    goto end_pop;
   } else if (!DCCTYPE_ISINT(source_type->t_type)) {
    /* Check if vbottom is an integral type and warn if it isn't */
    WARN(W_POINTER_ARITHMETIC_EXPECTED_INTEGRAL,&vbottom[0].sv_ctype);
   }
   /* Mark 'vbottom' for copy-on-write, as we're about to modify it. */
   vbottom->sv_flags |= DCC_SFLAG_COPY;
   /* Make sure the multiplier base is of proper (and sufficient) typing. */
   { struct DCCType t = {DCCTYPE_INTPTR,NULL};
     DCCStackValue_Cast(vbottom,&t);
   }
   DCCStackValue_Binary(&multiplier,vbottom,'*');
   goto genbinary;
  }
  break;

 case '&':
 case '|':
 case '^':
  /* Warn about use on pointer types. */
  if (target_type->t_type&DCCTYPE_POINTER)
      WARN(W_BITWISE_OPERATOR_ON_POINTER_TYPE,&vbottom[0].sv_ctype);
  if (DCCType_Effective(&vbottom[1].sv_ctype)->t_type&DCCTYPE_POINTER)
      WARN(W_BITWISE_OPERATOR_WITH_POINTER_TYPE,&vbottom[0].sv_ctype);
  break;

 case TOK_SHL:
 case TOK_SHR:
  if (target_type->t_type&DCCTYPE_POINTER) {
   WARN(W_SHIFT_OPERATOR_ON_POINTER_TYPE,&vbottom[0].sv_ctype);
  } else if (!DCCTYPE_ISUNSIGNED(target_type->t_type) &&
             !DCCStackValue_IsUnsignedReg(&vbottom[1])) {
   if (op == TOK_SHR) op = TOK_RANGLE3; /* Use the signed opcode. */
  }
  if (DCCType_Effective(&vbottom[1].sv_ctype)->t_type&DCCTYPE_POINTER)
      WARN(W_SHIFT_OPERATOR_WITH_POINTER_TYPE,&vbottom[0].sv_ctype);
  break;
 default: break;
 }

 {
  struct DCCType const *lhs_type; int wid;
  lhs_type = DCCType_Effective(&vbottom[1].sv_ctype);
  /* Emit warnings about incompatibilities between 'vbottom[0]' and 'vbottom[1]'. */
  wid = DCCStackValue_AllowCast(vbottom,lhs_type,0);
  if (wid) {
   /* NOTE: This certain set of warnings is also ignored in shift/binary operations. */
   if ((is_cmp_op || op == TOK_SHL || op == TOK_SHR || op == TOK_RANGLE3 ||
                     op == '&' || op == '|' || op == '^') &&
       /* Ignore constant/volatile/sign/overflow warnings for compare operations. */
      (wid == W_CAST_CONST_POINTER || wid == W_CAST_CONST_LVALUE ||
       wid == W_CAST_RVALUE_TO_LVALUE || wid == W_CAST_VOLATILE_POINTER ||
       wid == W_CAST_INTEGRAL_OVERFLOW || wid == W_CAST_INTEGRAL_MAYOVERFLOW ||
       wid == W_CAST_INTEGRAL_SIGNLOSS || wid == W_CAST_INTEGRAL_MAYSIGNLOSS))
       goto genbinary;
   if ((op == '+' || op == '-') &&
       (wid == W_CAST_INTEGRAL_SIGNLOSS ||
        wid == W_CAST_INTEGRAL_MAYSIGNLOSS))
        goto genbinary;
   WARN(wid,&vbottom->sv_ctype,&vbottom[1].sv_ctype);
  }
 }
genbinary:
 DCCStackValue_Binary(vbottom,vbottom+1,op);
end_pop:
 vpop(1);
 if (is_cmp_op) {
  vbottom->sv_flags |=  (DCC_SFLAG_RVALUE);
 } else {
  vbottom->sv_flags &= ~(DCC_SFLAG_RVALUE);
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Store(int initial_store) {
 struct DCCStackValue *target;
 struct DCCType const *target_type;
 int wid;
 assert(vsize >= 2);
 VLOG(-1,("vstore(%s)\n",initial_store ? "initial" : ""));
 /* Update the WUNUSED flag. */
 vbottom->sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
 if (!initial_store ||
    (!DCCTYPE_ISARRAY(vbottom[1].sv_ctype.t_type) &&
      DCCTYPE_GROUP(vbottom[1].sv_ctype.t_type) != DCCTYPE_LVALUE) ||
     !DCCTYPE_ISARRAY(vbottom[0].sv_ctype.t_type)) {
  vprom();
 }
 target = &vbottom[1];
 /* Update the WUNUSED flag. */
 if (!(target->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_RVALUE)))
       target->sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
 /* Warn if the target is const. */
 target_type = &target->sv_ctype;
 while (DCCTYPE_GROUP(target_type->t_type) == DCCTYPE_LVALUE)
        assert(target_type->t_base),
        target_type = &target_type->t_base->d_type;
 /* NOTE: Allow assignment to a constant only during the initial store! */
 if (!initial_store &&
    (target_type->t_type&DCCTYPE_CONST))
     WARN(W_ASSIGN_CONSTANT_TYPE,&vbottom->sv_ctype,target_type);
 if ((target->sv_flags&DCC_SFLAG_RVALUE) &&
     (DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE))
     WARN(W_ASSIGN_RVALUE_TYPE,&vbottom->sv_ctype,target_type);

 /* Allow unqualified array assignment during an initial
  * store, as the contents are copied in that case. */
 switch (DCCTYPE_GROUP(target_type->t_type)) {
 case DCCTYPE_BUILTIN:
  if (DCCTYPE_ISBASIC(target_type->t_type,DCCTYPE_VOID)) {
   /* NOTE: We use a different warning for 'void -> void' assignment,
    *       which is part of the WG_EXTENSIONS warning group, meaning
    *       that void -> void is actually allowed as an extension! */
   WARN(DCCTYPE_ISBASIC(vbottom->sv_ctype.t_type,DCCTYPE_VOID)
        ? W_ASSIGN_VOID_VOID : W_ASSIGN_VOID,
        &vbottom->sv_ctype,target_type);
   goto donepop;
  }
  break;
 case DCCTYPE_ARRAY:
  assert(target_type->t_base);
  if (target_type->t_base->d_kind == DCC_DECLKIND_VLA)
      WARN(W_ASSIGN_VLA_TYPE,target_type);
  if (initial_store &&
     (DCCTYPE_GROUP(vbottom->sv_ctype.t_type) == DCCTYPE_ARRAY) &&
     (vbottom->sv_ctype.t_base->d_kind == DCC_DECLKIND_ARRAY) &&
     (target_type->t_base->d_kind == DCC_DECLKIND_ARRAY) &&
     (vbottom->sv_ctype.t_base->d_tdecl.td_size == target_type->t_base->d_tdecl.td_size) &&
      DCCType_IsCompatible(&vbottom->sv_ctype.t_base->d_type,
                           &target_type->t_base->d_type,1)) goto genstore;
  break;
 default: break;
 }
 /* Check if there is something wrong with implicitly casting the assignment. */
#if 1
 if (DCCTYPE_GROUP(vbottom->sv_ctype.t_type) == DCCTYPE_LVALUE)
     target_type = &target->sv_ctype;
 wid = DCCStackValue_AllowCast(vbottom,target_type,0);
 if (wid) WARN(wid,&vbottom->sv_ctype,target_type);
#else
 wid = DCCStackValue_AllowCast(vbottom,target_type,0);
 if (wid) WARN(wid,&vbottom->sv_ctype,target_type);
#endif
genstore:
 DCCStackValue_Store(vbottom,target,initial_store);
donepop:
 vpop(1);
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_StoreCC(int invert_test,
                  int initial_store) {
 assert(vsize >= 3);
 if (visconst_bool()) {
  /* Compile-time constant condition. */
  int should_assign = (int)vgtconst_bool() ^ invert_test;
  vpop(1);
  if (should_assign) vgen2('=');
  else               vpop(1);
 } else {
  /* TODO: Use cmovCC */
  struct DCCSym *skip_jmp;
  /* Make sure that the target can be modified at runtime. */
  if (!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE) &&
       (vbottom[2].sv_reg == DCC_RC_CONST))
        DCCStackValue_Load(&vbottom[2]);
  else  DCCStackValue_Cow(&vbottom[2]);
  vbottom[2].sv_flags &= ~(DCC_SFLAG_RVALUE);
  skip_jmp = DCCUnit_AllocSym();
  skip_jmp ? vpushs(skip_jmp) : vpushv();
  /* Skip the assignment when the test is true. */
  vgen1('&'),vjcc(!invert_test);
  /* Generate a regular, old store. */
  vstore(initial_store);
  if (skip_jmp) t_defsym(skip_jmp);
 }
}



static target_ptr_t const basic_type_weights[] = {
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_INT,   /* DCCTYPE_INT */
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_CHAR,  /* DCCTYPE_BYTE */
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_SHORT, /* DCCTYPE_SHORT */
 DCC_TARGET_BITPERBYTE*8,                       /* DCCTYPE_IB8 */
};

PRIVATE int DCC_VSTACK_CALL
DCCType_IsUnionCompatible(struct DCCType const *__restrict union_type,
                          struct DCCType const *__restrict target_type,
                          int explicit_cast) {
 assert(union_type);
 assert(target_type);
 if (DCCTYPE_GROUP(union_type->t_type) == DCCTYPE_STRUCTURE) {
  struct DCCDecl *struct_decl = union_type->t_base;
  assert(struct_decl);
  if (struct_decl->d_kind == DCC_DECLKIND_UNION &&
      struct_decl->d_attr &&
     (explicit_cast || /* Following GCC, always allow explicit union-casts. */
     (struct_decl->d_attr->a_specs&DCC_ATTRSPEC_TRANSUNION))) {
   struct DCCStructField *iter,*end;
   /* Check the members of a transparent union. */
   end = (iter = struct_decl->d_tdecl.td_fieldv)+
                 struct_decl->d_tdecl.td_size;
   for (; iter != end; ++iter) if (!iter->sf_off) {
    assert(iter->sf_decl);
    assert(iter->sf_decl->d_kind&DCC_DECLKIND_TYPE);
    /* Check if the union member is compatible. */
    if (DCCType_IsCompatible(&iter->sf_decl->d_type,
                             target_type,1)
        ) return 1;
   }
  }
 }
 return 0;
}


PUBLIC int DCC_VSTACK_CALL
DCCStackValue_AllowCast(struct DCCStackValue const *__restrict value,
                        struct DCCType const *__restrict type,
                        int explicit_cast) {
 struct DCCType const *vtyp;
 tyid_t tid,vid;
 int is_const;
 assert(value);
 assert(type);
 /* Most generic case: If the types are compatible,
  *                    we're allowed to cast as-is. */
 vtyp = DCCType_Effective(&value->sv_ctype);
 if (DCCType_IsCompatible(vtyp,type,1))
  return 0;
 is_const = value->sv_reg == DCC_RC_CONST &&
          !(value->sv_flags&DCC_SFLAG_LVALUE) &&
          !(value->sv_sym);
 tid = type->t_type&~(DCCTYPE_QUAL);
 vid = vtyp->t_type&~(DCCTYPE_QUAL);
 switch (DCCTYPE_GROUP(tid)) {
 case DCCTYPE_BUILTIN:
  if ((tid&DCCTYPE_BASICMASK) == DCCTYPE_VOID) {
   /* Anything can be (explicitly) cast to void. */
   return explicit_cast ? 0 : W_CAST_TO_VOID;
  }
  if (DCCTYPE_GROUP(vid) == DCCTYPE_BUILTIN) {
   target_ptr_t tweight;
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   tid &= DCCTYPE_BASICMASK;
   vid &= DCCTYPE_BASICMASK;
   /* Implicit casts between floating point types. */
   if (DCCTYPE_ISFLOATT(tid)) {
    if (DCCTYPE_ISFLOATT(vid)) {
     /* Cast between float-point values. */
     if (!is_const) {
      int wt = tid-DCCTYPE_FLOAT;
      int wb = vid-DCCTYPE_FLOAT;
      /* Warn about downwards cast of a float-point value. */
      if (wt < wb) return W_CAST_FLOAT_DOWNCAST;
     }
    }
    return 0;
   } else if (DCCTYPE_ISFLOATT(vid)) {
    /* Implicit cast of floating-point to integer. */
    return W_CAST_FLOAT_TO_INT;
   }
   if (tid == DCCTYPE_BOOL) tweight = 1;
   else tweight = basic_type_weights[tid&3];
   if (is_const) {
    /* Special case: We know the value at compile-time
     * and can better decide what it really needs. */
    int_t iv = value->sv_const.it;
    unsigned int is_signed = iv < 0;
    unsigned int req_bits = 0;
    if (is_signed) iv = -iv;
    while (iv) ++req_bits,*(uint_t *)&iv >>= 1;
    /*req_bits += is_signed;*/
    /* If more bits are required that available, emit a warning. */
    if (req_bits > tweight) return W_CAST_INTEGRAL_OVERFLOW;
    if ((tid&DCCTYPE_UNSIGNED) && is_signed &&
         /* NOTE: Don't emit this warning if the sign-bit is set intentionally. */
       !(value->sv_flags&DCC_SFLAG_NO_WSIGN))
         return W_CAST_INTEGRAL_SIGNLOSS;
    return 0;
   }
   if (vid < 8) {
    /* Warn about implicit down-casts. */
    if (tweight < basic_type_weights[vid&3]) return W_CAST_INTEGRAL_MAYOVERFLOW;
    /* Warn if the target type isn't signed, but the source type was. */
    if ((tid&DCCTYPE_UNSIGNED) && !(vid&DCCTYPE_UNSIGNED)) return W_CAST_INTEGRAL_MAYSIGNLOSS;
   }
   /* Allow the cast. */
   return 0;
  }
  if (vid&DCCTYPE_POINTER) {
   /* Pointer --> integral. */
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   if (DCCTYPE_ISFLOATT(tid)) return W_CAST_POINTER_TO_FLOAT;
   if (tid >= 8) return W_CAST_INCOMPATIBLE_TYPES;
   if (basic_type_weights[tid&3] != DCC_TARGET_SIZEOF_POINTER*DCC_TARGET_BITPERBYTE)
       return W_CAST_POINTER_TO_INT_SIZ;
   return W_CAST_POINTER_TO_INT;
  }
  if (DCCTYPE_GROUP(vid) == DCCTYPE_STRUCTURE &&
      value->sv_ctype.t_base->d_attr &&
     (value->sv_ctype.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)) {
   target_ptr_t tweight;
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   /* arith-structure --> integral. */
   if (tid == DCCTYPE_BOOL) tweight = 1;
   else tweight = basic_type_weights[tid&3];
   if (tweight < DCCType_Sizeof(&value->sv_ctype,NULL,1)*DCC_TARGET_BITPERBYTE) return W_CAST_INTEGRAL_MAYOVERFLOW;
   /* Warn if the target type isn't signed, but the source type was. */
   if ((tid&DCCTYPE_UNSIGNED) && !(vid&DCCTYPE_UNSIGNED)) return W_CAST_INTEGRAL_MAYSIGNLOSS;
   return 0;
  }
  break;

 { /* Pointer cast: Check bases. */
 case DCCTYPE_POINTER:
  if (DCCTYPE_GROUP(vid) == DCCTYPE_BUILTIN) {
   /* Warn about float-pointer casts. */
   if (DCCTYPE_ISFLOATT(vid)) return W_CAST_FLOAT_TO_POINTER;
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   /* Allow implicit cast from constant 0 to NULL. */
   if (is_const && !value->sv_const.it) return 0;
   vid &= DCCTYPE_BASICMASK;
   /* Check for non-integral basic type. */
   if (vid >= 8) return W_CAST_TO_POINTER;
   /* Check for int2ptr of different size. */
   if (basic_type_weights[vid&3] != DCC_TARGET_SIZEOF_POINTER*DCC_TARGET_BITPERBYTE)
       return W_CAST_INT_TO_POINTER_SIZ;
   return W_CAST_INT_TO_POINTER;
  } else if (vid&DCCTYPE_POINTER) {
   int is_void;
   assert(type->t_base);
   assert(vtyp->t_base);
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   type = &type->t_base->d_type;
   vtyp = &vtyp->t_base->d_type;
   /* Check the pointer base types for compatibility. */
   is_void = 0;
   if (!DCCType_IsCompatible(type,vtyp,1)) {
    if (DCCTYPE_ISBASIC(type->t_type,DCCTYPE_VOID)); /* Allow implicit cast to void-pointers. */
    else if (DCCTYPE_ISBASIC(vtyp->t_type,DCCTYPE_VOID)) is_void = 1; /* Warn about implicit casts from void-pointers. */
    else return W_CAST_INCOMPATIBLE_POINTERS;
   }
   /* Check the const/volatile qualifiers, and if they are lost. */
   if (!(type->t_type&DCCTYPE_CONST) &&
        (vtyp->t_type&DCCTYPE_CONST)) return W_CAST_CONST_POINTER;
   if (!(type->t_type&DCCTYPE_VOLATILE) &&
        (vtyp->t_type&DCCTYPE_VOLATILE)) return W_CAST_VOLATILE_POINTER;
   /* If the source pointer base type was void, emit a warning:
    * >> void *get_p();
    * >> char *p = get_p(); // Warning: void-pointer --> char-pointer
    */
   if (is_void) return W_CAST_INCOMPATIBLE_POINTERS_VOID;
   /* Allow the pointer cast. */
   return 0;
  }
  if (DCCTYPE_GROUP(vid) == DCCTYPE_STRUCTURE &&
      value->sv_ctype.t_base->d_attr &&
     (value->sv_ctype.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)) {
   /* arith-structure --> pointer. */
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   /* Check for int2ptr of different size. */
   if (DCCType_Sizeof(&value->sv_ctype,NULL,1) !=
       DCC_TARGET_SIZEOF_POINTER) return W_CAST_INT_TO_POINTER_SIZ;
   return W_CAST_INT_TO_POINTER;
  }
 } break;

 { /* Compare the bases of the given l-value. */
 case DCCTYPE_LVALUE:
  /* Always OK for explicit l-value casts.
   * NOTE: r-values are converted to l-values. */
  if (explicit_cast) return 0;
  if (!(value->sv_flags&DCC_SFLAG_LVALUE) &&
       (DCCTYPE_GROUP(value->sv_ctype.t_type) != DCCTYPE_LVALUE)
        ) return W_CAST_RVALUE_TO_LVALUE;
  assert(type->t_base);
  type = &type->t_base->d_type;
  if (!DCCType_IsCompatible(vtyp,type,1) &&
      !DCCType_IsUnionCompatible(vtyp,type,explicit_cast) &&
      !DCCType_IsUnionCompatible(type,vtyp,explicit_cast)
      ) return W_CAST_INCOMPATIBLE_LVALUE;
  return 0;
 } break;

 case DCCTYPE_FUNCTION:
  return W_CAST_TO_FUNCTION;
 case DCCTYPE_ARRAY:
  return W_CAST_TO_ARRAY;
 case DCCTYPE_VARRAY:
  return W_CAST_TO_VARRAY;

 case DCCTYPE_STRUCTURE:
  assert(type->t_base);
  if (DCCType_IsUnionCompatible(type,&value->sv_ctype,explicit_cast)) return 0;
  if (type->t_base->d_attr &&
     (type->t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)) {
   /* Cast to arithmetic structure type. */
   if (DCCTYPE_GROUP(vid) == DCCTYPE_BUILTIN) {
    target_ptr_t tweight;
    if (explicit_cast) return 0; /* Always OK for explicit casts. */
    vid &= DCCTYPE_BASICMASK;
    if (DCCTYPE_ISFLOATT(vid)) return W_CAST_FLOAT_TO_INT; /* Implicit cast of floating-point to integer. */
    tweight = DCCType_Sizeof(type,NULL,1)*DCC_TARGET_BITPERBYTE;
    if (is_const) {
     /* Special case: We know the value at compile-time
      * and can better decide what it really needs. */
     int_t iv = value->sv_const.it;
     unsigned int is_signed = iv < 0;
     unsigned int req_bits = 0;
     if (is_signed) iv = -iv;
     while (iv) ++req_bits,*(uint_t *)&iv >>= 1;
     /*req_bits += is_signed;*/
     /* If more bits are required that available, emit a warning. */
     if (req_bits > tweight) return W_CAST_INTEGRAL_OVERFLOW;
     if ((tid&DCCTYPE_UNSIGNED) && is_signed &&
          /* NOTE: Don't emit this warning if the sign-bit is set intentionally. */
        !(value->sv_flags&DCC_SFLAG_NO_WSIGN))
          return W_CAST_INTEGRAL_SIGNLOSS;
     return 0;
    }
    if (vid < 8) {
     /* Warn about implicit down-casts. */
     if (tweight < basic_type_weights[vid&3]) return W_CAST_INTEGRAL_MAYOVERFLOW;
     /* Warn if the target type isn't signed, but the source type was. */
     if ((tid&DCCTYPE_UNSIGNED) && !(vid&DCCTYPE_UNSIGNED)) return W_CAST_INTEGRAL_MAYSIGNLOSS;
    }
    /* Allow the cast. */
    return 0;
   }
   if (vid&DCCTYPE_POINTER) {
    /* Pointer --> integral. */
    if (explicit_cast) return 0; /* Always OK for explicit casts. */
    if (DCCType_Sizeof(type,NULL,1) != DCC_TARGET_SIZEOF_POINTER)
        return W_CAST_POINTER_TO_INT_SIZ;
    return W_CAST_POINTER_TO_INT;
   }
   if (DCCTYPE_GROUP(vid) == DCCTYPE_STRUCTURE &&
       value->sv_ctype.t_base->d_attr &&
      (value->sv_ctype.t_base->d_attr->a_specs&DCC_ATTRSPEC_ARITHMETIC)) {
    if (explicit_cast) return 0; /* Always OK for explicit casts. */
    /* arith-structure --> integral. */
    if (DCCType_Sizeof(type,NULL,1) <
        DCCType_Sizeof(&value->sv_ctype,NULL,1)) return W_CAST_INTEGRAL_MAYOVERFLOW;
    /* Warn if the target type isn't signed, but the source type was. */
    if ((tid&DCCTYPE_UNSIGNED) && !(vid&DCCTYPE_UNSIGNED)) return W_CAST_INTEGRAL_MAYSIGNLOSS;
    return 0;
   }
  }
  break;

 default: break;
 }

 /* Check members of a transparent union. */
 if (DCCType_IsUnionCompatible(&value->sv_ctype,type,explicit_cast)) return 0;

 /* fallback: don't allow the cast. */
 return W_CAST_INCOMPATIBLE_TYPES;
}

PUBLIC int DCC_VSTACK_CALL
DCCVStack_IsSame(int same_declaration) {
 assert(vsize >= 2);
 if (vbottom[0].sv_flags != vbottom[1].sv_flags ||
     vbottom[0].sv_reg != vbottom[1].sv_reg ||
     vbottom[0].sv_reg2 != vbottom[1].sv_reg2 ||
     vbottom[0].sv_const.it != vbottom[1].sv_const.it ||
    (same_declaration && !DCCType_IsCompatible(&vbottom[0].sv_ctype,&vbottom[1].sv_ctype,0))
     ) return 0;
 if (vbottom[0].sv_sym) {
  if (!vbottom[1].sv_sym) return 0;
  if (!DCCSym_Equal(vbottom[0].sv_sym,vbottom[1].sv_sym)) return 0;
 } else if (vbottom[1].sv_sym) {
  return 0;
 }
 return 1;
}




//////////////////////////////////////////////////////////////////////////
// 
//  PROMOTION
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_PromInt2(void) {
 assert(vsize >= 2);
#if HAVE_VLOG
 VLOG(0,("vpromi2()\n"));
#endif
 if (DCCTYPE_GROUP(vbottom[0].sv_ctype.t_type) == DCCTYPE_BUILTIN &&
     DCCTYPE_GROUP(vbottom[1].sv_ctype.t_type) == DCCTYPE_BUILTIN &&
     /* Only perform promotions between integral types. */
    !DCCTYPE_ISFLOAT_OR_VOID(vbottom[0].sv_ctype.t_type) &&
    !DCCTYPE_ISFLOAT_OR_VOID(vbottom[1].sv_ctype.t_type)) {
  tyid_t common_type;
  DCCStackValue_PromoteInt(&vbottom[0]);
  DCCStackValue_PromoteInt(&vbottom[1]);
#if 8 >= DCC_TARGET_SIZEOF_INT
  /* propagate long-long integeral width modifiers. */
  if ((vbottom[0].sv_ctype.t_type&(DCCTYPE_BASICMASK&~(DCCTYPE_UNSIGNED))) == DCCTYPE_IB8 ||
      (vbottom[1].sv_ctype.t_type&(DCCTYPE_BASICMASK&~(DCCTYPE_UNSIGNED))) == DCCTYPE_IB8)
       common_type = DCCTYPE_IB8;
  else
#endif
#if DCC_TARGET_SIZEOF_LONG >= DCC_TARGET_SIZEOF_INT
  /* propagate long integeral width modifiers. */
  if (vbottom[0].sv_ctype.t_type&DCCTYPE_ALTLONG ||
      vbottom[1].sv_ctype.t_type&DCCTYPE_ALTLONG)
      common_type = DCCTYPE_LONG|DCCTYPE_ALTLONG;
  else
#endif
  { common_type = DCCTYPE_INT; }
  /* Propagate the unsigned-flag. */
  if (vbottom[0].sv_ctype.t_type&DCCTYPE_UNSIGNED ||
      vbottom[1].sv_ctype.t_type&DCCTYPE_UNSIGNED)
      common_type |= DCCTYPE_UNSIGNED;
  /* Apply the common typing to with stack-values. */
  vbottom[0].sv_ctype.t_type &= ~(DCCTYPE_BASICMASK|DCCTYPE_ALTMASK);
  vbottom[1].sv_ctype.t_type &= ~(DCCTYPE_BASICMASK|DCCTYPE_ALTMASK);
  vbottom[0].sv_ctype.t_type |=   common_type;
  vbottom[1].sv_ctype.t_type |=   common_type;
 }
}







//////////////////////////////////////////////////////////////////////////
// 
//  CAST
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Cast(struct DCCType const *__restrict t,
               int explicit_case) {
 int wid;
 assert(vsize >= 1);
#if HAVE_VLOG
 if DCC_MACRO_COND(HAVE_VLOG)
 { struct TPPString *tyname;
   tyname = DCCType_ToTPPString(t,NULL);
   VLOG(0,("vcast(%s",explicit_case ? "X:'" : "'"));
   if (tyname) { dcc_outf("%s",tyname->s_text); TPPString_Decref(tyname); }
   dcc_outf("')\n");
 }
#endif

 /* Warn if there are problems with this cast. */
 wid = DCCStackValue_AllowCast(vbottom,t,explicit_case);
 if (wid) WARN(wid,&vbottom->sv_ctype,t);

 DCCStackValue_Cast(vbottom,t);
 if (DCCTYPE_GROUP(t->t_type) == DCCTYPE_LVALUE) {
  vbottom->sv_flags &= ~(DCC_SFLAG_RVALUE);
 } else {
  vbottom->sv_flags |=  (DCC_SFLAG_RVALUE);
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_CastTst(uint8_t test) {
 test_t old_test;
 assert(vsize >= 1);
 assert(test == DCC_TEST_Z || test == DCC_TEST_NZ);
 /* Make sure we're actually working with a test. */
 if (!(vbottom->sv_flags&DCC_SFLAG_TEST)) vgen1('!'),vgen1('!');
 if (!(vbottom->sv_flags&DCC_SFLAG_TEST)) return; /* Probably a constant expression. */
 old_test = DCC_SFLAG_GTTEST(vbottom->sv_flags);
 if (old_test != test) {
  /* Update the EFLAGS registers. */
  rc_t temp = DCCVStack_GetReg(DCC_RC_I8,1);
  if (test == DCC_TEST_Z) old_test ^= DCC_TEST_NBIT;
  DCCDisp_SccReg(old_test,temp);
  DCCDisp_RegBinReg('t',temp,temp,1);
  vbottom->sv_flags &= ~(DCC_SFLAG_TEST_MASK);
  vbottom->sv_flags |=  (test << DCC_SFLAG_TEST_SHIFT);
 }
}
PUBLIC uint8_t DCC_VSTACK_CALL
DCCVStack_UniTst(uint8_t test) {
 if (test == DCC_UNITST_FIRST) {
  vgen1('!'),vgen1('!');
  if (vbottom->sv_flags&DCC_SFLAG_TEST) {
   test = DCC_SFLAG_GTTEST(vbottom->sv_flags);
   if (test != DCC_TEST_Z && test != DCC_TEST_NZ) {
    /* Must force into a valid common test. */
    test = DCC_TEST_Z; /* Default to using ZERO tests (the inversion of non-ZERO, which are generally more common). */
    goto cast_test;
   }
  }
 } else {
cast_test:
  DCCVStack_CastTst(test);
 }
 return test;
}




/* Push a given stack value aligned by 'align' onto the hardware stack.
 * NOTE: The caller is responsible for casting 'self' to the proper type (if known) beforehand!
 * @return: The actual amount of bytes pushed. */
PRIVATE target_siz_t DCC_VSTACK_CALL
DCCStackValue_PushAligned(struct DCCStackValue *__restrict self,
                          target_siz_t align) {
 struct DCCMemLoc symaddr;
 target_siz_t result,filler;
 assert(self);
 DCCStackValue_LoadLValue(self);
 DCCStackValue_FixBitfield(self);
 result = DCCType_Sizeof(&self->sv_ctype,NULL,1);
 if unlikely(!result) return 0; /* nothing to do here? */
 filler = (align-result)&(align-1);
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  symaddr.ml_reg = self->sv_reg;
  symaddr.ml_off = self->sv_const.offset;
  symaddr.ml_sym = self->sv_sym;
#if DCC_TARGET_STACKDOWN
  DCCDisp_NdfPush(filler);
  DCCDisp_MemRevPush(&symaddr,result);
#else
  DCCDisp_MemPush(&symaddr,result);
  DCCDisp_NdfPush(filler);
#endif
 } else if (self->sv_reg == DCC_RC_CONST) {
  int          sign_byte = 0;
  target_siz_t sign_size = 0;
  target_siz_t curr_size;
  int_t iv = self->sv_const.it;
  symaddr.ml_sad.sa_off = self->sv_const.offset;
  symaddr.ml_sad.sa_sym = self->sv_sym;
  if (result < align) {
   /* Fix integral constant size to reduce assembly overhead. */
   result = align < DCC_TARGET_SIZEOF_POINTER
          ? align : DCC_TARGET_SIZEOF_POINTER;
   filler = (align-result)&(align-1);
  }
  curr_size = result;
#if DCC_TARGET_STACKDOWN
  DCCDisp_NdfPush(filler);
#endif
  if (curr_size > 8) {
   sign_size = (curr_size-8);
   curr_size = 8;
   if (!DCCTYPE_ISUNSIGNED_OR_PTR(self->sv_ctype.t_type) &&
        iv < 0) sign_byte = 0xff;
  }
#if (DCC_TARGET_BYTEORDER == 4321) ^ DCC_TARGET_STACKDOWN
  DCCDisp_BytPush(sign_byte,sign_size);
#endif
#if DCC_TARGET_SIZEOF_IMM_MAX < 8 && \
   (DCC_TARGET_BYTEORDER == 4321) ^ DCC_TARGET_STACKDOWN
  if (curr_size > 4) {
   struct DCCMemLoc upper_cst;
   upper_cst.ml_reg = DCC_RC_CONST;
   upper_cst.ml_off = (target_off_t)(iv >> 32);
   upper_cst.ml_sym = NULL;
   DCCDisp_CstPush(&symaddr.ml_sad,curr_size-4);
   curr_size = 4;
  }
  DCCDisp_CstPush(&symaddr.ml_sad,curr_size);
#elif DCC_TARGET_SIZEOF_IMM_MAX < 8
  DCCDisp_CstPush(&symaddr.ml_sad,curr_size > 4 ? 4 : curr_size);
#else
  DCCDisp_CstPush(&symaddr.ml_sad,curr_size);
#endif
#if DCC_TARGET_SIZEOF_IMM_MAX < 8 && \
   (DCC_TARGET_BYTEORDER == 1234) ^ DCC_TARGET_STACKDOWN
  if (curr_size > 4) {
   struct DCCMemLoc upper_cst;
   upper_cst.ml_reg = DCC_RC_CONST;
   upper_cst.ml_off = (target_off_t)(iv >> 32);
   upper_cst.ml_sym = NULL;
   DCCDisp_CstPush(&symaddr.ml_sad,curr_size-4);
   curr_size = 4;
  }
#endif
#if (DCC_TARGET_BYTEORDER == 1234) ^ DCC_TARGET_STACKDOWN
  DCCDisp_BytPush(sign_byte,sign_size);
#endif
#if !DCC_TARGET_STACKDOWN
  DCCDisp_NdfPush(filler);
#endif
 } else if (self->sv_reg != DCC_RC_CONST) {
  target_siz_t register_memory,sign_memory;
  rc_t r1,r2;
  /* TODO: Floating-point registers? */
  /* Push register values. */
  DCCStackValue_FixRegOffset(self);
  r1 = self->sv_reg,r2 = self->sv_reg2;
  register_memory = DCC_RC_SIZE(r1);
  if (r2 != DCC_RC_CONST) register_memory += DCC_RC_SIZE(r2);
  assert(register_memory <= DCC_TARGET_SIZEOF_GP_REGISTER*2);
  sign_memory = 0;
  if (register_memory > result) {
   if (result <= DCC_TARGET_SIZEOF_GP_REGISTER) r2 = DCC_RC_CONST;
#ifdef DCC_RC_I64
   if (result <= 4) r1 &= ~(DCC_RC_I64);
#endif
   if (result <= 2) r1 &= ~(DCC_RC_I32);
   if (result <= 1) r1 &= ~(DCC_RC_I16);
  } else if (register_memory < result) {
   sign_memory = (result-register_memory);
   result      =  register_memory;
  }
#if DCC_TARGET_STACKDOWN
  DCCDisp_NdfPush(filler);
#endif
  /* TODO: Sign-memory, while working, is very inefficient:
   * >> char ch = ' ';
   * >> int i = isspace(ch);
   * Current assembly generation:
   * >> mov $' ', -1(%ebp) # ch = ' ';
   * >> mov -1(%ebp), %al  # Load 'ch' into AL
   * >> mov %al, %cl       # Create a new copy of 'ch' in 'CL'
   * >> sar $7, %cl        # Sign-extend 'CL'
   * >> sub $3, %esp       # Allocate 3 bytes of stack memory (sign-memory; 'sizeof(int)-sizeof(char)')
   * >> mov %cl, 0(%esp)   # Fill those 3 bytes with the sign-extend of 'CL'
   * >> mov %cl, 1(%esp)   # *ditto*
   * >> mov %cl, 2(%esp)   # *ditto*
   * >> dec %esp           # Allocate 1 byte of stack memory for the character itself
   * >> mov %al, 0(%esp)   # Push the character onto the stack
   * >> call isspace       # Call the function
   * >> add $4, %esp       # Cleanup
   * Instead, do this:
   * >> mov    $' ', -1(%ebp) # ch = ' ';
   * >> movsxb -1(%ebp), %eax # Load+sign-extend 'ch' into 'EAX'
   * >> pushl  %eax           # Push the 4-byte sign-extended character
   * >> call isspace          # Call the function
   * >> add $4, %esp          # Cleanup
   * WARNING:
   *    The current solution must still remain implemented, as
   *    it works for _ANY_ type size, and _ANY_ sign extension,
   *    whereas the proposed optimization only works for 1 --> 4
   *    byte, with the ability of more for 1 --> 2 and 2 --> 4
   */
#if (DCC_TARGET_BYTEORDER == 4321) ^ DCC_TARGET_STACKDOWN
  if (sign_memory) {
   rc_t temp = DCCVStack_GetReg(DCC_RC_I8,1);
   DCCDisp_RegMovReg(r1,temp,0);
   DCCDisp_SignMirrorReg(temp);
   DCCDisp_ByrPush(temp,sign_memory);
  }
#endif
  if (!(result&(result-1))) {
   /* Likely case: When the type-size is one of 1,2,4 or 8,
    *              we can push the register(s) directly! */
   if (r2 != DCC_RC_CONST) DCCDisp_RegPush(r2);
   DCCDisp_RegPush(r1);
  } else {
   /* Difficult case: Must manually handle special type sizes. */
#ifdef DCC_RC_I64
   if (result == 7 || result == 6 || result == 5) {
    rc_t temp;
    struct DCCSymAddr cst;
    assert(r1&DCC_RC_I64);
    temp = DCCVStack_GetReg(DCC_RC_I64,(result != 5));
    DCCDisp_RegMovReg(r1,temp,1);
    cst.sa_off = (result-1)*DCC_TARGET_BITPERBYTE;
    cst.sa_sym = NULL;
    DCCDisp_CstBinReg(TOK_SHR,&cst,temp,1);
         if (result == 7) temp &= ~(DCC_RC_I64),assert(temp&DCC_RC_I32);
    else if (result == 6) temp &= ~(DCC_RC_I64|DCC_RC_I32),assert(temp&DCC_RC_I16);
    else                  temp &= ~(DCC_RC_I64|DCC_RC_I32|DCC_RC_I16),assert(temp&DCC_RC_I8);
    DCCDisp_ByrPush(temp,1);
    DCCDisp_RegPush(r1&~(DCC_RC_I64));
   } else
#endif
   {
    rc_t temp;
    struct DCCSymAddr cst;
    assert(result == 3);
    assert(r1&DCC_RC_I16);
    assert(r1&DCC_RC_I32);
    temp = DCCVStack_GetReg(DCC_RC_I32,0);
    assert(temp&DCC_RC_I8);
    DCCDisp_RegMovReg(r1,temp,1);
    cst.sa_off = DCC_TARGET_BITPERBYTE*2;
    cst.sa_sym = NULL;
    DCCDisp_CstBinReg(TOK_SHR,&cst,temp,1);
    DCCDisp_RegPush(temp&~(DCC_RC_I32|DCC_RC_I16));
    DCCDisp_RegPush(r1&~(DCC_RC_I32));
   }
  }
#if (DCC_TARGET_BYTEORDER == 1234) ^ DCC_TARGET_STACKDOWN
  if (sign_memory) {
   DCCDisp_SignMirrorReg(r1);
   DCCDisp_ByrPush(r1,sign_memory);
  }
#endif
#if !DCC_TARGET_STACKDOWN
  DCCDisp_NdfPush(filler);
#endif
  result += sign_memory;
 }
 return result+filler;
}

//////////////////////////////////////////////////////////////////////////
//
//  CALL
//
#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Call(size_t n_args) {
 struct DCCStackValue *arg_first,*function;
 struct DCCDecl *funty_decl;
 struct DCCAttrDecl *funty_attr;
 target_siz_t arg_size,stack_align;
 uint32_t cc; size_t argc,untyped;
 struct DCCStructField *argv;
 struct DCCStackValue return_value;
 assert(vsize >= (1+n_args));
 VLOG(-(ptrdiff_t)n_args,("vcall(%lu)\n",(unsigned long)n_args));
 /* Kill all tests. */
 DCCVStack_KillTst();

 /* Promote array types. */
 /* NOTE: 'arg_end' is also the function slot. */
 function = (arg_first = vbottom)+n_args;
 DCCStackValue_LoadLValue(function);
 DCCStackValue_Promote(function);
 DCCStackValue_FixBitfield(function);
 DCCStackValue_FixTest(function);
 if (DCCTYPE_GROUP(function->sv_ctype.t_type) == DCCTYPE_POINTER) {
  assert(function->sv_ctype.t_base);
  /* Special case for function-pointers (This is actually what we want!) */
  if (DCCTYPE_GROUP(function->sv_ctype.t_base->d_type.t_type) ==
      DCCTYPE_FUNCTION) goto after_typefix;
  /* The function is a pointer.
   * To follow c conventions, we must load that pointer's source:
   * >> int (*funa)();
   * >> int funb();
   * >> 
   * >> funa(); // Same as '(*funa)()'
   * >> funb();
   */
  DCCStackValue_Unary(function,'*');
 }
 if (DCCTYPE_GROUP(function->sv_ctype.t_type) == DCCTYPE_FUNCTION) {
  /* If we're actually calling a function, make sure
   * to call its address, not the dereference of that. */
  DCCStackValue_Unary(function,'&');
 }
after_typefix:
 funty_decl = function->sv_ctype.t_base;
 if (DCCTYPE_GROUP(function->sv_ctype.t_type) == DCCTYPE_POINTER) {
  assert(funty_decl);
  assert(funty_decl->d_kind&DCC_DECLKIND_TYPE);
  funty_decl = funty_decl->d_type.t_base;
 }
 if (!funty_decl ||
     (funty_decl->d_kind != DCC_DECLKIND_FUNCTION &&
      funty_decl->d_kind != DCC_DECLKIND_OLDFUNCTION)) {
  WARN(W_EXPECTED_FUNCTION_TYPE_FOR_CALL,&function->sv_ctype);
  funty_decl = NULL;
 }
 DCCDecl_XIncref(funty_decl);
 cc          = DCC_ATTRFLAG_CC_CDECL;
 stack_align = DCC_TARGET_STACKALIGN;
 funty_attr  = NULL;
 argc        = 0;
 argv        = NULL;
 untyped     = n_args;
 if (funty_decl) {
  int wid;
  if ((funty_attr = funty_decl->d_attr) != NULL) {
   cc = (funty_attr->a_flags&DCC_ATTRFLAG_MASK_CALLCONV);
  }
  /* Ignore argument information when calling an old-style function. */
  if (funty_decl->d_kind != DCC_DECLKIND_OLDFUNCTION) {
   argc = funty_decl->d_tdecl.td_size;
   argv = funty_decl->d_tdecl.td_fieldv;
   if (n_args < argc) {
    /* Too few arguments */
    wid = W_CALL_TO_FEW_ARGUMENTS;
    goto warn_argc;
   } else if (n_args > argc && !(funty_decl->d_flag&DCC_DECLFLAG_VARIADIC)) {
    struct DCCType fty;
    /* Too many arguments */
    wid = W_CALL_TO_MANY_ARGUMENTS;
warn_argc:
    fty.t_type = DCCTYPE_FUNCTION;
    fty.t_base = funty_decl;
    WARN(wid,&fty,function->sv_sym
         ? function->sv_sym->sy_name
         : NULL,argc,n_args);
    if (argc > n_args) argc = n_args;
   }
   assert(argc <= n_args);
   argv += argc;
   untyped = n_args-argc;
  }
 }
 assert(!argc || argv);
 assert(argc+untyped == n_args);
 /* TODO: Calling conventions other than cdecl and stdcall! */
 /* TODO: __attribute__((regparm(...))) */
 arg_size    = 0;

#if DCC_TARGET_STACKDOWN
 while (vbottom != function) {
  assert(vbottom < function);
  if (!untyped) {
   --argv;
   assert(argv->sf_decl);
   /* Cast arguments to those specified by 'function'
    * (unless it's an old-style, or variadic function). */
   vcast(&argv->sf_decl->d_type,0);
  } else {
   --untyped;
   /* Just do regular type promotions on anything else. */
   vprom();
  }
  /* Function arguments are used by default. */
  vbottom->sv_flags &= ~(DCC_SFLAG_DO_WUNUSED);
  arg_size += DCCStackValue_PushAligned(vbottom,stack_align);
  vpop(1);
 }
#else
#error FIXME
#endif

 DCCVStack_KillAll(1); /* Kill all temporary registers still in use. */
 DCCStackValue_GetReturn(&return_value,funty_decl);

 DCCStackValue_Call(function); /* Generate the call instructions. */
 vpop(1); /* Pop the function. */


 if (arg_size && cc != DCC_ATTRFLAG_CC_STDCALL) {
  struct DCCSymAddr temp;
  /* Reclaim stack memory. */
  temp.sa_off = (target_off_t)arg_size;
  temp.sa_sym = NULL;
  DCCDisp_CstBinReg('+',&temp,DCC_RR_XSP,0);
 }
 /* TODO: What about caller-allocated structure memory? */

 vpush(&return_value);
 /* Set the WUNUSED flag for the return value
  * of functions marked with [[warn_unused_result]] */
 if (funty_attr &&
    (funty_attr->a_specs&DCC_ATTRSPEC_WUNUSED))
     vbottom->sv_flags |= DCC_SFLAG_DO_WUNUSED;
 vbottom->sv_flags |= DCC_SFLAG_RVALUE;
 DCCDecl_XDecref(funty_decl);
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif



//////////////////////////////////////////////////////////////////////////
//
//  JUMP
//
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Jcc(int invert) {
 assert(vsize >= 2);
 VLOG(-1,("vjcc(%s)\n",invert ? "~" : ""));
 DCCStackValue_Jcc(vbottom+1,vbottom,invert);
 vpop(1);
 vpop(1);
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Jmp(void) {
 VLOG(-1,("vjmp()\n"));
 assert(vsize >= 1);
 DCCStackValue_Jmp(vbottom);
 vpop(1);
}




//////////////////////////////////////////////////////////////////////////
//
//  SUBSCRIPT
//
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Subscript(struct TPPKeyword const *__restrict name) {
 assert(vsize >= 1);
 assert(name);
 VLOG(0,("vsubscript('%s')\n",name->k_name));
 DCCStackValue_FixBitfield(vbottom);
 DCCStackValue_FixTest(vbottom);
 DCCStackValue_Subscript(vbottom,name);
}



DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_C */
