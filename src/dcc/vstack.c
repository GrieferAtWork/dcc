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
PRIVATE void DCC_VSTACK_CALL DCCStackValue_FixRegOffset(struct DCCStackValue *__restrict self);
PRIVATE void DCC_VSTACK_CALL DCCStackValue_Dup(struct DCCStackValue *__restrict self);
PRIVATE int  DCC_VSTACK_CALL DCCStackValue_IsDuplicate(struct DCCStackValue *__restrict self, struct DCCStackValue *__restrict duplicate);

PRIVATE void DCC_VSTACK_CALL DCCStackValue_LodTest(struct DCCStackValue *__restrict self); /* The opposite of 'DCCStackValue_FixTest': Force 'self' to be a test. */

PUBLIC tyid_t DCC_VSTACK_CALL DCC_RC_GETTYPE(rc_t rc) {
 tyid_t result;
#ifdef DCC_RC_I64
 if (rc&DCC_RC_I64) result = DCCTYPE_INT64;
 else
#endif
      if (rc&DCC_RC_I32) result = DCCTYPE_INT32;
 else if (rc&DCC_RC_I16) result = DCCTYPE_INT16;
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
 default: result = DCC_TARGET_SIZEOF_POINTER; break;
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
DCCVStack_ReplaceCopy(void) {
 assert(vsize >= 1);
 /* Nothing else really needs to take place for this to happen... */
 vbottom->sv_flags |= (DCC_SFLAG_COPY|DCC_SFLAG_RVALUE);
}
PUBLIC void DCC_VSTACK_CALL DCCVStack_Dup(int copy) {
 assert(vsize >= 1);
 struct DCCStackValue *dst_slot,*src_slot;
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
 //DCCType_MkOldFunc(&slot.sv_ctype);
 //assert(slot.sv_ctype.t_base);
 vpush(&slot);
 //DCCDecl_Decref(slot.sv_ctype.t_base);
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
 if (decl->d_kind != DCC_DECLKIND_MLOC) {
  vpushi(DCCTYPE_VOID,0);
  return;
 }
 if (decl->d_mdecl.md_loc.ml_reg != DCC_RC_CONST) {
  DCCStackValue_SetMemDecl(&slot,&decl->d_mdecl);
 } else {
  slot.sv_reg      = decl->d_mdecl.md_loc.ml_reg;
  slot.sv_const.it = decl->d_mdecl.md_loc.ml_off;
  slot.sv_sym      = decl->d_mdecl.md_loc.ml_sym;
 }
 slot.sv_flags = DCC_SFLAG_LVALUE;
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
DCCVStack_PushReturn(struct DCCType const *funtype) {
 struct DCCStackValue slot;
 slot.sv_ctype.t_type = DCCTYPE_INT;
 slot.sv_ctype.t_base = NULL;
 slot.sv_flags        = DCC_SFLAG_NONE;
 slot.sv_const.it     = 0;
 slot.sv_sym          = NULL;
 slot.sv_reg2         = DCC_RC_CONST;
 if (!funtype || DCCTYPE_GROUP(funtype->t_type) != DCCTYPE_FUNCTION) {
push_default:
  slot.sv_reg = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
 } else {
  assert(funtype->t_base);
  slot.sv_ctype = funtype->t_base->d_type;
  if (DCCTYPE_GROUP(slot.sv_ctype.t_type) == DCCTYPE_BUILTIN) {
   /* TODO: floating-point registers. */
   if (DCCTYPE_ISSIGNLESSBASIC(slot.sv_ctype.t_type,DCCTYPE_INT64)) {
#ifdef DCC_RC_I64
    slot.sv_reg  = DCC_RC_I64|DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
#else
    slot.sv_reg  = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
    slot.sv_reg2 = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EDX;
#endif
   } else {
    slot.sv_reg = DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX;
   }
  } else if (DCCTYPE_GROUP(slot.sv_ctype.t_type) == DCCTYPE_POINTER ||
             DCCTYPE_GROUP(slot.sv_ctype.t_type) == DCCTYPE_LVALUE) {
   slot.sv_reg = DCC_RC_PTRX|DCC_ASMREG_EAX;
  } else {
   /* TODO: structure types. */
   goto push_default;
  }
 }
 vpush(&slot);
}


PUBLIC void DCC_VSTACK_CALL
DCCVStack_PushStr(char const *__restrict p, size_t s) {
 struct DCCStackValue slot;
 struct DCCSym *str_sym;
 /* Place a data symbol for the string. */
 str_sym = DCCSection_DAllocSym(unit.u_str,p,s*sizeof(char),
                               (s+1)*sizeof(char),1,0);
 if unlikely(!str_sym) { vpushi(DCCTYPE_INT,0); return; }
 slot.sv_ctype.t_base = NULL;
 /* TODO: 'DCCTYPE_CONST' should somehow be configurable. */
 slot.sv_ctype.t_type = DCCTYPE_BUILTIN|DCCTYPE_BYTE|DCCTYPE_CONST;
 if (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED)
  slot.sv_ctype.t_type |= DCCTYPE_UNSIGNED;
 /* Use a character array as typing for the string. */
 DCCType_MkArray(&slot.sv_ctype,s);
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
PUBLIC void DCC_VSTACK_CALL DCCVStack_Pop(int del) {
 assert(vsize != 0);
 if (!del);
 else if (/* Only perform destruction if we're supposed to. */
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
 DCCType_Quit(&vbottom->sv_ctype);
 DCCSym_XDecref(vbottom->sv_sym);
 ++vbottom;
}
PUBLIC void DCC_VSTACK_CALL DCCVStack_Swap(void) {
 struct DCCStackValue temp;
 assert(vsize >= 2);
 temp = vbottom[0];
 vbottom[0] = vbottom[1];
 vbottom[1] = temp;
}

PUBLIC void DCC_VSTACK_CALL DCCVStack_LRot(size_t n) {
 struct DCCStackValue temp;
 assert(n),--n;
 assert(vsize >= n);
 memcpy(&temp,vbottom,sizeof(struct DCCStackValue));
 memmove(vbottom,vbottom+1,n*sizeof(struct DCCStackValue));
 memcpy(vbottom+n,&temp,sizeof(struct DCCStackValue));
}

PUBLIC void DCC_VSTACK_CALL DCCVStack_RRot(size_t n) {
 struct DCCStackValue temp;
 assert(n),--n;
 assert(vsize >= n);
 memcpy(&temp,vbottom+n,sizeof(struct DCCStackValue));
 memmove(vbottom+1,vbottom,n*sizeof(struct DCCStackValue));
 memcpy(vbottom,&temp,sizeof(struct DCCStackValue));
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_Kill(struct DCCStackValue *__restrict self) {
 target_ptr_t s,a; int was_lvalue;
 struct DCCStackValue local_target;
 assert(self);
 /* Figure out how much space is required. */
 s = DCCType_Sizeof(&self->sv_ctype,&a,1);
 /* Allocate local stack-space. */
 local_target.sv_ctype        = self->sv_ctype;
 local_target.sv_flags        = DCC_SFLAG_LVALUE;
 local_target.sv_reg          = DCC_RR_XBP;
 local_target.sv_reg2         = DCC_RC_CONST;
 local_target.sv_const.it     = 0;
 local_target.sv_const.offset = DCCCompiler_HWStackAlloc(s,a,0);
 local_target.sv_sym          = NULL;
 /* Generate the store. */
 was_lvalue = !!(self->sv_flags&DCC_SFLAG_LVALUE);
 self->sv_flags &= ~(DCC_SFLAG_LVALUE);
 DCCStackValue_Store(self,&local_target,1);
 if (self->sv_sym) DCCSym_Decref(self->sv_sym);
 memcpy(self,&local_target,sizeof(struct DCCStackValue));
 /* If the variable was already an lvalue, we
  * must add an additional level of indirection. */
 if (was_lvalue) DCCType_MkLValue(&self->sv_ctype);
}

PUBLIC void DCC_VSTACK_CALL
DCCStackValue_Load(struct DCCStackValue *__restrict self) {
 struct DCCStackValue local_target;
 assert(self);
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 /* Figure out how much space is required. */
 local_target.sv_ctype = self->sv_ctype;
 local_target.sv_flags = DCC_SFLAG_NONE;
#if DCC_TARGET_SIZEOF_POINTER < 8
 if (DCCTYPE_ISSIGNLESSBASIC(self->sv_ctype.t_type,DCCTYPE_INT64)) {
  local_target.sv_reg  = DCCVStack_GetReg(DCC_RC_I32|DCC_R64_PREFLO,1);
  local_target.sv_reg2 = DCCVStack_GetReg(DCC_RC_I32|DCC_R64_PREFHI,1);
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
 memcpy(self,&local_target,sizeof(struct DCCStackValue));
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
   DCCDisp_SccReg((test_t)DCC_SFLAG_GTTEST(self->sv_flags),
                 target_reg);
   if (target->sv_reg2 != DCC_RC_CONST) {
    DCCDisp_RegBinReg('^',target->sv_reg2,target->sv_reg2,1);
   }
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
                     DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
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

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_FixRegOffset(struct DCCStackValue *__restrict self) {
 assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
 if (self->sv_const.it) {
  DCCStackValue_FixRegister(self);
  if (self->sv_const.it) {
   struct DCCSymAddr temp;
   temp.sa_off = (target_off_t)(self->sv_const.it);
   temp.sa_sym = self->sv_sym;
   if (self->sv_reg2 == DCC_RC_CONST) {
    DCCDisp_AddReg(&temp,self->sv_reg);
   } else {
    if (temp.sa_off < 0)
     temp.sa_off = -temp.sa_off,
     DCCDisp_CstBinReg('-',&temp,self->sv_reg,0);
    else DCCDisp_CstBinReg('+',&temp,self->sv_reg,0);
    temp.sa_off = (target_off_t)(self->sv_const.it >> 32);
    if (temp.sa_off < 0)
     temp.sa_off = -temp.sa_off,
     DCCDisp_CstBinReg(TOK_DEC,&temp,self->sv_reg2,0);
    else DCCDisp_CstBinReg(TOK_INC,&temp,self->sv_reg2,0);
   }
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
   DCCStackValue_Binary(&val,self,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type)
                        ? TOK_RANGLE3 : TOK_SHR);
  }
 }
}



#if DCC_TARGET_BIN == DCC_BINARY_PE
extern struct DCCSym *pe_getitasym(struct DCCSym *__restrict basesym);
PUBLIC void
DCCLinker_PEIndImport(struct DCCStackValue *__restrict self) {
 struct DCCSym *pesym,*iat_sym;
 struct DCCSection *symsec;
 assert(self);
 //if (!(self->sv_flags&DCC_SFLAG_LVALUE)) return;
 if (self->sv_reg != DCC_RC_CONST) return;
 if ((pesym = self->sv_sym) == NULL) return;
 if (!(pesym->sy_flags&DCC_SYMFLAG_PE_ITA_IND)) return;
 if ((symsec = pesym->sy_sec) == NULL) return;
 if (!DCCSection_ISIMPORT(symsec)) return;
 /* Fix this symbol through PE indirection. */
 if ((iat_sym = pesym->sy_peind) == NULL) {
  /* Don't generate missing IAT functions when no code should be generated. */
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
 if (self->sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(self);
 self->sv_flags |= DCC_SFLAG_LVALUE;
}
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */


PUBLIC void DCC_VSTACK_CALL
DCCStackValue_LoadLValue(struct DCCStackValue *__restrict self) {
 assert(self);
#if DCC_TARGET_BIN == DCC_BINARY_PE
 DCCLinker_PEIndImport(self);
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 while (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_LVALUE) {
  DCCStackValue_FixBitfield(self);
  DCCStackValue_FixTest(self);
  if (self->sv_flags&DCC_SFLAG_LVALUE) {
   struct DCCMemLoc addr; rc_t new_register;
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
  }
  DCCType_MkBase(&self->sv_ctype);
 }
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Dup(struct DCCStackValue *__restrict self) {
 struct DCCStackValue copy;
 struct DCCType *source_type;
 int is_basic_type;
 assert(vsize >= 1);
 if ((!(self->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_XREGISTER)) &&
      /* Must still _always_ copy restricted pointers (e.g.: '%esp') */
      !DCCStackValue_ISPROTECTED(self)) ||
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
#if DCC_TARGET_SIZEOF_POINTER < 8
  if (DCCTYPE_ISSIGNLESSBASIC(source_type->t_type,DCCTYPE_INT64)) {
   /* Need a second 32-bit register for this. */
   copy.sv_reg2 = DCCVStack_GetReg(DCC_RC_I32,1);
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
      DCCStackValue_IsDuplicate(self,self-1)) --self;
  else if (self != compiler.c_vstack.v_end &&
           DCCStackValue_IsDuplicate(self,self+1)) ++self;
  else goto done;
  DCCStackValue_Dup(self);
 }
done:

 assert(!(self->sv_flags&DCC_SFLAG_COPY));
}

PRIVATE int DCC_VSTACK_CALL
DCCStackValue_ConstUnary(struct DCCStackValue *__restrict self, tok_t op) {
 int_t iv; int ty;
 assert(self);
 assert(self->sv_reg == DCC_RC_CONST);
 ty = DCCTYPE_BASIC(self->sv_ctype.t_type) & ~(DCCTYPE_UNSIGNED);
      if (ty == DCCTYPE_INT64) iv = (int_t)self->sv_const.s64;
 else if (ty == DCCTYPE_WORD)  iv = (int_t)self->sv_const.s16;
 else if (ty == DCCTYPE_BYTE)  iv = (int_t)self->sv_const.s8;
 else                          iv = (int_t)self->sv_const.s32;
 switch (op) {
 case '!':
  iv = !(iv || self->sv_sym);
  DCCSym_XDecref(self->sv_sym);
  self->sv_sym = NULL;
  break;
 case '~':
 case '-':
  if (self->sv_sym) return 0;
  if (op == '~') iv = ~iv;
  else           iv = -iv;
  break;
 case TOK_INC: WARN(W_ASM_CONSTEXPR_INVALID_OPERATION); ++iv; break;
 case TOK_DEC: WARN(W_ASM_CONSTEXPR_INVALID_OPERATION); --iv; break;
 default: break;
 }
 self->sv_const.it = iv;
 return 1;
}

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Unary(struct DCCStackValue *__restrict self, tok_t op) {
 assert(self);
 if (op != '&' && op != '*') {
  /* Warn if an arithmetic operation is used with a non-arithmetic structure type. */
  if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_STRUCTURE &&
     (!self->sv_ctype.t_base->d_attr ||
      !(self->sv_ctype.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)
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
  if (!(self->sv_flags&(DCC_SFLAG_LVALUE|DCC_SFLAG_BITFLD))) {
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
  DCCStackValue_Promote(self);
  if (self->sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(self);
  if (!(self->sv_ctype.t_type&DCCTYPE_POINTER))
   WARN(W_EXPECTED_POINTER_FOR_DEREF,&self->sv_ctype);
  else DCCType_MkBase(&self->sv_ctype);
  self->sv_flags |= DCC_SFLAG_LVALUE;
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

 case '-':
  if (DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type)) {
   WARN(W_UNARY_NEG_ON_UNSIGNED_TYPE,&self->sv_ctype);
  }
  break;

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
                    target,n,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
  return;
 }

 if (self->sv_reg == DCC_RC_CONST) {
  /* Store a regular constant in 'target'. */
  temp.sa_sym = self->sv_sym;
  switch (n) {
  case 1: temp.sa_off = (target_off_t)self->sv_const.u8; goto integral_common;
  case 2: temp.sa_off = (target_off_t)DCC_H2T16(self->sv_const.u16); goto integral_common;
  case 4: temp.sa_off = (target_off_t)DCC_H2T32(self->sv_const.u32);
integral_common: DCCDisp_CstBinMem(op,&temp,target,n,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type)); break;
#ifdef DCC_RC_I64
  case 8: temp.sa_off = (target_off_t)DCC_H2T64(self->sv_const.u64); goto integral_common;
#endif
  default:
   DCCDisp_VecBinMem(op,&self->sv_const,sizeof(self->sv_const),
                     target,n,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
   break;
  }
  return;
 }
 /* mov %reg, symaddr */
 DCCStackValue_FixRegOffset(self);
 DCCDisp_RegsBinMems(op,self->sv_reg,self->sv_reg2,target,n,
                     DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
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
  DCCDisp_MemsBinReg(op,&src,DCCType_Sizeof(&self->sv_ctype,NULL,1),
                     dst,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
 } else if (source_reg == DCC_RC_CONST) {
#if DCC_TARGET_SIZEOF_POINTER < 8
  struct DCCSymExpr temp;
  temp.e_int = self->sv_const.it;
  temp.e_sym = self->sv_sym;
  DCCDisp_CstBinRegs(op,&temp,dst,dst2,
                     DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
#else
  struct DCCSymAddr temp;
  temp.sa_off = self->sv_const.it;
  temp.sa_sym = self->sv_sym;
  DCCDisp_CstBinReg(op,&temp,dst,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
  if (dst2 != DCC_RC_CONST) DCCDisp_RegBinReg('^',dst2,dst2,1);
#endif
 } else {
  /* *op %source_reg, target */
  DCCStackValue_FixRegOffset(self);
  DCCDisp_RegsBinRegs(op,self->sv_reg,self->sv_reg2,
                      dst,dst2,DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type));
 }
}

PRIVATE int DCC_VSTACK_CALL
DCCStackValue_ConstBinary(struct DCCStackValue *__restrict self, tok_t op,
                          struct DCCSymExpr const *__restrict exprval) {
 int_t iv; int ty;
 assert(self);
 assert(self->sv_reg == DCC_RC_CONST);
 assert(self->sv_reg2 == DCC_RC_CONST);
 assert(!(self->sv_flags&DCC_SFLAG_LVALUE));
 assert(exprval);
 ty = DCCTYPE_BASIC(self->sv_ctype.t_type) & ~(DCCTYPE_UNSIGNED);
      if (ty == DCCTYPE_INT64) iv = (int_t)self->sv_const.s64;
 else if (ty == DCCTYPE_WORD)  iv = (int_t)self->sv_const.s16;
 else if (ty == DCCTYPE_BYTE)  iv = (int_t)self->sv_const.s8;
 else                          iv = (int_t)self->sv_const.s32;
 switch (op) {

 case '+':
 case TOK_INC:
  iv += exprval->e_int;
  if (exprval->e_sym) {
   if (self->sv_sym) return 0;
   self->sv_sym = exprval->e_sym;
   DCCSym_Incref(exprval->e_sym);
  }
  break;

 case '-':
 case '?':
 case TOK_DEC:
  iv -= exprval->e_int;
  /* Special case: Difference between two symbols. */
  if (self->sv_sym && exprval->e_sym) {
   if (self->sv_sym == exprval->e_sym) {
    /* Always succeeds: Difference between the same symbol. */
   } else if (DCCSym_SECTION(self->sv_sym) &&
              DCCSym_SECTION(self->sv_sym) ==
              DCCSym_SECTION(exprval->e_sym)) {
    /* Special case: Difference between two defined symbols from the same section. */
    iv += self->sv_sym->sy_addr-exprval->e_sym->sy_addr;
    DCCSym_Decref(self->sv_sym);
    self->sv_sym = NULL;
   } else {
    /* Special case: undefined symbols, or symbols from different sections. */
    return 0;
   }
  }
  break;

 case '&':
 case '|':
 case '^':
 case '*':
 case '/':
 case '%':
  if (self->sv_sym || exprval->e_sym) {
   WARN(W_ASM_INVALID_SYMBOL_OPERATION);
   DCCSym_XDecref(self->sv_sym);
   self->sv_sym = NULL;
  }
       if (op == '&') iv &= exprval->e_int;
  else if (op == '|') iv |= exprval->e_int;
  else if (op == '^') iv ^= exprval->e_int;
  else if (op == '*') iv *= exprval->e_int;
  else if (!exprval->e_int) WARN(W_DIVIDE_BY_ZERO);
  else if (op == '/') iv /= exprval->e_int;
  else                iv %= exprval->e_int;
  break;

 {
  int_t rhsv;
 case TOK_LOWER:
 case TOK_LOWER_EQUAL:
 case TOK_EQUAL:
 case TOK_NOT_EQUAL:
 case TOK_GREATER:
 case TOK_GREATER_EQUAL:
  /* Special handling for compare operations. */
  rhsv = exprval->e_int;
  if (self->sv_sym && exprval->e_sym) {
   /* Symbols are given on both sides. */
   if (DCCSym_SECTION(self->sv_sym) &&
       DCCSym_SECTION(self->sv_sym) ==
       DCCSym_SECTION(exprval->e_sym)) {
    /* Symbols are located within the same section. */
    iv   += self->sv_sym->sy_addr;
    rhsv += exprval->e_sym->sy_addr;
   } else {
    /* Symbols from different sections (can't generate constant expression) */
    return 0;
   }
  } else if (self->sv_sym || exprval->e_sym) {
   /* Only one side has a symbol. */
   return 0;
  }
  switch (op) {
   case TOK_LOWER:       iv = iv <  rhsv; break;
   case TOK_LOWER_EQUAL: iv = iv <= rhsv; break;
   case TOK_EQUAL:       iv = iv == rhsv; break;
   case TOK_NOT_EQUAL:   iv = iv != rhsv; break;
   case TOK_GREATER:     iv = iv >  rhsv; break;
   default:              iv = iv >= rhsv; break;
  }
 } break;

 default: break;
 }
 self->sv_const.it = iv;
 return 1;
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
 int_t iv; int ty;
 assert(self);
 assert(target);
 if (op != '=') {
  /* Warn if an arithmetic operation is used with a non-arithmetic structure type. */
  if (DCCTYPE_GROUP(self->sv_ctype.t_type) == DCCTYPE_STRUCTURE &&
     (!self->sv_ctype.t_base->d_attr ||
      !(self->sv_ctype.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)
      )) WARN(W_STRUCTURE_ARITHMETIC,&self->sv_ctype);
  if (DCCTYPE_GROUP(target->sv_ctype.t_type) == DCCTYPE_STRUCTURE &&
     (!target->sv_ctype.t_base->d_attr ||
      !(target->sv_ctype.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)
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
  if (!(lhs->sv_flags&DCC_SFLAG_LVALUE) &&
       (lhs->sv_reg == DCC_RC_CONST)) {
   struct cmppair const *iter = compare_pairs;
   /* Lhs is a constant operand.
    * >> Swap the arguments to simply stuff further below! */
   DCCStackValue_Swap(lhs,rhs);
   for (;; ++iter) {
    if (op == iter->a) { op = iter->b; break; }
    if (op == iter->b) { op = iter->a; break; }
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
     if (DCCStackValue_ConstBinary(lhs,op,&temp)) goto end_cmp;
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
    assertf(!lhs->sv_sym,"Register offsets can't include symbols");
    assertf(!rhs->sv_sym,"Register offsets can't include symbols");
    temp.e_int   = rhs->sv_const.it;
    temp.e_sym   = NULL;
    lhs->sv_reg  = DCC_RC_CONST;
    lhs->sv_reg2 = DCC_RC_CONST;
    /* The following must always succeed... */
#if DCC_DEBUG
    cresult = DCCStackValue_ConstBinary(lhs,op,&temp);
    assert(cresult);
#else
    DCCStackValue_ConstBinary(lhs,op,&temp);
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
    uint16_t tok,cmp;
   } const compare_ids[] = {
    {TOK_LOWER,        DCC_TEST_L},
    {TOK_LOWER_EQUAL,  DCC_TEST_LE},
    {TOK_EQUAL,        DCC_TEST_E},
    {TOK_NOT_EQUAL,    DCC_TEST_NE},
    {TOK_GREATER,      DCC_TEST_G},
    {TOK_GREATER_EQUAL,DCC_TEST_GE},
   };
   test = 0;
   while (compare_ids[test].tok != op) ++test;
   test = compare_ids[test].cmp;
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
  if (op == '?')
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

 if (target->sv_reg == DCC_RC_CONST) {
  /* NOTE: two-sided constant binary is handled later. */
  if (self->sv_reg == DCC_RC_CONST) goto default_binary;
  if ((op == '+' || op == '*' || op == '&' ||
       op == '|' || op == '^' || op == '?') &&
      DCCTYPE_GROUP(self  ->sv_ctype.t_type) != DCCTYPE_LVALUE &&
      DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE) {
#ifdef __INTELLISENSE__
   /* This is how you write offsetof in a way that intellisense can understand!
    * >> How da fuq did you not realize that, Microsoft? You're the DEV of both! */
#undef offsetof
#define offsetof(s,m) (size_t)&((s *)0)->m
#endif
   char temp[sizeof(struct DCCStackValue)-offsetof(struct DCCStackValue,sv_flags)];
   /* Handle optimizations for commutative operators in rhs.
    * >> This not only simplifies optimization, but is kind-of
    *    a requirement to prevent assembly code like:
    * >> int x = 10+y;
    *    mov $10, %eax
    *    mov y, %ecx
    *    add %eax, %ecx
    *    mov %ecx, x
    * >> // Instead of:
    *    mov y, %eax
    *    add $10, %eax
    *    mov %eax, x
    */
   memcpy(temp,&self->sv_flags,sizeof(temp));
   memcpy(&self->sv_flags,&target->sv_flags,sizeof(temp));
   memcpy(&target->sv_flags,temp,sizeof(temp));
   goto constant_rhs;
  }
  if (!target->sv_sym) {
   ty = DCCTYPE_BASIC(target->sv_ctype.t_type) & ~(DCCTYPE_UNSIGNED);
        if (ty == DCCTYPE_INT64) iv = (int_t)target->sv_const.s64;
   else if (ty == DCCTYPE_WORD)  iv = (int_t)target->sv_const.s16;
   else if (ty == DCCTYPE_BYTE)  iv = (int_t)target->sv_const.s8;
   else                          iv = (int_t)target->sv_const.s32;
   switch (op) {
   case '-':
    if (!iv) {
     /* '0-foo()' --> '-foo()' */
     DCCStackValue_Swap(self,target);
     DCCStackValue_Unary(self,'-');
     return;
    }
    break;

   case TOK_SHR: case TOK_RANGLE3:
    if (iv == 1) {
     /* '1 >> foo()' --> '!foo()' */
     DCCStackValue_Swap(self,target);
     DCCStackValue_Unary(self,'!');
     return;
    }
   case '/':
   case '%':
   case TOK_SHL:
    if (!iv) {
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
  if (!self->sv_sym) {
   assert(self->sv_reg == DCC_RC_CONST);
   ty = DCCTYPE_BASIC(self->sv_ctype.t_type) & ~(DCCTYPE_UNSIGNED);
        if (ty == DCCTYPE_INT64) iv = (int_t)self->sv_const.s64;
   else if (ty == DCCTYPE_WORD)  iv = (int_t)self->sv_const.s16;
   else if (ty == DCCTYPE_BYTE)  iv = (int_t)self->sv_const.s8;
   else                          iv = (int_t)self->sv_const.s32;
   switch (op) {

   case '+':
   case '-':
    /* 'foo -= 0' --> 'foo()' */
    /* 'foo += 0' --> 'foo()' */
    if (!iv) return;
    break;

   {
    int_t n; int shift;
   case '*':
    if (!iv) {
set_zero:
     /* 'foo *= 0' --> 'foo ^= foo' */
     DCCStackValue_Binary(target,target,'^');
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
    int_t n; int shift;
   case '/':
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
     op = DCCTYPE_ISUNSIGNED(target->sv_ctype.t_type) ? TOK_RANGLE3 : TOK_SHR;
    }
   } break;

   {
    int_t n;
   case '%':
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
   case TOK_SHR: case TOK_RANGLE3:
    /* 'foo <<= 0' --> 'foo()' */
    /* 'foo >>= 0' --> 'foo()' */
    if (!iv) return;
    break;

   case TOK_AND:
    /* 'foo &= 0' --> 'foo ^= foo' */
    /* 'foo &= -1' --> 'foo' */
    if (!iv) goto set_zero;
    /* TODO: Check the size of 'self' and if all bits of
     *       'iv' are set for that size, don't do anything. */
    if (iv == -1) return;
    break;

   case TOK_OR:
    /* 'foo |= 0' --> 'foo' */
    /* 'foo |= -1' --> 'foo = -1' */
    if (!iv) return;
    /* TODO: Check the size of 'self' and if no bits of
     *       'iv' are set for that size, don't do anything. */
    if (iv == -1) { DCCStackValue_Store(self,target,0); return; }
    break;

   case TOK_XOR:
    /* 'foo ^= 0' --> 'foo' */
    /* 'foo ^= -1' --> 'foo = ~foo' */
    if (!iv) return;
    if (iv == -1) { DCCStackValue_Unary(target,'~'); return; }
    break;

   default: break;
   }
  }
 }
default_binary:

 DCCStackValue_LoadLValue(self);
 DCCStackValue_LoadLValue(target);

 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 assert(DCCTYPE_GROUP(target->sv_ctype.t_type) != DCCTYPE_LVALUE);
 if (self->sv_reg == DCC_RC_CONST &&
     self->sv_reg2 == DCC_RC_CONST &&
    (op == '+' || op == '-') &&
    (!target->sv_sym || !self->sv_sym) &&
    (!(target->sv_flags&DCC_SFLAG_LVALUE)) &&
    (!self->sv_sym || target->sv_reg == DCC_RC_CONST)) {
  /* Special case: Add/Sub offset. */
  if (self->sv_sym) assert(!target->sv_sym),target->sv_sym = self->sv_sym;
  if (op == '+') target->sv_const.it += self->sv_const.it;
  else           target->sv_const.it -= self->sv_const.it;
  /* Set the X-offset flag to ensure the offset is added during pop. */
  target->sv_flags |= DCC_SFLAG_XOFFSET;
  return;
 }

 /* Kill all tests. */
 DCCVStack_KillTst();
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 assert(!(target->sv_flags&DCC_SFLAG_TEST));

 /* Invoke copy-on-write for the target. */
 if (op != '?') DCCStackValue_Cow(target);

 if ((target->sv_flags&DCC_SFLAG_LVALUE) ||
      target->sv_reg == DCC_RC_CONST) {
  struct DCCMemLoc dest;
  if (!(target->sv_flags&DCC_SFLAG_LVALUE)) {
   if (self->sv_reg == DCC_RC_CONST &&
       self->sv_reg2 == DCC_RC_CONST &&
     !(target->sv_flags&DCC_SFLAG_LVALUE)) {
    struct DCCSymExpr temp;
    /* Binary operation on constant expression. */
    temp.e_int = self->sv_const.it;
    temp.e_sym = self->sv_sym;
    if (DCCStackValue_ConstBinary(target,op,&temp)) return;
   }
   WARN(W_EXPECTED_LVALUE_FOR_BINARY_OP,&target->sv_ctype);
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

PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Cast(struct DCCStackValue *__restrict self,
                   struct DCCType const *__restrict type) {
 struct DCCDecl *base_decl; int group;
 assert(self);
 assert(type);
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
  static rc_t const classes[] = {
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
  if (group == DCCTYPE_POINTER || group == DCCTYPE_FUNCTION) to = DCCTYPE_SIZE,was_unsigned = 1;
  else to = DCCTYPE_BASIC(self->sv_ctype.t_type)&~(DCCTYPE_UNSIGNED),
       was_unsigned = DCCTYPE_ISUNSIGNED(type->t_type);
  if (DCCTYPE_GROUP(type->t_type) == DCCTYPE_POINTER) tn = DCCTYPE_SIZE;
  else tn = DCCTYPE_BASIC(type->t_type)&~(DCCTYPE_UNSIGNED);
  /* Special case: No change in size. */
  if (to == tn) goto done;
  /* Fix tests. */
  DCCStackValue_FixTest(self);
  if (to == DCCTYPE_BYTE && (self->sv_reg&4)) {
   /* High-order 8-bit register (Need to move into the low-order one) */
   DCCVStack_GetRegExact(DCC_RC_I8|(self->sv_reg&3));
  }
  assert(tn >= 0 && tn <= 3);
  new_class = classes[tn];
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
   if (!DCCTYPE_ISUNSIGNED(type->t_type)) {
    /* sign-extend 'self->sv_reg' into 'self->sv_reg2' */
    DCCDisp_RegMovReg(self->sv_reg2,self->sv_reg2,1);
    DCCDisp_SignExtendReg(self->sv_reg2);
   } else {
    DCCDisp_RegBinReg('^',self->sv_reg2,self->sv_reg2,1);
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
                        struct TPPKeyword const *__restrict member_name) {
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
   result = DCCDecl_FindStructField(field_decl->d_type.t_base,member_name);
   if (result) return result;
  }
 }
 return NULL;
}


PRIVATE void DCC_VSTACK_CALL
DCCStackValue_Subscript(struct DCCStackValue *__restrict self,
                        struct TPPKeyword const *__restrict member_name) {
 struct DCCStructField *field;
 struct DCCDecl *field_type_base;
 assert(self);
 assert(member_name);
 assert(!(self->sv_flags&DCC_SFLAG_TEST));
 DCCStackValue_LoadLValue(self);
 assert(DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_LVALUE);
 if (DCCTYPE_GROUP(self->sv_ctype.t_type) != DCCTYPE_STRUCTURE) goto err;
 /* NOTE: No need to check the 'DCC_SYMFLAG_FORWARD' flag.
  *       If the struct is only forward-declared, it will have an empty field vector. */
 field = DCCDecl_FindStructField(self->sv_ctype.t_base,member_name);
 if unlikely(!field) goto err;
 DCCStackValue_AddOffset(self,field->sf_off);
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
 WARN(W_UNKNOWN_FIELD,&self->sv_ctype,member_name);
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
   self->sv_flags &= ~(DCC_SFLAG_LVALUE|DCC_SFLAG_COPY);
  }
 } break;
 case DCCTYPE_FUNCTION:
  if (!(self->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(self);
  DCCType_MkPointer(&self->sv_ctype);
  self->sv_flags &= ~(DCC_SFLAG_LVALUE|DCC_SFLAG_COPY);
  break;
 {
 case DCCTYPE_LVALUE:
  struct DCCType *lv_base;
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


PUBLIC rc_t DCC_VSTACK_CALL
DCCVStack_GetReg(rc_t rc, int allow_ptr_regs) {
 struct DCCStackValue *iter,*end;
 uint8_t r,in_use,pref; /* Bitset of registers already in use. */
 int second_pass;
 assertf(!(rc&DCC_RI_MASK),"The register part of RC must be ZERO");
#ifdef DCC_RC_I64
 if (rc&DCC_RC_I64) rc |= DCC_RC_I32;
#endif
 if (rc&DCC_RC_I32) rc |= DCC_RC_I16;
 if (rc&DCC_RC_I16) rc |= DCC_RC_I8;
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
  for (; iter != end; ++iter) {
   /* Only kill explicit register values on a second pass (aka, when we're forced, to!) */
   if ((!(iter->sv_flags&DCC_SFLAG_XREGISTER) || second_pass) &&
       (((rc&DCC_RC_I) && ((iter->sv_reg&DCC_RC_I) ||
                           (iter->sv_reg2&DCC_RC_I))) ||
        ((iter->sv_reg&DCC_RC_MASK) == rc) ||
        ((iter->sv_reg2&DCC_RC_MASK) == rc))) {
#ifdef IA32_PROTECTED_REGISTERS
    if (iter->sv_reg&(DCC_RC_I16|DCC_RC_I3264)) {
     if ((iter->sv_reg&4) ||
         (iter->sv_reg&7) == DCC_ASMREG_BX) continue;
    } else {
     if ((iter->sv_reg&7) == DCC_ASMREG_BL ||
         (iter->sv_reg&7) == DCC_ASMREG_BH) continue;
    }
#else /* IA32_PROTECTED_REGISTERS */
    /* Make sure to skip pointer registers if those aren't allowed. */
    if (!allow_ptr_regs && (
       (iter->sv_reg&(DCC_RC_I16|DCC_RC_I3264) && DCC_ASMREG_ISSPTR(iter->sv_reg&7)) ||
       (iter->sv_reg2&(DCC_RC_I16|DCC_RC_I3264) && DCC_ASMREG_ISSPTR(iter->sv_reg2&7))
       )) continue;
#endif /* !IA32_PROTECTED_REGISTERS */
    /* Kill the stack value. */
    DCCStackValue_Kill(iter);
    /* Start over.
     * NOTE: Must search again for a case like 'al'+'ah' were
     *       both in use, but we only killed one of them. */
    goto again;
   }
  }
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
  for (; iter != end; ++iter) {
   /* Only kill explicit register values on a second pass (aka, when we're forced, to!) */
   if ((!(iter->sv_flags&DCC_SFLAG_XREGISTER) || second_pass) &&
       (((rc&DCC_RC_I) && ((iter->sv_reg&DCC_RC_I) ||
                           (iter->sv_reg2&DCC_RC_I))) ||
        ((iter->sv_reg&DCC_RC_MASK) == rc) ||
        ((iter->sv_reg2&DCC_RC_MASK) == rc))) {
    /* Make sure to skip pointer registers if those aren't allowed. */
    if (!(wanted_set&(1 << (iter->sv_reg&7))) &&
        !(wanted_set&(1 << (iter->sv_reg2&7)))
        ) continue;
    /* Kill the stack value. */
    DCCStackValue_Kill(iter);
    /* Start over.
     * NOTE: Must search again for a case like 'al'+'ah' were
     *       both in use, but we only killed one of them. */
    goto again;
   }
  }
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
DCCVStack_KillAll(void) {
 struct DCCStackValue *iter,*end;
 iter = compiler.c_vstack.v_bottom;
 end  = compiler.c_vstack.v_end;
 for (; iter != end; ++iter) {
  if (!(iter->sv_flags&DCC_SFLAG_LVALUE) &&
      ((iter->sv_reg != DCC_RC_CONST) ||
       (iter->sv_reg2 != DCC_RC_CONST))) {
   /* Kill this stack value. */
   DCCStackValue_Kill(iter);
   assert(iter->sv_flags&DCC_SFLAG_LVALUE);
  }
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_KillTst(void) {
 struct DCCStackValue *iter,*end;
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





//////////////////////////////////////////////////////////////////////////
// 
//  UNARY
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Unary(tok_t op) {
 assert(vsize >= 1);
 if (op != '!' && op != '*' &&
   !(vbottom->sv_flags&DCC_SFLAG_COPY)) {
  if (vbottom->sv_ctype.t_type&DCCTYPE_CONST)
      WARN(W_UNARY_CONSTANT_TYPE,&vbottom->sv_ctype);
  if (vbottom->sv_flags&DCC_SFLAG_RVALUE)
      WARN(W_UNARY_RVALUE_TYPE,&vbottom->sv_ctype);
 }
 if ((op == TOK_INC || op == TOK_DEC) &&
     (vbottom->sv_ctype.t_type&DCCTYPE_POINTER)) {
  struct DCCStackValue multiplier;
  /* Pointer arithmetic: Add/Sub the size of the pointer base. */
  assert(vbottom->sv_ctype.t_base);
  assert(vbottom->sv_ctype.t_base->d_kind&DCC_DECLKIND_TYPE);
  multiplier.sv_const.ptr = DCCType_Sizeof(&vbottom->sv_ctype.t_base->d_type,NULL,0);
  if (!multiplier.sv_const.ptr) {
   if (HAS(EXT_VOID_ARITHMETIC)) multiplier.sv_const.ptr = 1;
   else { WARN(W_POINTER_ARITHMETIC_VOID,&vbottom->sv_ctype.t_base->d_type); return; }
  }
  if (multiplier.sv_const.ptr == 1) goto gen_unary; /* Compile as a regular inc/dec. */
  multiplier.sv_ctype.t_type = DCCTYPE_SIZE|DCCTYPE_UNSIGNED;
  multiplier.sv_ctype.t_base = NULL;
  multiplier.sv_flags        = DCC_SFLAG_NONE;
  multiplier.sv_reg          = DCC_RC_CONST;
  multiplier.sv_reg2         = DCC_RC_CONST;
  multiplier.sv_const.it     = 0;
  multiplier.sv_sym          = NULL;
  DCCStackValue_Binary(vbottom,&multiplier,op == TOK_INC ? '+' : '-');
  return;
 }
gen_unary:
 DCCStackValue_Unary(vbottom,op);
 if (op == '!' || op == '&') {
  vbottom->sv_flags |=  (DCC_SFLAG_RVALUE);
 } else {
  vbottom->sv_flags &= ~(DCC_SFLAG_RVALUE);
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Bitfldf(sflag_t flags) {
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
 assert(vsize >= 2);
 /* Promote array types. */
 DCCStackValue_Promote(vbottom+1);
 vprom();
 if (op != '?' && !(vbottom[1].sv_flags&DCC_SFLAG_COPY)) {
  if (vbottom[1].sv_ctype.t_type&DCCTYPE_CONST)
      WARN(W_BINARY_CONSTANT_TYPE,&vbottom->sv_ctype,&vbottom[1].sv_ctype);
  if (vbottom[1].sv_flags&DCC_SFLAG_RVALUE)
      WARN(W_BINARY_RVALUE_TYPE,&vbottom->sv_ctype,&vbottom[1].sv_ctype);
 }
 switch (op) {
 case '+':
 case '-':
  if (vbottom[1].sv_ctype.t_type&DCCTYPE_POINTER) {
   struct DCCType *pointer_base;
   struct DCCStackValue multiplier;
   assert(vbottom[1].sv_ctype.t_base);
   assert(vbottom[1].sv_ctype.t_base->d_kind&DCC_DECLKIND_TYPE);
   pointer_base = &vbottom[1].sv_ctype.t_base->d_type;
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
   if (op == '-' && (vbottom[0].sv_ctype.t_type&DCCTYPE_POINTER)) {
    static struct DCCType const ty_ptrdiff = {DCCTYPE_PTRDIFF,NULL};
    /* Pointer/pointer difference >> (a-b)/sizeof(*a). */
    int compatible = DCCType_IsCompatible(pointer_base,&vbottom[0].sv_ctype.t_base->d_type,1);
    if (!compatible) WARN(W_POINTER_ARITHMETIC_INCOMPATIBLE_DIFF,
                          &vbottom[1].sv_ctype,&vbottom[0].sv_ctype);
    /* Calculate the difference between the operands. */
    DCCStackValue_Binary(vbottom,vbottom+1,'-');
    /* Divide the result by the multiplier size (size of the pointer base). */
    DCCStackValue_Binary(&multiplier,vbottom+1,'/');
    /* Cast the result to 'ptrdiff_t'. */
    DCCStackValue_Cast(vbottom+1,&ty_ptrdiff);
    goto end_pop;
   } else if (!DCCTYPE_ISINT(vbottom->sv_ctype.t_type)) {
    /* Check if vbottom is an integral type and warn if it isn't */
    WARN(W_POINTER_ARITHMETIC_EXPECTED_INTEGRAL,&vbottom->sv_ctype);
   }
   /* Mark 'vbottom' for copy-on-write, as we're about to modify it. */
   vbottom->sv_flags |= DCC_SFLAG_COPY;
   DCCStackValue_Binary(&multiplier,vbottom,'*');
   goto genbinary;
  }
  break;

 case TOK_SHL:
 case TOK_SHR:
  if (DCCTYPE_ISUNSIGNED(vbottom[1].sv_ctype.t_type)) {
   if (op == TOK_SHR) op = TOK_RANGLE3; /* Use the unsigned opcode. */
  }
  break;
 default: break;
 }

 {
  struct DCCType const *lhs_type; int wid;
  lhs_type = &vbottom[1].sv_ctype;
  while (DCCTYPE_GROUP(lhs_type->t_type) == DCCTYPE_LVALUE)
         assert(lhs_type->t_base),
         lhs_type = &lhs_type->t_base->d_type;
  /* Emit warnings about incompatibilities between 'vbottom[0]' and 'vbottom[1]'. */
  wid = DCCStackValue_AllowCast(vbottom,lhs_type,0);
  if (wid) WARN(wid,&vbottom->sv_ctype,lhs_type);
 }
genbinary:
 DCCStackValue_Binary(vbottom,vbottom+1,op);
end_pop:
 vpop(1);
 if (op != '?' &&
     op != TOK_LOWER && op != TOK_LOWER_EQUAL &&
     op != TOK_EQUAL && op != TOK_NOT_EQUAL &&
     op != TOK_GREATER && op != TOK_GREATER_EQUAL) {
  vbottom->sv_flags &= ~(DCC_SFLAG_RVALUE);
 } else {
  vbottom->sv_flags |=  (DCC_SFLAG_RVALUE);
 }
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Store(int initial_store) {
 struct DCCStackValue *target;
 struct DCCType const *target_type;
 int wid;
 assert(vsize >= 2);
 if (!initial_store ||
    (!DCCTYPE_ISARRAY(vbottom[1].sv_ctype.t_type) &&
      DCCTYPE_GROUP(vbottom[1].sv_ctype.t_type) != DCCTYPE_LVALUE) ||
     !DCCTYPE_ISARRAY(vbottom[0].sv_ctype.t_type)) {
  vprom();
 }
 target = &vbottom[1];
 /* Warn if the target is const. */
 target_type = &target->sv_ctype;
 while (DCCTYPE_GROUP(target_type->t_type) == DCCTYPE_LVALUE)
        assert(target_type->t_base),
        target_type = &target_type->t_base->d_type;
 /* NOTE: Allow assignment to a constant only during the initial store! */
 if (!initial_store &&
    (target_type->t_type&DCCTYPE_CONST))
     WARN(W_ASSIGN_CONSTANT_TYPE,&vbottom->sv_ctype,target_type);
 if (target->sv_flags&DCC_SFLAG_RVALUE)
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
 wid = DCCStackValue_AllowCast(vbottom,target_type,0);
 if (wid) WARN(wid,&vbottom->sv_ctype,target_type);
genstore:
 DCCStackValue_Store(vbottom,target,initial_store);
donepop:
 vpop(1);
}


static target_ptr_t const basic_type_weights[] = {
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_INT,       /* DCCTYPE_INT */
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_CHAR,      /* DCCTYPE_BYTE */
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_SHORT,     /* DCCTYPE_SHORT */
 DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_LONG_LONG, /* DCCTYPE_LLONG */
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
     (struct_decl->d_attr->a_flags&DCC_ATTRFLAG_TRANSUNION))) {
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
 vtyp = &value->sv_ctype;
 if (DCCType_IsCompatible(vtyp,type,1))
  return 0;
 is_const = value->sv_reg == DCC_RC_CONST &&
          !(value->sv_flags&DCC_SFLAG_LVALUE) &&
          !(value->sv_sym);
 tid = type->t_type&~(DCCTYPE_QUAL);
 vid = vtyp->t_type&~(DCCTYPE_QUAL);
 switch (DCCTYPE_GROUP(tid)) {
 case DCCTYPE_BUILTIN:
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
    while (iv) ++req_bits,iv >>= 1;
    req_bits += is_signed;
    /* If more bits are required that available, emit a warning. */
    if (req_bits > tweight) return W_CAST_INTEGRAL_OVERFLOW;
    if ((tid&DCCTYPE_UNSIGNED) && is_signed) return W_CAST_INTEGRAL_SIGNLOSS;
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
   if (basic_type_weights[tid&3] != DCC_TARGET_SIZEOF_POINTER*8)
    return W_CAST_POINTER_TO_INT_SIZ;
   return W_CAST_POINTER_TO_INT;
  }
  if (DCCTYPE_GROUP(vid) == DCCTYPE_STRUCTURE &&
      value->sv_ctype.t_base->d_attr &&
     (value->sv_ctype.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)) {
   target_ptr_t tweight;
   if (explicit_cast) return 0; /* Always OK for explicit casts. */
   /* arith-structure --> integral. */
   if (tid == DCCTYPE_BOOL) tweight = 1;
   else tweight = basic_type_weights[tid&3];
   if (tweight < DCCType_Sizeof(&value->sv_ctype,NULL,1)*8) return W_CAST_INTEGRAL_MAYOVERFLOW;
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
   if (basic_type_weights[vid&3] != DCC_TARGET_SIZEOF_POINTER*8
       ) return W_CAST_INT_TO_POINTER_SIZ;
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
     (value->sv_ctype.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)) {
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
  /* Always OK for explicit casts.
   * NOTE: r-values are converted to l-values. */
  if (explicit_cast) return 0;
  if (!(value->sv_flags&DCC_SFLAG_LVALUE) &&
        DCCTYPE_GROUP(vid) != DCCTYPE_LVALUE
      ) return W_CAST_RVALUE_TO_LVALUE;
  if (DCCTYPE_GROUP(vid) == DCCTYPE_LVALUE)
      assert(vtyp->t_base),
      vtyp = &vtyp->t_base->d_type;
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
     (type->t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)) {
   /* Cast to arithmetic structure type. */
   if (DCCTYPE_GROUP(vid) == DCCTYPE_BUILTIN) {
    target_ptr_t tweight;
    if (explicit_cast) return 0; /* Always OK for explicit casts. */
    vid &= DCCTYPE_BASICMASK;
    if (DCCTYPE_ISFLOATT(vid)) return W_CAST_FLOAT_TO_INT; /* Implicit cast of floating-point to integer. */
    tweight = DCCType_Sizeof(type,NULL,1)*8;
    if (is_const) {
     /* Special case: We know the value at compile-time
      * and can better decide what it really needs. */
     int_t iv = value->sv_const.it;
     unsigned int is_signed = iv < 0;
     unsigned int req_bits = 0;
     if (is_signed) iv = -iv;
     while (iv) ++req_bits,iv >>= 1;
     req_bits += is_signed;
     /* If more bits are required that available, emit a warning. */
     if (req_bits > tweight) return W_CAST_INTEGRAL_OVERFLOW;
     if ((tid&DCCTYPE_UNSIGNED) && is_signed) return W_CAST_INTEGRAL_SIGNLOSS;
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
      (value->sv_ctype.t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC)) {
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
//  CAST
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Cast(struct DCCType const *__restrict t,
               int explicit_case) {
 int wid;
 assert(vsize >= 1);

 /* Special case: Explicit cast to void. */
 if (DCCTYPE_ISBASIC(t->t_type,DCCTYPE_VOID)) {
  /* Anything can be cast to void. */
  vbottom->sv_reg          = DCC_RC_CONST;
  vbottom->sv_reg2         = DCC_RC_CONST;
  vbottom->sv_flags        = DCC_SFLAG_RVALUE;
  vbottom->sv_const.it     = 0;
  vbottom->sv_sym          = NULL;
  DCCType_Quit(&vbottom->sv_ctype);
  vbottom->sv_ctype.t_type = t->t_type;
  vbottom->sv_ctype.t_base = NULL;
  return;
 }

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


/* Push a given stack value promoted to at least 32-bit onto the stack.
 * @return: The actual amount of bytes pushed. */
PRIVATE target_siz_t DCC_VSTACK_CALL
DCCStackValue_PushProm32(struct DCCStackValue *__restrict self) {
 static uint8_t const padding[] = {0,0,0};
 struct DCCMemLoc symaddr;
 target_siz_t result;
 assert(self);
 DCCStackValue_LoadLValue(self);
 DCCStackValue_FixBitfield(self);
 if (self->sv_flags&DCC_SFLAG_LVALUE) {
  result = DCCType_Sizeof(&self->sv_ctype,NULL,1);
  symaddr.ml_reg = self->sv_reg;
  symaddr.ml_off = self->sv_const.offset;
  symaddr.ml_sym = self->sv_sym;
#if DCC_TARGET_BYTEORDER == 1234
  if (result < 4) DCCDisp_VecPush(padding,4-result);
  DCCDisp_MemRevPush(&symaddr,result);
  if (result < 4) result = 4;
#else
  DCCDisp_MemPush(&symaddr,result);
  if (result < 4) { DCCDisp_VecPush(padding,4-result); result = 4; }
#endif
 } else if (self->sv_reg != DCC_RC_CONST) {
  DCCStackValue_FixRegOffset(self);
  /* Make sure that the register is at least 32-bit wide.
   * TODO: This operation should be merged with 'PushReg'! */
  result = 4;
  if (!(self->sv_reg&(DCC_RC_I3264))) {
   self->sv_reg = DCCVStack_CastReg(self->sv_reg,
                                    DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type),
                                    DCC_RC_I32);
  }
  DCCDisp_RegPush(self->sv_reg);
  if (self->sv_reg2) {
   if (!(self->sv_reg2&(DCC_RC_I3264))) {
    self->sv_reg2 = DCCVStack_CastReg(self->sv_reg2,
                                      DCCTYPE_ISUNSIGNED(self->sv_ctype.t_type),
                                      DCC_RC_I32);
   }
   DCCDisp_RegPush(self->sv_reg2);
   result = 8;
  }
 } else {
  /* TODO: 64-bit push. */
  symaddr.ml_sad.sa_off = self->sv_const.offset;
  symaddr.ml_sad.sa_sym = self->sv_sym;
  DCCDisp_CstPush(&symaddr.ml_sad,DCC_TARGET_SIZEOF_POINTER);
  result = DCC_TARGET_SIZEOF_POINTER;
 }
 return result;
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
 struct DCCType *pfunction_type,function_type;
 target_siz_t arg_size; uint32_t cc;
 assert(vsize >= (1+n_args));
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
 pfunction_type = &function->sv_ctype;
 if (DCCTYPE_GROUP(function->sv_ctype.t_type) == DCCTYPE_POINTER) {
  assert(pfunction_type->t_base);
  assert(pfunction_type->t_base->d_kind&DCC_DECLKIND_TYPE);
  pfunction_type = &pfunction_type->t_base->d_type;
 }
 if (DCCTYPE_GROUP(pfunction_type->t_type) != DCCTYPE_FUNCTION) {
  WARN(W_EXPECTED_FUNCTION_TYPE_FOR_CALL,pfunction_type);
  pfunction_type = NULL;
 }
 if (pfunction_type) {
  function_type = *pfunction_type;
  DCCDecl_XIncref(function_type.t_base);
  pfunction_type = &function_type;
 } else {
  function_type.t_type = 0;
  function_type.t_base = NULL;
 }

 cc = DCC_ATTRFLAG_CDECL;
 if (function_type.t_base &&
     function_type.t_base->d_attr
     ) cc = function_type.t_base->d_attr->a_flags&
            DCC_ATTRFLAG_MASK_CALLCONV;

 /* TODO: Calling conventions other than cdecl and stdcall! */
 /* TODO: __attribute__((regparm(...))) */
 arg_size = 0;
 while (vbottom != function) {
  assert(vbottom < function);
  vprom();
  /* TODO: Cast arguments to those specified by 'function'
   *      (unless it's an old-style, or variadic function). */
  arg_size += DCCStackValue_PushProm32(vbottom);
  vpop(1);
 }
 DCCVStack_KillAll(); /* Kill all temporary registers still in use. */
 DCCStackValue_Call(function); /* Generate the call instructions. */
 vpop(1); /* Pop the function. */


 if (arg_size && cc != DCC_ATTRFLAG_STDCALL) {
  struct DCCSymAddr temp;
  /* Reclaim stack memory. */
  temp.sa_off = (target_off_t)arg_size;
  temp.sa_sym = NULL;
  DCCDisp_CstBinReg('+',&temp,DCC_RR_XSP,0);
 }
 /* TODO: What about caller-allocated structure memory? */

 DCCVStack_PushReturn(pfunction_type);
 vbottom->sv_flags |= DCC_SFLAG_RVALUE;
 if (pfunction_type) DCCDecl_XDecref(function_type.t_base);
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
 DCCStackValue_Jcc(vbottom+1,vbottom,invert);
 vpop(1);
 vpop(1);
}
PUBLIC void DCC_VSTACK_CALL
DCCVStack_Jmp(void) {
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
 DCCStackValue_FixBitfield(vbottom);
 DCCStackValue_FixTest(vbottom);
 DCCStackValue_Subscript(vbottom,name);
}



DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_C */
