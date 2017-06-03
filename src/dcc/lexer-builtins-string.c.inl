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
#ifndef GUARD_DCC_LEXER_BUILTINS_STRING_C_INL
#define GUARD_DCC_LEXER_BUILTINS_STRING_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/vstack.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>

#include <string.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

/*  __builtin_memcpy, __builtin_memmove  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinMemcpy(void) {
 int may_overlap = TOK == KWD___builtin_memmove;
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 /* dst, src, size */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST &&
      !vbottom->sv_sym) {
  struct DCCMemLoc src_loc,dst_loc;
  target_siz_t copy_size = (target_siz_t)vbottom->sv_const.offset;
  int compiletime_overlap;
  if (!copy_size) {
   /* Special case: Empty copy/move. */
fix_stack:
   vpop(1);  /* dst, src */
   vpop(1);  /* dst */
   vrcopy(); /* ddst */
   return;
  }
  /* Generate a compile-time memcpy. */
  if (vbottom[2].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[2]);
  if (vbottom[1].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[1]);
  assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE));
  src_loc.ml_reg = vbottom[1].sv_reg;
  src_loc.ml_off = vbottom[1].sv_const.offset;
  src_loc.ml_sym = vbottom[1].sv_sym;
  dst_loc.ml_reg = vbottom[2].sv_reg;
  dst_loc.ml_off = vbottom[2].sv_const.offset;
  dst_loc.ml_sym = vbottom[2].sv_sym;
                                compiletime_overlap = DCCMemLoc_Contains(&src_loc,copy_size,&dst_loc);
  if (compiletime_overlap != 1) compiletime_overlap = DCCMemLoc_Contains(&dst_loc,copy_size,&src_loc);
  if (compiletime_overlap == 2) {
   /* Overlap could not be determined at compile-time. */
   if (may_overlap) goto call_extern; /*  */
  } else if (compiletime_overlap == 1) {
   if (DCCMemLoc_Equal(&src_loc,&dst_loc)) {
    WARN(W_BUILTIN_MEMCPY_POINTERS_ALWAYS_EQUAL);
    goto fix_stack;
   }
   /* The pointers _always_ overlap. */
   if (may_overlap) goto call_extern;
   WARN(W_BUILTIN_MEMCPY_POINTERS_ALWAYS_OVERLAP);
  } else if (may_overlap) {
   WARN(W_BUILTIN_MEMMOVE_POINTERS_NEVER_OVERLAP);
   goto call_extern;
  }
  assert(!may_overlap);
  /* Mark the destination pointer to copy-on-write. */
  vpop(1);  /* dst, src */
  vswap();  /* src, dst */
  vrcopy(); /* src, ddst */
  vswap();  /* ddst, src */
  /* Dispense compiler-generated memcpy() instructions. */
  DCCDisp_MemMovMem(&src_loc,copy_size,
                    &dst_loc,copy_size,1);
  vpop(1);  /* ddst */
 } else {
  struct DCCSym *funsym;
call_extern:
  funsym = DCCUnit_NewSyms(may_overlap ? "memmove" : "memcpy",DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_vpfun(funsym);
  else vpushv(); /* dst, src, size, memcpy */
  vlrot(4);      /* memcpy, dst, src, size */
  vcall(3);      /* ret */
 }
}


/*  __builtin_memset  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinMemset(void) {
 assert(TOK == KWD___builtin_memset);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID,0),vused();
 DCCType_CheckWritablePtr(&vbottom->sv_ctype);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_INT,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 /* Compile-time optimizations for known memset sizes. */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST &&
      !vbottom->sv_sym) {
  struct DCCMemLoc dst_loc;
  target_siz_t fill_size = (target_siz_t)vbottom->sv_const.offset;
  if (!fill_size) {
   /* Special case: Empty memset. */
   vpop(1);  /* dst, byt */
   vpop(1);  /* dst */
   vrcopy(); /* ddst */
   return;
  }
  if (vbottom[2].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[2]);
  if (vbottom[1].sv_flags&DCC_SFLAG_LVALUE || vbottom[1].sv_sym) DCCStackValue_Load(&vbottom[1]);
  assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_sym));
  dst_loc.ml_reg = vbottom[2].sv_reg;
  dst_loc.ml_off = vbottom[2].sv_const.offset;
  dst_loc.ml_sym = vbottom[2].sv_sym;
  vpop(1);  /* dst, byt */
  vswap();  /* byt, dst */
  vrcopy(); /* byt, ddst */
  vswap();  /* ddst, byt */
  if (vbottom->sv_reg == DCC_RC_CONST) {
   /* The filler byte is known at compile-time. */
   DCCDisp_BytMovMem(vbottom->sv_const.u8,fill_size,&dst_loc,fill_size,1);
  } else {
   /* The filler byte is known at runtime. */
   DCCDisp_ByrMovMem(vbottom->sv_reg,fill_size,&dst_loc,fill_size,1);
  }
  vpop(1);
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms("memset",DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_vpfun(funsym);
  else vpushv(); /* dst, byte, size, memset */
  vlrot(4);      /* memset, dst, byte, size */
  vcall(3);      /* ret */
 }
}


/*  __builtin_memcmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinMemcmp(void) {
 assert(TOK == KWD___builtin_memcmp);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_VOID|DCCTYPE_CONST,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 /* a, b, size */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST &&
      !vbottom->sv_sym) {
  struct DCCMemLoc a_loc,b_loc; rc_t resreg,tempreg;
  target_siz_t cmp_size = (target_siz_t)vbottom->sv_const.offset;
  if (!cmp_size) {
   /* Special case: Empty copy/move. */
fix_stack:
   vpop(1);               /* a, b */
   vpop(1);               /* a */
   vpop(1);               /* . */
   vpushi(DCCTYPE_INT,0); /* 0 */
   return;
  }
  /* Generate a compile-time memcmp. */
  if (vbottom[2].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[2]);
  if (vbottom[1].sv_flags&DCC_SFLAG_LVALUE) DCCStackValue_Load(&vbottom[1]);
  assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
  assert(!(vbottom[2].sv_flags&DCC_SFLAG_LVALUE));
  a_loc.ml_reg = vbottom[1].sv_reg;
  a_loc.ml_off = vbottom[1].sv_const.offset;
  a_loc.ml_sym = vbottom[1].sv_sym;
  b_loc.ml_reg = vbottom[2].sv_reg;
  b_loc.ml_off = vbottom[2].sv_const.offset;
  b_loc.ml_sym = vbottom[2].sv_sym;
  if (DCCMemLoc_Equal(&a_loc,&b_loc)) {
   WARN(W_BUILTIN_MEMCMP_POINTERS_ALWAYS_EQUAL);
   goto fix_stack;
  }
  vpop(1);  /* a, b */
  { /* Compile-time memcmp. */
   void const *data_a,*data_b;
   if ((data_a = DCCMemLoc_CompilerData(&a_loc,cmp_size)) != NULL &&
       (data_b = DCCMemLoc_CompilerDataUpdate(&b_loc,(void **)&data_a,cmp_size)) != NULL) {
    int result = memcmp(data_a,data_b,cmp_size);
    /* Clamp the result to -1, 0 or 1 */
         if (result < 0) result = -1;
    else if (result > 0) result = 1;
    vpop(1);
    vpop(1);
    vpushi(DCCTYPE_INT,result);
    return;
   }
  }

  /* Dispense compiler-generated memcpy() instructions. */
  DCCDisp_MemBinMem('?',&a_loc,cmp_size,
                        &b_loc,cmp_size,1);
  vpop(1); /* a */
  vpop(1); /* . */
  resreg = DCCVStack_GetReg(DCC_RC_IN(DCC_TARGET_SIZEOF_INT),1);
  /* Clear the result register. */
  { struct DCCSymAddr val = {0,NULL};
    DCCDisp_CstMovRegRaw(&val,resreg);
  }
  /* push the result register. */
  vpushr(resreg);
  vrval();
  /* Allocate a second temporary register.
   * NOTE: Only do so after we've pushed the result register,
   *       so-as to prevent the same register from being allocated. */
  tempreg = DCCVStack_GetReg(DCC_RC_IN(DCC_TARGET_SIZEOF_INT),1);
  { struct DCCSymAddr val = {-1,NULL};
    /* Store 0/1 in the result register when 'a > b'. */
    DCCDisp_SccReg(DCC_TEST_G,resreg&~(DCC_RC_I3264|DCC_RC_I16));
    /* Return -1 if 'a < b' */
    DCCDisp_CstMovRegRaw(&val,tempreg);
    DCCDisp_RegCMovReg(DCC_TEST_L,tempreg,resreg,1);
  }
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms("memcmp",DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_ifun(funsym);
  else vpushv(); /* a, b, size, memcmp */
  vlrot(4);      /* memcmp, a, b, size */
  vcall(3);      /* ret */
 }
 vwunused();
}


/*  __builtin_strlen  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinStrlen(void) {
 int nlen_mode;
 assert(TOK == KWD___builtin_strlen ||
        TOK == KWD___builtin_strnlen);
 nlen_mode = TOK == KWD___builtin_strnlen;
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_pt(DCCTYPE_CHAR|DCCTYPE_CONST,0),vused();
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
       vbottom->sv_reg == DCC_RC_CONST && vbottom->sv_sym &&
       DCCSym_ISDEFINED(vbottom->sv_sym)) {
  /* Special case: The address of the string is known at compile-time. */
  struct DCCSection *sec = DCCSym_SECTION(vbottom->sv_sym);
  uint8_t *section_begin,*section_end; target_ptr_t secmax;
  target_ptr_t secaddr = vbottom->sv_sym->sy_addr;
  if (sec == unit.u_curr) {
   section_begin = unit.u_tbuf.tb_begin;
   section_end = unit.u_tbuf.tb_max;
   if (section_end > unit.u_tbuf.tb_end)
       section_end = unit.u_tbuf.tb_end;
  } else {
   section_begin = sec->sc_text.tb_begin;
   section_end = sec->sc_text.tb_max;
   if (section_end > sec->sc_text.tb_end)
       section_end = sec->sc_text.tb_end;
  }
  secmax = (target_ptr_t)(section_end-section_begin);
  /* shouldn't happen, but make sure the string is really located inside the section. */
  if (secaddr >= secmax) secaddr = 0;
  else secaddr = (target_ptr_t)strnlen((char *)(section_begin+secaddr),
                                       (size_t)(secmax-secaddr));
  vpop(1); /* Push the actual string length. */
  vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,secaddr);
  if (nlen_mode) {
   if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
   vrcopy();         /* Prevent warnings... */
   DCCParse_Expr1(); /* clen, maxlen */
   vminmax('<');     /* min(clen,maxlen) */
   vrval();
  }
 } else if (nlen_mode) {
  struct DCCSym *funsym;
  if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
  DCCParse_Expr1(); /* str, maxlen */
  funsym = DCCUnit_NewSyms("strnlen",DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_szfun(funsym);
  else vpushv(); /* str, maxlen, strnlen */
  vlrot(3);      /* strnlen, str, maxlen */
  vcall(2);      /* ret */
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms("strlen",DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_szfun(funsym);
  else vpushv(); /* str, strlen */
  vswap();       /* strlen, str */
  vcall(1);      /* ret */
 }
 vwunused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_BUILTINS_STRING_C_INL */
