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
#ifndef GUARD_DCC_LEXER_BUILTINS_C_INL
#define GUARD_DCC_LEXER_BUILTINS_C_INL 1

#include <dcc/common.h>
#include <dcc/vstack.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/byteorder.h>
#include <dcc/gen.h>

#include <string.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN


/*  __builtin_typestr  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinTypeStr(void) {
 struct DCCType     query_type;
 struct DCCAttrDecl query_attr = DCCATTRDECL_INIT;
 struct TPPKeyword *query_name;
 struct TPPString *type_str;
 assert(TOK == KWD___builtin_typestr);
 YIELD();
 DCCParse_ParPairBegin();
 query_name = DCCParse_CType(&query_type,&query_attr);
 DCCAttrDecl_Quit(&query_attr);
 if (query_name) {
  type_str = DCCType_ToTPPString(&query_type,query_name);
 } else {
  struct DCCSym *name_sym;
  pushf();
  compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
  DCCParse_Expr1();
  name_sym = vbottom->sv_sym;
  type_str = DCCType_ToTPPString(&vbottom->sv_ctype,
                                 name_sym ? name_sym->sy_name : NULL);
  vpop(1);
  popf();
 }
 DCCType_Quit(&query_type);
 assert(type_str),
 DCCVStack_PushStr(type_str->s_text,type_str->s_size);
 TPPString_Decref(type_str);
 DCCParse_ParPairEnd();
}


/*  __builtin_constant_p  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinConstantP(void) {
 int is_constant = 1;
 assert(TOK == KWD___builtin_constant_p);
 YIELD();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_ParPairBegin();
 while (DCCParse_IsExpr()) {
  DCCParse_Expr1();
  is_constant &= DCCStackValue_ISCONST(vbottom);
  vpop(1);
 }
 DCCParse_ParPairEnd();
 popf();
 vpushi(DCCTYPE_BOOL,is_constant);
}


/*  __builtin_choose_expr  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinChooseExpr(void) {
 int is_true;
 assert(TOK == KWD___builtin_choose_expr);
 YIELD();
 DCCParse_ParPairBegin();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr1();
 if (visconst_bool()) {
  is_true = vgtconst_bool();
 } else {
  WARN(W_BUILTIN_CHOOSE_EXPR_EXPECTED_CONSTANT_EXPRESSION);
  is_true = 0;
 }
 vpop(1);
 popf();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (is_true) DCCParse_Expr1(); else DCCParse_SkipExpr();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (is_true) DCCParse_SkipExpr(); else DCCParse_Expr1();
 DCCParse_ParPairEnd();
}



/*  __builtin_types_compatible_p  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinTypesCompatibleP(void) {
 struct DCCType ta,tb;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 assert(TOK == KWD___builtin_types_compatible_p);
 YIELD();
 DCCParse_ParPairBegin();
 if (!DCCParse_CType(&ta,&attr)) WARN(W_EXPECTED_TYPE_AFTER_BUILTIN_TYPES_COMPATIBLE_P);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 if (!DCCParse_CType(&tb,&attr)) WARN(W_EXPECTED_TYPE_AFTER_BUILTIN_TYPES_COMPATIBLE_P);
 DCCParse_ParPairEnd();
 DCCAttrDecl_Quit(&attr);
 vpushi(DCCTYPE_BOOL,DCCType_IsCompatible(&ta,&tb,0));
 DCCType_Quit(&tb);
 DCCType_Quit(&ta);
}



/*  __builtin_unreachable, __builtin_trap, __builtin_breakpoint  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFence(void) {
 tok_t t = TOK;
 YIELD();
 DCCParse_ParPairBegin();
 DCCDisp_Fence(t);
 DCCParse_ParPairEnd();
 vpushv();
}




/*  __builtin_alloca */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAlloca(void) {
 /* Allocate stack memory. */
 assert(TOK == KWD___builtin_alloca);
 DCCParse_WarnAllocaInLoop();
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 DCCParse_ParPairEnd();
 vpushxr(DCC_RR_XSP); /* x, %esp */
 vswap();             /* %esp, x */
 vgen2('-');          /* %esp */
 vpop(1);             /* Force apply disposition. */
 vpushxr(DCC_RR_XSP); /* Push the ESP register again. */
 vcast_pt(DCCTYPE_VOID,1);
}



/*  __builtin_alloca_with_align */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAllocaWithAlign(void) {
 /* Allocate stack memory. */
 assert(TOK == KWD___builtin_alloca_with_align);
 DCCParse_WarnAllocaInLoop();
 YIELD();
 /* generate code equivalent to:
  * >> %esp -= s;
  * >> %esp &= -a;
  * >> return %esp
  */
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vpushxr(DCC_RR_XSP); /* s, %esp */
 vswap();             /* %esp, s */
 vgen2('-');          /* %esp-=s */
 vpop(1);             /* . */
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1();
 vrcopy();            /* na=a   (Make sure alignment is just a copy of itself) */
 /* Prevent warnings about using '-' with an unsigned operand. */
 if (DCCTYPE_ISUNSIGNED(vbottom->sv_ctype.t_type))
     vbottom->sv_ctype.t_type &= ~(DCCTYPE_UNSIGNED);
 vgen1('-');          /* na=-na (Same as 'na = ~(na-1)') */
 vpushxr(DCC_RR_XSP); /* na, %esp */
 vswap();             /* %esp, na */
 vgen2('&');          /* %esp &= na */
 vcast_pt(DCCTYPE_VOID,1);
 DCCParse_ParPairEnd();
}



/*  __builtin_assume  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAssume(void) {
 assert(TOK == KWD___builtin_assume ||
        TOK == KWD___assume);
 YIELD();
 pushf();
 DCCParse_ParPairBegin();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr1(),vused();
 DCCParse_ParPairEnd();
 popf();
 /* If a compile-time assumption fails, it means
  * this code location can't possibly be reached. */
 if (visconst_bool() && !vgtconst_bool()) {
  if (!(compiler.c_flags&DCC_COMPILER_FLAG_DEAD) &&
        compiler.c_scope.s_kind == DCC_SCOPEKIND_FUNC) {
   /* Warn if this failed assumption can't be avoided. */
   WARN(W_FAILED_ASSUMPTION_UNAVOIDABLE);
  }
  DCCDisp_Fence(DCC_FENCE_UNREACHABLE);
 }
 /* xxx: maybe do something else with 'vbottom?' */
 vpop(1);
 vpushv();
}



/*  __builtin_expect  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinExpect(void) {
 assert(TOK == KWD___builtin_expect);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 pushf();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr1();
 vpop(1); /* Simply ignore assumptions. */
 popf();
 DCCParse_ParPairEnd();
 vrval();
}



/*  __builtin_FILE  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFILE(void) {
 char const *p; size_t s;
 assert(TOK == KWD___builtin_FUNCTION);
 p = TPPLexer_FILE(&s);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_ParPairEnd();
 DCCVStack_PushStr(p,s);
}



/*  __builtin_LINE  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinLINE(void) {
 int line;
 assert(TOK == KWD___builtin_LINE);
 line = TPPLexer_LINE();
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_ParPairEnd();
 vpushi(DCCTYPE_INT,line);
}



/*  __builtin_FUNCTION  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinFUNCTION(void) {
 struct TPPKeyword const *function_name;
 assert(TOK == KWD___builtin_FUNCTION);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_ParPairEnd();
 function_name = compiler.c_fun
  ? compiler.c_fun->d_name
  : &TPPKeyword_Empty;
 assert(function_name);
 DCCVStack_PushStr(function_name->k_name,
                   function_name->k_size);
}



/*  __builtin_bitfield  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinBitField(void) {
 int_t bit_index,bit_size;
 assert(TOK == KWD___builtin_bitfield);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 /* bit-wise integral access. */
 bit_index = DCCParse_CExpr(1),bit_size = 1;
 if (TOK == ',') { YIELD(); bit_size = DCCParse_CExpr(1); }
 DCCParse_ParPairEnd();
 vbitfld((uint8_t)bit_index,(uint8_t)bit_size);
}




/*  __builtin_offsetof  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinOffsetof(void) {
 struct DCCType type;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 assert(TOK == KWD___builtin_offsetof);
 YIELD();
 /* This builtin is only provided for compatibility with gcc.
  * DCC already implicitly supports the dump approach to such
  * functionality (which can also already be resolved at compile-time):
  * >> #define offsetof(s,m)  (__SIZE_TYPE__)&((s *)0)->m
  */
 DCCParse_ParPairBegin();
 DCCParse_CTypeOnly(&type,&attr);
 DCCAttrDecl_Quit(&attr);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCType_MkPointer(&type);
 vpushc(&type,0);
 DCCType_Quit(&type);
 vgen1('*');
 /* Really hacky way to get the suffix
  * parser to start by reading a '.' */
 assert(TOKEN.t_begin >= TOKEN.t_file->f_begin);
 assert(TOKEN.t_begin <= TOKEN.t_file->f_end);
 TOKEN.t_file->f_pos = TOKEN.t_begin;
 TOKEN.t_id = '.';

 /* Parse unary suffix expressions. */
 DCCParse_ExprUnarySuffix();
 vgen1('&');
 /* Cast the result to 'size_t'. */
 vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
 DCCParse_ParPairEnd();
}




/*  __builtin_assume_aligned  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinAssumeAligned(void) {
 assert(TOK == KWD___builtin_assume_aligned);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 vcast_pt(DCCTYPE_VOID,0);
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 /* Discard all other arguments. */
 DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
}




#define VA_SIZE(x) (((x)+(DCC_TARGET_VA_ALIGN-1)) & ~(DCC_TARGET_VA_ALIGN-1))

PRIVATE target_off_t dcc_va_off(void) {
 target_off_t result = DCC_TARGET_SIZEOF_POINTER*2;
 if (compiler.c_fun) {
  struct DCCDecl *funty_decl = compiler.c_fun->d_type.t_base;
  if (funty_decl &&
     (funty_decl->d_kind == DCC_DECLKIND_FUNCTION ||
      funty_decl->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
   if (!(funty_decl->d_flag&DCC_DECLFLAG_VARIADIC))
         WARN(W_BUILTIN_VA_START_NO_VARARGS);
   if (funty_decl->d_tdecl.td_size) {
    struct DCCStructField *last_field;
    if (funty_decl->d_flag&DCC_DECLFLAG_VARIADICLAST) {
     if (funty_decl->d_tdecl.td_size >= 2) {
      /* Add the Alignment offset of the last argument. */
      last_field = &funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-2];
      assert(last_field->sf_decl);
      result += VA_SIZE(DCCType_Sizeof(&last_field->sf_decl->d_type,NULL,0));
      result += last_field->sf_off;
     }
    } else {
     last_field = &funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-1];
     assert(last_field->sf_decl);
     result   += last_field->sf_off;
     result   += VA_SIZE(DCCType_Sizeof(&last_field->sf_decl->d_type,NULL,0));
    }
   }
   if ((funty_decl->d_attr) &&
       (funty_decl->d_attr->a_specs&DCC_ATTRSPEC_NAKED))
        result -= DCC_TARGET_SIZEOF_POINTER;
  }
 }
 return result;
}
PRIVATE rc_t dcc_va_base(void) {
 rc_t result = DCC_RR_XBP;
 if (compiler.c_fun) {
  struct DCCDecl *funty_decl = compiler.c_fun->d_type.t_base;
  if (funty_decl->d_attr &&
     (funty_decl->d_attr->a_specs&DCC_ATTRSPEC_NAKED))
      result = DCC_RR_XSP;
 }
 return result;
}
PRIVATE target_off_t dcc_va_start(void) {
 target_off_t result = DCC_TARGET_SIZEOF_POINTER*2;
 if (compiler.c_fun) {
  struct DCCDecl *funty_decl = compiler.c_fun->d_type.t_base;
  if (funty_decl &&
     (funty_decl->d_kind == DCC_DECLKIND_FUNCTION ||
      funty_decl->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
   if (!(funty_decl->d_flag&DCC_DECLFLAG_VARIADIC))
         WARN(W_BUILTIN_VA_START_NO_VARARGS);
   if (funty_decl->d_tdecl.td_size) {
    if (funty_decl->d_flag&DCC_DECLFLAG_VARIADICLAST) {
     if (funty_decl->d_tdecl.td_size >= 2) {
      result += funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-2].sf_off;
     }
    } else {
     result += funty_decl->d_tdecl.td_fieldv[funty_decl->d_tdecl.td_size-1].sf_off;
    }
   }
   if ((funty_decl->d_attr) &&
       (funty_decl->d_attr->a_specs&DCC_ATTRSPEC_NAKED))
        result -= DCC_TARGET_SIZEOF_POINTER;
  }
 }
 return result;
}

/*  __builtin_va_start  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaStart(void) {
 target_siz_t alignment;
 assert(TOK == KWD___builtin_va_start);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') {
  /* Load the default location for the va-argument. */
  struct DCCStackValue vararg;
  WARN((compiler.c_fun && compiler.c_fun->d_type.t_base &&
        compiler.c_fun->d_type.t_base->d_kind == DCC_DECLKIND_OLDFUNCTION)
        ? W_BUILTIN_VA_START_MISSING_SECOND_OLDSTYLE
        : W_BUILTIN_VA_START_MISSING_SECOND_ARGUMENT);
  vararg.sv_ctype.t_type = DCCTYPE_VOID;
  vararg.sv_ctype.t_base = NULL;
  vararg.sv_flags        = DCC_SFLAG_LVALUE;
  vararg.sv_reg          = dcc_va_base();
  vararg.sv_reg2         = DCC_RC_CONST;
  vararg.sv_const.it     = (int_t)dcc_va_off();
  vararg.sv_sym          = NULL;
  vpush(&vararg);
 } else {
  target_off_t default_off;
  YIELD();
  DCCParse_Expr1();
  /* Check if the va-argument is the last element before the '...' */
  if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) ||
       (vbottom->sv_sym != NULL) ||
       (vbottom->sv_reg != dcc_va_base()) ||
       ((default_off     = dcc_va_start()) != 0 &&
        (vbottom->sv_const.offset != default_off))) {
   WARN(W_BUILTIN_VA_START_NOT_LAST_ARG);
  }
 }
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 alignment = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
 alignment = VA_SIZE(alignment);
 /* ap = (va_list)(((uintptr_t)&start)+alignment) */
 vgen1('&');
 vrcopy();
 vcast_t(DCCTYPE_UNSIGNED|DCCTYPE_INTPTR,1);
 vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,alignment);
 vgen2('+');
 /* Store the calculated address in 'ap'. */
 vcast_pt(DCCTYPE_CHAR,1);
 vstore(0);
 vpop(1);
 vpushv();
}

/*  __builtin_va_copy  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaCopy(void) {
 assert(TOK == KWD___builtin_va_copy);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 vstore(0);
 vpop(1);
 vpushv();
}

/*  __builtin_va_end  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaEnd(void) {
 assert(TOK == KWD___builtin_va_end);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 if (!(compiler.c_flags&DCC_COMPILER_FLAG_NOCLEANUP)) {
  vpushi(DCCTYPE_INT,0);
  vstore(0);
 }
 vpop(1);
 vpushv();
}

/*  __builtin_va_arg  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinVaArg(void) {
 struct DCCType va_type;
 struct DCCAttrDecl va_attr = DCCATTRDECL_INIT;
 target_siz_t va_size;
 assert(TOK == KWD___builtin_va_arg);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_CTypeOnly(&va_type,&va_attr);
 DCCAttrDecl_Quit(&va_attr);
 va_size = VA_SIZE(DCCType_Sizeof(&va_type,NULL,1));
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 vdup(1);           /* ap, dap */
 vswap();           /* dap, ap */
 vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,va_size); /* dap, ap, siz */
 vgen2('+');        /* dap, ap+=siz */
 vpop(1);           /* dap */
 DCCType_MkPointer(&va_type);
 vcast(&va_type,1); /* (T *)dap */
 DCCType_Quit(&va_type);
 vgen1('*');        /* *(T *)dap */
 vrval();           /* Cause warnings when the caller tries to assign to the VA-argument. */
}


/* Layout of a target jump buffer (the last field is used for EIP) */
static rc_t const target_jmpbuf[] = {
 DCC_RR_XBX,DCC_RR_XDI,DCC_RR_XSI,
 DCC_RR_XSP,DCC_RR_XBP,DCC_RC_CONST
};
/* Signal-value register used for setjmp/longjmp */
#define TARGET_LONGJMP_REGISTER   (DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_EAX)


/*  __builtin_setjmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinSetJmp(void) {
 struct DCCMemLoc jmpbuf_loc;
 struct DCCSymAddr eip; rc_t const *iter;
 assert(TOK == KWD___builtin_setjmp);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
 DCCParse_ParPairEnd();
 DCCStackValue_LoadLValue(vbottom);
 DCCType_CheckWritable(&vbottom->sv_ctype);
 DCCVStack_KillAll(1); /* Kill all registers. */
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 if (DCCType_Sizeof(&vbottom->sv_ctype,NULL,1) != DCC_TARGET_SIZEOF_JMP_BUF)
  WARN(W_BUILTIN_JMPBUF_HAS_INCORRECT_SIZE,&vbottom->sv_ctype);
 jmpbuf_loc.ml_reg = vbottom->sv_reg;
 jmpbuf_loc.ml_off = vbottom->sv_const.offset;
 jmpbuf_loc.ml_sym = vbottom->sv_sym;
 for (iter = target_jmpbuf; *iter; ++iter) {
  DCCDisp_RegMovMem(*iter,&jmpbuf_loc);
  jmpbuf_loc.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 }
 eip.sa_sym = DCCUnit_AllocSym();
 if unlikely(!eip.sa_sym) return;
 eip.sa_off = 0;
 /* Save EIP */
 DCCDisp_CstMovMem(&eip,&jmpbuf_loc,DCC_TARGET_SIZEOF_POINTER);
 vpop(1); /* pop the jump buffer. */
 /* Clear the longjmp register. */
 DCCDisp_IntMovReg(0,TARGET_LONGJMP_REGISTER);
 /* This is where a longjmp will jmp to! */
 t_defsym(eip.sa_sym);
 /* Push the longjmp register. */
 vpushr(TARGET_LONGJMP_REGISTER);
 vrval();
 //vwunused(); /* Well... You should, but you don't ~really~ have to... */
}

/*  __builtin_longjmp  */
LEXDECL void DCC_PARSE_CALL
DCCParse_BuiltinLongJmp(void) {
 struct DCCMemLoc jmpbuf_loc;
 struct DCCMemLoc restore_last_loc;
 rc_t const *iter;
 assert(TOK == KWD___builtin_longjmp);
 YIELD();
 /* NOTE: No need to kill all registers in here, because this function function
  *       will simply mark the control flow as dead, meaning that all registers
  *       still alive when one gets here are, will be meaningless afterwards! */
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vused();
 DCCStackValue_LoadLValue(&vbottom[1]);
 if (!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(&vbottom[1]);
 if (DCCType_Sizeof(&vbottom[1].sv_ctype,NULL,1) != DCC_TARGET_SIZEOF_JMP_BUF)
  WARN(W_BUILTIN_JMPBUF_HAS_INCORRECT_SIZE,&vbottom[1].sv_ctype);
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
fix_zero_signal:
  /* Load the signal value into the longjmp register. */
  DCCStackValue_LoadExplicit(vbottom,TARGET_LONGJMP_REGISTER);
  /* Load '1' into '%ECX' (used below in conditional mov) */
  DCCDisp_IntMovReg(1,DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_ECX);
  /* Test the register for being ZERO(0). */
  DCCDisp_RegBinReg('t',TARGET_LONGJMP_REGISTER,
                        TARGET_LONGJMP_REGISTER,1);
  /* If the longjmp register was ZERO(0), mov '%ECX' (which is ONE(1)) into it. */
  DCCDisp_RegCMovReg(DCC_TEST_Z,DCC_RC_I32|DCC_RC_I16|DCC_RC_I8|DCC_ASMREG_ECX,
                                TARGET_LONGJMP_REGISTER,1);
 } else if (vbottom->sv_sym) {
  /* non-lvalue symbol means that a non-zero integer is added to the
   * signal, thus implying that the signal should never be ZERO(0). */
 } else if (vbottom->sv_reg != DCC_RC_CONST) {
  /* The value is dependent on a register. - Must make that that register isn't ZERO(0). */
  goto fix_zero_signal;
 } else if (vbottom->sv_const.it == 0) {
  /* symbol-independent constant expression is ZERO(0). */
  WARN(W_BUILTIN_LONGJMP_VALZERO_IS_ONE);
  vbottom->sv_const.it = 1;
 }
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 vpushr(TARGET_LONGJMP_REGISTER); /* buf, val, %REG */
 vswap();                         /* buf, %REG, val */
 vstore(1);                       /* buf, %REG=val */
 vpop(1);                         /* buf */
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 jmpbuf_loc.ml_reg = vbottom->sv_reg;
 jmpbuf_loc.ml_off = vbottom->sv_const.offset;
 jmpbuf_loc.ml_sym = vbottom->sv_sym;
 vpop(1);                         /* . */
 restore_last_loc.ml_reg = DCC_RC_CONST;
 /* Restore all regular registers (except for a buffer-location-dependent one). */
 for (iter = target_jmpbuf; *iter; ++iter) {
  if ((jmpbuf_loc.ml_reg&DCC_RC_I) &&
     (*iter&7) == (jmpbuf_loc.ml_reg&7)) {
   /* This register must be restored last! */
   restore_last_loc = jmpbuf_loc;
  } else {
   /* Restore this register. */
   DCCDisp_MemMovReg(&jmpbuf_loc,*iter);
  }
  jmpbuf_loc.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 }
 if (restore_last_loc.ml_reg != DCC_RC_CONST) {
  /* When restoring a last register, we know that the completion of
   * that action will make it impossible to access the jump buffer again.
   * Yet because it still contains the EIP of where we're supposed to jump to,
   * we must load that address into a temporary register (which must be an IA-32 clobber)
   * >> With '%EAX' out of the question (already used as jump
   *    signal value), that leaves us either '%ECX' or '%EDX' (we choose %ECX here) */
  DCCDisp_MemMovReg(&jmpbuf_loc,DCC_RR_XCX);
  DCCDisp_MemMovReg(&restore_last_loc,jmpbuf_loc.ml_reg);
  DCCDisp_RegJmp(DCC_RR_XCX);
 } else {
  /* The jump buffer memory location does not depend on a register.
   * With that in mind, we can simply to a direct jmp to
   * the stored EIP, to which 'jmpbuf_loc' already points. */
  DCCDisp_MemJmp(&jmpbuf_loc,DCC_TARGET_SIZEOF_POINTER);
 }
 /* mark the current branch as dead. */
 compiler.c_flags |= (DCC_COMPILER_FLAG_NOCGEN|
                      DCC_COMPILER_FLAG_DEAD);
 vpushv(); /* void */
}

#define ASCII_ISCNTRL(ch)  ((uint8_t)(ch) <= 0x1f || (uint8_t)(ch) == 0x7f)
#define ASCII_ISBLANK(ch)  ((uint8_t)(ch) == 0x09 || (uint8_t)(ch) == 0x20)
#define ASCII_ISSPACE(ch)  (((uint8_t)(ch) >= 0x09 && (uint8_t)(ch) <= 0x0d) || (uint8_t)(ch) == 0x20)
#define ASCII_ISUPPER(ch)  ((uint8_t)(ch) >= 0x41 && (uint8_t)(ch) <= 0x5a)
#define ASCII_ISLOWER(ch)  ((uint8_t)(ch) >= 0x61 && (uint8_t)(ch) <= 0x7a)
#define ASCII_ISALPHA(ch)  (ASCII_ISUPPER(ch) || ASCII_ISLOWER(ch))
#define ASCII_ISDIGIT(ch)  ((uint8_t)(ch) >= 0x30 && (uint8_t)(ch) <= 0x39)
#define ASCII_ISXDIGIT(ch) (ASCII_ISDIGIT(ch) || \
                           ((uint8_t)(ch) >= 0x41 && (uint8_t)(ch) <= 0x46) || \
                           ((uint8_t)(ch) >= 0x61 && (uint8_t)(ch) <= 0x66))
#define ASCII_ISALNUM(ch)  (ASCII_ISUPPER(ch) || ASCII_ISLOWER(ch) || ASCII_ISDIGIT(ch))
#define ASCII_ISPUNCT(ch)  (((uint8_t)(ch) >= 0x21 && (uint8_t)(ch) <= 0x2f) || \
                            ((uint8_t)(ch) >= 0x3a && (uint8_t)(ch) <= 0x40) || \
                            ((uint8_t)(ch) >= 0x5b && (uint8_t)(ch) <= 0x60) || \
                            ((uint8_t)(ch) >= 0x7b && (uint8_t)(ch) <= 0x7e))
#define ASCII_ISGRAPH(ch)  ((uint8_t)(ch) >= 0x21 && (uint8_t)(ch) <= 0x7e)
#define ASCII_ISPRINT(ch)  ((uint8_t)(ch) >= 0x20 && (uint8_t)(ch) <= 0x7e)
#define ASCII_TOLOWER(ch)  (ASCII_ISUPPER(ch) ? ((uint8_t)(ch)+0x20) : (uint8_t)(ch))
#define ASCII_TOUPPER(ch)  (ASCII_ISLOWER(ch) ? ((uint8_t)(ch)-0x20) : (uint8_t)(ch))

PRIVATE int ascii_iscntrl(int ch) { return ASCII_ISCNTRL(ch); }
PRIVATE int ascii_isblank(int ch) { return ASCII_ISBLANK(ch); }
PRIVATE int ascii_isspace(int ch) { return ASCII_ISSPACE(ch); }
PRIVATE int ascii_isupper(int ch) { return ASCII_ISUPPER(ch); }
PRIVATE int ascii_islower(int ch) { return ASCII_ISLOWER(ch); }
PRIVATE int ascii_isalpha(int ch) { return ASCII_ISALPHA(ch); }
PRIVATE int ascii_isdigit(int ch) { return ASCII_ISDIGIT(ch); }
PRIVATE int ascii_isxdigit(int ch) { return ASCII_ISXDIGIT(ch); }
PRIVATE int ascii_isalnum(int ch) { return ASCII_ISALNUM(ch); }
PRIVATE int ascii_ispunct(int ch) { return ASCII_ISPUNCT(ch); }
PRIVATE int ascii_isgraph(int ch) { return ASCII_ISGRAPH(ch); }
PRIVATE int ascii_isprint(int ch) { return ASCII_ISPRINT(ch); }
PRIVATE int ascii_tolower(int ch) { return ASCII_TOLOWER(ch); }
PRIVATE int ascii_toupper(int ch) { return ASCII_TOUPPER(ch); }

struct ctype_callback {
 char const *name;
 int       (*func)(int);
};

PRIVATE struct ctype_callback const builtin_ctype[] = {
 /* __builtin_iscntrl  */{"iscntrl", &ascii_iscntrl},
 /* __builtin_isblank  */{"isblank", &ascii_isblank},
 /* __builtin_isspace  */{"isspace", &ascii_isspace},
 /* __builtin_isupper  */{"isupper", &ascii_isupper},
 /* __builtin_islower  */{"islower", &ascii_islower},
 /* __builtin_isalpha  */{"isalpha", &ascii_isalpha},
 /* __builtin_isdigit  */{"isdigit", &ascii_isdigit},
 /* __builtin_isxdigit */{"isxdigit",&ascii_isxdigit},
 /* __builtin_isalnum  */{"isalnum", &ascii_isalnum},
 /* __builtin_ispunct  */{"ispunct", &ascii_ispunct},
 /* __builtin_isgraph  */{"isgraph", &ascii_isgraph},
 /* __builtin_isprint  */{"isprint", &ascii_isprint},
 /* __builtin_tolower  */{"tolower", &ascii_tolower},
 /* __builtin_toupper  */{"toupper", &ascii_toupper},
};


LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinCType(void) {
 struct ctype_callback const *spec;
 /* int __builtin_isXXX(int ch); */
 assert(TOK >= KWD___builtin_iscntrl &&
        TOK <= KWD___builtin_toupper);
 spec = &builtin_ctype[TOK-KWD___builtin_iscntrl];
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vcast_t(DCCTYPE_INT,0),vused();
 DCCParse_ParPairEnd();
 if (visconst_int()) {
  /* Calculate at compile-time. */
  vbottom->sv_const.it = (int_t)(*spec->func)((int)vgtconst_int());
  vbottom->sv_flags   |= DCC_SFLAG_RVALUE;
 } else {
  /* Generate a function call. */
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms(spec->name,DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_ifun(funsym);
  else vpushv(); /* ch, ... */
  vswap();       /* ..., ch */
  vcall(1);      /* ret */
 }
 vwunused();
}






/*  __sync_val_compare_and_swap  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncCompareAndSwap(void) {
 struct DCCType *cas_type; struct DCCMemLoc cas_mem;
 target_siz_t cas_size; rc_t reg_class; int bool_mode;
 assert(TOK == KWD___sync_bool_compare_and_swap ||
        TOK == KWD___sync_val_compare_and_swap);
 bool_mode = (TOK == KWD___sync_bool_compare_and_swap);
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
 vgen1('*'); /* Dereference first argument. */
 DCCStackValue_LoadLValue(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 cas_type = &vbottom->sv_ctype;
 cas_mem.ml_reg = vbottom->sv_reg;
 cas_mem.ml_off = vbottom->sv_const.offset;
 cas_mem.ml_sym = vbottom->sv_sym;
 cas_size = DCCType_Sizeof(cas_type,NULL,1);
 if (cas_size != 1 && cas_size != 2 && cas_size != 4)
  WARN(W_UNSUPPORTED_CAS_SIZE,cas_type),cas_size = 4;
 reg_class = cas_size == 1 ? DCC_RC_I8 :
             cas_size == 2 ? DCC_RC_I16|DCC_RC_I8 :
                             DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast(cas_type,0),vused();
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast(cas_type,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCStackValue_LoadExplicit(&vbottom[1],(rc_t)(DCC_ASMREG_EAX|reg_class));
 DCCStackValue_LoadClass(&vbottom[0],reg_class,1);
 assert(!(vbottom[1].sv_flags&DCC_SFLAG_LVALUE));
 assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
 assert(vbottom[1].sv_reg == (DCC_ASMREG_EAX|reg_class));
 assert(vbottom[0].sv_reg != DCC_RC_CONST);
 /* *dst, EAX, reg */
 DCCDisp_AtomicRegCmpXchMemAX(vbottom[0].sv_reg,&cas_mem);
 if (bool_mode) {
  struct DCCStackValue test;
  test.sv_ctype.t_type = DCCTYPE_BOOL;
  test.sv_ctype.t_base = NULL;
  test.sv_flags        = DCC_SFLAG_MKTEST(DCC_TEST_E);
  test.sv_reg          = DCC_RC_CONST;
  test.sv_reg2         = DCC_RC_CONST;
  test.sv_sym          = NULL;
  test.sv_const.it     = 0;
  vpush(&test); /* *dst, EAX, reg, test */
  vlrot(4);     /* test, *dst, EAX, reg */
 }
 vpop(1); /* [test], *dst, EAX */
 vswap(); /* [test], EAX, *dst */
 vpop(1); /* [test], EAX */
 if (bool_mode) vpop(1); /* test */
 vrval(); /* rEAX/rtest */
}



/*  __sync_synchronize  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncSynchronize(void) {
 assert(TOK == KWD___sync_synchronize);
 YIELD();
 DCCParse_ParPairBegin();
 if (DCCParse_IsExpr()) DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCDisp_Fence(DCC_FENCE_MFENCE);
 vpushv();
}



/*  __sync_... */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncBinary(void) {
 struct DCCType *cas_type; struct DCCMemLoc cas_mem;
 target_siz_t cas_size; rc_t reg_class; tok_t opcode;
 int fmode = DCCDISP_ATOMICBIN_FETCHNEVER;
 switch (TOK) {
#define TRIPLE(op,fop,opf,id) \
 case fop: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE;\
 if (DCC_MACRO_FALSE) { case opf: fmode = DCCDISP_ATOMICBIN_FETCHAFTER; }\
 case op: opcode = id; break
 default:
 TRIPLE(KWD___sync_add,  KWD___sync_fetch_and_add,  KWD___sync_add_and_fetch,  '+');
 TRIPLE(KWD___sync_sub,  KWD___sync_fetch_and_sub,  KWD___sync_sub_and_fetch,  '-');
 TRIPLE(KWD___sync_or,   KWD___sync_fetch_and_or,   KWD___sync_or_and_fetch,   '|');
 TRIPLE(KWD___sync_and,  KWD___sync_fetch_and_and,  KWD___sync_and_and_fetch,  '&');
 TRIPLE(KWD___sync_xor,  KWD___sync_fetch_and_xor,  KWD___sync_xor_and_fetch,  '^');
 TRIPLE(KWD___sync_nand, KWD___sync_fetch_and_nand, KWD___sync_nand_and_fetch, '~');
 TRIPLE(KWD___sync_store,KWD___sync_fetch_and_store,KWD___sync_store_and_fetch,'=');
#undef TRIPLE
 case KWD___sync_lock_test_and_set: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE; opcode = '='; break;
 }
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
 vgen1('*'); /* Dereference first argument. */
 DCCStackValue_LoadLValue(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 cas_type = &vbottom->sv_ctype;
 cas_mem.ml_reg = vbottom->sv_reg;
 cas_mem.ml_off = vbottom->sv_const.offset;
 cas_mem.ml_sym = vbottom->sv_sym;
 cas_size = DCCType_Sizeof(cas_type,NULL,1);
 if (cas_size != 1 && cas_size != 2 && cas_size != 4)
  WARN(W_UNSUPPORTED_CAS_SIZE,cas_type),cas_size = 4;
 reg_class = cas_size == 1 ? DCC_RC_I8 :
             cas_size == 2 ? DCC_RC_I16|DCC_RC_I8 :
                             DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 if (TOK != ',') WARN(W_EXPECTED_COMMA); else YIELD();
 DCCParse_Expr1(),vcast(cas_type,0),vused();
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 DCCStackValue_LoadClass(&vbottom[0],reg_class,1);
 assert(!(vbottom[0].sv_flags&DCC_SFLAG_LVALUE));
 assert(vbottom[0].sv_reg != DCC_RC_CONST);
 DCCDisp_AtomicRegBinMem(opcode,fmode,vbottom[0].sv_reg,&cas_mem);
 vswap(); /* value, ptr */
 vpop(1); /* value */
 vrval(); /* rvalue */
 if (fmode == DCCDISP_ATOMICBIN_FETCHNEVER) {
  vpop(1);  /* . */
  vpushv(); /* void */
 }
}




/*  __sync_lock_release  */
LEXPRIV void DCC_PARSE_CALL
DCCParse_SyncUnary(void) {
 struct DCCType *cas_type; struct DCCMemLoc cas_mem;
 target_siz_t cas_size; tok_t opcode; struct DCCStackValue result;
 int fmode = DCCDISP_ATOMICBIN_FETCHNEVER;
 switch (TOK) {
#define TRIPLE(op,fop,opf,id) \
 case fop: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE;\
 if (DCC_MACRO_FALSE) { case opf: fmode = DCCDISP_ATOMICBIN_FETCHAFTER; }\
 case op: opcode = id; break
 default:
 TRIPLE(KWD___sync_inc,KWD___sync_fetch_and_inc,KWD___sync_inc_and_fetch,TOK_INC);
 TRIPLE(KWD___sync_dec,KWD___sync_fetch_and_dec,KWD___sync_dec_and_fetch,TOK_DEC);
 TRIPLE(KWD___sync_neg,KWD___sync_fetch_and_neg,KWD___sync_neg_and_fetch,'-');
 TRIPLE(KWD___sync_not,KWD___sync_fetch_and_not,KWD___sync_not_and_fetch,'~');
#undef TRIPLE
 case KWD___sync_lock_release: fmode = DCCDISP_ATOMICBIN_FETCHBEFORE; opcode = '='; break;
 }
 YIELD();
 DCCParse_ParPairBegin();
 DCCParse_Expr1(),vused();
 vgen1('*'); /* Dereference first argument. */
 DCCStackValue_LoadLValue(vbottom);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE)) DCCStackValue_Kill(vbottom);
 assert(vbottom->sv_flags&DCC_SFLAG_LVALUE);
 cas_type = &vbottom->sv_ctype;
 cas_mem.ml_reg = vbottom->sv_reg;
 cas_mem.ml_off = vbottom->sv_const.offset;
 cas_mem.ml_sym = vbottom->sv_sym;
 cas_size = DCCType_Sizeof(cas_type,NULL,1);
 if (cas_size != 1 && cas_size != 2 && cas_size != 4)
  WARN(W_UNSUPPORTED_CAS_SIZE,cas_type),cas_size = 4;
 if (TOK == ',') YIELD(),DCCParse_ExprDiscard();
 DCCParse_ParPairEnd();
 result.sv_reg      = cas_size == 1 ? DCC_RC_I8 :
                      cas_size == 2 ? DCC_RC_I16|DCC_RC_I8 :
                                      DCC_RC_I32|DCC_RC_I16|DCC_RC_I8;
 result.sv_reg2     = DCC_RC_CONST;
 result.sv_const.it = 0;
 result.sv_sym      = NULL;
 result.sv_flags    = DCC_SFLAG_RVALUE;
 if (fmode != DCCDISP_ATOMICBIN_FETCHNEVER) {
  result.sv_reg = DCCVStack_GetReg(result.sv_reg,1);
  DCCType_InitCopy(&result.sv_ctype,cas_type);
 } else {
  result.sv_ctype.t_base = NULL;
  result.sv_ctype.t_type = DCCTYPE_VOID;
 }
 vpush(&result);
 if (opcode == '=') {
  /* Store '0' in the result register, which will be exchanged. */
  DCCDisp_IntMovReg(0,result.sv_reg);
  DCCDisp_AtomicRegBinMem(opcode,DCCDISP_ATOMICBIN_FETCHBEFORE,
                          result.sv_reg,&cas_mem);
 } else {
  DCCDisp_AtomicUnaryMem(opcode,fmode,result.sv_reg,&cas_mem);
  result.sv_reg = DCC_RC_CONST;
 }
 vswap(); /* result, ptr */
 vpop(1); /* result */
}




LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinNoop(void) {
 assert(TOK == KWD___builtin_noop ||
        TOK == KWD___noop);
 YIELD();
 pushf(); /* Parse an expression, but don't do anything with it. */
 DCCParse_ParPairBegin();
 compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
 DCCParse_Expr();
 vpop(1);
 DCCParse_ParPairEnd();
 popf();
 /* Return an integral constant ZERO(0). */
 vpushi(DCCTYPE_INT,0);
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "lexer-builtins-string.c.inl"
#include "lexer-builtins-util.c.inl"
#endif


#endif /* !GUARD_DCC_LEXER_BUILTINS_C_INL */
