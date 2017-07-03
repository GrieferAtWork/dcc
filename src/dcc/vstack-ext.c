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
#ifndef GUARD_DCC_VSTACK_EXT_C
#define GUARD_DCC_VSTACK_EXT_C 1
#define _GNU_SOURCE /* enable 'memrchr' */

#include <dcc/common.h>
#include <dcc/byteorder.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>
#include <dcc/linker.h>
#include <dcc/target-crt.h>
#include <dcc/target.h>
#include <dcc/type.h>
#include <dcc/unit.h>
#include <dcc/vstack.h>

DCC_DECL_BEGIN

INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_vfun(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_VOID,NULL};
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_vpfun(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_VOID,NULL};
 DCCType_MkPointer(&type);
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_cpfun(struct DCCSym *__restrict sym) {
 struct DCCType type;
 type.t_type = DCCTYPE_USERCHAR;
 type.t_base = NULL;
 DCCType_MkPointer(&type);
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_stdcall_vpfun(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_VOID,NULL};
 DCCType_MkPointer(&type);
 DCCType_MkOldFunc(&type);
 if (type.t_base) {
  struct DCCAttrDecl attr = DCCATTRDECL_INIT;
  attr.a_flags |= DCC_ATTRFLAG_CC_STDCALL;
  DCCDecl_SetAttr(type.t_base,&attr);
 }
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_ifun(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_INT,NULL};
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}
INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_szfun(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_SIZE|DCCTYPE_UNSIGNED,NULL};
 DCCType_MkOldFunc(&type);
 vpushst(&type,sym);
 DCCType_Quit(&type);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Alloca(void) {
 assert(vsize >= 1);
 if (linker.l_flags&DCC_LINKER_FLAG_GENDEBUG) {
  struct DCCSym *sym; int eax_mode;
#if DCC_TARGET_BIN == DCC_BINARY_PE
pe_alloca:
#endif
  eax_mode = 0;
  DCCStackValue_FixBitfield(vbottom);
  DCCStackValue_FixTest(vbottom);
  if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE))
        DCCStackValue_FixRegOffset(vbottom);
  if ((vbottom->sv_reg != DCC_RC_CONST &&
      (vbottom->sv_reg&DCC_RI_MASK) == DCC_ASMREG_EAX) ||
      !DCCVStack_GetRegInuse(DCC_RR_XAX)) {
   DCCStackValue_LoadExplicit(vbottom,DCC_RR_XAX);
   eax_mode = 1;
   sym = DCCUnit_NewSyms(DCC_TARGET_CRT_ALLOCA_EAX,DCC_SYMFLAG_HIDDEN);
  } else {
   sym = DCCUnit_NewSyms(DCC_TARGET_CRT_ALLOCA,DCC_SYMFLAG_HIDDEN);
  }
  sym ? DCCVStack_PushSym_stdcall_vpfun(sym) : vpushv();
  vswap();
  if (!eax_mode) vcall(1);
  else   vpop(1),vcall(0);
 } else {
#if DCC_TARGET_BIN == DCC_BINARY_PE
  /* Make sure to handle very large, or unknown-sized
   * requests with __pe_alloca on PE targets. */
  if (!visconst_int() ||
       vgtconst_int() >= DCC_TARGET_PAGESIZE)
       goto pe_alloca;
#endif
  vpushxr(DCC_RR_XSP); /* x, %esp */
  vswap();             /* %esp, x */
  vgen2('-');          /* %esp */
 }
 vpop(1);             /* Force apply disposition. */
 vpushxr(DCC_RR_XSP); /* Push the ESP register again. */
 vcast_pt(DCCTYPE_VOID,1);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_BSwap(void) {
 target_siz_t size;
 assert(vsize >= 1);
 DCCStackValue_LoadLValue(vbottom);
 DCCStackValue_Cow(vbottom);
 DCCStackValue_FixTest(vbottom);
 DCCStackValue_FixBitfield(vbottom);
 size = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
 if (DCCSTACKVALUE_ISCONST_XVAL(vbottom)) {
  if (vbottom->sv_sym) DCCStackValue_Load(vbottom);
  else { /* Constant optimizations. */
   switch (size) {
   case 1: return;
   case 2: vbottom->sv_const.it = (int_t)DCC_BSWAP16(vbottom->sv_const.u16); break;
   case 4: vbottom->sv_const.it = (int_t)DCC_BSWAP32(vbottom->sv_const.u32); break;
   case 8: vbottom->sv_const.it = (int_t)DCC_BSWAP64(vbottom->sv_const.u64); break;
   default: goto copy_elem;
   }
   return;
  }
 } else {
copy_elem:
  /* Create a copy of the argument. */
  vrcopy();
 }
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc loc;
  loc.ml_reg = vbottom->sv_reg;
  loc.ml_off = vbottom->sv_const.offset;
  loc.ml_sym = vbottom->sv_sym;
  DCCDisp_BSwapMem(&loc,size);
 } else {
  assert(vbottom->sv_reg);
  DCCDisp_BSwapReg(vbottom->sv_reg);
  if (vbottom->sv_reg2 != DCC_RC_CONST) {
   rc_t temp;
   /* Swap the second register. */
   DCCDisp_BSwapReg(vbottom->sv_reg2);
   /* Exchange the registers. */
   temp = vbottom->sv_reg2;
   vbottom->sv_reg2 = vbottom->sv_reg;
   vbottom->sv_reg = temp;
  }
 }
}






//////////////////////////////////////////////////////////////////////////
// 
//  BIT-SCANNER
// 
DCCFUN void DCC_VSTACK_CALL
DCCVStack_Scanner(tok_t mode) {
 target_siz_t size;
 rc_t result_register;
 assert(vsize >= 1);
 DCCStackValue_FixBitfield(vbottom);
 size = DCCType_Sizeof(&vbottom->sv_ctype,NULL,0);
 if (!(vbottom->sv_flags&DCC_SFLAG_LVALUE) &&
      (vbottom->sv_reg == DCC_RC_CONST)) {
  if (vbottom->sv_sym)
cstdef: DCCStackValue_Load(vbottom);
  else { /* Constant optimizations. */
   uint64_t iv,shift; int result;
   switch (size) {
   case 1: iv = (uint64_t)vbottom->sv_const.u8; break;
   case 2: iv = (uint64_t)vbottom->sv_const.u16; break;
   case 4: iv = (uint64_t)vbottom->sv_const.u32; break;
   case 8: iv = (uint64_t)vbottom->sv_const.u64; break;
   default: goto default_ffs;
   }
   if (mode == KWD___builtin_clz) {
    result = 0,shift = (uint64_t)1 << 63;
    while (!(iv&shift)) {
     /* Generate code and let the CPU decide on undefined behavior! */
     if (++result == 64) goto cstdef;
     shift >>= 1;
    }
    /* Adjust for zero-padding on constants. */
    result -= (8-size)*8;
   } else {
    result = 1,shift = 1;
    while (!(iv&shift)) {
     if (++result == 64) break;
     shift <<= 1;
    }
   }
   DCCType_Quit(&vbottom->sv_ctype);
   vbottom->sv_ctype.t_type = DCCTYPE_INT;
   vbottom->sv_ctype.t_base = NULL;
   assert(!vbottom->sv_sym);
   vbottom->sv_const.it = (int_t)result;
   vbottom->sv_flags    = DCC_SFLAG_NONE;
   return;
  }
 }
default_ffs:
 if (vbottom->sv_flags&DCC_SFLAG_TEST) {
  if (mode == KWD___builtin_clz) {
   /* CLZ for 0/1 is simply 'size*8-(0|1)' */
   vpushi(DCCTYPE_INT,size*DCC_TARGET_BITPERBYTE); /* test, 32 */
   vswap();    /* 32, test */
   vgen2('-'); /* 32-test */
  } else {
   /* Special optimization: Since a test is either 0/1,
    *                       its ffs-value is equal to it! */
   vcast_t(DCCTYPE_INT,1);
  }
  return;
 }
 if ((vbottom->sv_reg&DCC_RC_IN(DCC_TARGET_SIZEOF_INT)) &&
     !(vbottom->sv_flags&(DCC_SFLAG_COPY|DCC_SFLAG_TEST)) &&
     !DCC_ASMREG_ISSPTR(vbottom->sv_reg&DCC_RI_MASK)
      ) result_register = vbottom->sv_reg;
 else result_register = DCCVStack_GetReg(DCC_RC_IN(DCC_TARGET_SIZEOF_INT),1);
 vpushr(result_register); /* x, res */
 vswap();                 /* res, x */
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
  struct DCCMemLoc loc;
  loc.ml_reg = vbottom->sv_reg;
  loc.ml_off = vbottom->sv_const.offset;
  loc.ml_sym = vbottom->sv_sym;
  if (mode == KWD___builtin_clz)
       DCCDisp_CLZMem(&loc,result_register,size);
  else DCCDisp_FFSMem(&loc,result_register,size);
 } else {
  /* Scan register(s). */
  if (mode == KWD___builtin_clz)
       DCCDisp_CLZRegs(vbottom->sv_reg,vbottom->sv_reg2,result_register);
  else DCCDisp_FFSRegs(vbottom->sv_reg,vbottom->sv_reg2,result_register);
 }
 vpop(1); /* res */
}






//////////////////////////////////////////////////////////////////////////
// 
//  MINMAX
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_MinMax(tok_t mode) {
 assert(vsize >= 2);
 vdup(1);     /* target, other, other */
 vrrot(3);    /* other, other, target */
 vdup(1);     /* other, other, target, target */
 vlrot(4);    /* target, other, other, target */
 vgen2(mode); /* target, other, other#target */
 vstorecc(0,0); /* e.g.: if (other < target) target = other; */
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Memcmp(void) {
 assert(vsize >= 3);
 /* a, b, size */
 if (DCCSTACKVALUE_ISCONST_INT(vbottom)) {
  struct DCCMemLoc a_loc,b_loc;
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
  /* Fix explicit register offsets now, as the addition/
   * subtraction may otherwise skew the result below. */
  if (vbottom[1].sv_flags&DCC_SFLAG_XOFFSET) DCCStackValue_FixRegOffset(&vbottom[1]);
  if (vbottom[2].sv_flags&DCC_SFLAG_XOFFSET) DCCStackValue_FixRegOffset(&vbottom[2]);
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
#if 1
  vpushxt(DCC_TEST_G); /* a, b, xt(g) */
  vlrot(3);            /* xt(g), a, b */
  vpop(1);             /* xt(g), a */
  vpop(1);             /* xt(g) */
#else
  {
   rc_t resreg,tempreg;
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
  }
#endif
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms(DCC_TARGET_RT_NAME_MEMCMP,DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_ifun(funsym);
  else vpushv(); /* a, b, size, memcmp */
  vlrot(4);      /* memcmp, a, b, size */
  vcall(3);      /* ret */
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Memset(void) {
 assert(vsize >= 3);
 /* Compile-time optimizations for known memset sizes. */
 if (DCCSTACKVALUE_ISCONST_INT(vbottom)) {
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
            /* dst, byt, size */
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
  funsym = DCCUnit_NewSyms(DCC_TARGET_RT_NAME_MEMSET,DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_vpfun(funsym);
  else vpushv(); /* dst, byte, size, memset */
  vlrot(4);      /* memset, dst, byte, size */
  vcall(3);      /* ret */
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Memcpy(int may_overlap) {
 assert(vsize >= 3);
 /* dst, src, size */
 if (DCCSTACKVALUE_ISCONST_INT(vbottom)) {
  struct DCCMemLoc src_loc,dst_loc; int compiletime_overlap;
  target_siz_t copy_size = (target_siz_t)DCCSTACKVALUE_GTCONST_INT(vbottom);
  if (copy_size == 0) {
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
#if DCC_TARGET_RT_HAVE_MEMCPY
  funsym = DCCUnit_NewSyms(may_overlap
                           ? DCC_TARGET_RT_NAME_MEMMOVE
                           : DCC_TARGET_RT_NAME_MEMCPY,
                           DCC_SYMFLAG_NONE);
#else
  funsym = DCCUnit_NewSyms(DCC_TARGET_RT_NAME_MEMMOVE,
                           DCC_SYMFLAG_NONE);
#endif
  if (funsym) DCCVStack_PushSym_vpfun(funsym);
  else vpushv(); /* dst, src, size, memcpy */
  vlrot(4);      /* memcpy, dst, src, size */
  vcall(3);      /* ret */
 }
}

#ifdef _WIN32
#define memrchr  dcc_memrchr
LOCAL void *
dcc_memrchr(void const *p, int c, size_t n) {
 uint8_t *iter = (uint8_t *)p+n;
 while (iter != (uint8_t *)p) {
  if (*--iter == c) return iter;
 }
 return NULL;
}
#endif


#if 1
#define PRIVATE_GETSYM0(name) DCCUnit_GetSyms(name)
#define PRIVATE_GETSYM1(name) DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE)
#define PRIVATE_GETSYM(avail,name) PRIVATE_GETSYM##avail(name)
#define GET_SYM(avail,name) PRIVATE_GETSYM(avail,name)
#else
#define GET_SYM(avail,name) \
 (DCC_MACRO_COND(avail) \
  ? DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE)\
  : DCCUnit_GetSyms(name))
#endif

#define VS_PTR  (&vbottom[2])
#define VS_CHAR (&vbottom[1])
#define VS_SIZE (&vbottom[0])

LOCAL int DCC_VSTACK_CALL
DCCVStack_Scas_Strnlen(uint32_t flags) {
 struct DCCSym *funsym;
 /* Generate a str(n)len/str(n)end function calls. */
 if (flags&DCC_VSTACK_SCAS_FLAG_NULL) return 0;
 if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
  if ((funsym = GET_SYM(DCC_TARGET_RT_HAVE_STRNLEN,
                        DCC_TARGET_RT_NAME_STRNLEN)) != NULL) {
   vswap(); /* ptr, size, char */
   vpop(1); /* ptr, size */
   DCCVStack_PushSym_szfun(funsym);
             /* ptr, size, strnlen */
   vlrot(3); /* strnlen, ptr, size */
   vcall(2); /* ret */
   return 1;
  }
 } else {
  if ((funsym = GET_SYM(DCC_TARGET_RT_HAVE_STRNEND,
                        DCC_TARGET_RT_NAME_STRNEND)) != NULL) {
   /* The symbol has been declared, so we assume it exists! */
   vswap(); /* ptr, size, char */
   vpop(1); /* ptr, size */
   DCCVStack_PushSym_vpfun(funsym);
             /* ptr, size, strnend */
   vlrot(3); /* strnend, ptr, size */
   vcall(2); /* ret */
   return 1;
  }
 } /* !(flags&DCC_VSTACK_SCAS_FLAG_SIZE) */
 return 0;
}
LOCAL int DCC_VSTACK_CALL
DCCVStack_Scas_Strlen(uint32_t flags, target_siz_t ct_size) {
 struct DCCSym *funsym;
 if (ct_size == (target_siz_t)-1) {
  if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
   if ((funsym = GET_SYM(DCC_TARGET_RT_HAVE_STRLEN,
                         DCC_TARGET_RT_NAME_STRLEN)) != NULL) {
    vswap(); /* ptr, size, char */
    vpop(1); /* ptr, size */
    /* Maxlen (aka. unlimited search --> strlen) */
    vpop(1); /* ptr */
    DCCVStack_PushSym_szfun(funsym);
              /* ptr, strlen */
    vswap();  /* strlen, ptr */
    vcall(1); /* ret */
    return 1;
   }
  } else {
   if ((funsym = GET_SYM(DCC_TARGET_RT_HAVE_STREND,
                         DCC_TARGET_RT_NAME_STREND)) != NULL) {
    /* The symbol has been declared, so we assume it exists! */
    vswap(); /* ptr, size, char */
    vpop(1); /* ptr, size */
    /* Maxlen (aka. unlimited search --> strlen) */
    vpop(1);  /* ptr */
    DCCVStack_PushSym_vpfun(funsym);
              /* ptr, strend */
    vswap();  /* strend, ptr */
    vcall(1); /* ret */
    return 1;
   }
  } /* !(flags&DCC_VSTACK_SCAS_FLAG_SIZE) */
 }
 return DCCVStack_Scas_Strnlen(flags);
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Scas(uint32_t flags) {
 struct DCCSym *funsym;
 tyid_t pointer_type = (flags&DCC_VSTACK_SCAS_FLAG_NUL) ? DCCTYPE_VOID : DCCTYPE_USERCHAR;
 assert(vsize >= 3);
 /* ptr, char, size */
 if (flags&DCC_VSTACK_SCAS_FLAG_NUL) {
  /* string-mode is enabled.
   * >> When the string itself is known at compile-time,
   *    replace the max-length value with the minimum
   *    of it and the string's actual length. */
  struct DCCSymAddr cc_ptr;
  void *cc_ptr_data; size_t cc_ptr_msize;
  if (!DCCSTACKVALUE_ISCONST_XVAL(VS_PTR) || !VS_PTR->sv_sym) goto default_scas;
  /* Don't try to reduce the size value if it isn't known at compile-time.
   * If we attempted to do so anyways, we'd just degrade out performance... */
  if (!DCCSTACKVALUE_ISCONST_INT(VS_SIZE)) goto default_scas;
  if (!DCCSym_LoadAddr(VS_PTR->sv_sym,&cc_ptr,0)) goto default_scas;
  assert(cc_ptr.sa_sym);
  assert(DCCSym_ISDEFINED(cc_ptr.sa_sym));
  if (DCCSection_ISIMPORT(DCCSym_SECTION(cc_ptr.sa_sym)))
      goto default_scas;
  /* Make sure the section is readable, but not writable! */
  if ((VS_PTR->sv_sym->sy_sec->sc_start.sy_flags&
      (DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)) !=
      (DCC_SYMFLAG_SEC_R)) goto default_scas;
  cc_ptr.sa_off += VS_PTR->sv_const.offset;
  cc_ptr.sa_off += cc_ptr.sa_sym->sy_addr;
  cc_ptr_data    = DCCSection_TryGetText(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                                        &cc_ptr_msize,NULL);
  if (!cc_ptr_data) goto default_scas;
  cc_ptr_msize = strnlen((char *)cc_ptr_data,cc_ptr_msize);
  if (DCCSection_Hasrel(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                        cc_ptr_msize+1)) goto default_scas;
  vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,(int_t)cc_ptr_msize); /* ptr, char, size, strlen(ptr) */
  vx_min();  /* ptr, char, min(size,strlen(ptr)) */
 }
default_scas:
 if (DCCSTACKVALUE_ISCONST_INT(VS_SIZE)) {
  /* Size is known at compile-time. */
  target_siz_t ct_size;
  ct_size = (target_siz_t)DCCSTACKVALUE_GTCONST_INT(VS_SIZE);
  if unlikely(!ct_size) {
   /* Empty search range: Never found. */
   vpop(1); /* ptr, char */
   vpop(1); /* ptr */
   if (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       vpop(1),vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
   else if (flags&DCC_VSTACK_SCAS_FLAG_NULL)
       vpop(1),vpushc(&DCCType_BuiltinPointers[pointer_type],0);
   else vcast_pt(pointer_type,1);
   return;
  }
  if (DCCSTACKVALUE_ISCONST_INT(VS_CHAR)) {
   /* The search size & character is known at compile-time. */
   int cc_char,is_oob_search;
   struct DCCSymAddr cc_ptr;
   void *cc_ptr_data,*cc_locptr;
   size_t cc_ptr_msize,search_size;
   target_siz_t cc_ptr_vsize;
   cc_char = (int)(uint8_t)DCCSTACKVALUE_GTCONST_INT(VS_CHAR);
   if (!DCCSTACKVALUE_ISCONST_XVAL(VS_PTR) || !VS_PTR->sv_sym) goto no_compiletime;
   if (!DCCSym_LoadAddr(VS_PTR->sv_sym,&cc_ptr,0)) goto no_compiletime;
   assert(cc_ptr.sa_sym);
   assert(DCCSym_ISDEFINED(cc_ptr.sa_sym));
   if (DCCSection_ISIMPORT(DCCSym_SECTION(cc_ptr.sa_sym)))
       goto no_compiletime;
   /* Make sure the section is readable, but not writable! */
   if ((VS_PTR->sv_sym->sy_sec->sc_start.sy_flags&
       (DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)) !=
       (DCC_SYMFLAG_SEC_R)) goto no_compiletime;
   cc_ptr.sa_off += VS_PTR->sv_const.offset;
   cc_ptr.sa_off += cc_ptr.sa_sym->sy_addr;
   cc_ptr_data    = DCCSection_TryGetText(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                                         &cc_ptr_msize,&cc_ptr_vsize);
   if (!cc_ptr_data) goto no_compiletime;
   /* Full compile-time search. */
   search_size = (size_t)ct_size;
   is_oob_search = 0;
   if (search_size > cc_ptr_msize) {
    search_size   = cc_ptr_msize;
    is_oob_search = (cc_char == 0 && cc_ptr_vsize > cc_ptr_msize);
   }
   if (is_oob_search && (flags&DCC_VSTACK_SCAS_FLAG_REV)) {
    /* In reverse search mode, the first ZERO-character
     * is the last out-of-bounds pointer! */
    assert(!cc_char);
    if (ct_size > cc_ptr_vsize)
        ct_size = cc_ptr_vsize;
    cc_locptr = (void *)((uintptr_t)cc_ptr_data+(size_t)ct_size);
   } else {
    cc_locptr = (flags&DCC_VSTACK_SCAS_FLAG_REV)
       ? memrchr(cc_ptr_data,cc_char,search_size)
       :  memchr(cc_ptr_data,cc_char,search_size);
    /* For failed ZERO-searches, the first out-of-bounds
     * character (if apart), is the queried pointer. */
    if (!cc_locptr && is_oob_search)
         cc_locptr = (void *)((uintptr_t)cc_ptr_data+cc_ptr_msize);
   }
   if (cc_locptr) {
    target_siz_t cc_ptr_offset;
    /* Managed to find the character.
     * Simply return the pointer/offset to that character.
     * NOTE: For the pointer case, we return a symbol-pointer
     *       offset from PTR, as this way   */
    cc_ptr_offset = (target_siz_t)((uintptr_t)cc_locptr-
                                   (uintptr_t)cc_ptr_data);
    /* Make sure there are no relocations between the symbol and '+=cc_ptr_offset' */
    if (DCCSection_Hasrel(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                          cc_ptr_offset)) goto no_compiletime;
    /* Either push '(size_t)cc_ptr_offset' or '(void *)(ptr+cc_ptr_offset)' */
    vpop(1); /* ptr, char */
    vpop(1); /* ptr */
    if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
     vpop(1); /* . */
     vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,cc_ptr_offset);
              /* size */
    } else {
     vbottom->sv_const.offset += cc_ptr_offset;
    }
   } else if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
    /* Only keep 'size' on the stack (in a memlen-style way). */
    vswap(); /* ptr, size, char */
    vpop(1); /* ptr, size */
    vswap(); /* size, ptr */
    vpop(1); /* size */
    vrval(); /* rsize */
   } else if (flags&DCC_VSTACK_SCAS_FLAG_NULL) {
    /* We're supposed to return NULL for this case! */
    vpop(1); /* ptr, char */
    vpop(1); /* ptr */
    vpop(1); /* . */
    vpushc(&DCCType_BuiltinPointers[pointer_type],0);
   } else {
    /* Return a pointer after the last character searched (memend-style) */
    if (flags&DCC_VSTACK_SCAS_FLAG_REV) {
     vpop(1); /* ptr, char */
     vpop(1); /* ptr */
     vcast_t(DCCTYPE_INTPTR|DCCTYPE_UNSIGNED,1);
     vgen1(TOK_DEC);
    } else {
     vswap();    /* ptr, size, char */
     vpop(1);    /* ptr, size */
     vswap();    /* size, ptr */
     vcast_t(DCCTYPE_INTPTR|DCCTYPE_UNSIGNED,1);
     vswap();    /* ptr, size */
     vgen2('+'); /* ptr+size */
    }
    vcast_pt(pointer_type,1);
   }
   return;
no_compiletime:
   if (!cc_char && !(flags&DCC_VSTACK_SCAS_FLAG_REV)) {
    /* Generate a str(n)len/str(n)end function calls. */
    if (DCCVStack_Scas_Strlen(flags,ct_size)) return;
   } /* !search_char && !(flags&DCC_VSTACK_SCAS_FLAG_REV) */
  } /* DCCSTACKVALUE_ISCONST_INT(VS_CHAR) */
  /* Only the search size is known. */
  if (ct_size == (target_siz_t)-1) {
   /* Try to generate calls to raw/unlimited scanners. */
   if (!(flags&DCC_VSTACK_SCAS_FLAG_REV)) {
    if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_NUL)
       ? (        (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       ? GET_SYM(DCC_TARGET_RT_HAVE_STROFF,DCC_TARGET_RT_NAME_STROFF)
       : (        (flags&DCC_VSTACK_SCAS_FLAG_NULL)
       ? GET_SYM(DCC_TARGET_RT_HAVE_STRCHR,DCC_TARGET_RT_NAME_STRCHR)
       : GET_SYM(DCC_TARGET_RT_HAVE_STRCHRNUL,DCC_TARGET_RT_NAME_STRCHRNUL)))
       : (        (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       ? GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMLEN,DCC_TARGET_RT_NAME_RAWMEMLEN)
       : GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMCHR,DCC_TARGET_RT_NAME_RAWMEMCHR))) != NULL) {
call_inf_fun:
     vpop(1);  /* ptr, char */
     (flags&DCC_VSTACK_SCAS_FLAG_SIZE) ? DCCVStack_PushSym_szfun(funsym) :
     (flags&DCC_VSTACK_SCAS_FLAG_NUL)  ? DCCVStack_PushSym_cpfun(funsym) :
                                         DCCVStack_PushSym_vpfun(funsym);
     vlrot(3); /* func, ptr, char */
     vcall(2); /* ret */
     return;
    }
   } else {
    if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_NUL)
       ? (        (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       ? GET_SYM(DCC_TARGET_RT_HAVE_STRROFF,DCC_TARGET_RT_NAME_STRROFF)
       : (        (flags&DCC_VSTACK_SCAS_FLAG_NULL)
       ? GET_SYM(DCC_TARGET_RT_HAVE_STRRCHR,DCC_TARGET_RT_NAME_STRRCHR)
       : GET_SYM(DCC_TARGET_RT_HAVE_STRRCHRNUL,DCC_TARGET_RT_NAME_STRRCHRNUL)))
       : (        (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       ? GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMRLEN,DCC_TARGET_RT_NAME_RAWMEMRLEN)
       : GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMRCHR,DCC_TARGET_RT_NAME_RAWMEMRCHR))) != NULL) {
     goto call_inf_fun;
    }
   }
  }
 }
 if (DCCSTACKVALUE_ISCONST_INT(VS_CHAR) &&
    !DCCSTACKVALUE_GTCONST_INT(VS_CHAR) &&
    !(flags&DCC_VSTACK_SCAS_FLAG_REV)) {
  /* When only the character is known to be ZERO at compile-time,
   * still generate calls to to strnlen/strnend */
  if (DCCVStack_Scas_Strnlen(flags)) return;
 }

 if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
  /* Try to call memlen/memrlen */
  if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_NUL)
     ? (        (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_STRNOFF,DCC_TARGET_RT_NAME_STRNOFF)
     : GET_SYM(DCC_TARGET_RT_HAVE_STRNROFF,DCC_TARGET_RT_NAME_STRNROFF))
     : (        (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_MEMLEN,DCC_TARGET_RT_NAME_MEMLEN)
     : GET_SYM(DCC_TARGET_RT_HAVE_MEMRLEN,DCC_TARGET_RT_NAME_MEMRLEN))) != NULL) {
   DCCVStack_PushSym_szfun(funsym);
             /* ptr, char, size, mem(r)len */
   vlrot(4); /* mem(r)len, ptr, char, size */
   vcall(3); /* ret. */
   return;
  }
 } else if (flags&DCC_VSTACK_SCAS_FLAG_NULL) {
#if 0 /* This is already performed below! */
  /* Try to call memend/memrend */
  if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_NUL)
     ? (        (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_STRNCHR,DCC_TARGET_RT_NAME_STRNCHR)
     : GET_SYM(DCC_TARGET_RT_HAVE_STRNRCHR,DCC_TARGET_RT_NAME_STRNRCHR))
     : (        (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? DCCUnit_NewSyms(DCC_TARGET_RT_NAME_MEMCHR,DCC_SYMFLAG_NONE)
     : DCCUnit_NewSyms(DCC_TARGET_RT_NAME_MEMRCHR,DCC_SYMFLAG_NONE))) != NULL) {
   DCCVStack_PushSym_vpfun(funsym);
             /* ptr, char, size, mem(r)end */
   vlrot(4); /* mem(r)len, ptr, char, size */
   vcall(3); /* ret. */
   return;
  }
#endif
 } else {
  /* Try to call memend/memrend */
  if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_NUL)
     ? (        (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_STRNCHRNUL, DCC_TARGET_RT_NAME_STRNCHRNUL)
     : GET_SYM(DCC_TARGET_RT_HAVE_STRNRCHRNUL,DCC_TARGET_RT_NAME_STRNRCHRNUL))
     : (        (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_MEMEND, DCC_TARGET_RT_NAME_MEMEND)
     : GET_SYM(DCC_TARGET_RT_HAVE_MEMREND,DCC_TARGET_RT_NAME_MEMREND))) != NULL) {
   DCCVStack_PushSym_vpfun(funsym);
             /* ptr, char, size, mem(r)end */
   vlrot(4); /* mem(r)len, ptr, char, size */
   vcall(3); /* ret. */
   return;
  }
 }

 if (flags&DCC_VSTACK_SCAS_FLAG_NUL) {
  funsym = (flags&DCC_VSTACK_SCAS_FLAG_REV)
          ? GET_SYM(DCC_TARGET_RT_HAVE_STRNCHR, DCC_TARGET_RT_NAME_STRNCHR)
          : GET_SYM(DCC_TARGET_RT_HAVE_STRNRCHR,DCC_TARGET_RT_NAME_STRNRCHR);
  if (!funsym) {
   /* Manually call strnlen & use its return value as minimum operand. */
   /* >> strnchr(p,c,s); --> memchr(p,c,strnlen(p,s)); */
                 /* str, char, size */
   vrrot(3);     /* char, size, str */
   vdup(1);      /* char, size, str, str */
   vlrot(4);     /* str, char, size, str */
   vswap();      /* str, char, str, size */
   vx_strnlen(); /* str, char, min_size */
   DCCVStack_Scas(flags&~(DCC_VSTACK_SCAS_FLAG_NUL));
   return;
  }
 } else {
  /* Fallback: Call memchr/memrchr */
  funsym = DCCUnit_NewSyms((flags&DCC_VSTACK_SCAS_FLAG_REV)
                           ? DCC_TARGET_RT_NAME_MEMRCHR
                           : DCC_TARGET_RT_NAME_MEMCHR,
                           DCC_SYMFLAG_NONE);
 }
 /* Generate with inline code calling either 'memchr' or 'memrchr'
  * NOTE: This is why DCC assumes that the runtime be implementing at least these! */
again:
 switch (flags&(DCC_VSTACK_SCAS_FLAG_SIZE|
                DCC_VSTACK_SCAS_FLAG_NULL|
                DCC_VSTACK_SCAS_FLAG_REV)) {

 {
 case DCC_VSTACK_SCAS_MEMRCHR: /* >> return memrchr(ptr,char,size); */
 case DCC_VSTACK_SCAS_MEMCHR: /* >> return memchr(ptr,char,size); */
  funsym ? DCCVStack_PushSym_vpfun(funsym)
         : vpushv();
            /* ptr, char, size, mem(r)chr */
  vlrot(4); /* mem(r)chr, ptr, char, size */
  vcall(3); /* ret */
 } break;

 { /* >> void *temp   = ptr-1;
    * >> void *result = memrchr(ptr,char,size);
    * >> if (!result) result = temp;
    * >> return result; */
 case DCC_VSTACK_SCAS_MEMREND:
  /* ptr, char, size */
  vrrot(3); /* char, size, ptr */
  vdup(1);  /* char, size, ptr, dptr */
  vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
  vgen1(TOK_DEC); /* char, size, ptr, dptr-1 */
  vcast_pt(pointer_type,1);
  vlrot(4); /* dptr-1, char, size, ptr */
  vlrot(3); /* dptr-1, ptr, char, size */
  if (DCC_MACRO_FALSE) {
   /* >> void *temp   = ptr+size;
    * >> void *result = memchr(ptr,char,size);
    * >> if (!result) result = temp;
    * >> return result; */
 case DCC_VSTACK_SCAS_MEMEND:
   /* ptr, char, size */
   vrrot(3); /* char, size, ptr */
   vdup(1);  /* char, size, ptr, dptr */
   vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
   vrrot(3); /* char, ptr, dptr, size */
   vdup(1);  /* char, ptr, dptr, size, dsize */
   vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
   vrrot(3); /* char, ptr, size, dsize, dptr */
   vgen2('+'); /* char, ptr, size, dsize+dptr */
   vcast_pt(pointer_type,1);
   vlrot(4); /* dsize+dptr, char, ptr, size */
   vswap();  /* dsize+dptr, char, size, ptr */
   vlrot(3); /* dsize+dptr, ptr, char, size */
  }
  /* <temp>, ptr, char, size */
  funsym ? DCCVStack_PushSym_vpfun(funsym)
         : vpushv();
                 /* <temp>, ptr, char, size, mem(r)chr */
  vlrot(4);      /* <temp>, mem(r)chr, ptr, char, size */
  vcall(3);      /* <temp>, ret */
  vdup(1);       /* <temp>, ret, dret */
  vrrot(3);      /* ret, dret, <temp> */
  vswap();       /* ret, <temp>, dret */
  vstorecc(1,0); /* ret */
 } break;

 { /* >> size_t result = -1;
    * >> void *old_ptr = ptr;
    * >> void *loc_ptr = memrchr(ptr,char,size);
    * >> if (loc_ptr) result = loc_ptr-old_ptr;
    * >> return result; */
 case DCC_VSTACK_SCAS_MEMRLEN:
  vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,-1); /* ptr, char, size, -1 */
  vlrot(4);                                 /* -1, ptr, char, size */
  if (DCC_MACRO_FALSE) {
  /* >> size_t result  = size;
    * >> void  *old_ptr = ptr;
    * >> void  *loc_ptr = memrchr(ptr,char,size);
    * >> if (loc_ptr) result = loc_ptr-old_ptr;
    * >> return result; */
 case DCC_VSTACK_SCAS_MEMLEN:
   vdup(1);  /* ptr, char, size, dsize */
   vlrot(4); /* dsize, ptr, char, size */
  }
            /* <result>, ptr, char, size */
  vrrot(3); /* <result>, char, size, ptr */
  vdup(1);  /* <result>, char, size, ptr, dptr */
  vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
  vlrot(4); /* <result>, dptr, char, size, ptr */
  vlrot(3); /* <result>, dptr, ptr, char, size */
  funsym ? DCCVStack_PushSym_vpfun(funsym)
         : vpushv();
            /* <result>, dptr, ptr, char, size, mem(r)chr */
  vlrot(4); /* <result>, dptr, mem(r)chr, ptr, char, size */
  vcall(3); /* <result>, dptr, ret */
  vdup(1);  /* <result>, dptr, ret, loc_ptr */
  vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
  vrrot(3); /* <result>, ret, loc_ptr, dptr */
  vgen2('-'); /* <result>, ret, loc_ptr-dptr */
  vswap();  /* <result>, loc_ptr-dptr, ret */
  vstorecc(0,0); /* <result> */
 } break;

 default: /* The NULL-flag is ignored by some cases. */
  assert(flags&DCC_VSTACK_SCAS_FLAG_NULL);
  flags &= ~(DCC_VSTACK_SCAS_FLAG_NULL);
  goto again;
 }
#undef VS_SIZE
#undef VS_CHAR
#undef VS_PTR
}

#define VS_DST   (&vbottom[2])
#define VS_SRC   (&vbottom[1])
#define VS_MAX   (&vbottom[0])
PUBLIC void DCC_VSTACK_CALL DCCVStack_Strcpy(int append) {
 struct DCCSym *funsym;
 assert(vsize >= 3);
 /* Optimizations:
  * >> strncpy(p,get_text(),0);          --> p;
  * >> strncat(p,get_text(),0);          --> p;
  * >> strncpy(p,"foo",constant_x);      --> strncpy(p,"foo",min(3,constant_x));
  * >> strncat(p,"foo",constant_x);      --> strncat(p,"foo",min(3,constant_x));
  * >> strncpy(p,"foo",3);               --> memcpy(p,"foo",4);
  * >> strncat(p,"foo",3);               --> memcpy(strend(p),"foo",4),p;
  * >> strncpy(p,get_text(),(size_t)-1); --> strcpy(p,get_text());
  * >> strncat(p,get_text(),(size_t)-1); --> strcat(p,get_text());
  */
 if (DCCSTACKVALUE_ISCONST_INT(VS_MAX)) {
  size_t maxlen = (size_t)DCCSTACKVALUE_GTCONST_INT(VS_MAX);
  if (!maxlen) { vpop(1); vpop(1); return; }
  if (DCCSTACKVALUE_ISCONST_XVAL(VS_SRC) && VS_SRC->sv_sym) {
   struct DCCSymAddr cc_ptr;
   void *cc_ptr_data; size_t cc_ptr_msize;
   if (!DCCSym_LoadAddr(VS_SRC->sv_sym,&cc_ptr,0)) goto no_const_src;
   assert(cc_ptr.sa_sym);
   assert(DCCSym_ISDEFINED(cc_ptr.sa_sym));
   if (DCCSection_ISIMPORT(DCCSym_SECTION(cc_ptr.sa_sym)))
       goto no_const_src;
   /* Make sure the section is readable, but not writable! */
   if ((VS_SRC->sv_sym->sy_sec->sc_start.sy_flags&
       (DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)) !=
       (DCC_SYMFLAG_SEC_R)) goto no_const_src;
   cc_ptr.sa_off += VS_SRC->sv_const.offset;
   cc_ptr.sa_off += cc_ptr.sa_sym->sy_addr;
   cc_ptr_data    = DCCSection_TryGetText(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                                         &cc_ptr_msize,NULL);
   if (!cc_ptr_data) goto no_const_src;
   cc_ptr_msize = strnlen((char *)cc_ptr_data,cc_ptr_msize);
   if (DCCSection_Hasrel(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                         cc_ptr_msize+1)) goto no_const_src;
   vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,(int_t)cc_ptr_msize); /* dst, src, size, strlen(src) */
   vx_min();  /* dst, src, min(size,strlen(src)) */
   if unlikely(!DCCSTACKVALUE_ISCONST_INT(VS_MAX)) goto default_fallback;
   maxlen = (size_t)DCCSTACKVALUE_GTCONST_INT(VS_MAX);
   /* Both 'src' and 'max' are known at compile-time. - Compile using 'memcpy'. */
   if (append) {
                 /* dst, src, size */
    vrrot(3);    /* src, size, dst */
    vdup(1);     /* src, size, dst, ddst */
    vswap();     /* src, size, ddst, dst */
    vx_strend(); /* src, size, ddst, strend(dst) */
    vlrot(4);    /* strend(dst), src, size, ddst */
    vlrot(4);    /* ddst, strend(dst), src, size */
   }
   //vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
   vnorval();
   vgen1(TOK_INC); /* Include the terminating  */
   if (cc_ptr_msize != maxlen) {
    /* Replace the source operand with a statically
     * allocated copy containing the proper terminator byte. */
             /* dst, src, size */
    vswap(); /* dst, size, src */
    vpop(1); /* dst, size */
    vpushstr((char *)cc_ptr_data,maxlen);
             /* dst, size, srcA */
    vprom(); /* dst, size, srcZ (Promote Array to Zero-terminated) */
    vswap(); /* dst, srcZ, size */
   }
   vx_memcpy();  /* [ddst], ret */
   if (append) vpop(1); /* ddst */
   return;
  }
no_const_src:
  if (maxlen == (size_t)-1) {
   funsym = append ? GET_SYM(DCC_TARGET_RT_HAVE_STRCAT,DCC_TARGET_RT_NAME_STRCAT)
                   : GET_SYM(DCC_TARGET_RT_HAVE_STRCPY,DCC_TARGET_RT_NAME_STRCPY);
   if (funsym) {
    vpop(1);
    funsym ? DCCVStack_PushSym_cpfun(funsym)
           : vpushv(); /* dst, src, strcpy */
    vlrot(3);          /* strcpy, dst, src */
    vcall(2);          /* ret */
    return;
   }
  }
 }
default_fallback:
 /* Fallback: Call strncpy/strncat */
 funsym = DCCUnit_NewSyms(append
                          ? DCC_TARGET_RT_NAME_STRNCAT
                          : DCC_TARGET_RT_NAME_STRNCPY,
                          DCC_SYMFLAG_DEFAULT);
 funsym ? DCCVStack_PushSym_cpfun(funsym)
        : vpushv(); /* dst, src, max, strncpy */
 vlrot(4);          /* strncpy, dst, src, max */
 vcall(3);          /* ret */
}
#undef VS_MAX
#undef VS_SRC
#undef VS_DST

DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_EXT_C */
