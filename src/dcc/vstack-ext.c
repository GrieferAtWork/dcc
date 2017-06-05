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

#include <dcc/common.h>
#include <dcc/vstack.h>
#include <dcc/unit.h>
#include <dcc/gen.h>
#include <dcc/type.h>
#include <dcc/compiler.h>

DCC_DECL_BEGIN

INTERN void DCC_VSTACK_CALL
DCCVStack_PushSym_vpfun(struct DCCSym *__restrict sym) {
 struct DCCType type = {DCCTYPE_VOID,NULL};
 DCCType_MkPointer(&type);
 DCCType_MkOldFunc(&type);
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


//////////////////////////////////////////////////////////////////////////
// 
//  MINMAX
// 
PUBLIC void DCC_VSTACK_CALL
DCCVStack_MinMax(tok_t mode) {
 vdup(1);     /* target, other, other */
 vrrot(3);    /* other, other, target */
 vdup(1);     /* other, other, target, target */
 vlrot(4);    /* target, other, other, target */
 vgen2(mode); /* target, other, other#target */
 vstorecc(0,0); /* e.g.: if (other < target) target = other; */
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Strlen(int nlen_mode) {
 struct DCCStackValue *str = vbottom+!!nlen_mode;
 assert(vsize >= (size_t)(nlen_mode ? 2 : 1));
 if (!(str->sv_flags&DCC_SFLAG_LVALUE) &&
       str->sv_reg == DCC_RC_CONST && str->sv_sym &&
       DCCSym_ISDEFINED(str->sv_sym)) {
  /* Special case: The address of the string is known at compile-time. */
  size_t s_max;
  char *s = (char *)DCCSection_TryGetText(DCCSym_SECTION(str->sv_sym),
                                          str->sv_sym->sy_addr,&s_max,NULL);
  s_max = strnlen(s,s_max);
  if (nlen_mode) vswap();
  vpop(1); /* [max] */
  /* Push the actual string length. */
  vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,s_max);
  if (nlen_mode) {
   vrcopy();     /* maxd */
   vswap();      /* maxd, clen */
   vminmax('<'); /* min(maxd,clen) */
   vrval();
  }
 } else {
  struct DCCSym *funsym;
  funsym = DCCUnit_NewSyms(nlen_mode ? "strnlen" : "strlen",DCC_SYMFLAG_NONE);
  if (funsym) DCCVStack_PushSym_szfun(funsym);
  else vpushv(); /* str, [max], strlen */
  if (nlen_mode) {
   vlrot(3); /* strlen, str, max */
   vcall(2); /* ret */
  } else {
   vswap();  /* strlen, str */
   vcall(1); /* ret */
  }
 }
}

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Memcmp(void) {
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
}


PUBLIC void DCC_VSTACK_CALL
DCCVStack_Memset(void) {
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

PUBLIC void DCC_VSTACK_CALL
DCCVStack_Memcpy(int may_overlap) {
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



PUBLIC void DCC_VSTACK_CALL
DCCVStack_Scas(uint32_t flags) {
 struct DCCSym *funsym;
#define VS_PTR  (&vbottom[2])
#define VS_CHAR (&vbottom[1])
#define VS_SIZE (&vbottom[0])
 /* ptr, char, size */
 assert(vsize >= 2);
 if (DCCSTACKVALUE_ISCONST_INT(VS_SIZE)) {
  /* Size is known at compile-time. */
  target_siz_t size;
  size = (target_siz_t)DCCSTACKVALUE_GTCONST_INT(VS_SIZE);
  if unlikely(!size) {
   /* Empty search range: Never found. */
   vpop(1); /* ptr, char */
   vpop(1); /* ptr */
   if (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE)
       vpop(1),vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
   else if (flags&DCC_VSTACK_MEMCHR_FLAG_NULL)
       vpop(1),vpushc(&DCCType_BuiltinPointers[DCCTYPE_VOID],0);
   else vcast_pt(DCCTYPE_VOID,1);
   return;
  }
  if (DCCSTACKVALUE_ISCONST_INT(VS_CHAR)) {
   /* The search character is known at compile-time. */
   int search_char;
   search_char = (int)(uint8_t)DCCSTACKVALUE_GTCONST_INT(VS_CHAR);
   if (DCCSTACKVALUE_ISCONST_XVAL(VS_PTR)) {
    /* TODO: Full compile-time search. */
   }
   if (!search_char && !(flags&DCC_VSTACK_MEMCHR_FLAG_REV)) {
    /* Generate a str(n)len/str(n)end function calls. */
    if (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE) {
     vswap(); /* ptr, size, char */
     vpop(1); /* ptr, size */
     if (size == (target_siz_t)-1) {
      /* Maxlen (aka. unlimited search --> strlen) */
      vpop(1); /* ptr */
      funsym = DCCUnit_NewSyms("strlen",DCC_SYMFLAG_NONE);
      funsym ? DCCVStack_PushSym_szfun(funsym) : vpushv();
      vcall(1); /* ret */
     } else {
      funsym = DCCUnit_NewSyms("strnlen",DCC_SYMFLAG_NONE);
      funsym ? DCCVStack_PushSym_szfun(funsym) : vpushv();
      vcall(2); /* ret */
     }
     return;
    } else {
     if ((funsym = (size == (target_siz_t)-1)
        ? DCCUnit_GetSyms("strend")
        : DCCUnit_GetSyms("strnend")) != NULL) {
      /* The symbol has been declared, so we assume it exists! */
      vswap(); /* ptr, size, char */
      vpop(1); /* ptr, size */
      if (size == (target_siz_t)-1) {
       /* Maxlen (aka. unlimited search --> strlen) */
       vpop(1); /* ptr */
       DCCVStack_PushSym_vpfun(funsym);
       vcall(1); /* ret */
      } else {
       DCCVStack_PushSym_vpfun(funsym);
       vcall(2); /* ret */
      }
      return;
     }
    } /* !(flags&DCC_VSTACK_MEMCHR_FLAG_SIZE) */
   } /* !search_char && !(flags&DCC_VSTACK_MEMCHR_FLAG_REV) */
  } /* DCCSTACKVALUE_ISCONST_INT(VS_CHAR) */
  if (size == (target_siz_t)-1) {
   /* Try to generate calls to 'rawmemchr'/'rawmemrchr'/'rawmemlen'/'rawmemrlen' */
   if (!(flags&DCC_VSTACK_MEMCHR_FLAG_REV)) {
    if ((funsym = (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE)
       ? DCCUnit_GetSyms("rawmemlen")
       : (flags&DCC_VSTACK_MEMCHR_FLAG_NULL)
       ? DCCUnit_GetSyms("rawmemchr")
       : DCCUnit_GetSyms("rawmemend")) != NULL) {
     vpop(1); /* ptr, char */
     (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE)
      ? DCCVStack_PushSym_szfun(funsym)
      : DCCVStack_PushSym_vpfun(funsym); /* ptr, char, func */
     vlrot(3); /* func, ptr, char */
     vcall(2); /* ret */
     return;
    }
   } else {
    if ((funsym = (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE)
       ? DCCUnit_GetSyms("rawmemrlen")
       : (flags&DCC_VSTACK_MEMCHR_FLAG_NULL)
       ? DCCUnit_GetSyms("rawmemrchr")
       : DCCUnit_GetSyms("rawmemrend")) != NULL) {
     /* ptr, char, size */
     vrrot(3); /* char, size, ptr */
     vcast_t(DCCTYPE_INTPTR|DCCTYPE_UNSIGNED,1);
     vrcopy();
     vswap();        /* char, ptr, size */
     vgen2('+');     /* char, ptr+size */
     vgen1(TOK_DEC); /* char, (ptr+size)-1 */
     vswap();        /* (ptr+size)-1, char */
     (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE)
      ? DCCVStack_PushSym_szfun(funsym)
      : DCCVStack_PushSym_vpfun(funsym);
     vlrot(3); /* func, (ptr+size)-1, char */
     vcall(2); /* ret */
     return;
    }
   }
  }
 }
 if (flags&DCC_VSTACK_MEMCHR_FLAG_SIZE) {
  /* Try to call memlen/memrlen */
 } else if (flags&DCC_VSTACK_MEMCHR_FLAG_NULL) {
  /* Try to call memend/memrend */
 }

 /* Fallback: Call memchr/memrchr */

 /* TODO (Generate with inline code):
  * >> void  *_p,*p_result;
  * >> size_t _s,*s_result;
  * >> switch (mode) {
  * >>
  * >> case REVERSE_MEMCHR:
  * >>    p_result = memrchr(p,s);
  * >>    break;
  * >>
  * >> case REVERSE_MEMEND:
  * >>    _p = p-1;
  * >>    p_result = memrchr(p,s);
  * >>    if (!p_result) p_result = _p;
  * >>    break;
  * >>
  * >> case REVERSE_MEMLEN:
  * >>    _p = p;
  * >>    p_result = memrchr(p,s);
  * >>    if (!p_result) s_result = -1;
  * >>    else           s_result = p_result-_p;
  * >>    break;
  * >>
  * >> case MEMCHR:
  * >>    p_result = memchr(p,s);
  * >>    break;
  * >>
  * >> case MEMEND:
  * >>    _p = p+s;
  * >>    p_result = memchr(p,s);
  * >>    if (!p_result) p_result = _p;
  * >>    break;
  * >>
  * >> case MEMLEN:
  * >>    _p = p;
  * >>    _s = s;
  * >>    p_result = memrchr(p,s);
  * >>    if (!p_result) s_result = _s;
  * >>    else           s_result = p_result-_p;
  * >>    break;
  * >>    
  * >> }
  */
 funsym = DCCUnit_NewSyms(flags&DCC_VSTACK_MEMCHR_FLAG_REV ?
                          "memrchr" : "memchr",DCC_SYMFLAG_NONE);
 DCCVStack_PushSym_vpfun(funsym); /* [...], ptr, char, size, func */
 vlrot(4); /* [...], func, ptr, char, size */
 vcall(3); /* [...], ret */

#undef VS_SIZE
#undef VS_CHAR
#undef VS_PTR
}



DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_EXT_C */
