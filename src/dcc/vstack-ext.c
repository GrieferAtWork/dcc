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


#define GET_SYM(avail,name) \
 (DCC_MACRO_COND(avail) \
  ? DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE)\
  : DCCUnit_GetSyms(name))


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
  target_siz_t ct_size;
  if unlikely((ct_size = (target_siz_t)DCCSTACKVALUE_GTCONST_INT(VS_SIZE)) == 0) {
   /* Empty search range: Never found. */
   vpop(1); /* ptr, char */
   vpop(1); /* ptr */
   if (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       vpop(1),vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,0);
   else if (flags&DCC_VSTACK_SCAS_FLAG_NULL)
       vpop(1),vpushc(&DCCType_BuiltinPointers[DCCTYPE_VOID],0);
   else vcast_pt(DCCTYPE_VOID,1);
   return;
  }
  if (DCCSTACKVALUE_ISCONST_INT(VS_CHAR)) {
   /* The search character is known at compile-time. */
   int cc_char;
   cc_char = (int)(uint8_t)DCCSTACKVALUE_GTCONST_INT(VS_CHAR);
   if (DCCSTACKVALUE_ISCONST_XVAL(VS_PTR) && VS_PTR->sv_sym) {
    struct DCCSymAddr cc_ptr;
    if (DCCSym_LoadAddr(VS_PTR->sv_sym,&cc_ptr,0)) {
     assert(cc_ptr.sa_sym);
     assert(cc_ptr.sa_sym->sy_sec);
     /* Make sure the section is readable, but not writable! */
     if ((VS_PTR->sv_sym->sy_sec->sc_start.sy_flags&
         (DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W)) ==
         (DCC_SYMFLAG_SEC_R)) {
      void        *cc_ptr_data;
      size_t       cc_ptr_msize;
      target_siz_t cc_ptr_vsize;
      cc_ptr.sa_off += VS_PTR->sv_const.offset;
      cc_ptr.sa_off += cc_ptr.sa_sym->sy_addr;
      cc_ptr_data = DCCSection_TryGetText(cc_ptr.sa_sym->sy_sec,cc_ptr.sa_off,
                                         &cc_ptr_msize,&cc_ptr_vsize);
      if (cc_ptr_data) {
       /* Full compile-time search. */
       void *cc_locptr;
       size_t search_size = (size_t)ct_size;
       int is_oob_search = 0;
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
        vpushc(&DCCType_BuiltinPointers[DCCTYPE_VOID],0);
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
        vcast_pt(DCCTYPE_VOID,1);
       }
       return;
      }
     }
    }
   }
no_compiletime:
   if (!cc_char && !(flags&DCC_VSTACK_SCAS_FLAG_REV)) {
    /* Generate a str(n)len/str(n)end function calls. */
    if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
     if ((funsym = (ct_size == (target_siz_t)-1)
        ? GET_SYM(DCC_TARGET_RT_HAVE_STRLEN,"strlen")
        : GET_SYM(DCC_TARGET_RT_HAVE_STRNLEN,"strnlen")) != NULL) {
      vswap(); /* ptr, size, char */
      vpop(1); /* ptr, size */
      if (ct_size == (target_siz_t)-1) {
       /* Maxlen (aka. unlimited search --> strlen) */
       vpop(1); /* ptr */
       DCCVStack_PushSym_szfun(funsym);
                 /* ptr, strlen */
       vswap();  /* strlen, ptr */
       vcall(1); /* ret */
      } else {
       DCCVStack_PushSym_szfun(funsym);
                 /* ptr, size, strnlen */
       vlrot(3); /* strnlen, ptr, size */
       vcall(2); /* ret */
      }
      return;
     }
    } else {
     if ((funsym = (ct_size == (target_siz_t)-1)
        ? GET_SYM(DCC_TARGET_RT_HAVE_STREND,"strend")
        : GET_SYM(DCC_TARGET_RT_HAVE_STRNEND,"strnend")) != NULL) {
      /* The symbol has been declared, so we assume it exists! */
      vswap(); /* ptr, size, char */
      vpop(1); /* ptr, size */
      if (ct_size == (target_siz_t)-1) {
       /* Maxlen (aka. unlimited search --> strlen) */
       vpop(1);  /* ptr */
       DCCVStack_PushSym_vpfun(funsym);
                 /* ptr, strend */
       vswap();  /* strend, ptr */
       vcall(1); /* ret */
      } else {
       DCCVStack_PushSym_vpfun(funsym);
                 /* ptr, size, strnend */
       vlrot(3); /* strnend, ptr, size */
       vcall(2); /* ret */
      }
      return;
     }
    } /* !(flags&DCC_VSTACK_SCAS_FLAG_SIZE) */
   } /* !search_char && !(flags&DCC_VSTACK_SCAS_FLAG_REV) */
  } /* DCCSTACKVALUE_ISCONST_INT(VS_CHAR) */
  if (ct_size == (target_siz_t)-1) {
   /* Try to generate calls to 'rawmemchr'/'rawmemrchr'/'rawmemlen'/'rawmemrlen' */
   if (!(flags&DCC_VSTACK_SCAS_FLAG_REV)) {
    if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       ? GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMLEN,"rawmemlen")
       : GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMCHR,"rawmemchr")) != NULL) {
     vpop(1); /* ptr, char */
     (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
      ? DCCVStack_PushSym_szfun(funsym)
      : DCCVStack_PushSym_vpfun(funsym); /* ptr, char, func */
     vlrot(3); /* func, ptr, char */
     vcall(2); /* ret */
     return;
    }
   } else {
    if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
       ? GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMRLEN,"rawmemrlen")
       : GET_SYM(DCC_TARGET_RT_HAVE_RAWMEMRCHR,"rawmemrchr")) != NULL) {
               /* ptr, char, size */
     vrrot(3); /* char, size, ptr */
     vcast_t(DCCTYPE_INTPTR|DCCTYPE_UNSIGNED,1);
     vrcopy();
     vswap();        /* char, ptr, size */
     vgen2('+');     /* char, ptr+size */
     vgen1(TOK_DEC); /* char, (ptr+size)-1 */
     vswap();        /* (ptr+size)-1, char */
     (flags&DCC_VSTACK_SCAS_FLAG_SIZE)
      ? DCCVStack_PushSym_szfun(funsym)
      : DCCVStack_PushSym_vpfun(funsym);
     vlrot(3); /* func, (ptr+size)-1, char */
     vcall(2); /* ret */
     return;
    }
   }
  }
 }
 if (flags&DCC_VSTACK_SCAS_FLAG_SIZE) {
  /* Try to call memlen/memrlen */
  if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_MEMLEN,"memlen")
     : GET_SYM(DCC_TARGET_RT_HAVE_MEMRLEN,"memrlen")) != NULL) {
   DCCVStack_PushSym_szfun(funsym);
             /* ptr, char, size, mem(r)len */
   vlrot(4); /* mem(r)len, ptr, char, size */
   vcall(3); /* ret. */
   return;
  }
 } else if (flags&DCC_VSTACK_SCAS_FLAG_NULL) {
  /* Try to call memend/memrend */
  if ((funsym = (flags&DCC_VSTACK_SCAS_FLAG_REV)
     ? GET_SYM(DCC_TARGET_RT_HAVE_MEMEND,"memend")
     : GET_SYM(DCC_TARGET_RT_HAVE_MEMREND,"memrend")) != NULL) {
   DCCVStack_PushSym_vpfun(funsym);
             /* ptr, char, size, mem(r)end */
   vlrot(4); /* mem(r)len, ptr, char, size */
   vcall(3); /* ret. */
   return;
  }
 }

 /* Fallback: Call memchr/memrchr */

#define CASE_REVERSE_MEMCHR   (DCC_VSTACK_SCAS_FLAG_REV|DCC_VSTACK_SCAS_FLAG_NULL)
#define CASE_REVERSE_MEMEND   (DCC_VSTACK_SCAS_FLAG_REV)
#define CASE_REVERSE_MEMLEN   (DCC_VSTACK_SCAS_FLAG_REV|DCC_VSTACK_SCAS_FLAG_SIZE)
#define CASE_FORWARD_MEMCHR   (DCC_VSTACK_SCAS_FLAG_NULL)
#define CASE_FORWARD_MEMEND   (DCC_VSTACK_SCAS_FLAG_NONE)
#define CASE_FORWARD_MEMLEN   (DCC_VSTACK_SCAS_FLAG_SIZE)
 /* Generate with inline code calling either 'memchr' or 'memrchr'
  * NOTE: This is why DCC assumes that the runtime be implementing at least these! */
 funsym = DCCUnit_NewSyms(flags&DCC_VSTACK_SCAS_FLAG_REV ?
                          "memrchr" : "memchr",DCC_SYMFLAG_NONE);
again:
 switch (flags&(DCC_VSTACK_SCAS_FLAG_SIZE|
                DCC_VSTACK_SCAS_FLAG_NULL|
                DCC_VSTACK_SCAS_FLAG_REV)) {

 {
 case CASE_REVERSE_MEMCHR: /* >> return memrchr(ptr,char,size); */
 case CASE_FORWARD_MEMCHR: /* >> return memchr(ptr,char,size); */
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
 case CASE_REVERSE_MEMEND:
  /* ptr, char, size */
  vrrot(3); /* char, size, ptr */
  vdup(1);  /* char, size, ptr, dptr */
  vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
  vgen1(TOK_DEC); /* char, size, ptr, dptr-1 */
  vcast_pt(DCCTYPE_VOID,1);
  vlrot(4); /* dptr-1, char, size, ptr */
  vlrot(3); /* dptr-1, ptr, char, size */
  if (DCC_MACRO_FALSE) {
   /* >> void *temp   = ptr+size;
    * >> void *result = memchr(ptr,char,size);
    * >> if (!result) result = temp;
    * >> return result; */
 case CASE_FORWARD_MEMEND:
   /* ptr, char, size */
   vrrot(3); /* char, size, ptr */
   vdup(1);  /* char, size, ptr, dptr */
   vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
   vrrot(3); /* char, ptr, dptr, size */
   vdup(1);  /* char, ptr, dptr, size, dsize */
   vcast_t(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,1);
   vrrot(3); /* char, ptr, size, dsize, dptr */
   vgen2('+'); /* char, ptr, size, dsize+dptr */
   vcast_pt(DCCTYPE_VOID,1);
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
 case CASE_REVERSE_MEMLEN:
  vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,-1); /* ptr, char, size, -1 */
  vlrot(4);                                 /* -1, ptr, char, size */
  if (DCC_MACRO_FALSE) {
  /* >> size_t result  = size;
    * >> void  *old_ptr = ptr;
    * >> void  *loc_ptr = memrchr(ptr,char,size);
    * >> if (loc_ptr) result = loc_ptr-old_ptr;
    * >> return result; */
 case CASE_FORWARD_MEMLEN:
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



DCC_DECL_END

#endif /* !GUARD_DCC_VSTACK_EXT_C */
