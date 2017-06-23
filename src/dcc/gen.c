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
#ifndef GUARD_DCC_GEN_C
#define GUARD_DCC_GEN_C 1
#define X86_UTIL_GENERATOR_SOURCE

#include <dcc/common.h>
#include <dcc/gen.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/linker.h>
#include <drt/drt.h>

#include <string.h>

#include "x86_util.h"

#if DCC_CONFIG_HAVE_DRT
#include "../drt/drt.h"
#endif

DCC_DECL_BEGIN

PRIVATE void DCCDisp_SymAddr8(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp8(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymAddr16(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp16(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymAddr32(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp32(struct DCCSymAddr const *__restrict expr);
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
PRIVATE void DCCDisp_SymAddr64(struct DCCSymAddr const *__restrict expr);
PRIVATE void DCCDisp_SymDisp64(struct DCCSymAddr const *__restrict expr);
#endif


#if DCC_CONFIG_HAVE_DRT
PUBLIC void
DCCDisp_Probe_(struct DCCMemLoc const *__restrict addr,
               size_t n_bytes) {
 /* ECX = 'addr'; [EDX = 'n_bytes']; */
 struct DCCSymAddr val;
 int push_regs = 0;
#define PUSH_ECX 0x01
#define PUSH_EDX 0x02
 if (!n_bytes) return;
 if (n_bytes == 1 ||
     n_bytes == 2 ||
     n_bytes == 4
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
  || n_bytes == 8
#endif
     ) {
  rc_t rc = DCC_RC_FORSIZE(n_bytes);
  /* Try to generate a 'mov' instruction with an empty register. */
  rc = DCCVStack_GetReg(rc|DCC_ASMREG_ECX,1|2);
  /* TODO: Dirty hack to not use EAX or EDX because
   *       they might be dangling return registers.
   *       Instead, somehow mark the registers as
   *       in-use during return statements. */
  if (rc && (DCC_RC_ISCONST(addr->ml_reg) ||
            (addr->ml_reg&DCC_RI_MASK) != (rc&DCC_RI_MASK)) &&
     (rc&DCC_RI_MASK) != DCC_ASMREG_AX &&
     (rc&DCC_RI_MASK) != DCC_ASMREG_DX) {
   DCCDisp_MemMovReg(addr,rc);
  } else {
   /* Fallback: Compile a 'test' instruction. */
   int has_test = DCCVStack_HasTst();
   val.sa_off = 0,val.sa_sym = NULL;
   if (has_test) t_putb(0x9c); /* pushf */
   DCCDisp_CstBinMem('t',&val,addr,n_bytes,1); /* test $0, addr */
   if (has_test) t_putb(0x9d); /* popf */
  }
  return;
 }

 /* Fallback: Call a compiler-internal function. */
 if (DCCVStack_GetRegInuse(DCC_RR_XDX)) {
  push_regs |= PUSH_EDX;
  DCCDisp_RegPush(DCC_RR_XDX);
 }
 val.sa_sym = DCCUnit_NewSyms("__probe_nb",DCC_SYMFLAG_WEAK);
 if (val.sa_sym && DCCSym_ISFORWARD(val.sa_sym))
     DCCSym_Define(val.sa_sym,&DCCSection_Abs,(target_ptr_t)(void *)&DRT_U_ProbeN,0,1);
 DCCDisp_IntMovReg(n_bytes,DCC_RR_XDX);
 if (DCCVStack_GetRegInuse(DCC_RR_XCX)) {
  push_regs |= PUSH_ECX;
  DCCDisp_RegPush(DCC_RR_XCX);
 }
 /* Load the probe address into 'CX' */
 DCCDisp_LeaReg(addr,DCC_RR_XCX);
 val.sa_off = 0;
 /* Call the probe function.
  * NOTE: This is compiled manually to prevent an infinite recursion when
  *       'DCCDisp_LocCll' would try to probe the probe functions themself. */
 t_putb(0xe8);
 DCCDisp_SymDisp32(&val);
 if (push_regs&PUSH_ECX) DCCDisp_PopReg(DCC_RR_XCX);
 if (push_regs&PUSH_EDX) DCCDisp_PopReg(DCC_RR_XDX);
}
#endif /* DCC_CONFIG_HAVE_DRT */



LOCAL void *
DCCMemLoc_CompilerAddr_impl(struct DCCMemLoc const *__restrict l,
                            void **update_ptr, size_t n_bytes) {
 if ((compiler.c_flags&DCC_COMPILER_FLAG_SINIT) &&
      DCCMEMLOC_ISMEMOFF_S(l)) {
  struct DCCSymAddr symaddr;
  if (DCCSym_LoadAddr(l->ml_sym,&symaddr,0)) {
   uint8_t *target_data;
   struct DCCSection *target_sec;
   assert(symaddr.sa_sym->sy_sec);
   assert(!symaddr.sa_sym->sy_alias);
   target_sec = symaddr.sa_sym->sy_sec;
   /* Directly write to target memory (at compile-time).
    * >> This is used for static initializer of global data. */
   DCCSection_TBEGIN(target_sec);
   if (update_ptr && ((uint8_t *)*update_ptr >= target_sec->sc_dat.sd_text.tb_begin &&
                      (uint8_t *)*update_ptr <= target_sec->sc_dat.sd_text.tb_end)) {
    *(uintptr_t *)update_ptr -= (uintptr_t)target_sec->sc_dat.sd_text.tb_begin;
    target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off+
                                                symaddr.sa_sym->sy_addr+
                                                l->ml_off,n_bytes);
    *(uintptr_t *)update_ptr += (uintptr_t)target_sec->sc_dat.sd_text.tb_begin;
   } else {
    target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off+
                                                symaddr.sa_sym->sy_addr+
                                                l->ml_off,n_bytes);
   }
   DCCSection_TEND(target_sec);
   return target_data;
  }
 }
 return NULL;
}

LOCAL void const *
DCCMemLoc_CompilerData_impl(struct DCCMemLoc const *__restrict l,
                            void **update_ptr, size_t n_bytes) {
 if (DCCMEMLOC_ISMEMOFF_S(l)) {
  struct DCCSymAddr symaddr;
  if (DCCSym_LoadAddr(l->ml_sym,&symaddr,0)) {
   uint8_t *target_data;
   struct DCCSection *target_sec;
   assert(symaddr.sa_sym->sy_sec);
   assert(!symaddr.sa_sym->sy_alias);
   target_sec = symaddr.sa_sym->sy_sec;
   if (!(compiler.c_flags&DCC_COMPILER_FLAG_SINIT) &&
        (target_sec->sc_start.sy_flags&DCC_SYMFLAG_SEC_W)) return NULL;
   symaddr.sa_off += symaddr.sa_sym->sy_addr;
   symaddr.sa_off += l->ml_off;
   /* If there are relocations within the requested address range, we can't
    * evaluate the data at compile-time, as it might change during linking.
    * A situation in which this is important:
    * >> extern char s[];
    * >> // 'ps' cannot be resolved at compile-time because its data block contains relocations.
    * >> static char *const ps __attribute__((section(".data"))) = s; */
   if (DCCSection_Hasrel(target_sec,symaddr.sa_off,n_bytes)) return NULL;
   /* Directly write to target memory (at compile-time).
    * >> This is used for static initializer of global data. */
   DCCSection_TBEGIN(target_sec);
   if (update_ptr && ((uint8_t *)*update_ptr >= target_sec->sc_dat.sd_text.tb_begin &&
                      (uint8_t *)*update_ptr <= target_sec->sc_dat.sd_text.tb_end)) {
    *(uintptr_t *)update_ptr -= (uintptr_t)target_sec->sc_dat.sd_text.tb_begin;
    target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off,n_bytes);
    *(uintptr_t *)update_ptr += (uintptr_t)target_sec->sc_dat.sd_text.tb_begin;
   } else {
    target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off,n_bytes);
   }
   DCCSection_TEND(target_sec);
   return target_data;
  }
 }
 return NULL;
}

PUBLIC void *
DCCMemLoc_CompilerAddr(struct DCCMemLoc const *__restrict l, size_t n_bytes) {
 return DCCMemLoc_CompilerAddr_impl(l,NULL,n_bytes);
}
PUBLIC void const *
DCCMemLoc_CompilerData(struct DCCMemLoc const *__restrict l, size_t n_bytes) {
 return DCCMemLoc_CompilerData_impl(l,NULL,n_bytes);
}
PUBLIC void *
DCCMemLoc_CompilerAddrUpdate(struct DCCMemLoc const *__restrict l,
                             void **__restrict update_ptr, size_t n_bytes) {
 assert(update_ptr);
 return DCCMemLoc_CompilerAddr_impl(l,update_ptr,n_bytes);
}
PUBLIC void const *
DCCMemLoc_CompilerDataUpdate(struct DCCMemLoc const *__restrict l,
                             void **__restrict update_ptr, size_t n_bytes) {
 assert(update_ptr);
 return DCCMemLoc_CompilerData_impl(l,update_ptr,n_bytes);
}


PUBLIC void const *
DCCMemLoc_CompilerText(struct DCCMemLoc const *__restrict l,
                       struct DCCCompilerText *__restrict text,
                       size_t n_bytes) {
 if (DCCMEMLOC_ISMEMOFF_S(l)) {
  struct DCCSymAddr symaddr;
  if (DCCSym_LoadAddr(l->ml_sym,&symaddr,0)) {
   uint8_t *target_data;
   struct DCCSection *target_sec;
   assert(symaddr.sa_sym->sy_sec);
   assert(!symaddr.sa_sym->sy_alias);
   target_sec = symaddr.sa_sym->sy_sec;
   if (!(compiler.c_flags&DCC_COMPILER_FLAG_SINIT) &&
        (target_sec->sc_start.sy_flags&DCC_SYMFLAG_SEC_W)) return NULL;
   symaddr.sa_off += symaddr.sa_sym->sy_addr;
   symaddr.sa_off += l->ml_off;
   text->ct_sec  = target_sec;
   text->ct_base = symaddr.sa_off;
   /* Lookup relocations inside the symbol's address range. */
   text->ct_relv = DCCSection_GetRel(target_sec,symaddr.sa_off,
                                     n_bytes,&text->ct_relc);
   /* Directly write to target memory (at compile-time).
    * >> This is used for static initializer of global data. */
   DCCSection_TBEGIN(target_sec);
   target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off,n_bytes);
   DCCSection_TEND(target_sec);
   return target_data;
  }
 }
 return NULL;
}

#if DCC_TARGET_HASI(I_X86)
PUBLIC void
DCCDisp_X86PutSegmentPrefix(rc_t memrc) {
 uint8_t segid;
 assert(DCC_RC_HAS_86SEGP(memrc));
 segid = memrc >> DCC_RC_SHFT_86SEGP;
 if (memrc&DCC_RC_I) {
  /* Check for special segment/register combinations that don't require a prefix. */
  if ((memrc&DCC_RI_MASK) == DCC_ASMREG_BP) {
   /* The 'BP' family of registers defaults to using
    * the 'SS' segment when none is specified. */
   if (segid == DCC_RC_86SEGP_SS+1) return;
  } else {
   /* All other registers default to using 'DS' */
   if (segid == DCC_RC_86SEGP_DS+1) return;
  }
 }
 /* Fallback: Actually write the prefix. */
 t_putb((DCCAsmReg_86SegPrefix-1)[segid]);
}
#endif /* I_X86 */


LOCAL target_ptr_t reg_max(rc_t reg) {
 target_ptr_t result;
#ifdef DCC_RC_I64
 if (reg&DCC_RC_I64) result = (target_ptr_t)0xffffffffffffffffull; else
#endif
      if (reg&DCC_RC_I32) result = (target_ptr_t)0xfffffffful;
 else if (reg&DCC_RC_I16) result = (target_ptr_t)0xfffful;
 else                     result = (target_ptr_t)0xfful;
 return result;
}

LOCAL target_off_t
DCCDisp_PutAddrRel(struct DCCSymAddr const *__restrict addr, rel_t rel) {
 struct DCCSym *xsym; target_off_t xval;
 assert(addr);
 xval = addr->sa_off;
 if ((xsym = addr->sa_sym) != NULL) {
  struct DCCSymAddr xsymaddr;
  if (DCCSym_LoadAddr(xsym,&xsymaddr,0)) {
   xval += (target_off_t)xsymaddr.sa_off;
   xsym  =  xsymaddr.sa_sym;
   if (DCCSection_HASBASE(DCCSym_SECTION(xsym))) {
    /* The associated section has a fixed base associated with it.
     * >> We don't need to emit a relocation, because
     *    we can simply add the base value here! */
    xval += (target_off_t)xsym->sy_addr;
    xval += (target_off_t)DCCSection_BASE(DCCSym_SECTION(xsym));
    rel   = DCC_R_NONE; /* Place an empty relocation for dependency mapping. */
   }
  }
  /* Generate a relocation at the current address.
   * >> This relocation must later add its base to the symbol. */
  t_putrel(rel,xsym);
 }
 return xval;
}
LOCAL target_off_t
DCCDisp_PutDispRel(struct DCCSymAddr const *__restrict addr, rel_t rel) {
 struct DCCSym *xsym; target_off_t xval;
 assert(addr);
 xval = addr->sa_off;
 if ((xsym = addr->sa_sym) != NULL) {
  struct DCCSymAddr xsymaddr;
  if (DCCSym_LoadAddr(xsym,&xsymaddr,0)) {
   xval += (target_off_t)xsymaddr.sa_off;
   xsym  = xsymaddr.sa_sym;
#if 0
   if (DCCSym_SECTION(xsym) == unit.u_curr) {
    /* Emit to current section (no relocation required). */
    xval += xsym->sy_addr;
    rel   = DCC_R_NONE; /* Place an empty relocation for dependency mapping. */
   }
#endif
  }
  t_putrel(rel,xsym);
  xval -= t_addr;
 } else {
#if 0 /* This never happens, but if it did, this would make sense! */
  if (unit.u_curr == &DCCSection_Abs) {
   /* Emit to current section (no relocation required). */
   xval += xsym->sy_addr;
   /* NOTE: No need to create an empty relocation for symbols in the ABS section! */
  } else
#endif
  {
   /* Because this is a DISP, we must link against the global section. */
   t_putrel(rel,&DCCSection_Abs.sc_start);
  }
  xval -= t_addr;
 }
 return xval;
}


PRIVATE void DCCDisp_SymAddr8(struct DCCSymAddr const *__restrict expr) { t_putb((uint8_t)DCCDisp_PutAddrRel(expr,DCC_R_DATA_8)); }
PRIVATE void DCCDisp_SymDisp8(struct DCCSymAddr const *__restrict expr) { t_putb((uint8_t)((int8_t)DCCDisp_PutDispRel(expr,DCC_R_DISP_8)-1)); }
PRIVATE void DCCDisp_SymAddr16(struct DCCSymAddr const *__restrict expr) { t_putw((uint16_t)DCCDisp_PutAddrRel(expr,DCC_R_DATA_16)); }
PRIVATE void DCCDisp_SymDisp16(struct DCCSymAddr const *__restrict expr) { t_putw((uint16_t)((int16_t)DCCDisp_PutDispRel(expr,DCC_R_DISP_16)-2)); }
PRIVATE void DCCDisp_SymAddr32(struct DCCSymAddr const *__restrict expr) { t_putl((uint32_t)DCCDisp_PutAddrRel(expr,DCC_R_DATA_32)); }
PRIVATE void DCCDisp_SymDisp32(struct DCCSymAddr const *__restrict expr) { t_putl((uint32_t)((int32_t)DCCDisp_PutDispRel(expr,DCC_R_DISP_32)-4)); }
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
PRIVATE void DCCDisp_SymAddr64(struct DCCSymAddr const *__restrict expr) { t_putq((uint64_t)DCCDisp_PutAddrRel(expr,DCC_R_DATA_64)); }
PRIVATE void DCCDisp_SymDisp64(struct DCCSymAddr const *__restrict expr) { t_putq((uint64_t)((int64_t)DCCDisp_PutDispRel(expr,DCC_R_DISP_64)-8)); }
#endif

PUBLIC void
DCCDisp_SymAddr(struct DCCSymAddr const *__restrict expr,
                width_t width) {
 assert(CHECK_WIDTH(width));
 switch (width) {
 case 1:  DCCDisp_SymAddr8(expr); break;
 case 2:  DCCDisp_SymAddr16(expr); break;
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
 case 4:  DCCDisp_SymAddr32(expr); break;
 default: DCCDisp_SymAddr64(expr); break;
#else
 default: DCCDisp_SymAddr32(expr); break;
#endif
 }
}
PUBLIC void
DCCDisp_SymDisp(struct DCCSymAddr const *__restrict expr,
                width_t width) {
 assert(CHECK_WIDTH(width));
 switch (width) {
 case 1:  DCCDisp_SymDisp8(expr); break;
 case 2:  DCCDisp_SymDisp16(expr); break;
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
 case 4:  DCCDisp_SymDisp32(expr); break;
 default: DCCDisp_SymDisp64(expr); break;
#else
 default: DCCDisp_SymDisp32(expr); break;
#endif
 }
}

LOCAL target_off_t
readw(void const *__restrict p, width_t width, int is_unsigned) {
 target_off_t result;
 assert(CHECK_WIDTH(width));
 if (is_unsigned) switch (width) {
 case 1:  result = (target_off_t)*(uint8_t  *)p; break;
 case 2:  result = (target_off_t)*(uint16_t *)p; break;
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
 case 4:  result = (target_off_t)*(uint32_t *)p; break;
 default: result = (target_off_t)*(uint64_t *)p; break;
#else
 default: result = (target_off_t)*(uint32_t *)p; break;
#endif
 } else switch (width) {
 case 1:  result = (target_off_t)*(int8_t  *)p; break;
 case 2:  result = (target_off_t)*(int16_t *)p; break;
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
 case 4:  result = (target_off_t)*(int32_t *)p; break;
 default: result = (target_off_t)*(int64_t *)p; break;
#else
 default: result = (target_off_t)*(int32_t *)p; break;
#endif
 }
 return result;
}

LOCAL void
writew(void const *__restrict p, target_off_t v, width_t width) {
 assert(CHECK_WIDTH(width));
 switch (width) {
 case 1:  *(int8_t  *)p = (int8_t)v;  break;
 case 2:  *(int16_t *)p = (int16_t)v; break;
#if DCC_TARGET_SIZEOF_IMM_MAX >= 8
 case 4:  *(int32_t *)p = (int32_t)v; break;
 default: *(int64_t *)p = (int64_t)v; break;
#else
 default: *(int32_t *)p = (int32_t)v; break;
#endif
 }
}



PRIVATE void
asm_modmem(uint8_t group, struct DCCMemLoc const *__restrict ml) {
 assert(ml);
 if (DCCMEMLOC_ISREGOFF(ml)) {
  uint8_t mod,rm_reg = ml->ml_reg&7;
  /* Select displacement encoding. */
  if (ml->ml_sym) goto rdisp32; /* Force 32-bit displacement if offset from symbol. */
  if (!ml->ml_off && rm_reg != DCC_ASMREG_EBP) mod = MODRM_MOD(B(00)); /* No displacement. */
  else if (ml->ml_off == (int8_t)ml->ml_off) mod = MODRM_MOD(B(01));       /* 8-bit, signed displacement. */
  else rdisp32: mod = MODRM_MOD(B(10)); /* 32-bit displacement. */
  /* Figure out if an SIB byte is needed. */
  t_putb((uint8_t)(mod|MODRM_REG(group)|MODRM_RM(rm_reg)));
  if (rm_reg == MODRM_SIBREGISTER) {
   /* Add the SIB byte. */
   t_putb((uint8_t)(MODRM_MOD(0)| /* shift */
                      MODRM_REG(MODRM_SIBREGISTER)| /* No index */
                      MODRM_RM(rm_reg)));
  }
  /* add offset */
       if (mod == MODRM_MOD(B(01))) t_putb((uint8_t)ml->ml_off);
  else if (mod == MODRM_MOD(B(10))) DCCDisp_SymAddr32(&ml->ml_sad);
 } else {
  /* Symbol-relative r/m argument. */
  t_putb(MODRM_DISP32(group&7));
  DCCDisp_SymAddr32(&ml->ml_sad);
 }
}
PRIVATE void
asm_modreg(uint8_t group, uint8_t reg) {
 t_putb(MODRM_REGISTER(group,reg));
}


PUBLIC void
DCCDisp_SignMirrorReg(rc_t dst) {
 uint8_t shift; /* sar $7/15/31, %dst */
 if ((dst&(DCC_RC_I16|DCC_RC_I32)) == DCC_RC_I16) t_putb(0x66);
 if (dst&DCC_RC_I16) t_putb(0xc1),shift = (dst&DCC_RC_I32) ? 31 : 15;
 else                t_putb(0xc0),shift = 7;
 asm_modreg(7,dst&7);
 t_putb(shift);
}

PUBLIC void
DCCDisp_MemSignExtendReg(struct DCCMemLoc const *__restrict src,
                         rc_t dst) {
 rc_t b8reg;
 if (dst&DCC_RC_I8) b8reg = dst&~(DCC_RC_I3264|DCC_RC_I16);
 else               b8reg = DCCVStack_GetReg(DCC_RC_I8,1);
 DCCDisp_MemMovReg(src,b8reg);   /* Load the byte at the given address. */
 DCCDisp_SignMirrorReg(b8reg);   /* Sign-extend that byte. */
 DCCDisp_RegMovReg(b8reg,dst,0); /* Sign-extend it again, and move it into 'dst'. */
}


PUBLIC void
DCCDisp_SignMirrorMem(struct DCCMemLoc const *__restrict dst,
                      target_siz_t n_bytes) {
 /* Special case: Small memory area. */
 if (CHECK_WIDTH(n_bytes)) {
  uint8_t shift;
  DCCDisp_X86Segp(dst->ml_reg);
  if (n_bytes == 2) t_putb(0x66);
  if (n_bytes >= 2) t_putb(0xc1),shift = (n_bytes == 4) ? 31 : 15;
  else              t_putb(0xc0),shift = 7;
  asm_modmem(7,dst);
  t_putb(shift);
 } else if (n_bytes != 0) {
  rc_t temp_register;
  struct DCCMemLoc msb;
  /* Fallback: Must load the highest-order byte,
   * sign-extend it, when fill the memory with that byte. */
  temp_register = DCCVStack_GetReg(DCC_RC_I8,1);
  msb           = *dst;
  msb.ml_off   += n_bytes-1;
  DCCDisp_MemMovReg(&msb,temp_register);
  DCCDisp_SignMirrorReg(temp_register);
  DCCDisp_ByrMovMem(temp_register,n_bytes,dst,n_bytes,1);
 }
}

#define EFLAGS_CF  (1 << 0) /* Carry flag */
#define EFLAGS_PF  (2 << 0) /* Parity flag */
#define EFLAGS_AF  (4 << 0) /* Adjust flag */
#define EFLAGS_ZF  (6 << 0) /* Zero flag */
#define EFLAGS_SF  (7 << 0) /* Sign flag */
#define EFLAGS_OF (11 << 0) /* Overflow flag */

PUBLIC void
DCCDisp_SetTst(test_t tst) {
 struct DCCMemLoc flgs;
 struct DCCSymAddr val;
 assert(tst <= 0xf);
 /* TODO: Special optimizations for certain cases
  *       when unused registers are available. */
 t_putb(0x9c); /* pushf */
 flgs.ml_off = 0;
 flgs.ml_reg = DCC_RR_XSP;
 flgs.ml_sym = NULL;
 val.sa_sym = NULL;
 switch (tst) {
 case DCC_TEST_O : val.sa_off =   EFLAGS_OF;  goto orf;  /* (OF=1). */
 case DCC_TEST_NO: val.sa_off = ~(EFLAGS_OF); goto andf; /* (OF=0). */
 case DCC_TEST_B : val.sa_off =   EFLAGS_CF;  goto orf;  /* (CF=1). */
 case DCC_TEST_AE: val.sa_off = ~(EFLAGS_CF); goto andf; /* (CF=0). */
 case DCC_TEST_E : val.sa_off =   EFLAGS_ZF;  goto orf;  /* (ZF=1). */
 case DCC_TEST_NE: val.sa_off = ~(EFLAGS_ZF); goto andf; /* (ZF=0). */
 case DCC_TEST_BE: val.sa_off =  (EFLAGS_CF|EFLAGS_ZF); goto orf;  /* (CF=1 or ZF=1). */
 case DCC_TEST_A : val.sa_off = ~(EFLAGS_CF|EFLAGS_ZF); goto andf; /* (CF=0 and ZF=0). */
 case DCC_TEST_S : val.sa_off =   EFLAGS_SF;  goto orf;  /* (SF=1). */
 case DCC_TEST_NS: val.sa_off = ~(EFLAGS_SF); goto andf; /* (SF=0). */
 case DCC_TEST_P : val.sa_off =  (EFLAGS_PF); goto orf;  /* (PF=1). */
 case DCC_TEST_NP: val.sa_off = ~(EFLAGS_PF); goto andf; /* (PF=0). */
 case DCC_TEST_L : val.sa_off =  (EFLAGS_SF); 
                   DCCDisp_CstBinMem('|',&val,&flgs,4,1);
                   val.sa_off = ~(EFLAGS_OF); goto andf; /* (SF<>OF). */
 case DCC_TEST_GE: val.sa_off = (EFLAGS_SF|EFLAGS_OF); goto orf; /* (SF=OF). */
 case DCC_TEST_LE: val.sa_off = (EFLAGS_ZF); goto orf; /* (ZF=1 or SF<>OF). */
 case DCC_TEST_G :
 default:          val.sa_off = ~(EFLAGS_ZF|EFLAGS_SF|EFLAGS_OF); goto andf; /* (ZF=0 and SF=OF). */
 }
 if (DCC_MACRO_FALSE) {orf:  DCCDisp_CstBinMem('|',&val,&flgs,4,1); }
 if (DCC_MACRO_FALSE) {andf: DCCDisp_CstBinMem('&',&val,&flgs,4,1); }
 t_putb(0x9d); /* popf */
}



/* As noted here: http://stackoverflow.com/questions/6776385/what-is-faster-jmp-or-string-of-nops */
static uint8_t const nopseq1[1] = {0x90};
static uint8_t const nopseq2[2] = {0x66,0x90};
static uint8_t const nopseq3[3] = {0x0f,0x1f,0x00};
static uint8_t const nopseq4[4] = {0x0f,0x1f,0x40,0x00};
static uint8_t const nopseq5[5] = {0x0f,0x1f,0x44,0x00,0x00};
static uint8_t const nopseq6[6] = {0x66,0x0f,0x1f,0x44,0x00,0x00};
static uint8_t const nopseq7[7] = {0x0f,0x1f,0x80,0x00,0x00,0x00,0x00};
static uint8_t const nopseq8[8] = {0x0f,0x1f,0x84,0x00,0x00,0x00,0x00,0x00};
static uint8_t const nopseq9[9] = {0x66,0x0f,0x1f,0x84,0x00,0x00,0x00,0x00,0x00};
static uint8_t const *const nopseq[9] = {
 nopseq1,nopseq2,nopseq3,
 nopseq4,nopseq5,nopseq6,
 nopseq7,nopseq8,nopseq9
};

PUBLIC void
DCCDisp_FillNop(void *__restrict p, size_t n_bytes) {
 if (n_bytes >= 54) {
  /* TODO: Generate a jmp (would require at least 6 instructions). */
 }
 while (n_bytes >= 9) {
  memcpy(p,nopseq9,9);
  *(uintptr_t *)&p += 9;
  n_bytes -= 9;
 }
 if (n_bytes) memcpy(p,nopseq[n_bytes-1],n_bytes);
}

#if DCC_TARGET_BIN == DCC_BINARY_PE
#   define MAX_PROLOG_SIZE  10
#else
#   define MAX_PROLOG_SIZE  9
#endif

PUBLIC void
DCCDisp_FunProlog(struct DCCDispFunction *__restrict info) {
 uint8_t *p;
 assert(info);
 info->df_proaddr = t_addr;
 if ((p = (uint8_t *)t_alloc(MAX_PROLOG_SIZE)) != NULL) {
#if DCC_CONFIG_HAVE_DRT
  if (DRT_ENABLED()) {
   uint8_t *iter = p;
   /* Allocate a large stack-frame by default. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
   struct DCCSymAddr chkstk;
   if ((chkstk.sa_sym = DCCUnit_NewSyms("__chkstk",DCC_SYMFLAG_HIDDEN)) != NULL) {
    uint8_t *saved_textptr;
    chkstk.sa_off = 0;
    *iter++ = 0xb8; /* movl $drt.rt_framesize, %eax */
    *(uint32_t *)iter = drt.rt_framesize;
    iter += 4;
    *iter++ = 0xe8; /* call */
    saved_textptr         = unit.u_tbuf.tb_pos;
    assert(saved_textptr >= unit.u_tbuf.tb_begin);
    assert(saved_textptr <= unit.u_tbuf.tb_end);
    assert(iter          >= unit.u_tbuf.tb_begin);
    assert(iter          <= unit.u_tbuf.tb_end);
    unit.u_tbuf.tb_pos    = iter;
    *(uint32_t *)iter     = (uint32_t)((int32_t)DCCDisp_PutDispRel(&chkstk,DCC_R_DISP_32)-4);
    assert(saved_textptr >= unit.u_tbuf.tb_begin);
    assert(saved_textptr <= unit.u_tbuf.tb_end);
    unit.u_tbuf.tb_pos = saved_textptr;
   } else
#endif
   {
    *iter++ = 0x50+DCC_ASMREG_EBP; /* push %ebp */
    *iter++ = 0x89,*iter++ = 0xe5; /* mov %esp, %ebp */
    *iter++ = 0x81,*iter++ = 0xec; /* sub $stack_size, %esp */
    *(uint32_t *)iter = drt.rt_framesize;
    iter += 4;
#if MAX_PROLOG_SIZE == 10
    *iter++ = DCCGEN_NOPBYTE; /* nop. */
#elif MAX_PROLOG_SIZE > 9
    DCCDisp_FillNop(iter,MAX_PROLOG_SIZE-9);
#endif
   }
  } else
#endif /* DCC_CONFIG_HAVE_DRT */
  {
#if DCC_DEBUG
   DCCDisp_FillNop(p,MAX_PROLOG_SIZE);
#endif
  }
 }

#if 0 /* HAVE_HLOG Hardcore logging! */
 if (compiler.c_fun && strcmp(compiler.c_fun->d_name->k_name,"__hardlog") != 0) {
  struct DCCMemLoc loc;
  loc.ml_sym = DCCUnit_NewSyms("__hardlog",DCC_SYMFLAG_NONE);
  if (loc.ml_sym) loc.ml_off = 0,loc.ml_reg = DCC_RC_CONST,DCCDisp_LocCll(&loc);
 }
#endif
#if 0 /* Hardcore debugging! */
 { struct DCCMemLoc loc;
   loc.ml_sym = DCCUnit_NewSyms("_CrtCheckMemory",DCC_SYMFLAG_DLLIMPORT);
   if (loc.ml_sym) loc.ml_off = 0,loc.ml_reg = DCC_RC_CONST,DCCDisp_LocCll(&loc);
 }
#endif
}
PUBLIC void
DCCDisp_GenProlog(struct DCCDispFunction *__restrict info) {
 uint8_t *prologue;
 target_ptr_t stack_size;
 /* Write the prologue. */
 assert(info);
 stack_size = compiler.c_hwstack.hws_maxoffset;
 prologue = unit.u_tbuf.tb_begin+info->df_proaddr;
#if DCC_TARGET_BIN == DCC_BINARY_PE
 if (stack_size >= DCC_TARGET_PAGESIZE ||
    ((linker.l_flags&DCC_LINKER_FLAG_GENDEBUG) && stack_size)) {
  struct DCCSymAddr chkstk;
  /* Call '__chkstk' to allocate large amounts of stack memory.
   * NOTE: Also call it for non-empty allocations in debug-mode. */
  if ((chkstk.sa_sym = DCCUnit_NewSyms("__chkstk",DCC_SYMFLAG_HIDDEN)) != NULL) {
   uint8_t *saved_textptr;
   chkstk.sa_off = 0;
   *prologue++ = 0xb8; /* movl $stack_size, %eax */
   /* Make sure that EAX is at least __SIZEOF_POINTER__ when __chkstk is called.
    * This, alongside aligning the stackframe-size by pointers,
    * makes it much easier to view stack memory in a debugger. */
   stack_size +=  (DCC_TARGET_SIZEOF_POINTER-1);
   stack_size &= ~(DCC_TARGET_SIZEOF_POINTER-1);

   *(uint32_t *)prologue = (uint32_t)stack_size;
   prologue += 4;
   *prologue++ = 0xe8; /* call */
   /* This looks kind of complicated, but simply
    * generates a DISP relocation to 'chkstk'. */
   saved_textptr         = unit.u_tbuf.tb_pos;
   assert(saved_textptr >= unit.u_tbuf.tb_begin);
   assert(saved_textptr <= unit.u_tbuf.tb_end);
   assert(prologue      >= unit.u_tbuf.tb_begin);
   assert(prologue      <= unit.u_tbuf.tb_end);
   unit.u_tbuf.tb_pos    = prologue;
   *(uint32_t *)prologue = (uint32_t)((int32_t)DCCDisp_PutDispRel(&chkstk,DCC_R_DISP_32)-4);
   assert(saved_textptr >= unit.u_tbuf.tb_begin);
   assert(saved_textptr <= unit.u_tbuf.tb_end);
   unit.u_tbuf.tb_pos = saved_textptr;
   return;
  }
 }
#endif
 *prologue++ = 0x50+DCC_ASMREG_EBP;     /* push %ebp */
 *prologue++ = 0x89,*prologue++ = 0xe5; /* mov %esp, %ebp */
 if (stack_size) {
  *prologue++ = 0x81,*prologue++ = 0xec; /* sub $stack_size, %esp */
  *(uint32_t *)prologue = stack_size;
#if MAX_PROLOG_SIZE == 10
  *prologue++ = DCCGEN_NOPBYTE; /* nop. */
#elif MAX_PROLOG_SIZE > 9
  DCCDisp_FillNop(prologue,MAX_PROLOG_SIZE-9);
#endif
 } else {
  /* Fill the rest with NOPs (Still faster than 'sub $0, %esp'...) */
  DCCDisp_FillNop(prologue,MAX_PROLOG_SIZE-3);
 }
#if DCC_CONFIG_HAVE_DRT
 if (DRT_ENABLED()) {
  /* TODO: Delete any mapping for 'info->df_proaddr...+=MAX_PROLOG_SIZE'
   *       in 'unit.u_curr->sc_dat.sd_rt.rs_mirror', as well as somehow
   *       get DRT to mirror those changes during the next synchronization
   *       point in the event that changes have been made. */
 }
#endif /* DCC_CONFIG_HAVE_DRT */
}
PUBLIC void DCCDisp_Ret(void) { t_putb(0xc3); }
PUBLIC void DCCDisp_FunEpilog(void) {
 /* Write the epilogue. */
 t_putb(0xc9); /* leave */
 t_putb(0xc3); /* ret */
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "gen-bin.c.inl"
#include "gen-jmp.c.inl"
#include "gen-large.c.inl"
#include "gen-misc.c.inl"
#include "gen-mov.c.inl"
#include "gen-one.c.inl"
#endif

#endif /* !GUARD_DCC_GEN_C */
