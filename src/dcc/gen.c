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

#include <string.h>

#include "x86_util.h"

DCC_DECL_BEGIN

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
   if (update_ptr && ((uint8_t *)*update_ptr >= target_sec->sc_text.tb_begin &&
                      (uint8_t *)*update_ptr <= target_sec->sc_text.tb_end)) {
    *(uintptr_t *)update_ptr -= (uintptr_t)target_sec->sc_text.tb_begin;
    target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off+
                                                symaddr.sa_sym->sy_addr+
                                                l->ml_off,n_bytes);
    *(uintptr_t *)update_ptr += (uintptr_t)target_sec->sc_text.tb_begin;
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
   if (update_ptr && ((uint8_t *)*update_ptr >= target_sec->sc_text.tb_begin &&
                      (uint8_t *)*update_ptr <= target_sec->sc_text.tb_end)) {
    *(uintptr_t *)update_ptr -= (uintptr_t)target_sec->sc_text.tb_begin;
    target_data = (uint8_t *)DCCSection_GetText(target_sec,symaddr.sa_off,n_bytes);
    *(uintptr_t *)update_ptr += (uintptr_t)target_sec->sc_text.tb_begin;
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
   text->ct_relv = DCCSection_Getrel(target_sec,symaddr.sa_off,
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
DCCDisp_SignExtendReg(rc_t dst) {
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
 DCCDisp_SignExtendReg(b8reg);   /* Sign-extend that byte. */
 DCCDisp_RegMovReg(b8reg,dst,0); /* Sign-extend it again, and move it into 'dst'. */
}


PUBLIC void
DCCDisp_SignExtendMem(struct DCCMemLoc const *__restrict dst,
                      target_siz_t n_bytes) {
 /* Special case: Small memory area. */
 if (CHECK_WIDTH(n_bytes)) {
  uint8_t shift;
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
  DCCDisp_SignExtendReg(temp_register);
  DCCDisp_ByrMovMem(temp_register,n_bytes,dst,n_bytes,1);
 }
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

#define PROLOG_SIZE  9

PUBLIC void
DCCDisp_FunProlog(struct DCCDispFunction *__restrict info) {
 assert(info);
 info->df_proaddr = t_addr;
#if DCC_DEBUG
 {
  void *p = t_alloc(PROLOG_SIZE);
  if (p) DCCDisp_FillNop(p,PROLOG_SIZE);
 }
#else
 t_alloc(PROLOG_SIZE);
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
 *prologue++ = 0x50+DCC_ASMREG_EBP;     /* push %ebp */
 *prologue++ = 0x89,*prologue++ = 0xe5; /* mov %esp, %ebp */
 if (stack_size) {
  *prologue++ = 0x81,*prologue++ = 0xec; /* sub $stack_size, %esp */
  *(uint32_t *)prologue = stack_size;
 } else {
  /* FIll the rest with NOPs (Still faster than 'sub $0, %esp'...) */
  DCCDisp_FillNop(prologue,6);
 }
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
