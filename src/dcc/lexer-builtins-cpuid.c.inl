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
#ifndef GUARD_DCC_LEXER_BUILTINS_CPUID_C_INL
#define GUARD_DCC_LEXER_BUILTINS_CPUID_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/vstack.h>
#include <dcc/compiler.h>
#include <dcc/gen.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

INTDEF size_t /* from 'tpp.c' */
fuzzy_match(char const *__restrict a, size_t alen,
            char const *__restrict b, size_t blen);

#if DCC_TARGET_HASI(I_X86)

/* Additional support for feature detection. */
#define EXT_INTEL_CPU     1
#define EXT_CPUID_HAS     1 /* __builtin_cpu_supports("cpuid") */
#define EXT_CPUID_MAX     1 /* __builtin_cpu_supports("cpuid-max") */
#define EXT_CPUFEATURES_1 1
#define EXT_CPUFEATURES_7 1

#define CPUINFO_SYMNAME                      "__cpu_info"
#define CPUINFO_SYMFLAG                     (DCC_SYMFLAG_WEAK|DCC_SYMFLAG_HIDDEN)
#define CPUINFO_GETSYM()                     DCCUnit_NewSyms(CPUINFO_SYMNAME,CPUINFO_SYMFLAG)
#define CPUINFO_SIZEOF                       36
#define CPUINFO_ALIGNOF                      4
#define CPUINFO_OFFSETOF_CPUID1_EAX          0
#define CPUINFO_OFFSETOF_CPUID1_EBX          4
#define CPUINFO_OFFSETOF_CPUID1_ECX          8
#define CPUINFO_OFFSETOF_CPUID1_EDX          12
#define CPUINFO_OFFSETOF_CPUID7_EBX          16
#define CPUINFO_OFFSETOF_CPUID7_EDX          20
#define CPUINFO_OFFSETOF_CPUID7_ECX          24
#define CPUINFO_OFFSETOF_CPUID0_EAX          28 /* NOTE: Aka. the cpuid-max field. */
#define CPUINFO_OFFSETOF_CPUID0_EBX          32
#define CPUINFO_OFFSETOF_VENDOR              CPUINFO_OFFSETOF_CPUID0_EBX
#define CPUINFO_OFFSETOF_MAXID               CPUINFO_OFFSETOF_CPUID0_EAX
#define CPUINFO_SIZEOF_VENDOR                13

/* ECX CPU features */
#define FEATURE_SHIFT_FIELD 24
#define FEATURE_MASK_FIELD  0xff000000
#define FEATURE_MASK_BIT    0x00ffffff
#define FEATURE(name,field,bit) {name,((field) << FEATURE_SHIFT_FIELD)|((bit)&FEATURE_MASK_BIT)}
struct feature { char const *f_name; uint32_t f_info; };
struct vendor { char const *v_name; uint32_t v_sign; };


#define VEND(a,b,c,d) (a|b<<8|c<<16|d<<24)
PRIVATE struct vendor const vendors[] = {
  {"intel",    VEND('G','e','n','u')}, /* Genu[ineIntel] */
  {"amd",      VEND('A','u','t','h')}, /* Auth[enticAMD] */
#if 1 /* Other physical machines. */
  {"centaur",  VEND('C','e','n','t')}, /* Cent[aurHauls] */
  {"cyrix",    VEND('C','y','r','i')}, /* Cyri[xInstead] */
  {"transmeta",VEND('T','r','a','n')}, /* Tran[smetaCPU] */
//{"transmeta",VEND('G','e','n','u')}, /* Genu[ineTMx86] */
  {"geode",    VEND('G','e','o','d')}, /* Geod[e by NSC] */
  {"nexgen",   VEND('N','e','x','G')}, /* NexG[enDriven] */
  {"rise",     VEND('R','i','s','e')}, /* Rise[RiseRise] */
  {"sis",      VEND('S','i','S',' ')}, /* SiS [SiS SiS ] */
  {"umc",      VEND('U','M','C',' ')}, /* UMC [UMC UMC ] */
  {"via",      VEND('V','I','A',' ')}, /* VIA [VIA VIA ] */
  {"vortex",   VEND('V','o','r','t')}, /* Vort[ex86 SoC] */
#endif
#if 1 /* Virtual machines. */
  {"kvm",      VEND('K','V','M',' ')}, /* KVM [KVM KVM ] */
  {"Micr",     VEND('M','i','c','r')}, /* Micr[osoft Hv] */
  {"parallels",VEND(' ','l','r','p')}, /*  lrp[epyh vr ] */
  {"vmware",   VEND('V','M','w','a')}, /* VMwa[reVMware] */
  {"xen-hvm",  VEND('X','e','n','V')}, /* XenV[MMXenVMM] */
#endif
 {NULL,0}
};


PRIVATE struct feature const features[] = {
 FEATURE("sse",CPUINFO_OFFSETOF_CPUID1_EDX,25),
 FEATURE("sse2",CPUINFO_OFFSETOF_CPUID1_EDX,26),
 FEATURE("sse3",CPUINFO_OFFSETOF_CPUID1_ECX,0),
 FEATURE("ssse3",CPUINFO_OFFSETOF_CPUID1_ECX,9),
 FEATURE("sse4.1",CPUINFO_OFFSETOF_CPUID1_ECX,19),
 FEATURE("sse4.2",CPUINFO_OFFSETOF_CPUID1_ECX,20),
 FEATURE("cmov",CPUINFO_OFFSETOF_CPUID1_EDX,15),
 FEATURE("mmx",CPUINFO_OFFSETOF_CPUID1_EDX,23),
 FEATURE("popcnt",CPUINFO_OFFSETOF_CPUID1_ECX,23),
 FEATURE("avx",CPUINFO_OFFSETOF_CPUID1_ECX,28),
 FEATURE("avx2",CPUINFO_OFFSETOF_CPUID7_EBX,5),
#if EXT_CPUFEATURES_7
 FEATURE("fsgsbase",CPUINFO_OFFSETOF_CPUID7_EBX,0),
 FEATURE("IA32_TSC_ADJUST",CPUINFO_OFFSETOF_CPUID7_EBX,1),
 FEATURE("sgx",CPUINFO_OFFSETOF_CPUID7_EBX,2),
 FEATURE("bmi1",CPUINFO_OFFSETOF_CPUID7_EBX,3),
 FEATURE("hle",CPUINFO_OFFSETOF_CPUID7_EBX,4),
 FEATURE("smep",CPUINFO_OFFSETOF_CPUID7_EBX,7),
 FEATURE("bmi2",CPUINFO_OFFSETOF_CPUID7_EBX,8),
 FEATURE("erms",CPUINFO_OFFSETOF_CPUID7_EBX,9),
 FEATURE("invpcid",CPUINFO_OFFSETOF_CPUID7_EBX,10),
 FEATURE("rtm",CPUINFO_OFFSETOF_CPUID7_EBX,11),
 FEATURE("pqm",CPUINFO_OFFSETOF_CPUID7_EBX,12),
 FEATURE("FPU_CS_DS_DEPRECATED",CPUINFO_OFFSETOF_CPUID7_EBX,13),
 FEATURE("mpx",CPUINFO_OFFSETOF_CPUID7_EBX,14),
 FEATURE("pqe",CPUINFO_OFFSETOF_CPUID7_EBX,15),
 FEATURE("avx512f",CPUINFO_OFFSETOF_CPUID7_EBX,16),
 FEATURE("avx512dq",CPUINFO_OFFSETOF_CPUID7_EBX,17),
 FEATURE("rdseed",CPUINFO_OFFSETOF_CPUID7_EBX,18),
 FEATURE("adx",CPUINFO_OFFSETOF_CPUID7_EBX,19),
 FEATURE("smap",CPUINFO_OFFSETOF_CPUID7_EBX,20),
 FEATURE("avx512ifma",CPUINFO_OFFSETOF_CPUID7_EBX,21),
 FEATURE("pcommit",CPUINFO_OFFSETOF_CPUID7_EBX,22),
 FEATURE("clflushopt",CPUINFO_OFFSETOF_CPUID7_EBX,23),
 FEATURE("clwb",CPUINFO_OFFSETOF_CPUID7_EBX,24),
 FEATURE("intel_pt",CPUINFO_OFFSETOF_CPUID7_EBX,25),
 FEATURE("avx512pf",CPUINFO_OFFSETOF_CPUID7_EBX,26),
 FEATURE("avx512er",CPUINFO_OFFSETOF_CPUID7_EBX,27),
 FEATURE("avx512cd",CPUINFO_OFFSETOF_CPUID7_EBX,28),
 FEATURE("sha",CPUINFO_OFFSETOF_CPUID7_EBX,29),
 FEATURE("avx512bw",CPUINFO_OFFSETOF_CPUID7_EBX,30),
 FEATURE("avx512vl",CPUINFO_OFFSETOF_CPUID7_EBX,31),
 FEATURE("prefetchwt1",CPUINFO_OFFSETOF_CPUID7_ECX,0),
 FEATURE("avx512vbmi",CPUINFO_OFFSETOF_CPUID7_ECX,1),
 FEATURE("umip",CPUINFO_OFFSETOF_CPUID7_ECX,2),
 FEATURE("pku",CPUINFO_OFFSETOF_CPUID7_ECX,3),
 FEATURE("ospke",CPUINFO_OFFSETOF_CPUID7_ECX,4),
 FEATURE("avx512vpopcntdq",CPUINFO_OFFSETOF_CPUID7_ECX,14),
 FEATURE("rdpid",CPUINFO_OFFSETOF_CPUID7_ECX,22),
 FEATURE("avx512_4vnniw",CPUINFO_OFFSETOF_CPUID7_EDX,2),
 FEATURE("avx512_4fmaps",CPUINFO_OFFSETOF_CPUID7_EDX,3),
#endif
#if EXT_CPUFEATURES_1
 FEATURE("fpu",CPUINFO_OFFSETOF_CPUID1_EDX,0),
 FEATURE("vme",CPUINFO_OFFSETOF_CPUID1_EDX,1),
 FEATURE("de",CPUINFO_OFFSETOF_CPUID1_EDX,2),
 FEATURE("pse",CPUINFO_OFFSETOF_CPUID1_EDX,3),
 FEATURE("tsc",CPUINFO_OFFSETOF_CPUID1_EDX,4),
 FEATURE("msr",CPUINFO_OFFSETOF_CPUID1_EDX,5),
 FEATURE("pae",CPUINFO_OFFSETOF_CPUID1_EDX,6),
 FEATURE("mce",CPUINFO_OFFSETOF_CPUID1_EDX,7),
 FEATURE("cx8",CPUINFO_OFFSETOF_CPUID1_EDX,8),
 FEATURE("apic",CPUINFO_OFFSETOF_CPUID1_EDX,9),
 FEATURE("sep",CPUINFO_OFFSETOF_CPUID1_EDX,11),
 FEATURE("mtrr",CPUINFO_OFFSETOF_CPUID1_EDX,12),
 FEATURE("pge",CPUINFO_OFFSETOF_CPUID1_EDX,13),
 FEATURE("mca",CPUINFO_OFFSETOF_CPUID1_EDX,14),
 FEATURE("pat",CPUINFO_OFFSETOF_CPUID1_EDX,16),
 FEATURE("pse-36",CPUINFO_OFFSETOF_CPUID1_EDX,17),
 FEATURE("psn",CPUINFO_OFFSETOF_CPUID1_EDX,18),
 FEATURE("clfsh",CPUINFO_OFFSETOF_CPUID1_EDX,19),
 FEATURE("ds",CPUINFO_OFFSETOF_CPUID1_EDX,21),
 FEATURE("acpi",CPUINFO_OFFSETOF_CPUID1_EDX,22),
 FEATURE("mmx",CPUINFO_OFFSETOF_CPUID1_EDX,23),
 FEATURE("fxsr",CPUINFO_OFFSETOF_CPUID1_EDX,24),
 FEATURE("ss",CPUINFO_OFFSETOF_CPUID1_EDX,27),
 FEATURE("htt",CPUINFO_OFFSETOF_CPUID1_EDX,28),
 FEATURE("tm",CPUINFO_OFFSETOF_CPUID1_EDX,29),
 FEATURE("ia64",CPUINFO_OFFSETOF_CPUID1_EDX,30),
 FEATURE("pbe",CPUINFO_OFFSETOF_CPUID1_EDX,31),
 FEATURE("pclmulqdq",CPUINFO_OFFSETOF_CPUID1_ECX,1),
 FEATURE("dtes64",CPUINFO_OFFSETOF_CPUID1_ECX,2),
 FEATURE("monitor",CPUINFO_OFFSETOF_CPUID1_ECX,3),
 FEATURE("ds-cpl",CPUINFO_OFFSETOF_CPUID1_ECX,4),
 FEATURE("vmx",CPUINFO_OFFSETOF_CPUID1_ECX,5),
 FEATURE("smx",CPUINFO_OFFSETOF_CPUID1_ECX,6),
 FEATURE("est",CPUINFO_OFFSETOF_CPUID1_ECX,7),
 FEATURE("tm2",CPUINFO_OFFSETOF_CPUID1_ECX,8),
 FEATURE("cnxt-id",CPUINFO_OFFSETOF_CPUID1_ECX,10),
 FEATURE("sdbg",CPUINFO_OFFSETOF_CPUID1_ECX,11),
 FEATURE("fma",CPUINFO_OFFSETOF_CPUID1_ECX,12),
 FEATURE("cx16",CPUINFO_OFFSETOF_CPUID1_ECX,13),
 FEATURE("xtpr",CPUINFO_OFFSETOF_CPUID1_ECX,14),
 FEATURE("pdcm",CPUINFO_OFFSETOF_CPUID1_ECX,15),
 FEATURE("pcid",CPUINFO_OFFSETOF_CPUID1_ECX,17),
 FEATURE("dca",CPUINFO_OFFSETOF_CPUID1_ECX,18),
 FEATURE("x2apic",CPUINFO_OFFSETOF_CPUID1_ECX,21),
 FEATURE("movbe",CPUINFO_OFFSETOF_CPUID1_ECX,22),
 FEATURE("popcnt",CPUINFO_OFFSETOF_CPUID1_ECX,23),
 FEATURE("tsc-deadline",CPUINFO_OFFSETOF_CPUID1_ECX,24),
 FEATURE("aes",CPUINFO_OFFSETOF_CPUID1_ECX,25),
 FEATURE("xsave",CPUINFO_OFFSETOF_CPUID1_ECX,26),
 FEATURE("osxsave",CPUINFO_OFFSETOF_CPUID1_ECX,27),
 FEATURE("avx",CPUINFO_OFFSETOF_CPUID1_ECX,28),
 FEATURE("f16c",CPUINFO_OFFSETOF_CPUID1_ECX,29),
 FEATURE("rdrnd",CPUINFO_OFFSETOF_CPUID1_ECX,30),
 FEATURE("hypervisor",CPUINFO_OFFSETOF_CPUID1_ECX,31),
#endif
 {NULL,0}
};

#define EAX  (DCC_RC_I8|DCC_RC_I16|DCC_RC_I32|DCC_ASMREG_EAX)
#define EBX  (DCC_RC_I8|DCC_RC_I16|DCC_RC_I32|DCC_ASMREG_EBX)
#define ECX  (DCC_RC_I8|DCC_RC_I16|DCC_RC_I32|DCC_ASMREG_ECX)
#define EDX  (DCC_RC_I8|DCC_RC_I16|DCC_RC_I32|DCC_ASMREG_EDX)

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinCPUInit(void) {
 struct DCCSym *nocpuid;
 struct DCCSymAddr val;
 struct DCCMemLoc info;
 /* void __builtin_cpu_init(void); */
 assert(TOK == KWD___builtin_cpu_init);
 YIELD();
 DCCParse_ParPairBegin();
 info.ml_reg = DCC_RC_CONST;
 if unlikely(compiler.c_flags&DCC_COMPILER_FLAG_NOCGEN) goto end;
 if unlikely((info.ml_sym = CPUINFO_GETSYM()) == NULL) goto end;
 if unlikely((nocpuid = DCCUnit_AllocSym()) == NULL) goto end;
 /* Make sure memory has been allocated for the symbol. */
 if (DCCSym_ISFORWARD(info.ml_sym)) {
  target_ptr_t addr;
  addr = DCCSection_DAlloc(unit.u_bss,CPUINFO_SIZEOF,CPUINFO_ALIGNOF,0);
  DCCSym_Define(info.ml_sym,unit.u_bss,addr,CPUINFO_SIZEOF,CPUINFO_ALIGNOF);
 }
 val.sa_sym = NULL;

 /* Check if CPU info was already initialized. */
 val.sa_off = 0;
 info.ml_off = CPUINFO_OFFSETOF_CPUID0_EAX;
 DCCDisp_CstBinMem('?',&val,&info,4,1);
 DCCDisp_SymJcc(DCC_TEST_NE,nocpuid);

#define CPUID_BIT (1 << 21)

 /* Kill an backup registers that will be modified by 'cpuid' */
 DCCVStack_KillTst();
 DCCVStack_KillAll(0);
 /* Test for 'CPUID' */
 t_putb(0x9c);                /* pushfd */
 t_putb(0x58+DCC_ASMREG_EAX); /* pop %eax */
 val.sa_off = (target_off_t)CPUID_BIT;
 DCCDisp_CstBinReg('^',&val,EAX,1); /* Flip the CPUID bit. */
 t_putb(0x50+DCC_ASMREG_EAX); /* push %eax */
 t_putb(0x9d);                /* popfd */
 t_putb(0x9c);                /* pushfd */
 t_putb(0x58+DCC_ASMREG_ECX); /* pop %ecx */
 /* When the 'CPUID_BIT' of 'EAX' and 'ECX' is equal, cpuid is supported. */
 DCCDisp_CstBinReg('&',&val,EAX,1);
 DCCDisp_CstBinReg('&',&val,ECX,1);
 DCCDisp_RegBinReg('?',ECX,EAX,1);
 DCCDisp_SymJcc(DCC_TEST_NE,nocpuid); /* Don't invoke 'cpuid' when it's not available. */

 /* Actually initialize the symbol. */
 DCCDisp_RegPush(DCC_RR_XBX);

 /* Invoke cpuid for level '0x0' */
 DCCDisp_IntMovReg(0x0,EAX);
 t_putb(0x0f),t_putb(0xa2); /* cpuid */
 info.ml_off = CPUINFO_OFFSETOF_CPUID0_EAX,DCCDisp_RegMovMem(EAX,&info);
 info.ml_off = CPUINFO_OFFSETOF_CPUID0_EBX,DCCDisp_RegMovMem(EBX,&info);

 /* Invoke cpuid for level '0x1' */
 DCCDisp_IntMovReg(0x1,EAX);
 t_putb(0x0f),t_putb(0xa2); /* cpuid */
 info.ml_off = CPUINFO_OFFSETOF_CPUID1_EAX,DCCDisp_RegMovMem(EAX,&info);
 info.ml_off = CPUINFO_OFFSETOF_CPUID1_EBX,DCCDisp_RegMovMem(EBX,&info);
 info.ml_off = CPUINFO_OFFSETOF_CPUID1_EDX,DCCDisp_RegMovMem(EDX,&info);
 info.ml_off = CPUINFO_OFFSETOF_CPUID1_ECX,DCCDisp_RegMovMem(ECX,&info);

 /* Make sure that level #7 is available. */
 val.sa_off = 7;
 DCCDisp_CstBinReg('?',&val,EAX,1);
 DCCDisp_SymJcc(DCC_TEST_B,nocpuid);

 /* Invoke cpuid for level '0x7' */
 DCCDisp_IntMovReg(0x7,EAX);
 t_putb(0x0f),t_putb(0xa2); /* cpuid */
 info.ml_off = CPUINFO_OFFSETOF_CPUID7_EBX,DCCDisp_RegMovMem(EBX,&info);
 info.ml_off = CPUINFO_OFFSETOF_CPUID7_EDX,DCCDisp_RegMovMem(EDX,&info);
 info.ml_off = CPUINFO_OFFSETOF_CPUID7_ECX,DCCDisp_RegMovMem(ECX,&info);

 /* Invoke more levels here. */

 DCCDisp_PopReg(DCC_RR_XBX);
 t_defsym(nocpuid);
end:
 DCCParse_ParPairEnd();
 vpushv();
}

enum cpu_model {
 CPU_UNKNOWN,
 CPU_INTEL = 0x10,
 CPU_INTEL_PENTIUM,
 CPU_INTEL_ATOM,
 CPU_INTEL_CORE2,
 CPU_INTEL_COREI7,
 CPU_INTEL_NEHALEM,
 CPU_INTEL_WESTMERE,
 CPU_INTEL_SANDYBRIDGE,
#if EXT_INTEL_CPU
 CPU_INTEL_BONELL,
 CPU_INTEL_SILVERMONT,
 CPU_INTEL_KNL,
 CPU_INTEL_IVYBRIDGE,
 CPU_INTEL_HASWELL,
 CPU_INTEL_BROADWELL,
 CPU_INTEL_SKYLAKE,
 CPU_INTEL_SKYLAKE_AVX512,
#endif /* EXT_INTEL_CPU */
 CPU_AMD = 0x40,
 CPU_AMD_AMDFAM10H,
 CPU_AMD_BARCELONA,
 CPU_AMD_SHANGHAI,
 CPU_AMD_ISTANBUL,
 CPU_AMD_BTVER1,
 CPU_AMD_AMDFAM15H,
 CPU_AMD_BDVER1,
 CPU_AMD_BDVER2,
 CPU_AMD_BDVER3,
 CPU_AMD_BTVER2,
};

struct modelname { enum cpu_model m_cpu; char m_name[16]; };
PRIVATE struct modelname const cpu_model_names[] = {
 {CPU_INTEL_PENTIUM,       "pentium"},
 {CPU_INTEL_ATOM,          "atom"},
 {CPU_INTEL_CORE2,         "core2"},
 {CPU_INTEL_COREI7,        "corei7"},
 {CPU_INTEL_NEHALEM,       "nehalem"},
 {CPU_INTEL_WESTMERE,      "westmere"},
 {CPU_INTEL_SANDYBRIDGE,   "sandybridge"},
#if EXT_INTEL_CPU
 {CPU_INTEL_BONELL,        "bonell"},
 {CPU_INTEL_SILVERMONT,    "silvermont"},
 {CPU_INTEL_KNL,           "knl"},
 {CPU_INTEL_IVYBRIDGE,     "ivybridge"},
 {CPU_INTEL_HASWELL,       "haswell"},
 {CPU_INTEL_BROADWELL,     "broadwell"},
 {CPU_INTEL_SKYLAKE,       "skylake"},
 {CPU_INTEL_SKYLAKE_AVX512,"skylake-avx512"},
#endif
 {CPU_AMD_AMDFAM10H,       "amdfam10h"},
 {CPU_AMD_BARCELONA,       "barcelona"},
 {CPU_AMD_SHANGHAI,        "shanghai"},
 {CPU_AMD_ISTANBUL,        "istanbul"},
 {CPU_AMD_BTVER1,          "btver1"},
 {CPU_AMD_AMDFAM15H,       "amdfam15h"},
 {CPU_AMD_BDVER1,          "bdver1"},
 {CPU_AMD_BDVER2,          "bdver2"},
 {CPU_AMD_BDVER3,          "bdver3"},
 {CPU_AMD_BTVER2,          "btver2"},
 {CPU_UNKNOWN,{0}},
};


struct model { enum cpu_model m_cpu; uint8_t const *m_numv; };
PRIVATE uint8_t const numv_intel_atom[]   = {0xc1,0x62,0}; /* ATOM */
PRIVATE uint8_t const numv_intel_core2[]  = {0x71,0xd1,0xf0,0};
PRIVATE uint8_t const numv_intel_corei7[] = {0xa1,0xe1,0xf1,0xe2,0x52,0xc2,
                                             0xf2,0xa2,0xd2,0xa3,0xe3,0x64,
                                             0x54,0xf3,0xc3,0x65,0xf4,0x74,
                                             0xd3,0xe5,0xe4,0x55,0};
PRIVATE uint8_t const numv_intel_nehalem[]     = {0xa1,0xe1,0xf1,0xe2,0};
PRIVATE uint8_t const numv_intel_westmere[]    = {0x52,0xc2,0xf2,0};
PRIVATE uint8_t const numv_intel_sandybridge[] = {0xa2,0xd2,0};
#if EXT_INTEL_CPU
PRIVATE uint8_t const numv_intel_bonell[]         = {0xc1,0x62,0};
PRIVATE uint8_t const numv_intel_silvermont[]     = {0x7c,0xa4,0xd4,0xa5,0xd5,0};
PRIVATE uint8_t const numv_intel_knl[]            = {0x75,0};
PRIVATE uint8_t const numv_intel_ivybridge[]      = {0xa3,0xe3,0};
PRIVATE uint8_t const numv_intel_haswell[]        = {0x64,0x54,0xf3,0xc3,0};
PRIVATE uint8_t const numv_intel_broadwell[]      = {0x65,0xf4,0x74,0xd3,0};
PRIVATE uint8_t const numv_intel_skylake[]        = {0xe5,0xe4,0};
PRIVATE uint8_t const numv_intel_skylake_avx512[] = {0x55,0};
#endif /* EXT_INTEL_CPU */
PRIVATE struct model const intel_model_nums[] = {
 {CPU_INTEL_ATOM,          numv_intel_atom},
 {CPU_INTEL_CORE2,         numv_intel_core2},
 {CPU_INTEL_COREI7,        numv_intel_corei7},
 {CPU_INTEL_NEHALEM,       numv_intel_nehalem},
 {CPU_INTEL_WESTMERE,      numv_intel_westmere},
 {CPU_INTEL_SANDYBRIDGE,   numv_intel_sandybridge},
#if EXT_INTEL_CPU
 {CPU_INTEL_BONELL,        numv_intel_bonell},
 {CPU_INTEL_SILVERMONT,    numv_intel_silvermont},
 {CPU_INTEL_KNL,           numv_intel_knl},
 {CPU_INTEL_IVYBRIDGE,     numv_intel_ivybridge},
 {CPU_INTEL_HASWELL,       numv_intel_haswell},
 {CPU_INTEL_BROADWELL,     numv_intel_broadwell},
 {CPU_INTEL_SKYLAKE,       numv_intel_skylake},
 {CPU_INTEL_SKYLAKE_AVX512,numv_intel_skylake_avx512},
#endif /* EXT_INTEL_CPU */
 {CPU_UNKNOWN,NULL},
};


LOCAL char const *
likely_model_or_vendor(char const *name, size_t name_size) {
 char const *result = NULL;
 struct vendor const *viter = vendors;
 struct modelname const *miter = cpu_model_names;
 size_t distance = (size_t)-1,new_distance;
 for (; viter->v_name; ++viter) {
  new_distance = fuzzy_match(viter->v_name,strlen(viter->v_name),
                             name,name_size);
  if (new_distance < distance) {
   result   = viter->v_name;
   distance = new_distance;
  }
 }
 for (; miter->m_cpu != CPU_UNKNOWN; ++miter) {
  new_distance = fuzzy_match(miter->m_name,strlen(miter->m_name),
                             name,name_size);
  if (new_distance < distance) {
   result   = miter->m_name;
   distance = new_distance;
  }
 }
 return result;
}

LOCAL char const *
likely_feature(char const *name, size_t name_size) {
 char const *result = NULL;
 struct feature const *iter = features;
 size_t distance = (size_t)-1,new_distance;
 for (; iter->f_name; ++iter) {
  new_distance = fuzzy_match(iter->f_name,strlen(iter->f_name),
                             name,name_size);
  if (new_distance < distance) {
   result   = iter->f_name;
   distance = new_distance;
  }
 }
#define CHECK_FEATURE(s) \
 { char const *_s = (s); \
   new_distance = fuzzy_match(_s,DCC_COMPILER_STRLEN(s),name,name_size); \
   if (new_distance < distance) { \
    result   = _s; \
    distance = new_distance; \
   } \
 }
#if EXT_CPUID_HAS
 CHECK_FEATURE("cpuid");
#endif /* EXT_CPUID_HAS */
#if EXT_CPUID_MAX
 CHECK_FEATURE("cpuid-max");
#endif /* EXT_CPUID_MAX */
#undef CHECK_FEATURE
 return result;
}



PRIVATE void query_model(enum cpu_model model) {
 rc_t result;
 struct DCCSym *sym_done;
 struct DCCMemLoc cpuinfo;
 struct DCCSymAddr val;
 result = DCCVStack_GetReg(DCC_RC_I8,1);
 DCCDisp_IntMovReg(0,result);
 sym_done = DCCUnit_AllocSym();
 if unlikely(!sym_done) goto done;
 cpuinfo.ml_reg = DCC_RC_CONST;
 cpuinfo.ml_sym = CPUINFO_GETSYM();
 cpuinfo.ml_off = CPUINFO_OFFSETOF_VENDOR;
 /* Step #1: Confirm the CPU signature. */
 val.sa_off = (model >= CPU_AMD)
  ? VEND('A','u','t','h')
  : VEND('G','e','n','u');
 val.sa_sym = NULL;
 DCCDisp_CstBinMem('?',&val,&cpuinfo,4,1);
 DCCDisp_SymJcc(DCC_TEST_NE,sym_done);
 if (model >= CPU_AMD) {
  /* TODO */
 } else {
  rc_t temp,temp2;
  struct model const *model_info;
  uint8_t const *model_numbers;
  struct DCCSym *has_number;

  /* Make sure that 'CPUINFO_OFFSETOF_CPUID1_EBX & 0xff == 0' */
  val.sa_off     = 0xff;
  cpuinfo.ml_off = CPUINFO_OFFSETOF_CPUID1_EBX;
  DCCDisp_CstBinMem('t',&val,&cpuinfo,4,1);
  DCCDisp_SymJcc(DCC_TEST_NE,sym_done);
  /* Make sure that '(CPUINFO_OFFSETOF_CPUID1_EAX >> 8) & 0x0f == 0x06' */
  temp  = DCCVStack_GetRegOf(DCC_RC_I8,(uint8_t)~(1 << (result&DCC_RI_MASK)));
  cpuinfo.ml_off = CPUINFO_OFFSETOF_CPUID1_EAX+1;
  DCCDisp_MemMovReg(&cpuinfo,temp);
  val.sa_off = 0x0f,DCCDisp_CstBinReg('&',&val,temp,1);
  if (model == CPU_INTEL_PENTIUM) {
   val.sa_off = 0x05,DCCDisp_CstBinReg('?',&val,temp,1);
   DCCDisp_SccReg(DCC_TEST_E,result);
   goto done;
  }
  val.sa_off = 0x06,DCCDisp_CstBinReg('?',&val,temp,1);
  DCCDisp_SymJcc(DCC_TEST_NE,sym_done);
  /* Load the model number. */
  cpuinfo.ml_off = CPUINFO_OFFSETOF_CPUID1_EAX;
  DCCDisp_MemMovReg(&cpuinfo,temp);
  val.sa_off = 0xf0,DCCDisp_CstBinReg('&',&val,temp,1);
  temp2 = DCCVStack_GetRegOf(DCC_RC_I8,(uint8_t)~((1 << (result&DCC_RI_MASK))|
                                                  (1 << (temp&DCC_RI_MASK))));
  cpuinfo.ml_off = CPUINFO_OFFSETOF_CPUID1_EAX+2;
  DCCDisp_MemMovReg(&cpuinfo,temp2);
  val.sa_off = 0x0f,DCCDisp_CstBinReg('&',&val,temp2,1);
  DCCDisp_RegBinReg('|',temp2,temp,1);
  /* The model number is now stored in 'temp'. */

  model_info = intel_model_nums;
  while (model_info->m_cpu != model)
       ++model_info,assert(model_info->m_numv);
  model_numbers = model_info->m_numv;
  assert(model_numbers);
  /* Check if any of the known model numbers apply. */
  if ((has_number = DCCUnit_AllocSym()) != NULL) {
   for (; *model_numbers; ++model_numbers) {
    val.sa_off = (target_off_t)*model_numbers;
    DCCDisp_CstBinReg('?',&val,temp,1);
    DCCDisp_SymJcc(DCC_TEST_E,has_number);
   }
   DCCDisp_SymJmp(sym_done);
   t_defsym(has_number);
   /* goti! */
   DCCDisp_IntMovReg(1,result);
  }
 }
 t_defsym(sym_done);
done:
 vpushr(result);
}


LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinCPUQuery(void) {
 int query_feature; tok_t info_mode = '&';
 struct TPPString *info_name;
 struct DCCStackValue info_slot;
 uint32_t info_mask;
 info_slot.sv_sym          = CPUINFO_GETSYM();
 info_slot.sv_flags        = DCC_SFLAG_LVALUE|DCC_SFLAG_COPY;
 info_slot.sv_reg          = DCC_RC_CONST;
 info_slot.sv_reg2         = DCC_RC_CONST;
 info_slot.sv_ctype.t_base = NULL;
 info_slot.sv_ctype.t_type = DCCTYPE_IB4|DCCTYPE_UNSIGNED;
 /* int __builtin_cpu_is(char const *cpuname),
  *     __builtin_cpu_supports(char const *feature); */
 assert(TOK == KWD___builtin_cpu_is ||
        TOK == KWD___builtin_cpu_supports);
 query_feature = TOK == KWD___builtin_cpu_supports;
 YIELD();
 DCCParse_ParPairBegin();
 if (TPP_ISSTRING(TOK))
  info_name = DCCParse_String();
 else {
  WARN(W_BUILTIN_CPU_EXPECTED_STRING);
  info_name = NULL;
 }
 DCCParse_ParPairEnd();
 if (!info_name || !info_slot.sv_sym) goto unsupported;

#define IS_NAME(x) \
 (info_name->s_size == DCC_COMPILER_STRLEN(x) && \
 !memcmp(info_name->s_text,x,sizeof(x)-sizeof(char)))

 if (query_feature) {
  /* CPU features */
  struct feature const *iter;
  for (iter = features; iter->f_name; ++iter) {
   if (!strcmp(iter->f_name,info_name->s_text)) {
    info_mask             = (uint32_t)1 << (iter->f_info&FEATURE_MASK_BIT);
    info_slot.sv_const.it = iter->f_info >> FEATURE_SHIFT_FIELD;
    goto lookup;
   }
  }
  /* Additional/special features */
#if EXT_CPUID_HAS
  if (IS_NAME("cpuid")) {
   /* Confirm if 'cpuid' is even supported by
    * checking if we've read a vendor name. */
   info_mask             = 0xffffffff;
   info_slot.sv_const.it = CPUINFO_OFFSETOF_VENDOR;
   goto lookup;
  }
#endif /* EXT_CPUID_HAS */
#if EXT_CPUID_MAX
  if (IS_NAME("cpuid-max")) {
   /* Return the greatest allowed CPU identifier. */
   info_slot.sv_const.it = CPUINFO_OFFSETOF_MAXID;
   vpush(&info_slot);
   goto done;
  }
#endif /* EXT_CPUID_MAX */
  /* Emit a -Wquality warning suggesting a likely match. */
  WARN(W_BUILTIN_CPU_UNKNOWN_FEATURE,info_name->s_text,
       likely_feature(info_name->s_text,info_name->s_size));
 } else {
  struct vendor const *viter;
  struct modelname const *miter;
  for (viter = vendors; viter->v_name; ++viter) {
   if (!strcmp(viter->v_name,info_name->s_text)) {
    info_mask             = viter->v_sign;
    info_slot.sv_const.it = CPUINFO_OFFSETOF_CPUID0_EBX;
    info_mode             = TOK_EQUAL;
    goto lookup;
   }
  }
  for (miter = cpu_model_names; miter->m_cpu != CPU_UNKNOWN; ++miter) {
   if (!strcmp(miter->m_name,info_name->s_text)) {
    query_model(miter->m_cpu);
    goto done;
   }
  }
  /* Emit a -Wquality warning suggesting a likely match. */
  WARN(W_BUILTIN_CPU_UNKNOWN_MODEL,info_name->s_text,
       likely_model_or_vendor(info_name->s_text,info_name->s_size));
 }
unsupported:
 vpushi(DCCTYPE_IB4|DCCTYPE_UNSIGNED,0);
 goto done;
lookup:
 /* Lookup CPU information. */
 vpush(&info_slot);
 vpushi(DCCTYPE_IB4|DCCTYPE_UNSIGNED,info_mask);
 vgen2(info_mode);
 vrval();
 vwunused();
done:
 if (info_name) TPPString_Decref(info_name);
}

#define CPU_VENDORSIZE   (3*4+1)
#define CPU_BRANDSIZE  (4*3*4+1)

LEXPRIV void DCC_PARSE_CALL
DCCParse_BuiltinCPUVendor(void) {
 struct DCCStackValue val; int must_pop_return;
 struct DCCSym *nosup_sym,*done_sym;
 struct DCCMemLoc return_data;
 int want_brand = TOK == KWD___builtin_cpu_brand;
 target_siz_t string_size = want_brand ? CPU_BRANDSIZE : CPU_VENDORSIZE;
 assert(TOK == KWD___builtin_cpu_vendor ||
        TOK == KWD___builtin_cpu_brand);
 /* char (&__builtin_cpu_brand([char *buf]))[...]; */
 YIELD();
 DCCParse_ParPairBegin();
 if (DCCParse_IsExpr()) {
  DCCParse_Expr1(),vused(),vcast_pt(DCCTYPE_CHAR,0);
  if (TOK == ',') { YIELD(); DCCParse_ExprDiscard(); }
 } else {
  vx_alloca_n(string_size); /* ret */
 }
 nosup_sym = DCCUnit_AllocSym();
 done_sym  = DCCUnit_AllocSym();
 if unlikely(!nosup_sym || !done_sym) goto end;

 val.sv_ctype.t_base = NULL;
 val.sv_ctype.t_type = DCCTYPE_BUILTIN|DCCTYPE_IB4|DCCTYPE_UNSIGNED;
 if (TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED)
     val.sv_ctype.t_type |= DCCTYPE_UNSIGNED;
 val.sv_const.it = CPUINFO_OFFSETOF_CPUID1_EAX;
 val.sv_flags    = DCC_SFLAG_LVALUE|DCC_SFLAG_RVALUE|DCC_SFLAG_DO_WUNUSED;
 val.sv_reg      = DCC_RC_CONST;
 val.sv_reg2     = DCC_RC_CONST;
 val.sv_sym      = CPUINFO_GETSYM();
 DCCVStack_KillAll(0);
 vpush(&val);   /* ret, <CPUID_MAX> */
 vpushs(nosup_sym); /* ret, <CPUID_MAX>, nosup */
 vgen1('&');    /* ret, <CPUID_MAX>, &nosup */
 vjcc(1);       /* ret */

 if (want_brand) {
  /* Check for extended cpuid values. */
  DCCDisp_IntMovReg(0x80000000,EAX);
  t_putb(0x0f),t_putb(0xa2); /* cpuid */
  { struct DCCSymAddr cst_val = {0x80000004,NULL};
    DCCDisp_CstBinReg('?',&cst_val,EAX,1);
  }
  DCCDisp_SymJcc(DCC_TEST_B,nosup_sym);
 }

 /* cpuid for a brand-string is supported. */
 DCCDisp_RegPush(DCC_RR_XBX);
 if (vbottom->sv_flags&DCC_SFLAG_LVALUE) {
  return_data.ml_off = vbottom->sv_const.offset;
  return_data.ml_sym = vbottom->sv_sym;
  return_data.ml_reg = vbottom->sv_reg;
  /* Must load the return pointer into a register unaffected by 'cpuid'. */
  DCCDisp_RegPush(DCC_RR_XDI);
  DCCDisp_MemMovReg(&return_data,DCC_RR_XDI);
  return_data.ml_off = 0;
  return_data.ml_sym = NULL;
  return_data.ml_reg = DCC_RR_XDI;
  must_pop_return    = 1;
 } else {
  return_data.ml_off = vbottom->sv_const.offset;
  return_data.ml_sym = vbottom->sv_sym;
  return_data.ml_reg = vbottom->sv_reg;
  must_pop_return    = 0;
  /* Special case: Because of the push above, we must adjust the
   *               indirection offset of an ESP-offset target. */
  if (!DCC_RC_ISCONST(return_data.ml_reg) &&
      (return_data.ml_reg&DCC_RI_MASK) == DCC_ASMREG_ESP)
       return_data.ml_off += DCC_TARGET_SIZEOF_GP_REGISTER;
 }

 if (want_brand) {
  DCCDisp_IntMovReg(0x80000002,EAX);
  t_putb(0x0f),t_putb(0xa2); /* cpuid */
  DCCDisp_RegMovMem(EAX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EBX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(ECX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EDX,&return_data),return_data.ml_off += 4;
  DCCDisp_IntMovReg(0x80000003,EAX);
  t_putb(0x0f),t_putb(0xa2); /* cpuid */
  DCCDisp_RegMovMem(EAX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EBX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(ECX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EDX,&return_data),return_data.ml_off += 4;
  DCCDisp_IntMovReg(0x80000004,EAX);
  t_putb(0x0f),t_putb(0xa2); /* cpuid */
  DCCDisp_RegMovMem(EAX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EBX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(ECX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EDX,&return_data),return_data.ml_off += 4;
 } else {
  DCCDisp_IntMovReg(0,EAX);
  t_putb(0x0f),t_putb(0xa2); /* cpuid */
  DCCDisp_RegMovMem(EBX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(EDX,&return_data),return_data.ml_off += 4;
  DCCDisp_RegMovMem(ECX,&return_data),return_data.ml_off += 4;
 }
 { struct DCCSymAddr term = {'\0',NULL};
   DCCDisp_CstMovMem(&term,&return_data,DCC_TARGET_SIZEOF_CHAR);
 }

 if (must_pop_return) DCCDisp_PopReg(return_data.ml_reg);
 DCCDisp_PopReg(DCC_RR_XBX);
 vpushs(done_sym); /* ret, done */
 vgen1('&');       /* ret, &done */
 vjmp();           /* ret */

 t_defsym(nosup_sym);
 /* cpuid for a brand-string is unsupported.
  * >> Fill the used buffer with '\0'-characters.
  * NOTE: 'ret' must be duplicated around this block
  *        so-as to keep it synchronized with the return
  *        value of the other block.
  *     >> Without said dup, the expression itself might return a conditionally
  *        temporary buffer (that's likely '0xcccccccc'; aka. uninitialized). */
 vdup(0);                  /* ret */
 vpushi(DCCTYPE_INT,'\0'); /* ret, dret, '\0' */
 vpushi(DCCTYPE_SIZE|DCCTYPE_UNSIGNED,string_size); /* ret, dret, '\0', 49 */
 vx_memset();               /* ret, dret */
 vpop(0);

 t_defsym(done_sym);

 /* Return an array type.
  * Using this, the user can figure out the brand size at compile-time:
  * >> char buf[sizeof(__builtin_cpu_brand())];
  * >> printf("brand = %s\n",__builtin_cpu_brand(buf));
  */
 { struct DCCType return_type = {DCCTYPE_CHAR,NULL};
   DCCType_MkArray(&return_type,string_size);
   DCCType_MkPointer(&return_type);
   vcast(&return_type,1);
   DCCType_Quit(&return_type);
   vgen1('*');
 }
end:;
 DCCParse_ParPairEnd();
}

#else
#error FIXME
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_BUILTINS_CPUID_C_INL */
