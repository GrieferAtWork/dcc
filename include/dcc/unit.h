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
#ifndef GUARD_DCC_UNIT_H
#define GUARD_DCC_UNIT_H 1

#include "common.h"
#include "target.h"
#include "lexer.h"

#include <stddef.h>
#include <stdint.h>
#include <elf.h>

DCC_DECL_BEGIN

struct DCCSym;
struct DCCSection;
struct DCCUnit;

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4201)
#endif

struct DCCMemLoc {
 /* Descriptor for a generic memory location (which may be offset from a register) */
#ifdef DCC_PRIVATE_API
 rc_t                ml_reg; /*< Register to-be added to the address, or 'DCC_RC_CONST' for if not register-based. */
#else
 DCC(rc_t)           ml_reg; /*< Register to-be added to the address, or 'DCC_RC_CONST' for if not register-based. */
#endif
union{struct{
#ifdef DCC_PRIVATE_API
 target_off_t        ml_off; /*< Integral constant offset added to the symbol/register. */
#else
 DCC(target_off_t)   ml_off; /*< Integral constant offset added to the symbol/register. */
#endif
 struct DCCSym      *ml_sym; /*< [0..1] Symbol added to 'ml_off' during relocations, or NULL if unused. */
};                  
 struct DCCSymAddr   ml_sad; /*< Symbol address. */
};};
#ifdef _MSC_VER
#pragma warning(disable: 4201)
#pragma warning(pop)
#endif

#define DCCMEMLOC_ISREGOFF(self)   ((self)->ml_reg != DCC_RC_CONST)
#define DCCMEMLOC_ISMEMOFF(self)   ((self)->ml_reg == DCC_RC_CONST)
#define DCCMEMLOC_ISREGOFF_S(self) (DCCMEMLOC_ISREGOFF(self) && (self)->ml_sym)
#define DCCMEMLOC_ISMEMOFF_S(self) (DCCMEMLOC_ISMEMOFF(self) && (self)->ml_sym)

DCCFUN int DCCMemLoc_Equal(struct DCCMemLoc const *__restrict a,
                           struct DCCMemLoc const *__restrict b);
 
/* @return: 0: 'vector' and 'pointer' _most_ _definitely_ don't overlap.
 * @return: 1: 'vector' and 'pointer' _most_ _definitely_ do overlap.
 * @return: 2: The correlation between 'vector' and 'pointer' cannot be determined at compile-time. */
DCCFUN int
DCCMemLoc_Contains(struct DCCMemLoc const *__restrict vector,
                   DCC(target_siz_t) vector_size_in_bytes,
                   struct DCCMemLoc const *__restrict pointer);






#define DCC_SYMFLAG_NONE      0x00000000 /*< '[[visibility("default")]]': No special flags/default (aka. public) visibility. */
#define DCC_SYMFLAG_PROTECTED 0x00000001 /*< '[[visibility("protected")]]': Protected symbol (don't export from the compilation unit). */
#define DCC_SYMFLAG_PRIVATE   0x00000002 /*< '[[visibility("hidden")]]': Private symbol (don't export from a binary/library). */
#define DCC_SYMFLAG_INTERNAL  0x00000003 /*< '[[visibility("internal")]]': Internal symbol (Usually the same as 'DCC_SYMFLAG_PRIVATE', which it also implies). */
#define DCC_SYMFLAG_STATIC    0x00000004 /*< 'static': FLAG: Protected symbol (don't export from the compilation unit). */
#define DCC_SYMFLAG_WEAK      0x00000008 /*< '[[weak]]': FLAG: Weak symbol. A weak symbols can be overwritten at any time by another
                                          *   symbol, meaning should assumptions may be made about its address, or its relation.
                                          *   Access to such a symbol should take place exclusively through relocations. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_SYMFLAG_DLLIMPORT 0x00000010 /*< '[[dllimport]]': On PE targets: explicit dllimport. */
#define DCC_SYMFLAG_DLLEXPORT 0x00000020 /*< '[[dllexport]]': On PE targets: explicit dllexport. */
#endif
#define DCC_SYMFLAG_USED      0x00000040 /*< '[[used]]': FLAG: Don't delete this symbol, even if it appears unused. */
#define DCC_SYMFLAG_UNUSED    0x00000080 /*< '[[unused]]': FLAG: Don't warn if this symbol is deleted during unused symbol collection. */
#define DCC_SYMFLAG_VISIBILITY     0x000000ff /*< Mask for visibility symbol flags. */
#define DCC_SYMFLAG_VISIBILITYBASE 0x00000003 /*< Mask for base visibility. */

/* Additional symbol flags only meaningful for section start symbols. */
#define DCC_SYMFLAG_SEC_R     0x01000000 /*< The section is readable. */
#define DCC_SYMFLAG_SEC_W     0x02000000 /*< The section is writable. */
#define DCC_SYMFLAG_SEC_X     0x04000000 /*< The section is executable. */
#define DCC_SYMFLAG_SEC_S     0x08000000 /*< The section is shared between multiple instances of a running binary (NOTE: May not be available for some targets). */
#define DCC_SYMFLAG_SEC_M     0x10000000 /*< Symbols in this section can be merged. */
#define DCC_SYMFLAG_SEC_U     0x20000000 /*< This section has no alignment requirements (sc_align is ignored and interpreted as '1'). */
#define DCC_SYMFLAG_SEC(r,w,x,s,m,u) \
 (((r)?DCC_SYMFLAG_SEC_R:0)|((w)?DCC_SYMFLAG_SEC_W:0)\
 |((x)?DCC_SYMFLAG_SEC_X:0)|((s)?DCC_SYMFLAG_SEC_S:0)\
 |((m)?DCC_SYMFLAG_SEC_M:0)|((u)?DCC_SYMFLAG_SEC_U:0))
#define DCC_SYMFLAG_SEC_ISIMPORT 0x00000100 /*< The section is not known at compile-time, but is linked as a shared library at run-time (The library name is used as section name). */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_SYMFLAG_PE_ITA_IND   0x00000200 /*< Try to perform PE:ITA-indirection on this symbol. */
#endif
#define DCC_SYMFLAG_SEC_NOALLOC  0x40000000 /*< The runtime linker is not required to allocate this section. */
#if DCC_HOST_CPU == DCC_TARGET_CPU
#define DCC_SYMFLAG_SEC_OWNSBASE 0x80000000 /*< The base address is allocated through system-functions and must be freed. */
#endif

typedef uint32_t DCC(symflag_t);

struct DCCSym {
 /* Descriptor for a link-type symbol address/offset. */
 unsigned int              sy_refcnt;      /*< Reference counter. */
 struct DCCSym           **sy_sec_pself;   /*< [1..1][0..1][(!= NULL) == (sy_sec != NULL)]
                                            *  Self-pointer within the 'sy_sec_next...' linked list used by section symbol tables. */
 struct DCCSym            *sy_sec_next;    /*< [0..1] Next section symbol with the same modulated 'sy_name' address. */
 struct DCCSym            *sy_unit_next;   /*< [0..1] Next declaration symbol with the same modulated 'sy_name' address, within the same unit. */
 /*ref*/struct DCCSym     *sy_unit_before; /*< [0..1][->sy_name == sy_name] Used for redefining symbols: previous version of this
                                            *              symbol within the same unit (e.g.: when defining local assembly labels). */
 struct TPPKeyword const  *sy_name;        /*< [1..1][const] Symbol name (When set to '&TPPKeyword_Empty', the 'DCC_SYMFLAG_STATIC' flag must be set).
                                            *   NOTE: This name is also known as the assembly name (because it is what can be set with '__asm__'
                                            *         after a static-duration definition, as well as from assembly and during linkage)
                                            *      >> If this symbol has an accompaning c-definition, it is
                                            *         still most likely that that definition shares this name! */
#ifdef DCC_PRIVATE_API
 symflag_t                 sy_flags;       /*< Symbol flags (Set of 'DCC_SYMFLAG_*'). */
#else
 DCC(symflag_t)            sy_flags;       /*< Symbol flags (Set of 'DCC_SYMFLAG_*'). */
#endif
#if DCC_TARGET_BIN == DCC_BINARY_PE
 /*ref*/struct DCCSym     *sy_peind;       /*< [0..1] When non-NULL, the IAT pointer for this imports on PE targets. */
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 /*ref*/struct DCCSym     *sy_alias;       /*< [0..1] When non-NULL, another label aliased by this one. */
 /*ref*/struct DCCSection *sy_sec;         /*< [0..1][const_if(!= NULL)] Section this symbol is defined inside of (or NULL if unknown).
                                            *   NOTE: If the address in this field points to a section who's 'sc_start' symbol is equal
                                            *         to this symbol, this symbol is actually a section and behaves as pointing to the
                                            *         start of that same section.
                                            *   WARNING: This field _must_ be NULL when 'sy_alias' isn't NULL, and the same holds true the other way around. */
 /* TODO: 'sy_addr' should act as an offset in aliased symbols!
  *       The ability to do this is required for proper symbol declaration in assembly:
  *       >> # 'foobar' must be an alias for 'foo', with an offset of '8'.
  *       >> # If it isn't, the symbol will be linked improperly if the linker descides to move foo
  *       >> foobar = foo+8
  *       >> 
  *       >> _start:
  *       >>    call foobar
  *       >>    ret
  *       >> 
  *       >> foo:
  *       >>    .skip 8
  *       >>    push $text
  *       >>    call printf
  *       >>    add $4, %esp
  *       >>    ret
  */
#ifdef DCC_PRIVATE_API
 target_ptr_t              sy_addr;        /*< [const_if(sy_sec != NULL)] Symbol address (offset from associated section; undefined if 'sy_sec == NULL')
                                            *   NOTE: When this symbol depends on a library, this field may be used as a linker hint. */
 target_siz_t              sy_size;        /*< Symbol size, or 0 if unknown (used for generating better debug information & tracking free section data)
                                            *  >> When a symbol who's size is known is deleted, the freed memory becomes available for future allocations within the accompaning section. */
#else
 DCC(target_ptr_t)         sy_addr;        /*< [const_if(sy_sec != NULL)] Symbol address (offset from associated section; undefined if 'sy_sec == NULL')
                                            *   NOTE: When this symbol depends on a library, this field may be used as a linker hint. */
 DCC(target_siz_t)         sy_size;        /*< Symbol size, or 0 if unknown (used for generating better debug information & tracking free section data)
                                            *  >> When a symbol who's size is known is deleted, the freed memory becomes available for future allocations within the accompaning section. */
#endif
};
#define DCCSym_HASH(self) ((self)->sy_name->k_id)

/* Check if a given symbol is unused and can be deleted. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCCSym_ISUNUSED(self) \
  ((self)->sy_refcnt == 1 && !DCCSym_ISSECTION(self) && \
 !((self)->sy_flags&(DCC_SYMFLAG_DLLEXPORT|DCC_SYMFLAG_USED)) && !(self)->sy_peind)
#else /* DCC_TARGET_BIN == DCC_BINARY_PE */
#define DCCSym_ISUNUSED(self) \
  ((self)->sy_refcnt == 1 && !DCCSym_ISSECTION(self) && \
 !((self)->sy_flags&DCC_SYMFLAG_USED))
#endif /* DCC_TARGET_BIN != DCC_BINARY_PE */

/* Assert various assumptions that can always be made about valid symbols. */
#if DCC_DEBUG
#define DCCSym_DOASSERT(self) \
 (DCC_ASSERT((self)->sy_name),\
  DCC_ASSERTF( (self)->sy_sec_pself || (!(self)->sy_sec_next),"Invalid linkage in section symbol map"),\
  DCC_ASSERTF(!(self)->sy_sec_pself || *(self)->sy_sec_pself == (self),"The self-pointer is mapped inforrectly"),\
  DCC_ASSERTF(((self)->sy_sec != NULL) == ((self)->sy_sec_pself != NULL || (self) == &(self)->sy_sec->sc_start),"Section and section-self-pointer existance must mirror each other"),\
/*DCC_ASSERTF(((self)->sy_sec != NULL) || ((self)->sy_addr == 0),"A section is required for address information"),\
  DCC_ASSERTF(((self)->sy_sec != NULL) || ((self)->sy_size == 0),"A section is required for size information"),*/\
  DCC_ASSERTF(((self)->sy_alias == NULL) || ((self)->sy_sec == NULL),"Aliasing symbols may not be offset from sections"),\
  DCC_ASSERTF(((self)->sy_name != &TPPKeyword_Empty) || ((self)->sy_flags&DCC_SYMFLAG_STATIC),"Unnamed symbols must be static"))
#define DCCSym_ASSERT(self)  (DCC_ASSERT(self),DCCSym_DOASSERT(self))
#define DCCSym_XASSERT(self) ((self) ? DCCSym_DOASSERT(self) : (void)0)
#else
#define DCCSym_DOASSERT(self) (void)0
#define DCCSym_ASSERT(self)   (void)0
#define DCCSym_XASSERT(self)  (void)0
#endif
#define DCCSym_SECTION(self)    (self)->sy_sec
#define DCCSym_IMPORT(self)     (self)->sy_import

#define DCCSym_TOSECTION(self) ((struct DCCSection *)((uintptr_t)(self)-offsetof(struct DCCSection,sc_start)))
#define DCCSym_ISSECTION(self) ((self) == &(self)->sy_sec->sc_start)

#define DCCSym_ISDEFINED(self) ((self)->sy_sec != NULL)
#define DCCSym_ISALIAS(self)   ((self)->sy_alias != NULL)
#define DCCSym_ISFORWARD(self) ((self)->sy_sec == NULL && (self)->sy_alias == NULL)

#define DCCSym_Incref(self)    (void)(++(self)->sy_refcnt)
#define DCCSym_Decref(self)    (void)(--(self)->sy_refcnt || (_DCCSym_Delete(self),0))
#define DCCSym_XIncref(self)   ((self) ? DCCSym_Incref(self) : (void)0)
#define DCCSym_XDecref(self)   ((self) ? DCCSym_Decref(self) : (void)0)
DCCFUN void _DCCSym_Delete(struct DCCSym *__restrict self);

/* Convert a given string 'text' into the correct ELF-style symbol visibility class:
 * NOTE: This function is used by:
 *   - '#pragma GCC visibility push(...)'
 *   - '__attribute__((visibility(...)))'
 * The names are mapped as follows:
 *   - "default"   >> DCC_SYMFLAG_NONE
 *   - "protected" >> DCC_SYMFLAG_PROTECTED
 *   - "hidden"    >> DCC_SYMFLAG_PRIVATE
 *   - "internal"  >> DCC_SYMFLAG_INTERNAL
 * @return: (symflag_t)-1: The given 'text' was none of the above.
 *                         In this case, the caller must decide what should happen,
 *                         as no warning will have been emit by this function. */
DCCFUN DCC(symflag_t) DCCSymflag_FromString(struct TPPString const *__restrict text);


#define DCCSym_Clear(self) \
 (DCCSym_ISSECTION(self) ? DCCSection_Clear(DCCSym_TOSECTION(self)) : (void)0)

/* Create a new symbol, given its name 'name' */
DCCFUN /*ref*/struct DCCSym *
DCCSym_New(struct TPPKeyword const *__restrict name, DCC(symflag_t) flags);

/* Define a given symbol 'self' to describe the given location.
 * NOTE: If 'self' was already defined, emit a warning, but allow it to be re-defined. */
DCCFUN void DCCSym_Define(struct DCCSym *__restrict self,
                          struct DCCSection *__restrict section,
                          DCC(target_ptr_t) addr, DCC(target_siz_t) size);
DCCFUN void DCCSym_Alias(struct DCCSym *__restrict self,
                         struct DCCSym *__restrict alias_sym);

/* Clear the current definition of 'self' */
DCCFUN void DCCSym_ClrDef(struct DCCSym *__restrict self);

/* Explicitly import a given symbol from a specific 'import_sec' */
#define DCCSym_Import(self,import_sec,hint) \
       (assert(DCCSection_ISIMPORT(import_sec)),\
        DCCSym_Define(self,import_sec,hint,0))

/* Returns TRUE (non-zero) if the given symbols are equal
 * (that is: both are declared to point to the same location in memory)
 * NOTE: NULL may be passed for either symbol, which will always return in FALSE(0) */
DCCFUN int DCCSym_Equal(struct DCCSym const *a,
                        struct DCCSym const *b);

/* Returns TRUE (non-zero) if the given symbol 'p' is contained
 * within a vector of 'vsiz' bytes starting at 'v'.
 * Returns FALSE if either symbol is undefined or the symbols are from different sections. */
DCCFUN int DCCSym_Contains(struct DCCSym const *__restrict v, DCC(target_siz_t) vsiz,
                           struct DCCSym const *__restrict p);


typedef uint32_t DCC(rel_t); /* One of 'DCC_R_*', or CPU-specific ELF relocation. */

/* ELF relocation types. */
#if DCC_TARGET_IA32(386)
#   define DCC_R_NONE      R_386_NONE /* Empty relocation (used for symbol dependency) */
#   define DCC_R_DATA_8    R_386_8
#   define DCC_R_DATA_16   R_386_16
#   define DCC_R_DATA_32   R_386_32
#   define DCC_R_DATA_PTR  R_386_32
#   define DCC_R_DISP_8    R_386_PC8
#   define DCC_R_DISP_16   R_386_PC16
#   define DCC_R_DISP_32   R_386_PC32
#   define DCC_R_JMP_SLOT  R_386_JMP_SLOT
#   define DCC_R_COPY      R_386_COPY
#   define DCC_R_RELATIVE  R_386_RELATIVE
#elif DCC_TARGET_CPU == DCC_TARGET_X86_64
#   define DCC_R_NONE      R_X86_64_NONE
#   define DCC_R_DATA_8    R_X86_64_8
#   define DCC_R_DATA_16   R_X86_64_16
#   define DCC_R_DATA_32   R_X86_64_32
#   define DCC_R_DATA_64   R_X86_64_64
#   define DCC_R_DATA_PTR  R_X86_64_64
#   define DCC_R_DISP_8    R_X86_64_PC8
#   define DCC_R_DISP_16   R_X86_64_PC16
#   define DCC_R_DISP_32   R_X86_64_PC32
#   define DCC_R_JMP_SLOT  R_X86_64_JUMP_SLOT
#   define DCC_R_COPY      R_X86_64_COPY
#   define DCC_R_RELATIVE  R_X86_64_RELATIVE
#else
#   error FIXME
#endif



struct DCCRel {
 DCC(target_ptr_t)     r_addr; /*< Relocation address (within the section that owns this relocation).
                                *  NOTE: The relocation vector is sorted by this number! */
 DCC(rel_t)            r_type; /*< Relocation type (One of 'DCC_R_*', or CPU-specific ELF relocation). */
 /*ref*/struct DCCSym *r_sym;  /*< [1..1] Symbol to which this relocation points. */
};

#define DCC_FREEDATA_INVPTR   ((DCC(target_ptr_t))-1)
struct DCCFreeRange {
 struct DCCFreeRange *fr_next; /*< [0..1][->fr_addr > fr_addr+fr_size] Next range (Address range of this must neither overlap, or touch 'fr_addr+fr_size', as well as be greater) */
 DCC(target_ptr_t)    fr_addr; /*< Start address. */
 DCC(target_siz_t)    fr_size;
};
struct DCCFreeData {
 /* Tracking of unallocated data within a section/on the stack. */
 struct DCCFreeRange *fd_begin; /*< [0..1][->fr_prev == NULL][owned|owned(->fr_next->...)]
                                 *   Linked list head of free address ranges. */
};

DCCFUN void DCCFreeData_Quit(struct DCCFreeData *__restrict self);

/* Try to initialize 'self' as a copy of 'right'
 * If initialization succeeded, return TRUE(1), or FALSE(0) otherwise. */
DCCFUN int
DCCFreeData_InitCopy(struct DCCFreeData *__restrict self,
                     struct DCCFreeData const *__restrict right);
#define DCCFreeData_Clear(self) (void)(DCCFreeData_Quit(self),(self)->fd_begin = NULL)

/* Acquires/releases free memory with.
 * @return: DCC_FREEDATA_INVPTR: No free memory area compliant to the
 *                               given specifications could be found. */
DCCFUN DCC(target_ptr_t)
DCCFreeData_Acquire(struct DCCFreeData *__restrict self,
                    DCC(target_siz_t) size, DCC(target_siz_t) align,
                    DCC(target_siz_t) offset);
DCCFUN DCC(target_ptr_t)
DCCFreeData_AcquireAt(struct DCCFreeData *__restrict self,
                      DCC(target_ptr_t) addr, DCC(target_siz_t) size);
DCCFUN void
DCCFreeData_Release(struct DCCFreeData *__restrict self,
                    DCC(target_ptr_t) addr, DCC(target_siz_t) size);


struct DCCTextBuf {
 uint8_t *tb_begin; /*< [0..1][<= s_end][owned] Start pointer for assembly code. */
 uint8_t *tb_end;   /*< [0..1][>= s_begin] Compile-time allocated code end. */
 uint8_t *tb_max;   /*< [0..1][>= tb_pos] Used code end (overflow above 'tb_end' symbolically describes zero-initialized memory). */
 uint8_t *tb_pos;   /*< [0..1][>= s_begin] Current code position (NOTE: May be placed above 'tb_end' for lazy alloc-on-write).
                     * >> Everything between this and 'tb_max' should be considered as ZERO-initialized, virtual memory. */
};
#define DCCTextBuf_ADDR(self)            ((DCC(target_ptr_t))((self)->tb_pos-(self)->tb_begin))
#define DCCTextBuf_SETADDR(self,a) (void)((self)->tb_pos = (self)->tb_begin+(a))



struct DCCSection {
 /* Descriptor for a section/library dependency. */
 struct DCCSym       sc_start; /*< Start symbol (always a label at offset '0' of this same section).
                                *  NOTE: This symbol also contains the name & flags of this section. */
 size_t              sc_symc;  /*< Amount of symbols in 'sc_symv'. */
 size_t              sc_syma;  /*< Allocated bucket count in 'sc_symv'. */
 struct DCCSym     **sc_symv;  /*< [0..1][chain(sy_sec_pself|sy_sec_next->...)][0..sc_syma][owned]
                                *   Hash-map of symbols declared within this section. */
 /* NOTE: The linked list described by the following pointer triple depends on 'DCCSection_ISIMPORT(self)'. */
 struct DCCUnit     *sc_unit;  /*< [0..1] Unit associated with this section. */
 struct DCCSection **sc_pself; /*< [1..1|==self][0..1] Self-pointer in the 'sc_next->...' section chain. */
 struct DCCSection  *sc_next;  /*< [0..1][chain(sc_next->...)] Next section in the unit with the same moduled name-id. */
 /* NOTE: None of the below are available when 'DCCSection_ISIMPORT(self)' evaluates to non-ZERO(0). */
 /* TODO: Add a system for dynamically counting usage of text ranges.
  *    >> Required for tracking references when merging data.
  *       Basically: 'DCCSection_DFree' must act like a decref() function. */
 struct DCCTextBuf   sc_text;  /*< Flushed section text buffer (May not be up-to-date for the current section). */
 struct DCCFreeData  sc_free;  /*< Tracker for free section data. */
#ifdef DCC_PRIVATE_API
 target_ptr_t        sc_align; /*< Minimum section alignment (ignored & interpreted as '1' when 'DCC_SYMFLAG_SEC_U' is set) */
#else
 DCC(target_ptr_t)   sc_align; /*< Minimum section alignment (ignored & interpreted as '1' when 'DCC_SYMFLAG_SEC_U' is set) */
#endif
 target_ptr_t        sc_base;  /*< Base address of this section (or NULL if &DCCSection_Abs, or not runtime-relocated) */
 target_ptr_t        sc_merge; /*< Used during merging: Base address of merge destination. */
 size_t              sc_relc;  /*< Amount of relocations currently in use. */
 size_t              sc_rela;  /*< Amount of allocated relocations. */
 struct DCCRel      *sc_relv;  /*< [0..sc_relc|alloc(sc_rela)|sort(->r_addr,<)][owned]
                                *   Vector of relocations to memory within this section. */
};

/* Builtin section: ABS (Absolute section always relocated & with a base at ZERO) */
DCCDAT struct DCCSection DCCSection_Abs;

#define DCCSection_ISIMPORT(self)   ((self)->sc_start.sy_flags&DCC_SYMFLAG_SEC_ISIMPORT)

#define DCCSection_Incref(self)     (void)(++(self)->sc_start.sy_refcnt)
#define DCCSection_Decref(self)     (void)(--(self)->sc_start.sy_refcnt || (_DCCSym_Delete(&(self)->sc_start),0))
#define DCCSection_XIncref(self)    ((self) ? DCCSection_Incref(self) : (void)0)
#define DCCSection_XDecref(self)    ((self) ? DCCSection_Decref(self) : (void)0)

#define DCCSection_VSIZE(self) ((DCC(target_siz_t))((self)->sc_text.tb_max-(self)->sc_text.tb_begin))
#define DCCSection_MSIZE(self) ((DCC(target_siz_t))(((self)->sc_text.tb_end < (self)->sc_text.tb_max ? \
                                                     (self)->sc_text.tb_end : (self)->sc_text.tb_max)-\
                                                     (self)->sc_text.tb_begin))
#define DCCSection_HASBASE(self) ((self) == &DCCSection_Abs || (!DCCSection_ISIMPORT(self) && (self)->sc_base))
#define DCCSection_ENUMSYM(sym,self) \
 for (struct DCCSym **sym_iter = (self)->sc_symv,\
                    **sym_end  = sym_iter+(self)->sc_syma;\
      sym_iter != sym_end; ++sym_iter)\
 for ((sym) = *sym_iter; (sym); (sym) = (sym)->sy_sec_next)


/* Allocates and returns a new section, given its name.
 * NOTE: When the 'DCC_SYMFLAG_SEC_ISIMPORT' flag is set, the section is allocated as a library. */
DCCFUN /*ref*/struct DCCSection *DCCSection_New(struct TPPKeyword const *__restrict name, DCC(symflag_t) flags);
DCCFUN /*ref*/struct DCCSection *DCCSection_News(char const *__restrict name, DCC(symflag_t) flags);

/* Clears a given section, resolving all self-references (used during cleanup). */
DCCFUN void DCCSection_Clear(struct DCCSection *__restrict self);

/* Returns TRUE(1) if the given section 'self' is currently selected by the compilation unit. */
#define DCCSection_ISCURR(self) ((self) == DCCUnit_Current.u_curr)

/* Safely begin/end working with the text of a given unit.
 * >> These functions are required to properly flush the text cache when
 *    the given section is currently selected by the compilation unit.
 * WARNING: While working with the text buffer of a given section, no
 *          code writing text data using the 'DCCUnit_*' interface may be
 *          invoked, and the currently selected section may not be changed!
 * TODO: Generate a jump across a TBEGIN/TEND region if the section is the text section?
 * WARNING: Don't do so from assembly!
 * @requires: !DCCSection_ISIMPORT(self) */
#if DCC_DEBUG
#define DCCSection_TBEGIN(self) \
do{ DCC_ASSERT(!DCCSection_ISIMPORT(self));\
    if (DCCSection_ISCURR(self)) {\
     DCC_ASSERTF(!(DCCCompiler_Current.c_flags&DCC_COMPILER_FLAG_TEXTFLUSH),"Text was already flushed");\
     DCCCompiler_Current.c_flags |= DCC_COMPILER_FLAG_TEXTFLUSH;\
     memcpy(&(self)->sc_text,&DCCUnit_Current.u_tbuf,\
            sizeof(struct DCCTextBuf));\
    }\
}while(DCC_MACRO_FALSE)
#define DCCSection_TEND(self) \
do{ DCC_ASSERT(!DCCSection_ISIMPORT(self));\
    if (DCCSection_ISCURR(self)) {\
     DCC_ASSERTF(DCCCompiler_Current.c_flags&DCC_COMPILER_FLAG_TEXTFLUSH,"Text was not flushed");\
     DCCCompiler_Current.c_flags &= ~(DCC_COMPILER_FLAG_TEXTFLUSH);\
     memcpy(&DCCUnit_Current.u_tbuf,&(self)->sc_text,\
            sizeof(struct DCCTextBuf));\
    }\
}while(DCC_MACRO_FALSE)
#else
#define DCCSection_TBEGIN(self) \
do{ DCC_ASSERT(!DCCSection_ISIMPORT(self));\
    if (DCCSection_ISCURR(self))\
    memcpy(&(self)->sc_text,&DCCUnit_Current.u_tbuf,\
           sizeof(struct DCCTextBuf));\
}while(DCC_MACRO_FALSE)
#define DCCSection_TEND(self) \
do{ DCC_ASSERT(!DCCSection_ISIMPORT(self));\
    if (DCCSection_ISCURR(self)) \
    memcpy(&DCCUnit_Current.u_tbuf,&(self)->sc_text,\
           sizeof(struct DCCTextBuf));\
}while(DCC_MACRO_FALSE)
#endif

/* Add a new relocation (NOTE: The given 'relo' is copied)
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_Putrelo(struct DCCSection *__restrict self,
                               struct DCCRel const *__restrict relo);

/* Allocate 'n_relocs' relocations and return them.
 * NOTE: Following this, the caller is responsible for initializing them.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: NULL: Failed to allocate more relocations (A lexer error was set). */
DCCFUN struct DCCRel *
DCCSection_Allocrel(struct DCCSection *__restrict self, size_t n_relocs);

/* Try to resolve disposition relocations.
 * >> This function handles any redundant relocation
 *    pointing back into the same section.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: * : The amount of deleted relocations. */
DCCFUN size_t DCCSection_ResolveDisp(struct DCCSection *__restrict self);

/* Set the base address of a given section.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_SetBaseTo(struct DCCSection *__restrict self, target_ptr_t address);

#if DCC_HOST_CPU == DCC_TARGET_CPU
/* Allocate a base address for the given section.
 * This function is required for execution of code after it has been generated.
 * NOTE: Upon failure, a lexer error is set.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_SetBase(struct DCCSection *__restrict self);
#endif /* DCC_HOST_CPU == DCC_TARGET_CPU */

/* Execute relocations on the section.
 * WARNING: The caller is responsible to set a section base address beforehand.
 * WARNING: If any relocation depends on a section that had yet to
 *          have its base set, this function may link incorrectly.
 * WARNING: Once this function is called, symbol address information is lost forever.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_Reloc(struct DCCSection *__restrict self);

/* Delete all relocations inside 'addr...+=size'
 * @return: * : The amount of deleted relocations.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN size_t
DCCSection_Delrel(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr,
                  DCC(target_siz_t) size);

DCC_LOCAL void
DCCSection_Putrel(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr, DCC(rel_t) type,
                  struct DCCSym *__restrict sym) {
 struct DCCRel rel;
 rel.r_addr = addr;
 rel.r_type = type;
 rel.r_sym = sym;
 DCCSection_Putrelo(self,&rel);
}

/* Returns a symbol 'name' apart of the given section 'self'
 * WARNING: If the section contains multiple symbols named 'name', which symbol
 *          will be returned is undefined (although one of them will be returned).
 * @return: NULL: The section doesn't contain a symbol with the given name. */
DCCFUN struct DCCSym *
DCCSection_GetSym(struct DCCSection *__restrict self,
                  struct TPPKeyword const *__restrict name);
DCCFUN struct DCCSym *
DCCSection_GetSyms(struct DCCSection *__restrict self,
                   char const *__restrict name);


/* Returns a pointer to section data at 'addr', ensuring that
 * at least 'size' bytes are actually allocated for reading/writing.
 * WARNING: The caller is responsible for ensuring that the address range is not out-of-bounds.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: NULL: Failed to allocate the requested memory range (A lexer error was also set).
 * @return: * :   A modifyable pointer to section memory (by default initialized to ZERO) */
DCCFUN void *
DCCSection_GetText(struct DCCSection *__restrict self,
                   DCC(target_ptr_t) addr,
                   DCC(target_siz_t) size);


/* Allocates zero-initialized section memory.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN DCC(target_ptr_t)
DCCSection_DAlloc(struct DCCSection *__restrict self,
                  DCC(target_siz_t) size,
                  DCC(target_siz_t) align,
                  DCC(target_siz_t) offset);

/* Similar to 'DCCSection_DAlloc', but only allocates memory at the end,
 * thereby ensuring data consistency over multiple calls to this function.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN DCC(target_ptr_t)
DCCSection_DAllocBack(struct DCCSection *__restrict self,
                      DCC(target_siz_t) size,
                      DCC(target_siz_t) align,
                      DCC(target_siz_t) offset);

/* Tries to allocate 'size' bytes of zero-initialized memory at the given 'addr'.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: 0: The specified memory location is already in use. */
DCCFUN int DCCSection_DAllocAt(struct DCCSection *__restrict self,
                               DCC(target_ptr_t) addr, DCC(target_siz_t) size);

/* Re-allocate section memory, attempting to do so in-place, but allocating
 * a new vector of data if the space that the caller tried to re-allocate into
 * is already in use.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN DCC(target_ptr_t)
DCCSection_DRealloc(struct DCCSection *__restrict self,
                    DCC(target_ptr_t) old_addr, DCC(target_siz_t) old_size,
                    DCC(target_siz_t) new_size, DCC(target_siz_t) new_align,
                    DCC(target_siz_t) new_offset);

/* Tries to merge data data location with an identical instance
 * of the same memory located at a lower memory address range.
 * If such an instance is found, 
 * NOTE: If the 'DCC_SYMFLAG_SEC_M' flag isn't set, this function returns 'addr' immediately.
 * @requires: !DCCSection_ISIMPORT(self)
 */
DCCFUN DCC(target_ptr_t)
DCCSection_DMerge(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr, DCC(target_siz_t) size,
                  DCC(target_siz_t) min_align);

/* Same as 'DCCSection_DAlloc', but if the 'DCC_SYMFLAG_SEC_M' flag is set in
 * the given section, symbol data may be merged with equivalent, already-existing data.
 * >> This is equal to calling 'DCCSection_DAlloc()', 'DCCSection_GetText()' and 'DCCSection_DMerge()'
 * @param: mem_size: The size of 'memory' in bytes (Must be <= 'size')
 * @param: size:     The size of the allocated memory within the section
 *                  (Any difference between this and 'mem_size' is filled with ZEROes)
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: * : A pointer aligned by 'align' and apart of the section. */
DCCFUN DCC(target_ptr_t)
DCCSection_DAllocMem(struct DCCSection *__restrict self,
                     void const *__restrict memory,
                     DCC(target_siz_t) mem_size, DCC(target_siz_t) size,
                     DCC(target_siz_t) align, DCC(target_siz_t) offset);

/* A further simplification for 'DCCSection_DAllocMem', that returns
 * a reference to a symbol describing the newly allocated data.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN /*ref*/struct DCCSym *
DCCSection_DAllocSym(struct DCCSection *__restrict self,
                     void const *__restrict memory,
                     DCC(target_siz_t) mem_size, DCC(target_siz_t) size,
                     DCC(target_siz_t) align, DCC(target_siz_t) offset);

/* Free a given section address range & clear all relocations inside.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void
DCCSection_DFree(struct DCCSection *__restrict self,
                 DCC(target_ptr_t) addr, DCC(target_siz_t) size);

#define DCCSection_TADDR(self)       DCCTextBuf_ADDR(&(self)->sc_text)
/* Allocate 'size' bytes of text memory, advancing
 * the text pointer and returning its old address.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: NULL: Failed to allocate more memory (a lexer error was set). */
DCCFUN void *DCCSection_TAlloc(struct DCCSection *__restrict self,
                               DCC(target_siz_t) size);

/* Write the given data to the section as text.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_TWrite(struct DCCSection *__restrict self,
                              void const *__restrict p, size_t s);

/* Align the text pointer by 'align', filling overflow memory with NOP bytes.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_TAlign(struct DCCSection *__restrict self,
                              DCC(target_siz_t) align,
                              DCC(target_siz_t) offset);

/* Write one given byte 'byte' of text memory, advancing the text pointer.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_TPutb(struct DCCSection *__restrict self,
                             uint8_t byte);


struct DCCUnit {
 /* Compilation unit. */
 size_t                 u_symc; /*< Amount of symbols in 'u_symv'. */
 size_t                 u_syma; /*< Allocated bucket count in 'u_symv'. */
 /*ref*/struct DCCSym **u_symv; /*< [0..1][chain(sy_sec_pself->...)][0..u_syma][owned]
                                 *   Hash-map of all public/private/protected symbols & section
                                 *   declared within this compilation unit. */
 size_t                 u_secc; /*< Amount of sections reachable through the 'u_secs->sc_next->...' chain. */
 struct DCCSection     *u_secs; /*< [0..1][chain(sc_next->...)] Pointer to the lastly allocated section, or NULL if no sections exist.
                                 *   NOTE: This chain does _NOT_ contain library imports. */
 size_t                 u_impc; /*< Amount of imports reachable through the 'u_imps->sc_next->...' chain. */
 struct DCCSection     *u_imps; /*< [0..1][chain(sc_next->...)] Pointer to the lastly allocated import, or NULL if no imports exist.
                                 *   NOTE: This chain _ONLY_ contains library imports. */
 /*ref*/struct DCCSym  *u_nsym; /*< [0..1] Linked list of unnamed symbols. */
 struct DCCSection     *u_text; /*< [0..1] Default section: '.text' (Used for read/execute code) */
 struct DCCSection     *u_data; /*< [0..1] Default section: '.data' (Used for read-only data) */
 struct DCCSection     *u_bss;  /*< [0..1] Default section: '.text' (Used for read/write data) */
 struct DCCSection     *u_str;  /*< [0..1] Default section: '.str'  (Used for read-only, mergeable data) */
 struct DCCSection     *u_prev; /*< [0..1] The section selected before the current one (or NULL if none was). */
 struct DCCSection     *u_curr; /*< [0..1] Currently selected section (target for writing text). */
 struct DCCTextBuf      u_tbuf; /*< Current text buffer (in-lined local cache for 'u_curr->sc_text') */
};

/* Global object: The current compilation unit. */
DCCDAT struct DCCUnit DCCUnit_Current;

DCCFUN void DCCUnit_Init(struct DCCUnit *__restrict self);
DCCFUN void DCCUnit_Quit(struct DCCUnit *__restrict self);

/* Merge 'other' into the currently selected unit.
 * NOTE: Before calling this function, the caller should first call:
 *       >> DCCUnit_SetCurr(NULL); // Make sure no section is selected.
 *       >> DCCUnit_Merge(other); */
DCCFUN void DCCUnit_Merge(struct DCCUnit *__restrict other);

/* Extract and re-initialize the current unit,
 * storing the old one in '*other'.
 * NOTE: This function does a bit more than simply moving
 *       some memory, as it updates section-unit pointers. */
DCCFUN void DCCUnit_Extract(struct DCCUnit *__restrict other);

/* Clear any preallocated memory reachable from 'self'
 * NOTE: Some flush operations are always performed:
 *        - Clear unused relocation.
 * @param: flags: A set of 'DCCUNIT_FLUSHFLAG_*'
 */
DCCFUN void DCCUnit_Flush(struct DCCUnit *__restrict self, uint32_t flags);
#define DCCUNIT_FLUSHFLAG_NONE     0x00000000
#define DCCUNIT_FLUSHFLAG_SECMEM   0x00000001 /*< Clear pre-allocated section text memory (when 'tb_max < tb_end', lower 'tb_end' to 'tb_max') */
#define DCCUNIT_FLUSHFLAG_SECTRAIL 0x00000002 /*< Free trailing ZERO-memory in section texts (Sections will continue to behave as before) */
#define DCCUNIT_FLUSHFLAG_SYMTAB   0x00000004 /*< Shrink the allocated size of symbol hash tables to the minimum assumed by automatic rehashing. */
#define DCCUNIT_FLUSHFLAG_TABMIN   0x00000010 /*< More aggressive behavior for 'DCCUNIT_FLUSHFLAG_SYMTAB': Reduce the hash size to ONE(1) (Only use this as last way out) */


#define DCCUnit_ENUMSYM(sym) \
 for (struct DCCSym **sym_iter = DCCUnit_Current.u_symv,\
                    **sym_end  = sym_iter+DCCUnit_Current.u_syma;\
      sym_iter != sym_end; ++sym_iter)\
 for ((sym) = *sym_iter; (sym); (sym) = (sym)->sy_unit_next)
#if 1
#define DCCUnit_ENUMSEC(sec) \
 for ((sec) = DCCUnit_Current.u_secs; (sec); (sec) = (sec)->sc_next)
#define DCCUnit_ENUMIMP(sec) \
 for ((sec) = DCCUnit_Current.u_imps; (sec); (sec) = (sec)->sc_next)
#else
#define DCCUnit_ENUMSEC(sec) \
 for (struct DCCSym **sym_iter = DCCUnit_Current.u_symv,\
                    **sym_end  = sym_iter+DCCUnit_Current.u_syma;\
      sym_iter != sym_end; ++sym_iter)\
 for ((sec) = (struct DCCSection *)*sym_iter; \
      (sec); (sec) = (struct DCCSection *)(sec)->sc_start.sy_unit_next) \
 if (!DCCSym_ISSECTION(&(sec)->sc_start)); else
#define DCCUnit_ENUMIMP(sec) \
 for (struct DCCSym **sym_iter = DCCUnit_Current.u_symv,\
                    **sym_end  = sym_iter+DCCUnit_Current.u_syma;\
      sym_iter != sym_end; ++sym_iter)\
 for ((sec) = (struct DCCSection *)*sym_iter; \
      (sec); (sec) = (struct DCCSection *)(sec)->sc_start.sy_unit_next) \
 if (!DCCSym_ISSECTION(&(sec)->sc_start) || \
     !DCCSection_ISIMPORT(sec)); else
#endif

/* Clear all symbols with a reference counter of '1', effectively
 * removing all unnamed symbols not used anywhere but in relocations.
 * WARNING: The caller is required to ensure that no such symbols are currently in use,
 *          and simply pending to-be being defined, such as a jump-symbol in an if-statement.
 *       >> By default, this function is called after compilation is done, and before linking is performed.
 * >> Essentially, this function performs unused symbol elimination */
DCCFUN size_t DCCUnit_ClearUnused(void);

/* Clear all symbols that were defined as static,
 * essentially removing all unused static declaration. */
DCCFUN size_t DCCUnit_ClearStatic(void);

/* Clear all libraries without any used functions. */
DCCFUN size_t DCCUnit_ClearUnusedLibs(void);

/* Lookup/Create a new symbol/section 'name'.
 * Note, that every section is implicitly a symbol!
 * NOTE: None of these functions return a reference to the symbol!
 * NOTE: [DCCUnit_NewSym*] If a symbol with the same name already exists, it will be returned instead!
 * HINT: To get/allocate an import, use 'DCCUnit_(Get|New)Sec{s}(...,DCC_SYMFLAG_SEC_ISIMPORT)'
 * @return: NULL: [DCCUnit_GetSym*] No known symbol/section with the given name.
 *                [DCCUnit_NewSym*] Failed to allocate the new symbol/section (A lexer error was set) */
DCCFUN struct DCCSym *DCCUnit_GetSym(struct TPPKeyword const *__restrict name);
DCCFUN struct DCCSym *DCCUnit_NewSym(struct TPPKeyword const *__restrict name, DCC(symflag_t) flags);
DCCFUN struct DCCSym *DCCUnit_GetSyms(char const *__restrict name);
DCCFUN struct DCCSym *DCCUnit_NewSyms(char const *__restrict name, DCC(symflag_t) flags);
DCCFUN struct DCCSection *DCCUnit_GetSec(struct TPPKeyword const *__restrict name);
DCCFUN struct DCCSection *DCCUnit_NewSec(struct TPPKeyword const *__restrict name, DCC(symflag_t) flags);
DCCFUN struct DCCSection *DCCUnit_GetSecs(char const *__restrict name);
DCCFUN struct DCCSection *DCCUnit_NewSecs(char const *__restrict name, DCC(symflag_t) flags);

/* Dynamically load a binary from a given stream 's',
 * hooking it as a shared library under the given name 'name'.
 *  - All public symbols exported from the binary will become available as assembly names.
 * @return: NULL: Data form the given stream 's' does not describe a binary. (No lexer error was set)
 *                A critical error occurred while parsing the given stream 's'. (A lexer error was set)
 * @return: * :   Pointer to the library-section that was dynamically linked.
 */
DCCFUN struct DCCSection *DCCUnit_DynLoadPE(char *__restrict name, DCC(stream_t) s);
DCCFUN struct DCCSection *DCCUnit_DynLoad(char *__restrict name, int warn_unknown);
DCCFUN struct DCCSection *DCCUnit_DynLoadStream(char *__restrict name, DCC(stream_t) s, int warn_unknown);


/* Extended version of 'DCCUnit_NewSym' that allows the name to be a printf-style string. */
DCCFUN struct DCCSym *DCCUnit_NewSymf(DCC(symflag_t) flags, char const *__restrict fmt, ...);

/* Allocate a new forward symbol.
 * Same as '*DCCUnit_NewSym', but if an existing symbol evaluates true for 'DCCSym_ISFORWARD(sym)',
 * a new symbol is allocated and the existing one replaced and moved into the 'sy_unit_before' chain.
 * >> This function is used for re-usable local label declarations in assembly. */
DCCFUN struct DCCSym *DCCUnit_NewForwardSym(struct TPPKeyword const *__restrict name, DCC(symflag_t) flags);

/* Allocate a new, unnamed & private (aka. protected) symbol.
 * >> This function is very useful for allocating local labels. */
DCCFUN struct DCCSym *DCCUnit_AllocSym(void);

/* Set the current section to 'sec', returning the one previously selected.
 * WARNING: Once set, no text operations may be performed on 'sec', unless the caller
 *          wraps the call inside a 'DCCSection_TBEGIN'...'DCCSection_TEND' block.
 * NOTE: The text buffer of any previously set section will be flushed.
 * @param: sec: When non-NULL, load the text buffer of this section after flushing that of the old one.
 * @return: * : The previously set text section, or NULL if none was. */
DCCFUN struct DCCSection *DCCUnit_SetCurr(struct DCCSection *sec);

/* Allocate/write/align text within the currently selected section.
 * NOTE: These are all no-ops when the 'DCC_COMPILER_FLAG_NOCGEN' flag is set in the current compiler. */
DCCFUN void *DCCUnit_TAlloc(DCC(target_siz_t) size);
DCCFUN void  DCCUnit_TPutb(uint8_t byte);
DCCFUN void  DCCUnit_TAlign(DCC(target_siz_t) align, DCC(target_siz_t) offset);
DCCFUN void  DCCUnit_TWrite(void const *__restrict p, size_t s);
DCC_LOCAL void DCCUnit_TPutw(uint16_t word) { DCCUnit_TWrite(&word,2); }
DCC_LOCAL void DCCUnit_TPutl(uint32_t dword) { DCCUnit_TWrite(&dword,4); }
DCC_LOCAL void DCCUnit_TPutq(uint64_t qword) { DCCUnit_TWrite(&qword,8); }

#define DCCUnit_TADDR()  DCCTextBuf_ADDR(&DCCUnit_Current.u_tbuf)

/* Define a given symbol 'sym' to point to the current text address. */
#define DCCUnit_DEFSYM(sym) DCCSym_Define(sym,DCCUnit_Current.u_curr,DCCUnit_TADDR(),0)

/* Clear the cache of pre-allocated symbols. */
DCCFUN void DCCUnit_ClearCache(void);

#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
DCCDAT struct DCCUnit     unit;
DCCDAT target_ptr_t       t_addr; /* The current text address. */
DCCDAT struct DCCSection *u_text; /* == unit.u_text */

/* Write the given data to the current text poitner. */
DCCFUN void t_putb(uint8_t byte);
DCCFUN void t_putw(uint16_t word);
DCCFUN void t_putl(uint32_t dword);
DCCFUN void t_putq(uint64_t qword);

/* Generate a relocated 'type' using 'sym' within the
 * current text section, at the current text address. */
DCCFUN void t_putrel(rel_t type, struct DCCSym *sym);

/* Allocate and return text memory. */
DCCFUN void *t_alloc(target_siz_t s);

/* Write the given data to the current section. */
DCCFUN void t_write(void *p, size_t s);

/* Define the given symbol to be located at the current text position. */
DCCFUN void t_defsym(struct DCCSym *sym);

#else
#define unit                DCCUnit_Current
#define t_addr              DCCUnit_TADDR()
#define t_text              DCCUnit_Current.u_text
#define t_putb              DCCUnit_TPutb
#define t_putw              DCCUnit_TPutw
#define t_putl              DCCUnit_TPutl
#define t_putq              DCCUnit_TPutq
#define t_putrel(type,sym) (DCCCompiler_ISCGEN() ? DCCSection_Putrel(DCCUnit_Current.u_text,DCCUnit_TADDR(),type,sym) : (void)0)
#define t_alloc             DCCUnit_TAlloc
#define t_write             DCCUnit_TWrite
#define t_defsym            DCCUnit_DEFSYM
#endif
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_H */
