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
#include "stream.h"
#include "../elf.h"

#include <stddef.h>
#include <stdint.h>

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






#define DCC_SYMFLAG_NONE       0x00000000 /*< '[[visibility("default")]]': No special flags/default (aka. public) visibility. */
#define DCC_SYMFLAG_PROTECTED  0x00000001 /*< '[[visibility("protected")]]': Protected symbol (don't export from the compilation unit). */
#define DCC_SYMFLAG_PRIVATE    0x00000002 /*< '[[visibility("hidden")]]': Private symbol (don't export from a binary/library). */
#define DCC_SYMFLAG_INTERNAL   0x00000003 /*< '[[visibility("internal")]]': Internal symbol (Usually the same as 'DCC_SYMFLAG_PRIVATE', which it also implies). */
#define DCC_SYMFLAG_VISIBILITY 0x00000003 /*< Mask for ELF-style symbol visibility. */
#define DCC_SYMFLAG_STATIC     0x00000004 /*< 'static': FLAG: Protected symbol (don't export from the compilation unit). */
#define DCC_SYMFLAG_WEAK       0x00000008 /*< '[[weak]]': FLAG: Weak symbol. A weak symbols can be overwritten at any time by another
                                           *   symbol, meaning should assumptions may be made about its address, or its relation.
                                           *   Access to such a symbol should take place exclusively through relocations. */
#define DCC_SYMFLAG_NOCOLL     0x00000020 /*< '[[nocoll]]': Don't allow this symbol to be collapsed. */
#define DCC_SYMFLAG_USED       0x00000040 /*< '[[used]]': FLAG: Don't delete this symbol, even if it appears unused. */
#define DCC_SYMFLAG_UNUSED     0x00000080 /*< '[[unused]]': FLAG: Don't warn if this symbol is deleted during unused symbol collection. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_SYMFLAG_DLLIMPORT  0x00000100 /*< '[[dllimport]]': On PE targets: explicit dllimport. */
#define DCC_SYMFLAG_DLLEXPORT  0x00000200 /*< '[[dllexport]]': On PE targets: explicit dllexport. */
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
/* TODO: Add more flags for symbol typing (unknown|function|object) */

/* Additional symbol flags only meaningful for section start symbols. */
#define DCC_SYMFLAG_SEC_R     0x00010000 /*< The section is readable. */
#define DCC_SYMFLAG_SEC_W     0x00020000 /*< The section is writable. */
#define DCC_SYMFLAG_SEC_X     0x00040000 /*< The section is executable. */
#define DCC_SYMFLAG_SEC_S     0x00080000 /*< The section is shared between multiple instances of a running binary (NOTE: May not be available for some targets). */
#define DCC_SYMFLAG_SEC_M     0x00100000 /*< Symbols in this section can be merged. */
#define DCC_SYMFLAG_SEC_U     0x00200000 /*< This section has no alignment requirements ('sc_start.sy_align' is ignored and interpreted as '1'). */
#define DCC_SYMFLAG_SEC(r,w,x,s,m,u) \
 (((r)?DCC_SYMFLAG_SEC_R:0)|((w)?DCC_SYMFLAG_SEC_W:0)\
 |((x)?DCC_SYMFLAG_SEC_X:0)|((s)?DCC_SYMFLAG_SEC_S:0)\
 |((m)?DCC_SYMFLAG_SEC_M:0)|((u)?DCC_SYMFLAG_SEC_U:0))
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_SYMFLAG_PE_ITA_IND   0x08000000 /*< Try to perform PE:ITA-indirection on this symbol. */
#endif
#define DCC_SYMFLAG_SEC_ISIMPORT 0x10000100 /*< The section is not known at compile-time, but is linked as a shared library at run-time (The library name is used as section name). */
#define DCC_SYMFLAG_SEC_FIXED    0x20000000 /*< The section must be loaded to a fixed address already specified by 'sy_addr'.
                                             *  When multiple sections overlap at the same virtual address, it is the linker's job to solve such problems. */
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
 /*ref*/struct DCCSym     *sy_unit_before; /*< [0..1][->sy_name == sy_name] Used for redefining symbols: previous version of this symbol
                                            *                               within the same unit (e.g.: when defining local assembly labels). */
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
 /*ref*/struct DCCSym     *sy_peind;       /*< [0..1] When non-NULL, the ITA pointer for this imports on PE targets. */
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 /*ref*/struct DCCSym     *sy_alias;       /*< [0..1] When non-NULL, another label aliased by this one. */
 /*ref*/struct DCCSection *sy_sec;         /*< [0..1][const_if(!= NULL)] Section this symbol is defined inside of (or NULL if unknown).
                                            *   NOTE: If the address in this field points to a section who's 'sc_start' symbol is equal
                                            *         to this symbol, this symbol is actually a section and behaves as pointing to the
                                            *         start of that same section.
                                            *   WARNING: This field _must_ be NULL when 'sy_alias' isn't NULL, and the same holds true the other way around. */
 uint32_t                  sy_elfid;       /*< Used during binary generation: Elf Symbol ID. */
 /* 'sy_addr' acts as an offset in aliased symbols!
  * The ability to do this is required for proper symbol declaration in assembly:
  * >> # 'foobar' must be an alias for 'foo', with an offset of '8'.
  * >> # If it isn't, the symbol will be linked improperly if the linker descides to move foo
  * >> foobar = foo+8
  * >> 
  * >> _start:
  * >>    call foobar
  * >>    ret
  * >> 
  * >> foo:
  * >>    .skip 8
  * >>    push $text
  * >>    call printf
  * >>    add $4, %esp
  * >>    ret */
#ifdef DCC_PRIVATE_API
 /* NOTE: When defined inside a section, the following two hold a data reference
  *       to the pointed-to memory, meaning that upon symbol deletion, any data
  *       not otherwise shared with other symbols will be made available again.
  * WARNING: In the event of 'sy_sec' being an import section, this behavior will not take place.
  * WARNING: For this reason, 'sy_addr' and 'sy_size' may only be modified through
  *          symbol re-definitions with calls to 'DCCSym_Define', 'DCCSym_Alias' and 'DCCSym_SetSize' */
union{
 target_ptr_t              sy_addr;        /*< [const_if(sy_sec != NULL)] Symbol address (offset from associated section; undefined if 'sy_sec == NULL')
                                            *   NOTE: When this symbol depends on a library, this field may be used as a linker hint.
                                            *   NOTE: Base address of this section (or ZERO(0) if &DCCSection_Abs, or not relocated) */
 target_off_t              sy_off;         /*< [if(DCCSym_ISALIAS(self))] Offset added to the address of the aliased symbol. */
 target_ptr_t              sy_base;        /*< [if(DCCSym_ISSECTION(self))] Base address of this section (or ZERO(0) if &DCCSection_Abs, or not relocated) */
};
 target_siz_t              sy_size;        /*< Symbol size, or 0 if unknown (used for generating better debug information & tracking free section data)
                                            *  >> When a symbol who's size is known is deleted, the freed memory becomes available for future allocations within the accompaning section. */
 target_siz_t              sy_align;       /*< [!0] Minimum symbol alignment (Unused for symbol alias declarations).
                                            *  NOTE: For section symbols, this is the alignment of the section. */
#else
union{
 DCC(target_ptr_t)         sy_addr;        /*< [const_if(sy_sec != NULL)] Symbol address (offset from associated section; undefined if 'sy_sec == NULL')
                                            *   NOTE: When this symbol depends on a library, this field may be used as a linker hint.
                                            *   NOTE: Base address of this section (or ZERO(0) if &DCCSection_Abs, or not relocated) */
 DCC(target_off_t)         sy_off;         /*< [if(DCCSym_ISALIAS(self))] Offset added to the address of the aliased symbol. */
 DCC(target_ptr_t)         sy_base;        /*< [if(DCCSym_ISSECTION(self))] Base address of this section (or ZERO(0) if &DCCSection_Abs, or not relocated) */
};
 DCC(target_siz_t)         sy_size;        /*< Symbol size, or 0 if unknown (used for generating better debug information & tracking free section data)
                                            *  >> When a symbol who's size is known is deleted, the freed memory becomes available for future allocations within the accompaning section. */
 DCC(target_siz_t)         sy_align;       /*< [!0] Minimum symbol alignment (Unused for symbol alias declarations).
                                            *  NOTE: For section symbols, this is the alignment of the section. */
#endif
};
#define DCCSym_HASH(self) ((self)->sy_name->k_id)

/* Check if a given symbol is unused and can be deleted. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCCSym_ISUNUSED(self) \
  ((self)->sy_refcnt == 1 && !DCCSym_ISSECTION(self) && \
 !((self)->sy_flags&(DCC_SYMFLAG_DLLEXPORT|DCC_SYMFLAG_USED)) && !(self)->sy_peind)
#elif DCC_TARGET_BIN == DCC_BINARY_ELF
#define DCCSym_ISUNUSED(self) \
  ((self)->sy_refcnt == 1 && !DCCSym_ISSECTION(self) && \
 !((self)->sy_flags&DCC_SYMFLAG_USED) && \
    /* Unused symbols with global visibility can only \
     * be removed when they're imported from libraries. */\
  (((self)->sy_flags&(DCC_SYMFLAG_VISIBILITY|DCC_SYMFLAG_STATIC)) != DCC_SYMFLAG_NONE || \
    (DCCSym_ISDEFINED(self) && DCCSection_ISIMPORT(DCCSym_SECTION(self)))))
#else /* ... */
#define DCCSym_ISUNUSED(self) \
  ((self)->sy_refcnt == 1 && !DCCSym_ISSECTION(self) && \
 !((self)->sy_flags&DCC_SYMFLAG_USED))
#endif /* !... */

/* Similar to 'DCCSym_ISUNUSED', but only returns TRUE for symbols
 * that were never reference or defined, simple created and never used.
 * NOTE: The symbol may still be defined as a library import, but will
 *       still be considered as obsolete when not used otherwise. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCCSym_ISOBSOLETE(self) \
 ((self)->sy_refcnt == 1 && !(self)->sy_alias && !(self)->sy_peind &&\
 (!(self)->sy_sec || DCCSection_ISIMPORT((self)->sy_sec)))
#else
#define DCCSym_ISOBSOLETE(self) \
 ((self)->sy_refcnt == 1 && !(self)->sy_alias &&\
 (!(self)->sy_sec || DCCSection_ISIMPORT((self)->sy_sec)))
#endif



/* Assert various assumptions that can always be made about valid symbols. */
#if DCC_DEBUG
#define DCCSym_DOASSERT(self) \
 (DCC_ASSERT((self)->sy_name),\
  DCC_ASSERTF( (self)->sy_sec_pself || (!(self)->sy_sec_next),"Invalid linkage in section symbol map"),\
  DCC_ASSERTF(!(self)->sy_sec_pself || *(self)->sy_sec_pself == (self),"The self-pointer is mapped inforrectly"),\
  DCC_ASSERTF(((self)->sy_sec != NULL) == ((self)->sy_sec_pself != NULL || DCCSym_ISSECTION(self)),"Section and section-self-pointer existance must mirror each other"),\
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
#define DCCSym_Decref(self)    (void)(DCC_ASSERT((self)->sy_refcnt),--(self)->sy_refcnt || (_DCCSym_Delete(self),0))
#define DCCSym_XIncref(self)   ((self) ? DCCSym_Incref(self) : (void)0)
#define DCCSym_XDecref(self)   ((self) ? DCCSym_Decref(self) : (void)0)
DCCFUN void _DCCSym_Delete(struct DCCSym *__restrict self);

/* Load the effective address of a given symbol, resolving symbol aliasing.
 * WARNING: This function should only be called during linkage, once all symbols are known.
 * WARNING: 'result->sa_sym' is _NOT_ filled with a reference, but simply a dangling pointer.
 * @param: self: The symbol to load.
 * @param: load_weak: When non-ZERO, allow loading of weak symbols (which may be re-defined)
 * @return: 0: Failed to resolve the symbol as it, or a symbol it is aliasing is not defined.
 *             The contents of 'result' are undefined in this case.
 * @return: 1: Successfully loaded the symbol, filling 'result->sa_sym' with one conforming to 'DCCSym_ISDEFINED()'.
 *             In this case, 'result->sa_off' is filled with the sum of alias offsets of all symbols. */
DCCFUN int DCCSym_LoadAddr(struct DCCSym *__restrict self,
                           struct DCCSymAddr *__restrict result,
                           int load_weak);

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
 * NOTES:
 *   - If 'self' was already defined, emit a warning, but allow it to be re-defined.
 *   - When 'size' is non-ZERO, the reference counter of the pointed-to data in
 *     the given section 'section' will be incremented using 'DCCSection_DIncRef'.
 *     With that in mind, it is _NOT_ the responsibility of the caller to track
 *     symbol data reference counts! */
DCCFUN void DCCSym_Define(struct DCCSym *__restrict self,
                          struct DCCSection *__restrict section,
                          DCC(target_ptr_t) addr,
                          DCC(target_siz_t) size,
                          DCC(target_siz_t) align);
DCCFUN void DCCSym_Alias(struct DCCSym *__restrict self,
                         struct DCCSym *__restrict alias_sym,
                         DCC(target_ptr_t) offset);

/* Same as 'DCCSym_Define', but no redefinition warnings are emit. */
DCCFUN void DCCSym_Redefine(struct DCCSym *__restrict self,
                            struct DCCSection *__restrict section,
                            DCC(target_ptr_t) addr,
                            DCC(target_siz_t) size,
                            DCC(target_siz_t) align);

/* Set the size of a given symbol to 'size', updating
 * the reference counter of potentially mapped data.
 * Note though, that while setting the size of an undefined symbol
 * is technically allowed, there is not much point to it as
 * it being re-defined as either an alias, or section symbol
 * will override the stored size unless the caller at that time
 * would explicitly state the stored size as new size. */
DCCFUN void DCCSym_SetSize(struct DCCSym *__restrict self,
                           DCC(target_siz_t) size);

/* Define a given symbol 'self' to describe the symbol address 'symaddr'.
 * During this operation, 'self' will be redefined as offset-alias, or section/abs symbol. */
DCCFUN void DCCSym_DefAddr(struct DCCSym *__restrict self,
                           struct DCCSymAddr const *__restrict symaddr);

/* Clear the current definition of 'self' */
DCCFUN void DCCSym_ClrDef(struct DCCSym *__restrict self);

/* Explicitly import a given symbol from a specific 'import_sec' */
#define DCCSym_Import(self,import_sec,hint) \
       (assert(DCCSection_ISIMPORT(import_sec)),\
        DCCSym_Define(self,import_sec,hint,0,1))

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
#   define DCC_R_NUM       R_386_NUM
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
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
#   define DCC_R_NUM       R_X86_64_NUM
#else
#   error FIXME
#endif

/* The size of memory affected relocations (in bytes).
 * NOTE: Relocations with unknown affect ranges have this set to ZERO(0). */
DCCDAT size_t const DCC_relsize[DCC_R_NUM];
#define DCC_RELSIZE(r) ((size_t)(r) >= DCC_R_NUM ? 0 : DCC_relsize[r])


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
 DCC(target_siz_t)    fr_size; /*< Range size. */
};

struct DCCFreeData {
 /* Tracking of unallocated data within a section/on the stack. */
 struct DCCFreeRange *fd_begin; /*< [0..1][owned|owned(->fr_next->...)]
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
#define DCCFreeData_Acquire(self,size,align,offset) \
        DCCFreeData_AcquireBelow(self,(DCC(target_ptr_t))-1,size,align,offset)

DCCFUN DCC(target_ptr_t) /* Only acquire pointers below 'below_addr' */
DCCFreeData_AcquireBelow(struct DCCFreeData *__restrict self,
                         DCC(target_ptr_t) below_addr,
                         DCC(target_siz_t) size, DCC(target_siz_t) align,
                         DCC(target_siz_t) offset);
DCCFUN DCC(target_ptr_t)
DCCFreeData_AcquireAt(struct DCCFreeData *__restrict self,
                      DCC(target_ptr_t) addr, DCC(target_siz_t) size);
DCCFUN void
DCCFreeData_Release(struct DCCFreeData *__restrict self,
                    DCC(target_ptr_t) addr, DCC(target_siz_t) size);

/* Check if a given address range is part of the free data.
 * @return: 0 : No address within the range is marked as free.
 * @return: 1 : Some portion of the given range is marked.
 * @return: 2 : The entirety of the given range is marked. */
DCCFUN int
DCCFreeData_Has(struct DCCFreeData *__restrict self,
                DCC(target_ptr_t) addr, DCC(target_siz_t) size);


struct DCCAllocRange {
 struct DCCAllocRange *ar_next;   /*< [0..1][->ar_addr > ar_addr+ar_size] Next range (Address range of this must neither overlap, or touch 'ar_addr+ar_size', as well as be greater) */
#ifdef DCC_PRIVATE_API
 target_ptr_t          ar_addr;   /*< Start address. */
 target_siz_t          ar_size;   /*< [!0] Range size. */
#else
 DCC(target_ptr_t)     ar_addr;   /*< Start address. */
 DCC(target_siz_t)     ar_size;   /*< [!0] Range size. */
#endif
 unsigned int          ar_refcnt; /*< [!0] Range reference counter (When this hits ZERO, the range is deallocated). */
};



struct DCCTextBuf {
 uint8_t *tb_begin; /*< [0..1][<= tb_end][owned] Start pointer for assembly code. */
 uint8_t *tb_end;   /*< [0..1][>= tb_begin] Compile-time allocated code end. */
 uint8_t *tb_max;   /*< [0..1][>= tb_pos] Used code end (overflow above 'tb_end' symbolically describes zero-initialized memory). */
 uint8_t *tb_pos;   /*< [0..1][>= tb_begin] Current code position (NOTE: May be placed above 'tb_end' for lazy alloc-on-write).
                     * >> Everything between this and 'tb_max' should be considered as ZERO-initialized, virtual memory. */
};
#define DCCTextBuf_ADDR(self)            ((DCC(target_ptr_t))((self)->tb_pos-(self)->tb_begin))
#define DCCTextBuf_SETADDR(self,a) (void)((self)->tb_pos = (self)->tb_begin+(a))

struct DCCSection {
 /* Descriptor for a section/library dependency. */
 struct DCCSym         sc_start; /*< Start symbol (always a label at offset '0' of this same section).
                                  *  NOTE: This symbol also contains the name & flags of this section. */
 size_t                sc_symc;  /*< Amount of symbols in 'sc_symv'. */
 size_t                sc_syma;  /*< Allocated bucket count in 'sc_symv'. */
 struct DCCSym       **sc_symv;  /*< [0..1][chain(->sy_sec_pself|->sy_sec_next->...)][0..sc_syma][owned]
                                  *   Hash-map of symbols declared within this section. */
 /* NOTE: The linked list described by the following pointer triple depends on 'DCCSection_ISIMPORT(self)'. */
 struct DCCUnit       *sc_unit;  /*< [0..1] Unit associated with this section. */
 struct DCCSection   **sc_pself; /*< [1..1|==self][0..1] Self-pointer in the 'sc_next->...' section chain. */
 struct DCCSection    *sc_next;  /*< [0..1][chain(->sc_next->...)] Next section in the unit with the same moduled name-id. */
 /* NOTE: None of the below are available when 'DCCSection_ISIMPORT(self)' evaluates to non-ZERO(0). */
 struct DCCTextBuf     sc_text;  /*< Flushed section text buffer (May not be up-to-date for the current section). */
 struct DCCFreeData    sc_free;  /*< Tracker for free section data. */
 /* Why is reference-counted section memory important? - This is why:
  *     Because now that assembly can define symbol sizes, we can no
  *     longer safely use those values for section data tracking.
  *     Instead, changing the size in assembly must acquire/release
  *     references which is going to be a requirement for keeping
  *     data references throughout object file generation.
  * PROBLEM: Two assembly symbols describe the same area of memory (aka. hard-alias)
  * >> a:                    
  * >> b:                    
  * >> .string "This is the data"                    
  * >> .size a, . - a
  * >> .size b, . - b
  * When neither 'a', nor 'b' are used, both will try to free the same data.
  * >> CRASH!
  * ALSO: This will be required for object-file hard aliases. */
 struct DCCAllocRange *sc_alloc; /*< [0..1][chain(->ar_next->...)] Reference-counted tracking of section text. */
 DCC(target_ptr_t)     sc_merge; /*< Used during merging: Base address of merge destination. */
#if DCC_TARGET_BIN == DCC_BINARY_ELF
 struct DCCSection    *sc_elflnk; /*< [0..1] Used by ELF: Link section. */
#endif /* DCC_TARGET_BIN == DCC_BINARY_ELF */
 size_t                sc_relc;  /*< Amount of relocations currently in use. */
 size_t                sc_rela;  /*< Amount of allocated relocations. */
 struct DCCRel        *sc_relv;  /*< [0..sc_relc|alloc(sc_rela)|sort(->r_addr,<)][owned]
                                  *   Vector of relocations to memory within this section. */
};

/* Builtin section: ABS (Absolute section always relocated & with a base at ZERO) */
DCCDAT struct DCCSection DCCSection_Abs;

#define DCCSection_ISIMPORT(self)   ((self)->sc_start.sy_flags&DCC_SYMFLAG_SEC_ISIMPORT)

#define DCCSection_Incref(self)     (void)(++(self)->sc_start.sy_refcnt)
#define DCCSection_Decref(self)     (void)(--(self)->sc_start.sy_refcnt || (_DCCSym_Delete(&(self)->sc_start),0))
#define DCCSection_XIncref(self)    ((self) ? DCCSection_Incref(self) : (void)0)
#define DCCSection_XDecref(self)    ((self) ? DCCSection_Decref(self) : (void)0)

#define DCCSection_BASE(self)             ((self)->sc_start.sy_addr)
#define DCCSection_SETBASE(self,v)  (void)((self)->sc_start.sy_addr=(v))
#define DCCSection_VSIZE(self) ((DCC(target_siz_t))((self)->sc_text.tb_max-(self)->sc_text.tb_begin))
#define DCCSection_MSIZE(self) ((DCC(target_siz_t))(((self)->sc_text.tb_end < (self)->sc_text.tb_max ? \
                                                     (self)->sc_text.tb_end : (self)->sc_text.tb_max)-\
                                                     (self)->sc_text.tb_begin))

#ifdef DCC_SYMFLAG_SEC_OWNSBASE
#define DCCSection_TEXTBASE(self) \
 ((self)->sc_start.sy_flags&DCC_SYMFLAG_SEC_OWNSBASE) \
  ? (uint8_t *)DCCSection_BASE(self) : ((self)->sc_text.tb_begin)
#else
#define DCCSection_TEXTBASE(self) ((self)->sc_text.tb_begin)
#endif

#define DCCSection_HASBASE(self) \
 (DCCSection_BASE(self) || ((self)->sc_start.sy_flags&DCC_SYMFLAG_SEC_FIXED))

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


/* Looking at the reference counters of section data,
 * mark all regions of section memory not in use as free.
 * All free memory regions that existed before a call to
 * this function will be dropped, meaning that after a
 * call to this function, free memory will map perfectly
 * against refcnt=0 regions.
 * NOTE: Calling this function is the first step to performing
 *       late section collapsing, where unused section memory
 *       is dropped and used memory is moved downwards in order
 *       to keep unused memory regions as small as possible,
 *       as well as get rid of any defined, but in the end
 *       unused constructs, such as unused static/inline
 *       functions in headers.
 * NOTE: This step of code generation of completely optional! */
DCCFUN void DCCSection_FreeUnused(struct DCCSection *__restrict self);

/* Similar to 'DCCSection_CollapseSymbols', but instead of collapsing
 * symbols to re-use free memory regions at lower addresses, merge
 * symbols that describe the same data.
 * Using this function, symbol data can be merged very late, allowing
 * for whole-program optimization for same strings, such as when
 * two source files use the same string constant, or a string
 * is used to initialize a static variable inside a header.
 * NOTE: Symbol addresses and relocations will be updated accordingly.
 * NOTE: Per-symbol alignment is respected.
 * @return: * : The amount of symbols merged into the data locations of others. */
DCCFUN size_t DCCSection_MergeSymbols(struct DCCSection *__restrict self);

/* Looking at all symbols defined in the section, as well as all
 * regions of memory marked as free, try to move symbols to lower
 * memory locations, thereby allowing for late removal of unused
 * data, as well as erasure of associated references.
 * NOTE: Symbol addresses and relocations will be updated accordingly.
 * NOTE: Per-symbol alignment is respected.
 * @return: * : The amount of symbols that were moved. */
DCCFUN size_t DCCSection_CollapseSymbols(struct DCCSection *__restrict self);

/* Check if the section contains a free address range at its end, and
 * if so: trim the section's text to not include that last portion.
 * HINT: To fully optimize symbol memory usage within
 *       a section, the following calls are performed:
 *       #1: DCCSection_FreeUnused(sec);
 *       #2: DCCSection_MergeSymbols(sec);
 *       #3: DCCSection_CollapseSymbols(sec);
 *       #4: DCCSection_TrimFree(sec);
 * @return: * : The amount of bytes successfully trimmed from the end.
 */
DCCFUN DCC(target_siz_t) DCCSection_TrimFree(struct DCCSection *__restrict self);


/* Add a new relocation
 * NOTE: The given 'relo' is copied, meaning
 *       its symbol reference is _NOT_ inherited.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_Putrelo(struct DCCSection *__restrict self,
                               struct DCCRel const *__restrict relo);

/* Allocate 'n_relocs' relocations and return them.
 * NOTE: Following this, the caller is responsible for initializing them.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: NULL: Failed to allocate more relocations (A lexer error was set). */
DCCFUN struct DCCRel *
DCCSection_Allocrel(struct DCCSection *__restrict self,
                    size_t n_relocs, DCC(target_ptr_t) min_addr);

/* Delete all relocations inside 'addr...+=size'
 * @return: * : The amount of deleted relocations.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN size_t
DCCSection_Delrel(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr,
                  DCC(target_siz_t) size);

/* Check if there are relocations within the given address range.
 * @return: 0 : No relocations where found.
 * @return: !0 : At least one relocation was found.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN int
DCCSection_Hasrel(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr,
                  DCC(target_siz_t) size);

/* Retrieve a vector relocations inside the given address range.
 * WARNING: This function returning NULL does not necessarily
 *          coincide with '*relc' being set to ZERO.
 *          To handle no-relocations, the caller must check '*relc'. */
DCCFUN struct DCCRel *
DCCSection_Getrel(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr,
                  DCC(target_siz_t) size,
                  size_t *__restrict relc);

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

/* Move all relocations within the address
 * range 'old_addr..+=n_bytes' to a new
 * memory location at 'new_addr'.
 * This function is internally called
 * when section data is re-sized.
 * @return: * : The amount of relocations moved.
 * WARNING: When both relocations & data must be moved,
 *          relocations must be shifted first, as DISP
 *          relocations have to be adjusted to a new
 *          section offset.
 */
DCCFUN size_t
DCCSection_Movrel(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) new_addr,
                  DCC(target_ptr_t) old_addr,
                  DCC(target_siz_t) n_bytes);


/* Try to resolve disposition relocations.
 * >> This function handles any redundant relocation
 *    pointing back into the same section.
 * NOTE: All relocations executed are replaced with 'DCC_R_NONE'
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: * : The amount of executed relocations. */
DCCFUN size_t DCCSection_ResolveDisp(struct DCCSection *__restrict self);

/* Execute relocations on the section.
 * WARNING: The caller is responsible to set all section base addresses beforehand.
 * WARNING: If any relocation depends on a section that had yet to
 *          have its base set, this function may link incorrectly.
 * NOTE: Unresolved reference warning will be emit for any undefined,
 *       non-weak symbol that is not imported from a library.
 * NOTE: All relocations executed are replaced with 'DCC_R_NONE'.
 * @requires: !DCCSection_ISIMPORT(self) 
 * @param: resolve_weak: When non-ZERO, resolve relocations to weak symbols.
 *                    >> When a relocation is liked against a weak symbol,
 *                       this function will attempt to resolve that symbol
 *                       at compile-time.
 *              WARNING: This also means that such weak symbols cannot
 *                       be re-defined at a later point, meaning that this
 *                       argument is only of meaning for binary targets
 *                       that do not support weak linking (such as PE),
 *                       or when code is to be executed in immediate mode.
 *                 HINT: When this argument is non-ZERO, all existing
 *                       relocations (recognized by the linker) will have
 *                       been executed, meaning that no further address
 *                       resolution will be required at any future point.
 * @return: * : The amount of executed relocations. */
DCCFUN size_t DCCSection_Reloc(struct DCCSection *__restrict self, int resolve_weak);

/* Set the base address of a given section.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_SetBaseTo(struct DCCSection *__restrict self, DCC(target_ptr_t) address);

#if DCC_HOST_CPU == DCC_TARGET_CPU
/* Allocate a base address for the given section.
 * This function is required for execution of code after it has been generated.
 * NOTE: Upon failure, a lexer error is set.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void DCCSection_SetBase(struct DCCSection *__restrict self);
#endif /* DCC_HOST_CPU == DCC_TARGET_CPU */

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

/* Attempt to access section memory at a given, unvalidated offset 'addr'.
 * HINT: Unlike 'DCCSection_GetText', 'self' is allowed
 *       to be the currently selected text section.
 * @param: max_msize: When non-NULL, filled with the max amount of
 *                    compile-time memory accessible, starting at '@return'
 * @param: max_vsize: The max amount of virtual memory (Always >= '*max_msize')
 *                    This memory size also considers unallocated (aka. ZERO-)memory.
 * @return: NULL: Invalid address (When given, '*max_msize' and '*max_vsize' are set to ZERO(0))
 * @return: * :   Compile-time address of text data. */
DCCFUN void *
DCCSection_TryGetText(struct DCCSection *__restrict self,
                      DCC(target_ptr_t) addr,
                      size_t            *max_msize,
                      DCC(target_siz_t) *max_vsize);

#define DCCSection_TEXTOFF(self,p) \
 (target_ptr_t)((uint8_t *)(p)-((self) == unit.u_curr ? unit.u_tbuf.tb_begin : (self)->sc_text.tb_begin))


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
 * @return: addr:                Successfully allocated memory at the given address.
 * @return: DCC_FREEDATA_INVPTR: The specified memory location is already in use. */
DCCFUN DCC(target_ptr_t)
DCCSection_DAllocAt(struct DCCSection *__restrict self,
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
 * WARNING: When the 'DCC_LINKER_FLAG_O_NOMERGESYM' linker flag is set, no data merging is performed!
 * NOTE: If the 'DCC_SYMFLAG_SEC_M' flag isn't set, this function returns 'addr' immediately.
 * @requires: !DCCSection_ISIMPORT(self)
 */
DCCFUN DCC(target_ptr_t)
DCCSection_DMerge(struct DCCSection *__restrict self,
                  DCC(target_ptr_t) addr, DCC(target_siz_t) size,
                  DCC(target_siz_t) min_align, int free_old);

/* Same as 'DCCSection_DAlloc', but if the 'DCC_SYMFLAG_SEC_M' flag is set in
 * the given section, symbol data may be merged with equivalent, already-existing data.
 * >> This is equal to calling 'DCCSection_DAlloc()', 'DCCSection_GetText()' and 'DCCSection_DMerge()'
 * WARNING: When the 'DCC_LINKER_FLAG_O_NOMERGESYM' linker flag is set, no data merging is performed!
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
 * NOTE: The symbol is added to the current unit as an unnamed, static definition.
 * NOTE: A reference to the allocated memory will be associated
 *       with the symbol, meaning that data will be released
 *       through 'DCCSection_DDecRef' upon the symbol's deletion.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN struct DCCSym *
DCCSection_DAllocSym(struct DCCSection *__restrict self,
                     void const *__restrict memory,
                     DCC(target_siz_t) mem_size, DCC(target_siz_t) size,
                     DCC(target_siz_t) align, DCC(target_siz_t) offset);

/* Free a given section address range & clear all relocations inside.
 * NOTE: This function differs from section incref/decref,
 *       as associated memory is freed immediately.
 * @requires: !DCCSection_ISIMPORT(self) */
DCCFUN void
DCCSection_DFree(struct DCCSection *__restrict self,
                 DCC(target_ptr_t) addr, DCC(target_siz_t) size);

/* Increments the reference counter of shared section data.
 * NOTES:
 *   - Unlike how one might expect, section data allocated through
 *    'DCCSection_DAlloc*' and friends is initialized to ZERO(0)
 *     references, meaning that to begin using reference-counted
 *     section memory, the data must first be incref'd.
 *   - Though in most cases, the job of adding said reference
 *     does not fall on the caller, as most uses of section
 *     data allocators are to later define a symbol, which in
 *     turn is going to incref() the associated memory itself,
 *     thereby creating a dependency branch from the symbol being
 *     used towards the associated section data.
 *   - Out-of-bounds address ranges will be allocated symbolically,
 *     meaning that it is possible to hold references to unallocated
 *     regions of memory.
 *   - Simply because this function never modifies the section
 *     text buffer, it is fully legitimate to pass 'unit.u_curr'
 *     as argument for 'self', meaning this function still
 *     behaves properly, even when the passed section is
 *     the one currently selected as text target.
 * WARNING: Upon internal failure, a lexer error may be set.
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: 0: Failed to allocate a dynamic reference-counter range (A lexer error was set).
 * @return: 1: Successfully modified all affected reference counters. */
DCCFUN int
DCCSection_DIncref(struct DCCSection *__restrict self,
                   DCC(target_ptr_t) addr,
                   DCC(target_siz_t) size);

/* Doing the opposite of 'DCCSection_DIncref', the reference
 * counter associated with the given address range is
 * decremented, with the additional side-effect being
 * that any part of the address range containing a
 * reference counter that is then going to be ZERO(0), is
 * going to be de-allocated with a call to 'DCCSection_DFree'.
 * NOTE: In the event that data is freed using 'DCCSection_DFree',
 *       additional checks are performed to ensure that no out-of-bounds
 *       data would be marked as free, with a warning being emit
 *       in the event that data was out-of-bounds.
 * On the flip-side though, just as with 'DCCSection_DIncref',
 * it is still possible to pass out-of-bounds address ranges
 * simply because of the ability of explicitly setting/overriding
 * a symbol's size from assembly.
 * WARNING: Upon internal failure, a lexer error may be set.
 * NOTE: Unlike all other data/text-related section functions,
 *       it is allowed to pass 'unit.u_curr' for 'self', as the
 *       function will automatically flush/restore the section
 *       text buffer in the event that it must be modified
 *       when data is freed during a call to 'DCCSection_DFree'
 * @requires: !DCCSection_ISIMPORT(self)
 * @return: 0: Failed to allocate a dynamic reference-counter range (A lexer error was set).
 * @return: 1: Successfully modified all affected reference counters. */
DCCFUN int
DCCSection_DDecref(struct DCCSection *__restrict self,
                   DCC(target_ptr_t) addr,
                   DCC(target_siz_t) size);

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
 size_t                 u_symc;  /*< Amount of symbols in 'u_symv'. */
 size_t                 u_syma;  /*< Allocated bucket count in 'u_symv'. */
 /*ref*/struct DCCSym **u_symv;  /*< [0..1][chain(sy_sec_pself->...)][0..u_syma][owned]
                                  *   Hash-map of all public/private/protected symbols & section
                                  *   declared within this compilation unit. */
 size_t                 u_secc;  /*< Amount of sections reachable through the 'u_secs->sc_next->...' chain. */
 struct DCCSection     *u_secs;  /*< [0..1][chain(sc_next->...)] Pointer to the lastly allocated section, or NULL if no sections exist.
                                  *   NOTE: This chain does _NOT_ contain library imports. */
 size_t                 u_impc;  /*< Amount of imports reachable through the 'u_imps->sc_next->...' chain. */
 struct DCCSection     *u_imps;  /*< [0..1][chain(sc_next->...)] Pointer to the lastly allocated import, or NULL if no imports exist.
                                  *   NOTE: This chain _ONLY_ contains library imports. */
 /*ref*/struct DCCSym  *u_nsym;  /*< [0..1] Linked list of unnamed symbols. */
 size_t                 u_nsymc; /*< Amount of unnamed symbols reachable through 'u_nsym'. */
 struct DCCSection     *u_text;  /*< [0..1] Default section: '.text' (Used for read/execute code) */
 struct DCCSection     *u_data;  /*< [0..1] Default section: '.data' (Used for read-only data) */
 struct DCCSection     *u_bss;   /*< [0..1] Default section: '.text' (Used for read/write data) */
 struct DCCSection     *u_str;   /*< [0..1] Default section: '.str'  (Used for read-only, mergeable data) */
 struct DCCSection     *u_prev;  /*< [0..1] The section selected before the current one (or NULL if none was). */
 struct DCCSection     *u_curr;  /*< [0..1] Currently selected section (target for writing text). */
 struct DCCTextBuf      u_tbuf;  /*< Current text buffer (in-lined local cache for 'u_curr->sc_text') */
};

/* Global object: The current compilation unit. */
DCCDAT struct DCCUnit DCCUnit_Current;

DCCFUN void DCCUnit_Init(struct DCCUnit *__restrict self);
DCCFUN void DCCUnit_Quit(struct DCCUnit *__restrict self);

/* Merge 'other' into the currently selected unit.
 * NOTES:
 *   - Before calling this function, the caller should first call:
 *     >> DCCUnit_SetCurr(NULL); // Make sure no section is selected.
 *     >> DCCUnit_Merge(other);
 *   - If the given unit 'other' is empty, this function is a no-op.
 *   - If the current unit is empty, this function behaves the same as:
 *     'DCCUnit_Quit(&DCCUnit_Current),DCCUnit_Restore(other);'
 *   - Upon return, the given unit 'other' may no longer be used for
 *     anything, but must still be destroyed using 'DCCUnit_Quit(other)' */
DCCFUN void DCCUnit_Merge(struct DCCUnit *__restrict other);

/* Extract and re-initialize the current unit,
 * storing the old one in '*other'.
 * NOTE: This function does a bit more than simply moving
 *       some memory, as it updates section-unit pointers.
 * NOTE: This function will leave '&DCCUnit_Current' in
 *       an undefined state, meaning that the caller is
 *       left responsible to call 
 * WARNING: You may not pass '&DCCUnit_Current' for other. */
DCCFUN void DCCUnit_Extract(struct DCCUnit *__restrict other);

/* Overwrite the current using using '*other'.
 * This is the reverse operation of 'DCCUnit_Extract()'
 * WARNING: The caller is responsible to call
 *         'DCCUnit_Quit(&DCCUnit_Current)' in
 *          the event that the current unit was
 *          initialized before.
 * NOTE: '*other' must have previously been initialized with
 *       'DCCUnit_Extract()' or 'DCCUnit_Init()'
 * WARNING: You may not pass '&DCCUnit_Current' for other. */
DCCFUN void DCCUnit_Restore(struct DCCUnit *__restrict other);

/* Clear any preallocated memory reachable from 'self'
 * @param: flags: A set of 'DCCUNIT_FLUSHFLAG_*' (Unknown flags are ignored) */
DCCFUN void DCCUnit_Flush(struct DCCUnit *__restrict self, uint32_t flags);
#define DCCUNIT_FLUSHFLAG_NONE   0x00000000
#define DCCUNIT_FLUSHFLAG_SECMEM 0x00000001 /*< Clear pre-allocated section text memory (when 'tb_max < tb_end', lower 'tb_end' to 'tb_max') */
#define DCCUNIT_FLUSHFLAG_SYMTAB 0x00000002 /*< Shrink the allocated size of symbol hash tables to the minimum assumed by automatic rehashing. */
#define DCCUNIT_FLUSHFLAG_TABMIN 0x00000010 /*< More aggressive behavior for 'DCCUNIT_FLUSHFLAG_SYMTAB': Reduce the hash size to ONE(1) (Only use this as last way out) */
#define DCCUNIT_FLUSHFLAG_RELOCS 0x00000100 /*< Clear unused relocations. */



#define DCCUNIT_POP_DISCARD_NEW 0 /* Discard the new unit and restore the old one. */
#define DCCUNIT_POP_MERGE       1 /* Merge the new unit with the old. */
#define DCCUNIT_POP_DISCARD_OLD 2 /* Discard the old unit and keep the new one. */

/* Push/Pop a new compilation unit, allowing you to either merge the
 * units during pop, or discard either the new, or the old one.
 * @param: mode: One of 'DCCUNIT_POP_*'
 * HINT: Since DISCARD_NEW is ZERO(0) and MERGE is ONE(1),
 *       the most common way to to uses macros is:
 *    >> DCCUnitPush();
 *    >> generate_code();
 *    >> DCCUnitPop(OK); // Merge if OK, otherwise: discard new unit
 */
#define DCCUnit_Push() \
do{ struct DCCUnit _old_unit; \
    DCCUnit_Extract(&_old_unit);\
    DCCUnit_Init(&DCCUnit_Current)
#define DCCUnit_Pop(mode) \
    switch ((int)(mode)) {\
    case DCCUNIT_POP_DISCARD_NEW:\
     DCCUnit_Quit(&DCCUnit_Current);\
     DCCUnit_Restore(&_old_unit);\
     break;\
    default:\
     DCCUnit_Merge(&_old_unit);\
    case DCCUNIT_POP_DISCARD_OLD:\
     DCCUnit_Quit(&_old_unit);\
     break;\
    }\
}while(DCC_MACRO_FALSE)


#define DCCUnit_ENUMSYM(sym) \
 for (struct DCCSym **sym_iter = DCCUnit_Current.u_symv,\
                    **sym_end  = sym_iter+DCCUnit_Current.u_syma;\
      sym_iter != sym_end; ++sym_iter)\
 for ((sym) = *sym_iter; (sym); (sym) = (sym)->sy_unit_next)
#define DCCUnit_ENUMNSYM(sym) \
 for ((sym) = DCCUnit_Current.u_nsym; (sym); (sym) = (sym)->sy_unit_next)
#define DCCUnit_ENUMALLSYM(sym) \
 for (struct DCCSym **sym_iter = DCCUnit_Current.u_symv,\
                    **sym_end  = sym_iter+DCCUnit_Current.u_syma,\
                     *sym_nsym = DCCUnit_Current.u_nsym;\
      (sym_iter < sym_end) || sym_nsym; ++sym_iter)\
 for ((sym_iter < sym_end) ? (sym) = *sym_iter : ((sym) = sym_nsym,sym_nsym = NULL); \
     (sym); (sym) = (sym)->sy_unit_next)

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
 * >> Essentially, this function performs unused symbol elimination
 * NOTE: When the 'DCC_LINKER_FLAG_O_CLRUNUSED'
 */
DCCFUN size_t DCCUnit_ClearUnused(void);

/* Clear all libraries without any used functions. */
DCCFUN size_t DCCUnit_ClearUnusedLibs(void);

/* Collapse symbols in all sections, re-claiming unused memory.
 * NOTE: When the 'DCC_LINKER_FLAG_O_COLLSEC' linker flag isn't set,
 *       this function is a no-op and always returns ZERO(0)
 * WARNING: This optimization must be performed
 *          before DISP relocations are resolved!
 * HINT: For best results, perform this optimization after
 *       unused symbols and libraries have been removed.
 * @return: * : The sum of bytes that could be trimmed from sections. */
DCCFUN DCC(target_siz_t) DCCUnit_CollapseSections(void);

/* Resolve disposition relocations in all sections. */
DCC_LOCAL void DCCUnit_ResolveDisp(void) {
 struct DCCSection *sec;
 DCCUnit_ENUMSEC(sec) DCCSection_ResolveDisp(sec);
}

/* Clear all unused symbols that were defined as static,
 * essentially removing all unused static declaration. */
DCCFUN size_t DCCUnit_ClearStatic(void);

/* Clear all obsolete symbols and sections.
 * While this function falls under the same restrictions
 * as 'DCCUnit_ClearUnused', it is still less powerful in
 * that it will only clear symbols that are considered obsolete,
 * where obsolete is defined as a symbol that is never used in any
 * way, shape or form, as well as never defined as anything, with
 * the exception that library import symbol must only be never used.
 * -> By default, this function is usually called before an object
 *    file is generated in an attempt not to include symbols from
 *    declarations in headers that aren't actually used by the
 *    compilation unit:
 *    >> // Pull in symbol definitions from <stdio.h>
 *    >> #include <stdio.h>
 *    >>
 *    >> // When generating an object file, we only need
 *    >> // 2 symbols 'my_log' and 'printf'.
 *    >>
 *    >> // And even though many more symbols have been defined,
 *    >> // such as 'puts' and 'fopen', they can all be considered
 *    >> // obsolete and are removed before the object file is created.
 *    >>
 *    >> int my_log(char const *msg) {
 *    >>     return printf("LOG: %s\n",msg);
 *    >> }
 * WARNING: The USED flag does not affect whether or not a symbol
 *          is considered obsolete, as it is only designed for
 *          being used on actual symbol implementations.
 * HINT: Clearing obsolete symbols should never emit warnings about them being unused!
 * NOTE: This function must not be called when 'DCCUnit_ClearUnused' is
 *       invoked as well. - All that this function can ever do is also
 *       performed by 'DCCUnit_ClearUnused'! */
DCCFUN size_t DCCUnit_ClearObsolete(void);

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


#define DCC_LIBDEF_FLAG_NONE   0x00000000

/* Try to perform static linkage.
 *  - When this flag is set, libraries will be linked statically when possible.
 *  - Note though, that if a static library uses dynamic import itself,
 *    those imports will be added to the the current compilation unit.
 *  - If the library can only be imported dynamically, it will be
 *    imported as that, unless the 'DCC_LIBDEF_FLAG_NODYN' flag is set.
 * WARNING: The caller is responsible to ensure that the current compilation unit is empty.
 *          If the intend is to load a static binary into an existing one,
 *          unit merging must be performed using 'DCCUnit_Merge()'.
 */
#define DCC_LIBDEF_FLAG_STATIC 0x00000001

/* Don't allow dynamic libraries to be loaded (Should be used with 'DCC_LIBDEF_FLAG_STATIC')
 * NOTE: When this flag isn't set and a library can both be imported statically & dynamically,
 *       it will always be imported statically when the 'DCC_LIBDEF_FLAG_STATIC' flag is set,
 *       meaning that this flag isn't required when intending to import something statically if possible! */
#define DCC_LIBDEF_FLAG_NODYN  0x00000002

/* Don't emit a warning when no library matching the given description was found.
 * Setting this flag might be useful during internal library load calls
 * where the intend is to try additional approaches should the standard way fail. */
#define DCC_LIBDEF_FLAG_NOWARNMISSING 0x00000004

/* Don't check the standard library search path,
 * but instead use the given filename as only option. */
#define DCC_LIBDEF_FLAG_NOSEARCHSTD 0x00000008

/* Don't try various file extensions of the 'lib'
 * filename prefix when searching for library files. */
#define DCC_LIBDEF_FLAG_NOSEARCHEXT 0x00000010

/* Always name the library after the given user-name (def->ld_name).
 * NOTE: This flag has no effect when linking is performed statically. */
#define DCC_LIBDEF_FLAG_USERNAME 0x00000020

/* Don't attempt to load library files, but always assume
 * that the input file is some kind of source file (see below.)
 * NOTE: If none of the 'DCC_LIBDEF_FLAG_*_SOURCE' flags are
 *       or'd together with this one, the library loader will
 *       always fail.
 * NOTE: This flag implies 'DCC_LIBDEF_FLAG_NODYN' behavior.
 * NOTE: Only when this flag is set, can file streams not supporting
 *       the seek interface be used as with 'DCCUnit_ImportStream',
 *       as this flag will also prevent seek operations on 'fd', which
 *       would otherwise be necessary for loading header magic.
 */
#define DCC_LIBDEF_FLAG_ONLY_SOURCE 0x00000040

/* After parsing a source file, don't reset
 * macros, assertions, keyword flags, etc. */
#define DCC_LIBDEF_FLAG_NO_RESET_TPP 0x00000100

/* This library defines an internal object.
 * When this flag is set, a special, hard-coded
 * search path is used for locating the object. */
#define DCC_LIBDEF_FLAG_INTERN     0x00008000

/* When used alongside 'DCC_LIBDEF_FLAG_STATIC', open the
 * library as a source file, using 'ld_name' as direct path.
 * WARNING: These flags are ignored when 'DCC_LIBDEF_FLAG_STATIC' is not
 *          set, as source files can only be linked statically (obviously...)
 * >> When used together with 'DCC_LIBDEF_FLAG_NOSEARCHSTD'
 *    and 'DCC_LIBDEF_FLAG_NOSEARCHEXT', this flag can be used
 *    to automatically load an input file, either as a static
 *    library, or as source-code.
 * NOTE: Multiple source-code flags may be or'd together
 *       if the intend is for DCC to automatically deduce
 *       the source type from the file extension. */
#define DCC_LIBDEF_FLAG_C_SOURCE   0x80000000
#define DCC_LIBDEF_FLAG_ASM_SOURCE 0x40000000

/* Enable all source types with fully automatic
 * detection of what kind of source is given. */
#define DCC_LIBDEF_FLAG_SOURCE     0xffff0000

struct DCCLibDef {
 uint32_t           ld_flags; /*< Loader flags (A set of 'DCC_LIBDEF_FLAG_*'). */
 char const        *ld_name;  /*< [0..ld_size] Library fallback (Used when the library doesn't specify an explicit name) */
 size_t             ld_size;  /*< Length of the library name (in characters; excluding the terminating \0 character) */
 /* The following two describe symbol flag modifiers.
  * When a library defines a symbol, a default set of flags is calculated and modified as follows:
  * >> symflag_t final_flags = get_default_flags(sym);
  * >> final_flags &= ld->ld_symfa;
  * >> final_flags |= ld->ld_symfo;
  * >> define_symbol(sym,final_flags);
  * Using this when loading a library, you can explicitly re-export symbols:
  * >> ld->ld_expsymfa = ~(DCC_SYMFLAG_NONE);
  * >> ld->ld_expsymfo =  (DCC_SYMFLAG_DLLEXPORT); // PE-only (Elf does this by default)
  * Or when statically linking, you could ensure that no symbols are re-exported:
  * >> #ifdef DCC_SYMFLAG_DLLEXPORT
  * >> ld->ld_expsymfa = ~(DCC_SYMFLAG_VISIBILITY|DCC_SYMFLAG_DLLEXPORT);
  * >> #else
  * >> ld->ld_expsymfa = ~(DCC_SYMFLAG_VISIBILITY);
  * >> #endif
  * >> ld->ld_expsymfo =  (DCC_SYMFLAG_PRIVATE);
  * WARNING: Make sure to set 'ld_expsymfa' to '-1' and 'ld_expsymfo' to '0' if 1-to-1 linkage is intended. */
 DCC(symflag_t)     ld_expsymfa; /*< And-operand when determining flags of exported symbols. */
 DCC(symflag_t)     ld_expsymfo; /*< Or-operand when determining flags of exported symbols. */
 /* The following two behave the same as 'ld_expsymf(a|o)', but for imported symbols.
  * WARNING: Make sure to set 'ld_impsymfa' to '-1' and 'ld_impsymfo' to '0' if 1-to-1 linkage is intended.
  * NOTE: These members are ignored for dynamic imports (aka. the 'DCC_LIBDEF_FLAG_STATIC' flag isn't set) */
 DCC(symflag_t)     ld_impsymfa; /*< And-operand when determining flags of imported symbols. */
 DCC(symflag_t)     ld_impsymfo; /*< Or-operand when determining flags of imported symbols. */
 struct DCCSection *ld_dynlib;   /* [?..1] Filled by a call to 'DCCUnit_Load()' upon successful loading of a dynamic library.
                                  *        When a library was successfully loaded statically, this field is set to NULL.
                                  * >> When set, this field points to the import section
                                  *    associated with the newly loaded dynamic library. */
};
#define DCCLibDef_EXPFLAGS(self,f) (((f)&(self)->ld_expsymfa)|(self)->ld_expsymfo)
#define DCCLibDef_IMPFLAGS(self,f) (((f)&(self)->ld_impsymfa)|(self)->ld_impsymfo)

#define DCCUNIT_IMPORTCALL  DCC_ATTRIBUTE_FASTCALL
#define DCCUNIT_EXPORTCALL  DCC_ATTRIBUTE_FASTCALL


/* Load a given library definition into the current compilation unit.
 * NOTE: [DCCUnit_Import] The library is search by its name ('def->ld_name|def->ld_size')
 *                        within all library paths specified within the current linker.
 *                        s.a.: 'DCCLinker_AddLibPath()'
 * @param: def:   The library definition (s.a.: documentation of 'DCCLibDef')
 * @param: fd:    The file stream to load data from.
 * @param: start: An offset within 'fd', as well as the current seek-position inside.
 *                This argument is added to any absolute file-pointer inside the
 *                library, meaning that using this it can be stated that the actual
 *                contents of a file only start at a specific offset.
 *                NOTE: This argument is ignored for position-independent formats
 *                      such as '*.def' files, source files, as well as any text-based
 *                      library definition for that matter.
 * @return: 0: - Data form the given stream 'fd' does not describe a binary.
 *               >> No lexer error was set & the current unit was not modified.
 *             - A critical error occurred while parsing the given stream 'fd'.
 *               >> A lexer error was set; the current unit may have been modified.
 * @return: 1: Successfully loaded a binary into the current unit (either statically, or dynamically) */
DCCFUN int DCCUNIT_IMPORTCALL DCCUnit_Import(struct DCCLibDef *__restrict def);
DCCFUN int DCCUNIT_IMPORTCALL DCCUnit_ImportStream(struct DCCLibDef *__restrict def,
                                                   char const *__restrict filename,
                                                   DCC(stream_t) fd, DCC(soff_t) start);


#define DCC_EXPFMT_ELF 0 /* Generate ELF object files. */
typedef uint32_t DCC(expfmt_t); /* Export format (One of 'DCC_EXPFMT_*'). */

#define DCC_EXPFLAG_NONE      0x00000000
#define DCC_EXPFLAG_ELF_NOEXT 0x00000001 /* Don't use DCC ELF-extensions, rather
                                          * creating a (potentially broken) object
                                          * file that can also be used by GCC. */

struct DCCExpDef {
 /* Compilation unit export definition.
  * NOTE: This function is used to create so-called object
  *       files that can later be statically linked against. */
 DCC(expfmt_t) ed_fmt;   /*< Object file export format. */
 uint32_t      ed_flags; /*< Export flags (set of 'DCC_EXPFLAG_*') */
};

/* Export the current compilation unit into a stream, or a file.
 * The exported file can later be re-imported using
 * 'DCCUnit_Import*', and should always be linked statically.
 * Upon failure, a lexer error is set.
 * WARNING: Do not attempt to clear any kind of unused 
 */
DCCFUN void DCCUNIT_EXPORTCALL
DCCUnit_Export(struct DCCExpDef *__restrict def,
               char const *__restrict filename);
DCCFUN void DCCUNIT_EXPORTCALL
DCCUnit_ExportStream(struct DCCExpDef *__restrict def,
                     DCC(stream_t) fd);

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
#define DCCUnit_DEFSYM(sym) DCCSym_Define(sym,DCCUnit_Current.u_curr,DCCUnit_TADDR(),0,1)

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
