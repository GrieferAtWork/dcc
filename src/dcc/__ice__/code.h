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
#ifndef GUARD_DCC_CODE_H
#define GUARD_DCC_CODE_H 1

#include "common.h"
#include "lexer.h"
#include "symbol.h"
#include "vstack.h"

#include <stdint.h>
#include <stddef.h>

DCC_DECL_BEGIN

#define DCC_REL_OPSET  0x00
#define DCC_REL_OPADD  0x01
#define DCC_REL_OPDISP 0x02
#define DCC_REL_SIZE8  0x00
#define DCC_REL_SIZE16 0x10
#define DCC_REL_SIZE32 0x20
#define DCC_REL_SIZE64 0x30

/* Relocation type (One of DCC_REL_OP*, or'd together with one of 'DCC_REL_SIZE*') */
typedef unsigned int DCC(rel_t);

struct DCCRel {
 DCC(rel_t)            r_type;   /*< Relocation type. */
 DCC(target_ptr_t)     r_offset; /*< Offset from code base in this relocation's section. */
 /*ref*/struct DCCSym *r_symbol; /*< [0..1] Symbol associated with this relocation.
                                  *  NOTE: This symbol _must_ be a 'DCC_SYMTYPE_LABEL' */
};

struct DCCTextBuf {
 uint8_t *tb_begin;   /*< [0..1][<= s_end][owned] Start pointer for assembly code. */
 uint8_t *tb_pos;     /*< [0..1][>= s_begin && <= at_end] Current code position. */
 uint8_t *tb_end;     /*< [0..1][>= s_begin] Allocated code end. */
};
#define DCCTextBuf_ADDR(self)            ((DCC(target_ptr_t))((self)->tb_pos-(self)->tb_begin))
#define DCCTextBuf_SETADDR(self,a) (void)((self)->tb_pos = (self)->tb_begin+(a))

struct DCCSection {
 /* TODO: Direct code mode: Still generate relocations, but
  *       execute them as soon as symbols become available.
  *    >> Will be required for direct execution mode. */
 struct DCCTextBuf s_text;    /*< Text buffer. */
 void             *s_base;    /*< [0..1][owned_if(!= s_text.tb_begin)]
                               *   Used for immediate execution: Base address that this section is relocated to.
                               *   WARNING: The alloc/free technique of this address is platform-dependent. */
#define DCC_SECTION_R 0x01    /*< The section is readable. */
#define DCC_SECTION_W 0x02    /*< The section is writable. */
#define DCC_SECTION_X 0x04    /*< The section is executable. */
#define DCC_SECTION_S 0x08    /*< The section is shared between multiple instances of a running binary (NOTE: May not be available for some targets). */
#define DCC_SECTION_M 0x10    /*< Symbols in this section can be merged. */
#define DCC_SECTION_U 0x20    /*< This section has no alignment requirements (a_align is equal to '1'). */
 uint32_t          s_type;    /*< Section type+flags (Set of 'DCC_SECTION_*'). */
 DCC(target_ptr_t) s_align;   /*< [!0] Minimum alignment requirements for this section. */
 size_t            s_rela;    /*< [>= s_relc] Allocated amount of relocations. */
 size_t            s_relc;    /*< [<= s_rela] Used amount of relocations. */
 struct DCCRel    *s_relv;    /*< [0..s_relc|alloc(s_rela)][owned] Vector of relocations within this section. */
 struct DCCSym     s_start;   /*< [const] Placeholder symbol for the start of the section (can be used for section offsets). */
 size_t            s_nmsz;    /*< [const] Size of the section's name (in bytes). */
 DCC(hash_t)       s_nmhs;    /*< [const] Hash of the section's name. */
 char              s_name[1]; /*< [const][s_nmsz] Name of this section. */
 //char            s_zero;    /*< [const] Force zero-termination. */
};
#define DCCSection_ADDR(self)       DCCTextBuf_ADDR(&(self)->s_text)
#define DCCSection_SETADDR(self,a)  DCCTextBuf_SETADDR(&(self)->s_text,a)
#define DCCSection_HASBASE(self)  ((self)->s_base || (self) == &DCCSection_Global)

//////////////////////////////////////////////////////////////////////////
// A global section that may be used to specify absolute symbols.
DCCDAT struct DCCSection DCCSection_Global;

//////////////////////////////////////////////////////////////////////////
// Execute relocations on the section.
// NOTE: No-op if the section has no allocated base address.
// WARNING: If any relocation depends on a section that had yet to
//          have its base set, this function may link incorrectly.
// @param: free_data: When non-zero, relocation data is freed afterwards.
DCCFUN void DCCSection_Reloc(struct DCCSection *__restrict self, int free_data);

//////////////////////////////////////////////////////////////////////////
// Resolve DISP (aka. section-local) relocation.
// Such relocations were previously used for forward-jumps,
// who's target addresses are now known and can be replaced.
// >> Calling this function before writing data to a file
//    will result in lower load times, but when executing
//    data directly after using 'DCCSection_Reloc' to reloc
//    generated data immediately, calling this function is
//    unnecessary and will unnecessarily reduve performance.
// @param: warn_unknown_targets: When non-ZERO(0), warn about DISP
//                               relocations with an undefined target.
// @return: *: The number of resolved DISP relocations.
DCCFUN size_t DCCSection_ResolveDisp(struct DCCSection *__restrict self,
                                     int warn_unknown_targets);

/* Resize an area of memory located at 'addr' and taking up
 * 'old_size' bytes, to have a new length of 'new_size'.
 * In the event that data already exists after addr+old_size,
 * thus preventing it from being expanded normally, an entirely
 * new block of data will be allocated at the end of the section,
 * and all of the data block's previous contents (including any
 * relocation pointing into it) will be copied into that new area.
 * NOTE: This function can also be used to free data (and relocations).
 * NOTE: New data is automatically initialized to ZERO(0)
 * @return: The new base address (usually equal to 'addr')
 */
DCCFUN target_ptr_t
DCCSection_ResizeData(struct DCCSection *__restrict self,
                      target_ptr_t addr, target_ptr_t old_size,
                      target_ptr_t new_size);

/* Mark the given address range as free inside the section,
 * thus allowing future calls that require allocating memory
 * anywhere inside to re-use that area.
 * @param: free_relocations: When non-ZERO, also free relocations. */
DCCFUN void
DCCSection_FreeData(struct DCCSection *__restrict self,
                    target_ptr_t addr, target_ptr_t size,
                    int free_relocations);

//////////////////////////////////////////////////////////////////////////
// Allocates a base address for the given section,
// and copies all generated into that buffer.
// NOTE: Dependent 
// @param: free_data: When non-zero, free the original buffer when not re-used.
//              NOTE: When set, 's_begin' is subtracted from 's_pos' and set to NULL.
// @return: 0: Failed to allocate a memory address.
DCCFUN int DCCSection_SetBase(struct DCCSection *__restrict self, int free_data);

//////////////////////////////////////////////////////////////////////////
// Add the given relocation to the section. Upon failure, a lexer error is set.
DCCFUN void DCCSection_PutReloc(struct DCCSection *__restrict self,
                                DCC(rel_t) type, struct DCCSym *sym);
DCCFUN void DCCSection_PutRelocAt(struct DCCSection *__restrict self,
                                  DCC(rel_t) type, struct DCCSym *sym,
                                  DCC(target_ptr_t) offset);

DCCFUN void DCCSection_Putb(struct DCCSection *__restrict self, uint8_t b);
DCCFUN void DCCSection_Write(struct DCCSection *__restrict self, void const *p, size_t s);
#define DCCSection_Skip(self,s) DCCSection_Write(self,NULL,s)

//////////////////////////////////////////////////////////////////////////
// Allocate and return buffer memory within the given section.
// @return: NULL: Not enough available memory (A lexer error has been set)
DCCFUN void *DCCSection_Alloc(struct DCCSection *__restrict self, size_t s);
DCCFUN void *DCCSection_AAlloc(struct DCCSection *__restrict self, size_t s, size_t a);

//////////////////////////////////////////////////////////////////////////
// Place 's' bytes of data, aligned by 'align' with the given section,
// returning an offset within that section to the newly placed data.
// NOTE: If the section is read-only, this function will search
//       the section for any prior mentioning of that same chunk
//       of data, meaning that this function will implicitly
//       perform same-string elimination.
// NOTE: Upon error, this function will return ZERO(0).
//       Note however, that ZERO(0) may also be returned upon success!
DCCFUN DCC(target_ptr_t) DCCSection_Putdata(struct DCCSection *__restrict self, void const *__restrict p, size_t s, size_t align);
DCCFUN DCC(target_ptr_t) DCCSection_Putbyte(struct DCCSection *__restrict self, int byte, size_t s, size_t align);
#define DCC_MIN_ALIGNMENT  1

//////////////////////////////////////////////////////////////////////////
// Similar to 'DCCSection_Putdata', but returns a symbol with a unique
// name describing the section location that the data was stored at.
DCCFUN struct DCCSym *DCCSection_PutdataSym(struct DCCSection *__restrict self, void const *__restrict p, size_t s, size_t align);
DCCFUN struct DCCSym *DCCSection_PutbyteSym(struct DCCSection *__restrict self, int byte, size_t s, size_t align);

#define DCC_SCOPE_NAMESPACE_LOCALS 0 /* Local variables, enum constants and typedef types. */
#define DCC_SCOPE_NAMESPACE_STRUCT 1 /* struct/union/enum namespace. */
#define DCC_SCOPE_NAMESPACE_LABELS 2 /* GCC extension label namespace. */
struct DCCScope {
 struct DCCScope  *s_prev;    /*< [0..1] Previous scope. */
 struct DCCSymtab  s_tabs[3]; /*< Symbol tables (Index is one of 'DCC_SCOPE_NAMESPACE_*'). */
#define DCC_SCOPEKIND_FUNC  0 /*< Function scope. */
#define DCC_SCOPEKIND_LOCAL 1 /*< Local scope. */
 uint16_t          s_kind;    /*< The kind of this scope (One of 'DCC_SCOPEKIND_*') */
 DCC(scopeid_t)    s_id;      /*< Scope id (aka. amount of other function scopes reachable through 's_prev->...') */
};
#define DCCScope_Quit(self) \
 (DCCSymtab_Quit((self)->s_tabs+0,0),\
  DCCSymtab_Quit((self)->s_tabs+1,0),\
  DCCSymtab_Quit((self)->s_tabs+2,0))


struct DCCStackAllocator {
#ifdef __INTELLISENSE__
 target_ptr_t      sa_maxoffset; /*< Greatest offset required within the current function. */
 target_ptr_t      sa_curoffset; /*< Offset of the last variable. */
#else
 DCC(target_ptr_t) sa_maxoffset; /*< Greatest offset required within the current function. */
 DCC(target_ptr_t) sa_curoffset; /*< Offset of the last variable. */
#endif
};

#define DCCStackAllocator_PushFunction() \
do{ DCC(target_ptr_t) const _old_maxoffset = dcc_current->t_stackalloc.sa_maxoffset;\
    DCC(target_ptr_t) const _old_curoffset = dcc_current->t_stackalloc.sa_curoffset;\
    dcc_current->t_stackalloc.sa_maxoffset = 0;\
    dcc_current->t_stackalloc.sa_curoffset = 0

#define DCCStackAllocator_PopFunction() \
    dcc_current->t_stackalloc.sa_maxoffset = _old_maxoffset;\
    dcc_current->t_stackalloc.sa_curoffset = _old_curoffset;\
}while(DCC_MACRO_FALSE)
#define DCCStackAllocator_PushScope() \
do{ DCC(target_ptr_t) const _old_curoffset = dcc_current->t_stackalloc.sa_curoffset
#define DCCStackAllocator_PopScope() \
    if (!(dcc_current->t_flags&DCC_TARGET_FLAG_NOSHARED))\
          dcc_current->t_stackalloc.sa_curoffset = _old_curoffset;\
}while(DCC_MACRO_FALSE)

//////////////////////////////////////////////////////////////////////////
// Allocate 's' bytes of stack memory, aligned by 'a'
DCCFUN DCC(target_ptr_t)
DCCStackAllocator_Alloc(DCC(target_ptr_t) s,
                        DCC(target_ptr_t) a);


struct DCCTarget {
#define DCC_TARGET_FLAG_NONE      0x00000000
#define DCC_TARGET_FLAG_CODE16    0x00000001 /*< Inside a '.code16' assembler block (WARNING: Not necessary respected in code generated outside the assembler). */
#define DCC_TARGET_FLAG_NOCGEN    0x00000002 /*< Don't generate code (Used for sizeof()-style expressions). */
#define DCC_TARGET_FLAG_DEAD      0x00000004 /*< When used in conjunction with 'DCC_TARGET_FLAG_NOCGEN', allow both flags to be removed in the event that a label appears.
                                              *  This can be used to make sure that a label is capable of re-enabling code inside a dead branch.
                                              *  In addition, 't_deadjmp' may be defined as a jump target to jump to before code following the label is compiled.
                                              *  >> Together with 'DCC_TARGET_FLAG_NOCGEN' and 't_deadjmp', this is used for dead-branch elimination. */
#define DCC_TARGET_FLAG_INASM     0x00000008 /*< Set while parsing assembler code (used for the '__ASSEMBLER__' builtin macro). */
#define DCC_TARGET_FLAG_SDEAD     0x00000010 /*< Set when a switch-statement is dead, meaning that case statements may not re-enable it. */
#define DCC_TARGET_FLAG_SINIT     0x00000020 /*< When set, allow vstack store operations for static initializers using absolute symbol addresses as target, to directly write into the target section. */
#define DCC_TARGET_FLAG_INLOOP    0x01000000 /*< Set while inside a loop (enabling certain warnings when '__builtin_alloca'-style constructs are used). */
#define DCC_TARGET_FLAG_NODFLAG   0x08000000 /*< Unless set, consider the direction bit in EFLAGS as volatile, meaning that it may be changed at any time.
                                              *  This in turn forced the code generator to emit std/cld instructions before every 'rep' command.
                                              *  >> When set, the compiler will assume that the direction flag is always cleared. */
#define DCC_TARGET_FLAG_NOCLEANUP 0x10000000 /*< Don't perform cleanup operations that would otherwise be unnecessary (e.g.: '__builtin_va_end()' clears the AP pointer). */
#define DCC_TARGET_FLAG_NOFUNNOP  0x20000000 /*< Don't place NOPs at the end of functions. */
#define DCC_TARGET_FLAG_NOSHARED  0x40000000 /*< Don't share stack memory between variables not visible to each other. */
#define DCC_TARGET_FLAG_NOUNREACH 0x80000000 /*< Unless set, generate traps for __builtin_unreachable(), as well as other unreachable code locations. */
 uint32_t                 t_flags;    /*< Current target flags. */
#define DCC_TARGET_LFLAG_SHARED       0x00000001 /*< Create shared libraries. */
#define DCC_TARGET_LFLAG_NOSTDLIB     0x00000002 /*< Don't include standard libraries. */
#define DCC_TARGET_LFLAG_NOUNDERSCORE 0x00000004 /*< Don't prepend leading underscores. */
 uint32_t                 t_lflags;   /*< Linker flags. */
 size_t                   t_secc;     /*< Allocated amount of sections. */
 struct DCCSection      **t_secv;     /*< [1..1][owned][0..b_secc][owned] Vector of sections. */
 struct DCCSection       *t_data;     /*< [0..1|in(*t_secv)] Pointer to the current .data section. */
 struct DCCSection       *t_text;     /*< [0..1|in(*t_secv)] Pointer to the current .text section. */
 struct DCCSection       *t_bss;      /*< [0..1|in(*t_secv)] Pointer to the current .bss section. */
 struct DCCSection       *t_curr;     /*< [0..1] The currently select section for writing code. */
 struct DCCSection       *t_prev;     /*< [0..1] The section selected before the current one (or NULL if none was). */
 struct DCCTextBuf        t_currbuf;  /*< Inlined copy of 't_curr->s_text'. */
 /* Symbol tables. */
 struct DCCSymtab         t_asmlbl;   /*< Table for local assembly labels (e.g.: '1: jmp 1b'). */
 struct DCCSymtab         t_labels;   /*< Table for per-function c labels. */
 struct DCCSymtab         t_intern;   /*< Table for internal symbols used for unique data locations. */
 struct DCCScope         *t_global;   /*< [1..1][== &t_scope.[t_prev->...]] Global scope. */
#define t_public          t_global->s_tabs[DCC_SCOPE_NAMESPACE_LOCALS] /*< List of global symbols considered for import/export (but also static globals). */
 /* Function/scope stack. */
 struct DCCScope          t_scope;    /*< Current scope. */
 struct DCCStackAllocator t_stackalloc; /* Stack allocator. */
 /* Virtual stack / local storage manager / unnamed symbol list. */
 struct DCCVStack         t_vstack;   /*< Virtual stack of operands, used as basis for code generation. */
 struct DCCSymlst         t_symlst;   /*< List of random, unnamed symbols (Used for e.g.: jmp labels in if-statements) */
 struct DCCSym           *t_function; /*< [0..1] Symbol for the function currently being compiled. */
 struct DCCSym           *t_return;   /*< [0..1] Symbol to jump to for frame cleanup before returning. */
 /* Special symbols/expressions used during parsing. */
 struct DCCSym           *t_bsym;     /*< [0..1] Label symbol used as target during 'break' statements. */
 struct DCCSym           *t_csym;     /*< [0..1] Label symbol used as target during 'continue' statements. */
 struct DCCSym           *t_default;  /*< [0..1] Label symbol used as target when defining switch-default cases. */
 struct DCCSym           *t_deadjmp;  /*< [0..1] Label symbol used as target when a dead branch is re-enabled through use of a label. */
 struct DCCSym           *t_casejmp;  /*< [0..1] Label symbol for jumping to the next case of a switch-statement. */
 struct DCCStackValue     t_sexpr;    /*< Switch expression. */
};


/* Current compiler target. */
#ifdef __INTELLISENSE__
extern struct DCCTarget    *const dcc_current;
extern struct DCCTarget           dcc_target;
extern struct DCCSection   *      dcc_text;
extern struct DCCSection   *      dcc_data;
extern struct DCCSection   *      dcc_bss;
extern struct DCCSection   *const dcc_curr;
#else
DCCDAT struct DCCTarget dcc_target;
#define dcc_current   (&dcc_target)
#define dcc_text        dcc_target.t_text
#define dcc_data        dcc_target.t_data
#define dcc_bss         dcc_target.t_bss
#define dcc_curr        dcc_target.t_curr
#endif


/* Define the target of a given symbol 'sym', that must
 * be a label as pointing to the current test position. */
#define DCCTarget_DEFLABEL(sym) \
 (void)(assert((sym)->s_kind == DCC_SYMTYPE_LABEL),\
       (sym)->s_label.sl_addr = DCCTarget_TEXTADDR(),\
       (sym)->s_label.sl_sec  = dcc_curr)

#define DCCTarget_Pushf() \
do{ uint32_t const _old_flags = dcc_current->t_flags
#define DCCTarget_Popf() \
    dcc_current->t_flags = _old_flags;\
}while(DCC_MACRO_FALSE)

#define DCCTarget_PushBCLabels() \
do{ struct DCCSym *const _old_bsym = dcc_current->t_bsym;\
    struct DCCSym *const _old_csym = dcc_current->t_csym
#define DCCTarget_PopBCLabels() \
    dcc_current->t_csym = _old_csym;\
    dcc_current->t_bsym = _old_bsym;\
}while(DCC_MACRO_FALSE)

#define DCCTarget_PushScope(kind) \
do{ struct DCCScope _old_scope; \
    memcpy(&_old_scope,&dcc_current->t_scope,sizeof(struct DCCScope)); \
    memset(&dcc_current->t_scope,0,sizeof(struct DCCScope)); \
    dcc_current->t_scope.s_prev = &_old_scope; \
    if (dcc_current->t_global == &dcc_current->t_scope) \
        dcc_current->t_global = &_old_scope;\
    dcc_current->t_scope.s_id = _old_scope.s_id+(kind == DCC_SCOPEKIND_FUNC);\
    dcc_current->t_scope.s_kind = (kind)
#define DCCTarget_PopScope() \
    assert(dcc_current->t_scope.s_prev == &_old_scope);\
    DCCScope_Quit(&dcc_current->t_scope);\
    memcpy(&dcc_current->t_scope,&_old_scope,sizeof(struct DCCScope));\
    if (dcc_current->t_global == &_old_scope) \
        dcc_current->t_global = &dcc_current->t_scope;\
}while(DCC_MACRO_FALSE)


#define DCCTarget_Init(self) \
 (void)(memset(self,0,sizeof(struct DCCTarget)),\
       (self)->t_global = &(self)->t_scope)

DCCFUN void DCCTarget_Quit(struct DCCTarget *__restrict self);

//////////////////////////////////////////////////////////////////////////
// Set the current text section, updating any prior changes.
// WARNING: The given section must be contained within the t_secv vector.
// HINT: Pass NULL for 'section' to set flush the current text section without setting a new one.
DCCFUN void DCCTarget_SetCurrSection(struct DCCSection *section);
DCCFUN void DCCTarget_LoadTextSection(void);
DCCFUN void DCCTarget_FlushTextSection(void);

//////////////////////////////////////////////////////////////////////////
// Get/get+create a given section my name.
// @return: NULL: [DCCTarget_GetSection] No such section exists.
// @return: NULL: [DCCTarget_NewSection] Not enough available memory. (A TPP lexer error was set)
// @return: * :    A pointer to the requested section.
DCCFUN struct DCCSection *DCCTarget_GetSection(char const *__restrict name, size_t name_size);
DCCFUN struct DCCSection *DCCTarget_NewSection(char const *__restrict name, size_t name_size, uint32_t type);


//////////////////////////////////////////////////////////////////////////
// Lookup/create new symbols visible through the scope chain:
//  - DCCTarget_ScopeGet: Search the entire scope stack (return NULL if not found)
//  - DCCTarget_ScopeNew: Search the entire scope stack (create & return new entry in top)
//  - DCCTarget_ScopePut: Only Search the top scope (create & return new entry in top)
// @param: ns: One of 'DCC_SCOPE_NAMESPACE_*'
DCCFUN struct DCCSym *DCCTarget_ScopeGet(struct TPPKeyword const *__restrict name, int ns);
DCCFUN struct DCCSym *DCCTarget_ScopeNew(struct TPPKeyword const *__restrict name, int ns);
#define DCCTarget_ScopePut(name,ns) \
 DCCSymtab_New(&dcc_current->t_scope.s_tabs[ns],name)

/* Lookup compiler-internal symbols.
 * >> used during large-integer arithmetic, or by builtin function proxies. */
DCCFUN struct DCCSym *DCCTarget_LookupInternalSym(char const *name);
DCCFUN struct DCCSym *DCCTarget_LookupInternalSymf(char const *fmt, ...);


#define DCCTarget_TEXTADDR() DCCTextBuf_ADDR(&dcc_current->t_currbuf)

//////////////////////////////////////////////////////////////////////////
// Write data to the current text section,
// dynamically increasing its size when necessary.
// NOTE: Upon failure, a lexer error will be set.
// WARNING: Causes undefined behavior if no text section has been set.
DCCFUN void DCCTarget_TextPutb(uint8_t b);
DCCFUN void DCCTarget_TextWrite(void const *__restrict p, size_t s);
DCCFUN void DCCTarget_TextFill(int byte, size_t s);
DCCFUN void DCCTarget_TextAlign(int byte, size_t a);
#define DCCTarget_AllocData(p,s,align) DCCSection_PutdataSym(dcc_current->t_data,p,s,align)
#define DCCTarget_AllocByte(b,s,align) DCCSection_PutbyteSym(dcc_current->t_data,b,s,align)

//////////////////////////////////////////////////////////////////////////
// Allocate and return a new, unnamed symbol.
#define DCCTarget_AllocSym() DCCSymlst_Alloc(&dcc_current->t_symlst)
DCC_LOCAL struct DCCSym *DCCTarget_AllocLbl(void) {
 struct DCCSym *result = DCCTarget_AllocSym();
 if (result) {
  result->s_kind = DCC_SYMTYPE_LABEL;
  /* Labels are of type 'void'.
   * Remembering that labels _always_ describe the indirect memory at a given
   * address, and not the address itself (which is why pushing a label is usually
   * followed by a vgen1('&') in order to dereference it), it only makes sense
   * for labels to have no size either (essentially requiring the user
   * to cast a label before it would be used).
   */
  DCC_ASSERT(!result->s_type.t_base);
  result->s_type.t_type = DCCTYPE_VOID;
 }
 return result;
}



//////////////////////////////////////////////////////////////////////////
// Allocate and return buffer memory at the current text position.
// @return: NULL: Not enough available memory (A lexer error has been set)
//                WARNING: When the 'DCC_TARGET_FLAG_NOCGEN' flag is set,
//                         this function will return NULL too, but won't
//                         be setting a lexer error.
//                      >> In the end, simply check if it returned non-NULL,
//                         and only if it did, fill that pointer, but continue
//                         working in both cases.
DCCFUN void *DCCTarget_TextAlloc(size_t s);

//////////////////////////////////////////////////////////////////////////
// Write a given expression (which may either be an integral, or a symbol)
// to the current text section, generating relocations if necessary.
DCCFUN void DCCTarget_TextExpr8(struct DCCSymExpr const *__restrict expr);
DCCFUN void DCCTarget_TextExpr16(struct DCCSymExpr const *__restrict expr);
DCCFUN void DCCTarget_TextExpr32(struct DCCSymExpr const *__restrict expr);
#if DCC_TARGET_SIZEOF_POINTER >= 8
DCCFUN void DCCTarget_TextExpr64(struct DCCSymExpr const *__restrict expr);
#endif

DCCFUN void DCCTarget_TextDisp8(struct DCCSymExpr const *__restrict expr);
DCCFUN void DCCTarget_TextDisp16(struct DCCSymExpr const *__restrict expr);
DCCFUN void DCCTarget_TextDisp32(struct DCCSymExpr const *__restrict expr);
#if DCC_TARGET_SIZEOF_POINTER >= 8
DCCFUN void DCCTarget_TextDisp64(struct DCCSymExpr const *__restrict expr);
#endif



DCC_LOCAL void DCCTarget_TextPutw(uint16_t w) { DCCTarget_TextWrite(&w,2); }
DCC_LOCAL void DCCTarget_TextPutl(uint32_t l) { DCCTarget_TextWrite(&l,4); }
#if DCC_TARGET_SIZEOF_POINTER >= 8
DCC_LOCAL void DCCTarget_TextPutq(uint64_t q) { DCCTarget_TextWrite(&q,8); }
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_CODE_H */
