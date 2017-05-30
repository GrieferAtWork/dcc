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
#ifndef GUARD_DCC_COMPILER_H
#define GUARD_DCC_COMPILER_H 1

#include "common.h"
#include "target.h"
#include "lexer.h"
#include "type.h"
#include "vstack.h"
#include "unit.h"

#include <stddef.h>
#include <stdint.h>
#include <elf.h>

DCC_DECL_BEGIN

#define DCC_DECLKIND_NONE         0x0000 /*< Unused declaration. */
#define DCC_DECLKIND_MLOC         0x0001 /*< An addressable location in memory. */
#define DCC_DECLKIND_MREF         0x0003 /*< Same as 'DCC_DECLKIND_MLOC', but the memory location shall not be dereferenced (HINT: May also be abused for constants). */

#define DCC_DECLKIND_TYPE         0x0008 /*< A C type (Used as flag). */
#define DCC_DECLKIND_DECL        (DCC_DECLKIND_TYPE|0x0000) /*< A regular type declaration (aka. 'typedef int INT;'). */
#define DCC_DECLKIND_ENUM        (DCC_DECLKIND_TYPE|0x0010) /*< An enum type (aka. 'enum foo { ... };') ('d_type' is an alias for 'int'). */
#define DCC_DECLKIND_STRUCT      (DCC_DECLKIND_TYPE|0x0020) /*< A struct type (aka. 'struct bar { ... };') ('d_type' is a self-reference). */
#define DCC_DECLKIND_UNION       (DCC_DECLKIND_TYPE|0x0030) /*< A union type (aka. 'union bar { ... };') ('d_type' is a self-reference). */
#define DCC_DECLKIND_FUNCTION    (DCC_DECLKIND_TYPE|0x0040) /*< A function prototype ('d_type' declares the base). */
#define DCC_DECLKIND_OLDFUNCTION (DCC_DECLKIND_TYPE|0x0050) /*< An old-style function prototype ('d_type' declares the base). */
#define DCC_DECLKIND_ARRAY       (DCC_DECLKIND_TYPE|0x0060) /*< A fixed-length array (The element count is stored in 'd_tdecl.td_size'). */
#define DCC_DECLKIND_VLA         (DCC_DECLKIND_TYPE|0x0070) /*< A variable-length array (Its absolute, multiplied size is stored as a target-size_t at 'EBP+d_tdecl.td_vlaoff'). */

#define DCC_DECLFLAG_NONE         0x0000
#define DCC_DECLFLAG_VARIADIC     0x0001 /*< Used by 'DCC_DECLKIND_FUNCTION': Additional arguments are variadic. */
#define DCC_DECLFLAG_FORWARD      0x0002 /*< Used by forward-type declarations (such as structures, unions or enums). */
#define DCC_DECLFLAG_VARIADICLAST 0x0004 /*< Set in conjunction with 'DCC_DECLFLAG_VARIADIC': The last argument is the start of the variadic parameter list. */
#define DCC_DECLFLAG_INTERN       0x1000 /*< Internal declaration symbol (which may not be cleared). */
struct DCCStructField {
 DCC(target_off_t)      sf_off;    /*< Field offset, or offset from %EBP for function types. */
 /*ref*/struct DCCDecl *sf_decl;   /*< [1..1] Field type+name, or function argument.
                                    *   NOTE: When the name is 'TPPKeyword_Empty', a struct/union
                                    *         should be inlined, or a member should be unnamed. */
 DCC(sflag_t)           sf_bitfld; /*< Stack-value flags or'd to instances of this member (Used for bit-fields).
                                    *  NOTE: During structure parsing, this field is used to hold the raw bit-field size, or -1 when none is given. */
};
struct DCCTypeDef { /* DCC_DECLKIND_TYPE */
union{
#ifdef DCC_PRIVATE_API
 target_siz_t           td_size;         /*< Array types use this as element count,
                                          *  or the amount of fields in a struct type,
                                          *  or amount of arguments in a function type. */
 target_off_t           td_vlaoff;       /*< Local variable offset for a target-size_t for the absolute size of the array. */
#else
 DCC(target_siz_t)      td_size;         /*< Array types use this as element count,
                                          *  or the amount of fields in a struct type,
                                          *  or amount of arguments in a function type. */
 DCC(target_off_t)      td_vlaoff;       /*< Local variable offset for a target-size_t for the absolute size of the array. */
#endif
};
 struct DCCStructField *td_fieldv;       /*< [0..td_size|0..0][owned] Vector of structure fields/function arguments. */
union{
 size_t                 td_struct_size;  /*< Used for struct/union types: Total size of the structure. */
#ifdef DCC_PRIVATE_API
 scopeid_t              td_vlascope;     /*< Scope id in which 'td_vlaoff' resides. */
#else
 DCC(scopeid_t)         td_vlascope;     /*< Scope id in which 'td_vlaoff' resides. */
#endif
};
 size_t                 td_struct_align; /*< Used for struct/union types: Minimum alignment of the structure. */
};

struct DCCMemDecl {
 struct DCCMemLoc md_loc;   /*< Label/variable declaration (NOTE: The symbol in this is a reference). */
#ifdef DCC_PRIVATE_API
 scopeid_t        md_scope; /*< Scope ID used for offset-based memory locations. */
#else
 DCC(scopeid_t)   md_scope; /*< Scope ID used for offset-based memory locations. */
#endif
};

struct DCCDecl {
 /* type/global/local variable declaration. */
 unsigned int             d_refcnt; /*< Reference counter. */
 struct DCCDecl          *d_next;   /*< [0..1] Next declaration with the same modulated 'd_name'. */
 struct TPPKeyword const *d_name;   /*< [1..1] Definition name. */
 /*ref*/struct TPPFile   *d_file;   /*< [1..1] Reference to the file this declaration was declared inside of. */
#ifdef DCC_PRIVATE_API
 line_t                   d_line;   /*< Line associated with 'd_file'. */
#else
 DCC(line_t)              d_line;   /*< Line associated with 'd_file'. */
#endif
 uint16_t                 d_kind;   /*< Definition type (One of 'DCC_DECLKIND_*'). */
 uint16_t                 d_flag;   /*< Definition flags (Set of 'DCC_SYMFLAG_*'). */
#ifdef DCC_PRIVATE_API
 scopedepth_t             d_depth;  /*< Scope depth used for tracking how deep declarations should be cleared when the scope ends. */
#else
 DCC(scopedepth_t)        d_depth;  /*< Scope depth used for tracking how deep declarations should be cleared when the scope ends. */
#endif
 struct DCCType           d_type;   /*< The C-style type of this declaration (Default to 'int', as created by zero-initializing this field). */
 struct DCCAttrDecl      *d_attr;   /*< [0..1] Optional __attribute__((...)) properties. */
 union{
  struct DCCMemDecl       d_mdecl;  /*< [DCC_DECLKIND_MLOC] C-style global/local variable declaration. */
  struct DCCTypeDef       d_tdecl;  /*< [&DCC_DECLKIND_TYPE] C-style type declaration. */
 };
};

#define DCCDecl_Incref(self)  (void)(++(self)->d_refcnt)
#define DCCDecl_Decref(self)  (void)(DCC_ASSERT((self)->d_refcnt),--(self)->d_refcnt || (_DCCDecl_Delete(self),0))
#define DCCDecl_XIncref(self) ((self) ? DCCDecl_Incref(self) : (void)0)
#define DCCDecl_XDecref(self) ((self) ? DCCDecl_Decref(self) : (void)0)
DCCFUN void _DCCDecl_Delete(struct DCCDecl *__restrict self);

/* Returns TRUE(1) if the given declaration is empty (aka. may still be declared) */
#define DCCDecl_ISEMPTY(self) ((self)->d_kind == DCC_DECLKIND_NONE)

/* Allocate a new, empty C-declaration 'name' and return a reference to it.
 * NOTE: file and line will be initialized to the current effective TPP file/line positions.
 *       'd_depth' is filled according to the depth of the current scope. */
DCCFUN /*ref*/struct DCCDecl *
DCCDecl_New(struct TPPKeyword const *__restrict name);

/* Recursively clears a given declaration, resolving any recursive reference pointers.
 * >> This function is called for every entry when the associated declaration tab is destroyed. */
DCCFUN void DCCDecl_Clear(struct DCCDecl *__restrict self, DCC(scopedepth_t) min_depth);

/* Returns non-ZERO(0) if the given declarations 'a' and 'b' describe the same thing.
 * @param: same_type: When ZERO(0), only require identical storage
 *                    location for offset/label declarations, not
 *                    failing because of incompatible types. */
DCCFUN int DCCDecl_Equal(struct DCCDecl const *__restrict a,
                         struct DCCDecl const *__restrict b,
                         int same_type);

/* Given any random declaration, figure out what
 * symbol flags should be associated with it, with
 * respect to attribute and type storage. */
DCCFUN DCC(symflag_t)
DCCDecl_CalculateSymflags(struct DCCDecl const *__restrict self);

/* Check if a given 'p' is located inside 'v...+=s'
 * NOTE: The caller is responsible to ensure that both
 *       declarations describe a memory location.
 * @return: 0: 'vector' and 'pointer' _most_ _definitely_ don't overlap.
 * @return: 1: 'vector' and 'pointer' _most_ _definitely_ do overlap.
 * @return: 2: The correlation between 'vector' and 'pointer' cannot be determined at compile-time. */
DCCFUN int DCCDecl_Contains(struct DCCDecl const *__restrict v, DCC(target_siz_t) s,
                            struct DCCDecl const *__restrict p);

/* Set the given attributes in 'self'.
 * NOTE: If 'attr' is empty, don't do anything.
 * NOTE: If 'self' already has attributes set, merge them. */
DCCFUN void DCCDecl_SetAttr(struct DCCDecl *__restrict self,
                            struct DCCAttrDecl const *__restrict attr);

/* Allocate static/local storage, as requested by the c-type of 'self'
 * NOTE: This function automatically understands that automatic declarations
 *       in the global scope are always public/extern, unless an initializer
 *       is given, or static was specified. It also understands that 
 *       automatic declarations in a local scope use local storage unless
 * NOTE: Based on const qualifiers, global/static storage
 *       may either be allocated in .bss, or in .data.
 * WARNING: Upon entry, the given symbol must be of 'DCC_DECLKIND_NONE' typing,
 *          while upon exit, it will be one of 'DCC_SYMTYPE_LABEL, DCC_SYMTYPE_OFFSET'
 * @param: has_init: Set to non-zero if an initializer is given, which
 *                   forces variables in the global scope to be declared
 *                   as public, unless static was specified.
 * @param: asmname:  When non-NULL, the assembly name used for symbol declarations.
 *                   This argument is ignored for non-global declarations and
 *                   when NULL is passed, 'DCCDecl_GenAsmname(self)' is used instead.
 */
DCCFUN void DCCDecl_AllocStorage(struct DCCDecl *__restrict self, int has_init,
                                 struct TPPKeyword const *asmname);


/* Generate the default assembly name for a given declaration. */
DCCFUN struct TPPKeyword const *
DCCDecl_GenAsmname(struct DCCDecl *__restrict self);



struct DCCDecltab {
 /* Named declaration table. */
 size_t                  dt_declc; /*< Amount of declarations in 'dt_declv'. */
 size_t                  dt_decla; /*< Allocated bucket count in 'dt_declv'. */
 /*ref*/struct DCCDecl **dt_declv; /*< [0..1][chain(d_next->...)][0..dt_decla][owned]
                                    *   Hash-map of c-declarations. */
};

#define DCCDecltab_Init(self) memset(self,0,sizeof(struct DCCDecltab))
DCCFUN void DCCDecltab_Quit(struct DCCDecltab *__restrict self, DCC(scopedepth_t) depth);

/* Lookup/Create a new C-declaration 'name'.
 * NOTE: None of these functions return a reference to the symbol!
 * NOTE: [DCCCompiler_New*] If a declaration with the same name already exists, it will be returned instead!
 * @return: NULL: [DCCCompiler_Get*] No known declaration with the given name.
 *                [DCCCompiler_New*] Failed to allocate the new declaration (A lexer error was set) */
DCCFUN struct DCCDecl *
DCCDecltab_GetDecl(struct DCCDecltab *__restrict self,
                   struct TPPKeyword const *__restrict name);
DCCFUN struct DCCDecl *
DCCDecltab_NewDecl(struct DCCDecltab *__restrict self,
                   struct TPPKeyword const *__restrict name);



#define DCC_NS_LOCALS 0 /*< Local/global variable/type declarations. */
#define DCC_NS_STRUCT 1 /*< struct/union/enum namespace (the ones that require the prefix). */
#define DCC_NS_LABELS 2 /*< C-label namespace (only used by 'goto' and '&&'). */
typedef int DCC(ns_t);  /*< C namespace kind (One of 'DCC_NS_*') */
#define DCC_NS_COUNT  3 /*< Amount of different namespaces. */

#define DCC_SCOPEKIND_FUNC  0 /*< Function scope (Must be ZERO(0)). */
#define DCC_SCOPEKIND_LOCAL 1 /*< Local scope. */
struct DCCScope {
 struct DCCScope  *s_prev;               /*< [0..1] Previous scope. */
 struct DCCDecltab s_tabs[DCC_NS_COUNT]; /*< Declaration tables (Index is one of 'DCC_NS_*'). */
 uint16_t          s_kind;               /*< The kind of this scope (One of 'DCC_SCOPEKIND_*') */
#ifdef DCC_PRIVATE_API
 scopedepth_t      s_depth;              /*< Scope depth (Always unique for every existing scope; amount of ~actual~ scopes surrounding this one) */
 scopeid_t         s_id;                 /*< Scope id (aka. amount of other function scopes reachable through 's_prev->...') */
#else
 DCC(scopedepth_t) s_depth;              /*< Scope depth (Always unique for every existing scope; amount of ~actual~ scopes surrounding this one) */
 DCC(scopeid_t)    s_id;                 /*< Scope id (aka. amount of other function scopes reachable through 's_prev->...') */
#endif
};
#define DCCScope_Quit(self) \
 (DCCDecltab_Quit((self)->s_tabs+0,(self)->s_depth),\
  DCCDecltab_Quit((self)->s_tabs+1,(self)->s_depth),\
  DCCDecltab_Quit((self)->s_tabs+2,(self)->s_depth))


struct DCCHWStack {
 /* Hardware stack allocator (aka. stackframes manager). */
#ifdef DCC_PRIVATE_API
 target_ptr_t       hws_minalign;  /*< Greatest stack alignment ever required by code. */
 target_ptr_t       hws_maxoffset; /*< Greatest offset required within the current function. */
 target_ptr_t       hws_curoffset; /*< Offset of the last variable. */
#else
 DCC(target_ptr_t)  hws_minalign;  /*< Greatest stack alignment ever required by code. */
 DCC(target_ptr_t)  hws_maxoffset; /*< Greatest offset required within the current function. */
 DCC(target_ptr_t)  hws_curoffset; /*< Offset of the last variable. */
#endif
 struct DCCFreeData hws_free;
};

/* push/pop the hardware stack around a scope, allowing for
 * parallel allocation between scopes invisible to each other. */
#define DCCHWStack_Push(self) \
do{ struct DCCFreeData _old_free;\
    DCC(target_ptr_t) _old_curoffset = (self)->hws_curoffset;\
    int _old_free_has = DCCFreeData_InitCopy(&_old_free,&(self)->hws_free)
#define DCCHWStack_Pop(self) \
    DCCFreeData_Quit(&(self)->hws_free);\
    (self)->hws_curoffset = _old_curoffset;\
    if likely(_old_free_has) (self)->hws_free = _old_free;\
    else                     (self)->hws_free.fd_begin = NULL;\
}while(DCC_MACRO_FALSE)

#define DCCHWStack_PushNew(self) \
do{ struct DCCHWStack const _old_hw_stack = *(self);\
    memset(self,0,sizeof(struct DCCHWStack))
#define DCCHWStack_PopNew(self) \
    DCCFreeData_Quit(&(self)->hws_free);\
    *(self) = _old_hw_stack;\
}while(DCC_MACRO_FALSE)


struct DCCPackStack {
 /* Structure packing stack for '#pragma pack(push|pop)' */
 size_t        ps_stackc; /*< Amount of pushed pack-values currently in use
                           *  NOTE: The next pop-value is located in 'ps_stackv[ps_stackc-1]'. */
 size_t        ps_stacka; /*< Allocated amount of old pack-values. */
 target_siz_t *ps_stackv; /*< [0..ps_stackc|alloc(ps_stacka)][owned] Vector of old pack-values. */
 target_siz_t  ps_pack;   /*< Current packing value. NOTE: When ZERO(0), ignore this modifier. */
};

struct DCCVisibilityStack {
 /* Default visibility stack for '#pragma GCC visibility push|pop' */
 size_t          vs_stackc; /*< Amount of pushed visibility-values currently in use
                             *  NOTE: The next pop-value is located in 'vs_stackv[vs_stackc-1]'. */
 size_t          vs_stacka; /*< Allocated amount of old visibility-values. */
 DCC(symflag_t) *vs_stackv; /*< [0..vs_stackc|alloc(vs_stacka)][owned] Vector of old visibility-values. */
 DCC(symflag_t)  vs_viscur; /*< Current default visibility. */
};


struct DCCCompiler {
 /* C-language-specific per-unit members. */
 struct DCCScope           c_scope;      /*< Current scope. */
 struct DCCHWStack         c_hwstack;    /*< Stack allocator. */
 struct DCCVStack          c_vstack;     /*< Virtual operand stack. */
 struct DCCSym            *c_return;     /*< [0..1] Symbol to jump to for frame cleanup before returning. */
 struct DCCDecl           *c_fun;        /*< [0..1] When non-NULL: the declaration of the current function. */
 struct DCCSym            *c_bsym;       /*< [0..1] Symbol used as target during 'break' statements. */
 struct DCCSym            *c_csym;       /*< [0..1] Symbol used as target during 'continue' statements. */
 struct DCCSym            *c_defsym;     /*< [0..1] Symbol used as target when defining switch-default cases. */
 struct DCCSym            *c_deadjmp;    /*< [0..1] Symbol used as target when a dead branch is re-enabled through use of a label. */
 struct DCCSym            *c_casejmp;    /*< [0..1] Symbol for jumping to the next case of a switch-statement. */
 struct DCCStackValue      c_sexpr;      /*< Switch expression (owned by the switch statement). */
 struct DCCPackStack       c_pack;       /*< Structure packing information. */
 struct DCCVisibilityStack c_visibility; /*< Default symbol visibility information. */
#define DCC_COMPILER_FLAG_NONE      0x00000000
#if DCC_TARGET_IA32(386)
#define DCC_COMPILER_FLAG_CODE16    0x00000001 /*< Inside a '.code16' assembler block (WARNING: Not necessary respected in code generated outside the assembler). */
#endif
#define DCC_COMPILER_FLAG_NOCGEN    0x00000002 /*< Don't generate code (Used for sizeof()-style expressions). */
#define DCC_COMPILER_FLAG_DEAD      0x00000004 /*< When used in conjunction with 'DCC_COMPILER_FLAG_NOCGEN', allow both flags to be removed in the event that a label appears.
                                                *  This can be used to make sure that a label is capable of re-enabling code inside a dead branch.
                                                *  In addition, 'c_deadjmp' may be defined as a jump target to jump to before code following the label is compiled.
                                                *  >> Together with 'DCC_COMPILER_FLAG_NOCGEN' and 'c_deadjmp', this is used for dead-branch elimination. */
#define DCC_COMPILER_FLAG_INASM     0x00000008 /*< Set while parsing assembler code (used for the '__ASSEMBLER__' builtin macro). */
#define DCC_COMPILER_FLAG_SDEAD     0x00000010 /*< Set when a switch-statement is dead, meaning that case statements may not re-enable it. */
#define DCC_COMPILER_FLAG_SINIT     0x00000020 /*< When set, allow vstack store operations for static initializers using absolute symbol addresses as target, to directly write into the target section. */
#if DCC_DEBUG
#define DCC_COMPILER_FLAG_TEXTFLUSH 0x00100000 /*< Used to track the flush-state of the current section buffer. */
#endif
#define DCC_COMPILER_FLAG_INLOOP    0x01000000 /*< Set while inside a loop (enabling certain warnings when '__builtin_alloca'-style constructs are used). */
#define DCC_COMPILER_FLAG_NODFLAG   0x08000000 /*< Unless set, consider the direction bit in EFLAGS as volatile, meaning that it may be changed at any time.
                                                *  This in turn forced the code generator to emit std/cld instructions before every 'rep' command.
                                                *  >> When set, the compiler will assume that the direction flag is always cleared. */
#define DCC_COMPILER_FLAG_NOCLEANUP 0x10000000 /*< Don't perform cleanup operations that would otherwise be unnecessary (e.g.: '__builtin_va_end()' clears the AP pointer). */
#define DCC_COMPILER_FLAG_NOFUNNOP  0x20000000 /*< Don't place NOPs at the end of functions. */
#define DCC_COMPILER_FLAG_NOSHARED  0x40000000 /*< Don't share stack memory between variables not visible to each other. */
#define DCC_COMPILER_FLAG_NOUNREACH 0x80000000 /*< Unless set, generate traps for __builtin_unreachable(), as well as other unreachable code locations. */
 uint32_t                  c_flags;   /*< Current c-related code flags (Set of 'DCC_COMPILER_FLAG_*'). */
};

/* Global object: The current compiler. */
DCCDAT struct DCCCompiler DCCCompiler_Current;

DCCFUN void DCCCompiler_Init(struct DCCCompiler *__restrict self);
DCCFUN void DCCCompiler_Quit(struct DCCCompiler *__restrict self);

/* Clear any preallocated memory reachable from 'self'
 * NOTE: Some flush operations are always performed:
 *        - Clear unused relocation.
 * @param: flags: A set of 'DCCCOMPILER_FLUSHFLAG_*' (Unknown flags are ignored) */
DCCFUN void DCCCompiler_Flush(struct DCCCompiler *__restrict self, uint32_t flags);
#define DCCCOMPILER_FLUSHFLAG_NONE      0x00000000
#define DCCCOMPILER_FLUSHFLAG_DECLTAB   0x00000008 /*< Shrink the allocated size of declaration hash tables to the minimum assumed by automatic rehashing. */
#define DCCCOMPILER_FLUSHFLAG_TABMIN    0x00000010 /*< More aggressive behavior for 'DCCUNIT_FLUSHFLAG_DECLTAB': Reduce the hash size to ONE(1) (Only use this as last way out) */
#define DCCCOMPILER_FLUSHFLAG_VSTACK    0x00000020 /*< Free unused v-stack entries. */
#define DCCCOMPILER_FLUSHFLAG_PACKSTACK 0x00000040 /*< Free unused pack-stack entries. */
#define DCCCOMPILER_FLUSHFLAG_VISISTACK 0x00000080 /*< Free unused visibility-stack entries. */

#define DCCCompiler_Pushf() \
do{ uint32_t const _old_flags = DCCCompiler_Current.c_flags
#define DCCCompiler_Popf() \
    DCCCompiler_Current.c_flags = _old_flags;\
}while(DCC_MACRO_FALSE)

#define DCCCompiler_PushSym(name) \
do{ struct DCCSym *const _old_##name = DCCCompiler_Current.name
#define DCCCompiler_PopSym(name) \
    DCCCompiler_Current.name = _old_##name;\
}while(DCC_MACRO_FALSE)

#define DCCCompiler_PushBCSyms() \
do{ struct DCCSym *const _old_bsym = DCCCompiler_Current.c_bsym;\
    struct DCCSym *const _old_csym = DCCCompiler_Current.c_csym
#define DCCCompiler_PopBCSyms() \
    DCCCompiler_Current.c_csym = _old_csym;\
    DCCCompiler_Current.c_bsym = _old_bsym;\
}while(DCC_MACRO_FALSE)


#define DCCCompiler_PushFunctionScope() \
do{ struct DCCScope _old_fun_scope; \
    memcpy(&_old_fun_scope,&DCCCompiler_Current.c_scope,sizeof(struct DCCScope)); \
    memset(&DCCCompiler_Current.c_scope,0,sizeof(struct DCCScope)); \
    DCCCompiler_Current.c_scope.s_prev  = &_old_fun_scope; \
    DCCCompiler_Current.c_scope.s_depth = _old_fun_scope.s_depth+1;\
    DCCCompiler_Current.c_scope.s_id    = _old_fun_scope.s_id+1;\
    DCCCompiler_Current.c_scope.s_kind  = DCC_SCOPEKIND_FUNC;\
    DCCHWStack_PushNew(&DCCCompiler_Current.c_hwstack)
#define DCCCompiler_PopFunctionScope() \
    DCCHWStack_PopNew(&DCCCompiler_Current.c_hwstack);\
    DCC_ASSERT(DCCCompiler_Current.c_scope.s_prev == &_old_fun_scope);\
    DCCScope_Quit(&DCCCompiler_Current.c_scope);\
    memcpy(&DCCCompiler_Current.c_scope,&_old_fun_scope,sizeof(struct DCCScope));\
}while(DCC_MACRO_FALSE)

#define DCCCompiler_PushLocalScope() \
do{ struct DCCScope _old_loc_scope; \
    memcpy(&_old_loc_scope,&DCCCompiler_Current.c_scope,sizeof(struct DCCScope)); \
    memset(&DCCCompiler_Current.c_scope,0,sizeof(struct DCCScope)); \
    DCCCompiler_Current.c_scope.s_prev  = &_old_loc_scope; \
    DCCCompiler_Current.c_scope.s_depth = _old_loc_scope.s_depth+1;\
    DCCCompiler_Current.c_scope.s_id    = _old_loc_scope.s_id;\
    DCCCompiler_Current.c_scope.s_kind  = DCC_SCOPEKIND_LOCAL;\
    DCCHWStack_Push(&DCCCompiler_Current.c_hwstack)
#define DCCCompiler_PopLocalScope() \
    DCCHWStack_Pop(&DCCCompiler_Current.c_hwstack);\
    DCC_ASSERT(DCCCompiler_Current.c_scope.s_prev == &_old_loc_scope);\
    DCCScope_Quit(&DCCCompiler_Current.c_scope);\
    memcpy(&DCCCompiler_Current.c_scope,&_old_loc_scope,sizeof(struct DCCScope));\
}while(DCC_MACRO_FALSE)

#define DCCCompiler_ISDEAD()   (DCCCompiler_Current.c_flags&DCC_COMPILER_FLAG_DEAD)
#define DCCCompiler_ISCGEN() (!(DCCCompiler_Current.c_flags&DCC_COMPILER_FLAG_NOCGEN))

#define DCCCompiler_ENUMSCOPE(scope) \
 for ((scope) = &DCCCompiler_Current.c_scope;\
      (scope); (scope) = (scope)->s_prev)
#define DCCCompiler_ENUMFUNSCOPE(scope) \
 for ((scope) = &DCCCompiler_Current.c_scope;\
      (scope); (scope) = (scope)->s_kind == \
       DCC_SCOPEKIND_FUNC ? NULL : (scope)->s_prev)

/* Clear all C-declarations. */
DCCFUN void DCCCompiler_ClearDecl(void);

/* Lookup/Create a new C-declaration 'name'.
 * NOTE: All functions will search the entirety of the current scope stack
 *       for an existing definition. Only in the event that none is found,
 *       will a new one be allocated in the current top scope.
 * NOTE: None of these functions return a reference to the symbol!
 * NOTE: [DCCCompiler_NewDecl*] If a declaration with the same name already exists, it will be returned instead!
 * @return: NULL: [DCCCompiler_GetDecl*] No known declaration with the given name.
 *                [DCCCompiler_NewDecl*] Failed to allocate the new declaration (A lexer error was set) */
DCCFUN struct DCCDecl *DCCCompiler_GetDecl(struct TPPKeyword const *__restrict name, DCC(ns_t) ns);
DCCFUN struct DCCDecl *DCCCompiler_NewDecl(struct TPPKeyword const *__restrict name, DCC(ns_t) ns);
DCCFUN struct DCCDecl *DCCCompiler_GetDecls(char const *__restrict name, DCC(ns_t) ns);
DCCFUN struct DCCDecl *DCCCompiler_NewDecls(char const *__restrict name, DCC(ns_t) ns);

#ifdef __INTELLISENSE__
DCCFUN struct DCCDecl *DCCCompiler_NewLocalDecl(struct TPPKeyword const *__restrict name, DCC(ns_t) ns);
#else
#define DCCCompiler_NewLocalDecl(name,ns) DCCDecltab_NewDecl(&DCCCompiler_Current.c_scope.s_tabs[ns],name)
#endif

/* Lookup/declare a new c-label 'name'.
 * >> Label symbols are declared as unnamed and the declarations are of void-typing. */
DCCFUN struct DCCDecl *DCCCompiler_NewLabel(struct TPPKeyword const *__restrict name);

/* Allocate/Free HW stack memory.
 * NOTE: Stack memory is still subject to scoping, which will forceably push/pop stack requirements.
 * NOTE: These are all no-ops when the 'DCC_COMPILER_FLAG_NOCGEN' flag is set. */
DCCFUN DCC(target_off_t) DCCCompiler_HWStackAlloc(DCC(target_siz_t) size, DCC(target_siz_t) align, DCC(target_siz_t) offset);
DCCFUN void DCCCompiler_HWStackFree(DCC(target_off_t) addr, DCC(target_siz_t) size);


/* Push/pop the current structure packing state.
 * NOTE: [DCCCompiler_PackPop] If no old state is available, emit a warning. */
DCCFUN void DCCCompiler_PackPush(void);
DCCFUN void DCCCompiler_PackPop(void);

/* Push/pop the current structure packing state.
 * NOTE: [DCCCompiler_VisibilityPop] If no old state is available, emit a warning. */
DCCFUN void DCCCompiler_VisibilityPush(void);
DCCFUN void DCCCompiler_VisibilityPop(void);


/* Clear the cache of pre-allocated decls. */
DCCFUN void DCCCompiler_ClearCache(void);


#define DCC_COMPILER_STD_CURRENT  0 /*< Keep the currently selected STD version. */
#define DCC_COMPILER_STD_DCC      1 /*< DCC's own C-dialect with all features & extensions enabled (default). */
#define DCC_COMPILER_STD_KR       2 /*< K&R-style C-dialect, disabling any warning about use of its deprecated syntax. */
#define DCC_COMPILER_STD_C90      3 /*< C90 dialect. */
#define DCC_COMPILER_STD_C99      4 /*< C99 dialect. */
#define DCC_COMPILER_STD_C11      5 /*< C11 dialect. */
#define DCC_COMPILER_STD_COUNT    6
#define DCC_COMPILER_STD_KNOWN(i) ((unsigned int)(i) < DCC_COMPILER_STD_COUNT)

#define DCC_COMPILER_STD_ANSI     DCC_COMPILER_STD_C90


/* Set the STD version of C syntax sub-groups that should be recognized by DCC.
 * @param: std: One of 'DCC_COMPILER_STD_*'
 * NOTE: no-op for unknown versions of STD.
 * WARNING: STD behavior is controlled through extensions and warnings,
 *          meaning that user-code can re-configure STD behavior using
 *          '#pragma extension(...)' and '#pragma warning(...)' */
DCCFUN void DCCCompiler_SetStd(int std);

struct DCCStdName {
 char const *sn_nam; /*< [1..1|temp(0..0)] Name of this standard configuration. */
 int         sn_std; /*< STD id. */
};

/* List of all recognized compiler STD versions (terminated with '!sn_nam' entry) */
DCCDAT struct DCCStdName const DCCCompiler_Std[];



#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
DCCDAT struct DCCCompiler compiler;
#else
#define compiler  DCCCompiler_Current
#endif

/* push/pop the current unit c-flags. */
#define pushf      DCCCompiler_Pushf
#define popf       DCCCompiler_Popf
#define setf(f)   (void)(compiler.c_flags|= (f))
#define unsetf(f) (void)(compiler.c_flags&=~(f))

/* push/pop the break/continue symbols. */
#define push_bc DCCCompiler_PushBCSyms
#define pop_bc  DCCCompiler_PopBCSyms

/* push/pop a new function/local scope. */
#define pushscope_function  DCCCompiler_PushFunctionScope
#define popscope_function   DCCCompiler_PopFunctionScope
#define pushscope_local     DCCCompiler_PushLocalScope
#define popscope_local      DCCCompiler_PopLocalScope

#endif

DCC_DECL_END

#endif /* !GUARD_DCC_COMPILER_H */
