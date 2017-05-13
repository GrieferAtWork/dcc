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
#ifndef GUARD_DCC_SYMBOL_H
#define GUARD_DCC_SYMBOL_H 1

#include "common.h"
#include "lexer.h"
#include "target.h"
#ifndef GUARD_DCC_TYPE_H
#include "type.h"
#endif

#include <stddef.h>
#include <stdint.h>

DCC_DECL_BEGIN

struct TPPFile;
struct DCCSym;

#define DCC_SYMTYPE_NONE         0x0000 /*< Unused symbol. */
#define DCC_SYMTYPE_LABEL        0x0001 /*< An addressable location in memory (HINT: May also be abused for constants). */
#define DCC_SYMTYPE_OFFSET       0x0002 /*< An offset to another register. */

#define DCC_SYMTYPE_TYPE         0x0004 /*< A C type (Used as flag). */
#define DCC_SYMTYPE_DECL        (DCC_SYMTYPE_TYPE|0x0000) /*< A regular type declaration (aka. 'typedef int INT;'). */
#define DCC_SYMTYPE_ENUM        (DCC_SYMTYPE_TYPE|0x0010) /*< An enum type (aka. 'enum foo { ... };') ('s_type' is an alias for 'int'). */
#define DCC_SYMTYPE_STRUCT      (DCC_SYMTYPE_TYPE|0x0020) /*< A struct type (aka. 'struct bar { ... };') ('s_type' is a self-reference). */
#define DCC_SYMTYPE_UNION       (DCC_SYMTYPE_TYPE|0x0030) /*< A union type (aka. 'union bar { ... };') ('s_type' is a self-reference). */
#define DCC_SYMTYPE_FUNCTION    (DCC_SYMTYPE_TYPE|0x0040) /*< A function prototype ('s_type' declares the base). */
#define DCC_SYMTYPE_OLDFUNCTION (DCC_SYMTYPE_TYPE|0x0050) /*< An old-style function prototype ('s_type' declares the base; 'st_size' is 0, and 'st_fieldv' is NULL). */
#define DCC_SYMTYPE_ARRAY       (DCC_SYMTYPE_TYPE|0x0060) /*< A fixed-length array (The element count is stored in 's_ctype.st_size'). */
#define DCC_SYMTYPE_VLA         (DCC_SYMTYPE_TYPE|0x0070) /*< A variable-length array (Its absolute, multiplied size is stored as a target-size_t at 'EBP+s_ctype.st_vlaoff'). */

#define DCC_SYMFLAG_NONE        0x0000
#define DCC_SYMFLAG_PUBLIC      0x0000 /*< Public symbol. */
#define DCC_SYMFLAG_HIDDEN      0x0001 /*< Private symbol (not exported from binary). */
#define DCC_SYMFLAG_PROTECTED   0x0002 /*< Protected symbol (not exported from compilation unit). */
#define DCC_SYMFLAG_WEAK        0x0003 /*< Weak symbol. */
#define DCC_SYMFLAG_VISIBILITY  0x000f /*< Mask for visibility flags. */
#define DCC_SYMFLAG_FORWARD     0x0010 /*< Used with 'DCC_SYMTYPE_LABEL': Forward label declaration. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DCC_SYMFLAG_DLLIMPORT   0x0020 /*< On PE targets: dllimport. */
#define DCC_SYMFLAG_DLLEXPORT   0x0040 /*< On PE targets: dllexport. */
#endif
#define DCC_SYMFLAG_VARIADIC    0x1000 /*< Used by 'DCC_SYMTYPE_FUNCTION': Additional arguments are variadic. */
#define DCC_SYMFLAG_COMPDEF     0x2000 /*< When set, the symbol was compiler-defined (meaning that it's type should be interpreted as compatible with any redefinition) */


struct DCCSymLabel { /* DCC_SYMTYPE_LABEL */
 /*ref*/struct DCCSym *sl_alias; /*< [0..1] When non-NULL, another label aliased by this one. */
 struct DCCSection    *sl_sec;   /*< [0..1][const_if(!= NULL)] Section this symbol is defined inside of (or NULL if unknown). */
#ifdef __INTELLISENSE__
 target_ptr_t          sl_addr;  /*< [const_if(sl_sec != NULL)] Symbol address (offset from associated section; undefined if 'sl_sec == NULL') */
 target_ptr_t          sl_size;  /*< Symbol size, or 0 if unknown (used for generating better debug information) */
#else
 DCC(target_ptr_t)     sl_addr;  /*< [const_if(sl_sec != NULL)] Symbol address (offset from associated section; undefined if 'sl_sec == NULL') */
 DCC(target_ptr_t)     sl_size;  /*< Symbol size, or 0 if unknown (used for generating better debug information) */
#endif
 size_t                sl_relc;  /*< Amount of relocations using this symbol (When zero at the time this symbol is destroyed, free section data) */
};

struct DCCSymOffset { /* DCC_SYMTYPE_OFFSET */
 DCC(rc_t)         so_reg;   /*< Register this symbol is derived from. */
 DCC(target_off_t) so_off;   /*< Offset from the associated register. */
 DCC(scopeid_t)    so_scope; /*< The scope associated with this offset-variable. */
};
struct DCCStructField {
 DCC(target_off_t)     sf_off; /*< Field offset, or offset from %EBP for function types. */
 /*ref*/struct DCCSym *sf_sym; /*< [1..1] Field type+name, or function argument.
                                *   NOTE: When the name is 'TPPKeyword_Empty', a struct/union
                                *         should be inlined, or a member should be unnamed. */
 /* TODO: bitfields. */
};
struct DCCSymType { /* DCC_SYMTYPE_TYPE */
union{
 DCC(target_siz_t)      st_size;         /*< Array types use this as element count,
                                          *  or the amount of fields in a struct type,
                                          *  or amount of arguments in a function type. */
 DCC(target_off_t)      st_vlaoff;       /*< Local variable offset for a target-size_t for the absolute size of the array. */
};
 struct DCCStructField *st_fieldv;       /*< [0..st_size|0..0][owned] Vector of structure fields/function arguments. */
union{
 size_t                 st_struct_size;  /*< Used for struct/union types: Total size of the structure. */
 DCC(scopeid_t)         st_vlascope;     /*< Scope id in which 'st_vlaoff' resides. */
};
 size_t                 st_struct_align; /*< Used for struct/union types: Minimum alignment of the structure. */
};

struct DCCSym {
 unsigned int             s_refcnt; /*< Symbol reference counter. */
 struct DCCSym           *s_next;   /*< [0..1][owned] Next symbol with the same modulated 's_name->k_id'. */
 struct DCCSym           *s_before; /*< [0..1][owned] Used for redefining symbols: previous version of this symbol (e.g.: when defining local assembly labels). */
 struct TPPKeyword const *s_name;   /*< [1..1] Symbol name. */
 /*ref*/struct TPPFile   *s_file;   /*< [1..1] Reference to the file this symbol was declared/referenced inside of. */
 DCC(line_t)              s_line;   /*< Line associated with 's_file'. */
 uint16_t                 s_kind;   /*< Symbol type (One of 'DCC_SYMTYPE_*'). */
 uint16_t                 s_flag;   /*< Symbol flags (Set of 'DCC_SYMFLAG_*'). */
 struct DCCType           s_type;   /*< The C-style type of this symbol (Default to 'int', as created by zero-initializing this field). */
 struct DCCAttrDecl      *s_attr;   /*< [0..1] Optional __attribute__((...)) properties. */
 union{
  struct DCCSymLabel      s_label;  /*< [DCC_SYMTYPE_LABEL] Label-style symbol. */
  struct DCCSymOffset     s_offset; /*< [DCC_SYMTYPE_OFFSET] Register-offset-style symbol. */
  struct DCCSymType       s_ctype;  /*< [DCC_SYMTYPE_TYPE] C-style type declaration symbol. */
 };
};



/* Allocates and returns a new symbol.
 * NOTE: All members of that symbol are initialized to NULL, except:
 *       - s_refcnt: Initialized to ONE(1)
 *       - s_file:   Contains a reference to TPPLexer_Textfile()
 *       - s_line:   Contains the current line that the parser of 's_file' is located at.
 *       - s_name:   Set to '&TPPKeyword_Empty' */
DCCFUN /*ref*/struct DCCSym *DCCSym_New(void);

/* Clear the cache of pre-allocated symbol objects. */
DCCFUN void DCCSym_ClearCache(void);

/* Clear various fields of the symbol, allowing for safe cleanup of
 * recursive symbols (e.g.: 'A structure with a self-pointer')
 * This function is called during termination of a symbol tab
 * and can be used to resolve self-referencing symbols by
 * breaking the links.
 * WARNING: Calling this function at any time other than
 *          during destruction will cause weak undefined
 *          behavior, in that the symbol will no longer
 *          be guarantied to behave as expected, yet
 *          should not cause DCC to crash. */
DCCFUN void DCCSym_Clear(struct DCCSym *__restrict self, int force);

DCCFUN void DCCSym_Destroy(struct DCCSym *__restrict self);
#define DCCSym_Incref(self) (void)(++(self)->s_refcnt)
#define DCCSym_Decref(self) (void)(--(self)->s_refcnt || (DCCSym_Destroy(self),0))

/* Set the given attributes in 'self'.
 * NOTE: If 'attr' is empty, don't do anything.
 * NOTE: If 'self' already has attributes set, merge them. */
DCCFUN void DCCSym_SetAttr(struct DCCSym *__restrict self,
                           struct DCCAttrDecl const *__restrict attr);

/* Allocate static/local storage, as requested by the c-type of 'self'
 * NOTE: This function automatically understands that automatic declarations
 *       in the global scope are always public/extern, unless an initializer
 *       is given, or static was specified. It also understands that 
 *       automatic declarations in a local scope use local storage unless
 * NOTE: Based on const qualifiers, global/static storage
 *       may either be allocated in .bss, or in .data.
 * WARNING: Upon entry, the given symbol must be of 'DCC_SYMTYPE_NONE' typing,
 *          while upon exit, it will be one of 'DCC_SYMTYPE_LABEL, DCC_SYMTYPE_OFFSET'
 * @param: has_init: Set to non-zero if an initializer is given, which
 *                   forces variables in the global scope to be declared
 *                   as public, unless static was specified.
 */
DCCFUN void DCCSym_AllocStorage(struct DCCSym *__restrict self, int has_init);

/* Returns non-ZERO(0) if the given symbol 'a' and 'b' describe the same thing.
 * @param: same_declaration: When ZERO(0), only require identical storage
 *                           location for offset/label symbols, not failing
 *                           because of incompatible types. */
DCCFUN int DCCSym_Equal(struct DCCSym const *__restrict a,
                        struct DCCSym const *__restrict b,
                        int same_declaration);

/* Check if a given 'pointer' is located inside 'vector...+=vector_size_in_bytes'
 * NOTE: The caller is responsible to ensure that both symbols
 *       are either of offset- or label-classification.
 * @return: 0: 'vector' and 'pointer' _most_ _definitely_ don't overlap.
 * @return: 1: 'vector' and 'pointer' _most_ _definitely_ do overlap.
 * @return: 2: The correlation between 'vector' and 'pointer' cannot be determined at compile-time. */
DCCFUN int DCCSym_Contains(struct DCCSym const *__restrict vector,
                           DCC(target_siz_t) vector_size_in_bytes,
                           struct DCCSym const *__restrict pointer);

/* Returns the actual, absolute address of
 * a label within a relocated section. */
#define DCCSym_LabelAddr(self) \
 ((void *)((uintptr_t)(self)->s_label.sl_sec->s_base+\
                      (self)->s_label.sl_addr))

struct DCCSymtab {
 size_t          st_symc; /*< Amount of symbols in this table. */
 size_t          st_syma; /*< Allocated map size (aka. bucket count). */
 struct DCCSym **st_symv; /*< [0..1][owned][0..st_syma][owned] Hash-map of symbol (index == s_name->k_id % st_syma) */
};

#define DCCSymtab_Init(self) (void)memset(self,0,sizeof(struct DCCSymtab))
DCCFUN void DCCSymtab_Quit(struct DCCSymtab *__restrict self, int force);
#define DCCSymtab_Clear(self) (DCCSymtab_Quit(self,0),DCCSymtab_Init(self))

/* Lookup/Create a new symbol within the given symtab.
 * NOTE: 'DCCSymtab_New' behaves identical to
 *       'DCCSymtab_Get', but will create missing symbols.
 * @return: NULL: [DCCSymtab_Get] No such section exists.
 * @return: NULL: [DCCSymtab_New] Not enough available memory. (A TPP lexer error was set)
 * @return: * :    A pointer to the requested section. */
DCCFUN struct DCCSym *DCCSymtab_Get(struct DCCSymtab *__restrict self, struct TPPKeyword const *__restrict name);
DCCFUN struct DCCSym *DCCSymtab_New(struct DCCSymtab *__restrict self, struct TPPKeyword const *__restrict name);

/* Same as 'DCCSymtab_Get', but you may instead specify a string. */
DCCFUN struct DCCSym *
DCCSymtab_Lookup(struct DCCSymtab *__restrict self,
                 char const *__restrict name,
                 int create_missing);
DCC_LOCAL struct DCCSym *
DCCSymtab_LookupType(struct DCCSymtab *__restrict self,
                     char const *__restrict name, uint16_t type) {
 struct DCCSym *result = DCCSymtab_Lookup(self,name,0);
 if (result && result->s_kind != type) result = NULL;
 return result;
}

/* Defines a new label 'name' within the given
 * symbol table, pointing to dest_section and 'addr' */
DCC_LOCAL struct DCCSym *
DCCSymtab_DefLabel(struct DCCSymtab *__restrict self, char const *__restrict name,
                   struct DCCSection *__restrict dest_section, DCC(target_ptr_t) addr) {
 struct DCCSym *result = DCCSymtab_Lookup(self,name,1);
 if (result) {
  if (result->s_kind == DCC_SYMTYPE_NONE) result->s_kind = DCC_SYMTYPE_LABEL;
  if (result->s_kind == DCC_SYMTYPE_LABEL) {
   result->s_label.sl_sec  = dest_section;
   result->s_label.sl_addr = addr;
  }
 }
 return result;
}

/* Define an absolute symbol located at 'addr' */
#define DCCSymtab_DefAbsLabel(self,name,addr) \
 DCCSymtab_DefLabel(self,name,&DCCSection_Global,addr)


/* Lookup/create a new forward symbol, as used
 * by 'jmp 1f'-style forward declarations. */
DCCFUN struct DCCSym *
DCCSymtab_NewForwardLabel(struct DCCSymtab *__restrict self,
                          struct TPPKeyword *__restrict name);


struct DCCSymlst {
 /*ref*/struct DCCSym *sl_sym; /*< [1..1] Last symbol (chained through 's_next') */
};

DCC_LOCAL struct DCCSym *
DCCSymlst_Alloc(struct DCCSymlst *__restrict self) {
 struct DCCSym *result;
 DCC_ASSERT(self);
 if ((result = DCCSym_New()) != NULL) {
  result->s_next = self->sl_sym; /* Inherit reference. */
  self->sl_sym   = result;       /* Inherit reference. */
 }
 return result;
}

DCC_LOCAL void
DCCSymlst_Quit(struct DCCSymlst *__restrict self) {
 struct DCCSym *iter,*next;
 DCC_ASSERT(self);
 iter = self->sl_sym;
 while (iter) {
  next = iter->s_next;
  DCCSym_Clear(iter,1);
  DCCSym_Decref(iter);
  iter = next;
 }
}




DCC_DECL_END

#endif /* !GUARD_DCC_SYMBOL_H */
