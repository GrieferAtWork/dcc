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
#ifndef GUARD_DCC_LEXER_DECL_C_INL
#define GUARD_DCC_LEXER_DECL_C_INL 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/fundecl.h>

#include <string.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

struct DCCArgAllocator {
 rc_t         aa_basereg; /*< Base-register used for argument allocation. */
 target_off_t aa_offset;  /*< Current argument offset from the base register. */
};

PRIVATE void
DCCParse_AllocFunArgs(struct DCCArgAllocator *__restrict allocator,
                      struct DCCDecl const *__restrict funty_decl) {
 struct DCCStructField *iter,*end;
 end = (iter = funty_decl->d_tdecl.td_fieldv)+
               funty_decl->d_tdecl.td_size;
 for (; iter != end; ++iter) {
  struct TPPKeyword const *name;
  struct DCCDecl *local_decl;
  assert(iter->sf_decl);
  name = iter->sf_decl->d_name;
  assert(name);
  assert(iter->sf_decl->d_kind&DCC_DECLKIND_TYPE);
  if (name != &TPPKeyword_Empty) {
   local_decl = DCCCompiler_NewLocalDecl(iter->sf_decl->d_name,DCC_NS_LOCALS);
   if unlikely(!local_decl) break;
   if (local_decl->d_kind == DCC_DECLKIND_NONE) {
    assert(local_decl->d_mdecl.md_loc.ml_sym == NULL);
    DCCType_InitCopy(&local_decl->d_type,&iter->sf_decl->d_type);
    local_decl->d_kind                = DCC_DECLKIND_MLOC;
    local_decl->d_mdecl.md_loc.ml_off = allocator->aa_offset+iter->sf_off;
    local_decl->d_mdecl.md_loc.ml_reg = allocator->aa_basereg;
    local_decl->d_mdecl.md_scope      = compiler.c_scope.s_id;
   } else {
    /* Warning: re-used argument name. */
    WARN(W_DECL_ALREADY_DEFINED,local_decl);
   }
  }
 }
}

LEXPRIV void DCC_PARSE_CALL
DCCParse_Function(struct DCCDecl *fun_decl, struct TPPKeyword const *asmname,
                  struct DCCType const *__restrict fun_type,
                  struct DCCAttrDecl const *__restrict fun_attr) {
 struct DCCFunctionFrame frame;
 struct DCCSym *fun_sym = NULL;
 struct DCCArgAllocator function_allocator;
 /* TODO: arrow-style return expressions/return-type-declarations:
  * >> auto add(int x, int y) -> typeof(x+y) { return x+y; }
  * >> auto add(int x, int y) -> x+y; // Deemon-style (Define return type & expression)
  */
 /* Create the function scope. */
 pushscope_function();
 if (fun_attr->a_specs&DCC_ATTRSPEC_NAKED) {
  function_allocator.aa_offset  = DCC_TARGET_SIZEOF_POINTER;
  function_allocator.aa_basereg = DCC_RR_XSP;
  --compiler.c_scope.s_id; /* Naked functions don't generate new stack-frames. */
 } else {
  function_allocator.aa_offset  = DCC_TARGET_SIZEOF_POINTER*2;
  function_allocator.aa_basereg = DCC_RR_XBP;
 }
 /* Allocate arguments. */
 if (DCCTYPE_GROUP(fun_type->t_type) == DCCTYPE_FUNCTION &&
    (fun_type->t_base->d_kind == DCC_DECLKIND_FUNCTION ||
     fun_type->t_base->d_kind == DCC_DECLKIND_OLDFUNCTION)) {
  DCCParse_AllocFunArgs(&function_allocator,fun_type->t_base);
 }
 /* Actually parse the function body. */
 if (fun_decl) {
  if (fun_decl->d_kind == DCC_DECLKIND_NONE)
      fun_decl->d_kind =  DCC_DECLKIND_MLOC;
  if (fun_decl->d_kind&DCC_DECLKIND_MLOC) {
   fun_sym = fun_decl->d_mdecl.md_loc.ml_sym;
   /* TODO: Shouldn't local function be static+unnamed to prevent symbol re-declaration?
    *       What does GCC do here? (Go figure it out and mirror that behavior!) */
   if (!fun_sym) {
    /* Allocate missing function symbols. */
    if (!asmname) asmname = DCCDecl_GenAsmname(fun_decl);
    fun_sym = (asmname == &TPPKeyword_Empty)
     ? DCCUnit_AllocSym()
     : DCCUnit_NewSym(asmname,DCC_SYMFLAG_NONE);
    fun_decl->d_mdecl.md_loc.ml_sym = fun_sym;
    DCCSym_XIncref(fun_sym);
   }
  }
 }
 if unlikely(!fun_sym) fun_sym = DCCUnit_AllocSym();
 if unlikely(!fun_sym) goto done;
 DCCFunctionFrame_Enter(&frame,fun_decl,fun_sym,fun_attr);
 /* Parse the function scope text. */
 YIELD();
 if (TOK != '}') {
  DCCParse_ScopeText(DCC_PFLAG_NONE);
  vpop(1); /* Pop the last expression within the function. */
 }
 if (TOK != '}') WARN(W_EXPECTED_RBRACE); else YIELD();
 DCCFunctionFrame_Leave(&frame);
done:
 popscope_function();
}


LEXPRIV int DCC_PARSE_CALL
DCCParse_OneDeclWithBase(struct DCCType *__restrict base_type,
                         struct DCCAttrDecl *__restrict base_attr,
                         int *__restrict has_real_base_type) {
 struct DCCType type;
 struct DCCAttrDecl attr; int result;
 struct TPPKeyword *decl_name;
again:
 DCCType_ASSERT(base_type);
 DCCType_InitCopy(&type,base_type);
 DCCAttrDecl_InitCopy(&attr,base_attr);
 decl_name = DCCParse_CTypeSuffix(&type,&attr);
 DCCType_ASSERT(&type);
 if (!*has_real_base_type &&
     decl_name != &TPPKeyword_Empty &&
     type.t_type == (DCCTYPE_BUILTIN|DCCTYPE_INT)) {
  /* Explicitly check for situations like this:
   * >> LOCAL int add(int x, int y) { return x+y; }
   *    ^^^^^ - This was never defined
   * Mainly because this is quite a reasonably common situation,
   * as well as the fact that while the lexer is fully capable of
   * doing ~something~ even without this check, we perform it to
   * improve error messages, as well as increase the chances of
   * still managing to parse broken code correctly.
   * >> The way this check if performed, is by looking
   *    at weather the determined type is 'int' and
   *    if that type was just guessed. */
  assert(!type.t_base);
  /* Check if what follows indicates an initializer. */
  if (TOK == ',' || TOK == ';' || TOK == '=' || TOK == '{' ||
     (TOK == KWD_asm && HAS(EXT_SHORT_EXT_KEYWORDS)) ||
      TOK == KWD___asm || TOK == KWD___asm__ ||
     (HAS(EXT_OLD_VARIABLE_INIT) && DCCParse_IsExpr()));
  else {
   /* Something that isn't an initializer follows!
    * Looking back at the example above, 'int' follows
    * the unknown LOCAL keyword.
    * _BUT_ you have to remember that there is that whole
    * implicit int-typing thing, meaning that unless we
    * can prove that a real type follows 'LOCAL', we'll
    * be forced to compile the statement as usual. */
   struct DCCAttrDecl follow_attr = DCCATTRDECL_INIT;
   struct DCCType follow_type;
   if (DCCParse_CTypePrefix(&follow_type,&follow_attr)) {
    /* There is a real type located after the unknown keyword.
     * So with that in mind, emit a warning and begin using
     * that ~real~ type as base for all further declarations
     * within the current type-group. */
    WARN(W_UNKNOWN_KEYWORD_BEFORE_DECLARATION,decl_name);
    /* Indicate that a real type base has been found. */
    *has_real_base_type = 1;
    /* Override the base type with the real
     * follow-type after the unknown keyword. */
    DCCType_Quit(base_type);
    *base_type = follow_type;
    /* Merge all attribute we've encountered along the way. */
    DCCAttrDecl_Merge(base_attr,&attr);
    DCCAttrDecl_Quit(&attr);
    DCCAttrDecl_Merge(base_attr,&follow_attr);
    DCCAttrDecl_Quit(&follow_attr);
    /* Start over parsing with the new base & attributes in mind. */
    goto again;
   }
   /* Merge follow-attributes into 'attr' */
   DCCAttrDecl_Merge(&attr,&follow_attr);
   DCCAttrDecl_Quit(&follow_attr);
  }
 }
 result = DCCParse_OneDeclWithType(&type,&attr,decl_name);
 DCCType_Quit(&type);
 DCCAttrDecl_Quit(&attr);
 return result;
}
LEXDECL int DCC_PARSE_CALL
DCCParse_OneDeclWithType(struct DCCType *__restrict decl_type,
                         struct DCCAttrDecl *__restrict decl_attr,
                         struct TPPKeyword *__restrict decl_name) {
 struct DCCDecl *decl;
 struct TPPKeyword const *asmname = NULL;
 struct DCCType *real_decl_type;
 int result = 1;
 assert(decl_type);
 assert(decl_attr);
 assert(decl_name);
 DCCType_ASSERT(decl_type);

 if ((TOK == KWD_asm && HAS(EXT_SHORT_EXT_KEYWORDS)) ||
      TOK == KWD___asm || TOK == KWD___asm__) {
  struct TPPString *asmname_string;
  /* Assembly name declaration. */
  YIELD();
  DCCParse_ParPairBegin();
  if (TPP_ISSTRING(TOK)) {
   asmname_string = DCCParse_String();
  } else {
   WARN(W_EXPECTED_STRING_FOR_ASSEMBLY_NAME);
   asmname_string = NULL;
  }
  DCCParse_ParPairEnd();
  if (asmname_string) {
   asmname = TPPLexer_LookupKeyword(asmname_string->s_text,
                                    asmname_string->s_size,1);
   TPPString_Decref(asmname_string);
  }
  DCCParse_Attr(decl_attr);
 }
 if (DCCType_ISVLA(decl_type)) DCCParse_WarnAllocaInLoop();
 if (decl_name != &TPPKeyword_Empty) {
  decl = DCCCompiler_NewLocalDecl(decl_name,DCC_NS_LOCALS);
 } else if (TOK != '=' && TOK != '{' &&
           (!HAS(EXT_OLD_VARIABLE_INIT) || !DCCParse_IsExpr())) {
  decl = NULL;
  goto push_void;
 } else {
  decl = DCCDecl_New(decl_name);
 }
 /* Check if the declaration already exists */
 if (decl) {
  if (decl->d_kind != DCC_DECLKIND_NONE) {
   int declaration_did_change = 0;
   /* Fix redeclaration types. */
   DCCType_FixRedeclaration(decl_type,&decl->d_type);
   if (decl->d_kind&DCC_DECLKIND_MLOC) {
    /* Warn about re-declarations. */
    if (decl->d_mdecl.md_loc.ml_reg != DCC_RC_CONST) {
     WARN(W_DECL_ALREADY_DEFINED,decl);
     declaration_did_change = 1;
    } else if (decl->d_mdecl.md_loc.ml_sym) {
     if (!DCCSym_ISFORWARD(decl->d_mdecl.md_loc.ml_sym) &&
         (TOK == '=' || TOK == '{' || (HAS(EXT_OLD_VARIABLE_INIT) && DCCParse_IsExpr()))) {
      WARN(W_DECL_ALREADY_DEFINED,decl);
      declaration_did_change = 1;
     }
     if (!asmname) asmname = decl->d_mdecl.md_loc.ml_sym->sy_name;
     else if (decl->d_mdecl.md_loc.ml_sym->sy_name != asmname) {
      WARN(W_INCOMPATIBLE_ASM_NAMES,decl,
           decl->d_mdecl.md_loc.ml_sym->sy_name,asmname);
      declaration_did_change = 1;
     }
    }
   }
   /* Warn if the types changed between the declaration and here! */
   if (!DCCType_IsCompatible(&decl->d_type,decl_type,0)) {
    WARN((decl_type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_TYPEDEF
         ? W_INCOMPATIBLE_TYPEDEF_TYPES
         : W_INCOMPATIBLE_IMPLEMENTATION_TYPES,
         decl,&decl->d_type,decl_type);
    declaration_did_change = 1;
   }
   /* Clear the declaration before re-initialization. */
   if (decl->d_depth < compiler.c_scope.s_depth ||
       decl->d_type.t_base) {
    struct DCCDecl *newdecl;
    if (declaration_did_change) {
     WARN(W_INCOMPATIBLE_CANNOT_REDEFINE,
          decl,&decl->d_type,decl_type);
    }
    newdecl = DCCDecl_New(&TPPKeyword_Empty);
    if (decl_name == &TPPKeyword_Empty)
        DCCDecl_Decref(decl);
    if unlikely(!newdecl) goto no_decl;
    decl      = newdecl;
    decl_name = &TPPKeyword_Empty;
   } else {
    DCCDecl_Clear(decl,compiler.c_scope.s_depth);
   }
  }
  DCCType_ASSERT(decl_type);
  DCCType_InitCopy(&decl->d_type,decl_type);
  DCCDecl_SetAttr(decl,decl_attr);
  real_decl_type = &decl->d_type;
 } else {
no_decl:
  real_decl_type = decl_type;
 }
 if (TOK == '=') {
  /* Symbol initializer. */
  YIELD();
init_after_equals:
  pushf();
  compiler.c_flags &= ~(DCC_COMPILER_FLAG_SINIT);
  if (!compiler.c_fun ||
     (real_decl_type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_EXTERN ||
     (real_decl_type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_STATIC) {
   /* Parse a static initializer. */
   /* TODO: One-time static initialization?
    * s.a.: 'W_NON_CONSTANT_GLOBAL_INITIALIZER' and
    *       'W_NON_CONSTANT_STATIC_INITIALIZER'
    *       in 'lexer-init.c.inl' */
   compiler.c_flags |= (DCC_COMPILER_FLAG_SINIT);
  }
  if (decl && (real_decl_type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_TYPEDEF) {
   WARN(W_DECL_TYPEDEF_WITH_INITIALIZER,decl);
   goto declare_typedef;
  }
  DCCParse_Init(real_decl_type,decl_attr,NULL,
                DCCPARSE_INITFLAG_INITIAL);
  DCCParse_FixType(real_decl_type);
  if (decl) {
   DCCDecl_AllocStorage(decl,1,asmname);
   vpushd(decl); /* init, decl */
   vswap();      /* decl, init */
   if (DCCType_ISVLA(real_decl_type)) {
    /* Special case: Ignore VLA initializers. */
    vpop(1);
   } else {
    vstore(1);
   }
   assert(decl->d_kind == DCC_DECLKIND_MLOC);
   if (decl->d_mdecl.md_loc.ml_reg == DCC_RC_CONST) {
    struct DCCSym *decl_sym = decl->d_mdecl.md_loc.ml_sym;
    assert(vbottom->sv_sym == decl_sym);
    if likely(decl_sym && decl_sym->sy_sec &&
             !DCCSection_ISIMPORT(decl_sym->sy_sec) &&
             /* The following check will be repeated later,
              * but since '.data' and '.bss' (the most likely candidates)
              * both are not mergeable by default, this is such a rare
              * case that it is usually faster to skip all the no-ops
              * function calls below. */
             (decl_sym->sy_sec->sc_start.sy_flags&DCC_SYMFLAG_SEC_M)) {
     target_siz_t type_align;
     target_ptr_t newaddr;
     /* Try to merge declaration memory in mergeable sections. */
     DCCType_Sizeof(real_decl_type,&type_align,0);
     newaddr = DCCSection_DMerge(decl_sym->sy_sec,
                                 decl_sym->sy_addr,
                                 decl_sym->sy_size,
                                 type_align,0);
     if (newaddr != decl_sym->sy_addr) {
      /* Update the symbol address. */
      DCCSection_DIncref(decl_sym->sy_sec,newaddr,decl_sym->sy_size);
      DCCSection_DDecref(decl_sym->sy_sec,decl_sym->sy_addr,decl_sym->sy_size);
      decl_sym->sy_addr = newaddr;
     }
    }
   }
  }
  popf();
  goto end_nopush; /* Don't push again below! */
 } else if (TOK == '{') {
  if (decl && (real_decl_type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_TYPEDEF)
      WARN(W_DECL_TYPEDEF_WITH_INITIALIZER,decl),decl = NULL;
  if (HAS(EXT_OLD_VARIABLE_INIT) && decl &&
      DCCTYPE_GROUP(decl->d_type.t_type) != DCCTYPE_FUNCTION)
      goto old_style_init; /* Old-style brace initializer. */
  DCCParse_Function(decl,asmname,real_decl_type,decl_attr);
  result = 2;
  if (decl && (real_decl_type->t_type&DCCTYPE_STOREBASE) ==
      DCCTYPE_TYPEDEF) goto declare_typedef;
 } else if (HAS(EXT_OLD_VARIABLE_INIT) && DCCParse_IsExpr()) {
old_style_init:
  WARN(W_OLD_STYLE_INITIALIZER_ASSIGNMENT);
  goto init_after_equals;
 } else if (decl) {
  if ((real_decl_type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_TYPEDEF) {
declare_typedef:
   decl->d_kind = DCC_DECLKIND_TYPE; /* Typedef */
   /* Delete storage-class information in typedefs. */
   real_decl_type->t_type &= ~(DCCTYPE_STOREMASK);
  } else {
   /* Uninitialized storage initialization. */
   if (real_decl_type->t_type == DCCTYPE_AUTO) {
    WARN(W_AUTO_TYPE_REQUIRES_INITIALIZER);
    real_decl_type->t_type = DCCTYPE_INT;
   }
   DCCDecl_AllocStorage(decl,0,asmname);
  }
 } else {
  /* Simply push void if we failed to allocate a declaration. */
push_void:
  vpushv();
 }
 if (decl) {
  vpushd(decl);
  /* Drop a reference from an unnamed decl (created as reference above!) */
end_nopush:
  if (decl_name == &TPPKeyword_Empty)
      DCCDecl_XDecref(decl);
 }
 return result;
}

LEXPRIV int DCC_PARSE_CALL
DCCParse_DeclWithBase(struct DCCType *__restrict base_type,
                      struct DCCAttrDecl *__restrict base_attr,
                      int has_real_base_type) {
 int result;
 assert(base_type);
 for (;;) {
  DCCType_ASSERT(base_type);
  result = DCCParse_OneDeclWithBase(base_type,base_attr,
                                   &has_real_base_type);
  DCCType_ASSERT(base_type);
  if (TOK != ',') break;
  vpop(1); /* Pop this declaration. */
  YIELD();
 }
 return result;
}

struct missing_type {
 char const    *mt_name; /*< [1..1|eof(0..0)] Name of the type. */
 size_t         mt_size; /*< == strlen(mt_name). */
 struct DCCType mt_type; /*< The type itself. */
};
#define TYPE(name,ty)       {name,DCC_COMPILER_STRLEN(name),{ty,NULL}}
#define XTYPE(name,ty,base) {name,DCC_COMPILER_STRLEN(name),{ty,base}}
PRIVATE struct missing_type const mtypes[] = {
 /* Recognize a collection of standard types by name.
  * (Improves error handling for unknown types) */
#if 1 /* <stddef.h> */
 TYPE("size_t",DCCTYPE_SIZE|DCCTYPE_UNSIGNED),
 TYPE("ptrdiff_t",DCCTYPE_PTRDIFF),
#endif
#if 1 /* misc... */
 TYPE("ssize_t",DCCTYPE_SIZE),
#endif
#if 1 /* <stdint.h> */
 TYPE("intmax_t",DCCTYPE_INTMAX),
 TYPE("uintmax_t",DCCTYPE_INTMAX|DCCTYPE_UNSIGNED),
#ifdef DCCTYPE_INT8
 TYPE("int8_t",DCCTYPE_INT8),
 TYPE("uint8_t",DCCTYPE_INT8|DCCTYPE_UNSIGNED),
#endif
#ifdef DCCTYPE_INT16
 TYPE("int16_t",DCCTYPE_INT16),
 TYPE("uint16_t",DCCTYPE_INT16|DCCTYPE_UNSIGNED),
#endif
#ifdef DCCTYPE_INT32
 TYPE("int32_t",DCCTYPE_INT32),
 TYPE("uint32_t",DCCTYPE_INT32|DCCTYPE_UNSIGNED),
#endif
#ifdef DCCTYPE_INT64
 TYPE("int64_t",DCCTYPE_INT64),
 TYPE("uint64_t",DCCTYPE_INT64|DCCTYPE_UNSIGNED),
#endif
 TYPE("int_least8_t",DCCTYPE_INT_LEAST8),
 TYPE("int_least16_t",DCCTYPE_INT_LEAST16),
 TYPE("int_least32_t",DCCTYPE_INT_LEAST32),
 TYPE("int_least64_t",DCCTYPE_INT_LEAST64),
 TYPE("uint_least8_t",DCCTYPE_INT_LEAST8|DCCTYPE_UNSIGNED),
 TYPE("uint_least16_t",DCCTYPE_INT_LEAST16|DCCTYPE_UNSIGNED),
 TYPE("uint_least32_t",DCCTYPE_INT_LEAST32|DCCTYPE_UNSIGNED),
 TYPE("uint_least64_t",DCCTYPE_INT_LEAST64|DCCTYPE_UNSIGNED),
 TYPE("int_fast8_t",DCCTYPE_INT_FAST8),
 TYPE("int_fast16_t",DCCTYPE_INT_FAST16),
 TYPE("int_fast32_t",DCCTYPE_INT_FAST32),
 TYPE("int_fast64_t",DCCTYPE_INT_FAST64),
 TYPE("uint_fast8_t",DCCTYPE_INT_FAST8|DCCTYPE_UNSIGNED),
 TYPE("uint_fast16_t",DCCTYPE_INT_FAST16|DCCTYPE_UNSIGNED),
 TYPE("uint_fast32_t",DCCTYPE_INT_FAST32|DCCTYPE_UNSIGNED),
 TYPE("uint_fast64_t",DCCTYPE_INT_FAST64|DCCTYPE_UNSIGNED),
 TYPE("intptr_t",DCCTYPE_INTPTR),
 TYPE("uintptr_t",DCCTYPE_INTPTR|DCCTYPE_UNSIGNED),
#endif
#if 1 /* <wchar.h> */
 TYPE("wchar_t",DCCTYPE_WCHAR|DCCTYPE_UNSIGNED),
 TYPE("__wchar_t",DCCTYPE_WCHAR|DCCTYPE_UNSIGNED),
#endif
#if 1 /* <stdatomic.h> */
 TYPE("sig_atomic_t",DCCTYPE_SIG_ATOMIC|DCCTYPE_UNSIGNED),
#endif
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
 /* Type names commonly used in windows. */
 TYPE("VOID",DCCTYPE_VOID),
 TYPE("INT",DCCTYPE_INT),
 TYPE("UINT",DCCTYPE_INT|DCCTYPE_UNSIGNED),
 TYPE("CHAR",DCCTYPE_CHAR),
 TYPE("WCHAR",DCCTYPE_WCHAR),
 TYPE("SHORT",DCCTYPE_SHORT),
 TYPE("LONG",DCCTYPE_LONG|DCCTYPE_ALTLONG),
 TYPE("BOOL",DCCTYPE_INT),
 TYPE("BYTE",DCCTYPE_BYTE|DCCTYPE_UNSIGNED),
 TYPE("BOOLEAN",DCCTYPE_BYTE|DCCTYPE_UNSIGNED),
 TYPE("WORD",DCCTYPE_WORD|DCCTYPE_UNSIGNED),
 TYPE("DWORD",DCCTYPE_LONG|DCCTYPE_ALTLONG|DCCTYPE_UNSIGNED),
 TYPE("FLOAT",DCCTYPE_FLOAT),
 TYPE("LONGLONG",DCCTYPE_LLONG),
 TYPE("ULONGLONG",DCCTYPE_LLONG|DCCTYPE_UNSIGNED),
 TYPE("INT_PTR",DCCTYPE_INTPTR),
 TYPE("UINT_PTR",DCCTYPE_INTPTR|DCCTYPE_UNSIGNED),
#if DCC_TARGET_SIZEOF_POINTER == DCC_TARGET_SIZEOF_LONG
 TYPE("LONG_PTR",DCCTYPE_LONG|DCCTYPE_ALTLONG),
 TYPE("ULONG_PTR",DCCTYPE_LONG|DCCTYPE_ALTLONG|DCCTYPE_UNSIGNED),
#else
 TYPE("LONG_PTR",DCCTYPE_INTPTR),
 TYPE("ULONG_PTR",DCCTYPE_INTPTR|DCCTYPE_UNSIGNED),
#endif
 TYPE("INT8",  DCCTYPE_INT8),
 TYPE("INT16", DCCTYPE_INT16),
 TYPE("INT32", DCCTYPE_INT32),
 TYPE("INT64", DCCTYPE_INT64),
 TYPE("INT8",  DCCTYPE_INT8|DCCTYPE_UNSIGNED),
 TYPE("INT16", DCCTYPE_INT16|DCCTYPE_UNSIGNED),
 TYPE("INT32", DCCTYPE_INT32|DCCTYPE_UNSIGNED),
 TYPE("INT64", DCCTYPE_INT64|DCCTYPE_UNSIGNED),
 TYPE("LONG32",DCCTYPE_INT32),
#endif
#if !!(DCC_TARGET_OS&DCC_OS_F_UNIX)
 /* Type names commonly used in unix. */
 TYPE("s8", DCCTYPE_INT8), TYPE("__s8", DCCTYPE_INT8),
 TYPE("s16",DCCTYPE_INT16),TYPE("__s16",DCCTYPE_INT16),
 TYPE("s32",DCCTYPE_INT32),TYPE("__s32",DCCTYPE_INT32),
 TYPE("s64",DCCTYPE_INT64),TYPE("__s64",DCCTYPE_INT64),
 TYPE("u8", DCCTYPE_INT8|DCCTYPE_UNSIGNED), TYPE("__u8", DCCTYPE_INT8|DCCTYPE_UNSIGNED),
 TYPE("u16",DCCTYPE_INT16|DCCTYPE_UNSIGNED),TYPE("__u16",DCCTYPE_INT16|DCCTYPE_UNSIGNED),
 TYPE("u32",DCCTYPE_INT32|DCCTYPE_UNSIGNED),TYPE("__u32",DCCTYPE_INT32|DCCTYPE_UNSIGNED),
 TYPE("u64",DCCTYPE_INT64|DCCTYPE_UNSIGNED),TYPE("__u64",DCCTYPE_INT64|DCCTYPE_UNSIGNED),
#endif
 {NULL,0,{0,NULL}},
};
#undef XTYPE
#undef TYPE


PUBLIC int DCC_PARSE_CALL DCCParse_Decl(void) {
 struct DCCType base; int result;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
parse_prefix:
 result = DCCParse_CTypePrefix(&base,&attr);
 if (result) {
got_prefix:
  result = DCCParse_DeclWithBase(&base,&attr,1);
 } else {
  struct DCCDecl *struct_decl;
  struct TPPKeyword *next_kwd;
  struct TPPFile *next_file; char *next_tok;
  int recognized_type;
  /* Better error handling for something like this:
   * >> DWORD x = 42;
   *    ^^^^^ - Never defined
   * Currently, when appearing inside a function-scope,
   * the compiler will take 'DWORD' and compile it as
   * 'extern int DWORD();', before being annoyed that
   * there is a ';' missing afterwards.
   * #1 Handle this by first confirming that the
   *    current token is a keyword that is not
   *    part of the 'DCC_NS_LOCALS' namespace.
   * #2 Following that, peek the text for the next
   *    token and check if it only contains ALNUM
   *    characters (as well as escaped linefeeds).
   * #3 With the next token parsed as a keyword,
   *    check if there is a macro defined under
   *    the same name (If there is, stop).
   * #4 Make sure that the following keyword can't appear
   *    in an expression suffix (currently only '__pack' can)
   * If all of the above succeed, it is _most_ likely
   * that the current token is quite simply an unknown
   * type name that we can warn about before compiling
   * it as 'int', meaning that without an existing
   * typedef for 'DWORD', the above expression will be
   * compiled as 'int x = 42;' */
  if (!TPP_ISKEYWORD(TOK)) goto end;
  assert(TOKEN.t_kwd);
  if (DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_LOCALS)) goto end;
  next_tok = peek_next_token(&next_file);
  next_kwd = peek_keyword(next_file,next_tok,1);
  /* Check that the next token isn't a defined macro. */
  if (!next_kwd || TPPKeyword_ISDEFINED(next_kwd)) goto end;
  /* xxx: Any additional keywords that can appear after an expression must be added here! */
  if (next_kwd->k_id == KWD___pack) goto end;

  /* As an additional check after all of the above,
   * in the event that the original keyword _is_
   * known as a type, but lacking a struct/union/enum prefix
   * (which could easily happen when importing code from c++,
   *  with the fact that DCC even supports l-values in mind),
   * emit a different warning and automatically prepend the prefix. */
  struct_decl = DCCCompiler_GetDecl(TOKEN.t_kwd,DCC_NS_STRUCT);
  recognized_type = 0;
  if (struct_decl) {
   if (struct_decl->d_kind == DCC_DECLKIND_STRUCT ||
       struct_decl->d_kind == DCC_DECLKIND_UNION) {
    base.t_type = DCCTYPE_STRUCTURE;
    base.t_base = struct_decl;
    DCCDecl_Incref(struct_decl);
    WARN(struct_decl->d_kind == DCC_DECLKIND_STRUCT
         ? W_DECLARATION_TYPE_MISSES_PREFIX_STRUCT
         : W_DECLARATION_TYPE_MISSES_PREFIX_UNION,
         &base);
   } else if (struct_decl->d_kind == DCC_DECLKIND_ENUM) {
    assert(base.t_type == DCCTYPE_INT);
    assert(!base.t_base);
    WARN(W_DECLARATION_TYPE_MISSES_PREFIX_ENUM,&base);
   } else goto default_unknown_type;
   recognized_type = 1;
  } else {
   struct missing_type const *iter;
   char const *type_name; size_t type_size;
   tyid_t flags;
default_unknown_type:
   /* Check if we recognize the type's name from the C standard library. */
   type_name = TOKEN.t_kwd->k_name;
   type_size = TOKEN.t_kwd->k_size;
   assert(!base.t_base);
   flags = 0;
   for (;;) {
    if (type_size > 7 &&
       !memcmp(type_name,"atomic_",7*sizeof(char))) {
     /* Automatically recognize 'atomic_*' prefix, as used in <stdatomic.h> */
     type_name += 7;
     type_size -= 7;
     flags     |= DCCTYPE_ATOMIC;
     continue;
    }
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
    /* Windows-style leading type modifiers (e.g.: 'typedef void *PVOID,*LPVOID;') */
    if (type_size > 1 && type_name[0] == 'P') { ++type_name,--type_size; flags |= DCCTYPE_POINTER; continue; }
    if (type_size > 2 && type_name[0] == 'L' && type_name[1] == 'P') { type_name += 2,type_size -= 2; flags |= DCCTYPE_POINTER; continue; }
    if (type_size > 1 && type_name[0] == 'C') { ++type_name,--type_size; flags |= DCCTYPE_CONST; continue; }
    if (type_size > 1 && type_name[0] == 'V') { ++type_name,--type_size; flags |= DCCTYPE_VOLATILE; continue; }
#endif
    break;
   }

   for (iter = mtypes; iter->mt_name; ++iter) {
    if (iter->mt_size == type_size &&
       !memcmp(iter->mt_name,type_name,type_size*sizeof(char)) &&
       (!(flags&DCCTYPE_ATOMIC) || !iter->mt_type.t_base)) {
     DCCType_InitCopy(&base,&iter->mt_type);
     base.t_type |= flags&~(DCCTYPE_GROUPMASK);
     if (flags&DCCTYPE_POINTER) DCCType_MkPointer(&base);
     recognized_type = 1;
     break;
    }
   }
   WARN(W_DECLARATION_CONTAINS_UNKNOWN_TYPE,&base);
  }
  YIELD();
  /* Special case: Parse the type again if the ~real~ keyword
   *               following an unknown one inside a declaration
   *               is exclusive to type declarations. */
  if (DCC_ISTYPEKWD(TOKEN.t_id)) {
   /* TODO: What about:
    * >> [[arithmetic]] struct int128 { char v[16]; };
    * >> 
    * >> int128 unsigned x = 0; // The fallback parser for this still decides wrong.
    * >> unsigned int128 x = 0; // The fallback parser will ignore the 'unsigned' in this.
    */
   if (!recognized_type) goto parse_prefix;
  }
  /* Continue parsing with the type  */
  goto got_prefix;
 }
end:
 DCCType_Quit(&base);
 DCCAttrDecl_Quit(&attr);
 return result;
}


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_DECL_C_INL */
