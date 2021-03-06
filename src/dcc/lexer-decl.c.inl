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
#include <dcc/compiler.h>
#include <dcc/fundecl.h>
#include <dcc/lexer.h>
#include <dcc/type.h>
#include <dcc/unit.h>

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
   if (DCCParse_CTypeDeclBase(&follow_type,&follow_attr,
                               DCCPARSE_CTYPEDECLBASE_CTX_EXPR)) {
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

PUBLIC int DCC_PARSE_CALL
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

PUBLIC int DCC_PARSE_CALL DCCParse_LocalDecl(void) {
 struct DCCType base; int result;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 result = DCCParse_CTypeDeclBase(&base,&attr,DCCPARSE_CTYPEDECLBASE_CTX_EXPR);
 if (result) result = DCCParse_DeclWithBase(&base,&attr,1);
 DCCType_Quit(&base);
 DCCAttrDecl_Quit(&attr);
 return result;
}

PUBLIC int DCC_PARSE_CALL DCCParse_GlobalDecl(void) {
 struct DCCType base; int error;
 struct DCCAttrDecl attr = DCCATTRDECL_INIT;
 error = DCCParse_CTypeDeclBase(&base,&attr,DCCPARSE_CTYPEDECLBASE_CTX_GLOB);
 DCCType_ASSERT(&base);
 /* HINT: 'base' was already initialized to 'int'. */
 if (!error) WARN(W_EXPECTED_TYPE_FOR_GLOBAL_DECLARATION);
 /* HINT: 'DCCParse_DeclWithBase' never returns ZERO(0). */
 error = DCCParse_DeclWithBase(&base,&attr,error);
 DCCType_Quit(&base);
 DCCAttrDecl_Quit(&attr);
 return error;
}



DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_DECL_C_INL */
