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
#ifndef GUARD_DCC_LEXER_ATTRIBUTES_C_INL
#define GUARD_DCC_LEXER_ATTRIBUTES_C_INL 1

#include <dcc/lexer.h>
#include <dcc/unit.h>

#include "lexer-priv.h"

#include <string.h>

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

DCC_DECL_BEGIN

PUBLIC void
DCCAttrDecl_Quit(struct DCCAttrDecl *__restrict self) {
 assert(self);
 if (self->a_alias) DCCSym_Decref(self->a_alias);
 if (self->a_reach) TPPString_Decref(self->a_reach);
}


#define CCNAME_SHIFT 20
PRIVATE char const *const cc_names[5] = {
 /* [DCC_ATTRFLAG_CC_CDECL    >> CCNAME_SHIFT] = */"cdecl",
 /* [DCC_ATTRFLAG_CC_STDCALL  >> CCNAME_SHIFT] = */"stdcall",
 /* [DCC_ATTRFLAG_CC_THISCALL >> CCNAME_SHIFT] = */"thiscall",
 /* [DCC_ATTRFLAG_CC_FASTCALL >> CCNAME_SHIFT] = */"fastcall",
};

#define ELFVISNAME_SHIFT   28
PRIVATE char const *const elfvis_names[5] = {
 NULL,
 /* [DCC_ATTRFLAG_VIS_DEFAULT   >> ELFVISNAME_SHIFT] = */"default",
 /* [DCC_ATTRFLAG_VIS_HIDDEN    >> ELFVISNAME_SHIFT] = */"hidden",
 /* [DCC_ATTRFLAG_VIS_PROTECTED >> ELFVISNAME_SHIFT] = */"protected",
 /* [DCC_ATTRFLAG_VIS_INTERNAL  >> ELFVISNAME_SHIFT] = */"internal",
};


PUBLIC void
DCCAttrDecl_Merge(struct DCCAttrDecl *__restrict self,
                  struct DCCAttrDecl const *__restrict rhs) {
 assert(self);
 assert(rhs);
 assert(self != rhs);
 self->a_specs |= rhs->a_specs;
 /* If no alignment was set, inherit that from the right. */
 if (!(self->a_specs&DCC_ATTRSPEC_FIXEDALIGN) &&
     !(self->a_alias)) self->a_align = rhs->a_align;
 /* TODO: Do more strict checking when comparing old against new attributes. */
 self->a_flags |= rhs->a_flags&(DCC_ATTRFLAG_MASK_MODE
                               |DCC_ATTRFLAG_MASK_REACHABLE
                               |DCC_ATTRFLAG_MASK_WARNING
#ifdef DCC_ATTRFLAG_DLLIMPORT
                               |DCC_ATTRFLAG_DLLIMPORT
#endif
#ifdef DCC_ATTRFLAG_DLLEXPORT
                               |DCC_ATTRFLAG_DLLEXPORT
#endif
                                );
 if ((rhs->a_flags&DCC_ATTRFLAG_MASK_CALLCONV) &&
     (rhs->a_flags&DCC_ATTRFLAG_MASK_CALLCONV) !=
     (self->a_flags&DCC_ATTRFLAG_MASK_CALLCONV)) {
  if (self->a_flags&DCC_ATTRFLAG_MASK_CALLCONV) {
   /* Warn about miss-matched calling conventions. */
   WARN(W_ATTRIBUTE_MERGE_CALLCONV,
        cc_names[(rhs->a_flags&DCC_ATTRFLAG_MASK_CALLCONV) >> CCNAME_SHIFT],
        cc_names[(self->a_flags&DCC_ATTRFLAG_MASK_CALLCONV) >> CCNAME_SHIFT]);
   self->a_flags &= ~(DCC_ATTRFLAG_MASK_CALLCONV);
  }
  self->a_flags |= rhs->a_flags&DCC_ATTRFLAG_MASK_CALLCONV;
 }
 if ((rhs->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY) &&
     (rhs->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY) !=
     (self->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY)) {
  /* Warn about miss-matched ELF visibility. */
  WARN(W_ATTRIBUTE_MERGE_VISIBILITY,
       elfvis_names[(rhs->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY) >> ELFVISNAME_SHIFT],
       elfvis_names[(self->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY) >> ELFVISNAME_SHIFT]);
  if ((rhs->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY) >
      (self->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY)) {
   self->a_flags &= ~(DCC_ATTRFLAG_MASK_ELFVISIBILITY);
   self->a_flags |= (rhs->a_flags&DCC_ATTRFLAG_MASK_ELFVISIBILITY);
  }
 }
 if (rhs->a_reach) {
  if (!self->a_reach) {
   self->a_reach = rhs->a_reach;
   TPPString_Decref(self->a_reach);
  } else if (self->a_reach->s_size != rhs->a_reach->s_size ||
            !memcmp(self->a_reach->s_text,rhs->a_reach->s_text,
                    self->a_reach->s_size*sizeof(char))) {
   WARN(W_ATTRIBUTE_MERGE_REACHMSG,
        self->a_reach->s_text,
        rhs->a_reach->s_text);
  }
 }
 if (rhs->a_specs&DCC_ATTRSPEC_CONSTRUCTOR) self->a_c_prio = rhs->a_c_prio;
 if (rhs->a_specs&DCC_ATTRSPEC_DESTRUCTOR)  self->a_d_prio = rhs->a_d_prio;
 /* If the right has a lower alignment that 'self', inherit it. */
 if (rhs->a_specs&DCC_ATTRSPEC_FIXEDALIGN) {
  if (DCCATTRDECL_HASALIAS(self)) self->a_specs &= ~(DCC_ATTRSPEC_FIXEDALIGN);
  else if (self->a_align > rhs->a_align)
           self->a_align = rhs->a_align;
 }
}
PUBLIC void
DCCAttrDecl_InitCopy(struct DCCAttrDecl *__restrict self,
                     struct DCCAttrDecl const *__restrict rhs) {
 assert(self);
 assert(rhs);
 assert(self != rhs);
 *self = *rhs;
 if (self->a_alias) DCCSym_Incref(self->a_alias);
 if (self->a_reach) TPPString_Incref(self->a_reach);
}


PUBLIC int
DCCAttrDecl_IsEmpty(struct DCCAttrDecl const *__restrict self) {
 uint8_t *iter,*end;
 assert(self);
 end = (iter = (uint8_t *)self)+sizeof(struct DCCAttrDecl);
 for (; iter != end; ++iter) if (*iter) return 0;
 return 1;
}



LEXPRIV void DCC_PARSE_CALL
DCCParse_AttrContent(struct DCCAttrDecl *__restrict self, int kind) {
 struct TPPKeyword *function; assert(self);
 uint32_t flag,mask;
 (void)kind;
 function = tok_without_underscores();
 if unlikely(!function) goto fallback;
 switch (function->k_id) {
 {
  int delete_flag,isspec; uint32_t *p;
#define SPECF(f)  (isspec = 1,mask =     flag = (f))
#define SPEC(m,f) (isspec = 1,mask = (m),flag = (f))
#define FLAGF(f)  (isspec = 0,mask =     flag = (f))
#define FLAG(m,f) (isspec = 0,mask = (m),flag = (f))
  if (DCC_MACRO_FALSE) { case KWD_noinline:                SPECF(DCC_ATTRSPEC_NOINLINE); }
  if (DCC_MACRO_FALSE) { case KWD_noreturn:                SPECF(DCC_ATTRSPEC_NORETURN); }
  if (DCC_MACRO_FALSE) { case KWD_warn_unused_result:      SPECF(DCC_ATTRSPEC_WUNUSED); }
  if (DCC_MACRO_FALSE) { case KWD_weak:case KWD_selectany: SPECF(DCC_ATTRSPEC_WEAK); }
  if (DCC_MACRO_FALSE) { case KWD_used:                    SPECF(DCC_ATTRSPEC_USED); }
  if (DCC_MACRO_FALSE) { case KWD_unused:                  SPECF(DCC_ATTRSPEC_UNUSED); }
  if (DCC_MACRO_FALSE) { case KWD_dllexport:               SPECF(DCC_ATTRSPEC_DLLEXPORT); }
  if (DCC_MACRO_FALSE) { case KWD_dllimport:               SPECF(DCC_ATTRSPEC_DLLIMPORT); }
  if (DCC_MACRO_FALSE) { case KWD_naked:                   SPECF(DCC_ATTRSPEC_NAKED); }
  if (DCC_MACRO_FALSE) { case KWD_packed:                  SPECF(DCC_ATTRSPEC_PACKED); }
  if (DCC_MACRO_FALSE) { case KWD_transparent_union:       SPECF(DCC_ATTRSPEC_TRANSUNION); }
  if (DCC_MACRO_FALSE) { case KWD_ms_struct:               SPECF(DCC_ATTRSPEC_MSSTRUCT); }
  if (DCC_MACRO_FALSE) { case KWD_arithmetic:              SPECF(DCC_ATTRSPEC_ARITHMETIC); }
  if (DCC_MACRO_FALSE) { case KWD_gcc_struct:              SPEC(DCC_ATTRSPEC_MSSTRUCT,0); }
  if (DCC_MACRO_FALSE) { case KWD_cdecl:                   FLAG(DCC_ATTRFLAG_MASK_CALLCONV,DCC_ATTRFLAG_CC_CDECL); }
  if (DCC_MACRO_FALSE) { case KWD_stdcall:                 FLAG(DCC_ATTRFLAG_MASK_CALLCONV,DCC_ATTRFLAG_CC_STDCALL); }
  if (DCC_MACRO_FALSE) { case KWD_thiscall:                FLAG(DCC_ATTRFLAG_MASK_CALLCONV,DCC_ATTRFLAG_CC_THISCALL); }
  if (DCC_MACRO_FALSE) { case KWD_fastcall:                FLAG(DCC_ATTRFLAG_MASK_CALLCONV,DCC_ATTRFLAG_CC_FASTCALL); }
  if (DCC_MACRO_FALSE) { case KWD_force_align_arg_pointer: SPEC(0,0); } /* TODO */
  delete_flag = 0; YIELD();
  if (HAS(EXT_ATTRIBUTE_CONDITION) && (TOK == '(' || TOK == KWD___pack)) {
   int_t val;
   DCCParse_ParPairBegin();
   val = DCCParse_CExpr(0);
   DCCParse_ParPairEnd();
   delete_flag = !val; /* Don't set the flag, based on a constant expression. */
  }
#if DCC_TARGET_BIN != DCC_BINARY_PE
  if (isspec && (flag == DCC_ATTRSPEC_DLLIMPORT ||
                 flag == DCC_ATTRSPEC_DLLEXPORT)) {
   self->a_flags &= ~(DCC_ATTRFLAG_MASK_ELFVISIBILITY);
   self->a_flags |= (delete_flag
                     ? DCC_ATTRFLAG_VIS_HIDDEN
                     : DCC_ATTRFLAG_VIS_NONE);
   break;
  }
#endif
  p = isspec ? &self->a_specs : &self->a_flags;
  if (delete_flag) {
   if ((*p&mask) != flag) WARN(W_ATTRIBUTE_NOT_DEFINED,function);
   *p &= ~(mask);
  } else {
   if ((*p&mask) == flag && (mask || flag)) WARN(W_ATTRIBUTE_ALREADY_DEFINED,function);
   *p &= ~(mask);
   *p |=   flag;
  }
 } break;

 case KWD_weakref:
  self->a_specs |= DCC_ATTRSPEC_WEAK;
  YIELD();
  if (TOK == '(' || TOK == KWD___pack) goto parse_alias;
  break;

 {
 case KWD_regparm:
 case KWD_aligned:
  YIELD();
  DCCParse_ParPairBegin();
  if (function->k_id == KWD_aligned) {
   if (DCCATTRDECL_HASALIAS(self)) {
    WARN(W_ATTRIBUTE_ALIGNED_WITH_ALIAS);
    assert(!(self->a_specs&DCC_ATTRSPEC_SECTION));
    DCCSym_Decref(self->a_alias);
    self->a_alias = NULL;
   }
   self->a_align  = (target_siz_t)DCCParse_CExpr(0);
   self->a_specs |= DCC_ATTRSPEC_FIXEDALIGN;
   if (self->a_align&(self->a_align-1))
       WARN(W_ATTRIBUTE_ALIGNED_EXPECTED_POWER_OF_TWO,self->a_align);
  } else {
   self->a_regparm = (uint8_t)DCCParse_CExpr(0);
  }
  DCCParse_ParPairEnd();
 } break;

 {
  if (DCC_MACRO_FALSE) { case KWD_constructor: flag = DCC_ATTRSPEC_CONSTRUCTOR; }
  if (DCC_MACRO_FALSE) { case KWD_destructor:  flag = DCC_ATTRSPEC_DESTRUCTOR; }
  YIELD();
  if (self->a_specs&flag) WARN(W_ATTRIBUTE_ALREADY_DEFINED,function);
  self->a_specs |= flag;
  if (TOK == '(' || TOK == KWD___pack) {
   DCCParse_ParPairBegin();
   if (flag == DCC_ATTRSPEC_CONSTRUCTOR)
        self->a_c_prio = (int)DCCParse_CExpr(0);
   else self->a_d_prio = (int)DCCParse_CExpr(0);
   DCCParse_ParPairEnd();
  }
 } break;


 {
  struct TPPString *text;
  int has_paren;
 case KWD_deprecated:
 case KWD_warning:
 case KWD_error:
  YIELD();
  if (TOK == '(' || TOK == KWD___pack) goto parse_alias;
  /* Empty string. */
  text = TPPFile_Empty.f_text;
  TPPString_Incref(TPPFile_Empty.f_text);
  has_paren = 0;
  goto set_text;

 case KWD_visibility:
 case KWD_alias:
 case KWD_section:
 case KWD_lib:
  YIELD();
parse_alias:
  has_paren = TOK == '(';
  if (has_paren) YIELD();
  else if (TOK == KWD___pack)  {
   YIELD();
   has_paren = (TOK == '(');
   if (has_paren) YIELD();
  } else has_paren = 1,WARN(W_EXPECTED_LPAREN);
  if (TOK == ')') text = NULL;
  else if (!TPP_ISSTRING(TOK)) {
   WARN(W_ATTRIBUTE_EXPECTED_STRING,function);
   text = NULL;
  } else text = DCCParse_String();
set_text:
  switch (function->k_id) {

  { /* Specify a message and context emit when the symbol is reached. */
   if (DCC_MACRO_FALSE) { case KWD_deprecated: flag = DCC_ATTRFLAG_REACH_DEPR; }
   if (DCC_MACRO_FALSE) { case KWD_warning:    flag = DCC_ATTRFLAG_REACH_WARN; }
   if (DCC_MACRO_FALSE) { case KWD_error:      flag = DCC_ATTRFLAG_REACH_ERROR; }
   if (self->a_reach) {
    if (text) WARN(W_ATTRIBUTE_DEPRECATED_ALREADY_DEFINED);
    TPPString_Decref(self->a_reach);
   }
   self->a_reach  = text;
   self->a_flags &= ~(DCC_ATTRFLAG_MASK_REACHABLE);
   if (text) self->a_flags |= flag;
   goto done_paren;
  } break;

  { /* Select a custom section. */
  case KWD_section:
   if (!text) self->a_section = NULL;
   else {
    struct TPPKeyword *sec_name = TPPLexer_LookupKeyword(text->s_text,text->s_size,0);
    if (self->a_section) {
     if (self->a_specs&DCC_ATTRSPEC_SECTION) {
      WARN(W_ATTRIBUTE_SECTION_ALREADY_SET,
           self->a_section->sc_start.sy_name);
      self->a_specs &= ~(DCC_ATTRSPEC_SECTION);
     } else WARN(W_ATTRIBUTE_ALIAS_WITH_SECTION);
     DCCSym_Decref(self->a_alias);
    }
    assert(!(self->a_specs&DCC_ATTRSPEC_SECTION));
    self->a_section = sec_name ? DCCUnit_GetSec(sec_name) : NULL;
    if (!self->a_section || DCCSection_ISIMPORT(self->a_section)) {
     WARN(W_ATTRIBUTE_SECTION_UNKNOWN_SECTION,text->s_text);
     self->a_section = NULL;
    } else {
incref_section:
     self->a_specs |= DCC_ATTRSPEC_SECTION;
     DCCSection_Incref(self->a_section);
    }
   }
  } break;

  { /* Explicitly define the origin of a library dependency. */
  case KWD_lib:
   if (text) {
    struct TPPKeyword *sec_name;
    if (self->a_section) {
     if (self->a_specs&DCC_ATTRSPEC_SECTION) {
      WARN(W_ATTRIBUTE_LIB_ALREADY_SET,
           self->a_section->sc_start.sy_name);
      self->a_specs &= ~(DCC_ATTRSPEC_SECTION);
     } else WARN(W_ATTRIBUTE_ALIAS_WITH_DLL);
     DCCSym_Decref(self->a_alias);
    }
    assert(!(self->a_specs&DCC_ATTRSPEC_SECTION));
    sec_name = DCCParse_GetLibname(text->s_text,text->s_size);
    self->a_section = sec_name ? DCCUnit_NewSec(sec_name,DCC_SYMFLAG_SEC_ISIMPORT) : NULL;
    if (!self->a_section);
    else if (!DCCSection_ISIMPORT(self->a_section)) {
     WARN(W_ATTRIBUTE_LIB_IS_A_SECTION,text->s_text);
     self->a_section = NULL;
    } else {
     goto incref_section;
    }
   }
  } break;

  { /* Define a symbol aliasing another. */
  case KWD_alias:
  case KWD_weakref:
   if (self->a_specs&DCC_ATTRSPEC_FIXEDALIGN) {
    WARN(W_ATTRIBUTE_ALIGNED_WITH_ALIAS);
    self->a_specs &= ~(DCC_ATTRSPEC_FIXEDALIGN);
   }
   if (self->a_specs&DCC_ATTRSPEC_SECTION) {
    assert(self->a_section);
    if (DCCSection_ISIMPORT(self->a_section))
         WARN(W_ATTRIBUTE_ALIAS_WITH_DLL);
    else WARN(W_ATTRIBUTE_ALIAS_WITH_SECTION);
    DCCSection_Decref(self->a_section);
    self->a_specs &= ~(DCC_ATTRSPEC_SECTION);
   } else if (self->a_alias) {
    WARN(W_ATTRIBUTE_ALIAS_ALREADY_DEFINED,
         self->a_alias->sy_name);
    DCCSym_Decref(self->a_alias);
   }
   if (!text || (function = TPPLexer_LookupKeyword(text->s_text,text->s_size,1)) == NULL)
    self->a_alias = NULL;
   else {
    struct DCCSym *alias;
    alias = DCCUnit_NewSym(function,DCC_SYMFLAG_NONE);
    DCCSym_XIncref(alias);
    self->a_alias = alias; /* Inherit reference. */
   }
   assert(!(self->a_specs&DCC_ATTRSPEC_SECTION));
   self->a_offset = 0;
   if (TOK == ',') {
    /* Extension: Alias offset. */
    WARN(W_ATTRIBUTE_ALIAS_OFFSET_EXTENSION);
    YIELD();
    self->a_offset = (target_off_t)DCCParse_CExpr(1);
   }
  } break;

  { /* Set visibility. */
   symflag_t new_visibility;
  case KWD_visibility:
   if (!text) {
    new_visibility = DCC_ATTRFLAG_VIS_NONE;
    goto set_visibility;
   } else {
#define CHECK(s) \
    (sizeof(s)/sizeof(char)-1 == text->s_size && \
    !memcmp(s,text->s_text,sizeof(s)-sizeof(char)))
         if (CHECK("default"))   new_visibility = DCC_ATTRFLAG_VIS_DEFAULT;
    else if (CHECK("hidden"))    new_visibility = DCC_ATTRFLAG_VIS_HIDDEN;
    else if (CHECK("protected")) new_visibility = DCC_ATTRFLAG_VIS_PROTECTED;
    else if (CHECK("internal"))  new_visibility = DCC_ATTRFLAG_VIS_INTERNAL;
    else { WARN(W_ATTRIBUTE_VISIBILITY_UNKNOWN_VISIBILITY,text->s_text); goto cleanup_text; }
#undef CHECK
set_visibility :
    self->a_flags &= ~(DCC_ATTRFLAG_MASK_ELFVISIBILITY);
    self->a_flags |= new_visibility;
   }
   self->a_flags &= ~(DCC_ATTRFLAG_MASK_ELFVISIBILITY);
   self->a_flags |= new_visibility;
  } break;

  default: break;
  }
cleanup_text:
  if (text) TPPString_Decref(text);
done_paren:
  if (has_paren) {
   if (TOK == ')') YIELD();
   else WARN(W_EXPECTED_RPAREN);
  }
 } break;

 {
  struct TPPKeyword *mode_name;
 case KWD_mode:
  YIELD();
  DCCParse_ParPairBegin();
  mode_name = tok_without_underscores();
  if (!mode_name) WARN(W_ATTRIBUTE_MODE_EXPECTED_KEYWORD);
  else {
   uint32_t mode_flag = 0;
   if (self->a_flags&DCC_ATTRFLAG_MASK_MODE) {
    WARN(W_ATTRIBUTE_MODE_ALREADY_DEFINED);
    self->a_flags &= ~(DCC_ATTRFLAG_MASK_MODE);
   }
   switch (mode_name->k_id) {
    case KWD_QI: mode_flag = DCC_ATTRFLAG_MODE_QI; break;
    case KWD_HI: mode_flag = DCC_ATTRFLAG_MODE_HI; break;
    case KWD_SI: mode_flag = DCC_ATTRFLAG_MODE_SI; break;
    case KWD_DI: mode_flag = DCC_ATTRFLAG_MODE_DI; break;
    case KWD_SF: mode_flag = DCC_ATTRFLAG_MODE_SF; break;
    case KWD_DF: mode_flag = DCC_ATTRFLAG_MODE_DF; break;
    default    : WARN(W_ATTRIBUTE_MODE_UNKNOWN_MODE); break;
   }
   self->a_flags |= mode_flag;
   YIELD();
  }
  DCCParse_ParPairEnd();
 } break;

 case KWD_returns_twice:
 case KWD_const:
 case KWD_pure:
 case KWD_cold:
 case KWD_hot:
 case KWD_nothrow:
 case KWD_noclone:
 case KWD_nonnull:
 case KWD_malloc:
 case KWD_leaf:
 case KWD_format_arg:
 case KWD_format:
 case KWD_externally_visible:
 case KWD_alloc_size:
 case KWD_always_inline:
 case KWD_gnu_inline:
 case KWD_artificial:
 case KWD_noalias:
 case KWD_restrict:
  /* Unsupported, but recognized warnings. */
  WARN(W_ATTRIBUTE_UNSUPPORTED);
  goto skip_attribute;

 default:fallback:
  WARN(W_ATTRIBUTE_UNKNOWN);
  if (TPP_ISKEYWORD(TOK)) {
skip_attribute:
   YIELD();
   if (TOK == '(') {
    unsigned int recursion = 1;
    while (TOK > 0) {
     YIELD();
          if (TOK == '(') ++recursion;
     else if (TOK == ')' && !--recursion) break;
    }
    YIELD();
   }
  }
  break;
 }
}

PUBLIC void DCC_PARSE_CALL
DCCParse_Attr(struct DCCAttrDecl *__restrict self) {
 assert(self);
again:
 switch (TOK) {
 case KWD_attribute:
  if (!HAS(EXT_SHORT_EXT_KEYWORDS) ||
      /* NOTE: Also keep the next token for being a '(',
       *       as variables named 'attribute' are actually
       *       something you might see in the wild! */
      peek_next_token(NULL)[0] != '(') break;
 case KWD___attribute:
 case KWD___attribute__:
  if (!HAS(EXT_GCC_ATTRIBUTE)) break;
  YIELD();
  DCCParse_ParPairBegin();
  DCCParse_ParPairBegin();
  if (TOK != ')') for (;;) {
   DCCParse_AttrContent(self,ATTRKIND_ATTRIBUTE);
   if (TOK != ',') break;
   YIELD();
  }
  DCCParse_ParPairEnd();
  DCCParse_ParPairEnd();
  goto again;

 case KWD__declspec:
 case KWD___declspec:
  if (!HAS(EXT_MSVC_ATTRIBUTE)) break;
  YIELD();
  DCCParse_ParPairBegin();
  if (TOK != ')') for (;;) {
   DCCParse_AttrContent(self,ATTRKIND_DECLSPEC);
   if (TOK != ',') break;
   YIELD();
  }
  DCCParse_ParPairEnd();
  goto again;

 {
  char *second_bracket;
  struct TPPFile *second_bracket_file;
 case '[':
  if (!HAS(EXT_CXX11_ATTRIBUTE)) break;
  /* Kind-of hacky, and not macro-compatible,
   * but check if the next token is a '['. */
  second_bracket = peek_next_token(&second_bracket_file);
  if (*second_bracket != '[') break;
  /* Parse the next token after this second bracket. */
  second_bracket_file->f_pos = second_bracket+1;
  assert(second_bracket_file->f_pos >= second_bracket_file->f_begin);
  assert(second_bracket_file->f_pos <= second_bracket_file->f_end);
  YIELD();
  if (TOK != ']') for (;;) {
   DCCParse_AttrContent(self,ATTRKIND_BRACKETS);
   if (TOK != ',') break;
   YIELD();
  }
  if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
  if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
  goto again;
 } break;

 case KWD__Noreturn:
  self->a_specs |= DCC_ATTRSPEC_NORETURN;
  goto yield_again;

 {
 case KWD__Alignas:
  YIELD();
  DCCParse_ParPairBegin();
  if (DCCATTRDECL_HASALIAS(self)) {
   WARN(W_ATTRIBUTE_ALIGNED_WITH_ALIAS);
   assert(!(self->a_specs&DCC_ATTRSPEC_SECTION));
   DCCSym_Decref(self->a_alias);
   self->a_alias = NULL;
  }
  self->a_align  = (target_siz_t)DCCParse_CExpr(0);
  self->a_specs |= DCC_ATTRSPEC_FIXEDALIGN;
  if (self->a_align&(self->a_align-1))
      WARN(W_ATTRIBUTE_ALIGNED_EXPECTED_POWER_OF_TWO,self->a_align);
  DCCParse_ParPairEnd();
 } goto again;

 {
  uint32_t new_cc;
  if (DCC_MACRO_FALSE) { case KWD__cdecl: case KWD___cdecl: new_cc = DCC_ATTRFLAG_CC_CDECL; }
  if (DCC_MACRO_FALSE) { case KWD__stdcall: case KWD___stdcall: new_cc = DCC_ATTRFLAG_CC_STDCALL; }
  if (DCC_MACRO_FALSE) { case KWD___thiscall: new_cc = DCC_ATTRFLAG_CC_THISCALL; }
  if (DCC_MACRO_FALSE) { case KWD__fastcall: case KWD___fastcall: new_cc = DCC_ATTRFLAG_CC_FASTCALL; }
  if (!HAS(EXT_CALLING_CONVENTION_ATTR)) break;
  self->a_flags &= ~(DCC_ATTRFLAG_MASK_CALLCONV);
  self->a_flags |= new_cc;
yield_again:
  YIELD();
  goto again;
 } break;

 default: break;
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_ATTRIBUTES_C_INL */
