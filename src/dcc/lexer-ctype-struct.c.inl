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
#ifndef GUARD_DCC_LEXER_CTYPE_STRUCT_C_INL
#define GUARD_DCC_LEXER_CTYPE_STRUCT_C_INL 1

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/unit.h>

#include "lexer-priv.h"

DCC_DECL_BEGIN

INTERN void
DCCDecl_CalculateStructureOffsets(struct DCCDecl *__restrict self) {
 int use_ms_alignment,is_union,pack_structure;
 target_off_t current_offset;
 target_siz_t max_alignment,s,a,max_size;
 unsigned int bitpos = 0; /* bit-position offset added to 'current_offset' */
 struct DCCAttrDecl *attr;
 struct DCCStructField *iter,*end;
 assert(self);
 assert(self->d_kind == DCC_DECLKIND_STRUCT ||
        self->d_kind == DCC_DECLKIND_UNION);
 /* Align all structure members, considering attributes from 'attr' */
 /* NOTE: Make sure to consider 'self->d_attr' */
 use_ms_alignment = 0;
 pack_structure = 0;
 max_alignment = 1;
 current_offset = 0;
 max_size = 0;
 is_union = self->d_kind == DCC_DECLKIND_UNION;
 if ((attr = self->d_attr) != NULL) {
  if (attr->a_specs&DCC_ATTRSPEC_PACKED) pack_structure = 1;
  if (attr->a_specs&DCC_ATTRSPEC_MSSTRUCT) use_ms_alignment = 1;
  if (attr->a_specs&DCC_ATTRSPEC_FIXEDALIGN) max_alignment = attr->a_align;
 }
 end = (iter = self->d_tdecl.td_fieldv)+self->d_tdecl.td_size;
 (void)use_ms_alignment; /* TODO: Special ms algorithm. */
 for (; iter != end; ++iter) {
  assert(iter->sf_decl);
  attr = iter->sf_decl->d_attr;
  s = DCCType_Sizeof(&iter->sf_decl->d_type,&a,0);
  if (iter->sf_bitfld != (sflag_t)-1) {
   target_siz_t ceil_offset;
   sflag_t field_size = iter->sf_bitfld;
   /* Make bitfields in unions are always located at the beginning. */
   if (is_union) current_offset = 0,bitpos = 0;
   /* Warn if the bit-field is larger than the underlying type. */
   if (field_size > s*DCC_TARGET_BITPERBYTE) WARN(W_TYPE_STRUCT_BITFIELD_TOO_LARGE,iter->sf_decl);
   /* Special case: No need for a bit-field, everything is already aligning. */
   if (field_size == s*DCC_TARGET_BITPERBYTE && !bitpos) goto no_bitfield;
#define MAX_BITFIELD_SIZE   (DCC_SFLAG_BITSIZ_MASK >> DCC_SFLAG_BITSIZ_SHIFT)
   if (field_size > MAX_BITFIELD_SIZE) {
    WARN(W_TYPE_STRUCT_BITFIELD_TOO_LARGE,iter->sf_decl);
    field_size = MAX_BITFIELD_SIZE; /* This is an actual implementation limit! */
   }
#undef MAX_BITFIELD_SIZE
   iter->sf_off    = current_offset;
   /* Generate the bitfield flags. */
   iter->sf_bitfld = DCC_SFLAG_MKBITFLD(bitpos,field_size);
   bitpos         += field_size;
   current_offset += (bitpos/DCC_TARGET_BITPERBYTE);
   bitpos         %= DCC_TARGET_BITPERBYTE;
   ceil_offset     = (target_siz_t)current_offset;
   if (bitpos) ++ceil_offset;
   /* Must use the ceiling of the current offset when checking against max_size. */
   if (max_size < ceil_offset)
       max_size = ceil_offset;
   goto check_align;
  } else {
   if (bitpos) {
    /* If the last field was a non-empty bit-field,
     * advance the structure pointer to prevent overlap. */
    bitpos = 0;
    ++current_offset;
   }
no_bitfield:
   iter->sf_bitfld = 0;
  }
  /* Allow per-field alignment override. */
  if (attr && (attr->a_specs&DCC_ATTRSPEC_FIXEDALIGN)) {
   if (a < attr->a_align) a = attr->a_align;
  } else if (pack_structure || (attr && attr->a_specs&DCC_ATTRSPEC_PACKED)) a = 1;
  else if (compiler.c_pack.ps_pack &&
           compiler.c_pack.ps_pack < a) a = compiler.c_pack.ps_pack;
  /* Force ZERO-offsets in unions, thus
   * allocating all fields in the same position. */
  if (is_union) current_offset = 0;
  else current_offset = (current_offset+(a-1))&~(a-1);
  iter->sf_off = current_offset;
  current_offset += s;
  if (max_size < (target_siz_t)current_offset)
      max_size = (target_siz_t)current_offset;
check_align:
  if (max_alignment < a)
      max_alignment = a;
 }
 if (self->d_attr &&
    (self->d_attr->a_specs&DCC_ATTRSPEC_FIXEDALIGN) &&
     max_alignment > self->d_attr->a_align) {
  WARN(W_TYPE_STRUCT_EXPLICIT_ALIGNMENT_TOO_LOW,
       self,self->d_attr->a_align,max_alignment);
 }
 self->d_tdecl.td_struct_size  = max_size;
 self->d_tdecl.td_struct_align = max_size ? max_alignment : 0;
}


PUBLIC void DCC_PARSE_CALL
DCCParse_Struct(struct DCCDecl *__restrict struct_decl) {
 size_t fieldc,fielda,new_fielda;
 struct DCCStructField *fieldv,*new_fieldv;
 struct DCCType base,part;
 struct DCCAttrDecl attr;
 struct TPPKeyword *field_name;
 struct DCCDecl *field_decl;
 assert(struct_decl);
 assert(struct_decl->d_kind == DCC_DECLKIND_STRUCT ||
        struct_decl->d_kind == DCC_DECLKIND_UNION);
 assert(struct_decl->d_tdecl.td_fieldv == NULL);
 fieldc = fielda = 0,fieldv = NULL;
 for (;;) {
  memset(&attr,0,sizeof(attr));
  if (!DCCParse_CTypeDeclBase(&base,&attr,DCCPARSE_CTYPEDECLBASE_CTX_TYPE)) break;
  for (;;) {
   sflag_t bitfield = (sflag_t)-1;
   DCCType_InitCopy(&part,&base);
   field_name = DCCParse_CTypeSuffix(&part,&attr);
   assert(field_name);
   if (fieldc == fielda) {
    new_fielda = fielda ? fielda*2 : 2;
    new_fieldv = (struct DCCStructField *)realloc(fieldv,new_fielda*
                                                  sizeof(struct DCCStructField));
    if unlikely(!new_fieldv) {seterr: TPPLexer_SetErr(); goto next_field; }
    fielda = new_fielda;
    fieldv = new_fieldv;
   }
   /* TODO: Warn if 'field_name' was already used in this structure.
    * NOTE: Must check unnamed structures recursively for this! */
   field_decl = DCCDecl_New(field_name);
   if unlikely(!field_decl) goto seterr;
   field_decl->d_kind = DCC_DECLKIND_TYPE;
   field_decl->d_type = part,part.t_base = NULL; /* Inherit reference. */
   DCCDecl_SetAttr(field_decl,&attr);
   if (TOK == ':') {
    int_t field_size;
    /* Bit-field declaration. */
    YIELD();
    field_size = DCCParse_CExpr(1);
    if (field_size < 0) WARN(W_TYPE_STRUCT_BITFIELD_NEGATIVE,field_decl);
    else bitfield = (sflag_t)field_size;
    /* Check the field type for being a scalar. */
    if (!DCCType_IsScalar(&field_decl->d_type))
        WARN(W_TYPE_STRUCT_BITFIELD_SCALAR,field_decl);
    /* TODO: Didn't STD-C only allow 'unsigned int/int' for bitfields?
     *    >> If so, emit an extension-warning if something else is used! */
   }
   fieldv[fieldc].sf_decl   = field_decl;
   fieldv[fieldc].sf_bitfld = bitfield;
   ++fieldc;
next_field:
   DCCType_Quit(&part);
   if (TOK != ',') break;
   YIELD();
   DCCParse_Attr(&attr);
  }
  DCCType_Quit(&base);
  DCCAttrDecl_Quit(&attr);
  if (TOK != ';') WARN(W_EXPECTED_SEMICOLON); else YIELD();
 }
 DCCAttrDecl_Quit(&attr);
 if (fieldc != fielda) {
  new_fieldv = (struct DCCStructField *)realloc(fieldv,fieldc*
                                                sizeof(struct DCCStructField));
  if (new_fieldv) fieldv = new_fieldv;
 }
 assert((fieldc != 0) == (fieldv != NULL));
 struct_decl->d_tdecl.td_size   = fieldc;
 struct_decl->d_tdecl.td_fieldv = fieldv; /* Inherit data. */
 /* Warn about empty structures. */
 if (!fieldc) WARN(W_TYPE_STRUCT_EMPTY,struct_decl);
}



PUBLIC void DCC_PARSE_CALL
DCCParse_Enum(void) {
 struct TPPKeyword *cst_name;
 struct DCCDecl *constant_decl;
 struct DCCSymExpr next_val;
 int done = 0;
 next_val.e_sym = NULL;
 next_val.e_int = 0;
 while (!done) {
  struct DCCAttrDecl attr = DCCATTRDECL_INIT;
  DCCParse_Attr(&attr);
  cst_name = NULL;
  if (TPP_ISKEYWORD(TOK)) {
   cst_name = TOKEN.t_kwd;
   YIELD();
   DCCParse_Attr(&attr);
  }
  if (TOK == '=') {
   YIELD();
   /* NOTE: We actually allow enum constants to depend on symbols. */
   DCCParse_CExpr2(1,&next_val);
   DCCParse_Attr(&attr);
  }
  if (cst_name) {
   /* Declare a constant symbol. */
   constant_decl = DCCCompiler_NewLocalDecl(cst_name,DCC_NS_LOCALS);
   if (constant_decl) {
    if (constant_decl->d_kind != DCC_DECLKIND_NONE) {
     WARN(W_DECL_ALREADY_DEFINED,constant_decl);
    } else {
     DCCDecl_SetAttr(constant_decl,&attr);
     /* TODO: clamp to int? (Shouldn't always be done; also: enum classes?) */
     assert(constant_decl->d_mdecl.md_loc.ml_reg == DCC_RC_CONST);
     constant_decl->d_kind                = DCC_DECLKIND_MREF;
     constant_decl->d_mdecl.md_loc.ml_off = (target_off_t)next_val.e_int;
     constant_decl->d_mdecl.md_loc.ml_sym = next_val.e_sym;
     DCCSym_XIncref(next_val.e_sym); /* Create reference. */
    }
    ++next_val.e_int;
   }
  }
  DCCAttrDecl_Quit(&attr);
  if (TOK != ',') break;
  YIELD();
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_CTYPE_STRUCT_C_INL */
