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
#ifndef GUARD_DCC_LEXER_INIT_C_INL
#define GUARD_DCC_LEXER_INIT_C_INL 1

#include <dcc/common.h>
#include <dcc/vstack.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/gen.h>

#include "lexer-priv.h"

#include <string.h>

DCC_DECL_BEGIN

PRIVATE void DCC_PARSE_CALL
push_target(struct DCCType const *__restrict type,
            struct DCCMemLoc const *__restrict target) {
 struct DCCStackValue sval;
 assert(type);
 assert(target);
 sval.sv_ctype    = *type;
 sval.sv_flags    = DCC_SFLAG_LVALUE;
 sval.sv_reg2     = DCC_RC_CONST;
 sval.sv_const.it = (int_t)target->ml_off;
 sval.sv_sym      = target->ml_sym;
 sval.sv_reg      = target->ml_reg;
 vpush(&sval);
}

PUBLIC void DCC_PARSE_CALL
DCCParse_Init(struct DCCType const *__restrict type,
              struct DCCAttrDecl const *attr,
              struct DCCMemLoc const *target,
              uint32_t flags) {
 int has_brace;
 assert(type);
 if (TOK == '{') {
#define KIND_STRUCT  0
#define KIND_UNION   1
#define KIND_ARRAY   2
#define KIND_VARRAY  3
  int kind;
  struct DCCStructField *field_curr,*field_end;
  struct DCCMemLoc base_target,elem_target;
  /* - Current index within an array initializer. */
  target_ptr_t target_maxindex,target_index;
  target_ptr_t elem_size,elem_align;
  has_brace       = 1;
  goto parse_braceblock;
parse_braceblock:
  target_maxindex = 0;
  target_index    = 0;
  elem_size       = 0;
  elem_align      = 0;
  field_curr      =
  field_end       = NULL;
  /* Warn about initializer-assignment to constant target. */
  if (!(flags&DCCPARSE_INITFLAG_INITIAL) &&
        type->t_type&DCCTYPE_CONST)
        WARN(W_ASSIGN_INIT_CONSTANT_TYPE,type);
  switch (DCCTYPE_GROUP(type->t_type)) {

  case DCCTYPE_LVALUE:
   /* L-value initializer. */
   assert(type->t_base);
   if (flags&DCCPARSE_INITFLAG_INITIAL) {
    WARN(W_BRACE_INITIALIZER_FOR_LVALUE_TYPE,type);
lvalue_initial:
    /* Fallback: Generate an automatic target and assign it to the current. */
    DCCParse_Init(&type->t_base->d_type,NULL,NULL,flags);
    DCCParse_FixType(&type->t_base->d_type);
    if (!target) {
     /* Explicitly cast to l-value type to generate indirection. */
     vcast(type,1);
    } else {
     push_target(type,target);
     vswap();   /* target, init */
     vstore(1); /* target=init */
    }
   } else {
    struct DCCMemLoc lv_target;
    if (!target) {
     WARN(W_BRACE_INITIALIZER_FOR_LVALUE_TYPE_NOTARGET,type);
     goto lvalue_initial;
    }
    /* Point the target at the pointed-to value. */
    lv_target.ml_reg = DCCVStack_GetReg(DCC_RC_PTR,1);
    lv_target.ml_off = 0;
    lv_target.ml_sym = NULL;
    DCCDisp_LeaReg(target,lv_target.ml_reg);
    /* Recursively compile an initialize for the l-value target. */
    DCCParse_Init(&type->t_base->d_type,NULL,&lv_target,flags);
    DCCParse_FixType(&type->t_base->d_type);
   }
   return;

  case DCCTYPE_STRUCTURE:
   assert(type->t_base);
   assert(type->t_base->d_kind == DCC_DECLKIND_STRUCT ||
          type->t_base->d_kind == DCC_DECLKIND_UNION);
   field_curr = type->t_base->d_tdecl.td_fieldv;
   field_end = field_curr+type->t_base->d_tdecl.td_size;
   kind = type->t_base->d_kind == DCC_DECLKIND_STRUCT ? KIND_STRUCT : KIND_UNION;
   break;

  case DCCTYPE_ARRAY:
   assert(type->t_base);
   assert(type->t_base->d_kind == DCC_DECLKIND_ARRAY ||
          type->t_base->d_kind == DCC_DECLKIND_VLA);
   /* VLA array types can't have initializers. */
   if (type->t_base->d_kind == DCC_DECLKIND_VLA) {
    WARN(W_BRACE_INITIALIZER_FOR_VLA_TYPE,type);
    if (has_brace) YIELD();
    pushf();
    compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
    for (;;) {
     DCCParse_Init(type,attr,target,flags|DCCPARSE_INITFLAG_INBRACE);
     vpop(1);
     if (TOK != ',' || !has_brace) break;
     YIELD();
    }
    popf();
    vpushv();
    goto end_brace;
   }
   kind            = KIND_ARRAY;
   target_maxindex = type->t_base->d_tdecl.td_size;
   if (DCC_MACRO_FALSE) { case DCCTYPE_VARRAY: kind = KIND_VARRAY; }
   elem_size       = DCCType_Sizeof(&type->t_base->d_type,&elem_align,1);
   break;

  default:
   WARN(W_BRACE_INITIALIZER_FOR_DEFAULT_TYPE,type);
   if (has_brace) YIELD();
   for (;;) {
    DCCParse_Init(type,attr,target,flags|DCCPARSE_INITFLAG_INBRACE);
    if (TOK != ',' || !has_brace) break;
    vpop(1);
    YIELD();
   }
   goto end_brace;
  }
  /* Determine initialization target. */
  if (target) {
   base_target = *target;
  } else {
   target_ptr_t type_s,type_a;
   type_s = DCCType_Sizeof(type,&type_a,1);
   if (!compiler.c_fun ||
      (type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_EXTERN ||
      (type->t_type&DCCTYPE_STOREBASE) == DCCTYPE_STATIC ||
      (compiler.c_flags&DCC_COMPILER_FLAG_SINIT)) {
    struct DCCSection *target_section;
    /* Global storage duration. */
    target_section = attr ? DCCATTRDECL_GETSECTION_OR_IMPORT(attr) : NULL;
    if (!target_section || DCCSection_ISIMPORT(target_section)) {
     target_section = DCCTYPE_STATICWRITABLE(type->t_type) ? unit.u_bss : unit.u_data;
    }
    assert(target_section && !DCCSection_ISIMPORT(target_section));
    if (!type_a) type_a = 1;
    DCCSection_TBEGIN(target_section);
    DCCSection_TAlign(target_section,type_a,0);
    base_target.ml_reg = DCC_RC_CONST;
    base_target.ml_sym = &target_section->sc_start;
    base_target.ml_off = (target_off_t)DCCSection_TADDR(target_section);
    if (kind != KIND_VARRAY) {
     void *p = DCCSection_TAlloc(target_section,type_s);
     if (p) memset(p,0,type_s); /* Pre-initialize everything to ZEROes. */
    }
    DCCSection_TEND(target_section);
   } else if (kind == KIND_VARRAY) {
    /* Because the work-around for this, as found below is _so_ inefficient, warn about its use. */
    WARN(W_LOCAL_VARRAY_VERY_INEFFICIENT,type);
    base_target.ml_off = -(target_off_t)compiler.c_hwstack.hws_curoffset;
    goto stack_target;
   } else { /* Local storage duration. */
    base_target.ml_off = DCCCompiler_HWStackAlloc(type_s,type_a,0);
stack_target:
    base_target.ml_reg = DCC_RR_XBP;
    base_target.ml_sym = NULL;
   }
  }
  if (has_brace) YIELD();
  /* Brace initializer. */
  while (TOK > 0 && TOK != '}') {
   switch (kind) {

   { /* structure/union. */
    int cxx_field_name;
   case KIND_UNION:
   case KIND_STRUCT:
    if (TOK == '.') {
     struct DCCStructField *next_field;
     YIELD();
     if (!TPP_ISKEYWORD(TOK)) {
      WARN(W_EXPECTED_KEYWORD_FOR_FIELD_NAME,type);
      goto parse_field;
     }
     cxx_field_name = 0;
parse_field_name:
     /* Locate field by name. */
     assert(TPP_ISKEYWORD(TOK));
     assert(type->t_base);
     assert(type->t_base->d_kind == DCC_DECLKIND_STRUCT ||
            type->t_base->d_kind == DCC_DECLKIND_UNION);
     next_field = DCCDecl_FindStructField(type->t_base,TOKEN.t_kwd);
     if (!next_field) WARN(W_UNKNOWN_FIELD,type,TOKEN.t_kwd);
     else field_curr = next_field; /* Select the explicit field. */
     YIELD();
     if (!cxx_field_name) {
      if (TOK != '=') WARN(W_EXPECTED_EQUAL_AFTER_FIELD_NAME);
      else YIELD();
     }
    } else if (TPP_ISKEYWORD(TOK)) {
     /* Alternative field name designators (as seen in older versions of GCC):
      * >> struct point p = {
      * >>    x: 42,
      * >>    y: 64,
      * >> };
      */
     struct TPPFile *peek_file;
     char *peek_token = peek_next_token(&peek_file);
     if (*peek_token == ':') {
      peek_file->f_pos = peek_token+1;
      cxx_field_name = 1;
      goto parse_field_name;
     }
    }
parse_field:
    if (field_curr == field_end) {
     target_ptr_t old_offset;
     WARN(W_STRUCTURE_FULLY_INITIALIZED,type);
     old_offset = compiler.c_hwstack.hws_curoffset;
     DCCParse_Expr1();
     compiler.c_hwstack.hws_curoffset = old_offset;
     vpop(1);
    } else {
     struct DCCType *field_type;
     assert(field_curr);
     assert(field_curr->sf_decl);
     assert(field_curr->sf_decl->d_kind&DCC_DECLKIND_TYPE);
     elem_target         = base_target;
     elem_target.ml_off += field_curr->sf_off;
     field_type          = &field_curr->sf_decl->d_type;
     if (field_curr->sf_bitfld) {
      push_target(field_type,&elem_target);
      vbitfldf(field_curr->sf_bitfld);
      DCCParse_Init(field_type,attr,NULL,
                    flags|DCCPARSE_INITFLAG_INBRACE);
      vstore(flags&DCCPARSE_INITFLAG_INITIAL);
     } else {
      DCCParse_Init(field_type,attr,&elem_target,
                    flags|DCCPARSE_INITFLAG_INBRACE);
     }
     vpop(0);
     ++field_curr;
    }
   } break;

   { /* array/variadic array. */
    target_ptr_t repeat,elem_addr;
    int has_initializer;
    struct DCCType *elem_type;
   case KIND_VARRAY:
   case KIND_ARRAY:
    assert(type->t_base);
    elem_type = &type->t_base->d_type;
    repeat = 1;
    if (TOK == '[') {
     int_t index_begin,index_end;
     /* Parse an explicit initializer index. */
     YIELD();
     index_end = (index_begin = DCCParse_CExpr(0))+1;
     /* Range-style array initializer. */
     if (TOK == TOK_DOTS) {
      YIELD();
      index_end = DCCParse_CExpr(0)+1;
     }
     if (TOK != ']') WARN(W_EXPECTED_RBRACKET); else YIELD();
     if (TOK != '=') WARN(W_EXPECTED_EQUAL_AFTER_ARRAY_INDEX); else YIELD();
     if (index_begin < 0) WARN(W_NEGATIVE_INDEX_IN_ARRAY_INITIALIZER,type),index_begin = 0;
     if (kind != KIND_VARRAY && index_end > target_maxindex)
      WARN(W_OUT_OF_BOUNDS_IN_ARRAY_INITIALIZER,type),
      index_end = target_maxindex;
     if (index_begin > index_end)
      WARN(W_UNORDERED_RANGE_IN_ARRAY_INITIALIZER,type),
      index_begin = index_end;
     else if (index_end == index_begin)
      WARN(W_EMPTY_RANGE_IN_ARRAY_INITIALIZER,type);
     target_index = (target_ptr_t)index_begin;
     repeat = (target_ptr_t)index_end-target_index;
    } else if (kind != KIND_VARRAY && target_index >= target_maxindex) {
     WARN(W_ARRAY_FULLY_INITIALIZED,type);
     repeat = 0; /* Don't repeat! */
    }
    has_initializer = 0;
    if (DCCTYPE_ISBASIC(elem_type->t_type,DCCTYPE_AUTO)) {
     /* Special case: 'auto x[] = {10,20,30};' --> 'int x[] = {10,20,30};' */
     DCCParse_Init(elem_type,NULL,NULL,flags|
                   DCCPARSE_INITFLAG_INBRACE);
     has_initializer = 1;
     assert(vsize);
     assert(!elem_type->t_base);
     DCCType_InitCopy(elem_type,&vbottom->sv_ctype);
     elem_size = DCCType_Sizeof(elem_type,&elem_align,1);
    }
    elem_target         = base_target;
    elem_target.ml_off += target_index*elem_size;
    elem_target.ml_off +=  (elem_align-1); /* alignment */
    elem_target.ml_off &= ~(elem_align-1); /* *ditto* */
    if (kind == KIND_VARRAY) {
     /* Figure out where this target will end. */
     target_ptr_t after_target = target_index+repeat;
     if (after_target > target_maxindex) {
      if (elem_target.ml_reg == DCC_RC_CONST) {
       struct DCCSection *target_section;
       if (!elem_target.ml_sym ||
          (target_section = DCCSym_SECTION(elem_target.ml_sym)) == NULL) /* TODO: Error */;
       else {
        /* Resize the array if it got extended. */
        DCCSection_TBEGIN(target_section);
        base_target.ml_off = DCCSection_DRealloc(target_section,base_target.ml_off,
                                                 target_maxindex*elem_size,
                                                 after_target*elem_size,
                                                 elem_align,0);
        DCCSection_TEND(target_section);
        goto update_elem_target_off;
       }
      } else {
       /* Special case: Since the stack grows downwards, we'd need to know
        * >> This is _very_ inefficient, because we're moving data downwards.
        */
#define array_base  base_target.ml_off
       struct DCCMemLoc src,dst;
       target_off_t new_array_base;
       target_ptr_t neg_array_base;
       new_array_base = array_base-(after_target-target_maxindex)*elem_size;
       neg_array_base = (target_ptr_t)-new_array_base;
       /* Allocate stack memory */
       if (DCCCompiler_ISCGEN()) {
        if (neg_array_base > compiler.c_hwstack.hws_curoffset) compiler.c_hwstack.hws_curoffset = neg_array_base;
        if (neg_array_base > compiler.c_hwstack.hws_maxoffset) compiler.c_hwstack.hws_maxoffset = neg_array_base;
       }
       /* memmove(%EBP+new_array_base,%EBP+array_base,target_maxindex*elem_size); */
       src.ml_reg = dst.ml_reg = elem_target.ml_reg;
       src.ml_sym = dst.ml_sym = elem_target.ml_sym;
       src.ml_off =     array_base;
       dst.ml_off = new_array_base;
       DCCDisp_MemMovMem(&src,target_maxindex*elem_size,
                         &dst,target_maxindex*elem_size,1);
       elem_addr  = new_array_base+target_index*elem_size;
       array_base = new_array_base;
#undef array_base
update_elem_target_off:
       elem_target.ml_off = base_target.ml_off+target_index*elem_size;
      }
      target_maxindex = after_target;
     }
    }
    /* Parse the array element. */
    if (!repeat) {
     pushf();
     compiler.c_flags |=   DCC_COMPILER_FLAG_NOCGEN;
     compiler.c_flags &= ~(DCC_COMPILER_FLAG_SINIT);
     if (!has_initializer) {
      DCCParse_Init(elem_type,attr,NULL,flags|
                    DCCPARSE_INITFLAG_INBRACE);
     }
     vpop(1);
     popf();
    } else if (repeat == 1) {
     if (!has_initializer) {
      DCCParse_Init(elem_type,attr,&elem_target,
                    flags|DCCPARSE_INITFLAG_INBRACE);
     } else {
      push_target(elem_type,&elem_target);
      vswap(); /* target, init */
      vstore(flags&DCCPARSE_INITFLAG_INITIAL);
     }
     vpop(0);
    } else {
     /* Special case: Must initialize more than one thing. */
     if (!has_initializer) {
      DCCParse_Init(elem_type,attr,NULL,
                    flags|DCCPARSE_INITFLAG_INBRACE);
     }
     for (;;) {
      /* Force 'vbottom' into the target. */
      push_target(elem_type,&elem_target);
      vswap();   /* target, init */
      /* Warn if vbottom is a register, or register-offset. */
      if (!--repeat) {
       vstore(flags&DCCPARSE_INITFLAG_INITIAL); /* target=init */
       break;
      }
      vdup(1);                                 /* target, init, init */
      vlrot(3);                                /* init, target, init */
      vstore(flags&DCCPARSE_INITFLAG_INITIAL); /* init, target=init */
      vpop(0);                                 /* init */
      elem_target.ml_off += elem_size;
     }
     vpop(0);    /* . */
    }
    /* Advance the array index. */
    target_index += repeat;
   } break;

   default: break;
   }
   if (TOK != ',') break;
   YIELD();
  }
  /* Push the final target & fix variadic array types. */
  {
   struct DCCType final_type = *type;
   if (kind == KIND_VARRAY) {
    /* Set the finalized array length. */
    assert(final_type.t_base);
    assert(final_type.t_base->d_kind == DCC_DECLKIND_ARRAY);
    final_type.t_type = final_type.t_base->d_type.t_type;
    final_type.t_base = final_type.t_base->d_type.t_base;
    DCCDecl_XIncref(final_type.t_base);
    DCCType_MkArray(&final_type,target_maxindex);
   }
   if (!target && base_target.ml_reg == DCC_RC_CONST) {
    target_siz_t final_size;
    struct DCCSection *target_section;
    if (kind == KIND_ARRAY || kind == KIND_VARRAY)
         final_size = target_maxindex*elem_size;
    else final_size = DCCType_Sizeof(type,&elem_align,1);
    /* Since we've auto-generated the target, let's
     * allocate a reference-counted symbol and merge
     * it within the target section. */
    DCCSym_ASSERT(base_target.ml_sym);
    assert(DCCSym_ISSECTION(base_target.ml_sym));
    target_section = DCCSym_TOSECTION(base_target.ml_sym);
    /* Merge section data (This is a no-op if the section doesn't allow merging). */
    base_target.ml_off = (target_off_t)DCCSection_DMerge(target_section,
                                                        (target_ptr_t)base_target.ml_off,
                                                         final_size,elem_align);
    /* Allocate a data symbol within the section.
     * >> Allocating the symbol here is not ~really~ required, but if
     *    we fail to do so, the section data associated with this initializer
     *    will become danging as it will be impossible to trace data association. */
    base_target.ml_sym = DCCUnit_AllocSym();
    if unlikely(!base_target.ml_sym)
       base_target.ml_sym = &target_section->sc_start;
    else {
     /* This symbol inherits all section data from the initializer. */
     DCCSym_Define(base_target.ml_sym,target_section,
                  (target_ptr_t)base_target.ml_off,
                   final_size);
     base_target.ml_off = 0; /* Now stored in the symbol. */
    }
   }
   /* Actually push the finalized type & target. */
   push_target(&final_type,&base_target);
   if (kind == KIND_VARRAY)
       DCCType_Quit(&final_type);
  }
end_brace:
  if (has_brace) {
   if (TOK != '}') WARN(W_EXPECTED_RBRACE);
   else YIELD();
  }
 } else {
#if 0 /* This _still_ doesn't work for string --> char[] */
  /* Special case: handle missing braces around struct/union/array types.
   * NOTE: Though we must not handle missing braces around arithmetic structure types! */
  if (type->t_base && (flags&DCCPARSE_INITFLAG_INBRACE)) {
   uint16_t base_kind = type->t_base->d_kind;
   if ((base_kind == DCC_DECLKIND_STRUCT &&
       (!type->t_base->d_attr || !(type->t_base->d_attr->a_flags&DCC_ATTRFLAG_ARITHMETIC))) ||
        base_kind == DCC_DECLKIND_UNION ||
        base_kind == DCC_DECLKIND_ARRAY ||
        base_kind == DCC_DECLKIND_VLA) {
    WARN(W_INITIALIZER_MISSING_BRACE,type);
    has_brace = 0;
    goto parse_braceblock;
   }
  }
#endif
  DCCParse_Expr1();
  if (vbottom->sv_reg != DCC_RC_CONST) {
   if (!compiler.c_fun) WARN(W_NON_CONSTANT_GLOBAL_INITIALIZER,type);
   else if (compiler.c_flags&DCC_COMPILER_FLAG_SINIT) {
    WARN(W_NON_CONSTANT_STATIC_INITIALIZER,type);
   }
  }
  if (target) {
   push_target(type,target);
   vswap();
   vstore(flags&DCCPARSE_INITFLAG_INITIAL);
  }
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_INIT_C_INL */
