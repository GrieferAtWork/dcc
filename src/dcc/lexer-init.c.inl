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
              int initial_init) {
 assert(type);
 if (TOK == '{') {
#define KIND_STRUCT  0
#define KIND_UNION   1
#define KIND_ARRAY   2
#define KIND_VARRAY  3
  int kind;
  struct DCCStructField *field_curr,*field_end;
  struct DCCMemLoc base_target,elem_target;
  /* - Current index within an array initializer.
   * - When non-zero, a union has been initialized. */
  target_ptr_t target_maxindex = 0,target_index = 0;
  target_ptr_t elem_size = 0;
  field_curr = field_end = NULL;
  /* Warn about initializer-assignment to constant target. */
  if (!initial_init && type->t_type&DCCTYPE_CONST)
       WARN(W_ASSIGN_INIT_CONSTANT_TYPE,type);
  switch (DCCTYPE_GROUP(type->t_type)) {

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
    YIELD();
    pushf();
    compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
    for (;;) {
     DCCParse_Init(type,attr,target,initial_init);
     vpop(1);
     if (TOK != ',') break;
     YIELD();
    }
    popf();
    vpushv();
    goto end_brace;
   }
   kind            = KIND_ARRAY;
   target_maxindex = type->t_base->d_tdecl.td_size;
   if (DCC_MACRO_FALSE) { case DCCTYPE_VARRAY: kind = KIND_VARRAY; }
   elem_size       = DCCType_Sizeof(&type->t_base->d_type,NULL,1);
   break;

  default:
   WARN(W_BRACE_INITIALIZER_FOR_DEFAULT_TYPE,type);
   YIELD();
   for (;;) {
    DCCParse_Init(type,attr,target,initial_init);
    if (TOK != ',') break;
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
      (type->t_type&DCCTYPE_STOREMASK) == DCCTYPE_EXTERN ||
      (type->t_type&DCCTYPE_STOREMASK) == DCCTYPE_STATIC ||
      (compiler.c_flags&DCC_COMPILER_FLAG_SINIT)) {
    struct DCCSection *target_section;
    /* Global storage duration. */
    target_section = attr ? attr->a_section : NULL;
    if (!target_section || DCCSection_ISIMPORT(target_section)) {
     target_section = type->t_type&DCCTYPE_CONST
      ? unit.u_data : unit.u_bss;
    }
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
    /* Because the work-around for this, as found below is _so_ inefficient, warn about it. */
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
  YIELD();
  /* Brace initializer. */
  while (TOK > 0 && TOK != '}') {
   switch (kind) {

   { /* structure/union. */
    int cxx_field_name;
   case KIND_UNION:
    if (target_index) WARN(W_UNION_ALREADY_INITIALIZED,type);
    target_index = 1;
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
      if (TOK != '=') WARN(W_EXPECTED_EQUALS_AFTER_FIELD_NAME);
      else YIELD();
     }
    } else if (TPP_ISKEYWORD(TOK)) {
     /* Alternative field name designators (as seen in older versions of GCC):
      * >> struct point p = {
      * >>    x: 42,
      * >>    y: 64,
      * >> };
      */
     char *peek_token = peek_next_token();
     if (*peek_token == ':') {
      TOKEN.t_file->f_pos = peek_token+1;
      cxx_field_name = 1;
      goto parse_field_name;
     }
    }
parse_field:
    if (field_curr == field_end) {
     target_ptr_t old_offset;
     WARN(W_STRUCTURE_FULLY_INITIALIZED);
     old_offset = compiler.c_hwstack.hws_curoffset;
     DCCParse_Expr1();
     compiler.c_hwstack.hws_curoffset = old_offset;
     vpop(1);
    } else {
     assert(field_curr);
     assert(field_curr->sf_decl);
     assert(field_curr->sf_decl->d_kind&DCC_DECLKIND_TYPE);
     elem_target = base_target;
     elem_target.ml_off += field_curr->sf_off;
     DCCParse_Init(&field_curr->sf_decl->d_type,attr,
                   &elem_target,initial_init);
     vpop(0);
     ++field_curr;
    }
   } break;

   { /* array/variadic array. */
    target_ptr_t repeat,elem_addr;
   case KIND_VARRAY:
   case KIND_ARRAY:
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
     if (TOK != '=') WARN(W_EXPECTED_EQUALS_AFTER_ARRAY_INDEX); else YIELD();
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
    /* alignment? */
    elem_target         = base_target;
    elem_target.ml_off += target_index*elem_size;
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
                                                 1/* TODO: Alignment? */,0);
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
     DCCParse_Init(&type->t_base->d_type,attr,NULL,initial_init);
     vpop(1);
     popf();
    } else if (repeat == 1) {
     DCCParse_Init(&type->t_base->d_type,attr,&elem_target,initial_init);
     vpop(0);
    } else {
     struct DCCType *elem_type = &type->t_base->d_type;
     /* Special case: Must initialize more than one thing. */
     DCCParse_Init(elem_type,attr,NULL,initial_init);
     for (;;) {
      /* Force 'vbottom' into the target. */
      push_target(elem_type,&elem_target);
      vswap();   /* target, init */
      /* Warn if vbottom is a register, or register-offset. */
      if (!--repeat) {
       vstore(initial_init); /* target=init */
       break;
      }
      vdup(1);              /* target, init, init */
      vlrot(3);             /* init, target, init */
      vstore(initial_init); /* init, target=init */
      vpop(0);              /* init */
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
  if (kind == KIND_VARRAY) {
   struct DCCType array_type;
   /* Set the finalized array length. */
   assert(type->t_base);
   assert(type->t_base->d_kind == DCC_DECLKIND_ARRAY);
   array_type.t_type = type->t_base->d_type.t_type;
   array_type.t_base = type->t_base->d_type.t_base;
   if (array_type.t_base) DCCDecl_Incref(array_type.t_base);
   DCCType_MkArray(&array_type,target_maxindex);
   /* Push the fully initialized value. */
   push_target(&array_type,&base_target);
   DCCType_Quit(&array_type);
  } else {
   /* Push the fully initialized value. */
   push_target(type,&base_target);
  }
end_brace:
  if (TOK != '}') WARN(W_EXPECTED_RBRACE); else YIELD();
 } else {
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
   vstore(initial_init);
  }
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_INIT_C_INL */
