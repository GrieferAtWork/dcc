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
#ifndef GUARD_DCC_ADDR2LINE_C
#define GUARD_DCC_ADDR2LINE_C 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/addr2line.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>

#include <stdio.h>
#include <string.h>

DCC_DECL_BEGIN

A2L_IMPL a2l_op_t *
A2L_NAME(a2l_setarg)(a2l_op_t *code, a2l_arg_t arg) {
 a2l_arg_t temp,shift;
 if (A2L_ARG_NEEDSIGN(arg)) {
  *code++ = A2L_A_NEG;
  arg = A2L_ARG_NEGATE(arg);
 }
 temp = arg,shift = 0;
 do temp >>= A2L_A_SFT,
    shift += A2L_A_SFT;
 while (temp);
 do {
  a2l_op_t byte;
  shift -= A2L_A_SFT;
  byte = (a2l_op_t)((arg >> shift)&A2L_A_MAX);
  if (shift) byte |= A2L_A_CON;
  *code++ = byte;
 } while (shift);
 return code;
}

PRIVATE a2l_op_t *
a2l_exec1(struct A2lState *__restrict state,
          a2l_op_t const *__restrict code) {
 a2l_exec(state,state,&code,A2L_CAPTURE_ONE);
 return (a2l_op_t *)code;
}

DCCFUN char const *DCCA2lState_GetPath(struct A2lState const *__restrict self);
DCCFUN char const *DCCA2lState_GetFile(struct A2lState const *__restrict self);
DCCFUN char const *DCCA2lState_GetName(struct A2lState const *__restrict self);

#if DCC_DEBUG
PRIVATE void
DCCA2lChunk_AssertIntegrity(struct DCCA2lChunk *__restrict self) {
 assert(self);
 assert(self->c_code_end  >= self->c_code_pos);
 assert(self->c_code_pos  >= self->c_code_last);
 assert(self->c_code_last >= self->c_code_begin ||
       (!self->c_code_last && self->c_code_pos == self->c_code_begin));
 if (self->c_code_pos != self->c_code_begin) {
  struct A2lState prev_state,state = self->c_smin;
  a2l_op_t *prev_code,*code = self->c_code_begin;
  while (code != self->c_code_pos) {
   assertf(code < self->c_code_pos,
           "Code out-of-bounds by %lu bytes",
          (unsigned long)(code-self->c_code_pos));
   prev_code = code;
   prev_state = state;
   a2l_exec(&state,&state,
           (a2l_op_t const **)&code,
            A2L_CAPTURE_ONE);
   assert(prev_state.s_addr <= state.s_addr);
  }
  assertf(!memcmp(&state,&self->c_smax,
                  sizeof(struct A2lState)),
          "Miss-matching final states\n"
          "             state   |  c_smax\n"
          "s_line     = %p   %p\n"
          "s_col      = %p   %p\n"
          "s_path     = %p ('%s') %p ('%s')\n"
          "s_file     = %p ('%s') %p ('%s')\n"
          "s_name     = %p ('%s') %p ('%s')\n"
          "s_features = %p   %p\n"
          "s_addr     = %p   %p\n"
         ,state.s_line
         ,self->c_smax.s_line
         ,state.s_col
         ,self->c_smax.s_col
         ,state.s_path,dbgstr(state.s_path)
         ,self->c_smax.s_path,dbgstr(self->c_smax.s_path)
         ,state.s_file,dbgstr(state.s_file)
         ,self->c_smax.s_file,dbgstr(self->c_smax.s_file)
         ,state.s_name,dbgstr(state.s_name)
         ,self->c_smax.s_name,dbgstr(self->c_smax.s_name)
         ,state.s_features
         ,self->c_smax.s_features
         ,state.s_addr
         ,self->c_smax.s_addr
          );
 }
}
#else
#define DCCA2lChunk_AssertIntegrity(self) (void)0
#endif


PRIVATE int
DCCA2lChunk_IncCode(struct DCCA2lChunk *__restrict self) {
 a2l_op_t *newbuf;
 size_t oldsize,newsize;
 assert(self);
 oldsize = (size_t)(self->c_code_end-self->c_code_begin);
 newsize = oldsize;
 if (!newsize) newsize = 1;
 newsize *= 2;
 newbuf = (a2l_op_t *)realloc(self->c_code_begin,newsize*sizeof(a2l_op_t));
 if unlikely(!newbuf) { DCC_AllocFailed(newsize*sizeof(a2l_op_t)); return 0; }
 self->c_code_pos   = newbuf+(self->c_code_pos-self->c_code_begin);
 self->c_code_last  = newbuf+(self->c_code_last-self->c_code_begin);
 self->c_code_end   = newbuf+newsize;
 self->c_code_begin = newbuf;
 return 1;
}


/* Max amount of code generated by a worst-case transition. */
#if USE_FULL_DEBUG
#define A2L_TRANSITION_MAXSIZE ((1+A2L_ARG_MAXBYTES)*6)
#else
#define A2L_TRANSITION_MAXSIZE \
 ((1+A2L_ARG_MAXBYTES)*3 /* path, file, name */\
 +(1+A2L_ARG_MAXBYTES*3) /* addr+line+col (worst case: change all 3; all other cases are smaller) */)
#endif


PRIVATE a2l_op_t *
a2l_build_transition(a2l_op_t *__restrict buf,
                     struct A2lState const *__restrict old_state,
                     struct A2lState const *__restrict new_state) {
#define FLUSH_DELFLAGS() \
 do{ if (del_flags) { O(del_flags); del_flags = 0; } \
 }while(DCC_MACRO_FALSE)
 a2l_op_t del_flags = 0;
#define O(x) (void)(*buf++ = (a2l_op_t)(x))
#define A(a) (void)(buf = a2l_setarg(buf,a))
 assert(buf);
 assert(old_state);
 assert(new_state);
 /* Update PATH */
 if (old_state->s_features&A2L_STATE_HASPATH) {
  if (new_state->s_features&A2L_STATE_HASPATH) {
   if (old_state->s_path != new_state->s_path) goto set_path;
  } else del_flags |= A2L_O_DEL_P;
 } else if (new_state->s_features&A2L_STATE_HASPATH) {
set_path:
  O(A2L_O_SP);
  A(new_state->s_path);
 }

 /* Update FILE */
 if (old_state->s_features&A2L_STATE_HASFILE) {
  if (new_state->s_features&A2L_STATE_HASFILE) {
   if (old_state->s_file != new_state->s_file) goto set_file;
  } else del_flags |= A2L_O_DEL_F;
 } else if (new_state->s_features&A2L_STATE_HASFILE) {
set_file:
  O(A2L_O_SF);
  A(new_state->s_file);
 }

 /* Update NAME */
 if (old_state->s_features&A2L_STATE_HASNAME) {
  if (new_state->s_features&A2L_STATE_HASNAME) {
   if (old_state->s_name != new_state->s_name) goto set_name;
  } else
      del_flags |= A2L_O_DEL_N;
 } else if (new_state->s_features&A2L_STATE_HASNAME) {
set_name:
  O(A2L_O_SN);
  A(new_state->s_name);
 }

 if (!(new_state->s_features&A2L_STATE_HASLINE)) {
  /* Special handling for addr2line information without line. */
  if (old_state->s_features&A2L_STATE_HASLINE) del_flags |= A2L_O_DEL_L;
  if (new_state->s_addr == old_state->s_addr) goto update_col;
  goto update_col_with_address;
 }
 if (new_state->s_addr == old_state->s_addr) {
  /* No change in address. - Manually update line & column. */
  if (new_state->s_line != old_state->s_line) {
   O(A2L_O_IL);
   A(new_state->s_line-old_state->s_line);
  }
update_col:
  if (new_state->s_features&A2L_STATE_HASCOL) {
write_col:
   /* Set a new column value. */
   O(old_state->s_features&A2L_STATE_HASCOL ? A2L_O_IC : A2L_O_SC);
   A(new_state->s_col-old_state->s_col);
  } else if (old_state->s_features&A2L_STATE_HASCOL) {
   del_flags |= A2L_O_DEL_C;
  }
  goto end;
 }

 /* At this point we know that:
  *  - A line is available
  *  - The address has changed */
 if (new_state->s_line != old_state->s_line) {
  /* Update addr+line[+col] at once (most common case). */
  FLUSH_DELFLAGS();
  if (new_state->s_features&A2L_STATE_HASCOL) {
   a2l_arg_t line_off = (a2l_arg_t)(new_state->s_line-
                                    old_state->s_line);
   assert(line_off);
   if (line_off <= A2L_MAX_NLSCIA) {
    /* Generate a small line disposition. */
    O((A2L_O_NLLO_SC_IA-1)+line_off);
   } else {
    O(A2L_O_IL_SC_IA);
    A(line_off);
   }
   A(new_state->s_col);
  } else {
   /* Update new_state->s_addr+line */
   a2l_arg_t line_off = (a2l_arg_t)(new_state->s_line-
                                    old_state->s_line);
   assert(line_off);
   if (line_off <= A2L_MAX_NLIA) {
    O((A2L_O_NLLO_IA-1)+line_off);
   } else {
    O(A2L_O_IL_IA);
    A(line_off);
   }
  }
  goto addr_offset;
 }
update_col_with_address:
 if (new_state->s_features&A2L_STATE_HASCOL) {
  a2l_arg_t col_offset;
  if (new_state->s_addr == old_state->s_addr) goto write_col;
  /* Update addr+column. */
  col_offset = (a2l_arg_t)(new_state->s_col-
                           old_state->s_col);
  FLUSH_DELFLAGS();
  if (!col_offset) {
   /* No column offset. */
   O(A2L_O_IA);
  } else if (col_offset <= A2L_MAX_NCIA) {
   O((A2L_O_NCLO_IA-1)+col_offset);
  } else {
   O(A2L_O_IC_IA);
   A(col_offset);
  }
  goto addr_offset;
 } else {
  if (old_state->s_features&A2L_STATE_HASCOL)
      del_flags |= A2L_O_DEL_C;
  if (new_state->s_addr != old_state->s_addr) {
   FLUSH_DELFLAGS();
   O(A2L_O_IA);
addr_offset:
   A(new_state->s_addr-
     old_state->s_addr);
  }
 }
end:
 FLUSH_DELFLAGS();
 return buf;
#undef FLUSH_DELFLAGS
#undef A
#undef O
}



/* Lookup the boundary pointers of code specifying
 * debug information about 'new_state->s_addr'. */
PRIVATE void
DCCA2lChunk_GetInsPtr(struct DCCA2lChunk *__restrict self,
                      a2l_addr_t addr,
                      a2l_op_t **__restrict ins_begin,
                      a2l_op_t **__restrict ins_end,
                      struct A2lState *__restrict ins_begin_state,
                      struct A2lState *__restrict ins_end_state) {
 a2l_op_t *code,*newcode;
 struct A2lState state;
 assert(self);
 assert(ins_begin);
 assert(ins_end);
 assert(ins_begin_state);
 assert(ins_end_state);
 assert(addr >= self->c_smin.s_addr);
 assert(addr <= self->c_smax.s_addr);
 state = self->c_smin;
 code = self->c_code_begin;
 for (;;) {
  assert(state.s_addr <= addr);
  *ins_begin_state = state;
  newcode = a2l_exec1(&state,code);
  if (state.s_addr >= addr) {
   *ins_begin = code;
   /* When the last capture opcode describes 'addr' directly,
    * use the opcode's end for '*ins_end' in order to override
    * its meaning with new debug information inside the caller.
    * Otherwise, insert new debug info by setting '*ins_end'
    * equal '*ins_begin'. */
   if (state.s_addr == addr) {
    *ins_end       = newcode;
    *ins_end_state = state;
   } else {
    *ins_end       = code;
    *ins_end_state = *ins_begin_state;
   }
   break;
  }
  code = newcode;
 }
}

PRIVATE void
A2lState_RelocString(struct A2lState *__restrict self,
                     target_off_t offset) {
 assert(self);
 if (self->s_features&A2L_STATE_HASPATH) self->s_path += offset;
 if (self->s_features&A2L_STATE_HASFILE) self->s_file += offset;
 if (self->s_features&A2L_STATE_HASNAME) self->s_name += offset;
}


#if 0
#define HAVE_RELOC_LOG
#define RELOC_LOG(x) printf x
#else
#define RELOC_LOG(x) (void)0
#endif

DCCFUN void
DCCA2lChunk_RelocString(struct DCCA2lChunk *__restrict self,
                        target_off_t offset) {
 a2l_op_t *arg_end,op,new_arg[A2L_ARG_MAXBYTES];
 size_t new_argsize,old_argsize,old_last;
 a2l_arg_t arg;
 assert(self);
 assert(self->c_code_pos <= self->c_code_end);
 RELOC_LOG(("\n\n\n\n"));

 DCCA2lChunk_AssertIntegrity(self);
 /* Relocate the min/max/last states. */
 A2lState_RelocString(&self->c_smin,offset);
 A2lState_RelocString(&self->c_smax,offset);
 A2lState_RelocString(&self->c_slast,offset);

 /* NOTE: We (ab-)use 'c_code_last' as iterator, as it will
  *       automatically be updated when the chunk is resized. */
 old_last = self->c_code_last-self->c_code_begin;
 self->c_code_last = self->c_code_begin;
 /* Search for opcodes with string operands. */
 while (self->c_code_last != self->c_code_pos) {
  assert(self->c_code_last < self->c_code_pos);
  op = *self->c_code_last++;
  RELOC_LOG(("[%p] OP %x",(self->c_code_last-1)-self->c_code_begin,op));
  if (A2L_ISSTROP(op)) {
   arg_end     = self->c_code_last;
   arg         = a2l_getarg((a2l_op_t const **)&arg_end);
   old_argsize = (size_t)(arg_end-self->c_code_last);
   RELOC_LOG((" %x --> %x",arg,arg+offset));
   arg        += offset;
   new_argsize = (size_t)(a2l_setarg(new_arg,arg)-new_arg);
   if (old_argsize == new_argsize) {
    /* Simple case: Override the argument directly. */
    memcpy(self->c_code_last,new_arg,new_argsize);
   } else if (old_argsize > new_argsize) {
    size_t diff;
    /* Shift the code vector downwards. */
    memcpy(self->c_code_last,new_arg,new_argsize);
    memmove(self->c_code_last+new_argsize,arg_end,
           (size_t)(self->c_code_pos-arg_end));
    diff = old_argsize-new_argsize;
    self->c_code_pos -= diff;
    old_last         -= diff;
   } else {
    size_t avail,required;
    /* Must resize the code. */
    required = new_argsize-old_argsize;
    for (;;) {
     avail = (size_t)(self->c_code_end-self->c_code_pos);
     if (avail >= required) break;
     *(uintptr_t *)&arg_end -= (uintptr_t)self->c_code_begin;
     if (!DCCA2lChunk_IncCode(self)) return;
     *(uintptr_t *)&arg_end += (uintptr_t)self->c_code_begin;
    }
    memmove(arg_end+required,arg_end,
           (size_t)(self->c_code_pos-arg_end));
    memcpy(self->c_code_last,new_arg,new_argsize);
    self->c_code_pos += required;
    old_last         += required;
   }
   self->c_code_last += new_argsize;
  } else {
   unsigned int opc = A2L_GETOPC(op);
   while (opc--) {
    assert(self->c_code_last < self->c_code_pos);
#ifdef HAVE_RELOC_LOG
    RELOC_LOG((" %x",a2l_getarg((a2l_op_t const **)&self->c_code_last)));
#else
    a2l_getarg((a2l_op_t const **)&self->c_code_last);
#endif
   }
  }
  RELOC_LOG(("\n"));
 }
 self->c_code_last = self->c_code_begin+old_last;
 DCCA2lChunk_AssertIntegrity(self);
}


PRIVATE void
DCCA2lChunk_Insert(struct DCCA2lChunk *__restrict self,
                   struct A2lState const *__restrict data) {
 a2l_op_t buf[A2L_TRANSITION_MAXSIZE*2];
 size_t size,ins_size;
 a2l_op_t *ins_begin,*ins_end;
 struct A2lState ins_begin_state;
 struct A2lState ins_end_state;
 assert(self);
 assert(data);
 if (self->c_code_pos == self->c_code_begin &&
    !self->c_smin.s_features &&
    !self->c_smax.s_features) {
  /* Special case: The chunk is empty, meaning we are allowed to
   *               specify anything we want for min/max states! */
  self->c_smax = self->c_smin = *data;
  DCCA2lChunk_AssertIntegrity(self);
  return;
 }

 if (data->s_addr == self->c_smax.s_addr) {
  /* Special case: Re-write the last state transition. */
  if (self->c_code_begin == self->c_code_pos) {
   /* Re-write the initial capture point. */
   self->c_smin = *data;
  } else {
   self->c_code_pos = self->c_code_last;
   size = (size_t)(a2l_build_transition(buf,&self->c_slast,data)-buf);
   while (self->c_code_pos+size > self->c_code_end)
    if (!DCCA2lChunk_IncCode(self)) goto end;
   memcpy(self->c_code_pos,buf,size);
   self->c_code_pos += size;
  }
  self->c_smax = *data;
  DCCA2lChunk_AssertIntegrity(self);
  return;
 }

 if (data->s_addr > self->c_smax.s_addr) {
  /* Simple case: Append at the end.
   * Can easily be handled by transitioning from the current end to the new. */
  size = (size_t)(a2l_build_transition(buf,&self->c_smax,data)-buf);
  assertf(size,"But the address changed?");
  while (self->c_code_pos+size > self->c_code_end)
   if (!DCCA2lChunk_IncCode(self)) goto end;
  memcpy(self->c_code_pos,buf,size);
  self->c_slast     = self->c_smax;
  self->c_code_last = self->c_code_pos;
  self->c_code_pos += size;
  self->c_smax = *data;
  DCCA2lChunk_AssertIntegrity(self);
  return;
 }
 if (data->s_addr < self->c_smin.s_addr) {
  /* Special case: Prepend before the chunk's start. */
  size = (size_t)(a2l_build_transition(buf,data,&self->c_smin)-buf);
  assertf(size,"But the address changed?");
  while (self->c_code_pos+size > self->c_code_end)
   if (!DCCA2lChunk_IncCode(self)) goto end;
  memmove(self->c_code_begin+size,self->c_code_begin,
         (size_t)(self->c_code_pos-self->c_code_begin));
  memcpy(self->c_code_begin,buf,size);
  self->c_code_last += size;
  self->c_code_pos  += size;
  self->c_smin = *data;
  DCCA2lChunk_AssertIntegrity(self);
  return;
 }
 DCCA2lChunk_GetInsPtr(self,data->s_addr,
                      &ins_begin,&ins_end,
                      &ins_begin_state,
                      &ins_end_state);
 assert(ins_end >= ins_begin);
 assert(ins_begin >= self->c_code_begin);
 assert(ins_end   <= self->c_code_pos);
 /* Check for updating special state locations. */
 if (ins_begin == self->c_code_begin) {
  /* Insert at the front. */
  assert(!memcmp(&ins_begin_state,&self->c_smin,sizeof(struct A2lState)));
  size = (size_t)(a2l_build_transition(buf,data,&ins_begin_state)-buf);
  self->c_smin = *data;
  goto ins_commit;
 }
 if (ins_end == self->c_code_pos) {
  /* Insert at the back. */
  assert(!memcmp(&ins_end_state,&self->c_smax,sizeof(struct A2lState)));
  size = (size_t)(a2l_build_transition(buf,&ins_begin_state,data)-buf);
  self->c_smax = *data;
  goto ins_commit;
 }
 if (ins_begin == self->c_code_last) {
  /* Update the last-code state. */
  assert(!memcmp(&ins_end_state,&self->c_slast,sizeof(struct A2lState)));
  self->c_slast = *data;
 }
 assert(ins_begin > self->c_code_begin);
 assert(ins_end   < self->c_code_pos);

 /* Generate a transition from 'ins_state' to 'data',
  * overwriting old/inserting new code from 'ins_begin' to 'ins_end'
  * NOTE: A second transition must also be generated for the following debug block!
  *    >> That second transition must shift from 'data' to 'ins_state'+1
  */
 {
  a2l_op_t *iter;
  iter = a2l_build_transition(buf,&ins_begin_state,data);
  iter = a2l_build_transition(iter,data,&ins_end_state);
  size = (size_t)(iter-buf);
 }
ins_commit:
 ins_size = (size_t)(ins_end-ins_begin);
 if (size == ins_size) {
  /* Simplified case: Old code has the same size as new. - Just overwrite it directly. */
  memcpy(ins_begin,buf,size);
 } else if (ins_size > size) {
  /* Old insert size is greater than the new.
   * >> Overwrite and deleting padding code. */
  memcpy(ins_begin,buf,size);
  ins_begin += size;
  ins_size  -= size;
  memmove(ins_begin,ins_begin+ins_size,
         (size_t)(self->c_code_pos-(ins_begin+ins_size)));
  self->c_code_pos  -= ins_size;
  self->c_code_last -= ins_size;
 } else {
  /* New size is greater than the old.
   * In this case, we must check available chunk
   * memory and allocate more if necessary. */
  ins_size = size-ins_size; /* Missing space. */
  while (self->c_code_pos+ins_size > self->c_code_end)
   if (!DCCA2lChunk_IncCode(self)) goto end;
  memmove(ins_begin+size,(ins_begin+size)-ins_size,
         (size_t)(self->c_code_pos-(ins_begin+(size-ins_size))));
  memcpy(ins_begin,buf,size);
  self->c_code_pos  += ins_size;
  self->c_code_last += ins_size;
 }
end:
 DCCA2lChunk_AssertIntegrity(self);
}

PRIVATE void
DCCA2lChunk_Clear(struct DCCA2lChunk *__restrict self,
                  target_ptr_t addr) {
 assert(self);
 free(self->c_code_begin);
 memset(self,0,sizeof(struct DCCA2lChunk));
 self->c_smin.s_addr =
 self->c_smax.s_addr = addr;
}
PRIVATE void
DCCA2lChunk_DeleteBefore(struct DCCA2lChunk *__restrict self,
                         target_ptr_t addr) {
 struct A2lState state;
 a2l_op_t *code;
 size_t shift;
 assert(self);
 if (addr <= self->c_smin.s_addr) return;
 if (addr >  self->c_smax.s_addr) {
  /* Clear this chunk. */
  DCCA2lChunk_Clear(self,self->c_smin.s_addr);
  return;
 }
 if (addr == self->c_smax.s_addr) {
  /* Special case: Delete everything but the last state. */
  state = self->c_smax;
  code  = self->c_code_pos;
 } else {
  state = self->c_smin;
  code  = self->c_code_begin;
  a2l_exec(&state,&state,
          (a2l_op_t const **)&code,
           addr);
 }
 assert(code >= self->c_code_begin);
 assert(code <= self->c_code_pos);
 assert(code != self->c_code_pos ||
       !memcmp(&state,&self->c_smax,sizeof(struct A2lState)));

 /* Delete all debug information about addresses below 'addr'. */
 self->c_smin = state;
 memmove(self->c_code_begin,code,
        (size_t)(self->c_code_pos-code));
 shift = (size_t)(code-self->c_code_begin);
 self->c_code_pos  -= shift;
 self->c_code_last -= shift;
 assert(self->c_code_pos >= self->c_code_begin);
 if (self->c_code_last < self->c_code_begin) {
  /* This can happen when the deleted portion includes 'c_code_last'.
   * In this case, assert that really everything is being deleted. */
  assert(self->c_code_pos == self->c_code_begin);
  assert(!memcmp(&self->c_smin,&self->c_smax,sizeof(struct A2lState)));
  self->c_code_last = self->c_code_begin;
 }
 assert(self->c_code_last >= self->c_code_begin);
 if (self->c_code_last == self->c_code_begin)
     self->c_slast = self->c_smin;
}

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4700)
#endif
PRIVATE void
DCCA2lChunk_DeleteAfter(struct DCCA2lChunk *__restrict self,
                        target_ptr_t addr) {
 struct A2lState prev_state,last_state,state;
 a2l_op_t *prev_code,*last_code,*code;
 assert(self);
 if (addr >  self->c_smax.s_addr) return;
 if (addr <= self->c_smin.s_addr) {
  /* Clear this chunk. */
  DCCA2lChunk_Clear(self,self->c_smin.s_addr);
  return;
 }
 last_state = self->c_smin;
 code       = self->c_code_begin;
 prev_code  = NULL;
 A2lState_RESET(&prev_state);
 for (;;) {
  last_code = code;
  assert(code >= self->c_code_begin);
  assert(code <= self->c_code_pos);
  a2l_exec(&last_state,&state,
          (a2l_op_t const **)&code,
           A2L_CAPTURE_ONE);
  if (state.s_addr >= addr) break;
  prev_state = last_state;
  prev_code  = last_code;
  last_state = state;
 }
 assert(last_code >= self->c_code_begin);
 assert(last_code <= self->c_code_pos);
 /* Delete all debug information about addresses after 'addr'. */
 self->c_slast     = prev_state;
 self->c_code_last = prev_code;
 self->c_smax      = last_state;
 self->c_code_pos  = last_code;
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif


PRIVATE void
DCCA2lChunk_DeleteRange(struct DCCA2lChunk *__restrict self,
                        target_ptr_t addr, target_siz_t size) {
 assert(self);
 if unlikely(!size) return;
 if (addr <= self->c_smin.s_addr) {
  DCCA2lChunk_DeleteBefore(self,addr+size);
 } else if (addr+size >= self->c_smax.s_addr) {
  DCCA2lChunk_DeleteAfter(self,addr);
 } else {
  struct A2lState state;
  struct A2lState delete_begin_state;
  a2l_op_t *delete_begin,*code;
  a2l_op_t trans[A2L_TRANSITION_MAXSIZE];
  size_t   trans_size,delete_size;
  /* Make sure the chunk isn't already empty. */
  if unlikely(self->c_code_begin == self->c_code_pos) return;
  /* Must actually search for what to delete... */
  delete_begin_state = self->c_smin;
  code               = self->c_code_begin;
  /* Find where we should start deleting. */
  for (;;) {
   delete_begin = code;
   a2l_exec(&delete_begin_state,&state,
           (a2l_op_t const **)&code,
            A2L_CAPTURE_ONE);
   if (state.s_addr >= addr) break;
   delete_begin_state = state;
  }
  /* NOTE: 'delete_begin' may be equal to 'self->c_code_begin' when
   *        debug information starting at the second capture point
   *        is supposed to be deleted.
   *       (Remember that the first capture point is described by
   *        'c_smin' and will only be created later during linking) */
  assertf(delete_begin >= self->c_code_begin &&
          delete_begin <  self->c_code_pos,
          "delete_begin        = %p\n"
          "code                = %p\n"
          "self->c_code_begin  = %p\n"
          "self->c_code_pos    = %p\n"
          "addr                = %p\n"
          "addr+size           = %p\n"
          "self->c_smin.s_addr = %p\n"
          "self->c_smax.s_addr = %p\n"
         ,delete_begin,code
         ,self->c_code_begin
         ,self->c_code_pos
         ,addr,addr+size
         ,self->c_smin.s_addr
         ,self->c_smax.s_addr);
  if (state.s_addr < addr+size)
      a2l_exec(&state,&state,(a2l_op_t const **)&code,addr+size);
  assertf(code >  self->c_code_begin &&
          code <= self->c_code_pos,
          "delete_begin        = %p\n"
          "code                = %p\n"
          "self->c_code_begin  = %p\n"
          "self->c_code_pos    = %p\n"
          "addr                = %p\n"
          "addr+size           = %p\n"
          "self->c_smin.s_addr = %p\n"
          "self->c_smax.s_addr = %p\n"
         ,delete_begin,code
         ,self->c_code_begin
         ,self->c_code_pos
         ,addr,addr+size
         ,self->c_smin.s_addr
         ,self->c_smax.s_addr);
  /* Generate a transition between 'delete_begin_state' and
   * 'state' that will bridge the gap between 'delete_begin'
   * and 'code'. */
  trans_size  = (size_t)(a2l_build_transition(trans,&delete_begin_state,&state)-trans);
  delete_size = (size_t)(code-delete_begin);
  /* Although this shouldn't happen, make sure the
   * transition isn't larger than the existing code. */
  if likely(trans_size <= delete_size) {
   /* When only the end capture point isn't deleted, that
    * also means that the previous point is being removed,
    * meaning that we must update its state to mirror the
    * new location it will point to once 'self->c_code_last'
    * is updated below. */
   if (code == self->c_code_pos)
       self->c_slast = delete_begin_state;
   memmove(delete_begin+trans_size,delete_begin+delete_size,
          (size_t)(self->c_code_pos-(delete_begin+delete_size)));
   memcpy(delete_begin,trans,trans_size);
   /* Re-adjust code pointers. */
   delete_size       -= trans_size;
   self->c_code_pos  -= delete_size;
   self->c_code_last -= delete_size;
  }
 }
}


/* Lookup, or allocate the chunk in which debug
 * information for 'addr' must be specified.
 * WARNING: The chunk may not have been initialized yet. */
PRIVATE struct DCCA2lChunk *
DCCA2l_GetChunkFor(struct DCCA2l *__restrict self,
                   target_ptr_t addr) {
 size_t index = addr/DCC_A2L_CHUNK_SIZE;
 assert(self);
 assert(self->d_chunkc <= self->d_chunka);
 if (index >= self->d_chunkc) {
  struct DCCA2lChunk *iter,*end;
  /* Must initialize and potentially allocate new chunks. */
  iter = self->d_chunkv;
  if (index >= self->d_chunka) {
   size_t newalloc = self->d_chunka;
   if (!newalloc) newalloc = 1;
   do newalloc *= 2; while (newalloc <= index);
   /* Re-allocate the chunk vector. */
   iter = (struct DCCA2lChunk *)realloc(iter,newalloc*
                                        sizeof(struct DCCA2lChunk));
   if unlikely(!iter) { DCC_AllocFailed(newalloc*sizeof(struct DCCA2lChunk)); return NULL; }
   self->d_chunkv = iter;
   self->d_chunka = newalloc;
  }
  end            = iter+(index+1);
  iter          += self->d_chunkc;
  self->d_chunkc = index+1;
  assert(iter < end);
  memset(iter,0,(size_t)(end-iter)*
         sizeof(struct DCCA2lChunk));
  /* Properly initialize all intermediate chunks
   * (including the result chunk itself) */
  assert(self->d_chunkv+index == end-1);
  for (; iter != end; ++iter) {
   iter->c_smin.s_addr =
   iter->c_smax.s_addr = (a2l_addr_t)(iter-self->d_chunkv)*
                                      DCC_A2L_CHUNK_SIZE;
  }
 }
 return self->d_chunkv+index;
}



PUBLIC void
DCCA2l_Quit(struct DCCA2l *__restrict self) {
 struct DCCA2lChunk *iter,*end;
 assert(self);
 end = (iter = self->d_chunkv)+self->d_chunkc;
 for (; iter != end; ++iter) free(iter->c_code_begin);
 free(self->d_chunkv);
}

PUBLIC /*inherited*/a2l_op_t *
DCCA2l_Link(struct DCCA2l const *__restrict self,
            size_t *__restrict code_size) {
 struct DCCA2lChunk *iter,*end;
 struct A2lState curr_state;
 a2l_op_t *result,*p;
 size_t result_size;
 assert(self);
 assert(code_size);
 /* Allocate for worst-case transition expense. */
 result_size = self->d_chunkc*A2L_TRANSITION_MAXSIZE;
 if unlikely(!result_size) { *code_size = 0; return NULL; }
 end = (iter = self->d_chunkv)+self->d_chunkc;
 for (; iter != end; ++iter) {
  DCCA2lChunk_AssertIntegrity(iter);
  result_size += (size_t)(iter->c_code_end-
                          iter->c_code_begin);
 }
 result_size += sizeof(a2l_op_t); /* EOF */
 p = result = (a2l_op_t *)malloc(result_size);
 if unlikely(!result) { DCC_AllocFailed(result_size); return NULL; }
 iter = self->d_chunkv;
 /* Start out with a RESET()-state, as required for linked A2L code. */
 A2lState_RESET(&curr_state);
 for (; iter != end; ++iter) {
  size_t codesize = (size_t)(iter->c_code_pos-iter->c_code_begin);
  /* Generate a state transition. */
  p = a2l_build_transition(p,&curr_state,&iter->c_smin);
  /* Copy all chunk code. */
  memcpy(p,iter->c_code_begin,codesize);
  p         += codesize;
  curr_state = iter->c_smax;
 }
 *p++ = A2L_O_EOF;
 assert(p <= result+result_size);
 /* Try to conserve some memory. */
 *code_size = (size_t)(p-result);
 p = (a2l_op_t *)realloc(result,(size_t)(p-result));
 if (p) result = p;
 return result;
}

PUBLIC void
DCCA2l_Import(struct DCCA2l *__restrict self,
              a2l_op_t *__restrict code,
              size_t code_size) {
 struct A2lState state;
 a2l_op_t *begin,*iter,*end;
 assert(self);
 assert(!code_size || code);
 assert(!self->d_chunkc);
 if unlikely(!code_size) return;
 /* TODO: Check if all address instructions move forward,
  *       and if they do, directly inherit the entirety
  *       of the given code. */
 iter = code;
 if (iter[code_size-1] != A2L_O_EOF) {
  /* Manually add EOF to the end. */
  iter = (a2l_op_t *)malloc(code_size+(1+A2L_ARG_MAXBYTES));
  if unlikely(!iter) { DCC_AllocFailed(code_size+(1+A2L_ARG_MAXBYTES)); return; }
  memcpy(iter,code,code_size);
  memset(iter+code_size,A2L_O_EOF,(1+A2L_ARG_MAXBYTES));
 }
 A2lState_RESET(&state);
 end = (begin = iter)+code_size;
 while (iter < end) {
  iter = a2l_exec1(&state,iter);
  DCCA2l_Insert(self,&state);
 }
 if (begin != code) free(begin);
}



PUBLIC void
DCCA2l_Merge(struct DCCA2l *__restrict self,
             struct DCCA2l *__restrict other,
             target_ptr_t other_base) {
 struct DCCA2lChunk *iter,*end;
 assert(self);
 assert(other);
 if (!other_base && !self->d_chunkc) {
  /* Simple case: Inherit everything. */
  *self = *other;
  memset(other,0,sizeof(struct DCCA2l));
  return;
 }
 /* xxx: This could be done better... */
 end = (iter = other->d_chunkv)+
               other->d_chunkc;
 for (; iter != end; ++iter) {
  struct A2lState state;
  a2l_op_t const *code,*code_end;
  code     = iter->c_code_begin;
  code_end = iter->c_code_pos;
#if 1
  if (code == code_end) {
   /* Skip empty chunks. */
   assert(!memcmp(&iter->c_smin,&iter->c_smax,sizeof(struct A2lState)));
   if (!iter->c_smin.s_features) continue;
  }
#endif
  assert(code <= code_end);
  state = iter->c_smin;
  /* Simply re-insert all A2L capture points. */
  for (;;) {
   assert(code <= code_end);
   state.s_addr += other_base;
   DCCA2l_Insert(self,&state);
   state.s_addr -= other_base;
   if (code == code_end) break;
   code = a2l_exec1(&state,code);
  }
  assertf(!memcmp(&state,&iter->c_smax,
                  sizeof(struct A2lState)),
          "             &state  |  &iter->c_smax\n"
          "s_line     = %p   %p\n"
          "s_col      = %p   %p\n"
          "s_path     = %p ('%s') %p ('%s')\n"
          "s_file     = %p ('%s') %p ('%s')\n"
          "s_name     = %p ('%s') %p ('%s')\n"
          "s_features = %p   %p\n"
          "s_addr     = %p   %p\n"
         ,state.s_line
         ,iter->c_smax.s_line
         ,state.s_col
         ,iter->c_smax.s_col
         ,state.s_path,dbgstr(state.s_path)
         ,iter->c_smax.s_path,dbgstr(iter->c_smax.s_path)
         ,state.s_file,dbgstr(state.s_file)
         ,iter->c_smax.s_file,dbgstr(iter->c_smax.s_file)
         ,state.s_name,dbgstr(state.s_name)
         ,iter->c_smax.s_name,dbgstr(iter->c_smax.s_name)
         ,state.s_features
         ,iter->c_smax.s_features
         ,state.s_addr
         ,iter->c_smax.s_addr
          );
 }
}

PUBLIC void
DCCA2l_RelocString(struct DCCA2l *__restrict self,
                   target_off_t offset) {
 struct DCCA2lChunk *iter,*end;
 assert(self);
 if unlikely(!offset) return; /* Nothing to do here! */
 end = (iter = self->d_chunkv)+self->d_chunkc;
 for (; iter != end; ++iter) {
  DCCA2lChunk_RelocString(iter,offset);
 }
}


PUBLIC void
DCCA2l_Mov(struct DCCA2l *__restrict self,
           target_ptr_t new_addr,
           target_ptr_t old_addr,
           target_siz_t n_bytes) {
 assert(self);
 if unlikely(!n_bytes) return;
 if unlikely(old_addr == new_addr) return;
 /* TODO: This one'll be quite hard. */
}

PUBLIC void
DCCA2l_Delete(struct DCCA2l *__restrict self,
              target_ptr_t addr, target_siz_t size) {
#if 0
 (void)self;
 (void)addr;
 (void)size;
#else
 struct DCCA2lChunk *c_iter,*c_end;
 target_ptr_t end = addr+size;
 assert(self);
 if unlikely(!size) return;
 (void)addr;
 /* TODO: This one'll be fairly simple. */
 c_iter = self->d_chunkv+(addr/DCC_A2L_CHUNK_SIZE);
 if (c_iter >= self->d_chunkv+self->d_chunkc ||
     c_iter < self->d_chunkv) return;
 c_end = self->d_chunkv+(end/DCC_A2L_CHUNK_SIZE);
 assert(c_end >= c_iter);
 if (c_iter == c_end) {
  /* Special case: Delete inside a single chunk. */
  DCCA2lChunk_DeleteRange(c_iter,addr,size);
  return;
 }
 DCCA2lChunk_DeleteAfter(c_iter,addr);
 if (c_end >= self->d_chunkv+self->d_chunkc ||
     c_end <  self->d_chunkv) {
clear_back:
  /* End is out-of-bounds. - Delete all full chunks after 'c_iter' */
  c_end = self->d_chunkv+self->d_chunkc;
  /* Mark all chunks following 'c_iter' as unused,
   * including 'c_iter' itself when it is empty. */
  self->d_chunkc = (c_iter-self->d_chunkv);
  if (c_iter->c_code_pos != c_iter->c_code_begin)
     ++self->d_chunkc;
  else free(c_iter->c_code_begin);
 } else {
  DCCA2lChunk_DeleteBefore(c_end,end);
  /* Check if we can clear chunks from the back. */
  if (c_end == (self->d_chunkv+self->d_chunkc)-1 &&
      c_iter->c_code_pos == c_iter->c_code_begin)
      goto clear_back;
  --c_end;
 }
 /* Cleanup all intermediate chunks. */
 for (; c_iter != c_end; ++c_iter) {
  free(c_iter->c_code_begin);
  memset(c_iter,0,sizeof(struct DCCA2lChunk));
  c_iter->c_smin.s_addr =
  c_iter->c_smax.s_addr = (a2l_addr_t)(c_iter-self->d_chunkv)*
                                       DCC_A2L_CHUNK_SIZE;
 }
#endif
}


PUBLIC void
DCCA2l_Insert(struct DCCA2l *__restrict self,
              struct A2lState const *__restrict data) {
 struct DCCA2lChunk *chunk;
 assert(self),assert(data);
 chunk = DCCA2l_GetChunkFor(self,data->s_addr);
 if likely(chunk) DCCA2lChunk_Insert(chunk,data);
}

PUBLIC int
DCCA2l_Lookup(struct DCCA2l const *__restrict self,
              struct A2lState *__restrict result) {
 struct DCCA2lChunk *chunk;
 a2l_addr_t addr; size_t index;
 a2l_op_t const *code;
 assert(self);
 assert(result);
 addr = result->s_addr;
 index = addr/DCC_A2L_CHUNK_SIZE;
 A2lState_RESET(result);
 if (index >= self->d_chunkc) return 0;
 chunk = self->d_chunkv+index;
 if (addr > chunk->c_smax.s_addr) return 0;
 *result = chunk->c_smin;
 if (chunk->c_code_pos == chunk->c_code_begin) return 0;
 /* Make sure the chunk's code is EOF-terminated. */
 if (chunk->c_code_pos == chunk->c_code_end &&
    !DCCA2lChunk_IncCode((struct DCCA2lChunk *)chunk)) return 0;
 assert(chunk->c_code_pos != chunk->c_code_end);
 /* NOTE: We don't actually commit this opcode! */
 *chunk->c_code_pos = A2L_O_EOF;
 code = chunk->c_code_begin;
 a2l_exec(result,NULL,&code,addr);
 return 1;
}

PUBLIC int
DCCA2l_LookupAdr(struct A2lState *__restrict result,
                 struct DCCSymAddr const *__restrict adr) {
 struct DCCSymAddr full_addr;
 assert(result);
 assert(adr);
 if unlikely(!adr->sa_sym) goto fail;
 /* Load the symbol address. (NOTE: Weak aliases are evaluated, too) */
 if unlikely(!DCCSym_LoadAddr(adr->sa_sym,&full_addr,1)) goto fail;
 assert(full_addr.sa_sym);
 assert(!full_addr.sa_sym->sy_alias);
 if unlikely(DCCSym_ISFORWARD(full_addr.sa_sym)) goto fail;
 /* Calculate the full symbol address by adding everything together. */
 full_addr.sa_off += adr->sa_off;
 if (!DCCSym_ISSECTION(adr->sa_sym))
      full_addr.sa_off += adr->sa_sym->sy_addr;
 result->s_addr = (a2l_addr_t)full_addr.sa_off;
 /* Lookup information about the generated A2L state in the associated section. */
 if (DCCA2l_Lookup(&adr->sa_sym->sy_sec->sc_a2l,result)) return 1;
fail:
 A2lState_RESET(result);
 return 0;
}
PUBLIC int
DCCA2l_LookupSym(struct A2lState *__restrict result,
                 struct DCCSym const *sym) {
 struct DCCSymAddr adr;
 adr.sa_off = 0;
 adr.sa_sym = (struct DCCSym *)sym;
 return DCCA2l_LookupAdr(result,&adr);
}


PUBLIC char const *
DCCA2lState_GetPath(struct A2lState const *__restrict self) {
 char const *result = dbgstr(self->s_path);
 if (!(self->s_features&A2L_STATE_HASPATH)) result = NULL;
 return result;
}
PUBLIC char const *
DCCA2lState_GetFile(struct A2lState const *__restrict self) {
 char const *result = dbgstr(self->s_file);
 if (!result || !(self->s_features&A2L_STATE_HASFILE)) result = "??" "?";
 return result;
}
PUBLIC char const *
DCCA2lState_GetName(struct A2lState const *__restrict self) {
 char const *result = dbgstr(self->s_name);
 if (!result || !(self->s_features&A2L_STATE_HASNAME)) result = "??" "?";
 return result;
}


PUBLIC void
DCCA2l_CaptureState(struct A2lState *__restrict result,
                    uint32_t features) {
 struct TPPFile *textfile;
 struct DCCSym *sym;
 textfile = TPPLexer_Textfile();
 assert(textfile);
 assert(textfile->f_kind == TPPFILE_KIND_TEXT);
 assert(unit.u_text);
 /* Special case: Don't generate A2L information for the empty file. */
 if unlikely(textfile == &TPPFile_Empty) return;
 memset(result,0,sizeof(struct A2lState));
 result->s_addr     = (a2l_addr_t)t_addr;
 result->s_features = features;
 if (features&A2L_STATE_HASPATH) {
  sym = DCCCompiler_GetPathName(textfile);
  if (sym) result->s_path      = sym->sy_addr;
  else     result->s_path      = 0,
           result->s_features &= ~(A2L_STATE_HASPATH);
 }
 if (features&A2L_STATE_HASFILE) {
  sym = DCCCompiler_GetFileName(textfile);
  if (sym) result->s_file      = sym->sy_addr;
  else     result->s_file      = 0,
           result->s_features &= ~(A2L_STATE_HASFILE);
 }
 if (features&A2L_STATE_HASNAME) {
  sym = DCCCompiler_GetFuncName();
  if (sym) result->s_name      = sym->sy_addr;
  else     result->s_name      = 0,
           result->s_features &= ~(A2L_STATE_HASNAME);
 }
 if (features&(A2L_STATE_HASLINE|A2L_STATE_HASCOL)) {
  struct TPPLCInfo lc_info;
  /* Lookup line/column information. */
  if (TOKEN.t_begin >= TOKEN.t_file->f_begin &&
      TOKEN.t_begin <= TOKEN.t_file->f_end &&
      /* Make sure not to use macro-locations!
       * (Calling 'TPPFile_LCAt' while 'TOKEN' is inside of a
       *  macro will query A2L information of the macro definition) */
      TOKEN.t_file->f_kind == TPPFILE_KIND_TEXT) {
   TPPFile_LCAt(TOKEN.t_file,&lc_info,TOKEN.t_begin);
  } else {
   TPPLexer_LC(&lc_info);
  }
  if (features&A2L_STATE_HASLINE) result->s_line = lc_info.lc_line;
  if (features&A2L_STATE_HASCOL) result->s_col = lc_info.lc_col;
 }
}

DCC_DECL_END

#endif /* !GUARD_DCC_ADDR2LINE_C */
