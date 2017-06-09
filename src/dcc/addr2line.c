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

#include <stdio.h>
#include <string.h>

DCC_DECL_BEGIN

A2L_IMPL a2l_op_t *a2l_setarg(a2l_op_t *code, a2l_arg_t arg) {
 a2l_arg_t byte,shift;
 while (arg > A2L_A_MAX) {
  byte = arg,shift = 0;
  do byte >>= A2L_A_SFT,shift += A2L_A_SFT;
  while (byte > A2L_A_MAX);
  *code++ = (a2l_op_t)(A2L_A_CON|(byte&A2L_A_MAX));
  arg &= ~(((a2l_arg_t)-1) << shift);
 }
 *code++ = (a2l_op_t)arg;
 return code;
}


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
 self->c_code_last    = newbuf+(self->c_code_last-self->c_code_begin);
 self->c_code_end   = newbuf+newsize;
 self->c_code_begin = newbuf;
 return 1;
}



PRIVATE a2l_op_t *
a2l_exec1(struct A2lState *__restrict state,
          a2l_op_t const *__restrict code) {
 DCC_a2l_exec(state,&code,state->s_addr+1);
 return (a2l_op_t *)code;
}


/* Max amount of code generated by a worst-case transition. */
#define A2L_TRANSITION_MAXSIZE \
 ((1+A2L_ARG_MAXBYTES)*3 /* path, file, name */\
 +(1+A2L_ARG_MAXBYTES*3) /* addr+line+col (worst case: change all 3; all other cases are smaller) */)


PRIVATE a2l_op_t *
a2l_build_transition(a2l_op_t *__restrict buf,
                     struct A2lState const *__restrict old_state,
                     struct A2lState const *__restrict new_state) {
#define O(x) (void)(*buf++ = (x))
#define A(a) (void)(buf = a2l_setarg(buf,a))
 assert(buf);
 assert(old_state);
 assert(new_state);

 /* Update PATH */
 if (old_state->s_features&A2L_STATE_HASPATH) {
  if (new_state->s_features&A2L_STATE_HASPATH) {
   if (old_state->s_path != new_state->s_path) goto set_path;
  } else O(A2L_O_DEL_P);
 } else if (new_state->s_features&A2L_STATE_HASPATH) {
set_path:
  O(A2L_O_SP);
  A(new_state->s_path);
 }

 /* Update FILE */
 if (old_state->s_features&A2L_STATE_HASFILE) {
  if (new_state->s_features&A2L_STATE_HASFILE) {
   if (old_state->s_file != new_state->s_file) goto set_file;
  } else O(A2L_O_DEL_F);
 } else if (new_state->s_features&A2L_STATE_HASFILE) {
set_file:
  O(A2L_O_SF);
  A(new_state->s_file);
 }

 /* Update NAME */
 if (old_state->s_features&A2L_STATE_HASNAME) {
  if (new_state->s_features&A2L_STATE_HASNAME) {
   if (old_state->s_name != new_state->s_name) goto set_name;
  } else O(A2L_O_DEL_N);
 } else if (new_state->s_features&A2L_STATE_HASNAME) {
set_name:
  O(A2L_O_SN);
  A(new_state->s_name);
 }

 if (!(new_state->s_features&A2L_STATE_HASLINE)) {
  /* Special handling for addr2line information without line. */
  if (old_state->s_features&A2L_STATE_HASLINE) O(A2L_O_DEL_L);
  goto update_col;
 }
 if (new_state->s_addr == old_state->s_addr) {
  /* No change in address. - Manually update line & column. */
  if (new_state->s_line != old_state->s_line) {
   if (new_state->s_line > old_state->s_line)
        O(A2L_O_IL),A(new_state->s_line-old_state->s_line);
   else O(A2L_O_DL),A(old_state->s_line-new_state->s_line);
  }
  goto update_col;
 }

 /* At this point we know that:
  *  - A line is available
  *  - The address has changed */
 if (new_state->s_line != old_state->s_line) {
  /* Update addr+line[+col] at once (most common case). */
  if (new_state->s_features&A2L_STATE_HASCOL) {
   a2l_op_t op = A2L_O_IL_SC_IA;
   a2l_arg_t addr_off,line_off;
   if (new_state->s_addr > old_state->s_addr) {
    addr_off = new_state->s_addr-old_state->s_addr;
   } else {
    op      += (A2L_O_IL_SC_DA-A2L_O_IL_SC_IA);
    addr_off = old_state->s_addr-new_state->s_addr;
   }
   if (new_state->s_line > old_state->s_line) {
    line_off = new_state->s_line-old_state->s_line;
   } else {
    op      += (A2L_O_DL_SC_IA-A2L_O_IL_SC_IA);
    line_off = old_state->s_line-new_state->s_line;
   }
   assert(A2L_GETOPC(op) == 3);
   O(op),A(line_off),A(new_state->s_col),A(addr_off);
  } else {
   /* Update new_state->s_addr+line */
   a2l_op_t op = A2L_O_IL_IA;
   a2l_arg_t addr_off,line_off;
   if (new_state->s_addr > old_state->s_addr) {
    addr_off = new_state->s_addr-old_state->s_addr;
   } else {
    op      += (A2L_O_IL_DA-A2L_O_IL_IA);
    addr_off = old_state->s_addr-new_state->s_addr;
   }
   if (new_state->s_line > old_state->s_line) {
    line_off = new_state->s_line-old_state->s_line;
   } else {
    op      += (A2L_O_DL_IA-A2L_O_IL_IA);
    line_off = old_state->s_line-new_state->s_line;
   }
   assert(A2L_GETOPC(op) == 2);
   O(op),A(line_off),A(addr_off);
  }
  return buf;
 }
update_col:
 if (new_state->s_features&A2L_STATE_HASCOL) {
  /* Set a new column value. */
  if (old_state->s_features&A2L_STATE_HASCOL) {
   if (new_state->s_col > old_state->s_col)
    O(A2L_O_IC),A(new_state->s_col-old_state->s_col);
   else if (new_state->s_col < old_state->s_col)
    O(A2L_O_DC),A(old_state->s_col-new_state->s_col);
  } else {
   O(A2L_O_IC),A(new_state->s_col);
  }
 } else if (old_state->s_features&A2L_STATE_HASCOL) {
  O(A2L_O_DEL_C);
 }
 if (new_state->s_addr != old_state->s_addr) {
  if (new_state->s_addr > old_state->s_addr)
       O(A2L_O_IA),A(new_state->s_addr-old_state->s_addr);
  else O(A2L_O_DA),A(old_state->s_addr-new_state->s_addr);
 }
 return buf;
#undef A
#undef O
}



/* Lookup the boundry pointers of code specifying
 * debug information about 'new_state->s_addr', or when '*ins_begin'
 * and '*ins_end' are both set to the same pointer,
 * the insert address of where new code describing
 * a transition from 'ins_state' to whichever state
 * the caller desires must be inserted at! */
PRIVATE void
DCCA2lChunk_GetInsPtr(struct DCCA2lChunk *__restrict self,
                      a2l_addr_t addr,
                      a2l_op_t **__restrict ins_begin,
                      a2l_op_t **__restrict ins_end,
                      struct A2lState *__restrict ins_state) {
 a2l_op_t *code,*newcode;
 assert(self);
 assert(ins_begin);
 assert(ins_end);
 assert(addr >= self->c_smin.s_addr);
 assert(addr <= self->c_smax.s_addr);
 *ins_state = self->c_smin;
 code = self->c_code_begin;
 for (;;) {
  assert(ins_state->s_addr <= addr);
  newcode = a2l_exec1(ins_state,code);
  if (ins_state->s_addr >= addr) {
   *ins_begin = code;
   /* When the last capture opcode describes 'addr' directly,
    * use the opcode's end for '*ins_end' in order to override
    * its meaning with new debug information inside the caller.
    * Otherwise, insert new debug info by setting '*ins_end'
    * equal '*ins_begin'. */
   *ins_end = (ins_state->s_addr == addr) ? newcode : code;
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


DCCFUN void
DCCA2lChunk_RelocString(struct DCCA2lChunk *__restrict self,
                        target_off_t offset) {
 a2l_op_t *arg_end,op,new_arg[A2L_ARG_MAXBYTES];
 size_t new_argsize,old_argsize,old_last;
 a2l_arg_t arg;
 assert(self);
 assert(self->c_code_pos <= self->c_code_end);
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
  if (op == A2L_O_SP || op == A2L_O_SF || op == A2L_O_SN) {
   arg_end     = self->c_code_last;
   arg         = DCC_a2l_getarg((a2l_op_t const **)&arg_end);
   old_argsize = (size_t)(arg_end-self->c_code_last);
   arg        += offset;
   new_argsize = (size_t)(a2l_setarg(new_arg,arg)-new_arg);
   if (old_argsize == new_argsize) {
    /* Simple case: Override the argument directly. */
    memcpy(self->c_code_last,new_arg,new_argsize);
   } else if (old_argsize > new_argsize) {
    /* Shift the code vector downwards. */
    memcpy(self->c_code_last,new_arg,new_argsize);
    memmove(self->c_code_last+new_argsize,arg_end,
           (size_t)(self->c_code_pos-arg_end));
    old_argsize -= new_argsize;
    self->c_code_pos -= old_argsize;
    old_last         -= old_argsize;
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
    DCC_a2l_getarg((a2l_op_t const **)&self->c_code_last);
   }
  }
 }
 self->c_code_last = self->c_code_begin+old_last;
}


PRIVATE void
DCCA2lChunk_Insert(struct DCCA2lChunk *__restrict self,
                   struct A2lState const *__restrict data) {
 a2l_op_t buf[A2L_TRANSITION_MAXSIZE];
 size_t size,ins_size; a2l_op_t *ins_begin,*ins_end;
 struct A2lState ins_state;
 assert(self);
 assert(data);
 if (self->c_code_pos == self->c_code_begin &&
    !self->c_smin.s_features &&
    !self->c_smax.s_features) {
  /* Special case: The chunk is empty, meaning we are allowed to
   *               specify anything we want for min/max states! */
  self->c_smax = self->c_smin = *data;
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
    if (!DCCA2lChunk_IncCode(self)) return;
   memcpy(self->c_code_pos,buf,size);
   self->c_code_pos += size;
  }
  self->c_smax = *data;
  return;
 }

 if (data->s_addr > self->c_smax.s_addr) {
  /* Simple case: Append at the end.
   * Can easily be handled by transitioning from the current end to the new. */
  size = (size_t)(a2l_build_transition(buf,&self->c_smax,data)-buf);
  if (size) {
   while (self->c_code_pos+size > self->c_code_end)
    if (!DCCA2lChunk_IncCode(self)) return;
   memcpy(self->c_code_pos,buf,size);
   self->c_slast     = self->c_smax;
   self->c_code_last = self->c_code_pos;
   self->c_code_pos += size;
  }
  self->c_smax = *data;
  return;
 }
 if (data->s_addr < self->c_smin.s_addr) {
  /* Special case: Prepend before the chunk's start. */
  size = (size_t)(a2l_build_transition(buf,data,&self->c_smin)-buf);
  if (size) {
   while (self->c_code_pos+size > self->c_code_end)
    if (!DCCA2lChunk_IncCode(self)) return;
   memmove(self->c_code_begin+size,self->c_code_begin,
          (size_t)(self->c_code_pos-self->c_code_begin));
   memcpy(self->c_code_begin,buf,size);
   self->c_code_last += size;
   self->c_code_pos  += size;
  }
  self->c_smin = *data;
  return;
 }
 DCCA2lChunk_GetInsPtr(self,data->s_addr,&ins_begin,&ins_end,&ins_state);
 assert(ins_end >= ins_begin);
 assert(ins_begin >= self->c_code_begin);
 assert(ins_end   <= self->c_code_pos);
 /* Generate a transition from 'ins_state' to 'data',
  * overwriting old/inserting new code from 'ins_begin' to 'ins_end' */
 size = (size_t)(a2l_build_transition(buf,&ins_state,data)-buf);
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
   if (!DCCA2lChunk_IncCode(self)) return;
  memmove(ins_begin+size,(ins_begin+size)-ins_size,
         (size_t)(self->c_code_pos-(ins_begin+(size-ins_size))));
  memcpy(ins_begin,buf,size);
  self->c_code_pos  += ins_size;
  self->c_code_last += ins_size;
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
 struct A2lState s;
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
 A2lState_RESET(&s);
 end = (begin = iter)+code_size;
 while (iter < end) {
  iter = a2l_exec1(&s,iter);
  DCCA2l_Insert(self,&s);
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
 end = (iter = other->d_chunkv)+other->d_chunkc;
 for (; iter != end; ++iter) {
  struct A2lState state;
  a2l_op_t const *code,*code_end;
  code     = iter->c_code_begin;
  code_end = iter->c_code_pos;
  assert(code <= code_end);
  if (code == code_end) continue;
  state = iter->c_smin;
  /* Simply re-insert all A2L capture points. */
  do {
   assert(code < code_end);
   code = a2l_exec1(&state,code);
   state.s_addr += other_base;
   DCCA2l_Insert(self,&state);
   state.s_addr -= other_base;
  } while (code != code_end);
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
 assert(self);
 if unlikely(!size) return;
 (void)addr;
 /* TODO: This one'll be fairly simple. */
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
 DCC_a2l_exec(result,&code,addr);
 return 1;
}


/* Returns the PATH id or '-2' if the file doesn't have a path. */
LOCAL a2l_path_t TPPFile_GetA2LPath(struct TPPFile *__restrict self) {
 assert(self);
 assert(self->f_kind == TPPFILE_KIND_TEXT);
 if (!self->f_textfile.f_dbg_pathaddr) {
  char const *path_begin,*path_end;
  /* Allocate a string for the file's path in 'unit.u_dbgstr' */
  path_end = (path_begin = self->f_name)+self->f_namesize;
  while (path_end != path_begin && (*path_end != '\\' &&
                                    *path_end != '/')
         ) --path_end;
  if (path_end == path_begin)
   /* This file has no associated path. */
   self->f_textfile.f_dbg_pathaddr = (target_ptr_t)-1;
  else {
   target_ptr_t name_addr;
   target_siz_t name_size = ((target_siz_t)(path_end-path_begin)+1)*sizeof(char);
   name_addr = DCCSection_DAllocMem(unit.u_dbgstr,path_begin,
                                  ((size_t)(path_end-path_begin))*sizeof(char),
                                    name_size,1,0);
   self->f_textfile.f_dbg_pathaddr = name_addr+1;
   /* Create a dangling reference to this string. */
   DCCSection_DIncref(unit.u_dbgstr,name_addr,name_size);
  }
 }
 return self->f_textfile.f_dbg_pathaddr-1;
}

/* Returns the FILE id or '-2' if the file doesn't have a name. */
LOCAL a2l_file_t TPPFile_GetA2LFile(struct TPPFile *__restrict self) {
 assert(self);
 assert(self->f_kind == TPPFILE_KIND_TEXT);
 if (!self->f_textfile.f_dbg_fileaddr) {
  char const *path_begin,*path_end,*file_begin;
  /* Allocate a string for the file's filename in 'unit.u_dbgstr' */
  path_end = file_begin = (path_begin = self->f_name)+self->f_namesize;
  while (file_begin != path_begin && (file_begin[-1] != '\\' &&
                                      file_begin[-1] != '/')
         ) --file_begin;
  if (file_begin == path_end)
   /* This file has no associated name. */
   self->f_textfile.f_dbg_fileaddr = (target_ptr_t)-1;
  else {
   target_ptr_t name_addr;
   target_siz_t name_size = ((target_siz_t)(path_end-file_begin)+1)*sizeof(char);
   name_addr = DCCSection_DAllocMem(unit.u_dbgstr,file_begin,
                                  ((size_t)(path_end-file_begin))*sizeof(char),
                                    name_size,1,0);
   self->f_textfile.f_dbg_fileaddr = name_addr+1;
   /* Create a dangling reference to this string. */
   DCCSection_DIncref(unit.u_dbgstr,name_addr,name_size);
  }
 }
 return self->f_textfile.f_dbg_fileaddr-1;
}



PUBLIC void
DCCA2l_CaptureState(struct A2lState *__restrict result,
                    uint32_t features) {
 struct TPPFile *textfile;
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
  result->s_path = TPPFile_GetA2LPath(textfile);
  if (result->s_path == (a2l_path_t)-2) {
   result->s_path      = 0;
   result->s_features &= ~(A2L_STATE_HASPATH);
  }
 }
 if (features&A2L_STATE_HASFILE) {
  result->s_file = TPPFile_GetA2LFile(textfile);
  if (result->s_file == (a2l_path_t)-2) {
   result->s_file      = 0;
   result->s_features &= ~(A2L_STATE_HASFILE);
  }
 }
 if (features&A2L_STATE_HASNAME) {
  /* TODO: Function name. */
  result->s_features &= ~(A2L_STATE_HASNAME);
 }
 if (features&(A2L_STATE_HASLINE|A2L_STATE_HASCOL)) {
  struct TPPLCInfo lc_info;
  /* Lookup line/column information. */
  if (TOKEN.t_begin >= TOKEN.t_file->f_begin &&
      TOKEN.t_begin <= TOKEN.t_file->f_end) {
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
