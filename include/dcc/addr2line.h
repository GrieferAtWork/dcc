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
#ifndef GUARD_DCC_ADDR2LINE_H
#define GUARD_DCC_ADDR2LINE_H 1

#include "common.h"
#include "lexer.h"
#include "../../lib/src/addr2line.h"

#include <stdint.h>

DCC_DECL_BEGIN

#define DCC_A2L_CHUNK_SIZE 512

struct DCCA2lChunk {
 /* A2L data chunk (covering at most 'DCC_A2L_CHUNK_SIZE' bytes of text).
  * HINT: During linking, A2L chunks are merged and additional code may be
  *       generated to ensure proper transitions from the previous chunk's
  *      'c_smax' to the next chunk's 'c_smin' A2L state.
  */
 struct A2LState     c_smin;       /*< [.s_addr <= c_smin.s_addr] The state required for the start of this chunk. */
 struct A2LState     c_smax;       /*< [.s_addr >= c_smin.s_addr] The state required for the end of this chunk.
                                    *  NOTE: This state represents the A2L state at 'c_code.c_pos'. */
 struct A2LState     c_slast;      /*< [valid_if(c_code_last != c_code.c_pos)] The state before 'c_code_last'. */
 A2L_TYPE(a2l_op_t) *c_code_begin; /*< [!][0..1][owned] Begin address of allocated A2L code.
                                    *   WARNING: This code vector does is _NOT_ necessarily terminated with an 'A2L_O_EOF' op.
                                    *   NOTE: This code vector must _NEVER_ contain instructions that decrement the A2L address! */
 A2L_TYPE(a2l_op_t) *c_code_last;  /*< [?][0..1][>= c_code_begin] Address of the last written state change at the end.
                                    *   This pointer is used to speed up changes to debug informations after no code was generated. */
 A2L_TYPE(a2l_op_t) *c_code_pos;   /*< [?][0..1][>= c_code_last] Current code position, and the pointer to the next opcode. */
 A2L_TYPE(a2l_op_t) *c_code_end;   /*< [?][0..1][>= c_code_pos] Allocated end address. */
};



struct DCCA2l {
 size_t              d_chunka; /*< Allocated amount of chunks. */
 size_t              d_chunkc; /*< [<= d_chunka] Greatest chunk index+1. */
 struct DCCA2lChunk *d_chunkv; /*< [0..d_chunkc|alloc(d_chunka)][owned][sort(<,->c_smin.s_addr)]
                                *  [require([i].c_smin.s_addr >= (i  )*DCC_A2L_CHUNK_SIZE &&
                                *           [i].c_smin.s_addr <  (i+1)*DCC_A2L_CHUNK_SIZE)]
                                *   Strictly ordered & sorted vector of chunks. */
};

#define DCCA2l_Init(self) memset(self,0,sizeof(struct DCCA2lWriter))
DCCFUN void DCCA2l_Quit(struct DCCA2l *__restrict self);


/* Merge 'other' offset from 'other_base' into 'self'.
 * NOTE: During this process, 'other' may be modified, but will remain in a valid state.
 * NOTE: In addition to this, 'string_base' is added to all path/file operands. */
DCCFUN void
DCCA2l_Merge(struct DCCA2l *__restrict self,
             struct DCCA2l *__restrict other,
             DCC(target_ptr_t) other_base);

/* Relocate all string offsets by adding 'offset' to each. */
DCCFUN void
DCCA2l_RelocString(struct DCCA2l *__restrict self,
                   DCC(target_off_t) offset);

/* Move all addr2line debug informations from
 * 'old_addr..+=n_bytes' to 'new_addr..+=n_bytes' */
DCCFUN void
DCCA2l_Mov(struct DCCA2l *__restrict self,
           DCC(target_ptr_t) new_addr,
           DCC(target_ptr_t) old_addr,
           DCC(target_siz_t) n_bytes);

/* Delete all A2L debug informations inside the given address range. */
DCCFUN void
DCCA2l_Delete(struct DCCA2l *__restrict self,
              DCC(target_ptr_t) addr,
              DCC(target_siz_t) size);

/* Insert debug data at a given address 'data->s_addr'.
 * NOTE: Data inserted will affect all addresses '>= data->s_addr' and '< next_greater_descriptor()'.  */
DCCFUN void DCCA2l_Insert(struct DCCA2l *__restrict self,
                          struct A2LState const *__restrict data);

/* Link all A2L code and return it.
 * @return: NULL: Failed to allocate enough memory (a warning has been emit).
 * @return: *:    Malloc'ed pointer to the A2L pseudo code describing debug informations.
 *                The code has been generated to work with a RESET() initial state. */
DCCFUN /*inherited*/A2L_TYPE(a2l_op_t) *
DCCA2l_Link(struct DCCA2l const *__restrict self,
            size_t *__restrict code_size);

/* Lookup address information.
 * NOTE: Only 'result->s_addr' must be initialized upon entry.
 * @return: 0: No debug information was available for the given address.
 *            'result' has been initialized to either a RESET()-state,
 *             or the state of the last address that was still recognized.
 * @return: 1: Managed to find the descriptor associated with 'result->s_addr'. */
DCCFUN int DCCA2l_Lookup(struct DCCA2l const *__restrict self,
                         struct A2LState *__restrict result);


/* Capture the current lexer state as an A2L state.
 * NOTE: The generated state will offset at most 'features',
 *       which is a set of 'A2L_STATE_*', though some
 *       features may be deducted based on the current context.
 * NOTE: 'result->s_addr' is set to 't_addr'. */
DCCFUN void
DCCA2l_CaptureState(struct A2LState *__restrict result,
                    uint32_t features);




DCC_DECL_END

#endif /* !GUARD_DCC_ADDR2LINE_H */
