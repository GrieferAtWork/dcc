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

DCC_DECL_BEGIN

struct DCCA2lWriter {
 struct A2LState     w_state;  /*< The current a2l state, as it will be during execution. */
 A2L_TYPE(a2l_op_t) *w_cbegin; /*< [0..1] Base address of allocated A2L code.
                                *   NOTE: When  */
 A2L_TYPE(a2l_op_t) *w_cend;   /*< [0..1] End address of allocated A2L code. */
};

struct DCCA2lInfo {
 A2L_TYPE(a2l_path_t) ai_path;     /*< Offset in 'unit.u_dbgstr' to the path of the current file. */
 A2L_TYPE(a2l_file_t) ai_file;     /*< Offset in 'unit.u_dbgstr' to the path of the current file. */
 struct TPPLCInfo     ai_linecol;  /*< Line & column information compatible with TPP. */
 uint32_t             ai_features; /*< Available a2l features (Set of 'A2L_STATE_HAS*'). */
};

#define     DCCA2lWriter_Init(self) memset(self,0,sizeof(struct DCCA2lWriter))
DCCFUN void DCCA2lWriter_Quit(struct DCCA2lWriter *__restrict self);


/* Generate debug information pointing 'addr'
 * to the current source location from 'tpp'. */
DCCFUN void DCCA2lWriter_Put(struct DCCA2lWriter *__restrict self, target_ptr_t addr);

/* Similar to 'DCCA2lWriter_Put', but generate information for 'info' instead. */
DCCFUN void DCCA2lWriter_PutEx(struct DCCA2lWriter *__restrict self, target_ptr_t addr,
                               struct DCCA2lInfo const *__restrict info);

/* Merge 'other' offset from 'other_base' into 'self'.
 * NOTE: During this process, 'other' may be modified,
 *       but will remain in a valid state.
 * NOTE: In addition to this, 'string_base' is added to all path/file operands. */
DCCFUN void DCCA2lWriter_Merge(struct DCCA2lWriter *__restrict self,
                               struct DCCA2lWriter *__restrict other,
                               target_ptr_t other_base,
                               target_ptr_t string_base);


struct DCCA2lAnswer {
 char const *aa_path; /*< [1..1] Path of the associated file, or "." for CWD, or "???" when 'aa_file' is unknown. */
 char const *aa_file; /*< [1..1] Name of the associated file, or "???" when unknown. */
 DCC(line_t) aa_line; /*< 1-based lined index within the file, or ZERO(0) when unknown. */
 DCC(col_t)  aa_col;  /*< 1-based lined column within the file, or ZERO(0) when unknown. */
};


/* Lookup A2L information about 'addr' within the given A2L writer 'self'.
 * @return: 0: No debug information was available for the given address.
 * @return: 1: Some, or all fields in '*answer' were filled with proper data.
 * NOTE: In all cases, all fields in '*answer' are always filled in.
 */
DCCFUN int
DCCA2lWriter_Lookup(struct DCCA2lWriter const *__restrict self, target_ptr_t addr,
                    struct DCCA2lAnswer *__restrict answer);



DCC_DECL_END

#endif /* !GUARD_DCC_ADDR2LINE_H */
