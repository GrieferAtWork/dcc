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

#include <string.h>

DCC_DECL_BEGIN

A2L_IMPL a2l_op_t *a2l_setarg(a2l_op_t *code, a2l_arg_t arg) {
 while (arg > A2L_A_MAX) {
  *code++ = (a2l_op_t)(A2L_A_CON|(arg&A2L_A_MAX));
  arg -= A2L_A_MAX;
 }
 *code++ = (a2l_op_t)arg;
 return code;
}


PUBLIC void
DCCA2lWriter_Quit(struct DCCA2lWriter *__restrict self) {
 assert(self);
 free(self->w_cbegin);
}

PRIVATE void
a2l_write(struct DCCA2lWriter *__restrict self,
          void const *__restrict p, size_t s) {
 size_t avail;
 assert(self->w_state.s_code <= self->w_cend);
 avail = (size_t)(self->w_cend-self->w_state.s_code);
 if (avail < s) {
  size_t oldsize = (size_t)(self->w_state.s_code-self->w_cbegin);
  size_t newsize = (size_t)(self->w_cend-self->w_cbegin);
  a2l_op_t *newvec;
  if (!newsize) newsize = 2;
  do newsize *= 2; while (newsize < oldsize+s);
  newvec = (a2l_op_t *)realloc(self->w_cbegin,newsize*sizeof(a2l_op_t));
  if unlikely(!newvec) { DCC_AllocFailed(newsize*sizeof(a2l_op_t)); return; }
  self->w_cbegin       = newvec;
  self->w_state.s_code = newvec+oldsize;
  self->w_cend         = newvec+newsize;
 }
 memcpy(self->w_state.s_code,p,s);
 *(uintptr_t *)&self->w_state.s_code += s;
}

PRIVATE void
a2l_put0(struct DCCA2lWriter *__restrict self,
         a2l_op_t op) {
 a2l_write(self,&op,sizeof(a2l_op_t));
}
PRIVATE void
a2l_put1(struct DCCA2lWriter *__restrict self,
         a2l_op_t op, a2l_arg_t a1) {
 uint8_t buf[sizeof(a2l_op_t)+1*A2L_ARG_MAXBYTES];
 a2l_op_t *iter = (a2l_op_t *)buf;
 *iter++ = op;
 iter = a2l_setarg(iter,a1);
 a2l_write(self,buf,(size_t)(iter-buf));
}
PRIVATE void
a2l_put2(struct DCCA2lWriter *__restrict self,
         a2l_op_t op, a2l_arg_t a1, a2l_arg_t a2) {
 uint8_t buf[sizeof(a2l_op_t)+2*A2L_ARG_MAXBYTES];
 a2l_op_t *iter = (a2l_op_t *)buf;
 *iter++ = op;
 iter = a2l_setarg(iter,a1);
 iter = a2l_setarg(iter,a2);
 a2l_write(self,buf,(size_t)(iter-buf));
}
PRIVATE void
a2l_put3(struct DCCA2lWriter *__restrict self,
         a2l_op_t op, a2l_arg_t a1, a2l_arg_t a2, a2l_arg_t a3) {
 uint8_t buf[sizeof(a2l_op_t)+3*A2L_ARG_MAXBYTES];
 a2l_op_t *iter = (a2l_op_t *)buf;
 *iter++ = op;
 iter = a2l_setarg(iter,a1);
 iter = a2l_setarg(iter,a2);
 iter = a2l_setarg(iter,a3);
 a2l_write(self,buf,(size_t)(iter-buf));
}

#define PUT0(op)          a2l_put0(self,op)
#define PUT1(op,a1)       a2l_put1(self,op,a1)
#define PUT2(op,a1,a2)    a2l_put2(self,op,a1,a2)
#define PUT3(op,a1,a2,a3) a2l_put3(self,op,a1,a2,a3)

PUBLIC void
DCCA2lWriter_PutEx(struct DCCA2lWriter *__restrict self, target_ptr_t addr,
                   struct DCCA2lInfo const *__restrict info) {
#define STATE (self->w_state)
 assert(self);
 assert(info);
 /* Check for changes to PATH and FILE. */
 if (info->ai_features&A2L_STATE_HASPATH) {
  if (info->ai_path != STATE.s_path ||
    !(STATE.s_features&A2L_STATE_HASPATH)) {
   PUT1(A2L_O_SP,info->ai_path);
   STATE.s_path      = info->ai_path;
   STATE.s_features |= (A2L_STATE_HASPATH);
  }
 } else if (STATE.s_features&A2L_STATE_HASPATH) {
  STATE.s_path      = 0;
  STATE.s_features &= ~(A2L_STATE_HASPATH);
  PUT0(A2L_O_DEL_P);
 }
 if (info->ai_features&A2L_STATE_HASFILE) {
  if (info->ai_file != STATE.s_file ||
    !(STATE.s_features&A2L_STATE_HASFILE)) {
   PUT1(A2L_O_SF,info->ai_file);
   STATE.s_file      = info->ai_file;
   STATE.s_features |= (A2L_STATE_HASFILE);
  }
 } else if (STATE.s_features&A2L_STATE_HASFILE) {
  STATE.s_file      = 0;
  STATE.s_features &= ~(A2L_STATE_HASFILE);
  PUT0(A2L_O_DEL_F);
 }
 if (!(info->ai_features&A2L_STATE_HASLINE)) {
  /* Special handling for addr2line information without line. */
  if (STATE.s_features&A2L_STATE_HASLINE) {
   PUT0(A2L_O_DEL_L);
   STATE.s_features &= ~(A2L_STATE_HASLINE);
   STATE.s_line      = 0;
  }
  goto update_col;
 }
 if (addr == STATE.s_addr) {
  /* No change in address. - Manually update line & column. */
  if (info->ai_linecol.lc_line != STATE.s_line) {
   if (info->ai_linecol.lc_line > STATE.s_line)
        PUT1(A2L_O_IL,info->ai_linecol.lc_line-STATE.s_line);
   else PUT1(A2L_O_DL,STATE.s_line-info->ai_linecol.lc_line);
   STATE.s_line      = info->ai_linecol.lc_line;
   STATE.s_col       = 0;
   STATE.s_features |=  (A2L_STATE_HASLINE);
   STATE.s_features &= ~(A2L_STATE_HASCOL);
  }
  goto update_col;
 }

 /* At this point we know that:
  *  - A line is available
  *  - The address has changed */
 if (info->ai_linecol.lc_line != STATE.s_line) {
  /* Update addr+line[+col] at once (most common case). */
  if (info->ai_features&A2L_STATE_HASCOL) {
   a2l_op_t op = A2L_O_IL_SC_IA;
   a2l_arg_t addr_off;
   a2l_arg_t line_off;
   if (addr > STATE.s_addr) {
    addr_off = addr-STATE.s_addr;
   } else {
    op      += (A2L_O_IL_SC_DA-A2L_O_IL_SC_IA);
    addr_off = STATE.s_addr-addr;
   }
   if (info->ai_linecol.lc_line > STATE.s_line) {
    line_off = info->ai_linecol.lc_line-STATE.s_line;
   } else {
    op      += (A2L_O_DL_SC_IA-A2L_O_IL_SC_IA);
    line_off = STATE.s_line-info->ai_linecol.lc_line;
   }
   PUT3(op,line_off,info->ai_linecol.lc_col,addr_off);
   STATE.s_col       = info->ai_linecol.lc_col;
   STATE.s_features |= A2L_STATE_HASCOL;
  } else {
   /* Update addr+line */
   a2l_op_t op = A2L_O_IL_IA;
   a2l_arg_t addr_off;
   a2l_arg_t line_off;
   if (addr > STATE.s_addr) {
    addr_off = addr-STATE.s_addr;
   } else {
    op      += (A2L_O_IL_DA-A2L_O_IL_IA);
    addr_off = STATE.s_addr-addr;
   }
   if (info->ai_linecol.lc_line > STATE.s_line) {
    line_off = info->ai_linecol.lc_line-STATE.s_line;
   } else {
    op      += (A2L_O_DL_IA-A2L_O_IL_IA);
    line_off = STATE.s_line-info->ai_linecol.lc_line;
   }
   PUT2(op,line_off,addr_off);
   STATE.s_col       = 0;
   STATE.s_features &= ~(A2L_STATE_HASCOL);
  }
  STATE.s_line      = info->ai_linecol.lc_line;
  STATE.s_features |= A2L_STATE_HASLINE;
  STATE.s_addr      = addr;
  return;
 }
update_col:
 if (info->ai_features&A2L_STATE_HASCOL) {
  /* Set a new column value. */
  if (STATE.s_features&A2L_STATE_HASCOL) {
   if (info->ai_linecol.lc_col > STATE.s_col)
    PUT1(A2L_O_IC,info->ai_linecol.lc_col-STATE.s_col);
   else if (info->ai_linecol.lc_col < STATE.s_col)
    PUT1(A2L_O_DC,STATE.s_col-info->ai_linecol.lc_col);
  } else {
   PUT1(A2L_O_SC,info->ai_linecol.lc_col);
  }
  STATE.s_features |= A2L_STATE_HASCOL;
  STATE.s_col       = info->ai_linecol.lc_col;
 } else if (STATE.s_features&A2L_STATE_HASCOL) {
  STATE.s_col       = 0;
  STATE.s_features &= ~(A2L_STATE_HASCOL);
  PUT0(A2L_O_DEL_C);
 }
 if (addr != STATE.s_addr) {
  if (addr > STATE.s_addr)
       PUT1(A2L_O_IA,addr-STATE.s_addr);
  else PUT1(A2L_O_DA,STATE.s_addr-addr);
  STATE.s_addr = addr;
 }
#undef STATE
}

PUBLIC void
DCCA2lWriter_Merge(struct DCCA2lWriter *__restrict self,
                   struct DCCA2lWriter *__restrict other,
                   target_ptr_t other_base,
                   target_ptr_t string_base) {
 struct DCCA2lInfo addr_align;
 assert(self);
 assert(other);
 /* Nothing to do when 'other' is empty! */
 if (other->w_state.s_code == other->w_cbegin) return;
 if (self->w_state.s_code == self->w_cbegin) {
  if (!other_base) {
   if (string_base) goto fix_stringoff;
   /* Special case: Can directly override */
   free(self->w_cbegin),*self = *other;
   memset(other,0,sizeof(struct DCCA2lWriter));
   return;
  }
  /* TODO: This can be done better by searching
   *       through the generated code of 'other'
   *       and fixing the first address offset
   *       to include 'other_base'. */
 }
 memset(&addr_align,0,sizeof(struct DCCA2lInfo));
 /* Align the addr2line writer to the base address of 'other',
  * while resetting the A2L state machine to ZERO. */
 DCCA2lWriter_PutEx(self,other_base,&addr_align);
 if (!string_base) {
  /* Without a string base offset, we can
   * simply copy everything directly. */
  a2l_write(self,other->w_cbegin,
           (size_t)(other->w_state.s_code-other->w_cbegin));
 } else {
  a2l_op_t op,*flush_start,*iter,*end;
  /* Fix all path/file offsets. */
fix_stringoff:
  iter = other->w_cbegin;
  end  = other->w_state.s_code;
  flush_start = iter;
  while (iter != end) {
   op = *iter++;
   if (op == A2L_O_SP || op == A2L_O_SF) {
    a2l_arg_t orig_addr;
    /* Flush any unwritten data. */
    if (iter != flush_start)
        a2l_write(self,flush_start,(size_t)(iter-flush_start));
    orig_addr  = A2L_NAME(a2l_getarg)(&iter);
    /* Re-write the opcode with the updated string offset. */
    orig_addr += string_base;
    PUT1(op,orig_addr);

    flush_start = iter;
   } else {
    /* Simply skip all other opcodes. */
    unsigned int opc = A2L_GETOPC(op);
    while (opc--) A2L_NAME(a2l_getarg)(&iter);
   }
  }
  if (iter != flush_start) {
   a2l_write(self,flush_start,(size_t)(iter-flush_start));
  }
 }
}

#undef PUT3
#undef PUT2
#undef PUT1
#undef PUT0


PUBLIC void
DCCA2lWriter_Put(struct DCCA2lWriter *__restrict self, target_ptr_t addr) {
 struct DCCA2lInfo info;
 struct TPPFile *textfile;
 textfile = TPPLexer_Textfile();
 assert(textfile);
 assert(textfile->f_kind == TPPFILE_KIND_TEXT);
 /* Special case: Don't generate A2L information for the empty file. */
 if unlikely(textfile == &TPPFile_Empty) return;
 if (!textfile->f_textfile.f_dbg_pathaddr) {
  char const *path_begin,*path_end;
  /* Allocate a string for the file's path in 'unit.u_dbgstr' */
  path_end = (path_begin = textfile->f_name)+textfile->f_namesize;
  while (path_end != path_begin && (*path_end != '\\' &&
                                    *path_end != '/')
         ) --path_end;
  if (path_end == path_begin)
   /* This file has no associated path. */
   textfile->f_textfile.f_dbg_pathaddr = (target_ptr_t)-1;
  else {
   target_ptr_t name_addr;
   target_siz_t name_size = ((target_siz_t)(path_end-path_begin)+1)*sizeof(char);
   name_addr = DCCSection_DAllocMem(unit.u_dbgstr,path_begin,
                                  ((size_t)(path_end-path_begin))*sizeof(char),
                                    name_size,1,0);
   textfile->f_textfile.f_dbg_pathaddr = name_addr+1;
   /* Create a dangling reference to this string. */
   DCCSection_DIncref(unit.u_dbgstr,name_addr,name_size);
  }
 }
 if (!textfile->f_textfile.f_dbg_fileaddr) {
  char const *path_begin,*path_end,*file_begin;
  /* Allocate a string for the file's filename in 'unit.u_dbgstr' */
  path_end = file_begin = (path_begin = textfile->f_name)+textfile->f_namesize;
  while (file_begin != path_begin && (file_begin[-1] != '\\' &&
                                      file_begin[-1] != '/')
         ) --file_begin;
  if (file_begin == path_end)
   /* This file has no associated name. */
   textfile->f_textfile.f_dbg_fileaddr = (target_ptr_t)-1;
  else {
   target_ptr_t name_addr;
   target_siz_t name_size = ((target_siz_t)(path_end-file_begin)+1)*sizeof(char);
   name_addr = DCCSection_DAllocMem(unit.u_dbgstr,file_begin,
                                  ((size_t)(path_end-file_begin))*sizeof(char),
                                    name_size,1,0);
   textfile->f_textfile.f_dbg_fileaddr = name_addr+1;
   /* Create a dangling reference to this string. */
   DCCSection_DIncref(unit.u_dbgstr,name_addr,name_size);
  }
 }

 /* Lookup file/column information. */
 if (TOKEN.t_begin >= TOKEN.t_file->f_begin &&
     TOKEN.t_begin <= TOKEN.t_file->f_end) {
  TPPFile_LCAt(TOKEN.t_file,&info.ai_linecol,TOKEN.t_begin);
 } else {
  TPPLexer_LC(&info.ai_linecol);
 }
 info.ai_file     = textfile->f_textfile.f_dbg_fileaddr-1;
 info.ai_path     = textfile->f_textfile.f_dbg_pathaddr-1;
 info.ai_features = (A2L_STATE_HASLINE|A2L_STATE_HASCOL|
                     A2L_STATE_HASPATH|A2L_STATE_HASFILE);
 if (info.ai_path == (target_ptr_t)-2) info.ai_path = 0,info.ai_features &= ~(A2L_STATE_HASPATH);
 if (info.ai_file == (target_ptr_t)-2) info.ai_file = 0,info.ai_features &= ~(A2L_STATE_HASFILE);
 DCCA2lWriter_PutEx(self,addr,&info);
}

PRIVATE char const *dbgstr(target_ptr_t addr) {
 struct DCCSection *sec;
 char const *result = NULL;
 if ((sec = unit.u_dbgstr) == NULL)
      sec = DCCUnit_GetSecs(A2L_STRING_SECTION);
 if (sec && sec != unit.u_curr &&
     sec->sc_text.tb_begin+addr < sec->sc_text.tb_max) {
  char  *data = (char *)sec->sc_text.tb_begin+addr;
  size_t size = (size_t)((char *)sec->sc_text.tb_end-data);
  size = strnlen(data,size)+1;
  result = (char const *)DCCSection_GetText(sec,addr,size);
 }
 return result;
}

DCCFUN int
DCCA2lWriter_Lookup(struct DCCA2lWriter const *__restrict self, target_ptr_t addr,
                    struct DCCA2lAnswer *__restrict answer) {
 struct A2LState state; int result = 1;
 if (self->w_cbegin == self->w_cend)
  result = 0; /* Don't do anything below! */
 else {
  /* Make sure the A2L code is EOF-terminated. */
  if (self->w_state.s_code == self->w_cend)
       a2l_put0((struct DCCA2lWriter *)self,A2L_O_EOF),
       --((struct DCCA2lWriter *)self)->w_state.s_code;
  else *self->w_state.s_code = A2L_O_EOF;
 }
 A2LState_INIT(&state,self->w_cbegin);
 answer->aa_col  = answer->aa_line = 0;
 answer->aa_path = answer->aa_file = NULL;
 if (result) result = A2L_NAME(a2l_exec)(&state,addr);
 if (result) {
  if (state.s_features&A2L_STATE_HASLINE) answer->aa_line = state.s_line;
  if (state.s_features&A2L_STATE_HASCOL)  answer->aa_col  = state.s_col;
  if (state.s_features&A2L_STATE_HASPATH) answer->aa_path = dbgstr(state.s_path);
  if (state.s_features&A2L_STATE_HASFILE) {
   answer->aa_file = dbgstr(state.s_file);
   /* Must reload the other debug string in case the
    * section memory was re-allocated during text lookup. */
   if (state.s_features&A2L_STATE_HASPATH)
       answer->aa_path = dbgstr(state.s_path);
  }
 }
 if (!answer->aa_path) answer->aa_path = "??" "?";
 if (!answer->aa_file) answer->aa_file = "??" "?";
 return 1;
}



DCC_DECL_END

#endif /* !GUARD_DCC_ADDR2LINE_C */
