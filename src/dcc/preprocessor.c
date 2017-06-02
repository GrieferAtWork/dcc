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
#ifndef GUARD_DCC_PREPROCESSOR_C
#define GUARD_DCC_PREPROCESSOR_C 1

#include <dcc/common.h>
#include <dcc/preprocessor.h>
#include <dcc/stream.h>

#include <string.h>
#include <stdlib.h>
#include "cmd.h"

DCC_DECL_BEGIN

PUBLIC struct DCCPreprocessor DCCPreprocessor_Current;
PUBLIC void
DCCPreprocessor_OutAuto(char const *first_infile) {
 assert(!preproc.p_outfile);
 assert(!(preproc.p_flags&DCC_PREPROCESSOR_FLAG_OWNSOUTFILE));
 if (DCC_PREPROCESSOR_PPMODE(preproc.p_flags) == DCC_PREPROCESSOR_FLAG_PPPREPROC) {
  /* In preprocessor mode, default to stdout ("-") as output. */
  preproc.p_outfile = "-";
 } else if (first_infile) {
  char const *ext,*newext;
  size_t infile_len,newext_len;
  char *output_name;
  ext = strrchr(first_infile,'.');
  if (!ext) ext = first_infile+strlen(first_infile);
  infile_len = (size_t)(ext-first_infile);
  newext = (preproc.p_flags&DCC_PREPROCESSOR_FLAG_COMPILEONLY) ? DCC_OUTFILE_EXTOBJ : DCC_OUTFILE_EXTEXE;
  newext_len = strlen(newext);
  output_name = (char *)DCC_Malloc((infile_len+newext_len+1)*sizeof(char),0);
  if unlikely(!output_name) return;
  memcpy(output_name,first_infile,infile_len*sizeof(char));
  memcpy(output_name+infile_len,newext,newext_len*sizeof(char));
  output_name[infile_len+newext_len] = '\0';
  preproc.p_outfile = output_name;
  preproc.p_flags  |= DCC_PREPROCESSOR_FLAG_OWNSOUTFILE;
 } else {
  /* Automatically deduce the output name. */
  if (!(preproc.p_flags&DCC_PREPROCESSOR_FLAG_COMPILEONLY))
       preproc.p_outfile = DCC_OUTFILE_STDEXE;
  else preproc.p_outfile = DCC_OUTFILE_STDOBJ;
 }
 preproc.p_flags |= DCC_PREPROCESSOR_FLAG_OUTNAMEAUTO;
}


PRIVATE void ppdep_printfile(char const *__restrict name, size_t size) {
 /* TODO: Escape special characters, such as ' ' or '\\' */
 s_writea(preproc.p_depfd,name,size*sizeof(char));
}
PRIVATE void ppdep_printfile_escapemake(char const *__restrict name, size_t size) {
 /* TODO: Escape special characters, such as '$' */
 ppdep_printfile(name,size);
}


PUBLIC void
DCCPreprocessor_Init(struct DCCPreprocessor *__restrict self) {
 assert(self);
 memset(self,0,sizeof(struct DCCPreprocessor));
 self->p_depfd         = TPP_STREAM_INVALID;
 self->p_depfd_maxline = DCC_PREPROCESSOR_DEFAULT_DEPFD_MAXLINE;
}
PUBLIC void
DCCPreprocessor_Quit(struct DCCPreprocessor *__restrict self) {
 char **iter,**end;
 assert(self);
 if (self->p_depfd != TPP_STREAM_INVALID &&
     self->p_depfd != DCC_STREAM_STDOUT)
     s_close(self->p_depfd);
 end = (iter = self->p_depfv)+self->p_depfc;
 for (; iter != end; ++iter) free(*iter);
 free(self->p_depfv);
 if (self->p_flags&DCC_PREPROCESSOR_FLAG_OWNSOUTFILE)
     free((char *)self->p_outfile);
}

PUBLIC void
DCCPreprocessor_DepAdd(char const *__restrict name, size_t size) {
 char *copy,**newvec;
 /* No need to track these if we're not supposed to emit dummy targets. */
 if (!(preproc.p_flags&DCC_PREPROCESSOR_FLAG_DEPDUMMY)) return;
 copy = (char *)malloc((size+1)*sizeof(char));
 if unlikely(!copy) { DCC_AllocFailed((size+1)*sizeof(char)); return; }
 newvec = preproc.p_depfv;
 if (preproc.p_depfc == preproc.p_depfa) {
  if (!preproc.p_depfa) preproc.p_depfa = 1;
  preproc.p_depfa *= 2;
  newvec = (char **)realloc(newvec,preproc.p_depfa*sizeof(char *));
  if unlikely(!newvec) { DCC_AllocFailed(preproc.p_depfa*sizeof(char *)); return; }
  preproc.p_depfv = newvec;
 }
 newvec[preproc.p_depfc++] = copy;
 memcpy(copy,name,size*sizeof(char));
 copy[size] = '\0';
}

PUBLIC void
DCCPreprocessor_DepPrint(char const *__restrict name, size_t size) {
 assert(preproc.p_depfd != TPP_STREAM_INVALID);
 if ((preproc.p_depfd_curline += (size+1)) >= preproc.p_depfd_maxline) {
  s_writea(preproc.p_depfd," \\\n\t",4*sizeof(char));
  preproc.p_depfd_curline = 0;
 } else {
  s_writea(preproc.p_depfd," ",1*sizeof(char));
 }
 ppdep_printfile(name,size);
}
PUBLIC void DCCPreprocessor_DepFirst(void) {
 assert(preproc.p_depfd != TPP_STREAM_INVALID);
 assert(preproc.p_outfile);
 if (preproc.p_deptarget) {
  (preproc.p_flags&DCC_PREPROCESSOR_FLAG_DEPESCAPE)
   ? ppdep_printfile_escapemake(preproc.p_deptarget,strlen(preproc.p_deptarget))
   : ppdep_printfile           (preproc.p_deptarget,strlen(preproc.p_deptarget));
 } else {
  ppdep_printfile_escapemake(preproc.p_outfile,strlen(preproc.p_outfile));
 }
 s_writea(preproc.p_depfd,":",1*sizeof(char));
}
PUBLIC void DCCPreprocessor_DepDummy(void) {
 char **iter,**end,*filename;
 assert(preproc.p_depfd != TPP_STREAM_INVALID);
 end = (iter = preproc.p_depfv)+preproc.p_depfc;
 for (; iter != end; ++iter) {
  filename = *iter;
  s_writea(preproc.p_depfd,"\n\n",2*sizeof(char));
  ppdep_printfile_escapemake(filename,strlen(filename));
  s_writea(preproc.p_depfd,":",sizeof(char));
 }
 s_writea(preproc.p_depfd,"\n",1*sizeof(char));
}

struct pp_state {
 struct TPPFile *last_token_file;
 char           *last_token_end;
 size_t          file_offset;
 int             is_at_linefeed;
 int             current_line;
 stream_t        out;
};

LOCAL size_t get_file_offset(char *p) {
 struct TPPFile *f = TPPLexer_Current->l_token.t_file;
 size_t result = p-f->f_begin;
 if (f->f_kind == TPPFILE_KIND_TEXT) {
  result += (f->f_textfile.f_rdata-f->f_text->s_size);
 }
 return result;
}
LOCAL int
count_linefeeds(char const *iter, char const *end) {
 int result = 0;
 while (iter != end) {
  if (*iter == '\r') {
   if (iter != end-1 &&
       iter[1] == '\n') ++iter;
   ++result;
  } else if (*iter == '\n') ++result;
  ++iter;
 }
 return result;
}
static int out_printer(char const *buf, size_t bufsize, void *closure) {
 s_writea((stream_t)closure,buf,bufsize);
 return 0;
}
void put_line(struct pp_state *__restrict s) {
 static char const *last_filename = NULL;
 size_t      filename_size;
 char const *filename_text;
 struct TPPFile *f;
 int line; char buffer[16];
 if (DCC_PREPROCESSOR_LINEMODE(preproc.p_flags) ==
     DCC_PREPROCESSOR_FLAG_LINENONE) return;
 f = TPPLexer_Textfile();
 if (TPPLexer_Current->l_token.t_file == f) {
  /* Try to use the start of the current token.
   * NOTE: Something like 'f->f_oldpos' would be more
   *       appropriate to use, but we don't track that... */
  line = TPPFile_LineAt(f,TPPLexer_Current->l_token.t_begin);
 } else {
  /* Fallback: Use the current position within the file. */
  line = TPPFile_LineAt(f,f->f_pos);
 }
 filename_text = TPPFile_Filename(f,&filename_size);
 if (s->current_line == line && last_filename == filename_text) return;
 if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) !=
     DCC_PREPROCESSOR_FLAG_TOKSEPERATE &&
   !(preproc.p_flags&DCC_PREPROCESSOR_FLAG_NOMAGICLF) &&
     last_filename == filename_text && s->current_line <= line-1 &&
     s->current_line >= line-2) {
  /* Optimization: For smaller line-offsets of less than 2, it is usually
   *               easier to simply emit the linefeeds individually.
   * WARNING: We can't do this in ZERO-mode though, as in this mode linefeeds
   *          must only be emit when they actually exist
   */
  size_t offset = line-s->current_line;
  s->current_line = line;
  switch (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags)) {
  case DCC_PREPROCESSOR_FLAG_TOKOUTLINE : s_writea(s->out,"[\n][\n]",(offset*3)*sizeof(char)); break;
//case DCC_PREPROCESSOR_FLAG_TOKSEPERATE: s_writea(s->out,"\n\0\n\0",(offset*2)*sizeof(char)); break;
  default                               : s_writea(s->out,"\n\n",offset*sizeof(char)); break;
  }
  return;
 }
 if (!s->is_at_linefeed) s_writea(s->out,"\n",sizeof(char));
 s->current_line = line;
 if (DCC_PREPROCESSOR_LINEMODE(preproc.p_flags) ==
     DCC_PREPROCESSOR_FLAG_LINECXX) {
  s_writea(s->out,"# ",2*sizeof(char));
 } else {
  s_writea(s->out,"#line ",6*sizeof(char));
 }
 s_writea(s->out,buffer,(TPP_Itos(buffer,(TPP(int_t))(line+1))-buffer)*sizeof(char));
 if (last_filename != filename_text) {
  char *quote_buffer;
  size_t quote_size;
  last_filename = filename_text;
  s_writea(s->out," \"",2);
  quote_size = TPP_SizeofEscape(filename_text,filename_size);
  quote_buffer = (char *)malloc(quote_size*sizeof(char));
  if (quote_buffer) {
   TPP_Escape(quote_buffer,filename_text,filename_size);
   s_writea(s->out,quote_buffer,quote_size*sizeof(char));
   free(quote_buffer);
  }
  s_writea(s->out,DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
                  DCC_PREPROCESSOR_FLAG_TOKSEPERATE
           ? "\"\0" : "\"\n",2*sizeof(char));
 } else {
  s_writea(s->out,DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
                  DCC_PREPROCESSOR_FLAG_TOKSEPERATE
           ? "\0" : "\n",sizeof(char));
 }
 s->is_at_linefeed = 1;
}
static void pp_emit_raw(struct pp_state *__restrict s) {
 s->last_token_file = TPPLexer_Current->l_token.t_file;
 s->last_token_end  = TPPLexer_Current->l_token.t_end;
 s->file_offset     = get_file_offset(s->last_token_end);
 if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
     DCC_PREPROCESSOR_FLAG_TOKOUTLINE)
     s_writea(s->out,"[",sizeof(char));
 if (!(preproc.p_flags&DCC_PREPROCESSOR_FLAG_NODECODETOK)) {
  TPP_PrintToken(&out_printer,(void *)s->out);
  if (TPPLexer_Current->l_token.t_id == '\n') ++s->current_line;
 } else {
  s_writea(s->out,TPPLexer_Current->l_token.t_begin,
          (size_t)(TPPLexer_Current->l_token.t_end-
                   TPPLexer_Current->l_token.t_begin)*
           sizeof(char));
  /* Track what we expect the current line number to be,
   * which is them compared to the actual line number. */
  s->current_line += count_linefeeds(TPPLexer_Current->l_token.t_begin,
                                     TPPLexer_Current->l_token.t_end);
 }
 switch (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags)) {
  case DCC_PREPROCESSOR_FLAG_TOKSEPERATE: s_writea(s->out,"\0",sizeof(char)); s->is_at_linefeed = 1; break;
  case DCC_PREPROCESSOR_FLAG_TOKOUTLINE:  s_writea(s->out,"]",sizeof(char));  s->is_at_linefeed = 0; break;
  default: 
   s->is_at_linefeed = s->last_token_end[-1] == '\n' ||
                       s->last_token_end[-1] == '\r';
   break;
 }
}
static void pp_emit(struct pp_state *__restrict s) {
 if (s->last_token_file != TPPLexer_Current->l_token.t_file ||
    (s->last_token_end != TPPLexer_Current->l_token.t_begin &&
     s->file_offset != get_file_offset(TPPLexer_Current->l_token.t_begin))) {
  /* The file changed, or there is a difference in the in-file position
   * between the end of the last token and the start of this one.
   * >> In any case, we must update the #line offset. */
  put_line(s);
 }
 pp_emit_raw(s);
}

PRIVATE struct pp_state *curr_state = NULL;

DCCFUN void
DCCPreprocessor_PrintPP(stream_t out) {
 struct pp_state s,*old_state;
 /* Initial values to simulate the last token
  * ending where the first file starts. */
 s.last_token_file = NULL; // infile; /* Force a line directive at the first token. */
 s.last_token_end  = TPPLexer_Current->l_token.t_file->f_begin;
 s.file_offset     = 0;
 s.current_line    = 0;
 s.is_at_linefeed  = 1;
 s.out             = out;
 old_state = curr_state;
 curr_state = &s;
 while (TPPLexer_Yield() > 0) pp_emit(&s);
 if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
     DCC_PREPROCESSOR_FLAG_TOKSEPERATE) {
  s_writea(out,"\0",sizeof(char));
 }
 curr_state = old_state;
}


PUBLIC struct TPPFile *
DCCPreprocessor_DepUnknown(char *filename,
                           size_t filename_size) {
 DCCPreprocessor_DepAdd(filename,filename_size);
 DCCPreprocessor_DepPrint(filename,filename_size);
 return NULL;
}

PUBLIC int
DCCPreprocessor_DepNewTextfile(struct TPPFile *file,
                               int is_system_header) {
 /* Don't emit entries for system header if we're not supposed to. */
 if (is_system_header && !(preproc.p_flags&DCC_PREPROCESSOR_FLAG_DEPSYSTEM)) return 1;
 DCCPreprocessor_DepAdd(file->f_name,file->f_namesize);
 DCCPreprocessor_DepPrint(file->f_name,file->f_namesize);
 return 1;
}

PRIVATE int reemit_pragma(int gcc_pragma) {
#define PRAGMA_COPYMASK  (TPPLEXER_FLAG_WANTCOMMENTS|\
                          TPPLEXER_FLAG_WANTSPACE|\
                          TPPLEXER_FLAG_WANTLF)
#define PRINT(s)   s_writea(curr_state->out,s,sizeof(s)-sizeof(char))
#define PRINTZ(s)  s_writea(curr_state->out,s,sizeof(s))
#define PRINTZX(s) s_writea(curr_state->out,s,(DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) == DCC_PREPROCESSOR_FLAG_TOKSEPERATE) ? sizeof(s) : sizeof(s)-sizeof(char))
 if unlikely(!curr_state) return 0; /*  */
 if (!curr_state->is_at_linefeed &&
      DCC_PREPROCESSOR_LINEMODE(preproc.p_flags) !=
      DCC_PREPROCESSOR_FLAG_LINENONE) {
  if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
      DCC_PREPROCESSOR_FLAG_TOKOUTLINE)
       PRINT("[\n]");
  else PRINTZX("\n");
  ++curr_state->current_line;
 }
 if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
     DCC_PREPROCESSOR_FLAG_TOKOUTLINE) {
  PRINT("[#][pragma]");
  if (preproc.p_baseflags&TPPLEXER_FLAG_WANTSPACE) PRINT("[ ]");
  if (gcc_pragma) {
   PRINT("[GCC]");
   if (preproc.p_baseflags&TPPLEXER_FLAG_WANTSPACE) PRINT("[ ]");
  }
 } else {
  if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
      DCC_PREPROCESSOR_FLAG_TOKSEPERATE)
       PRINTZ("#\0pragma");
  else PRINT("#pragma");
  if (preproc.p_baseflags&TPPLEXER_FLAG_WANTSPACE) PRINTZX(" ");
  if (gcc_pragma) {
   PRINTZX("GCC");
   if (preproc.p_baseflags&TPPLEXER_FLAG_WANTSPACE) PRINTZX(" ");
  }
 }
 TPPLexer_Current->l_flags &= ~(PRAGMA_COPYMASK);
 TPPLexer_Current->l_flags |= (preproc.p_baseflags&PRAGMA_COPYMASK);
 do pp_emit_raw(curr_state); while (TPPLexer_Yield() > 0);
 if (DCC_PREPROCESSOR_TOKMODE(preproc.p_flags) ==
     DCC_PREPROCESSOR_FLAG_TOKOUTLINE)
      PRINT("[\n]");
 else PRINTZX("\n");
 ++curr_state->current_line;
 curr_state->is_at_linefeed = 1;
 curr_state->last_token_file = NULL;
 curr_state->last_token_end  = NULL;
 return 1;
}
PUBLIC int DCCPreprocessor_ReemitPragma(void) { return reemit_pragma(0); }
PUBLIC int DCCPreprocessor_ReemitGCCPragma(void) { return reemit_pragma(1); }




DCC_DECL_END

#endif /* !GUARD_DCC_PREPROCESSOR_C */
