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
#ifndef GUARD_MAIN_C
#define GUARD_MAIN_C 1

#include <dcc/assembler.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/linker.h>
#include <dcc/stream.h>
#include <dcc/unit.h>
#include <dcc/preprocessor.h>

#include <string.h>
#include <stdio.h>

#include "dcc/cmd.h"

DCC_DECL_BEGIN

#ifdef __DCC_VERSION__
/* Hot-fix for broken system headers. TODO: REMOVE ME */
unsigned long __readfsdword(unsigned long) { return 0; }
#endif


INTDEF void dcc_dump_symbols(void);

INTERN void def(char const *name, target_ptr_t addr) {
 struct DCCSym *sym = DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE);
 if (sym) DCCSym_Define(sym,&DCCSection_Abs,addr,0,1);
}

static void add_import(char const *filename) {
 DCCUnit_Push();
 { /* Load a static library or source file. */
   struct DCCLibDef def;
   def.ld_flags    = (DCC_LIBDEF_FLAG_STATIC|
                      DCC_LIBDEF_FLAG_NODYN|
                      DCC_LIBDEF_FLAG_NOSEARCHSTD|
                      DCC_LIBDEF_FLAG_NOSEARCHEXT|
                      DCC_LIBDEF_FLAG_SOURCE);
   def.ld_name     = filename;
   def.ld_size     = strlen(filename);
   def.ld_expsymfa = (symflag_t)-1;
   def.ld_expsymfo = (symflag_t) 0;
   def.ld_impsymfa = (symflag_t)-1;
   def.ld_impsymfo = (symflag_t) 0;
   DCCUnit_Import(&def);
 }
 DCCUnit_Pop(OK); /* Merge if OK */
}

static void add_library(char const *filename) {
 /* Add a dynamic library dependency. */
 struct DCCLibDef def;
 def.ld_flags    = (DCC_LIBDEF_FLAG_NONE);
 def.ld_name     = filename;
 def.ld_size     = strlen(filename);
 def.ld_expsymfa = (symflag_t)-1;
 def.ld_expsymfo = (symflag_t) 0;
 def.ld_impsymfa = (symflag_t)-1;
 def.ld_impsymfo = (symflag_t) 0;
 DCCUnit_Import(&def);
}

static void save_object(char const *filename) {
 struct DCCExpDef def;
 def.ed_fmt   = DCC_EXPFMT_ELF;
 def.ed_flags = DCC_EXPFLAG_NONE;
 DCCUnit_Export(&def,filename);
}

static void tpp_clrfile(void) {
 struct TPPFile *pop_file;
 while (TOKEN.t_file != &TPPFile_Empty) {
  pop_file = TOKEN.t_file;
  TOKEN.t_file = pop_file->f_prev;
  TPPFile_Decref(pop_file);
 }
 TOKEN.t_begin =
 TOKEN.t_end   = TOKEN.t_file->f_pos;
 TOKEN.t_kwd   = NULL;
 if (TOKEN.t_id > 0) TOKEN.t_id = TOK_EOF;
}

#if DCC_TARGET_BIN == DCC_BINARY_PE
#define SO(x) x ".dll"
#else
#define SO(x) x ".so"
#endif
#define STDLIB(name,f) \
 {f,name,DCC_COMPILER_STRLEN(name),(symflag_t)-1,0,(symflag_t)-1,0,NULL}

static struct DCCLibDef default_stdlib[] = {
#if DCC_TARGET_OS == DCC_OS_WINDOWS || \
    DCC_TARGET_OS == DCC_OS_CYGWIN
 STDLIB(SO("msvcrt"),DCC_LIBDEF_FLAG_NOSEARCHEXT),
 STDLIB(SO("kernel32"),DCC_LIBDEF_FLAG_NOSEARCHEXT),
#else
 STDLIB(SO("libc"),DCC_LIBDEF_FLAG_NOSEARCHEXT),
#endif
 {0,NULL,0,0,0,0,0,NULL},
};

static struct DCCLibDef default_crt[] = {
 STDLIB("crt.o",DCC_LIBDEF_FLAG_INTERN|DCC_LIBDEF_FLAG_STATIC),
 {0,NULL,0,0,0,0,0,NULL},
};

static struct DCCLibDef default_crt_dbg[] = {
 STDLIB("dbg-crt.o",DCC_LIBDEF_FLAG_INTERN|DCC_LIBDEF_FLAG_STATIC),
 {0,NULL,0,0,0,0,0,NULL},
};

static void load_chain(struct DCCLibDef *chain) {
 for (; chain->ld_name; ++chain) {
  if (chain->ld_flags&DCC_LIBDEF_FLAG_STATIC) {
   DCCUnit_Push();
   DCCUnit_Import(chain);
   DCCUnit_Pop(OK);
  } else {
   DCCUnit_Import(chain);
  }
 }
}


static void load_stdlib(void) {
 if (!(linker.l_flags&DCC_LINKER_FLAG_NOSTDLIB) &&
     !(preproc.p_flags&DCC_PREPROCESSOR_FLAG_COMPILEONLY)) {
  /* Load default libraries. */
  load_chain(default_stdlib);
  if (linker.l_flags&DCC_LINKER_FLAG_GENDEBUG) {
   load_chain(default_crt_dbg);
  } else {
   load_chain(default_crt);
  }
 }
}

int main(int argc, char *argv[]) {
 int result = 0;

 /*_CrtSetBreakAlloc(33398);*/
 if (!TPP_INITIALIZE()) return 1;
 DCCPreprocessor_Init(&preproc);

 /* Initialize TPP callback hooks. */
 TPPLexer_Current->l_callbacks.c_parse_pragma     = &DCCParse_Pragma;
 TPPLexer_Current->l_callbacks.c_parse_pragma_gcc = &DCCParse_GCCPragma;
 TPPLexer_Current->l_callbacks.c_ins_comment      = &DCCParse_InsComment;

 DCCLinker_Init(&linker);
 DCCUnit_Init(&unit);
 TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_TERMINATE_STRING_LF
                              |TPPLEXER_FLAG_COMMENT_NOOWN_LF
#ifdef _WIN32
                              |TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
#endif
                               );
 if (argc) --argc,++argv;

 TPPLexer_PushFile(&TPPFile_Cmd);

 /* Parse the commandline. */
 { struct cmd c;
   cmd_init(&c,argc,argv);
   while (OK && cmd_yield(&c)) {
    /* True commandline options allow for a few more settings. */
    exec_cmd(&c,1);
   }
   //printf("argc     = %d\n",argc);
   //printf("argv     = %p\n",argv);
   //printf("c.c_argc = %d\n",c.c_argc);
   //printf("c.c_argv = %p\n",c.c_argv);
   argc = c.c_argc;
   argv = c.c_argv;
 }
 if (!OK) goto end;

 if (!preproc.p_outfile) {
  char **first_infile,**end,*name;
  end = (first_infile = argv)+argc;
  /* Deduce default output name from first source file. */
  while (first_infile != end &&
        (*first_infile)[0] == '-' &&
        (*first_infile)[1] == 'l') ++first_infile;
  if (first_infile == end ||
     (name = *first_infile,
     !strcmp(name,"-"))) name = NULL;
  DCCPreprocessor_OutAuto(name);
 }
 if (!(linker.l_flags&DCC_LINKER_FLAG_NOSTDINC))
       DCCLinker_AddSysPaths(preproc.p_outfile);
 if (preproc.p_flags&DCC_PREPROCESSOR_MASK_HELP) {
  struct cmd hc;
  hc.c_argc  = 0;
  hc.c_argv  = NULL;
  hc.c_arg   = NULL;
  hc.c_grp   = NULL;
  hc.c_state = 0;
  hc.c_id    = OPT_help;
  hc.c_val   = (preproc.p_flags&DCC_PREPROCESSOR_FLAG_HELPINC) ? (char *)"include-paths" :
               (preproc.p_flags&DCC_PREPROCESSOR_FLAG_HELPLIB) ? (char *)"library-paths" :
                                                                 (char *)"";
  exec_cmd(&hc,2);
 }
 if unlikely(!argc) WARN(W_LINKER_NO_INPUT_FILES);
 if (!OK) goto end;


 //dcc_dump_symbols();
 if (!OK) goto end;

 if (preproc.p_depfd != TPP_STREAM_INVALID) {
  TPPLexer_Current->l_callbacks.c_new_textfile = &DCCPreprocessor_DepNewTextfile;
  DCCPreprocessor_DepFirst();
 }
 /* Store the initial lexer flags. */
 preproc.p_baseflags = TPPLexer_Current->l_flags;

 assert(preproc.p_outfile);
 switch (DCC_PREPROCESSOR_PPMODE(preproc.p_flags)) {
 { /* Only generate dependencies/preprocessed code. */
  stream_t pp_out;
 case DCC_PREPROCESSOR_FLAG_PPDEP:
  pp_out = TPP_STREAM_INVALID;
  if (DCC_MACRO_FALSE) {
 case DCC_PREPROCESSOR_FLAG_PPPREPROC:
   if (!strcmp(preproc.p_outfile,"-"))
        pp_out = DCC_STREAM_STDOUT;
   else pp_out = s_openw(preproc.p_outfile);
  }
  TPPLexer_Current->l_callbacks.c_parse_pragma     = NULL;
  TPPLexer_Current->l_callbacks.c_parse_pragma_gcc = NULL;
  while (argc) {
   struct TPPFile *f;
   char *filename = argv[0];
   /* Parse the input code. */
   if (!strcmp(filename,"-"))
        f = TPPFile_OpenStream(DCC_STREAM_STDIN,"<stdin>");
   else f = TPPFile_Open(filename);
   if unlikely(!f) WARN(W_LIB_NOT_FOUND,filename,strlen(filename));
   else {
    assert(!f->f_textfile.f_usedname);
    if (preproc.p_srcname) {
     f->f_textfile.f_usedname = TPPString_New(preproc.p_srcname,
                                              strlen(preproc.p_srcname));
    }
    if (preproc.p_depfd != TPP_STREAM_INVALID)
        DCCPreprocessor_DepPrint(f->f_name,f->f_namesize);
    TPPLexer_PushFileInherited(f);
    if (DCC_PREPROCESSOR_PPMODE(preproc.p_flags) ==
        DCC_PREPROCESSOR_FLAG_PPPREPROC) {
     if (!(preproc.p_flags&DCC_PREPROCESSOR_FLAG_NOUNIFYPRGM)) {
      TPPLexer_Current->l_callbacks.c_parse_pragma     = &DCCPreprocessor_ReemitPragma;
      TPPLexer_Current->l_callbacks.c_parse_pragma_gcc = &DCCPreprocessor_ReemitGCCPragma;
     }
     DCCPreprocessor_PrintPP(pp_out);
    } else while (TPPLexer_Yield() > 0);
   }
   if (!OK) break;
   TPPLexer_Reset(TPPLexer_Current,
                  TPPLEXER_RESET_MACRO|TPPLEXER_RESET_ASSERT|
                  TPPLEXER_RESET_KWDFLAGS|TPPLEXER_RESET_COUNTER|
                  TPPLEXER_RESET_FONCE);
   ++argv,--argc;
  }
  if (pp_out != TPP_STREAM_INVALID &&
      pp_out != DCC_STREAM_STDOUT)
      s_close(pp_out);
 } break;

 { /* DEFAULT: Actually compile stuff. */
 default:
  load_stdlib();
  while (argc) {
   /* Parse the input code. */
   if (!memcmp(argv[0],"-l",2*sizeof(char)))
        add_library(argv[0]+2);
   else add_import(argv[0]);
   if (!OK) break;
   ++argv,--argc;
  }
 } break;

 }
 if (preproc.p_depfd != TPP_STREAM_INVALID)
     DCCPreprocessor_DepDummy();

 tpp_clrfile();

 if (DCC_PREPROCESSOR_PPMODE(preproc.p_flags) == DCC_PREPROCESSOR_FLAG_PPPREPROC ||
     DCC_PREPROCESSOR_PPMODE(preproc.p_flags) == DCC_PREPROCESSOR_FLAG_PPDEP)
     goto end;

 TPPLexer_PushFile(&TPPFile_Linker);
 if (!OK) goto end;

 /* Cleanup unused stuff. */
 DCCUnit_ClearStatic();

#ifdef DCC_LINKER_FLAG_PEDYNAMIC
 /* Dynamically export PE symbols. */
 /* TODO: This flag behaves similar to '--export-all-symbols'.
  *    >> Add support for that flag:
  *       https://linux.die.net/man/1/ld */
 linker.l_flags |= DCC_LINKER_FLAG_PEDYNAMIC;
#endif

 if (preproc.p_flags&DCC_PREPROCESSOR_FLAG_COMPILEONLY) {
  /* NOTE: Only clear obsolete symbols here, as they'd otherwise be
   *       cleared again by 'DCCLinker_Make' (which is unnecessary) */
  DCCUnit_ClearObsolete();
  save_object(preproc.p_outfile);
 } else {
  DCCUnit_MkDebugSym();
  if (!strcmp(preproc.p_outfile,"-")) {
   DCCLinker_Make(DCC_STREAM_STDOUT);
  } else {
   stream_t s_out;
   s_out = s_openw(preproc.p_outfile);
   DCCLinker_Make(s_out); /* Generate the binary. */
   s_close(s_out);
  }
 }
 tpp_clrfile();

#if 0 /* TODO: Direct execution mode. */
 {
  struct DCCSection *sec;
  struct DCCSym *entry_sym;
  void(*entry)(void);

  /* Ugly hack to get minimal stdio in tests. */
  def("printf",(target_ptr_t)(uintptr_t)(void *)&printf);
  //def("printf",(void *)0xDEADBEEF);

  /* Search for an entry point. */
  entry_sym = DCCUnit_GetSyms("__start");
  if (!entry_sym) {
   fprintf(stderr,"Entry point not found (Defaulting to start of .text)\n");
   entry_sym = &unit.u_text->sc_start;
   dcc_dump_symbols();
  }

  DCCSym_Incref(entry_sym);
  DCCUnit_ClearUnused();
  DCCUnit_ClearUnusedLibs();
  DCCSym_Decref(entry_sym);

  DCCUnit_ENUMSEC(sec) DCCSection_SetBase(sec);
  if (!OK) goto end;
  DCCUnit_ENUMSEC(sec) DCCSection_Reloc(sec);
  if (!OK) goto end;

  {
   struct DCCSymAddr entryaddr;
   if (!DCCSym_LoadAddr(entry_sym,&entryaddr,1)) {
    fprintf(stderr,"Entry point not found (Defaulting to start of .text)\n");
    entryaddr.sa_sym = &unit.u_text->sc_start;
    entryaddr.sa_off = 0;
    dcc_dump_symbols();
   }
   if (!OK) goto end;

   assert(DCCSym_SECTION(entryaddr.sa_sym));
   *(void **)&entry = (void *)(entryaddr.sa_off+entryaddr.sa_sym->sy_addr+
                               DCCSection_BASE(DCCSym_SECTION(entryaddr.sa_sym)));
  }

  /* Execute the generated code. */
  (*entry)();
 }
#endif

end:
 if (!OK) { fprintf(stderr,"A fatal error caused dcc to abort!\n"); }
 result = OK ? 0 : 1;
 //if (!OK) dcc_dump_symbols();

 DCCUnit_Quit(&unit);
 DCCLinker_Quit(&linker);
 DCCUnit_ClearCache();
 DCCPreprocessor_Quit(&preproc);
 TPP_FINALIZE();
#ifdef _CRTDBG_MAP_ALLOC
 _CrtDumpMemoryLeaks();
#endif
 return result;
}

DCC_DECL_END

#endif /* !GUARD_MAIN_C */
