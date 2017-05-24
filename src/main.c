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

#include <string.h>
#include <stdio.h>

#include "dcc/cmd.h"

DCC_DECL_BEGIN

INTDEF void dcc_dump_symbols(void);

void def(char const *name, void *addr) {
 struct DCCSym *sym = DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE);
 if (sym) DCCSym_Define(sym,&DCCSection_Abs,(target_ptr_t)addr,0);
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
 if (linker.l_flags&DCC_LINKER_FLAG_OBJCLRUNUSED) {
  DCCUnit_ClearUnused();
  DCCUnit_ClearUnusedLibs();
 }
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
 TOKEN.t_id    = TOK_EOF;
}

int main(int argc, char *argv[]) {
 struct DCCSection *sec;
 int result = 0;
 char const *outfile_name = NULL;
#define F_COMPILEONLY  0x00000001
#define F_PREPROCESSOR 0x00000002
 uint32_t flags = 0;

 if (!TPP_INITIALIZE()) return 1;

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
    switch (c.c_id) {

    case OPT_E: flags |= F_PREPROCESSOR; break;
    case OPT_o: outfile_name = c.c_val; break;
    case OPT_c: flags |= F_COMPILEONLY; break;
     /* TODO: '-B' changes the internal library path. */

    default: exec_cmd(&c,1); break;
    }
   }
   argc = c.c_argc;
   argv = c.c_argv;
 }

 if unlikely(!argc) WARN(W_LINKER_NO_INPUT_FILES);
 if (!OK) goto end;

 if (!outfile_name) {
  /* TODO: deduce default output name from first source file? */
  if (!(flags&F_COMPILEONLY))
       outfile_name = DCC_OUTFILE_STDEXE;
  else outfile_name = DCC_OUTFILE_STDOBJ;
 }

 //_CrtSetBreakAlloc(5941);
 DCCLinker_AddSysPaths(outfile_name);

 if (!(linker.l_flags&DCC_LINKER_FLAG_NOSTDLIB) &&
     !(flags&F_COMPILEONLY)) {
  /* Load default libraries. */
#define STDLIB(name,f) \
   {f,name,DCC_COMPILER_STRLEN(name),(symflag_t)-1,0,(symflag_t)-1,0,NULL}
  static struct DCCLibDef default_stdlib[] = {
   STDLIB("crt1.o",DCC_LIBDEF_FLAG_INTERN|DCC_LIBDEF_FLAG_STATIC),
#if DCC_TARGET_OS == DCC_OS_WINDOWS
   STDLIB("msvcrt.dll",DCC_LIBDEF_FLAG_NOSEARCHEXT),
#else
   STDLIB("libc.so",DCC_LIBDEF_FLAG_NOSEARCHEXT),
#endif
   {0,NULL,0,0,0,0,0,NULL},
  };
#undef STDLIB
  struct DCCLibDef *chain = default_stdlib;
  for (; chain->ld_name; ++chain) DCCUnit_Import(chain);
 }

 //dcc_dump_symbols();
 if (!OK) goto end;

 /* TODO: Preprocessor mode ('-E'). */

 while (argc) {
  /* Parse the input code. */
  if (!memcmp(argv[0],"-l",2*sizeof(char)))
       add_library(argv[0]+2);
  else add_import(argv[0]);
  if (!OK) goto end;
  ++argv,--argc;
 }

 tpp_clrfile();
 TPPLexer_PushFile(&TPPFile_Linker);

 /* Cleanup unused stuff. */
 DCCUnit_ClearStatic();

#ifdef DCC_LINKER_FLAG_PEDYNAMIC
 /* Dynamically export PE symbols. */
 linker.l_flags |= DCC_LINKER_FLAG_PEDYNAMIC;
#endif

 if (flags&F_COMPILEONLY)
     save_object(outfile_name);
 else {
  stream_t s_out;
  /* Prepare generated code for output to file. */
  DCCUnit_ENUMSEC(sec) DCCSection_ResolveDisp(sec);
  s_out = s_openw(outfile_name);
  DCCLinker_Make(s_out); /* Generate the binary. */
  s_close(s_out);
 }
 tpp_clrfile();

#if 0 /* TODO: Direct execution mode. */
 {
  struct DCCSym *entry_sym;
  void(*entry)(void);

  /* Ugly hack to get minimal stdio in tests. */
  def("printf",(void *)&printf);
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
   *(void **)&entry = (void *)(entryaddr.sa_off+
                               entryaddr.sa_sym->sy_addr+
                               DCCSym_SECTION(entryaddr.sa_sym)->sc_base);
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
 TPP_FINALIZE();
#ifdef _CRTDBG_MAP_ALLOC
 _CrtDumpMemoryLeaks();
#endif
 return result;
}

DCC_DECL_END

#endif /* !GUARD_MAIN_C */
