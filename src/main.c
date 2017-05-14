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

#include <dcc/assembler.h>
#include <dcc/unit.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/binary.h>
#include <dcc/stream.h>

#include <string.h>
#include <stdio.h>


void def(char const *name, void *addr) {
 struct DCCSym *sym = DCCUnit_NewSyms(name,DCC_SYMFLAG_NONE);
 if (sym) DCCSym_Define(sym,&DCCSection_Abs,(target_ptr_t)addr,0);
}

void dump_symbols(void) {
 struct DCCSym *sym;
 DCCUnit_ENUMSYM(sym) {
#if DCC_TARGET_BIN == DCC_BINARY_PE
  if (sym->sy_flags&DCC_SYMFLAG_DLLIMPORT) printf("IMPORT ");
  if (sym->sy_flags&DCC_SYMFLAG_DLLEXPORT) printf("EXPORT ");
#endif
  if (sym->sy_flags&DCC_SYMFLAG_STATIC) printf("static ");
  if (sym->sy_flags&DCC_SYMFLAG_WEAK) printf("weak ");
  switch (sym->sy_flags&DCC_SYMFLAG_VISIBILITYBASE) {
   default                   : printf("public "); break;
   case DCC_SYMFLAG_PROTECTED: printf("protected "); break;
   case DCC_SYMFLAG_PRIVATE  : printf("private "); break;
   case DCC_SYMFLAG_INTERNAL : printf("internal "); break;
  }
  if (DCCSym_ISSECTION(sym)) {
   char flags[7],*iter = flags;
   if (sym->sy_flags&DCC_SYMFLAG_SEC_R) *iter++ = 'R';
   if (sym->sy_flags&DCC_SYMFLAG_SEC_W) *iter++ = 'W';
   if (sym->sy_flags&DCC_SYMFLAG_SEC_X) *iter++ = 'X';
   if (sym->sy_flags&DCC_SYMFLAG_SEC_S) *iter++ = 'S';
   if (sym->sy_flags&DCC_SYMFLAG_SEC_M) *iter++ = 'M';
   if (sym->sy_flags&DCC_SYMFLAG_SEC_U) *iter++ = 'U';
   *iter = '\0';
   if (DCCSection_ISIMPORT(DCCSym_TOSECTION(sym))) {
    printf("%s: LIBRARY('%s','%s')\n",
           sym->sy_name->k_name,
           sym->sy_name->k_name,flags);
   } else {
    printf("%s: SECTION('%s','%s')\n",
           sym->sy_name->k_name,
           sym->sy_name->k_name,flags);
   }
  } else if (sym->sy_alias) {
   printf("%s: ALIAS('%s'+%lu)\n",
          sym->sy_name->k_name,
          sym->sy_alias->sy_name->k_name,
         (unsigned long)sym->sy_addr);
  } else if (sym->sy_sec) {
   if (sym->sy_sec == &DCCSection_Abs) {
    printf("%s: ABS(%#lx)\n",sym->sy_name->k_name,
          (unsigned long)sym->sy_addr);
   } else if (sym->sy_size) {
    printf("%s: '%s'+%lu (%lu bytes)\n",sym->sy_name->k_name,
           sym->sy_sec->sc_start.sy_name->k_name,
          (unsigned long)sym->sy_addr,
          (unsigned long)sym->sy_size);
   } else {
    printf("%s: '%s'+%lu\n",sym->sy_name->k_name,
           sym->sy_sec->sc_start.sy_name->k_name,
          (unsigned long)sym->sy_addr);
   }
  } else {
   printf("%s: UNDEFINED\n",sym->sy_name->k_name);
  }
 }
}



int main(int argc, char *argv[]) {
 struct TPPFile *infile;
 struct DCCSection *sec;
 int first = 1,result = 0;
 if (!TPP_INITIALIZE()) return 1;
 TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_TERMINATE_STRING_LF
                              |TPPLEXER_FLAG_COMMENT_NOOWN_LF
#ifdef _WIN32
                              |TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
#endif
                               );
 if (argc) --argc,++argv;
 if unlikely(!argc) { result = 1; goto end_tpp; }

 //_CrtSetBreakAlloc(122);

 do {
  struct DCCUnit old_unit;
  /* Parse the input code. */
  if (strcmp(argv[0],"-") != 0) {
   infile = TPPLexer_OpenFile(TPPLEXER_OPENFILE_MODE_NORMAL|
                              TPPLEXER_OPENFILE_FLAG_NOCASEWARN,
                              argv[0],strlen(argv[0]),NULL);
   if (infile) TPPFile_Incref(infile);
   if (!infile) fprintf(stderr,"File not found: \"%s\"\n",argv[0]),_exit(1);
  } else {
   /* Fallback: Use stdin as input stream. */
#ifdef _WIN32
   infile = TPPFile_OpenStream(GetStdHandle(STD_INPUT_HANDLE),"<stdin>");
#else
   infile = TPPFile_OpenStream(STDIN_FILENO,"<stdin>");
#endif
  }
  /* Yield the initial token. */
  TPPLexer_PushFileInherited(infile);
  TPPLexer_Current->l_callbacks.c_parse_pragma     = &DCCParse_Pragma;
  TPPLexer_Current->l_callbacks.c_parse_pragma_gcc = &DCCParse_GCCPragma;
  TPPLexer_Current->l_callbacks.c_ins_comment      = &DCCParse_InsComment;
  TPPLexer_Current->l_extokens = (TPPLEXER_TOKEN_LANG_C|
                                  TPPLEXER_TOKEN_TILDETILDE);
  if (!first) {
   DCCUnit_Extract(&old_unit);
   /* TODO: Reset TPP warnings & macros (But _not_ keyword!) */
   DCCCompiler_Quit(&compiler);
  } else {
   /* Initialize DCC and create+set the current text target. */
   DCCUnit_Init(&unit);
  }
  DCCCompiler_Init(&compiler);

  TPPLexer_Yield();
  compiler.c_flags |= DCC_COMPILER_FLAG_NOCGEN;
  unit.u_text = DCCUnit_NewSecs(".text",DCC_SYMFLAG_SEC_X|DCC_SYMFLAG_SEC_R);
  unit.u_data = DCCUnit_NewSecs(".data",DCC_SYMFLAG_SEC_R);
  unit.u_bss  = DCCUnit_NewSecs(".bss",DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_W);
  unit.u_str  = DCCUnit_NewSecs(".string",DCC_SYMFLAG_SEC_R|DCC_SYMFLAG_SEC_M);
  if (!OK) goto end;

  DCCUnit_SetCurr(unit.u_text);
  DCCParse_AllGlobal();
  DCCUnit_SetCurr(NULL);

  if (first) first = 0;
  else DCCUnit_Merge(&old_unit),
       DCCUnit_Quit(&old_unit);
  if (!OK) goto end;

  ++argv,--argc;
 } while (argc);

 /* Cleanup unused stuff. */
 DCCCompiler_ClearDecl();
 DCCUnit_ClearStatic();

 /* Dynamically export PE symbols. */
 compiler.l_flags |= DCC_LINKER_FLAG_PEDYNAMIC;

#if 1
 /* Prepare generated code for output to file. */
 DCCUnit_ENUMSEC(sec) DCCSection_ResolveDisp(sec);
 { stream_t hout = s_openw("a.exe");
   DCCBin_Generate(hout); /* Generate the binary. */
   s_close(hout);
 }
#else
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
   dump_symbols();
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
    dump_symbols();
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
 /*if (!OK) */dump_symbols();

 DCCCompiler_Quit(&compiler);
 DCCUnit_Quit(&unit);
 DCCUnit_ClearCache();
end_tpp:
 TPP_FINALIZE();
#ifdef _CRTDBG_MAP_ALLOC
 _CrtDumpMemoryLeaks();
#endif
 return result;
}



