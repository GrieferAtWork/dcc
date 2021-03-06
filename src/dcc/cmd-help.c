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
#ifndef GUARD_DCC_CMD_HELP_C
#define GUARD_DCC_CMD_HELP_C 1

#include <dcc/common.h>
#include <dcc/unit.h>
#include <dcc/lexer.h>
#include <dcc/linker.h>
#include <dcc/compiler.h>

#include "cmd.h"

#include <stdio.h>
#include <string.h>

DCC_DECL_BEGIN

INTERN void dcc_dump_symbols(void) {
 struct DCCSym *sym;
 DCCUnit_ENUMSYM(sym) {
#if DCC_TARGET_BIN == DCC_BINARY_PE
  if (sym->sy_flags&DCC_SYMFLAG_DLLIMPORT) printf("IMPORT ");
  if (sym->sy_flags&DCC_SYMFLAG_DLLEXPORT) printf("EXPORT ");
#endif
  if (sym->sy_flags&DCC_SYMFLAG_STATIC) printf("static ");
  if (sym->sy_flags&DCC_SYMFLAG_WEAK) printf("weak ");
  printf("%s ",DCCSymflag_ToString(sym->sy_flags));
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
    printf("%s: LIBRARY('%s')\n",
           sym->sy_name->k_name,flags);
   } else {
    printf("%s: SECTION('%s')\n",
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
    if (sym->sy_align != 1) {
     printf("%s: '%s'+%lu (%lu align, %lu bytes)\n",sym->sy_name->k_name,
            sym->sy_sec->sc_start.sy_name->k_name,
           (unsigned long)sym->sy_addr,
           (unsigned long)sym->sy_align,
           (unsigned long)sym->sy_size);
    } else {
     printf("%s: '%s'+%lu (%lu bytes)\n",sym->sy_name->k_name,
            sym->sy_sec->sc_start.sy_name->k_name,
           (unsigned long)sym->sy_addr,
           (unsigned long)sym->sy_size);
    }
   } else if (sym->sy_align != 1) {
    printf("%s: '%s'+%lu (%lu align)\n",sym->sy_name->k_name,
           sym->sy_sec->sc_start.sy_name->k_name,
          (unsigned long)sym->sy_addr,
          (unsigned long)sym->sy_align);
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

#if defined(_MSC_FULL_VER) && defined(__cplusplus)
#   define DCC_HOST_COMPILER "VC++ " DCC_PP_STR(_MSC_FULL_VER)
#elif defined(_MSC_FULL_VER)
#   define DCC_HOST_COMPILER "VC " DCC_PP_STR(_MSC_FULL_VER)
#elif defined(__clang__) && defined(__cplusplus)
#   define DCC_HOST_COMPILER "clang++ " DCC_PP_STR(__clang__)
#elif defined(__clang__)
#   define DCC_HOST_COMPILER "clang " DCC_PP_STR(__clang__)
#elif defined(__GNUC__) && defined(__cplusplus)
#   define DCC_HOST_COMPILER "g++ " DCC_PP_STR(__GNUC__) "." DCC_PP_STR(__GNUC_MINOR__) "." DCC_PP_STR(__GNUC_PATCHLEVEL__)
#elif defined(__GNUC__)
#   define DCC_HOST_COMPILER "gcc " DCC_PP_STR(__GNUC__) "." DCC_PP_STR(__GNUC_MINOR__) "." DCC_PP_STR(__GNUC_PATCHLEVEL__)
#elif defined(__DCC_VERSION__) && defined(__DCC_HOSTED__)
#   define DCC_HOST_COMPILER "dcc " DCC_PP_STR(__DCC_VERSION__) " - Yay for recursion x" DCC_PP_STR(__DCC_HOSTED__) "!"
#elif defined(__DCC_VERSION__)
#   define DCC_HOST_COMPILER "dcc " DCC_PP_STR(__DCC_VERSION__) " - Yay for recursion!"
#elif defined(__TINYC__)
#   define DCC_HOST_COMPILER "tcc " DCC_PP_STR(__TINYC__)
#elif defined(__cplusplus)
#   define DCC_HOST_COMPILER "Unknown c++ compiler"
#else
#   define DCC_HOST_COMPILER "Unknown c compiler"
#endif


PRIVATE char const dcc_version_text[] =
"dcc version "
DCC_PP_STR(DCC_API_VERSION) "/"
DCC_PP_STR(DCC_COMPILER_VERSION)
" - Direct C Compiler - "
"Copyright (C) 2017 Griefer@Work\n"
"[" DCC_TARGET_TRIPLET "|"
#if DCC_TARGET_BIN == DCC_BINARY_ELF
"elf"
#elif DCC_TARGET_BIN == DCC_BINARY_PE
"pe"
#else
"UNKNOWN(" DCC_PP_STR(DCC_TARGET_BIN) ")"
#endif
"]["
#if DCC_DEBUG
"DEBUG"
#else
"NDEBUG"
#endif
"|" DCC_HOST_COMPILER "|"
#ifdef __TIME__
__TIME__
#endif
" "
#ifdef __DATE__
__DATE__
#endif
"]\n\n";


struct tpp_extension {
 int         e_flag;
 char const *e_name;
 size_t      e_size;
};
INTDEF struct tpp_extension const tpp_extensions[];
INTDEF char const *const wgroup_names[WG_COUNT+1];

INTERN void dcc_help(char const *subject) {
 if (subject && !*subject) subject = NULL;
#define INDENT "\t"
 if (subject) {
  if (!strcmp(subject,"linker")) {
   fprintf(stderr,"Linker options:\n"
                  INDENT "-Wl,-Bsymbolic              Same as '-symbolic'\n"
#if DCC_TARGET_BIN == DCC_BINARY_PE
                  INDENT "-Wl,--dll\n"
#endif
                  INDENT "-Wl,-Bshareable\n"
                  INDENT "-Wl,-shared                 Same as '-shared'\n"
                  INDENT "-Wl,-nostdlib               Same as '-nostdlib'\n"
                  INDENT "-Wl,-pie                    Generate relocations required to create a position-independent image\n"
                  INDENT "-Wl,--pic-executable        Generate a position-independent executable\n"
                  INDENT "-Wl,-init=sym               Define a symbol to-be called at link-time initialization\n"
                  INDENT "-Wl,-fini=sym               Define a symbol to-be called at link-time finalization\n"
                  INDENT "-Wl,-e,sym\n"
                  INDENT "-Wl,--entry=sym             Define the symbol used as entry point\n"
                  INDENT "-Wl,--defsym,sym=val        Define an absolute symbol 'sym' pointing at 'val'\n"
                  INDENT "                            Note, that 'val' may contain arithmetic and depend on additional symbols\n"
                  INDENT "-Wl,--section-start=sec=val Define the start address of 'sec' to be fixed at 'val'\n"
#if DCC_TARGET_BIN == DCC_BINARY_PE && !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
                  INDENT "                            Note, that the windows PE loader will likely refuse to load illegal values\n"
#endif
                  INDENT "-Wl,-Tbss=val               Same as '-Wl,--section-start=.bss=val'\n"
                  INDENT "-Wl,-Tdata=val              Same as '-Wl,--section-start=.data=val'\n"
                  INDENT "-Wl,-Ttext=val              Same as '-Wl,--section-start=.text=val'\n"
                  INDENT "-Wl,--image-base=val        Set the preferred/fixed base address of the image\n"
                  INDENT "-Wl,--section-alignment=val Set the required alignment between file-offsets and automatic section addresses\n"
                  INDENT "                            Note, that the linker will likely refuse to load the image if 'val' isn't a multiple of PAGESIZE\n"
#if DCC_TARGET_BIN == DCC_BINARY_PE
                  INDENT "-Wl,--file-alignment=val    Set the PE file alignment\n"
                  INDENT "-Wl,--stack,val             Set 'SizeOfStackReserve' in the PE file header\n"
                  INDENT "-Wl,--subsystem,sys         Set 'Subsystem' in the PE file header\n"
                  INDENT "                            In addition to an integer value, 'sys' is recognized as one of:\n"
                  INDENT "                            windows|native|console|gui|posix|efiapp|efiboot|efiruntime|efirom\n"
#endif
                  INDENT "-Wl,-h,name\n"
                  INDENT "-Wl,--soname=name           Set the linker-name of the generated shared object\n"
                  INDENT "-Wl,--[no-]fatal-warnings   Enable/Disable any warning as a fatal error\n"
                  INDENT "-Wl,--allow-multiple-definition Allow multiple definitions of the same symbol\n"
                  INDENT "-Wl,--[no-]allow-shlib-undefined Allow creation of images with unresolved references\n"
                  INDENT "-Wl,--no-warn-mismatch      Don't warn about linking incompatible object files\n"
                  INDENT "-Wl,--no-warn-search-mismatch Don't warn when incompatible object files are encountered when searching for dependencies\n"
                  INDENT "-Wl,--[no-]clear-unused     Delete unused symbols from binaries (enabled for -On>=1; default: off)\n"
                  INDENT "-Wl,--[no-]collapse         Collapse section data to generate more compact binaries (enabled for -On>=2; default: off)\n"
                  INDENT "-Wl,--[no-]merge-sym        Merge identical symbols such as strings during compilation (disabled for -On<=0; default: on)\n"
                  INDENT "-Wl,-o,outfile\n"
                  INDENT "-Wl,--output=outfile        Same as '-o outfile'\n"
                  INDENT "-Wl,-L,path\n"
                  INDENT "-Wl,--library-path=path     Same as '-L path'\n"
                  INDENT "-Wl,-qmagic\n"
                  INDENT "-Wl,-Qy\n"
                  INDENT "-Wl,-i\n"
                  INDENT "-Wl,-r\n"
                  INDENT "-Wl,-g                      Ignored\n"
                  );
   goto done;
  }
  if (!strcmp(subject,"pp")) {
   fprintf(stderr,"Preprocessor options:\n"
                  INDENT "-trigraphs                  Enable recognition of trigraph character sequences\n"
                  INDENT "-undef                      Disable all builtin macros\n"
                  INDENT "-P                          Disable emission of #line adjustment directives\n"
                  INDENT "-M                          Instead of emitting preprocessor output, emit a make-style list of dependencies\n"
                  INDENT "-MM                         Similar to '-M', but don't include system headers\n"
                  INDENT "-MD                         Like '-M', but don't disable preprocessing\n"
                  INDENT "-MMD                        Like '-MD', but don't disable preprocessing\n"
                  INDENT "-MG                         Disable preprocessing, but include missing files as dependencies, assuming they will be generated\n"
                  INDENT "-MP                         Emit dummy targets for every dependency\n"
                  INDENT "-MF <file>                  Enable dependency tracking and emit its output to <file>, but also preprocess regularly\n"
                  INDENT "-MT <target>                Specify the target object name used within the generated make dependency\n"
                  INDENT "-MQ <target>                Same as '-MT', but escape characters special to make, such as '$'\n"
                  INDENT "--tok                       Outline all tokens using the [...] notation (Default: off)\n"
                  INDENT "--pp                        Enable preprocess-mode, which emits all tokens separated by '\\0'-bytes\n"
                  INDENT "                            Enabling this option also disabled SPACE and LF tokens, though\n"
                  INDENT "                            they can be re-enabled using the -spc and -lf switches\n"
                  INDENT "-f[no-]magiclf              Enable/Disable magic linefeeds sometimes used in place of #line (Default: off)\n"
                  INDENT "-f[(cpp|no)-]line           Enable/Disable emission of #line directives (Default: '-fcpp-line')\n"
                  INDENT "-f[no-]decode               Enable/Disable decoding of di/tri-graphs, as well as escaped linefeeds in output (Default: on)\n"
                  INDENT "-f[no-]unify-pragma         Unify all unknown pragmas to use the preprocessor-directive syntax before re-emission (Default: on)\n"
                  INDENT "-f[no-]spc                  Enable/Disable SPACE tokens (useful in '--pp'-mode)\n"
                  INDENT "-f[no-]lf                   Enable/Disable LF tokens (useful in '--pp'-mode)\n"
                  INDENT "-f[no-]comments             Enable/Disable COMMENT tokens (useful in '--pp'-mode)\n"
                  INDENT "-f[no-]longstring(s)        Enable/Disable string continuation between lines (Default: off)\n"
                  );
   goto done;
  }
  if (!strcmp(subject,"extensions")) {
   struct tpp_extension const *iter;
   for (iter = tpp_extensions; iter->e_name; ++iter) {
    fprintf(stderr,"-f%s%s\n",HAS(iter-tpp_extensions) ? "" : "no-",iter->e_name);
   }
   goto done;
  }
  if (!strcmp(subject,"warnings")) {
   char const *const *iter;
#define getstate(wid) \
 (TPP(wstate_t))((CURRENT.l_warnings.w_curstate->ws_state[(wid)/(8/TPP_WARNING_BITS)] \
             >> (((wid)%(8/TPP_WARNING_BITS))*TPP_WARNING_BITS)) & 3)
   for (iter = wgroup_names; *iter; ++iter) {
    fprintf(stderr,"-W%s%s\n",getstate(iter-wgroup_names) == TPP(WSTATE_DISABLED) ? "no-" : "",*iter);
   }
#undef getstate
   goto done;
  }
  if (!strcmp(subject,"include-paths")) {
   struct TPPString **iter,**end;
   end = (iter = CURRENT.l_syspaths.il_pathv)+
                 CURRENT.l_syspaths.il_pathc;
   for (; iter != end; ++iter) fprintf(stderr,"-I%s\n",(*iter)->s_text);
   goto done;
  }
  if (!strcmp(subject,"library-paths")) {
   struct TPPString **iter,**end;
   end = (iter = linker.l_paths.lp_pathv)+
                 linker.l_paths.lp_pathc;
   for (; iter != end; ++iter) fprintf(stderr,"-L%s\n",(*iter)->s_text);
   goto done;
  }
  if (!strcmp(subject,"internal-paths")) {
   struct TPPString **iter,**end;
   end = (iter = linker.l_intpaths.lp_pathv)+
                 linker.l_intpaths.lp_pathc;
   for (; iter != end; ++iter) fprintf(stderr,"%s\n",(*iter)->s_text);
   goto done;
  }
  if (!strcmp(subject,"std")) {
   struct DCCStdName const *iter = DCCCompiler_Std;
   static char const *const std_help[DCC_COMPILER_STD_COUNT] = {
    /* [DCC_COMPILER_STD_CURRENT] = */"",
    /* [DCC_COMPILER_STD_DCC    ] = */"DCC's own C-dialect with all features & extensions enabled (default):\n",
    /* [DCC_COMPILER_STD_KR     ] = */"K&R-style C-dialect, disabling any warning about use of its deprecated syntax:\n",
    /* [DCC_COMPILER_STD_C90    ] = */"C90 dialect:\n",
    /* [DCC_COMPILER_STD_C99    ] = */"C99 dialect:\n",
    /* [DCC_COMPILER_STD_C11    ] = */"C11 dialect:\n",
   };
   for (; iter->sn_nam; ++iter) {
    if (iter != DCCCompiler_Std &&
        iter[-1].sn_std != iter->sn_std)
        fprintf(stderr,"\n");
    if (iter == DCCCompiler_Std ||
        iter[-1].sn_std != iter->sn_std) {
     assert(iter->sn_std < DCC_COMPILER_STD_COUNT);
     fwrite(std_help[iter->sn_std],sizeof(char),
            strlen(std_help[iter->sn_std]),stderr);
    }
    fprintf(stderr,"\t-std=%s\n",iter->sn_nam);
   }
   goto done;
  }
  fprintf(stderr,"Unknown help subject: '%s'\n",subject);
  goto done;
 }
 fprintf(stderr,"Usage: %s [options...] [-o outfile] [-|INFILE...|-lLIB...]\n"
#if DCC_CONFIG_HAVE_DRT
                "       %s [options...] [-d] [-|INFILE...|-lLIB...]\n"
#endif /* DCC_CONFIG_HAVE_DRT */
                "       %s --has-feature FEATURE\n"
                "options:\n"
#if DCC_CONFIG_HAVE_DRT
                INDENT "-d                          Directly execute generated assembly asynchronously\n"
#endif /* DCC_CONFIG_HAVE_DRT */
                INDENT "-c                          Create a relocatable object file instead of a binary\n"
                INDENT "-E                          Enable preprocessor mode (s.a.: 'dcc --help pp')\n"
                INDENT "-o <name>                   Redirect output to a given file (defauls to '" DCC_OUTFILE_STDEXE "'/'" DCC_OUTFILE_STDOBJ "')\n"
                INDENT "-On                         Set the level of compiler/linker optimizations to 'n'\n"
                INDENT "-shared                     Create a shared library instead of an executable\n"
#if DCC_TARGET_BIN == DCC_BINARY_ELF
                INDENT "-symbolic                   Bind global references to those from shared libraries\n"
#else
                INDENT "-symbolic                   Ignored by the target binary\n"
#endif
                INDENT "-nostdinc                   Don't include standard include/library paths\n"
                INDENT "-nostdlib                   Don't link against standard libraries\n"
                INDENT "-Ldir                       Adds 'dir' to the list of library search paths\n"
                INDENT "-Idir                       Adds 'dir' to the list of #include <...> paths\n"
                INDENT "-Dsym[=val=1]               Defines a macro 'sym' as 'val'\n"
                INDENT "-Usym                       Undefine a previously defined macro 'sym'\n"
                INDENT "-Apred=answer               Define an assertion 'pred' as 'answer'\n"
                INDENT "-A-pred[=answer]            Delete 'answer' or all assertions previously made about 'pred'\n"
                INDENT "-f(pic|PIC|pie|PIE)         Generate position-independent images (Always enabled for object files)\n"
                INDENT "-fvisibility=vis            Set the default ELF visibility to one of (default|hidden|protected|internal)\n"
                INDENT "-ftabstop=width             Set the width of tab characters used by __COLUMN__ and in warning/error messages (Default: " DCC_PP_STR(TPPLEXER_DEFAULT_TABSIZE) ")\n"
                INDENT "-f[un]signed-char           Configure the default sign-behavior of the builtin c type 'char'\n"
                INDENT "-f[no-]leading-underscore   Enable/Disable leading underscores in symbol on targets that require them\n"
                INDENT "-f[no-]stack-check          Currently ignored\n"
                INDENT "-f[no-]<extension>          Enable/Disable a given 'extension' (s.a.: '--help extensions')\n"
                INDENT "-Wall                       Change all disabled warnings to warn-mode\n"
                INDENT "-Werror                     Every warning is interpreted as an error\n"
                INDENT "-Wsystem-headers            Enable warnings in system headers\n"
                INDENT "-W[no-]<warning>            Enable/Disable a given 'warning' group (s.a.: '--help warnings')\n"
                INDENT "--name <name>               Set the name used for __FILE__ by INFILE (Useful when INFILE is stdin)\n"
                INDENT "--help [subject]            Display this help and exit. When specified, subject may be one of:\n"
                INDENT "                            linker|extensions|warnings|include-paths|library-paths|internal-paths|std|pp\n"
                INDENT "--version                   Display version information and exit\n"
#ifdef _WIN32
                INDENT "--message-format={msvc|gcc} Set the format for error message (Default: msvc)\n"
#else
                INDENT "--message-format={msvc|gcc} Set the format for error message (Default: gcc)\n"
#endif
                INDENT "-ansi                       Same as -std=c90\n"
                INDENT "-std=name                   Set the effective C dialect to 'name' (s.a.: '--help std')\n"
                INDENT "-traditional[-cpp]          Emulate traditional C-compiler behavior, not warning about old-style\n"
                INDENT "                            functions and accepting old-style spelling of inplace operation tokens\n"
                INDENT "--enum-feature(s)           Enumerate all host features supported by DCC\n"
                INDENT "--has-feature <name>        Check host feature 'name' and print '1' + exit with '0' when found, or the reverse otherwise\n"
                "-:       Use <stdin> as input source file\n"
                "INFILE:  A list of input source files intermixed with object files\n"
                "LIB:     Add 'LIB' as a dynamic library dependency\n"
                "FEATURE: Name of a feature to query (Enumerate known with 'dcc --enum-features')\n"
               ,"dcc"
#if DCC_CONFIG_HAVE_DRT
               ,"dcc"
#endif /* DCC_CONFIG_HAVE_DRT */
               ,"dcc");
#undef INDENT
done:
 fflush(stderr);
 exit(2);
}


INTERN void dcc_version(void) {
 fwrite(dcc_version_text,sizeof(char),
       (sizeof(dcc_version_text)/sizeof(char))-1,
        stderr);
 fflush(stderr);
 exit(2);
}


DCC_DECL_END

#endif /* !GUARD_DCC_CMD_HELP_C */
