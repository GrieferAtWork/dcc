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
#ifndef GUARD_DCC_PREPROCESSOR_H
#define GUARD_DCC_PREPROCESSOR_H 1

#include "common.h"
#include "lexer.h"

/* Contrary to the name of this file, only functions used
 * to unify commandline options, such as '-E' or '-M'. */
DCC_DECL_BEGIN

#define DCC_PREPROCESSOR_FLAG_NONE        0x00000000

#define DCC_PREPROCESSOR_FLAG_LINECXX     0x00000000 /*< '# <line> [<file>]' */
#define DCC_PREPROCESSOR_FLAG_LINESTDC    0x00000001 /*< '#line <line> [<file>]' */
#define DCC_PREPROCESSOR_FLAG_LINENONE    0x00000002 /*< Don't emit #line directives. */
/*                                        0x00000003 */
#define DCC_PREPROCESSOR_MASK_LINEMODE    0x00000003 /*< Mask for active #line-mode. */

#define DCC_PREPROCESSOR_FLAG_TOKNORMAL   0x00000000 /*< Emit tokens normally. */
#define DCC_PREPROCESSOR_FLAG_TOKOUTLINE  0x00000004 /*< Outline tokens using the [...] notation. */
#define DCC_PREPROCESSOR_FLAG_TOKSEPERATE 0x00000008 /*< Separate tokens with '\0' characters. */
/*                                        0x0000000c */
#define DCC_PREPROCESSOR_MASK_TOKMODE     0x0000000c /*< Mask for active token emission mode. */

#define DCC_PREPROCESSOR_FLAG_PPSOURCE    0x00000000 /*< Do what a compiler should do, and compile the input! */
#define DCC_PREPROCESSOR_FLAG_PPDIRECT    0x00000010 /*< Compile & execute source code directly (TODO). */
#define DCC_PREPROCESSOR_FLAG_PPPREPROC   0x00000020 /*< Perform regular preprocessing. */
#define DCC_PREPROCESSOR_FLAG_PPDEP       0x00000030 /*< Only generate dependency information. */
#define DCC_PREPROCESSOR_MASK_PPMODE      0x00000030 /*< Mask for preprocessor mode. */

#define DCC_PREPROCESSOR_FLAG_HELPINC     0x00000100 /*< Emit help about include paths. */
#define DCC_PREPROCESSOR_FLAG_HELPLIB     0x00000200 /*< Emit help about library paths. */
#define DCC_PREPROCESSOR_MASK_HELP        0x00000300 /*< Mask for help flags. */

#define DCC_PREPROCESSOR_FLAG_NOMAGICLF   0x00010000 /*< ["-fmagiclf"] Disable ~magic~ tokens for small line-shifts to prevent a #line being emit. */
#define DCC_PREPROCESSOR_FLAG_NODECODETOK 0x00020000 /*< ["-fdecode"] Decode stuff like escape sequences and trigraphs before writing to out. */
#define DCC_PREPROCESSOR_FLAG_NOUNIFYPRGM 0x00040000 /*< ["-funify-pragma"] Unify #pragma directives. */
#define DCC_PREPROCESSOR_FLAG_DEPSYSTEM   0x00080000 /*< ["-M"|"-MD"] Include system headers in dependency information. */
#define DCC_PREPROCESSOR_FLAG_DEPDUMMY    0x00100000 /*< ["-MP"] Emit a dummy target for every dependency. */
#define DCC_PREPROCESSOR_FLAG_DEPESCAPE   0x00200000 /*< ["-MQ"] Escape character special to make inside the dependency target name. */
#define DCC_PREPROCESSOR_FLAG_COMPILEONLY 0x00400000 /*< ["-c"] Don't generate a binary, but output an object file. */
#define DCC_PREPROCESSOR_FLAG_OWNSOUTFILE 0x00800000 /*< The 'p_outfile' string is owned. */
#define DCC_PREPROCESSOR_FLAG_OUTNAMEAUTO 0x01000000 /*< The 'p_outfile' string was automatically deduced. */

#define DCC_PREPROCESSOR_LINEMODE(x) ((x)&DCC_PREPROCESSOR_MASK_LINEMODE)
#define DCC_PREPROCESSOR_TOKMODE(x)  ((x)&DCC_PREPROCESSOR_MASK_TOKMODE)
#define DCC_PREPROCESSOR_PPMODE(x)   ((x)&DCC_PREPROCESSOR_MASK_PPMODE)

#define DCC_PREPROCESSOR_SET_LINEMODE(x,f) (((x)&~(DCC_PREPROCESSOR_MASK_LINEMODE))|(f))
#define DCC_PREPROCESSOR_SET_TOKMODE(x,f)  (((x)&~(DCC_PREPROCESSOR_MASK_TOKMODE))|(f))
#define DCC_PREPROCESSOR_SET_PPMODE(x,f)   (((x)&~(DCC_PREPROCESSOR_MASK_PPMODE))|(f))


#define DCC_PREPROCESSOR_DEFAULT_DEPFD_MAXLINE 70

struct DCCPreprocessor {
 uint32_t      p_flags;         /*< Preprocessor mode & flags. */
 DCC(stream_t) p_depfd;         /*< File descriptor used for writing dependency information. */
 char const   *p_deptarget;     /*< [0..1] Dependency target name (Defaults to the resulting object file, or the input file's extension replaced with '.o'). */
 char const   *p_outfile;       /*< [0..1][owned_if(p_flags&DCC_PREPROCESSOR_FLAG_OWNSOUTFILE)] Output file name. */
 char const   *p_srcname;       /*< [0..1] Name override for base source-files. */
 size_t        p_depfd_curline; /*< Length of the current line in the dependency file. */
 size_t        p_depfd_maxline; /*< Maximal length of the single line in the dependency file. */
 size_t        p_depfc;         /*< Amount of dependencies. */
 size_t        p_depfa;         /*< Allocated amount of dependencies. */
 char        **p_depfv;         /*< [1..1][owned][0..p_depfc|alloc(p_depfa)][owned] Vector of dependency file names. */
 uint32_t      p_baseflags;     /*< Base lexer flags. */
};

/* The currently selected preprocessor. */
DCCDAT struct DCCPreprocessor DCCPreprocessor_Current;

DCCFUN void DCCPreprocessor_Init(struct DCCPreprocessor *__restrict self);
DCCFUN void DCCPreprocessor_Quit(struct DCCPreprocessor *__restrict self);

/* Automatically deduce the out-file name from 'first_infile'
 * NOTE: The caller is responsible for ensuring that no name has yet to be have been set.
 * NOTE: When 'first_infile' is NULL, a fallback name is chosen instead. */
DCCFUN void DCCPreprocessor_OutAuto(char const *first_infile);

/* Add the given name to the list of preprocessor dependencies. */
DCCFUN void DCCPreprocessor_DepAdd(char const *__restrict name, size_t size);

/* Print the given dependency. */
DCCFUN void DCCPreprocessor_DepPrint(char const *__restrict name, size_t size);

/* Print the first portion of the dependency file.
 * NOTE: The caller is responsible for setting an output filename beforehand. */
DCCFUN void DCCPreprocessor_DepFirst(void);

/* Print dummy dependency lists for all collected dependencies. */
DCCFUN void DCCPreprocessor_DepDummy(void);

/* Preprocess and print the entirety of the current INCLUDE-stack to 'out' */
DCCFUN void DCCPreprocessor_PrintPP(DCC(stream_t) out);

/* TPP callbacks for preprocessor hooks. */
DCCFUN struct TPPFile *DCCPreprocessor_DepUnknown(char *filename, size_t filename_size);
DCCFUN int DCCPreprocessor_DepNewTextfile(struct TPPFile *file, int is_system_header);
DCCFUN int DCCPreprocessor_ReemitPragma(void);
DCCFUN int DCCPreprocessor_ReemitGCCPragma(void);


#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
DCCDAT struct DCCPreprocessor preproc;
#else
#define preproc  DCCPreprocessor_Current
#endif
#endif


DCC_DECL_END

#endif /* !GUARD_DCC_PREPROCESSOR_H */
