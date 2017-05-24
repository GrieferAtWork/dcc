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
#ifndef GUARD_DCC_CMD_H
#define GUARD_DCC_CMD_H 1

#include <dcc/common.h>
#include <stddef.h>

DCC_DECL_BEGIN

#if DCC_TARGET_OS == DCC_OS_WINDOWS
#define DCC_OUTFILE_STDEXE "a.exe"
#else
#define DCC_OUTFILE_STDEXE "a.out"
#endif
#define DCC_OUTFILE_STDOBJ "a.o"

struct option {
 int16_t              o_id;     /*< Unique Option ID emit when it is encountered. */
#define OPTION_FLAG_NONE   0x0000
#define OPTION_FLAG_VALUE  0x0001 /*< An option value follows immediately, or in the next argument. */
#define OPTION_FLAG_EQUAL  0x0002 /*< Set for options that expect a value following a '=' character */
#define OPTION_FLAG_PREFIX 0x1000 /*< The option name is a prefix (when countered, 'c_val' is set to the text following the prefix).
                                   *  This can be used for processing dynamic options like '-f<extension>' or '-Wno-<warning>' */
#define OPTION_FLAG_NOOPT  0x2000 /*< Set when this is not meant to be an option, but should later be parsed as a source file (e.g.: 'dcc foo.c -lm') */
 uint16_t             o_flags;  /*< Set of 'OPTION_FLAG_*' */
 char const          *o_short;  /*< Short option name, or NULL when unused (e.g.: "E" for '-E') */
 char const          *o_long;   /*< [0..1] Long option name, or NULL when unknown. */
 struct option const *o_subopt; /*< [0..1] Sub-options that may be encountered as a comma-separated list
                                 *         immediately following this option (e.g.: '-Wl,-subsystem=...')
                                 *   NOTE: When this field is set, 'o_id' is ignored. */
};
#define OPTION_SENTINAL       {0,0,NULL,NULL,NULL}
#define OPTION_ISSENTINAL(o) (!(o)->o_short && !(o)->o_long)

struct cmd {
 int                  c_argc;  /*< Argument count. */
 char               **c_argv;  /*< Argument count. */
 char                *c_arg;   /*< [0..1] The current argument. */
 struct option const *c_grp;   /*< [0..1] The current group in nested options. */
 uint16_t             c_state; /*< Internal commandline state (initialize to ZERO) */
 int16_t              c_id;    /*< Option id (copied from 'o_id') */
 char                *c_val;   /*< [0..1] Option value (non-NULL when the option has a value) */
};

enum{
 OPT_UNKNOWN = -1,
 OPT_EOF     =  0,
#define GROUP_BEGIN(n)          /* nothing */
#define GROUP_END               /* nothing */
#define OPTION(n,f,s,l,g)       n,
#define OPTION_ALIAS(n,f,s,l,g) /* nothing */
#define OPTION_UNNAMED(f,s,l,g) /* nothing */
#include "cmd-def.inl"
#undef OPTION_UNNAMED
#undef OPTION_ALIAS
#undef OPTION
#undef GROUP_END
#undef GROUP_BEGIN
};

enum{
#define GROUP_BEGIN(n)          OPG_##n,
#define GROUP_END               /* nothing */
#define OPTION(n,f,s,l,g)       /* nothing */
#define OPTION_ALIAS(n,f,s,l,g) /* nothing */
#define OPTION_UNNAMED(f,s,l,g) /* nothing */
#include "cmd-def.inl"
#undef OPTION_UNNAMED
#undef OPTION_ALIAS
#undef OPTION
#undef GROUP_END
#undef GROUP_BEGIN
 OPG_COUNT
};

#define cmd_init(self,argc,argv) \
do{ (self)->c_argc  = (argc);\
    (self)->c_argv  = (argv);\
    (self)->c_arg   = NULL;\
    (self)->c_grp   = NULL;\
    (self)->c_state = 0;\
}while(DCC_MACRO_FALSE)

/* NOTE: Once all options have been parsed, the portion
 *       of 'cmd' describes all non-command arguments. */
INTDEF uint16_t cmd_yield(struct cmd *__restrict c);


INTDEF void exec_cmd(struct cmd *__restrict c, int from_cmd);

/* Execute commandline options from 'grp'.
 * @param: grp: One of 'OPG_*' (e.g.: 'OPG_grp_main')
 * NOTE: Upon return, '*argc' and '*argv' will point to a
 *       vector of non-option arguments (aka. the source files).
 * NOTE: The caller is responsible for popping the
 *       application name from the argument vector. */
INTDEF void DCCCmd_Exec(int grp, int *argc, char ***argv);

/* Same as 'DCCCmd_Exec', but the commandline isn't split into argc|argv.
 * NOTE: This function is used for implementing '#pragma comment(compiler|linker,...)' */
INTDEF void DCCCmd_ExecString(int grp, char const *str, size_t len);


DCC_DECL_END

#endif /* !GUARD_DCC_CMD_H */
