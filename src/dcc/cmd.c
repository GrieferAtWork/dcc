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
#ifndef GUARD_DCC_CMD_C
#define GUARD_DCC_CMD_C 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include "cmd.h"

#include <string.h>

DCC_DECL_BEGIN

#define GROUP_BEGIN(n)          PRIVATE struct option const n[] = {
#define GROUP_END               OPTION_SENTINAL};
#define OPTION(n,f,s,l,g)       {n,f,s,l,g},
#define OPTION_ALIAS(n,f,s,l,g) {n,f,s,l,g},
#define OPTION_UNNAMED(f,s,l,g) {OPT_UNKNOWN,f,s,l,g},
#include "cmd-def.inl"
#undef OPTION_UNNAMED
#undef OPTION_ALIAS
#undef OPTION
#undef GROUP_END
#undef GROUP_BEGIN

INTERN struct option const *const dcc_cmd_groups[OPG_COUNT] = {
#define GROUP_BEGIN(n)          n,
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
};


PRIVATE struct option const *
find_option(struct option const *grp, char *text, int is_long) {
 char const *n;
 size_t nlen,textlen;
 textlen = strlen(text);
 for (;; ++grp) {
  n = is_long ? grp->o_long : grp->o_short;
  if (!n) { if (!grp->o_long && !grp->o_short) return NULL; continue; }
  nlen = strlen(n);
  if (nlen <= textlen && !memcmp(n,text,nlen*sizeof(char))) break;
 }
 return grp;
}

#define STATE_MAIN   0 /* In main namespace. */
#define STATE_SUBOPT 1 /* In sub-option namespace. */


INTERN uint16_t
cmd_yield(struct cmd *__restrict c) {
 char **arg_iter,**end,*arg;
 struct option const *grp;
 struct option const *opt;
 int is_long_option;
 assert(c);
again:
 grp = c->c_grp;
 is_long_option = 0;
 if unlikely(!grp) grp = grp_main;
 if ((arg = c->c_arg) != NULL) {
  /* Parse options from merged flags. */
  if (*arg == '-') {
   is_long_option = 1;
   opt = find_option(grp,++arg,1);
  } else {
   opt = find_option(grp,arg,0);
  }
  goto parse_arg;
 }
 end = (arg_iter = c->c_argv)+c->c_argc;
 for (;;) {
  if (arg_iter == end) goto eof;
  if (arg_iter[0][0] == '-' && arg_iter[0][1]) {
   opt = (arg_iter[0][1] == '-') ? find_option(grp,arg_iter[0]+2,1)
                                 : find_option(grp,arg_iter[0]+1,0);
   if (!opt || !(opt->o_flags&OPTION_FLAG_NOOPT)) break;
  }
  ++arg_iter;
 }
 arg = *arg_iter;
 if (arg_iter != c->c_argv) {
  /* Shift the argument list to move the option downwards. */
  assert(arg_iter >= c->c_argv);
  memmove(c->c_argv+1,c->c_argv,
         (arg_iter-c->c_argv)*sizeof(char *));
  c->c_argv[0] = arg;
 }
 /* Consume one argument. */
 --c->c_argc;
 ++c->c_argv;
 c->c_arg = ++arg;
 if (*arg == '-') {
  ++arg;
  opt = find_option(grp,arg,1);
  is_long_option = 1;
 } else {
  opt = find_option(grp,arg,0);
 }
parse_arg:
 if (opt) {
  /* Skip the option name. */
  arg += strlen(is_long_option ? opt->o_long : opt->o_short);
  c->c_id = opt->o_id;
  if (opt->o_subopt && *arg == ',') {
   /* Grouped option. */
   if (*++arg == '-') ++arg;
   c->c_arg   = arg;
   c->c_grp   = opt->o_subopt;
   c->c_state = STATE_SUBOPT;
   /* Parse sub-option namespace. */
   goto again;
  }
  if (opt->o_flags&OPTION_FLAG_PREFIX) {
consume_argval:
   c->c_val = arg; /* Prefix option. */
   if (c->c_state == STATE_SUBOPT) {
    arg = strchr(arg,',');
    if (arg) *arg++ = '\0';
    else     arg = (char *)"";
    if (*arg == '-') ++arg;
   } else arg = (char *)"";
  } else if (opt->o_flags&OPTION_FLAG_EQUAL) {
   if (*arg == '=' || *arg == ',') { ++arg; goto consume_argval; }
   c->c_val = (char *)"";
  } else if (opt->o_flags&OPTION_FLAG_VALUE) {
   /* Value option. */
   if (c->c_state == STATE_SUBOPT) {
    if (*arg == ',') { ++arg;
    if (*arg == '-')   ++arg; }
   }
   if (*arg) goto consume_argval;
   if (c->c_argc) { /* Consume the next CMD argument. */
    c->c_val = c->c_argv[0];
    ++c->c_argv;
    --c->c_argc;
   } else {
    c->c_val = (char *)""; /* Empty value. */
   }
  } else {
   c->c_val = NULL;
   if (c->c_state == STATE_SUBOPT) {
    if (*arg == ',') { ++arg;
    if (*arg == '-')   ++arg; }
   }
  }
 } else if (!*arg) {
  /* Try the next argument if the current was empty. */
  c->c_id = OPT_EOF;
 } else {
  /* Unknown option. */
  c->c_val = arg++;
  c->c_id  = OPT_UNKNOWN;
 }

 if (c->c_id == OPT_UNKNOWN &&
     c->c_state == STATE_SUBOPT &&
     *arg == ',') { c->c_arg = arg+1; goto again; }

 /* Store the remaining argument portion for the next yield. */
 if (!is_long_option && *arg) c->c_arg = arg;
 else if (c->c_state == STATE_SUBOPT) {
  char *next_arg = strchr(arg,',');
  if (next_arg) {
   /* continuation of sub-namespace:
    * $ dcc -Wl,-shared,-soname=foo
    */
   *next_arg = '\0';
   c->c_arg  = next_arg;
  } else {
   /* Switch back to the root option namespace. */
   c->c_state = STATE_MAIN;
   c->c_grp   = NULL;
   c->c_arg   = NULL;
  }
 } else {
  if (*arg) {
   assert(is_long_option);
   while (arg[-1] != '-') --arg;
   c->c_val = arg;
   c->c_id = OPT_UNKNOWN;
  }
  c->c_arg = NULL;
 }
 /* If an option ended, try the next. */
 if (c->c_id == OPT_EOF) goto again;
end: return c->c_id;
eof: c->c_id = OPT_EOF; goto end;
}

DCC_DECL_END

#ifndef __INTELLISENSE__
#include "cmd-parse.c.inl"
#endif

#endif /* !GUARD_DCC_CMD_C */
