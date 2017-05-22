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
#ifndef GUARD_DCC_CMD_PARSE_C_INL
#define GUARD_DCC_CMD_PARSE_C_INL 1

#include <dcc/common.h>

#include <dcc/lexer.h>
#include <dcc/compiler.h>
#include <dcc/unit.h>
#include <dcc/linker.h>

#include "cmd.h"

#include <string.h>

#if DCC_TARGET_BIN == DCC_BINARY_PE
#include "linker-pe.h"
#endif

DCC_DECL_BEGIN

LOCAL int_t strtoint(char const *s) {
 int_t result; int is_neg = 0;
 char *b = TOKEN.t_begin,*e = TOKEN.t_end;
 while (*s == '-') ++s,is_neg ^= 1;
 TOKEN.t_begin = (char *)s;
 TOKEN.t_end   = (char *)s+strlen(s);
 if (!TPP_Atoi(&result)) result = 0;
 TOKEN.t_begin = b,TOKEN.t_end = e;
 if (is_neg) result = -result;
 return result;
}

PRIVATE void tpp_wall(void) {
 uint8_t *iter,*end,b;
 struct TPPWarningStateEx *ext_iter,*ext_end;
 /* Enable all warnings. */
 iter = CURRENT.l_warnings.w_curstate->ws_state;
 end = iter+TPP_WARNING_BITSETSIZE;
 for (; iter != end; ++iter) {
  b = *iter;
  if ((b&B(00000011)) == (WSTATE_DISABLE << 0)) b |= (WSTATE_WARN << 0);
  if ((b&B(00001100)) == (WSTATE_DISABLE << 2)) b |= (WSTATE_WARN << 2);
  if ((b&B(00110000)) == (WSTATE_DISABLE << 4)) b |= (WSTATE_WARN << 4);
  if ((b&B(11000000)) == (WSTATE_DISABLE << 6)) b |= (WSTATE_WARN << 6);
 }
 ext_end = (ext_iter = CURRENT.l_warnings.w_curstate->ws_extendedv)+
                       CURRENT.l_warnings.w_curstate->ws_extendeda;
 for (; ext_iter != ext_end; ++ext_iter) {
  if (ext_iter->wse_oldstate == WSTATE_DISABLE)
      ext_iter->wse_oldstate =  WSTATE_WARN;
 }
}


INTERN void exec_cmd(struct cmd *__restrict c, int from_cmd) {
 char *v = c->c_val;
 switch (c->c_id) {

 case OPT_Wl_Bsymbolic: linker.l_flags |= DCC_LINKER_FLAG_SYMBOLIC; break;
 case OPT_Wl_nostdlib:  linker.l_flags |= DCC_LINKER_FLAG_NOSTDLIB; break;

 { /* initialization/finalization/entry symbols. */
  char *v_copy; size_t v_len;
 case OPT_Wl_init:
 case OPT_Wl_fini:
 case OPT_Wl_entry:
  v_len = strlen(v);
  v_copy = (char *)DCC_Malloc((v_len+1)*sizeof(char),0);
  if unlikely(!v_copy) break;
  memcpy(v_copy,v,(v_len+1)*sizeof(char));
  c->c_id == OPT_Wl_init ? linker.l_init  = v_copy :
  c->c_id == OPT_Wl_fini ? linker.l_fini  = v_copy :
                           linker.l_entry = v_copy;
 } break;

 case OPT_Wl_image_base:
  linker.l_imgbase = (target_ptr_t)strtoint(v);
  linker.l_flags  |= DCC_LINKER_FLAG_IMGBASE;
  break;

 case OPT_Wl_section_alignment:
  linker.l_secalign = (target_siz_t)strtoint(v);
  break;

#if DCC_TARGET_BIN == DCC_BINARY_PE
 case OPT_Wl_file_alignment: linker.l_secalign = (target_siz_t)strtoint(v); break;
 case OPT_Wl_stack: linker.l_pe_stacksiz = (target_siz_t)strtoint(v); break;
 case OPT_Wl_subsystem:
#ifdef DCC_TARGET_X86
       if (!strcmp(v,"windows"));
  else if (!strcmp(v,"native"))     linker.l_pe_subsys = IMAGE_SUBSYSTEM_NATIVE;
  else if (!strcmp(v,"console"))    linker.l_pe_subsys = IMAGE_SUBSYSTEM_WINDOWS_CUI;
  else if (!strcmp(v,"gui"))        linker.l_pe_subsys = IMAGE_SUBSYSTEM_WINDOWS_GUI;
  else if (!strcmp(v,"posix"))      linker.l_pe_subsys = IMAGE_SUBSYSTEM_POSIX_CUI;
  else if (!strcmp(v,"efiapp"))     linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_APPLICATION;
  else if (!strcmp(v,"efiboot"))    linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER;
  else if (!strcmp(v,"efiruntime")) linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER;
  else if (!strcmp(v,"efirom"))     linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_ROM;
  else
#endif
       linker.l_pe_subsys = (uint8_t)strtoint(v);
  break;
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 case OPT_Wl_soname: linker.l_soname = TPPString_New(v,strlen(v)); break;

 { char *val;
 case OPT_D:
   val = strchr(v,'=');
   if (val) *val++ = '\0';
   else      val = "1";
   if (!TPPLexer_Define(v,strlen(v),val,strlen(val),
                        from_cmd ? TPPLEXER_DEFINE_FLAG_BUILTIN
                                 : TPPLEXER_DEFINE_FLAG_NONE)) goto seterr;
 } break;

 case OPT_U: TPPLexer_Undef(v,strlen(v)); break;
 case OPT_A: {
  int add = 1; char *val;
  if (*v == '-') ++v,add = 0;
  val = strchr(v,'=');
  if (val) *val++ = '\0'; else if (add) { WARN(W_CMD_A_EXPECTED_VALUE); break; }
  add ? TPPLexer_AddAssert(v,strlen(v),val,strlen(val))
      : TPPLexer_DelAssert(v,strlen(v),val,val ? strlen(val) : 0);
 } break;

 case OPT_I:
  if (!TPPLexer_AddIncludePath(v,strlen(v))) goto seterr;
  break;

 case OPT_W:
  /* Special warning commands. */
       if (!strcmp(v,"all")) tpp_wall();
  else if (!strcmp(v,"error")) CURRENT.l_flags |= TPPLEXER_FLAG_WERROR;
  else if (!strcmp(v,"system-headers")) CURRENT.l_flags |= TPPLEXER_FLAG_WSYSTEMHEADERS;
  else goto enable_warning;
  break;

 { int error; /* Configure warning groups. */
   if (DCC_MACRO_FALSE) { case OPT_Wno:   error = TPPLexer_SetWarnings(v,WSTATE_DISABLE); }
   if (DCC_MACRO_FALSE) { enable_warning: error = TPPLexer_SetWarnings(v,WSTATE_WARN);    }
   if (error == 2) {
    struct TPPConst val;
    val.c_kind = TPP_CONST_STRING;
    val.c_data.c_string = TPPString_New(v,strlen(v));
    if (val.c_data.c_string) WARN(W_INVALID_WARNING,&val),
                             TPPString_Decref(val.c_data.c_string);
   }
 } break;

 {
  int enable;
 case OPT_fno:
 case OPT_f:
  enable = c->c_id == OPT_f;
#define SETFLAG(s,f) (enable?((s)|=(f)):((s)&=~(f)))

       if (!strcmp(v,"spc") && from_cmd) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_WANTSPACE);
  else if (!strcmp(v,"lf") && from_cmd) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_WANTLF);
  else if (!strcmp(v,"comments") && from_cmd) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_WANTCOMMENTS);
  else if (!strcmp(v,"longstring")) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_TERMINATE_STRING_LF);
  else {
   enable = TPPLexer_SetExtension(v,enable);
   if (enable == 2) WARN(W_UNKNOWN_EXTENSION,v);
  }
 } break;


 case OPT_undef:
  TPPLexer_DisableExtension(EXT_CPU_MACROS);
  TPPLexer_DisableExtension(EXT_SYSTEM_MACROS);
  TPPLexer_DisableExtension(EXT_UTILITY_MACROS);
  break;
 case OPT_trigraphs: TPPLexer_EnableExtension(EXT_TRIGRAPHS); break;

 case OPT_UNUSED: break;
 default:
  WARN(W_CMD_UNKNOWN,c->c_val);
  break;
 }
 return;
seterr:
 TPPLexer_SetErr();
}

extern struct option const *const dcc_cmd_groups[];
INTERN void DCCCmd_Exec(int grp, int *argc, char ***argv) {
 struct cmd c;
 cmd_init(&c,*argc,*argv);
 for (;;) {
  if (grp != OPG_grp_main)
      c.c_grp = dcc_cmd_groups[grp];
  if (!OK || !cmd_yield(&c)) break;
  exec_cmd(&c,0);
 }
 *argc = c.c_argc;
 *argv = c.c_argv;
}

INTERN void DCCCmd_ExecString(int grp, char const *str, size_t len) {
 int argc;
 char **argv;
 (void)len; /* TODO: Split arguments. */
 argc = 1;
 argv = (char **)&str;
 DCCCmd_Exec(grp,&argc,&argv);
}


DCC_DECL_END

#endif /* !GUARD_DCC_CMD_PARSE_C_INL */
