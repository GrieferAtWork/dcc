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

#include <dcc/assembler.h>
#include <dcc/lexer.h>
#include <dcc/compiler.h>
#include <dcc/unit.h>
#include <dcc/linker.h>
#include <dcc/preprocessor.h>

#include "cmd.h"

#include <string.h>

#if DCC_TARGET_BIN == DCC_BINARY_PE
#include "linker-pe.h"
#endif

DCC_DECL_BEGIN

INTDEF DCC_ATTRIBUTE_NORETURN void dcc_help(char const *subject);
INTDEF DCC_ATTRIBUTE_NORETURN void dcc_version(void);


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
LOCAL target_ptr_t hextoint(char const *s) {
 target_ptr_t result = 0;
 char ch;
 while ((ch = *s,
         ch == '0' ||
         ch == 'x' ||
         ch == 'x'
         )) ++s;
 for (;;) {
  ch = *s++;
       if (ch >= '0' && ch <= '9') result = (result*16)+(ch-'0');
  else if (ch >= 'a' && ch <= 'z') result = (result*16)+(ch-'a');
  else if (ch >= 'A' && ch <= 'Z') result = (result*16)+(ch-'A');
  else if (!ch) break;
 }
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
  *iter = b;
 }
 ext_end = (ext_iter = CURRENT.l_warnings.w_curstate->ws_extendedv)+
                       CURRENT.l_warnings.w_curstate->ws_extendeda;
 for (; ext_iter != ext_end; ++ext_iter) {
  if (ext_iter->wse_oldstate == WSTATE_DISABLE)
      ext_iter->wse_oldstate =  WSTATE_WARN;
 }
}

PRIVATE void asm_strexpr(struct DCCSymAddr *__restrict v,
                         char const *__restrict str) {
 struct TPPString *s = TPPString_New(str,strlen(str));
 struct TPPFile *f,*oldeof;
 if unlikely(!s) return;
 f = TPPFile_NewExplicitInherited(s);
 if unlikely(!f) { TPPString_Decref(s); return; }
 assert(TOKEN.t_begin >= TOKEN.t_file->f_begin);
 assert(TOKEN.t_begin <= TOKEN.t_file->f_end);
 TOKEN.t_file->f_pos = TOKEN.t_begin;
 oldeof = CURRENT.l_eof_file;
 CURRENT.l_eof_file = f;
 TPPLexer_PushFileInherited(f);
 DCCParse_AsmBegin();
 YIELD();
 DCCParse_AsmExpr(v);
 DCCParse_AsmEnd();
 while (TOK > 0) YIELD();
 CURRENT.l_eof_file = oldeof;
 YIELD();
}

LOCAL char *getval(char *v) {
 char *result = strchr(v,'=');
 if (result) *result++ = '\0';
 else         result   = (char *)"0";
 return result;
}

INTDEF size_t /* From 'tpp.c' */
fuzzy_match(char const *__restrict a, size_t alen,
            char const *__restrict b, size_t blen);

#define CMD_ONLY() do{if (!from_cmd) goto illegal; }while(DCC_MACRO_FALSE)
INTERN void exec_cmd(struct cmd *__restrict c, int from_cmd) {
 char *v = c->c_val;
 switch (c->c_id) {

 case OPT_Wl_Bsymbolic: linker.l_flags |= DCC_LINKER_FLAG_SYMBOLIC; break;
 case OPT_Wl_shared:    linker.l_flags |= DCC_LINKER_FLAG_SHARED; break;
 case OPT_Wl_nostdlib:  linker.l_flags |= DCC_LINKER_FLAG_NOSTDLIB; break;
 case OPT_Wl_pie:            linker.l_flags |= DCC_LINKER_FLAG_PIC; break;
 case OPT_Wl_pic_executable: linker.l_flags |= DCC_LINKER_FLAG_PIC;
                             linker.l_flags &= ~(DCC_LINKER_FLAG_SHARED);
                             break;

 case OPT_Wl_no_clear_unused:     linker.l_flags &= ~(DCC_LINKER_FLAG_O_CLRUNUSED); break;
 case OPT_Wl_clear_unused:        linker.l_flags |=  (DCC_LINKER_FLAG_O_CLRUNUSED); break;
 case OPT_Wl_no_collapse:         linker.l_flags &= ~(DCC_LINKER_FLAG_O_COLLSEC); break;
 case OPT_Wl_collapse:            linker.l_flags |=  (DCC_LINKER_FLAG_O_COLLSEC); break;
 case OPT_Wl_no_merge_sym:        linker.l_flags |=  (DCC_LINKER_FLAG_O_NOMERGESYM); break;
 case OPT_Wl_merge_sym:           linker.l_flags &= ~(DCC_LINKER_FLAG_O_NOMERGESYM); break;

 {
  struct DCCSection *sec;
  target_ptr_t addr;
  {
   char *new_secname;
   if (DCC_MACRO_FALSE) { case OPT_Wl_Tbss:  new_secname = ".bss"; }
   if (DCC_MACRO_FALSE) { case OPT_Wl_Tbata: new_secname = ".data"; }
   if (DCC_MACRO_FALSE) { case OPT_Wl_Ttext: new_secname = ".text"; }
   addr = hextoint(v);
   v = new_secname;
   goto def_secbase;
  }
 case OPT_Wl_section_start:
  addr = hextoint(getval(v));
def_secbase:
  sec = DCCUnit_GetSecs(v);
  if unlikely(!sec) WARN(W_CMD_WL_SECTION_START_UNKNOWN_SECTION,v);
  else {
   /* Set the section base address and mark it as fixed. */
   DCCSection_SetBaseTo(sec,addr);
   sec->sc_start.sy_flags |= DCC_SYMFLAG_SEC_FIXED;
  }
 } break;


 { /* initialization/finalization/entry symbols. */
  char *v_copy; size_t v_len;
 case OPT_Wl_init:
 case OPT_Wl_fini:
 case OPT_Wl_entry:
  v_len = strlen(v);
  v_copy = (char *)DCC_Malloc((v_len+1)*sizeof(char),0);
  if unlikely(!v_copy) break;
  memcpy(v_copy,v,(v_len+1)*sizeof(char));
  c->c_id == OPT_Wl_init ? (void)(linker.l_init  = v_copy) :
  c->c_id == OPT_Wl_fini ? (void)(linker.l_fini  = v_copy) :
                           (void)(linker.l_entry = v_copy);
 } break;

 case OPT_Wl_image_base:
  linker.l_imgbase = (target_ptr_t)strtoint(v);
  linker.l_flags  |= DCC_LINKER_FLAG_IMGBASE;
  break;

 { struct DCCSymAddr symaddr;
   struct DCCSym *defsym;
 case OPT_Wl_defsym:
   asm_strexpr(&symaddr,getval(v));
   defsym = DCCUnit_NewSyms(v,DCC_SYMFLAG_NONE);
   if unlikely(!defsym) break;
   DCCSym_DefAddr(defsym,&symaddr);
 } break;

 case OPT_Wl_section_alignment:
  linker.l_secalign = (target_siz_t)strtoint(v);
  break;

#if DCC_TARGET_BIN == DCC_BINARY_PE
 case OPT_Wl_file_alignment: linker.l_secalign = (target_siz_t)strtoint(v); break;
 case OPT_Wl_stack: linker.l_pe_stacksiz = (target_siz_t)strtoint(v); break;
 case OPT_Wl_subsystem:
  /* OK:   --subsystem which */
  /* TODO: --subsystem which:major */
  /* TODO: --subsystem which:major.minor */
       if (!strcmp(v,"windows")) /* ??? */;
  else if (!strcmp(v,"native"))     linker.l_pe_subsys = IMAGE_SUBSYSTEM_NATIVE;
  else if (!strcmp(v,"console"))    linker.l_pe_subsys = IMAGE_SUBSYSTEM_WINDOWS_CUI;
  else if (!strcmp(v,"gui"))        linker.l_pe_subsys = IMAGE_SUBSYSTEM_WINDOWS_GUI;
  else if (!strcmp(v,"os/2"))       linker.l_pe_subsys = IMAGE_SUBSYSTEM_OS2_CUI;
  else if (!strcmp(v,"posix"))      linker.l_pe_subsys = IMAGE_SUBSYSTEM_POSIX_CUI;
  else if (!strcmp(v,"efiapp"))     linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_APPLICATION;
  else if (!strcmp(v,"efiboot"))    linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER;
  else if (!strcmp(v,"efiruntime")) linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER;
  else if (!strcmp(v,"efirom"))     linker.l_pe_subsys = IMAGE_SUBSYSTEM_EFI_ROM;
  else if (!strcmp(v,"xbox"))       linker.l_pe_subsys = IMAGE_SUBSYSTEM_XBOX;
  else linker.l_pe_subsys = (uint8_t)strtoint(v);
  break;
#endif /* DCC_TARGET_BIN == DCC_BINARY_PE */
 case OPT_Wl_soname: linker.l_soname = TPPString_New(v,strlen(v)); break;

 case OPT_Wl_fatal_warnings:    CURRENT.l_flags |= TPPLEXER_FLAG_WERROR; break;
 case OPT_Wl_no_fatal_warnings: CURRENT.l_flags &= ~(TPPLEXER_FLAG_WERROR); break;
 case OPT_Wl_allow_multiple_definition:
  TPPLexer_SetWarning(W_SYMBOL_ALREADY_DEFINED_SEC,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_SYMBOL_ALREADY_DEFINED_IMP,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_SYMBOL_ALREADY_DEFINED_ALIAS,WSTATE_DISABLE);
  break;
 case OPT_Wl_allow_shlib_undefined:
  TPPLexer_SetWarning(W_UNRESOLVED_REFERENCE,WSTATE_DISABLE);
  break;
 case OPT_Wl_no_allow_shlib_undefined:
  TPPLexer_SetWarning(W_UNRESOLVED_REFERENCE,WSTATE_DEFAULT);
  break;
 case OPT_Wl_no_warn_mismatch:
#if DCC_LIBFORMAT_ELF
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_CLASS,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_DATA,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_VERSION,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_OSABI,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_MACHINE,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_VERSION2,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_STATIC_SHARED,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_DYNAMIC_EXEC,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_DYNAMIC_RELO,WSTATE_DISABLE);
#endif
  break;
 case OPT_Wl_no_warn_search_mismatch:
#if DCC_LIBFORMAT_PE_DYNAMIC || DCC_LIBFORMAT_PE_STATIC
  TPPLexer_SetWarning(W_LIB_PE_INVALID_MACHINE,WSTATE_DISABLE);
#endif
#if DCC_LIBFORMAT_ELF
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_DATA,WSTATE_DISABLE);
  TPPLexer_SetWarning(W_LIB_ELF_INVALID_MACHINE,WSTATE_DISABLE);
#endif
  break;


 { char *val;
 case OPT_D:
   val = getval(v);
   if (!TPPLexer_Define(v,strlen(v),val,strlen(val),
                        from_cmd ? TPPLEXER_DEFINE_FLAG_BUILTIN
                                 : TPPLEXER_DEFINE_FLAG_NONE)) goto seterr;
 } break;

 case OPT_U: TPPLexer_Undef(v,strlen(v)); break;
 case OPT_A: {
  int add = 1; char *val;
  if (*v == '-') ++v,add = 0;
  val = strchr(v,'=');
  if (val) *val++ = '\0';
  else if (add) { WARN(W_CMD_A_EXPECTED_VALUE); break; }
  add ? TPPLexer_AddAssert(v,strlen(v),val,strlen(val))
      : TPPLexer_DelAssert(v,strlen(v),val,val ? strlen(val) : 0);
 } break;

 case OPT_I: if (!TPPLexer_AddIncludePath(v,strlen(v))) goto seterr; break;
 case OPT_L: DCCLinker_AddLibPath(v,strlen(v)); break;

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
#define SETFLAG(s,f)  (enable?((s)|=(f)):((s)&=~(f)))
#define SETFLAGI(s,f) (enable?((s)&=~(f)):((s)|=(f)))
       if (!strcmp(v,"pic") || !strcmp(v,"PIC") ||
           !strcmp(v,"pie") || !strcmp(v,"PIE") &&
           enable) SETFLAG(linker.l_flags,DCC_LINKER_FLAG_PIC);
  else if (!memcmp(v,"visibility=",DCC_COMPILER_STRLEN("visibility=")*sizeof(char)) && enable) {
   char *val = v+DCC_COMPILER_STRLEN("visibility=");
   symflag_t newvis;
        if (!strcmp(val,"default"))   newvis = DCC_SYMFLAG_NONE;
   else if (!strcmp(val,"hidden"))    newvis = DCC_SYMFLAG_PRIVATE;
   else if (!strcmp(val,"protected")) newvis = DCC_SYMFLAG_PROTECTED;
   else if (!strcmp(val,"internal"))  newvis = DCC_SYMFLAG_INTERNAL;
   else { WARN(W_CMD_FVISIBILITY_UNKNOWN_VISIBILITY,val); break; }
   if (from_cmd) linker.l_visdefault             = newvis;
   else          compiler.c_visibility.vs_viscur = newvis;
  }
  else if (!strcmp(v,"leading-underscore")) SETFLAGI(linker.l_flags,DCC_LINKER_FLAG_NOUNDERSCORE);
  else if (!strcmp(v,"stack-check"));
  else if (!strcmp(v,"spc")) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_WANTSPACE);
  else if (!strcmp(v,"lf")) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_WANTLF);
  else if (!strcmp(v,"comments")) SETFLAG(TPPLexer_Current->l_flags,TPPLEXER_FLAG_WANTCOMMENTS);
  else if (!strcmp(v,"longstring") || !strcmp(v,"longstrings"))
   SETFLAGI(TPPLexer_Current->l_flags,TPPLEXER_FLAG_TERMINATE_STRING_LF);
  else if (!memcmp(v,"tabstop=",8*sizeof(char))) TPPLexer_Current->l_tabsize = (size_t)atol(v+8);
  else if (!strcmp(v,"signed-char")) TPPLexer_Current->l_flags |= ~(TPPLEXER_FLAG_CHAR_UNSIGNED);
  else if (!strcmp(v,"unsigned-char")) TPPLexer_Current->l_flags |= TPPLEXER_FLAG_CHAR_UNSIGNED;
  else if (!strcmp(v,"cpp-line")) preproc.p_flags = DCC_PREPROCESSOR_SET_LINEMODE(preproc.p_flags,DCC_PREPROCESSOR_FLAG_LINECXX);
  else if (!strcmp(v,"line")) preproc.p_flags = DCC_PREPROCESSOR_SET_LINEMODE(preproc.p_flags,enable
                                                                         ? DCC_PREPROCESSOR_FLAG_LINESTDC
                                                                         : DCC_PREPROCESSOR_FLAG_LINENONE);
  else if (!strcmp(v,"magiclf")) SETFLAGI(preproc.p_flags,DCC_PREPROCESSOR_FLAG_NOMAGICLF);
  else if (!strcmp(v,"decode")) SETFLAG(preproc.p_flags,DCC_PREPROCESSOR_FLAG_DECODETOK);
  else if (!strcmp(v,"unify-pragma")) SETFLAG(preproc.p_flags,DCC_PREPROCESSOR_FLAG_UNIFYPRGM);
  else {
   enable = TPPLexer_SetExtension(v,enable);
   if (!enable) WARN(W_UNKNOWN_EXTENSION,v);
  }
#undef SETFLAGI
#undef SETFLAG
 } break;

 /* Preprocessor flags. */
 case OPT_o: CMD_ONLY(); preproc.p_outfile = v; break;
 case OPT_c: CMD_ONLY(); preproc.p_flags |= DCC_PREPROCESSOR_FLAG_COMPILEONLY; break;
 case OPT_E: CMD_ONLY();
  preproc.p_flags = DCC_PREPROCESSOR_SET_PPMODE(preproc.p_flags,DCC_PREPROCESSOR_FLAG_PPPREPROC);
  TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_WANTSPACE|
                                TPPLEXER_FLAG_WANTLF);
  break;
 case OPT_P: CMD_ONLY(); preproc.p_flags = DCC_PREPROCESSOR_SET_LINEMODE(preproc.p_flags,DCC_PREPROCESSOR_FLAG_LINENONE); break;
 case OPT_M: CMD_ONLY(); preproc.p_flags |= DCC_PREPROCESSOR_FLAG_DEPSYSTEM; /* fallthrough. */
 case OPT_MM: CMD_ONLY(); /* Enable dependency mode. */
set_dep_mode:  preproc.p_flags = DCC_PREPROCESSOR_SET_PPMODE(preproc.p_flags,DCC_PREPROCESSOR_FLAG_PPDEP);
               TPPLexer_Current->l_flags |= (TPPLEXER_FLAG_NO_WARNINGS);
check_depfile: if (preproc.p_depfd == TPP_STREAM_INVALID) preproc.p_depfd = DCC_STREAM_STDOUT;
  break;
 case OPT_MD:  CMD_ONLY(); preproc.p_flags |= DCC_PREPROCESSOR_FLAG_DEPSYSTEM;
 case OPT_MMD: CMD_ONLY(); goto check_depfile;
 case OPT_MG:  CMD_ONLY();
  TPPLexer_Current->l_callbacks.c_unknown_file = &DCCPreprocessor_DepUnknown;
  TPPLexer_SetWarning(TPP(W_FILE_NOT_FOUND),TPP(WSTATE_DISABLE));
  goto set_dep_mode;
 case OPT_MP: CMD_ONLY(); preproc.p_flags |= DCC_PREPROCESSOR_FLAG_DEPDUMMY; break;

 case OPT_MF: CMD_ONLY();
  if (preproc.p_depfd != TPP_STREAM_INVALID &&
      preproc.p_depfd != DCC_STREAM_STDOUT) s_close(preproc.p_depfd);
  preproc.p_depfd = s_openw(v);
  break;

 case OPT_MQ: CMD_ONLY();
  preproc.p_flags |= DCC_PREPROCESSOR_FLAG_DEPESCAPE;
 case OPT_MT: CMD_ONLY();
  preproc.p_deptarget = v;
  break;

 case OPT_name:
  if (from_cmd) preproc.p_srcname = v;
  else {
   /* Override the used name of the base source file. */
   struct TPPFile *f = TOKEN.t_file;
   if (f != &TPPFile_Empty) {
    while (f->f_prev != &TPPFile_Empty) f = f->f_prev;
    if (f->f_kind == TPPFILE_KIND_TEXT) {
     struct TPPString *newname;
     if unlikely((newname = TPPString_New(v,strlen(v))) == NULL) break;
     if (f->f_textfile.f_usedname) TPPString_Decref(f->f_textfile.f_usedname);
     f->f_textfile.f_usedname = newname; /* Inherit reference. */
    }
   }
  }
  break;

  /* TODO: '-B' changes the internal library path. */

 case OPT_traditional:
  /* Enable old-style spelling of inplace operators ('x += 42;' --> 'x =+ 42;') */
  TPPLexer_Current->l_extokens |= TPPLEXER_TOKEN_EQUALBINOP;
  /* Disable old-style function warnings. */
  TPPLexer_SetWarning(WG_OLD_FUNCTION_DECL,WSTATE_DISABLE);
  break;


 case OPT_undef:
  TPPLexer_DisableExtension(EXT_CPU_MACROS);
  TPPLexer_DisableExtension(EXT_SYSTEM_MACROS);
  TPPLexer_DisableExtension(EXT_UTILITY_MACROS);
  break;
 case OPT_trigraphs: TPPLexer_EnableExtension(EXT_TRIGRAPHS); break;

 case OPT_message_format:
       if (!strcmp(v,"msvc")) CURRENT.l_flags |= TPPLEXER_FLAG_MSVC_MESSAGEFORMAT;
  else if (!strcmp(v,"gcc"))  CURRENT.l_flags &= ~(TPPLEXER_FLAG_MSVC_MESSAGEFORMAT);
  else WARN(W_CMD_MESSAGE_FORMAT_UNKNOWN,v);
  break;

 case OPT_ansi:
  DCCCompiler_SetStd(DCC_COMPILER_STD_ANSI);
  break;

 {
  struct DCCStdName const *iter;
  struct DCCStdName const *closest;
  size_t score,vlen;
 case OPT_std:
  for (iter = DCCCompiler_Std; iter->sn_nam; ++iter) {
   if (!strcmp(iter->sn_nam,v)) {
    DCCCompiler_SetStd(iter->sn_std);
    goto done_std;
   }
  }
  iter = DCCCompiler_Std;
  closest = NULL;
  score   = (size_t)-1;
  vlen    = strlen(v);
  for (; iter->sn_nam; ++iter) {
   size_t newscore = fuzzy_match(iter->sn_nam,strlen(iter->sn_nam),v,vlen);
   if (newscore < score || !closest) {
    closest = iter;
    score   = newscore;
   }
  }
  assert(closest);
  WARN(W_CMD_STD_UNKNOWN,v,closest->sn_nam);
done_std:;
 } break;

 {
  int level;
 case OPT_O:
  level = atoi(v);
  linker.l_flags &= ~(DCC_LINKER_FLAG_O_NOMERGESYM|
                      DCC_LINKER_FLAG_O_CLRUNUSED|
                      DCC_LINKER_FLAG_O_COLLSEC);
  if (level <= 0) linker.l_flags |= DCC_LINKER_FLAG_O_NOMERGESYM;
  if (level >= 1) linker.l_flags |= DCC_LINKER_FLAG_O_CLRUNUSED;
  if (level >= 2) linker.l_flags |= DCC_LINKER_FLAG_O_COLLSEC;
 } break;

 case OPT_UNUSED: break;

  /* NOTE: The 'help' and 'version' callbacks are noreturn,
   *       so no need to guard against fall-through */
 case OPT_help:
  if (from_cmd) {
   if (from_cmd == 2) goto help_def;
        if (!strcmp(v,"include-paths")) preproc.p_flags |= DCC_PREPROCESSOR_FLAG_HELPINC;
   else if (!strcmp(v,"library-paths")) preproc.p_flags |= DCC_PREPROCESSOR_FLAG_HELPLIB;
   else help_def: dcc_help(v);
  }
  break;

 case OPT_version:
  if (from_cmd) dcc_version();
illegal: WARN(W_CMD_ILLEGAL);
  break;

 default:
/*unknown:*/
  WARN(W_CMD_UNKNOWN,c->c_val);
  break;
 }
 return;
seterr: TPPLexer_SetErr();
}

INTDEF struct option const *const dcc_cmd_groups[];
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
