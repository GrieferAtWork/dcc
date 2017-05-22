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

#define OPTION_O  OPTION
#define OPTION_A  OPTION_ALIAS
#define OPTION_N  OPTION_UNNAMED

#define OPT_FLG(name,str)         OPTION(name,OPTION_FLAG_NONE,str,str,NULL)
#define OPT_VAL(name,str)         OPTION(name,OPTION_FLAG_VALUE,str,str,NULL)
#define OPT_VAL2(name,short,long) OPTION(name,OPTION_FLAG_VALUE,short,long,NULL)
#define OPT_EQU(name,str)         OPTION(name,OPTION_FLAG_EQUAL,str,str,NULL)
#define OPT_EQU2(name,short,long) OPTION(name,OPTION_FLAG_EQUAL,short,long,NULL)
#define OPT_PRE(name,short,long)  OPTION(name,OPTION_FLAG_PREFIX,short,long,NULL)
#define OPT_GRP(short,long,group) OPTION_UNNAMED(OPTION_FLAG_NONE,short,long,group)
#define OPT_NOP(short,long)       OPTION_UNNAMED(OPTION_FLAG_NOOPT,short,long,NULL)

GROUP_BEGIN(grp_Wl)
   OPTION_O(OPT_Wl_Bsymbolic,        OPTION_FLAG_NONE, "Bsymbolic",NULL,        NULL) /* -Wl,-Bsymbolic */
   OPTION_O(OPT_Wl_shared,           OPTION_FLAG_NONE, "shared",   NULL,        NULL) /* -Wl,-shared */
   OPTION_A(OPT_Wl_shared,           OPTION_FLAG_NONE, "Bshareable",NULL,       NULL) /* -Wl,-Bshareable */
   OPTION_O(OPT_Wl_nostdlib,         OPTION_FLAG_NONE, "nostdlib", NULL,        NULL) /* -Wl,-nostdlib */
   OPTION_O(OPT_Wl_pie,              OPTION_FLAG_NONE, "pie",      NULL,        NULL) /* -Wl,-pie */
   OPTION_O(OPT_Wl_pic_executable,   OPTION_FLAG_NONE, NULL,       "pic-executable",NULL) /* -Wl,--pic-executable */
   OPTION_O(OPT_Wl_init,             OPTION_FLAG_EQUAL,"init",     NULL,        NULL) /* -Wl,-init=my_init */
   OPTION_O(OPT_Wl_fini,             OPTION_FLAG_EQUAL,"fini",     NULL,        NULL) /* -Wl,-fini=my_fini */
   OPTION_O(OPT_Wl_entry,            OPTION_FLAG_VALUE,"e",        NULL,        NULL) /* -Wl,-e,foo */
   OPTION_A(OPT_Wl_entry,            OPTION_FLAG_EQUAL,NULL,"entry",            NULL) /* -Wl,--entry=_start */
   OPTION_O(OPT_Wl_defsym,           OPTION_FLAG_VALUE,NULL,"defsym",           NULL) /* -Wl,--defsym,foo=42 */
   OPTION_O(OPT_Wl_section_start,    OPTION_FLAG_EQUAL,NULL,"section-start",    NULL) /* -Wl,--section-start=.text=0x803000 */
   OPTION_O(OPT_Wl_image_base,       OPTION_FLAG_EQUAL,NULL,"image-base",       NULL) /* -Wl,--image-base=0x801000 */
   OPTION_O(OPT_Wl_section_alignment,OPTION_FLAG_EQUAL,NULL,"section-alignment",NULL) /* -Wl,--section-alignment=4096 */
#if DCC_TARGET_BIN == DCC_BINARY_PE
   OPTION_O(OPT_Wl_file_alignment,   OPTION_FLAG_EQUAL,NULL,"file-alignment",   NULL) /* -Wl,--file-alignment=256 */
   OPTION_O(OPT_Wl_stack,            OPTION_FLAG_VALUE,NULL,"stack",            NULL) /* -Wl,--stack,0x1000 */
   OPTION_O(OPT_Wl_subsystem,        OPTION_FLAG_VALUE,NULL,"subsystem",        NULL) /* -Wl,--subsystem,windows */
#endif
   OPTION_O(OPT_Wl_soname,           OPTION_FLAG_VALUE,"h",NULL,                NULL) /* -Wl,-h,foo */
   OPTION_A(OPT_Wl_soname,           OPTION_FLAG_EQUAL,NULL,"soname",           NULL) /* -Wl,--soname=foo */
   OPTION_O(OPT_Wl_fatal_warnings,   OPTION_FLAG_NONE, NULL,"fatal-warnings",   NULL) /* -Wl,--fatal-warnings */
   OPTION_O(OPT_Wl_no_fatal_warnings,OPTION_FLAG_NONE, NULL,"no-fatal-warnings",NULL) /* -Wl,--no-fatal-warnings */
   OPTION_O(OPT_Wl_allow_multiple_definition,OPTION_FLAG_NONE,NULL,"allow-multiple-definition",NULL) /* -Wl,--allow-multiple-definition */
   OPTION_O(OPT_Wl_allow_shlib_undefined,OPTION_FLAG_NONE,NULL,"allow-shlib-undefined",NULL) /* -Wl,--allow-shlib-undefined */
   OPTION_O(OPT_Wl_no_allow_shlib_undefined,OPTION_FLAG_NONE,NULL,"no-allow-shlib-undefined",NULL) /* -Wl,--no-allow-shlib-undefined */
   OPTION_O(OPT_Wl_no_warn_mismatch,OPTION_FLAG_NONE,NULL,"no-warn-mismatch",NULL) /* -Wl,--no-warn-mismatch */
   OPTION_O(OPT_Wl_no_warn_search_mismatch,OPTION_FLAG_NONE,NULL,"no-warn-search-mismatch",NULL) /* -Wl,--no-warn-search-mismatch */

   OPTION_A(OPT_o,                   OPTION_FLAG_VALUE,"o",NULL,                NULL) /* -Wl,-o,a.out */
   OPTION_A(OPT_o,                   OPTION_FLAG_EQUAL,NULL,"output",           NULL) /* -Wl,--output=a.out */
   OPTION_A(OPT_L,                   OPTION_FLAG_VALUE,"L",NULL,                NULL) /* -Wl,-L/usr/lib */
   OPTION_A(OPT_L,                   OPTION_FLAG_EQUAL,NULL,"library-path",     NULL) /* -Wl,--library-path=/usr/lib */
   OPTION_A(OPT_O,                   OPTION_FLAG_VALUE,"O",NULL,                NULL) /* -Wl,-O3 */
   /* TODO: '-Wl,-rpath=/usr/lib'
    *    >> Add directory that will be added to the ELF runtime search path list. */

   /* Ignored options. */
   OPTION_A(OPT_UNUSED,              OPTION_FLAG_NONE, "qmagic",   NULL,        NULL) /* -Wl,-qmagic */
   OPTION_A(OPT_UNUSED,              OPTION_FLAG_NONE, "Qy",       NULL,        NULL) /* -Wl,-Qy */
   OPTION_A(OPT_UNUSED,              OPTION_FLAG_NONE, "i",        NULL,        NULL) /* -Wl,-i */
   OPTION_A(OPT_UNUSED,              OPTION_FLAG_NONE, "r",        NULL,        NULL) /* -Wl,-r */
   OPTION_O(OPT_UNUSED,              OPTION_FLAG_NONE, "g",        NULL,        NULL) /* -Wl,-g */
GROUP_END

GROUP_BEGIN(grp_main) /* Main options group */
   OPTION_N(OPTION_FLAG_NOOPT,"l",NULL,NULL) /* -l... (dynamic library dependency) */
   OPTION_N(OPTION_FLAG_NONE,"Wl",NULL,grp_Wl) /* -Wl,... */

   OPTION_O(OPT_E,  OPTION_FLAG_NONE,  "E",   NULL,          NULL) /* -E */
   OPTION_O(OPT_D,  OPTION_FLAG_VALUE, "D",   NULL,          NULL) /* -DFOO */
   OPTION_O(OPT_U,  OPTION_FLAG_VALUE, "U",   NULL,          NULL) /* -UFOO */
   OPTION_O(OPT_A,  OPTION_FLAG_VALUE, "A",   NULL,          NULL) /* -AFOO */
   OPTION_O(OPT_I,  OPTION_FLAG_VALUE, "I",   NULL,          NULL) /* -Iinclude */
   OPTION_O(OPT_o,  OPTION_FLAG_VALUE, "o",   NULL,          NULL) /* -o a.exe */
   OPTION_O(OPT_c,  OPTION_FLAG_NONE,  "c",   NULL,          NULL) /* -c */
   OPTION_O(OPT_L,  OPTION_FLAG_VALUE, "L",   NULL,          NULL) /* -L/usr/lib */
   OPTION_O(OPT_O,  OPTION_FLAG_VALUE, "O",   NULL,          NULL) /* -O3 */

   OPTION_O(OPT_Wno,OPTION_FLAG_PREFIX,"Wno-",NULL,NULL) /* -Wno-syntax */
   OPTION_O(OPT_W,  OPTION_FLAG_PREFIX,"W",   NULL,NULL) /* -Wsyntax */
   OPTION_O(OPT_fno,OPTION_FLAG_PREFIX,"fno-",NULL,NULL) /* -fno-expression-statements */
   OPTION_O(OPT_f,  OPTION_FLAG_PREFIX,"f",   NULL,NULL) /* -fexpression-statements */

   /* GCC extension flags. */
   OPTION_O(OPT_undef,       OPTION_FLAG_NONE,"undef",    NULL,NULL) /* -undef */
   OPTION_O(OPT_trigraphs,   OPTION_FLAG_NONE,"trigraphs",NULL,NULL) /* -trigraphs */
   OPTION_A(OPT_Wl_Bsymbolic,OPTION_FLAG_NONE,"symbolic", NULL,NULL) /* -symbolic */
GROUP_END


#undef OPTION_N
#undef OPTION_A
#undef OPTION_O

