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
#ifndef GUARD_DCC_UNIT_IMPORT_DEF_DYNAMIC_C_INL
#define GUARD_DCC_UNIT_IMPORT_DEF_DYNAMIC_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/target.h>

#if DCC_LIBFORMAT_DEF_DYNAMIC
#include <dcc/unit.h>
#include <dcc/lexer.h>

#include "lexer-priv.h"

#include <string.h>
#include <stdlib.h>

DCC_DECL_BEGIN

PRIVATE char *getline(struct TPPFile *__restrict f) {
 char *result;
 char *line_end;
 size_t file_size;
 assert(f);
 result = f->f_pos;
 assert(result <= f->f_end);
 *result = f->f_textfile.f_padding[0];
 if (result == f->f_end) {
  if (!TPPFile_NextChunk(f,TPPFILE_NEXTCHUNK_FLAG_NONE))
       return NULL; /* EOF */
  result = f->f_pos;
 }
 if (result != f->f_begin &&
     result[-1] == '\r' &&
     result[0] == '\n') ++result;
 assert(result <= f->f_end);
 file_size = (size_t)(f->f_end-(result+1));
                line_end = (char *)memchr(result+1,'\r',file_size);
 if (!line_end) line_end = (char *)memchr(result+1,'\n',file_size);
 if (!line_end) line_end = f->f_end;
 f->f_textfile.f_padding[0] = *line_end;
 *line_end = '\0';
 f->f_pos = line_end;
 return result;
}


INTERN int DCCUNIT_IMPORTCALL
DCCUnit_DynLoadDEF2(struct DCCLibDef *__restrict def,
                      char const *__restrict file, stream_t fd) {
 struct DCCSection *ressec = NULL;
 assert(def);
 assert(file);
 { /* Open the file and begin parsing it. */
   char *line; int state = 0;
   struct TPPFile *f = TPPFile_OpenStream(fd,file);
   if unlikely(!f) goto end;
   f->f_textfile.f_padding[0] = '\0';
   /* Link the file into TPP, so that when we
    * emit warnings, they'll point into our file. */
   f->f_prev = TOKEN.t_file;
   TOKEN.t_file = f;

   /* Read the file line-by-line */
   while ((line = getline(f)) != NULL) {
    while (tpp_isspace_nz(*line)) ++line; /* Skip leading whitespace */
    if (!*line || *line == ';') continue; /* Skip comments & empty lines. */
    switch (state) {
    { /* START */
     char *lib_end;
     struct TPPKeyword *lib_name;
    case 0: /* The file must start with LIBRARY. - If it doesn't, it's not a .def file! */
     if (memcmp(line,"LIBRARY",7*sizeof(char)) != 0) goto done;
     line += 7;
     if (!tpp_isspace_nz(*line)) goto done;
     do ++line; while (tpp_isspace_nz(*line));
     /* Figure out the library name described by the file. */
     lib_end = line+strlen(line);
     while (lib_end != line && tpp_isspace(lib_end[-1])) --lib_end;
     if (def->ld_flags&DCC_LIBDEF_FLAG_USERNAME)
          lib_name = TPPLexer_LookupKeyword(def->ld_name,def->ld_size,1);
     else lib_name = TPPLexer_LookupKeyword(line,(size_t)(lib_end-line),1);
     if unlikely(!lib_name) goto done;
     def->ld_dynlib = ressec = DCCUnit_NewSec(lib_name,DCC_SYMFLAG_SEC_ISIMPORT);
     if unlikely(!ressec) goto done;
     state = 1;
    } break;

    { /* SCAN for EXPORTS. */
    case 1:
     if (memcmp(line,"EXPORTS",7*sizeof(char)) != 0) {
expect_exports:
      WARN(W_LIB_DEF_EXPECTED_EXPORTS,line);
      break;
     }
     line += 7;
     while (tpp_isspace_nz(*line)) ++line;
     if (*line) goto expect_exports;
     state = 2;
    } break;

    { /* Define exports. */
     char *export_end;
     struct TPPKeyword *export_kwd;
     struct DCCSym *export_sym;
    default:
     assert(ressec);
     export_end = line+strlen(line);
     while (export_end != line && tpp_isspace(export_end[-1])) --export_end;
     export_kwd = TPPLexer_LookupKeyword(line,(size_t)(export_end-line),1);
     if (export_kwd) {
#ifdef DCC_SYMFLAG_DLLIMPORT
      export_sym = DCCUnit_NewSym(export_kwd,DCCLibDef_EXPFLAGS(def,DCC_SYMFLAG_DLLIMPORT));
#else
      export_sym = DCCUnit_NewSym(export_kwd,DCCLibDef_EXPFLAGS(def,DCC_SYMFLAG_NONE));
#endif
      if (export_sym && export_sym->sy_sec != ressec) {
       DCCSym_Define(export_sym,ressec,0,0);
      }
     }
    } break;
    }
   }
done:
   assert(TOKEN.t_file == f);
   TOKEN.t_file = f->f_prev;
   TPPFile_Decref(f);
 }
end:
 return ressec != NULL;
}

INTERN int DCCUNIT_IMPORTCALL
DCCUnit_DynLoadDEF(struct DCCLibDef *__restrict def,
                     char const *__restrict file, stream_t fd) {
 size_t len;
 assert(def);
 assert(file);
 /* Check for the '.def' file extension. */
 if ((len = strlen(file)) < 4 ||
     memcmp(file+len-4,".def",4*
     sizeof(char)) != 0) return NULL;
 return DCCUnit_DynLoadDEF2(def,file,fd);
}

DCC_DECL_END
#endif /* DCC_LIBFORMAT_DEF_DYNAMIC */

#endif /* !GUARD_DCC_UNIT_IMPORT_DEF_DYNAMIC_C_INL */
