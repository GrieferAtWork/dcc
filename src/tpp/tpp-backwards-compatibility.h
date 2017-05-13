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
#ifndef GUARD_TPP_BACKWARDS_COMPATIBILITY_H
#define GUARD_TPP_BACKWARDS_COMPATIBILITY_H 1

#ifndef TPP
#define TPP(x) TPP_##x
#endif

#include "tpp.h"

/* NOTE: This header is completely optional and only required
 *       for backwards compatibility with the old TPP. */

#ifdef __cplusplus
extern "C" {
#endif

/* Old token names. */
#define TPP_TOK_EOF            TPP(TOK_EOF)
#define TPP_TOK_SPACE          TPP(TOK_SPACE)
#define TPP_TOK_LF             TPP(TOK_LF)
#define TPP_TOK_INT            TPP(TOK_INT)
#define TPP_TOK_FLOAT          TPP(TOK_FLOAT)
#define TPP_TOK_COMMENT        TPP(TOK_COMMENT)
#define TPP_TOK_CHR            TPP(TOK_CHAR)
#define TPP_TOK_STR            TPP(TOK_STRING)
#define TPP_TOK_ASSIGN         TPP(TOK_ASSIGN)
#define TPP_TOK_CMP_LO         TPP(TOK_LOWER)
#define TPP_TOK_CMP_GR         TPP(TOK_GREATER)
#define TPP_TOK_QUESTION       TPP(TOK_QUESTION)
#define TPP_TOK_COLLON         TPP(TOK_COLLON)
#define TPP_TOK_ADD            TPP(TOK_ADD)
#define TPP_TOK_SUB            TPP(TOK_SUB)
#define TPP_TOK_MUL            TPP(TOK_MUL)
#define TPP_TOK_DIV            TPP(TOK_DIV)
#define TPP_TOK_MOD            TPP(TOK_MOD)
#define TPP_TOK_DOT            TPP(TOK_DOT)
#define TPP_TOK_COMMA          TPP(TOK_COMMA)
#define TPP_TOK_SEMICOLON      TPP(TOK_SEMICOLON)
#define TPP_TOK_HASH           TPP(TOK_HASH)
#define TPP_TOK_LPAREN         TPP(TOK_LPAREN)
#define TPP_TOK_RPAREN         TPP(TOK_RPAREN)
#define TPP_TOK_LBRACKET       TPP(TOK_LBRACKET)
#define TPP_TOK_RBRACKET       TPP(TOK_RBRACKET)
#define TPP_TOK_LBRACE         TPP(TOK_LBRACE)
#define TPP_TOK_RBRACE         TPP(TOK_RBRACE)
#define TPP_TOK_TILDE          TPP(TOK_TILDE)
#define TPP_TOK_AT             TPP(TOK_AT)
#define TPP_TOK_BIN_AND        TPP(TOK_AND)
#define TPP_TOK_BIN_OR         TPP(TOK_OR)
#define TPP_TOK_BIN_XOR        TPP(TOK_XOR)
#define TPP_TOK_EXCLAIM        TPP(TOK_NOT)
#define TPP_TOK_CMP_LE         TPP(TOK_LOWER_EQUAL)
#define TPP_TOK_CMP_EQ         TPP(TOK_EQUAL)
#define TPP_TOK_CMP_NE         TPP(TOK_NOT_EQUAL)
#define TPP_TOK_CMP_GE         TPP(TOK_GREATER_EQUAL)
#define TPP_TOK_ADD_ASSIGN     TPP(TOK_ADD_EQUAL)
#define TPP_TOK_SUB_ASSIGN     TPP(TOK_SUB_EQUAL)
#define TPP_TOK_MUL_ASSIGN     TPP(TOK_MUL_EQUAL)
#define TPP_TOK_DIV_ASSIGN     TPP(TOK_DIV_EQUAL)
#define TPP_TOK_MOD_ASSIGN     TPP(TOK_MOD_EQUAL)
#define TPP_TOK_SHL            TPP(TOK_SHL)
#define TPP_TOK_SHR            TPP(TOK_SHR)
#define TPP_TOK_SHL_ASSIGN     TPP(TOK_SHL_EQUAL)
#define TPP_TOK_SHR_ASSIGN     TPP(TOK_SHR_EQUAL)
#define TPP_TOK_DOTS           TPP(TOK_DOTS)
#define TPP_TOK_GLUE           TPP(TOK_GLUE)
#define TPP_TOK_BIN_AND_ASSIGN TPP(TOK_AND_EQUAL)
#define TPP_TOK_BIN_OR_ASSIGN  TPP(TOK_OR_EQUAL)
#define TPP_TOK_BIN_XOR_ASSIGN TPP(TOK_XOR_EQUAL)
#define TPP_TOK_LAND           TPP(TOK_LAND)
#define TPP_TOK_LOR            TPP(TOK_LOR)
#define TPP_TOK_INC            TPP(TOK_INC)
#define TPP_TOK_DEC            TPP(TOK_DEC)
#define TPP_TOK_POW            TPP(TOK_POW)
#define TPP_TOK_POW_ASSIGN     TPP(TOK_POW_EQUAL)
#define TPP_TOK_LXOR           TPP(TOK_LXOR)
#define TPP_TOK_TILDE_TILDE    TPP(TOK_TILDE_TILDE)
#define TPP_TOK_ARROW          TPP(TOK_ARROW)
#define TPP_TOK_COLLON_ASSIGN  TPP(TOK_COLLON_EQUAL)
#define TPP_TOK_COLLON_COLLON  TPP(TOK_COLLON_COLLON)
#define TPP_TOK_IDENT_START    TPP(TOK_KEYWORD_BEGIN)

typedef TPP(tok_t) TPPTokenID;
#define TPPTokenID_IS_KEYWORD      TPP_ISKEYWORD
#define TPPTokenID_IS_USER_KEYWORD TPP_ISUSERKEYWORD
#define TPPTokenID_IS_INT(id)    ((id) == TPP_TOK_INT || (id) == TPP_TOK_CHR)

typedef TPP(int_t) TPPInteger;
typedef TPP(int_t) TPPUInteger;
typedef TPP(int_t) TPPCounter;
typedef size_t     TPPKeywordHash;

#define TPPMacroCallingConvention_PAREN   0
#define TPPMacroCallingConvention_BRACKET 1
#define TPPMacroCallingConvention_BRACE   2
#define TPPMacroCallingConvention_ANGLE   3

#define TPPKeywordFlag_NONE                   0x00
#define TPPKeywordFlag_HAS_ATTRIBUTE          0x01
#define TPPKeywordFlag_HAS_BUILTIN            0x02
#define TPPKeywordFlag_HAS_CPP_ATTRIBUTE      0x04
#define TPPKeywordFlag_HAS_DECLSPEC_ATTRIBUTE 0x08
#define TPPKeywordFlag_HAS_EXTENSION          0x10
#define TPPKeywordFlag_HAS_FEATURE            0x20
#define TPPKeywordFlag_IS_DEPRECATED          0x40

#  define TPPLexer_FLAG_NONE          TPPLEXER_FLAG_NONE
#  define TPPLexer_FLAG_WANT_LF       TPPLEXER_FLAG_WANTLF
#  define TPPLexer_FLAG_WANT_SPC      TPPLEXER_FLAG_WANTSPACE
#  define TPPLexer_FLAG_NO_MACROS     TPPLEXER_FLAG_NO_MACROS
#  define TPPLexer_FLAG_NO_DIRECTIVES TPPLEXER_FLAG_NO_DIRECTIVES
#  define TPPLexer_FLAG_ASM_COMMENTS  TPPLEXER_FLAG_ASM_COMMENTS
#  define TPPLexer_FLAG_ONE_FILE      TPPLEXER_FLAG_NO_POP_ON_EOF
//#define TPPLexer_FLAG_ONE_REAL_FILE TPPLEXER_FLAG_NO_POP_ON_EOF
#  define TPPLexer_FLAG_INC_STRING    TPPLEXER_FLAG_INCLUDESTRING
#  define TPPLexer_FLAG_RAND_INIT     TPPLEXER_FLAG_RANDOM_INITIALIZED
#  define TPPLexer_FLAG_NO_DEPRECATED TPPLEXER_FLAG_NO_DEPRECATED
#  define TPPLexer_FLAG_TOK_COMMENTS  TPPLEXER_FLAG_WANTCOMMENTS



#  define f_refcnt       f_refcnt
#  define f_prev         f_prev
#  define f_name         f_name
#  define f_new_name     f_textfile.f_usedname->s_text
#  define f_ref_file     f_macro.m_deffile
//#define f_ref_file_off f_macro.m_defline
#  define f_line_offset  f_textfile.f_lineoff
#  define f_iter         f_pos
#  define f_end          f_end
#  define f_data         f_text->s_text
typedef struct TPPFile TPPFileObject;

#  define tk_id        t_id
#  define tk_file      t_file
//#define tk_file_off  ...
#  define tk_str_file  t_file
#  define tk_str_begin t_begin
#  define tk_str_end   t_end
typedef struct TPPToken TPPTokenObject;

#  define he_next     k_next
#  define he_id       k_id
#  define he_flags    k_rare->kr_flags
#  define he_counter  k_rare->kr_counter
#  define he_raw_hash k_hash
#  define he_str_len  k_size
#  define he_str      k_name
typedef struct TPPKeyword TPPKeywordListHashEntryObject;

//#define kl_next       km_entryc
#  define kl_hash_alloc km_bucketc
#  define kl_hash       km_bucketv
typedef struct TPPKeywordMap TPPKeywordListObject;

#  define a_name             ai_id
#  define a_references       ai_ins_exp
#  define a_quote_references ai_ins_str
#define TPPMacroArgument TPP(arginfo_t)


//typedef ... TPPMacroObject;     /* Replaced by 'TPPFile'. */
//typedef ... TPPMacroListObject; /* Merged with 'TPPKeywordMap'. */


#  define il_size  il_pathc
//#define il_paths il_pathv
typedef struct TPPIncludeList TPPIncludeListObject;


#  define e_value   iss_mode
//#define e_token   ...
typedef struct TPPIfdefStackSlot TPPIfdefStackEntryObject;

#define s_size  is_slotc
#define s_alloc is_slota
#define s_stack is_slotv
typedef struct TPPIfdefStack TPPIfdefStackObject;

//typedef ... TPPIncludeCacheEntry;  /* Merged with 'TPPKeyword'. */
//typedef ... TPPIncludeCacheObject; /* Merged with 'TPPKeywordMap'. */

#define wc_prev     ws_prev
#define wc_warnings ws_state
typedef struct TPPWarningState TPPWarningsFrameObject;

#define w_top       w_curstate
typedef struct TPPWarnings TPPWarningsObject;


#  define l_files            l_token.t_file
#  define l_keywords         l_keywords
#  define l_flags            l_flags
#  define l_macros           l_keywords
//#define l_include_list     ...
#  define l_sys_include_list l_syspaths
#  define l_ifdef_stack      l_ifdef
#  define l_include_cache    l_keywords
//#define l_one_file_rec     ...
#  define l_warnings         l_warnings
#  define l_counter          l_counter
typedef struct TPPLexer TPPLexerObject;



#define TPPLexer_FastDefine(self,name,code) \
 (assert(TPPLexer_Current == (self)),TPPLexer_Define(name,strlen(name),code,strlen(code)) ? 0 : -1)



#ifdef __cplusplus
}
#endif


#endif /* !GUARD_TPP_BACKWARDS_COMPATIBILITY_H */
