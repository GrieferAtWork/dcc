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
#ifndef KWD
#define TPP_DEFS_DEFINES_KWD
#define KWD(name,str)
#endif
#ifndef KWD_FLAGS
#define TPP_DEFS_DEFINES_KWD_FLAGS
#define KWD_FLAGS(kwd,flags)
#endif
#ifndef WGROUP
#define TPP_DEFS_DEFINES_WGROUP
#define WGROUP(name,str,default)
#endif
#ifndef WARNING
#define TPP_DEFS_DEFINES_WARNING
#define WARNING(name,groups,default)
#endif
#ifndef WARNING_NAMESPACE
#define TPP_DEFS_DEFINES_WARNING_NAMESPACE
#define WARNING_NAMESPACE(name,start)
#endif
#ifndef MACRO
#define TPP_DEFS_DEFINES_MACRO
/* Reference a keyword that would behave as a predefined
 * macro only defined when 'if' evaluates to true at runtime.
 * NOTE: Such a macro can still be re-defined by the user,
 *       but may later be #undef-ined again to re-enable
 *       its original meaning. */
#define MACRO(name,if)
#endif
#ifndef BUILTIN_MACRO
#define TPP_DEFS_DEFINES_BUILTIN_MACRO
#define BUILTIN_MACRO(name,value)
#endif
#ifndef BUILTIN_FUNCTION
#define TPP_DEFS_DEFINES_BUILTIN_FUNCTION
#define BUILTIN_FUNCTION(name,argc,expr)
#endif
#ifndef WARNING_MESSAGE
#define TPP_DEFS_DEFINES_WARNING_MESSAGE
#define WARNING_MESSAGE(name,expr)
#endif
#ifndef EXTENSION
#define TPP_DEFS_DEFINES_EXTENSION
#define EXTENSION(name,str,default)
#endif


#define DEF_K(name)         KWD(KWD_##name,#name)
#define DEF_M(name)         KWD(KWD_##name,#name) MACRO(KWD_##name,1)
#define DEF_M_IF(name,expr) KWD(KWD_##name,#name) MACRO(KWD_##name,expr)
#define DEF_WARNING(name,groups,default,expr) WARNING(name,groups,default) WARNING_MESSAGE(name,expr)

#define HAS_BUILTIN_IF(name,if)   KWD(KWD_##name,#name) KWD_FLAGS(KWD_##name,(if) ? TPP_KEYWORDFLAG_HAS_BUILTIN : 0u)
#define HAS_FEATURE_IF(name,if)   KWD(KWD_##name,#name) KWD_FLAGS(KWD_##name,(if) ? TPP_KEYWORDFLAG_HAS_FEATURE : 0u)
#define HAS_EXTENSION_IF(name,if) KWD(KWD_##name,#name) KWD_FLAGS(KWD_##name,(if) ? TPP_KEYWORDFLAG_HAS_EXTENSION : 0u)

#define PREDEFINED_KWDMACRO(name,str,value)                       KWD(name,str) MACRO(name,1) BUILTIN_MACRO(name,value)
#define PREDEFINED_KWDMACRO_IF(name,str,if,value)                 KWD(name,str) MACRO(name,if) BUILTIN_MACRO(name,value)
#define PREDEFINED_MACRO(name,value)                              PREDEFINED_KWDMACRO(KWD_##name,#name,value)
#define PREDEFINED_MACRO_IF(name,if,value)                        PREDEFINED_KWDMACRO_IF(KWD_##name,#name,if,value)
#define PREDEFINED_KWDFUNCTION_IF(name,str,if,argc,expr)          KWD(name,str) BUILTIN_FUNCTION(name,(if) ? (int)(argc) : -1,expr)
#define PREDEFINED_KWDFUNCTION(name,str,argc,expr)                KWD(name,str) BUILTIN_FUNCTION(name,argc,expr)
#define PREDEFINED_FUNCTION_IF(name,if,argc,expr)                 PREDEFINED_KWDFUNCTION_IF(KWD_##name,#name,if,argc,expr)
#define PREDEFINED_FUNCTION(name,argc,expr)                       PREDEFINED_KWDFUNCTION_IF(KWD_##name,#name,1,argc,expr)
#if TPP_CONFIG_MINGCCFUNC < 2
#define PREDEFINED_BUILTIN_KWDFUNCTION_IF(name,str,if,argc,expr)  KWD(name,str) KWD_FLAGS(name,(HAS(EXT_BUILTIN_FUNCTIONS) && (if)) ? (TPP_KEYWORDFLAG_HAS_BUILTIN|TPP_KEYWORDFLAG_HAS_TPP_BUILTIN) : 0) BUILTIN_FUNCTION(name,(if) ? (int)(argc) : -1,expr)
#define PREDEFINED_BUILTIN_KWDFUNCTION(name,str,argc,expr)        KWD(name,str) KWD_FLAGS(name,HAS(EXT_BUILTIN_FUNCTIONS) ? (TPP_KEYWORDFLAG_HAS_BUILTIN|TPP_KEYWORDFLAG_HAS_TPP_BUILTIN) : 0) BUILTIN_FUNCTION(name,argc,expr)
#else
#define PREDEFINED_BUILTIN_KWDFUNCTION_IF(name,str,if,argc,expr)  KWD(name,str) KWD_FLAGS(name,(if) ? (TPP_KEYWORDFLAG_HAS_BUILTIN|TPP_KEYWORDFLAG_HAS_TPP_BUILTIN) : 0) BUILTIN_FUNCTION(name,(if) ? (int)(argc) : -1,expr)
#define PREDEFINED_BUILTIN_KWDFUNCTION(name,str,argc,expr)        KWD(name,str) KWD_FLAGS(name,TPP_KEYWORDFLAG_HAS_BUILTIN|TPP_KEYWORDFLAG_HAS_TPP_BUILTIN) BUILTIN_FUNCTION(name,argc,expr)
#endif
#define PREDEFINED_BUILTIN_FUNCTION_IF(name,if,argc,expr)         PREDEFINED_BUILTIN_KWDFUNCTION_IF(KWD_##name,#name,if,argc,expr)
#define PREDEFINED_BUILTIN_FUNCTION(name,argc,expr)               PREDEFINED_BUILTIN_KWDFUNCTION_IF(KWD_##name,#name,1,argc,expr)

#define TPP_PP_STR2(x) #x
#define TPP_PP_STR(x)  TPP_PP_STR2(x)

/* Keywords for known preprocessor directives. */
DEF_K(if)           /*< #if defined(FOO) && FOO == 10. */
DEF_K(ifdef)        /*< #ifdef FOOBAR. */
DEF_K(ifndef)       /*< #ifndef FOOBAR. */
DEF_K(elif)         /*< #elif defined(BAR) && BAR == 20. */
DEF_K(else)         /*< #else. */
DEF_K(endif)        /*< #endif. */
DEF_K(define)       /*< #define FOO 42. */
DEF_K(defined)      /*< #if defined(FOO). */
DEF_K(undef)        /*< #undef FOO. */
DEF_K(include)      /*< #include "header.h". */
DEF_K(include_next) /*< #include_next <stdlib.h>. */
DEF_K(import)       /*< #import "header.h". */
DEF_K(line)         /*< #line 42 "foo.h". */
DEF_K(error)        /*< #error C5A9. */
DEF_K(warning)      /*< #warning $H17. */
DEF_K(ident)        /*< #ident "text". */
DEF_K(sccs)         /*< #sccs "text". */
DEF_K(assert)       /*< #assert machine(i386). */
DEF_K(unassert)     /*< #assert unmachine. */

/* Various names for #pragma-directives (TPP supports everything) */
DEF_K(pragma)   /*< #pragma once. */
DEF_M(_Pragma)  /*< GCC defines _Pragma as a macro... So I'll just do that as well! */
DEF_M(__pragma) /*< I promised this would be defined as a macro... */

/* STD-C predefined macros. */
DEF_M(__FILE__)
DEF_M(__LINE__)
DEF_M(__TIME__)
DEF_M(__DATE__)
DEF_M_IF(__BASE_FILE__,    HAS(EXT_BASEFILE))
DEF_M_IF(__INCLUDE_LEVEL__,HAS(EXT_INCLUDE_LEVEL))
DEF_M_IF(__INCLUDE_DEPTH__,HAS(EXT_INCLUDE_LEVEL))
DEF_M_IF(__COUNTER__,      HAS(EXT_COUNTER))
DEF_M_IF(__TIMESTAMP__,    HAS(EXT_TIMESTAMP))
DEF_M_IF(__COLUMN__,       HAS(EXT_COLUMN))

DEF_M_IF(__is_identifier,         HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__is_builtin_identifier, HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__is_deprecated,         HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_attribute,         HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_builtin,           HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_tpp_builtin,       HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_cpp_attribute,     HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_declspec_attribute,HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_feature,           HAS(EXT_CLANG_FEATURES))
DEF_M_IF(__has_extension,         HAS(EXT_CLANG_FEATURES)) /* __has_extension(name) / __has_extension("-fname"): Query the state of an extension (returns 0 for disabled/unknown extensions). */
DEF_M_IF(__has_warning,           HAS(EXT_CLANG_FEATURES)) /* __has_warning("-Wname"): Query the state of a warning (returns 0 for unknown warnings). */
DEF_M_IF(__has_known_extension,   HAS(EXT_CLANG_FEATURES)) /* __has_known_extension("-fname"): Returns 0/1 if 'name' is a known extension. */
DEF_M_IF(__has_known_warning,     HAS(EXT_CLANG_FEATURES)) /* __has_known_warning(id) / __has_known_warning("-Wname"): Returns 0/1 if 'id'/'name' is a known warning. */
DEF_M_IF(__has_include,           HAS(EXT_HAS_INCLUDE))
DEF_M_IF(__has_include_next,      HAS(EXT_HAS_INCLUDE))

/* Helper keywords used to implement variadic macros & extensions.
 * HINT: '__VA_NARGS__' is something new I added when I began reviving TPP:
 *    >> #define _sum0()       0
 *    >> #define _sum1(a)      a
 *    >> #define _sum2(a,b)    a+b
 *    >> #define _sum3(a,b,c)  a+b+c
 *    >> #define sum(args...)  _sum##__VA_NARGS__(args)
 *    >>
 *    >> // Overloading macros by argument count was never this easy!
 *    >> sum()       // []
 *    >> sum( )      // [ ] (or '[]' depending on 'TPPLEXER_FLAG_KEEP_ARG_WHITESPACE')
 *    >> sum(42)     // [42]
 *    >> sum(42,16)  // [42][+][16]
 */
KWD(KWD___VA_ARGS__, "__VA_ARGS__")
KWD(KWD___VA_COMMA__,"__VA_COMMA__")
KWD(KWD___VA_NARGS__,"__VA_NARGS__")

/* Builtin pragmas. */
DEF_K(once)
DEF_K(push_macro)
DEF_K(pop_macro)
DEF_K(region)
DEF_K(endregion)
DEF_K(message)
DEF_K(deprecated)
DEF_K(tpp_exec)
DEF_K(tpp_set_keyword_flags)
DEF_K(extension)

/* Argument keywords for #pragma warning. */
#ifndef TPP_CONFIG_USERDEFINED_KWD_PUSH
DEF_K(push)
#endif
#ifndef TPP_CONFIG_USERDEFINED_KWD_POP
DEF_K(pop)
#endif
DEF_K(disable)
DEF_K(enable)
DEF_K(suppress)
DEF_K(default)

/* Explicit namespace for TPP pragma extensions.
 * Can be used with the following pragmas:
 * >> #pragma TPP warning(...)
 * >> #pragma TPP extension(...)
 * >> #pragma TPP tpp_exec(...)
 * >> #pragma TPP tpp_set_keyword_flags(...)
 * >> #pragma TPP include_path(...)
 */
DEF_K(TPP)

/* Additional keywords used for pragmas capable of
 * adding/deleting/pushing/popping system #include-paths:
 * >> #include <stdlib.h> // FILE NOT FOUND
 * >> #pragma TPP include_path(push,+ "/usr/include")
 * >> #include <stdlib.h>
 * >> #pragma TPP include_path(pop)
 * >> #include <stdlib.h> // FILE NOT FOUND
 */
DEF_K(include_path)
DEF_K(clear)

/* Additional keywords required to implement some GCC stuff. */
DEF_K(GCC)
DEF_K(diagnostic)
DEF_K(ignored)
DEF_K(system_header)


/* TPP extension macros. */
DEF_M_IF(__TPP_EVAL,         HAS(EXT_TPP_EVAL))
DEF_M_IF(__TPP_LOAD_FILE,    HAS(EXT_TPP_LOAD_FILE))
DEF_M_IF(__TPP_COUNTER,      HAS(EXT_TPP_COUNTER))
DEF_M_IF(__TPP_RANDOM,       HAS(EXT_TPP_RANDOM))
DEF_M_IF(__TPP_STR_DECOMPILE,HAS(EXT_TPP_STR_DECOMPILE))
DEF_M_IF(__TPP_STR_AT,       HAS(EXT_TPP_STR_SUBSTR))
DEF_M_IF(__TPP_STR_SUBSTR,   HAS(EXT_TPP_STR_SUBSTR))
DEF_M_IF(__TPP_STR_PACK,     HAS(EXT_TPP_STR_PACK))
DEF_M_IF(__TPP_STR_SIZE,     HAS(EXT_TPP_STR_SIZE))
DEF_M_IF(__TPP_UNIQUE,       HAS(EXT_TPP_UNIQUE))
DEF_M_IF(__TPP_COUNT_TOKENS, HAS(EXT_TPP_COUNT_TOKENS))

DEF_M_IF(__DATE_DAY__,  HAS(EXT_DATEUTILS))
DEF_M_IF(__DATE_WDAY__, HAS(EXT_DATEUTILS))
DEF_M_IF(__DATE_YDAY__, HAS(EXT_DATEUTILS))
DEF_M_IF(__DATE_MONTH__,HAS(EXT_DATEUTILS))
DEF_M_IF(__DATE_YEAR__, HAS(EXT_DATEUTILS))
DEF_M_IF(__TIME_SEC__,  HAS(EXT_TIMEUTILS))
DEF_M_IF(__TIME_MIN__,  HAS(EXT_TIMEUTILS))
DEF_M_IF(__TIME_HOUR__, HAS(EXT_TIMEUTILS))


HAS_EXTENSION_IF(tpp_dollar_is_alpha,             HAS(EXT_DOLLAR_IS_ALPHA))
HAS_EXTENSION_IF(tpp_va_args,                     HAS(EXT_VA_ARGS))
HAS_EXTENSION_IF(tpp_named_va_args,               HAS(EXT_GCC_VA_ARGS))
HAS_EXTENSION_IF(tpp_va_comma,                    HAS(EXT_VA_COMMA))
HAS_EXTENSION_IF(tpp_reemit_unknown_pragmas,      !(TPPLexer_Current->l_flags&TPPLEXER_FLAG_EAT_UNKNOWN_PRAGMA))
HAS_EXTENSION_IF(tpp_msvc_integer_suffix,         HAS(EXT_MSVC_FIXED_INT))
HAS_EXTENSION_IF(tpp_charize_operator,            HAS(EXT_HASH_AT))
HAS_EXTENSION_IF(tpp_trigraphs,                   HAS(EXT_TRIGRAPHS))
HAS_EXTENSION_IF(tpp_digraphs,                    HAS(EXT_DIGRAPHS))
HAS_EXTENSION_IF(tpp_pragma_push_macro,           TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_pop_macro,            TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_region,               TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_endregion,            TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_warning,              TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_message,              TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_error,                TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_once,                 TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_tpp_exec,             TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_deprecated,           TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_pragma_tpp_set_keyword_flags,TPP_PREPROCESSOR_VERSION >= 200)
HAS_EXTENSION_IF(tpp_directive_include_next,      HAS(EXT_INCLUDE_NEXT))
HAS_EXTENSION_IF(tpp_directive_import,            HAS(EXT_IMPORT))
HAS_EXTENSION_IF(tpp_directive_warning,           HAS(EXT_WARNING))
HAS_EXTENSION_IF(tpp_lxor,                        HAS(EXT_LXOR))
HAS_EXTENSION_IF(tpp_token_tilde_tilde,           TPPLexer_Current->l_extokens&TPPLEXER_TOKEN_TILDETILDE)
HAS_EXTENSION_IF(tpp_token_pow,                   TPPLexer_Current->l_extokens&TPPLEXER_TOKEN_STARSTAR)
HAS_EXTENSION_IF(tpp_token_lxor,                  TPPLexer_Current->l_extokens&TPPLEXER_TOKEN_ROOFROOF)
HAS_EXTENSION_IF(tpp_token_arrow,                 TPPLexer_Current->l_extokens&TPPLEXER_TOKEN_ARROW)
HAS_EXTENSION_IF(tpp_token_collon_assign,         TPPLexer_Current->l_extokens&TPPLEXER_TOKEN_COLLONASSIGN)
HAS_EXTENSION_IF(tpp_token_collon_collon,         TPPLexer_Current->l_extokens&TPPLEXER_TOKEN_COLLONCOLLON)
HAS_EXTENSION_IF(tpp_macro_calling_conventions,   HAS(EXT_ALTMAC))
HAS_EXTENSION_IF(tpp_strict_whitespace,           (TPPLexer_Current->l_flags&TPPLEXER_FLAG_KEEP_ARG_WHITESPACE))
HAS_EXTENSION_IF(tpp_strict_integer_overflow,     TPP_WSTATE_ISENABLED(TPPLexer_GetWarning(W_INTEGRAL_OVERFLOW)) ||
                                                  TPP_WSTATE_ISENABLED(TPPLexer_GetWarning(W_INTEGRAL_CLAMPED)))
HAS_EXTENSION_IF(tpp_support_ansi_characters,     0) /* TODO: (Re-)add support for this. */
HAS_EXTENSION_IF(tpp_emit_lf_after_directive,     (TPPLexer_Current->l_flags&TPPLEXER_FLAG_DIRECTIVE_NOOWN_LF))
HAS_EXTENSION_IF(tpp_if_cond_expression,          HAS(EXT_IFELSE_IN_EXPR))
HAS_EXTENSION_IF(tpp_debug,                       TPP_CONFIG_DEBUG)

/* Predefined macros and their values.
 * NOTE: These behave like other predefined macros, allowing
 *       the user to redefine them, but returning when they
 *       #undef their versions again. */
PREDEFINED_MACRO(__TPP_VERSION__,TPP_PP_STR(TPP_PREPROCESSOR_VERSION))

#if !TPP_CONFIG_MINMACRO
/* Pull in GCC-specific definitions. */
#include "tpp-gcc-defs.inl"
#endif /* !TPP_CONFIG_MINMACRO */

#if TPP_CONFIG_GCCFUNC
#if TPP_CONFIG_MINGCCFUNC == 0
/* Builtin functions recognized in expressions. */
#ifdef DECLARE_BUILTIN_FUNCTIONS
PRIVATE int_t tpp_ffs(int_t i) {
 int_t result;
 if (!i) return 0;
 for (result = 1; !(i&1); ++result) i >>= 1;
 return result;
}
PRIVATE int_t tpp_clz(int_t i) {
 int_t mask = ~(((int_t)-1)/2); /* Only set MSB (e.g.: '0x80000000'). */
 int_t result = 0;
 while (!(i&mask)) ++result,mask >>= 1;
 return result;
}
PRIVATE int_t tpp_ctz(int_t i) {
 int_t mask = 1; /* Only set LSB. */
 int_t result = 0;
 while (!(i&mask)) ++result,mask <<= 1;
 return result;
}
PRIVATE int_t tpp_clrsb(int_t i) {
 int_t mask = ~(((int_t)-1)/2); /* Only set MSB (e.g.: '0x80000000'). */
 int_t result = 0;
 int msb = !!(i&mask); /* Extract the MSB bit. */
 for (;;) {
  mask >>= 1;
  if (!!(i&mask) != msb) break;
  ++result;
 }
 return result;
}
PRIVATE int_t tpp_popcount(int_t i) {
 int_t result = 0;
 while (i) { if (i&1) ++result; i >>= 1; }
 return result;
}
PRIVATE int_t tpp_parity(int_t i) {
 int_t result = 0;
 while (i) { if (i&1) result ^= 1; i >>= 1; }
 return result;
}
#endif
#endif /* TPP_CONFIG_MINGCCFUNC == 0 */

#if TPP_CONFIG_MINGCCFUNC < 2
/* Special functions that require designated preprocessor support.
 * NOTE: These are not part of the min-gcc-func configuration. */
HAS_BUILTIN_IF(__builtin_constant_p,HAS(EXT_BUILTIN_FUNCTIONS))
HAS_BUILTIN_IF(__builtin_choose_expr,HAS(EXT_BUILTIN_FUNCTIONS))
#endif

#if TPP_CONFIG_MINGCCFUNC == 0
/* Define regular builtin functions. */
PREDEFINED_BUILTIN_FUNCTION(__builtin_ffs,1,{ RETURN_INT(tpp_ffs(INT(0)&((int)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_ffsl,1,{ RETURN_INT(tpp_ffs(INT(0)&((long)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_ffsll,1,{ RETURN_INT(tpp_ffs(INT(0)&((llong_t)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_strlen,1,{ RETURN_INT(IS_STRING(0) ? strlen(STRING(0)->s_text) : 0); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_bswap16,1,{ RETURN_INT(__builtin_bswap16((uint16_t)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_bswap32,1,{ RETURN_INT(__builtin_bswap32((uint32_t)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_bswap64,1,{ RETURN_INT(__builtin_bswap64((uint64_t)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_strcmp,2,{ RETURN_INT((IS_STRING(0) && IS_STRING(1)) ? strcmp(STRING(0)->s_text,STRING(1)->s_text) : -1); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_strncmp,3,{ RETURN_INT((IS_STRING(0) && IS_STRING(1)) ? strncmp(STRING(0)->s_text,STRING(1)->s_text,(size_t)INT(2)) : -1); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_strcasecmp,2,{ RETURN_INT((IS_STRING(0) && IS_STRING(1)) ? strcasecmp(STRING(0)->s_text,STRING(1)->s_text) : -1); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_strncasecmp,3,{ RETURN_INT((IS_STRING(0) && IS_STRING(1)) ? strncasecmp(STRING(0)->s_text,STRING(1)->s_text,(size_t)INT(2)) : -1); })
//PREDEFINED_BUILTIN_FUNCTION(__builtin_strchr,2,{ RETURN_INT(IS_STRING(0) ? !!strchr(STRING(0)->s_text,(int)INT(1)) : 0); })
//PREDEFINED_BUILTIN_FUNCTION(__builtin_strrchr,2,{ RETURN_INT(IS_STRING(0) ? !!strchr(STRING(0)->s_text,(int)INT(1)) : 0); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_expect,2,{ RETURN_COPY(A(0)); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_assume_aligned,2,{ RETURN_COPY(A(0)); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_abs,1,{ int_t x = INT(0)&((int)-1); RETURN_INT(x < 0 ? -x : x); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_labs,1,{ int_t x = INT(0)&((long)-1); RETURN_INT(x < 0 ? -x : x); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_llabs,1,{ int_t x = INT(0)&((llong_t)-1); RETURN_INT(x < 0 ? -x : x); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_clz,1,{ RETURN_INT(tpp_clz(INT(0)&((int)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_clzl,1,{ RETURN_INT(tpp_clz(INT(0)&((long)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_clzll,1,{ RETURN_INT(tpp_clz(INT(0)&((llong_t)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_ctz,1,{ RETURN_INT(tpp_ctz(INT(0)&((int)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_ctzl,1,{ RETURN_INT(tpp_ctz(INT(0)&((long)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_ctzll,1,{ RETURN_INT(tpp_ctz(INT(0)&((llong_t)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_clrsb,1,{ RETURN_INT(tpp_clrsb(INT(0)&((int)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_clrsbl,1,{ RETURN_INT(tpp_clrsb(INT(0)&((long)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_clrsbll,1,{ RETURN_INT(tpp_clrsb(INT(0)&((llong_t)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_popcount,1,{ RETURN_INT(tpp_popcount(INT(0)&((int)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_popcountl,1,{ RETURN_INT(tpp_popcount(INT(0)&((long)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_popcountll,1,{ RETURN_INT(tpp_popcount(INT(0)&((llong_t)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_parity,1,{ RETURN_INT(tpp_parity(INT(0)&((int)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_parityl,1,{ RETURN_INT(tpp_parity(INT(0)&((long)-1))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_parityll,1,{ RETURN_INT(tpp_parity(INT(0)&((llong_t)-1))); })
// TODO: __builtin_isfinite
// TODO: __builtin_isinf
// TODO: __builtin_isinff
// TODO: __builtin_isinfl
// TODO: __builtin_isnan
// TODO: __builtin_isnanf
// TODO: __builtin_isnanl
// TODO: __builtin_isnormal
// TODO: __builtin_isgreater
// TODO: __builtin_isgreaterequal
// TODO: __builtin_isless
// TODO: __builtin_islessequal
// TODO: __builtin_islessgreater
// TODO: __builtin_isunordered
// TODO: ... (Floating point math.h functions)
PREDEFINED_BUILTIN_FUNCTION(__builtin_FILE,0,{
 size_t length;
 char const *filename = TPPLexer_FILE(&length);
 struct TPPString *s = TPPString_New(filename,length);
 if unlikely(!s) SETERR();
 RETURN_STRING(s);
})
PREDEFINED_BUILTIN_FUNCTION(__builtin_LINE,0,{ RETURN_INT(TPPLexer_LINE()); })
// TODO?: char const *__builtin_FUNCTION(void);
PREDEFINED_BUILTIN_FUNCTION(__builtin_isalnum,1,{ RETURN_INT(isalnum((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isalpha,1,{ RETURN_INT(isalpha((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isascii,1,{ RETURN_INT(isascii((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isblank,1,{ RETURN_INT(isblank((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_iscntrl,1,{ RETURN_INT(iscntrl((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isdigit,1,{ RETURN_INT(isdigit((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isgraph,1,{ RETURN_INT(isgraph((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_islower,1,{ RETURN_INT(islower((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isprint,1,{ RETURN_INT(isprint((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_ispunct,1,{ RETURN_INT(ispunct((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isspace,1,{ RETURN_INT(isspace((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isupper,1,{ RETURN_INT(isupper((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_isxdigit,1,{ RETURN_INT(isxdigit((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_toascii,1,{ RETURN_INT(toascii((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_tolower,1,{ RETURN_INT(tolower((int)INT(0))); })
PREDEFINED_BUILTIN_FUNCTION(__builtin_toupper,1,{ RETURN_INT(toupper((int)INT(0))); })
#endif /* TPP_CONFIG_MINGCCFUNC == 0 */
#endif /* TPP_CONFIG_GCCFUNC */

/* Declare builtin extensions. */
EXTENSION(EXT_TRIGRAPHS,        "trigraphs",                    0)
EXTENSION(EXT_DIGRAPHS,         "digraphs",                     1)
EXTENSION(EXT_GCC_VA_ARGS,      "named-varargs-in-macros",      1)
EXTENSION(EXT_GCC_VA_COMMA,     "glue-comma-in-macros",         1)
EXTENSION(EXT_GCC_IFELSE,       "if-else-optional-true",        1)
EXTENSION(EXT_VA_COMMA,         "va-comma-in-macros",           1)
EXTENSION(EXT_VA_NARGS,         "va-nargs-in-macros",           1)
EXTENSION(EXT_VA_ARGS,          "va-args-in-macros",            1)
EXTENSION(EXT_STR_E,            "escape-e-in-strings",          1)
EXTENSION(EXT_ALTMAC,           "alternative-macro-parenthesis",1)
EXTENSION(EXT_RECMAC,           "macro-recursion",              0)
EXTENSION(EXT_BININTEGRAL,      "binary-literals",              1)
EXTENSION(EXT_MSVC_PRAGMA,      "msvc-pragma-support",          1)
EXTENSION(EXT_STRINGOPS,        "strings-in-expressions",       1)
EXTENSION(EXT_HASH_AT,          "charize-macro-argument",       1)
EXTENSION(EXT_HASH_XCLAIM,      "dont-expand-macro-argument",   1)
EXTENSION(EXT_WARNING,          "warning-directives",           1)
EXTENSION(EXT_SHEBANG,          "shebang-directives",           1)
EXTENSION(EXT_INCLUDE_NEXT,     "include-next-directives",      1)
EXTENSION(EXT_IMPORT,           "import-directives",            1)
EXTENSION(EXT_IDENT_SCCS,       "ident-directives",             1)
EXTENSION(EXT_BASEFILE,         "basefile-macro",               1)
EXTENSION(EXT_INCLUDE_LEVEL,    "include-level-macro",          1)
EXTENSION(EXT_COUNTER,          "counter-macro",                1)
EXTENSION(EXT_CLANG_FEATURES,   "has-feature-macros",           1)
EXTENSION(EXT_HAS_INCLUDE,      "has-include-macros",           1)
EXTENSION(EXT_LXOR,             "logical-xor-in-expressions",   1)
EXTENSION(EXT_MULTICHAR_CONST,  "multichar-constants",          1)
EXTENSION(EXT_DATEUTILS,        "numeric-date-macros",          1)
EXTENSION(EXT_TIMEUTILS,        "numeric-time-macros",          1)
EXTENSION(EXT_TIMESTAMP,        "timestamp-macro",              1)
EXTENSION(EXT_COLUMN,           "column-macro",                 1)
EXTENSION(EXT_TPP_EVAL,         "tpp-eval-macro",               1)
EXTENSION(EXT_TPP_UNIQUE,       "tpp-unique-macro",             1)
EXTENSION(EXT_TPP_LOAD_FILE,    "tpp-load-file-macro",          1)
EXTENSION(EXT_TPP_COUNTER,      "tpp-counter-macro",            1)
EXTENSION(EXT_TPP_RANDOM,       "tpp-random-macro",             1)
EXTENSION(EXT_TPP_STR_DECOMPILE,"tpp-str-decompile-macro",      1)
EXTENSION(EXT_TPP_STR_SUBSTR,   "tpp-str-substr-macro",         1)
EXTENSION(EXT_TPP_STR_PACK,     "tpp-str-pack-macro",           1)
EXTENSION(EXT_TPP_STR_SIZE,     "tpp-str-size-macro",           1)
EXTENSION(EXT_TPP_COUNT_TOKENS, "tpp-count-tokens-macro",       1)
EXTENSION(EXT_DOLLAR_IS_ALPHA,  "dollars-in-identifiers",       1)
EXTENSION(EXT_ASSERTIONS,       "assertions",                   1)
EXTENSION(EXT_CANONICAL_HEADERS,"canonical-system-headers",     1)
EXTENSION(EXT_EXT_ARE_FEATURES, "extensions-are-features",      1)
EXTENSION(EXT_MSVC_FIXED_INT,   "fixed-length-integrals",       1)
EXTENSION(EXT_NO_EXPAND_DEFINED,"dont-expand-defined",          1)
EXTENSION(EXT_IFELSE_IN_EXPR,   "ifelse-in-expressions",        1)
EXTENSION(EXT_EXTENDED_IDENTS,  "extended-identifiers",         1)
EXTENSION(EXT_TRADITIONAL_MACRO,"traditional-macro",            0) /* Traditional macro expansion rules. */
#if TPP_CONFIG_MINGCCFUNC < 2
#if TPP_CONFIG_GCCFUNC
EXTENSION(EXT_BUILTIN_FUNCTIONS,"builtins-in-expressions",      1)
#endif /* TPP_CONFIG_GCCFUNC */
#if !TPP_CONFIG_MINMACRO
EXTENSION(EXT_CPU_MACROS,       "define-cpu-macros",            1)
EXTENSION(EXT_SYSTEM_MACROS,    "define-system-macros",         1)
EXTENSION(EXT_UTILITY_MACROS,   "define-utility-macros",        1)
#endif /* !TPP_CONFIG_MINMACRO */
#endif


/* Declare builtin warning groups. */
WGROUP(WG_COMMENT,             "comment",             WSTATE_ERROR)
WGROUP(WG_COMMENTS,            "comments",            WSTATE_ERROR)
WGROUP(WG_MACROS,              "macros",              WSTATE_ERROR)
WGROUP(WG_SYNTAX,              "syntax",              WSTATE_ERROR)
WGROUP(WG_USAGE,               "usage",               WSTATE_ERROR)
WGROUP(WG_VALUE,               "value",               WSTATE_ERROR)
WGROUP(WG_BOOLVALUE,           "boolean-value",       WSTATE_ERROR)
WGROUP(WG_USER,                "user",                WSTATE_ERROR)
WGROUP(WG_ENVIRON,             "environ",             WSTATE_ERROR)
WGROUP(WG_LIMIT,               "limit",               WSTATE_ERROR)
WGROUP(WG_UNDEF,               "undef",               WSTATE_DISABLED)
WGROUP(WG_TRIGRAPHS,           "trigraphs",           WSTATE_DISABLED)
WGROUP(WG_EXPANSION_TO_DEFINED,"expansion-to-defined",WSTATE_DISABLED)
WGROUP(WG_DIRECTIVE,           "directive",           WSTATE_ERROR)
WGROUP(WG_QUALITY,             "quality",             WSTATE_ERROR)
WGROUP(WG_DEPRECATED,          "deprecated",          WSTATE_ERROR)

/* NOTE: These warnings are arranged to mirror those from the old TPP. */
/* 0*/DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_DEFINE,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword after #define, but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedKeywordAfterDefine). */
/* 1*/DEF_WARNING(W_REDEFINING_BUILTIN_KEYWORD,(WG_MACROS),WSTATE_DISABLED,WARNF("Defining macro for builtin keyword " Q("%s"),KWDNAME())) /*< [struct TPPKeyword *] OLD(TPPWarn_RedefiningBuiltinKeyword). */
/* 2*/DEF_WARNING(W_UNKNOWN_PREPROCESSOR_DIRECTIVE,(WG_SYNTAX),WSTATE_WARN,WARNF("Unknown preprocessor directive " TOK_S,TOK_A)) /*< OLD(TPPWarn_UnknownPreprocessorDirective). */
/* 3*/DEF_WARNING(W_STARSLASH_OUTSIDE_OF_COMMENT,(WG_COMMENT,WG_COMMENTS),WSTATE_WARN,WARNF(Q("*" "/") " outside of comment")) /*< [char *] OLD(TPPWarn_StarSlashOutsideOfComment). */
/* 4*/DEF_WARNING(W_ERROR,(WG_USER),WSTATE_ERROR,{ char *temp = ARG(char *); WARNF("ERROR : %.*s",(int)ARG(size_t),temp); }) /*< [char const *,size_t] OLD(TPPWarn_DirectiveError). */
/* 5*/DEF_WARNING(W_WARNING,(WG_USER),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("WARNING : %.*s",(int)ARG(size_t),temp); }) /*< [char const *,size_t] OLD(TPPWarn_DirectiveWarning). */
/* 6*/DEF_WARNING(W_EXPECTED_KWDLPAR_AFTER_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword or " Q("(") " after " Q("defined") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedKeywordOrLParenAfterDefined). */
/* 7*/DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword after " Q("defined") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedKeywordAfterDefined). */
/* 8*/DEF_WARNING(W_EXPECTED_RPAREN_AFTER_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(")") " after " Q("defined") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedRParenAfterDefined). */
/* 9*/DEF_WARNING(W_EXPECTED_MACRO_ARGUMENT_NAME,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected argument name")) /*< [tok_t] OLD(TPPWarn_ExpectedKeywordForMacroArgument). */
/*10*/DEF_WARNING(W_EXPECTED_ARGEND_AFTER_VARARGS,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected end of argument list after arg-args, but got " TOK_S,TOK_A)) /*< [tok_t] OLD(TPPWarn_ExpectedRParenAfterVaArgs). */
/*11*/DEF_WARNING(W_EXPECTED_COMMA_OR_ARGEND,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(",") " or end of argument list, but got " TOK_S,TOK_A)) /*< [tok_t] OLD(TPPWarn_ExpectedCommaOrRParenForMacroFuncDecl). */
/*12*/DEF_WARNING(W_TOO_MANY_MACRO_ARGUMENTS,(WG_MACROS),WSTATE_WARN,WARNF("Too many arguments for " Q("%s"),FILENAME())) /*< [struct TPPFile *] OLD( TPPWarn_InvalidMacroArgCount). */
/*13*/DEF_WARNING(W_EOF_IN_MACRO_ARGUMENT_LIST,(WG_SYNTAX),WSTATE_WARN,WARNF("EOF in macro argument list")) /*< OLD(TPPWarn_UnexpectedEOFInMacroArgList). */
/*14*/DEF_WARNING(W_EXPECTED_INCLUDE_STRING,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected #include-string, but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedIncludeString). */
/*15*/DEF_WARNING(W_FILE_NOT_FOUND,(WG_ENVIRON),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("File not found: " Q("%.*s"),(int)ARG(size_t),temp); }) /*< [char const *,size_t] OLD(TPPWarn_IncludeFileNotFound). */
/*16*/WARNING(W_UNUSED_00,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_SysIncludeFileNotFound). */
/*17*/WARNING(W_UNUSED_01,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedLParenAfterTPPCounter). */
/*18*/WARNING(W_UNUSED_02,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedKeywordAfterTPPCounter). */
/*19*/WARNING(W_UNUSED_03,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedRParenAfterTPPCounter). */
/*20*/WARNING(W_UNUSED_04,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedLParenAfterTPPRandom). */
/*21*/WARNING(W_UNUSED_05,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedIntegerAfterTPPRandom1). */
/*22*/WARNING(W_UNUSED_06,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedCommaAfterTPPRandom). */
/*23*/WARNING(W_UNUSED_07,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedIntegerAfterTPPRandom2). */
/*24*/WARNING(W_UNUSED_08,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedRParenAfterTPPRandom). */
/*25*/DEF_WARNING(W_REDEFINING_MACRO,(WG_MACROS),WSTATE_WARN,{
 /* [struct TPPKeyword *] OLD(TPPWarn_MacroAlreadyDefined). */
 struct TPPFile *textfile;
 struct TPPKeyword *kwd = ARG(struct TPPKeyword *);
 assert(kwd->k_macro);
 assert(kwd->k_macro->f_kind == TPPFILE_KIND_MACRO);
 assert((kwd->k_macro->f_macro.m_flags&TPP_MACROFILE_KIND) != TPP_MACROFILE_KIND_EXPANDED);
 textfile = kwd->k_macro->f_macro.m_deffile;
 assert(textfile);
 assert(textfile->f_kind == TPPFILE_KIND_TEXT);
 WARNF("Redefining macro " Q("%s") "\n",kwd->k_name); 
 WARNF(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
       ? "%s(%d,%d) : " : "%s:%d:%d: "
       , textfile->f_textfile.f_usedname
       ? textfile->f_textfile.f_usedname->s_text
       : textfile->f_name
       ,(int)(kwd->k_macro->f_macro.m_defloc.lc_line+1)
       ,(int)(kwd->k_macro->f_macro.m_defloc.lc_col+1));
 WARNF("See reference to previous definition"); 
})
/*26*/DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_UNDEF,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword after " Q("#undef") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedKeywordAfterUndef). */
/*27*/DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_IFDEF,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword after " Q("#ifdef") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedKeywordAfterIfdef). */
/*28*/DEF_WARNING(W_ELSE_WITHOUT_IF,(WG_SYNTAX),WSTATE_WARN,WARNF("#else without #if")) /*< OLD(TPPWarn_ElseWithoutIfdef). */
/*29*/DEF_WARNING(W_ELSE_AFTER_ELSE,(WG_SYNTAX),WSTATE_WARN,{
 /* [struct TPPIfdefStackSlot *] OLD(TPPWarn_ElseAfterElse). */
 struct TPPIfdefStackSlot *ifdef_slot;
 WARNF("#else after #else\n");
 ifdef_slot = ARG(struct TPPIfdefStackSlot *);
 WARNF(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
       ? "%s(%d) : " : "%s:%d: ",
       ifdef_slot->iss_file->f_name,
      (int)(ifdef_slot->iss_line+1));
 WARNF("See reference to previous #else");
})
/*30*/DEF_WARNING(W_ELIF_WITHOUT_IF,(WG_SYNTAX),WSTATE_WARN,WARNF("#elif without #if")) /*< OLD(TPPWarn_ElifWithoutIfdef). */
/*31*/DEF_WARNING(W_ELIF_AFTER_ELSE,(WG_SYNTAX),WSTATE_WARN,{
 /* [struct TPPIfdefStackSlot *] OLD(TPPWarn_ElifAfterElse). */
 struct TPPIfdefStackSlot *ifdef_slot;
 WARNF("#elif after #else\n");
 ifdef_slot = ARG(struct TPPIfdefStackSlot *);
 WARNF(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
       ? "%s(%d) : " : "%s:%d: ",
       ifdef_slot->iss_file->f_name,
      (int)(ifdef_slot->iss_line+1));
 WARNF("See reference to #else");
})
/*32*/DEF_WARNING(W_IF_WITHOUT_ENDIF,(WG_SYNTAX),WSTATE_WARN,WARNF("#if without #endif")) /*< [struct TPPIfdefStackSlot *] OLD(TPPWarn_IfdefWithoutEndif). */
/*33*/DEF_WARNING(W_ENDIF_WITHOUT_IF,(WG_SYNTAX),WSTATE_WARN,WARNF("#endif without #if")) /*< OLD(TPPWarn_EndifWithoutIfdef). */
/*34*/WARNING(W_UNUSED_09,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedIntAfterLine). */
/*35*/DEF_WARNING(W_EXPECTED_STRING_AFTER_LINE,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after #line, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedStringAfterLine). */
/*36*/DEF_WARNING(W_MACRO_NOT_DEFINED,(WG_VALUE),WSTATE_DISABLED,WARNF("Macro " Q("%s") " is not defined",KWDNAME())) /*< [struct TPPKeyword *] OLD(TPPWarn_MacroDoesntExist). */
/*37*/DEF_WARNING(W_CANT_UNDEF_BUILTIN_MACRO,(WG_VALUE),WSTATE_WARN,WARNF("Cannot #undef builtin macro " Q("%s"),KWDNAME())) /*< [struct TPPKeyword *] OLD(TPPWarn_CantUndefBuiltinMacro). */
/*38*/WARNING(W_UNUSED_0A,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedLParenAfterHasInclude). */
/*39*/WARNING(W_UNUSED_0B,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedRParenAfterHasInclude). */
/*40*/DEF_WARNING(W_EXPECTED_COLLON_AFTER_QUESTION,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(":") " after " Q("?"))) /*< OLD(TPPWarn_ExpectedCollonAfterQuestion). */
/*41*/DEF_WARNING(W_INVALID_INTEGER_SUFFIX,(WG_SYNTAX),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("Invalid integer suffix " Q("%.*s"),(int)ARG(size_t),temp); }) /*< [char const *,size_t] OLD(TPPWarn_ExpectedInteger). */
/*42*/DEF_WARNING(W_EXPECTED_RPAREN_IN_EXPRESSION,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(")") " in expression, but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedRparenAfterLParen). */
/*43*/DEF_WARNING(W_UNKNOWN_TOKEN_IN_EXPR_IS_ZERO,(WG_UNDEF,WG_SYNTAX),WSTATE_WARN,WARNF("Unrecognized token " TOK_S " is replaced with " Q("0") " in expression",TOK_A)) /*< OLD(TPPWarn_UnexpectedTokenInConstExpr). */
/*44*/WARNING(W_UNUSED_0C,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedLParenAfterTPPEval). */
/*45*/WARNING(W_UNUSED_0D,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedRParenAfterTPPEval). */
/*46*/WARNING(W_UNUSED_0E,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedLParenAfterPragma). */
/*47*/DEF_WARNING(W_EXPECTED_STRING_AFTER_PRAGMA,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after _Pragma, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedStringAfterPragma). */
/*48*/WARNING(W_UNUSED_0F,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedRParenAfterPragma). */
/*49*/WARNING(W_UNUSED_10,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_UnexpectedToken). */
/*50*/DEF_WARNING(W_INVALID_WARNING,(WG_VALUE),WSTATE_WARN,{
 /* [struct TPPConst *] OLD(TPPWarn_InvalidWarning). */
 struct TPPConst *c = ARG(struct TPPConst *);
 if (c->c_kind == TPP_CONST_STRING) {
  char const *wname = c->c_data.c_string->s_text;
  if (*wname == '-') ++wname;
  if (*wname == 'W') ++wname;
  if (!memcmp(wname,"no-",3)) wname += 3;
  WARNF("Invalid warning " Q("%s") " (Did you mean " Q("%s") ")",wname,find_most_likely_warning(wname));
 } else if (c->c_kind == TPP_CONST_FLOAT) {
  WARNF("Invalid warning " Q("%f"),(double)c->c_data.c_float);
 } else if (c->c_kind == TPP_CONST_INTEGRAL) {
  WARNF("Invalid warning " Q("%ld"),(long)c->c_data.c_int);
 } else {
  WARNF("Invalid warning");
 }
})
/*51*/DEF_WARNING(W_CANT_POP_WARNINGS,(WG_VALUE),WSTATE_WARN,WARNF("Can't pop warnings")) /*< OLD(TPPWarn_CantPopWarnings). */
/*52*/WARNING(W_UNUSED_11,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_TPPLoadFileNotFound). */
/*53*/DEF_WARNING(W_EXPECTED_STRING_AFTER_PUSHMACRO,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after push_macro, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedStringAfterPushMacro). */
/*54*/WARNING(W_UNUSED_12,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_CantPopMacro). */
/*55*/WARNING(W_UNUSED_13,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedLParenAfterTPPStrDecompile). */
/*56*/DEF_WARNING(W_EXPECTED_STRING_AFTER_TPP_STRD,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after __TPP_STR_DECOMPILE, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedStringAfterTPPStrDecompile). */
/*57*/WARNING(W_UNUSED_14,(WG_VALUE),WSTATE_DISABLED) /*< OLD(TPPWarn_ExpectedRParenAfterTPPStrDecompile). */
/*58*/DEF_WARNING(W_EXPECTED_LPAREN,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q("(") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedLParen). */
/*59*/DEF_WARNING(W_EXPECTED_RPAREN,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(")") ", but got " TOK_S,TOK_A)) /*< OLD(TPPWarn_ExpectedRParen). */
/*60*/DEF_WARNING(W_DEPRECATED_IDENTIFIER,(WG_DEPRECATED),WSTATE_WARN,WARNF("DEPRECATED : " Q("%s"),KWDNAME())) /*< [struct TPPKeyword *] OLD(TPPWarn_DeprecatedKeyword). */
/*61*/DEF_WARNING(W_NONPARTABLE_FILENAME_CASING,(WG_ENVIRON),WSTATE_WARN,{
 /* [char const *,char const *,size_t,char const *]. (path,wrong_begin,wrong_size,corrent_begin)
  *  OLD(TPPWarn_InvalidPathCasing). */
 char *temp; char *temp2; size_t temp3;
 temp = ARG(char *); temp2 = ARG(char *); temp3 = ARG(size_t);
 WARNF("Non-portable casing in " Q("%s") ": " Q("%.*s") " should be " Q("%s") " instead",
       temp,(int)temp3,temp2,ARG(char *));
})
/*62*/DEF_WARNING(W_DIVIDE_BY_ZERO,(WG_VALUE),WSTATE_WARN,WARNF("Divide by ZERO")) /*< OLD(TPPWarn_DivideByZero|TPPWarn_ModuloByZero). */
/*63*/DEF_WARNING(W_ARGUMENT_NAMED_ALREADY_TAKEN,(WG_MACROS),WSTATE_WARN,WARNF("Argument name " Q("%s") " is already in use",TOK_NAME())) /*< [tok_t] OLD(TPPWarn_ReusedMacroParameter). */
/*64*/DEF_WARNING(W_SPECIAL_ARGUMENT_NAME,(WG_MACROS),WSTATE_WARN,WARNF("Special keyword " Q("%s") " used as argument name",KWDNAME())) /*< [struct TPPKeyword *] OLD(TPPWarn_VaArgsUsedAsMacroParameter). */
/*65*/WARNING(W_UNUSED_15,(WG_VALUE),WSTATE_WARN) /*< OLD(TPPWarn_VaCommaUsedAsMacroParameter). */
/*66*/WARNING(W_UNUSED_16,(WG_VALUE),WSTATE_WARN) /*< OLD(TPPWarn_Unexpected). */
/*67*/WARNING(W_UNUSED_17,(WG_VALUE),WSTATE_WARN) /*< OLD(TPPWarn_VaArgsMustBeLastParameter). */
/*68*/WARNING(W_EXPECTED_BOOL,(WG_BOOLVALUE,WG_VALUE),WSTATE_DISABLED) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedBoolExpression). */
/*69*/WARNING(W_EXPECTED_BOOL_UNARY,(WG_BOOLVALUE,WG_VALUE),WSTATE_DISABLED) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedBoolExpressionNot). */
/*70*/WARNING(W_EXPECTED_BOOL_BINARY_LHS,(WG_BOOLVALUE,WG_VALUE),WSTATE_DISABLED) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedBoolExpressionLhsOP). */
/*71*/WARNING(W_EXPECTED_BOOL_BINARY_RHS,(WG_BOOLVALUE,WG_VALUE),WSTATE_DISABLED) /*< [struct TPPConst *] OLD(TPPWarn_ExpectedBoolExpressionRhsOP). */
/*72*/WARNING(W_UNUSED_18,(WG_VALUE),WSTATE_WARN) /*< OLD(TPPWarn_ExpectedKeyword). */
#ifdef DECLARE_WARNING_MESSAGES
{ char const *use; /* Warn about non-boolean integral. */
  if (FALSE) { case W_EXPECTED_BOOL           : use = ""; }
  if (FALSE) { case W_EXPECTED_BOOL_UNARY     : use = " for operand"; }
  if (FALSE) { case W_EXPECTED_BOOL_BINARY_LHS: use = " for left operand"; }
  if (FALSE) { case W_EXPECTED_BOOL_BINARY_RHS: use = " for right operand"; }
  WARNF("Expected boolean expression%s, but got %s",use,CONST_STR());
} break;
#endif

WARNING_NAMESPACE(WN_TPX,100)

/* Warnings added by the new TPP. */
DEF_WARNING(W_SLASHSTAR_INSIDE_OF_COMMENT,(WG_COMMENTS,WG_COMMENT),WSTATE_WARN,WARNF( Q("/" "*") " repeated inside of comment")) /*< [char *]. */
DEF_WARNING(W_LINE_COMMENT_CONTINUED,(WG_COMMENTS,WG_COMMENT),WSTATE_WARN,WARNF("Line-comment continued")) /*< . */
DEF_WARNING(W_VA_KEYWORD_IN_REGULAR_MACRO,(WG_MACROS),WSTATE_WARN,WARNF("Variadic keyword " Q("%s") " used in regular macro",KWDNAME())) /*< [struct TPPKeyword *]. */
DEF_WARNING(W_KEYWORD_MACRO_ALREADY_ONSTACK,(WG_MACROS),WSTATE_DISABLED,WARNF("Keyword-style macro " Q("%s") " is already being expanded",FILENAME())) /*< [struct TPPFile *]. */
DEF_WARNING(W_FUNCTION_MACRO_ALREADY_ONSTACK,(WG_MACROS),WSTATE_DISABLED,WARNF("Function-style macro " Q("%s") " is expanded to the same text",FILENAME())) /*< [struct TPPFile *]. */
DEF_WARNING(W_NOT_ENGOUH_MACRO_ARGUMENTS,(WG_MACROS),WSTATE_WARN,WARNF("Too enough arguments for " Q("%s"),FILENAME())) /*< [struct TPPFile *]. */
DEF_WARNING(W_CHARACTER_TOO_LONG,(WG_VALUE),WSTATE_WARN,WARNF("Character sequence is too long")) /*< . */
DEF_WARNING(W_MULTICHAR_NOT_ALLOWED,(WG_VALUE),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("The multi-character sequence " Q("%.*s") " is not not allowed",(int)ARG(size_t),temp); }) /*< [char const *,size_t]. */
DEF_WARNING(W_INDEX_OUT_OF_BOUNDS,(WG_VALUE),WSTATE_DISABLED,{ struct TPPString *s = ARG(struct TPPString *); WARNF("Index %ld is out-of-bounds of 0..%lu",(unsigned long)s->s_size,(unsigned long)ARG(ptrdiff_t)); }) /*< [struct TPPString *,ptrdiff_t]. */
DEF_WARNING(W_STRING_TERMINATED_BY_LINEFEED,(WG_SYNTAX),WSTATE_WARN,WARNF("String was terminated by a linefeed")) /*< . */
DEF_WARNING(W_STRING_TERMINATED_BY_EOF,(WG_SYNTAX),WSTATE_WARN,WARNF("String was terminated by EOF")) /*< . */
DEF_WARNING(W_COMMENT_TERMINATED_BY_EOF,(WG_SYNTAX),WSTATE_WARN,WARNF("Comment was terminated by EOF")) /*< . */
DEF_WARNING(W_ENCOUNTERED_TRIGRAPH,(WG_TRIGRAPHS),WSTATE_WARN,WARNF("Encountered trigraph character sequence " Q("%.3s"),ARG(char *))) /*< [char (*)[3]]. */
DEF_WARNING(W_EXPECTED_RBRACKET_IN_EXPRESSION,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q("]") " in expression, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_COLLON_AFTER_WARNING,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(":") " after #pragma warning, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_COMMA,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(",") ", but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_STRING_IN_EXPRESSION,(WG_VALUE),WSTATE_WARN,WARNF("Expected string in expression, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_MESSAGE,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after message, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_DEPRECATED,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after deprecated, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_TPP_EXEC,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after tpp_exec, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_TPP_SETF,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after tpp_set_keyword_flags, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_TPP_STRAT,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after __TPP_STR_AT|__TPP_STR_SUBSTR, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_TPP_CNTTOK,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after __TPP_COUNT_TOKENS, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_PRGERROR,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after #pragma error, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_EXTENSION,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after #pragma extension, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_TPP_INCPTH,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after #pragma TPP include_path, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_MACRO_RECURSION_LIMIT_EXCEEDED,(WG_LIMIT),WSTATE_WARN,WARNF("Macro recursion limit exceeded when expanding " Q("%s") " (Consider passing " Q("-fno-macro-recursion") ")",FILENAME())) /*< [struct TPPFile *]. */
DEF_WARNING(W_INCLUDE_RECURSION_LIMIT_EXCEEDED,(WG_LIMIT),WSTATE_WARN,WARNF("Include recursion limit exceeded when including " Q("%s"),FILENAME())) /*< [struct TPPFile *]. */
DEF_WARNING(W_UNKNOWN_EXTENSION,(WG_VALUE),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("Unknown extension " Q("%s") " (Did you mean " Q("%s") "?)",temp,find_most_likely_extension(temp)); }) /*< [char const *]. */
DEF_WARNING(W_DEFINED_IN_MACRO_BODY,(WG_EXPANSION_TO_DEFINED),WSTATE_WARN,WARNF(Q("defined") " found in macro body"))
DEF_WARNING(W_IDENT_SCCS_IGNORED,(WG_DIRECTIVE),WSTATE_WARN,WARNF("#ident/sccs with " Q("%s") " is ignored",CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_IDENT,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after #ident, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_ASSERT,(WG_DIRECTIVE),WSTATE_WARN,WARNF("Expected keyword after #assert, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_PREDICATE,(WG_DIRECTIVE),WSTATE_WARN,WARNF("Expected keyword after predicate " Q("%s") " in #assert, but got " TOK_S,KWDNAME(),TOK_A)) /*< [struct TPPKeyword *]. */
DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_EXPR_HASH,(WG_DIRECTIVE),WSTATE_WARN,WARNF("Expected keyword after # in expression, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_EXPR_PRED,(WG_DIRECTIVE),WSTATE_WARN,WARNF("Expected keyword after predicate " Q("%s") " in expression, but got " TOK_S,KWDNAME(),TOK_A)) /*< . */
DEF_WARNING(W_UNKNOWN_ASSERTION,(WG_VALUE),WSTATE_DISABLED,{ char const *temp = KWDNAME(); WARNF("Assertion " Q("%s") " does not contain a predicate " Q("%s"),temp,KWDNAME()); }) /* [struct TPPKeyword *,struct TPPKeyword *]. */
DEF_WARNING(W_EXPECTED_STRING_AFTER_GCC_DIAG,(WG_VALUE),WSTATE_WARN,WARNF("Expected string after #pragma GCC diagnostic <mode>, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_CANT_POP_EXTENSIONS,(WG_VALUE),WSTATE_WARN,WARNF("Can't pop extensions")) /*< . */
DEF_WARNING(W_CANT_POP_INCLUDE_PATH,(WG_VALUE),WSTATE_WARN,WARNF("Can't pop #include paths")) /*< . */
DEF_WARNING(W_CONSIDER_PAREN_AROUND_LAND,(WG_QUALITY),WSTATE_WARN,WARNF("Consider adding parenthesis around " Q("&&") " to prevent confusion with " Q("||"))) /*< . */
DEF_WARNING(W_INTEGRAL_OVERFLOW,(WG_VALUE),WSTATE_WARN,WARNF("Integral constant overflow")) /*< [int_t,int_t]. */
DEF_WARNING(W_INTEGRAL_CLAMPED,(WG_VALUE),WSTATE_WARN,WARNF("Integral constant clamped to fit")) /*< [int_t,int_t]. */
DEF_WARNING(W_UNKNOWN_INCLUDE_PATH,(WG_VALUE),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("Unknown system #include-path " Q("%.*s"),(int)ARG(size_t),temp); }) /*< [char const *,size_t]. */
DEF_WARNING(W_INCLUDE_PATH_ALREADY_EXISTS,(WG_VALUE),WSTATE_WARN,{ char *temp = ARG(char *); WARNF("System #include-path " Q("%.*s") " already exists",(int)ARG(size_t),temp); }) /*< [char const *,size_t]. */
DEF_WARNING(W_EXPECTED_ELSE_IN_EXPRESSION,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q("else") " in expression, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_STATEMENT_IN_EXPRESSION,(WG_USAGE,WG_SYNTAX),WSTATE_WARN,WARNF("GCC-style statement " TOK_S " in expression is not understood",TOK_A)) /*< . */
DEF_WARNING(W_TYPECAST_IN_EXPRESSION,(WG_USAGE,WG_SYNTAX,WG_UNDEF),WSTATE_WARN,WARNF("C-style type cast " TOK_S " in expression is not understood (Consider using bit-masks to narrow integral types)",TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_RPAREN_AFTER_CAST,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q(")") " after casting type, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_RBRACE_AFTER_STATEMENT,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected " Q("}") " after statement, but got " TOK_S,TOK_A)) /*< . */
DEF_WARNING(W_EXPECTED_WARNING_NAMEORID,(WG_VALUE),WSTATE_WARN,WARNF("Expected warning name or id, but got " Q("%s"),CONST_STR())) /*< [struct TPPConst *]. */
DEF_WARNING(W_CANT_DEFINE_LOCKED_KEYWORD,(WG_VALUE),WSTATE_DISABLED,WARNF("Cannot #define macro for locked keyword " Q("%s"),KWDNAME())) /*< [struct TPPKeyword *]. */
DEF_WARNING(W_CANT_UNDEF_LOCKED_KEYWORD,(WG_VALUE),WSTATE_DISABLED,WARNF("Cannot #undef macro for locked keyword " Q("%s"),KWDNAME())) /*< [struct TPPKeyword *]. */
/* _always_ add new warnings here! */


#ifdef TPP_USERDEFS
#include TPP_USERDEFS
#endif

#undef TPP_PP_STR
#undef TPP_PP_STR2
#undef PREDEFINED_BUILTIN_FUNCTION_IF
#undef PREDEFINED_BUILTIN_FUNCTION
#undef PREDEFINED_BUILTIN_KWDFUNCTION_IF
#undef PREDEFINED_BUILTIN_KWDFUNCTION
#undef PREDEFINED_FUNCTION_IF
#undef PREDEFINED_FUNCTION
#undef PREDEFINED_KWDFUNCTION_IF
#undef PREDEFINED_KWDFUNCTION
#undef PREDEFINED_MACRO_IF
#undef PREDEFINED_MACRO
#undef PREDEFINED_KWDMACRO_IF
#undef PREDEFINED_KWDMACRO
#undef HAS_EXTENSION_IF
#undef HAS_FEATURE_IF
#undef HAS_BUILTIN_IF
#undef DEF_WARNING
#undef DEF_M_IF
#undef DEF_M
#undef DEF_K

#ifdef TPP_DEFS_DEFINES_EXTENSION
#undef TPP_DEFS_DEFINES_EXTENSION
#undef EXTENSION
#endif
#ifdef TPP_DEFS_DEFINES_WARNING_MESSAGE
#undef TPP_DEFS_DEFINES_WARNING_MESSAGE
#undef WARNING_MESSAGE
#endif
#ifdef TPP_DEFS_DEFINES_BUILTIN_FUNCTION
#undef TPP_DEFS_DEFINES_BUILTIN_FUNCTION
#undef BUILTIN_FUNCTION
#endif
#ifdef TPP_DEFS_DEFINES_BUILTIN_MACRO
#undef TPP_DEFS_DEFINES_BUILTIN_MACRO
#undef BUILTIN_MACRO
#endif
#ifdef TPP_DEFS_DEFINES_MACRO
#undef TPP_DEFS_DEFINES_MACRO
#undef MACRO
#endif
#ifdef TPP_DEFS_DEFINES_WARNING_NAMESPACE
#undef TPP_DEFS_DEFINES_WARNING_NAMESPACE
#undef WARNING_NAMESPACE
#endif
#ifdef TPP_DEFS_DEFINES_WARNING
#undef TPP_DEFS_DEFINES_WARNING
#undef WARNING
#endif
#ifdef TPP_DEFS_DEFINES_WGROUP
#undef TPP_DEFS_DEFINES_WGROUP
#undef WGROUP
#endif
#ifdef TPP_DEFS_DEFINES_KWD_FLAGS
#undef TPP_DEFS_DEFINES_KWD_FLAGS
#undef KWD_FLAGS
#endif
#ifdef TPP_DEFS_DEFINES_KWD
#undef TPP_DEFS_DEFINES_KWD
#undef KWD
#endif
