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
#ifndef GUARD_TPP_C
#define GUARD_TPP_C 1

/* Source configuration. */
#ifndef TPP_CONFIG_EXPORT
#define TPP_CONFIG_EXPORT  0 /* Export all 'extern' functions from the header, for use by shared libraries. */
#endif



#define TPP(x) x /* Global namespace. */

/* Ugh... Every f-ing time. I'M NOT GONNA BIND MYSELF TO YOUR PLATFORM! */
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_WARNINGS

#ifndef __has_attribute
#   define __has_attribute(x) 0
#endif
#ifndef __has_builtin
#   define __has_builtin(x) 0
#endif
#ifndef __GNUC__
#   define __attribute__(x) /* nothing */
#endif

#if defined(__GNUC__) || __has_attribute(__builtin_expect)
#   define likely(x)   (__builtin_expect(!!(x),1))
#   define unlikely(x) (__builtin_expect(!!(x),0))
#else
#   define likely   /* nothing */
#   define unlikely /* nothing */
#endif
#   define LOCAL    TPP_LOCAL
#if TPP_CONFIG_EXPORT
#if defined(_MSC_VER)
#   define PUBLIC   __declspec(dllexport)
#elif defined(__ELF__)
#   define PUBLIC   __attribute__((__visibility__("default")))
#else
#   define PUBLIC   __attribute__((__dllexport__))
#endif
#endif
#ifndef PRIVATE
#ifdef __ELF__
#   define PRIVDEF  __attribute__((__visibility__("private")))
#   define PRIVATE  __attribute__((__visibility__("private")))
#elif 1
#ifdef _MSC_VER
#   define PRIVDEF  extern
#   define PRIVATE  extern
#else
#   define PRIVDEF  extern
#   define PRIVATE  /* nothing */
#endif
#else
#   define PRIVDEF  static
#   define PRIVATE  static
#endif
#endif

#ifdef PUBLIC
#   define TPPFUN  extern PUBLIC
#else
#   define PUBLIC  /* nothing */
#endif

#define _GNU_SOURCE /* enable 'memrchr' */
#include "tpp.h"
#undef TPPFUN

#include <string.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include <time.h>

#ifdef _WIN32
#ifndef NO_INCLUDE_WINDOWS_H
#include <Windows.h>
#endif
#define stream_close(fd) CloseHandle(fd)
#else
#if defined(__CYGWIN__) || defined(__MINGW32__)
#ifndef NO_INCLUDE_WINDOWS_H
#include <Windows.h>
#endif
#endif
#include <endian.h>
#include <fcntl.h>
#include <unistd.h>
#define stream_close(fd) close(fd)
#endif

#ifdef _MSC_VER
#include <malloc.h>
#else
#include <alloca.h>
#endif

#ifdef _MSC_VER
#include <intrin.h>
#define __builtin_bswap16 _byteswap_ushort
#define __builtin_bswap32 _byteswap_ulong
#define __builtin_bswap64 _byteswap_uint64
#define strcasecmp  stricmp
#define strncasecmp strnicmp
#endif

#undef FALSE
#define FALSE    TPP_MACRO_FALSE

#if defined(__BYTE_ORDER__)
#   define TPP_BYTEORDER   __BYTE_ORDER__
#elif defined(__BYTE_ORDER)
#   define TPP_BYTEORDER   __BYTE_ORDER
#elif defined(_BYTE_ORDER)
#   define TPP_BYTEORDER   _BYTE_ORDER
#elif defined(BYTE_ORDER)
#   define TPP_BYTEORDER   BYTE_ORDER
#elif (defined(__hppa__))\
   || (defined(__m68k__) || defined(__MC68000__) || defined(_M_M68K))\
   || (defined(__MIPS__) && defined(__MISPEB__))\
   || (defined(__ppc__) || defined(__powerpc__) || defined(_M_PPC))\
   || (defined(__ARMEB__) || defined(__sparc__)) /* Big endian... */
#   define TPP_BYTEORDER  4321
#else /* Fallback: Assume little-endian. */
#   define TPP_BYTEORDER  1234
#endif


#define SEP    '/'  /* The path-separator used in sanitized pathnames. */
#if defined(_WIN32) || defined(__CYGWIN__) || defined(__MINGW32__)
/* An alternate path-separator that is replaced with 'SEP' during sanitization.
 * >> We're going all-out linux here! (No backslashes allowed, because they _suck_!) */
#define ALTSEP '\\'
#define HAVE_INSENSITIVE_PATHS
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#define memrchr  tpp_memrchr
LOCAL void *
tpp_memrchr(void const *p, int c, size_t n) {
 uint8_t *iter = (uint8_t *)p+n;
 while (iter != (uint8_t *)p) {
  if (*--iter == c) return iter;
 }
 return NULL;
}
#endif

#ifdef _MSC_VER
extern void __debugbreak(void);
#define TPP_BREAKPOINT  __debugbreak
#elif __has_builtin(__builtin_breakpoint)
#define TPP_BREAKPOINT  __builtin_breakpoint
#elif (defined(__i386__) || defined(__i386) || defined(i386) || \
       defined(__x86_64__)) && (defined(__GNUC__) || defined(__DCC_VERSION__))
#define TPP_BREAKPOINT() ({ __asm__ __volatile__("int $3\n"); (void)0; })
#endif

#undef assert
#undef assertf
#if TPP_CONFIG_DEBUG
PRIVATE void tpp_vlogerrf(char const *format, va_list args) {
#ifdef _WIN32
 char buffer[4096];
 vsprintf(buffer,format,args);
 OutputDebugStringA(buffer);
 fwrite(buffer,sizeof(char),strlen(buffer),stderr);
#else
 vfprintf(stderr,format,args);
#endif
}
PRIVATE void tpp_logerrf(char const *format, ...) {
 va_list args;
 va_start(args,format);
 tpp_vlogerrf(format,args);
 va_end(args);
}

PRIVATE char *tpp_hexrepr(void const *start, size_t size) {
 char *result = (char *)(malloc)(((size*2)+1)*sizeof(char));
 if (result) {
  char *iter = result;
  uint8_t temp,byte,*data = (uint8_t *)start;
  while (size--) {
   byte = *data++;
   temp = (byte&0xf0) >> 4;
   *iter++ = (char)(temp >= 10 ? 'a'+(temp-10) : '0'+temp);
   temp = (byte&0x0f);
   *iter++ = (char)(temp >= 10 ? 'a'+(temp-10) : '0'+temp);
  }
  *iter = '\0';
 }
 return result;
}

PRIVATE
#ifndef TPP_BREAKPOINT
#if defined(__GNUC__) || __has_attribute(__noreturn__)
__attribute__((__noreturn__))
#elif defined(_MSC_VER)
__declspec(noreturn)
#endif
#endif
void tpp_assertion_failed(char const *expr, char const *file, int line,
                          char const *format, ...) {
#ifdef _MSC_VER
#define USE_MSVC_FORMAT (!TPPLexer_Current || (TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT))
#else
#define USE_MSVC_FORMAT (TPPLexer_Current && (TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT))
#endif
 tpp_logerrf("\n");
 tpp_logerrf(USE_MSVC_FORMAT ? "%s(%d) : " : "%s:%d: ",file,line);
 tpp_logerrf("Assertion failed : %s\n",expr);
 if (format) {
  va_list args;
  va_start(args,format);
  tpp_vlogerrf(format,args);
  va_end(args);
  tpp_logerrf("\n");
 }
 {
  static int in_assertion = 0;
  if (!in_assertion && TPPLexer_Current) {
   struct TPPLCInfo info;
   char const *lx_file;
   ++in_assertion;
   TPPLexer_LC(&info);
   lx_file = TPPLexer_FILE(NULL);
   --in_assertion;
   tpp_logerrf(USE_MSVC_FORMAT ? "%s(%d,%d) : " : "%s:%d:%d: ",
               lx_file,info.lc_line+1,info.lc_col+1);
   tpp_logerrf("See reference to current lexer position\n");
  }
 }
#undef USE_MSVC_FORMAT
#ifndef TPP_BREAKPOINT
 exit(1);
#endif
}
#define TPP_EXPAND_FORMAT(...) __VA_ARGS__

#ifdef TPP_BREAKPOINT
#   define assert(expr)    ((expr) || (tpp_assertion_failed(#expr,__FILE__,__LINE__,NULL),TPP_BREAKPOINT(),0))
#ifdef TPP_EXPAND_FORMAT
#   define assertf(expr,f) ((expr) || (tpp_assertion_failed(#expr,__FILE__,__LINE__,TPP_EXPAND_FORMAT f),TPP_BREAKPOINT(),0))
#else
#   define assertf(expr,f) ((expr) || (tpp_assertion_failed(#expr,__FILE__,__LINE__,NULL),TPP_BREAKPOINT(),0))
#endif
#else
#   define assert(expr)    ((expr) || (tpp_assertion_failed(#expr,__FILE__,__LINE__,NULL),0))
#ifdef TPP_EXPAND_FORMAT
#   define assertf(expr,f) ((expr) || (tpp_assertion_failed(#expr,__FILE__,__LINE__,TPP_EXPAND_FORMAT f),0))
#else
#   define assertf(expr,f) ((expr) || (tpp_assertion_failed(#expr,__FILE__,__LINE__,NULL),0))
#endif
#endif
#elif defined(_MSC_VER)
#   define assert          __assume
#   define assertf(expr,f) __assume(expr)
#elif __has_builtin(__builtin_assume)
#   define assert          __builtin_assume
#   define assertf(expr,f) __builtin_assume(expr)
#else /* TPP_CONFIG_DEBUG */
#   define assert(expr)    (void)0
#   define assertf(expr,f) (void)0
#endif /* !TPP_CONFIG_DEBUG */

#define CH_ISALPHA     0x01
#define CH_ISDIGIT     0x02
#define CH_ISSPACE     0x04
#define CH_ISANSI      0x08 /*< The character is an ansi-alpha character. */
#define CH_ISTRIGRAPH  0x10 /*< The character can be used as the 3th byte of a trigraph. */
#define CH_ISMULTICHAR 0x20 /*< The character is the first byte of a multi-character token (non-keyword; non-number; non-string). */
#define CH_ISLF        0x40 /*< The character is '\r' or '\n'. */
#define CH_ISZERO      0x80 /*< The character is '\0'. */

#define ceildiv(x,y) (((x)+((y)-1))/(y))

#define tpp_isalpha(ch)         (chrattr[(uint8_t)(ch)]&CH_ISALPHA)
#define tpp_isdigit(ch)         (chrattr[(uint8_t)(ch)]&CH_ISDIGIT)
#define tpp_isalnum(ch)         (chrattr[(uint8_t)(ch)]&(CH_ISALPHA|CH_ISDIGIT))
#define tpp_isansi(ch)          (chrattr[(uint8_t)(ch)]&CH_ISANSI)
#define tpp_isspace(ch)         (chrattr[(uint8_t)(ch)]&CH_ISSPACE)
#define tpp_islf(ch)            (chrattr[(uint8_t)(ch)]&CH_ISLF)
#define tpp_islforzero(ch)      (chrattr[(uint8_t)(ch)]&(CH_ISLF|CH_ISZERO))
#define tpp_isspace_nolf(ch)   ((chrattr[(uint8_t)(ch)]&(CH_ISSPACE|CH_ISLF))==CH_ISSPACE)
#define tpp_isnospace_orlf(ch) ((chrattr[(uint8_t)(ch)]&(CH_ISSPACE|CH_ISLF))!=CH_ISSPACE)
#define tpp_istrigraph(ch)      (chrattr[(uint8_t)(ch)]&CH_ISTRIGRAPH)
#define tpp_ismultichar(ch)     (chrattr[(uint8_t)(ch)]&CH_ISMULTICHAR)

PRIVATE uint8_t const chrattr[256] = {
/*[[[deemon
for (local i = 0; i < 256; ++i) {
  if ((i % 16) == 0) print "  ",;
  local flags = 0;
  if (i >= 'a' && i <= 'z' ||
      i >= 'A' && i <= 'Z' ||
      i == '_' || i == '$') flags |= CH_ISALPHA;
  if (i >= '0' && i <= '9') flags |= CH_ISDIGIT;
  if (i <= 32) flags |= CH_ISSPACE;
  if (i in ['=','(','/',')','\'','<','!','>','-','?']) flags |= CH_ISTRIGRAPH;
  if (i in ['<','>','=','!','.','+','-','*','/','%',
            '&','|','^','#','~',':','@',]) flags |= CH_ISMULTICHAR;
  if (i in ['\r','\n']) flags |= CH_ISLF;
  if (i in ['\0']) flags |= CH_ISZERO;
  if (i >= 192) flags |= CH_ISANSI;
  print "0x%.2x," % flags,;
  if ((i % 16) == 15) print;
}
]]]*/
  0x84,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x44,0x04,0x04,0x44,0x04,0x04,
  0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,0x04,
  0x04,0x30,0x00,0x20,0x01,0x20,0x20,0x10,0x10,0x10,0x20,0x20,0x00,0x30,0x20,0x30,
  0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x20,0x00,0x30,0x30,0x30,0x10,
  0x20,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,
  0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x00,0x00,0x00,0x20,0x01,
  0x00,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,
  0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x00,0x20,0x00,0x20,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
  0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
  0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
  0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,0x08,
//[[[end]]]
};

#undef TOKEN
#undef TOK
#undef YIELD
#undef WARN
#undef HAS
#undef CURRENT

/* Helpers macro/implementation variables for the current TPP context. */
#if TPP_CONFIG_ONELEXER
PUBLIC struct TPPLexer TPPLexer_Global;
#define CURRENT        TPPLexer_Global
#else
PUBLIC struct TPPLexer *TPPLexer_Current = NULL;
#define CURRENT       (*TPPLexer_Current)
#endif

#define TOKEN CURRENT.l_token
#define TOK   TOKEN.t_id
#define YIELD TPPLexer_Yield
#define WARN  TPPLexer_Warn
#define HAS   TPPLexer_HasExtension

#define pushfile_inherited  TPPLexer_PushFileInherited
#define pushfile            TPPLexer_PushFile

#define pushf() \
do{uint32_t const _oldflags = CURRENT.l_flags
#define breakf() (void)(CURRENT.l_flags = _oldflags|(CURRENT.l_flags&TPPLEXER_FLAG_MERGEMASK))
#define popf() \
   /* NOTE: Don't override the error-flag! */\
   CURRENT.l_flags = _oldflags|(CURRENT.l_flags&TPPLEXER_FLAG_MERGEMASK);\
}while(FALSE)

/* Similar to 'popf', but flags part of 'mask' are not restored. */
#define breakff(mask) \
  (void)(CURRENT.l_flags = (_oldflags&~(mask))|(CURRENT.l_flags&(TPPLEXER_FLAG_MERGEMASK|(mask))))
#define popff(mask) \
         CURRENT.l_flags = (_oldflags&~(mask))|(CURRENT.l_flags&(TPPLEXER_FLAG_MERGEMASK|(mask)));\
}while(FALSE)

#define pusheob() \
do{struct TPPFile *const _oldeob = CURRENT.l_eob_file;\
   CURRENT.l_eob_file            = TOKEN.t_file
#define breakeob() (void)(CURRENT.l_eob_file = _oldeob)
#define popeob() \
   CURRENT.l_eob_file = _oldeob;\
}while(FALSE)
#define pusheof() \
do{struct TPPFile *const _oldeof = CURRENT.l_eof_file;\
   CURRENT.l_eof_file            = TOKEN.t_file
#define breakeof() (void)(CURRENT.l_eof_file = _oldeof)
#define popeof() \
   CURRENT.l_eof_file = _oldeof;\
}while(FALSE)
PRIVDEF void on_popfile(struct TPPFile *file);
#if TPP_CONFIG_DEBUG
#define pushtok() \
do{ tok_t              _old_tok_id    = TOKEN.t_id;\
    unsigned long      _old_tok_num   = TOKEN.t_num;\
    struct TPPFile    *_old_tok_file  = TOKEN.t_file;\
    char              *_old_tok_begin = TOKEN.t_begin;\
    char              *_old_tok_end   = TOKEN.t_end;\
    struct TPPKeyword *_old_tok_kwd   = TOKEN.t_kwd
#define poptok() \
    assert(TOKEN.t_file == _old_tok_file);\
    TOKEN.t_kwd   = _old_tok_kwd;\
    TOKEN.t_end   = _old_tok_end;\
    TOKEN.t_begin = _old_tok_begin;\
    TOKEN.t_num   = _old_tok_num;\
    TOKEN.t_id    = _old_tok_id;\
}while(FALSE)
#else
#define pushtok() \
do{ tok_t              _old_tok_id    = TOKEN.t_id;\
    unsigned long      _old_tok_num   = TOKEN.t_num;\
    char              *_old_tok_begin = TOKEN.t_begin;\
    char              *_old_tok_end   = TOKEN.t_end;\
    struct TPPKeyword *_old_tok_kwd   = TOKEN.t_kwd
#define poptok() \
    TOKEN.t_kwd   = _old_tok_kwd;\
    TOKEN.t_end   = _old_tok_end;\
    TOKEN.t_begin = _old_tok_begin;\
    TOKEN.t_num   = _old_tok_num;\
    TOKEN.t_id    = _old_tok_id;\
}while(FALSE)
#endif


/* Given two pointer 'iter' and 'end', skip a wrapped linefeed
 * in 'iter' so long as it doesn't overflow into 'end'.
 * @return: 0: No wrapped linefeed was found.
 * @return: 1: A wrapped linefeed was skipped. */
#define SKIP_WRAPLF(iter,end) \
 (*(iter) == '\\' && (iter) != (end)-1\
  ? ((iter)[1] == '\n' ? ((iter) += 2,1) :\
     (iter)[1] == '\r' ? ((iter) += \
    ((iter) != (end)-2 && (iter)[2] == '\n') ? 3 : 2,1)\
  : 0) : 0)
#define SKIP_WRAPLF_REV(iter,begin) \
 ((iter)[-1] == '\n' && (iter) != (begin)+1\
  ? ((iter)[-2] == '\\' ? ((iter) -= 2,1) :\
    ((iter)[-2] == '\r' && (iter) != (begin)+2 && (iter)[-3] == '\\') ? ((iter) -= 3,1)\
  : 0) : (((iter)[-1] == '\r' && (iter) != (begin)+1 && (iter)[-2] == '\\')\
  ? ((iter) -= 2,1) : 0))

#define LSTRIP_SPACE(begin,end) \
    while ((begin) != (end)) {\
     while (SKIP_WRAPLF(begin,end));\
     if (!tpp_isspace(*(begin))) break;\
     ++(begin);\
    }
#define RSTRIP_SPACE(begin,end) \
    while ((end) != (begin)) {\
     while (SKIP_WRAPLF_REV(end,begin));\
     if (!tpp_isspace((end)[-1])) break;\
     --(end);\
    }


/* Active feature configurations. */
#define HAVE_FEATURE_TRIGRAPHS           HAS(EXT_TRIGRAPHS)
#define HAVE_FEATURE_DIGRAPHS            HAS(EXT_DIGRAPHS)
#define HAVE_EXTENSION_GCC_VA_ARG        HAS(EXT_GCC_VA_ARGS)
#define HAVE_EXTENSION_GCC_VA_COMMA      HAS(EXT_GCC_VA_COMMA)
#define HAVE_EXTENSION_GCC_IFELSE        HAS(EXT_GCC_IFELSE)
#define HAVE_EXTENSION_VA_COMMA          HAS(EXT_VA_COMMA)
#define HAVE_EXTENSION_VA_NARGS          HAS(EXT_VA_NARGS)
#define HAVE_EXTENSION_VA_ARGS           HAS(EXT_VA_ARGS)
#define HAVE_EXTENSION_STR_E             HAS(EXT_STR_E)
#define HAVE_EXTENSION_ALTMAC            HAS(EXT_ALTMAC)
#define HAVE_EXTENSION_RECMAC            HAS(EXT_RECMAC)
#define HAVE_EXTENSION_BININTEGRAL       HAS(EXT_BININTEGRAL)
#define HAVE_EXTENSION_MSVC_PRAGMA       HAS(EXT_MSVC_PRAGMA)
#define HAVE_EXTENSION_STRINGOPS         HAS(EXT_STRINGOPS)
#define HAVE_EXTENSION_HASH_AT           HAS(EXT_HASH_AT)
#define HAVE_EXTENSION_HASH_XCLAIM       HAS(EXT_HASH_XCLAIM)
#define HAVE_EXTENSION_WARNING           HAS(EXT_WARNING)
#define HAVE_EXTENSION_SHEBANG           HAS(EXT_SHEBANG)
#define HAVE_EXTENSION_INCLUDE_NEXT      HAS(EXT_INCLUDE_NEXT)
#define HAVE_EXTENSION_IMPORT            HAS(EXT_IMPORT)
#define HAVE_EXTENSION_IDENT_SCCS        HAS(EXT_IDENT_SCCS)
#define HAVE_EXTENSION_BASEFILE          HAS(EXT_BASEFILE)
#define HAVE_EXTENSION_INCLUDE_LEVEL     HAS(EXT_INCLUDE_LEVEL)
#define HAVE_EXTENSION_COUNTER           HAS(EXT_COUNTER)
#define HAVE_EXTENSION_CLANG_FEATURES    HAS(EXT_CLANG_FEATURES)
#define HAVE_EXTENSION_HAS_INCLUDE       HAS(EXT_HAS_INCLUDE)
#define HAVE_EXTENSION_LXOR              HAS(EXT_LXOR)
#define HAVE_EXTENSION_MULTICHAR_CONST   HAS(EXT_MULTICHAR_CONST)
#define HAVE_EXTENSION_DATEUTILS         HAS(EXT_DATEUTILS)
#define HAVE_EXTENSION_TIMEUTILS         HAS(EXT_TIMEUTILS)
#define HAVE_EXTENSION_TIMESTAMP         HAS(EXT_TIMESTAMP)
#define HAVE_EXTENSION_COLUMN            HAS(EXT_COLUMN)
#define HAVE_EXTENSION_TPP_EVAL          HAS(EXT_TPP_EVAL)
#define HAVE_EXTENSION_TPP_UNIQUE        HAS(EXT_TPP_UNIQUE)
#define HAVE_EXTENSION_TPP_LOAD_FILE     HAS(EXT_TPP_LOAD_FILE)
#define HAVE_EXTENSION_TPP_COUNTER       HAS(EXT_TPP_COUNTER)
#define HAVE_EXTENSION_TPP_RANDOM        HAS(EXT_TPP_RANDOM)
#define HAVE_EXTENSION_TPP_STR_DECOMPILE HAS(EXT_TPP_STR_DECOMPILE)
#define HAVE_EXTENSION_TPP_STR_SUBSTR    HAS(EXT_TPP_STR_SUBSTR)
#define HAVE_EXTENSION_TPP_STR_SIZE      HAS(EXT_TPP_STR_SIZE)
#define HAVE_EXTENSION_TPP_STR_PACK      HAS(EXT_TPP_STR_PACK)
#define HAVE_EXTENSION_TPP_COUNT_TOKENS  HAS(EXT_TPP_COUNT_TOKENS)
#define HAVE_EXTENSION_DOLLAR_IS_ALPHA   HAS(EXT_DOLLAR_IS_ALPHA)
#define HAVE_EXTENSION_ASSERTIONS        HAS(EXT_ASSERTIONS)
#define HAVE_EXTENSION_CANONICAL_HEADERS HAS(EXT_CANONICAL_HEADERS)
#define HAVE_EXTENSION_EXT_ARE_FEATURES  HAS(EXT_EXT_ARE_FEATURES)
#define HAVE_EXTENSION_MSVC_FIXED_INT    HAS(EXT_MSVC_FIXED_INT)
#define HAVE_EXTENSION_NO_EXPAND_DEFINED HAS(EXT_NO_EXPAND_DEFINED)
#define HAVE_EXTENSION_IFELSE_IN_EXPR    HAS(EXT_IFELSE_IN_EXPR)
#define HAVE_EXTENSION_EXTENDED_IDENTS   HAS(EXT_EXTENDED_IDENTS)
#define HAVE_EXTENSION_TRADITIONAL_MACRO HAS(EXT_TRADITIONAL_MACRO)
#if TPP_CONFIG_GCCFUNC
#if TPP_CONFIG_MINGCCFUNC < 2
#define HAVE_EXTENSION_BUILTIN_FUNCTIONS HAS(EXT_BUILTIN_FUNCTIONS)
#else
#define HAVE_EXTENSION_BUILTIN_FUNCTIONS 1
#endif
#endif /* TPP_CONFIG_GCCFUNC */
#if !TPP_CONFIG_MINMACRO
#define HAVE_EXTENSION_CPU_MACROS        HAS(EXT_CPU_MACROS)
#define HAVE_EXTENSION_SYSTEM_MACROS     HAS(EXT_SYSTEM_MACROS)
#define HAVE_EXTENSION_UTILITY_MACROS    HAS(EXT_UTILITY_MACROS)
#endif /* !TPP_CONFIG_MINMACRO */



/* Debug logging helpers */
#define LOG_CALLMACRO   0x00000001
#define LOG_LEGACYGUARD 0x00000002
#define LOG_PRAGMA      0x00000004
#define LOG_ENCODING    0x00000008
#define LOG_MC_OPGEN    0x00000010
//#define LOG_LEVEL     0x00000000 /* Change this to select active logs. */
#ifndef LOG_LEVEL
#if defined(__DCC_VERSION__) && 0
#   define LOG_LEVEL    0x7fffffff
#else
#   define LOG_LEVEL    0x00000000
#endif
#endif
#define LOG_RAW         0x80000000
#if TPP_CONFIG_DEBUG && (LOG_LEVEL != 0)
#define HAVELOG(level) (!!((level)&LOG_LEVEL)) /* Change this to select active logs. */
LOCAL void tpp_log(char const *fmt, ...) {
 struct TPPLCInfo info;
 va_list args;
 assert(TPPLexer_Current);
 va_start(args,fmt);
 TPPLexer_LC(&info);
 fprintf(stderr,CURRENT.l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
                ? "%s(%d,%d) : " : "%s:%d:%d: ",
                TPPLexer_FILE(NULL),
               (int)(info.lc_line+1),
               (int)(info.lc_col+1));
 fwrite("DEBUG: ",sizeof(char),7,stderr);
 vfprintf(stderr,fmt,args);
 va_end(args);
 fflush(stderr);
}
LOCAL void tpp_log_raw(char const *fmt, ...) {
 va_list args;
 va_start(args,fmt);
 vfprintf(stderr,fmt,args);
 va_end(args);
 fflush(stderr);
}
#define LOG(level,x)   (HAVELOG(level) ? ((level&LOG_RAW) ? tpp_log_raw x : tpp_log x) : (void)0)
#else
#define HAVELOG(level) 0
#define LOG(level,x)   (void)0
#endif


struct TPPExplicitFile {
 /* NOTE: Keep in sync with 'TPPFile'! */
 refcnt_t                 f_refcnt;
 unsigned int             f_kind;
 /*C:ref*/struct TPPFile *f_prev;
 char                    *f_name;
 size_t                   f_namesize;
 size_t                   f_namehash;
 /*ref*/struct TPPString *f_text;
 char                    *f_begin;
 char                    *f_end;
 char                    *f_pos;
};


#define funop_getarg(iter) _funop_getarg(&(iter))
LOCAL size_t
_funop_getarg(funop_t **piter) {
 size_t result = 0;
 uint8_t byte;
 do {
  byte = *(*piter)++;
  result = (result << 7)|(byte&0x7f);
 } while (byte&0x80);
 return result;
}
LOCAL funop_t *
funop_putarg(funop_t *piter, size_t arg) {
 size_t temp,shift;
 temp = arg,shift = 0;
 do temp >>= 7,
    shift += 7;
 while (temp);
 do {
  uint8_t byte;
  shift -= 7;
  byte = (uint8_t)((arg >> shift)&0x7f);
  if (shift) byte |= 0x80;
  *piter++ = byte;
 } while (shift);
 return piter;
}



#define EMPTY_STRING_HASH 1

#if defined(__TPP_VERSION__) && \
   (__SIZEOF_POINTER__ == 4 || __SIZEOF_POINTER__ == 8)
#define PP_CAT2(a,b) a##b
#define PP_CAT(a,b) PP_CAT2(a,b)
#define pp_hashof2_0(result,str) result
#pragma warning(disable: 108) /* Index out-of-bounds in '__TPP_EVAL' */

/* Using some sick-a$$ TPP extensions, we can actually
 * calculate keyword hashes within the preprocessor! */
#pragma extension(push,"-fmacro-recursion")
#if __SIZEOF_POINTER__ == 4
#   define pp_hashof2_1(result,str) pp_hashof2(__TPP_EVAL((result*263+str[0])&0xfffffffful),__TPP_EVAL(str[1:]))
#elif __SIZEOF_POINTER__ == 8
#   define pp_hashof2_1(result,str) pp_hashof2(__TPP_EVAL((result*263+str[0])&0xfffffffffffffffful),__TPP_EVAL(str[1:]))
#endif
#define pp_hashof2(result,str) PP_CAT(pp_hashof2_,__TPP_EVAL(!!str))(result,str)
#pragma extension(pop)
#if __SIZEOF_POINTER__ == 4
#   define pp_hashof(str)         PP_CAT(pp_hashof2(1,str),ul)
#elif __SIZEOF_POINTER__ == 8
#   define pp_hashof(str)         PP_CAT(pp_hashof2(1,str),ull)
#endif
#endif

#ifdef pp_hashof
#define pp_maybe_hashof      pp_hashof
#else
#define pp_maybe_hashof(str) 0
#endif

#ifdef __TPP_EVAL
#define COMPILER_STRLEN(str)  __TPP_EVAL(#(str))
#define COMPILER_STRLEN0(str) __TPP_EVAL(#(str)+1)
#else
#define COMPILER_STRLEN(str)  (sizeof(str)/sizeof(char)-1)
#define COMPILER_STRLEN0(str) (sizeof(str)/sizeof(char))
#endif


#define hashof   TPP_Hashof
PUBLIC hash_t
TPP_Hashof(void const *data, size_t size) {
 hash_t result = 1;
 unsigned char const *iter,*end;
 end = (iter = (unsigned char const *)data)+size;
 for (; iter != end; ++iter)
  result = result*263+*iter;
 return result;
}

PRIVATE char *
skip_whitespace_and_comments(char *iter, char *end) {
 assert(iter <= end);
 while (iter != end) {
  while (iter != end) {
   while (SKIP_WRAPLF(iter,end));
   if (!tpp_isspace(*iter)) break;
   ++iter;
  }
  if (iter == end) break;
  if (*iter == '/' && iter+1 != end) {
   char *forward = iter+1;
   while (SKIP_WRAPLF(forward,end));
   if (*forward == '*' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_C_COMMENT)) {
    ++forward;
    while (forward != end) {
     while (forward != end && *forward != '*') ++forward;
     if (forward == end || ++forward == end) return forward;
     while (SKIP_WRAPLF(forward,end));
     if (*forward == '/') { ++forward; break; }
    }
    iter = forward;
    continue;
   }
   if (*forward == '/' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_CPP_COMMENT)) {
    iter = forward;
    while (iter != end) {
     if (*iter == '\\' && iter+1 != end && tpp_islf(iter[1])) {
      if (*++iter == '\r' && *iter == '\n' && iter+1 != end) ++iter;
      ++iter;
     } else if (tpp_islf(*iter)) break;
     ++iter;
    }
   }
  }
  break;
 }
 return iter;
}

PRIVATE char *
skip_whitespace_and_comments_rev(char *iter, char *begin) {
 char *forward;
 assert(iter >= begin);
next:
 while (iter != begin) {
  while (iter != begin) {
   while (SKIP_WRAPLF_REV(iter,begin));
   if (!tpp_isspace_nolf(iter[-1])) break;
   --iter;
  }
  if (iter == begin) break;
  if (iter[-1] == '/' && iter-1 != begin) {
   forward = iter-1;
   while (SKIP_WRAPLF_REV(forward,begin));
   if (forward[-1] == '*' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_C_COMMENT)) {
    --forward;
    while (forward != begin) {
     while (forward != begin && forward[-1] != '*') --forward;
     if (forward == begin || --forward == begin) return forward;
     while (SKIP_WRAPLF_REV(forward,begin));
     if (forward[-1] == '/') { --forward; break; }
    }
    iter = forward;
    continue;
   }
  }
  if (tpp_islf(iter[-1])) {
   /* HINT: This linefeed is known not to be escaped, due to the 'SKIP_WRAPLF_REV' above. */
   if (CURRENT.l_extokens&TPPLEXER_TOKEN_CPP_COMMENT) {
    /* Check for a C++-style comment. */
    forward = iter-1;
    if (*forward == '\n' && forward != begin && forward[-1] == '\r') --forward;
    while (forward != begin && !tpp_islf(forward[-1])) {
     if (forward[-1] == '/' && *forward == '/') {
      /* Found a C++-style comment.
       * >> Now we must continue skipping whitespace before 'forward-1' */
      iter = forward-1;
      goto next;
     }
     --forward;
    }
   }
   /* Handle this linefeed like space. */
   --iter;
   continue;
  }
  break;
 }
 return iter;
}

/* Similar to 'skip_whitespace_and_comments_rev',
 * but doesn't consider linefeeds as whitespace. */
PRIVATE char *
skip_whitespacenolf_and_comments_rev(char *iter, char *begin) {
 assert(iter >= begin);
 while (iter != begin) {
  while (iter != begin) {
   while (SKIP_WRAPLF_REV(iter,begin));
   if ((chrattr[iter[-1]]&(CH_ISSPACE|CH_ISLF|CH_ISZERO))!=CH_ISSPACE) break;
   --iter;
  }
  if (iter == begin) break;
  if (iter[-1] == '/' && iter-1 != begin &&
     (CURRENT.l_extokens&TPPLEXER_TOKEN_C_COMMENT)) {
   char *forward = iter-1;
   while (SKIP_WRAPLF_REV(forward,begin));
   if (forward[-1] == '*') {
    --forward;
    while (forward != begin) {
     while (forward != begin && forward[-1] != '*') --forward;
     if (forward == begin || --forward == begin) return forward;
     while (SKIP_WRAPLF_REV(forward,begin));
     if (forward[-1] == '/') { --forward; break; }
    }
    iter = forward;
    continue;
   }
  }
  break;
 }
 return iter;
}


struct TPPStringEmpty {
 refcnt_t s_refcnt;
 size_t   s_size;
 char     s_text[1];
};

PRIVATE struct TPPStringEmpty tpp_empty_string = {0x80000000,0,{'\0'}};
#define empty_string  ((struct TPPString *)&tpp_empty_string)


PUBLIC /*ref*/struct TPPString *
TPPString_Cat(/*ref*/struct TPPString *__restrict lhs,
              /*ref*/struct TPPString *__restrict rhs) {
 struct TPPString *result;
 size_t total_size,alloc_size,lhs_size;
 assert(lhs),assert(rhs),assert(lhs != rhs);
 if ((total_size = rhs->s_size) == 0) { TPPString_Decref(rhs); return lhs; }
 if (!lhs->s_size) { TPPString_Decref(lhs); return rhs; }
 total_size += (lhs_size = lhs->s_size);
 alloc_size  = (total_size+1)*sizeof(char);
 alloc_size += TPP_OFFSETOF(struct TPPString,s_text);
 if (lhs->s_refcnt == 1) {
  /* Re-use 'a' (and append 'b') */
  result = (struct TPPString *)realloc(lhs,alloc_size);
  if unlikely(!result) goto err;
copy_result2:
  memcpy(result->s_text+lhs_size,
         rhs->s_text,
         rhs->s_size*sizeof(char));
  TPPString_Decref(rhs);
 } else if (rhs->s_refcnt == 1) {
  /* Re-use 'b' (and insert 'a') */
  result = (struct TPPString *)realloc(rhs,alloc_size);
  if unlikely(!result) goto err;
  memmove(result->s_text+lhs_size,
          result->s_text,
         (total_size-lhs_size)*sizeof(char));
  memcpy(result->s_text+lhs_size,
         result->s_text,lhs_size*sizeof(char));
  --lhs->s_refcnt;
  assert(lhs->s_refcnt);
 } else {
  result = (struct TPPString *)malloc(alloc_size);
  if unlikely(!result) goto err;
  result->s_refcnt = 1;
  memcpy(result->s_text,
         lhs->s_text,
         lhs_size*sizeof(char));
  --lhs->s_refcnt;
  assert(lhs->s_refcnt);
  goto copy_result2;
 }
 assert(result->s_refcnt == 1);
 result->s_size = total_size;
 result->s_text[total_size] = '\0';
 return result;
err:
 TPPString_Decref(lhs);
 TPPString_Decref(rhs);
 return NULL;
}

PUBLIC /*ref*/struct TPPString *
TPPString_New(char const *__restrict text, size_t size) {
 struct TPPString *result;
 if unlikely(!size) {
  /* Special case: Can return the empty string. */
  TPPString_Incref(empty_string);
  return empty_string;
 }
 result = (struct TPPString *)malloc(TPP_OFFSETOF(struct TPPString,s_text)+
                                    (size+1)*sizeof(char));
 if unlikely(!result) return NULL;
 result->s_refcnt = 1;
 result->s_size   = size;
 result->s_text[size] = '\0';
 memcpy(result->s_text,text,size*sizeof(char));
 return result;
}
PUBLIC /*ref*/struct TPPString *
TPPString_NewSized(size_t size) {
 struct TPPString *result;
 if unlikely(!size) {
  TPPString_Incref(empty_string);
  return empty_string;
 }
 result = (struct TPPString *)malloc(TPP_OFFSETOF(struct TPPString,s_text)+
                                    (size+1)*sizeof(char));
 if unlikely(!result) return NULL;
 result->s_refcnt = 1;
 result->s_size   = size;
 result->s_text[size] = '\0';
 return result;
}


static funop_t const empty_code[] = {TPP_FUNOP_END};

PUBLIC void
TPPFile_Destroy(struct TPPFile *__restrict self) {
 assert(self);
 assert(self->f_text);
 TPPString_Decref(self->f_text);
 switch (self->f_kind) {
  case TPPFILE_KIND_TEXT:
   free(self->f_name);
   if (self->f_textfile.f_cacheentry) {
    assert(self->f_textfile.f_cacheentry->f_kind == TPPFILE_KIND_TEXT);
    assert(self->f_textfile.f_cacheentry->f_textfile.f_cacheinc);
    --self->f_textfile.f_cacheentry->f_textfile.f_cacheinc;
    TPPFile_Decref(self->f_textfile.f_cacheentry);
   }
   if (self->f_textfile.f_usedname) TPPString_Decref(self->f_textfile.f_usedname);
   if (self->f_textfile.f_ownedstream != TPP_STREAM_INVALID) stream_close(self->f_textfile.f_ownedstream);
   break;
  case TPPFILE_KIND_MACRO:
   if (self->f_macro.m_flags&TPP_MACROFILE_FLAG_OWNSNAME) free(self->f_name);
   if (self->f_macro.m_deffile) TPPFile_Decref(self->f_macro.m_deffile);
   switch (self->f_macro.m_flags&TPP_MACROFILE_KIND) {
    case TPP_MACROFILE_KIND_KEYWORD:
    case TPP_MACROFILE_KIND_FUNCTION:
     if ((self->f_macro.m_flags&TPP_MACROFILE_KIND) != TPP_MACROFILE_KIND_KEYWORD) {
      free(self->f_macro.m_function.f_argbuf);
      free(self->f_macro.m_function.f_arginfo);
      if (self->f_macro.m_function.f_expand != empty_code)
       free(self->f_macro.m_function.f_expand);
     }
     break;
    {
     struct TPPFile *origin;
    case TPP_MACROFILE_KIND_EXPANDED:
     origin = self->f_macro.m_expand.e_expand_origin;
     assert(origin);
     assert(origin->f_kind == TPPFILE_KIND_MACRO);
     assert((origin->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION);
     assert(origin->f_macro.m_function.f_expansions);
     /* Decrement the on-stack counter. */
     --origin->f_macro.m_function.f_expansions;
     TPPFile_Decref(origin);
    } break;
    default: break;
   }
   break;
  default: break;
 }
 free(self);
}



PUBLIC struct TPPFile TPPFile_Empty = {
 /* f_refcnt                 */0x80000000,
 /* f_kind                   */TPPFILE_KIND_TEXT,
 /* f_prev                   */NULL,
 /* f_name                   */(char *)"<NULL>",
 /* f_namesize               */6,
#if __SIZEOF_SIZE_T__ == 4
 /* f_namehash               */2699259396lu,
#elif __SIZEOF_SIZE_T__ == 8
 /* f_namehash               */406800526700036llu,
#else
#   error "FIXME"
#endif
 /* f_text                   */empty_string,
 /* f_begin                  */empty_string->s_text,
 /* f_end                    */empty_string->s_text,
 /* f_pos                    */empty_string->s_text,{{
 /* f_textfile.f_cacheentry  */NULL,
 /* f_textfile.f_usedname    */NULL,
 /* f_textfile.f_lineoff     */0,
 /* f_textfile.f_stream      */TPP_STREAM_INVALID,
 /* f_textfile.f_ownedstream */TPP_STREAM_INVALID,
 /* f_textfile.f_guard       */NULL,
 /* f_textfile.f_cacheinc    */0,
 /* f_textfile.f_rdata       */0,
 /* f_textfile.f_prefixdel   */0,
 /* f_textfile.f_flags       */TPP_TEXTFILE_FLAG_NOGUARD|TPP_TEXTFILE_FLAG_INTERNAL,
 /* f_textfile.f_encoding    */TPP_ENCODING_UTF8,
 /* f_textfile.f_padding     */{0},
 /* f_textfile.f_newguard    */NULL}}
};


PUBLIC /*ref*/struct TPPFile *
TPPFile_NewExplicitInherited(/*ref*/struct TPPString *__restrict inherited_text) {
 struct TPPFile *result;
 assert(inherited_text);
 assert(inherited_text->s_refcnt);
 assert(!inherited_text->s_text[inherited_text->s_size]);
 result = (struct TPPFile *)malloc(TPPFILE_SIZEOF_EXPLICIT);
 if unlikely(!result) return NULL;
 result->f_refcnt   = 1;
 result->f_kind     = TPPFILE_KIND_EXPLICIT;
 result->f_prev     = NULL;
 result->f_name     = (char *)"";
 result->f_namesize = 0;
 result->f_namehash = EMPTY_STRING_HASH;
 result->f_text     = inherited_text;
 result->f_pos      =
 result->f_begin    = inherited_text->s_text;
 result->f_end      = inherited_text->s_text+
                      inherited_text->s_size;
 return result;
}



/* Locate a suitable chunk ending not ending in an escape linefeed. */
LOCAL char *
string_find_suitable_end(char *begin, size_t length) {
 char *last_linefeed,*temp;
again:
 /* Check for special case: Empty or terminating linefeed unknown.
  * >> If the last character is a '\r', we need to know the next
  *    so say for sure if it's a MAC-style linefeed or a WINDOWS-style. */
 if (!length || begin[length-1] == '\r') return NULL;
 last_linefeed = (char *)memrchr(begin,'\n',length*sizeof(char));
 temp = (char *)memrchr(begin,'\r',length*sizeof(char));
 if (temp && (!last_linefeed ||         /* Mac-style line-feed. */
               temp > last_linefeed ||  /* Mac-style line-feed past unix-style. */
               temp == last_linefeed-1) /* Windows-style CRLF linefeed extension. */
     ) last_linefeed = temp;
 if (last_linefeed && (last_linefeed != begin && last_linefeed[-1] == '\\')) {
  /* This linefeed is escaped (Must try again in a more narrow search area) */
  length = (size_t)(last_linefeed-begin)-1;
  goto again;
 }
 if (last_linefeed) {
  if (++last_linefeed != begin+length && *last_linefeed == '\n') ++last_linefeed;
 }
 return last_linefeed;
}

LOCAL char *
string_find_eol(char *begin, size_t length) {
 char *linefeed,*temp;
again:
 linefeed = (char *)memchr(begin,'\n',length*sizeof(char));
 temp = (char *)memchr(begin,'\r',length*sizeof(char));
 if (temp && (!linefeed || temp < linefeed)) linefeed = temp;
 if (linefeed && (linefeed != begin && linefeed[-1] == '\\')) {
  /* This linefeed is escaped (Must try again in a more narrow search area) */
  if (*linefeed == '\r') ++linefeed;
  begin = linefeed+1;
  goto again;
 }
 return linefeed;
}

LOCAL char *
string_find_eol_after_comments(char *iter, char *end) {
 assert(iter <= end);
 while (iter != end) {
       if (*iter == '\\') { if (++iter == end) break; }
  else if (tpp_islf(*iter)) break;
  else if (*iter == '/' &&
          (CURRENT.l_extokens&TPPLEXER_TOKEN_C_COMMENT)) {
   if (++iter != end) while (SKIP_WRAPLF(iter,end));
   if (*iter == '/') return (++iter,string_find_eol(iter,(size_t)(end-iter)));
   if (*iter == '*') {
    ++iter;
    while (iter != end) {
     while (iter != end && *iter != '*') ++iter;
     if (iter == end) break;
     while (SKIP_WRAPLF(iter,end));
     if (*++iter == '/') break;
    }
   }
  }
  ++iter;
 }
 return iter;
}

LOCAL size_t 
string_count_lf(char *iter, size_t length) {
 char *end = iter+length;
 size_t result = 0;
 while (iter != end) {
  assert(iter < end);
  if (*iter == '\r') {
   if (iter != end-1 &&
       iter[1] == '\n') ++iter;
   ++result;
  } else if (*iter == '\n') {
   ++result;
  }
  ++iter;
 }
 return result;
}



PUBLIC /*ref*/struct TPPFile *
TPPFile_OpenStream(stream_t stream, char const *name) {
 struct TPPFile *result;
 result = (struct TPPFile *)malloc(TPPFILE_SIZEOF_TEXT);
 if unlikely(!result) return NULL;
 memset(&result->f_textfile,0,sizeof(result->f_textfile));
 result->f_textfile.f_stream      = stream;
 result->f_textfile.f_ownedstream = TPP_STREAM_INVALID;
#if TPP_ENCODING_UTF8
 result->f_textfile.f_encoding    = TPP_ENCODING_UTF8;
#endif
 result->f_refcnt                 = 1;
 result->f_kind                   = TPPFILE_KIND_TEXT;
 result->f_prev                   = NULL;
 result->f_namesize               = strlen(name);
 result->f_name                   = (char *)malloc((result->f_namesize+1)*sizeof(char));
 if unlikely(!result->f_name) goto err_r;
 memcpy(result->f_name,name,(result->f_namesize+1)*sizeof(char));
 result->f_namehash = hashof(result->f_name,result->f_namesize);
 TPPString_Incref(empty_string);
 result->f_text  = empty_string;
 result->f_begin =
 result->f_end   =
 result->f_pos   = empty_string->s_text;
 /* Read the first chunk. */
 return result;
err_r:
 free(result);
 return NULL;
}

PRIVATE void
lcinfo_handle(struct TPPLCInfo *__restrict self,
              char *__restrict text, size_t size) {
 char ch,*end = text+size;
 assert(self);
 assert(text <= end);
 for (; text != end; ++text) {
  ch = *text;
  if (ch == '\r') {
   if (text != end-1 && text[1] == '\n') ++text;
lf:
   self->lc_col = 0;
   ++self->lc_line;
  } else if (ch == '\n') {
   goto lf;
  } else if (ch == '\t') {
   self->lc_col += CURRENT.l_tabsize;
  } else {
   ++self->lc_col;
  }
 }
}

struct lc_scan {
 struct TPPFile const *ls_src_file;
 char                 *ls_src_text;
 struct TPPFile const *ls_exp_file;
 char                 *ls_exp_text;
 char                 *ls_exp_end;
 struct TPPLCInfo      ls_info;
 funop_t              *ls_code;
};

PRIVATE int
TPPMacroFile_LCScan(struct lc_scan *__restrict scan,
                    char *__restrict text_pointer) {
 char candy;
 struct lc_scan s;
 size_t arg,ptroff;
 int must_align;
 assert(scan);
 s = *scan;
 /* Simple case: Already found! */
 if (s.ls_exp_text >= text_pointer) return 1;
 must_align = 0;
 while (text_pointer > s.ls_exp_text) {
  funop_t cmd = *s.ls_code++;
//exec_cmd:
  switch (cmd) {
  case TPP_FUNOP_ADV:
   if (must_align) { --s.ls_code; goto do_align; }
   /* Simplest case: Shared text. */
   arg = funop_getarg(s.ls_code);
   /* Make sure the text is identical. - If it isn't, something went wrong. */
   if (memcmp(s.ls_src_text,s.ls_exp_text,arg) != 0)
       return 0;
   ptroff = (size_t)(text_pointer-s.ls_exp_text);
   if (arg > ptroff) arg = ptroff;
   lcinfo_handle(&s.ls_info,s.ls_src_text,arg);
   if (ptroff == arg) goto done;
   s.ls_src_text += arg;
   s.ls_exp_text += arg;
   break;
  case TPP_FUNOP_INS:
  case TPP_FUNOP_INS_EXP:
  case TPP_FUNOP_INS_STR:
  case TPP_FUNOP_INS_CHR:
   funop_getarg(s.ls_code); /* Argument index (we know nothing of this...) */
   must_align = 1;
   goto advance_src;
  case TPP_FUNOP_VA_COMMA:
  case TPP_FUNOP_VA_NARGS:
   must_align = 1;
   /* fallthrough */
  case TPP_FUNOP_DEL:
advance_src:
   arg = funop_getarg(s.ls_code);
   lcinfo_handle(&s.ls_info,s.ls_src_text,arg);
   s.ls_src_text += arg;
   break;
  default:
   --s.ls_code;
   if (must_align) goto do_align;
   /* Handle the remaining text. */
   assert(s.ls_src_text <= s.ls_src_file->f_end);
   arg = (size_t)(s.ls_src_file->f_end-s.ls_src_text);
   if (arg != (size_t)(s.ls_exp_end-s.ls_exp_text)) return 0;
   ptroff = (size_t)(text_pointer-s.ls_exp_text);
   if (arg > ptroff) arg = ptroff;
   assert(arg <= (size_t)(s.ls_exp_end-s.ls_exp_text));
   /* Make sure the last text portion is also identical! */
   if (memcmp(s.ls_src_text,s.ls_exp_text,arg) != 0) return 0;
   lcinfo_handle(&s.ls_info,s.ls_src_text,arg);
   goto done;
  }
 }
done:
 *scan = s;
 return 1;
do_align:
 /* An argument got inserted and we must search for the correct end of insertion.
  * >> For this part, all we can really do is guess... */
 candy = *s.ls_src_text;
 for (;;) {
  s.ls_exp_text = (char *)memchr(s.ls_exp_text,candy,
                                (size_t)(s.ls_exp_end-s.ls_exp_text));
  if unlikely(!s.ls_exp_text) break;
  if (TPPMacroFile_LCScan(&s,text_pointer)) goto done;
  ++s.ls_exp_text; /* Continue scanning after this candidate. */
 }
 return 0;
}

PRIVATE int
TPPMacroFile_LCInfo(struct TPPFile const *__restrict self,
                    struct TPPLCInfo *__restrict info,
                    char *__restrict text_pointer) {
 struct lc_scan scanner;
 int result;
 assert(self);
 assert(info);
 assert(self->f_kind == TPPFILE_KIND_MACRO);
 assert((self->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_EXPANDED);
 assert(text_pointer >= self->f_begin && text_pointer <= self->f_end);
 scanner.ls_exp_file = self;
 scanner.ls_src_file = self->f_macro.m_expand.e_expand_origin;
 assert(scanner.ls_src_file);
 assert(scanner.ls_src_file->f_kind                               == TPPFILE_KIND_MACRO);
 assert((scanner.ls_src_file->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION);
 scanner.ls_code     = scanner.ls_src_file->f_macro.m_function.f_expand;
 scanner.ls_src_text = scanner.ls_src_file->f_begin;
 scanner.ls_exp_text = self->f_begin;
 scanner.ls_exp_end  = self->f_end;
 scanner.ls_info = *info;
 result = TPPMacroFile_LCScan(&scanner,text_pointer);
 *info = scanner.ls_info;
 return result;
}


PUBLIC void
TPPFile_LCAt(struct TPPFile const *__restrict self,
             struct TPPLCInfo *__restrict info,
             char const *__restrict text_pointer) {
 assert(self);
 assert(info);
 assert(text_pointer >= self->f_begin && text_pointer <= self->f_end);
 switch (self->f_kind) {
  {
   char const *begin,*iter;
   col_t column_num;
  case TPPFILE_KIND_TEXT:
   /* Start out with the base line-offset, as tracked
    * by the current file-chunk, or #line directives. */
   info->lc_line = self->f_textfile.f_lineoff;
fallback_cl:
   /* Found regular, old line-feeds. */
   info->lc_line += (line_t)string_count_lf(self->f_begin,
                                           (size_t)(text_pointer-self->f_begin));
   /* NOTE: Must accept \0 as linefeed here to correctly
    *       determine column numbers after a #define directive. */
   column_num = 0;
   begin = self->f_text->s_text,iter = text_pointer;
   for (; iter != begin && !tpp_islforzero(iter[-1]); --iter)
        column_num += iter[-1] == '\t' ? CURRENT.l_tabsize : 1;
   info->lc_col   = column_num;
  } break;
  case TPPFILE_KIND_MACRO:
   *info = self->f_macro.m_defloc;
   if ((self->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_EXPANDED) {
    /* Unless the macro file was constructed manually,
     * or simply non-conforming to all the minor details
     * of how TPP creates them, this should never fail...
     * >> When this does however fail, it means that the expanded
     *    code from 'self' does not conform to the macro text
     *    descriptor (The vector of 'TPP_FUNOP_*' commands),
     *    meaning it is impossible to re-trace the origin of the
     *    given text point inside the non-expanded base-macro. */
    if (TPPMacroFile_LCInfo(self,info,(char *)text_pointer)) return;
   }
   /* Fallback code for anything that isn't an expanded macro. */
   goto fallback_cl;
  case TPPFILE_KIND_EXPLICIT:
   if ((self = self->f_prev) != NULL) {
    TPPFile_LCAt(self,info,self->f_pos);
   } else {
    /* Shouldn't happen, but only the lexer file-chain
     * is guarantied to terminate with a text file. */
    info->lc_line = 0;
    info->lc_col  = 0;
   }
   break;
  default: break;
 }
}

PUBLIC char const *
TPPFile_Filename(struct TPPFile const *__restrict self,
                 size_t *opt_filename_length) {
 
 for (;;) {
  assert(self);
  /* Walk along macro files to locate the underlying textfile. */
  if (self->f_kind == TPPFILE_KIND_EXPLICIT) {
   if ((self = self->f_prev) == NULL) return NULL;
  } else if (self->f_kind == TPPFILE_KIND_MACRO) {
   /* The definitions file may be NULL for predefined macros. */
   if ((self = self->f_macro.m_deffile) == NULL) return NULL;
  } else break;
 }
 assert(self->f_kind == TPPFILE_KIND_TEXT);
 if (self->f_textfile.f_usedname) {
  if (opt_filename_length) *opt_filename_length = self->f_textfile.f_usedname->s_size;
  return self->f_textfile.f_usedname->s_text;
 }
 if (opt_filename_length) *opt_filename_length = self->f_namesize;
 return self->f_name;
}

PUBLIC struct TPPFile *
TPPLexer_Textfile(void) {
 struct TPPFile *result;
 assert(TPPLexer_Current);
 result = TOKEN.t_file;
 for (;;) {
  assert(result);
  /* NOTE: The last file in the #include chain should
   *       always be TPPFile_Empty, which is a text file. */
  if (result->f_kind == TPPFILE_KIND_TEXT) break;
  result = result->f_prev;
 }
 return result;
}
PUBLIC struct TPPFile *
TPPLexer_Basefile(void) {
 struct TPPFile *result;
 assert(TPPLexer_Current);
 result = TOKEN.t_file;
 if (result != &TPPFile_Empty) {
  /* Search for the last file before the empty one. */
  while (result->f_prev != &TPPFile_Empty)
   result = result->f_prev;
 }
 return result;
}




PUBLIC /*ref*/struct TPPFile *
TPPFile_Open(char const *filename) {
 stream_t stream;
 struct TPPFile *result;
#ifdef _WIN32
 stream = CreateFileA(filename,GENERIC_READ,FILE_SHARE_READ|
                      FILE_SHARE_WRITE|FILE_SHARE_DELETE,NULL,
                      OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL);
 if (stream == INVALID_HANDLE_VALUE) { errno = ENOENT; return NULL; }
#else
 stream = open(filename,O_RDONLY);
 if (stream == -1) { errno = ENOENT; return NULL; }
#endif
 result = TPPFile_OpenStream(stream,filename);
 if unlikely(!result) { stream_close(stream); errno = ENOMEM; }
 else result->f_textfile.f_ownedstream = stream;
 return result;
}

PUBLIC /*ref*/struct TPPFile *
TPPFile_CopyForInclude(struct TPPFile *__restrict self) {
 struct TPPFile *result;
 assert(self);
 assert(self->f_kind == TPPFILE_KIND_TEXT);
 assert(!self->f_name[self->f_namesize]);
 /* Special case: No data was ever read from the file's stream,
  *               and it isn't part of the #include stack.
  * >> No need to create a copy (Just use this one) */
 if (!self->f_prev && !self->f_textfile.f_rdata) goto reuse_self;
 /* Open a new file, using the name of this one. */
 result = TPPFile_Open(self->f_name);
 if unlikely(!result) return NULL;
 TPPFile_Incref(self); /*< Create reference. */
 result->f_textfile.f_cacheentry = self; /*< Inherit reference. */
 ++self->f_textfile.f_cacheinc;
 return result;
reuse_self:
 TPPFile_Incref(self);
 return self;
}

/* Determine the encoding from a given chunk
 * of data located at the start of a file. */
LOCAL encoding_t
determine_encoding(char **pdata, size_t data_size) {
 static unsigned char const seq_utf32_be[] = {0x00,0x00,0xFE,0xFF};
 static unsigned char const seq_utf32_le[] = {0xFF,0xFE,0x00,0x00};
 static unsigned char const seq_utf16_be[] = {0xFE,0xFF};
 static unsigned char const seq_utf16_le[] = {0xFF,0xFE};
 static unsigned char const seq_utf8[]     = {0xEF,0xBB,0xBF};
 encoding_t result = TPP_ENCODING_UTF8;
#define CHECK_SEQ(seq) (sizeof(seq) <= data_size && !memcmp(data,seq,sizeof(seq)) && (data += sizeof(seq),1))
 char *data;
 assert(pdata);
 assert(*pdata);
 assert(data_size);
 data = *pdata;
 if (CHECK_SEQ(seq_utf8)) /*result = TPP_ENCODING_UTF8*/;
 else if (CHECK_SEQ(seq_utf32_be)) result = TPP_ENCODING_UTF32_BE;
 else if (CHECK_SEQ(seq_utf32_le)) result = TPP_ENCODING_UTF32_LE;
 else if (CHECK_SEQ(seq_utf16_be)) result = TPP_ENCODING_UTF16_BE;
 else if (CHECK_SEQ(seq_utf16_le)) result = TPP_ENCODING_UTF16_LE;
 /* Detect from recurring patterns. */
 else if (data_size >= 8 &&  data[0] && !data[1] && !data[2] && !data[3] &&
                             data[4] && !data[5] && !data[6] && !data[7]) result = TPP_ENCODING_UTF32_LE;
 else if (data_size >= 8 && !data[0] && !data[1] && !data[2] &&  data[3] &&
                            !data[4] && !data[5] && !data[6] &&  data[7]) result = TPP_ENCODING_UTF32_BE;
 else if (data_size >= 4 &&  data[0] && !data[1] &&
                             data[2] && !data[3]) result = TPP_ENCODING_UTF16_LE;
 else if (data_size >= 4 && !data[0] &&  data[1] &&
                            !data[2] &&  data[3]) result = TPP_ENCODING_UTF16_BE;
 else {
  /* Default: Continue assuming no special encoding. */
 }
 *pdata = data;
 return result;
#undef CHECK_SEQ
}

static unsigned char const uni_bytemarks[] = {0x00,0x00,0xC0,0xE0,0xF0/*,0xF8,0xFC*/};

PRIVATE size_t TPP_SizeofDecodeUtf32(uint32_t *iter, size_t datalength) {
 uint32_t ch; size_t result = 0;
 uint32_t *end = iter+datalength;
 while (iter != end) {
  ch = *iter++;
       if (ch >= 0xD800 && ch <= 0xDFFF) result += 1;
  else if (ch < 0x80)                    result += 1;
  else if (ch < 0x800)                   result += 2;
  else if (ch < 0x10000)                 result += 3;
  else if (ch <= 0x0010FFFF)             result += 4;
  else                                   result += 1;
 }
 return result;
}
PRIVATE char *TPP_DecodeUtf32(char *buf, uint32_t *iter, size_t datalength) {
 uint32_t ch; size_t dst_size;
 uint32_t *end = iter+datalength;
 while (iter != end) {
  ch = *iter++;
       if (ch >= 0xD800 && ch <= 0xDFFF) dst_size = 1;
  else if (ch < 0x80) dst_size = 1;
  else if (ch < 0x800) dst_size = 2;
  else if (ch < 0x10000) dst_size = 3;
  else if (ch <= 0x0010FFFF) dst_size = 4;
  else dst_size = 1;
  switch (dst_size) {
   case 4: buf[3] = (char)((ch|0x80)&0xBF); ch >>= 6;
   case 3: buf[2] = (char)((ch|0x80)&0xBF); ch >>= 6;
   case 2: buf[1] = (char)((ch|0x80)&0xBF); ch >>= 6;
   case 1: buf[0] = (char) (ch|uni_bytemarks[dst_size]);
  }
  buf += dst_size;
 }
 return buf;
}

PRIVATE size_t TPP_SizeofDecodeUtf16(uint16_t *iter, size_t datalength) {
 uint16_t const *end = iter+datalength;
 uint32_t ch,ch2; size_t result = 0;
 while (iter != end) {
  ch = *iter++; /* Convert surrogate pair to Utf32 */
  if (ch >= 0xD800 && ch <= 0xDBFF) {
   if (iter == end) ch = (unsigned char)ch;
   else if ((ch2 = *iter,ch2 >= 0xDC00 && ch2 <= 0xDFFF)) {
    ch = ((ch-0xD800)<<10)+(ch2-0xDC00)+0x0010000;
    ++iter;
   }
  } else if (ch >= 0xDC00 && ch <= 0xDFFF) {
   ch = (unsigned char)ch;
  }
       if (ch < 0x80)     result += 1;
  else if (ch < 0x800)    result += 2;
  else if (ch < 0x10000)  result += 3;
  else if (ch < 0x110000) result += 4;
  else                    result += 1;
 }
 return result;
}
PRIVATE char *TPP_DecodeUtf16(char *buf, uint16_t *iter, size_t datalength) {
 uint16_t const *end = iter+datalength;
 uint32_t ch,ch2; size_t dst_size;
 while (iter != end) {
  ch = *iter++; /* Convert surrogate pair to Utf32 */
  if (ch >= 0xD800 && ch <= 0xDBFF) {
   if (iter == end) ch = (unsigned char)ch;
   else if ((ch2 = *iter,ch2 >= 0xDC00 && ch2 <= 0xDFFF)) {
    ch = ((ch-0xD800)<<10)+(ch2-0xDC00)+0x0010000;
    ++iter;
   }
  } else if (ch >= 0xDC00 && ch <= 0xDFFF) {
   ch = (unsigned char)ch;
  }
       if (ch < 0x80) dst_size = 1;
  else if (ch < 0x800) dst_size = 2;
  else if (ch < 0x10000) dst_size = 3;
  else if (ch < 0x110000) dst_size = 4;
  else dst_size = 1;
  switch (dst_size) {
   case 4: buf[3] = (char)((ch|0x80)&0xBF); ch >>= 6;
   case 3: buf[2] = (char)((ch|0x80)&0xBF); ch >>= 6;
   case 2: buf[1] = (char)((ch|0x80)&0xBF); ch >>= 6;
   case 1: buf[0] = (char)(ch|uni_bytemarks[dst_size]);
  }
  buf += dst_size;
 }
 return buf;
}

LOCAL void swap16(uint16_t *p, size_t n) { while (n--) *p = __builtin_bswap16(*p),++p; }
LOCAL void swap32(uint32_t *p, size_t n) { while (n--) *p = __builtin_bswap32(*p),++p; }

/* Convert a given chunk of data from 'encoding' to UTF-8. */
LOCAL /*ref*/struct TPPString *
convert_encoding(/*ref*/struct TPPString *data,
                 char *data_start, size_t *pdata_size,
                 encoding_t encoding) {
 size_t data_size,size_avail,size_used,req_size;
 struct TPPString *new_data;
 assert(data);
 assert(data_start);
 assert(pdata_size);
 data_size = *pdata_size;
 assert(data_size);
 assert(encoding != TPP_ENCODING_UTF8);
 assert(data_start+data_size > data_start);
 assert(data_start >= data->s_text);
 assert(data_start+data_size <= data->s_text+data->s_size);
 size_used  = (size_t)(data_start-data->s_text);
 size_avail = data->s_size-size_used;
 switch (encoding) {
#if TPP_BYTEORDER == 1234
  case TPP_ENCODING_UTF16_BE:
   swap16((uint16_t *)data_start,data_size/2);
  case TPP_ENCODING_UTF16_LE:
#elif TPP_BYTEORDER == 4321
  case TPP_ENCODING_UTF16_LE:
   swap16((uint16_t *)data_start,data_size/2);
  case TPP_ENCODING_UTF16_BE:
#else
#  error FIXME
#endif
   req_size = TPP_SizeofDecodeUtf16((uint16_t *)data_start,data_size/2);
   new_data = TPPString_NewSized(size_used+req_size);
   if unlikely(!new_data) return NULL;
   memcpy(new_data->s_text,data->s_text,size_used*sizeof(char));
   TPP_DecodeUtf16(new_data->s_text+size_used,(uint16_t *)data_start,data_size/2);
   TPPString_Decref(data);
   data = new_data;
   break;
#if TPP_BYTEORDER == 1234
  case TPP_ENCODING_UTF32_BE:
   swap32((uint32_t *)data_start,data_size/4);
  case TPP_ENCODING_UTF32_LE:
#elif TPP_BYTEORDER == 4321
  case TPP_ENCODING_UTF32_LE:
   swap32((uint32_t *)data_start,data_size/4);
  case TPP_ENCODING_UTF32_BE:
#else
#  error FIXME
#endif
   req_size = TPP_SizeofDecodeUtf32((uint32_t *)data_start,data_size/4);
   new_data = TPPString_NewSized(size_used+req_size);
   if unlikely(!new_data) return NULL;
   memcpy(new_data->s_text,data->s_text,size_used*sizeof(char));
   TPP_DecodeUtf32(new_data->s_text+size_used,(uint32_t *)data_start,data_size/4);
   TPPString_Decref(data);
   data = new_data;
   break;
  default: break;
 }

 *pdata_size = data_size;
 return data;
}


/* Buffer size hint when reading data.
 * NOTE: This value doesn't really change anything semantically:
 *       The chunk reader will continue to only read as much as
 *       is necessary, and this value is only used to weigh
 *       buffer size requirements for pipe input again those
 *       more effective for file input.
 */
#if TPP_CONFIG_DEBUG && 0
#define STREAM_BUFSIZE  1 /* This must still work, but also makes errors show up more easily. */
#else
#define STREAM_BUFSIZE  4096
#endif

PUBLIC int
TPPFile_NextChunk(struct TPPFile *__restrict self, int flags) {
 char *effective_end,*old_textbegin;
 struct TPPString *newchunk;
 size_t end_offset,prefix_size;
#ifdef _WIN32
 DWORD read_bufsize;
#else
 ssize_t read_bufsize;
#endif
 assert(self);
 assert(self->f_text);
 assert(self->f_pos   >= self->f_begin);
 assert(self->f_pos   <= self->f_end);
 assert(self->f_begin >= self->f_text->s_text);
 assert(self->f_end   <= self->f_text->s_text+self->f_text->s_size);
 if (self->f_kind              != TPPFILE_KIND_TEXT ||
     self->f_textfile.f_stream == TPP_STREAM_INVALID) return 0;
 /* Load a new chunk from the associated stream. */
 for (;;) {
  assert(!self->f_end || !*self->f_end ||
         *self->f_end == self->f_textfile.f_prefixdel);
  end_offset = (size_t)(self->f_end-self->f_text->s_text);
  if (flags&TPPFILE_NEXTCHUNK_FLAG_EXTEND) {
   /* Extend the existing chunk (not dropping existing
    * data and creating a continuous character-stream) */
   prefix_size = self->f_text->s_size;
   if (self->f_text->s_refcnt == 1) {
    newchunk = (struct TPPString *)realloc(self->f_text,
                                           TPP_OFFSETOF(struct TPPString,s_text)+
                                          (STREAM_BUFSIZE+1+prefix_size)*sizeof(char));
    if unlikely(!newchunk) return 0;
   } else {
    newchunk = (struct TPPString *)malloc(TPP_OFFSETOF(struct TPPString,s_text)+
                                         (STREAM_BUFSIZE+1+prefix_size)*sizeof(char));
    if unlikely(!newchunk) return 0;
    memcpy(newchunk->s_text,self->f_text->s_text,prefix_size*sizeof(char));
    newchunk->s_refcnt = 1;
    TPPString_Decref(self->f_text);
   }
   newchunk->s_size = STREAM_BUFSIZE+prefix_size;
   assert(end_offset < newchunk->s_size);
   newchunk->s_text[end_offset] = self->f_textfile.f_prefixdel; /* Restore the previously deleted file end. */
   self->f_begin = newchunk->s_text+(self->f_begin-self->f_text->s_text);
   self->f_pos   = newchunk->s_text+(self->f_pos-self->f_text->s_text);
  } else {
   /* Create a new chunk, potentially copying some small portion of data
    * located after the end of the previous one, creating a new chunk
    * and freeing up memory that is no longer used. */
   newchunk = self->f_text;
   prefix_size = newchunk->s_size-end_offset;
   /* Adjust the current line-offset based on the data we're about to drop. */
   self->f_textfile.f_lineoff += string_count_lf(self->f_begin,
                                                (size_t)(self->f_end-self->f_begin));
   assert(self->f_end == newchunk->s_text+end_offset);
   if (newchunk->s_refcnt == 1) {
    memmove(newchunk->s_text,self->f_end,prefix_size*sizeof(char));
    newchunk = (struct TPPString *)realloc(newchunk,TPP_OFFSETOF(struct TPPString,s_text)+
                                          (STREAM_BUFSIZE+1+prefix_size)*sizeof(char));
    if unlikely(!newchunk) return 0; /* It ~should?~ be OK if we don't clean up the 'memmove' above. */
   } else {
    newchunk = (struct TPPString *)malloc(TPP_OFFSETOF(struct TPPString,s_text)+
                                         (STREAM_BUFSIZE+1+prefix_size)*sizeof(char));
    if unlikely(!newchunk) return 0;
    memcpy(newchunk->s_text,self->f_end,prefix_size*sizeof(char));
    newchunk->s_refcnt = 1;
    TPPString_Decref(self->f_text);
   }
   newchunk->s_text[0] = self->f_textfile.f_prefixdel;
   newchunk->s_size    = STREAM_BUFSIZE+prefix_size;
   self->f_pos         = self->f_begin = newchunk->s_text;
  }
  self->f_text = newchunk;
#ifdef _WIN32
  if (!ReadFile(self->f_textfile.f_stream,
                newchunk->s_text+prefix_size,
                STREAM_BUFSIZE,&read_bufsize,NULL)
      ) read_bufsize = 0;
#else
  read_bufsize = read(self->f_textfile.f_stream,
                      newchunk->s_text+prefix_size,
                      STREAM_BUFSIZE);
  if (read_bufsize < 0) read_bufsize = 0;
#endif
  assert(read_bufsize <= STREAM_BUFSIZE);
  /* Clamp the chunk size to what was actually read. */
  assert(newchunk == self->f_text);
  if (read_bufsize != STREAM_BUFSIZE) {
   /* Free unused buffer memory. */
   newchunk->s_size -= STREAM_BUFSIZE;
   newchunk->s_size += read_bufsize;
   old_textbegin = newchunk->s_text;
   newchunk = (struct TPPString *)realloc(newchunk,TPP_OFFSETOF(struct TPPString,s_text)+
                                         (newchunk->s_size+1)*sizeof(char));
   if (newchunk) self->f_text = newchunk;
   else newchunk = self->f_text;
   self->f_begin = newchunk->s_text+(self->f_begin-old_textbegin);
   self->f_pos   = newchunk->s_text+(self->f_pos-old_textbegin);
  }
  if (!(flags&TPPFILE_NEXTCHUNK_FLAG_BINARY) &&
      !(CURRENT.l_flags&TPPLEXER_FLAG_NO_ENCODING) &&
        read_bufsize) {
   char *text_start = newchunk->s_text+prefix_size;
   if (!self->f_textfile.f_rdata) {
    char *new_text_start = text_start;
    /* Determine encoding based on the initial chunk. */
    self->f_textfile.f_encoding = determine_encoding(&new_text_start,read_bufsize);
    if (text_start != new_text_start) {
     size_t size_diff = (size_t)(new_text_start-text_start);
     assert(new_text_start > text_start);
     assert(size_diff <= read_bufsize);
     read_bufsize -= size_diff;
     memmove(text_start,new_text_start,read_bufsize*sizeof(char));
     newchunk->s_size -= size_diff;
     if (self->f_begin >= new_text_start) self->f_begin -= size_diff;
     if (self->f_pos >= new_text_start) self->f_pos -= size_diff;
    }
   }
   if (self->f_textfile.f_encoding != TPP_ENCODING_UTF8) {
    /* Must translate encoding. */
    size_t new_bufsize = read_bufsize;
    old_textbegin = newchunk->s_text;
    newchunk = convert_encoding(newchunk,text_start,&new_bufsize,
                                self->f_textfile.f_encoding);
    if unlikely(!newchunk) return 0;
    self->f_text  = newchunk;
    self->f_begin = newchunk->s_text+(self->f_begin-old_textbegin);
    self->f_pos   = newchunk->s_text+(self->f_pos-old_textbegin);
    read_bufsize  = new_bufsize;
   }
  }
  self->f_textfile.f_rdata += read_bufsize;
  newchunk->s_text[newchunk->s_size] = '\0';
  if (!read_bufsize) {
   /* True input stream EOF. */
   assert(self->f_name != (char *)(uintptr_t)-1);
   assert(self->f_namesize != (size_t)-1);
   self->f_textfile.f_stream = TPP_STREAM_INVALID;
   assert(self->f_name != (char *)(uintptr_t)-1);
   assert(self->f_namesize != (size_t)-1);
   if (self->f_textfile.f_ownedstream != TPP_STREAM_INVALID) {
    stream_close(self->f_textfile.f_ownedstream);
    self->f_textfile.f_ownedstream = TPP_STREAM_INVALID;
   }
   /* There may be ~some~ data available... */
   self->f_end = self->f_text->s_text+self->f_text->s_size;
   break;
  }
  effective_end = self->f_text->s_text+self->f_text->s_size;
search_suitable_end_again:
  self->f_end = string_find_suitable_end(self->f_begin,(size_t)
                                        (effective_end-self->f_begin));
  if (self->f_end) {
   char *iter,*end,ch,*last_zero_mode;
   int mode = 0,termstring_onlf;
   assert(self->f_end >= self->f_begin);
   assert(self->f_end <= effective_end);
   /* Special case: If we managed to read something, but
    * the suitable end didn't increase, just read some more! */
   if (end_offset == (size_t)(self->f_end-self->f_text->s_text)) goto extend_more;
#define MODE_INSTRING  0x01
#define MODE_INCHAR    0x02
#define MODE_INCOMMENT 0x04
   /* >> We managed to find a text chunk suitable to our needs (ends with a non-escaped linefeed)
    *    Yet there are still some more restrictions: It must not end inside an unfinished comment,
    *    and it must not contain an unterminated string/character (if the necessary flag is set). */
   last_zero_mode = iter = self->f_begin,end = self->f_end;
   termstring_onlf = (CURRENT.l_flags&TPPLEXER_FLAG_TERMINATE_STRING_LF);
   while (iter != end) {
    assert(iter < end);
    if (!mode) last_zero_mode = iter;
    ch = *iter++;
         if (ch == '\\' && iter != end) ++iter;
    else if (ch == '\'' && !(mode&~(MODE_INCHAR))) mode ^= MODE_INCHAR;
    else if (ch == '\"' && !(mode&~(MODE_INSTRING))) mode ^= MODE_INSTRING;
    /* TODO: Linefeeds should also terminate strings when the line started with a '#':
     *    >> #define m  This macro's fine!
     *    >> #error This error contains an unmatched ", but that's OK (< and so was that)
     * NOTE: Though remember that escaped linefeeds are always more powerful!
     */
    else if (termstring_onlf && tpp_islf(ch)) mode &= ~(MODE_INCHAR|MODE_INSTRING);
    else if (iter != end) {
     if (mode&MODE_INCOMMENT) {
      /* End multi-line comment. */
      if (ch == '*') {
       while (SKIP_WRAPLF(iter,end));
       if (*iter == '/') mode &= ~(MODE_INCOMMENT);
       ++iter;
      }
     } else if (!mode && ch == '/') {
      while (SKIP_WRAPLF(iter,end));
      ch = *iter;
      if (ch == '*') {
       /* Multi-line comment. */
       mode |= MODE_INCOMMENT;
       ++iter;
      } else if (ch == '/') {
       /* Line-comment. */
       while (iter != end && !tpp_islf(*iter)) ++iter;
       if (iter != end && *iter == '\r') ++iter;
       if (iter != end && *iter == '\n') ++iter;
      }
     }
    }
   }
   if (!mode) break; /* everything's OK! */
   /* The special mode doesn't terminate.
    * Instead: Take the last time the current mode was ZERO(0)
    *          and perform another search for a suitable end
    *          until that point.
    *          If there is enough space, this way we don't
    *          accidentally attempt to read more data from
    *          a potentially slow writer, when there's
    *          actually enough data already available for
    *          us to start preprocessing ~something~.
    */
   if (last_zero_mode != self->f_begin) {
    effective_end = last_zero_mode;
    goto search_suitable_end_again;
   }
  }
  /* Check if the data that was read contains a suitable end. */
extend_more:
  flags |= TPPFILE_NEXTCHUNK_FLAG_EXTEND; /* Extend the data some more... */
  assert(self->f_text->s_size);
  self->f_end = self->f_text->s_text+self->f_text->s_size;
  self->f_textfile.f_prefixdel = newchunk->s_text[0];
  assert(!self->f_end || !*self->f_end ||
         *self->f_end == self->f_textfile.f_prefixdel);
 }
#define DBG_INFO \
 ("self->f_begin        = %p\n"\
  "self->f_pos          = %p\n"\
  "self->f_end          = %p\n"\
  "self->f_text         = %p\n"\
  "self->f_text->s_text = %p\n"\
  "self->f_text->s_size = %p\n"\
  "end(self->f_text)    = %p\n"\
 ,self->f_begin,self->f_pos,self->f_end\
 ,self->f_text,self->f_text->s_text,self->f_text->s_size\
 ,self->f_text->s_text+self->f_text->s_size)
 assertf(self->f_pos   >= self->f_begin,DBG_INFO);
 assertf(self->f_pos   <= self->f_end,  DBG_INFO);
 assertf(self->f_begin <= self->f_end,  DBG_INFO);
 assertf(self->f_end    >= self->f_text->s_text,DBG_INFO);
 assertf(self->f_end    <= self->f_text->s_text+self->f_text->s_size,DBG_INFO);
 assertf(self->f_begin  >= self->f_text->s_text,DBG_INFO);
 assertf(self->f_begin  <= self->f_text->s_text+self->f_text->s_size,DBG_INFO);
 assert(!self->f_text->s_text[self->f_text->s_size]);
#undef DBG_INFO
 self->f_textfile.f_prefixdel = *self->f_end;
 *self->f_end = '\0';
 if unlikely(!self->f_text->s_size) {
  /* Replace with the empty string. */
  TPPString_Incref(empty_string);
  TPPString_Decref(self->f_text);
  self->f_text = empty_string;
  self->f_textfile.f_prefixdel = '\0';
  self->f_begin =
  self->f_end   = 
  self->f_pos   = empty_string->s_text;
 }
 return self->f_pos != self->f_end;
}



PUBLIC char *
TPP_Unescape(char *buf, char const *data, size_t size) {
 char *iter,*end,ch;
 unsigned char val;
 end = (iter = (char *)data)+size;
 while (iter != end) {
  /* TODO: en/decode trigraph character sequences. */
  /* TODO: decode escaped linefeeds. */
  if ((ch = *iter++) == '\\' && iter != end) {
   ch = *iter++;
   switch (ch) {
    case 'a': ch = '\a'; goto put_ch;
    case 'b': ch = '\b'; goto put_ch;
    case 'f': ch = '\f'; goto put_ch;
    case 'n': ch = '\n'; goto put_ch;
    case 'r': ch = '\r'; goto put_ch;
    case 't': ch = '\t'; goto put_ch;
    case 'v': ch = '\v'; goto put_ch;
    case '\\': case '\'': case '\"': goto put_ch;
    {
     unsigned int ith;
     char *start_iter;
    case 'x':
     start_iter = iter,val = 0;
     for (ith = 0; ith != 2; ++ith) {
      unsigned char partval;
      if (iter == end) goto abort_hex;
      ch = *iter;
           if (ch >= '0' && ch <= '9') partval = (unsigned char)(ch-'0');
      else if (ch >= 'a' && ch <= 'f') partval = (unsigned char)(10+(ch-'a'));
      else if (ch >= 'A' && ch <= 'F') partval = (unsigned char)(10+(ch-'A'));
      else if (!ith) {abort_hex: iter = start_iter; goto def_putch; }
      else break;
      val = (unsigned char)((val << 4)|partval);
      ++iter;
     }
     *buf++ = val;
    } break;
    case 'e':
     if (HAVE_EXTENSION_STR_E) {
      ch = '\033';
      goto put_ch;
     }
     /* fallthrough. */
    default:
     if (ch >= '0' && ch <= '7') {
      char *maxend;
      val = (unsigned char)(ch-'0');
      if ((maxend = iter+2) > end) maxend = end;
      while (iter != maxend &&
            (ch = *iter,ch >= '0' && ch <= '7')
             ) val = (unsigned char)((val << 3)|(ch-'0')),++iter;
      *buf++ = val;
     } else {
def_putch:
      *buf++ = '\\';
      goto put_ch;
     }
   }
  } else {
put_ch:
   *buf++ = ch;
  }
 }
 return buf;
}
PUBLIC size_t
TPP_SizeofUnescape(char const *data, size_t size) {
 char *iter,*end,ch;
 size_t result = size;
 end = (iter = (char *)data)+size;
next:
 while (iter != end) {
  /* TODO: en/decode trigraph character sequences. */
  /* TODO: decode escaped linefeeds. */
  if ((ch = *iter++) == '\\' && iter != end) {
   ch = *iter++;
   switch (ch) {
    case 'e':
     if (!HAVE_EXTENSION_STR_E) break;
     /* fallthrough */
    case 'a': case 'b': case 'f': case 'n':
    case 'r': case 't': case 'v':
    case '\\': case '\'': case '\"':
     --result;
     break;
    {
     unsigned int ith;
     char *start_iter;
    case 'x':
     start_iter = iter;
     for (ith = 0; ith != 2; ++ith) {
      if (iter == end) goto abort_hex;
      ch = *iter;
           if (ch >= '0' && ch <= '9');
      else if (ch >= 'a' && ch <= 'f');
      else if (ch >= 'A' && ch <= 'F');
      else if (!ith) {
abort_hex:
       result += (size_t)(iter-start_iter);
       iter = start_iter;
       goto next;
      } else break;
      ++iter;
      --result;
     }
     --result;
    } break;
    default:
     if (ch >= '0' && ch <= '7') {
      char *maxend = iter+2;
      if (maxend > end) maxend = end;
      while (iter != maxend &&
            (ch = *iter,ch >= '0' && ch <= '7')
             ) --result,++iter;
      --result;
     }
     break;
   }
  }
 }
 return result;
}
PUBLIC char *
TPP_Escape(char *buf, char const *data, size_t size) {
 unsigned char *iter,*end,temp,ch;
 end = (iter = (unsigned char *)data)+size;
 for (; iter != end; ++iter) {
  /* TODO: en/decode trigraph character sequences. */
  ch = *iter;
  switch (ch) {
   case '\a':   ch = 'a'; goto escape_ch;
   case '\b':   ch = 'b'; goto escape_ch;
   case '\f':   ch = 'f'; goto escape_ch;
   case '\n':   ch = 'n'; goto escape_ch;
   case '\r':   ch = 'r'; goto escape_ch;
   case '\t':   ch = 't'; goto escape_ch;
   case '\v':   ch = 'v'; goto escape_ch;
   case '\\': case '\'': case '\"':
escape_ch:
    *buf++ = '\\';
    *buf++ = (char)ch;
    break;
   case '\033':
    if (HAVE_EXTENSION_STR_E) { ch = 'e'; goto escape_ch; }
    /* fallthrough */
   default:
    if (ch < 32) {
     *buf++ = '\\';
     if (ch >= 010) *buf++ = (char)('0'+(ch/010));
     *buf++ = (char)('0'+(ch%010));
    } else if (ch >= 127) {
     *buf++ = '\\';
     *buf++ = 'x';
     temp = (unsigned char)((ch & 0xf0) >> 4);
     *buf++ = (char)(temp >= 10 ? 'A'+(temp-10) : '0'+temp);
     temp = (unsigned char)(ch & 0x0f);
     *buf++ = (char)(temp >= 10 ? 'A'+(temp-10) : '0'+temp);
    } else {
     *buf++ = (char)ch;
    }
    break;
  }
 }
 return buf;
}
PUBLIC size_t
TPP_SizeofEscape(char const *data, size_t size) {
 unsigned char *iter,*end,ch;
 size_t result = size;
 end = (iter = (unsigned char *)data)+size;
 for (; iter != end; ++iter) {
  /* TODO: en/decode trigraph character sequences. */
  ch = *iter;
  switch (ch) {
   case '\033':
    result += HAVE_EXTENSION_STR_E ? 1u : 2u; /* '\e' vs. '\33' */
    break;
   case '\a': case '\b': case '\f':
   case '\n': case '\r': case '\t':
   case '\v': case '\\': case '\'': case '\"':
    ++result;
    break;
   default:
    if (ch < 32) {
     result += ch >= 010 ? 2u : 1u;
    } else if (ch >= 127) {
     result += 3;
    }
    break;
  }
 }
 return result;
}

PUBLIC char *
TPP_Itos(char *buf, int_t i) {
 char *result;
 assert(buf);
 if (i < 0) *buf++ = '-',i = -i;
 result = (buf += TPP_SizeofItos(i));
 do *--buf = (char)('0'+(i % 10));
 while ((i /= 10) != 0);
 return result;
}
PUBLIC size_t
TPP_SizeofItos(int_t i) {
 size_t result = 0;
 assert(i != 0 || i == 0);
 if (i < 0) ++result,i = -i;
 do ++result;
 while ((i /= 10) != 0);
 return result;
}

PUBLIC char *
TPP_Ftos(char *buf, float_t f) {
 (void)f; /* TODO */
 return buf;
}
PUBLIC size_t
TPP_SizeofFtos(float_t f) {
 (void)f; /* TODO */
 return 0;
}


LOCAL size_t
wraplf_memlen(char const *iter, size_t n) {
 char const *end = iter+n;
 size_t result = 0;
 while (iter != end) {
  while (SKIP_WRAPLF(iter,end));
  if (iter == end) break;
  ++iter,++result;
 }
 return result;
}
LOCAL void
wraplf_memcpy(char *dst, char const *src, size_t n) {
 char const *end = src+n;
 while (src != end) {
  while (SKIP_WRAPLF(src,end));
  if (src == end) break;
  *dst++ = *src++;
 }
}



struct codewriter {
 funop_t *cw_begin; /*< [0..1][<= cw_end] Allocated base address. */
 funop_t *cw_end;   /*< [0..1][>= cw_begin] End address of allocated memory (NOTE: Still a valid memory location when non-NULL). */
 funop_t *cw_pos;   /*< [0..1][in(cw_begin..cw_end)] Address where the next instruction will be written, or 'cw_end' if the buffer is full. */
};
#define CODEWRITER_INIT  {NULL,NULL,NULL}

LOCAL void codewriter_quit(struct codewriter *self) { free(self->cw_begin); }
LOCAL funop_t *codewriter_pack(struct codewriter *self) {
 if (self->cw_pos) *self->cw_pos = TPP_FUNOP_END;
 return self->cw_begin;
}
LOCAL int
codewriter_putp(struct codewriter *self,
                funop_t const *p, size_t s) {
 funop_t *newvec; size_t avail,newsize;
 avail = (size_t)(self->cw_end-self->cw_pos);
 if (avail < s) {
  avail   = (size_t)(self->cw_pos-self->cw_begin);
  newsize = (size_t)(self->cw_end-self->cw_begin);
  newsize = newsize ? newsize*2 : 2;
  if (newsize-avail < s) newsize = avail+s;
  newvec = (funop_t *)realloc(self->cw_begin,
                             (newsize+1)*sizeof(funop_t));
  if unlikely(!newvec) return 0;
  self->cw_pos   = newvec+(self->cw_pos-self->cw_begin);
  self->cw_begin = newvec;
  self->cw_end   = newvec+newsize;
 }
 memcpy(self->cw_pos,p,s*sizeof(funop_t));
 self->cw_pos += s;
 return 1;
}
LOCAL int codewriter_put0(struct codewriter *self, funop_t op) {
 LOG(LOG_MC_OPGEN,("[MC] OPx0: %lx -> %x\n",
                  (unsigned long)(self->cw_pos-self->cw_begin),
                  (unsigned int)op));
 return codewriter_putp(self,&op,1);
}
LOCAL int codewriter_put1(struct codewriter *self, funop_t op, size_t a1) {
 funop_t buf[1+ceildiv(sizeof(size_t)*8,7)];
 funop_t *iter = buf; *iter++ = op;
 LOG(LOG_MC_OPGEN,("[MC] OPx1: %lx -> %x (%lx)\n",
                  (unsigned long)(self->cw_pos-self->cw_begin),
                  (unsigned int)op,(unsigned long)a1));
 iter = funop_putarg(iter,a1);
 return codewriter_putp(self,buf,(size_t)(iter-buf));
}
LOCAL int codewriter_put2(struct codewriter *self, funop_t op, size_t a1, size_t a2) {
 funop_t buf[1+ceildiv(sizeof(size_t)*8,7)];
 funop_t *iter = buf; *iter++ = op;
 LOG(LOG_MC_OPGEN,("[MC] OPx2: %lx -> %x (%lx, %lx)\n",
                  (unsigned long)(self->cw_pos-self->cw_begin),
                  (unsigned int)op,(unsigned long)a1,(unsigned long)a2));
 iter = funop_putarg(iter,a1);
 iter = funop_putarg(iter,a2);
 return codewriter_putp(self,buf,(size_t)(iter-buf));
}


PRIVDEF struct TPPKeyword *
lookup_escaped_keyword(char const *name, size_t namelen,
                       size_t unescaped_size, int create_missing);

#define func self->f_macro.m_function

/* Performs traditional parsing of a function-style macro.
 * @return: * : Text-pointer to the end of the function block.
 * @return: NULL: An error occurred and a lexer error was set. */
PRIVATE char *
macro_function_scan_block_traditional(struct TPPFile *self) {
 struct codewriter writer = CODEWRITER_INIT;
 struct arginfo_t *argv_iter,*argv_begin,*argv_end;
 char *iter,*end,ch,*last_text_pointer;
 assert(self);
 assert(self->f_kind == TPPFILE_KIND_MACRO);
 assert((self->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION);
 func.f_deltotal  = 0;
 func.f_n_vacomma = 0;
 func.f_n_vanargs = 0;
 argv_end = (argv_begin = func.f_arginfo)+func.f_argc;
 iter = self->f_begin,end = self->f_end;
 last_text_pointer = iter;
 /* Do custom old-style macro parsing: */
 // #define CAT(a,b) a/**/b
 // #define STR(x)   "x"
 /* Basically, old-style macros didn't have tokens.
  * >> The scanner went through and simply deleted
  *    comments and replace function-style arguments
  *    and always expanding them while ignoring where
  *    strings start or stop. */
next:
 while (iter != end) {
  ch = *iter;
  if (ch == '/' && (CURRENT.l_extokens&TPPLEXER_TOKEN_C_COMMENT)) {
   char *comment_start = iter++;
   while (SKIP_WRAPLF(iter,end));
   if (*iter == '*') {
    if likely(iter != end) ++iter;
    /* Remove comments. */
skipcom:
    while (iter != end && *iter++ != '*');
    while (SKIP_WRAPLF(iter,end));
    if (iter != end && *iter != '/') goto skipcom;
    if likely(iter != end) ++iter;
    /* Delete a comment string from 'comment_start..iter' */
    assert(last_text_pointer <= comment_start);
    if (last_text_pointer != comment_start) {
     if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                 (size_t)(comment_start-last_text_pointer))
                 ) goto seterr;
    }
    if unlikely(!codewriter_put1(&writer,TPP_FUNOP_DEL,
                                (size_t)(iter-comment_start))
                ) goto seterr;
    func.f_deltotal += (size_t)(iter-comment_start);
    last_text_pointer = iter;
   }
   goto next;
  }
  if (tpp_isalpha(ch) || (HAVE_EXTENSION_EXTENDED_IDENTS && tpp_isansi(ch))
                      || (HAVE_EXTENSION_DOLLAR_IS_ALPHA && ch == '$')) {
   /* Scan an identifier. */
   struct TPPKeyword *arg_name;
   char *keyword_begin = iter++;
   size_t name_escapesize,name_size = 1;
   uint8_t chflags = CH_ISALPHA|CH_ISDIGIT;
   /* Set the ANSI flag if we're supporting those characters. */
   if (HAVE_EXTENSION_EXTENDED_IDENTS) chflags |= CH_ISANSI;
   /* keyword: scan until a non-alnum character is found. */
   if (HAVE_EXTENSION_DOLLAR_IS_ALPHA) for (;;) {
    while (SKIP_WRAPLF(iter,end));
    if (!(chrattr[*iter]&chflags)) break;
    ++iter,++name_size;
   } else for (;;) {
    while (SKIP_WRAPLF(iter,end));
    if (!(chrattr[*iter]&chflags) || *iter == '$') break;
    ++iter,++name_size;
   }
   name_escapesize = (size_t)(iter-keyword_begin);
   /* Lookup the argument keyword.
    * NOTE: If we don't find it, we already know that
    *       this isn't an argument, as the name does not
    *       appear as a previously recognized keyword. */
   if (name_size == name_escapesize)
        arg_name = TPPLexer_LookupKeyword(keyword_begin,name_size,0);
   else arg_name = lookup_escaped_keyword(keyword_begin,name_escapesize,name_size,0);
   if (arg_name) {
    tok_t arg_id = arg_name->k_id;
    /* Start of identifier. - Check if this is a keyword. */
    for (argv_iter = argv_begin; argv_iter != argv_end; ++argv_iter) {
     if (argv_iter->ai_id == arg_id) {
      /* It does! */
      if (keyword_begin != last_text_pointer) {
       /* Advance the text pointer to cover the gap. */
       if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                   (size_t)(keyword_begin-last_text_pointer))
                   ) goto seterr;
      }
      /* Always insert argument with expansion. */
      if unlikely(!codewriter_put2(&writer,TPP_FUNOP_INS_EXP,
                                  (size_t)(argv_iter-argv_begin),
                                  (size_t)(iter-keyword_begin))
                  ) goto seterr;
      func.f_deltotal += (size_t)(iter-keyword_begin);
      last_text_pointer = iter;
      ++argv_iter->ai_ins_exp;
      goto next;
     }
    }
    /* Still support VA-extensions in traditional macro functions. */
    if ((arg_id == KWD___VA_COMMA__ && HAVE_EXTENSION_VA_COMMA) ||
        (arg_id == KWD___VA_NARGS__ && HAVE_EXTENSION_VA_NARGS)) {
     if (!(self->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_VARIADIC)) {
      if unlikely(!WARN(W_VA_KEYWORD_IN_REGULAR_MACRO,arg_name)) goto err;
     } else {
      if (keyword_begin != last_text_pointer) {
       if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                   (size_t)(keyword_begin-last_text_pointer))
                   ) goto seterr;
      }
      if (arg_id == KWD___VA_COMMA__) {
       /* Replace __VA_COMMA__ with ',' if necessary. */
       if unlikely(!codewriter_put1(&writer,TPP_FUNOP_VA_COMMA,
                                   (size_t)(iter-keyword_begin))
                   ) goto seterr;
       ++func.f_n_vacomma;
      } else {
       /* Replace __VA_NARGS__ with the integral representation of the argument argument size. */
       if unlikely(!codewriter_put1(&writer,TPP_FUNOP_VA_NARGS,
                                   (size_t)(iter-keyword_begin))
                   ) goto seterr;
       ++func.f_n_vanargs;
      }
      func.f_deltotal += (size_t)(iter-keyword_begin);
      last_text_pointer = iter;
     }
    }
   }
   goto next;
  }
  /* Skip escaped linefeeds. */
  if (ch == '\\') while (SKIP_WRAPLF(iter,end));
  if (tpp_islforzero(ch)) break;
  ++iter;
 }
 func.f_expand = codewriter_pack(&writer);
 if (!func.f_expand) func.f_expand = (funop_t *)empty_code;
done:
 return iter;
seterr: TPPLexer_SetErr();
err: iter = NULL; codewriter_quit(&writer); goto done;
}
PRIVATE int
macro_function_scan_block(struct TPPFile *self) {
 struct codewriter writer = CODEWRITER_INIT;
 struct arginfo_t *iter,*begin,*end;
 char const *last_text_pointer,*preglue_end,*preglue_begin;
 int last_was_glue = 0,result = 1;
 char *strop_begin; funop_t strop;
 tok_t preglue_tok;
 assert(self);
 assert(self->f_kind == TPPFILE_KIND_MACRO);
 assert((self->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION);
 /* Parse the macro text block regularly.
  * >> This way, we can easily detect argument references and special operations,
  *    as well as not having to deal with escaped linefeeds or strings. */
 pushf();
 /* Make sure strings are terminated by linefeeds.
  * >> If we didn't enable this, the macro may go on
  *    just because it contains an unterminated string.
  * NOTE: Since we make use of the token's t_begin field
  *       to calculate text pointer offsets, we also
  *       need the original file pointers instead of
  *       the better readable keyword pointers. */
 CURRENT.l_flags |= (TPPLEXER_FLAG_TERMINATE_STRING_LF|
                     TPPLEXER_FLAG_NO_SEEK_ON_EOB);
 CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS);
 func.f_deltotal = 0;
 func.f_n_vacomma = 0;
 func.f_n_vanargs = 0;
 end = (begin = func.f_arginfo)+func.f_argc;
 last_text_pointer = self->f_begin;
next:
 while (TOK > 0 && TOK != '\n') {
  assert(TOKEN.t_file  == self->f_macro.m_deffile);
  assert(TOKEN.t_begin >= last_text_pointer);
  if (TOK == '#') {
   strop_begin = TOKEN.t_begin;
   /* Check for special operations such as "#arg", "#@arg" or "#!arg"
    * NOTE: "#!" prevents expansion of the argument. */
   TPPLexer_YieldRaw();
   switch (TOK) {
    case '!': if (!HAVE_EXTENSION_HASH_XCLAIM) goto def_stringref; strop = TPP_FUNOP_INS; break;
    case '@': if (!HAVE_EXTENSION_HASH_AT) goto def_stringref; strop = TPP_FUNOP_INS_CHR; break;
    default:def_stringref: strop = TPP_FUNOP_INS_STR; goto strop_normal;
   }
   TPPLexer_YieldRaw();
strop_normal:
   assert(strop_begin >= last_text_pointer);
   if (TPP_ISKEYWORD(TOK)) {
    /* Check if this token matches any of our arguments. */
    for (iter = begin; iter != end; ++iter) {
     if (iter->ai_id == TOK) {
      /* It does! */
      if (strop_begin != last_text_pointer) {
       /* Advance the text pointer to cover the gap. */
       if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                   (size_t)(strop_begin-last_text_pointer))
                   ) goto seterr;
      }
      last_was_glue = 0;
      if unlikely(!codewriter_put2(&writer,strop,
                                  (size_t)(iter-begin),
                                  (size_t)(TOKEN.t_end-strop_begin))
                  ) goto seterr;
      func.f_deltotal += (size_t)(TOKEN.t_end-strop_begin);
      last_text_pointer = TOKEN.t_end;
      if (strop != TPP_FUNOP_INS) ++iter->ai_ins_str;
      else ++iter->ai_ins;
      TPPLexer_YieldRaw();
      goto next;
     }
    }
   }
  } else if (TPP_ISKEYWORD(TOK)) {
   /* Check for an argument reference. */
   for (iter = begin; iter != end; ++iter) {
    if (iter->ai_id == TOK) {
     size_t argi = (size_t)(iter-begin);
     size_t namesize;
     /* Got a regular argument reference. */
     if (TOKEN.t_begin != last_text_pointer) {
      /* Advance the text pointer to cover the gap. */
      if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                  (size_t)(TOKEN.t_begin-last_text_pointer))
                  ) goto seterr;
     }
     namesize = (size_t)(TOKEN.t_end-TOKEN.t_begin);
     last_text_pointer = TOKEN.t_end;
     TPPLexer_YieldRaw();
     if (TOK == TOK_GLUE) {
      /* Insert the argument without expansion and override
       * everything until the first token after the glue. */
      ++iter->ai_ins;
      TPPLexer_YieldRaw();
      /* Add the size of the glue token, as well as surrounding
       * whitespace to what should be deleted by the ins operation. */
      namesize += (size_t)(TOKEN.t_begin-last_text_pointer);
      last_text_pointer = TOKEN.t_begin;
      if unlikely(!codewriter_put2(&writer,TPP_FUNOP_INS,argi,namesize)) goto seterr;
      last_was_glue = 1;
     } else if (last_was_glue) {
      /* Insert the argument without expansion. */
      ++iter->ai_ins;
      if unlikely(!codewriter_put2(&writer,TPP_FUNOP_INS,argi,namesize)) goto seterr;
      last_was_glue = 0; /* The last token is no longer glue. */
     } else {
      /* Insert the argument with expansion. */
      ++iter->ai_ins_exp;
      if unlikely(!codewriter_put2(&writer,TPP_FUNOP_INS_EXP,argi,namesize)) goto seterr;
     }
     func.f_deltotal += namesize;
     goto next;
    }
   }
   /* Special variadic keywords. */
   if ((TOK == KWD___VA_COMMA__ && HAVE_EXTENSION_VA_COMMA) ||
       (TOK == KWD___VA_NARGS__ && HAVE_EXTENSION_VA_NARGS)) {
    if (!(self->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_VARIADIC)) {
     if unlikely(!WARN(W_VA_KEYWORD_IN_REGULAR_MACRO,TOKEN.t_kwd)) goto err;
    } else {
     if (TOKEN.t_begin != last_text_pointer) {
      if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                  (size_t)(TOKEN.t_begin-last_text_pointer))
                  ) goto seterr;
     }
     if (TOK == KWD___VA_COMMA__) {
      /* Replace __VA_COMMA__ with ',' if necessary. */
      if unlikely(!codewriter_put1(&writer,TPP_FUNOP_VA_COMMA,
                                  (size_t)(TOKEN.t_end-TOKEN.t_begin))
                  ) goto seterr;
      ++func.f_n_vacomma;
     } else {
      /* Replace __VA_NARGS__ with the integral representation of the argument argument size. */
      if unlikely(!codewriter_put1(&writer,TPP_FUNOP_VA_NARGS,
                                  (size_t)(TOKEN.t_end-TOKEN.t_begin))
                  ) goto seterr;
      ++func.f_n_vanargs;
     }
     func.f_deltotal += (size_t)(TOKEN.t_end-TOKEN.t_begin);
     last_text_pointer = TOKEN.t_end;
    }
   } else if (TOK == KWD_defined) {
    if unlikely(!WARN(W_DEFINED_IN_MACRO_BODY)) goto err;
    /* ----: GCC has an extension that prevents expansion of arguments after 'defined' in a macro body.
     *       Similar to our '#!' operator, gcc implicitly compiles:
     *    >> #define is_defined(x)  defined(x)
     *       as the TPP equivalent for:
     *    >> #define is_defined(x)  defined(#!x)
     *       With that in mind, implement that extension here.
     * NOTE: I thought GCC did this and think I even remember seeing something use it (maybe the linux kernel?)
     *       Anyways... ' guess that's Mandella for ya (And I'm from a parallel reality where GCC did do this!)
     *    >> I still implemented this as an extension though,
     *       simply because it feels like a good idea, that may
     *       potentially further confuse people not understanding
     *       the difference between this (huehuehue...):
     *    >> #define STR1(x) #x
     *    >> #define STR2(x) STR1(x)
     *    >> #define STR3(x) STR1(#!x)
     *    >> #define STR4(x) STR1(defined(x))
     *    >> #define FOO 42
     *    >> STR1(FOO) // "FOO"
     *    >> STR2(FOO) // "42"
     *    >> STR3(FOO) // "FOO" ??? (extensions!)
     *    >> STR4(FOO) // "defined(FOO)" << this extension!
     */
    if (HAVE_EXTENSION_NO_EXPAND_DEFINED && !last_was_glue) {
     preglue_begin = TOKEN.t_begin;
     preglue_end = TOKEN.t_end;
     preglue_tok = TOK;
     TPPLexer_YieldRaw();
     if unlikely(TOK == TOK_GLUE) goto begin_glue;
     if (TOK == '(') {
      preglue_begin = TOKEN.t_begin;
      preglue_end   = TOKEN.t_end;
      preglue_tok   = TOK;
      TPPLexer_YieldRaw();
      if unlikely(TOK == TOK_GLUE) goto begin_glue;
     }
     if (TPP_ISKEYWORD(TOK)) {
      /* If this token describes the name of an argument, don't expand it! */
      strop       = TPP_FUNOP_INS;
      strop_begin = TOKEN.t_begin;
      goto strop_normal;
     }
     /* Since we already did a yield, we must head for the next loop now. */
     goto next;
    }
   }
  }
  preglue_begin = TOKEN.t_begin;
  preglue_end = TOKEN.t_end;
  preglue_tok = TOK;
  TPPLexer_YieldRaw();
  last_was_glue = 0;
  if (TOK == TOK_GLUE) {
begin_glue:
   last_was_glue = 1;
   TPPLexer_YieldRaw();
   if (preglue_tok == ',' &&
       HAVE_EXTENSION_GCC_VA_COMMA) {
    assert(preglue_begin >= last_text_pointer);
    /* Create a GCC-style __VA_COMMA__: ',##__VA_ARGS__' */
    if (preglue_begin != last_text_pointer) {
     if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                 (size_t)(preglue_begin-last_text_pointer))
                 ) goto seterr;
    }
    /* Insert a __VA_COMMA__ and override the ',' and '##'
     * tokens up to the start of whatever follows. */
    if unlikely(!codewriter_put1(&writer,TPP_FUNOP_VA_COMMA,
                                (size_t)(TOKEN.t_begin-preglue_begin))
                ) goto seterr;
    func.f_deltotal += (size_t)(TOKEN.t_begin-preglue_begin);
    ++func.f_n_vacomma;
   } else {
    if (preglue_end != last_text_pointer) {
     /* Delete characters between 'preglue_end' and 'token.t_begin' */
     if unlikely(!codewriter_put1(&writer,TPP_FUNOP_ADV,
                                 (size_t)(preglue_end-last_text_pointer))
                 ) goto seterr;
    }
    if unlikely(!codewriter_put1(&writer,TPP_FUNOP_DEL,
                                (size_t)(TOKEN.t_begin-preglue_end))
                ) goto seterr;
    func.f_deltotal += (size_t)(TOKEN.t_begin-preglue_end);
   }
   last_text_pointer = TOKEN.t_begin;
  }
 }
 func.f_expand = codewriter_pack(&writer);
 if (!func.f_expand) func.f_expand = (funop_t *)empty_code;
done:
 popf();
 return TOK >= 0 && result;
seterr: TPPLexer_SetErr();
err: result = 0; codewriter_quit(&writer); goto done;
}
#undef func

PRIVDEF void TPPKeyword_Undef(struct TPPKeyword *__restrict self);

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif
PUBLIC struct TPPFile *
TPPFile_NewDefine(void) {
 struct TPPFile *result,*curfile; uint32_t macro_flags;
 struct TPPKeyword *keyword_entry; int fix_linenumber = 0;
 tok_t argend_token,argument_name;
 struct arginfo_t *arginfo_v,*new_arginfo_v; size_t arginfo_a;
 assert(CURRENT.l_flags&TPPLEXER_FLAG_WANTLF);
 assert(TPP_ISKEYWORD(TOK));
 keyword_entry = TOKEN.t_kwd;
 assert(keyword_entry);
 curfile = TOKEN.t_file;
 /* Emit some warning about redefining stuff. */
 if (TPP_ISKEYWORD(TOKEN.t_id) && !TPP_ISUSERKEYWORD(TOKEN.t_id) &&
    !WARN(W_REDEFINING_BUILTIN_KEYWORD,keyword_entry)) return NULL;
 while (SKIP_WRAPLF(curfile->f_pos,curfile->f_end));
 /* Check the character immediately after the name of the macro. */
 switch (*curfile->f_pos) {
  case '(': argend_token = ')',macro_flags = TPP_MACROFILE_KIND_FUNCTION|TPP_MACROFILE_FUNC_START_LPAREN;
common_function_macro:
   if (HAVE_EXTENSION_RECMAC) macro_flags |= TPP_MACROFILE_FLAG_FUNC_SELFEXPAND;
   break;
  case '[': argend_token = ']',macro_flags = TPP_MACROFILE_KIND_FUNCTION|TPP_MACROFILE_FUNC_START_LBRACKET; goto common_call_extension;
  case '{': argend_token = '}',macro_flags = TPP_MACROFILE_KIND_FUNCTION|TPP_MACROFILE_FUNC_START_LBRACE; goto common_call_extension;
  case '<': argend_token = '>',macro_flags = TPP_MACROFILE_KIND_FUNCTION|TPP_MACROFILE_FUNC_START_LANGLE; goto common_call_extension;
common_call_extension:
   if (HAVE_EXTENSION_ALTMAC) goto common_function_macro;
   /* fallthrough. */
  default: macro_flags = TPP_MACROFILE_KIND_KEYWORD; break;
 }
 result = (struct TPPFile *)malloc((macro_flags & TPP_MACROFILE_KIND_FUNCTION)
                                   ? TPPFILE_SIZEOF_MACRO_FUNCTION
                                   : TPPFILE_SIZEOF_MACRO_KEYWORD);
 if unlikely(!result) { TPPLexer_SetErr(); return NULL; }
 result->f_refcnt            = 1;
 result->f_kind              = TPPFILE_KIND_MACRO;
 result->f_prev              = NULL;
 result->f_name              = keyword_entry->k_name;
 result->f_namesize          = keyword_entry->k_size;
 result->f_namehash          = keyword_entry->k_hash;
 result->f_macro.m_flags     = macro_flags;
 result->f_macro.m_deffile   = TOKEN.t_file;
 result->f_macro.m_pushprev  = NULL; /* Used for macro push/pop */
 result->f_macro.m_pushcount = 0;    /* Used for macro push/pop */
 TPPFile_Incref(result->f_macro.m_deffile); /* Create reference. */
 /* Store a reference to the macro text block (part of the current source chunk). */
 result->f_text = result->f_macro.m_deffile->f_text;
 TPPString_Incref(result->f_text);
 if (macro_flags&TPP_MACROFILE_KIND_FUNCTION) {
  ++curfile->f_pos; /* Skip the parenthesis-character. */
  /* 'token' now contains the first token after the (/{/[/< */
  /* Parse the argument */
  result->f_macro.m_function.f_argc = 0;
  result->f_macro.m_function.f_argbuf = NULL; /* Lazily allocated later. */
  arginfo_a = 0,arginfo_v = NULL;
  TPPLexer_YieldRaw();
  if (TOK != argend_token) while (TOK > 0) {
   if (TPP_ISKEYWORD(TOK)) { /* Argument name. */
    argument_name = TOK;
    if unlikely(argument_name == KWD___VA_ARGS__ ||
               (argument_name == KWD___VA_COMMA__ && HAVE_EXTENSION_VA_COMMA) ||
               (argument_name == KWD___VA_NARGS__ && HAVE_EXTENSION_VA_NARGS)) {
     /* Warn amount argument name with otherwise special meaning. */
     if unlikely(!WARN(W_SPECIAL_ARGUMENT_NAME,TOKEN.t_kwd)) goto err_arginfo;
    }
add_macro_argument:
    /* Check if an argument named 'argument_name' already exists. */
    new_arginfo_v = arginfo_v+result->f_macro.m_function.f_argc;
    while (new_arginfo_v-- != arginfo_v) if (new_arginfo_v->ai_id == argument_name) {
     if unlikely(!WARN(W_ARGUMENT_NAMED_ALREADY_TAKEN,argument_name)) goto err_arginfo;
     goto skip_argument_name;
    }

    if (arginfo_a == result->f_macro.m_function.f_argc) {
     arginfo_a = arginfo_a ? arginfo_a*2 : 2;
     new_arginfo_v = (struct arginfo_t *)realloc(arginfo_v,arginfo_a*
                                                 sizeof(struct arginfo_t));
     if unlikely(!new_arginfo_v) goto seterr_arginfo;
     arginfo_v = new_arginfo_v;
    } else {
     new_arginfo_v = arginfo_v;
    }
    new_arginfo_v += result->f_macro.m_function.f_argc++;
    new_arginfo_v->ai_id = argument_name;
#if TPP_CONFIG_DEBUG
    if (argument_name == KWD___VA_ARGS__) {
     new_arginfo_v->ai_name     = (char *)"__VA_ARGS__";
     new_arginfo_v->ai_namesize = 11;
    } else {
     assert(TOKEN.t_kwd);
     new_arginfo_v->ai_name     = TOKEN.t_kwd->k_name;
     new_arginfo_v->ai_namesize = TOKEN.t_kwd->k_size;
    }
#endif
    new_arginfo_v->ai_ins      = 0;
    new_arginfo_v->ai_ins_exp  = 0;
    new_arginfo_v->ai_ins_str  = 0;
skip_argument_name:
    TPPLexer_YieldRaw();
    if (argument_name != KWD___VA_ARGS__ &&
        TOK == TOK_DOTS && HAVE_EXTENSION_GCC_VA_ARG) {
     /* GCC extension: "#define foo(args...) args" */
     result->f_macro.m_flags |= TPP_MACROFILE_FLAG_FUNC_VARIADIC;
     TPPLexer_YieldRaw();
    }
    if (result->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_VARIADIC && TOK != argend_token) {
     /* Expected ')' after varargs argument name. */
     if (!WARN(W_EXPECTED_ARGEND_AFTER_VARARGS,argend_token)) goto err_arginfo;
    }
   } else if (TOK == TOK_DOTS && HAVE_EXTENSION_VA_ARGS) {
    argument_name            = KWD___VA_ARGS__;
    result->f_macro.m_flags |= TPP_MACROFILE_FLAG_FUNC_VARIADIC;
    goto add_macro_argument;
   } else {
    if (!WARN(W_EXPECTED_MACRO_ARGUMENT_NAME,argend_token)) goto err_arginfo;
    break;
   }
   if (TOK == argend_token) break; /* Stop parsing the argument list. */
   if (TOK == ',') TPPLexer_YieldRaw();
   else if (!WARN(W_EXPECTED_COMMA_OR_ARGEND,argend_token)) goto err_arginfo;
  }
  if (result->f_macro.m_function.f_argc != arginfo_a) {
   new_arginfo_v = (struct arginfo_t *)realloc(arginfo_v,result->f_macro.m_function.f_argc*
                                               sizeof(struct arginfo_t));
   if (new_arginfo_v) arginfo_v = new_arginfo_v;
  }
  result->f_macro.m_function.f_arginfo = arginfo_v; /*< Inherit data. */
  /* At this point, 'TOK == argend_token' unless user-syntax was wrong.
   * >> For that reason, the file pointer should point to the first whitespace of the macro block. */
  assert(curfile == TOKEN.t_file);
  /* Figure out where the block should start.
   * NOTE: The yield in here is what requires LF tokens
   *       to be enabled, so that an empty macro body
   *       can be detected when immediately detecting
   *       a linefeed. */
  if (CURRENT.l_flags&TPPLEXER_FLAG_KEEP_MACRO_WHITESPACE) {
   result->f_begin = curfile->f_pos;
   TPPLexer_YieldRaw();
   assert(curfile == TOKEN.t_file);
  } else {
   TPPLexer_YieldRaw();
   assert(curfile == TOKEN.t_file);
   result->f_begin = TOKEN.t_begin;
  }
  result->f_macro.m_function.f_expansions = 0;
  /* Scan the entirety of the macro's text block.
   * NOTE: If requested to, use traditional scanning. */
  if (HAVE_EXTENSION_TRADITIONAL_MACRO) {
   char *macro_end;
   result->f_end = curfile->f_end;
   macro_end = macro_function_scan_block_traditional(result);
   if unlikely(!macro_end) goto err_arginfo;
   result->f_end = macro_end;
   assert(curfile == TOKEN.t_file);
   TOKEN.t_file->f_pos = macro_end;
   /* Yield the next token after the macro. */
   TPPLexer_YieldRaw();
  } else {
   if unlikely(!macro_function_scan_block(result)) goto err_arginfo;
   assert(curfile == TOKEN.t_file);
   result->f_end = TOKEN.t_begin;
  }
  /* NOTE: Because the scan_block function disabled chunk transitions,
   *       it is possible that the token now describes a symbolic EOF.
   *       >> In that case, simply try yielding another token
   *          now that the lexer flags have been restored. */
  if (!TOKEN.t_id) TPPLexer_YieldRaw();
 } else {
  result->f_begin = curfile->f_pos;
  /* Skip whitespace at the front. */
  if (!(CURRENT.l_flags&TPPLEXER_FLAG_KEEP_MACRO_WHITESPACE)) {
   while (result->f_begin != curfile->f_end) {
    while (SKIP_WRAPLF(result->f_begin,curfile->f_end));
    if (result->f_begin == curfile->f_end || 
        tpp_isnospace_orlf(*result->f_begin)) break;
    ++result->f_begin;
   }
  }
  /* Scan forward until the end of the macro block. */
  while (TOK > 0 && TOK != '\n') TPPLexer_YieldRaw();
  assert(curfile == TOKEN.t_file);
  result->f_end = TOKEN.t_begin;
 }
 /* Trim down the end of the macro's text block. */
 assert(result->f_end >= result->f_begin);
 if (!(CURRENT.l_flags&TPPLEXER_FLAG_KEEP_MACRO_WHITESPACE)) {
  while (result->f_end != result->f_begin) {
   while (SKIP_WRAPLF_REV(result->f_end,result->f_begin));
   if (result->f_end == result->f_begin ||
      !tpp_isspace(result->f_end[-1])) break;
   --result->f_end;
  }
 }
 assert(result->f_end >= result->f_begin);
 assert(result->f_end >= curfile->f_begin);
 assert(result->f_end <= curfile->f_end);
 TPPFile_LCAt(result->f_macro.m_deffile,
             &result->f_macro.m_defloc,
              result->f_begin);
 if (((*result->f_end == '\r' && result->f_end[1] != '\n') ||
       *result->f_end == '\n')) {
  /* Adjust the file's offset according to the linefeed we're deleting. */
  if (curfile->f_kind == TPPFILE_KIND_TEXT) {
   ++curfile->f_textfile.f_lineoff;
   fix_linenumber = 1;
  }
 }

 assert(result->f_macro.m_deffile);
 assert(*result->f_end != '#');
 *result->f_end = '\0';

 /* Check for special case: Empty text block.
  * >> We can optimize this case to remove a dependency on the current file chunk,
  *    thus meaning that the file's chunk may be easier to deallocate when its
  *    no longer being used. */
 if (result->f_begin == result->f_end) {
  TPPString_Incref(empty_string);
  TPPString_Decref(result->f_text);
  result->f_text  = empty_string;
  result->f_begin = result->f_end = empty_string->s_text;
 }
 /* Finally set the seek pointer to the front. */
 result->f_pos = result->f_begin;
 {
  int warn_error;
  uint32_t kwd_flags = TPPKeyword_Getflags(keyword_entry);
  if (keyword_entry->k_macro) {
   struct TPPFile *oldfile = keyword_entry->k_macro;
   if (((size_t)(oldfile->f_end-oldfile->f_begin) !=
        (size_t)(result ->f_end-result ->f_begin) ||
         memcmp(result->f_begin,oldfile->f_begin,
               (size_t)(result->f_end-result->f_begin)) != 0)) {
    /* Make sure to use the correct line number in this warning. */
    if (fix_linenumber) --curfile->f_textfile.f_lineoff;
    warn_error = WARN(W_REDEFINING_MACRO,keyword_entry);
    if (fix_linenumber) ++curfile->f_textfile.f_lineoff;
    if (!warn_error) goto err_result;
   }
   if (kwd_flags&TPP_KEYWORDFLAG_LOCKED) goto locked_keyword;
   TPPKeyword_Undef(keyword_entry);
  }
  if (kwd_flags&TPP_KEYWORDFLAG_LOCKED) {
locked_keyword:
   if (fix_linenumber) --curfile->f_textfile.f_lineoff;
   warn_error = WARN(W_CANT_DEFINE_LOCKED_KEYWORD,keyword_entry);
   if (fix_linenumber) ++curfile->f_textfile.f_lineoff;
err_result:
   TPPFile_Decref(result);
   result = keyword_entry->k_macro;
   if (!result) result = &TPPFile_Empty;
   if unlikely(!warn_error) result = NULL;
   goto done;
  }
 }
 keyword_entry->k_macro = result; /*< Inherit reference. */
done:
 return result;
seterr_arginfo: TPPLexer_SetErr();
err_arginfo: free(arginfo_v);
/*err_r:*/
 TPPFile_Decref(result->f_macro.m_deffile);
 TPPString_Decref(result->f_text);
 free(result);
 return NULL;
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif

PRIVATE int
keyword_pushmacro(struct TPPKeyword *self) {
 struct TPPFile *macro;
 assert(self);
 if (!self->k_macro) return 1;
 assert(self->k_macro->f_kind == TPPFILE_KIND_MACRO);
 if (!TPPKeyword_MAKERARE(self)) return 0;
 macro = self->k_macro;
 if (macro == self->k_rare->kr_oldmacro) {
  /* Special case: The macro was already pushed.
   * >> We can't just push it again, but instead must
   *    recursively safe how often this version was pushed. */
  assert(macro->f_macro.m_pushcount > 0);
 } else {
  assert(!macro->f_macro.m_pushprev);
  assert(!macro->f_macro.m_pushcount);
  macro->f_macro.m_pushprev = self->k_rare->kr_oldmacro;
  self->k_rare->kr_oldmacro = macro;
  TPPFile_Incref(macro);
 }
 ++macro->f_macro.m_pushcount;
 return 1;
}
PRIVATE void
keyword_popmacro(struct TPPKeyword *self, int maybe_keep_current) {
 struct TPPFile *macro;
 assert(self);
 /* Drop a reference to a current definition of the macro. */
 macro = self->k_rare ? self->k_rare->kr_oldmacro : NULL;
 if (!macro) goto set_macro; /* Special case: Macro wasn't defined before. */
 assert(macro);
 assert(macro->f_kind == TPPFILE_KIND_MACRO);
 if likely(!--macro->f_macro.m_pushcount) {
  /* Last pop (Must actually remove the file from the push/pop chain) */
  self->k_rare->kr_oldmacro = macro->f_macro.m_pushprev; /*< Inherit reference. */
  macro->f_macro.m_pushprev = NULL;
 } else {
  /* The macro was pushed multiple times and must remain on the push/pop stack. */
  TPPFile_Incref(macro);
 }
set_macro:
 if (self->k_macro) {
  /* If we're not supposed to override new definitions,  */
  if (maybe_keep_current) { TPPFile_Decref(macro); goto done; }
  TPPFile_Decref(self->k_macro);
 }
 self->k_macro = macro; /*< Inherit reference. */
done:
 assert(!macro || !macro->f_macro.m_pushprev);
 assert(!macro || !macro->f_macro.m_pushcount);
}

PRIVATE void
rare_rehash_asserts(struct TPPRareKeyword *self, size_t newsize) {
 struct TPPAssertion **newvec,*iter,*next,**bucket;
 struct TPPAssertion **bucket_iter,**bucket_end;
 assert(newsize);
 assert(newsize > self->kr_asserts.as_assa);
 assert((self->kr_asserts.as_assa != 0) ==
        (self->kr_asserts.as_assv != NULL));
 newvec = (struct TPPAssertion **)calloc(newsize,sizeof(struct TPPAssertion *));
 if unlikely(!newvec) return; /* Ignore errors here. */
 bucket_end = (bucket_iter = self->kr_asserts.as_assv)+
                             self->kr_asserts.as_assa;
 for (; bucket_iter != bucket_end; ++bucket_iter) {
  iter = *bucket_iter;
  while (iter) {
   /* Transfer all assertion entries into the new bucket vector. */
   assert(iter->as_kwd);
   next = iter->as_next;
   bucket = &newvec[iter->as_kwd->k_hash % newsize];
   iter->as_next = *bucket;
   *bucket = iter;
   iter = next;
  }
 }
 free(self->kr_asserts.as_assv);
 self->kr_asserts.as_assa = newsize;
 self->kr_asserts.as_assv = newvec;
}

PRIVATE int
keyword_addassert(struct TPPKeyword *__restrict self,
                  struct TPPKeyword *__restrict assertion) {
 struct TPPRareKeyword *rare;
 struct TPPAssertion **bucket,*slot;
 assert(self),assert(assertion);
 if unlikely(!TPPKeyword_MAKERARE(self)) return 0;
 rare = self->k_rare,assert(rare);
 if (rare->kr_asserts.as_assa) {
  slot = rare->kr_asserts.as_assv[assertion->k_hash % rare->kr_asserts.as_assa];
  while (slot) { if (slot->as_kwd == assertion) return 2; slot = slot->as_next; }
 }
 if (rare->kr_asserts.as_assc >= rare->kr_asserts.as_assa*2) {
  size_t newsize = rare->kr_asserts.as_assa;
  if ((newsize *= 2) == 0) newsize = 2;
  rare_rehash_asserts(rare,newsize);
  assert((rare->kr_asserts.as_assa != 0) ==
         (rare->kr_asserts.as_assv != NULL));
  /* Make sure that we didn't fail the first rehash. */
  if unlikely(!rare->kr_asserts.as_assa) return 0;
 }
 /* Must allocate a new slot. */
 slot = (struct TPPAssertion *)malloc(sizeof(struct TPPAssertion));
 if unlikely(!slot) return 0;
 bucket        = &rare->kr_asserts.as_assv[assertion->k_hash % rare->kr_asserts.as_assa];
 slot->as_next = *bucket;
 slot->as_kwd  = assertion;
 *bucket       = slot;
 ++rare->kr_asserts.as_assc;
 return 1;
}
PRIVATE int
keyword_delassert(struct TPPKeyword *__restrict self,
                  struct TPPKeyword *__restrict assertion) {
 struct TPPRareKeyword *rare;
 struct TPPAssertion **pelem,*elem;
 assert(self),assert(assertion);
 if unlikely((rare = self->k_rare) == NULL ||
             !rare->kr_asserts.as_assa ||
             !rare->kr_asserts.as_assc) return 0;
 pelem = &rare->kr_asserts.as_assv[assertion->k_hash % rare->kr_asserts.as_assa];
 while ((elem = *pelem) != NULL) {
  if (elem->as_kwd == assertion) {
   --rare->kr_asserts.as_assc;
   *pelem = elem->as_next;
   free(elem);
   return 1;
  }
  pelem = &elem->as_next;
 }
 return 0;
}
PRIVATE int
keyword_hasassert(struct TPPKeyword *__restrict self,
                  struct TPPKeyword *__restrict assertion) {
 struct TPPRareKeyword *rare;
 struct TPPAssertion *elem;
 assert(self),assert(assertion);
 if unlikely((rare = self->k_rare) == NULL ||
             !rare->kr_asserts.as_assa ||
             !rare->kr_asserts.as_assc) return 0;
 elem = rare->kr_asserts.as_assv[assertion->k_hash % rare->kr_asserts.as_assa];
 while (elem) { if (elem->as_kwd == assertion) return 1; elem = elem->as_next; }
 return 0;
}
PRIVATE int
keyword_clrassert(struct TPPKeyword *__restrict self) {
 struct TPPRareKeyword *rare;
 struct TPPAssertion **bucket_iter,**bucket_end,*iter,*next;
 assert(self);
 if unlikely((rare = self->k_rare) == NULL) return 0;
 assert((rare->kr_asserts.as_assa != 0) ==
        (rare->kr_asserts.as_assv != NULL));
 if unlikely(!rare->kr_asserts.as_assa) return 0;
 bucket_end = (bucket_iter = rare->kr_asserts.as_assv)+
                             rare->kr_asserts.as_assa;
 for (; bucket_iter != bucket_end; ++bucket_iter) {
  iter = *bucket_iter;
  while (iter) {
   next = iter->as_next;
   assert(iter->as_kwd);
   free(iter);
   iter = next;
  }
 }
 free(rare->kr_asserts.as_assv);
 rare->kr_asserts.as_assc = 0;
 rare->kr_asserts.as_assa = 0;
 rare->kr_asserts.as_assv = NULL;
 return 1;
}



PUBLIC int
TPP_ISBUILTINMACRO(tok_t id) {
 int result = 0;
 /* Check if builtin macros are even enabled. */
 if (!(CURRENT.l_flags&TPPLEXER_FLAG_NO_BUILTIN_MACROS)) {
  switch (id) {
#define MACRO(name,if) case name: result = !!(if); break;
#include "tpp-defs.inl"
#undef MACRO
   default: break;
  }
 }
 return result;
}

#if TPP_CONFIG_ONELEXER
#define KWD(name,str) \
static struct {\
 void *a[3];\
 tok_t b; size_t c; hash_t d;\
 char e[COMPILER_STRLEN0(str)];\
} builtin_##name = {{NULL,NULL,NULL},name,COMPILER_STRLEN(str),pp_maybe_hashof(str),str};
#else
struct kwd_def {
 tok_t  kd_id;
 size_t kd_size;
#ifdef pp_hashof
 size_t kd_hash;
#endif
 char   kd_name[TPP_SYMARRAY_SIZE];
};
#ifdef pp_hashof
#define KWD(name,str) \
 static struct { tok_t a; size_t b; size_t c; char d[COMPILER_STRLEN0(str)]; }\
 const builtin_##name = {name,pp_hashof(str),COMPILER_STRLEN(str),str};
#else
#define KWD(name,str) \
 static struct { tok_t a; size_t b; char c[COMPILER_STRLEN0(str)]; }\
 const builtin_##name = {name,COMPILER_STRLEN(str),str};
#endif
#endif
#include "tpp-defs.inl"
#undef KWD

#if TPP_CONFIG_ONELEXER
#define KWD(name,str)   (struct TPPKeyword *)&builtin_##name,
static struct TPPKeyword *
#else
#define KWD(name,str)   (struct kwd_def *)&builtin_##name,
static struct kwd_def const *
#endif
const builtin_keywords[_KWD_COUNT] = {
#include "tpp-defs.inl"
};
#undef KWD

PRIVATE void
cleanup_keyword(struct TPPKeyword *__restrict self) {
 struct TPPRareKeyword *rare;
 if (self->k_macro) TPPFile_Decref(self->k_macro);
 if ((rare = self->k_rare) != NULL) {
  struct TPPFile *fileiter,*filenext;
  struct TPPAssertion **ass_iter,**ass_end,*aiter,*anext;
  if (rare->kr_file) TPPFile_Decref(rare->kr_file);
  if (rare->kr_defmacro) TPPFile_Decref(rare->kr_defmacro);
  fileiter = rare->kr_oldmacro;
  while (fileiter) {
   assert(fileiter->f_kind == TPPFILE_KIND_MACRO);
   filenext = fileiter->f_macro.m_pushprev;
   TPPFile_Decref(fileiter);
   fileiter = filenext;
  }
  assert((rare->kr_asserts.as_assv != NULL) ==
         (rare->kr_asserts.as_assa != 0));
  ass_end = (ass_iter = rare->kr_asserts.as_assv)+
                        rare->kr_asserts.as_assa;
  for (; ass_iter != ass_end; ++ass_iter) {
   aiter = *ass_iter;
   while (aiter) {
    anext = aiter->as_next;
    assert(aiter->as_kwd);
    free(aiter);
    aiter = anext;
   }
  }
  free(rare->kr_asserts.as_assv);
  free(rare);
 }
}

PRIVATE void
destroy_keyword_map(struct TPPKeywordMap *__restrict self) {
 struct TPPKeyword **bucket_iter,**bucket_end,*iter,*next;
 bucket_end = (bucket_iter = self->km_bucketv)+self->km_bucketc;
 for (; bucket_iter != bucket_end; ++bucket_iter) {
  iter = *bucket_iter;
  while (iter) {
   next = iter->k_next;
   cleanup_keyword(iter);
#if TPP_CONFIG_ONELEXER
   /* Must only free if the keyword isn't a builtin. */
   if (TPP_ISUSERKEYWORD(iter->k_id))
#endif
   {
    free(iter);
   }
   iter = next;
  }
 }
 free(self->km_bucketv);
}

#if TPP_CONFIG_ONELEXER
PRIVATE void
def_builtin_keyword(struct TPPKeyword *keyword) {
 struct TPPKeyword **bucket;
#ifdef pp_hashof
 assert(keyword->k_hash == hashof(keyword->k_name,keyword->k_size));
#else
 keyword->k_hash = hashof(keyword->k_name,keyword->k_size);
#endif
 bucket = &CURRENT.l_keywords.km_bucketv[keyword->k_hash % 
           CURRENT.l_keywords.km_bucketc];
 keyword->k_next = *bucket;
 *bucket = keyword;
}
#else

PRIVATE int
def_builtin_keyword(struct TPPKeywordMap *__restrict self,
                    struct kwd_def const *__restrict def) {
 struct TPPKeyword *keyword,**bucket;
 keyword = (struct TPPKeyword *)malloc(TPP_OFFSETOF(struct TPPKeyword,k_name)+
                                      (def->kd_size+1)*sizeof(char));
 if unlikely(!keyword) return 0;
 keyword->k_macro = NULL;
 keyword->k_rare  = NULL;
 keyword->k_id    = def->kd_id;
#ifdef pp_hashof
 assert(def->kd_hash == hashof(def->kd_name,def->kd_size));
 keyword->k_hash  = def->kd_hash;
#else
 keyword->k_hash  = hashof(def->kd_name,def->kd_size);
#endif
 keyword->k_size  = def->kd_size;
 memcpy(keyword->k_name,def->kd_name,(def->kd_size+1)*sizeof(char));
 bucket = &self->km_bucketv[keyword->k_hash % self->km_bucketc];
 keyword->k_next = *bucket;
 *bucket = keyword;
 return 1;
}
#endif

PRIVATE int
load_builtin_keywords(struct TPPKeywordMap *__restrict self) {
 enum{
  first_hashsize = _KWD_COUNT <= 2 ? 2 :
                   _KWD_COUNT <= 4 ? 4 :
                   _KWD_COUNT <= 8 ? 8 :
                   _KWD_COUNT <= 16 ? 16 :
                   _KWD_COUNT <= 32 ? 32 :
                   _KWD_COUNT <= 64 ? 64 :
                   _KWD_COUNT <= 128 ? 128 :
                   _KWD_COUNT <= 256 ? 256 :
                   _KWD_COUNT <= 512 ? 512 :
                   _KWD_COUNT <= 1024 ? 1024 :
                   _KWD_COUNT
 };
 assert(self);
 /* Allocate the initial hashmap. */
 self->km_entryc  = _KWD_COUNT;
 self->km_bucketc = first_hashsize;
 self->km_bucketv = (struct TPPKeyword **)calloc(first_hashsize,
                                                 sizeof(struct TPPKeyword *));
 if (!self->km_bucketv) return 0;

 /* Load builtin keywords. */
 {
#if TPP_CONFIG_ONELEXER
  struct TPPKeyword *const *iter,*const *end;
  end = (iter = builtin_keywords)+_KWD_COUNT;
  for (; iter != end; ++iter) def_builtin_keyword(*iter);
#else
  struct kwd_def const *const *iter,*const *end;
  end = (iter = builtin_keywords)+_KWD_COUNT;
  for (; iter != end; ++iter) {
   if (!def_builtin_keyword(self,*iter)) goto err;
 }
#endif
 }

 return 1;
#if !TPP_CONFIG_ONELEXER
err:
 destroy_keyword_map(self);
 return 0;
#endif
}

PRIVATE struct {
#define EXTENSION(name,str,default)  unsigned int s_##name : 1;
#include "tpp-defs.inl"
#undef EXTENSION
} const default_extensions_state = {
#define EXTENSION(name,str,default)  default,
#include "tpp-defs.inl"
#undef EXTENSION
};


/* Figure out the default state of warnings. */
PRIVATE struct {
#define WGROUP(name,str,default)     unsigned int s_##name : TPP_WARNING_BITS;
#include "tpp-defs.inl"
#undef WGROUP
#define WARNING(name,groups,default) unsigned int s_##name : TPP_WARNING_BITS;
#include "tpp-defs.inl"
#undef WARNING
} const default_warnings_state = {
#define WGROUP(name,str,default)     default,
#include "tpp-defs.inl"
#undef WGROUP
#define WARNING(name,groups,default) default,
#include "tpp-defs.inl"
#undef WARNING
};
#define DEFAULT_WARNING_STATE(wid) \
 (wstate_t)((((uint8_t *)&default_warnings_state)[(wid)/(8/TPP_WARNING_BITS)] \
         >> (((wid)%(8/TPP_WARNING_BITS))*TPP_WARNING_BITS)) & 3)


PRIVATE char const *const wgroup_names[WG_COUNT+1] = {
#ifndef __INTELLISENSE__
#define WGROUP(name,str,default) str,
#include "tpp-defs.inl"
#undef WGROUP
#endif
 NULL
};

PUBLIC int TPPLexer_PushWarnings(void) {
 struct TPPWarningState *newstate,*curstate;
 assert(TPPLexer_Current);
 curstate = CURRENT.l_warnings.w_curstate;
 assert(curstate);
 assert((curstate == &CURRENT.l_warnings.w_basestate) ==
        (curstate->ws_prev == NULL));
 newstate = (struct TPPWarningState *)malloc(sizeof(struct TPPWarningState));
 if unlikely(!newstate) return 0;
 if ((newstate->ws_extendeda = curstate->ws_extendeda) != 0) {
  /* todo: maybe only copy slots that are actually in use? */
  size_t extended_size = newstate->ws_extendeda*sizeof(struct TPPWarningStateEx);
  newstate->ws_extendedv = (struct TPPWarningStateEx *)malloc(extended_size);
  if unlikely(!newstate->ws_extendedv) { free(newstate); return 0; }
  memcpy(newstate->ws_extendedv,
         curstate->ws_extendedv,
         extended_size);
 } else {
  newstate->ws_extendedv = NULL;
 }
 memcpy(newstate->ws_state,curstate->ws_state,TPP_WARNING_BITSETSIZE);
 newstate->ws_prev = curstate;
 CURRENT.l_warnings.w_curstate = newstate;
 return 1;
}
PUBLIC int TPPLexer_PopWarnings(void) {
 struct TPPWarningState *curstate;
 assert(TPPLexer_Current);
 curstate = CURRENT.l_warnings.w_curstate;
 assert(curstate);
 assert((curstate == &CURRENT.l_warnings.w_basestate) ==
        (curstate->ws_prev == NULL));
 /* Check if this state has a predecessor. */
 if unlikely(!curstate->ws_prev) return 0;
 CURRENT.l_warnings.w_curstate = curstate->ws_prev;
 free(curstate->ws_extendedv);
 free(curstate);
 return 1;
}

PUBLIC int TPPLexer_PushExtensions(void) {
 struct TPPExtState *state_copy;
 assert(TPPLexer_Current);
 state_copy = (struct TPPExtState *)malloc(sizeof(struct TPPExtState));
 if unlikely(!state_copy) return 0;
 /* Create a copy of the current extension state. */
 memcpy(state_copy,&CURRENT.l_extensions,sizeof(struct TPPExtState));
 /* Link the copy we've just created. */
 CURRENT.l_extensions.es_prev = state_copy;
 return 1;
}
PUBLIC int TPPLexer_PopExtensions(void) {
 struct TPPExtState *prev_state;
 assert(TPPLexer_Current);
 prev_state = CURRENT.l_extensions.es_prev;
 if unlikely(!prev_state) return 0;
 /* Restore the old extension state. */
 memcpy(&CURRENT.l_extensions,prev_state,
        sizeof(struct TPPExtState));
 free(prev_state);
 return 1;
}


PRIVATE int set_wstate(int wid, wstate_t state) {
 struct TPPWarningState *curstate; size_t newalloc;
 struct TPPWarningStateEx *iter,*end,*newslot;
 uint8_t *bitset_byte,byte_shift;
 assert(TPPLexer_Current);
 curstate = CURRENT.l_warnings.w_curstate;
 assert(curstate);
 assert((curstate == &CURRENT.l_warnings.w_basestate) ==
        (curstate->ws_prev == NULL));
 assert(wid < TPP_WARNING_TOTAL);
 if (state == WSTATE_DEFAULT) state = DEFAULT_WARNING_STATE(wid);
 end = (iter = curstate->ws_extendedv)+curstate->ws_extendeda;
 while (iter != end && iter->wse_wid < wid) ++iter;
 assert(iter == end || iter->wse_wid >= wid);
 bitset_byte = &curstate->ws_state[wid/(8/TPP_WARNING_BITS)];
 byte_shift  = (uint8_t)((wid%(8/TPP_WARNING_BITS))*TPP_WARNING_BITS);
 assert(byte_shift == 0 || byte_shift == 2 ||
        byte_shift == 4 || byte_shift == 6);
 if (state == WSTATE_SUPPRESS) {
  if (iter == end || iter->wse_wid != wid) {
   /* Must insert/allocate a new slot. */
   newslot = iter;
   while (newslot != end && newslot->wse_suppress) ++newslot;
   if (newslot == end) {
    /* No free slots. */
    newalloc = curstate->ws_extendeda ? curstate->ws_extendeda*2 : 2;
    newslot = (struct TPPWarningStateEx *)realloc(curstate->ws_extendedv,newalloc*
                                                  sizeof(struct TPPWarningStateEx));
    if unlikely(!newslot) return 0;
    /* ZERO-initialize the new memory. */
    memset(newslot+curstate->ws_extendeda,0,
          (newalloc-curstate->ws_extendeda)*
           sizeof(struct TPPWarningStateEx));
    iter = newslot+(iter-curstate->ws_extendedv);
    curstate->ws_extendedv = newslot;
    newslot += curstate->ws_extendeda;
    curstate->ws_extendeda = newalloc;
   }
   assertf(iter <= newslot,("iter = %p\nnewslot = %p\n",iter,newslot));
   assert(iter == newslot || iter->wse_wid > wid);
   assert(iter == curstate->ws_extendedv || iter[-1].wse_wid < wid);
   /* Move data between iter and newslot. */
   memmove(iter+1,iter,(size_t)(newslot-iter)*
           sizeof(struct TPPWarningStateEx));
   iter->wse_wid      = wid;
   iter->wse_suppress = 0;
   iter->wse_oldstate = (wstate_t)((*bitset_byte >> byte_shift) & 3);
   assert(iter->wse_oldstate != WSTATE_SUPPRESS);
  }
  /* Suppress the warning a little bit more. */
  ++iter->wse_suppress;
 }
 if (state != WSTATE_SUPPRESS &&
    (*bitset_byte & (3 << byte_shift)) == WSTATE_SUPPRESS) {
  /* The old state was suppress, but the new one isn't. */
  assert((iter != end && iter->wse_wid == wid) &&
         "Warning is set to suppress, but is lacking an extended entry");
  assert(iter->wse_suppress);
  --iter->wse_suppress;
 }
 /* Set the new warning state in the bitset. */
 *bitset_byte &= (uint8_t)~(3 << byte_shift);
 *bitset_byte |= (uint8_t)(state << byte_shift);
 return 1;
}

#define wid_isvalid(wid)  ((wid) < TPP_WARNING_TOTAL)
PRIVATE unsigned int wnum2id(int wnum) {
 int group_base = 0,group_start = WG_COUNT;
#ifndef __INTELLISENSE__
#define WARNING_NAMESPACE(name,id) \
 if (wnum >= id) group_base = id,group_start = _WID_##name##_START+WG_COUNT;
#include "tpp-defs.inl"
#undef WARNING_NAMESPACE
#endif
 assert(wnum >= group_base);
 wnum -= group_base;
 wnum += group_start;
 return (unsigned int)wnum;
}

PRIVATE wstate_t get_wstate(unsigned int wid) {
 struct TPPWarningState *curstate;
 uint8_t bitset_byte,byte_shift;
 assert(TPPLexer_Current);
 assert(wid_isvalid(wid));
 curstate = CURRENT.l_warnings.w_curstate;
 assert(curstate);
 assert((curstate == &CURRENT.l_warnings.w_basestate) ==
        (curstate->ws_prev == NULL));
 bitset_byte = curstate->ws_state[wid/(8/TPP_WARNING_BITS)];
 byte_shift  = (uint8_t)((wid%(8/TPP_WARNING_BITS))*TPP_WARNING_BITS);
 return (wstate_t)((bitset_byte >> byte_shift)&3);
}
PUBLIC wstate_t TPPLexer_GetWarning(int wnum) {
 unsigned int wid = wnum2id(wnum);
 if unlikely(!wid_isvalid(wid)) return WSTATE_UNKNOWN;
 return get_wstate(wid);
}
PUBLIC int TPPLexer_SetWarning(int wnum, wstate_t state) {
 unsigned int wid = wnum2id(wnum);
 return unlikely(!wid_isvalid(wid)) ? 2 : set_wstate(wid,state);
}
PUBLIC wstate_t TPPLexer_GetWarningGroup(int wgrp) {
 if unlikely((unsigned int)wgrp >= WG_COUNT) return WSTATE_UNKNOWN;
 return get_wstate((unsigned int)wgrp);
}
PUBLIC int TPPLexer_SetWarningGroup(int wgrp, wstate_t state) {
 return unlikely((unsigned int)wgrp >= WG_COUNT) ? 2 : set_wstate((unsigned int)wgrp,state);
}
PUBLIC int TPPLexer_SetWarnings(char const *__restrict group, wstate_t state) {
 char const *const *iter;
 for (iter = wgroup_names; *iter; ++iter) {
  if (!strcmp(*iter,group)) {
   return set_wstate((int)(iter-wgroup_names),state);
  }
 }
 return 2;
}
PUBLIC wstate_t TPPLexer_GetWarnings(char const *__restrict group) {
 char const *const *iter;
 assert(TPPLexer_Current);
 for (iter = wgroup_names; *iter; ++iter) {
  if (!strcmp(*iter,group)) {
   struct TPPWarningState *curstate;
   uint8_t bitset_byte,byte_shift;
   unsigned int wid = (unsigned int)(iter-wgroup_names);
   assert(wid_isvalid(wid));
   curstate = CURRENT.l_warnings.w_curstate;
   assert(curstate);
   assert((curstate == &CURRENT.l_warnings.w_basestate) ==
          (curstate->ws_prev == NULL));
   bitset_byte = curstate->ws_state[wid/(8/TPP_WARNING_BITS)];
   byte_shift  = (uint8_t)((wid%(8/TPP_WARNING_BITS))*TPP_WARNING_BITS);
   return (wstate_t)((bitset_byte >> byte_shift)&3);
  }
 }
 return WSTATE_UNKNOWN;
}
PRIVATE wstate_t do_invoke_wid(int wid) {
 struct TPPWarningState *curstate;
 struct TPPWarningStateEx *iter;
 uint8_t *bitset_byte,byte_shift;
 wstate_t state;
 assert(TPPLexer_Current);
 curstate = CURRENT.l_warnings.w_curstate;
 assert(curstate);
 assert((curstate == &CURRENT.l_warnings.w_basestate) ==
        (curstate->ws_prev == NULL));
 assert(wid < TPP_WARNING_TOTAL);
 bitset_byte = &curstate->ws_state[wid/(8/TPP_WARNING_BITS)];
 byte_shift  = (uint8_t)((wid%(8/TPP_WARNING_BITS))*TPP_WARNING_BITS);
 assert(byte_shift == 0 || byte_shift == 2 ||
        byte_shift == 4 || byte_shift == 6);
 state = (wstate_t)((*bitset_byte >> byte_shift) & 3);
 if (state == WSTATE_SUPPRESS) {
  iter = curstate->ws_extendedv;
  while ((assert(iter < curstate->ws_extendedv+curstate->ws_extendeda),
          iter->wse_wid != wid)) ++iter;
  if (!--iter->wse_suppress) {
   /* Last time this warning should be suppressed.
    * >> Must revert the bitset state to that before supression started. */
   *bitset_byte &= (uint8_t)~(3 << byte_shift);
   *bitset_byte |= ((uint8_t)iter->wse_oldstate << byte_shift);
  }
 }
 return state;
}

#ifndef __INTELLISENSE__
#define EXPAND_GROUPS(...)  {__VA_ARGS__,-1}
#define WARNING(name,groups,default) PRIVATE wgroup_t const wgroups_##name[] = EXPAND_GROUPS groups;
#include "tpp-defs.inl"
#undef WARNING
#undef EXPAND_GROUPS
#endif

PRIVATE wgroup_t const *const w_associated_groups[W_COUNT] = {
#ifndef __INTELLISENSE__
#define WARNING(name,groups,default) wgroups_##name,
#include "tpp-defs.inl"
#undef WARNING
#endif
};

PUBLIC int TPPLexer_InvokeWarning(int wnum) {
 wstate_t state; int found_warn = 0;
 wgroup_t const *group_iter;
 unsigned int wid = wnum2id(wnum);
 if unlikely(!wid_isvalid(wid)) return TPP_WARNINGMODE_WARN;
 state = do_invoke_wid(wid);
 group_iter = w_associated_groups[wid-WG_COUNT];
 for (;;) {
  if (state == WSTATE_SUPPRESS ||
      state == WSTATE_DISABLED) return TPP_WARNINGMODE_IGNORE;
  found_warn |= state == WSTATE_WARN;
  assert(wid >= WG_COUNT && wid < (WG_COUNT+W_COUNT));
  assert(group_iter);
  if (*group_iter < 0) break;
  state = do_invoke_wid(*group_iter++);
 }
 return found_warn ? TPP_WARNINGMODE_WARN : TPP_WARNINGMODE_ERROR;
}




PUBLIC int TPPLexer_Init(struct TPPLexer *__restrict self) {
 assert(self);
 /* Load builtin keywords. */
 if (!load_builtin_keywords(&self->l_keywords)) return 0;
 self->l_flags      = TPPLEXER_FLAG_DEFAULT;
 self->l_extokens   = TPPLEXER_TOKEN_DEFAULT;
 TPPFile_Incref(&TPPFile_Empty);
 self->l_token.t_id        = TOK_EOF;
 self->l_token.t_num       = 0;
 self->l_token.t_file      = &TPPFile_Empty;
 self->l_token.t_begin     = TPPFile_Empty.f_begin;
 self->l_token.t_end       = TPPFile_Empty.f_end;
 self->l_eob_file          = NULL;
 self->l_noerror           = 0;
 self->l_syspaths.il_prev  = NULL;
 self->l_syspaths.il_pathc = 0;
 self->l_syspaths.il_pathv = NULL;
 self->l_limit_mrec        = TPPLEXER_DEFAULT_LIMIT_MREC;
 self->l_limit_incl        = TPPLEXER_DEFAULT_LIMIT_INCL;
 self->l_eof_paren         = 0;
 self->l_warncount         = 0;
 self->l_tabsize           = TPPLEXER_DEFAULT_TABSIZE;
 self->l_counter           = 0;
 self->l_ifdef.is_slotc    = 0;
 self->l_ifdef.is_slota    = 0;
 self->l_ifdef.is_slotv    = NULL;
 self->l_warnings.w_curstate = &self->l_warnings.w_basestate;
 self->l_warnings.w_basestate.ws_extendeda = 0;
 self->l_warnings.w_basestate.ws_extendedv = NULL;
 self->l_warnings.w_basestate.ws_prev      = NULL;
 self->l_extensions.es_prev = NULL;
 assert(sizeof(default_warnings_state) >= TPP_WARNING_BITSETSIZE &&
        sizeof(default_warnings_state) <= TPP_OFFSETAFTER(struct TPPWarningState,ws_padding));
 assert(sizeof(default_extensions_state) >= TPP_EXTENSIONS_BITSETSIZE &&
        sizeof(default_extensions_state) <= TPP_OFFSETAFTER(struct TPPExtState,es_padding));
 memcpy(self->l_extensions.es_bitset,&default_extensions_state,TPP_EXTENSIONS_BITSETSIZE);
 memcpy(self->l_warnings.w_basestate.ws_state,&default_warnings_state,TPP_WARNING_BITSETSIZE);
 memset(&self->l_callbacks,0,sizeof(struct TPPCallbacks));
 assert(hashof("",0) == EMPTY_STRING_HASH);
#if __SIZEOF_SIZE_T__ == 4
 assert(hashof("<commandline>",13) == 3330511802ul);
#elif __SIZEOF_SIZE_T__ == 8
 assert(hashof("<commandline>",13) == 12182544704004658106ull);
#endif
#if TPP_BYTEORDER == 1234
 assert(*(uint32_t *)"\x00\x44\x88\xaa" == 0xaa884400);
#elif TPP_BYTEORDER == 4321
 assert(*(uint32_t *)"\x00\x44\x88\xaa" == 0x008844aa);
#elif TPP_BYTEORDER == 3412
 assert(*(uint32_t *)"\x00\x44\x88\xaa" == 0x88aa0044);
#endif
 assert(TPPFile_Empty.f_namehash ==
        hashof(TPPFile_Empty.f_name,
               TPPFile_Empty.f_namesize));
#if !TPP_CONFIG_MINMACRO
 assert(sizeof(void *) == __SIZEOF_POINTER__);
 assert(sizeof(wchar_t) == __SIZEOF_WCHAR_T__);
 assert(sizeof(short) == __SIZEOF_SHORT__);
 assert(sizeof(int) == __SIZEOF_INT__);
 assert(sizeof(long) == __SIZEOF_LONG__);
#if TPP_HAVE_LONGLONG
 assert(sizeof(long long) == __SIZEOF_LONG_LONG__);
#endif /* TPP_HAVE_LONGLONG */
 assert(sizeof(float) == __SIZEOF_FLOAT__);
 assert(sizeof(double) == __SIZEOF_DOUBLE__);
 assert(sizeof(long double) == __SIZEOF_LONG_DOUBLE__);
#endif /* !TPP_CONFIG_MINMACRO */
 return 1;
}
PUBLIC void TPPLexer_Quit(struct TPPLexer *__restrict self) {
 struct TPPFile *fileiter,*filenext;
 assert(self);
 /* Emit warnings about all unclosed #ifdef-blocks. */
 if (!(self->l_flags&TPPLEXER_FLAG_ERROR)) {
#if !TPP_CONFIG_ONELEXER
  struct TPPLexer *oldcurrent = TPPLexer_Current;
  TPPLexer_Current = self;
#endif /* !TPP_CONFIG_ONELEXER */
  while (self->l_ifdef.is_slotc--) {
   if (!WARN(W_IF_WITHOUT_ENDIF,&self->l_ifdef.is_slotv[
                                          self->l_ifdef.is_slotc])) break;
  }
#if !TPP_CONFIG_ONELEXER
  TPPLexer_Current = oldcurrent;
#endif /* !TPP_CONFIG_ONELEXER */
 }
 free(self->l_ifdef.is_slotv);
 { /* Free all non-popped and extended warnings information. */
  struct TPPWarningState *wstate,*wnext;
  wstate = self->l_warnings.w_curstate;
  assert(!self->l_warnings.w_basestate.ws_prev);
  for (;;) {
   assert(wstate);
   free(wstate->ws_extendedv);
   if (wstate == &self->l_warnings.w_basestate) break;
   wnext = wstate->ws_prev;
   free(wstate);
   wstate = wnext;
  }
 }
 /* Free keywords and remaining macros. */
 destroy_keyword_map(&self->l_keywords);
 /* Clear the remainder of the #include-stack. */
 fileiter = self->l_token.t_file;
 do {
  assert(fileiter);
  filenext = fileiter->f_prev;
  TPPFile_Decref(fileiter);
 } while ((fileiter = filenext) != NULL);
 { /* Clear system #include paths. */
  struct TPPIncludeList *include_curr,*include_prev;
  struct TPPString **iter,**end;
  include_curr = &self->l_syspaths;
  for (;;) {
   end = (iter = include_curr->il_pathv)+
                 include_curr->il_pathc;
   for (; iter != end; ++iter) TPPString_Decref(*iter);
   free(include_curr->il_pathv);
   include_prev = include_curr->il_prev;
   if (include_curr != &self->l_syspaths) free(include_curr);
   if (!include_prev) break;
   include_curr = include_prev;
  }
 }
 {
  struct TPPExtState *iter,*next;
  iter = self->l_extensions.es_prev;
  while (iter) {
   next = iter->es_prev;
   free(iter);
   iter = next;
  }
 }
}
PUBLIC void
TPPLexer_Reset(struct TPPLexer *__restrict self, uint32_t flags) {
 assert(self);
 if (flags&TPPLEXER_RESET_INCLUDE) {
  struct TPPFile *iter,*next;
  self->l_eob_file    = NULL;
  self->l_eof_file    = NULL;
  self->l_noerror     = TOK_EOF;
  self->l_warncount   = 0;
  self->l_token.t_id  = TOK_EOF;
  //self->l_token.t_num = 0; /* No need to do this... */
  self->l_token.t_kwd = NULL;
  iter = self->l_token.t_file;
  self->l_token.t_file = &TPPFile_Empty;
  TPPFile_Incref(&TPPFile_Empty);
  self->l_token.t_begin = TPPFile_Empty.f_begin;
  self->l_token.t_end   = TPPFile_Empty.f_end;
  /* Clear the #include-stack. */
  while (iter) {
   next = iter->f_prev;
   TPPFile_Decref(iter);
   iter = next;
  }
  /* Clear the #ifdef-stack. */
  self->l_ifdef.is_slotc = 0;
 }
 if (flags&TPPLEXER_RESET_ESTATE) {
  /* Reset the extension state. */
  memcpy(self->l_extensions.es_bitset,
        &default_extensions_state,
         TPP_EXTENSIONS_BITSETSIZE);
 }
 if (flags&TPPLEXER_RESET_ESTACK) {
  /* Clear the extension stack. */
  struct TPPExtState *iter,*next;
  iter = self->l_extensions.es_prev;
  self->l_extensions.es_prev = NULL;
  while (iter) {
   next = iter->es_prev;
   assert(iter != next);
   free(iter);
   iter = next;
  }
 }
 if (flags&TPPLEXER_RESET_WSTATE) {
  /* Reset the warning state. */
  memcpy(self->l_warnings.w_basestate.ws_state,
        &default_warnings_state,
         TPP_WARNING_BITSETSIZE);
  self->l_warnings.w_basestate.ws_extendeda = 0;
  free(self->l_warnings.w_basestate.ws_extendedv);
  self->l_warnings.w_basestate.ws_extendedv = NULL;
  self->l_warnings.w_basestate.ws_prev      = NULL;
 }
 if (flags&TPPLEXER_RESET_WSTACK) {
  /* Clear the warning stack. */
  struct TPPWarningState *iter,*next;
  iter = self->l_warnings.w_curstate;
  while (iter) {
   next = iter->ws_prev;
   assert(iter != next);
   if (iter != &self->l_warnings.w_basestate) {
    free(iter->ws_extendedv);
    free(iter);
   }
   iter = next;
  }
  self->l_warnings.w_curstate = &self->l_warnings.w_basestate;
 }
 if (flags&TPPLEXER_RESET_SYSPATHS) {
  struct TPPIncludeList *iter,*next;
  /* Clear all system #include-paths. */
  iter = &self->l_syspaths;
  do {
   struct TPPString **siter,**send;
   next = iter->il_prev;
   assert(iter != next);
   send = (siter = iter->il_pathv)+iter->il_pathc;
   for (; siter != send; ++siter)
        assert(*siter),
        TPPString_Decref(*siter);
   free(iter->il_pathv);
   if (iter != &self->l_syspaths) free(iter);
  } while ((iter = next) != NULL);
  self->l_syspaths.il_pathc = 0;
  self->l_syspaths.il_pathv = NULL;
 }
 if (flags&TPPLEXER_RESET_KEYWORDS) {
  struct TPPKeyword **bucket_iter,**bucket_end,**piter,*kwd;
  /* Clear all user-defined keywords. */
  bucket_end = (bucket_iter = self->l_keywords.km_bucketv)+
                              self->l_keywords.km_bucketc;
  for (; bucket_iter != bucket_end; ++bucket_iter) {
   piter = bucket_iter;
   while ((kwd = *piter) != NULL) {
    if (!TPP_ISUSERKEYWORD(kwd->k_id))
     piter = &kwd->k_next;
    else {
     cleanup_keyword(kwd);
     *piter = kwd->k_next;
     free(kwd);
    }
   }
  }
  self->l_keywords.km_entryc = _KWD_COUNT;
 } else if (flags&(TPPLEXER_RESET_MACRO|TPPLEXER_RESET_ASSERT|
                   TPPLEXER_RESET_KWDFLAGS|TPPLEXER_RESET_COUNTER|
                   TPPLEXER_RESET_FONCE)) {
  struct TPPKeyword **bucket_iter,**bucket_end,*iter;
  if (flags&TPPLEXER_RESET_COUNTER) self->l_counter = 0;

  /* When any of the per-keyword flags are set, iterate the keyword map. */
  bucket_end = (bucket_iter = self->l_keywords.km_bucketv)+
                              self->l_keywords.km_bucketc;
  for (; bucket_iter != bucket_end; ++bucket_iter) {
   for (iter = *bucket_iter; iter; iter = iter->k_next) {
    struct TPPRareKeyword *rare;
    if (iter->k_macro && (flags&TPPLEXER_RESET_MACRO))
        TPPFile_Decref(iter->k_macro),
        iter->k_macro = NULL;
    if ((rare = iter->k_rare) != NULL) {
     struct TPPAssertion **ass_iter,**ass_end,*aiter,*anext;
     if (rare->kr_file && (flags&TPPLEXER_RESET_FONCE) &&
         rare->kr_file->f_kind == TPPFILE_KIND_TEXT) {
      /* Delete include guard names. */
      rare->kr_file->f_textfile.f_guard    = NULL;
      rare->kr_file->f_textfile.f_newguard = NULL;
      rare->kr_file->f_textfile.f_flags    = TPP_TEXTFILE_FLAG_NONE;
     }
     if (flags&TPPLEXER_RESET_MACRO) {
      struct TPPFile *fileiter,*filenext;
      assert(!iter->k_macro);
      fileiter = rare->kr_oldmacro;
      rare->kr_oldmacro = NULL;
      while (fileiter) {
       assert(fileiter->f_kind == TPPFILE_KIND_MACRO);
       filenext = fileiter->f_macro.m_pushprev;
       TPPFile_Decref(fileiter);
       fileiter = filenext;
      }
      if (rare->kr_defmacro) {
       if (flags&TPPLEXER_RESET_NORESTOREMACROS) {
        TPPFile_Decref(rare->kr_defmacro);
        rare->kr_defmacro = NULL;
        rare->kr_flags   &= ~(TPP_KEYWORDFLAG_BUILTINMACRO);
       } else {
        iter->k_macro = rare->kr_defmacro;
        TPPFile_Incref(iter->k_macro);
       }
      }
     }
     assert((rare->kr_asserts.as_assv != NULL) ==
            (rare->kr_asserts.as_assa != 0));
     if (flags&TPPLEXER_RESET_ASSERT) {
      ass_end = (ass_iter = rare->kr_asserts.as_assv)+
                            rare->kr_asserts.as_assa;
      for (; ass_iter != ass_end; ++ass_iter) {
       aiter = *ass_iter;
       while (aiter) {
        anext = aiter->as_next;
        assert(aiter->as_kwd);
        free(aiter);
        aiter = anext;
       }
      }
      rare->kr_asserts.as_assc = 0;
     }
     if (flags&TPPLEXER_RESET_KWDFLAGS)
         rare->kr_flags = TPP_KEYWORDFLAG_NONE;
     if (flags&TPPLEXER_RESET_COUNTER)
         rare->kr_counter = 0;
     /* Delete the rare entry if it is empty. */
     if (!rare->kr_file &&
         !rare->kr_oldmacro &&
         !rare->kr_defmacro &&
         !rare->kr_counter &&
         !rare->kr_flags &&
         !rare->kr_asserts.as_assc) {
      free(rare->kr_asserts.as_assv);
      rare->kr_asserts.as_assa = 0;
      rare->kr_asserts.as_assv = NULL;
      free(rare);
      iter->k_rare = NULL;
     }
    }
   }
  }
 }
}

struct tpp_extension {
 int         e_flag;
 char const *e_name;
 size_t      e_size;
};
PRIVATE struct tpp_extension const tpp_extensions[] = {
#define EXTENSION(name,str,default)  {name,str,COMPILER_STRLEN(str)},
#include "tpp-defs.inl"
#undef EXTENSION
 {0,NULL,0},
};

#ifdef tolower
#undef tolower
#endif
#define tolower(c) (char)((c) >= 'A' && (c) <= 'Z' ? ((c)+('a'-'A')) : (c))

#if 0
/* Fuzzy match two strings */
PRIVATE size_t fuzzy_match(char const *__restrict a,
                           char const *__restrict b) {
 /* ~sigh~ such a pretty 'rithm lost to exponentiality... */
 char cha,chb; size_t result,temp;
 /* If either string ends, the resulting weight
  * is the remaining length of the other.
  * NOTE: Multiply the punishment by 2, because in theory
  *       for every character taken hereafter, we'd also
  *       need to add another to the other string. */
 if ((cha = *a) == '\0') return strlen(b)*2;
 if ((chb = *b) == '\0') return strlen(a)*2;
 if (tolower(cha) != tolower(chb)) {
  /* Choose the lower weight of comparison when taking
   * away the first character from each string as punishment.
   * A third chance of matching is be taking the character
   * from both strings and adding the punishment for doing so. */
  result = fuzzy_match(a+1,b)+1;
  if (result == 1) return 1;
  temp   = fuzzy_match(a,b+1)+1;
  if (result == 1) return 1;
  if (temp < result) result = temp;
  temp   = fuzzy_match(a+1,b+1)+2;
  if (temp < result) result = temp;
 } else {
  /* The characters matched. - Fuzzy-match the rest. */
  result = fuzzy_match(a+1,b+1);
 }
 return result;
}
#endif

/* As found here: https://en.wikipedia.org/wiki/Levenshtein_distance */
PRIVATE size_t
fuzzy_match(char const *__restrict a, size_t alen,
            char const *__restrict b, size_t blen) {
 size_t *v0,*v1,i,j,cost,temp;
 if unlikely(!alen) return blen;
 if unlikely(!blen) return alen;
 v0 = (size_t *)alloca((blen+1)*sizeof(size_t));
 v1 = (size_t *)alloca((blen+1)*sizeof(size_t));
 for (i = 0; i < blen; ++i) v0[i] = i;
 for (i = 0; i < alen; ++i) {
  v1[0] = i+1;
  for (j = 0; j < blen; j++) {
   cost  = (tolower(a[i]) == tolower(b[j])) ? 0u : 1u;
   cost += v0[j];
   temp  = v1[j]+1;
   if (temp < cost) cost = temp;
   temp  = v0[j+1]+1;
   if (temp < cost) cost = temp;
   v1[j+1] = cost;
  }
  memcpy(v0,v1,blen*sizeof(size_t));
 }
 return v1[blen];
}

PRIVATE char const *
find_most_likely_warning(char const *__restrict name) {
 char const *const *iter,*result = NULL;
 size_t name_len = strlen(name),new_weight,result_weight = (size_t)-1;
 for (iter = wgroup_names; *iter; ++iter) {
  new_weight = fuzzy_match(*iter,strlen(*iter),name,name_len);
  /* Select this new extension if it has a lower fuzzy matching value. */
  if (new_weight < result_weight) {
   result = *iter;
   result_weight = new_weight;
  }
 }
 return result;
}

PRIVATE char const *
find_most_likely_extension(char const *__restrict name) {
 struct tpp_extension const *iter,*result = NULL;
 size_t name_len = strlen(name),new_weight,result_weight = (size_t)-1;
 for (iter = tpp_extensions; iter->e_name; ++iter) {
  new_weight = fuzzy_match(iter->e_name,iter->e_size,
                           name,name_len);
  /* Select this new extension if it has a lower fuzzy matching value. */
  if (new_weight < result_weight) {
   result = iter;
   result_weight = new_weight;
  }
 }
 return result->e_name;
}

PUBLIC int
TPPLexer_SetExtension(char const *__restrict name,
                      int enabled) {
 struct tpp_extension const *iter;
 size_t name_size,id;
 assert(TPPLexer_Current);
 assert(name);
 name_size = strlen(name);
 iter = tpp_extensions;
 while (iter->e_name) {
  if (iter->e_size == name_size &&
     !memcmp(iter->e_name,name,name_size*sizeof(char))) {
   id = (size_t)(iter-tpp_extensions);
   /* Found it! */
   if (enabled) TPPLexer_EnableExtension(id);
   else         TPPLexer_DisableExtension(id);
   return 1;
  }
  ++iter;
 }
 return 0;
}
PUBLIC int
TPPLexer_GetExtension(char const *__restrict name) {
 struct tpp_extension const *iter;
 size_t name_size,id;
 assert(TPPLexer_Current);
 assert(name);
 name_size = strlen(name);
 iter = tpp_extensions;
 while (iter->e_name) {
  if (iter->e_size == name_size &&
     !memcmp(iter->e_name,name,name_size*sizeof(char))) {
   id = (size_t)(iter-tpp_extensions);
   /* Found it! */
   return !!HAS(id);
  }
  ++iter;
 }
 /* Unknown extension. */
 return -1;
}


PRIVATE void
rehash_keywords(size_t newsize) {
 struct TPPKeyword **newvec,*iter,*next,**bucket;
 struct TPPKeyword **bucket_iter,**bucket_end;
 assert(newsize);
 assert(CURRENT.l_keywords.km_bucketc);
 assert(CURRENT.l_keywords.km_bucketv);
 assertf(newsize > CURRENT.l_keywords.km_bucketc,
        ("New size %lu isn't greater than old size %lu (%lu entires)",
        (unsigned long)newsize,
        (unsigned long)CURRENT.l_keywords.km_bucketc,
        (unsigned long)CURRENT.l_keywords.km_entryc));
 newvec = (struct TPPKeyword **)calloc(newsize,sizeof(struct TPPKeyword *));
 if unlikely(!newvec) return; /* Ignore errors here. */
 bucket_end = (bucket_iter = CURRENT.l_keywords.km_bucketv)+
                             CURRENT.l_keywords.km_bucketc;
 for (; bucket_iter != bucket_end; ++bucket_iter) {
  iter = *bucket_iter;
  while (iter) {
   /* Transfer all keywords entries into the new bucket vector. */
   next = iter->k_next;
   bucket = &newvec[iter->k_hash % newsize];
   iter->k_next = *bucket;
   *bucket = iter;
   iter = next;
  }
 }
 free(CURRENT.l_keywords.km_bucketv);
 CURRENT.l_keywords.km_bucketc = newsize;
 CURRENT.l_keywords.km_bucketv = newvec;
}

PUBLIC struct TPPKeyword *
TPPLexer_LookupKeyword(char const *name, size_t namelen,
                       int create_missing) {
 hash_t namehash;
 struct TPPKeyword *kwd_entry,**bucket;
 assert(TPPLexer_Current);
 namehash = hashof(name,namelen);
 /* Try to rehash the keyword map. */
 if (TPPKeywordMap_SHOULDHASH(&CURRENT.l_keywords)) {
  assertf(CURRENT.l_keywords.km_entryc > CURRENT.l_keywords.km_bucketc,
         ("New size %lu isn't greater than old size %lu",
         (unsigned long)CURRENT.l_keywords.km_entryc,
         (unsigned long)CURRENT.l_keywords.km_bucketc));
  rehash_keywords(CURRENT.l_keywords.km_entryc);
 }
 assert(CURRENT.l_keywords.km_bucketc);
 assert(CURRENT.l_keywords.km_bucketv);
 bucket = &CURRENT.l_keywords.km_bucketv[namehash %
           CURRENT.l_keywords.km_bucketc];
 kwd_entry = *bucket;
 while (kwd_entry) {
  if (kwd_entry->k_hash == namehash &&
      kwd_entry->k_size == namelen &&
     !memcmp(kwd_entry->k_name,name,namelen*sizeof(char))
      ) return kwd_entry; /* Found it! */
  kwd_entry = kwd_entry->k_next;
 }
 if unlikely(!create_missing) return NULL;
 /* Must allocate a new keyword entry. */
 kwd_entry = (struct TPPKeyword *)malloc(TPP_OFFSETOF(struct TPPKeyword,k_name)+
                                        (namelen+1)*sizeof(char));
 if unlikely(!kwd_entry) return NULL;
 /* Setup the new keyword entry. */
 kwd_entry->k_rare     = NULL;
 kwd_entry->k_macro    = NULL;
 kwd_entry->k_id       = _KWD_BACK+(CURRENT.l_keywords.km_entryc++); /* Unique user-keyword ID. */
 kwd_entry->k_size     = namelen;
 kwd_entry->k_hash     = namehash;
 memcpy(kwd_entry->k_name,name,namelen*sizeof(char));
 kwd_entry->k_name[namelen] = '\0';
 kwd_entry->k_next = *bucket;
 return *bucket = kwd_entry;
}
PUBLIC struct TPPKeyword *
TPPLexer_LookupKeywordID(tok_t id) {
 struct TPPKeyword **bucket_iter,**bucket_end,*iter;
 assert(TPPLexer_Current);
 if (!TPP_ISKEYWORD(id)) return NULL;
#if TPP_CONFIG_ONELEXER
 /* Special case: In one-lexer mode, we can lookup builtin keywords by ID. */
 if (!TPP_ISUSERKEYWORD(id)) return builtin_keywords[id-TOK_KEYWORD_BEGIN];
#endif
 /* The slow way: Check _every_ keyword in the worst case. */
 bucket_end = (bucket_iter = CURRENT.l_keywords.km_bucketv)+
                             CURRENT.l_keywords.km_bucketc;
 for (; bucket_iter != bucket_end; ++bucket_iter) {
  iter = *bucket_iter;
  while (iter) {
   if (iter->k_id == id) return iter;
   iter = iter->k_next;
  }
 }
 return NULL;
}


PRIVATE char *
do_fix_filename(char *filename, size_t *pfilename_size) {
 char *text_iter,*text_end,*slash,*base;
 size_t filename_size;
 filename_size = *pfilename_size;
 /* General filename sanitizations. */
 while (tpp_isspace(*filename) && filename_size) ++filename,--filename_size;
 while (filename_size && tpp_isspace(filename[filename_size-1])) --filename_size;
 filename[filename_size] = '\0'; /* Create a zero-terminated string. */
 text_end = filename+filename_size;
#ifdef ALTSEP
 for (text_iter = filename; text_iter != text_end; ++text_iter) {
  if (*text_iter == ALTSEP) *text_iter = SEP;
 }
#endif
 /* Remove whitespace before & after slashes. */
 for (text_iter = filename; text_iter != text_end;) {
  assertf(text_iter < text_end,("text_iter = %p\ntext_end = %p",text_iter,text_end));
  if (*text_iter == SEP) {
   while (text_iter != filename && tpp_isspace(text_iter[-1])) {
    memmove(text_iter-1,text_iter, /* NOTE: This also moves the '\0'-terminator. */
           (size_t)((text_end-text_iter)+1)*sizeof(char));
    --text_iter,--text_end;
   }
   while (text_end != text_iter && tpp_isspace(text_iter[1])) {
    memmove(text_iter+1,text_iter+2, /* NOTE: This also moves the '\0'-terminator. */
           (size_t)((text_end-text_iter)-1)*sizeof(char));
    --text_end;
   }
   if (text_end == text_iter) break;
  }
  ++text_iter;
 }
 /* Remove double-slashes. */
 for (text_iter = filename; text_iter != text_end;) {
  if (*text_iter == SEP && text_iter[1] == SEP) {
   memmove(text_iter,text_iter+1, /* NOTE: This also moves the '\0'-terminator. */
          (text_end-text_iter)*sizeof(char));
   --text_end;
  } else {
   ++text_iter;
  }
 }

 /* Resolve . and .. references if possible. */
 for (text_iter = base = filename;;) {
  size_t partsize;
  slash = strchr(text_iter,SEP);
  if (!slash) slash = text_end;
  partsize = (size_t)(slash-text_iter);
  if (partsize >= 1 && *text_iter == '.') switch (partsize) {
   case 2: if (text_iter[1] == '.' && (text_iter-2) > base) {
    char *prev_folder_start;
    /* Try to remove a '..' path reference. */
    prev_folder_start = (char *)memrchr(base,SEP,(size_t)((text_iter-2)-base));
    if (!prev_folder_start) { base = slash+1; break; }
    /* Move everything from 'slash' to 'prev_folder_start'. */
    if (text_end == slash) *prev_folder_start = '\0';
    else memmove(prev_folder_start,slash,(size_t)((text_end-slash)+1)*sizeof(char));
    text_end -= (size_t)(slash-prev_folder_start);
    slash     = prev_folder_start;
   } break;
   case 1: {
    /* Remove a '.' path reference. */
    if (text_iter == filename) {
     filename = slash; /* Trim at start-of-file. */
     if (slash != text_end) ++filename;
    } else if (slash == text_end) {
     text_iter[-1] = '\0';
    } else {
     memmove(text_iter,slash+1, /* NOTE: This also moves the '\0'-terminator. */
            (size_t)(text_end-slash)*sizeof(char));
    }
   } break;
   default: break;
  }
  if (slash == text_end) break;
  text_iter = slash+1;
  assert(text_iter <= text_end);
 }

 *pfilename_size = (size_t)(text_end-filename);
 return filename;
}

PRIVATE char *
fix_filename(char *filename, size_t *pfilename_size) {
 char *result = do_fix_filename(filename,pfilename_size);
 if (result != filename) {
  /* Move the filename text back to where it came from. */
  memmove(filename,result,(*pfilename_size+1)*sizeof(char));
  result = filename;
 }
 return result;
}

PUBLIC int TPPLexer_PushInclude(void) {
 struct TPPIncludeList *oldstate;
 struct TPPString **iter,**end;
 assert(TPPLexer_Current);
 oldstate = (struct TPPIncludeList *)malloc(sizeof(struct TPPIncludeList));
 if unlikely(!oldstate) return 0;
 memcpy(oldstate,&CURRENT.l_syspaths,sizeof(struct TPPIncludeList));
 /* Allocate memory for a copy of the #include-vector. */
 CURRENT.l_syspaths.il_pathv = (struct TPPString **)malloc(oldstate->il_pathc*
                                                           sizeof(struct TPPString *));
 if unlikely(!CURRENT.l_syspaths.il_pathv) goto err_oldstate;
 memcpy(CURRENT.l_syspaths.il_pathv,oldstate->il_pathv,
        oldstate->il_pathc*sizeof(struct TPPString *));
 /* Generate references to the #include-paths for our vector copy. */
 end = (iter = oldstate->il_pathv)+oldstate->il_pathc;
 for (; iter != end; ++iter) TPPString_Incref(*iter);
 CURRENT.l_syspaths.il_prev = oldstate; /*< Inherit pointer. */
 return 1;
err_oldstate:
 memcpy(&CURRENT.l_syspaths,oldstate,sizeof(struct TPPIncludeList));
 free(oldstate);
 return 0;
}
PUBLIC int TPPLexer_PopInclude(void) {
 struct TPPIncludeList *oldstate;
 struct TPPString **iter,**end;
 assert(TPPLexer_Current);
 oldstate = CURRENT.l_syspaths.il_prev;
 if unlikely(!oldstate) return 0;
 end = (iter = CURRENT.l_syspaths.il_pathv)+
               CURRENT.l_syspaths.il_pathc;
 /* Cleanup include paths we're about to drop. */
 for (; iter != end; ++iter) TPPString_Decref(*iter);
 free(CURRENT.l_syspaths.il_pathv);
 /* Restore the data from the old state. */
 memcpy(&CURRENT.l_syspaths,oldstate,sizeof(struct TPPIncludeList));
 free(oldstate);
 return 1;
}

PUBLIC int
TPPLexer_AddIncludePath(char *__restrict path, size_t pathsize) {
 struct TPPString **iter,**end,*elem;
 assert(TPPLexer_Current);
 /* Normalize & fix the given path as best as possible. */
 if (HAVE_EXTENSION_CANONICAL_HEADERS) fix_filename(path,&pathsize);
 /* Handle special case: empty path & remove trailing slashes. */
 while (pathsize && path[-1] == SEP) --pathsize;
 if unlikely(!pathsize) path = (char *)".",pathsize = 1;
 /* Make sure that the path doesn't already exists. */
 end = (iter = CURRENT.l_syspaths.il_pathv)+
               CURRENT.l_syspaths.il_pathc;
 for (; iter != end; ++iter) {
  if ((elem = *iter)->s_size == pathsize &&
     !memcmp(elem->s_text,path,pathsize*sizeof(char))
     ) return 2; /* Path already exists. */
 }
 elem = TPPString_New(path,pathsize);
 if unlikely(!elem) return 0;
 iter = (struct TPPString **)realloc(CURRENT.l_syspaths.il_pathv,
                                    (CURRENT.l_syspaths.il_pathc+1)*
                                     sizeof(struct TPPString *));
 if unlikely(!iter) { TPPString_Decref(elem); return 0; }
 CURRENT.l_syspaths.il_pathv = iter;
 iter[CURRENT.l_syspaths.il_pathc++] = elem; /*< Inherit reference. */
 return 1;
}
PUBLIC int
TPPLexer_DelIncludePath(char *__restrict path, size_t pathsize) {
 struct TPPString **iter,**end,*elem;
 assert(TPPLexer_Current);
 if (HAVE_EXTENSION_CANONICAL_HEADERS) fix_filename(path,&pathsize);
 while (pathsize && path[-1] == SEP) --pathsize;
 if unlikely(!pathsize) path = (char *)".",pathsize = 1;
 /* Make sure that the path doesn't already exists. */
 end = (iter = CURRENT.l_syspaths.il_pathv)+
               CURRENT.l_syspaths.il_pathc;
 for (; iter != end; ++iter) {
  if ((elem = *iter)->s_size == pathsize &&
     !memcmp(elem->s_text,path,pathsize*sizeof(char))) {
   /* Found it! */
   memmove(iter,iter+1,((end-iter)-1)*sizeof(struct TPPString *));
   --CURRENT.l_syspaths.il_pathc;
   TPPString_Decref(elem);
   return 1;
  }
 }
 return 0;
}


PUBLIC int
TPPLexer_Define(char const *__restrict name, size_t name_size,
                char const *__restrict value, size_t value_size,
                uint32_t flags) {
 struct TPPKeyword *keyword;
 struct TPPString *value_string;
 struct TPPFile *macro_file,*oldfile;
 assert(TPPLexer_Current);
 assert(name);
 assert(value);
 /* Create the text string. */
 value_string = TPPString_New(value,value_size);
 if unlikely(!value_string) return 0;
 /* Lookup the keyword associated with 'name'. */
 keyword = TPPLexer_LookupKeyword(name,name_size,1);
 if unlikely(!keyword) goto err_value_string;
 /* Create a macro file inheriting the string. */
 macro_file = (struct TPPFile *)malloc(TPPFILE_SIZEOF_MACRO_KEYWORD);
 if unlikely(!macro_file) goto err_value_string;
 macro_file->f_refcnt                 = 1;
 macro_file->f_kind                   = TPPFILE_KIND_MACRO;
 macro_file->f_prev                   = NULL;
 macro_file->f_name                   = keyword->k_name;
 macro_file->f_namesize               = keyword->k_size;
 macro_file->f_namehash               = keyword->k_hash;
 macro_file->f_text                   = value_string; /*< Inherit reference. */
 macro_file->f_begin                  = value_string->s_text;
 macro_file->f_end                    = value_string->s_text+value_string->s_size;
 macro_file->f_pos                    = value_string->s_text;
 macro_file->f_macro.m_flags          = TPP_MACROFILE_KIND_KEYWORD;
 macro_file->f_macro.m_deffile        = NULL;
 macro_file->f_macro.m_defloc.lc_line = 0;
 macro_file->f_macro.m_defloc.lc_col  = 0;
 macro_file->f_macro.m_pushprev       = NULL;
 macro_file->f_macro.m_pushcount      = 0;
 oldfile          = keyword->k_macro; /*< Inherit reference. */
 keyword->k_macro = macro_file;       /*< Inherit reference. */
 /* Define keyword flags. */
 if (flags && TPPKeyword_MAKERARE(keyword)) {
  keyword->k_rare->kr_flags |= flags;
  if (flags&TPPLEXER_DEFINE_FLAG_BUILTIN) {
   struct TPPFile *olddef = keyword->k_rare->kr_defmacro;
   keyword->k_rare->kr_defmacro = macro_file;
   TPPFile_Incref(macro_file);
   if (olddef) TPPFile_Decref(olddef);
  }
 }
 if (oldfile) TPPFile_Decref(oldfile);
 return oldfile ? 2 : 1;
err_value_string: TPPString_Decref(value_string); return 0;
}
PRIVATE void
TPPKeyword_Undef(struct TPPKeyword *__restrict self) {
 assert(self);
 assert(self->k_macro);
 if (self->k_rare &&
    (self->k_rare->kr_flags&TPP_KEYWORDFLAG_BUILTINMACRO) &&
    !self->k_rare->kr_defmacro) {
  /* Backup the original definition of a builtin macro. */
  self->k_rare->kr_defmacro = self->k_macro;
 } else {
  TPPFile_Decref(self->k_macro);
 }
 self->k_macro = NULL;
}

PUBLIC int
TPPLexer_Undef(char const *__restrict name, size_t name_size) {
 struct TPPKeyword *keyword;
 assert(TPPLexer_Current);
 assert(name);
 /* Lookup the keyword associated with 'name'. */
 keyword = TPPLexer_LookupKeyword(name,name_size,0);
 if (!keyword || !keyword->k_macro) return 0;
 TPPKeyword_Undef(keyword);
 return 1;
}

PUBLIC int
TPPLexer_AddAssert(char const *__restrict predicate, size_t predicate_size,
                   char const *__restrict answer, size_t answer_size) {
 struct TPPKeyword *predicate_kwd,*answer_kwd;
 predicate_kwd = TPPLexer_LookupKeyword(predicate,predicate_size,1);
 answer_kwd = TPPLexer_LookupKeyword(answer,answer_size,1);
 if unlikely(!predicate_kwd || !answer_kwd) return 0;
 return keyword_addassert(predicate_kwd,answer_kwd);
}
PUBLIC int
TPPLexer_DelAssert(char const *__restrict predicate, size_t predicate_size,
                   char const *answer, size_t answer_size) {
 struct TPPKeyword *predicate_kwd,*answer_kwd;
 predicate_kwd = TPPLexer_LookupKeyword(predicate,predicate_size,0);
 if unlikely(!predicate_kwd) return 0;
 if unlikely(!answer) return keyword_clrassert(predicate_kwd);
 answer_kwd = TPPLexer_LookupKeyword(answer,answer_size,0);
 if unlikely(!answer_kwd) return 0;
 return keyword_delassert(predicate_kwd,answer_kwd);
}


#ifdef HAVE_INSENSITIVE_PATHS
PRIVATE int
check_path_spelling(char *filename, size_t filename_size) {
 char *part_begin,*next_sep,*end,backup,*temp; int error = 1;
 WIN32_FIND_DATAA filename_data; HANDLE find_handle;
 assert(filename);
 assert(filename_size);
 assert(!filename[filename_size]);
#if SEP != '\\'
 while ((temp = strchr(filename,SEP)) != NULL) *temp = '\\';
#endif
 end = filename+filename_size;
 part_begin = filename;
 for (;;) {
  next_sep = strchr(part_begin,'\\');
  if (!next_sep) next_sep = end;
  backup = *next_sep,*next_sep = '\0';
  /* Skip current/parent directory references. */
  if (part_begin[0] == '.' && (!part_begin[1] ||
     (part_begin[1] == '.' && !part_begin[2]))) goto next_part;
  find_handle = FindFirstFileA(filename,&filename_data);
  if (find_handle && find_handle != INVALID_HANDLE_VALUE) {
   if (strcmp(filename_data.cFileName,part_begin)) {
    *next_sep = backup; /* This part of the path is cased differently. */
    error = WARN(W_NONPARTABLE_FILENAME_CASING,filename,part_begin,
                         (size_t)(next_sep-part_begin),filename_data.cFileName);
   } else {
    error = 1;
   }
   FindClose(find_handle);
  }
next_part:
  *next_sep = backup;
  if unlikely(!error) break;
  if (next_sep == end) break;
  part_begin = next_sep+1;
 }
#if SEP != '\\'
 while ((temp = strchr(filename,'\\')) != NULL) *temp = SEP;
#endif
 return error;
}
#endif


PRIVATE struct TPPFile *
open_normal_file(char *filename, size_t filename_size,
                 struct TPPKeyword **pkeyword_entry,
                 int is_system_header) {
 struct TPPFile *result; struct TPPKeyword *kwd_entry;
 /* Find and cache a regular, old file. */
 kwd_entry = TPPLexer_LookupKeyword(filename,filename_size,0);
 if (kwd_entry) {
  if (kwd_entry->k_rare &&
     (result = kwd_entry->k_rare->kr_file) != NULL) goto end;
  result = TPPFile_Open(filename);
  if unlikely(!result) return NULL;
 } else {
  result = TPPFile_Open(filename);
  if unlikely(!result) return NULL;
  kwd_entry = TPPLexer_LookupKeyword(filename,filename_size,1);
  if unlikely(!kwd_entry) goto err_r;
 }
 if (!TPPKeyword_MAKERARE(kwd_entry)) goto err_r;
 assert(!kwd_entry->k_rare->kr_file);
 kwd_entry->k_rare->kr_file = result; /*< Inherit reference. */
 /* Run the callback to notify of a new text file. */
 if ( CURRENT.l_callbacks.c_new_textfile &&
   !(*CURRENT.l_callbacks.c_new_textfile)(result,is_system_header)) {
  kwd_entry->k_rare->kr_file = NULL;
  goto err_r;
 }
end:
 if (pkeyword_entry) *pkeyword_entry = kwd_entry;
 return result;
err_r: TPPFile_Decref(result);
 return NULL;
}

struct TPPFile *
TPPLexer_OpenFile(int mode, char *filename, size_t filename_size,
                  struct TPPKeyword **pkeyword_entry) {
 struct TPPFile *result;
 char *buffer = NULL,*newbuffer,*mfilename = NULL;
 int checked_empty_path = 0;
 size_t buffersize,newbuffersize;
 assert(mode != (TPPLEXER_OPENFILE_MODE_NORMAL|TPPLEXER_OPENFILE_FLAG_NEXT));
 /* Fix broken/distorted filenames to prevent ambiguity. */
 if (HAVE_EXTENSION_CANONICAL_HEADERS) {
  if (mode&TPPLEXER_OPENFILE_FLAG_CONSTNAME) {
   /* Create a copy that will be modified below. */
   mfilename = (char *)malloc((filename_size+1)*sizeof(char));
   if unlikely(!mfilename) return NULL;
   memcpy(mfilename,filename,(filename_size+1)*sizeof(char));
   filename = mfilename;
  }
  fix_filename(filename,&filename_size);
 }
 if ((mode&3) == TPPLEXER_OPENFILE_MODE_NORMAL) {
  result = open_normal_file(filename,filename_size,pkeyword_entry,0);
  if unlikely(!result) goto check_unknown_file;
#ifdef HAVE_INSENSITIVE_PATHS
  if (!(mode&TPPLEXER_OPENFILE_FLAG_NOCASEWARN) &&
      !check_path_spelling(filename,filename_size)
       ) {err_r: TPPFile_Decref(result); result = NULL; }
#endif
  goto end;
 }
 buffer = NULL,buffersize = 0;
 if (mode&TPPLEXER_OPENFILE_MODE_RELATIVE) {
  struct TPPFile *iter = TOKEN.t_file;
  /* Relative #include-mode (Scan for files relative to
   * the paths of all text files on the #include stack). */
  while ((assert(iter),iter != &TPPFile_Empty)) {
   size_t pathsize; char *path_end,*path,*used_filename;
   if (iter->f_kind != TPPFILE_KIND_TEXT) goto nextfile;
   path = iter->f_name;
   path_end = (char *)memrchr(path,'/',iter->f_namesize);
   if (!path_end) {
    /* Special case: Empty path (aka. current directory) */
    if (checked_empty_path) goto nextfile;
    checked_empty_path = 1;
    used_filename = filename;
    newbuffersize = filename_size;
   } else {
    pathsize = (size_t)(path_end-path)+1;
    /* Check if the last file we've checked had this _exact_ same path. */
    if (buffer && pathsize == buffersize-filename_size &&
       !memcmp(path,buffer,pathsize*sizeof(char))) goto nextfile;
    /* Make sure our name buffer is large enough. */
    newbuffersize = pathsize+filename_size;
    if (newbuffersize > buffersize) {
     /* Increase the buffer's size. */
     newbuffer = (char *)realloc(buffer,(newbuffersize+1)*sizeof(char));
     if unlikely(!newbuffer) goto err_buffer;
     buffer = newbuffer,buffersize = newbuffersize;
    }
    buffer[newbuffersize] = '\0';
    memcpy(buffer,path,pathsize*sizeof(char));
    memcpy(buffer+pathsize,filename,filename_size*sizeof(char));
    used_filename = buffer;
   }
   /* TODO: The is-system-header flag must depend on the
    *       path of the file we're using as basis. */
   result = open_normal_file(used_filename,newbuffersize,
                             pkeyword_entry,0);
   if (result) { /* Goti! */
#ifdef HAVE_INSENSITIVE_PATHS
    /* TODO: No need to check inherited path portion:
     *  tpp src/file.c
     *  >> #include "foo/bar.c"
     *  src/foo/bar.c
     *      [-------] Only need to check this portion.
     */
    if (!(mode&TPPLEXER_OPENFILE_FLAG_NOCASEWARN) &&
        !check_path_spelling(filename,filename_size)
        ) {err_buffer_r: free(buffer); goto err_r; }
#endif
    /* When running in include_next-mode, make sure
     * that the file isn't already being included. */
    if (!(mode&TPPLEXER_OPENFILE_FLAG_NEXT) ||
        !(result->f_prev)) goto end_buffer;
   }
nextfile:
   iter = iter->f_prev;
  }
 }
 /* Search system folders for this file. */
 {
  struct TPPString **iter,**begin,*elem;
  iter = (begin = CURRENT.l_syspaths.il_pathv)+
                  CURRENT.l_syspaths.il_pathc;
next_syspath:
  while (iter-- != begin) {
   elem = *iter;
   if (elem->s_size == 1 && elem->s_text[0] == '.') {
    /* Special case: CWD path. */
    if (checked_empty_path) goto next_syspath;
    checked_empty_path = 1;
    result = open_normal_file(filename,filename_size,pkeyword_entry,1);
   } else {
    newbuffersize = (elem->s_size+filename_size+1);
    if (newbuffersize > buffersize) {
     /* Increase the buffer's size. */
     newbuffer = (char *)realloc(buffer,(newbuffersize+1)*sizeof(char));
     if unlikely(!newbuffer) goto err_buffer;
     buffer = newbuffer,buffersize = newbuffersize;
    }
    memcpy(buffer,elem->s_text,elem->s_size*sizeof(char));
    buffer[elem->s_size] = '/';
    memcpy(buffer+elem->s_size+1,filename,filename_size*sizeof(char));
    buffer[newbuffersize] = '\0';
    result = open_normal_file(buffer,newbuffersize,pkeyword_entry,1);
   }
   if (result) { /* Got one! */
    /* When running in include_next-mode, make sure
     * that the file isn't already being included. */
    assert(result->f_kind == TPPFILE_KIND_TEXT);
    if (!(mode&TPPLEXER_OPENFILE_FLAG_NEXT) ||
        /* Make sure that the file isn't already included in include_next-mode.
         *  - When the file is already on the #include-stack, its 'f_prev' field will be set.
         *  - When the file doesn't reference another cached file, it is a cached file itself.
         *    In this case, also make sure that no other file referencing 'result' as cache
         *    is currently being included by making sure that 'f_cacheinc' is ZERO(0).
         */
        (!result->f_prev && (result->f_textfile.f_cacheentry ||
                            !result->f_textfile.f_cacheinc))) {
#ifdef HAVE_INSENSITIVE_PATHS
     /* TODO: No need to check the syspath portion:
      *  tpp -I/usr/include foo.c
      *  >> #include <stdlib.h>
      *  /usr/includes/stdlib.h
      *                [------] Only need to check this
      */
     if (!(mode&TPPLEXER_OPENFILE_FLAG_NOCASEWARN) &&
         !check_path_spelling(filename,filename_size)
         ) goto err_buffer_r;
#endif
     goto end_buffer;
    }
   }
  }
 }
check_unknown_file:
 result = NULL;
 if (!(mode&TPPLEXER_OPENFILE_FLAG_NOCALLBACK) &&
      (CURRENT.l_callbacks.c_unknown_file)) {
  /* Invoke the unknown-file callback. */
  result = (*CURRENT.l_callbacks.c_unknown_file)(filename,filename_size);
 }
end_buffer: free(buffer);
end: free(mfilename);
 return result;
err_buffer: result = NULL; goto end_buffer;
}



PRIVATE struct TPPKeyword *
lookup_escaped_keyword(char const *name, size_t namelen,
                       size_t unescaped_size, int create_missing) {
#define STACKBUF_SIZE  256
 char stack_buf[STACKBUF_SIZE]; /* Use a stack-allocated buffer for smaller sizes. */
 char *buf; struct TPPKeyword *result;
 if (unescaped_size <= STACKBUF_SIZE) {
  buf = stack_buf;
 } else {
  buf = (char *)malloc(unescaped_size*sizeof(char));
  if unlikely(!buf) return NULL;
 }
 wraplf_memcpy(buf,name,namelen);
 result = TPPLexer_LookupKeyword(buf,unescaped_size,create_missing);
 if (buf != stack_buf) free(buf);
 return result;
}
PUBLIC struct TPPKeyword *
TPPLexer_LookupEscapedKeyword(char const *name, size_t namelen,
                              int create_missing) {
 struct TPPKeyword *result;
 size_t namelen_real = wraplf_memlen(name,namelen);
 if (namelen == namelen_real) {
  result = TPPLexer_LookupKeyword(name,namelen,create_missing);
 } else {
  result = lookup_escaped_keyword(name,namelen,namelen_real,create_missing);
 }
 return result;
}


PUBLIC uint32_t
TPPKeyword_Getflags(struct TPPKeyword const *__restrict self) {
 uint32_t result;
 assert(self);
 assert(TPPLexer_Current);
again:
 switch (self->k_id) {
  /* Lookup dynamic flags. */
#ifndef __INTELLISENSE__
#define KWD_FLAGS(name,flags)  case name: result = (uint32_t)(flags); break;
#include "tpp-defs.inl"
#undef KWD_FLAGS
#endif
  default:
   if (self->k_rare && self->k_rare->kr_flags)
    result = self->k_rare->kr_flags;
   else {
    char const *begin,*end,*real_end;
    /* Check for an alias with less underscores. */
    real_end = end = (begin = self->k_name)+self->k_size;
    while (begin != end && *begin == '_') ++begin;
    while (end != begin && end[-1] == '_') --end;
    result = TPP_KEYWORDFLAG_NONE;
    if (begin != self->k_name || end != real_end) {
     /* Was able to remove some leading/terminating underscores.
      * >> Check for an alternative keyword with them removed. */
     self = TPPLexer_LookupKeyword(begin,(size_t)(end-begin),0);
     if (self && (!self->k_rare ||
         /* Don't allow alias if the no-underscores flag is set. */
       !(self->k_rare->kr_flags&TPP_KEYWORDFLAG_NO_UNDERSCORES)
         )) goto again;
    }
   }
   break;
 }
 return result;
}




#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701 4703)
#endif

PUBLIC tok_t TPPLexer_YieldRaw(void) {
 struct TPPFile *file,*prev_file;
 char *iter,*end,*forward; tok_t ch;
 assert(TPPLexer_Current);
 /* Check for special lexer state. */
 if (CURRENT.l_flags&(TPPLEXER_FLAG_ERROR|TPPLEXER_FLAG_EOF_ON_PAREN)) {
  /* Refuse parsing more data if an error occurred. */
  if (CURRENT.l_flags&TPPLEXER_FLAG_ERROR) return TOK_ERR;
  assert(CURRENT.l_flags&TPPLEXER_FLAG_EOF_ON_PAREN);
  /* Refuse to return anything other than EOF when eof-on-paren is
   * turned on an the parenthesis recursion has dropped to ZERO(0). */
  if (!CURRENT.l_eof_paren) { return TOK = TOK_EOF; }
 }
 file = TOKEN.t_file;
again:
 assert(file);
 iter = file->f_pos,end = file->f_end;
 /* Skip some leading wrapped linefeeds. */
startover_iter:
 assert(!*end);
 assert(iter <= end);
 while (SKIP_WRAPLF(iter,end));
 TOKEN.t_begin = iter;
 assert(iter <= end);
 /* Start reading data. */
 ch = (unsigned char)*iter++;
 if (tpp_ismultichar(ch)) {
parse_multichar:
  /* Common code for multi-char sequences such as '<<' or '!=' */
  forward = iter;
  while (SKIP_WRAPLF(forward,end));
 }
 assert(end == file->f_end);
 switch (ch) {

  case '\0':
   /* Sporadic \0-character (interpret as whitespace). */
   if unlikely(iter < end) goto whitespace;
   if unlikely(iter > end) iter = end;
   /* Don't allow seek if the EOB flag is set or
    * the EOB file matches the current one. */
   if ((CURRENT.l_flags&TPPLEXER_FLAG_NO_SEEK_ON_EOB) || 
        CURRENT.l_eob_file == file) { eof: TOKEN.t_begin = iter; goto settok; }
   /* Check for more data chunks within the current file. */
   file->f_pos = iter;
   if (TPPFile_NextChunk(file,TPPFILE_NEXTCHUNK_FLAG_NONE)) goto again;
   iter = file->f_pos,end = file->f_end;
   if ((CURRENT.l_flags&TPPLEXER_FLAG_NO_POP_ON_EOF) ||
        CURRENT.l_eof_file == file) goto eof;
   /* EOF (Check the include stack for more files) */
   if ((prev_file = file->f_prev) == NULL) {
    if (file == &TPPFile_Empty) goto eof;
    /* Use the empty placeholder file. */
    TPPFile_Incref(&TPPFile_Empty);
    prev_file = &TPPFile_Empty;
   }
   /* Switch to the previous file. */
   file->f_prev = NULL;
   on_popfile(file);
   TPPFile_Decref(file);
   TOKEN.t_file = file = prev_file;
   goto again; /* Load the file again. */

  case '\r':
   if (*iter == '\n') ++iter;
   ch = '\n';
  case '\n':
   /* Linefeed token. */
   if (!(CURRENT.l_flags&TPPLEXER_FLAG_WANTLF)) goto startover_iter;
   goto settok;

  case '\'':
  case '\"':
   /* Scan forward until the end of the string/character. */
   while (iter != end && *iter != (char)ch) {
    while (SKIP_WRAPLF(iter,end));
    if ((CURRENT.l_flags&TPPLEXER_FLAG_TERMINATE_STRING_LF) && tpp_islforzero(*iter)) {
     if unlikely(!WARN(W_STRING_TERMINATED_BY_LINEFEED)) goto err;
     break;
    }
    if (*iter == '\\' && iter != end &&
      !(CURRENT.l_flags&TPPLEXER_FLAG_INCLUDESTRING)) ++iter;
    ++iter;
   }
        if (*iter == (char)ch) ++iter;
   else if (iter == end && unlikely(!WARN(W_STRING_TERMINATED_BY_EOF))) goto err;
   /* Warn amount if multi-char constants if they are not enabled. */
   if (ch == '\'' && !HAVE_EXTENSION_MULTICHAR_CONST) {
    char *char_begin = TOKEN.t_begin+1,*char_end = iter;
    assert(char_end >= char_begin);
    if (char_end != char_begin && char_end[-1] == '\'') --char_end;
    if (TPP_SizeofUnescape(char_begin,(size_t)(char_end-char_begin)) != 1) {
     if unlikely(!WARN(W_MULTICHAR_NOT_ALLOWED,char_begin,
                               (size_t)(char_end-char_begin))) goto err;
    }
   }
   /* The string/char token ID is the startup character. */
   goto settok;

  case '?': /* Check for trigraphs. */
   if (HAVE_FEATURE_TRIGRAPHS &&
      (iter <= end-1 && *iter == '?')) {
    switch (iter[1]) {
     case '=':  ch = '#';  break; /* ??= */
     case '(':  ch = '[';  break; /* ??( */
     case '/':  ch = '\\'; break; /* ??/ */
     case ')':  ch = ']';  break; /* ??) */
     case '\'': ch = '^';  break; /* ??' */
     case '<':  ch = '{';  break; /* ??< */
     case '!':  ch = '|';  break; /* ??! */
     case '>':  ch = '}';  break; /* ??> */
     case '-':  ch = '~';  break; /* ??- */
     case '?':             break; /* ??? */
     default: goto settok;
    }
    if unlikely(!WARN(W_ENCOUNTERED_TRIGRAPH,iter-1)) goto err;
    iter += 2;
    if (tpp_ismultichar(ch)) goto parse_multichar;
   }
   goto settok;

  /* Multi-char characters (with 'forward' initialized) */
  case '<':
  case '>':
   if (*forward == (char)ch) {
    iter = ++forward;
    while (SKIP_WRAPLF(forward,end));
    /* Check for tri-char: '<<<' / '>>>' */
    if (*forward == ch) {
     if (CURRENT.l_extokens&TPPLEXER_TOKEN_ANGLE3_EQUAL) {
      char *forward2 = forward+1;
      while (SKIP_WRAPLF(forward2,end));
      if (*forward2 == '=') {
       ch = (ch == '<' ? TOK_LANGLE3_EQUAL : TOK_RANGLE3_EQUAL);
       iter = ++forward2;
       goto settok;
      }
     }
     if (CURRENT.l_extokens&TPPLEXER_TOKEN_ANGLE3) {
      ch = (ch == '<' ? TOK_LANGLE3 : TOK_RANGLE3);
      iter = ++forward;
      goto settok;
     }
    }

    /* Check for tri-char: '<<=' / '>>=' */
    if (*forward == '=') ch = (ch == '<' ? TOK_SHL_EQUAL : TOK_SHR_EQUAL),iter = ++forward;
    else ch = (ch == '<' ? TOK_SHL : TOK_SHR);
    goto settok;
   }
   /* Check for lower-greater: '<>' */
   if (*forward == '>' && ch == '<' && (CURRENT.l_extokens&TPPLEXER_TOKEN_LOGT)) { ch = TOK_LOGT; goto settok_forward1; }
   if (*forward == '=') { ch = (ch == '<' ? TOK_LOWER_EQUAL : TOK_GREATER_EQUAL); goto settok_forward1; }
   if (HAVE_FEATURE_DIGRAPHS && ch == '<') {
    if (*forward == '%') { ch = '{'; goto settok_forward1; } /* [<%] --> [{] */
    if (*forward == ':') { ch = '['; goto settok_forward1; } /* [<:] --> [[] */
   }
   goto settok;

  case '=':
   if (*forward == '=') { ch = TOK_EQUAL; goto settok_forward1; }
   if (!(CURRENT.l_extokens&TPPLEXER_TOKEN_EQUALBINOP)) goto settok;
   /*< '=<<', '=>>', '=>>>', '=<<<', '=@' and '=**' */
   switch (*forward) {
   case '+': ch = TOK_ADD_EQUAL; break;
   case '-': ch = TOK_SUB_EQUAL; break;
   case '/': ch = TOK_DIV_EQUAL; break;
   case '%': ch = TOK_MOD_EQUAL; break;
   case '&': ch = TOK_AND_EQUAL; break;
   case '|': ch = TOK_OR_EQUAL; break;
   case '^': ch = TOK_XOR_EQUAL; break;
   case '*':
    if (CURRENT.l_extokens&TPPLEXER_TOKEN_STARSTAR) {
     char *forward2 = forward+1;
     while (SKIP_WRAPLF(forward2,end));
     if (*forward2 == '*') { iter = ++forward2; ch = TOK_POW_EQUAL; goto settok; }
    }
    ch = TOK_MUL_EQUAL;
    break;
   case '@': if (!(CURRENT.l_extokens&TPPLEXER_TOKEN_ATEQUAL)) goto settok; ch = TOK_XOR_EQUAL; break;
   {
    char *forward2;
   case '<': case '>':
    forward2 = forward+1;
    while (SKIP_WRAPLF(forward2,end));
    if (*forward2 == *forward) {
     iter = ++forward2;
     ch = (*forward2 == '<') ? TOK_LANGLE2_EQUAL : TOK_RANGLE2_EQUAL;
     if (CURRENT.l_extokens&TPPLEXER_TOKEN_ANGLE3_EQUAL) {
      /* Check for 3-range inplace tokens. */
      while (SKIP_WRAPLF(forward2,end));
      if (*forward2 == ch) {
       iter = ++forward2;
       ch = (ch == '<') ? TOK_LANGLE3_EQUAL : TOK_RANGLE3_EQUAL;
      }
     }
    }
    goto settok;
   } break;

   default: goto settok;
   }
   goto settok_forward1;
   

  case '!':
   if (*forward == '=') { ch = TOK_NOT_EQUAL; goto settok_forward1; }
   goto settok;

  case '.':
   if (*forward == '*' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_DOTSTAR)) { ch = TOK_DOT_STAR; goto settok_forward1; }
   if (*forward == '.') {
    char *after_second = ++forward;
    while (SKIP_WRAPLF(forward,end));
    if (*forward == '.') {
     ch = TOK_DOTS;
     iter = forward+1;
     goto settok;
    } else if (CURRENT.l_extokens&TPPLEXER_TOKEN_DOTDOT) {
     ch = TOK_DOTDOT;
     iter = after_second;
    }
   }
   goto settok;

  case '…': /* Why not? */
   ch = TOK_DOTS;
   goto settok;

  case '+':
   if (*forward == '=') { ch = TOK_ADD_EQUAL; goto settok_forward1; }
   if (*forward == '+') { ch = TOK_INC; goto settok_forward1; }
   goto settok;

  case '-':
   if (*forward == '=') { ch = TOK_SUB_EQUAL; goto settok_forward1; }
   if (*forward == '-') { ch = TOK_DEC; goto settok_forward1; }
   if (*forward == '>' &&
      (CURRENT.l_extokens&(TPPLEXER_TOKEN_ARROW|TPPLEXER_TOKEN_ARROWSTAR))) {
    char *after_arrow = ++forward;
    if (CURRENT.l_flags&TPPLEXER_TOKEN_ARROWSTAR) {
     while (SKIP_WRAPLF(forward,end));
     if (*forward == '*') {
      iter = forward;
      ch = TOK_ARROW_STAR; /*< "->*". */
      goto settok_forward1;
     }
    }
    if (CURRENT.l_extokens&TPPLEXER_TOKEN_ARROW) {
     ch = TOK_ARROW;
     iter = after_arrow;
    }
   }
   goto settok;

  case '*':
   if (*forward == '*' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_STARSTAR)) { ch = TOK_POW; goto settok_forward1; }
   if (*forward == '=') { ch = TOK_MUL_EQUAL; goto settok_forward1; }
   if (*forward == '/') {
    if (!WARN(W_STARSLASH_OUTSIDE_OF_COMMENT,iter-1)) goto err;
   }
   goto settok;

  case '/':
   if (*forward == '=') { ch = TOK_DIV_EQUAL; goto settok_forward1; }
   if (*forward == '*' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_C_COMMENT)) {
    /* Parse a multi-line-comment. */
    ++forward;
    if (forward == end) ch = 0;
    else for (;;) {
     ch = *forward;
     if (!ch && forward == end) {
      if unlikely(!WARN(W_COMMENT_TERMINATED_BY_EOF)) goto err;
      break;
     }
     ++forward;
     if (ch == '*') {
      while (SKIP_WRAPLF(forward,end));
      if (*forward == '/') break;
     } else if (ch == '/') {
      while (SKIP_WRAPLF(forward,end));
      if (*forward == '*' &&
          !WARN(W_SLASHSTAR_INSIDE_OF_COMMENT,forward)) goto err;
     }
    }
    iter = forward;
    if (ch) ++iter;
    assert(iter <= end);
    if unlikely(!(CURRENT.l_flags&TPPLEXER_FLAG_WANTCOMMENTS)) goto startover_iter;
    goto set_comment;
   }
   if (*forward == '/' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_CPP_COMMENT)) {
    /* Parse a line-comment. */
    do {
     ++forward;
     while (SKIP_WRAPLF(forward,end)) {
      if unlikely(!WARN(W_LINE_COMMENT_CONTINUED)) goto err;
     }
    } while (!tpp_islforzero(*forward));
    if (!(CURRENT.l_flags&TPPLEXER_FLAG_COMMENT_NOOWN_LF) &&
       (*forward && *forward++ == '\r' && *forward == '\n')) ++forward;
    iter = forward;
    if unlikely(!(CURRENT.l_flags&TPPLEXER_FLAG_WANTCOMMENTS)) goto startover_iter;
set_comment:
    ch = TOK_COMMENT;
   }
   goto settok;

  case '%':
   if (*forward == '=') { ch = TOK_MOD_EQUAL; goto settok_forward1; }
   if (HAVE_FEATURE_DIGRAPHS) {
    if (*forward == '>') { ch = '}'; goto settok_forward1; } /* [%>] --> [}] */
    if (*forward == ':') {
     iter = ++forward;
     while (SKIP_WRAPLF(forward,end));
     if (*forward == '#') { glue_tok: /* %:# --> ## */ ch = TOK_GLUE; goto settok; }
     if (*forward == '%') {
      ++forward; /* %:%: --> ## */
      while (SKIP_WRAPLF(forward,end));
      if (*forward == ':') goto glue_tok;
     }
     ch = '#'; /* [%:] --> [#] */
     goto settok;
    }
   }
   goto settok;

  case '&':
  case '|':
  case '^':
   if (*forward == '=') {
    ch = (ch == '&') ? TOK_AND_EQUAL :
         (ch == '|') ? TOK_OR_EQUAL :
                       TOK_XOR_EQUAL;
    goto settok_forward1;
   }
   if (*forward == (char)ch &&
      ((CURRENT.l_extokens&TPPLEXER_TOKEN_ROOFROOF) || ch != '^')) {
    ch = (ch == '&') ? TOK_LAND :
         (ch == '|') ? TOK_LOR :
                       TOK_LXOR;
    goto settok_forward1;
   }
   goto settok;

  case '#':
   if (*forward == '#') { ch = TOK_GLUE; goto settok_forward1; }
   goto settok;

  case '~':
   /* NOTE: '~~' tokens can be disabled. */
   if (*forward == '~' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_TILDETILDE)) {
    ch = TOK_TILDE_TILDE;
    goto settok_forward1;
   }
   goto settok;

  case ':':
   if (*forward == ':' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_COLLONCOLLON)) { ch = TOK_COLLON_COLLON; goto settok_forward1; }
   if (*forward == '=' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_COLLONASSIGN)) { ch = TOK_COLLON_EQUAL; goto settok_forward1; }
   if (HAVE_FEATURE_DIGRAPHS &&
       *forward == '>') { ch = ']'; goto settok_forward1; } /* [:>] --> []] */
   goto settok;

  case '@':
   if (*forward == '=' &&
      (CURRENT.l_extokens&TPPLEXER_TOKEN_ATEQUAL)) { ch = TOK_AT_EQUAL; goto settok_forward1; }
   goto settok;

  case '(':
  case ')':
   if (CURRENT.l_flags&TPPLEXER_FLAG_EOF_ON_PAREN) {
    /* Handle EOF parenthesis recursion. */
    assert(CURRENT.l_eof_paren); /* Already checked above. */
         if (ch == '(') ++CURRENT.l_eof_paren;
    else if (!--CURRENT.l_eof_paren) ch = TOK_EOF;
   }
   goto settok;

  case '$':
   if ((CURRENT.l_extokens&TPPLEXER_TOKEN_DOLLAR) ||
       !HAVE_EXTENSION_DOLLAR_IS_ALPHA) goto settok;
  default:
   if (tpp_isalpha(ch) || (HAVE_EXTENSION_EXTENDED_IDENTS && tpp_isansi(ch))) {
    struct TPPKeyword *kwd_entry;
    size_t name_escapesize,name_size = 1;
    uint8_t chflags = CH_ISALPHA|CH_ISDIGIT;
    /* Set the ANSI flag if we're supporting those characters. */
    if (HAVE_EXTENSION_EXTENDED_IDENTS) chflags |= CH_ISANSI;
    /* keyword: scan until a non-alnum character is found. */
    if (HAVE_EXTENSION_DOLLAR_IS_ALPHA) for (;;) {
     while (SKIP_WRAPLF(iter,end));
     if (!(chrattr[*iter]&chflags)) break;
     ++iter,++name_size;
    } else for (;;) {
     while (SKIP_WRAPLF(iter,end));
     if (!(chrattr[*iter]&chflags) || *iter == '$') break;
     ++iter,++name_size;
    }
    /* Lookup/generate the token id of this keyword. */
    name_escapesize = (size_t)(iter-TOKEN.t_begin);
    if (name_size == name_escapesize) {
     kwd_entry = TPPLexer_LookupKeyword(TOKEN.t_begin,name_size,1);
    } else {
     kwd_entry = lookup_escaped_keyword(TOKEN.t_begin,name_escapesize,name_size,1);
    }
    if unlikely(!kwd_entry) goto seterr;
    ch = kwd_entry->k_id;
    TOKEN.t_id = kwd_entry->k_id;
    TOKEN.t_kwd = kwd_entry;
    if (kwd_entry->k_rare &&
       (kwd_entry->k_rare->kr_flags&TPP_KEYWORDFLAG_IS_DEPRECATED) &&
      !(CURRENT.l_flags&TPPLEXER_FLAG_NO_DEPRECATED)) {
     /* Check 'kwd_entry' for being a keyword marked as '#pragma deprecated'.
      * If it is, emit a warning. */
     TOKEN.t_end = iter;
     if (!WARN(W_DEPRECATED_IDENTIFIER,kwd_entry)) goto err;
    }
    break;
   }
   if (tpp_isdigit(ch)) {
    /* Integer/float. */
    TOKEN.t_id = TOK_INT;
    for (;;) {
     while (SKIP_WRAPLF(iter,end));
continue_int:
     if (!tpp_isalnum(*iter)) {
      switch (*iter) {
       case '.':
        /* Switch to a float-token for the first encountered dot. */
        if (TOKEN.t_id == TOK_FLOAT) goto done;
        TOKEN.t_id = TOK_FLOAT;
        break;
       case 'e':
       case 'E':
        /* Exponent suffix for floating-point.
         * >> Past this, we must allow for an immediate '+' or '-' */
        if (TOKEN.t_id != TOK_FLOAT) goto done;
        ++iter;
        while (SKIP_WRAPLF(iter,end));
        /* If it's not a '+' or '-', parse the character normally. */
        if (*iter != '+' && *iter != '-') goto continue_int;
        /* Skip the '+'/'-' at the '++iter' below. */
        break;
       default: goto done;
      }
     }
     ++iter;
    }
   }
   if (tpp_isspace(ch)) {
whitespace:
    /* Parse space tokens. */
    for (;;) {
     while (SKIP_WRAPLF(iter,end));
     if (!tpp_isspace_nolf(*iter) || iter == end) break;
     ++iter;
    }
    if (!(CURRENT.l_flags&TPPLEXER_FLAG_WANTSPACE)) goto startover_iter;
    ch = TOK_SPACE;
   }
   goto settok;
settok_forward1:
   iter = forward+1;
settok:
   /* default case: Single-character token.
    * NOTE: These all share their ids with the character decimal. */
   TOKEN.t_id = (tok_t)ch;
   break;
 }
 assert(end == file->f_end);
done:
 assert(end == file->f_end);
 assert(iter >= file->f_pos);
 assert(iter <= end);
 file->f_pos = iter;
 TOKEN.t_end = iter;
 ++TOKEN.t_num;
 return TOKEN.t_id;
seterr: TPPLexer_SetErr();
err: ch = (tok_t)TOK_ERR; goto settok;
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

PRIVATE int at_start_of_line(void) {
 char *line_begin,*file_begin;
 /* Check if the current token is at the start of
  * a line and perform a preprocessor directive. */
 file_begin = TOKEN.t_file->f_begin;
 line_begin = TOKEN.t_begin;
 /* NOTE: Due to how chunks are read from files, we can rely
  *       on the fact that nothing until to start of the current
  *       line is part of a previous chunk, meaning that we are
  *       safe to rely on our search here to determine the
  *       at-start-of-line-ity of this token.
  */
 /* STDC allows comments here (So we must ignore slash-star comments)! */
 line_begin = skip_whitespacenolf_and_comments_rev(line_begin,file_begin);
 if (line_begin != file_begin &&
    !tpp_islforzero(line_begin[-1])
     ) return 0; /* Not at the start of a line. */
 return 1;
}

PRIVATE void on_popfile(struct TPPFile *file) {
 struct TPPIfdefStackSlot *slot;
 if (file->f_kind == TPPFILE_KIND_TEXT &&                   /*< Is this a textfile. */
    !file->f_textfile.f_cacheentry &&                       /*< Make sure isn't not a cache-reference. */
     file->f_textfile.f_newguard &&                         /*< Make sure there is a possibility for a guard. */
    !file->f_textfile.f_guard &&                            /*< Make sure the file doesn't already have a guard. */
   !(file->f_textfile.f_flags&TPP_TEXTFILE_FLAG_NOGUARD)) { /*< Make sure the file is allowed to have a guard. */
  /* DEBUG: Log detection of legacy #include-guards. */
  LOG(LOG_LEGACYGUARD,("Determined #include-guard '%s' for '%s'\n",
                       file->f_textfile.f_newguard->k_name,file->f_name));
  /* This entire file is protected by a guard. */
  file->f_textfile.f_guard    = file->f_textfile.f_newguard;
  /* Prevent overhead from future checks. */
  file->f_textfile.f_flags   |= TPP_TEXTFILE_FLAG_NOGUARD;
  file->f_textfile.f_newguard = NULL;
 }
 while (CURRENT.l_ifdef.is_slotc &&
       (slot = &CURRENT.l_ifdef.is_slotv[CURRENT.l_ifdef.is_slotc-1],
        slot->iss_file == file)) {
  WARN(W_IF_WITHOUT_ENDIF,slot);
  --CURRENT.l_ifdef.is_slotc;
 }
}
PUBLIC void TPPLexer_PopFile(void) {
 struct TPPFile *popfile = TOKEN.t_file;
 assert(popfile);
 assert(popfile != CURRENT.l_eob_file);
 assert(popfile != CURRENT.l_eof_file);
 if ((TOKEN.t_file = popfile->f_prev) == NULL) {
  TPPFile_Incref(&TPPFile_Empty);
  TOKEN.t_file = &TPPFile_Empty;
 }
 popfile->f_prev = NULL;
 on_popfile(popfile);
 TPPFile_Decref(popfile);
}

PRIVATE struct TPPIfdefStackSlot *alloc_ifdef(int mode) {
 struct TPPIfdefStackSlot *result;
 result = CURRENT.l_ifdef.is_slotv;
 if (CURRENT.l_ifdef.is_slota == CURRENT.l_ifdef.is_slotc) {
  size_t newalloc = CURRENT.l_ifdef.is_slota ? CURRENT.l_ifdef.is_slota*2 : 2;
  result = (struct TPPIfdefStackSlot *)realloc(result,newalloc*sizeof(struct TPPIfdefStackSlot));
  if unlikely(!result) return NULL;
  CURRENT.l_ifdef.is_slota = newalloc;
  CURRENT.l_ifdef.is_slotv = result;
 }
 result += CURRENT.l_ifdef.is_slotc++;
 result->iss_mode = mode;
 result->iss_file = TOKEN.t_file;
 result->iss_line = TPPLexer_LINE();
 return result;
}

PRIVATE int do_skip_pp_block(void) {
 unsigned int recursion = 1;
 uint32_t oldflags = CURRENT.l_flags;
 for (;;) {
  if (TOK != '#' || !at_start_of_line()) {
   if (TPPLexer_YieldRaw() <= 0) break;
  } else {
   /* NOTE: Must enable string-terminate-on-lf until the end of this line.
    *    >> Otherwise, we might get incorrect syntax because this line may
    *       contain an incomplete string ("#error a'b" is complete)
    */
   CURRENT.l_flags = oldflags|(TPPLEXER_FLAG_TERMINATE_STRING_LF|
                               TPPLEXER_FLAG_WANTLF|
                               TPPLEXER_FLAG_COMMENT_NOOWN_LF);
   TPPLexer_YieldRaw();
   switch (TOK) {
    case KWD_ifdef: case KWD_ifndef: case KWD_if: ++recursion; break;
    case KWD_endif: if (!--recursion) return 1; break;
    case KWD_elif: case KWD_else: if (recursion == 1) return 1; break;
    default: break;
   }
   /* Scan until the end of the line. */
   while (TOK > 0 && TOK != '\n') TPPLexer_YieldRaw();
   CURRENT.l_flags = oldflags;
   TPPLexer_YieldRaw();
  }
 }
 return 0;
}
PRIVATE int skip_pp_block(void) {
 int result;
 pushf();
 /* Disable unnecessary tokens. */
 CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTLF|
                      TPPLEXER_FLAG_WANTSPACE|
                      TPPLEXER_FLAG_WANTCOMMENTS);
 /* Warnings don't need to be emit. */
 CURRENT.l_flags |= TPPLEXER_FLAG_NO_WARNINGS;
 result = do_skip_pp_block();
 popf();
 return result;
}

PRIVATE int parse_include_string(char **begin, char **end) {
 int result = 1;
 pushf();
 CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTLF|
                      TPPLEXER_FLAG_WANTSPACE|
                      TPPLEXER_FLAG_WANTCOMMENTS);
 CURRENT.l_flags |= TPPLEXER_FLAG_INCLUDESTRING;
 TPPLexer_Yield();
 if (TOK == '\"') {
  /* Relative #include-string. */
  *begin = TOKEN.t_begin;
  *end = TOKEN.t_end;
  if ((*end)[-1] == '\"') --*end;
 } else if (TOK == '<') {
  /* System #include-string. */
  *begin = TOKEN.t_begin;
  *end = (char *)memchr(TOKEN.t_end,'>',
                       (size_t)(TOKEN.t_file->f_end-TOKEN.t_end));
  if (!*end) *end = TOKEN.t_end;
  else {
   /* Continue parsing _AFTER_ the '>' character! */
   TOKEN.t_file->f_pos = (*end)+1;
  }
 } else {
  WARN(W_EXPECTED_INCLUDE_STRING);
  TOKEN.t_file->f_pos = TOKEN.t_begin;
  result = 0;
 }
 assertf(!result || *begin <= *end,
        ("result                = %d\n"
         "*begin                = %p\n"
         "*end                  = %p\n"
         "TOKEN.t_begin         = %p\n"
         "TOKEN.t_end           = %p\n"
         "TOKEN.t_file->f_begin = %p\n"
         "TOKEN.t_file->f_pos   = %p\n"
         "TOKEN.t_file->f_end   = %p\n"
        ,result,*begin,*end
        ,TOKEN.t_begin
        ,TOKEN.t_end
        ,TOKEN.t_file->f_begin
        ,TOKEN.t_file->f_pos
        ,TOKEN.t_file->f_end
        ));
 popf();
 return result;
}


PUBLIC tok_t TPPLexer_YieldPP(void) {
 tok_t result;
again:
 result = TPPLexer_YieldRaw();
 if (result == '#' &&
     TOKEN.t_file->f_kind != TPPFILE_KIND_MACRO) {
  char *hash_begin = TOKEN.t_begin;
  if ((CURRENT.l_flags&(TPPLEXER_FLAG_NO_DIRECTIVES|TPPLEXER_FLAG_ASM_COMMENTS)) ==
                        TPPLEXER_FLAG_NO_DIRECTIVES
      ) return result; /* Directives are not being parsed. */
  if (!at_start_of_line()) return result; /* Not actually a preprocessor directive! */
  pushf();
  CURRENT.l_flags |= (TPPLEXER_FLAG_WANTLF|
                      TPPLEXER_FLAG_TERMINATE_STRING_LF|
                      TPPLEXER_FLAG_COMMENT_NOOWN_LF);
  CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTSPACE|TPPLEXER_FLAG_WANTCOMMENTS);
  pusheob();
  /* Parse these are assembler comments. */
  if (CURRENT.l_flags&TPPLEXER_FLAG_NO_DIRECTIVES) goto asm_comment;
  result = TPPLexer_YieldRaw();
parse_result_directive:
  switch (result) {
   { /* NULL-directive. */
   case '\n':
    ;
   } break;

   { /* Ignore shebang-style string markers (e.g.: '#!/bin/bash') */
   case '!':
    if (!HAVE_EXTENSION_SHEBANG) goto default_directive;
def_skip_until_lf:
    CURRENT.l_flags |= TPPLEXER_FLAG_WANTLF;
    while (TOK > 0 && TOK != '\n') TPPLexer_YieldRaw();
   } break;

   { /* Define a new macro. */
   case KWD_define:
    TPPLexer_YieldRaw();
    if (!TPP_ISKEYWORD(TOK)) {
     if unlikely(!(CURRENT.l_flags&TPPLEXER_FLAG_ASM_COMMENTS) &&
                 !WARN(W_EXPECTED_KEYWORD_AFTER_DEFINE)) goto err;
     goto def_skip_until_lf;
    } else {
     if unlikely(!TPPFile_NewDefine()) goto err;
    }
   } break;

   { /* Compiler/Preprocessor-specific pragma. */
    int pragma_error;
    char *old_eof,*new_eof,oldch;
   case KWD_pragma:
    TPPLexer_YieldRaw();
    /* Surprise, surprise! Microsoft and GCC work differently here:
     * >> On MSVC, macros _are_ expanded inside of #pragma directives,
     *    which besides being a _really_ bad idea considering the
     *    existence of pragma names such as 'once', which therefor
     *    can easily, accidentally and completely non-standard-conformingly
     *    be broken using a "#define once 42" (which will break any
     *    header that uses #pragma once), GCC does the sane thing
     *    by not expanding macros here.
     * >> Because of the obvious reason (which is that I'm not
     *    insane last time me and my invisible friend checked),
     *    this implementation follows GCC's example to disable
     *    macros while expanding #pragma directives.
     *    NOTE though, that since GCC doesn't have MSVC's '__pragma'
     *    keyword, we still follow the behavior of Microsoft's preprocessor
     *    by expanding macros inside of '__pragma'.
     *    HINT: But since we're also providing _Pragma, you can simply
     *          use that once, which will take care not to expand macros
     *          within the text block either.
     *
     * |MACRO-EXPANSION|MSVC|GCC|TPP|
     * |---------------+----|---|---|
     * |#pragma foo    | YES| NO| NO|
     * |_Pragma("foo") | N/A| NO| NO|
     * |__pragma(foo)  | YES|N/A|YES|
     * 
     * NOTE: Interestingly enough, intellisense (mostly) seems to
     *       be sane as well, and will actually not expand macros
     *       either after #pragma, or inside of __pragma.
     *      (So much about intellisense being meant to behave
     *       just like your core compiler, Bill... *sigh*)
     */
    CURRENT.l_flags |= (TPPLEXER_FLAG_NO_MACROS|
                        TPPLEXER_FLAG_NO_DIRECTIVES|
                        TPPLEXER_FLAG_NO_BUILTIN_MACROS);
    old_eof = TOKEN.t_file->f_end;
    /* Fake the file pointer to force an EOF where the pragma ends. */
    new_eof = string_find_eol_after_comments(TOKEN.t_file->f_pos,old_eof);
    oldch = *new_eof,*new_eof = '\0';
    TOKEN.t_file->f_end = new_eof;
    pragma_error = TPPLexer_ParsePragma();
    assert(TOKEN.t_file->f_end == new_eof);
    TOKEN.t_file->f_end = old_eof;
    *new_eof = oldch;
    if (!pragma_error && (CURRENT.l_flags&TPPLEXER_FLAG_EAT_UNKNOWN_PRAGMA)) pragma_error = 1;
    while (TOKEN.t_file != CURRENT.l_eob_file) TPPLexer_PopFile();
    assert(hash_begin >= TOKEN.t_file->f_begin);
    assert(hash_begin <= TOKEN.t_file->f_end);
    if (!pragma_error) TOKEN.t_file->f_pos = hash_begin; /* Restore the old file pointer. */
    if (pragma_error) goto def_skip_until_lf;
    /* Must re-emit this #pragma. */
    breakeob();
#if TPPLEXER_FLAG_PRAGMA_KEEPMASK
    breakff(TPPLEXER_FLAG_PRAGMA_KEEPMASK);
#else
    breakf();
#endif
    return TPPLexer_YieldRaw();
   } break;

   { /* Undefine a previously defined user-macro. */
    struct TPPKeyword *keyword;
   case KWD_undef:
    TPPLexer_YieldRaw();
    if (!TPP_ISKEYWORD(TOK)) {
     WARN(W_EXPECTED_KEYWORD_AFTER_UNDEF);
    } else {
     keyword = TOKEN.t_kwd;
     assert(keyword);
     if (keyword->k_macro) {
      if (TPPKeyword_Getflags(keyword)&TPP_KEYWORDFLAG_LOCKED)
          WARN(W_CANT_UNDEF_LOCKED_KEYWORD,keyword);
      else TPPKeyword_Undef(keyword);
     } else if (TPP_ISBUILTINMACRO(TOK)) {
      WARN(W_CANT_UNDEF_BUILTIN_MACRO,keyword);
     } else {
      WARN(W_MACRO_NOT_DEFINED,keyword);
     }
    }
    goto def_skip_until_lf;
   }

   { /* Modify the currently effective line/filename. */
    struct TPPConst val;
    struct TPPFile *textfile;
    int line_offset;
   case KWD_line:
    TPPLexer_Yield();
   case TOK_INT: /* For compatibility with cpp's line emissions. */
    if unlikely(!TPPLexer_Eval(&val)) goto err;
    TPPConst_ToInt(&val);
    textfile = TPPLexer_Textfile();
    assert(textfile->f_kind == TPPFILE_KIND_TEXT);
    line_offset  = TPPFile_LineAt(textfile,textfile->f_pos);
    line_offset -= textfile->f_textfile.f_lineoff;
    /* 'line_offset' is now the offset (in lines) from
     *  the file's current chunk to where we are now. */
    textfile->f_textfile.f_lineoff = (int)((val.c_data.c_int-1)-line_offset);
    if (TOK != '\n') {
     /* Parse a custom filename. */
     if unlikely(!TPPLexer_Eval(&val)) goto err;
     if (val.c_kind == TPP_CONST_STRING) {
      if (textfile->f_textfile.f_usedname) TPPString_Decref(textfile->f_textfile.f_usedname);
      textfile->f_textfile.f_usedname = val.c_data.c_string; /*< Inherit data. */
     } else {
      WARN(W_EXPECTED_STRING_AFTER_LINE,&val);
     }
    }
    goto def_skip_until_lf;
   }

   { /* Emit a warning/error message. */
    int wnum; char *begin,*end,*used_end;
    struct TPPFile *messagefile;
    /* NOTE: Only #error is officially endorsed by STD-C */
    if (FALSE) { case KWD_error:   wnum = W_ERROR; }
    if (FALSE) { case KWD_warning: wnum = W_WARNING; if (!HAVE_EXTENSION_WARNING) goto default_directive; }
    messagefile = TOKEN.t_file;
    assert(messagefile);
    begin = messagefile->f_pos; /* First character after [error]|[warning] */
    end   = string_find_eol_after_comments(begin,messagefile->f_end);
    if (!end) end = messagefile->f_end;
    used_end = end;
    LSTRIP_SPACE(begin,used_end);
    RSTRIP_SPACE(begin,used_end);
    /* Emit the warning/error. */
    if (!WARN(wnum,begin,(size_t)(used_end-begin))) goto err;
    messagefile->f_pos = end;
   } break;

#define ALLOW_LEGACY_GUARD(curfile) \
    (!(CURRENT.l_flags&TPPLEXER_FLAG_NO_LEGACY_GUARDS) && /*< Detection of legacy guard is allowed. */\
      ((curfile)->f_kind == TPPFILE_KIND_TEXT) &&         /*< The current file is a text-file. */\
      !((curfile)->f_textfile.f_cacheentry) &&            /*< The current file isn't a cache-reference. */\
      (!CURRENT.l_ifdef.is_slotc ||                       /*< This is an outer-most block for this file. */\
        CURRENT.l_ifdef.is_slotv[CURRENT.l_ifdef.is_slotc-1].iss_file != (curfile)) &&\
      !((curfile)->f_textfile.f_guard) &&                 /*< The current file doesn't already have a guard. */\
      !((curfile)->f_textfile.f_flags&TPP_TEXTFILE_FLAG_NOGUARD)) /* The current file is allowed to have a guard. */
   {
    int block_mode;
    struct TPPKeyword *ifndef_keyword;
    struct TPPFile *curfile;
    if (FALSE) { case KWD_ifdef:  block_mode = 0; }
    if (FALSE) { case KWD_ifndef: block_mode = 1; }
    assert(block_mode == (TOK == KWD_ifndef));
    ifndef_keyword = NULL;
    TPPLexer_YieldRaw();
    if (TPP_ISKEYWORD(TOK)) {
     ifndef_keyword = TOKEN.t_kwd;
     assert(ifndef_keyword);
    } else {
     WARN(W_EXPECTED_KEYWORD_AFTER_IFDEF);
     ifndef_keyword = NULL;
    }
    if (block_mode) {
     curfile = TOKEN.t_file;
     /* #ifndef: Check if we're at the start of the file to setup a #include guard.
      * WARNING: This detection code will fail to recognize guards when the
      *          #ifndef isn't located within the first chunk of the file.
      *          But since this detection is only used for optimization, that's OK. */

     if (ALLOW_LEGACY_GUARD(curfile)) {
define_ifndef_guard:
      if (curfile->f_textfile.f_newguard) {
       /* Special case: We already found a potential guard in this file.
        *               The fact that we're here again means that this file
        *               has multiple preprocessor-blocks at its top-level,
        *               meaning that the file is no longer a candidate for
        *               a legacy #include-guard. */
       LOG(LOG_LEGACYGUARD,("Second top-level guard '%s' nullifies '%s'\n",
                            ifndef_keyword->k_name,
                            curfile->f_textfile.f_newguard->k_name));
       curfile->f_textfile.f_flags   |= TPP_TEXTFILE_FLAG_NOGUARD;
       curfile->f_textfile.f_newguard = NULL;
      } else if (/* We are inside the first chunk, meaning we can correctly
                  * determine if this is the first thing inside the file. */
                (curfile->f_textfile.f_rdata == curfile->f_text->s_size) &&
                 /* Check if the '#' points to the first non-comment, non-space/lf character. */
                (assert(hash_begin >= curfile->f_text->s_text),
                 assert(hash_begin <= curfile->f_text->s_text+curfile->f_text->s_size),
                 skip_whitespace_and_comments(curfile->f_text->s_text,hash_begin) == hash_begin)
                 ) {
       /* Potential guard-block! Store the #ifndef-keyword as the potential future guard. */
       LOG(LOG_LEGACYGUARD,("Determined potential legacy-guard '%s'\n",
                            ifndef_keyword->k_name));
       curfile->f_textfile.f_newguard = ifndef_keyword;
      } else {
       /* There are other (important) tokens before the first #ifndef block
        * >> Don't allow this file to have a guard! */
       LOG(LOG_LEGACYGUARD,("Legacy-guard '%s' preceded by important tokens\n",
                            ifndef_keyword->k_name));
       curfile->f_textfile.f_flags |= TPP_TEXTFILE_FLAG_NOGUARD;
      }
     }
    }
    block_mode ^= ifndef_keyword && TPPKeyword_ISDEFINED(ifndef_keyword);
create_block:
    /* Enter a #ifdef block, that is enabled when 'guard_mode' is '1' */
    if (block_mode & TPP_IFDEFMODE_TRUE) {
     /* Push a new enabled #ifdef entry. */
     if unlikely(!alloc_ifdef(block_mode)) goto seterr;
    } else {
     breakeob();
     if unlikely(!skip_pp_block() && TOK < 0) goto err;
     result = TOK;
     if (!result || result == KWD_else || result == KWD_elif) {
      /* There's more to this block. - We must allocate it and parse additional directives. */
      if unlikely(!alloc_ifdef(block_mode)) goto seterr;
      goto parse_result_directive;
     } else {
      assert(result <= 0 || result == KWD_endif);
      /* The block simply ends normally. */
     }
    }
    /* Continue parsing the rest of the current line. */
    goto def_skip_until_lf;
   {
    struct TPPConst ifval;
   case KWD_if:
    YIELD();
    /* Automatically detect '#if !defined FOO'/'#if !defined(FOO)'-style guards.
     * NOTE: During this special evaluation, there can't be any macros!
     *       This is required to ensure that the behavior of this code
     *       doesn't depend on the expansion of a macro that might change! */
    if (TOK == '!' && CURRENT.l_eob_file == TOKEN.t_file &&
       (curfile = TOKEN.t_file,ALLOW_LEGACY_GUARD(curfile))) {
     int has_paren;
     char *expr_begin = TOKEN.t_begin;
     TPPLexer_YieldRaw();
     if (TOK != KWD_defined) goto not_a_guard;
     TPPLexer_YieldRaw();
     has_paren = TOK == '(';
     if (has_paren) TPPLexer_YieldRaw();
     if (!TPP_ISKEYWORD(TOK)) goto not_a_guard;
     ifndef_keyword = TOKEN.t_kwd;
     TPPLexer_YieldRaw();
     assert(ifndef_keyword);
     if (has_paren) { if (TOK != ')') goto not_a_guard; TPPLexer_YieldRaw(); }
     if (TOK != '\n') goto not_a_guard;
     /* Yes! This expression behaves exactly like #ifndef, and can therefor replace it! */
     block_mode = 1;
     goto define_ifndef_guard;
not_a_guard:
     while (TOKEN.t_file != CURRENT.l_eob_file) TPPLexer_PopFile();
     assert(expr_begin >= TOKEN.t_file->f_begin);
     assert(expr_begin <= TOKEN.t_file->f_end);
     TOKEN.t_file->f_pos = expr_begin;
     YIELD();
    }
    if unlikely(!TPPLexer_Eval(&ifval)) goto err;
    if (!TPPConst_IsBool(&ifval)) {
     WARN(W_EXPECTED_BOOL,&ifval);
     TPPConst_ToBool(&ifval);
    }
    block_mode = (int)ifval.c_data.c_int;
    goto create_block;
   } break;
   } /* End begin-#ifdef blocks */

   {
    struct TPPIfdefStackSlot *else_slot;
   case KWD_elif:
    if (!CURRENT.l_ifdef.is_slotc ||
       (else_slot = &CURRENT.l_ifdef.is_slotv[CURRENT.l_ifdef.is_slotc-1],
        else_slot->iss_file != TOKEN.t_file)) {
     WARN(W_ELIF_WITHOUT_IF);
    } else if (else_slot->iss_mode&TPP_IFDEFMODE_ELSE) {
     WARN(W_ELIF_AFTER_ELSE,else_slot);
    } else if (else_slot->iss_mode == TPP_IFDEFMODE_TRUE) {
     /* Some chunk of this block was already enabled before.
      * >> Must skip this #elif block! */
     goto skip_block_and_parse;
    } else {
     struct TPPConst elif_val;
     assert(!else_slot->iss_mode);
     YIELD();
     if unlikely(!TPPLexer_Eval(&elif_val)) goto err;
     if (!TPPConst_IsBool(&elif_val)) {
      WARN(W_EXPECTED_BOOL,&elif_val);
      TPPConst_ToBool(&elif_val);
     }
     else_slot->iss_mode = (int)elif_val.c_data.c_int;
     /* If the block is now enabled, skip the rest
      * of the line and continue from there on. */
     if (else_slot->iss_mode) goto def_skip_until_lf;
     /* Must skip this block now! */
skip_block_and_parse:
     breakeob();
     if unlikely(!skip_pp_block()) goto err;
     /* In all cases, we can simply let the
      * default directive-handlers do the job! */
     result = TOK;
     goto parse_result_directive;
    }
    goto def_skip_until_lf;
   } break;

   {
    struct TPPIfdefStackSlot *else_slot;
   case KWD_else:
    if (!CURRENT.l_ifdef.is_slotc ||
       (else_slot = &CURRENT.l_ifdef.is_slotv[CURRENT.l_ifdef.is_slotc-1],
        else_slot->iss_file != TOKEN.t_file)) {
     WARN(W_ELSE_WITHOUT_IF);
    } else if (else_slot->iss_mode&TPP_IFDEFMODE_ELSE) {
     WARN(W_ELSE_AFTER_ELSE,else_slot);
    } else {
     else_slot->iss_mode ^= (TPP_IFDEFMODE_TRUE|TPP_IFDEFMODE_ELSE);
     if (else_slot->iss_mode&TPP_IFDEFMODE_TRUE) goto def_skip_until_lf;
     /* Must skip this block now! */
     goto skip_block_and_parse;
    }
    goto def_skip_until_lf;
   } break;

   { /* End an enabled preprocessor block. */
   case KWD_endif:
    if (!CURRENT.l_ifdef.is_slotc ||
        (CURRENT.l_ifdef.is_slotv[CURRENT.l_ifdef.is_slotc-1].iss_file != TOKEN.t_file)) {
     WARN(W_ENDIF_WITHOUT_IF);
    } else {
     --CURRENT.l_ifdef.is_slotc;
    }
    goto def_skip_until_lf;
   } break;

#define TPPLEXER_OPENFILE_MODE_IMPORT   0x100
   { /* Include a file. */
    int mode; struct TPPKeyword *kwd_entry;
    struct TPPFile *curfile,*include_file,*final_file;
    char *include_begin,*include_end,*end_of_line;
    if (FALSE) { case KWD_include:      mode = TPPLEXER_OPENFILE_MODE_NORMAL; }
    if (FALSE) { case KWD_include_next: mode = TPPLEXER_OPENFILE_FLAG_NEXT;
                 if (!HAVE_EXTENSION_INCLUDE_NEXT) goto default_directive; }
    if (FALSE) { case KWD_import:       mode = TPPLEXER_OPENFILE_MODE_IMPORT;
                 if (!HAVE_EXTENSION_IMPORT) goto default_directive; }
    curfile = TOKEN.t_file; assert(curfile);
    /* Locate the end of the current line. */
    end_of_line = string_find_eol_after_comments(curfile->f_pos,curfile->f_end);
    /* Parse the include string. */
    if unlikely(!parse_include_string(&include_begin,&include_end)) goto def_skip_until_lf;
    /* Place the file pointer at the end of the directive's line. */
    assert(end_of_line >= curfile->f_begin);
    assert(end_of_line >= curfile->f_pos);
    assert(end_of_line <= curfile->f_end);
    curfile->f_pos = end_of_line;
    assert(include_begin[0] == '\"' || include_begin[0] == '<');
    if (include_begin[0] == '\"') {
     mode |= TPPLEXER_OPENFILE_MODE_RELATIVE; /* #include "foo.h" */
    } else {
     mode |= TPPLEXER_OPENFILE_MODE_SYSTEM; /* #include <stdlib.h> */
    }
    ++include_begin;
    assert(include_begin >= TOKEN.t_file->f_begin);
    assert(include_end   <= TOKEN.t_file->f_end);
    /* When the filename originates from something
     * other than a textfile, we must not modify it. */
    if (TOKEN.t_file->f_kind != TPPFILE_KIND_TEXT)
        mode |= TPPLEXER_OPENFILE_FLAG_CONSTNAME;
    include_file = TPPLexer_OpenFile(mode&0xff,include_begin,
                                    (size_t)(include_end-include_begin),
                                    &kwd_entry);
    if unlikely(!include_file) {
     TOKEN.t_begin = include_begin;
     WARN(W_FILE_NOT_FOUND,include_begin,
                  (size_t)(include_end-include_begin));
     break;
    }
    if unlikely(mode&TPPLEXER_OPENFILE_MODE_IMPORT) {
     /* Special semantics for #import ... */
     assert(kwd_entry);
     assert(kwd_entry->k_rare);
     /* #import can only be used to include a file once. */
     if (kwd_entry->k_rare->kr_flags&TPP_KEYWORDFLAG_IMPORTED) break;
     kwd_entry->k_rare->kr_flags |= TPP_KEYWORDFLAG_IMPORTED;
    }
    /* Check if the file is guarded by an active preprocessor macro. */
    assert(include_file->f_kind == TPPFILE_KIND_TEXT);
    if (include_file->f_textfile.f_guard &&
        TPPKeyword_ISDEFINED(include_file->f_textfile.f_guard)) {
     /* File is guarded by an active preprocessor macro (Don't #include it). */
     LOG(LOG_LEGACYGUARD,("Skip include for file '%s' guarded by active guard '%s'\n",
                          include_file->f_name,include_file->f_textfile.f_guard->k_name));
     break;
    }
    /* Make sure we're not exceeding the #include recursion limit. */
    if (include_file->f_textfile.f_cacheinc >= CURRENT.l_limit_incl) {
     WARN(W_INCLUDE_RECURSION_LIMIT_EXCEEDED,include_file);
     break;
    }
    final_file = TPPFile_CopyForInclude(include_file);
    if unlikely(!final_file) goto seterr;
#if 1
    assert(curfile);
    assert(curfile->f_refcnt);
    /* Make sure to pop any macro files still on the #include-stack.
     * If we'd fail to do this, they'd still count as currently being expanded! */
    while (TOKEN.t_file != curfile) TPPLexer_PopFile();
#endif
    pushfile_inherited(final_file);
   } break;

   { /* Define/delete an assertion. */
    struct TPPKeyword *assertion_kwd;
    tok_t mode;
   case KWD_assert:
   case KWD_unassert:
    if (!HAVE_EXTENSION_ASSERTIONS) goto default_directive;
    mode = TOK;
    TPPLexer_YieldRaw();
    if likely(TPP_ISKEYWORD(TOK)) {
     assertion_kwd = TOKEN.t_kwd;
     TPPLexer_YieldRaw();
    } else {
     WARN(W_EXPECTED_KEYWORD_AFTER_ASSERT);
     assertion_kwd = NULL;
    }
    if (TOK != '(') {
     if (mode == KWD_unassert) {
      /* '#unassert foobar' is allowed to clear assertions for 'foobar' */
      keyword_clrassert(assertion_kwd);
      goto def_skip_until_lf;
     }
     WARN(W_EXPECTED_LPAREN);
    } else TPPLexer_YieldRaw();
    if likely(TPP_ISKEYWORD(TOK)) {
     if (assertion_kwd) {
      if (mode == KWD_assert) {
       if unlikely(!keyword_addassert(assertion_kwd,TOKEN.t_kwd)) goto seterr;
      } else {
       assert(mode == KWD_unassert);
       if unlikely(!keyword_delassert(assertion_kwd,TOKEN.t_kwd) &&
                   !WARN(W_UNKNOWN_ASSERTION,assertion_kwd,TOKEN.t_kwd)) goto err;
      }
     }
     TPPLexer_YieldRaw();
    } else if (assertion_kwd) {
     WARN(W_EXPECTED_KEYWORD_AFTER_PREDICATE,assertion_kwd);
    }
    if (TOK != ')') WARN(W_EXPECTED_LPAREN);
    else TPPLexer_YieldRaw();
    goto def_skip_until_lf;
   } break;


   { /* >> https://gcc.gnu.org/onlinedocs/cpp/Other-Directives.html */
    struct TPPConst ident_string; int werror;
   case KWD_ident:
   case KWD_sccs:
    if (!HAVE_EXTENSION_IDENT_SCCS) goto default_directive;
    TPPLexer_Yield();
    if unlikely(!TPPLexer_Eval(&ident_string)) goto err;
    if unlikely(ident_string.c_kind != TPP_CONST_STRING) {
     werror = WARN(W_EXPECTED_STRING_AFTER_IDENT,&ident_string);
    } else if (!CURRENT.l_callbacks.c_ins_comment) {
     werror = WARN(W_IDENT_SCCS_IGNORED,&ident_string);
    } else {
     werror = (*CURRENT.l_callbacks.c_ins_comment)(ident_string.c_data.c_string);
    }
    TPPConst_Quit(&ident_string);
    if unlikely(!werror) goto err;
    goto def_skip_until_lf;
   } break;

   default:
    if unlikely(result <= 0) { breakeob(); breakf(); return result; }
default_directive:
    if (CURRENT.l_flags&TPPLEXER_FLAG_ASM_COMMENTS) {
     char *comment_end;
asm_comment:
     if (!(_oldflags&TPPLEXER_FLAG_WANTCOMMENTS)) goto def_skip_until_lf;
     breakf();
     /* Literally emit this directive as a comment!
      * WARNING: Similar to line-comments this kind of comment
      *          takes precedence before slash-start comments.
      *          For that reason (and unlike other directives),
      *          we search for the regular EOL instead of the
      *          one past potential c-style comments.
      *       >> Therefor, we call 'string_find_eol' below,
      *          instead of 'string_find_eol_after_comments'.
      */
     TOKEN.t_begin = hash_begin;
     comment_end   = string_find_eol(hash_begin,(size_t)(TOKEN.t_file->f_end-hash_begin));
     if (!comment_end) comment_end = TOKEN.t_file->f_end;
     /* The linefeed past the comment is still part of it (just like with regular line-comments.) */
     if (*comment_end == '\r') {
      if (comment_end[1] == '\n') ++comment_end;
      ++comment_end;
     } else if (*comment_end == '\n') {
      ++comment_end;
     }
     TOKEN.t_file->f_pos = TOKEN.t_end = comment_end;
     TOKEN.t_id = result = TOK_COMMENT;
    } else {
     breakf();
     if unlikely(!WARN(W_UNKNOWN_PREPROCESSOR_DIRECTIVE)) goto err;
     /* Revert the file pointer to where the '#'-token started.
      * >> We are going to re-emit this (supposed) preprocessor directive. */
     assert(hash_begin >= TOKEN.t_file->f_begin);
     assert(hash_begin <= TOKEN.t_file->f_end);
     TOKEN.t_file->f_pos = hash_begin;
     result = TPPLexer_YieldRaw();
    }
    breakeob();
    return result;

seterr: TPPLexer_SetErr();
err:    breakeob();
        breakf();
        return TOK_ERR;

  }
  popeob();
  popf();
  /* Check if we're supposed to re-emit the linefeed here. */
  if ((CURRENT.l_flags&(TPPLEXER_FLAG_WANTLF|TPPLEXER_FLAG_DIRECTIVE_NOOWN_LF)) !=
                       (TPPLEXER_FLAG_WANTLF|TPPLEXER_FLAG_DIRECTIVE_NOOWN_LF)
      ) goto again;
 }
 return result;
}

PRIVATE /*ref*/struct TPPString *
escape_entire_file(struct TPPFile *infile) {
 struct TPPString *result,*newresult;
 size_t insize,reqsize,allocsize;
 /* Load the initial chunk (in binary mode, to prevent detection of encoding). */
 if unlikely(infile->f_begin == infile->f_end &&
            !TPPFile_NextChunk(infile,TPPFILE_NEXTCHUNK_FLAG_BINARY)) {
  /* Special case: Empty file. */
  TPPString_Incref(empty_string);
  return empty_string;
 }
 insize = (size_t)(infile->f_end-infile->f_begin);
 allocsize = reqsize = TPP_SizeofEscape(infile->f_begin,insize)+2;
 assert(reqsize);
 result = TPPString_NewSized(reqsize);
 if unlikely(!result) return NULL;
 TPP_Escape(result->s_text+1,infile->f_begin,insize);
 while (TPPFile_NextChunk(infile,TPPFILE_NEXTCHUNK_FLAG_BINARY)) {
  /* Escape and append this chunk. */
  insize = (size_t)(infile->f_end-infile->f_begin);
  reqsize = result->s_size+TPP_SizeofEscape(infile->f_begin,insize);
  if (reqsize > allocsize) {
   while (allocsize < reqsize) allocsize *= 2;
   newresult = (struct TPPString *)realloc(result,TPP_OFFSETOF(struct TPPString,s_text)+
                                          (allocsize+1)*sizeof(char));
   if unlikely(!newresult) goto err_r;
   result = newresult;
  }
  TPP_Escape(result->s_text+(result->s_size-1),infile->f_begin,insize);
  result->s_size = reqsize;
 }
 if (result->s_size != allocsize) {
  newresult = (struct TPPString *)realloc(result,TPP_OFFSETOF(struct TPPString,s_text)+
                                         (result->s_size+1)*sizeof(char));
  if likely(newresult) result = newresult;
 }
 assert(result->s_size);
 result->s_text[result->s_size-1] = '\"';
 result->s_text[result->s_size] = '\0';
 return result;
err_r:
 free(result);
 return NULL;
}


PRIVATE char const date_month_names[12][4] = {
 "Jan","Feb","Mar","Apr","May","Jun",
 "Jul","Aug","Sep","Oct","Nov","Dec"};
PRIVATE char const date_wday_names[7][4] = {
 "Sun","Mon","Tue","Wed","Thu","Fri","Sat"};

#if !TPP_CONFIG_MINMACRO || TPP_CONFIG_MINGCCFUNC >= 2
static char const *const intc_suffix[] = {
 /* __INT8_C    */"i8\0",
 /* __INT16_C   */"i16\0",
#if __SIZEOF_INT__ != 4
 /* __INT32_C   */"i32\0l",
#else
 /* __INT32_C   */"i32\0",
#endif
#if __SIZEOF_LONG__ == 8
 /* __INT64_C   */"i64\0l",
#else
 /* __INT64_C   */"i64\0ll",
#endif
 /* __UINT8_C   */"ui8\0",
 /* __UINT16_C  */"ui16\0",
#if __SIZEOF_INT__ != 4
 /* __INT32_C   */"ui32\0ul",
#else
 /* __INT32_C   */"ui32\0u",
#endif
#if __SIZEOF_LONG__ == 8
 /* __INT64_C   */"ui64\0ul",
#else
 /* __INT64_C   */"ui64\0ull",
#endif
 /* __INTMAX_C  */"i64\0ll",
 /* __UINTMAX_C */"ui64\0ull",
};
#endif /* !TPP_CONFIG_MINMACRO */


PUBLIC tok_t TPPLexer_Yield(void) {
 struct TPPFile *macro;
 struct TPPString *string_text;
 tok_t result;
 int_t intval;
again:
 result = TPPLexer_YieldPP();
 if (TPP_ISKEYWORD(result)) {
  if ((assert(TOKEN.t_kwd),
      (macro = TOKEN.t_kwd->k_macro) != NULL) &&
     !(CURRENT.l_flags&TPPLEXER_FLAG_NO_MACROS)) {
   /* User-defined macro expansion. */
   assert(macro->f_kind == TPPFILE_KIND_MACRO);
   assert((macro->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION ||
          (macro->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_KEYWORD);
   if ((macro->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_KEYWORD) {
    if (!macro->f_prev) { /* Simple case: keyword-style macro. */
     macro->f_pos = macro->f_begin;
     pushfile(macro);
     goto again; /* Parse the macro now located on the #include-stack. */
    }
    if (!WARN(W_KEYWORD_MACRO_ALREADY_ONSTACK)) return TOK_ERR;
    goto end;
   }
   /* Check if this macro is allowed to expand:
    * >> Only if it's allowed to self-expand, or isn't already
    *    being expanded may we even attempt to expand it. */
   if (!macro->f_macro.m_function.f_expansions || 
        macro->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_SELFEXPAND) {
    /* Function-style macro. */
    switch (TPPLexer_ExpandFunctionMacro(macro)) {
     case 2:  goto end;   /* Macro was not expanded. */
     case 1:  goto again; /* Macro was expanded. */
     default: return TOK_ERR;
    }
   }
  }
  /* Expand builtin macros.
   * NOTE: Only check for these _after_ we've already checked for a user-defined macro,
   *       so-to allow the user to override builtin macros and simply restore regular
   *       behavior by #undef-ing their's again. */
  if unlikely(CURRENT.l_flags&TPPLEXER_FLAG_NO_BUILTIN_MACROS) goto end;

  switch (TOK) {

   { /* STDC-style pragma. */
    char *old_filepos;
    int pragma_error;
    struct TPPConst pragma_const;
    struct TPPFile *pragma_file;
   case KWD__Pragma:
    old_filepos = TOKEN.t_begin;
    assert(old_filepos >= TOKEN.t_file->f_begin &&
           old_filepos <= TOKEN.t_file->f_end);
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    TPPLexer_Yield();
    if (TOK == '(') TPPLexer_Yield();
    else WARN(W_EXPECTED_LPAREN);
    /* At this point, we should be hovering over a string.
     * >> Evaluating a constant expression here might be overkill,
     *    but doing it this way, we don't need to write more code. */
    if unlikely(!TPPLexer_Eval(&pragma_const)) {err_pragmaf: breakf(); return TOK_ERR; }
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    if (pragma_const.c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_AFTER_PRAGMA,&pragma_const);
     pragma_error = 0;
    } else {
     pragma_file = TPPFile_NewExplicitInherited(pragma_const.c_data.c_string);
     if unlikely(!pragma_file) { TPPString_Decref(pragma_const.c_data.c_string); goto err_pragmaf; }
     pushfile_inherited(pragma_file);
     TPPLexer_YieldRaw();
     /* Disable macros & directives in this kinds of pragma.
      * No-macros-allowed is the whole point of encoding everything as a string! */
     CURRENT.l_flags |= (TPPLEXER_FLAG_NO_MACROS|
                         TPPLEXER_FLAG_NO_DIRECTIVES|
                         TPPLEXER_FLAG_NO_BUILTIN_MACROS);
     pusheof();
     pragma_error = TPPLexer_ParsePragma();
     if (!pragma_error && (CURRENT.l_flags&TPPLEXER_FLAG_EAT_UNKNOWN_PRAGMA)) pragma_error = 1;
     while (TOKEN.t_file != CURRENT.l_eof_file) TPPLexer_PopFile();
     popeof();
    }
#if TPPLEXER_FLAG_PRAGMA_KEEPMASK
    popff(TPPLEXER_FLAG_PRAGMA_KEEPMASK);
#else
    popf();
#endif
    if (!pragma_error) {
     TOKEN.t_file->f_pos = old_filepos; /* Restore the old file pointer. */
    } else {
     /* If we managed to parse the pragma, continue parsing afterwards. */
     goto again;
    }
    /* Failed to parse the pragma. (The file pointer was restored, so
     * now we can re-parse the '_Pragma' token to emit it manually). */
    result = TPPLexer_YieldPP();
   } break;

   { /* Generate an escaped string representation of the current file's name. */
    char const *name;
    size_t      size,quoted_size;
    struct TPPFile   *string_file;
   case KWD___FILE__:
    name = TPPLexer_FILE(&size);
put_filename:
    quoted_size = 2+TPP_SizeofEscape(name,size);
    string_text = TPPString_NewSized(quoted_size);
    if unlikely(!string_text) goto seterr;
    string_text->s_text[0]             = '\"';
    TPP_Escape(string_text->s_text+1,name,size);
    string_text->s_text[quoted_size-1] = '\"';
create_string_file:
    string_file = TPPFile_NewExplicitInherited(string_text);
    if unlikely(!string_file) { TPPString_Decref(string_text); goto seterr; }
    pushfile_inherited(string_file);
    goto again;
   case KWD___BASE_FILE__:
    if (!HAVE_EXTENSION_BASEFILE) break;
    name = TPPLexer_BASEFILE(&size);
    goto put_filename;
   }

   { /* STD-C Date/Time strings. */
    time_t timenow; struct tm *tmnow;
   case KWD___TIME__:
    string_text = TPPString_NewSized(10);
    if unlikely(!string_text) goto seterr;
    timenow = time(NULL);
    tmnow = localtime(&timenow);
    if (!tmnow) strcpy(string_text->s_text,"\"??:??:??\"");
    else sprintf(string_text->s_text,"\"%02d:%02d:%02d\"",
                 tmnow->tm_hour,tmnow->tm_min,tmnow->tm_sec);
    goto create_string_file;
   case KWD___DATE__:
    string_text = TPPString_NewSized(13);
    if unlikely(!string_text) goto seterr;
    timenow = time(NULL);
    tmnow = localtime(&timenow);
    if (!tmnow) strcpy(string_text->s_text,"\"??" "? ?? ??" "??\"");
    else sprintf(string_text->s_text,"\"%s %2d %4d\"",
                 date_month_names[tmnow->tm_mon],
                 tmnow->tm_mday,tmnow->tm_year+1900);
    goto create_string_file;
   case KWD___TIMESTAMP__:
    if (!HAVE_EXTENSION_TIMESTAMP) break;
    string_text = TPPString_NewSized(26);
    if unlikely(!string_text) goto seterr;
    timenow = time(NULL);
    tmnow = localtime(&timenow);
    if (!tmnow) strcpy(string_text->s_text,"\"??? ??? ?? ??:??:?? ????\"");
    else sprintf(string_text->s_text,"\"%s %s%3d %.2d:%.2d:%.2d %4d\"",
                 date_wday_names[tmnow->tm_wday],
                 date_month_names[tmnow->tm_mon],
                 tmnow->tm_mday,tmnow->tm_hour,tmnow->tm_min,
                 tmnow->tm_sec,1900+tmnow->tm_year);
    goto create_string_file;
   }

   { /* Generate an integral constant representing
      * the 1-based index of the current source-line. */
    size_t intsize;
   case KWD___LINE__:
    intval = (int_t)TPPLexer_LINE()+1;
create_int_file:
    intsize = TPP_SizeofItos(intval);
    string_text = TPPString_NewSized(intsize);
    if unlikely(!string_text) goto seterr;
    TPP_Itos(string_text->s_text,intval);
    goto create_string_file;
   }

   { /* Expand into an integral constant representing the current column number. */
   case KWD___COLUMN__:
    if (!HAVE_EXTENSION_COLUMN) break;
    intval = (int_t)TPPLexer_COLUMN()+1;
    goto create_int_file;
   } break;

   { /* Expand into an integral constant representing the current include level. */
    struct TPPFile *file_iter;
   case KWD___INCLUDE_LEVEL__:
   case KWD___INCLUDE_DEPTH__:
    if (!HAVE_EXTENSION_INCLUDE_LEVEL) break;
    intval = -1,file_iter = TOKEN.t_file;
    while (file_iter != &TPPFile_Empty) {
     if (file_iter->f_kind == TPPFILE_KIND_TEXT) ++intval;
     file_iter = file_iter->f_prev;
    }
    if unlikely(intval < 0) intval = 0;
    goto create_int_file;
   } break;

   { /* Expand into a non-recurring, ever-incrementing integral constant. */
   case KWD___COUNTER__:
    if (!HAVE_EXTENSION_COUNTER) break;
    intval = CURRENT.l_counter++;
    goto create_int_file;
   } break;

   { /* Check various attributes of keyword. */
    char *keyword_begin,*keyword_end,*file_end,*file_pos;
    size_t keyword_rawsize,keyword_realsize;
    struct TPPKeyword *keyword; tok_t mode;
    int create_missing_keyword;
   case KWD___has_attribute:
   case KWD___has_builtin:
   case KWD___has_tpp_builtin:
   case KWD___has_cpp_attribute:
   case KWD___has_declspec_attribute:
   case KWD___has_extension:
   case KWD___has_known_extension:
   case KWD___has_warning:
   case KWD___has_known_warning:
   case KWD___has_feature:
    /* Create keywords for these to work around leading/terminating underscore quirks. */
    create_missing_keyword = 1;
    if (FALSE) { case KWD___is_deprecated:
                 case KWD___is_identifier:
                 case KWD___is_builtin_identifier: create_missing_keyword = 0; }
    if (!HAVE_EXTENSION_CLANG_FEATURES) break;
    if (FALSE) { case KWD___TPP_UNIQUE:  if (!HAVE_EXTENSION_TPP_UNIQUE) break;
                                         create_missing_keyword = 1; }
    if (FALSE) { case KWD___TPP_COUNTER: if (!HAVE_EXTENSION_TPP_COUNTER) break;
                                         create_missing_keyword = 1; }
    mode = TOK;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    TPPLexer_YieldPP();
    popf();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN);
    keyword_begin = TOKEN.t_end,file_end = TOKEN.t_file->f_end;
    keyword_begin = skip_whitespace_and_comments(keyword_begin,file_end);
    /* Special handling if the next token is a string/number. */
    if (*keyword_begin == '\"' || tpp_isdigit(*keyword_begin)) {
     struct TPPConst name;
     YIELD();
     if (!TPPLexer_Eval(&name)) {
      name.c_kind       = TPP_CONST_INTEGRAL;
      name.c_data.c_int = 0;
     }
     if (TOK != ')') WARN(W_EXPECTED_RPAREN);
     /* Special handling for __has_extension("-fname"), etc. */
     switch (mode) {
     case KWD___has_extension:
     case KWD___has_known_extension: /* __has_extension("-fmacro-recursion") */
      if likely(name.c_kind == TPP_CONST_STRING) {
       char *n = name.c_data.c_string->s_text;
       if (*n == '-') ++n;
       if (*n == 'f') ++n;
       intval = (int_t)TPPLexer_GetExtension(n);
       intval = mode == KWD___has_extension ? ((int)intval == 1)
                                            : ((int)intval >= 0);
       TPPString_Decref(name.c_data.c_string);
       goto create_int_file;
      }
      break;
     case KWD___has_warning:
     case KWD___has_known_warning: /* __has_warning("-Wsyntax") | __has_warning(42) */
      if (name.c_kind == TPP_CONST_STRING) {
       char *n = name.c_data.c_string->s_text;
       if (*n == '-') ++n;
       if (*n == 'W') ++n;
       intval = (int_t)TPPLexer_GetWarnings(n);
       TPPString_Decref(name.c_data.c_string);
      } else if likely(name.c_kind != TPP_CONST_FLOAT) {
       intval = (int_t)TPPLexer_GetWarning((int)name.c_data.c_int);
      } else {
       intval = (int_t)TPPLexer_GetWarning((int)name.c_data.c_float);
      }
      intval = mode == KWD___has_warning ? TPP_WSTATE_ISENABLED((int)intval)
                                         : intval != WSTATE_UNKNOWN;
      goto create_int_file;
     default: break;
     }
     /* Fallback: Create a keyword from the constant expression. */
     if (name.c_kind == TPP_CONST_STRING) {
      keyword = TPPLexer_LookupKeyword(name.c_data.c_string->s_text,
                                       name.c_data.c_string->s_size,
                                       create_missing_keyword);
      TPPString_Decref(name.c_data.c_string);
     } else {
      struct TPPString *cstname;
      if ((cstname = TPPConst_ToString(&name)) != NULL)
           keyword = TPPLexer_LookupKeyword(cstname->s_text,
                                            cstname->s_size,
                                            create_missing_keyword),
           TPPString_Decref(cstname);
      else keyword = NULL;
     }
    } else {
     keyword_end = keyword_begin;
     while (keyword_end != file_end) {
      while (SKIP_WRAPLF(keyword_end,file_end));
      if (!tpp_isalnum(*keyword_end)) break;
      ++keyword_end;
     }
     file_pos = skip_whitespace_and_comments(keyword_end,file_end);
     TOKEN.t_file->f_pos = file_pos;
     if (*file_pos != ')') {
      WARN(W_EXPECTED_RPAREN);
      keyword_end = keyword_begin;
     } else {
      ++TOKEN.t_file->f_pos; /* Skip the ')' character. */
     }
     /* Whitespace and comments have been stripped from both ends, and
      * we can be sure that the keyword only contains alnum characters. */
     assert(keyword_begin <= keyword_end);
     keyword_rawsize = (size_t)(keyword_end-keyword_begin);
     keyword_realsize = wraplf_memlen(keyword_begin,keyword_rawsize);
     if (keyword_realsize == keyword_rawsize) {
      keyword = TPPLexer_LookupKeyword(keyword_begin,keyword_rawsize,
                                       create_missing_keyword);
     } else {
      keyword = lookup_escaped_keyword(keyword_begin,keyword_rawsize,
                                       keyword_realsize,
                                       create_missing_keyword);
     }
    }
    /* Handle case: Unknown/unused keyword. */
    if (!keyword) {
     if unlikely(create_missing_keyword) goto seterr;
     intval = 0;
    } else switch (mode) {
   /*case KWD___is_identifier:*/
     default: intval = 1; break;
     case KWD___is_builtin_identifier:
      intval = !TPP_ISUSERKEYWORD(keyword->k_id);
      break;
     case KWD___TPP_UNIQUE:
      assert(keyword->k_id >= TOK_KEYWORD_BEGIN);
      intval = (keyword->k_id-TOK_KEYWORD_BEGIN);
      break;
     case KWD___TPP_COUNTER:
      if unlikely(!TPPKeyword_MAKERARE(keyword)) goto seterr;
      intval = keyword->k_rare->kr_counter++;
      break;
      /* Default behavior: Use the keyword name for lookup. */
     case KWD___has_warning:
      intval = TPP_WSTATE_ISENABLED(TPPLexer_GetWarnings(keyword->k_name));
      break;
     case KWD___has_known_extension:
      intval = TPPLexer_GetExtension(keyword->k_name) != -1;
      break;
     case KWD___has_known_warning:
      intval = TPPLexer_GetWarnings(keyword->k_name) != WSTATE_UNKNOWN;
      break;

     case KWD___is_deprecated:
      /* Don't allow leading/terminating underscores here! */
      intval = keyword->k_rare && !!(keyword->k_rare->kr_flags&TPP_KEYWORDFLAG_IS_DEPRECATED);
      break;
     { /* Flag-based keyword properties. */
      uint32_t mask;
      if (FALSE) { case KWD___has_attribute:          mask  = TPP_KEYWORDFLAG_HAS_ATTRIBUTE;          }
      if (FALSE) { case KWD___has_builtin:            mask  = TPP_KEYWORDFLAG_HAS_BUILTIN;            }
      if (FALSE) { case KWD___has_tpp_builtin:        mask  = TPP_KEYWORDFLAG_HAS_TPP_BUILTIN;        }
      if (FALSE) { case KWD___has_cpp_attribute:      mask  = TPP_KEYWORDFLAG_HAS_CPP_ATTRIBUTE;      }
      if (FALSE) { case KWD___has_declspec_attribute: mask  = TPP_KEYWORDFLAG_HAS_DECLSPEC_ATTRIBUTE; }
      if (FALSE) { case KWD___has_extension:          mask  = TPP_KEYWORDFLAG_HAS_EXTENSION;          }
      if (FALSE) { case KWD___has_feature:            mask  = TPP_KEYWORDFLAG_HAS_FEATURE;
                 if (HAVE_EXTENSION_EXT_ARE_FEATURES) mask |= TPP_KEYWORDFLAG_HAS_EXTENSION; }
      intval = !!(TPPKeyword_Getflags(keyword)&mask);
      break;
     }
    }
    goto create_int_file;
   } break;

   { /* Check various attributes of include files. */
    tok_t function; int mode;
    struct TPPFile *incfile;
    char *include_begin,*include_end;
   case KWD___has_include:
   case KWD___has_include_next:
    if (!HAVE_EXTENSION_CLANG_FEATURES) break;
    if (FALSE) {
   case KWD___TPP_LOAD_FILE:
    if (!HAVE_EXTENSION_TPP_LOAD_FILE) break;
    }
    function = TOK,mode = 0;
    if (function == KWD___has_include_next) mode |= TPPLEXER_OPENFILE_FLAG_NEXT;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    TPPLexer_YieldPP();
    popf();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN);
    if unlikely(!parse_include_string(&include_begin,&include_end)) break;
    assert(include_end > include_begin);
    assert(include_begin[0] == '\"' || include_begin[0] == '<');
    if (include_begin[0] == '\"') {
     mode |= TPPLEXER_OPENFILE_MODE_RELATIVE; /* #include "foo.h" */
    } else {
     mode |= TPPLEXER_OPENFILE_MODE_SYSTEM; /* #include <stdlib.h> */
    }
    ++include_begin;
    assert(include_begin >= TOKEN.t_file->f_begin);
    assert(include_end   <= TOKEN.t_file->f_end);
    /* When the filename originates from something
     * other than a textfile, we must not modify it. */
    if (TOKEN.t_file->f_kind != TPPFILE_KIND_TEXT)
        mode |= TPPLEXER_OPENFILE_FLAG_CONSTNAME;
    /* Try to open the file described.
     * NOTE: If we do manage to open it, it will already
     *       be cached when a #include tries to use it. */
    incfile = TPPLexer_OpenFile(mode,include_begin,
                               (size_t)(include_end-include_begin),
                                NULL);
    assert(!incfile || !(mode&TPPLEXER_OPENFILE_FLAG_NEXT) ||
                       !(incfile->f_prev));
    if (function == KWD___TPP_LOAD_FILE && !incfile) {
     WARN(W_FILE_NOT_FOUND,include_begin,
                  (size_t)(include_end-include_begin));
    }
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    TPPLexer_YieldPP();
    popf();
    if (TOK != ')') {
     WARN(W_EXPECTED_RPAREN);
     assert(TOKEN.t_begin >= TOKEN.t_file->f_begin);
     assert(TOKEN.t_begin <= TOKEN.t_file->f_pos);
     assert(TOKEN.t_begin <= TOKEN.t_file->f_end);
     TOKEN.t_file->f_pos = TOKEN.t_begin;
    }
    if (function == KWD___TPP_LOAD_FILE) {
     if (!incfile) string_text = TPPString_New("\"\"",2);
     else {
      /* Special case: Load the raw text of this file. */
      incfile = TPPFile_CopyForInclude(incfile);
      if unlikely(!incfile) goto seterr;
      string_text = escape_entire_file(incfile);
      TPPFile_Decref(incfile);
     }
     if unlikely(!string_text) goto seterr;
     goto create_string_file;
    }
    intval = incfile != NULL;
    goto create_int_file;
   } break;

   {
    tok_t mode; time_t timenow;
    struct tm *tmnow;
   case KWD___DATE_DAY__:
   case KWD___DATE_WDAY__:
   case KWD___DATE_YDAY__:
   case KWD___DATE_MONTH__:
   case KWD___DATE_YEAR__:
    if (!HAVE_EXTENSION_DATEUTILS) break;
    if (FALSE) {
   case KWD___TIME_SEC__:
   case KWD___TIME_MIN__:
   case KWD___TIME_HOUR__:
    if (!HAVE_EXTENSION_TIMEUTILS) break; }
    mode = TOK;
    timenow = time(NULL);
    tmnow = localtime(&timenow);
    if (!tmnow) intval = 0;
    else switch (mode) {
     case KWD___DATE_DAY__  : intval = tmnow->tm_mday; break;
     case KWD___DATE_WDAY__ : intval = tmnow->tm_wday; break;
     case KWD___DATE_YDAY__ : intval = tmnow->tm_yday; break;
     case KWD___DATE_MONTH__: intval = tmnow->tm_mon+1; break;
     case KWD___DATE_YEAR__ : intval = tmnow->tm_year+1900; break;
     case KWD___TIME_SEC__  : intval = tmnow->tm_sec; break;
     case KWD___TIME_MIN__  : intval = tmnow->tm_min; break;
   /*case KWD___TIME_HOUR__ :*/
     default                : intval = tmnow->tm_hour; break;
    }
    goto create_int_file;
   } break;

   { /* MSVC-style pragma. */
    char *old_filepos;
    int pragma_error; size_t old_eofparen;
   case KWD___pragma:
    if (!HAVE_EXTENSION_MSVC_PRAGMA) break;
    pushf();
    pusheof();
    /* Disable some unnecessary tokens. */
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    old_filepos = TOKEN.t_begin;
    assert(old_filepos >= TOKEN.t_file->f_begin &&
           old_filepos <= TOKEN.t_file->f_end);
    TPPLexer_Yield();
    if (TOK == '(') TPPLexer_Yield();
    else WARN(W_EXPECTED_LPAREN);
    /* WARNING: We leave macros turned on for this, because (perhaps surprisingly),
     *          visual studio _does_ expand macros inside its __pragma helper. */
    CURRENT.l_flags    |= TPPLEXER_FLAG_EOF_ON_PAREN;
    old_eofparen        = CURRENT.l_eof_paren;
    CURRENT.l_eof_paren = 1; /* Use EOF-on-paren recursion here. */
    pragma_error        = TPPLexer_ParsePragma();
    CURRENT.l_eof_paren = old_eofparen;
    CURRENT.l_flags    &= ~(TPPLEXER_FLAG_EOF_ON_PAREN);
    CURRENT.l_flags    |= (_oldflags&TPPLEXER_FLAG_EOF_ON_PAREN);
    if (!pragma_error && (CURRENT.l_flags&TPPLEXER_FLAG_EAT_UNKNOWN_PRAGMA)) pragma_error = 1;
    while (TOKEN.t_file != CURRENT.l_eof_file) TPPLexer_PopFile();
    if (!TOK && *TOKEN.t_begin == ')') TOK = ')';
    if (pragma_error) {
     int recursion = 1;
     while (TOK > 0) {
           if (TOK == '(') ++recursion;
      else if (TOK == ')' && !--recursion) break;
      TPPLexer_Yield();
     }
     if (TOK == ')') TPPLexer_Yield();
     else WARN(W_EXPECTED_RPAREN),TOK = 0; /* NOTE: Set TOK to 0 to force a reload below. */
    }
    if (!pragma_error) TOKEN.t_file->f_pos = old_filepos; /* Restore the old file pointer. */
    popeof();
#if TPPLEXER_FLAG_PRAGMA_KEEPMASK
    popff(TPPLEXER_FLAG_PRAGMA_KEEPMASK);
#else
    popf();
#endif
    if (!pragma_error) TPPLexer_YieldPP(); /* Don't parse pragmas again to prevent infinite recursion. */
    else if (!TOK) goto again; /* If we managed to parse the pragma, continue parsing afterwards. */
    result = TOK;
   } break;

   { /* TPP's good 'ol __TPP_EVAL() extension (Now with intrinsic string-support!). */
    struct TPPConst val;
    struct TPPString *val_str;
    struct TPPFile *val_file;
    int eval_error;
   case KWD___TPP_EVAL:
    if (!HAVE_EXTENSION_TPP_EVAL) break;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK == '(') YIELD();
    else WARN(W_EXPECTED_LPAREN);
    /* Evaluate a constant preprocessor-expression. */
    eval_error = TPPLexer_Eval(&val);
    popf();
    if unlikely(!eval_error) return TOK_ERR;
    if (TOK != ')') {
     WARN(W_EXPECTED_RPAREN);
     TOKEN.t_file->f_pos = TOKEN.t_begin;
    }
    val_str = TPPConst_ToString(&val);
    TPPConst_Quit(&val);
    if unlikely(!val_str) goto seterr;
    val_file = TPPFile_NewExplicitInherited(val_str);
    if unlikely(!val_file) { TPPString_Decref(val_str); goto seterr; }
    pushfile_inherited(val_file);
    goto again;
   } break;

   { /* Expand into the integral representation of how may tokens are given.
      * Very useful when wanting to detect extension token flags:
      * >> #if __TPP_COUNT_TOKENS(":=") == 1
      * >> #pragma message("The ':=' token is enabled")
      * >> #endif
      * >> #if __TPP_COUNT_TOKENS(" ") != 0
      * >> #pragma message("Whitespace is treated as its own token")
      * >> #endif
      * >> #if __TPP_COUNT_TOKENS("\n") != 0
      * >> #pragma message("Linefeeds are kept during preprocessing")
      * >> #endif
      */
    struct TPPConst val;
    struct TPPFile *cnt_file;
   case KWD___TPP_COUNT_TOKENS:
    if (!HAVE_EXTENSION_TPP_COUNT_TOKENS) break;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if likely(TOK == '(') YIELD();
    else WARN(W_EXPECTED_LPAREN);
    if unlikely(!TPPLexer_Eval(&val)) {err_cnttokf: breakf(); return TOK_ERR; }
    if unlikely(val.c_kind != TPP_CONST_STRING &&
               !WARN(W_EXPECTED_STRING_AFTER_TPP_CNTTOK,&val)) goto err_cnttokf;
    if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
    popf(); intval = 0;
    if unlikely(val.c_kind != TPP_CONST_STRING) goto create_int_file;
    cnt_file = TPPFile_NewExplicitInherited(val.c_data.c_string);
    if unlikely(!cnt_file) { TPPString_Decref(val.c_data.c_string); goto seterr; }
    /* Push the count-file and yield all tokens inside. */
    pushfile_inherited(cnt_file);
    pusheob();
    while (TPPLexer_YieldRaw() > 0) ++intval;
    popeob();
    assert(TOKEN.t_file == cnt_file);
    TOKEN.t_file = cnt_file->f_prev;
    TPPFile_Decref(cnt_file);
    goto create_int_file;
   } break;

   { /* Expand into a random integral. */
    int_t random_begin,random_end;
    struct TPPConst val;
   case KWD___TPP_RANDOM:
    if (!HAVE_EXTENSION_TPP_RANDOM) break;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK == '(') YIELD();
    else WARN(W_EXPECTED_LPAREN);
    /* Evaluate a constant preprocessor-expression. */
    if unlikely(!TPPLexer_Eval(&val)) {err_randomf: breakf(); return TOK_ERR; }
    TPPConst_ToInt(&val);
    if (TOK == ',') {
     YIELD();
     random_begin = val.c_data.c_int;
     if unlikely(!TPPLexer_Eval(&val)) goto err_randomf;
     TPPConst_ToInt(&val);
     random_end = val.c_data.c_int;
    } else {
     random_begin = 0;
     random_end = val.c_data.c_int;
    }
    popf();
    if (TOK != ')') {
     WARN(W_EXPECTED_RPAREN);
     TOKEN.t_file->f_pos = TOKEN.t_begin;
    }
    /* Generate a random integer between 'random_begin..random_end-1' */
    if (random_begin >= random_end) intval = random_begin;
    else {
     if (!(CURRENT.l_flags&TPPLEXER_FLAG_RANDOM_INITIALIZED)) {
      /* Initialize the RNG generate. */
      srand((unsigned int)time(NULL));
      CURRENT.l_flags |= TPPLEXER_FLAG_RANDOM_INITIALIZED;
     }
     random_end -= random_begin;
     /* NOTE: The module here ~should~ be unnecessary
      *      (But I don't trust nofin I didn't screw up myself)... */
     intval      = (((int_t)rand()*random_end)/RAND_MAX) % random_end;
     intval     += random_begin;
    }
    goto create_int_file;
   } break;

   { /* Expand a given string into the contained tokens. */
    struct TPPConst val;
   case KWD___TPP_STR_DECOMPILE:
    if (!HAVE_EXTENSION_TPP_STR_DECOMPILE) break;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK == '(') YIELD();
    else WARN(W_EXPECTED_LPAREN);
    /* Evaluate a constant preprocessor-expression. */
    /* WARNING: 'val' may be the unmodified text of the calling file (as loaded by '__TPP_LOAD_FILE').
     *           In that case, we have no way of detecting a possible infinite recursion, essentially
     *           meaning that you probably just shouldn't do that... */
    if unlikely(!TPPLexer_Eval(&val)) { breakf(); return TOK_ERR; }
    if (TOK != ')') {
     WARN(W_EXPECTED_RPAREN);
     TOKEN.t_file->f_pos = TOKEN.t_begin;
    }
    popf();
    if (val.c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_AFTER_TPP_STRD,&val);
     goto again;
    } else if (!val.c_data.c_string->s_size) {
     /* Special case: Empty string (No need to push anything) */
     TPPString_Decref(val.c_data.c_string);
     goto again;
    }
    string_text = val.c_data.c_string; /*< Inherit reference. */
    goto create_string_file;
   } break;

   { /* Pack a string from a variadic list of comma-separated integrals. */
    struct TPPConst val; size_t allocsize,cursize;
    struct TPPString *newtext; size_t reqsize; char ch;
   case KWD___TPP_STR_PACK:
    if (!HAVE_EXTENSION_TPP_STR_PACK) break;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK == '(') YIELD();
    else WARN(W_EXPECTED_LPAREN);
    allocsize = cursize = 0,string_text = NULL;
    TPPString_Incref(empty_string);
    while (TOK != ')') {
     if unlikely(!TPPLexer_Eval(&val)) { breakf(); return TOK_ERR; }
     if (TOK == ',') YIELD();
     TPPConst_ToInt(&val);
     ch = (char)val.c_data.c_int;
     reqsize = cursize+TPP_SizeofEscape(&ch,1);
     if (reqsize > allocsize) {
      do allocsize = allocsize ? allocsize*2 : 2;
      while (reqsize > allocsize);
      newtext = (struct TPPString *)realloc(string_text,TPP_OFFSETOF(struct TPPString,s_text)+
                                           (allocsize+3)*sizeof(char));
      if unlikely(!newtext) { breakf(); goto seterr; }
      string_text = newtext;
     }
     TPP_Escape(string_text->s_text+(cursize+1),&ch,1);
     cursize = reqsize;
    }
    popf();
    if (string_text) {
     string_text->s_refcnt = 1;
     string_text->s_size = cursize+2;
     string_text->s_text[cursize+2] = '\0';
    } else {
     assert(!cursize);
     string_text = TPPString_NewSized(2);
     if unlikely(!string_text) goto seterr;
    }
    /* Fill in common text data. */
    string_text->s_text[0]         = '\"';
    string_text->s_text[cursize+1] = '\"';
    goto create_string_file;
   } break;

   { /* Returns a sub-portion of a given string, either
      * encoded as character, or as another string.
      * NOTE: Except for the encode-as-char functionality,
      *       this is only here for backwards-compatibility. */
    char escape_char; struct TPPConst val;
    /*ref*/struct TPPString *basestring;
    size_t subindex,sublen,escape_size;
    char *sub_begin,*sub_end;
   case KWD___TPP_STR_AT:
   case KWD___TPP_STR_SUBSTR:
    if (!HAVE_EXTENSION_TPP_STR_SUBSTR) break;
    escape_char = (char)(TOK == KWD___TPP_STR_AT ? '\'' : '\"');
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN);
    else YIELD();
    if unlikely(!TPPLexer_Eval(&val)) { breakf(); return TOK_ERR; }
    if (val.c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_AFTER_TPP_STRAT,&val);
     basestring = empty_string;
     TPPString_Incref(empty_string);
    } else {
     basestring = val.c_data.c_string; /*< Inherit reference. */
    }
    if (TOK != ',') WARN(W_EXPECTED_COMMA);
    else YIELD();
    if unlikely(!TPPLexer_Eval(&val)) {
err_substrf: breakf();
err_substr:  TPPString_Decref(basestring);
     goto seterr;
    }
    TPPConst_ToInt(&val);
    subindex = 0;
    if (basestring->s_size) {
     val.c_data.c_int %= (int_t)basestring->s_size;
     if (val.c_data.c_int < 0) val.c_data.c_int += (int_t)basestring->s_size;
     subindex = (size_t)val.c_data.c_int;
    }
    if (TOK != ',') {
     sublen = basestring->s_size ? 1u : 0u;
    } else {
     YIELD();
     if unlikely(!TPPLexer_Eval(&val)) goto err_substrf;
     TPPConst_ToInt(&val);
     sublen = 0;
     if (basestring->s_size) {
      val.c_data.c_int %= (int_t)basestring->s_size;
      if (val.c_data.c_int < 0) val.c_data.c_int += basestring->s_size;
      sublen = (size_t)val.c_data.c_int;
     }
    }
    if (sublen >= basestring->s_size-subindex) sublen = basestring->s_size-subindex;
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    popf();
    sub_end = (sub_begin = basestring->s_text+subindex)+sublen;
    assert(sub_begin <= sub_end);
    assert(sub_begin >= basestring->s_text);
    assert(sub_end <= basestring->s_text+basestring->s_size);
    escape_size = TPP_SizeofEscape(sub_begin,(size_t)(sub_end-sub_begin));
    string_text = TPPString_NewSized(escape_size+2);
    if unlikely(!string_text) goto err_substr;
    string_text->s_text[0] = escape_char;
    TPP_Escape(string_text->s_text+1,sub_begin,(size_t)(sub_end-sub_begin));
    string_text->s_text[escape_size+1] = escape_char;
    TPPString_Decref(basestring);
    goto create_string_file;
   } break;

   { /* Returns the size (in characters) of a given string.
      * NOTE: This is only here for backwards-compatibility. */
    struct TPPConst val;
   case KWD___TPP_STR_SIZE:
    if (!HAVE_EXTENSION_TPP_STR_SIZE) break;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN);
    else YIELD();
    if unlikely(!TPPLexer_Eval(&val)) { breakf(); return TOK_ERR; }
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    popf();
    if (val.c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_AFTER_TPP_STRAT,&val);
     intval = 0;
    } else {
     intval = (int_t)val.c_data.c_string->s_size;
     TPPString_Decref(val.c_data.c_string);
    }
    goto create_int_file;
   } break;

   { /* Various (true) predefined macros. */
    struct TPPExplicitFile *predefined_macro;
    /* Predefined macros. */
#define BUILTIN_MACRO(name,value) case name:\
{ static struct { refcnt_t a; size_t b; char c[COMPILER_STRLEN0(value)]; }\
  text##name = {0x80000000,COMPILER_STRLEN(value),value};\
  static struct TPPExplicitFile predef##name = {\
   0x80000000,TPPFILE_KIND_EXPLICIT,NULL,(char *)"",0,EMPTY_STRING_HASH,\
   (struct TPPString *)&text##name,text##name.c,text##name.c+COMPILER_STRLEN(value),NULL};\
  predefined_macro = &predef##name;\
  goto predef_macro;\
}
#include "tpp-defs.inl"
#undef BUILTIN_MACRO
    ;
predef_macro:
    if (!TPP_ISBUILTINMACRO(TOK)) break;
    predefined_macro->f_pos = predefined_macro->f_begin;
    pushfile((struct TPPFile *)predefined_macro);
    goto again;
   } break;

#if !TPP_CONFIG_MINMACRO || TPP_CONFIG_MINGCCFUNC >= 2
   { /* Builtin macro functions for generating integral constants. */
    char const *suffix;
    struct TPPString *argstring;
    struct TPPFile *argfile;
    size_t argsize,suffix_size;
   case KWD___INT8_C:   case KWD___UINT8_C:
   case KWD___INT16_C:  case KWD___UINT16_C:
   case KWD___INT32_C:  case KWD___UINT32_C:
   case KWD___INT64_C:  case KWD___UINT64_C:
   case KWD___INTMAX_C: case KWD___UINTMAX_C:
#ifdef HAVE_EXTENSION_UTILITY_MACROS
    if (!HAVE_EXTENSION_UTILITY_MACROS) break;
#endif
    suffix = intc_suffix[TOK-KWD___INT8_C];
    if (!HAVE_EXTENSION_MSVC_FIXED_INT) suffix += strlen(suffix)+1;
    pushf();
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF);
    YIELD();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN);
    else YIELD();
    suffix_size = strlen(suffix);
    argsize     = (size_t)(TOKEN.t_end-TOKEN.t_begin);
    if (TOK == ')') argsize = 0;
    argstring   = TPPString_NewSized(argsize+suffix_size);
    if unlikely(!argstring) { breakf(); goto seterr; }
    memcpy(argstring->s_text,TOKEN.t_begin,argsize*sizeof(char));
    memcpy(argstring->s_text+argsize,suffix,suffix_size*sizeof(char));
    if (TOK != ')') YIELD();
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    popf();
    argfile = TPPFile_NewExplicitInherited(argstring);
    if unlikely(!argfile) { TPPString_Decref(argstring); goto seterr; }
    pushfile_inherited(argfile);
    goto again;
   } break;
#endif /* !TPP_CONFIG_MINMACRO */

   default: break;
  }
 }
end:
 return result;
seterr:
 TPPLexer_SetErr();
 return TOK_ERR;
}

struct argcache_t {
 union{ char *ac_begin; uintptr_t ac_offset_begin; } TPP_UNNAMED_UNION_DEF(ac_specific_begin);
 union{ char *ac_end;   uintptr_t ac_offset_end;   } TPP_UNNAMED_UNION_DEF(ac_specific_end);
 char  *ac_expand_begin; /*< [?..1][owned] Dynamically allocated buffer for the expanded version of the text. */
 size_t ac_expand_size;
};
#if !TPP_HAVE_UNNAMED_UNION
#define ac_begin         ac_specific_begin.ac_begin
#define ac_end           ac_specific_end.ac_end
#define ac_offset_begin  ac_specific_begin.ac_offset_begin
#define ac_offset_end    ac_specific_end.ac_offset_end
#endif


/* Expand the given 'text_begin..text_size' using regular
 * macro and preprocessor expansion, in the process clobbering
 * the lexer's current file's text and  */
LOCAL int
argcache_genexpand(struct argcache_t *__restrict self,
                   char *text_begin, char *text_end) {
 struct TPPFile *file;
 char old_end;
 char *buf_begin,*buf_end,*buf_pos;
 char *tok_begin,*tok_end,*new_buf;
 size_t reqsize,sizeavail,newsize;
 file = TOKEN.t_file;
 assert(file);
 assert(file->f_text);
 assert(text_begin <= text_end);
 assert(text_begin >= file->f_text->s_text);
 assert(text_end   <= file->f_text->s_text+
                      file->f_text->s_size);
 assert(file == CURRENT.l_eob_file);
 /* Clobber the file pointers. */
 /*file->f_begin =*/
 file->f_pos   = text_begin;
 file->f_end   = text_end;
 old_end = *text_end,*text_end = '\0';
 buf_begin = buf_end = buf_pos = NULL;
 /* Parse tokens until EOF is reached, appending the begin..end
  * string regions of each to the expansion buffer.
  * NOTE: We use yield to allow for recursion, as well as expansion of macros. */
 while (TPPLexer_Yield()) {
  if unlikely(TOK < 0) goto err_buffer;
  /* Skip empty buffers. */
  if ((tok_begin = TOKEN.t_begin) ==
      (tok_end = TOKEN.t_end)) continue;
  assert(tok_end > tok_begin);
  reqsize = (size_t)(tok_end-tok_begin);
  sizeavail = (size_t)(buf_end-buf_pos);
  if (reqsize > sizeavail) {
   sizeavail = (size_t)(buf_pos-buf_begin)+reqsize; /* Minimum size. */
   newsize   = (size_t)(buf_end-buf_begin);
   newsize   = newsize ? newsize*2 : 2;
   while (newsize < sizeavail) newsize *= 2;
   new_buf = (char *)realloc(buf_begin,newsize*sizeof(char));
   if unlikely(!new_buf) goto err_buffer;
   buf_pos   = new_buf+(buf_pos-buf_begin);
   buf_begin = new_buf;
   buf_end   = new_buf+newsize;
  }
  memcpy(buf_pos,tok_begin,reqsize*sizeof(char));
  buf_pos += reqsize;
  assert(buf_pos <= buf_end);
 }
 /* NOTE: Not ZERO-terminated! */
 self->ac_expand_size = (size_t)(buf_pos-buf_begin);
 if (buf_pos != buf_end) {
  newsize = self->ac_expand_size;
  assert(newsize);
  /* Free up some unused buffer memory. */
  new_buf = (char *)realloc(buf_begin,newsize*sizeof(char));
  if (!new_buf) new_buf = buf_begin;
  self->ac_expand_begin = new_buf;
 } else {
  self->ac_expand_begin = buf_begin;
 }
 assert((self->ac_expand_begin == NULL) ==
        (self->ac_expand_size == 0));
 assert(TOKEN.t_file       == file);
 assert(CURRENT.l_eob_file == file);
 assert(!TOK);
 *text_end = old_end;
 return 1;
err_buffer:
 free(buf_begin);
 *text_end = old_end;
 return 0;
}


PRIVATE /*ref*/struct TPPFile *
expand_function_macro_impl(struct TPPFile *__restrict macro,
                           struct argcache_t *__restrict argv,
                           size_t va_size) {
 struct TPPFile *result;
 struct TPPString *result_text;
 struct argcache_t *arg_iter;
 struct arginfo_t *iter,*end,*begin;
 char *dest_iter,*dest_end,*source_iter,*source_end;
 size_t expanded_text_size; funop_t *code;
 size_t expanded_string_size;
 assert(macro);
 assert(macro->f_kind == TPPFILE_KIND_MACRO);
 assert((macro->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION);
 assert(macro->f_text);
 assert(macro->f_begin <= macro->f_end);
 assert(macro->f_begin >= macro->f_text->s_text);
 assert(macro->f_begin <= macro->f_text->s_text+macro->f_text->s_size);
 assert(macro->f_end >= macro->f_text->s_text);
 assert(macro->f_end <= macro->f_text->s_text+macro->f_text->s_size);
 code = macro->f_macro.m_function.f_expand;
 assert(code);
 if (*code == TPP_FUNOP_END) {
  /* Special optimization for macros not making use of their arguments. */
  result = (struct TPPFile *)malloc(TPPFILE_SIZEOF_MACRO_EXPANDED);
  if unlikely(!result) return NULL;
  result_text = macro->f_text;
  TPPString_Incref(result_text);
  result->f_text  = result_text; /*< Inherit reference. */
  /* Setup the resulting macro to reference the original's text. */
  result->f_pos   =
  result->f_begin = macro->f_begin;
  result->f_end   = macro->f_end;
  /* Sub-values to not cleanup anything below. */
  begin = end = NULL,argv = NULL;
  goto result_common;
 }

 /* Figure out what we need to know and precalculate that. */
 end = (iter = begin = macro->f_macro.m_function.f_arginfo)+
                       macro->f_macro.m_function.f_argc;
 arg_iter            = argv;
 expanded_text_size  = (size_t)(macro->f_end-macro->f_begin);
 expanded_text_size -= macro->f_macro.m_function.f_deltotal;
 for (; iter != end; ++iter,++arg_iter) {
  if (iter->ai_ins_str) {
   /* Figure out and cache how long the stringy-fied version of this is. */
   expanded_string_size = 2+TPP_SizeofEscape(arg_iter->ac_begin,
                                            (size_t)(arg_iter->ac_end-arg_iter->ac_begin));
   expanded_text_size += (iter->ai_ins_str*expanded_string_size);
  }
  if (iter->ai_ins_exp) {
   /* Expand and cache the argument. */
   /* todo: maybe optimize the memory footprint of this by not
    *       ~really~ needing to create an explicit malloc()-block
    *       when the argument's expanded text doesn't differ
    *       from the original. (e.g.: simple arguments like 'add(10,20)') */
   if (!argcache_genexpand(arg_iter,arg_iter->ac_begin,arg_iter->ac_end)) goto err_argcache;
   expanded_text_size += (iter->ai_ins_exp*arg_iter->ac_expand_size);
  }
  if (iter->ai_ins) {
   expanded_text_size += (iter->ai_ins*(size_t)
                         (arg_iter->ac_end-arg_iter->ac_begin));
  }
 }
 /* Adjust for __VA_COMMA__ and __VA_NARGS__. */
 if (va_size != 0) expanded_text_size += macro->f_macro.m_function.f_n_vacomma;
 if (macro->f_macro.m_function.f_n_vanargs) {
  expanded_text_size += macro->f_macro.m_function.f_n_vanargs*TPP_SizeofItos(va_size);
 }

 /* Allocate the actual text-buffer of the resulting expanded macro text. */
 result_text = TPPString_NewSized(expanded_text_size);
 if unlikely(!result_text) goto err_argcache_full;
 dest_iter   = result_text->s_text;
 dest_end    = dest_iter+expanded_text_size;
 source_iter = macro->f_begin;
 source_end  = macro->f_end;
#define ARGC           (macro->f_macro.m_function.f_argc)
#define ARGINFO(argi)  (begin[argi])
#define ARGCACHE(argi) (argv[argi])
 /* We know all the performance characteristics for the arguments. */
 for (;;) {
  size_t arg,temp;
#if TPP_CONFIG_DEBUG
  uint8_t *op_start = code;
# define DBG_TEXT    "[%x | %lx..%lx (%lu) <%s>] "
# define DBG_DATA    (unsigned int)*op_start,\
                     (unsigned long)(op_start-macro->f_macro.m_function.f_expand),\
                     (unsigned long)(code-macro->f_macro.m_function.f_expand),\
                     (unsigned long)(code-op_start),\
                      tpp_hexrepr(op_start,(size_t)(code-op_start)),
#else
# define DBG_TEXT    /* nothing */
# define DBG_DATA    /* nothing */
#endif
  assertf(dest_iter   <= dest_end,
         (DBG_TEXT "Destination iterator %p is out-of-bounds from %p by %lu characters",
          DBG_DATA dest_iter,dest_end,
         (unsigned long)(dest_iter-dest_end)));
  assertf(source_iter <= source_end,
         (DBG_TEXT "Source iterator %p is out-of-bounds from %p by %lu characters",
          DBG_DATA source_iter,source_end,
         (unsigned long)(source_iter-source_end)));
  switch (*code++) {

   case TPP_FUNOP_ADV:
    /* Advance: Simply copy text from src --> dst. */
    arg = funop_getarg(code);
    assertf(arg <= (size_t)(dest_end-dest_iter),
           (DBG_TEXT "Insufficient memory for text advancing in DST (Required: %lu; Available: %lu; dest_iter: %p)",
            DBG_DATA (unsigned long)(arg),
                     (unsigned long)(dest_end-dest_iter),dest_iter));
    assertf(arg <= (size_t)(source_end-source_iter),
           (DBG_TEXT "Insufficient memory for text advancing in SRC (Required: %lu; Available: %lu)",
            DBG_DATA (unsigned long)(arg),
                     (unsigned long)(source_end-source_iter)));
    memcpy(dest_iter,source_iter,arg*sizeof(char));
    dest_iter   += arg;
    source_iter += arg;
    break;

   case TPP_FUNOP_INS:
    /* INSERT: Insert an argument without expansion. */
    arg = funop_getarg(code);
    assertf(arg < ARGC,
           (DBG_TEXT "Out-of-bound argument index %lu is greater than %lu",
            DBG_DATA (unsigned long)arg,(unsigned long)ARGC));
    arg_iter = &ARGCACHE(arg);
    temp = (size_t)(arg_iter->ac_end-arg_iter->ac_begin);
    assert((size_t)(dest_end-dest_iter) >= temp);
    memcpy(dest_iter,arg_iter->ac_begin,temp*sizeof(char));
    dest_iter += temp;
    goto advance_src;

   case TPP_FUNOP_INS_EXP:
    /* INSERT_EXP: Insert an argument with expansion. */
    arg = funop_getarg(code);
    assertf(arg < ARGC,
           (DBG_TEXT "Out-of-bound argument index %lu is greater than %lu",
            DBG_DATA (unsigned long)arg,
                     (unsigned long)ARGC));
    arg_iter = &ARGCACHE(arg);
    assertf((size_t)(dest_end-dest_iter) >= arg_iter->ac_expand_size,
           (DBG_TEXT "Insufficient memory for expanded argument insertion (Required: %lu; Available: %lu)",
            DBG_DATA (unsigned long)(arg_iter->ac_expand_size),
                     (unsigned long)(dest_end-dest_iter)));
    memcpy(dest_iter,arg_iter->ac_expand_begin,
           arg_iter->ac_expand_size*sizeof(char));
    dest_iter += arg_iter->ac_expand_size;
    goto advance_src;

    if (FALSE) { case TPP_FUNOP_INS_STR: temp = '\"'; }
    if (FALSE) { case TPP_FUNOP_INS_CHR: temp = '\''; }
    assert(dest_iter < dest_end);
    *dest_iter++ = (char)temp;
    arg = funop_getarg(code);
    assertf(arg < ARGC,
           (DBG_TEXT "Out-of-bound argument index %lu is greater than %lu",
            DBG_DATA (unsigned long)arg,(unsigned long)ARGC));
    arg_iter = &ARGCACHE(arg);
    assertf((size_t)(dest_end-dest_iter) >=
             TPP_SizeofEscape(arg_iter->ac_begin,
                             (size_t)(arg_iter->ac_end-
                                      arg_iter->ac_begin)),
            (DBG_TEXT "Insufficient memory for string escape (available: %lu; required: %lu)",
             DBG_DATA (unsigned long)(dest_end-dest_iter),
                      (unsigned long)TPP_SizeofEscape(arg_iter->ac_begin,
                                                     (size_t)(arg_iter->ac_end-
                                                              arg_iter->ac_begin))));
    /* Insert the escaped, non-expanded argument text. */
    dest_iter = TPP_Escape(dest_iter,arg_iter->ac_begin,
                          (size_t)(arg_iter->ac_end-arg_iter->ac_begin));
    assert(dest_iter < dest_end);
    *dest_iter++ = (char)temp;
    goto advance_src;

   case TPP_FUNOP_DEL:
advance_src:
    /* Delete: Simply advance the source text pointer. */
    arg = funop_getarg(code);
    assertf(arg <= (size_t)(source_end-source_iter),
           (DBG_TEXT "Delete operand %lu larger than %lu",
            DBG_DATA (unsigned long)(arg),
                     (unsigned long)(source_end-source_iter)));
    source_iter += arg;
    break;

   case TPP_FUNOP_VA_COMMA:
    /* VA_COMMA: Insert a ',' character if va_size is non-empty. */
    if (va_size) *dest_iter++ = ',';
    goto advance_src;

   case TPP_FUNOP_VA_NARGS:
    /* VA_NARGS: Insert an integral representation of the variadic argument size. */
    assertf((size_t)(dest_end-dest_iter) >= TPP_SizeofItos((int_t)va_size),
           (DBG_TEXT "Insufficient buffer space for ITOS (remaining: %lu; required: %lu)",
            DBG_DATA (unsigned long)(dest_end-dest_iter),
                     (unsigned long)TPP_SizeofItos((int_t)va_size)));
    dest_iter = TPP_Itos(dest_iter,(int_t)va_size);
    goto advance_src;

   default:
    assertf(code[-1] == TPP_FUNOP_END,
           (DBG_TEXT "Invalid trailing opcode %x",
            DBG_DATA (int)code[-1]));
    goto done_exec;
  }
#undef DBG_DATA
#undef DBG_TEXT
 }
done_exec:
 /* Copy the remaining source text. */
 assertf(dest_iter <= dest_end,
        ("The final DST iterator %p is out-of-bounds of %p by %lu characters",
         dest_iter,dest_end,(unsigned long)(dest_iter-dest_end)));
 assertf(source_iter <= source_end,
        ("The final SRC iterator %p is out-of-bounds of %p by %lu characters",
         source_iter,source_end,(unsigned long)(source_iter-source_end)));
 assertf((size_t)(dest_end-dest_iter) ==
         (size_t)(source_end-source_iter),
        ("Difference between the overflow buffer sizes (%lu != %lu).\n"
         "This means that either the cache generator, or interpreter is flawed.",
        (unsigned long)(dest_end-dest_iter),
        (unsigned long)(source_end-source_iter)));
 memcpy(dest_iter,source_iter,(size_t)(source_end-source_iter));

 result = (struct TPPFile *)malloc(TPPFILE_SIZEOF_MACRO_EXPANDED);
 if unlikely(!result) goto err_text;
 /* Fill in members of the resulting expanded macro. */
 result->f_text     = result_text; /*< Inherit reference. */
 result->f_pos      =
 result->f_begin    = result_text->s_text;
 assertf(dest_end == result_text->s_text+
                     result_text->s_size,
        ("The destination end pointer %p isn't the end of the test at %p.\n"
         "%ld bytes remain unused",dest_end,result_text->s_text+result_text->s_size,
        (long)((result_text->s_text+result_text->s_size)-dest_end)));
 result->f_end      = dest_end;
result_common:
 result->f_refcnt   = 1;
 result->f_kind     = TPPFILE_KIND_MACRO;
 result->f_name     = macro->f_name;
 result->f_namesize = macro->f_namesize;
 result->f_namehash = macro->f_namehash;
 result->f_macro.m_flags     = TPP_MACROFILE_KIND_EXPANDED; /* NOTE: Don't set the owns-name flag. */
 result->f_macro.m_deffile   = macro->f_macro.m_deffile;
 result->f_macro.m_defloc    = macro->f_macro.m_defloc;
 result->f_macro.m_pushprev  = NULL;
 result->f_macro.m_pushcount = 0;
 TPPFile_Incref(result->f_macro.m_deffile);
 TPPFile_Incref(macro);
 result->f_macro.m_expand.e_expand_origin = macro; /*< Inherit reference. */
 /* Track the amount of times this macro is being expanded. */
 ++macro->f_macro.m_function.f_expansions;
#undef ARGCACHE
#undef ARGINFO
#undef ARGC
 /* Free the expansion buffers. */
 iter = begin,arg_iter = argv;
 for (; iter != end; ++iter,++arg_iter) {
  if (iter->ai_ins_exp) free(arg_iter->ac_expand_begin);
 }
 return result;
/*err_r:  free(result);*/
err_text: free(result_text);
err_argcache_full:
 arg_iter = argv+macro->f_macro.m_function.f_argc;
 iter     = begin+macro->f_macro.m_function.f_argc;
err_argcache:
 while (arg_iter-- != argv) {
  /* Free the expansion buffer. */
  if ((--iter)->ai_ins_exp) free(arg_iter->ac_expand_begin);
 }
 return NULL;
}

PRIVATE /*ref*/struct TPPFile *
expand_function_macro(struct TPPFile *__restrict macro,
                      struct argcache_t *__restrict argv,
                      struct TPPFile *__restrict arguments_file,
                      size_t va_size) {
 struct TPPFile *result,*old_eob;
 struct TPPString *old_text;
 char *old_begin,*old_end,*old_pos;
 /* Enable all tokens to properly include _everything_ when expanding arguments. */
 CURRENT.l_flags |= TPPLEXER_FLAG_WANTCOMMENTS|
                    TPPLEXER_FLAG_WANTSPACE|
                    TPPLEXER_FLAG_WANTLF|
                    TPPLEXER_FLAG_COMMENT_NOOWN_LF;
 /* We use explicit EOB by setting the 'l_eob_file' field below! */
 CURRENT.l_flags &= ~(TPPLEXER_FLAG_NO_SEEK_ON_EOB);
 /* NOTE: The expansion implementation screws with the current lexer's
  *       top file in order to easily expand arguments during insertion.
  *       Since I don't want to bother with restoring it above, a
  *       backup the current file's begin/end/pos and text pointers
  *       is made here and restored when the function returns.
  * HINT: No need to deal with ensuring the top file isn't popped,
  *       as we are setting the 'l_eob_file' field, meaning that
  *       it will not be removed with incorrect settings.
  */
 assert(arguments_file == TOKEN.t_file);
 old_eob            = CURRENT.l_eob_file;
 old_text           = arguments_file->f_text;
 old_begin          = arguments_file->f_begin;
 old_end            = arguments_file->f_pos;
 old_pos            = arguments_file->f_end;
 CURRENT.l_eob_file = arguments_file;
 result = expand_function_macro_impl(macro,argv,va_size);
 if likely(result) {
  /* NOTE: Must not restore data if an error occurred! */
  assert(TOKEN.t_file == arguments_file);
  assert(CURRENT.l_eob_file == arguments_file);
  CURRENT.l_eob_file      = old_eob;
  arguments_file->f_text  = old_text;
  arguments_file->f_begin = old_begin;
  arguments_file->f_pos   = old_end;
  arguments_file->f_end   = old_pos;
 }
 return result;
}

struct incback_slot_t {
 /*ref*/struct TPPFile *is_file; /*< [1..1] Backup file pointer. */
 char                  *is_fpos; /*< [1..1] Old 'f_pos' pointer. */
};
#define incback_slot_quit(self) TPPFile_Decref((self)->is_file)

PRIVATE void
incback_slot_backup(struct incback_slot_t *__restrict self) {
 assert(self);
 assert(TOKEN.t_file);
 assert(TOKEN.t_file != &TPPFile_Empty);
 assert(TOKEN.t_file->f_prev);
 assert(TOKEN.t_file->f_kind == TPPFILE_KIND_MACRO);
 /* Backup and transfer one file into this include backup. */
 self->is_file = TOKEN.t_file; /*< Inherit reference. */
 self->is_fpos = TOKEN.t_file->f_pos;
 TOKEN.t_file = TOKEN.t_file->f_prev;
#if TPP_CONFIG_DEBUG
 self->is_file->f_prev = NULL;
#endif
}
PRIVATE void
incback_slot_restore(struct incback_slot_t *self) {
 struct TPPFile *file;
 assert(self);
 file = self->is_file;
 assert(file);
#if TPP_CONFIG_DEBUG
 assert(!file->f_prev);
#endif
 assert(self->is_fpos >= file->f_text->s_text);
 assert(self->is_fpos <= file->f_text->s_text+
                         file->f_text->s_size);
 file->f_pos  = self->is_fpos;
 file->f_prev = TOKEN.t_file; /*< Inherit reference. */
 TOKEN.t_file = file;         /*< Inherit reference. */
}

struct incback_t {
 size_t                 ib_morec;     /*< Amount of additional backup slots. */
 size_t                 ib_morea;     /*< Allocated amount of additional backup slots. */
 struct incback_slot_t *ib_morev;     /*< [0..ib_morec][owned] Vector of additional backup slots. */
 char                  *ib_tok_begin; /*< [1..1] Old 't_begin' pointer of the token. */
 char                  *ib_tok_end;   /*< [1..1] Old 't_end' pointer of the token. */
 struct TPPKeyword     *ib_tok_kwd;   /*< [1..1] Old 't_kwd' pointer of the token. */
 unsigned long          ib_tok_num;   /*< Old token number. */
 char                  *ib_args_fpos; /*< [1..1] Old 'f_pos' pointer of the arguments file. */
};

PRIVATE int
incback_pushdyn(struct incback_t *__restrict self) {
 struct incback_slot_t *slot;
 /* Difficult case: Must allocate a dynamic backup buffer entry. */
 if unlikely(self->ib_morec == self->ib_morea) {
  self->ib_morea = self->ib_morea ? self->ib_morea*2 : 2;
  slot = (struct incback_slot_t *)realloc(self->ib_morev,self->ib_morea*
                                          sizeof(struct incback_slot_t));
  if unlikely(!slot) return 0;
  self->ib_morev = slot;
 } else {
  slot = self->ib_morev;
 }
 slot += self->ib_morec++;
 incback_slot_backup(slot);
 return 1;
}
PRIVATE int
incback_init(struct incback_t *__restrict self,
             struct TPPFile *__restrict new_top_file) {
 assert(self);
 assert(TPP_ISKEYWORD(TOKEN.t_id));
 assert(TOKEN.t_kwd);
 assert(TOKEN.t_kwd->k_id == TOKEN.t_id);
 self->ib_morec = self->ib_morea = 0;
 self->ib_morev = NULL;
 while (TOKEN.t_file != new_top_file) {
  if (!incback_pushdyn(self)) return 0;
 }
 self->ib_tok_begin = TOKEN.t_begin;
 self->ib_tok_end   = TOKEN.t_end;
 self->ib_tok_kwd   = TOKEN.t_kwd;
 self->ib_tok_num   = TOKEN.t_num;
 self->ib_args_fpos = new_top_file->f_pos;
 return 1;
}
PRIVATE void
incback_quit(struct incback_t *__restrict self) {
 /* Destroy all include-backups. */
 struct incback_slot_t *iter,*begin;
 iter = (begin = self->ib_morev)+self->ib_morec;
 while (iter-- != begin) incback_slot_quit(iter);
 free(begin);
}

PRIVATE void
incback_restore(struct incback_t *__restrict self,
                struct TPPFile *__restrict arguments_file) {
 /* Restore all include-backups. */
 struct incback_slot_t *iter,*begin;
 iter = (begin = self->ib_morev)+self->ib_morec;
 while (iter-- != begin) incback_slot_restore(iter);
 free(begin);
 self->ib_morec = self->ib_morea = 0;
 self->ib_morev = NULL;
 assert(self->ib_tok_begin <= self->ib_tok_end);
 assert(self->ib_tok_begin >= TOKEN.t_file->f_begin);
 assert(self->ib_tok_begin <= TOKEN.t_file->f_end);
 assert(self->ib_tok_end   >= TOKEN.t_file->f_begin);
 assert(self->ib_tok_end   <= TOKEN.t_file->f_end);
 TOKEN.t_begin = self->ib_tok_begin;
 TOKEN.t_end   = self->ib_tok_end;
 TOKEN.t_kwd   = self->ib_tok_kwd;
 TOKEN.t_num   = self->ib_tok_num;
 TOKEN.t_id    = self->ib_tok_kwd->k_id;
 assert(self->ib_args_fpos >= arguments_file->f_begin);
 assert(self->ib_args_fpos <= arguments_file->f_end);
 arguments_file->f_pos = self->ib_args_fpos;
}


PRIVATE int
compare_text(char const *__restrict old_text,
             char const *__restrict new_text,
             size_t text_size) {
 char const *oend = old_text+text_size;
 for (; old_text != oend; ++old_text,++new_text) {
  /* NOTE: Must ignore \0-characters in the old text! */
  if (*old_text && *old_text != *new_text) return 0;
 }
 return 1;
}


/* Parenthesis recursion IDs. */
#define RECURSION_OF(flag)((flag) >> 12)
#define RECURSION_PAREN   RECURSION_OF(TPP_MACROFILE_FUNC_START_LPAREN)
#define RECURSION_BRACKET RECURSION_OF(TPP_MACROFILE_FUNC_START_LBRACKET)
#define RECURSION_BRACE   RECURSION_OF(TPP_MACROFILE_FUNC_START_LBRACE)
#define RECURSION_ANGLE   RECURSION_OF(TPP_MACROFILE_FUNC_START_LANGLE)

PUBLIC int
TPPLexer_ExpandFunctionMacro(struct TPPFile *__restrict macro) {
 static tok_t const paren_begin[4] = {'(','[','{','<'};
 struct TPPFile *arguments_file,*expand_file;
 tok_t begin_token; char *iter,*end;
 struct incback_t popped_includes;
 int calling_conv,check_mirrors,result;
 assert(macro);
 assert(macro->f_kind == TPPFILE_KIND_MACRO);
 assert((macro->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_FUNCTION);
 /* If the macro is allowed to self-expand, we must check for mirrors later. */
 check_mirrors = macro->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_SELFEXPAND;
 arguments_file = TOKEN.t_file;
 assert(arguments_file);
 calling_conv = RECURSION_OF(macro->f_macro.m_flags&TPP_MACROFILE_MASK_FUNC_STARTCH);
 assert(calling_conv >= 0 && calling_conv <= 3);
 begin_token = paren_begin[calling_conv];
 /* Skip all whitespace while unwinding the #include
  * stack, trying to find the 'begin_token' character. */
 for (;;) {
  iter = arguments_file->f_pos;
  end = arguments_file->f_end;
  while (iter != end) {
   while (SKIP_WRAPLF(iter,end));
   if (!tpp_isspace(*iter)) goto at_next_non_whitespace;
   ++iter;
  }
  arguments_file = arguments_file->f_prev;
  /* Special case: The entire lexer ends before another non-whitespace token is encountered. */
  if unlikely(!arguments_file) return 2;
 }
at_next_non_whitespace:
 /* Check the next non-whitespace character following the
  * macro name to match what is required to being expansion. */
 if (*iter != begin_token) {
  if (!HAVE_FEATURE_TRIGRAPHS) return 2;
  if (*iter != '?' || iter[1] != '?') return 2;
  iter += 2; /* Check for special trigraph parenthesis. */
  if ((*iter != '(' || begin_token != '[') &&
      (*iter != '<' || begin_token != '{')) return 2;
 }

 ++iter; /* Skip the opening parenthesis. */
 /* The file we drop here must be restored if later
  * code decides that the macro should not be expanded.
  * Only then can we revert to the file position where
  * the macro's name is stored safely, thus allowing
  * the caller to continue yielding the tokens used
  * to call the macro, and not its expanded version.
  * HINT: This is only required for macros with the
  *      'TPP_MACROFILE_FLAG_FUNC_SELFEXPAND' flag set,
  *       because for non-self-expanding macros, we
  *       already know that expansion won't be stopped
  *       because another copy with the same arguments
  *       is already on the #include-stack.
  */
 if (check_mirrors) {
  if (macro->f_macro.m_function.f_expansions >= CURRENT.l_limit_mrec) {
   /* Make sure that this macro call doesn't exceed the current recursion limit. */
   if unlikely(!WARN(W_MACRO_RECURSION_LIMIT_EXCEEDED,macro)) return 0;
   return 2;
  }
  if (!incback_init(&popped_includes,arguments_file)) return 0;
 } else {
  struct TPPFile *nextfile;
  while (TOKEN.t_file != arguments_file) {
   nextfile = TOKEN.t_file->f_prev;
   TOKEN.t_file->f_prev = NULL;
   TPPFile_Decref(TOKEN.t_file);
   TOKEN.t_file = nextfile;
  }
 }
 /* We are supposed to expand this macro (an argument list was found)
  * >> Now we must commit the #include-unwinding until 'arguments_file',
  *    as well as set arguments_file's f_pos pointer to 'iter'. */
 assert(iter >= arguments_file->f_pos);
 assert(iter <= arguments_file->f_end);
 arguments_file->f_pos = iter;
 /* OK! The lexer's file pointer is now situated on the
  *     first character of the function's first argument.
  *     Yet in order to ensure proper expansion, we must
  *     make sure that the entirety of the argument list
  *     is preloaded within the same chunk.
  *  >> Though this can easily be achieved through calls
  *     to the 'TPPFile_ExtendChunk' function.
  * NOTE: As not to have to create another string and comment
  *       parser, we use the 'TPPLEXER_FLAG_NO_SEEK_ON_EOB'
  *       flag to be able to manually extend the current
  *       chunk to not have it be dropped on EOF.
  *       That way, we can also handle incomplete argument
  *       lists across multiple macros as errors and
  *       already start figuring out the pointer ranges of
  *       the various macro arguments.
  */
 pushf();
 CURRENT.l_flags |= TPPLEXER_FLAG_NO_SEEK_ON_EOB;
 {
  int paren_recursion[4];
  struct argcache_t *argv,*arg_iter,*arg_last,*arg_end;
  size_t effective_argc = macro->f_macro.m_function.f_argc;
  size_t va_size = 0; uintptr_t text_offset;
  TPPLexer_YieldPP();
  /* NOTE: In order to reduce special cases below, we simply parse
   *       one argument for no-args functions, then later check
   *       to make sure that that argument is empty (except for whitespace) */
  if (!effective_argc) effective_argc = 1;
  /* Allocate a local buffer used to track argument offsets. */
  if (macro->f_macro.m_function.f_argbuf) {
   /* Take the preallocated lazy buffer. */
   argv = (struct argcache_t *)macro->f_macro.m_function.f_argbuf;
   macro->f_macro.m_function.f_argbuf = NULL;
  } else {
#if TPP_CONFIG_DEBUG
   argv = (struct argcache_t *)calloc(effective_argc,
                                      sizeof(struct argcache_t));
#else
   argv = (struct argcache_t *)malloc(effective_argc*
                                      sizeof(struct argcache_t));
#endif
   if unlikely(!argv) goto end0;
  }
  arg_last = (arg_iter = argv)+(effective_argc-1);
  memset(paren_recursion,0,sizeof(paren_recursion));
  paren_recursion[calling_conv] = 1;
  arg_iter->ac_offset_begin = (size_t)(TOKEN.t_begin-TOKEN.t_file->f_text->s_text);
  for (;;) {
   assert(TOKEN.t_file  == arguments_file);
   assert(TOKEN.t_begin <= TOKEN.t_end);
   assert(TOKEN.t_begin >= TOKEN.t_file->f_begin);
   assert(TOKEN.t_end   <= TOKEN.t_file->f_end);
   switch (TOK) {

    case 0:
     /* Special case: Must load more data from the arguments file. */
     if (!TPPFile_NextChunk(arguments_file,TPPFILE_NEXTCHUNK_FLAG_EXTEND)) {
      assert(TOKEN.t_file == arguments_file);
      TOKEN.t_begin = TOKEN.t_end = arguments_file->f_end;
      arg_iter->ac_offset_end = (size_t)(TOKEN.t_begin-arguments_file->f_text->s_text);
      if (!WARN(W_EOF_IN_MACRO_ARGUMENT_LIST)) goto err_argv;
      goto done_args;
     }
     break;

     /* Increase parenthesis. */
    case '(': ++paren_recursion[RECURSION_PAREN];
              break;
    case '[': if (calling_conv >= RECURSION_BRACKET &&
                 !paren_recursion[RECURSION_PAREN]
                  ) ++paren_recursion[RECURSION_BRACKET];
              break;
    case '{': if (calling_conv >= RECURSION_BRACE &&
                 !paren_recursion[RECURSION_PAREN] &&
                 !paren_recursion[RECURSION_BRACKET]
                  ) ++paren_recursion[RECURSION_BRACE];
              break;
    case TOK_LOWER_EQUAL:   /* '<=' */
    case TOK_SHL_EQUAL:     /* '<<=' / '=<<' */
    case TOK_LANGLE3_EQUAL: /* '<<<=' / '=<<<' */
     if (calling_conv >= RECURSION_ANGLE &&
        !paren_recursion[RECURSION_PAREN] &&
        !paren_recursion[RECURSION_BRACKET] &&
        !paren_recursion[RECURSION_BRACE]) {
      paren_recursion[RECURSION_ANGLE] += TOK == TOK_LANGLE3_EQUAL ? 3 :
                                          TOK == TOK_SHL_EQUAL ? 2 : 1;
      arguments_file->f_pos = TOKEN.t_end-1;
      assert(*arguments_file->f_pos == '=' ||
             *arguments_file->f_pos == '<');
      if (*arguments_file->f_pos == '<') ++arguments_file->f_pos;
     }
     break;
    case '<':         /* '<' */
    case TOK_SHL:     /* '<<' */
    case TOK_LANGLE3: /* '<<<' */
     if (calling_conv >= RECURSION_ANGLE &&
        !paren_recursion[RECURSION_PAREN] &&
        !paren_recursion[RECURSION_BRACKET] &&
        !paren_recursion[RECURSION_BRACE]) {
      paren_recursion[RECURSION_ANGLE] += TOK == TOK_LANGLE3 ? 3 :
                                          TOK == TOK_SHL ? 2 : 1;
     }
     break;

     /* Reduce parenthesis. */
     if (FALSE) { case ')': --paren_recursion[RECURSION_PAREN]; }
     if (FALSE) { case ']': if (calling_conv < RECURSION_BRACKET ||
                                paren_recursion[RECURSION_PAREN]) break;
                            --paren_recursion[RECURSION_BRACKET]; }
     if (FALSE) { case '}': if (calling_conv < RECURSION_BRACE ||
                                paren_recursion[RECURSION_BRACKET] ||
                                paren_recursion[RECURSION_PAREN]) break;
                            --paren_recursion[RECURSION_BRACE]; }
     if (FALSE) { case TOK_SHR:           /* '>>' */
                  case TOK_SHR_EQUAL:     /* '>>=' / '=>>' */
                  case TOK_GREATER_EQUAL: /* '>=' */
                  case TOK_RANGLE3:       /* '>>>' */
                  case TOK_RANGLE3_EQUAL: /* '>>>=' / '=>>>' */
                  case '>': if (calling_conv < RECURSION_ANGLE ||
                                paren_recursion[RECURSION_BRACE] ||
                                paren_recursion[RECURSION_BRACKET] ||
                                paren_recursion[RECURSION_PAREN]) break;
                            assert(arguments_file->f_pos == TOKEN.t_end);
                            switch (TOK) {
                             default: --paren_recursion[RECURSION_ANGLE]; break;
                             case TOK_GREATER_EQUAL:
                              --paren_recursion[RECURSION_ANGLE];
                              --arguments_file->f_pos; /* Parse the '=' again. */
                              break;
                             case TOK_SHR:       /* '>>' */
                             case TOK_SHR_EQUAL: /* '>>=' / '=>>' */
                              if (paren_recursion[RECURSION_ANGLE] >= 2) {
                               paren_recursion[RECURSION_ANGLE] -= 2;
                               if (TOK == TOK_SHR_EQUAL) {
                                assert(arguments_file->f_pos[-1] == '=' ||
                                       arguments_file->f_pos[-1] == '>');
                                if (arguments_file->f_pos[-1] == '=')
                                  --arguments_file->f_pos; /* Parse the '=' again. */
                               }
                               ++TOKEN.t_begin;
                              } else {
                               --paren_recursion[RECURSION_ANGLE];
                               if (TOK == TOK_SHR) {
                                assert(arguments_file->f_pos[-1] == '>');
                                --arguments_file->f_pos; /* Parse the second '>' again. */
                               } else {
                                assert(arguments_file->f_pos[-1] == '=' ||
                                       arguments_file->f_pos[-1] == '>');
                                assert(TOKEN.t_begin[0] == '>' ||
                                       TOKEN.t_begin[0] == '=');
                                /* Parse everything after the first '>' again. */
                                if (TOKEN.t_begin[0] != '=') {
                                 arguments_file->f_pos = TOKEN.t_begin+1;
                                } else {
                                 arguments_file->f_pos = TOKEN.t_begin+1;
                                 while (SKIP_WRAPLF(arguments_file->f_pos,
                                                    arguments_file->f_end));
                                 assert(arguments_file->f_pos[0] == '>');;
                                 ++arguments_file->f_pos;
                                }
                               }
                              }
                              break;
                             case TOK_RANGLE3:       /* '>>>' */
                             case TOK_RANGLE3_EQUAL: /* '>>>=' = '=>>>' */
                              if (paren_recursion[RECURSION_ANGLE] >= 3) {
                               paren_recursion[RECURSION_ANGLE] -= 3;
                               if (TOK == TOK_RANGLE3_EQUAL) {
                                assert(arguments_file->f_pos[-1] == '=' ||
                                       arguments_file->f_pos[-1] == '>');
                                if (arguments_file->f_pos[-1] == '=')
                                  --arguments_file->f_pos; /* Parse the '=' again. */
                               }
                               ++TOKEN.t_begin;
                               while (SKIP_WRAPLF(TOKEN.t_begin,TOKEN.t_end));
                               assert(*TOKEN.t_begin == '>');
                               ++TOKEN.t_begin;
                              } else if (paren_recursion[RECURSION_ANGLE] >= 2) {
                               paren_recursion[RECURSION_ANGLE] -= 2;
                               if (TOK == TOK_RANGLE3_EQUAL) {
                                assert(arguments_file->f_pos[-1] == '=' ||
                                       arguments_file->f_pos[-1] == '>');
                                --arguments_file->f_pos; /* Parse the '=' again. */
                                if (arguments_file->f_pos[0] != '>') {
                                 while (SKIP_WRAPLF_REV(arguments_file->f_pos,arguments_file->f_begin));
                                 assert(arguments_file->f_pos[-1] == '>');
                                 --arguments_file->f_pos; /* Parse the '>=' again. */
                                }
                               } else {
                                assert(arguments_file->f_pos[-1] == '>');
                                --arguments_file->f_pos; /* Parse the '>' again. */
                               }
                               ++TOKEN.t_begin;
                               assert(*TOKEN.t_begin == '>');
                              } else {
                               assert(*TOKEN.t_begin == '>' ||
                                      *TOKEN.t_begin == '=');
                               if (*TOKEN.t_begin == '=') {
                                ++TOKEN.t_begin;
                                while (SKIP_WRAPLF(TOKEN.t_begin,arguments_file->f_end));
                               }
                               --paren_recursion[RECURSION_ANGLE];
                               arguments_file->f_pos = TOKEN.t_begin+1; /* Parse the '>>' / '>>=' / '>' / '>>' again. */
                              }
                              break;
                            }
     }
     /* Check if the select calling convention has dropped to zero. */
     if (!paren_recursion[calling_conv]) goto add_arg;
     break;

    case ',': /* Add a new argument. */
     if (paren_recursion[calling_conv] == 1 &&
         paren_recursion[(calling_conv+1) % 4] == 0 &&
         paren_recursion[(calling_conv+2) % 4] == 0 &&
         paren_recursion[(calling_conv+3) % 4] == 0) {
add_arg:
      ++va_size;
      /* Add a new argument. */
      arg_iter->ac_offset_end = (size_t)(TOKEN.t_begin-TOKEN.t_file->f_text->s_text);
      if (!paren_recursion[calling_conv]) goto done_args;
      if (arg_iter != arg_last) {
       (++arg_iter)->ac_offset_begin = (size_t)(TOKEN.t_end-TOKEN.t_file->f_text->s_text);
      } else if (!(macro->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_VARIADIC)) {
       if unlikely(!WARN(W_TOO_MANY_MACRO_ARGUMENTS,macro)) { err_argv: free(argv); goto end0; }
      }
     }
     break;
    default: if (TOK < 0) goto done_args; break;
   }
   TPPLexer_YieldPP(); /* YieldPP: Allow preprocessor directives in macro arguments. */
  }
done_args:
  if (va_size < effective_argc) va_size = 1;
  else va_size -= (effective_argc-1);
  assert(TOKEN.t_file == arguments_file);
  if (arg_iter != arg_last) {
   if (macro->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_VARIADIC &&
       arg_iter == arg_last-1) {
    /* Special case: Fill in the last (optional) argument of a varargs function with an empty string.
     *               This way the following two version do exactly the same:
     *            >> #define foo(a,...) a(__VA_ARGS__)
     *            >> foo(bar,); // [bar][(][)][;]
     *            >> foo(baz);  // [baz][(][)][;]
     */
    ++arg_iter;
    arg_iter->ac_offset_begin = arguments_file->f_text->s_size;
    arg_iter->ac_offset_end   = arguments_file->f_text->s_size;
   } else {
    if unlikely(!WARN(W_NOT_ENGOUH_MACRO_ARGUMENTS,macro)) goto err_argv;
    for (;;) {
     ++arg_iter;
     /* Fill in the remaining arguments with empty-stubs. */
     arg_iter->ac_offset_begin = arguments_file->f_text->s_size;
     arg_iter->ac_offset_end   = arguments_file->f_text->s_size;
     if (arg_iter == arg_last) break;
    }
   }
  }
  /* All the arguments have been parsed and the file pointer of the arguments file
   * (which at this point is also the #include-stack top-file) is set to point
   * directly after the closing parenthesis.
   * >> At this point we know that the arguments file will no longer need to be
   *    expanded, meaning we can finally convert all the macro arguments from
   *    relative offsets to absolute pointers. */
  text_offset = (uintptr_t)arguments_file->f_text->s_text;
  arg_end = (arg_iter = argv)+effective_argc;
  for (; arg_iter != arg_end; ++arg_iter) {
   arg_iter->ac_offset_begin *= sizeof(char);
   arg_iter->ac_offset_end   *= sizeof(char);
   arg_iter->ac_offset_begin += text_offset;
   arg_iter->ac_offset_end   += text_offset;
   if (!(CURRENT.l_flags&TPPLEXER_FLAG_KEEP_ARG_WHITESPACE)) {
    arg_iter->ac_begin = skip_whitespace_and_comments(arg_iter->ac_begin,arg_iter->ac_end);
    arg_iter->ac_end = skip_whitespace_and_comments_rev(arg_iter->ac_end,arg_iter->ac_begin);
   }
  }

  /* Check for special case: No-arguments macro. */
  if (!macro->f_macro.m_function.f_argc) {
   /* The one argument that we parsed must only contain whitespace.
    * >> If it does not, emit a warning. */
   if unlikely((argv[0].ac_begin != argv[0].ac_end) &&
               !WARN(W_TOO_MANY_MACRO_ARGUMENTS,macro)) goto err_argv;
  }
  /* If the variadic portion of varargs function is empty, the variadic size drops to ZERO(0).
   * >> This is required to properly implement __VA_COMMA__/__VA_NARGS__ semantics. */
  if (macro->f_macro.m_flags&TPP_MACROFILE_FLAG_FUNC_VARIADIC &&
      arg_last->ac_begin == arg_last->ac_end) assert(va_size),--va_size;


#if HAVELOG(LOG_CALLMACRO) /* DEBUG: Log calls to macros. */
  {
   arg_end = (arg_iter = argv)+effective_argc;
   LOG(LOG_CALLMACRO|LOG_RAW,("[DEBUG] Calling: %.*s(\n",(int)macro->f_namesize,macro->f_name));
   for (; arg_iter != arg_end; ++arg_iter) {
    LOG(LOG_CALLMACRO|LOG_RAW,("\t[%.*s],\n",(int)(arg_iter->ac_end-arg_iter->ac_begin),arg_iter->ac_begin));
   }
   LOG(LOG_CALLMACRO|LOG_RAW,(")\n"));
  }
#endif
  /* Everything has been checked!
   * >> So now we can finally begin creating the expanded macro. */
  expand_file = expand_function_macro(macro,argv,arguments_file,va_size);
  /* Cleanup the argv cache. */
  if (!macro->f_macro.m_function.f_argbuf) {
   /* The cache slot is empty (it may not be for recursively expanding macros) */
   macro->f_macro.m_function.f_argbuf = argv;
  } else {
   /* If it isn't empty, we must free our own cache. */
   free(argv);
  }
  if unlikely(!expand_file) goto end0;
  /* Sanity checks. */
  assert(expand_file->f_kind == TPPFILE_KIND_MACRO);
  assert((expand_file->f_macro.m_flags&TPP_MACROFILE_KIND) ==
          TPP_MACROFILE_KIND_EXPANDED);
  assert(expand_file->f_macro.m_expand.e_expand_origin == macro);
  if (check_mirrors) {
   struct TPPFile *file_iter = TOKEN.t_file;
   /* Search for previous expansions of this macro who's
    * files contain the same text as 'expand_file'.
    * If any are found, expansion must not be allowed,
    * and instead, the the lexer state before the macro's
    * name was encountered must be restored.
    * s.a.: The other comment above.
    */
   while ((assert(file_iter),file_iter != &TPPFile_Empty)) {
     /* Check the text of a previous version again that of this one. */
    if (file_iter->f_kind == TPPFILE_KIND_MACRO &&
       (file_iter->f_macro.m_flags&TPP_MACROFILE_KIND) == TPP_MACROFILE_KIND_EXPANDED &&
        file_iter->f_macro.m_expand.e_expand_origin == macro &&
        file_iter->f_text->s_size == expand_file->f_text->s_size &&
        compare_text(file_iter->  f_text->s_text,
                     expand_file->f_text->s_text,
                     expand_file->f_text->s_size)) {
     TPPFile_Decref(expand_file);
     incback_restore(&popped_includes,arguments_file);
     if (!WARN(W_FUNCTION_MACRO_ALREADY_ONSTACK,macro)) goto end0;
     goto end2;
    }
    file_iter = file_iter->f_prev;
   }
  }
  /* Insert the new macro file into the expansion stack. */
  expand_file->f_prev = TOKEN.t_file;
  TOKEN.t_file = expand_file;
 }
 result = 1;
end:
 popf();
 if (check_mirrors) incback_quit(&popped_includes);
 return result;
end0: result = 0; goto end;
end2: result = 2; goto end;
}


PUBLIC /*ref*/struct TPPString *
TPPConst_ToString(struct TPPConst const *__restrict self) {
 struct TPPString *result;
 size_t result_size;
 assert(self);
 if (self->c_kind == TPP_CONST_STRING) {
  assert(self->c_data.c_string);
  result_size = 2+TPP_SizeofEscape(self->c_data.c_string->s_text,
                                   self->c_data.c_string->s_size);
  result = TPPString_NewSized(result_size);
  if unlikely(!result) return NULL;
  result->s_text[0]             = '\"';
  result->s_text[result_size-1] = '\"';
  TPP_Escape(result->s_text+1,
             self->c_data.c_string->s_text,
             self->c_data.c_string->s_size);
 } else if (self->c_kind == TPP_CONST_FLOAT) {
  result_size = TPP_SizeofFtos(self->c_data.c_float);
  result = TPPString_NewSized(result_size);
  if unlikely(!result) return NULL;
  TPP_Ftos(result->s_text,self->c_data.c_float);
 } else {
  result_size = TPP_SizeofItos(self->c_data.c_int);
  result = TPPString_NewSized(result_size);
  if unlikely(!result) return NULL;
  TPP_Itos(result->s_text,self->c_data.c_int);
 }
 return result;
}


/* Most of the time, the eval functions will only call the next lower level.
 * >> Looking at the fastcall calling convention on i386, the first argument
 *    is passed through EAX (which isn't clobbered because all functions return void).
 *    With this calling convention, the result-argument doesn't need to be
 *    re-push onto the stack every time the next lower level is called.
 */
#if defined(__GNUC__) && \
   (defined(__i386__) || defined(__i386) || defined(i386))
#define EVAL_CALL  __attribute__((__fastcall__))
#elif defined(_MSC_VER)
#define EVAL_CALL  __fastcall
#else
#define EVAL_CALL  /* nothing */
#endif


PUBLIC /*ref*/struct TPPString *
TPPLexer_ParseString(void) {
 struct TPPString *result,*newbuffer;
 size_t reqsize,allocsize;
 char *string_begin,*string_end;
 assert(TPPLexer_Current);
 assert(TOKEN.t_id == TOK_STRING);
 string_begin = TOKEN.t_begin;
 string_end = TOKEN.t_end;
 if (string_begin != string_end && *string_begin == '\"') ++string_begin;
 if (string_begin != string_end && string_end[-1] == '\"') --string_end;
 reqsize = TPP_SizeofUnescape(string_begin,
                             (size_t)(string_end-string_begin));
 result  = (struct TPPString *)malloc(TPP_OFFSETOF(struct TPPString,s_text)+
                                     (reqsize+1)*sizeof(char));
 if unlikely(!result) goto err;
 result->s_size = reqsize;
 TPP_Unescape(result->s_text,string_begin,
             (size_t)(string_end-string_begin));
 YIELD();
 allocsize = reqsize;
 while (TOK == TOK_STRING) {
  /* Cat multiple consecutive strings together. */
  string_begin = TOKEN.t_begin;
  string_end = TOKEN.t_end;
  if (string_begin != string_end && *string_begin == '\"') ++string_begin;
  if (string_begin != string_end && string_end[-1] == '\"') --string_end;
  reqsize = result->s_size+TPP_SizeofUnescape(string_begin,
                                             (size_t)(string_end-string_begin));
  if (reqsize > allocsize) {
   newbuffer = (struct TPPString *)realloc(result,TPP_OFFSETOF(struct TPPString,s_text)+
                                          (reqsize+1)*sizeof(char));
   if unlikely(!newbuffer) { free(result); goto err; }
   result    = newbuffer;
   allocsize = reqsize;
  }
  TPP_Unescape(result->s_text+result->s_size,string_begin,
              (size_t)(string_end-string_begin));
  result->s_size = reqsize;
  YIELD();
 }
 result->s_refcnt = 1;
 result->s_text[result->s_size] = '\0';
 return result;
err:
 TPPLexer_SetErr();
 return NULL;
}

PUBLIC int TPP_Atoi(int_t *__restrict pint) {
 char ch,*begin,*end; int_t intval,new_intval,more;
 int numsys,result = TPP_ATOI_OK|TPP_ATOI_TYPE_INT;
 assert(pint);
 assert(TPPLexer_Current);
 assert(TOK == TOK_INT || TOK == TOK_CHAR);
 begin = TOKEN.t_begin,end = TOKEN.t_end;
 assert(begin <= end);
 if (TOK == TOK_CHAR) {
  size_t esc_size,size;
  if likely(begin != end && *begin == '\'') ++begin;
  if likely(begin != end && end[-1] == '\'') --end;
  size = (size_t)(end-begin);
  esc_size = TPP_SizeofUnescape(begin,size);
  if (esc_size > sizeof(int_t)) {
   if unlikely(!WARN(W_CHARACTER_TOO_LONG)) goto err;
   do assert(size),--size;
   while (TPP_SizeofUnescape(begin,size) > sizeof(int_t));
  }
  *pint = 0;
  size = (size_t)(TPP_Unescape((char *)pint,begin,size)-(char *)pint);
#if TPP_BYTEORDER == 4321
  /* Adjust for big endian. */
  *pint >>= (sizeof(int_t)-size)*8;
#elif TPP_BYTEORDER != 1234
#   error FIXME
#endif
  if (CURRENT.l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED) result |= TPP_ATOI_UNSIGNED;
  return result;
 }
 /* Regular integral. */
 intval = 0;
 if (begin == end) goto done;
 if (*begin != '0') numsys = 10;
 else {
  ++begin;
  if (begin != end) while (SKIP_WRAPLF(begin,end));
  if (begin == end) goto done;
  ch = *begin,ch = tolower(ch);
       if (ch == 'x') ++begin,numsys = 16;
  else if (ch == 'b' && HAVE_EXTENSION_BININTEGRAL) ++begin,numsys = 2;
  else numsys = 8;
 }
 while (begin != end) {
  while (SKIP_WRAPLF(begin,end));
  ch = *begin;
       if (ch >= '0' && ch <= '9') more = (int_t)(ch-'0');
  else if (ch >= 'A' && ch <= 'F') more = (int_t)(10+(ch-'A'));
  else if (ch >= 'a' && ch <= 'f') more = (int_t)(10+(ch-'a'));
  else break;
  if unlikely(more >= numsys) break;
  new_intval = intval*numsys+more;
  if unlikely((uint64_t)new_intval < (uint64_t)intval) {
   /* Warn about overflow: */
   if unlikely(!WARN(W_INTEGRAL_OVERFLOW,intval,new_intval)) goto err;
  }
  intval = new_intval;
  ++begin;
 }
 /* Parse a suffix. */
 while (begin != end) {
  while (SKIP_WRAPLF(begin,end));
  ch = *begin,ch = tolower(ch);
       if (ch == 'u' && !(result&TPP_ATOI_UNSIGNED)) result |= TPP_ATOI_UNSIGNED;
  else if (ch == 'l' && !(result&TPP_ATOI_TYPE_LONGLONG)) {
   if (result&TPP_ATOI_TYPE_LONG)
    result &= ~(TPP_ATOI_TYPE_LONG),
    result |= TPP_ATOI_TYPE_LONGLONG;
   else result |= TPP_ATOI_TYPE_LONG;
  } else break;
  ++begin;
 }
 if (begin != end) {
  if (HAVE_EXTENSION_MSVC_FIXED_INT &&
     (result&TPP_ATOI_TYPE_MASK) == TPP_ATOI_TYPE_INT) {
   /* MSVC-style fixed-length integer suffix. */
   ch = *begin,ch = tolower(ch);
   if (ch == 'i') {
    char *forward = begin+1;
    if (forward != end) while (SKIP_WRAPLF(forward,end));
    if (forward == end) goto wrong_suffix;
    ch = *forward++;
         if (ch == '8') begin = forward,result |= TPP_ATOI_TYPE_INT8;
    else if (ch == '1' || ch == '3' || ch == '6') {
     char ch2;
     if (forward != end) while (SKIP_WRAPLF(forward,end));
     if (forward == end) goto wrong_suffix;
     ch2 = *forward++;
          if (ch == '1' && ch2 == '6') begin = forward,result |= TPP_ATOI_TYPE_INT16;
     else if (ch == '3' && ch2 == '2') begin = forward,result |= TPP_ATOI_TYPE_INT32;
     else if (ch == '6' && ch2 == '4') begin = forward,result |= TPP_ATOI_TYPE_INT64;
    }
   }
  }
wrong_suffix:
  if (begin != end) {
   /* Warning: Unknown suffix. */
   if unlikely(!WARN(W_INVALID_INTEGER_SUFFIX,
                              begin,(size_t)(end-begin))
               ) goto err;
  }
 }
 /* Clamp 'intval' with the determined type. */
 switch (result&TPP_ATOI_TYPE_MASK) {
#define T_MASK(T) (int_t)(~(T)0)
  default                    : new_intval = intval&T_MASK(int); break;
  case TPP_ATOI_TYPE_LONG    : new_intval = intval&T_MASK(long); break;
  case TPP_ATOI_TYPE_INT8    : new_intval = intval&T_MASK(int8_t); break;
  case TPP_ATOI_TYPE_INT16   : new_intval = intval&T_MASK(int16_t); break;
  case TPP_ATOI_TYPE_INT32   : new_intval = intval&T_MASK(int32_t); break;
  case TPP_ATOI_TYPE_LONGLONG: /* If 'long long' doesn't exist, assume it's supposed to be 64-bit. */
#if TPP_HAVE_LONGLONG
                               new_intval = intval&T_MASK(long long); break;
#endif /* TPP_HAVE_LONGLONG */
  case TPP_ATOI_TYPE_INT64   : new_intval = intval&T_MASK(int64_t); break;
#undef T_MASK
 }
 if unlikely(new_intval != intval) {
  /* Warn about clamped integral. */
  if unlikely(!WARN(W_INTEGRAL_CLAMPED,intval,new_intval)) goto err;
 }
 intval = new_intval;
done: *pint = intval;
end:   return result;
err:   result = TPP_ATOI_ERR; goto end;
}

PUBLIC int TPP_Atof(TPP(float_t) *__restrict pfloat) {
 assert(pfloat);
 assert(TPPLexer_Current);
 assert(TOK == TOK_FLOAT);
 *pfloat = 0; /* TODO! */
 return 0;
}


PUBLIC int TPP_PrintToken(printer_t printer, void *closure) {
 char *flush_start,*iter,*end,arg[1],temp; int error = 0;
#define print(s,l)         do{if((error=(*printer)(s,l,closure))!=0)goto done;}while(FALSE)
#define return_print(s,l)  do{error = (*printer)(s,l,closure);goto done;}while(FALSE)
 assert(TPPLexer_Current);
 assert(printer);
 /* Manually print digraph/trigraph characters. */
 switch (TOK) {
  case '{': case '[': case '}':
  case ']': case '#': case '\\':
  case '^': case '|': case '~':
  case '?': arg[0] = (char)TOK;
            return_print(arg,1);
  case TOK_GLUE: return_print("##",2);
  default: break;
 }
 iter = flush_start = TOKEN.t_begin,end = TOKEN.t_end;
 while (iter != end) {
  assert(iter < end);
  /* Handle escaped linefeeds. */
  if (*iter == '\\' && tpp_islf(iter[1])) {
   if (iter != flush_start) print(flush_start,(size_t)(iter-flush_start));
   if (*++iter == '\r' && iter[1] == '\n') ++iter;
   flush_start = ++iter;
   continue;
  }
  if (HAVE_FEATURE_TRIGRAPHS &&
      iter[0] == '?' && iter[1] == '?') {
   /* Decode trigraph sequences. */
   switch (iter[2]) {
    case '=':  temp = '#';  break; /* ??= */
    case '(':  temp = '[';  break; /* ??( */
    case '/':  temp = '\\'; break; /* ??/ */
    case ')':  temp = ']';  break; /* ??) */
    case '\'': temp = '^';  break; /* ??' */
    case '<':  temp = '{';  break; /* ??< */
    case '!':  temp = '|';  break; /* ??! */
    case '>':  temp = '}';  break; /* ??> */
    case '-':  temp = '~';  break; /* ??- */
    case '?':  temp = '?';  break; /* ??? */
    default: goto next;
   }
   arg[0] = temp;
   print(arg,1);
   iter += 3;
   continue;
  }
next:
  ++iter;
 }
 if (iter != flush_start) {
  error = (*printer)(flush_start,(size_t)(iter-flush_start),closure);
 }
done:
 return error;
#undef return_print
#undef print
}
PUBLIC int TPP_PrintComment(printer_t printer, void *closure) {
 (void)printer,(void)closure;
 return 0; /* TODO */
}

#if TPP_CONFIG_GCCFUNC
#ifndef __INTELLISENSE__
#define DECLARE_BUILTIN_FUNCTIONS
#include "tpp-defs.inl"
#undef DECLARE_BUILTIN_FUNCTIONS
#endif

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4065)
#endif

PRIVATE int get_builtin_argc(tok_t function) {
 int result = -1;
 switch (function) {
#ifndef __INTELLISENSE__
#define BUILTIN_FUNCTION(name,argc,expr) case name: result = (argc); break;
#include "tpp-defs.inl"
#undef BUILTIN_FUNCTION
#endif
  default: break;
 }
 return result;
}

#ifdef _MSC_VER
#pragma warning(pop)
#endif

#ifdef __GNUC__
#define UNUSED_LABEL  __attribute__((__unused__))
#else
#define UNUSED_LABEL  /* nothing */
#endif

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4065)
#pragma warning(disable: 4102)
#pragma warning(disable: 4702)
#endif

PRIVATE int EVAL_CALL
eval_call_builtin(struct TPPConst *__restrict result) {
 int retval = 0,argc; tok_t function = TOK;
 struct TPPConst *argv,*iter,*end;
 assert(result);
 if unlikely((argc = get_builtin_argc(function)) < 0) goto ret;
 YIELD();
 if (TOK != '(') WARN(W_EXPECTED_LPAREN);
 else YIELD();
 argv = (struct TPPConst *)alloca(argc*sizeof(struct TPPConst));
 end = (iter = argv)+argc;
 if (iter != end) for (;;) {
  if unlikely(!TPPLexer_Eval(iter)) goto err_iter;
  if (++iter == end) break;
  if (TOK != ',') WARN(W_EXPECTED_COMMA);
  else YIELD();
 }
 if (TOK != ')') WARN(W_EXPECTED_LPAREN);
 else YIELD();
 /* The argument list has been parsed. - Time for the big function-switch. */
 switch (function) {
#ifndef __INTELLISENSE__
#define SETERR()              goto seterr_argv
#define ERR()                 goto err_argv
#define A(i)                 (argv+(i))
#define INT(i)                TPPConst_AsInt(A(i))
#define FLOAT(i)              TPPConst_AsFloat(A(i))
#define STRING(i)            (A(i)->c_data.c_string)
#define IS_STRING(i)         (A(i)->c_kind == TPP_CONST_STRING)
#define RETURN_INHERIT(val) { *(result) = *(val); goto success; }
#define RETURN_COPY(val)    { TPPConst_InitCopy(result,val); goto success; }
#define RETURN_INT(val)     { result->c_data.c_int = (val); goto set_int_common; }
#define RETURN_STRING(val)  { result->c_data.c_string = (val); goto set_string_common; }
#define BUILTIN_FUNCTION(name,argc,expr) case name: expr; break;
#include "tpp-defs.inl"
#undef BUILTIN_FUNCTION
#undef RETURN_STRING
#undef RETURN_INT
#undef RETURN_COPY
#undef RETURN_INHERIT
#undef IS_STRING
#undef STRING
#undef FLOAT
#undef INT
#undef A
#undef ERR
#undef SETERR
#endif
  default:
#ifdef _MSC_VER
   __assume(0);
#elif defined(__GNUC__)
   __builtin_unreachable();
#else
   TPPConst_ZERO(result);
   break;
#endif
set_int_common:    UNUSED_LABEL; result->c_kind = TPP_CONST_INTEGRAL; break;
set_float_common:  UNUSED_LABEL; result->c_kind = TPP_CONST_FLOAT; break;
set_string_common: UNUSED_LABEL; result->c_kind = TPP_CONST_STRING; break;
 }
success:     UNUSED_LABEL; retval = 1;
err_argv:    UNUSED_LABEL; iter = argv+argc;
err_iter:    UNUSED_LABEL; while (iter-- != argv) TPPConst_Quit(iter);
ret:         UNUSED_LABEL; return retval;
seterr_argv: UNUSED_LABEL; TPPLexer_SetErr(); goto err_argv;
}
#undef UNUSED_LABEL
#ifdef _MSC_VER
#pragma warning(pop)
#endif
#endif /* TPP_CONFIG_GCCFUNC */


#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif
PRIVDEF void EVAL_CALL eval_question(struct TPPConst *result);

PRIVATE void EVAL_CALL eval_unary(struct TPPConst *result) {
 int try_parse_value = 0;
again_unary:
 switch (TOK) {

  { /* Parse an integer/character. */
  case TOK_INT:
  case TOK_CHAR:
   if (result) {
    result->c_kind = TPP_CONST_INTEGRAL;
    TPP_Atoi(&result->c_data.c_int);
   }
   YIELD();
  } break;

  { /* Parse a floating point value. */
  case TOK_FLOAT:
   if (result) {
    result->c_kind = TPP_CONST_FLOAT;
    TPP_Atof(&result->c_data.c_float);
   }
   YIELD();
  } break;

  { /* Concat any number of consecutive strings. */
  case TOK_STRING:
   if (!result) {
    do YIELD();
    while (TOK == TOK_STRING);
    break;
   }
   result->c_kind          = TPP_CONST_STRING;
   result->c_data.c_string = TPPLexer_ParseString();
  } break;

  case '!':
   YIELD();
   if (TOK == '!') {
    /* Prevent the warning below.
     * >> '!!42' should evaluate to '1' without a warning. */
    YIELD();
    eval_unary(result);
    if (result) TPPConst_ToBool(result);
    break;
   }
   eval_unary(result);
   if (result) {
    if (!TPPConst_IsBool(result)) {
     WARN(W_EXPECTED_BOOL_UNARY,result);
     TPPConst_ToBool(result);
    }
    result->c_data.c_int ^= 1;
   }
   break;

  case '~':
   YIELD();
   eval_unary(result);
   if (result) {
    TPPConst_ToInt(result);
    result->c_data.c_int = ~result->c_data.c_int;
   }
   break;

  case '+':
  case TOK_INC:         /* ++42 == +(+42) == 42 */
  case TOK_DEC:         /* --42 == -(-42) == 42 */
  case TOK_TILDE_TILDE: /* ~~42 == ~(~42) == 42 */
   YIELD();
   eval_unary(result);
   if (result) TPPConst_ToInt(result);
   break;

  case '-':
   YIELD();
   eval_unary(result);
   if (result) {
    TPPConst_ToInt(result);
    result->c_data.c_int = -result->c_data.c_int;
   }
   break;

  case '(':
   /* Extension for parsing c-style casting expressions:
    * >> __TPP_EVAL((int)42);
    * NOTE: Since the preprocessor doesn't know about c-types,
    *       it warns about such usage, but is still able to
    *       
    */
   YIELD();
   if (TPP_ISKEYWORD(TOK) &&
      (TPP_ISUSERKEYWORD(TOK) || (
#if TPP_CONFIG_GCCFUNC
       get_builtin_argc(TOK) == -1 &&
       TOK != KWD___builtin_constant_p &&
       TOK != KWD___builtin_choose_expr &&
#endif
       TOK != KWD_defined &&
       TOK != KWD_if))) {
    unsigned int recursion = 1;
    /* Keyword without any special meaning associated to it.
     * >> Assume c-style casting and recursively skip it. */
    if (result && !WARN(W_TYPECAST_IN_EXPRESSION)) return;
    while (TOK > 0) {
     YIELD();
          if (TOK == '(') ++recursion;
     else if (TOK == ')' && !--recursion) break;
    }
    if (TOK == ')') YIELD();
    else WARN(W_EXPECTED_RPAREN_AFTER_CAST);
    try_parse_value = 1;
    goto again_unary;
   }
   if (TOK == '{') {
    unsigned int recursion = 1;
    if (result) {
     /* Warn about statements in expressions, only if the result is used.
      * Don't warn if used like this:
      * >> #define compiletime_add(a,b)
      * >> #define foo()
      * >> __TPP_EVAL((__builtin_constant_p(42)) : )
      */
     WARN(W_STATEMENT_IN_EXPRESSION);
     TPPConst_ZERO(result);
    }
    while (TOK > 0) {
     YIELD();
          if (TOK == '{') ++recursion;
     else if (TOK == '}' && !--recursion) break;
    }
    if (TOK == '}') YIELD();
    else WARN(W_EXPECTED_RBRACE_AFTER_STATEMENT);
    goto rparen_after_expression;
   }
   goto begin_after_paren;
   for (;;) {
    YIELD();
begin_after_paren:
    eval_question(result);
    if (TOK != ',') break;
    if (result) TPPConst_Quit(result);
   }
rparen_after_expression:
   if (TOK == ')') YIELD();
   else WARN(W_EXPECTED_RPAREN_IN_EXPRESSION);
   break;

  case '#':
   /* Determine the length of a string. */
   if (!HAVE_EXTENSION_STRINGOPS &&
       !HAVE_EXTENSION_ASSERTIONS) goto defch;
   YIELD();
   if (TPP_ISKEYWORD(TOK) && HAVE_EXTENSION_ASSERTIONS) {
    struct TPPKeyword *ass_keyword;
    /* Evaluate assertion expression. */
    ass_keyword = TOKEN.t_kwd;
    TPPLexer_YieldRaw();
    if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
    else TPPLexer_YieldRaw();
    result->c_kind = TPP_CONST_INTEGRAL;
    if unlikely(!TPP_ISKEYWORD(TOK)) { WARN(W_EXPECTED_KEYWORD_AFTER_EXPR_PRED,ass_keyword); goto res_zero; }
    /* Lookup an assertion */
    result->c_data.c_int = (int_t)keyword_hasassert(ass_keyword,TOKEN.t_kwd);
    TPPLexer_YieldRaw();
    if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
    else TPPLexer_YieldRaw();
    break;
   } else if (!HAVE_EXTENSION_STRINGOPS) {
    if (result) { WARN(W_EXPECTED_KEYWORD_AFTER_EXPR_HASH); res_zero: TPPConst_ZERO(result); }
    break;
   }
   eval_unary(result);
   if (result) {
    if (result->c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_IN_EXPRESSION,result);
    } else {
     /* Extract the length of a string (in characters). */
     size_t length = result->c_data.c_string->s_size;
     TPPString_Decref(result->c_data.c_string);
     result->c_kind = TPP_CONST_INTEGRAL;
     result->c_data.c_int = (int_t)length;
    }
   }
   break;

  { /* Check if a given keyword is defined
     * HINT: You can easily use this in macro functions
     *       if you use the #! macro operator:
     * >> // Declare a macro to detect if a given keyword is defined as non-zero
     * >> // Very useful for optional configuration macros that _must_ be defined to 1
     * >> #define defined_and_nonzero(x) __TPP_EVAL(defined(#!x) && (x+0))
     * >> defined_and_nonzero(CONFIG_FOO) // [0]
     * >> #define CONFIG_FOO 0
     * >> defined_and_nonzero(CONFIG_FOO) // [0]
     * >> #undef CONFIG_FOO
     * >> #define CONFIG_FOO 1
     * >> defined_and_nonzero(CONFIG_FOO) // [1]
     */
   int with_paren;
  case KWD_defined:
   TPPLexer_YieldPP();
   with_paren = TOK == '(';
   if (with_paren) TPPLexer_YieldPP();
   if (TPP_ISKEYWORD(TOK) && TOKEN.t_kwd) {
    if (result) result->c_kind       =  TPP_CONST_INTEGRAL,
                result->c_data.c_int = (TOKEN.t_kwd->k_macro != NULL ||
                                        TPP_ISBUILTINMACRO(TOKEN.t_id));
    TPPLexer_YieldPP();
   } else if (result) {
    WARN(with_paren
                  ? W_EXPECTED_KEYWORD_AFTER_DEFINED
                  : W_EXPECTED_KWDLPAR_AFTER_DEFINED);
    if (with_paren) while (TOK > 0 && TOK != ')') TPPLexer_YieldPP();
    result->c_kind       = TPP_CONST_INTEGRAL;
    result->c_data.c_int = 0;
   }
   if (with_paren) {
    if (TOK == ')') TPPLexer_YieldPP();
    else if (result) {
     WARN(W_EXPECTED_RPAREN_AFTER_DEFINED);
    }
   }
  } break;

  { /* Statement-style if expressions. */
   int is_true;
  case KWD_if:
   if (!HAVE_EXTENSION_IFELSE_IN_EXPR) goto defch;
   YIELD();
   if (TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   eval_question(result);
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   if (result && !TPPConst_IsBool(result)) {
    WARN(W_EXPECTED_BOOL,result);
    TPPConst_ToBool(result);
   }
   is_true = !result || result->c_data.c_int;
   eval_question(is_true ? result : NULL);
   for (;;) {
    while (TOK == ';') YIELD();
    if (TOK != KWD_elif) break;
    YIELD();
    if (TOK != '(') WARN(W_EXPECTED_LPAREN);
    else YIELD();
    eval_question(is_true ? NULL : result);
    if (TOK != ')') WARN(W_EXPECTED_RPAREN);
    else YIELD();
    assert(result || is_true);
    eval_question((!is_true && TPPConst_IsTrue(result)) ?
                   (is_true = 1,result->c_kind == TPP_CONST_STRING
                   ? TPPString_Decref(result->c_data.c_string)
                   : (void)0,result) : NULL);
   }
   assert(result || is_true);
   if (TOK == KWD_else) {
    YIELD();
    eval_question(is_true ? NULL : result);
    while (TOK == ';') YIELD();
   } else if (!is_true) {
    WARN(W_EXPECTED_ELSE_IN_EXPRESSION);
    assert(result);
    TPPConst_ZERO(result);
   }
  } break;

#if TPP_CONFIG_GCCFUNC
  {
   size_t old_warncount; int eval_error;
  case KWD___builtin_constant_p:
   YIELD();
   if (TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   old_warncount = CURRENT.l_warncount;
   /* __builtin_constant_p() returns true if the
    * given expression can be resolved at compile-time.
    * The only thing that can prevent this here are unknown
    * identifiers, which as it turns out emit a warning when
    * encountered.
    * >> To implement this builtin, we simply check if any
    *    warnings occurred while evaluating the expression.
    * Using this builtin, TPP can implement some ~really~ high-level macros:
    * >> #define CAT2(a,b) a##b
    * >> #define CAT(a,b) CAT2(a,b)
    * >> #define __TPP_TRYEVAL_0(expr)              expr
    * >> #define __TPP_TRYEVAL_1(expr)   __TPP_EVAL(expr)
    * >> #define __TPP_TRYEVAL(expr) CAT(__TPP_TRYEVAL_,__TPP_EVAL(__builtin_constant_p(expr)))(expr)
    * >> #define add(a,b) __TPP_TRYEVAL(a+b)
    * >> add(10,20) // Expands to: [30]
    * >> add(10,b)  // Expands to: [10][+][b]
    * >> add(a,20)  // Expands to: [a][+][20]
    * >> add(a,b)   // Expands to: [a][+][b]
    * But the main purpose is for implicit, extended
    * compatibility with existing code like this:
    * >> extern uint16_t __arch_bswap16_impl(uint16_t x);
    * >> #define __arch_bswap16(x) \
    * >> ({ typeof(x) _x = (x); \
    * >>    if (cpu_version() >= 2) {\
    * >>      __asm__("call i386_bswamp16_v2\n" : "+a" (_x));\
    * >>    } else {\
    * >>      _x = __arch_bswap16_impl(_x);\
    * >>    }\
    * >>    _x;\
    * >> })
    * >> #define __compiler_bswap16(x) ((x) >> 8 | (x) << 8)
    * >> #define bswap16(x) (__builtin_constant_p(x) ? __compiler_bswap16(x) : __arch_bswap16(x))
    * >> int x = bswap16(0xabcd); // Expands to a bunch of code that will compile to a compile-time constant
    * >> int y = __TPP_EVAL(bswap16(0xabcd)); // Expands to [11259307] (Will _not_ warn about unknown identifier '__arch_bswap16', or use of a GCC statement-expression)
    * >> #if bswap16(0xabcd) == 11259307 // Obviously, the function can also be used in #if-directives
    * >> int z = bswap16(getchar()); // Expands to bunch of code that will compile to a runtime-time
    * >>                             // call to '__arch_bswap16', passing the return value 'getchar()'
    * >> #endif
    * >> int w = __TPP_EVAL(bswap16(x)); // [W0119] Expands to [0]
    */
   pushf();
   CURRENT.l_flags |= TPPLEXER_FLAG_NO_WARNINGS; /* Don't emit warnings (they'll still be counted, though!) */
   eval_error = TPPLexer_Eval(result);
   popf();
   if unlikely(!eval_error) goto restore_warnings;
   if (result) {
    TPPConst_Quit(result);
    result->c_kind = TPP_CONST_INTEGRAL;
    result->c_data.c_int = CURRENT.l_warncount == old_warncount;
   }
restore_warnings:
   CURRENT.l_warncount = old_warncount;
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  } break;

  {
   int is_true;
  case KWD___builtin_choose_expr:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   if unlikely(!TPPLexer_Eval(result)) return;
   is_true = 0;
   if (result) {
    if (!TPPConst_IsBool(result)) WARN(W_EXPECTED_BOOL,result);
    is_true = TPPConst_IsTrue(result);
    TPPConst_Quit(result);
   }
   if unlikely(TOK != ',') WARN(W_EXPECTED_COMMA);
   else YIELD();
   if unlikely(!TPPLexer_Eval(is_true ? result : NULL)) return;
   if unlikely(TOK != ',') WARN(W_EXPECTED_COMMA);
   else YIELD();
   if unlikely(!TPPLexer_Eval(is_true ? NULL : result)) return;
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  } break;
#endif

  default:defch:
   if (TOK < 0) return;
#if TPP_CONFIG_GCCFUNC
   if (TPP_ISKEYWORD(TOK) && result &&
       HAVE_EXTENSION_BUILTIN_FUNCTIONS &&
       eval_call_builtin(result)) break;
#endif
   if (try_parse_value) goto set_zero;
   if (result) WARN(W_UNKNOWN_TOKEN_IN_EXPR_IS_ZERO);
   if (TPP_ISKEYWORD(TOK)) {
    YIELD();
#if TPP_CONFIG_GCCFUNC || 1 /* Recursively skip paren-style arguments to an imaginary function. */
    while (TOK == '(') {
     unsigned int recursion = 1;
     while (TOK > 0) {
      YIELD();
           if (TOK == '(') ++recursion;
      else if (TOK == ')' && !--recursion) {
       YIELD();
       break;
      }
     }
    }
#endif
   }
set_zero:
   if (result) TPPConst_ZERO(result);
   break;
 }
 /* Unary suffix operation (string character/sub-string access). */
 switch (TOK) {

  {
  case '[':
   if (!HAVE_EXTENSION_STRINGOPS) break;
   YIELD();
   if (!result) {
skip_array_deref:
    if (TOK != ':' && TOK != ']') eval_question(NULL);
    if (TOK == ':') YIELD();
    if (TOK != ']') eval_question(NULL);
   } else {
    struct TPPConst temp;
    if (result->c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_IN_EXPRESSION);
     goto skip_array_deref;
    }

    if (TOK == ':') {
     temp.c_kind       = TPP_CONST_INTEGRAL;
     temp.c_data.c_int = 0;
    } else {
     eval_question(&temp);
     TPPConst_ToInt(&temp);
    }
    if (TOK == ':') {
     /* Range-style substring access. */
     ptrdiff_t index_begin,index_end;
     index_begin = (ptrdiff_t)temp.c_data.c_int;
     if (index_begin < 0) index_begin = (ptrdiff_t)result->c_data.c_string->s_size+index_begin;
     if (index_begin < 0 || (size_t)index_begin >= result->c_data.c_string->s_size) {
      WARN(W_INDEX_OUT_OF_BOUNDS,result->c_data.c_string,index_begin);
      index_begin %= (result->c_data.c_string->s_size+1);
      if (index_begin < 0) index_begin += result->c_data.c_string->s_size;
     }
     YIELD();
     if (TOK == ']') {
      index_end = (ptrdiff_t)result->c_data.c_string->s_size;
     } else {
      eval_question(&temp);
      TPPConst_ToInt(&temp);
      index_end = (ptrdiff_t)temp.c_data.c_int;
      if (index_end < 0) index_end = (ptrdiff_t)result->c_data.c_string->s_size+index_end;
      if (index_end < 0 || (size_t)index_end >= result->c_data.c_string->s_size) {
       WARN(W_INDEX_OUT_OF_BOUNDS,result->c_data.c_string,index_end);
       index_end %= (result->c_data.c_string->s_size+1);
       if (index_end < 0) index_end += result->c_data.c_string->s_size;
      }
     }
     if (index_begin > index_end) index_begin = index_end;
     assert(index_begin >= 0 && (size_t)index_begin <= result->c_data.c_string->s_size);
     assert(index_end   >= 0 && (size_t)index_end   <= result->c_data.c_string->s_size);
     if ((size_t)index_begin == 0 &&
         (size_t)index_end == result->c_data.c_string->s_size) {
      /* Special case: Full sub-range. */
     } else {
      struct TPPString *newstring;
      /* Regular case: Partial sub-range. */
      newstring = TPPString_New(result->c_data.c_string->s_text+index_begin,
                               (size_t)(index_end-index_begin));
      if unlikely(!newstring) goto err_r;
      TPPString_Decref(result->c_data.c_string);
      result->c_data.c_string = newstring; /*< Inherit reference. */
     }
    } else {
     char ch; /* Access character at 'begin'. */
     ptrdiff_t index = (ptrdiff_t)temp.c_data.c_int;
     if (index < 0 || (size_t)index >= result->c_data.c_string->s_size) {
      WARN(W_INDEX_OUT_OF_BOUNDS,result->c_data.c_string,index);
      index %= result->c_data.c_string->s_size;
      if (index < 0) index += result->c_data.c_string->s_size;
     }
     assert(index >= 0 && (size_t)index < result->c_data.c_string->s_size);
     ch = result->c_data.c_string->s_text[index];
     TPPString_Decref(result->c_data.c_string);
     result->c_kind = TPP_CONST_INTEGRAL;
     result->c_data.c_int = (int_t)ch;
    }
   }
   if (TOK == ']') YIELD();
   else WARN(W_EXPECTED_RBRACKET_IN_EXPRESSION);
  } break;

  default: break;
 }
 return;
err_r:   TPPConst_Quit(result);
/*err:*/ TPPLexer_SetErr();
 return;
}
PRIVATE void EVAL_CALL eval_prod(struct TPPConst *result) {
 eval_unary(result);
 for (;;) {
  struct TPPConst result2;
  register tok_t t;
  switch (t = TOK) {
   case '*':
   case '/':
   case '%':
    YIELD();
    eval_unary(result ? &result2 : NULL);
    if (result) {
     TPPConst_ToInt(result);
     TPPConst_ToInt(&result2);
     if (t == '*') result->c_data.c_int *= result2.c_data.c_int;
     else if (!result2.c_data.c_int) WARN(W_DIVIDE_BY_ZERO);
     else if (t == '/') result->c_data.c_int /= result2.c_data.c_int;
     else               result->c_data.c_int %= result2.c_data.c_int;
    }
    break;
   default: return;
  }
 }
}

PRIVATE void EVAL_CALL eval_sum(struct TPPConst *result) {
 eval_prod(result);
 for (;;) {
  struct TPPConst result2;
  register tok_t t;
  switch (t = TOK) {
   case '+':
   case '-':
    YIELD();
    eval_prod(result ? &result2 : NULL);
    if (result) {
     if (t == '+' &&
         result->c_kind == TPP_CONST_STRING &&
         result2.c_kind == TPP_CONST_STRING &&
         HAVE_EXTENSION_STRINGOPS) {
      /* Concat two strings. */
      struct TPPString *newstr = TPPString_Cat(result->c_data.c_string,
                                               result2.c_data.c_string);
      if unlikely(!newstr) {
       TPPString_Decref(result->c_data.c_string);
       TPPString_Decref(result2.c_data.c_string);
       TPPLexer_SetErr();
       return;
      }
      result->c_data.c_string = newstr; /*< Inherit reference. */
     } else {
      TPPConst_ToInt(result);
      TPPConst_ToInt(&result2);
      if (t == '+') result->c_data.c_int += result2.c_data.c_int;
      else          result->c_data.c_int -= result2.c_data.c_int;
     }
    }
    break;
   default: return;
  }
 }
}
PRIVATE void EVAL_CALL eval_shift(struct TPPConst *result) {
 eval_sum(result);
 for (;;) {
  struct TPPConst result2;
  register tok_t t;
  switch (t = TOK) {
   case TOK_SHL:
   case TOK_SHR:
    YIELD();
    eval_sum(result ? &result2 : NULL);
    if (result) {
     TPPConst_ToInt(result);
     TPPConst_ToInt(&result2);
     if (t == TOK_SHL) result->c_data.c_int <<= result2.c_data.c_int;
     else              result->c_data.c_int >>= result2.c_data.c_int;
    }
    break;
   default: return;
  }
 }
}
PRIVATE void EVAL_CALL eval_cmp(struct TPPConst *result) {
 eval_shift(result);
 for (;;) {
  struct TPPConst result2;
  register tok_t t;
  register int_t diff;
  switch (t = TOK) {
   case TOK_LOWER:
   case TOK_LOWER_EQUAL:
   case TOK_GREATER:
   case TOK_GREATER_EQUAL:
    YIELD();
    eval_shift(result ? &result2 : NULL);
    if (result) {
     if (result->c_kind == TPP_CONST_INTEGRAL ||
         result2.c_kind == TPP_CONST_INTEGRAL ||
        !HAVE_EXTENSION_STRINGOPS) {
      TPPConst_ToInt(result);
      TPPConst_ToInt(&result2);
     }
     if (result2.c_kind == TPP_CONST_INTEGRAL) {
      diff = result->c_data.c_int-result2.c_data.c_int;
     } else {
      assert(!result->c_data.c_string->s_text[result->c_data.c_string->s_size]);
      assert(!result2.c_data.c_string->s_text[result2.c_data.c_string->s_size]);
      diff = strcmp(result->c_data.c_string->s_text,
                    result2.c_data.c_string->s_text);
      TPPConst_Quit(result);
      TPPConst_Quit(&result2);
      result->c_kind = TPP_CONST_INTEGRAL;
     }
     switch (t) {
      default:                diff = diff <  0; break;
      case TOK_LOWER_EQUAL:   diff = diff <= 0; break;
      case TOK_GREATER:       diff = diff >  0; break;
      case TOK_GREATER_EQUAL: diff = diff >= 0; break;
     }
     result->c_data.c_int = diff;
    }
    break;
   default: return;
  }
 }
}
PRIVATE void EVAL_CALL eval_cmp_eq(struct TPPConst *result) {
 eval_cmp(result);
 for (;;) {
  struct TPPConst result2;
  register tok_t t;
  register int resval;
  switch (t = TOK) {
   case TOK_EQUAL:
   case TOK_NOT_EQUAL:
    YIELD();
    eval_cmp(result ? &result2 : NULL);
    if (result) {
     if (result->c_kind == TPP_CONST_INTEGRAL ||
         result2.c_kind == TPP_CONST_INTEGRAL ||
        !HAVE_EXTENSION_STRINGOPS) {
      TPPConst_ToInt(result);
      TPPConst_ToInt(&result2);
     }
     if (result2.c_kind == TPP_CONST_INTEGRAL) {
      resval = result->c_data.c_int == result2.c_data.c_int;
     } else {
      resval = (result->c_data.c_string->s_size ==
                result2.c_data.c_string->s_size) &&
               !memcmp(result->c_data.c_string->s_text,
                       result2.c_data.c_string->s_text,
                       result2.c_data.c_string->s_size*
                       sizeof(char));
      TPPConst_Quit(result);
      TPPConst_Quit(&result2);
      result->c_kind = TPP_CONST_INTEGRAL;
     }
     if (t == TOK_NOT_EQUAL) resval = !resval;
     result->c_data.c_int = (int_t)resval;
    }
    break;
   default: return;
  }
 }
}
PRIVATE void EVAL_CALL eval_and(struct TPPConst *result) {
 eval_cmp_eq(result);
 while (TOK == '&') {
  YIELD();
  if (result) {
   struct TPPConst result2;
   eval_cmp_eq(&result2);
   TPPConst_ToInt(result);
   TPPConst_ToInt(&result2);
   result->c_data.c_int &= result2.c_data.c_int;
  } else {
   eval_cmp_eq(NULL);
  }
 }
}
PRIVATE void EVAL_CALL eval_xor(struct TPPConst *result) {
 eval_and(result);
 while (TOK == '^') {
  YIELD();
  if (result) {
   struct TPPConst result2;
   eval_and(&result2);
   TPPConst_ToInt(result);
   TPPConst_ToInt(&result2);
   result->c_data.c_int ^= result2.c_data.c_int;
  } else {
   eval_and(NULL);
  }
 }
}
PRIVATE void EVAL_CALL eval_or(struct TPPConst *result) {
 eval_xor(result);
 while (TOK == '|') {
  YIELD();
  if (result) {
   struct TPPConst result2;
   eval_xor(&result2);
   TPPConst_ToInt(result);
   TPPConst_ToInt(&result2);
   result->c_data.c_int |= result2.c_data.c_int;
  } else {
   eval_xor(NULL);
  }
 }
}

PRIVATE void EVAL_CALL eval_land(struct TPPConst *result) {
 eval_or(result);
 if (TOK == TOK_LAND) {
  do {
   YIELD();
   if (result) {
    if (!TPPConst_IsBool(result)) {
     WARN(W_EXPECTED_BOOL_BINARY_LHS,result);
     TPPConst_ToBool(result);
    }
    if (!result->c_data.c_int) goto nullop2;
    //TPPConst_Quit(result); /* Unnecessary. */
    eval_or(result);
    if (!TPPConst_IsBool(result)) {
     WARN(W_EXPECTED_BOOL_BINARY_RHS,result);
     TPPConst_ToBool(result);
    }
   } else {
nullop2:
    eval_or(NULL);
   }
  } while (TOK == TOK_LAND);
  if (result && TOK == TOK_LOR) {
   WARN(W_CONSIDER_PAREN_AROUND_LAND);
  }
 }
}
PRIVATE void EVAL_CALL eval_lxor(struct TPPConst *result) {
 eval_land(result);
 while (TOK == TOK_LXOR && HAVE_EXTENSION_LXOR) {
  YIELD();
  if (result) {
   int oldbool;
   if (!TPPConst_IsBool(result)) {
    WARN(W_EXPECTED_BOOL_BINARY_LHS,result);
    TPPConst_ToBool(result);
   }
   oldbool = !!result->c_data.c_int;
   eval_land(result);
   if (!TPPConst_IsBool(result)) {
    WARN(W_EXPECTED_BOOL_BINARY_RHS,result);
    TPPConst_ToBool(result);
   }
   result->c_data.c_int ^= oldbool;
  } else {
   eval_land(NULL);
  }
 }
}
PRIVATE void EVAL_CALL eval_lor(struct TPPConst *result) {
 eval_lxor(result);
 while (TOK == TOK_LOR) {
  YIELD();
  if (result) {
   if (!TPPConst_IsBool(result)) {
    WARN(W_EXPECTED_BOOL_BINARY_LHS,result);
    TPPConst_ToBool(result);
   }
   if (result->c_data.c_int) goto nullop2;
   //TPPConst_Quit(result); /* Unnecessary. */
   eval_lxor(result);
   if (!TPPConst_IsBool(result)) {
    WARN(W_EXPECTED_BOOL_BINARY_RHS,result);
    TPPConst_ToBool(result);
   }
  } else {
nullop2:
   eval_lxor(NULL);
  }
 }
}

PRIVATE void EVAL_CALL eval_question(struct TPPConst *result) {
 eval_lor(result);
 if (TOK == '?') {
  YIELD();
  if (result) {
   if (TOK == ':' && HAVE_EXTENSION_GCC_IFELSE) {
    YIELD();
    if (TPPConst_IsTrue(result)) {
     eval_lor(NULL);
    } else {
     TPPConst_Quit(result);
     eval_lor(result);
    }
   } else {
    int was_true = TPPConst_IsTrue(result);
    /* NOTE: Only warn about non-boolean condition if
     *       this isn't a gcc if-then-else-style expression. */
    if (!TPPConst_IsBool(result)) WARN(W_EXPECTED_BOOL,result);
    TPPConst_Quit(result);
    eval_lor(was_true ? result : NULL);
    if (TOK == ':') {
     YIELD();
     eval_question(was_true ? NULL : result);
    } else {
     if (!WARN(W_EXPECTED_COLLON_AFTER_QUESTION)) TPPLexer_SetErr();
     if (!was_true) TPPConst_ZERO(result);
    }
   }
  } else {
   if (TOK != ':') eval_lor(NULL);
   if (TOK == ':') {
    YIELD();
    eval_question(NULL);
   }
  }
 }
}
#ifdef _MSC_VER
#pragma warning(pop)
#endif

PUBLIC int
TPPLexer_Eval(struct TPPConst *result) {
 if unlikely(TOK <= 0) return 0;
 eval_question(result);
 return TOK >= 0;
}

PUBLIC int
TPPLexer_ParsePragma(void) {
 int result = TPPLexer_ParseBuiltinPragma();
 if (!result && !(CURRENT.l_flags&TPPLEXER_FLAG_ERROR) &&
     CURRENT.l_callbacks.c_parse_pragma) {
  result = (*CURRENT.l_callbacks.c_parse_pragma)();
 }
 return result;
}

PRIVATE void
TPPLexer_ClearInclude(void) {
 struct TPPString **iter,**end;
 assert(TPPLexer_Current);
 end = (iter = CURRENT.l_syspaths.il_pathv)+
               CURRENT.l_syspaths.il_pathc;
 for (; iter != end; ++iter)
      assert(*iter),
      TPPString_Decref(*iter);
 free(CURRENT.l_syspaths.il_pathv);
 CURRENT.l_syspaths.il_pathc = 0;
 CURRENT.l_syspaths.il_pathv = NULL;
}

PUBLIC int
TPPLexer_ParseBuiltinPragma(void) {
 switch (TOK) {

  { /* Setup a #include guard for the current source file. */
   struct TPPFile *textfile;
  case KWD_once:
   textfile = TPPLexer_Textfile();
#if TPP_CONFIG_ONELEXER
   assert(builtin_keywords[KWD___LINE__-TOK_KEYWORD_BEGIN]->k_id == KWD___LINE__);
   textfile->f_textfile.f_guard = builtin_keywords[KWD___LINE__-TOK_KEYWORD_BEGIN];
#else
   { /* We must ~really~ lookup the keyword... */
    struct TPPKeyword *keyword;
    keyword = TPPLexer_LookupKeyword("__LINE__",8,0);
    assert(keyword);
    assert(keyword->k_id == KWD___LINE__);
    textfile->f_textfile.f_guard = keyword;
   }
#endif
   /* Prevent other code from attempting to detect a legacy guard. */
   textfile->f_textfile.f_flags   |= TPP_TEXTFILE_FLAG_NOGUARD;
   textfile->f_textfile.f_newguard = NULL;
   return 1;
  } break;

  { /* push/pop a macro definition. */
   struct TPPKeyword *keyword;
   struct TPPConst const_val;
   tok_t mode; int should_undef;
  case KWD_push_macro:
  case KWD_pop_macro:
   mode = TOK;
   YIELD();
   if (TOK == '(') YIELD();
   else WARN(W_EXPECTED_LPAREN);
   should_undef = TOK == KWD_undef;
   if (should_undef) {
    YIELD();
    if (TOK != ',') WARN(W_EXPECTED_COMMA);
    else YIELD();
   }
   for (;;) {
    if unlikely(!TPPLexer_Eval(&const_val)) goto err;
    if (const_val.c_kind != TPP_CONST_STRING) {
     WARN(W_EXPECTED_STRING_AFTER_PUSHMACRO,&const_val);
    } else {
     keyword = TPPLexer_LookupKeyword(const_val.c_data.c_string->s_text,
                                      const_val.c_data.c_string->s_size,1);
     if unlikely(!keyword) goto seterr;
     if (mode == KWD_push_macro) {
      if unlikely(!keyword_pushmacro(keyword)) goto seterr;
      /* Delete the macro definition. */
      if (should_undef && keyword->k_macro) TPPKeyword_Undef(keyword);
     } else {
      keyword_popmacro(keyword,should_undef);
     }
    }
    TPPConst_Quit(&const_val);
    if (TOK != ',') break;
    YIELD();
   }
   if (TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   return 1;
  } break;

  case KWD_region:
  case KWD_endregion:
   /* God damn! This was one of the hardest pragmas to add support for!
    * I still don't know how I did it, but I hope the following line manages to explain it. */
   return 1; /* It's literally meant to improve highlighting in IDEs! */

  { /* Emit a custom message to stderr. */
   int with_paren;
   struct TPPConst message;
  case KWD_message:
   YIELD();
   /* NOTE: In MSVC, the surrounding '(...)' are _not_ optional, but
    *       gcc does make them be (so we must follow its example...) */
   with_paren = TOK == '(';
   if (with_paren) YIELD();
   if unlikely(!TPPLexer_Eval(&message)) goto err;
   if (message.c_kind != TPP_CONST_STRING) {
    WARN(W_EXPECTED_STRING_AFTER_MESSAGE,&message);
   } else {
    if (CURRENT.l_flags&TPPLEXER_FLAG_MESSAGE_LOCATION) {
     struct TPPLCInfo lc_info; TPPLexer_LC(&lc_info);
     fprintf(stderr,CURRENT.l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
             ? "%s(%d,%d) : " : "%s:%d:%d: ",
             TPPLexer_FILE(NULL),
            (int)(lc_info.lc_line+1),
            (int)(lc_info.lc_col+1));
     fprintf(stderr,"#pragma message: ");
     fflush(stderr);
    }
    if (!(CURRENT.l_flags&TPPLEXER_FLAG_MESSAGE_NOLINEFEED)) {
     register char oldch,*poldch;
     oldch = *(poldch = message.c_data.c_string->s_text+
                        message.c_data.c_string->s_size);
     *poldch = '\n';
     fwrite(message.c_data.c_string->s_text,sizeof(char),
            message.c_data.c_string->s_size+1,stderr);
     *poldch = oldch;
    } else {
     fwrite(message.c_data.c_string->s_text,sizeof(char),
            message.c_data.c_string->s_size,stderr);
    }
   }
   TPPConst_Quit(&message);
   if (with_paren && unlikely(TOK != ')')) WARN(W_EXPECTED_RPAREN);
   else YIELD();
   return 1;
  } break;

  { /* Do exactly the same as we'd do for '#error',
     * with a syntax identical to '#pragma message'. */
   struct TPPConst error_message;
   int warning_error;
  case KWD_error:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   if unlikely(!TPPLexer_Eval(&error_message)) goto err;
   if (error_message.c_kind != TPP_CONST_STRING) {
    if (!WARN(W_EXPECTED_STRING_AFTER_PRGERROR,&error_message)) goto err;
   } else {
    warning_error = WARN(W_ERROR,error_message.c_data.c_string->s_text,
                                          error_message.c_data.c_string->s_size);
    TPPString_Decref(error_message.c_data.c_string);
    if unlikely(!warning_error) goto err;
   }
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
  } break;

  { /* Mark a given identifier as deprecated. */
   struct TPPConst ident_name;
   struct TPPKeyword *keyword;
  case KWD_deprecated:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   if unlikely(!TPPLexer_Eval(&ident_name)) goto err;
   if unlikely(ident_name.c_kind != TPP_CONST_STRING) {
    if (!WARN(W_EXPECTED_STRING_AFTER_DEPRECATED,&ident_name)) goto err;
   } else {
    keyword = TPPLexer_LookupKeyword(ident_name.c_data.c_string->s_text,
                                     ident_name.c_data.c_string->s_size,1);
    TPPConst_Quit(&ident_name);
    if unlikely(!keyword || !TPPKeyword_MAKERARE(keyword)) goto err;
    keyword->k_rare->kr_flags |= TPP_KEYWORDFLAG_IS_DEPRECATED;
   }
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   return 1;
  } break;

  { /* Execute a given string as within the preprocessor,
     * discarding all output but keeping everything else. */
   struct TPPConst exec_code;
   struct TPPFile *exec_file;
  case KWD_tpp_exec:
pragma_tpp_exec:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   if unlikely(!TPPLexer_Eval(&exec_code)) goto err;
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   if unlikely(exec_code.c_kind != TPP_CONST_STRING) {
    if (!WARN(W_EXPECTED_STRING_AFTER_TPP_EXEC,&exec_code)) goto err;
   } else {
    char *old_token_begin;
    struct TPPFile *prev_file;
    exec_file = TPPFile_NewExplicitInherited(exec_code.c_data.c_string);
    if unlikely(!exec_file) { TPPString_Decref(exec_code.c_data.c_string); goto err; }
    old_token_begin = TOKEN.t_begin;
    pushfile_inherited(exec_file);
    pushf();
    /* Disable some unnecessary tokens and make sure
     * that macros & preprocessor directives are enabled. */
    CURRENT.l_flags &= ~(TPPLEXER_FLAG_WANTCOMMENTS|
                         TPPLEXER_FLAG_WANTSPACE|
                         TPPLEXER_FLAG_WANTLF|
                         TPPLEXER_FLAG_NO_MACROS|
                         TPPLEXER_FLAG_NO_DIRECTIVES|
                         TPPLEXER_FLAG_NO_BUILTIN_MACROS);
    pusheof();
    /* Yield everything from this file. */
    while (TPPLexer_Yield() > 0);
    popeof();
    popf();
    assert(TOKEN.t_file);
    assert(TOKEN.t_file != &TPPFile_Empty);
    assert(TOKEN.t_file == exec_file);
    assert(TOKEN.t_file->f_prev);
    prev_file = TOKEN.t_file->f_prev;
    assert(old_token_begin >= prev_file->f_begin);
    assert(old_token_begin <= prev_file->f_end);
    assert(old_token_begin <= prev_file->f_pos);
    prev_file->f_pos = old_token_begin;
    TPPLexer_Yield();
   }
   return 1;
  } break;

  { /* Set the flags associated with a given keyword. */
   struct TPPConst const_val;
   struct TPPKeyword *keyword;
   uint32_t new_flags;
  case KWD_tpp_set_keyword_flags:
pragma_tpp_set_keyword_flags:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   if unlikely(!TPPLexer_Eval(&const_val)) goto err;
   if (const_val.c_kind != TPP_CONST_STRING) {
    WARN(W_EXPECTED_STRING_AFTER_TPP_SETF,&const_val);
    keyword = NULL;
   } else {
    keyword = TPPLexer_LookupKeyword(const_val.c_data.c_string->s_text,
                                     const_val.c_data.c_string->s_size,1);
    TPPString_Decref(const_val.c_data.c_string);
    if unlikely(!keyword || !TPPKeyword_MAKERARE(keyword)) goto err;
   }
   if unlikely(TOK != ',') WARN(W_EXPECTED_COMMA);
   else YIELD();
   if unlikely(!TPPLexer_Eval(&const_val)) goto err;
   TPPConst_ToInt(&const_val);
   new_flags = (uint32_t)const_val.c_data.c_int;
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   if likely(keyword) {
    assert(keyword->k_rare);
    keyword->k_rare->kr_flags &= ~(TPP_KEYWORDFLAG_USERMASK);
    keyword->k_rare->kr_flags |= (new_flags&TPP_KEYWORDFLAG_USERMASK);
   }
   return 1;
  } break;

  { /* Configure warning behavior by group/id.
     * NOTE: Warning IDs have been chosen for backwards-compatibility with the old TPP. */
  case KWD_warning:
pragma_warning:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   for (;;) {
    wstate_t newstate; int wset_error;
    struct TPPConst val; char *warning_text;
    switch (TOK) {
     case KWD_push: /* Push warnings. */
      if unlikely(!TPPLexer_PushWarnings()) goto seterr;
      YIELD();
      break;
     case KWD_pop: /* Pop warnings. */
      if (!TPPLexer_PopWarnings() &&
          !WARN(W_CANT_POP_WARNINGS)) goto err;
      YIELD();
      break;

     { /* Set the state of a given set of warnings. */
      if (FALSE) { case KWD_disable:  newstate = WSTATE_DISABLED; }
      if (FALSE) { /*case KWD_warning:*/
                   case KWD_enable:   newstate = WSTATE_WARN; }
      if (FALSE) { case KWD_suppress: newstate = WSTATE_SUPPRESS; }
      if (FALSE) { case KWD_error:    newstate = WSTATE_ERROR; }
      if (FALSE) { case KWD_default:  newstate = WSTATE_DEFAULT; }
      YIELD();
set_warning_newstate:
      if (TOK != ':') {
       /* NOTE: The old TPP didn't have a warning here. */
       if unlikely(WARN(W_EXPECTED_COLLON_AFTER_WARNING)) goto err;
      } else {
       YIELD();
       /* Parse warning numbers/names. */
       while (TOK > 0 && TOK != ',' && TOK != ')') {
        if unlikely(!TPPLexer_Eval(&val)) goto err;
        if (val.c_kind == TPP_CONST_INTEGRAL) {
         wset_error = TPPLexer_SetWarning((int)val.c_data.c_int,newstate);
        } else {
         warning_text = val.c_data.c_string->s_text;
         if (*warning_text == '-') ++warning_text;
         if (*warning_text == 'W') ++warning_text;
         wset_error = TPPLexer_SetWarnings(warning_text,newstate);
        }
        if (wset_error == 2) wset_error = WARN(W_INVALID_WARNING,&val);
        TPPConst_Quit(&val);
        if (!wset_error) goto seterr;
       }
      }
     } break;

     { /* Parse constant:
        * >> #pragma warning("-Wno-comments") // Same as '#pragma warning(disable: "comments")'
        * >> #pragma warning("-Wcomments")    // Same as '#pragma warning(default: "comments")'
        * The old TPP also allowed an integral ID for a new warning mode:
        * >> #pragma warning(-1: 42) // Same as '#pragma warning(error: 42)'
        * >> #pragma warning(0: 42) // Same as '#pragma warning(enable: 42)'
        * >> #pragma warning(1: 42) // Same as '#pragma warning(disable: 42)'
        * >> #pragma warning(2: 42) // Same as '#pragma warning(suppress: 42)'
        * Both of these idea are be merged here!
        */
     default:
      if unlikely(!TPPLexer_Eval(&val)) goto err;
      if (val.c_kind == TPP_CONST_INTEGRAL) {
       /* NOTE: Technically, the old TPP allowed numbers 'x' greater than 2 to
        *       be specified here, which would in return suppress the warning
        *       an additional 'x-1' times.
        *       This behavior is not supported by the new TPP!
        */
            if (val.c_data.c_int < 0) newstate = WSTATE_ERROR;
       else if (val.c_data.c_int == 0) newstate = WSTATE_WARN;
       else if (val.c_data.c_int == 1) newstate = WSTATE_DISABLED;
       else newstate = WSTATE_SUPPRESS;
       goto set_warning_newstate;
      } else if (val.c_kind == TPP_CONST_STRING) {
       /* Very nice-looking warning directives:
        * >> #pragma warning(push,"-Wno-syntax")
        * >> ... // Do something ~nasty~.
        * >> #pragma warning(pop)
        */
       newstate = WSTATE_ERROR;
       warning_text = val.c_data.c_string->s_text;
       if (*warning_text == '-') ++warning_text;
       if (*warning_text == 'W') ++warning_text;
            if (!memcmp(warning_text,"no-",3*sizeof(char))) warning_text += 3,newstate = WSTATE_DISABLED;
       else if (!memcmp(warning_text,"def-",4*sizeof(char))) warning_text += 4,newstate = WSTATE_DEFAULT;
       else if (!memcmp(warning_text,"sup-",4*sizeof(char))) warning_text += 4,newstate = WSTATE_SUPPRESS;
       else if (!memcmp(warning_text,"suppress-",9*sizeof(char))) warning_text += 9,newstate = WSTATE_SUPPRESS;
       wset_error = TPPLexer_SetWarnings(warning_text,newstate);
       if (wset_error == 2) wset_error = WARN(W_INVALID_WARNING,&val);
       TPPString_Decref(val.c_data.c_string);
       if unlikely(!wset_error) goto seterr;
      } else {
       if unlikely(!WARN(W_EXPECTED_WARNING_NAMEORID,&val)) goto err;
      }
     } break;
    }
    if (TOK != ',') break;
    YIELD();
   }
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   return 1;
  } break;

  { /* Configure TPP extensions from usercode. */
   struct TPPConst extname;
  case KWD_extension:
pragma_extension:
   YIELD();
   if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
   else YIELD();
   for (;;) {
    if (TOK == KWD_push) {
     if unlikely(!TPPLexer_PushExtensions()) goto seterr;
     goto yield_after_extension;
    } else if (TOK == KWD_pop) {
     if unlikely(!TPPLexer_PopExtensions() &&
                 !WARN(W_CANT_POP_EXTENSIONS)) goto err;
yield_after_extension:
     YIELD();
    } else {
     if unlikely(!TPPLexer_Eval(&extname)) goto err;
     if (extname.c_kind != TPP_CONST_STRING) {
      if unlikely(!WARN(W_EXPECTED_STRING_AFTER_EXTENSION,&extname)) goto err;
     } else {
      int ext_error,mode = 1;
      char const *name = extname.c_data.c_string->s_text;
      if (*name == '-') ++name;
      if (*name == 'f') ++name;
      if (!memcmp(name,"no-",3*sizeof(char))) name += 3,mode = 0;
      ext_error = TPPLexer_SetExtension(name,mode);
      if unlikely(!ext_error) ext_error = WARN(W_UNKNOWN_EXTENSION,name);
      TPPString_Decref(extname.c_data.c_string);
      if unlikely(!ext_error) goto err;
     }
    }
    if (TOK != ',') break;
    YIELD();
   }
   if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
   else YIELD();
   return 1;
  } break;

  { /* TPP-specific pragma namespace. */
  case KWD_TPP:
   YIELD();
   switch (TOK) {

    case KWD_warning              : goto pragma_warning;
    case KWD_extension            : goto pragma_extension;
    case KWD_tpp_exec             : goto pragma_tpp_exec;
    case KWD_tpp_set_keyword_flags: goto pragma_tpp_set_keyword_flags;

    { /* Configure system #include-paths. */
    case KWD_include_path:
     YIELD();
     if unlikely(TOK != '(') WARN(W_EXPECTED_LPAREN);
     else YIELD();
     while (TOK != ')') {
      switch (TOK) {
       { /* push/pop the system #include-path. */
        if (FALSE) { case KWD_clear: TPPLexer_ClearInclude(); }
        if (FALSE) { case KWD_push:  if unlikely(!TPPLexer_PushInclude()) goto seterr; }
        if (FALSE) { case KWD_pop:   if unlikely(!TPPLexer_PopInclude() && !WARN(W_CANT_POP_INCLUDE_PATH)) goto err; }
        YIELD();
       } break;
       {
        int mode,error;
        struct TPPConst path_string;
       case '+': YIELD();
       default:  mode = 1;
        if (FALSE) { case '-': mode = 0; YIELD(); }
        if unlikely(!TPPLexer_Eval(&path_string)) goto err;
        if (path_string.c_kind != TPP_CONST_STRING) {
         WARN(W_EXPECTED_STRING_AFTER_TPP_INCPTH,&path_string);
        } else {
         char *path; size_t size = path_string.c_data.c_string->s_size;
         if (path_string.c_data.c_string->s_refcnt == 1) {
          path = path_string.c_data.c_string->s_text;
         } else {
          path = (char *)malloc((size+1)*sizeof(char));
          if unlikely(!path) goto seterr;
          memcpy(path,path_string.c_data.c_string->s_text,
                (size+1)*sizeof(char));
         }
         error = mode ? TPPLexer_AddIncludePath(path,size)
                      : TPPLexer_DelIncludePath(path,size);
         if unlikely(mode && !error) goto seterr;
         if unlikely(error == 2) error = 0;
         if unlikely(!error) {
          WARN(mode ? W_INCLUDE_PATH_ALREADY_EXISTS
                             : W_UNKNOWN_INCLUDE_PATH,
                        path,size);
         }
         if (path != path_string.c_data.c_string->s_text) free(path);
         TPPString_Decref(path_string.c_data.c_string);
        }
        break;
       }
      }
      if (TOK != ',') break;
      YIELD();
     }
     if unlikely(TOK != ')') WARN(W_EXPECTED_RPAREN);
     else YIELD();
     return 1;
    } break;

    default: break; /* Reserved for future use (don't warn about unknown tpp-pragma). */
   }
  } break;

  { /* Emulate some GCC pragmas. */
  case KWD_GCC:
   YIELD();
   if (TOK == KWD_diagnostic) {
    YIELD();
    switch (TOK) {
     /* Push/Pop warnings */
     case KWD_push: if unlikely(!TPPLexer_PushWarnings()) goto seterr; break;
     case KWD_pop: if (!TPPLexer_PopWarnings() && !WARN(W_CANT_POP_WARNINGS)) goto err; break;

     { /* Configure the mode for a GCC diagnostic. */
      wstate_t newmode; struct TPPConst group_name;
      if (FALSE) { case KWD_warning: newmode = WSTATE_WARN; }
      if (FALSE) { case KWD_error:   newmode = WSTATE_ERROR; }
      if (FALSE) { case KWD_ignored: newmode = WSTATE_DISABLED; }
      YIELD();
      if unlikely(!TPPLexer_Eval(&group_name)) goto err;
      if (group_name.c_kind != TPP_CONST_STRING) {
       if unlikely(!WARN(W_EXPECTED_STRING_AFTER_GCC_DIAG,&group_name)) goto err;
      } else {
       int wset_error;
       char const *warning_name = group_name.c_data.c_string->s_text;
       if (*warning_name == '-') ++warning_name;
       if (*warning_name == 'W') ++warning_name;
       wset_error = TPPLexer_SetWarnings(warning_name,newmode);
       if (wset_error == 2) wset_error = WARN(W_INVALID_WARNING,&group_name);
       TPPString_Decref(group_name.c_data.c_string);
       if unlikely(!wset_error) goto seterr;
      }
      return 1;
     } break;

     default: return 0;
    }
    YIELD();
    return 1;
   } else if (TOK == KWD_system_header) {
    struct TPPFile *textfile;
    YIELD();
    textfile = TPPLexer_Textfile();
    /* Set the system-header flag in the current file (thus suppressing warnings) */
    textfile->f_textfile.f_flags |= TPP_TEXTFILE_FLAG_SYSHEADER;
    return 1;
   } else if (CURRENT.l_callbacks.c_parse_pragma_gcc) {
    /* Call the user-provided GCC pragma hook. */
    return (*CURRENT.l_callbacks.c_parse_pragma_gcc)();
   }
  } break;

  default:
   LOG(LOG_PRAGMA,("Unknown pragma '%.*s'\n",
                  (int)(TOKEN.t_end-TOKEN.t_begin),
                   TOKEN.t_begin));
   break;
 }
 return 0;
seterr: TPPLexer_SetErr();
err:    return 0;
}

#undef TPPLexer_SetErr
#undef TPPLexer_UnsetErr
PUBLIC int TPPLexer_SetErr(void) { return TPPLexer_SetErr_inline(); }
PUBLIC int TPPLexer_UnsetErr(void) { return TPPLexer_UnsetErr_inline(); }

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4701)
#endif

#if defined(TPP_WARNF)
#define WARNF      TPP_WARNF
#elif TPP_CONFIG_DEBUG && (defined(_WIN32) || defined(WIN32))
PRIVATE void tpp_warnf(char const *fmt, ...) {
 char buffer[1024];
 va_list args;
 size_t bufsiz;
 va_start(args,fmt);
#ifdef _MSC_VER
 _vsnprintf(buffer,sizeof(buffer),fmt,args);
#else
  vsnprintf(buffer,sizeof(buffer),fmt,args);
#endif
 va_end(args);
 bufsiz = strlen(buffer);
 fwrite(buffer,sizeof(char),bufsiz,stderr);
 OutputDebugStringA(buffer);
}
#define WARNF      tpp_warnf
#else
#define WARNF(...) fprintf(stderr,__VA_ARGS__)
#endif

#define Q(x) TPP_WARNF_QUOTE_BEGIN x TPP_WARNF_QUOTE_END

PUBLIC int TPPLexer_Warn(int wnum, ...) {
 va_list args; char const *true_filename;
 char const *macro_name = NULL; struct TPPKeyword *kwd;
 int macro_name_size,behavior; wgroup_t const *wgroups;
 struct TPPString *temp_string = NULL;
 struct TPPFile *effective_file;
#if 0 /* Don't warn when already in an error-state. */
 if (CURRENT.l_flags&TPPLEXER_FLAG_ERROR) return 0;
#endif
 ++CURRENT.l_warncount; /* Always count warnings, even if they'll be dismissed. */
 if (CURRENT.l_flags&TPPLEXER_FLAG_NO_WARNINGS) return 1; /* Warnings are disabled. */
 /* Check for per-warning behavior, as configured by the current lexer. */
 behavior = TPPLexer_InvokeWarning(wnum);
 if (behavior == TPP_WARNINGMODE_IGNORE) return 1; /* Warning is being ignored. */
 /* Turn warnings into errors. */
 if (behavior == TPP_WARNINGMODE_WARN) {
  /* Ignore warnings in system headers. */
  if (!(CURRENT.l_flags&TPPLEXER_FLAG_WSYSTEMHEADERS)) {
   struct TPPFile *textfile = TPPLexer_Textfile(); /* Walk the chain of a cached #include file. */
   if (textfile->f_textfile.f_flags&TPP_TEXTFILE_FLAG_SYSHEADER) return 1;
  }
  /* Turn warnings into errors. */
  if (CURRENT.l_flags&TPPLEXER_FLAG_WERROR) behavior = TPP_WARNINGMODE_ERROR;
 }
 va_start(args,wnum);
#define TOK_S       Q("%.*s")
#define TOK_A        (int)(TOKEN.t_end-TOKEN.t_begin),TOKEN.t_begin
#define ARG(T)        va_arg(args,T)
#define FILENAME()   (ARG(struct TPPFile *)->f_name)
#define KWDNAME()    (ARG(struct TPPKeyword *)->k_name)
#define TOK_NAME()   (kwd = TPPLexer_LookupKeywordID(ARG(tok_t)),kwd ? kwd->k_name : (char *)"??" "?")
#define CONST_STR()  (temp_string = TPPConst_ToString(ARG(struct TPPConst *)),temp_string ? temp_string->s_text : NULL)
 {
  struct TPPFile *macro_file;
  effective_file = NULL;
  macro_file = TOKEN.t_file;
  while (macro_file->f_kind == TPPFILE_KIND_EXPLICIT &&
         macro_file->f_prev) macro_file = macro_file->f_prev;
  if (macro_file->f_kind == TPPFILE_KIND_MACRO) {
   macro_name = macro_file->f_name;
   macro_name_size = (int)macro_file->f_namesize;
   effective_file = macro_file;
  } else {
   effective_file = TPPLexer_Textfile();
  }
 }
 true_filename = TPPFile_Filename(effective_file,NULL);
 TPP_TEXTFILE_FLAG_INTERNAL;
 switch (wnum) {
  { /* Special case for #if without #endif (display file/line of the #if) */
   struct TPPIfdefStackSlot *ifdef_slot;
  case W_IF_WITHOUT_ENDIF:
   ifdef_slot = ARG(struct TPPIfdefStackSlot *);
   WARNF(CURRENT.l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
         ? "%s(%d) : " : "%s:%d: ",
         ifdef_slot->iss_file->f_name,ifdef_slot->iss_line+1);
   macro_name = NULL;
  } break;
#ifdef TPP_USERLINES
  TPP_USERLINES
#endif

  {
  default:
   if (effective_file->f_kind == TPPFILE_KIND_TEXT &&
      (effective_file->f_textfile.f_flags&TPP_TEXTFILE_FLAG_INTERNAL)) {
    /* Don't display line/col information! */
    WARNF("%s: ",true_filename);
   } else {
    struct TPPLCInfo lc_info;
#if 1
    /* For better compatibility with custom parsers,
     * make sure that the token pointer can be used. */
    if (CURRENT.l_token.t_begin < CURRENT.l_token.t_file->f_begin ||
        CURRENT.l_token.t_begin > CURRENT.l_token.t_file->f_end) {
     TPPLexer_LC(&lc_info);
     macro_name = NULL;
    } else
#endif
    {
     TPPLexer_TRUE_LC(&lc_info);
    }
    WARNF(CURRENT.l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
          ? "%s(%d,%d) : " : "%s:%d:%d: ",
          true_filename,
          (int)(lc_info.lc_line+1),
          (int)(lc_info.lc_col+1));
   }
  } break; 
 }
 if (macro_name) WARNF("In macro " Q("%.*s") ": ",macro_name_size,macro_name);
#ifdef _MSC_VER
#define IFC(x)  if(0,x)
#else
#define IFC     if
#endif
 IFC(sizeof(TPP_WARNF_ERROR_BEGIN) != sizeof("") ||
     sizeof(TPP_WARNF_WARN_BEGIN)  != sizeof("")) {
  if (behavior == TPP_WARNINGMODE_ERROR)
       WARNF(TPP_WARNF_ERROR_BEGIN);
  else WARNF(TPP_WARNF_WARN_BEGIN);
 }
 WARNF("%c%04d(",(behavior == TPP_WARNINGMODE_ERROR) ? 'E' : 'W',wnum);
 /* print a list of all groups associated with the warning. */
 {
  unsigned int wid = wnum2id(wnum);
  if (wid_isvalid(wid)) {
   wgroups = w_associated_groups[wid-WG_COUNT];
#if 1
   if (*wgroups >= 0) WARNF("\"-W%s\"",wgroup_names[*wgroups]);
#else
   while (*wgroups >= 0) {
    char const *name = wgroup_names[*wgroups++];
    WARNF("\"-W%s\"%s",name,(*wgroups >= 0) ? "," : "");
   }
#endif
  }
 }
 IFC(sizeof(TPP_WARNF_ERROR_END) != sizeof("") ||
     sizeof(TPP_WARNF_WARN_END)  != sizeof("")) {
  if (behavior == TPP_WARNINGMODE_ERROR)
       WARNF(")" TPP_WARNF_ERROR_END ": ");
  else WARNF(")" TPP_WARNF_WARN_END ": ");
 } else {
  WARNF("): ");
 }
#undef IFC
 switch (wnum) {
#define DECLARE_WARNING_MESSAGES
#define WARNING_MESSAGE(name,expr) case name: expr; break;
#include "tpp-defs.inl"
#undef WARNING_MESSAGE
#undef DECLARE_WARNING_MESSAGES
  default: WARNF("? %d",wnum); break;
 }
 if (temp_string) TPPString_Decref(temp_string);
#undef FILENAME
#undef KWDNAME
#undef ARG
 va_end(args);
 WARNF("\n");
 if (macro_name) {
  struct TPPLCInfo lc_info;
#if 1
  struct TPPFile *iter = TOKEN.t_file;
  for (;;) {
   iter = iter->f_prev;
   assert(iter);
   if (iter->f_kind != TPPFILE_KIND_EXPLICIT) {
    TPPFile_LCAt(iter,&lc_info,iter->f_pos);
    WARNF(CURRENT.l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
          ? "%s(%d,%d) : " : "%s:%d:%d: ",
          TPPFile_Filename(iter,NULL),
         (int)(lc_info.lc_line+1),
         (int)(lc_info.lc_col+1));
    /* NOTE: The last file in the #include chain should
     *       always be TPPFile_Empty, which is a text file. */
    if (iter->f_kind == TPPFILE_KIND_TEXT) {
     WARNF("See reference to effective code location\n");
     break;
    } else {
     WARNF("See reference to macro " Q("%.*s") "\n",
          (unsigned int)iter->f_namesize,iter->f_name);
    }
   }
  }
  WARNF("\n"); /* Additional line-feed for better readability. */
#else
  TPPLexer_LC(&lc_info);
  WARNF(CURRENT.l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
        ? "%s(%d,%d) : " : "%s:%d:%d: ",
        TPPLexer_FILE(NULL),
       (int)(lc_info.lc_line+1),
       (int)(lc_info.lc_col+1));
  WARNF("See reference to effective code location\n");
#endif
 }
 fflush(stderr);
 if (behavior == TPP_WARNINGMODE_ERROR) {
  /* NOTE: On error, the current token _must_ be set to TOK_ERR */
  TPPLexer_SetErr();
  return 0;
 }
 return 1;
}
#undef Q

#ifdef _MSC_VER
#pragma warning(pop)
#endif


#ifdef __cplusplus
}
#endif


#endif /* !GUARD_TPP_C */
