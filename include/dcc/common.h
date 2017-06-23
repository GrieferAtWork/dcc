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
#ifndef GUARD_DCC_COMMON_H
#define GUARD_DCC_COMMON_H 1

#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#define _CRT_NONSTDC_NO_WARNINGS
#define snprintf  _snprintf
#define vsnprintf _vsnprintf
#endif

#ifndef DCC
#ifdef DCC_PRIVATE_API
#   define DCC(x) x
#else
#   define DCC(x) DCC_##x
#endif
#endif

#ifndef DCC_DEBUG
#if defined(NDEBUG) || \
    defined(_RELEASE) || \
    defined(RELEASE)
#  define DCC_DEBUG 0
#elif defined(_DEBUG) || \
      defined(DEBUG)
#  define DCC_DEBUG 1
#else
#  define DCC_DEBUG 1
#endif
#endif


/* DCC Configuration (NOTE: Target configs are in "/include/dcc/target.h") */
#define DCC_CONFIG_LIBPREFIX  "lib"


#ifndef __has_attribute
#define __has_attribute(x) 0
#endif
#ifndef __has_builtin
#define __has_builtin(x) 0
#endif
#ifndef __has_include
#define __has_include(x) 0
#endif


#if defined(_MSC_VER) || __has_include(<crtdbg.h>)
#ifndef __INTELLISENSE__
#if defined(DCC_PRIVATE_API) && DCC_DEBUG
/* Make use of MSVC's builtin memory leak detector. */
#define _CRTDBG_MAP_ALLOC
#include <stdlib.h>
#include <crtdbg.h>
#endif
#endif
#endif

#ifdef __cplusplus
#define DCC_DECL_BEGIN extern "C" {
#define DCC_DECL_END   }
#else
#define DCC_DECL_BEGIN
#define DCC_DECL_END
#endif

#define DCC_PRIVATE_PP_STR(x) #x
#define DCC_PP_STR(x)  DCC_PRIVATE_PP_STR(x)

#define DCC_PRIVATE_PP_CAT(a,b) a##b
#define DCC_PRIVATE_PP_CAT3(a,b,c) a##b##c
#define DCC_PP_CAT(a,b)    DCC_PRIVATE_PP_CAT(a,b)
#define DCC_PP_CAT3(a,b,c) DCC_PRIVATE_PP_CAT3(a,b,c)


#define DCC_TPP_FILE(x)  DCC_PP_STR(DCC_TPP_PATH/x)

/* Configure TPP */
#define TPP_USERDEFS     <dcc/def-keywords.inl>
#define TPP_CONFIG_USERDEFINED_KWD_PUSH
#define TPP_CONFIG_USERDEFINED_KWD_POP
#define TPP_CONFIG_DEBUG      DCC_DEBUG
#define TPP_CONFIG_MINMACRO   1 /*  */
#define TPP_CONFIG_GCCFUNC    1 /* Enable GCC builtin functions in TPP. */
#define TPP_CONFIG_MINGCCFUNC 2 /* Setup minimal default hooks for builtin GCC function (most are added manually) */

/**********************************************/
/*	Compiler/Host automatic configuration...   */
/**********************************************/

#ifdef __GNUC__
#if defined(__GNUC_MINOR__) && defined(__GNUC_PATCH__)
#   define DCC_COMPILER_GCC_VERSION (__GNUC__*10000+__GNUC_MINOR__*100+__GNUC_PATCH__)
#elif defined(__GNUC_MINOR__)
#   define DCC_COMPILER_GCC_VERSION (__GNUC__*10000+__GNUC_MINOR__*100)
#else
#   define DCC_COMPILER_GCC_VERSION (__GNUC__*10000)
#endif
#   define DCC_COMPILER_GCC(a,b,c) ((a)*10000+(b)*100+(c) <= DCC_COMPILER_GCC_VERSION)
#else
#   define DCC_COMPILER_GCC(a,b,c) 0
#endif

#if defined(_MSC_VER) || defined(__GNUC__) || \
    defined(__DCC_VERSION__)
#   define DCC_COMPILER_ALIGNOF  __alignof
#elif defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#   define DCC_COMPILER_ALIGNOF  _Alignof
#elif __has_builtin(__builtin_alignof)
#   define DCC_COMPILER_ALIGNOF  __builtin_alignof
#else
#   define DCC_COMPILER_ALIGNOF(T) ((size_t)&((struct { char x; T s; } *)0)->s)
#endif

#ifndef DCC_COMPILER_OFFSETOF
#if defined(__GNUC__) || __has_builtin(__builtin_offsetof)
#   define DCC_COMPILER_OFFSETOF       __builtin_offsetof
#else
#   define DCC_COMPILER_OFFSETOF(m,s) ((size_t)&((m *)0)->s)
#endif
#endif /* !DCC_COMPILER_OFFSETOF */

#ifndef DCC_COMPILER_OFFSETAFTER
#define DCC_COMPILER_OFFSETAFTER(m,s) ((size_t)(&((m *)0)->s+1))
#endif /* !DCC_COMPILER_OFFSETAFTER */

#if defined(__TPP_EVAL) && __TPP_VERSION__ >= 200
#   define DCC_COMPILER_STRLEN(s) __TPP_EVAL(#(s))
#else
#   define DCC_COMPILER_STRLEN(s) ((sizeof(s)/sizeof(char))-1)
#endif

#ifdef __DCC_VERSION__
#pragma warning("-Wno-declaration-in-for")
#endif


#define DCCDAT         extern
#define DCCFUN         extern
#define DCC_PRIVATE    static
#define DCC_PUBLIC     /* nothing */
#if defined(__cplusplus) || defined(inline)
#   define DCC_LOCAL   static inline
#elif defined(_MSC_VER)
#   define DCC_LOCAL   static __inline
#elif defined(__GNUC__) || defined(__TCC__) || defined(__DCC_VERSION__)
#   define DCC_LOCAL   static __inline__
#else
#   define DCC_LOCAL   static
#endif
#if defined(__ELF__) || \
   (!defined(__GNUC__) && __has_attribute(visibility)) || \
   (defined(__GNUC__) && !defined(_WIN32) && !defined(__CYGWIN__))
#   define DCC_INTERN         __attribute__((__visibility__("hidden")))
#   define DCC_INTDEF  extern __attribute__((__visibility__("hidden")))
#elif defined(_MSC_VER)
#   define DCC_INTERN  extern
#   define DCC_INTDEF  extern
#elif 1
#   define DCC_INTERN  /* nothing */
#   define DCC_INTDEF  extern
#endif

#ifdef DCC_PRIVATE_API
#   define PRIVATE        DCC_PRIVATE
#   define PUBLIC         DCC_PUBLIC
#   define LOCAL          DCC_LOCAL
#   define INTERN         DCC_INTERN
#   define INTDEF         DCC_INTDEF
#endif

#ifndef DCC_ATTRIBUTE_NORETURN
#ifdef _MSC_VER
#   define DCC_ATTRIBUTE_NORETURN  __declspec(noreturn)
#elif defined(__GNUC__) || __has_attribute(noreturn)
#   define DCC_ATTRIBUTE_NORETURN  __attribute__((__noreturn__))
#else
#   define DCC_NO_ATTRIBUTE_NORETURN
#   define DCC_ATTRIBUTE_NORETURN
#endif
#endif /* !DCC_ATTRIBUTE_NORETURN */

#ifndef DCC_ATTRIBUTE_DLLIMPORT
#ifdef _MSC_VER
#   define DCC_ATTRIBUTE_DLLIMPORT  __declspec(dllimport)
#   define DCC_ATTRIBUTE_DLLEXPORT  __declspec(dllexport)
#elif defined(__DCC_VERSION__) && defined(__PE__)
#   define DCC_ATTRIBUTE_DLLIMPORT  __attribute__((__dllimport__))
#   define DCC_ATTRIBUTE_DLLEXPORT  __attribute__((__dllexport__))
#elif (!defined(__GNUC__) && __has_attribute(dllimport)) || \
      (defined(__GNUC__) && (defined(_WIN32) || defined(__CYGWIN__)))
#   define DCC_ATTRIBUTE_DLLIMPORT  __attribute__((__dllimport__))
#   define DCC_ATTRIBUTE_DLLEXPORT  __attribute__((__dllexport__))
#else
#   define DCC_NO_ATTRIBUTE_DLLIMPORT
#   define DCC_NO_ATTRIBUTE_DLLEXPORT
#   define DCC_ATTRIBUTE_DLLIMPORT
#   define DCC_ATTRIBUTE_DLLEXPORT
#endif
#endif /* !DCC_ATTRIBUTE_DLLIMPORT */


#ifndef DCC_LIKELY
#if defined(__GNUC__) || __has_builtin(__builtin_expect)
#   define DCC_LIKELY(x)    (__builtin_expect(!!(x),1))
#   define DCC_UNLIKELY(x)  (__builtin_expect(!!(x),0))
#else
#   define DCC_LIKELY
#   define DCC_UNLIKELY
#endif
#endif /* !DCC_LIKELY */

#ifndef DCC_UNREACHABLE
#if defined(__GNUC__) || __has_builtin(__builtin_unreachable)
#   define DCC_UNREACHABLE   __builtin_unreachable
#elif defined(_MSC_VER)
#   define DCC_UNREACHABLE() __assume(0)
#else
#   define DCC_UNREACHABLE() (void)0
#endif
#endif /* !DCC_UNREACHABLE */

#ifndef DCC_ATTRIBUTE_FASTCALL
#if defined(_MSC_VER)
#   define DCC_ATTRIBUTE_FASTCALL  __fastcall
#elif (defined(__i386__) || defined(__i386) || defined(i386)) || \
      (__has_attribute(fastcall))
#   define DCC_ATTRIBUTE_FASTCALL  __attribute__((__fastcall__))
#else
#   define DCC_NO_ATTRIBUTE_FASTCALL
#   define DCC_ATTRIBUTE_FASTCALL  /* nothing */
#endif
#endif /* !DCC_ATTRIBUTE_FASTCALL */

#ifndef DCC_ATTRIBUTE_STDCALL
#if defined(_MSC_VER)
#   define DCC_ATTRIBUTE_STDCALL  __stdcall
#elif defined(__GNUC__) || defined(__DCC_VERSION__) || \
      __has_attribute(stdcall)
#   define DCC_ATTRIBUTE_STDCALL  __attribute__((__stdcall__))
#else
#   define DCC_NO_ATTRIBUTE_STDCALL
#   define DCC_ATTRIBUTE_STDCALL  /* nothing */
#endif
#endif /* !DCC_ATTRIBUTE_STDCALL */

DCC_DECL_BEGIN

#ifndef DCC_BREAKPOINT
#ifdef _MSC_VER
extern void __debugbreak(void);
#   pragma intrinsic(__debugbreak)
#   define DCC_BREAKPOINT  __debugbreak
#elif __has_builtin(__builtin_breakpoint)
#   define DCC_BREAKPOINT    __builtin_breakpoint
#elif (defined(__i386__) || defined(__i386) || defined(i386) || defined(__x86_64__))
#if defined(__GNUC__) || defined(__DCC_VERSION__)
#   define DCC_BREAKPOINT() (__extension__({ __asm__ __volatile__("int {$}3\n" : : : "memory"); (void)0; }))
#elif defined(__TCC__)
#   define DCC_BREAKPOINT() ({ __asm__ __volatile__("int $3\n" : : : "memory"); (void)0; })
#endif
#else
#   define DCC_NO_BREAKPOINT
#   define DCC_BREAKPOINT() (void)0
#endif
#endif /* !DCC_BREAKPOINT */


DCCFUN
#ifdef DCC_NO_BREAKPOINT
       DCC_ATTRIBUTE_NORETURN
#endif
void dcc_assertion_failed(char const *expr, char const *file, int line);
DCCFUN
#ifdef DCC_NO_BREAKPOINT
       DCC_ATTRIBUTE_NORETURN
#endif
void dcc_assertion_failedf(char const *expr, char const *file, int line, char const *fmt, ...);

DCC_DECL_END


#if DCC_DEBUG
#   define DCC_ASSERT(expr)      ((expr) || (dcc_assertion_failed(#expr,__FILE__,__LINE__),DCC_BREAKPOINT(),0))
#   define DCC_ASSERTF(expr,...) ((expr) || (dcc_assertion_failedf(#expr,__FILE__,__LINE__,__VA_ARGS__),DCC_BREAKPOINT(),0))
#elif defined(_MSC_VER)
#   define DCC_ASSERT __assume
#elif __has_builtin(__builtin_assume)
#   define DCC_ASSERT __builtin_assume
#else
#   define DCC_ASSERT(expr)       (void)0
#   define DCC_ASSERTF(expr,...)  (void)0
#endif

#ifndef DCC_ASSERTF
#   define DCC_ASSERTF(expr,...) DCC_ASSERT(expr)
#endif

/* Returns an integral with 'n' low bits set. */
#define DCC_BITS(n) ((1 << (n))-1)

#define DCC_PRIVATE_BINC(x) \
 ((((x)&00000000000000001u)\
 |(((x)&00000000000000010u) >> 2)\
 |(((x)&00000000000000100u) >> 4)\
 |(((x)&00000000000001000u) >> 6)\
 |(((x)&00000000000010000u) >> 8)\
 |(((x)&00000000000100000u) >> 10)\
 |(((x)&00000000001000000u) >> 12)\
 |(((x)&00000000010000000u) >> 14)\
 |(((x)&00000000100000000u) >> 16)\
 |(((x)&00000001000000000u) >> 18)\
 |(((x)&00000010000000000u) >> 20)\
 |(((x)&00000100000000000u) >> 22)\
 |(((x)&00001000000000000u) >> 24)\
 |(((x)&00010000000000000u) >> 26)\
 |(((x)&00100000000000000u) >> 28)\
 |(((x)&01000000000000000u) >> 30))\
 &0xffffu)

/* Create a binary constant integral. */
#if defined(__DCC_VERSION__)
#   define DCC_BINC(x)          0b##x##u
#else
#   define DCC_BINC(x)          DCC_PRIVATE_BINC(0##x##u)
#endif


#ifdef DCC_PRIVATE_API
#undef assert
#undef assertf
#   define assert   DCC_ASSERT
#   define assertf  DCC_ASSERTF
#   define BITS     DCC_BITS
#   define likely   DCC_LIKELY
#   define unlikely DCC_UNLIKELY
#   define B        DCC_BINC
#endif

#ifdef _MSC_VER
#   define DCC_MACRO_COND(x) (0,x)
#   define DCC_MACRO_FALSE    0,0
#else
#   define DCC_MACRO_COND     /* nothing */
#   define DCC_MACRO_FALSE    0
#endif


/* NOTE: Like usual, the first full release will have these set to '100' */
#define DCC_COMPILER_VERSION 1 /* Compiler version. */
#define DCC_API_VERSION      1 /* Api version (Version of this api). */

#include <stdint.h>
#include <stddef.h>

DCC_DECL_BEGIN

/* DCC Allocation functions.
 * These call the associated functions from <stdlib.h>, but
 * upon failure will start clearing unit/global caches
 * in an attempt to free up available memory before trying again.
 * If all attempts fail, 'DCC_AllocFailed' is called.
 * @param: flush_exclude: A set of 'DCCUNIT_FLUSHFLAG_*' or'd together with
 *                        a set of 'DCCCOMPILER_FLUSHFLAG_*'.
 *                     >> The flags provided describe flush operations
 *                        that shall not be performed, thus allowing save
 *                        memory management even for data blocks that would
 *                        normally be affected by flush operations.
 */
DCCFUN void *DCC_Malloc(size_t s, uint32_t flush_exclude);
DCCFUN void *DCC_Calloc(size_t s, uint32_t flush_exclude);
DCCFUN void *DCC_Realloc(void *p, size_t s, uint32_t flush_exclude);
DCCFUN void  DCC_Free(void *p);

#if DCC_DEBUG && !defined(__INTELLISENSE__)
DCCFUN void *DCC_dbg_realloc(void *p, size_t s, char const *f, int l);
#undef realloc
#define realloc(p,s) DCC_dbg_realloc(p,s,__FILE__,__LINE__)
#endif

/* Emit warnings about failed allocations
 * Depending on the warning state, a lexer error may be set. */
DCCFUN void DCC_AllocFailed(size_t s);

#if DCC_DEBUG && defined(_CRTDBG_MAP_ALLOC) && \
   !defined(__INTELLISENSE__)
DCC_LOCAL void *dcc_check_pointer(void *p, size_t s) {
 if (!p) DCC_AllocFailed(s);
 return p;
}
#define DCC_Malloc(s,flush_exclude)    dcc_check_pointer(malloc(s),s)
#define DCC_Calloc(s,flush_exclude)    dcc_check_pointer(calloc(1,s),s)
#define DCC_Realloc(p,s,flush_exclude) dcc_check_pointer(realloc(p,s),s)
#define DCC_Free(p)                    free(p)
#endif

DCC_DECL_END

#endif /* !GUARD_DCC_COMMON_H */
