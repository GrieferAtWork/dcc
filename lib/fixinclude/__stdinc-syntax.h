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
#pragma once

#ifndef __INTELLISENSE__
#error "This file is only meant to fix syntax highlighting with intellisense"
#endif
#ifndef __cplusplus
#error "Using this file in c-mode may cause invalid syntax highlighting"
#endif

/* Helper magic. */
template<class T> struct ____INTELLISENE_remlval { typedef T __type; };
template<class T> struct ____INTELLISENE_remlval<T &> { typedef T __type; };
struct ____INTELLISENE_void_helper {};
template<class T> T operator , (T &&,____INTELLISENE_void_helper);
template<class T> T const operator , (T const &&,____INTELLISENE_void_helper);
void ____INTELLISENE_typeof_helper(____INTELLISENE_void_helper &&);
template<class T> typename ____INTELLISENE_remlval<T>::__type ____INTELLISENE_typeof_helper(T&&);
template<class T> typename ____INTELLISENE_remlval<T>::__type const ____INTELLISENE_typeof_helper(T const &&);
template<class T, class ...Args> T (____INTELLISENE_typeof_helper(T (&)(Args...)))(Args...);
template<class T, class ...Args> T (____INTELLISENE_typeof_helper(T (&)(Args...,...)))(Args...,...);
template<class T1, class T2> struct ____INTELLISENE_sametype {enum{__val=false};};
template<class T1> struct ____INTELLISENE_sametype<T1,T1> {enum{__val=true};};
template<bool> struct ____INTELLISENE_static_if_helper {};
template<> struct ____INTELLISENE_static_if_helper<true> { bool __is_true__(); };
template<bool C, class T> struct ____INTELLISENE_enableif { typedef T __type; };
template<bool C, class T> struct ____INTELLISENE_enableif<false,T> {};

#define ____INTELLISENE_MUL8_1  8
#define ____INTELLISENE_MUL8_2  16
#define ____INTELLISENE_MUL8_4  32
#define ____INTELLISENE_MUL8_8  64
#define ____INTELLISENE_MUL8_12 96
#define ____INTELLISENE_MUL8_16 128
#define ____INTELLISENE_MUL82(x) ____INTELLISENE_MUL8_##x
#define ____INTELLISENE_MUL8(x)  ____INTELLISENE_MUL82(x)

#define ____INTELLISENE_TYPE_1   __int8
#define ____INTELLISENE_TYPE_2   __int16
#define ____INTELLISENE_TYPE_4   __int32
#define ____INTELLISENE_TYPE_8   __int64
#define ____INTELLISENE_TYPE2(s) ____INTELLISENE_TYPE_##s
#define ____INTELLISENE_TYPE(s)  ____INTELLISENE_TYPE2(s)

#define ____INTELLISENE_MIN_S1  (-127i8-1i8)
#define ____INTELLISENE_MAX_S1    127i8
#define ____INTELLISENE_MIN_U1    0ui8
#define ____INTELLISENE_MAX_U1    0xffui8
#define ____INTELLISENE_MIN_S2  (-32767i16-1i16)
#define ____INTELLISENE_MAX_S2    32767i16
#define ____INTELLISENE_MIN_U2    0ui16
#define ____INTELLISENE_MAX_U2    0xffffui16
#define ____INTELLISENE_MIN_S4  (-2147483647i32-1i32)
#define ____INTELLISENE_MAX_S4    2147483647i32
#define ____INTELLISENE_MIN_U4    0ui32
#define ____INTELLISENE_MAX_U4    0xffffffffui32
#define ____INTELLISENE_MIN_S8  (-9223372036854775807i64-1i64)
#define ____INTELLISENE_MAX_S8    9223372036854775807i64
#define ____INTELLISENE_MIN_U8    0ui64
#define ____INTELLISENE_MAX_U8    0xffffffffffffffffui64

#define ____INTELLISENE_MIN2_S(s) ____INTELLISENE_MIN_S##s
#define ____INTELLISENE_MAX2_S(s) ____INTELLISENE_MAX_S##s
#define ____INTELLISENE_MIN2_U(s) ____INTELLISENE_MIN_U##s
#define ____INTELLISENE_MAX2_U(s) ____INTELLISENE_MAX_U##s
#define ____INTELLISENE_MIN_S(s)  ____INTELLISENE_MIN2_S(s)
#define ____INTELLISENE_MAX_S(s)  ____INTELLISENE_MAX2_S(s)
#define ____INTELLISENE_MIN_U(s)  ____INTELLISENE_MIN2_U(s)
#define ____INTELLISENE_MAX_U(s)  ____INTELLISENE_MAX2_U(s)


#define __PE__              1
#define __STDC__            1
#define __STDC_VERSION__    199901L
#define __STDC_NO_COMPLEX__ 1
//#define __STRICT_ANSI__   1
#define __pic__             1
#define __PIC__             1
#define __pie__             1
#define __PIE__             1

#ifdef _M_IX86
#ifdef _WIN64
#define __x86_64__ 1
#else
#define __i386__   1
#define __i386     1
#define i386       1
#if _M_IX86 >= 400
#define __i486__   1
#define __i486     1
#define i486       1
#endif
#if _M_IX86 >= 500
#define __i586__   1
#define __i586     1
#define i586       1
#endif
#if _M_IX86 >= 600
#define __i686__   1
#define __i686     1
#define i686       1
#endif
#endif
#endif


#define __DCC_VERSION__     1
#define __WINDOWS__         1

#define __BYTE_ORDER__          1234
#define __FLOAT_WORD_ORDER__    1234
#define __ORDER_LITTLE_ENDIAN__ 1234
#define __ORDER_BIG_ENDIAN__    4321
#define __ORDER_PDP_ENDIAN__    3412

#define __SIZEOF_INT__            4
#ifdef _WIN32
#   define __SIZEOF_LONG__        4
#else
#   define __SIZEOF_LONG__        __SIZEOF_POINTER__
#endif
#define __SIZEOF_LONG_LONG__      8
#define __SIZEOF_SHORT__          2
#ifdef _WIN64
#define __SIZEOF_POINTER__        8
#else
#define __SIZEOF_POINTER__        4
#endif
#define __SIZEOF_FLOAT__          4
#define __SIZEOF_DOUBLE__         8
#ifdef __x86_64__
#define __SIZEOF_LONG_DOUBLE__    16
#elif defined(__i386__)
#define __SIZEOF_LONG_DOUBLE__    12
#endif
#define __SIZEOF_SIZE_T__         __SIZEOF_POINTER__
#ifdef __WINDOWS__
#define __SIZEOF_WCHAR_T__        2
#else
#define __SIZEOF_WCHAR_T__        4
#endif
#define __SIZEOF_WINT_T__         __SIZEOF_INT__
#define __SIZEOF_PTRDIFF_T__      __SIZEOF_POINTER__
#define __SIZEOF_CHAR__           1
#define __SIZEOF_INT_LEAST8_T__   1
#define __SIZEOF_INT_LEAST16_T__  2
#define __SIZEOF_INT_LEAST32_T__  4
#define __SIZEOF_INT_LEAST64_T__  8
#define __SIZEOF_INT_FAST8_T__    1
#if defined(__i386__) || defined(__x86_64__)
#define __SIZEOF_INT_FAST16_T__   4
#else
#define __SIZEOF_INT_FAST16_T__   2
#endif
#define __SIZEOF_INT_FAST32_T__   4
#define __SIZEOF_INT_FAST64_T__   8
#define __SIZEOF_SIG_ATOMIC_T__   __SIZEOF_INT__
#define __SIZEOF_INTMAX_T__       8

#define __SCHAR_WIDTH__           ____INTELLISENE_MUL8(__SIZEOF_CHAR__)
#define __SHRT_WIDTH__            ____INTELLISENE_MUL8(__SIZEOF_SHORT__)
#define __INT_WIDTH__             ____INTELLISENE_MUL8(__SIZEOF_INT__)
#define __LONG_WIDTH__            ____INTELLISENE_MUL8(__SIZEOF_LONG__)
#define __LONG_LONG_WIDTH__       ____INTELLISENE_MUL8(__SIZEOF_LONG_LONG__)
#define __PTRDIFF_WIDTH__         ____INTELLISENE_MUL8(__SIZEOF_PTRDIFF_T__)
#define __SIG_ATOMIC_WIDTH__      ____INTELLISENE_MUL8(__SIZEOF_SIG_ATOMIC_T__)
#define __SIZE_WIDTH__            ____INTELLISENE_MUL8(__SIZEOF_SIZE_T__)
#define __WCHAR_WIDTH__           ____INTELLISENE_MUL8(__SIZEOF_WCHAR_T__)
#define __WINT_WIDTH__            ____INTELLISENE_MUL8(__SIZEOF_WINT_T__)
#define __INT_LEAST8_WIDTH__      ____INTELLISENE_MUL8(__SIZEOF_INT_LEAST8_T__)
#define __INT_LEAST16_WIDTH__     ____INTELLISENE_MUL8(__SIZEOF_INT_LEAST16_T__)
#define __INT_LEAST32_WIDTH__     ____INTELLISENE_MUL8(__SIZEOF_INT_LEAST32_T__)
#define __INT_LEAST64_WIDTH__     ____INTELLISENE_MUL8(__SIZEOF_INT_LEAST64_T__)
#define __INT_FAST8_WIDTH__       ____INTELLISENE_MUL8(__SIZEOF_INT_FAST8_T__)
#define __INT_FAST16_WIDTH__      ____INTELLISENE_MUL8(__SIZEOF_INT_FAST16_T__)
#define __INT_FAST32_WIDTH__      ____INTELLISENE_MUL8(__SIZEOF_INT_FAST32_T__)
#define __INT_FAST64_WIDTH__      ____INTELLISENE_MUL8(__SIZEOF_INT_FAST64_T__)
#define __INTPTR_WIDTH__          ____INTELLISENE_MUL8(__SIZEOF_POINTER__)
#define __INTMAX_WIDTH__          ____INTELLISENE_MUL8(__SIZEOF_INTMAX_T__)


#define __INT8_TYPE__   signed __int8
#define __INT16_TYPE__  signed __int16
#define __INT32_TYPE__  signed __int32
#define __INT64_TYPE__  signed __int64
#define __UINT8_TYPE__  unsigned __int8
#define __UINT16_TYPE__ unsigned __int16
#define __UINT32_TYPE__ unsigned __int32
#define __UINT64_TYPE__ unsigned __int64

#define __SIZE_TYPE__      unsigned ____INTELLISENE_TYPE(__SIZEOF_SIZE_T__)
#define __PTRDIFF_TYPE__     signed ____INTELLISENE_TYPE(__SIZEOF_PTRDIFF_T__)
#define __WCHAR_TYPE__       wchar_t
#define __WINT_TYPE__        signed ____INTELLISENE_TYPE(__SIZEOF_WINT_T__)
#define __INTMAX_TYPE__      signed ____INTELLISENE_TYPE(__SIZEOF_INTMAX_T__)
#define __UINTMAX_TYPE__   unsigned ____INTELLISENE_TYPE(__SIZEOF_INTMAX_T__)
#define __SIG_ATOMIC_TYPE__  signed ____INTELLISENE_TYPE(__SIZEOF_SIG_ATOMIC_T__)
#define __SSIZE_TYPE__       signed ____INTELLISENE_TYPE(__SIZEOF_SIZE_T__)

#define __INT_LEAST8_TYPE__     signed ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST8_T__)
#define __INT_LEAST16_TYPE__    signed ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST16_T__)
#define __INT_LEAST32_TYPE__    signed ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST32_T__)
#define __INT_LEAST64_TYPE__    signed ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST64_T__)
#define __UINT_LEAST8_TYPE__  unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST8_T__)
#define __UINT_LEAST16_TYPE__ unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST16_T__)
#define __UINT_LEAST32_TYPE__ unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST32_T__)
#define __UINT_LEAST64_TYPE__ unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_LEAST64_T__)
#define __INT_FAST8_TYPE__      signed ____INTELLISENE_TYPE(__SIZEOF_INT_FAST8_T__)
#define __INT_FAST16_TYPE__     signed ____INTELLISENE_TYPE(__SIZEOF_INT_FAST16_T__)
#define __INT_FAST32_TYPE__     signed ____INTELLISENE_TYPE(__SIZEOF_INT_FAST32_T__)
#define __INT_FAST64_TYPE__     signed ____INTELLISENE_TYPE(__SIZEOF_INT_FAST64_T__)
#define __UINT_FAST8_TYPE__   unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_FAST8_T__)
#define __UINT_FAST16_TYPE__  unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_FAST16_T__)
#define __UINT_FAST32_TYPE__  unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_FAST32_T__)
#define __UINT_FAST64_TYPE__  unsigned ____INTELLISENE_TYPE(__SIZEOF_INT_FAST64_T__)

#define __INTPTR_TYPE__    signed ____INTELLISENE_TYPE(__SIZEOF_POINTER__)
#define __UINTPTR_TYPE__ unsigned ____INTELLISENE_TYPE(__SIZEOF_POINTER__)

#define __CHAR_BIT__              ____INTELLISENE_MUL8(__SIZEOF_CHAR__)

#ifdef __CHAR_UNSIGNED__
#define __CHAR_MIN__        ____INTELLISENE_MIN_U(__SIZEOF_CHAR__)
#define __CHAR_MAX__        ____INTELLISENE_MAX_U(__SIZEOF_CHAR__)
#else
#define __CHAR_MIN__        ____INTELLISENE_MIN_S(__SIZEOF_CHAR__)
#define __CHAR_MAX__        ____INTELLISENE_MAX_S(__SIZEOF_CHAR__)
#endif
#define __SCHAR_MIN__       ____INTELLISENE_MIN_S(__SIZEOF_CHAR__)
#define __SCHAR_MAX__       ____INTELLISENE_MAX_S(__SIZEOF_CHAR__)
#define __UCHAR_MAX__       ____INTELLISENE_MAX_U(__SIZEOF_CHAR__)
#define __WCHAR_MIN__       0
#define __WCHAR_MAX__       ____INTELLISENE_MAX_U(__SIZEOF_WCHAR_T__
#define __SHRT_MIN__        ____INTELLISENE_MIN_S(__SIZEOF_SHORT__)
#define __SHRT_MAX__        ____INTELLISENE_MAX_S(__SIZEOF_SHORT__)
#define __USHRT_MAX__       ____INTELLISENE_MAX_U(__SIZEOF_SHORT__)
#define __INT_MIN__         ____INTELLISENE_MIN_S(__SIZEOF_INT__)
#define __INT_MAX__         ____INTELLISENE_MAX_S(__SIZEOF_INT__)
#define __UINT_MAX__        ____INTELLISENE_MAX_U(__SIZEOF_INT__)
#define __LONG_MIN__        ____INTELLISENE_MIN_S(__SIZEOF_LONG__)
#define __LONG_MAX__        ____INTELLISENE_MAX_S(__SIZEOF_LONG__)
#define __ULONG_MAX__       ____INTELLISENE_MAX_U(__SIZEOF_LONG__)
#define __LONG_LONG_MIN__   ____INTELLISENE_MIN_S(__SIZEOF_LONG_LONG__)
#define __LONG_LONG_MAX__   ____INTELLISENE_MAX_S(__SIZEOF_LONG_LONG__)
#define __ULONG_LONG_MAX__  ____INTELLISENE_MAX_U(__SIZEOF_LONG_LONG__)
#define __WINT_MIN__        ____INTELLISENE_MIN_S(__SIZEOF_WINT_T__)
#define __WINT_MAX__        ____INTELLISENE_MAX_S(__SIZEOF_WINT_T__)
#define __SIZE_MAX__        ____INTELLISENE_MAX_U(__SIZEOF_SIZE_T__)
#define __SSIZE_MIN__       ____INTELLISENE_MIN_S(__SIZEOF_SIZE_T__)
#define __SSIZE_MAX__       ____INTELLISENE_MAX_S(__SIZEOF_SIZE_T__)
#define __PTRDIFF_MIN__     ____INTELLISENE_MIN_S(__SIZEOF_PTRDIFF_T__)
#define __PTRDIFF_MAX__     ____INTELLISENE_MAX_S(__SIZEOF_PTRDIFF_T__)
#define __INTMAX_MIN__      ____INTELLISENE_MIN_S(__SIZEOF_INTMAX_T__)
#define __INTMAX_MAX__      ____INTELLISENE_MAX_S(__SIZEOF_INTMAX_T__)
#define __UINTMAX_MAX__     ____INTELLISENE_MAX_U(__SIZEOF_INTMAX_T__)
#define __SIG_ATOMIC_MIN__  ____INTELLISENE_MIN_S(__SIZEOF_SIG_ATOMIC_T__)
#define __SIG_ATOMIC_MAX__  ____INTELLISENE_MAX_S(__SIZEOF_SIG_ATOMIC_T__)

#define __INT8_MIN__     ____INTELLISENE_MIN_S1
#define __INT16_MIN__    ____INTELLISENE_MIN_S2
#define __INT32_MIN__    ____INTELLISENE_MIN_S4
#define __INT64_MIN__    ____INTELLISENE_MIN_S4
#define __INT8_MAX__     ____INTELLISENE_MAX_S1
#define __INT16_MAX__    ____INTELLISENE_MAX_S2
#define __INT32_MAX__    ____INTELLISENE_MAX_S4
#define __INT64_MAX__    ____INTELLISENE_MAX_S4
#define __UINT8_MAX__    ____INTELLISENE_MAX_U1
#define __UINT16_MAX__   ____INTELLISENE_MAX_U2
#define __UINT32_MAX__   ____INTELLISENE_MAX_U4
#define __UINT64_MAX__   ____INTELLISENE_MAX_U4

#define __INT_LEAST8_MIN__    ____INTELLISENE_MIN_S(__SIZEOF_INT_LEAST8_T__)
#define __INT_LEAST16_MIN__   ____INTELLISENE_MIN_S(__SIZEOF_INT_LEAST16_T__)
#define __INT_LEAST32_MIN__   ____INTELLISENE_MIN_S(__SIZEOF_INT_LEAST32_T__)
#define __INT_LEAST64_MIN__   ____INTELLISENE_MIN_S(__SIZEOF_INT_LEAST64_T__)
#define __INT_LEAST8_MAX__    ____INTELLISENE_MAX_S(__SIZEOF_INT_LEAST8_T__)
#define __INT_LEAST16_MAX__   ____INTELLISENE_MAX_S(__SIZEOF_INT_LEAST16_T__)
#define __INT_LEAST32_MAX__   ____INTELLISENE_MAX_S(__SIZEOF_INT_LEAST32_T__)
#define __INT_LEAST64_MAX__   ____INTELLISENE_MAX_S(__SIZEOF_INT_LEAST64_T__)
#define __UINT_LEAST8_MAX__   ____INTELLISENE_MAX_U(__SIZEOF_INT_LEAST8_T__)
#define __UINT_LEAST16_MAX__  ____INTELLISENE_MAX_U(__SIZEOF_INT_LEAST16_T__)
#define __UINT_LEAST32_MAX__  ____INTELLISENE_MAX_U(__SIZEOF_INT_LEAST32_T__)
#define __UINT_LEAST64_MAX__  ____INTELLISENE_MAX_U(__SIZEOF_INT_LEAST64_T__)
#define __INT_FAST8_MIN__     ____INTELLISENE_MIN_S(__SIZEOF_INT_FAST8_T__)
#define __INT_FAST16_MIN__    ____INTELLISENE_MIN_S(__SIZEOF_INT_FAST16_T__)
#define __INT_FAST32_MIN__    ____INTELLISENE_MIN_S(__SIZEOF_INT_FAST32_T__)
#define __INT_FAST64_MIN__    ____INTELLISENE_MIN_S(__SIZEOF_INT_FAST64_T__)
#define __INT_FAST8_MAX__     ____INTELLISENE_MAX_S(__SIZEOF_INT_FAST8_T__)
#define __INT_FAST16_MAX__    ____INTELLISENE_MAX_S(__SIZEOF_INT_FAST16_T__)
#define __INT_FAST32_MAX__    ____INTELLISENE_MAX_S(__SIZEOF_INT_FAST32_T__)
#define __INT_FAST64_MAX__    ____INTELLISENE_MAX_S(__SIZEOF_INT_FAST64_T__)
#define __UINT_FAST8_MAX__    ____INTELLISENE_MAX_U(__SIZEOF_INT_FAST8_T__)
#define __UINT_FAST16_MAX__   ____INTELLISENE_MAX_U(__SIZEOF_INT_FAST16_T__)
#define __UINT_FAST32_MAX__   ____INTELLISENE_MAX_U(__SIZEOF_INT_FAST32_T__)
#define __UINT_FAST64_MAX__   ____INTELLISENE_MAX_U(__SIZEOF_INT_FAST64_T__)

#define __INTPTR_MIN__  ____INTELLISENE_MIN_S(__SIZEOF_POINTER__)
#define __INTPTR_MAX__  ____INTELLISENE_MAX_S(__SIZEOF_POINTER__)
#define __UINTPTR_MAX__ ____INTELLISENE_MAX_U(__SIZEOF_POINTER__)

//#define __CHAR_UNSIGNED__ 1
#define __WCHAR_UNSIGNED__  1

#if 1 /* Better highlighting for illegal usage cases. */
#define __NULL__     nullptr
#else
#define __NULL__     0
#endif


#if __SIZEOF_POINTER__ == 8 && \
    __SIZEOF_LONG__ == 8
#define __LP64__ 1
#define _LP64    1
#endif

#define __INT8_C(x)    x##i8
#define __INT16_C(x)   x##i16
#define __INT32_C(x)   x##i32
#define __INT64_C(x)   x##i64
#define __UINT8_C(x)   x##ui8
#define __UINT16_C(x)  x##ui16
#define __UINT32_C(x)  x##ui32
#define __UINT64_C(x)  x##ui64
#define __INTMAX_C(x)  x##i64
#define __UINTMAX_C(x) x##ui64

/* Stub-implement some TPP extensions. */

#define __DATE_DAY__   1
#define __DATE_WDAY__  0
#define __DATE_YDAY__  1
#define __DATE_MONTH__ 1
#define __DATE_YEAR__  2017
#define __TIME_HOUR__  19
#define __TIME_MIN__   02
#define __TIME_SEC__   13

#define __TPP_COUNTER(x)   __COUNTER__
#define __COLUMN__         0
#define __INCLUDE_LEVEL__  0
#define __INCLUDE_DEPTH__  0

#ifndef __is_identifier
#define __is_identifier(x)          0
#endif
#ifndef __is_builtin_identifier
#define __is_builtin_identifier(x)  0
#endif
#ifndef __is_deprecated
#define __is_deprecated(x)          0
#endif
#ifndef __has_attribute
#define __has_attribute(x)          0
#endif
#ifndef __has_builtin
#define __has_builtin(x)            0
#endif
#ifndef __has_tpp_builtin
#define __has_tpp_builtin(x)        0
#endif
#ifndef __has_cpp_attribute
#define __has_cpp_attribute(x)      0
#endif
#ifndef __has_declspec_attribute
#define __has_declspec_attribute(x) 0
#endif
#ifndef __has_feature
#define __has_feature(x)            0
#endif
#ifndef __has_extension
#define __has_extension(x)          0
#endif
#ifndef __has_warning
#define __has_warning(x)            0
#endif
#ifndef __has_known_extension
#define __has_known_extension(x)    0
#endif
#ifndef __has_known_warning
#define __has_known_warning(x)      0
#endif
#ifndef __has_include
#define __has_include(x)            0
#endif
#ifndef __has_include_next
#define __has_include_next(x)       0
#endif
#ifndef __is_deprecated
#define __is_deprecated(x)          0
#endif

/* Builtin keywords */

typedef bool _Bool;

#define __attribute(x)          /* nothing */
#define __attribute__(x)        /* nothing */
#define __asm(x)                /* nothing */
#define __asm__(x)              /* nothing */
#define __signed                signed
#define __signed__              signed
#define __unsigned              unsigned
#define __unsigned__            unsigned
#define __inline__              __inline
#define _Alignof                __alignof
#define __alignof__             __alignof
#define __label__               /* nothing */
#define __extension__           /* nothing */
#define _Noreturn               __declspec(noreturn)
#define _Atomic                 /* nothing */
#define _Thread_local           __declspec(thread)
#define __thread                __declspec(thread)
#define _Static_assert          static_assert
#define __auto_type             auto

#define typeof(...)     decltype(::____INTELLISENE_typeof_helper(((__VA_ARGS__),::____INTELLISENE_void_helper())))
#define __typeof(...)   decltype(::____INTELLISENE_typeof_helper(((__VA_ARGS__),::____INTELLISENE_void_helper())))
#define __typeof__(...) decltype(::____INTELLISENE_typeof_helper(((__VA_ARGS__),::____INTELLISENE_void_helper())))
#define __builtin_types_compatible_p(...) ____INTELLISENE_sametype< __VA_ARGS__ >::__val

#define __builtin_offsetof(s,m) (__SIZE_TYPE__)(&((s*)0)->m)
#define __builtin_bitfield(x,bit_index,bit_size) ((void)(bit_index),(void)(bit_size),(x))
#define __builtin_constant_p(x) ((void)(x),0)

#define __builtin_choose_expr(c,tt,ff) \
     __if_exists(::____INTELLISENE_static_if_helper<((c))>::__is_true__){tt} \
 __if_not_exists(::____INTELLISENE_static_if_helper<((c))>::__is_true__){ff}




void *__builtin_alloca(__SIZE_TYPE__ size);
void *__builtin_alloca_with_align(__SIZE_TYPE__ size, __SIZE_TYPE__ align);
void *__builtin_assume_aligned(void *ptr, __SIZE_TYPE__ alignment);

__declspec(noreturn) void __builtin_unreachable(void);
__declspec(noreturn) void __builtin_trap(void);
void __builtin_breakpoint(void);
void __builtin_sfence(void);
void __builtin_lfence(void);
void __builtin_mfence(void);


#define __builtin_typestr(T) "int"

void __builtin_assume(bool cond);
long __builtin_expect(long x, long expect);
char const *__builtin_FILE(void);
int __builtin_LINE(void);
char const *__builtin_FUNCTION(void);


typedef char *__builtin_va_list,*__gnuc_va_list;
void __builtin_va_start(__builtin_va_list &ap, ...);
template<class T> void __builtin_va_start(__builtin_va_list &ap, T &before_start, ...);
void __builtin_va_end(__builtin_va_list &ap, ...);
void __builtin_va_copy(__builtin_va_list &dst_ap, __builtin_va_list &src_ap, ...);
template<class T> T ____INTELLISENE_va_arg_helper(__builtin_va_list &);
#define __builtin_va_arg(ap,T,...)  ____INTELLISENE_va_arg_helper< T >(ap)


#if __SIZEOF_POINTER__ == 4
#   define __SIZEOF_JMP_BUF__ 32
#elif __SIZEOF_POINTER__ == 8
#   define __SIZEOF_JMP_BUF__ 64
#endif

template<class T> typename ____INTELLISENE_enableif<sizeof(T) == __SIZEOF_JMP_BUF__,int>::__type __builtin_setjmp(T &jbuf);
template<class T> __declspec(noreturn) typename ____INTELLISENE_enableif<sizeof(T) == __SIZEOF_JMP_BUF__,void>::__type __builtin_longjmp(T &jbuf, int sig);

__UINT16_TYPE__ __builtin_bswap16(__UINT16_TYPE__ x);
__UINT32_TYPE__ __builtin_bswap32(__UINT32_TYPE__ x);
__UINT64_TYPE__ __builtin_bswap64(__UINT64_TYPE__ x);
template<class T> T __builtin_bswapcc(T x, __SIZE_TYPE__ n_bytes);


int __builtin_ffs(int x);
int __builtin_ffsl(long x);
int __builtin_ffsll(long long x);
template<class T> int __builtin_ffscc(T x, __SIZE_TYPE__ n_bytes);

int __builtin_clz(int x);
int __builtin_clzl(long x);
int __builtin_clzll(long long x);
template<class T> int __builtin_clzcc(T x, __SIZE_TYPE__ n_bytes);

template<class T> T __builtin_min(T first, ...); int __builtin_min(void); 
template<class T> T __builtin_max(T first, ...); int __builtin_max(void); 

void *__builtin_memcpy(void *dst, void const *src, __SIZE_TYPE__ n_bytes);
void *__builtin_memmove(void *dst, void const *src, __SIZE_TYPE__ n_bytes);
void *__builtin_memset(void *dst, int byte, __SIZE_TYPE__ n_bytes);
int __builtin_memcmp(void const *a, void const *b, __SIZE_TYPE__ n_bytes);
__SIZE_TYPE__ __builtin_strlen(char const *str);
__SIZE_TYPE__ __builtin_strnlen(char const *str, __SIZE_TYPE__ max_chars);

void *__builtin_malloc(__SIZE_TYPE__ n_bytes);
void *__builtin_calloc(__SIZE_TYPE__ count, __SIZE_TYPE__ n_bytes);
void *__builtin_realloc(void *ptr, __SIZE_TYPE__ n_bytes);
void  __builtin_free(void *ptr);

void *__builtin_memchr(void const *p, int c, __SIZE_TYPE__ s);
__SIZE_TYPE__ __builtin_memlen(void const *p, int c, __SIZE_TYPE__ s);
void *__builtin_memend(void const *p, int c, __SIZE_TYPE__ s);
void *__builtin_memrchr(void const *p, int c, __SIZE_TYPE__ s);
__SIZE_TYPE__ __builtin_memrlen(void const *p, int c, __SIZE_TYPE__ s);
void *__builtin_memrend(void const *p, int c, __SIZE_TYPE__ s);
void *__builtin_rawmemchr(void const *p, int c);
__SIZE_TYPE__ __builtin_rawmemlen(void const *p, int c);
void *__builtin_rawmemrchr(void const *p, int c);
__SIZE_TYPE__ __builtin_rawmemrlen(void const *p, int c);
__SIZE_TYPE__ __builtin_stroff(char const *s, int c);
__SIZE_TYPE__ __builtin_strroff(char const *s, int c);
char *__builtin_strchr(char const *s, int c);
char *__builtin_strrchr(char const *s, int c);
char *__builtin_strchrnul(char const *s, int c);
char *__builtin_strrchrnul(char const *s, int c);
__SIZE_TYPE__ __builtin_strnoff(char const *s, int c, __SIZE_TYPE__ max);
__SIZE_TYPE__ __builtin_strnroff(char const *s, int c, __SIZE_TYPE__ max);
char *__builtin_strnchr(char const *s, int c, __SIZE_TYPE__ max);
char *__builtin_strnrchr(char const *s, int c, __SIZE_TYPE__ max);
char *__builtin_strnchrnul(char const *s, int c, __SIZE_TYPE__ max);
char *__builtin_strnrchrnul(char const *s, int c, __SIZE_TYPE__ max);

int __builtin_iscntrl(int ch);
int __builtin_isblank(int ch);
int __builtin_isspace(int ch);
int __builtin_isupper(int ch);
int __builtin_islower(int ch);
int __builtin_isalpha(int ch);
int __builtin_isdigit(int ch);
int __builtin_isxdigit(int ch);
int __builtin_isalnum(int ch);
int __builtin_ispunct(int ch);
int __builtin_isgraph(int ch);
int __builtin_isprint(int ch);
int __builtin_tolower(int ch);
int __builtin_toupper(int ch);

void *__builtin_return_address(__PTRDIFF_TYPE__ level);
void *__builtin_frame_address(__PTRDIFF_TYPE__ level);
void *__builtin_extract_return_addr(void *mangled_frame_addr);
void *__builtin_frob_return_address(void *unmangled_frame_addr);
int   __builtin_noop(...);

void   __builtin_cpu_init(void);
_Bool  __builtin_cpu_is(char const *name);
int    __builtin_cpu_supports(char const *feature);
#if defined(__i386__) || defined(__x86_64__)
char (&__builtin_cpu_vendor(void))[13];
char (&__builtin_cpu_brand(void))[49];
char (&__builtin_cpu_vendor(char buf[13]))[13];
char (&__builtin_cpu_brand(char buf[49]))[49];
#else
#error FIXME
#endif

template<class type> void __sync_inc(type volatile *ptr, type value, ...);
template<class type> void __sync_dec(type volatile *ptr, type value, ...);
template<class type> void __sync_neg(type volatile *ptr, type value, ...);
template<class type> void __sync_not(type volatile *ptr, type value, ...);
template<class type> void __sync_add(type volatile *ptr, type value, ...);
template<class type> void __sync_sub(type volatile *ptr, type value, ...);
template<class type> void __sync_or(type volatile *ptr, type value, ...);
template<class type> void __sync_and(type volatile *ptr, type value, ...);
template<class type> void __sync_xor(type volatile *ptr, type value, ...);
template<class type> void __sync_nand(type volatile *ptr, type value, ...);
template<class type> void __sync_store(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_inc(type volatile *ptr, ...);
template<class type> type __sync_fetch_and_dec(type volatile *ptr, ...);
template<class type> type __sync_fetch_and_neg(type volatile *ptr, ...);
template<class type> type __sync_fetch_and_not(type volatile *ptr, ...);
template<class type> type __sync_fetch_and_add(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_sub(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_or(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_and(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_xor(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_nand(type volatile *ptr, type value, ...);
template<class type> type __sync_fetch_and_store(type volatile *ptr, type value, ...);
template<class type> type __sync_inc_and_fetch(type volatile *ptr, ...);
template<class type> type __sync_dec_and_fetch(type volatile *ptr, ...);
template<class type> type __sync_neg_and_fetch(type volatile *ptr, ...);
template<class type> type __sync_not_and_fetch(type volatile *ptr, ...);
template<class type> type __sync_add_and_fetch(type volatile *ptr, type value, ...);
template<class type> type __sync_sub_and_fetch(type volatile *ptr, type value, ...);
template<class type> type __sync_or_and_fetch(type volatile *ptr, type value, ...);
template<class type> type __sync_and_and_fetch(type volatile *ptr, type value, ...);
template<class type> type __sync_xor_and_fetch(type volatile *ptr, type value, ...);
template<class type> type __sync_nand_and_fetch(type volatile *ptr, type value, ...);
template<class type> type __sync_store_and_fetch(type volatile *ptr, type value, ...);
template<class type> bool __sync_bool_compare_and_swap(type volatile *ptr, type oldval, type newval, ...);
template<class type> type __sync_val_compare_and_swap(type volatile *ptr, type oldval, type newval, ...);
void __sync_synchronize(...);
template<class type> type __sync_lock_test_and_set(type *ptr, type value, ...);
template<class type> void __sync_lock_release(type *ptr, ...);



