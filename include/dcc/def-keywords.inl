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

DEF_M(__INT8_C)
DEF_M(__INT16_C)
DEF_M(__INT32_C)
DEF_M(__INT64_C)
DEF_M(__UINT8_C)
DEF_M(__UINT16_C)
DEF_M(__UINT32_C)
DEF_M(__UINT64_C)
DEF_M(__INTMAX_C)
DEF_M(__UINTMAX_C)

/* Additional pragmas. */
DEF_K(comment) DEF_K(lib) /* #pragma comment(lib,"xxx") */
DEF_K(pack)               /* #pragma pack(...) */

#define DEF_BUILTIN(name) \
 KWD(KWD_##name,#name) \
 KWD_FLAGS(KWD_##name,TPP_KEYWORDFLAG_HAS_BUILTIN)

/* C statement keywords. */
DEF_K(while) DEF_K(do) DEF_K(for)
DEF_K(break) DEF_K(continue)
DEF_K(goto) DEF_K(return)
DEF_K(switch) DEF_K(case) DEF_K(_Generic)

/* Used-defined type keywords. */
DEF_K(struct) DEF_K(union) DEF_K(enum)

/* Builtin type keywords. */
DEF_K(char) DEF_K(__auto_type) DEF_K(auto)
DEF_K(short) DEF_K(long) DEF_K(_Bool)
DEF_K(unsigned) DEF_K(__unsigned) DEF_K(__unsigned__)
DEF_K(signed)   DEF_K(__signed)   DEF_K(__signed__)
DEF_K(__int8) DEF_K(__int16) DEF_K(__int32) DEF_K(__int64)
DEF_K(void) DEF_K(float) DEF_K(double)

/* Type storage keywords. */
DEF_K(register) DEF_K(static)
DEF_K(typedef) DEF_K(extern)
DEF_K(inline) DEF_K(__inline) DEF_K(__inline__)

/* Type flag keywords. */
DEF_K(_Atomic)

/* Type qualifier keywords. */
DEF_K(const)    DEF_K(__const)    DEF_K(__const__)
DEF_K(volatile) DEF_K(__volatile) DEF_K(__volatile__)
DEF_K(restrict) DEF_K(__restrict) DEF_K(__restrict__)

/* Extension statement keywords. */
DEF_K(asm) DEF_K(__asm) DEF_K(__asm__)
DEF_K(__label__)

/* Extension expression keywords. */
DEF_K(__pack) DEF_K(sizeof)
DEF_K(_Alignof) DEF_K(__alignof) DEF_K(__alignof__) DEF_BUILTIN(__builtin_alignof)
DEF_K(typeof) DEF_K(__typeof) DEF_K(__typeof__)
DEF_K(attribute) DEF_K(__attribute) DEF_K(__attribute__)
DEF_K(_declspec) DEF_K(__declspec)

/* Special keywords. */
DEF_K(__func__)
DEF_M(__FUNCTION__)
DEF_M(__PRETTY_FUNCTION__)

#define DEF_TPP_BUILTIN(name) KWD(KWD_##name,#name) KWD_FLAGS(KWD_##name,TPP_KEYWORDFLAG_HAS_BUILTIN|TPP_KEYWORDFLAG_HAS_TPP_BUILTIN)
#define DEF_BUILTIN(name)     KWD(KWD_##name,#name) KWD_FLAGS(KWD_##name,TPP_KEYWORDFLAG_HAS_BUILTIN)

/* Check if a given expression can be evaluated at compile-time.
 * NOTE: Also available in TPP expressions. */
DEF_TPP_BUILTIN(__builtin_constant_p)

/* Select an expression based on a constant value, skipping evaluation of the other.
 * NOTE: Also available in TPP expressions. */
DEF_TPP_BUILTIN(__builtin_choose_expr)

/* Allocate dynamic memory on the stack. */
DEF_BUILTIN(__builtin_alloca)
DEF_BUILTIN(__builtin_alloca_with_align)
DEF_BUILTIN(__builtin_assume_aligned)

/* Mark code as unreachable/trap the CPU when it is reached. */
DEF_BUILTIN(__builtin_unreachable)
DEF_BUILTIN(__builtin_trap)
DEF_BUILTIN(__builtin_breakpoint)
DEF_BUILTIN(__builtin_sfence)
DEF_BUILTIN(__builtin_lfence)
DEF_BUILTIN(__builtin_mfence)

/* Check two types for compatibility. */
DEF_BUILTIN(__builtin_types_compatible_p)

/* Used in compiler tests: return a human-readable string
 * representation, accepting the same arguments as 'sizeof' */
DEF_BUILTIN(__builtin_typestr)

/* Define a compile-time assumption (Currently only used
 * for specifying unreachable code with '__assume(0)'). */
DEF_BUILTIN(__builtin_assume) DEF_K(__assume)

/* Hint the compiler to expect a certain outcome. */
DEF_BUILTIN(__builtin_expect)

/* Same as '__FILE__', '__LINE__' and '__func__' */
DEF_BUILTIN(__builtin_FILE)
DEF_BUILTIN(__builtin_LINE)
DEF_BUILTIN(__builtin_FUNCTION)

/* Generate a bitfield-style expression. */
DEF_BUILTIN(__builtin_bitfield)

/* Builtin function for offsetof */
DEF_BUILTIN(__builtin_offsetof)

DEF_K(__builtin_va_list) MACRO(KWD___builtin_va_list,1)
KWD_FLAGS(KWD___builtin_va_list,TPP_KEYWORDFLAG_HAS_BUILTIN)
BUILTIN_MACRO(KWD___builtin_va_list,"char*")
DEF_BUILTIN(__builtin_va_start)
DEF_BUILTIN(__builtin_va_end)
DEF_BUILTIN(__builtin_va_copy)
DEF_BUILTIN(__builtin_va_arg)

/* Compiler intrinsics for setjmp()/longjmp() */
DEF_BUILTIN(__builtin_setjmp)
DEF_BUILTIN(__builtin_longjmp)
/* This macro is a DCC extension that expands to the size of the jump-buffer
 * required by '__builtin_setjmp'/'__builtin_longjmp' in bytes.
 * Using this macro, one can easily declare a jmp_buf-type as:
 * >> typedef struct {
 * >>    __int8 buf[__SIZEOF_JMP_BUF__];
 * >> } jmp_buf[1];
 */
PREDEFINED_MACRO(__SIZEOF_JMP_BUF__,DCC_PP_STR(DCC_TARGET_SIZEOF_JMP_BUF))

/* Swap byte order. */
DEF_BUILTIN(__builtin_bswap16)
DEF_BUILTIN(__builtin_bswap32)
DEF_BUILTIN(__builtin_bswap64)
/* Optional secondary argument with explicit compile-time size in bytes.
 * If omitted, size is deduced from first argument. */
DEF_BUILTIN(__builtin_bswapcc)

/* Find the least significant 1-bit, returning
 * (1-based) or ZERO(0) when the argument is ZERO(0). */
DEF_BUILTIN(__builtin_ffs)
DEF_BUILTIN(__builtin_ffsl)
DEF_BUILTIN(__builtin_ffsll)
DEF_BUILTIN(__builtin_ffscc) /* See above */

/* Count the number of leading 0-bits, starting at the most
 * significant index (Undefined behavior when the argument is ZERO(0)). */
DEF_BUILTIN(__builtin_clz)
DEF_BUILTIN(__builtin_clzl)
DEF_BUILTIN(__builtin_clzll)
DEF_BUILTIN(__builtin_clzcc) /* See above */


/* Automatically generates optimized code when
 * the size argument (3rd) is known at compile-time,
 * or when 'src' is known to be equal to 'dst'.  */
DEF_BUILTIN(__builtin_memcpy)
DEF_BUILTIN(__builtin_memmove)
DEF_BUILTIN(__builtin_memset)
DEF_BUILTIN(__builtin_memcmp)
DEF_BUILTIN(__builtin_strlen)

DEF_BUILTIN(__builtin_return_address)
DEF_BUILTIN(__builtin_frame_address)
DEF_BUILTIN(__builtin_extract_return_addr)
DEF_BUILTIN(__builtin_frob_return_address)

// TODO: __builtin_ctz
// TODO: __builtin_popcount
// TODO: __builtin_parity

// TODO: __builtin_apply_args
// TODO: __builtin_apply
// TODO: __builtin_return

DEF_BUILTIN(__sync_bool_compare_and_swap)
DEF_BUILTIN(__sync_val_compare_and_swap)
DEF_BUILTIN(__sync_synchronize)
/* As an extension, DCC allows you to call builtin sync operations that don't return an old value.
 * Note, that such operations may be faster for certain targets! */
DEF_BUILTIN(__sync_add)   DEF_BUILTIN(__sync_fetch_and_add)   DEF_BUILTIN(__sync_add_and_fetch)
DEF_BUILTIN(__sync_sub)   DEF_BUILTIN(__sync_fetch_and_sub)   DEF_BUILTIN(__sync_sub_and_fetch)
DEF_BUILTIN(__sync_or)    DEF_BUILTIN(__sync_fetch_and_or)    DEF_BUILTIN(__sync_or_and_fetch)
DEF_BUILTIN(__sync_and)   DEF_BUILTIN(__sync_fetch_and_and)   DEF_BUILTIN(__sync_and_and_fetch)
DEF_BUILTIN(__sync_xor)   DEF_BUILTIN(__sync_fetch_and_xor)   DEF_BUILTIN(__sync_xor_and_fetch)
DEF_BUILTIN(__sync_nand)  DEF_BUILTIN(__sync_fetch_and_nand)  DEF_BUILTIN(__sync_nand_and_fetch)
/* New addition: fetch-style store operations (offers 'atomic_exchange' operations). */
DEF_BUILTIN(__sync_store) DEF_BUILTIN(__sync_fetch_and_store) DEF_BUILTIN(__sync_store_and_fetch)
/* New addition: unary atomic operations. */
DEF_BUILTIN(__sync_inc)   DEF_BUILTIN(__sync_fetch_and_inc) DEF_BUILTIN(__sync_inc_and_fetch)
DEF_BUILTIN(__sync_dec)   DEF_BUILTIN(__sync_fetch_and_dec) DEF_BUILTIN(__sync_dec_and_fetch)
DEF_BUILTIN(__sync_neg)   DEF_BUILTIN(__sync_fetch_and_neg) DEF_BUILTIN(__sync_neg_and_fetch)
/* NOTE: 'not' is the complement ('~'). */
DEF_BUILTIN(__sync_not)   DEF_BUILTIN(__sync_fetch_and_not) DEF_BUILTIN(__sync_not_and_fetch)
DEF_BUILTIN(__sync_lock_test_and_set)
DEF_BUILTIN(__sync_lock_release)

/* TODO: __atomic_*  (New gcc atomic builtins) */

#undef DEF_BUILTIN
#undef DEF_TPP_BUILTIN


#define DEFINE_ATTRIBUTE(name) \
 KWD(KWD_##name,#name) \
 KWD_FLAGS(KWD_##name,TPP_KEYWORDFLAG_HAS_ATTRIBUTE|\
                      TPP_KEYWORDFLAG_HAS_CPP_ATTRIBUTE|\
                      TPP_KEYWORDFLAG_HAS_DECLSPEC_ATTRIBUTE)

/* Attribute keywords. */
DEFINE_ATTRIBUTE(noinline)
DEFINE_ATTRIBUTE(noreturn)
DEFINE_ATTRIBUTE(warn_unused_result)
DEFINE_ATTRIBUTE(weak)
#if DCC_TARGET_BIN == DCC_BINARY_PE
DEFINE_ATTRIBUTE(dllexport)
DEFINE_ATTRIBUTE(dllimport)
#else
DEF_K(dllexport)
DEF_K(dllimport)
#endif
#if DCC_TARGET_BIN == DCC_BINARY_ELF
DEFINE_ATTRIBUTE(visibility)
#else
DEF_K(visibility)
#endif
DEFINE_ATTRIBUTE(alias)
DEFINE_ATTRIBUTE(weakref)
DEFINE_ATTRIBUTE(used)
DEFINE_ATTRIBUTE(unused)
DEFINE_ATTRIBUTE(cdecl)
DEFINE_ATTRIBUTE(stdcall)
DEFINE_ATTRIBUTE(thiscall)
DEFINE_ATTRIBUTE(fastcall)
DEFINE_ATTRIBUTE(section)
DEFINE_ATTRIBUTE(dll)
DEFINE_ATTRIBUTE(returns_twice)
DEFINE_ATTRIBUTE(force_align_arg_pointer)
DEFINE_ATTRIBUTE(regparm)
DEFINE_ATTRIBUTE(cold)
DEFINE_ATTRIBUTE(hot)
DEFINE_ATTRIBUTE(pure)
DEFINE_ATTRIBUTE(nothrow)
DEFINE_ATTRIBUTE(noclone)
DEFINE_ATTRIBUTE(nonnull)
DEFINE_ATTRIBUTE(naked)
DEFINE_ATTRIBUTE(malloc)
DEFINE_ATTRIBUTE(leaf)
DEFINE_ATTRIBUTE(format_arg)
DEFINE_ATTRIBUTE(format)
DEFINE_ATTRIBUTE(externally_visible)
KWD_FLAGS(KWD_deprecated,TPP_KEYWORDFLAG_HAS_BUILTIN|
                         TPP_KEYWORDFLAG_HAS_CPP_ATTRIBUTE|
                         TPP_KEYWORDFLAG_HAS_DECLSPEC_ATTRIBUTE)
DEFINE_ATTRIBUTE(constructor)
DEFINE_ATTRIBUTE(destructor)
DEFINE_ATTRIBUTE(aligned)
DEFINE_ATTRIBUTE(packed)
DEFINE_ATTRIBUTE(alloc_size)
DEFINE_ATTRIBUTE(always_inline)
DEFINE_ATTRIBUTE(gnu_inline)
DEFINE_ATTRIBUTE(artificial)
DEFINE_ATTRIBUTE(transparent_union)
DEFINE_ATTRIBUTE(ms_struct)
DEFINE_ATTRIBUTE(gcc_struct)
DEFINE_ATTRIBUTE(mode)
DEFINE_ATTRIBUTE(arithmetic) /* Allow arithmetic operations on structure types. */
#undef DEFINE_ATTRIBUTE

DEF_K(QI) /*< An integer that is as wide as the smallest addressable unit, usually 8 bits. */
DEF_K(HI) /*< An integer, twice as wide as a QI mode integer, usually 16 bits. */
DEF_K(SI) /*< An integer, four times as wide as a QI mode integer, usually 32 bits. */
DEF_K(DI) /*< An integer, eight times as wide as a QI mode integer, usually 64 bits. */
DEF_K(SF) /*< A floating point value, as wide as a SI mode integer, usually 32 bits. */
DEF_K(DF) /*< A floating point value, as wide as a DI mode integer, usually 64 bits. */

DEF_K(_cdecl)    DEF_K(__cdecl)
DEF_K(_stdcall)  DEF_K(__stdcall)
DEF_K(_fastcall) DEF_K(__fastcall)
DEF_K(__thiscall)

/* Predefined macro: '__ASSEMBLER__' (Only defined while parsing assembler code) */
PREDEFINED_MACRO_IF(__ASSEMBLER__,compiler.c_flags&DCC_COMPILER_FLAG_INASM,"1")

/* Add the dcc-version predefined macro.
 * >> Got'a identify this compiler somehow! */
PREDEFINED_MACRO(__DCC_VERSION__,DCC_PP_STR(DCC_COMPILER_VERSION))

#if 1 /* Define all 3 versions of a keyword name. */
#define ARCH_MACRO3(name) \
 PREDEFINED_KWDMACRO(KWD_##name,#name,"1") \
 PREDEFINED_MACRO(__##name,"1") \
 PREDEFINED_MACRO(__##name##__,"1")
#else
#define ARCH_MACRO3(name) PREDEFINED_MACRO(__##name##__,"1")
#endif

/* Predefined macros. */
#if DCC_TARGET_IA32(300)
ARCH_MACRO3(i386)
#endif
#if DCC_TARGET_IA32(400)
ARCH_MACRO3(i486)
#endif
#if DCC_TARGET_IA32(500)
ARCH_MACRO3(i586)
#endif
#if DCC_TARGET_IA32(600)
ARCH_MACRO3(i686)
#endif
#if DCC_TARGET_CPU == DCC_TARGET_X86_64
PREDEFINED_MACRO(__x86_64__,"1")
#endif
#undef ARCH_MACRO3

#if DCC_TARGET_SIZEOF_POINTER == 8 && \
    DCC_TARGET_SIZEOF_LONG == 8
PREDEFINED_MACRO(__LP64__,"1")
PREDEFINED_MACRO(_LP64,"1")
#endif
PREDEFINED_MACRO(__BYTE_ORDER__,         DCC_PP_STR(DCC_TARGET_BYTEORDER))
PREDEFINED_MACRO(__FLOAT_WORD_ORDER__,   DCC_PP_STR(DCC_TARGET_FLOAT_WORD_ORDER))
PREDEFINED_MACRO(__ORDER_LITTLE_ENDIAN__,"1234")
PREDEFINED_MACRO(__ORDER_BIG_ENDIAN__,   "4321")
PREDEFINED_MACRO(__ORDER_PDP_ENDIAN__,   "3412")
PREDEFINED_MACRO(__ATOMIC_RELAXED,DCC_PP_STR(DCC_TARGET_ATOMIC_RELAXED))
PREDEFINED_MACRO(__ATOMIC_CONSUME,DCC_PP_STR(DCC_TARGET_ATOMIC_CONSUME))
PREDEFINED_MACRO(__ATOMIC_ACQUIRE,DCC_PP_STR(DCC_TARGET_ATOMIC_ACQUIRE))
PREDEFINED_MACRO(__ATOMIC_RELEASE,DCC_PP_STR(DCC_TARGET_ATOMIC_RELEASE))
PREDEFINED_MACRO(__ATOMIC_ACQ_REL,DCC_PP_STR(DCC_TARGET_ATOMIC_ACQ_REL))
PREDEFINED_MACRO(__ATOMIC_SEQ_CST,DCC_PP_STR(DCC_TARGET_ATOMIC_SEQ_CST))

PREDEFINED_MACRO(__SIZEOF_INT__,        DCC_PP_STR(DCC_TARGET_SIZEOF_INT))
PREDEFINED_MACRO(__SIZEOF_LONG__,       DCC_PP_STR(DCC_TARGET_SIZEOF_LONG))
PREDEFINED_MACRO(__SIZEOF_LONG_LONG__,  DCC_PP_STR(DCC_TARGET_SIZEOF_LONG_LONG))
PREDEFINED_MACRO(__SIZEOF_SHORT__,      DCC_PP_STR(DCC_TARGET_SIZEOF_SHORT))
PREDEFINED_MACRO(__SIZEOF_POINTER__,    DCC_PP_STR(DCC_TARGET_SIZEOF_POINTER))
PREDEFINED_MACRO(__SIZEOF_FLOAT__,      DCC_PP_STR(DCC_TARGET_SIZEOF_FLOAT))
PREDEFINED_MACRO(__SIZEOF_DOUBLE__,     DCC_PP_STR(DCC_TARGET_SIZEOF_DOUBLE))
PREDEFINED_MACRO(__SIZEOF_LONG_DOUBLE__,DCC_PP_STR(DCC_TARGET_SIZEOF_LONG_DOUBLE))
PREDEFINED_MACRO(__SIZEOF_SIZE_T__,     DCC_PP_STR(DCC_TARGET_SIZEOF_SIZE_T))
PREDEFINED_MACRO(__SIZEOF_WCHAR_T__,    DCC_PP_STR(DCC_TARGET_SIZEOF_WCHAR_T))
PREDEFINED_MACRO(__SIZEOF_WINT_T__,     DCC_PP_STR(DCC_TARGET_SIZEOF_WINT_T))
PREDEFINED_MACRO(__SIZEOF_PTRDIFF_T__,  DCC_PP_STR(DCC_TARGET_SIZEOF_PTRDIFF_T))
#if 1 /* Some more predefined sizeof-macros not available in GCC */
PREDEFINED_MACRO(__SIZEOF_CHAR__,         DCC_PP_STR(DCC_TARGET_SIZEOF_CHAR))
PREDEFINED_MACRO(__SIZEOF_INT_LEAST8_T__, DCC_PP_STR(DCC_TARGET_SIZEOF_INT_LEAST8_T))
PREDEFINED_MACRO(__SIZEOF_INT_LEAST16_T__,DCC_PP_STR(DCC_TARGET_SIZEOF_INT_LEAST16_T))
PREDEFINED_MACRO(__SIZEOF_INT_LEAST32_T__,DCC_PP_STR(DCC_TARGET_SIZEOF_INT_LEAST32_T))
PREDEFINED_MACRO(__SIZEOF_INT_LEAST64_T__,DCC_PP_STR(DCC_TARGET_SIZEOF_INT_LEAST64_T))
PREDEFINED_MACRO(__SIZEOF_INT_FAST8_T__,  DCC_PP_STR(DCC_TARGET_SIZEOF_INT_FAST8_T))
PREDEFINED_MACRO(__SIZEOF_INT_FAST16_T__, DCC_PP_STR(DCC_TARGET_SIZEOF_INT_FAST16_T))
PREDEFINED_MACRO(__SIZEOF_INT_FAST32_T__, DCC_PP_STR(DCC_TARGET_SIZEOF_INT_FAST32_T))
PREDEFINED_MACRO(__SIZEOF_INT_FAST64_T__, DCC_PP_STR(DCC_TARGET_SIZEOF_INT_FAST64_T))
PREDEFINED_MACRO(__SIZEOF_INTMAX_T__,     DCC_PP_STR(DCC_TARGET_SIZEOF_INTMAX_T))
#endif

PREDEFINED_MACRO(__SCHAR_WIDTH__,      DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_CHAR)))
PREDEFINED_MACRO(__SHRT_WIDTH__,       DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_SHORT)))
PREDEFINED_MACRO(__INT_WIDTH__,        DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT)))
PREDEFINED_MACRO(__LONG_WIDTH__,       DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_LONG)))
PREDEFINED_MACRO(__LONG_LONG_WIDTH__,  DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_LONG_LONG)))
PREDEFINED_MACRO(__PTRDIFF_WIDTH__,    DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_PTRDIFF_T)))
PREDEFINED_MACRO(__SIG_ATOMIC_WIDTH__, DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_SIG_ATOMIC_T)))
PREDEFINED_MACRO(__SIZE_WIDTH__,       DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_SIZE_T)))
PREDEFINED_MACRO(__WCHAR_WIDTH__,      DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_WCHAR_T)))
PREDEFINED_MACRO(__WINT_WIDTH__,       DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_WINT_T)))
PREDEFINED_MACRO(__INT_LEAST8_WIDTH__, DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_LEAST8_T)))
PREDEFINED_MACRO(__INT_LEAST16_WIDTH__,DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_LEAST16_T)))
PREDEFINED_MACRO(__INT_LEAST32_WIDTH__,DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_LEAST32_T)))
PREDEFINED_MACRO(__INT_LEAST64_WIDTH__,DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_LEAST64_T)))
PREDEFINED_MACRO(__INT_FAST8_WIDTH__,  DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_FAST8_T)))
PREDEFINED_MACRO(__INT_FAST16_WIDTH__, DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_FAST16_T)))
PREDEFINED_MACRO(__INT_FAST32_WIDTH__, DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_FAST32_T)))
PREDEFINED_MACRO(__INT_FAST64_WIDTH__, DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INT_FAST64_T)))
PREDEFINED_MACRO(__INTPTR_WIDTH__,     DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_POINTER)))
PREDEFINED_MACRO(__INTMAX_WIDTH__,     DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_INTMAX_T)))

PREDEFINED_MACRO(__SIZE_TYPE__,      DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_SIZE_T))
PREDEFINED_MACRO(__PTRDIFF_TYPE__,   DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_SIZE_T))
PREDEFINED_MACRO(__WCHAR_TYPE__,     DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_WCHAR_T))
PREDEFINED_MACRO(__WINT_TYPE__,      DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_WINT_T))
PREDEFINED_MACRO(__INTMAX_TYPE__,    DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INTMAX_T))
PREDEFINED_MACRO(__UINTMAX_TYPE__,   DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INTMAX_T))
PREDEFINED_MACRO(__SIG_ATOMIC_TYPE__,DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_SIG_ATOMIC_T))

#if DCC_TARGET_BIN == DCC_BINARY_ELF
PREDEFINED_MACRO(__ELF__,"1")
#elif DCC_TARGET_BIN == DCC_BINARY_PE
PREDEFINED_MACRO(__PE__,"1")
#endif


#ifdef DCC_TARGET_TYPE_S1
PREDEFINED_MACRO(__INT8_TYPE__,DCC_TARGET_TYPE_S1)
#endif
#ifdef DCC_TARGET_TYPE_S2
PREDEFINED_MACRO(__INT16_TYPE__,DCC_TARGET_TYPE_S2)
#endif
#ifdef DCC_TARGET_TYPE_S4
PREDEFINED_MACRO(__INT32_TYPE__,DCC_TARGET_TYPE_S4)
#endif
#ifdef DCC_TARGET_TYPE_S8
PREDEFINED_MACRO(__INT64_TYPE__,DCC_TARGET_TYPE_S8)
#endif
#ifdef DCC_TARGET_TYPE_U1
PREDEFINED_MACRO(__UINT8_TYPE__,DCC_TARGET_TYPE_U1)
#endif
#ifdef DCC_TARGET_TYPE_U2
PREDEFINED_MACRO(__UINT16_TYPE__,DCC_TARGET_TYPE_U2)
#endif
#ifdef DCC_TARGET_TYPE_U4
PREDEFINED_MACRO(__UINT32_TYPE__,DCC_TARGET_TYPE_U4)
#endif
#ifdef DCC_TARGET_TYPE_U8
PREDEFINED_MACRO(__UINT64_TYPE__,DCC_TARGET_TYPE_U8)
#endif

PREDEFINED_MACRO(__INT_LEAST8_TYPE__,  DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_LEAST8_T))
PREDEFINED_MACRO(__INT_LEAST16_TYPE__, DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_LEAST16_T))
PREDEFINED_MACRO(__INT_LEAST32_TYPE__, DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_LEAST32_T))
PREDEFINED_MACRO(__INT_LEAST64_TYPE__, DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_LEAST64_T))
PREDEFINED_MACRO(__UINT_LEAST8_TYPE__, DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_LEAST8_T))
PREDEFINED_MACRO(__UINT_LEAST16_TYPE__,DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_LEAST16_T))
PREDEFINED_MACRO(__UINT_LEAST32_TYPE__,DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_LEAST32_T))
PREDEFINED_MACRO(__UINT_LEAST64_TYPE__,DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_LEAST64_T))
PREDEFINED_MACRO(__INT_FAST8_TYPE__,   DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_FAST8_T))
PREDEFINED_MACRO(__INT_FAST16_TYPE__,  DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_FAST16_T))
PREDEFINED_MACRO(__INT_FAST32_TYPE__,  DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_FAST32_T))
PREDEFINED_MACRO(__INT_FAST64_TYPE__,  DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_INT_FAST64_T))
PREDEFINED_MACRO(__UINT_FAST8_TYPE__,  DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_FAST8_T))
PREDEFINED_MACRO(__UINT_FAST16_TYPE__, DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_FAST16_T))
PREDEFINED_MACRO(__UINT_FAST32_TYPE__, DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_FAST32_T))
PREDEFINED_MACRO(__UINT_FAST64_TYPE__, DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_INT_FAST64_T))

PREDEFINED_MACRO(__INTPTR_TYPE__, DCC_TARGET_TYPE_S(DCC_TARGET_SIZEOF_POINTER))
PREDEFINED_MACRO(__UINTPTR_TYPE__,DCC_TARGET_TYPE_U(DCC_TARGET_SIZEOF_POINTER))

PREDEFINED_MACRO(__CHAR_BIT__,DCC_PP_STR(DCC_MUL8(DCC_TARGET_SIZEOF_CHAR)))

PREDEFINED_MACRO(__SCHAR_MIN__,     DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_CHAR))
PREDEFINED_MACRO(__SCHAR_MAX__,     DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_CHAR))
PREDEFINED_MACRO(__WCHAR_MIN__,     "0")
PREDEFINED_MACRO(__WCHAR_MAX__,     DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_WCHAR_T))
PREDEFINED_MACRO(__SHRT_MIN__,      DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_SHORT))
PREDEFINED_MACRO(__SHRT_MAX__,      DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_SHORT))
PREDEFINED_MACRO(__USHRT_MAX__,     DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_SHORT))
PREDEFINED_MACRO(__INT_MIN__,       DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT))
PREDEFINED_MACRO(__INT_MAX__,       DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT))
PREDEFINED_MACRO(__UINT_MAX__,      DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT))
PREDEFINED_MACRO(__LONG_MIN__,      DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_LONG))
PREDEFINED_MACRO(__LONG_MAX__,      DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_LONG))
PREDEFINED_MACRO(__ULONG_MAX__,     DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_LONG))
PREDEFINED_MACRO(__LONG_LONG_MIN__, DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_LONG_LONG))
PREDEFINED_MACRO(__LONG_LONG_MAX__, DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_LONG_LONG))
PREDEFINED_MACRO(__ULONG_LONG_MAX__,DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_LONG_LONG))
PREDEFINED_MACRO(__WINT_MIN__,      DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_WINT_T))
PREDEFINED_MACRO(__WINT_MAX__,      DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_WINT_T))
PREDEFINED_MACRO(__SIZE_MAX__,      DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_SIZE_T))
PREDEFINED_MACRO(__SSIZE_MIN__,     DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_SIZE_T))
PREDEFINED_MACRO(__SSIZE_MAX__,     DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_SIZE_T))
PREDEFINED_MACRO(__PTRDIFF_MIN__,   DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_PTRDIFF_T))
PREDEFINED_MACRO(__PTRDIFF_MAX__,   DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_PTRDIFF_T))
PREDEFINED_MACRO(__INTMAX_MIN__,    DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INTMAX_T))
PREDEFINED_MACRO(__INTMAX_MAX__,    DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INTMAX_T))
PREDEFINED_MACRO(__UINTMAX_MAX__,   DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INTMAX_T))
PREDEFINED_MACRO(__SIG_ATOMIC_MIN__,DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_SIG_ATOMIC_T))
PREDEFINED_MACRO(__SIG_ATOMIC_MAX__,DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_SIG_ATOMIC_T))

PREDEFINED_MACRO(__INT8_MIN__,  DCC_TARGET_MIN_S1)
PREDEFINED_MACRO(__INT16_MIN__, DCC_TARGET_MIN_S2)
PREDEFINED_MACRO(__INT32_MIN__, DCC_TARGET_MIN_S4)
PREDEFINED_MACRO(__INT64_MIN__, DCC_TARGET_MIN_S4)
PREDEFINED_MACRO(__INT8_MAX__,  DCC_TARGET_MAX_S1)
PREDEFINED_MACRO(__INT16_MAX__, DCC_TARGET_MAX_S2)
PREDEFINED_MACRO(__INT32_MAX__, DCC_TARGET_MAX_S4)
PREDEFINED_MACRO(__INT64_MAX__, DCC_TARGET_MAX_S4)
PREDEFINED_MACRO(__UINT8_MAX__, DCC_TARGET_MAX_U1)
PREDEFINED_MACRO(__UINT16_MAX__,DCC_TARGET_MAX_U2)
PREDEFINED_MACRO(__UINT32_MAX__,DCC_TARGET_MAX_U4)
PREDEFINED_MACRO(__UINT64_MAX__,DCC_TARGET_MAX_U4)

PREDEFINED_MACRO(__INT_LEAST8_MIN__,  DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST8_T))
PREDEFINED_MACRO(__INT_LEAST16_MIN__, DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST16_T))
PREDEFINED_MACRO(__INT_LEAST32_MIN__, DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST32_T))
PREDEFINED_MACRO(__INT_LEAST64_MIN__, DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_LEAST64_T))
PREDEFINED_MACRO(__INT_LEAST8_MAX__,  DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST8_T))
PREDEFINED_MACRO(__INT_LEAST16_MAX__, DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST16_T))
PREDEFINED_MACRO(__INT_LEAST32_MAX__, DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST32_T))
PREDEFINED_MACRO(__INT_LEAST64_MAX__, DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_LEAST64_T))
PREDEFINED_MACRO(__UINT_LEAST8_MAX__, DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST8_T))
PREDEFINED_MACRO(__UINT_LEAST16_MAX__,DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST16_T))
PREDEFINED_MACRO(__UINT_LEAST32_MAX__,DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST32_T))
PREDEFINED_MACRO(__UINT_LEAST64_MAX__,DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_LEAST64_T))
PREDEFINED_MACRO(__INT_FAST8_MIN__,   DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_FAST8_T))
PREDEFINED_MACRO(__INT_FAST16_MIN__,  DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_FAST16_T))
PREDEFINED_MACRO(__INT_FAST32_MIN__,  DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_FAST32_T))
PREDEFINED_MACRO(__INT_FAST64_MIN__,  DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_INT_FAST64_T))
PREDEFINED_MACRO(__INT_FAST8_MAX__,   DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_FAST8_T))
PREDEFINED_MACRO(__INT_FAST16_MAX__,  DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_FAST16_T))
PREDEFINED_MACRO(__INT_FAST32_MAX__,  DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_FAST32_T))
PREDEFINED_MACRO(__INT_FAST64_MAX__,  DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_INT_FAST64_T))
PREDEFINED_MACRO(__UINT_FAST8_MAX__,  DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_FAST8_T))
PREDEFINED_MACRO(__UINT_FAST16_MAX__, DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_FAST16_T))
PREDEFINED_MACRO(__UINT_FAST32_MAX__, DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_FAST32_T))
PREDEFINED_MACRO(__UINT_FAST64_MAX__, DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_INT_FAST64_T))

PREDEFINED_MACRO(__INTPTR_MIN__, DCC_TARGET_MIN_S(DCC_TARGET_SIZEOF_POINTER))
PREDEFINED_MACRO(__INTPTR_MAX__, DCC_TARGET_MAX_S(DCC_TARGET_SIZEOF_POINTER))
PREDEFINED_MACRO(__UINTPTR_MAX__,DCC_TARGET_MAX_U(DCC_TARGET_SIZEOF_POINTER))
PREDEFINED_MACRO(__NULL__,       "0")

PREDEFINED_MACRO_IF(__CHAR_UNSIGNED__,TPPLexer_Current->l_flags&TPPLEXER_FLAG_CHAR_UNSIGNED,"1")
PREDEFINED_MACRO(__WCHAR_UNSIGNED__,"1")

/* Predefine with empty text to simply ignore it everywhere...
 * TODO: Now that DCC's starting to get more and more extension
 *       warnings, we might want to start using this... */
PREDEFINED_MACRO(__extension__,"")


/* 8-bit registers. */
DEF_K(al) DEF_K(cl) DEF_K(dl) DEF_K(bl)
DEF_K(ah) DEF_K(ch) DEF_K(dh) DEF_K(bh)

/* 16-bit registers. */
DEF_K(ax) DEF_K(cx) DEF_K(dx) DEF_K(bx)
DEF_K(sp) DEF_K(bp) DEF_K(si) DEF_K(di)

/* 32-bit registers. */
DEF_K(eax) DEF_K(ecx) DEF_K(edx) DEF_K(ebx)
DEF_K(esp) DEF_K(ebp) DEF_K(esi) DEF_K(edi)

#if DCC_TARGET_CPU == DCC_TARGET_X86_64
/* 64-bit registers. */
DEF_K(rax) DEF_K(rcx) DEF_K(rdx) DEF_K(rbx)
DEF_K(rsp) DEF_K(rbp) DEF_K(rsi) DEF_K(rdi)
#endif

/* MMX registers. */
DEF_K(mm0) DEF_K(mm1) DEF_K(mm2) DEF_K(mm3)
DEF_K(mm4) DEF_K(mm5) DEF_K(mm6) DEF_K(mm7)

/* SSE registers. */
DEF_K(xmm0) DEF_K(xmm1) DEF_K(xmm2) DEF_K(xmm3)
DEF_K(xmm4) DEF_K(xmm5) DEF_K(xmm6) DEF_K(xmm7)

/* Control registers. */
DEF_K(cr0) DEF_K(cr1) DEF_K(cr2) DEF_K(cr3)
DEF_K(cr4) DEF_K(cr5) DEF_K(cr6) DEF_K(cr7)

/* Test registers. */
DEF_K(tr0) DEF_K(tr1) DEF_K(tr2) DEF_K(tr3)
DEF_K(tr4) DEF_K(tr5) DEF_K(tr6) DEF_K(tr7)

/* Debug registers. */
DEF_K(db0) DEF_K(db1) DEF_K(db2) DEF_K(db3)
DEF_K(db4) DEF_K(db5) DEF_K(db6) DEF_K(db7)

/* Debug registers. */
DEF_K(dr0) DEF_K(dr1) DEF_K(dr2) DEF_K(dr3)
DEF_K(dr4) DEF_K(dr5) DEF_K(dr6) DEF_K(dr7)

/* Segment registers. */
DEF_K(es) DEF_K(cs) DEF_K(ss) DEF_K(ds)
DEF_K(fs) DEF_K(gs)

/* Floating point st(i) registers. */
DEF_K(st)

#define DEF_OPCODE(name,...)	 KWD(KWD_##name,#name)
#include "def-asm.inl"
#undef DEF_OPCODE

/* Keywords for assembly directives. */
DEF_K(align)
DEF_K(skip)
DEF_K(space)
DEF_K(quad)
/* DEF_K(short) */
DEF_K(byte)
DEF_K(word)
DEF_K(hword)
DEF_K(octa)
/* DEF_K(long) */
/* DEF_K(int) // Already an opcode */
DEF_K(fill)
DEF_K(org)
/* DEF_K(extern) */
DEF_K(global)
DEF_K(globl)
DEF_K(protected)
DEF_K(hidden)
DEF_K(internal)
/* DEF_K(weak) // Already an __attribute__ */
DEF_K(string)
DEF_K(ascii)
DEF_K(asciz)
DEF_K(text)
DEF_K(data)
DEF_K(bss)
DEF_K(file)
DEF_K(size)
DEF_K(type)
/* DEF_K(section) // Already an __attribute__ */
DEF_K(previous)
#if DCC_TARGET_IA32(0)
DEF_K(code16)
DEF_K(code32)
#endif
#if DCC_TARGET_CPU == DCC_TARGET_X86_64
DEF_K(code64)
#endif
DEF_K(set)
DEF_K(incbin)
DEF_K(lflags)
DEF_K(ln)


/* Use AT&T-style assembly syntax. */
EXTENSION(EXT_ASM_ATT,"asm-atnt",1)

/* Allow additional fixed-length instruction suffixes:
 * >> 8   --> b
 * >> 16  --> w
 * >> 32  --> l
 * >> 64  --> q
 * >> 128 --> (no mnemonic)
 * >> I   --> l/q (based on assembly target; aka. __SIZEOF_POINTER__) */
EXTENSION(EXT_ASM_FIXED_LENGTH,"asm-fixed-length",1)
EXTENSION(EXT_ASM_CASE_INSENSITIVE,"asm-case-insensitive",1) /* Case-insensitive opcode mnemonics. */

/* Allow c-style strings as immediate arguments:
 * >> mov $"foobar", %eax
 * >> cmp $'f', 0(%eax)
 * >> cmp $'o', 1(%eax)
 * >> cmp $'o', 2(%eax)
 * >>
 * >> mov "\x10\x20\x30\x40", %eax
 * >> cmp $0x40302010, %eax
 * NOTE: The strings are allocated in the '.string' section,
 *       and special optimizations are performed to prevent
 *       the same strings from being allocated more than once:
 * >> mov "foobar", %eax
 * >> mov "foobar", %eax // Nothing changed
 * >> mov "a\0b", %eax
 * >> mov "a", %eax // Nothing changed
 * >> mov "b", %eax // Will not allocate a new string
 */
EXTENSION(EXT_ASM_IMM_STRINGS,"asm-imm-strings",1)

EXTENSION(EXT_GCC_EXPRSTMT,"expression-statements",1)                    /*< int x = ({ int y = 42; y*2; }) */
EXTENSION(EXT_GCC_LABEL_EXPR,"label-expressions",1)                      /*< goto *(p + 42); */
EXTENSION(EXT_GCC_LOCAL_LABEL,"local-labels",1)                          /*< __label__ foo; */
EXTENSION(EXT_GCC_ATTRIBUTE,"gcc-attributes",1)                          /*< __attribute__((__noreturn__)) */
EXTENSION(EXT_MSVC_ATTRIBUTE,"msvc-attributes",1)                        /*< __declspec(noreturn) */
EXTENSION(EXT_CXX11_ATTRIBUTE,"cxx-11-attributes",1)                     /*< [[__noreturn__]] */
EXTENSION(EXT_ATTRIBUTE_CONDITION,"attribute-conditions",1)              /*< __attribute__((__noreturn__(x == 42))) */
EXTENSION(EXT_CALLING_CONVENTION_ATTR,"calling-convention-attributes",1) /*< __cdecl, __stdcall, ... */
EXTENSION(EXT_FIXED_LENGTH_INTEGER_TYPES,"fixed-length-integer-types",1) /*< __int(8|16|32|64) */
EXTENSION(EXT_ASM_REGISTERS,"asm-registers-in-expressions",1)            /*< int x = %eax; */
EXTENSION(EXT_ASM_ADDRESS,"asm-address-in-expressions",1)                /*< void *here = .; */
EXTENSION(EXT_VOID_ARITHMETIC,"void-arithmetic",1)                       /*< Allow pointer arithmetic on void/function types, faking their size as '1'. */
EXTENSION(EXT_STRUCT_COMPATIBLE,"struct-compatible",1)                   /*< Allow structures with the same contents and layout to be compatible with each other. */
EXTENSION(EXT_AUTO_FOR_AUTOTYPE,"auto-in-type-expressions",1)            /*< Allow 'auto' to refer to '__auto_type' in type expressions, as well as describe automatic storage. */
EXTENSION(EXT_VARIABLE_LENGTH_ARRAYS,"variable-length-arrays",1)         /*< Allow VLA-style arrays. */
EXTENSION(EXT_FUNCTION_STRING_LITERALS,"function-string-literals",1)     /*< Treat '__FUNCTION__' and '__PRETTY_FUNCTION__' as string literals during language-level string-concatation. */
WGROUP(WG_CONSTANT_CASE,"constant-case-expressions",WSTATE_ERROR)        /*< Warn about non-constant case expressions. */
WGROUP(WG_EXTENSIONS,"extensions",WSTATE_ERROR)                          /*< Enable/disable extension warnings (Those things that are really sweet syntactically, but you sadly can't use for standard-compliance). */
WGROUP(WG_CASE_RANGES,"case-ranges",WSTATE_ERROR)                        /*< Warn about using case-ranges. */
WGROUP(WG_DECL_IN_IF,"declaration-in-if",WSTATE_ERROR)                   /*< Warn about variable declarations in if-conditions. */
WGROUP(WG_DECL_IN_FOR,"declaration-in-for",WSTATE_ERROR)                 /*< Warn about variable declarations in for-initializers. */
WGROUP(WG_ZERO_TYPED_ARRAY,"zero-sized-arrays",WSTATE_ERROR)             /*< Warn about zero-sized array types. */
WGROUP(WG_NESTED_FUNCTIONS,"nested-functions",WSTATE_ERROR)              /*< Warn about nested functions. */
WGROUP(WG_EMPTY_STRUCTURES,"empty-structures",WSTATE_ERROR)              /*< Warn about empty structures. */
WGROUP(WG_OLD_FUNCTION_DECL,"old-function-decl",WSTATE_ERROR)            /*< Warn about old-style function declarations. */
WGROUP(WG_MIXED_DECLARATIONS,"mixed-declarations",WSTATE_ERROR)          /*< Warn about declarations mixed with statements. */
WGROUP(WG_TYPE_IN_EXPRESSION,"type-in-expression",WSTATE_ERROR)          /*< Warn if c++-style calls to types are used in expressions. */
WGROUP(WG_ASSIGN_INITIALIZER,"assign-initializer",WSTATE_ERROR)          /*< Warn if brace-initializer are used during assignment. */
WGROUP(WG_ASSIGN_VOID_VOID,"assign-void",WSTATE_ERROR)                   /*< Warn about assigning void-to-void (Also warned when returning a void-expression in a void-function). */
WGROUP(WG_POINTER_ARITHMETIC,"pointer-arithmetic",WSTATE_ERROR)          /*< Warn about illegal pointer arithmetic. */

WGROUP(WG_ASM,"asm",WSTATE_ERROR)
WGROUP(WG_PRAGMA,"pragma",WSTATE_ERROR)
WGROUP(WG_SIZEOF,"sizeof",WSTATE_ERROR) /*< Warn about questionable use of sizeof(). */
WGROUP(WG_C99,"c99",WSTATE_ERROR)       /*< Warn about use of c99-only features. */
WGROUP(WG_C11,"c11",WSTATE_ERROR)       /*< Warn about use of c11-only features. */
WGROUP(WG_ATTRIBUTE,"attribute",WSTATE_ERROR)
WGROUP(WG_TYPE,"type",WSTATE_ERROR)
WGROUP(WG_CAST,"cast",WSTATE_ERROR)
WGROUP(WG_LINKER,"linker",WSTATE_ERROR)
WGROUP(WG_LIBLOAD,"libload",WSTATE_ERROR)

#ifdef DECLARE_WARNING_MESSAGES
{
 struct DCCDecl *decl;
 char const *text;
print_declaration:
 if (!text) text = "See reference to declaration";
 WARNF(TPPLexer_Current->l_flags&TPPLEXER_FLAG_MSVC_MESSAGEFORMAT
       ? "%s(%d) : " : "%s:%d: ",
       TPPFile_Filename(decl->d_file,NULL),(int)decl->d_line+1);
 WARNF(text);
 break;
{
 struct TPPString *type_text;
print_typed_declaration_load:
 decl = ARG(struct DCCDecl *);
//print_typed_declaration:
 type_text = DCCType_ToTPPString(&decl->d_type,decl->d_name);
 WARNF(text,type_text->s_text);
 TPPString_Decref(type_text);
 text = NULL;
 goto print_declaration;
} break;
#endif
#define DECL_LOAD()              decl = ARG(struct DCCDecl *)
#define DECL_KWD()               decl->d_name
#define DECL_NAME()              decl->d_name->k_name
#define DECL_TYPE()            (&decl->d_type)
#define DECL_PRINT(msg)        { text = msg; goto print_declaration; }
#define DECL_PRINTTY(msg)      { text = msg; goto print_typed_declaration; }
#define DECL_PRINTTY_LOAD(msg) { text = msg; goto print_typed_declaration_load; }

WARNING_NAMESPACE(WN_ASM,1000)
DEF_WARNING(W_ASM_EXPECTED_INSTR,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected assembly instruction, but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_JUNK_AFTER_INSTR,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Encountered junk " TOK_S " after assembly instruction",TOK_A))
DEF_WARNING(W_ASM_INVALID_ST_REGISTER,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Invalid st register: %%st(%d)",ARG(int)))
DEF_WARNING(W_ASM_EXPECTED_REGISTER_NAME,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected register name, but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_EXPECTED_OPERAND,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected operand, but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_NO_SUCH_OVERLOAD,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Instruction has no compatible overload"))
DEF_WARNING(W_ASM_AMBIGUOUS_INSTR,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Ambiguous instruction"))
DEF_WARNING(W_ASM_INVALID_REGISTER,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Invalid register"))
DEF_WARNING(W_ASM_INVALID_EFFECTIVE_ADDRESS,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Invalid effective address"))
DEF_WARNING(W_ASM_INVALID_SHIFT,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Invalid shift '%s' (expected '1', '2', '4' or '8')",CONST_STR()))
DEF_WARNING(W_ASM_INVALID_SYMBOL_OPERATION,(WG_ASM,WG_USAGE),WSTATE_WARN,WARNF("Invalid symbol operation"))
DEF_WARNING(W_ASM_EXPECTED_EXPR,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected assembly expression, but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_UNKNOWN_LOCAL_LABEL,(WG_ASM,WG_USAGE),WSTATE_WARN,WARNF("Unknown local label '%s'",KWDNAME()))
DEF_WARNING(W_ASM_UNKNOWN_DIRECTIVE,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Unknown assembly directive " TOK_S,TOK_A))
DEF_WARNING(W_ASM_EXPECTED_INTEGER_EXPRESSION,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected integer expression, but got dependency on symbol '%s'",KWDNAME()))
DEF_WARNING(W_ASM_INVALID_ALIGNMENT,(WG_ASM,WG_VALUE),WSTATE_WARN,WARNF("Expected power-of-two for alignment, but got '%lu'",(unsigned long)ARG(size_t)))
DEF_WARNING(W_ASM_DIRECTIVE_VISIBILITY_EXPECTED_KEYWORD,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword for symbol visibility directive, but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_DIRECTIVE_SECTION_EXPECTED_STRING,(WG_ASM,WG_VALUE),WSTATE_WARN,WARNF("Expected string after '.section', but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_DIRECTIVE_SECTION_UNKNOWN_FLAG,(WG_ASM,WG_VALUE),WSTATE_WARN,WARNF("Unknown section flags '%c'",ARG(char)))
DEF_WARNING(W_ASM_DIRECTIVE_STRING_EXPECTED_STRING,(WG_ASM,WG_VALUE),WSTATE_WARN,WARNF("Expected string after '.string', but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_DIRECTIVE_PREV_NO_PREVIOUS_SECTION,(WG_ASM,WG_VALUE),WSTATE_WARN,WARNF("No previously selected section"))
DEF_WARNING(W_ASM_UNKNOWN_SYMBOL_IN_ABSOLUTE_LABEL,(WG_ASM,WG_VALUE),WSTATE_WARN,WARNF("Unknown symbols '%s' referenced in absolute label",KWDNAME()))
DEF_WARNING(W_ASM_DIRECTIVE_SET_EXPECTED_KEYWORD,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword after '.set', but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_DIRECTIVE_INCLUDE_EXPECTED_STRING,(WG_ASM,WG_SYNTAX),WSTATE_WARN,WARNF("Expected string after '.include', but got " TOK_S,TOK_A))
DEF_WARNING(W_ASM_CONSTEXPR_INVALID_OPERATION,(WG_SYNTAX),WSTATE_WARN,WARNF("Invalid operation in constant expression"))
#if DCC_TARGET_IA32(386)
DEF_WARNING(W_ASM_386_RM_SHIFT_IN_CODE16,(WG_SYNTAX),WSTATE_WARN,WARNF("Register shifts are not supported in .code16 regions"))
#endif

WARNING_NAMESPACE(WN_PRAGMA,1200)
DEF_WARNING(W_PRAGMA_UNKNOWN,(WG_PRAGMA,WG_SYNTAX),WSTATE_WARN,WARNF("Unknown pragma " TOK_S,TOK_A))
DEF_WARNING(W_PRAGMA_COMMENT_EXPECTED_KEYWORD,(WG_PRAGMA,WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword after #pragma comment group, but got " TOK_S,TOK_A))
DEF_WARNING(W_PRAGMA_COMMENT_EXPECTED_STRING,(WG_PRAGMA,WG_SYNTAX),WSTATE_WARN,WARNF("Expected string after #pragma comment group, but got " TOK_S,TOK_A))
DEF_WARNING(W_PRAGMA_COMMENT_UNKNOWN,(WG_PRAGMA,WG_SYNTAX),WSTATE_WARN,WARNF("Unknown #pragma comment group " TOK_S,TOK_A))
DEF_WARNING(W_PRAGMA_PACK_NOTHING_TO_POP,(WG_PRAGMA,WG_SYNTAX),WSTATE_WARN,WARNF("No old packing state to pop in #pragma pack(pop)"))
DEF_WARNING(W_PRAGMA_PACK_EXPECTED_POWER_OF_TWO,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Expected power-of-2 for #pragma pack(...), but got '%lu'",(unsigned long)ARG(DCC(target_siz_t))))

WARNING_NAMESPACE(WN_SYNTAX,1400)
DEF_WARNING(W_EXPECTED_LBRACKET,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected '[', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_RBRACKET,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected ']', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_LBRACE,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected '{', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_RBRACE,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected '}', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_SEMICOLON,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected ';', but got " TOK_S,TOK_A))
DEF_WARNING(W_SIZEOF_WITHOUT_PARENTHESIS,(WG_QUALITY),WSTATE_WARN,WARNF("Encountered '%s' without parenthesis",KWDNAME()))
DEF_WARNING(W_IASM_EXPECTED_KEYWORD_FOR_NAMED_OPERAND,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword for named assembly operand, but got " TOK_S,TOK_A))
DEF_WARNING(W_IASM_EXPECTED_STRING_FOR_CONSTRAINTS,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected string for assembly constraints, but got " TOK_S,TOK_A))
DEF_WARNING(W_IASM_UNKNOWN_CONSTRAINT,(WG_SYNTAX),WSTATE_WARN,WARNF("Unknown assembly constraint '%c'"))
DEF_WARNING(W_IASM_CONSTRAINT_NOT_FULFILLED,(WG_SYNTAX),WSTATE_WARN,WARNF("Assembly constraint '%s' was not fulfilled"))
DEF_WARNING(W_IASM_OUTPUT_CONSTRAINT_IN_INPUT_LIST,(WG_SYNTAX),WSTATE_WARN,WARNF("Output assembly constraint in input list"))

WARNING_NAMESPACE(WN_DCC,1500)

#ifdef DECLARE_WARNING_MESSAGES
{
 char const *format;
 struct TPPString *tyname;
emit_type_warning:
 tyname = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 WARNF(format,tyname->s_text);
 TPPString_Decref(tyname);
 break;
#endif
#define TYPE_WARNING(msg) { format = msg; goto emit_type_warning; }
DEF_WARNING(W_EXPECTED_LVALUE_IN_STORE,(WG_TYPE,WG_VALUE),WSTATE_WARN,TYPE_WARNING("Expected an lvalue '%s' as store operation target"))
DEF_WARNING(W_EXPECTED_LVALUE_FOR_REFERENCE,(WG_TYPE,WG_VALUE),WSTATE_WARN,TYPE_WARNING("Expected an lvalue '%s' for reference operator '&'"))
DEF_WARNING(W_EXPECTED_LVALUE_FOR_BINARY_OP,(WG_TYPE,WG_VALUE),WSTATE_WARN,TYPE_WARNING("Expected an lvalue '%s' as inplace-binary operation target"))
DEF_WARNING(W_EXPECTED_POINTER_FOR_DEREF,(WG_TYPE,WG_VALUE),WSTATE_WARN,TYPE_WARNING("Expected a pointer type '%s' for dereference operator '*'"))
DEF_WARNING(W_ARRAY_SIZE_NEGATIVE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Negative size for array type '%s'"))
DEF_WARNING(W_UNARY_NEG_ON_UNSIGNED_TYPE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Unary operator '-' on unsigned type '%s'"))

DEF_WARNING(W_AUTO_TYPE_REQUIRES_INITIALIZER,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("'__auto_type' requires an initializer"))
DEF_WARNING(W_VARIADIC_REQUIRES_INITIALIZER,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("variadic array type '%s' require an initializer"))
DEF_WARNING(W_LVALUE_REQUIRES_INITIALIZER,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("l-value type '%s' require an initializer"))
DEF_WARNING(W_EXPECTED_FUNCTION_TYPE_FOR_CALL,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Expected function type '%s' for call"))

DEF_WARNING(W_BRACE_INITIALIZER_FOR_DEFAULT_TYPE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Brace initializer for regular type '%s'"))
DEF_WARNING(W_BRACE_INITIALIZER_FOR_VLA_TYPE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Variable length array type '%s' cannot be brace-initialized"))
DEF_WARNING(W_UNION_ALREADY_INITIALIZED,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Union type '%s' has already been initalized"))
DEF_WARNING(W_STRUCTURE_FULLY_INITIALIZED,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("struct/union type '%s' was already fully initialized"))
DEF_WARNING(W_ARRAY_FULLY_INITIALIZED,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Array type '%s' was already fully initialized"))
DEF_WARNING(W_EXPECTED_KEYWORD_FOR_FIELD_NAME,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Expected keyword for field name of structure type '%s'"))
DEF_WARNING(W_NEGATIVE_INDEX_IN_ARRAY_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Negative index in array-type '%s' initializer "))
DEF_WARNING(W_OUT_OF_BOUNDS_IN_ARRAY_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Out-of-bounds index in array-type '%s' initializer "))
DEF_WARNING(W_UNORDERED_RANGE_IN_ARRAY_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Unordered index-range in array-type '%s' initializer "))
DEF_WARNING(W_EMPTY_RANGE_IN_ARRAY_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Empty index-rage in array-type '%s' initializer "))
DEF_WARNING(W_NON_CONSTANT_GLOBAL_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Static initializer for '%s' cannot be determined at compile-time (Consider moving it into a function scope)"))
DEF_WARNING(W_NON_CONSTANT_STATIC_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Static initializer for '%s' is evaluated at runtime"))
DEF_WARNING(W_TYPE_IN_EXPRESSION,(WG_TYPE_IN_EXPRESSION,WG_EXTENSIONS),WSTATE_WARN,TYPE_WARNING("Type '%s' appears in expression"))
DEF_WARNING(W_EXPECTED_LPAREN_AFTER_TYPE_IN_EXPRESSION,(WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Expected '(' or '{' after type '%s' in expression"))
DEF_WARNING(W_BRACE_INITIALIZER_DURING_ASSIGNMENT,(WG_ASSIGN_INITIALIZER,WG_EXTENSIONS),WSTATE_WARN,TYPE_WARNING("Encountered brace-initializers during assignment of type '%s'"))
DEF_WARNING(W_LOCAL_VARRAY_VERY_INEFFICIENT,(WG_QUALITY),WSTATE_WARN,TYPE_WARNING("Locally initialized variadic array type '%s' is very inefficient"))
DEF_WARNING(W_INCOMPLETE_TYPE_DESCRIPTOR,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Incomplete type descriptor interpreted as '%s'"))
DEF_WARNING(W_STRUCTURE_ARITHMETIC,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Arithmetic operation with structure type '%s'"))
DEF_WARNING(W_POINTER_ARITHMETIC_VOID,(WG_POINTER_ARITHMETIC,WG_TYPE),WSTATE_WARN,TYPE_WARNING("Pointer arithmetic on void or function type '%s'"))
DEF_WARNING(W_POINTER_ARITHMETIC_INCOMPLETE,(WG_POINTER_ARITHMETIC,WG_TYPE),WSTATE_WARN,TYPE_WARNING("Pointer arithmetic on pointer to incomplete type '%s'"))
DEF_WARNING(W_POINTER_ARITHMETIC_EXPECTED_INTEGRAL,(WG_POINTER_ARITHMETIC,WG_TYPE),WSTATE_WARN,TYPE_WARNING("Expected integral type for binary pointer arithmetic, but got '%s'"))
DEF_WARNING(W_UNARY_CONSTANT_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Unary operation on constant type '%s'"))
DEF_WARNING(W_UNARY_RVALUE_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Unary operation on r-value of type '%s'"))
DEF_WARNING(W_EXPECTED_COMPLETE_TYPE_FOR_FUNCTION_BASE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Expected a complete type '%s' as function base"))
DEF_WARNING(W_EXPECTED_COMPLETE_TYPE_FOR_ARRAY_BASE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Expected a complete type '%s' as array base"))
DEF_WARNING(W_SIZEOF_INCOMPLETE_TYPE,(WG_SIZEOF,WG_TYPE),WSTATE_WARN,TYPE_WARNING("An imcomplete type '%s' is not allowed by sizeof()"))
DEF_WARNING(W_SIZEOF_VOID_OR_FUNCTION,(WG_SIZEOF),WSTATE_WARN,TYPE_WARNING("Sizeof void or function type '%s'"))
DEF_WARNING(W_SIZEOF_VLA_ARRAY_TYPE,(WG_QUALITY,WG_SIZEOF),WSTATE_DISABLE,TYPE_WARNING("Sizeof vla array-type '%s' can only be determined at runtime"))
DEF_WARNING(W_ASSIGN_VLA_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Re-assignment VLA type '%s' pointer is not intended behavior"))
DEF_WARNING(W_SIGN_MODIFIER_MUST_BE_USED_WITH_ARITH,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("(un)signed modifier cannot be used with non-arithmetic type '%s'"))
DEF_WARNING(W_ASSIGN_INIT_CONSTANT_TYPE,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Assignment of initializer to constant type '%s'"))
DEF_WARNING(W_GENERIC_EXPRESSION_DEFAULT_NONLAST,(WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("DCC requires only the last case of a generic expression '%s' to be generic"))
DEF_WARNING(W_GENERIC_EXPRESSION_NO_MATCH,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("No matching case for '%s' in generic expression"))
DEF_WARNING(W_GENERIC_EXPRESSION_SECOND_MATCH,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Additional match for '%s' in generic expression"))
DEF_WARNING(W_GENERIC_EXPRESSION_EXPECTED_TYPE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Expected type for generic expression '%s'"))
DEF_WARNING(W_GENERIC_EXPRESSION_EXPECTED_COLON,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,TYPE_WARNING("Expected ':' after type in generic expression '%s'"))
DEF_WARNING(W_GENERIC_EXPRESSION_C11,(WG_C11,WG_EXTENSIONS),WSTATE_WARN,WARNF("_Generic expressions are only accepted by C11-compliant compilers"))
DEF_WARNING(W_BUILTIN_JMPBUF_HAS_INCORRECT_SIZE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Incorrect size for 'jmp_buf' of type '%s' in 'setjmp/longjmp'"))
DEF_WARNING(W_BUILTIN_LONGJMP_VALZERO_IS_ONE,(WG_TYPE),WSTATE_WARN,WARNF("Passing '0' as argument to 'longjmp' is translated to '1'"))
DEF_WARNING(W_BUILTIN_MEMMOVE_POINTERS_NEVER_OVERLAP,(WG_QUALITY),WSTATE_WARN,WARNF("Non-overlapping pointers passed to 'memmove'. Consider using 'memcpy' for better results"))
DEF_WARNING(W_BUILTIN_MEMCPY_POINTERS_ALWAYS_OVERLAP,(WG_QUALITY),WSTATE_WARN,WARNF("Overlapping pointers passed to 'memcpy'. For proper behavior, 'memmove' must be used"))
DEF_WARNING(W_BUILTIN_MEMCPY_POINTERS_ALWAYS_EQUAL,(WG_QUALITY),WSTATE_WARN,WARNF("Equal pointers passed to 'memcpy/memmove' makes the call redundant"))
DEF_WARNING(W_BUILTIN_MEMCMP_POINTERS_ALWAYS_EQUAL,(WG_QUALITY),WSTATE_WARN,WARNF("Equal pointers passed to 'memcmp' makes the call redundant"))
DEF_WARNING(W_BUILTIN_RETURN_ADDRESS_CONST_LEVEL,(WG_VALUE),WSTATE_WARN,WARNF("__builtin_return_address/__builtin_frame_address expect a constant integral as argument"))
DEF_WARNING(W_BUILTIN_RETURN_ADDRESS_NEG_LEVEL,(WG_VALUE),WSTATE_WARN,WARNF("__builtin_return_address/__builtin_frame_address expect a positive integral as argument"))
#undef TYPE_WARNING
#ifdef DECLARE_WARNING_MESSAGES
}
#endif

#ifdef DECLARE_WARNING_MESSAGES
{
 char const *format;
 struct TPPString *rettype;
 struct TPPString *tyrepr;
emit_funciton_return_warning:
 DECL_LOAD();
 rettype = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 tyrepr = DCCType_ToTPPString(&decl->d_type,decl->d_name);
 WARNF(format,rettype->s_text,tyrepr->s_text);
 TPPString_Decref(tyrepr);
 TPPString_Decref(rettype);
 DECL_PRINT("See reference to function declaration");
#endif
DEF_WARNING(W_FUNCTION_MISSING_RETURN,(WG_QUALITY),WSTATE_WARN,{
 format = "Return value '%s' of non-void function '%s' may be undefined\n";
 goto emit_funciton_return_warning;
})
DEF_WARNING(W_MISSING_RETURN_EXPRESSION,(WG_TYPE),WSTATE_WARN,{
 format = "Missing return expression '%s' in non-void function '%s'\n";
 goto emit_funciton_return_warning;
})
#ifdef DECLARE_WARNING_MESSAGES
}
#endif
DEF_WARNING(W_FAILED_ASSUMPTION_UNAVOIDABLE,(WG_USAGE),WSTATE_WARN,WARNF("Wrong compile-timed assumption is unavoidable"))
DEF_WARNING(W_UNKNOWN_SYMBOL_IN_EXPRESSION,(WG_VALUE),WSTATE_WARN,WARNF("Assuming 'extern int %s();' for unknown symbol",KWDNAME()))

DEF_WARNING(W_ATTRIBUTE_ALREADY_DEFINED,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Attribute '%s' was already set",KWDNAME()))
DEF_WARNING(W_ATTRIBUTE_NOT_DEFINED,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Attribute '%s' was not set",KWDNAME()))
DEF_WARNING(W_ATTRIBUTE_EXPECTED_STRING,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Expected string for __attribute__((%s(...))), but got " TOK_S,KWDNAME(),TOK_A))
DEF_WARNING(W_ATTRIBUTE_ALIAS_ALREADY_DEFINED,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Another __attribute__((alias(...))) was already defined for '%s'",KWDNAME()))
DEF_WARNING(W_ATTRIBUTE_VISIBILITY_UNKNOWN_VISIBILITY,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Unknown visibility '%s' for __attribute__((visibility(...)))",ARG(char *)))
DEF_WARNING(W_ATTRIBUTE_SECTION_UNKNOWN_SECTION,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Unknown section '%s' for __attribute__((section(...)))",ARG(char *)))
DEF_WARNING(W_ATTRIBUTE_DLL_IS_A_SECTION,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Section '%s' specified for __attribute__((dll(...)))",ARG(char *)))
DEF_WARNING(W_ATTRIBUTE_DEPRECATED_ALREADY_DEFINED,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("__attribute__((deprecated())) was already set"))
DEF_WARNING(W_ATTRIBUTE_ALIGNED_EXPECTED_POWER_OF_TWO,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Expected power-of-2 for __attribute__((aligned(...))), but got '%lu'",(unsigned long)ARG(DCC(target_ptr_t))))
DEF_WARNING(W_ATTRIBUTE_ALIAS_UNKNOWN_SYMBOL,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Unknown symbol '%s' for __attribute__((alias))",KWDNAME()))
DEF_WARNING(W_ATTRIBUTE_MODE_EXPECTED_KEYWORD,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Expected keyword in __attribute__((mode(...))), but got " TOK_S,TOK_A))
DEF_WARNING(W_ATTRIBUTE_MODE_UNKNOWN_MODE,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("Unknown name " TOK_S " for __attribute__((mode(...)))",TOK_A))
DEF_WARNING(W_ATTRIBUTE_MODE_ALREADY_DEFINED,(WG_ATTRIBUTE,WG_VALUE),WSTATE_WARN,WARNF("__attribute__((mode(...))) was already defined"))
DEF_WARNING(W_ATTRIBUTE_MODE_EXPECTS_BASIC_TYPE,(WG_ATTRIBUTE,WG_USER),WSTATE_WARN,WARNF("__attribute__((mode(...))) expects a basic type"))
DEF_WARNING(W_ATTRIBUTE_UNKNOWN,(WG_ATTRIBUTE,WG_SYNTAX),WSTATE_WARN,WARNF("Unknown attribute " TOK_S,TOK_A))
DEF_WARNING(W_ATTRIBUTE_UNSUPPORTED,(WG_ATTRIBUTE),WSTATE_DISABLE,WARNF("Unsupported attribute " TOK_S,TOK_A))
#ifdef DECLARE_WARNING_MESSAGES
{
 char const *format,*reason;
emit_attr_warning:
 DECL_LOAD();
 reason = ARG(char *);
 WARNF(format,DECL_NAME());
 if (reason && *reason) WARNF(": '%s'",reason);
 WARNF("\n");
 DECL_PRINT(NULL);
 break;
#endif
DEF_WARNING(W_ATTRIBUTE_DEPRECATED_WARNING,(WG_DEPRECATED,WG_USER),WSTATE_WARN, { format = "Deprecated symbol '%s'"; goto emit_attr_warning; })
DEF_WARNING(W_ATTRIBUTE_WARNING_WARNING,   (WG_DEPRECATED,WG_USER),WSTATE_WARN, { format = "Warning symbol '%s'"; goto emit_attr_warning; })
DEF_WARNING(W_ATTRIBUTE_ERROR_WARNING,     (WG_DEPRECATED,WG_USER),WSTATE_ERROR,{ format = "Error symbol '%s'"; goto emit_attr_warning; })
#ifdef DECLARE_WARNING_MESSAGES
}
#endif
DEF_WARNING(W_ATTRIBUTE_NAKED_USES_STACK,(WG_QUALITY),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Naked function '%s' uses local variables\n",DECL_NAME());
 DECL_PRINT("See reference to function declaration");
})
DEF_WARNING(W_ATTRIBUTE_NAKED_RETURNS_NORMALLY,(WG_QUALITY),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Naked function '%s' returns normally\n",DECL_NAME());
 DECL_PRINT("See reference to function declaration");
})
DEF_WARNING(W_TYPE_NOT_FORWARD,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Type '%s' is not forward declared\n",DECL_NAME());
 DECL_PRINT("See reference to previous declaration");
})
DEF_WARNING(W_DECL_ALREADY_DEFINED,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Declaration '%s' is already defined\n",DECL_NAME());
 DECL_PRINT("See reference to previous declaration");
})
DEF_WARNING(W_INCOMPATIBLE_ASM_NAMES,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,{
 char *old_name; char *new_name;
 DECL_LOAD();
 old_name = KWDNAME(); new_name = KWDNAME();
 WARNF("New assembly name '%s' for declaration '%s' doesn't match old name '%s'\n",
       new_name,DECL_NAME(),old_name);
 DECL_PRINT("See reference to previous declaration");
})
DEF_WARNING(W_DECL_TYPEDEF_WITH_INITIALIZER,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Found initializer for typedef '%s'\n",DECL_NAME());
 DECL_PRINT(NULL);
})

DEF_WARNING(W_STMT_ASM_EXPECTED_STRING,(WG_SYNTAX),WSTATE_WARN,
            WARNF("Expected string after __asm__, but got " TOK_S,TOK_A))
DEF_WARNING(W_CONSTANT_EXPR_DEPENDS_ON_SYMBOL,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("A constant expression here can't depend on a symbol '%s'\n",DECL_NAME());
 DECL_PRINT(NULL);
})
DEF_WARNING(W_EXPECTED_CONSTANT_EXPRESSION,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,WARNF("Expected a constant expression"))
DEF_WARNING(W_UNEXPECTED_TOKEN_IN_C_EXPRESSION,(WG_SYNTAX),WSTATE_WARN,WARNF("Unexpected token " TOK_S " in c expression",TOK_A))
DEF_WARNING(W_GOT_TYPE_IN_EXPRESSION,(WG_SYNTAX),WSTATE_WARN,WARNF("Got type in expression"))
DEF_WARNING(W_UNKNOWN_IDENTIFIER,(WG_SYNTAX),WSTATE_WARN,WARNF("Unknown identifier '%s'",KWDNAME()))
DEF_WARNING(W_EXPECTED_KEYWORD_FOR_SUBSCRIPT,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected keyword for subscript, but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_REGISTER_NAME,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected register name after '%', but got " TOK_S,TOK_A))
DEF_WARNING(W_INVALID_REGISTER_PAIR,(WG_SYNTAX),WSTATE_WARN,WARNF("Invalid register pair. Both registers must be 32-bit wide"))
DEF_WARNING(W_EXPECTED_PERCENT_BEFORE_REGISTER_NAME,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected '%' before register name"))

DEF_WARNING(W_CONTAINED_TYPENAME_IGNORED,(WG_SYNTAX),WSTATE_WARN,WARNF("Contained typename '%s' is ignored",KWDNAME()))
DEF_WARNING(W_BREAK_CONTINUE_NOT_ALLOWED,(WG_SYNTAX),WSTATE_WARN,WARNF("break/continue is not allowed here"))
DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_GOTO,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected a keyword after 'goto', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_WHILE_AFTER_DO,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected 'while' after 'do', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_KEYWORD_AFTER_LABEL_ADDRESS,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected label name after '&&', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_CONSTANT_EXPRESSION_FOR_BUILTIN_CHOOSE_EXPR,(WG_VALUE),WSTATE_WARN,WARNF("Expected a constant expression for '__builtin_choose_expr()'"))
DEF_WARNING(W_CASE_NOT_ALLOWED_HERE,(WG_SYNTAX),WSTATE_WARN,WARNF("'case' is only allowed in switch statements"))
DEF_WARNING(W_DEFAULT_NOT_ALLOWED_HERE,(WG_SYNTAX),WSTATE_WARN,WARNF("'default' cases are only allowed in switch statements"))
DEF_WARNING(W_DEFAULT_ALREADY_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("'default' case was already defined"))
DEF_WARNING(W_EXPECTED_COLON_AFTER_CASE,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected ':' after 'case', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_COLON_AFTER_DEFAULT,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected ':' after 'default', but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_CONSTANT_FOR_CASE,(WG_CONSTANT_CASE,WG_EXTENSIONS),WSTATE_WARN,WARNF("Non-constant case expression are a DCC extension"))
DEF_WARNING(W_LABEL_WITHOUT_STATEMENT,(WG_SYNTAX),WSTATE_WARN,WARNF("A label without a statement is not allowed"))
DEF_WARNING(W_UNEXPECTED_EOF_IN_STATEMENT,(WG_SYNTAX),WSTATE_WARN,WARNF("Unexpected EOF in statement"))
DEF_WARNING(W_TYPE_STORAGE_CLASS_ALREADY_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("inline stroage modifer was already defined"))
DEF_WARNING(W_TYPE_STORAGE_INLINE_ALREADY_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("Storage duration was already defined"))
DEF_WARNING(W_TYPE_MODIFIER_ATOMIC_C11,(WG_C11,WG_EXTENSIONS),WSTATE_WARN,WARNF("_Atomic type modifiers are only accepted by C11-compliant compilers"))
DEF_WARNING(W_TYPE_MODIFIER_ATOMIC_ALREADY_DEFINED,(WG_SYNTAX),WSTATE_WARN,WARNF("'_Atomic' was already defined"))
DEF_WARNING(W_TYPE_MODIFIER_ATOMIC_REQUIRES_INTEGRAL,(WG_SYNTAX),WSTATE_WARN,WARNF("'_Atomic' requires an integral type"))
DEF_WARNING(W_EXPR_FUNC_OUTSIDE_OF_FUNCTION,(WG_SYNTAX,WG_VALUE),WSTATE_WARN,WARNF("Encountered '__func__' outside of function"))
DEF_WARNING(W_PARSE_FAILED_TO_PARSE_ANYTHING,(WG_SYNTAX),WSTATE_WARN,WARNF("Encountered unexpected token " TOK_S " that prevented anything from being parsed",TOK_A))
#ifdef DECLARE_WARNING_MESSAGES
{
 char const *format;
 struct TPPString *tyrepr;
emit_typint_warning:
 tyrepr = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 WARNF(format,tyrepr->s_text,(unsigned long)ARG(target_siz_t));
 TPPString_Decref(tyrepr);
 break;
#endif
#define TYPINT_WARNING(fmt) { format = fmt; goto emit_typint_warning; }
DEF_WARNING(W_BUILTIN_BSWAP_INCORRECT_SIZE,(WG_SYNTAX),WSTATE_WARN,TYPINT_WARNING("Argument type '%s' for __builtin_bswapcc isn't '%lu' bytes large"))
DEF_WARNING(W_BUILTIN_FFS_INCORRECT_SIZE,(WG_SYNTAX),WSTATE_WARN,TYPINT_WARNING("Argument type '%s' for __builtin_ffscc isn't '%lu' bytes large"))
#undef TYPINT_WARNING
#ifdef DECLARE_WARNING_MESSAGES
}
#endif
DEF_WARNING(W_BUILTIN_ALLOCA_IN_LOOP,(WG_QUALITY),WSTATE_WARN,WARNF("using '__builtin_alloca' or variable-length-arrays may cause a stack overflow"))

/* Warnings about using extensions. */
DEF_WARNING(W_CASE_RANGES,(WG_CASE_RANGES,WG_EXTENSIONS),WSTATE_WARN,WARNF("Using case-ranges may not be portable"))
DEF_WARNING(W_DECLARATION_IN_IF_CONDITION,(WG_DECL_IN_IF,WG_EXTENSIONS),WSTATE_WARN,WARNF("if-condition contains a declaration"))
DEF_WARNING(W_DECLARATION_IN_FOR_INITIALIZER,(WG_DECL_IN_FOR,WG_EXTENSIONS),WSTATE_WARN,WARNF("for-initializer contains a declaration"))
DEF_WARNING(W_DECL_NESTED_FUNCTION_DECLARATION,(WG_NESTED_FUNCTIONS,WG_EXTENSIONS),WSTATE_WARN,{
 char const *nested_name = KWDNAME();
 DECL_LOAD();
 WARNF("Nested function declarations '%s' in '%s' may not be portable\n",
       nested_name,DECL_NAME());
 DECL_PRINT("See reference to surrounding function");
})
DEF_WARNING(W_TYPE_STRUCT_EMPTY,(WG_EMPTY_STRUCTURES,WG_EXTENSIONS),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Declared empty structure type '%s'\n",DECL_NAME(),KWDNAME());
 DECL_PRINT("See reference to first declaration");
})
DEF_WARNING(W_TYPE_STRUCT_BITFIELD_NEGATIVE,(WG_VALUE),WSTATE_WARN,DECL_PRINTTY_LOAD("Negative value for bit-field '%s'"))
DEF_WARNING(W_TYPE_STRUCT_BITFIELD_SCALAR,(WG_VALUE),WSTATE_WARN,DECL_PRINTTY_LOAD("bit-field '%s' requires scalar type"))
DEF_WARNING(W_TYPE_STRUCT_BITFIELD_LARGER_THAN_BASE,(WG_VALUE),WSTATE_WARN,DECL_PRINTTY_LOAD("bit-field '%s' exceeds size of underlying type"))
DEF_WARNING(W_TYPE_STRUCT_BITFIELD_TOO_LARGE,(WG_VALUE),WSTATE_WARN,DECL_PRINTTY_LOAD("bit-field '%s' is too long"))
DEF_WARNING(W_TYPE_STRUCT_EXPLICIT_ALIGNMENT_TOO_LOW,(WG_VALUE),WSTATE_WARN,{
 target_siz_t used_alignment;
 DECL_LOAD(); used_alignment = ARG(target_siz_t);
 WARNF("Explicit alignment '%lu' of structure type '%s' is too low (Minimum alignment is '%lu')\n",
       (unsigned long)used_alignment,DECL_NAME(),(unsigned long)ARG(target_siz_t));
 DECL_PRINT(NULL);
})
DEF_WARNING(W_OLD_STYLE_FUNCTION_DECLARATION,(WG_OLD_FUNCTION_DECL,WG_EXTENSIONS),WSTATE_WARN,WARNF("An old-style function declarations was used"))
DEF_WARNING(W_MIXED_DECLARATIONS,(WG_MIXED_DECLARATIONS,WG_C99,WG_EXTENSIONS),WSTATE_WARN,
            WARNF("Mixing statements with declarations requires a C99-compliant compiler"))
DEF_WARNING(W_BUILTIN_TYPE_BOOL_C99,(WG_C99,WG_EXTENSIONS),WSTATE_WARN,
            WARNF("Built-in type '_Bool' is only accepted by C99-compliant compilers"))
DEF_WARNING(W_ARRAY_SIZE_ZERO,(WG_ZERO_TYPED_ARRAY,WG_EXTENSIONS),WSTATE_WARN,{
 struct TPPString *tyrepr = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 WARNF("Zero-sized array type '%s'",tyrepr->s_text);
 TPPString_Decref(tyrepr);
})


DEF_WARNING(W_QUALIFIER_ALREADY_IN_USE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("Qualifier " TOK_S " is already being used",TOK_A))
DEF_WARNING(W_ALREADY_AN_LVALUE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("The type is already an l-value"))
DEF_WARNING(W_QUAL_ON_LVALUE,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("Qualifiers on l-value types have no defined semantics"))
DEF_WARNING(W_LVALUE_POINTER,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("Pointer to l-value type has no defined semantics"))
DEF_WARNING(W_AUTO_STORAGE_ALREADY_BY_DEFAULT,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("Using 'auto' to state automatic storage is unnecessary"))
DEF_WARNING(W_RESTRICT_EXPECTS_POINTER,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,WARNF("'restrict' is only allowed for pointer types"))
DEF_WARNING(W_UNKNOWN_FIELD,(WG_TYPE,WG_SYNTAX),WSTATE_WARN,{
 struct TPPString *tyrepr = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 WARNF("Type '%s' has no member '%s'",tyrepr->s_text,KWDNAME());
 TPPString_Decref(tyrepr);
})
DEF_WARNING(W_DEREF_VOID,(WG_TYPE,WG_VALUE),WSTATE_WARN,WARNF("Dereference yields a void/function type"))
DEF_WARNING(W_EXPECTED_EQUALS_AFTER_FIELD_NAME,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected '=' after field name in struct initializer, but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_EQUALS_AFTER_ARRAY_INDEX,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected '=' after index in array initializer, but got " TOK_S,TOK_A))
DEF_WARNING(W_EXPECTED_TYPE_FOR_DECLARATION,(WG_SYNTAX),WSTATE_WARN,WARNF("Assuming 'int' when no type base is given"))
DEF_WARNING(W_EXPECTED_TYPE_FOR_PROTOTYPE_ARGUMENT,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected type for function prototype argument"))
DEF_WARNING(W_EXPECTED_STRING_FOR_ASSEMBLY_NAME,(WG_SYNTAX),WSTATE_WARN,WARNF("Expected string for assembly name, but got " TOK_S,TOK_A))
DEF_WARNING(W_AUTO_USED_AS_STORAGE_CLASS,(WG_TYPE),WSTATE_WARN,WARNF("'auto' used as storage class"))
DEF_WARNING(W_AUTO_TYPE_USED_AS_POINTER_BASE,(WG_TYPE),WSTATE_WARN,WARNF("'__auto_type' used as pointer base"))
DEF_WARNING(W_QUAL_ON_AUTO_TYPE,(WG_TYPE),WSTATE_WARN,WARNF("Qualifiers on '__auto_type' have no defined semantics"))
DEF_WARNING(W_VARIABLE_LENGTH_ARRAYS_NOT_ALLOWED_HERE,(WG_TYPE),WSTATE_WARN,WARNF("VLA array types are not allowed here"))
DEF_WARNING(W_ARRAY_SIZE_DEPENDS_ON_SYMBOL,(WG_VALUE),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Array size depends on symbol '%s'\n",DECL_NAME());
 DECL_PRINT(NULL);
})

DEF_WARNING(W_EXPECTED_TYPE_AFTER_BUILTIN_TYPES_COMPATIBLE_P,(WG_TYPE),WSTATE_WARN,WARNF("Expected type after '__builtin_types_compatible_p'"))
//DEF_WARNING(W_INCOMPATIBLE_ALIAS_TYPES,(WG_TYPE),WSTATE_WARN,WARNF("Aliasing '%s' and '%s' with incompatible types",KWDNAME(),KWDNAME()))
DEF_WARNING(W_ALIAS_WITHOUT_AUTOMATIC_STORAGE,(WG_TYPE),WSTATE_WARN,WARNF("Alias '%s' does not use automatic storage",KWDNAME()))
DEF_WARNING(W_ALIAS_WITH_INITIALIZER,(WG_TYPE),WSTATE_WARN,WARNF("Alias '%s' has an initializer",KWDNAME()))
DEF_WARNING(W_EXTERN_VARIABLE_LOCALLY_INITIALIZED,(WG_TYPE),WSTATE_WARN,WARNF("Extern variable '%s' is being initialized locally",KWDNAME()))
DEF_WARNING(W_DECL_UNNAMED_IMPLIES_STATIC,(WG_LINKER),WSTATE_WARN,WARNF("Unnamed declaration implicitly declared as 'static'"))
DEF_WARNING(W_DECL_NONCONST_IN_RO_SECTION,(WG_LINKER),WSTATE_WARN,{
 struct TPPString *tyrepr; DECL_LOAD();
 tyrepr = DCCType_ToTPPString(DECL_TYPE(),DECL_KWD());
 WARNF("Non-constant declaration '%s' in read-only section '%s'\n",
       tyrepr->s_text,ARG(struct DCCSection *)->sc_start.sy_name->k_name);
 TPPString_Decref(tyrepr);
 DECL_PRINT(NULL);
})
DEF_WARNING(W_DECL_EXPECTED_FUNCTION_TYPE_FOR_CODE_INITIALIZER,(WG_TYPE),WSTATE_WARN,{
 struct TPPString *tyrepr; DECL_LOAD();
 tyrepr = DCCType_ToTPPString(DECL_TYPE(),DECL_KWD());
 WARNF("Expected function type for '%s' with code-initializer\n",
       tyrepr->s_text);
 TPPString_Decref(tyrepr);
 DECL_PRINT(NULL);
})

#ifdef DECLARE_WARNING_MESSAGES
{
 char const *format;
 struct TPPString *ra,*rb;
emit_typ_warning:
 ra = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 rb = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 WARNF(format,ra->s_text,rb->s_text);
 TPPString_Decref(rb);
 TPPString_Decref(ra);
 break;
#endif
#define TYPE_WARNING(msg) { format = msg; goto emit_typ_warning; }
DEF_WARNING(W_INCOMPATIBLE_IMPLEMENTATION_TYPES,(WG_TYPE),WSTATE_WARN,{
 DECL_LOAD();
 ra = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 rb = DCCType_ToTPPString(ARG(struct DCCType *),NULL);
 WARNF("Imcompatible declaration type '%s' and implementation type '%s' for '%s'\n",
       ra->s_text,rb->s_text,DECL_NAME());
 TPPString_Decref(rb);
 TPPString_Decref(ra);
 DECL_PRINT("See reference to previous declaration");
})

DEF_WARNING(W_POINTER_ARITHMETIC_INCOMPATIBLE_DIFF,(WG_POINTER_ARITHMETIC,WG_TYPE),WSTATE_WARN,TYPE_WARNING("Incompatible pointer types '%s' and '%s' used in pointer arithmetic"))
DEF_WARNING(W_ASSIGN_CONSTANT_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Assignment of '%s' to constant type '%s'"))
DEF_WARNING(W_ASSIGN_RVALUE_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Assignment of '%s' to r-value of type '%s'"))
DEF_WARNING(W_BINARY_CONSTANT_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Binary operation with '%s' on constant type '%s'"))
DEF_WARNING(W_BINARY_RVALUE_TYPE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Binary operation with '%s' on r-value of type '%s'"))
DEF_WARNING(W_CAST_INCOMPATIBLE_TYPES,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Cast between incompatible types '%s' and '%s'"))
DEF_WARNING(W_CAST_INTEGRAL_OVERFLOW,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from '%s' to '%s' causes an overflow"))
DEF_WARNING(W_CAST_INTEGRAL_MAYOVERFLOW,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from '%s' to '%s' may cause an overflow"))
DEF_WARNING(W_CAST_INTEGRAL_SIGNLOSS,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from '%s' to '%s' causes the sign to be lost"))
DEF_WARNING(W_CAST_INTEGRAL_MAYSIGNLOSS,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from '%s' to '%s' may cause the sign to be lost"))
DEF_WARNING(W_CAST_FLOAT_TO_INT,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from floating-point-type '%s' to integer-type '%s'"))
DEF_WARNING(W_CAST_FLOAT_DOWNCAST,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit downcast of floating-point-type '%s' to '%s'"))
DEF_WARNING(W_CAST_INCOMPATIBLE_POINTERS,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast between incompatible pointer types '%s' and '%s'"))
DEF_WARNING(W_CAST_INCOMPATIBLE_POINTERS_VOID,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast between void-pointer-type '%s' and non-void-pointer-type '%s'"))
DEF_WARNING(W_CAST_TO_POINTER,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from incompatible type '%s' to pointer-type '%s'"))
DEF_WARNING(W_CAST_FLOAT_TO_POINTER,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Casting floating-point-type '%s' to pointer-type '%s'"))
DEF_WARNING(W_CAST_INT_TO_POINTER,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from integer-type '%s' to pointer-type '%s'"))
DEF_WARNING(W_CAST_INT_TO_POINTER_SIZ,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from integer-type '%s' to pointer-type '%s' of different size"))
DEF_WARNING(W_CAST_POINTER_TO_FLOAT,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from pointer-type '%s' to floating-point-type '%s'"))
DEF_WARNING(W_CAST_POINTER_TO_INT,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from pointer-type '%s' to integer-type '%s'"))
DEF_WARNING(W_CAST_POINTER_TO_INT_SIZ,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from pointer-type '%s' to integer-type '%s' of different size"))
DEF_WARNING(W_CAST_CONST_POINTER,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from constant pointer-type '%s' to non-constant pointer-type '%s'"))
DEF_WARNING(W_CAST_CONST_LVALUE,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from constant lvalue-type '%s' to non-constant lvalue-type '%s'"))
DEF_WARNING(W_CAST_VOLATILE_POINTER,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from volatile pointer-type '%s' to non-volatile pointer-type '%s'"))
DEF_WARNING(W_CAST_RVALUE_TO_LVALUE,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from r-value '%s' to l-value '%s'"))
DEF_WARNING(W_CAST_INCOMPATIBLE_LVALUE,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Implicit cast from '%s' to incompatible l-value-type '%s'"))
DEF_WARNING(W_CAST_TO_FUNCTION,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Incompatible function types '%s' and '%s'"))
DEF_WARNING(W_CAST_TO_ARRAY,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Incompatible array types '%s' and '%s'"))
DEF_WARNING(W_CAST_TO_VARRAY,(WG_CAST),WSTATE_WARN,TYPE_WARNING("Incompatible variadic array types '%s' and '%s'"))
DEF_WARNING(W_INCOMPATIBLE_TYPES_FOR_VARRAY_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Incompatible types '%s' and '%s' for variadic-array initializer"))
DEF_WARNING(W_EXPECTED_ARRAY_FOR_VARRAY_INITIALIZER,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Expected an array type for the initializer of variadic-array-type '%s', but got '%s'"))
DEF_WARNING(W_UNSUPPORTED_CAS_SIZE,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Type '%s' has an unsupported CAS size"))
DEF_WARNING(W_ASSIGN_VOID,(WG_TYPE),WSTATE_WARN,TYPE_WARNING("Can't assign non-void-type '%s' to void-type '%s'"))
DEF_WARNING(W_ASSIGN_VOID_VOID,(WG_ASSIGN_VOID_VOID,WG_EXTENSIONS,WG_TYPE),WSTATE_WARN,TYPE_WARNING("Assigning void-type '%s' to void-type '%s'"))
#undef TYPE_WARNING
#ifdef DECLARE_WARNING_MESSAGES
}
#endif

/* Switch to the Linker warning namespace. */
WARNING_NAMESPACE(WN_LINKER,2000)
DEF_WARNING(W_OUT_OF_MEMORY,(WG_LINKER),WSTATE_ERROR,WARNF("Out of memory when allocating '%lu' bytes",(unsigned long)ARG(size_t)))
DEF_WARNING(W_LINKER_CANNOT_RELOCATE_SYMPLUSSYM,(WG_LINKER),WSTATE_WARN,WARNF("Symbol+Symbol expression cannot be relocated"))
DEF_WARNING(W_LINKER_CANNOT_RELOCATE_SYMMINUSSYM,(WG_LINKER),WSTATE_WARN,WARNF("Symbol-Symbol expressions can only be relocated when both symbols are declared and exist in the same section"))
DEF_WARNING(W_MISSING_ENTRY_POINT,(WG_LINKER,WG_USAGE),WSTATE_WARN,WARNF("Missing entry point '%s' (Using start of default .text section)",ARG(char *)))
DEF_WARNING(W_SYMBOL_ALREADY_DEFINED,(WG_LINKER),WSTATE_WARN,
            WARNF("Symbol '%s' was already defined\n",ARG(char *)))
DEF_WARNING(W_UNRESOLVED_REFERENCE,(WG_LINKER,WG_USAGE),WSTATE_ERROR,{
 char *kwdname = KWDNAME();
 char *secname = KWDNAME();
 target_ptr_t offset = ARG(target_ptr_t);
 WARNF("Unresolved reference to '%s' in section '%s'+%lu",
       kwdname,secname,(unsigned long)offset);
})
DEF_WARNING(W_JMP_TARGET_TRUNCATED,(WG_LINKER),WSTATE_WARN,WARNF("jmp target was truncated to fit"))
DEF_WARNING(W_ALLOC_OBJECT_IN_TEXT,(WG_LINKER),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Declaration '%s' allocated inside assembly of section '%s'\n",
       DECL_NAME(),ARG(struct DCCSection *)->sc_start.sy_name->k_name);
 DECL_PRINT("See reference to declaration");
})
DEF_WARNING(W_FUNCTION_EXIT_IN_DIFFERENT_SECTION,(WG_LINKER),WSTATE_WARN,{
 struct DCCSection *start;
 struct DCCSection *end;
 DECL_LOAD();
 start = ARG(struct DCCSection *);
 end = ARG(struct DCCSection *);
 WARNF("Function '%s' ends in section '%s', but begins in '%s'",
       DECL_NAME(),
       start->sc_start.sy_name->k_name,
       end->sc_start.sy_name->k_name);
 DECL_PRINT("See reference to function");
})
DEF_WARNING(W_DECL_FUNCTION_SECTION_NOT_EXECUTABLE,(WG_LINKER),WSTATE_WARN,{
 struct DCCSection *sec;
 DECL_LOAD(); sec = ARG(struct DCCSection *);
 WARNF("Section '%s' for function '%s' is not executable",
       sec->sc_start.sy_name->k_name,DECL_NAME()); 
 DECL_PRINT("See reference to function");
})
DEF_WARNING(W_INVALID_FRAME_INDIRECTION,(WG_LINKER),WSTATE_WARN,WARNF("Invalid frame indirection"))
DEF_WARNING(W_COMPILER_FUNCTION_NOT_DEFINED_AS_LABEL,(WG_LINKER),WSTATE_WARN,{
 DECL_LOAD();
 WARNF("Compiler-internal function '%s' was defined as non-addressable",DECL_NAME());
 DECL_PRINT(NULL);
})
DEF_WARNING(W_INVALID_PE_SYMBOL_LINKAGE,(WG_LINKER),WSTATE_WARN,
            WARNF("PE import symbol '%s' is liked without indirection",KWDNAME()))
DEF_WARNING(W_LINKER_CANT_MERGE_NONIMPORT_WITH_IMPORT,(WG_LINKER),WSTATE_WARN,{ char *name = KWDNAME(); WARNF("Can't merge non-import-section '%s' with import-section '%s'",name,KWDNAME()); })
DEF_WARNING(W_LINKER_CANT_MERGE_IMPORT_WITH_NONIMPORT,(WG_LINKER),WSTATE_WARN,{ char *name = KWDNAME(); WARNF("Can't merge import-section '%s' with non-import-section '%s'",name,KWDNAME()); })
DEF_WARNING(W_LINKER_CANT_RELOC_LIB_SECTION,(WG_LINKER),WSTATE_WARN,
            WARNF("Can't add relocations to library section '%s'",KWDNAME()))
#if DCC_TARGET_BIN == DCC_BINARY_PE
DEF_WARNING(W_LINKER_PE_DLLEXPORT_NEVER_DEFINED,(WG_LINKER),WSTATE_WARN,
            WARNF("Symbol '%s' marked for dllexport was never defined and is not exported",KWDNAME()))
DEF_WARNING(W_LINKER_PE_WEAKSYM_EXPORTED_AS_NORMAL,(WG_LINKER),WSTATE_WARN,
            WARNF("Weak symbol '%s' is exported as a normal declaration on PE targets",KWDNAME()))
DEF_WARNING(W_LINKER_PE_CANT_EXPORT_EMPTY_SECTION,(WG_LINKER),WSTATE_WARN,
            WARNF("Can't export empty section '%s' marked with dllexport on PE targets",KWDNAME()))
#endif

/* Switch to the lib-loader warning namespace. */
WARNING_NAMESPACE(WN_LIBLOADER,3000)

/* Library loader warnings. */
DEF_WARNING(W_LIB_NOT_FOUND,(WG_LIBLOAD),WSTATE_ERROR,WARNF("Library not found: '%s'",ARG(char *)))
DEF_WARNING(W_LIB_PE_INVMAGIC,(WG_LIBLOAD),WSTATE_ERROR,WARNF("Invalid header magic in PE library '%s'",ARG(char *)))
DEF_WARNING(W_LIB_PE_NO_DLL,(WG_QUALITY,WG_LIBLOAD),WSTATE_WARN,WARNF("Library '%s' is not a dll.",ARG(char *)))
DEF_WARNING(W_LIB_PE_NO_RELOCATIONS,(WG_LIBLOAD),WSTATE_WARN,WARNF("Can't link against PE library '%s' without relocations",ARG(char *)))
DEF_WARNING(W_LIB_PE_NO_EXPORT_TABLE,(WG_LIBLOAD),WSTATE_ERROR,WARNF("PE binary '%s' has no export table",ARG(char *)))
DEF_WARNING(W_LIB_PE_NO_SECTIONS,(WG_LIBLOAD),WSTATE_ERROR,WARNF("PE binary '%s' has no sections",ARG(char *)))
DEF_WARNING(W_LIB_PE_NO_SECTION_MAPPING,(WG_LIBLOAD),WSTATE_ERROR,{ char *name = ARG(char *); WARNF("No section of PE binary '%s' maps to virtual address %p",name,ARG(void *)); })
DEF_WARNING(W_LIB_PE_EMPTY_EXPORT_TABLE,(WG_LIBLOAD),WSTATE_ERROR,WARNF("PE binary '%s' has an empty export table",ARG(char *)))

#undef DECL_PRINTTY_LOAD
#undef DECL_PRINTTY
#undef DECL_PRINT
#undef DECL_NAME
#undef DECL_LOAD
#ifdef DECLARE_WARNING_MESSAGES
}
#endif

