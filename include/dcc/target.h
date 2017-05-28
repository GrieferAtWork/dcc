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
#ifndef GUARD_DCC_TARGET_H
#define GUARD_DCC_TARGET_H 1

#include "common.h"

#include <stdint.h>

DCC_DECL_BEGIN

/* Known CPU names. */
#define DCC_CPU_UNKNOWN 0
#define DCC_CPU_I386    1
#define DCC_CPU_I486    2
#define DCC_CPU_I586    3
#define DCC_CPU_I686    4
#define DCC_CPU_X86_64  5

/* Known OS names. */
#define DCC_OS_UNKNOWN         0x00
#define DCC_OS_WINDOWS         0x01
#define DCC_OS_UNIX            0x02
#define DCC_OS_LINUX          (DCC_OS_UNIX)
#define DCC_OS_FREEBSD        (0x10|DCC_OS_UNIX)
#define DCC_OS_FREEBSD_KERNEL (0x20|DCC_OS_FREEBSD|DCC_OS_UNIX)
#define DCC_OS_GENERIC         DCC_OS_UNIX /* If you don't know that it is, it's probably based on unix. */

/* Known BINARY output format names.
 * NOTE: Does not affect recognized input formats, which
 *       are configured below with 'DCC_LIBFORMAT_*'. */
#define DCC_BINARY_UNKNOWN 0x00
#define DCC_BINARY_PE      0x01
#define DCC_BINARY_ELF     0x02

/* DCC Master CPU target switch. */
#ifndef DCC_TARGET_CPU
#define DCC_TARGET_CPU     DCC_HOST_CPU
#endif

/* DCC Master Binary target format switch. */
#ifndef DCC_TARGET_BIN
#define DCC_TARGET_BIN     DCC_HOST_BIN
//#define DCC_TARGET_BIN     DCC_BINARY_ELF
#endif

/* DCC Master OS target switch. */
#ifndef DCC_TARGET_OS
#define DCC_TARGET_OS      DCC_HOST_OS
#endif


/* Recognized library formats during linking. */
#define DCC_LIBFORMAT_DEF_DYNAMIC 1 /* Recognize *.def files as dynamic libraries. */

#define DCC_LIBFORMAT_PE_DYNAMIC  1 /* Recognize PE binaries as dynamic libraries. */
#define DCC_LIBFORMAT_PE_STATIC   1 /* Recognize PE binaries as static libraries. */

#define DCC_LIBFORMAT_ELF         1 /* Recognize ELF binaries as dynamic/static libraries. */
#define DCC_LIBFORMAT_ELF_STATIC  1 /* Allow the DCC to attempt to statically link against ELF binaries,
                                     * even when base addresses have already been assigned.
                                     * Similar to how this is accomplished with 'DCC_LIBFORMAT_PE_STATIC',
                                     * this will require DCC to decompile executable code sections to
                                     * generate missing DISP relocations (which may not always be correct)
                                     * while assuming the presence of relocations required for position-independent
                                     * code (such as generated when '-fPIC' is passed on the commandline). */

#define DCC_LIBFORMAT_ARCH        1 /* Recognize '!<arch>' archives. */






/* Figure out the host configuration. */
#ifndef DCC_HOST_CPU
#if defined(__amd64__) || defined(__amd64) || \
    defined(__x86_64__) || defined(__x86_64) || \
    defined(_M_X64) || defined(_M_AMD64)
#   define DCC_HOST_CPU DCC_CPU_X86_64
#elif defined(__I86__)
#if __I86__ >= 6
#   define DCC_HOST_CPU DCC_CPU_I686
#elif __I86__ >= 5
#   define DCC_HOST_CPU DCC_CPU_I586
#elif __I86__ >= 4
#   define DCC_HOST_CPU DCC_CPU_I486
#else
#   define DCC_HOST_CPU DCC_CPU_I386
#endif
#elif defined(_M_IX86)
#if _M_IX86 >= 600
#   define DCC_HOST_CPU DCC_CPU_I686
#elif _M_IX86 >= 500
#   define DCC_HOST_CPU DCC_CPU_I586
#elif _M_IX86 >= 400
#   define DCC_HOST_CPU DCC_CPU_I486
#else
#   define DCC_HOST_CPU DCC_CPU_I386
#endif
#elif defined(__i686__) || defined(__i686) || defined(i686)
#   define DCC_HOST_CPU DCC_CPU_I686
#elif defined(__i586__) || defined(__i586) || defined(i586)
#   define DCC_HOST_CPU DCC_CPU_I586
#elif defined(__i486__) || defined(__i486) || defined(i486)
#   define DCC_HOST_CPU DCC_CPU_I486
#elif defined(__i386__) || defined(__i386) || defined(i386) || \
      defined(__X86__) || defined(_X86_) || \
      defined(__THW_INTEL__) || defined(__INTEL__)
#   define DCC_HOST_CPU DCC_CPU_I386
#endif
#ifndef DCC_HOST_CPU
#   define DCC_HOST_CPU DCC_CPU_UNKNOWN
#endif
#endif /* !DCC_HOST_CPU */

#ifndef DCC_HOST_OS
#if defined(_WIN16) || defined(WIN16) || \
    defined(_WIN32) || defined(WIN32) || \
    defined(_WIN64) || defined(WIN64) || \
    defined(__WIN32__) || defined(__TOS_WIN__) || \
    defined(_WIN32_WCE) || defined(WIN32_WCE)
#   define DCC_HOST_OS DCC_OS_WINDOWS
#elif defined(__FreeBSD_kernel__)
#   define DCC_HOST_OS DCC_OS_FREEBSD_KERNEL
#elif defined(__FreeBSD__)
#   define DCC_HOST_OS DCC_OS_FREEBSD
#elif defined(__linux__) || \
      defined(__linux) || defined(linux)
#   define DCC_HOST_OS DCC_OS_LINUX
#elif defined(__unix__) || defined(__unix) \
      defined(unix) || defined(__ANDROID__) || \
      defined(__ANDROID) || defined(__android__) || \
      defined(__android) || defined(__MACOS__) || \
      defined(__MACOSX__) || defined(__POSIX__)
#   define DCC_HOST_OS DCC_OS_UNIX
#else
#   define DCC_HOST_OS DCC_OS_UNKNOWN
#endif
#endif /* !DCC_HOST_OS */

#ifndef DCC_HOST_BIN
#ifdef __ELF__
#   define DCC_HOST_BIN    DCC_BINARY_ELF
#elif defined(__PE__)
#   define DCC_HOST_BIN    DCC_BINARY_PE
#elif DCC_HOST_OS == DCC_OS_WINDOWS
#   define DCC_HOST_BIN    DCC_BINARY_PE
#elif DCC_HOST_OS == DCC_OS_UNIX || \
      DCC_HOST_OS == DCC_OS_FREEBSD || \
      DCC_HOST_OS == DCC_OS_FREEBSD_KERNEL
#   define DCC_HOST_BIN    DCC_BINARY_ELF
#else
#   define DCC_HOST_BIN    DCC_BINARY_UNKNOWN
#endif
#endif /* !DCC_HOST_BIN */













#if DCC_TARGET_CPU == DCC_CPU_I386
#   define DCC_TARGET_IA32_VERSION  386
#elif DCC_TARGET_CPU == DCC_CPU_I486
#   define DCC_TARGET_IA32_VERSION  486
#elif DCC_TARGET_CPU == DCC_CPU_I586
#   define DCC_TARGET_IA32_VERSION  586
#elif DCC_TARGET_CPU == DCC_CPU_I686
#   define DCC_TARGET_IA32_VERSION  686
#endif

#ifdef DCC_TARGET_IA32_VERSION
#   define DCC_TARGET_IA32(v)  ((v) <= DCC_TARGET_IA32_VERSION)
#else
#   define DCC_TARGET_IA32(v)    0
#endif
#if DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_TARGET_IA64(v)    1
#else
#   define DCC_TARGET_IA64(v)    0
#endif

#if DCC_TARGET_IA32(0) || (DCC_TARGET_CPU == DCC_CPU_X86_64)
#   define DCC_TARGET_X86    1
#endif

#ifndef DCC_TARGET_ELFINTERP
/* Determine the apropriate target ELF interpreter name. */
#if DCC_TARGET_BIN == DCC_BINARY_ELF
#if DCC_TARGET_OS == DCC_OS_FREEBSD
#   define DCC_TARGET_ELFINTERP "/libexec/ld-elf.so.1"
#elif DCC_TARGET_OS == DCC_OS_FREEBSD_KERNEL
#   define DCC_TARGET_ELFINTERP "/lib/ld.so.1"
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_TARGET_ELFINTERP "/lib64/ld-linux-x86-64.so.2"
#else
#   define DCC_TARGET_ELFINTERP "/lib/ld-linux.so.2"
#endif
#endif
#endif


#   define DCC_TARGET_STACKDOWN        1 /* 0/1 indicating stack growth direction. */
#   define DCC_TARGET_BYTEORDER        1234
#   define DCC_TARGET_FLOAT_WORD_ORDER 1234
#if DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_TARGET_SIZEOF_POINTER   8
#else
#   define DCC_TARGET_SIZEOF_POINTER   4
#endif

#   define DCC_TARGET_SIZEOF_BOOL          1
#   define DCC_TARGET_SIZEOF_CHAR          1
#   define DCC_TARGET_SIZEOF_SHORT         2
#   define DCC_TARGET_SIZEOF_INT           4
#   define DCC_TARGET_SIZEOF_LONG          DCC_TARGET_SIZEOF_POINTER
#   define DCC_TARGET_SIZEOF_LONG_LONG     8
#   define DCC_TARGET_SIZEOF_FLOAT         4
#   define DCC_TARGET_SIZEOF_DOUBLE        8
#   define DCC_TARGET_SIZEOF_LONG_DOUBLE   12
#   define DCC_TARGET_SIZEOF_SIZE_T        DCC_TARGET_SIZEOF_POINTER
#if DCC_TARGET_OS == DCC_OS_WINDOWS
#   define DCC_TARGET_SIZEOF_WCHAR_T       2
#else
#   define DCC_TARGET_SIZEOF_WCHAR_T       4
#endif
#   define DCC_TARGET_SIZEOF_WINT_T        DCC_TARGET_SIZEOF_INT
#   define DCC_TARGET_SIZEOF_PTRDIFF_T     DCC_TARGET_SIZEOF_POINTER
#   define DCC_TARGET_SIZEOF_SIG_ATOMIC_T  DCC_TARGET_SIZEOF_INT
#   define DCC_TARGET_SIZEOF_INT_LEAST8_T  1
#   define DCC_TARGET_SIZEOF_INT_LEAST16_T 2
#   define DCC_TARGET_SIZEOF_INT_LEAST32_T 4
#   define DCC_TARGET_SIZEOF_INT_LEAST64_T 8
#   define DCC_TARGET_SIZEOF_INT_FAST8_T   1
#ifdef DCC_TARGET_X86
/* Real 16-bit usually requires the D16 prefix,
 * so 32-bit is actually faster most of the time. */
#   define DCC_TARGET_SIZEOF_INT_FAST16_T  4
#else
#   define DCC_TARGET_SIZEOF_INT_FAST16_T  2
#endif
#   define DCC_TARGET_SIZEOF_INT_FAST32_T  4
#   define DCC_TARGET_SIZEOF_INT_FAST64_T  8
#   define DCC_TARGET_SIZEOF_INTMAX_T      8
#   define DCC_TARGET_BITPERBYTE           8
#   define DCC_TARGET_PAGESIZE          4096

/* Default stack-alignment of normal arguments. */
#   define DCC_TARGET_STACKALIGN           DCC_TARGET_SIZEOF_INT

/* Stack-alignment of variadic arguments. */
#   define DCC_TARGET_VA_ALIGN             DCC_TARGET_SIZEOF_POINTER


#   define DCC_TARGET_TLSMODE_NONE         0 /*< TLS storage is not supported. */
#   define DCC_TARGET_TLSMODE_NATIVE       1 /*< TLS storage is supported natively (CPU-specific) (TODO: Missing) */
#   define DCC_TARGET_TLSMODE_EMULATED     2 /*< TLS storage is emulated with extern function calls (TODO: Missing) */
#   define DCC_TARGET_TLS                  DCC_TARGET_TLSMODE_NONE


#ifdef DCC_TARGET_X86
/* 6: %EBX, %EDI, %ESI, %ESP, %EBP, %EIP */
#if DCC_TARGET_SIZEOF_POINTER == 4
#   define DCC_TARGET_SIZEOF_JMP_BUF       32
#elif DCC_TARGET_SIZEOF_POINTER == 8
#   define DCC_TARGET_SIZEOF_JMP_BUF       64
#elif defined(__TPP_EVAL)
#   define DCC_TARGET_SIZEOF_JMP_BUF      __TPP_EVAL(DCC_TARGET_SIZEOF_POINTER*6)
#else
#   define DCC_TARGET_SIZEOF_JMP_BUF      (DCC_TARGET_SIZEOF_POINTER*6)
#endif
#else
#   error FIXME
#endif


#define DCC_TARGET_ATOMIC_RELAXED 0
#define DCC_TARGET_ATOMIC_CONSUME 1
#define DCC_TARGET_ATOMIC_ACQUIRE 2
#define DCC_TARGET_ATOMIC_RELEASE 3
#define DCC_TARGET_ATOMIC_ACQ_REL 4
#define DCC_TARGET_ATOMIC_SEQ_CST 5


#define DCC_TARGET_TYPE_S1   "signed char"
#define DCC_TARGET_TYPE_S2   "short"
#define DCC_TARGET_TYPE_S4   "int"
#define DCC_TARGET_TYPE_S8   "long long"
#define DCC_TARGET_TYPE_U1   "unsigned char"
#define DCC_TARGET_TYPE_U2   "unsigned short"
#define DCC_TARGET_TYPE_U4   "unsigned int"
#define DCC_TARGET_TYPE_U8   "unsigned long long"
#define DCC_TARGET_TYPE_S(b) DCC_PRIVATE_PP_CAT(DCC_TARGET_TYPE_S,b)
#define DCC_TARGET_TYPE_U(b) DCC_PRIVATE_PP_CAT(DCC_TARGET_TYPE_U,b)

#define DCC_PRIVATE_MUL8_0  0
#define DCC_PRIVATE_MUL8_1  8
#define DCC_PRIVATE_MUL8_2  16
#define DCC_PRIVATE_MUL8_4  32
#define DCC_PRIVATE_MUL8_8  64
#define DCC_PRIVATE_MUL8_16 128
#define DCC_PRIVATE_MUL8(x) DCC_PRIVATE_MUL8_##x
#define DCC_MUL8(x) DCC_PRIVATE_MUL8(x)

#define DCC_TARGET_MIN_S1   "(-127-1)"
#define DCC_TARGET_MIN_S2   "(-32767-1)"
#define DCC_TARGET_MIN_S4   "(-2147483647-1)"
#if DCC_TARGET_SIZEOF_LONG == 8
#define DCC_TARGET_MIN_S8   "(-9223372036854775807l-1)"
#else
#define DCC_TARGET_MIN_S8   "(-9223372036854775807ll-1)"
#endif
#define DCC_TARGET_MIN_S(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MIN_S,b)

#define DCC_TARGET_MAX_S1    "127"
#define DCC_TARGET_MAX_S2    "32767"
#define DCC_TARGET_MAX_S4    "2147483647"
#if DCC_TARGET_SIZEOF_LONG == 8
#define DCC_TARGET_MAX_S8    "9223372036854775807l"
#else
#define DCC_TARGET_MAX_S8    "9223372036854775807ll"
#endif
#define DCC_TARGET_MAX_S(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MAX_S,b)

#define DCC_TARGET_MAX_U1    "0xffu"
#define DCC_TARGET_MAX_U2    "0xffffu"
#define DCC_TARGET_MAX_U4    "0xffffffffu"
#if DCC_TARGET_SIZEOF_LONG == 8
#define DCC_TARGET_MAX_U8    "0xfffffffffffffffful"
#else
#define DCC_TARGET_MAX_U8    "0xffffffffffffffffull"
#endif
#define DCC_TARGET_MAX_U(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MAX_U,b)


#if DCC_TARGET_SIZEOF_POINTER == 1
typedef  int8_t  DCC(target_off_t);
typedef uint8_t  DCC(target_ptr_t);
#elif DCC_TARGET_SIZEOF_POINTER == 2
typedef  int16_t DCC(target_off_t);
typedef uint16_t DCC(target_ptr_t);
#elif DCC_TARGET_SIZEOF_POINTER == 4
typedef  int32_t DCC(target_off_t);
typedef uint32_t DCC(target_ptr_t);
#elif DCC_TARGET_SIZEOF_POINTER == 8
typedef  int64_t DCC(target_off_t);
typedef uint64_t DCC(target_ptr_t);
#else
#   error FIXME
#endif
typedef DCC(target_ptr_t) DCC(target_siz_t);


/* Register class/descriptor:
 * One of 'DCC_RC_*', potentially or'd together with one of 'DCC_ASMREG_*'. */
typedef uint16_t DCC(rc_t);



DCC_DECL_END

#endif /* !GUARD_DCC_TARGET_H */
