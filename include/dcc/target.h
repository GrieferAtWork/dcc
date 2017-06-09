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
#define DCC_CPU_UNKNOWN  0x00000000
#define DCC_CPU_F_INTEL  0x00000001
#define DCC_CPU_F_AMD    0x00000002
/*                       0x0000fffc */
#define DCC_CPU_I386    (0x00010000|DCC_CPU_F_INTEL)
#define DCC_CPU_I486    (0x00020000|DCC_CPU_F_INTEL)
#define DCC_CPU_I586    (0x00030000|DCC_CPU_F_INTEL)
#define DCC_CPU_I686    (0x00040000|DCC_CPU_F_INTEL)
#define DCC_CPU_X86_64  (0x00050000|DCC_CPU_F_INTEL|DCC_CPU_F_AMD)

/* Known OS names. */
#define DCC_OS_UNKNOWN         0x00000000
#define DCC_OS_F_UNIX          0x00000001
#define DCC_OS_F_KERNEL        0x00000002
#define DCC_OS_F_WINDOWS       0x00000004
/*                             0x0000fff8 */
#define DCC_OS_GENERIC        (0x00000000)
#define DCC_OS_GENERIC_UNIX   (0x00000000|DCC_OS_F_UNIX) /* If you don't know that it is, it's probably based on unix. */
#define DCC_OS_WINDOWS        (0x00010000|DCC_OS_F_WINDOWS)
#define DCC_OS_CYGWIN         (0x00020000|DCC_OS_F_WINDOWS)
#define DCC_OS_LINUX          (0x00030000|DCC_OS_F_UNIX)
#define DCC_OS_FREEBSD        (0x00040000|DCC_OS_F_UNIX)
#define DCC_OS_FREEBSD_KERNEL (0x00040000|DCC_OS_F_UNIX|DCC_OS_F_KERNEL)
#define DCC_OS_KOS            (0x00050000|DCC_OS_F_UNIX)

/* Known BINARY output format names.
 * NOTE: Does not affect recognized input formats, which
 *       are configured below with 'DCC_LIBFORMAT_*'. */
/*                         0x0000ffff */
#define DCC_BINARY_UNKNOWN 0x00000000
#define DCC_BINARY_PE      0x00010000
#define DCC_BINARY_ELF     0x00020000

/* DCC Master CPU target switch. */
#ifndef DCC_TARGET_CPU
#define DCC_TARGET_CPU     DCC_CPU_I686
//#define DCC_TARGET_CPU     DCC_HOST_CPU
#endif

/* DCC Master Binary target format switch. */
#ifndef DCC_TARGET_BIN
#define DCC_TARGET_BIN     DCC_HOST_BIN
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
    defined(_WIN32_WCE) || defined(WIN32_WCE) || \
    defined(__MINGW32__)
#   define DCC_HOST_OS DCC_OS_WINDOWS
#elif defined(__CYGWIN__) || defined(__CYGWIN32__)
#   define DCC_HOST_OS DCC_OS_CYGWIN
#elif defined(__FreeBSD_kernel__)
#   define DCC_HOST_OS DCC_OS_FREEBSD_KERNEL
#elif defined(__FreeBSD__)
#   define DCC_HOST_OS DCC_OS_FREEBSD
#elif defined(__KOS__)
#   define DCC_HOST_OS DCC_OS_KOS
#elif defined(__linux__) || \
      defined(__linux) || defined(linux)
#   define DCC_HOST_OS DCC_OS_LINUX
#elif defined(__unix__) || defined(__unix) || \
      defined(unix) || defined(__ANDROID__) || \
      defined(__ANDROID) || defined(__android__) || \
      defined(__android) || defined(__MACOS__) || \
      defined(__MACOSX__) || defined(__POSIX__)
#   define DCC_HOST_OS DCC_OS_GENERIC_UNIX
#else
#   define DCC_HOST_OS DCC_OS_GENERIC
#endif
#endif /* !DCC_HOST_OS */

#ifndef DCC_HOST_BIN
#ifdef __ELF__
#   define DCC_HOST_BIN    DCC_BINARY_ELF
#elif defined(__PE__)
#   define DCC_HOST_BIN    DCC_BINARY_PE
#elif !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
#   define DCC_HOST_BIN    DCC_BINARY_PE
#elif DCC_HOST_OS == DCC_OS_F_UNIX || \
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
#if DCC_TARGET_OS == DCC_OS_KOS
/* KOS implements the runtime linker in
 * kernel-space, meaning no interpreter
 * is necessary. */
/* #define DCC_TARGET_ELFINTERP <NOTHING> */
#elif DCC_TARGET_OS == DCC_OS_FREEBSD
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


#   define DCC_TARGET_STACKDOWN            1 /* 0/1 indicating stack growth direction. */
#   define DCC_TARGET_BYTEORDER            1234
#   define DCC_TARGET_FLOAT_WORD_ORDER     1234
#if DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_TARGET_SIZEOF_POINTER       8
#else
#   define DCC_TARGET_SIZEOF_POINTER       4
#endif
#   define DCC_TARGET_SIZEOF_GP_REGISTER   DCC_TARGET_SIZEOF_POINTER /* Greatest size of a general-purpose register. */
#   define DCC_TARGET_SIZEOF_ARITH_MAX     DCC_TARGET_SIZEOF_POINTER /* Size of the a greatest-width arithmetic operation. */
#   define DCC_TARGET_SIZEOF_IMM_MAX       DCC_TARGET_SIZEOF_POINTER /* Size of the greatest possible immediate value in assembly. */
#   define DCC_TARGET_SIZEOF_BYTE          1
#   define DCC_TARGET_SIZEOF_WORD          2
#   define DCC_TARGET_SIZEOF_INT           4
#   define DCC_TARGET_SIZEOF_BOOL          DCC_TARGET_SIZEOF_BYTE
#   define DCC_TARGET_SIZEOF_CHAR          DCC_TARGET_SIZEOF_BYTE
#   define DCC_TARGET_SIZEOF_SHORT         DCC_TARGET_SIZEOF_WORD
#if DCC_TARGET_OS == DCC_OS_WINDOWS
#   define DCC_TARGET_SIZEOF_LONG          4
#else
#   define DCC_TARGET_SIZEOF_LONG          DCC_TARGET_SIZEOF_POINTER
#endif
#   define DCC_TARGET_SIZEOF_LONG_LONG     8
#   define DCC_TARGET_SIZEOF_FLOAT         4
#   define DCC_TARGET_ALIGNOF_FLOAT        4
#   define DCC_TARGET_SIZEOF_DOUBLE        8
#if DCC_TARGET_IA32(386)
#if DCC_TARGET_BIN == DCC_BINARY_PE
#   define DCC_TARGET_ALIGNOF_LONG_LONG    8
#   define DCC_TARGET_ALIGNOF_DOUBLE       8
#else
#   define DCC_TARGET_ALIGNOF_LONG_LONG    4
#   define DCC_TARGET_ALIGNOF_DOUBLE       4
#endif
#else
#   define DCC_TARGET_ALIGNOF_LONG_LONG    8
#   define DCC_TARGET_ALIGNOF_DOUBLE       8
#endif
#if DCC_TARGET_IA32(386)
#   define DCC_TARGET_SIZEOF_LONG_DOUBLE   12
#   define DCC_TARGET_ALIGNOF_LONG_DOUBLE  4
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_TARGET_SIZEOF_LONG_DOUBLE   16
#   define DCC_TARGET_ALIGNOF_LONG_DOUBLE  8
#else
#   error FIXME
#endif
#   define DCC_TARGET_SIZEOF_SIZE_T        DCC_TARGET_SIZEOF_POINTER
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
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


#   define DCC_TARGET_SIZEOF_MAXALIGN_T  DCC_TARGET_SIZEOF_LONG_DOUBLE
#   define DCC_TARGET_ALIGNOF_MAXALIGN_T DCC_TARGET_ALIGNOF_LONG_DOUBLE
#if DCC_TARGET_ALIGNOF_DOUBLE > DCC_TARGET_ALIGNOF_MAXALIGN_T
#   undef DCC_TARGET_SIZEOF_MAXALIGN_T
#   undef DCC_TARGET_ALIGNOF_MAXALIGN_T
#   define DCC_TARGET_SIZEOF_MAXALIGN_T  DCC_TARGET_SIZEOF_DOUBLE
#   define DCC_TARGET_ALIGNOF_MAXALIGN_T DCC_TARGET_ALIGNOF_DOUBLE
#endif
#if DCC_TARGET_ALIGNOF_LONG_LONG > DCC_TARGET_ALIGNOF_MAXALIGN_T
#   undef DCC_TARGET_SIZEOF_MAXALIGN_T
#   undef DCC_TARGET_ALIGNOF_MAXALIGN_T
#   define DCC_TARGET_SIZEOF_MAXALIGN_T  DCC_TARGET_SIZEOF_LONG_LONG
#   define DCC_TARGET_ALIGNOF_MAXALIGN_T DCC_TARGET_ALIGNOF_LONG_LONG
#endif
#if DCC_TARGET_ALIGNOF_FLOAT > DCC_TARGET_ALIGNOF_MAXALIGN_T
#   undef DCC_TARGET_SIZEOF_MAXALIGN_T
#   undef DCC_TARGET_ALIGNOF_MAXALIGN_T
#   define DCC_TARGET_SIZEOF_MAXALIGN_T  DCC_TARGET_SIZEOF_FLOAT
#   define DCC_TARGET_ALIGNOF_MAXALIGN_T DCC_TARGET_ALIGNOF_FLOAT
#endif


/* Default stack-alignment of normal arguments. */
#   define DCC_TARGET_STACKALIGN           DCC_TARGET_SIZEOF_INT

/* Stack-alignment of variadic arguments. */
#   define DCC_TARGET_VA_ALIGN             DCC_TARGET_SIZEOF_POINTER


#   define DCC_TARGET_TLSMODE_NONE         0 /*< TLS storage is not supported. */
#   define DCC_TARGET_TLSMODE_NATIVE       1 /*< TLS storage is supported natively (CPU-specific) (TODO: Missing) */
#   define DCC_TARGET_TLSMODE_EMULATED     2 /*< TLS storage is emulated with extern function calls (TODO: Missing) */
#   define DCC_TARGET_TLS                  DCC_TARGET_TLSMODE_NONE


#if !(DCC_TARGET_SIZEOF_GP_REGISTER >= DCC_TARGET_SIZEOF_ARITH_MAX)
#error "Invalid collellation between sizeof(GP_REGISTER) and sizeof(ARITH_MAX)"
#endif
#if !(DCC_TARGET_SIZEOF_IMM_MAX >= DCC_TARGET_SIZEOF_GP_REGISTER)
#error "Invalid collellation between sizeof(IMM_MAX) and sizeof(GP_REGISTER)"
#endif

#if ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_BYTE) == 8) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_WORD) == 8) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_INT)  == 8)
#   define DCC_TARGET_HAVE_INT8  1
#else
#   define DCC_TARGET_HAVE_INT8  0
#endif
#if ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_BYTE) == 16) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_WORD) == 16) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_INT)  == 16)
#   define DCC_TARGET_HAVE_INT16 1
#else
#   define DCC_TARGET_HAVE_INT16 0
#endif
#if ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_BYTE) == 32) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_WORD) == 32) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_INT)  == 32)
#   define DCC_TARGET_HAVE_INT32 1
#else
#   define DCC_TARGET_HAVE_INT32 0
#endif
#if ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_BYTE) == 64) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_WORD) == 64) || \
    ((DCC_TARGET_BITPERBYTE*DCC_TARGET_SIZEOF_INT)  == 64) || \
    ((DCC_TARGET_BITPERBYTE*8)                      == 64)
#   define DCC_TARGET_HAVE_INT64 1
#else
#   define DCC_TARGET_HAVE_INT64 0
#endif



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


/* When compiling string operations, prefer generating
 * code that already knows the length of the string.
 * While this usually results in larger code, based
 * on the instruction set of the CPU, it is most of
 * the time much faster, as intrinsic scanning functions
 * can be used when code is not forced to handle an
 * encounter with a '\0'-character as premature terminator.
 * Essentially, this controls (e.g.) how this is compiled:
 * >> if (__builtin_strchr(".-!:",c)) return 42;
 * Compiled as:
 * >> #if DCC_TARGET_PREFER_STR_WITHLEN
 * >> if (__builtin_memchr(".-!:",c,4)) return 42;
 * >> #else
 * >> if (strchr(".-!:",c)) return 42;
 * >> #endif
 */
#define DCC_TARGET_PREFER_STR_WITHLEN 1


/* Optional functions that DCC can assume to always be provided by the runtime. */
#define DCC_TARGET_RT_HAVE_STRLEN     1 /* size_t strlen(char const *s); */
#define DCC_TARGET_RT_HAVE_STRNLEN    1 /* size_t strnlen(char const *s, size_t max); */
#define DCC_TARGET_RT_HAVE_STREND     0 /* char *strend(char const *s); */
#define DCC_TARGET_RT_HAVE_STRNEND    0 /* char *strnend(char const *s, size_t max); */
#define DCC_TARGET_RT_HAVE_RAWMEMLEN  0 /* size_t rawmemlen(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_RAWMEMRLEN 0 /* size_t rawmemrlen(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_RAWMEMCHR  0 /* void *rawmemchr(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_RAWMEMRCHR 0 /* void *rawmemrchr(void const *p, int c); */
#define DCC_TARGET_RT_HAVE_MEMLEN     0 /* size_t memlen(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_MEMRLEN    0 /* size_t memrlen(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_MEMEND     0 /* void *memend(void const *p, int c, size_t s); */
#define DCC_TARGET_RT_HAVE_MEMREND    0 /* void *memrend(void const *p, int c, size_t s); */
/* NOTE: For full functionality, the runtime should implement at least:
 *    >> void *memchr(void const *p, int c, size_t s);
 *    >> void *memrchr(void const *p, int c, size_t s); */


#if DCC_TARGET_SIZEOF_CHAR == 1
#    define DCC_TARGET_TYPE_S1   "signed char"
#    define DCC_TARGET_TYPE_U1   "unsigned char"
#elif DCC_TARGET_SIZEOF_SHORT == 1
#    define DCC_TARGET_TYPE_S1   "short"
#    define DCC_TARGET_TYPE_U1   "unsigned short"
#elif DCC_TARGET_SIZEOF_INT == 1
#    define DCC_TARGET_TYPE_S1   "int"
#    define DCC_TARGET_TYPE_U1   "unsigned int"
#elif DCC_TARGET_SIZEOF_LONG == 1
#    define DCC_TARGET_TYPE_S1   "long"
#    define DCC_TARGET_TYPE_U1   "unsigned long"
#elif DCC_TARGET_SIZEOF_LONG_LONG == 1
#    define DCC_TARGET_TYPE_S1   "long long"
#    define DCC_TARGET_TYPE_U1   "unsigned long long"
#endif

#if DCC_TARGET_SIZEOF_CHAR == 2
#    define DCC_TARGET_TYPE_S2   "signed char"
#    define DCC_TARGET_TYPE_U2   "unsigned char"
#elif DCC_TARGET_SIZEOF_SHORT == 2
#    define DCC_TARGET_TYPE_S2   "short"
#    define DCC_TARGET_TYPE_U2   "unsigned short"
#elif DCC_TARGET_SIZEOF_INT == 2
#    define DCC_TARGET_TYPE_S2   "int"
#    define DCC_TARGET_TYPE_U2   "unsigned int"
#elif DCC_TARGET_SIZEOF_LONG == 2
#    define DCC_TARGET_TYPE_S2   "long"
#    define DCC_TARGET_TYPE_U2   "unsigned long"
#elif DCC_TARGET_SIZEOF_LONG_LONG == 2
#    define DCC_TARGET_TYPE_S2   "long long"
#    define DCC_TARGET_TYPE_U2   "unsigned long long"
#endif

#if DCC_TARGET_SIZEOF_CHAR == 4
#    define DCC_TARGET_TYPE_S4   "signed char"
#    define DCC_TARGET_TYPE_U4   "unsigned char"
#elif DCC_TARGET_SIZEOF_SHORT == 4
#    define DCC_TARGET_TYPE_S4   "short"
#    define DCC_TARGET_TYPE_U4   "unsigned short"
#elif DCC_TARGET_SIZEOF_INT == 4
#    define DCC_TARGET_TYPE_S4   "int"
#    define DCC_TARGET_TYPE_U4   "unsigned int"
#elif DCC_TARGET_SIZEOF_LONG == 4
#    define DCC_TARGET_TYPE_S4   "long"
#    define DCC_TARGET_TYPE_U4   "unsigned long"
#elif DCC_TARGET_SIZEOF_LONG_LONG == 4
#    define DCC_TARGET_TYPE_S4   "long long"
#    define DCC_TARGET_TYPE_U4   "unsigned long long"
#endif

#if DCC_TARGET_SIZEOF_CHAR == 8
#    define DCC_TARGET_TYPE_S8   "signed char"
#    define DCC_TARGET_TYPE_U8   "unsigned char"
#elif DCC_TARGET_SIZEOF_SHORT == 8
#    define DCC_TARGET_TYPE_S8   "short"
#    define DCC_TARGET_TYPE_U8   "unsigned short"
#elif DCC_TARGET_SIZEOF_INT == 8
#    define DCC_TARGET_TYPE_S8   "int"
#    define DCC_TARGET_TYPE_U8   "unsigned int"
#elif DCC_TARGET_SIZEOF_LONG == 8
#    define DCC_TARGET_TYPE_S8   "long"
#    define DCC_TARGET_TYPE_U8   "unsigned long"
#elif DCC_TARGET_SIZEOF_LONG_LONG == 8
#    define DCC_TARGET_TYPE_S8   "long long"
#    define DCC_TARGET_TYPE_U8   "unsigned long long"
#endif

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

#if DCC_TARGET_SIZEOF_INT >= 1
#define DCC_TARGET_SFX_I1  ""
#elif DCC_TARGET_SIZEOF_LONG >= 1
#define DCC_TARGET_SFX_I1  "l"
#elif DCC_TARGET_SIZEOF_LONG_LONG >= 1
#define DCC_TARGET_SFX_I1  "ll"
#endif
#if DCC_TARGET_SIZEOF_INT >= 2
#define DCC_TARGET_SFX_I2  ""
#elif DCC_TARGET_SIZEOF_LONG >= 2
#define DCC_TARGET_SFX_I2  "l"
#elif DCC_TARGET_SIZEOF_LONG_LONG >= 2
#define DCC_TARGET_SFX_I2  "ll"
#endif
#if DCC_TARGET_SIZEOF_INT >= 4
#define DCC_TARGET_SFX_I4  ""
#elif DCC_TARGET_SIZEOF_LONG >= 4
#define DCC_TARGET_SFX_I4  "l"
#elif DCC_TARGET_SIZEOF_LONG_LONG >= 4
#define DCC_TARGET_SFX_I4  "ll"
#endif
#if DCC_TARGET_SIZEOF_INT >= 8
#define DCC_TARGET_SFX_I8  ""
#elif DCC_TARGET_SIZEOF_LONG >= 8
#define DCC_TARGET_SFX_I8  "l"
#elif DCC_TARGET_SIZEOF_LONG_LONG >= 8
#define DCC_TARGET_SFX_I8  "ll"
#endif

#ifdef DCC_TARGET_SFX_I1
#define DCC_TARGET_MIN_S1  "(-127" DCC_TARGET_SFX_I1 "-1" DCC_TARGET_SFX_I1 ")"
#define DCC_TARGET_MAX_S1    "127" DCC_TARGET_SFX_I1
#define DCC_TARGET_MIN_U1    "0u" DCC_TARGET_SFX_I1
#define DCC_TARGET_MAX_U1    "0xffu" DCC_TARGET_SFX_I1
#endif
#ifdef DCC_TARGET_SFX_I2
#define DCC_TARGET_MIN_S2  "(-32767" DCC_TARGET_SFX_I2 "-1" DCC_TARGET_SFX_I2 ")"
#define DCC_TARGET_MAX_S2    "32767" DCC_TARGET_SFX_I2
#define DCC_TARGET_MIN_U2    "0u" DCC_TARGET_SFX_I2
#define DCC_TARGET_MAX_U2    "0xffffu" DCC_TARGET_SFX_I2
#endif

#ifdef DCC_TARGET_SFX_I4
#define DCC_TARGET_MIN_S4  "(-2147483647" DCC_TARGET_SFX_I4 "-1" DCC_TARGET_SFX_I4 ")"
#define DCC_TARGET_MAX_S4    "2147483647" DCC_TARGET_SFX_I4
#define DCC_TARGET_MIN_U4    "0u" DCC_TARGET_SFX_I4
#define DCC_TARGET_MAX_U4    "0xffffffffu" DCC_TARGET_SFX_I4
#endif

#ifdef DCC_TARGET_SFX_I8
#define DCC_TARGET_MIN_S8  "(-9223372036854775807" DCC_TARGET_SFX_I8 "-1" DCC_TARGET_SFX_I8 ")"
#define DCC_TARGET_MAX_S8    "9223372036854775807" DCC_TARGET_SFX_I8
#define DCC_TARGET_MIN_U8    "0u" DCC_TARGET_SFX_I8
#define DCC_TARGET_MAX_U8    "0xffffffffffffffffu" DCC_TARGET_SFX_I8
#endif

#define DCC_TARGET_MIN_S(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MIN_S,b)
#define DCC_TARGET_MAX_S(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MAX_S,b)
#define DCC_TARGET_MIN_U(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MIN_U,b)
#define DCC_TARGET_MAX_U(b)  DCC_PRIVATE_PP_CAT(DCC_TARGET_MAX_U,b)


typedef DCC_PP_CAT3( int,DCC_MUL8(DCC_TARGET_SIZEOF_POINTER),    _t) DCC(target_off_t);
typedef DCC_PP_CAT3(uint,DCC_MUL8(DCC_TARGET_SIZEOF_POINTER),    _t) DCC(target_ptr_t);
typedef DCC_PP_CAT3(uint,DCC_MUL8(DCC_TARGET_SIZEOF_SIZE_T),     _t) DCC(target_siz_t);
typedef DCC_PP_CAT3(uint,DCC_MUL8(DCC_TARGET_SIZEOF_GP_REGISTER),_t) DCC(target_reg_t);
typedef DCC_PP_CAT3(uint,DCC_MUL8(DCC_TARGET_SIZEOF_IMM_MAX),    _t) DCC(target_imm_t);


/* Register class/descriptor:
 * One of 'DCC_RC_*', potentially or'd together with one of 'DCC_ASMREG_*'. */
typedef uint16_t DCC(rc_t);



DCC_DECL_END

#endif /* !GUARD_DCC_TARGET_H */
