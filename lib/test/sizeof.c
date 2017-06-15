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
#include <assert.h>

#define BPB 8

int main(int argc, char *argv[]) {

 /* Basic, builtin types. */
 assert(sizeof(unsigned char) == sizeof(char));
 assert(sizeof(signed char) == sizeof(char));
 assert(sizeof(unsigned short) == sizeof(short));
 assert(sizeof(signed short) == sizeof(short));
 assert(sizeof(unsigned short) == sizeof(short int));
 assert(sizeof(signed short) == sizeof(short int));
 assert(sizeof(unsigned short int) == sizeof(short));
 assert(sizeof(signed short int) == sizeof(short));
 assert(sizeof(unsigned short int) == sizeof(short int));
 assert(sizeof(signed short int) == sizeof(short int));
 assert(sizeof(unsigned int) == sizeof(int));
 assert(sizeof(signed int) == sizeof(int));
 assert(sizeof(unsigned) == sizeof(int));
 assert(sizeof(signed) == sizeof(int));
 assert(sizeof(unsigned long) == sizeof(long));
 assert(sizeof(signed long) == sizeof(long));
 assert(sizeof(unsigned long) == sizeof(long int));
 assert(sizeof(signed long) == sizeof(long int));
 assert(sizeof(unsigned long int) == sizeof(long));
 assert(sizeof(signed long int) == sizeof(long));
 assert(sizeof(unsigned long int) == sizeof(long int));
 assert(sizeof(signed long int) == sizeof(long int));
 assert(sizeof(unsigned long long) == sizeof(long long));
 assert(sizeof(signed long long) == sizeof(long long));
 assert(sizeof(unsigned long long) == sizeof(long long int));
 assert(sizeof(signed long long) == sizeof(long long int));
 assert(sizeof(unsigned long long int) == sizeof(long long));
 assert(sizeof(signed long long int) == sizeof(long long));
 assert(sizeof(unsigned long long int) == sizeof(long long int));
 assert(sizeof(signed long long int) == sizeof(long long int));

 assert(sizeof(void *) == sizeof(int *));
 assert(sizeof(int[64*__SIZEOF_POINTER__]) == 64*__SIZEOF_POINTER__*sizeof(int));
 assert(sizeof(int[64*__SIZEOF_POINTER__]) == sizeof(int(&)[64*__SIZEOF_POINTER__]));
 assert(sizeof(int[64*__SIZEOF_POINTER__]) != sizeof(int(*)[64*__SIZEOF_POINTER__]));

#if __STDC__
 /* STD-C conforming type size relations. */
 assert(sizeof(short) >= sizeof(char));
 assert(sizeof(int) >= sizeof(short));
 assert(sizeof(long) >= sizeof(int));
 assert(sizeof(long long) >= sizeof(long));
#endif

 /* STD-C integral size suffix. */
 assert(sizeof(0) == sizeof(int));
 assert(sizeof(1) == sizeof(int));
 assert(sizeof(1u) == sizeof(unsigned int));
 assert(sizeof(1l) == sizeof(long));
 assert(sizeof(1ul) == sizeof(unsigned long));
 assert(sizeof(1ll) == sizeof(long long));
 assert(sizeof(1ull) == sizeof(unsigned long long));

 /* Builtin fixed-length macros in both modes. */
#pragma extension("-ffixed-length-integrals")
 assert(sizeof(__INT8_C(42))  == 8/BPB);
 assert(sizeof(__INT16_C(42)) == 16/BPB);
 assert(sizeof(__INT32_C(42)) == 32/BPB);
 assert(sizeof(__INT64_C(42)) == 64/BPB);

#pragma extension("-fno-fixed-length-integrals")
 assert(sizeof(__INT8_C(42))  >= 8/BPB);
 assert(sizeof(__INT16_C(42)) >= 16/BPB);
 assert(sizeof(__INT32_C(42)) >= 32/BPB);
 assert(sizeof(__INT64_C(42)) >= 64/BPB);

 /* Preprocessor size macros. */
 assert(__SIZEOF_INT__ == sizeof(int));
 assert(__SIZEOF_LONG__ == sizeof(long));
 assert(__SIZEOF_LONG_LONG__ == sizeof(long long));
 assert(__SIZEOF_SHORT__ == sizeof(short));
 assert(__SIZEOF_POINTER__ == sizeof(void *));
 assert(__SIZEOF_POINTER__ == sizeof(__INTPTR_TYPE__));
 assert(__SIZEOF_POINTER__ == sizeof(__UINTPTR_TYPE__));
#ifdef __SIZEOF_FLOAT__
 assert(__SIZEOF_FLOAT__ == sizeof(float));
#endif
#ifdef __SIZEOF_DOUBLE__
 assert(__SIZEOF_DOUBLE__ == sizeof(double));
#endif
#ifdef __SIZEOF_LONG_DOUBLE__
 assert(__SIZEOF_LONG_DOUBLE__ == sizeof(long double));
#endif
 assert(__SIZEOF_SIZE_T__ == sizeof(__SIZE_TYPE__));
 assert(__SIZEOF_WCHAR_T__ == sizeof(__WCHAR_TYPE__));
 assert(__SIZEOF_WINT_T__ == sizeof(__WINT_TYPE__));
 assert(__SIZEOF_PTRDIFF_T__ == sizeof(__PTRDIFF_TYPE__));
#ifdef __SIZEOF_CHAR__ /* Extension sizeof macros. */
 assert(__SIZEOF_CHAR__ == sizeof(char));
 assert(__SIZEOF_INT_LEAST8_T__ == sizeof(__INT_LEAST8_TYPE__));
 assert(__SIZEOF_INT_LEAST16_T__ == sizeof(__INT_LEAST16_TYPE__));
 assert(__SIZEOF_INT_LEAST32_T__ == sizeof(__INT_LEAST32_TYPE__));
 assert(__SIZEOF_INT_LEAST64_T__ == sizeof(__INT_LEAST64_TYPE__));
 assert(__SIZEOF_INT_LEAST8_T__ == sizeof(__UINT_LEAST8_TYPE__));
 assert(__SIZEOF_INT_LEAST16_T__ == sizeof(__UINT_LEAST16_TYPE__));
 assert(__SIZEOF_INT_LEAST32_T__ == sizeof(__UINT_LEAST32_TYPE__));
 assert(__SIZEOF_INT_LEAST64_T__ == sizeof(__UINT_LEAST64_TYPE__));
 assert(__SIZEOF_INT_FAST8_T__ == sizeof(__INT_FAST8_TYPE__));
 assert(__SIZEOF_INT_FAST16_T__ == sizeof(__INT_FAST16_TYPE__));
 assert(__SIZEOF_INT_FAST32_T__ == sizeof(__INT_FAST32_TYPE__));
 assert(__SIZEOF_INT_FAST64_T__ == sizeof(__INT_FAST64_TYPE__));
 assert(__SIZEOF_INT_FAST8_T__ == sizeof(__UINT_FAST8_TYPE__));
 assert(__SIZEOF_INT_FAST16_T__ == sizeof(__UINT_FAST16_TYPE__));
 assert(__SIZEOF_INT_FAST32_T__ == sizeof(__UINT_FAST32_TYPE__));
 assert(__SIZEOF_INT_FAST64_T__ == sizeof(__UINT_FAST64_TYPE__));
 assert(__SIZEOF_INTMAX_T__ == sizeof(__INTMAX_TYPE__));
 assert(__SIZEOF_INTMAX_T__ == sizeof(__UINTMAX_TYPE__));
 assert(__SIZEOF_SIG_ATOMIC_T__ == sizeof(__SIG_ATOMIC_TYPE__));
 assert(__SIZEOF_SIZE_T__ == sizeof(__SSIZE_TYPE__));
#endif
 assert(__CHAR_BIT__ == sizeof(char)*BPB);

 assert(sizeof(__INT8_TYPE__) == 8/BPB);
 assert(sizeof(__INT16_TYPE__) == 16/BPB);
 assert(sizeof(__INT32_TYPE__) == 32/BPB);
 assert(sizeof(__INT64_TYPE__) == 64/BPB);
 assert(sizeof(__UINT8_TYPE__) == 8/BPB);
 assert(sizeof(__UINT16_TYPE__) == 16/BPB);
 assert(sizeof(__UINT32_TYPE__) == 32/BPB);
 assert(sizeof(__UINT64_TYPE__) == 64/BPB);

 return 0;
}


