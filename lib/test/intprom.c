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

#include <stdio.h>
#include <stdlib.h>

#pragma warning("-Wno-old-function-decl")

/* NOTE: Optimally, this function should never be called.
 *       Therefor we mark is as 'unused', as it should be optimized away! */
[[unused,noreturn]]
static void ass_fail2(char const *f, int l,
                      char const *t1, char const *tt1,
                      char const *t2, char const *tt2) {
 printf("%s(%d) : Assertion failed ('%s':'%s' != '%s':'%s')\n",
        f,l,t1,tt1,t2,tt2);
 exit(1);
}

/* AssertSameType */
#define AST(T1,T2) \
 (__builtin_types_compatible_p(T1,T2) || \
 (ass_fail2(__FILE__,__LINE__,\
            #T1,__builtin_typestr(T1),\
            #T2,__builtin_typestr(T2)),0))
#define NAST(T1,T2) \
 (!__builtin_types_compatible_p(T1,T2) || \
 (ass_fail2(__FILE__,__LINE__,\
            #T1,__builtin_typestr(T1),\
            #T2,__builtin_typestr(T2)),0))

int main(int argc, char *argv[]) {

 char               c   [[unused]];
 int                i   [[unused]];
 long               l   [[unused]];
 long long          ll  [[unused]];
 unsigned int       u   [[unused]];
 unsigned long      ul  [[unused]];
 unsigned long long ull [[unused]];

 /* Generic same-type assertions. */
#ifdef __CHAR_UNSIGNED__
 AST(char,unsigned char);
#else
 AST(char,signed char);
#endif

 AST(short,short int);
 AST(short,signed short);
 AST(short,signed short int);
 AST(unsigned short,unsigned short int);

 AST(signed,int);
 AST(signed,signed int);
 AST(unsigned,unsigned int);

 AST(long,long int);
 AST(long,signed long);
 AST(long,signed long int);
 AST(unsigned long,unsigned long int);

 AST(long long,long long int);
 AST(long long,signed long long);
 AST(long long,signed long long int);
 AST(unsigned long long,unsigned long long int);

 NAST(int,short);
 AST (int,signed);
 NAST(int,unsigned);
 NAST(int,long);
 NAST(int,long long);
 NAST(unsigned int,short);
 NAST(unsigned int,signed);
 AST (unsigned int,unsigned);
 NAST(unsigned int,long);
 NAST(unsigned int,long long);

 /* Integer promotions. */
 AST(char,        __typeof__(c));
 AST(int,         __typeof__(+c));
#ifdef __CHAR_UNSIGNED__
 AST(int,         __typeof__(-(signed char)c));
#else
 AST(int,         __typeof__(-c));
#endif
 AST(int,         __typeof__(~c));
 AST(int,         __typeof__(i));

 AST(int,               __typeof__(i+i));
 AST(int,               __typeof__(i+c));
 AST(int,               __typeof__(c+i));
 AST(int,               __typeof__(c+c));
 AST(long,              __typeof__(l+l));
 AST(long,              __typeof__(l+i));
 AST(long,              __typeof__(l+c));
 AST(long,              __typeof__(i+l));
 AST(long,              __typeof__(c+l));
 AST(long long,         __typeof__(ll+c));
 AST(long long,         __typeof__(ll+i));
 AST(long long,         __typeof__(ll+l));
 AST(long long,         __typeof__(c+ll));
 AST(long long,         __typeof__(i+ll));
 AST(long long,         __typeof__(l+ll));
 AST(unsigned int,      __typeof__(u+c));
 AST(unsigned int,      __typeof__(u+i));
 AST(unsigned int,      __typeof__(c+u));
 AST(unsigned int,      __typeof__(i+u));
 AST(unsigned long,     __typeof__(ul+c));
 AST(unsigned long,     __typeof__(ul+i));
 AST(unsigned long,     __typeof__(ul+l));
 AST(unsigned long,     __typeof__(c+ul));
 AST(unsigned long,     __typeof__(i+ul));
 AST(unsigned long,     __typeof__(l+ul));
 AST(unsigned long long,__typeof__(ull+c));
 AST(unsigned long long,__typeof__(ull+i));
 AST(unsigned long long,__typeof__(ull+l));
 AST(unsigned long long,__typeof__(ull+ll));
 AST(unsigned long long,__typeof__(c+ull));
 AST(unsigned long long,__typeof__(i+ull));
 AST(unsigned long long,__typeof__(l+ull));
 AST(unsigned long long,__typeof__(ll+ull));

 return 0;
}


