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
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __INTELLISENSE__
#define U(x) ((uint64_t)(x))
#define S(x)  ((int64_t)(x))
#else
#define U(x) ({ volatile uint64_t __x = (x); __x; })
#define S(x) ({ volatile  int64_t __x = (x); __x; })
#endif

#define ASSERT_EQ(x,y) \
do{ auto __x = (x); auto __y = (y);\
    if (__x != __y) { printf("%s(%d) : %llx == %llx\n",__FILE__,__LINE__,__x,__y); exit(1); }\
}while(0)


int main(int argc, char *argv[]) {

 /* Check shift operations. */
 ASSERT_EQ((U(__UINT64_C(0x0000000000000001)) << 1), __UINT64_C(0x0000000000000002));
 ASSERT_EQ((U(__UINT64_C(0x0000000000000010)) << 1), __UINT64_C(0x0000000000000020));
 ASSERT_EQ((U(__UINT64_C(0x0000000000000080)) << 1), __UINT64_C(0x0000000000000100));
 ASSERT_EQ((U(__UINT64_C(0x0000000000000001)) << 31),__UINT64_C(0x0000000080000000));
 ASSERT_EQ((U(__UINT64_C(0x0000000000000001)) << 32),__UINT64_C(0x0000000100000000));
 ASSERT_EQ((U(__UINT64_C(0x0000000000000001)) << 63),__UINT64_C(0x8000000000000000));
 ASSERT_EQ((S(__UINT64_C(0x8000000000000000)) >> 63),__UINT64_C(0xffffffffffffffff));
 ASSERT_EQ((U(__UINT64_C(0x8000000000000000)) >> 63),__UINT64_C(0x0000000000000001));
 ASSERT_EQ((U(__UINT64_C(0x8000000000000000)) >> 31),__UINT64_C(0x0000000100000000));
 ASSERT_EQ((S(__UINT64_C(0x8000000000000000)) >> 31),__UINT64_C(0xffffffff00000000));
 ASSERT_EQ((U(__UINT64_C(0x8000000000000000)) >> 32),__UINT64_C(0x0000000080000000));
 ASSERT_EQ((S(__UINT64_C(0x8000000000000000)) >> 32),__UINT64_C(0xffffffff80000000));
 ASSERT_EQ((U(__UINT64_C(0xffffffff00000000)) >> 32),__UINT64_C(0x00000000ffffffff));
 ASSERT_EQ((U(__UINT64_C(0x00000000ffffffff)) << 32),__UINT64_C(0xffffffff00000000));
 ASSERT_EQ((U(__UINT64_C(0x1234567800000000)) >> 32),__UINT64_C(0x0000000012345678));
 ASSERT_EQ((U(__UINT64_C(0x0000000012345678)) << 32),__UINT64_C(0x1234567800000000));

 return 0;
}


