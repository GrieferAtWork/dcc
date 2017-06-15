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

#ifdef __INTELLISENSE__
#define ASSERT_CONSTANT_P(x)
#else
#define ASSERT_CONSTANT_P(x) _Static_assert(__builtin_constant_p(x),"__builtin_constant_p(" #x ")")
#endif
#define ASSERT_CONSTEXPR(x) ASSERT_CONSTANT_P(#!x); _Static_assert(x,#x)

int main(int argc, char *argv[]) {

 /* make sure static assertions can even fail. */
#pragma warning(suppress: "-Wuser") /* Allow one user-error to be suppressed */
#if __has_warning("-Wuser")
#pragma warning(error: "-Wuser")
#error "User-warnings should be disabled"
#endif

 /* This assertion failing will consume the error-suppression above. */
 _Static_assert(0,"This should fail");

#if !__has_warning("-Wuser")
#pragma warning(error: "-Wuser")
#error "User-warnings are still disabled"
#endif


 ASSERT_CONSTANT_P(0);
 ASSERT_CONSTANT_P(42);
 ASSERT_CONSTANT_P(-42);
 ASSERT_CONSTANT_P(+42);
 ASSERT_CONSTANT_P(-0);
 ASSERT_CONSTANT_P(+0);
 ASSERT_CONSTEXPR(2*40 == 80);
 ASSERT_CONSTEXPR(2-40 == -38);
 ASSERT_CONSTEXPR(2+40 == 42);
 ASSERT_CONSTEXPR(0x1    << 1 == 0x2);
 ASSERT_CONSTEXPR(0x2    >> 1 == 0x1);
 ASSERT_CONSTEXPR(0x0001 << 4 == 0x0010);
 ASSERT_CONSTEXPR(0x0010 >> 4 == 0x0001);
 ASSERT_CONSTEXPR(0x0001 << 8 == 0x0100);
 ASSERT_CONSTEXPR(0x0100 >> 8 == 0x0001);
 ASSERT_CONSTEXPR(42/1 == 42);
 ASSERT_CONSTEXPR(42/2 == 21);
 ASSERT_CONSTEXPR(42*0 == 0);
 ASSERT_CONSTEXPR(42*1 == 42);
 ASSERT_CONSTEXPR(42*2 == 84);
 ASSERT_CONSTEXPR(42*3 == 126);

 /* Assert some integral relations. */
 ASSERT_CONSTEXPR(!(0 < 0));
 ASSERT_CONSTEXPR(0 <= 0);
 ASSERT_CONSTEXPR(0 == 0);
 ASSERT_CONSTEXPR(!(0 != 0));
 ASSERT_CONSTEXPR(0 >= 0);
 ASSERT_CONSTEXPR(!(0 > 0));
 ASSERT_CONSTEXPR(!(1 <  0));
 ASSERT_CONSTEXPR(!(1 <= 0));
 ASSERT_CONSTEXPR(!(1 == 0));
 ASSERT_CONSTEXPR(1 != 0);
 ASSERT_CONSTEXPR(1 >  0);
 ASSERT_CONSTEXPR(1 >= 0);
 ASSERT_CONSTEXPR(  -1 <  0);
 ASSERT_CONSTEXPR(  -1 <= 0);
 ASSERT_CONSTEXPR(!(-1 == 0));
 ASSERT_CONSTEXPR(  -1 != 0);
 ASSERT_CONSTEXPR(!(-1 >  0));
 ASSERT_CONSTEXPR(!(-1 >= 0));

 /* Assert 0/1 boolean expressions. */
 ASSERT_CONSTEXPR(1 == (-1 <  0));
 ASSERT_CONSTEXPR(1 == (-1 <= 0));
 ASSERT_CONSTEXPR(0 == (-1 == 0));
 ASSERT_CONSTEXPR(1 == (-1 != 0));
 ASSERT_CONSTEXPR(0 == (-1 >  0));
 ASSERT_CONSTEXPR(0 == (-1 >= 0));
 ASSERT_CONSTEXPR((-1 <  0) == 1);
 ASSERT_CONSTEXPR((-1 <= 0) == 1);
 ASSERT_CONSTEXPR((-1 == 0) == 0);
 ASSERT_CONSTEXPR((-1 != 0) == 1);
 ASSERT_CONSTEXPR((-1 >  0) == 0);
 ASSERT_CONSTEXPR((-1 >= 0) == 0);

 /* Some more complicated (and questionably compile-time constant) things. */
#pragma warning(suppress: "-Wquality") /* Equal pointers passed to 'memcmp' */
#pragma warning(suppress: "-Wquality")
 ASSERT_CONSTEXPR(!__builtin_memcmp("foo","foo",3*sizeof(char)));

 return 0;
}


