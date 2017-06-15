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
#define ASSERT_CONSTEXPR(x)  _Static_assert(x,#x)

int main(int argc, char *argv[]) {

 /* make sure static assertions can even fail. */
#pragma warning(error: "-Wuser")
#pragma warning(suppress: "-Wuser") /* Allow one user-error to be suppressed */
#if __has_warning("-Wuser")
#pragma warning(error: "-Wuser")
#error "User-warnings should be disabled"
#endif

 /* This assertion failing will consume the error-suppression above. */
 _Static_assert(0,"This should fail with a hidden warning");

#if !__has_warning("-Wuser")
#pragma warning(error: "-Wuser")
#error "User-warnings are still disabled ('_Static_assert' didn't fail?)"
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
#pragma warning(push,"-Wno-quality") /* Equal pointers passed to 'memcmp' */
 ASSERT_CONSTEXPR(!__builtin_memcmp("foo","foo",3*sizeof(char)));
#pragma warning(pop)

 /* Since symbols from '.string' can be merged, the same strings must be equal. */
 ASSERT_CONSTEXPR("foo" == "foo");
 ASSERT_CONSTEXPR("foo" == "foo");
 ASSERT_CONSTEXPR("foo" != "bar");
 ASSERT_CONSTEXPR("foo" != "bar");
 ASSERT_CONSTEXPR("bar" == "bar");
 ASSERT_CONSTEXPR("bar" == "bar");

 /* Make sure that sub-string string merging works as well. */
 ASSERT_CONSTEXPR("barbaz"+3 == "baz");

 /* Assert compile-time string length. */
 ASSERT_CONSTEXPR(__builtin_strlen("baz") == 3);

 ASSERT_CONSTEXPR(0xaabbu == 0xaabb);
 ASSERT_CONSTEXPR(0xaabb  == 0xaabbu);
 ASSERT_CONSTEXPR(0xaabbu == 0xaabbu);
 ASSERT_CONSTEXPR(0xaabbccddu == 0xaabbccdd);
 ASSERT_CONSTEXPR(0xaabbccdd  == 0xaabbccddu);
 ASSERT_CONSTEXPR(0xaabbccddu == 0xaabbccddu);

 ASSERT_CONSTEXPR(0xaabb == 0xaabb);
 ASSERT_CONSTEXPR(0xbbaa == 0xbbaa);
 ASSERT_CONSTEXPR(0xaabb != 0xbbaa);
 ASSERT_CONSTEXPR(0xaabbccdd == 0xaabbccdd);
 ASSERT_CONSTEXPR(0xaabbccdd != 0xddccbbaa);
 ASSERT_CONSTEXPR(0xaabbccdd != 0xddccbbaa);

 /* Assert byte-swap. */
 ASSERT_CONSTEXPR(__builtin_bswap16(__UINT16_C(0xaabb)) == __INT16_C(0xbbaa));
 ASSERT_CONSTEXPR(__builtin_bswap32(__UINT32_C(0xaabbccdd)) == __INT32_C(0xddccbbaa));
 ASSERT_CONSTEXPR(__builtin_bswap64(__UINT64_C(0x8899aabbccddeeff)) == __INT64_C(0xffeeddccbbaa9988));

 /* Assert advanced arithmetic */
 ASSERT_CONSTEXPR(((__INT64_TYPE__)1 << 64) == 0);

 /* Test arithmetic of integral overflow. */
#pragma warning(push,"-Wno-integral-trunc")
 ASSERT_CONSTEXPR((__INT8_TYPE__)(__INT8_MAX__+1) == __INT8_MIN__);
 ASSERT_CONSTEXPR((__INT8_TYPE__)(__INT8_MIN__-1) == __INT8_MAX__);
 ASSERT_CONSTEXPR((__INT16_TYPE__)(__INT16_MAX__+1) == __INT16_MIN__);
 ASSERT_CONSTEXPR((__INT16_TYPE__)(__INT16_MIN__-1) == __INT16_MAX__);
 ASSERT_CONSTEXPR((__INT32_TYPE__)(__INT32_MAX__+1) == __INT32_MIN__);
 ASSERT_CONSTEXPR((__INT32_TYPE__)(__INT32_MIN__-1) == __INT32_MAX__);
 ASSERT_CONSTEXPR((__INT64_TYPE__)(__INT64_MAX__+1) == __INT64_MIN__);
 ASSERT_CONSTEXPR((__INT64_TYPE__)(__INT64_MIN__-1) == __INT64_MAX__);
 ASSERT_CONSTEXPR(__INT_MAX__+1 == __INT_MIN__);
 ASSERT_CONSTEXPR(__INT_MIN__-1 == __INT_MAX__);
 ASSERT_CONSTEXPR(__LONG_MAX__+1 == __LONG_MIN__);
 ASSERT_CONSTEXPR(__LONG_MIN__-1 == __LONG_MAX__);
 ASSERT_CONSTEXPR(__LONG_LONG_MAX__+1 == __LONG_LONG_MIN__);
 ASSERT_CONSTEXPR(__LONG_LONG_MIN__-1 == __LONG_LONG_MAX__);
#pragma warning(pop)



 return 0;
}


