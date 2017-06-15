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
/* Compile with: $ dcc -nostdlib -c -o int64.o int64.c */

#include <stdint.h>

/* Declare everything with hidden visibility. */
#pragma GCC visibility push("hidden")

#define CRTDEF  [[visibility("hidden")]]

CRTDEF int64_t  __ashlti3(int64_t  x, int shift);
CRTDEF int64_t  __ashrti3(int64_t  x, int shift);
CRTDEF uint64_t __lshrti3(uint64_t x, int shift);
CRTDEF uint64_t __udivti3(uint64_t x, uint64_t y);
CRTDEF int64_t  __divti3 (int64_t  x, int64_t  y);
CRTDEF uint64_t __umodti3(uint64_t x, uint64_t y);
CRTDEF int64_t  __modti3 (int64_t  x, int64_t  y);
CRTDEF int64_t  __multi3 (int64_t  x, int64_t  y);



typedef union {
 int64_t  s;
 uint64_t u;
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
 struct { int32_t  los,his; };
 struct { uint32_t lou,hiu; };
#elif __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
 struct { int32_t  his,los; };
 struct { uint32_t hiu,lou; };
#else
#   error FIXME
#endif
} i64;
#define X  ((i64 &)x)
#define Y  ((i64 &)y)

int64_t __ashlti3(int64_t x, int shift) {
 /* return x << shift; */
 if (shift >= 32) {
  X.hiu = (X.lou << (shift-32));
  X.lou = (0);
 } else if (shift) {
  X.hiu = (X.hiu << shift) | (X.lou >> (32-shift));
  X.lou = (X.lou << shift);
 }
 return X.s;
}

int64_t __ashrti3(int64_t x, int shift) {
 /* return x >> shift; */
 if (shift >= 32) {
  X.los = (X.his >> (shift-32));
  X.his = (X.his >> 31);
 } else if (shift) {
  X.los = (X.lou >> shift) | (X.his << (32-shift));
  X.his = (X.his >> shift);
 }
 return X.s;
}

uint64_t __lshrti3(uint64_t x, int shift) {
 /* return x >> shift; */
 if (shift >= 32) {
  X.lou = (X.hiu >> (shift-32));
  X.hiu = (0);
 } else if (shift) {
  X.lou = (X.lou >> shift) | (X.his << (32-shift));
  X.hiu = (X.hiu >> shift);
 }
 return X.u;
}

uint64_t __udivti3(uint64_t x, uint64_t y) {
 /* TODO: return x / y; */
 return (uint32_t)x / (uint32_t)y;
}

int64_t __divti3(int64_t x, int64_t y) {
 /* TODO: return x / y; */
 return (int32_t)x / (int32_t)y;
}

uint64_t __umodti3(uint64_t x, uint64_t y) {
 /* TODO: return x % y; */
 return (uint32_t)x % (uint32_t)y;
}

int64_t __modti3(int64_t x, int64_t y) {
 /* TODO: return x % y; */
 return (int32_t)x % (int32_t)y;
}

int64_t __multi3(int64_t x, int64_t y) {
 /* TODO: return x * y; */
 return (int32_t)x * (int32_t)y;
}

#pragma GCC visibility pop
