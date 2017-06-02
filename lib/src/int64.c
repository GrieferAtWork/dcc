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

__attribute__((visibility("default")))
uint64_t __ashlti3(uint64_t x, int shift) {
	/* TODO: return x << shift; */
	return x;
}

__attribute__((visibility("default")))
uint64_t __ashrti3(uint64_t x, int shift) {
	/* TODO: return x >> shift; */
	return x;
}

__attribute__((visibility("default")))
int64_t __lshrti3(int64_t x, int shift) {
	/* TODO: return x >> shift; */
	return x;
}

__attribute__((visibility("default")))
uint64_t __udivti3(uint64_t x, uint64_t y) {
	/* TODO: return x / y; */
	return x;
}

__attribute__((visibility("default")))
int64_t __divti3(int64_t x, int64_t y) {
	/* TODO: return x / y; */
	return x;
}

__attribute__((visibility("default")))
uint64_t __umodti3(uint64_t x, int64_t y) {
	/* TODO: return x % y; */
	return x;
}

__attribute__((visibility("default")))
int64_t __modti3(int64_t x, int64_t y) {
	/* TODO: return x % y; */
	return x;
}

__attribute__((visibility("default")))
int64_t __multi3(int64_t x, int64_t y) {
	/* TODO: return x * y; */
	return x;
}

#pragma GCC visibility pop
