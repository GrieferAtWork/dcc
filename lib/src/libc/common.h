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
#ifndef GUARD_LIB_SRC_LIBC_COMMON_H
#define GUARD_LIB_SRC_LIBC_COMMON_H 1

#include <__stdinc.h>

#ifndef __INTELLISENSE__
#define ASM{...} __asm__(#__VA_ARGS__);
#endif

/* Use a default symbol visibility of 'HIDDEN'. */
#pragma comment(compiler,"-fvisibility=hidden")

#define DECL_BEGIN /* ... */
#define DECL_END   /* ... */
#define PUBLIC     [[visiblity("default")]]

#define likely(x)   (__builtin_expect(!!(x),1))
#define unlikely(x) (__builtin_expect(!!(x),0))


#ifdef _WIN32
/* Yes! Window's handles don't actually need to be pointers.
 * It is always sufficient to use 'unsigned short' (aka. 'uint16_t')
 * And with the fact that 'int' being 32-bit on windows, we
 * can easily fit any kind of regular, old HANDLE inside! */
#define FD_MAX 65536
extern void *w32_stdhandle(int fd);
#define W32_FTOH(fd) ((unsigned int)(fd) <= 2 ? (HANDLE)w32_stdhandle(fd) : (HANDLE)(fd))
#define W32_HTOF(hd) ((int)(hd))


/* Fix the given filename 'name', forcing it to be absolute
 * by prepending the current directory, as well as formating
 * any slashes, and either return a pointer to 'buf', a
 * caller-provided wide-buffer with 'W32_FILENAME_BUFSIZE'
 * elements, or a newly allocated dynamic buffer.
 * In both cases, the caller should call 'w32_freefilename(return,buf)'
 * in order to clean up the return buffer.
 * HINT: This function never returns NULL. */
#define W32_FILENAME_BUFSIZE 512
extern __WCHAR_TYPE__ *w32_allocfilename(__WCHAR_TYPE__ *__restrict buf, char const *__restrict name);
extern __WCHAR_TYPE__ *w32_allocfilenameat(__WCHAR_TYPE__ *__restrict buf, int fd, char const *__restrict name);
extern __WCHAR_TYPE__ *w32_allocfdname(__WCHAR_TYPE__ *__restrict buf, int fd);
#define w32_freefilename(fname,buf) (void)((fname) == (buf) || (__builtin_free(fname),0))

#define W32_ERRORCTX_NONE     0
#define W32_ERRORCTX_OPEN     1
#define W32_ERRORCTX_FCNTL    2
#define W32_ERRORCTX_FSYMC    3
#define W32_ERRORCTX_DUP      4
#define W32_ERRORCTX_DUP2     5
#define W32_ERRORCTX_LSEEK    6
#define W32_ERRORCTX_TRUNCATE 7
#define W32_ERRORCTX_CLOSE    8
#define W32_ERRORCTX_READ     9
#define W32_ERRORCTX_WRITE   10
#define W32_ERRORCTX_PIPE    11
#define W32_ERRORCTX_LINK    12
#define W32_ERRORCTX_SYMLINK 13

/* Set 'errno' from GetLastError()
 * Always returns '-1'
 * @param: context: One of 'W32_ERRORCTX_*' */
extern int w32_seterror(int context);
#else
#define FD_MAX 65536
#endif


#endif /* !GUARD_LIB_SRC_LIBC_COMMON_H */
