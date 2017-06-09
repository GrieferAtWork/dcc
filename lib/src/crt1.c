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
/* Compile with: $ dcc -nostdlib -c -o crt1.o crt1.c */

#pragma comment(lib,"msvcrt")

/* Declare everything with hidden visibility. */
#pragma GCC visibility push("hidden")

#include <__stdinc.h>


#define __UNKNOWN_APP    0
#define __CONSOLE_APP    1
#define __GUI_APP        2

typedef struct {
 int newmode;
} _startupinfo;

int main(int argc, char **argv, char **env);

__IMP void __set_app_type(int);
__IMP void _controlfp(unsigned a, unsigned b);
__IMP void __getmainargs(int *pargc, char ***pargv, char ***penv, int globb, _startupinfo*);
__IMP [[noreturn]] void exit(int exitcode);

[[noreturn,alias("_start")]] void __start(void);
[[noreturn]] void _start(void) {
 int argc; char **argv; char **env; int ret;
 _startupinfo start_info = {0};

#ifdef _WIN32
 /* TODO: Register initial SEH handler. */
#endif

 _controlfp(0x10000,0x30000);
 __set_app_type(__CONSOLE_APP);
 __getmainargs(&argc,&argv,&env,0,&start_info);

#if !defined(NDEBUG) && (defined(_WIN32) || defined(__CYGWIN32__)) && defined(__i386__)
 { extern void __dcc_dbg_init_exc_tracebacks(void);
   __dcc_dbg_init_exc_tracebacks();
 }
#endif

 ret = main(argc,argv,env);
 exit(ret);
}

#pragma GCC visibility pop
