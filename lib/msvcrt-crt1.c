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

/* Compile with:
 * $ dcc -c -o msvcrt-crt1.o msvcrt-crt1.c
 */

/* Clear unused symbol not marked as 'used' from the resulting object file.
 * >> This will prevent symbols from 'msvcrt' from being included,
 *    thus reducing the object size considerably! */
#pragma comment(linker,"--clear-unused-obj")
#pragma comment(lib,"msvcrt")

/* Declare everything with hidden visibility. */
#pragma GCC visibility push("hidden")


#define __UNKNOWN_APP    0
#define __CONSOLE_APP    1
#define __GUI_APP        2
void __set_app_type(int);
void _controlfp(unsigned a, unsigned b);


typedef struct {
	int newmode;
} _startupinfo;

void __getmainargs(int *pargc, char ***pargv, char ***penv, int globb, _startupinfo*);
int main(int argc, char **argv, char **env);

extern void exit(int exitcode) __attribute__((noreturn));

__attribute__((
	used, /* Must mark as 'used' to not delete
	       * this symbol from the object file
	       * NOTE: Symbol are only being deleted because of
	       *       the '--clear-unused-obj' comment above! */
	noreturn
))
#ifdef __PE__
void __start(void)
#else
void _start(void)
#endif
{
	int argc; char **argv; char **env; int ret;
	_startupinfo start_info = {0};
#ifdef _WIN32
	/* TODO: Register initial SEH handler. */
#endif

	_controlfp(0x10000, 0x30000);
	__set_app_type(__CONSOLE_APP);
	__getmainargs(&argc,&argv,&env,0,&start_info);

	ret = main(argc,argv,env);
	exit(ret);
}

#pragma GCC visibility pop
