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

/* Declare everything with hidden visibility. */
#pragma GCC visibility push("hidden")

#ifdef __PE__
int __start(void) __attribute__((used,alias("_start")));
#endif


#define __UNKNOWN_APP    0
#define __CONSOLE_APP    1
#define __GUI_APP        2
void __set_app_type(int);
void _controlfp(unsigned a, unsigned b);

#pragma comment(lib,"msvcrt")

typedef struct {
	int newmode;
} _startupinfo;

void __getmainargs(int *pargc, char ***pargv, char ***penv, int globb, _startupinfo*);
int main(int argc, char **argv, char **env);

extern void exit(int exitcode) __attribute__((noreturn));

void _start(void)
	__attribute__((used,noreturn))
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

#ifdef __DCC_VERSION__
/* Since this source file is directly linked against 'msvcrt',
 * clear all unused symbols except '_start' and '__start' in
 * order to greatly reduce the size of the object file. */
#pragma DCC delete_symbols(unused)
#endif


