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
#pragma once
#pragma GCC system_header

#ifndef __has_include_next
#define __has_include_next(x) 0
#endif

#if __has_include_next(<direct.h>)
#include_next <direct.h>
#else
#include <__stdinc.h>
#include <bits/types.h>


__IMP __CRT_UNSUPPORTED_GLIBC __CRT_UNSUPPORTED_KOS
__UINTPTR_TYPE__ (_beginthread)(void (__cdecl *__entry_point)(void *),
                                __UINT32_TYPE__ __stack_size, void *__arg);
__IMP __CRT_UNSUPPORTED_GLIBC __CRT_UNSUPPORTED_KOS void (_endthread)(void);

__IMP __CRT_UNSUPPORTED_GLIBC __CRT_UNSUPPORTED_KOS
__UINTPTR_TYPE__ (_beginthreadex)(void *__security, __UINT32_TYPE__ __stack_size,
                                  __UINT32_TYPE__ (__stdcall *__entry_point)(void *),
                                  void * __arg, __UINT32_TYPE__ _InitFlag,
                                  __UINT32_TYPE__ *__thread_addr);
__IMP __CRT_UNSUPPORTED_GLIBC __CRT_UNSUPPORTED_KOS
void (_endthreadex)(__UINT32_TYPE__ __return_value);

__IMP _Noreturn void (exit)(int __code);
__IMP _Noreturn void (_exit)(int __code);
__IMP _Noreturn void (abort)(void);


#ifdef __CRT_MSVC
#define __PROCESS_FUN(x) __asm__("_" x)
#else
#define __PROCESS_FUN(x)
#endif

/* MODE values for 'spawnxx'. */
#define P_WAIT         0
#define P_NOWAIT       1
#define P_OVERLAY      2
#define OLD_P_OVERLAY  P_OVERLAY
#define P_NOWAITO      3
#define P_DETACH       4

/* Action codes for 'cwait'. */
#define WAIT_CHILD      0
#define WAIT_GRANDCHILD 1

#if !defined(__CRT_MSVC) || __SIZEOF_POINTER__ == __SIZEOF_INT__
#define __PROC_RETURN __pid_t
#else
#define __PROC_RETURN __INTPTR_TYPE__
#endif
__IMP __PROC_RETURN (cwait)(int *__term_status, __PROC_RETURN __proc_handle, int __action) __PROCESS_FUN("cwait");
__IMP __PROC_RETURN (execl)(char const *__path, char const *__arg0, ...) __PROCESS_FUN("execl");
__IMP __PROC_RETURN (execle)(char const *__path, char const *__arg0, ...) __PROCESS_FUN("execle");
__IMP __PROC_RETURN (execlp)(char const *__path, char const *__arg0, ...) __PROCESS_FUN("execlp");
__IMP __PROC_RETURN (execlpe)(char const *__path, char const *__arg0, ...) __PROCESS_FUN("execlpe");
__IMP __PROC_RETURN (execv)(char const *__path, char *const __argv[]) __PROCESS_FUN("execv");
__IMP __PROC_RETURN (execve)(char const *__path, char *const __argv[], char *const __envp[]) __PROCESS_FUN("execve");
__IMP __PROC_RETURN (execvp)(char const *__path, char *const __argv[]) __PROCESS_FUN("execvp");
__IMP __PROC_RETURN (execvpe)(char const *__path, char *const __argv[], char *const __envp[]) __PROCESS_FUN("execvpe");
__IMP __PROC_RETURN (spawnl)(int __mode, char const *__path, char const *__arg0, ...) __PROCESS_FUN("spawnl");
__IMP __PROC_RETURN (spawnle)(int __mode, char const *__path, char const *__arg0, ...) __PROCESS_FUN("spawnle");
__IMP __PROC_RETURN (spawnlp)(int __mode, char const *__path, char const *__arg0, ...) __PROCESS_FUN("spawnlp");
__IMP __PROC_RETURN (spawnlpe)(int __mode, char const *__path, char const *__arg0, ...) __PROCESS_FUN("spawnlpe");
__IMP __PROC_RETURN (spawnv)(int __mode, char const *__path, char *const __argv[]) __PROCESS_FUN("spawnv");
__IMP __PROC_RETURN (spawnve)(int __mode, char const *__path, char *const __argv[], char *const __envp[]) __PROCESS_FUN("spawnve");
__IMP __PROC_RETURN (spawnvp)(int __mode, char const *__path, char *const __argv[]) __PROCESS_FUN("spawnvp");
__IMP __PROC_RETURN (spawnvpe)(int __mode, char const *__path, char *const __argv[], char *const __envp[]) __PROCESS_FUN("spawnvpe");
__IMP __pid_t (getpid)(void) __PROCESS_FUN("getpid");
#undef __PROC_RETURN

#undef __PROCESS_FUN
#endif
