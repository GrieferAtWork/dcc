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

#if __has_include_next(<stdio.h>)
#include_next <stdio.h>
#else
#include "__stdinc.h"

#undef size_t
typedef __SIZE_TYPE__ size_t;
#define NULL   __NULL__

#undef fpos_t
#if defined(_WIN32) || defined(__CYGWIN32__)
typedef __int64 fpos_t;
typedef struct {
	char   *__msvcrt_ptr;
	__int32 __msvcrt_cnt;
	char   *__msvcrt_base;
	__int32 __msvcrt_flag;
	__int32 __msvcrt_file;
	__int32 __msvcrt_charbuf;
	__int32 __msvcrt_bufsiz;
	char   *__msvcrt_tmpfname;
} *FILE;

#define BUFSIZ  512
#define EOF     (-1)

#define FILENAME_MAX    260
#define FOPEN_MAX       20
#define L_tmpnam        13
#define TMP_MAX         32767

#define _IOFBF          0x0000
#define _IOLBF          0x0040
#define _IONBF          0x0004

__IMP FILE *__iob_func(void);
#define stdin    (&__iob_func()[0])
#define stdout   (&__iob_func()[1])
#define stderr   (&__iob_func()[2])

#else
#error FIXME
#endif

#define SEEK_SET 0
#define SEEK_CUR 1
#define SEEK_END 2

__IMP int (remove)(char const *);
__IMP int (rename)(char const *,char const *);
__IMP __WUNUSED FILE *(tmpfile)(void);
__IMP char *(tmpnam)(char *);

__IMP int (fclose)(FILE *);
__IMP int (fflush)(FILE *);
__IMP __WUNUSED FILE *(fopen)(char const *,char const *);
__IMP FILE *(freopen)(char const *,char const *,FILE *);
__IMP void (setbuf)(FILE *,char *);
__IMP int (setvbuf)(FILE *,char *,int,size_t);

__IMP int (fprintf)(FILE *,char const *,...);
__IMP int (fscanf)(FILE *,char const *,...);
__IMP int (printf)(char const *,...);
__IMP int (scanf)(char const *,...);

__IMP int (sprintf)(char *,char const *,...);
__IMP int (sscanf)(char const *,char const *,...);
__IMP int (vfprintf)(FILE *,char const *,__builtin_va_list);
__IMP int (vprintf)(char const *,__builtin_va_list);
__IMP int (vsprintf)(char *,char const *,__builtin_va_list);

#if __STDLIB_VERSION__ >= 201112L
__IMP int (snprintf)(char *,size_t,const char *,...);
__IMP int (vfscanf)(FILE *,char const *,__builtin_va_list);
__IMP int (vscanf)(char const *,__builtin_va_list);
__IMP int (vsnprintf)(char *,size_t,char const *,__builtin_va_list);
__IMP int (vsscanf)(char const *,char const *,__builtin_va_list);
#endif

__IMP __WUNUSED int (fgetc)(FILE *);
__IMP char *(fgets)(char *,int,FILE *);
__IMP int (fputc)(int,FILE *);
__IMP int (fputs)(char const *,FILE *);
__IMP __WUNUSED int (getc)(FILE *);
__IMP __WUNUSED int (getchar)(void);
__IMP char *(gets)(char *);
__IMP int (putc)(int,FILE *);
__IMP int (putchar)(int,FILE *);
__IMP int (puts)(char const *);
__IMP int (ungetc)(int,FILE *);

__IMP size_t (fread)(void *,size_t,size_t,FILE *);
__IMP size_t (fwrite)(void const *,size_t,size_t,FILE *);

__IMP int (fgetpos)(FILE *,fpos_t *);
__IMP int (fseek)(FILE *,long,int);
__IMP int (fsetpos)(FILE *,fpos_t const *);
__IMP __WUNUSED long (ftell)(FILE *);
__IMP void (rewind)(FILE *);

__IMP void (clearerr)(FILE *);
__IMP __WUNUSED int (feof)(FILE *);
__IMP __WUNUSED int (ferror)(FILE *);
__IMP void (perror)(char const *);

#endif
