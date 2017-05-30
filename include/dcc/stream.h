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
#ifndef GUARD_DCC_STREAM_H
#define GUARD_DCC_STREAM_H 1

#include "common.h"
#include "lexer.h"

#ifdef _WIN32
#include <Windows.h>
#else
#include <fcntl.h>
#include <unistd.h>
#endif


DCC_DECL_BEGIN

typedef uint32_t DCC(soff_t); /* File offset. */

#ifdef _WIN32
#define DCC_STREAM_OPEN_R(filename) \
 CreateFileA(filename,GENERIC_READ,\
             FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,\
             NULL,OPEN_EXISTING,FILE_ATTRIBUTE_NORMAL,NULL)
#define DCC_STREAM_OPEN_W(filename) \
 CreateFileA(filename,GENERIC_WRITE,\
             FILE_SHARE_READ|FILE_SHARE_WRITE|FILE_SHARE_DELETE,\
             NULL,CREATE_ALWAYS,FILE_ATTRIBUTE_NORMAL,NULL)
#define DCC_STREAM_CLOSE(fd) CloseHandle(fd)
#define DCC_STREAM_SEEK(fd,off,whence) (DCC(soff_t))SetFilePointer(fd,off,NULL,whence)
DCC_LOCAL ptrdiff_t dcc_win32_stream_read(DCC(stream_t) fd, void *p, size_t s) {
 DWORD result;
 if (!ReadFile(fd,p,s,&result,NULL)) return -1;
 return (ptrdiff_t)result;
}
DCC_LOCAL ptrdiff_t dcc_win32_stream_write(DCC(stream_t) fd, void const *p, size_t s) {
 DWORD result;
 if (!WriteFile(fd,p,s,&result,NULL)) return -1;
 return (ptrdiff_t)result;
}
#define DCC_STREAM_READ(fd,p,s)  dcc_win32_stream_read(fd,p,s)
#define DCC_STREAM_WRITE(fd,p,s) dcc_win32_stream_write(fd,p,s)
#else
#define DCC_STREAM_OPEN_R(filename)    open(filename,O_RDONLY)
#define DCC_STREAM_OPEN_W(filename)    open(filename,O_WRONLY|O_CREAT,0644)
#define DCC_STREAM_CLOSE(fd)           close(fd)
#define DCC_STREAM_SEEK(fd,off,whence) lseek(fd,off,whence)
#define DCC_STREAM_READ(fd,p,s)        read(fd,p,s)
#define DCC_STREAM_WRITE(fd,p,s)       write(fd,p,s)
#endif


DCC_LOCAL int
DCCStream_ReadAll(DCC(stream_t) fd, void *p, size_t s) {
 ptrdiff_t part;
 size_t total = 0;
 while ((part = DCC_STREAM_READ(fd,p,s)) > 0) {
  if ((size_t)part > s) part = s; /* Shouldn't happen, but lets be careful. */
  s                -= part;
  *(uintptr_t *)&p += part;
 }
 return total == s;
}

DCC_LOCAL int
DCCStream_WriteAll(DCC(stream_t) fd, void const *p, size_t s) {
 ptrdiff_t part;
 size_t total = 0;
 while ((part = DCC_STREAM_WRITE(fd,p,s)) > 0) {
  if ((size_t)part > s) part = s; /* Shouldn't happen, but lets be careful. */
  s                -= part;
  *(uintptr_t *)&p += part;
 }
 return total == s;
}

DCCFUN int DCCStream_PadSize(DCC(stream_t) fd, size_t n_bytes);
DCCFUN int DCCStream_PadAddr(DCC(stream_t) fd, DCC(soff_t) offset);

#ifdef DCC_PRIVATE_API
#define s_openr  DCC_STREAM_OPEN_R
#define s_openw  DCC_STREAM_OPEN_W
#define s_close  DCC_STREAM_CLOSE
#define s_seek   DCC_STREAM_SEEK
#define s_read   DCC_STREAM_READ
#define s_write  DCC_STREAM_WRITE
#define s_reada  DCCStream_ReadAll
#define s_writea DCCStream_WriteAll
#endif

DCC_DECL_END

#endif /* !GUARD_DCC_STREAM_H */
