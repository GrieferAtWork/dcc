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
#ifndef GUARD_LIB_SRC_LIBC_UNISTD_C
#define GUARD_LIB_SRC_LIBC_UNISTD_C 1
#undef _FILE_OFFSET_BITS
#define _GNU_SOURCE

#include "common.h"

#include <bits/types.h>
#include <assert.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#ifdef _WIN32

#include <wchar.h>
#include <errno.h>
#include <ioapiset.h>
#include <Windows.h>

DECL_BEGIN

PUBLIC __WUNUSED int (dup)(int fd) {
 HANDLE result;
 if (!DuplicateHandle(GetCurrentProcess(),W32_FTOH(fd),
                      GetCurrentProcess(),&result,
                      0,TRUE,DUPLICATE_SAME_ACCESS)
     ) return w32_seterror(W32_ERRORCTX_DUP);
 return W32_HTOF(result);
}

static DWORD const std_handles_ids[3] = {
 /* [STDIN_FILENO ] = */STD_INPUT_HANDLE,
 /* [STDOUT_FILENO] = */STD_OUTPUT_HANDLE,
 /* [STDERR_FILENO] = */STD_ERROR_HANDLE,
};
static void *std_handles[3] = {
 /* [STDIN_FILENO ] = */INVALID_HANDLE_VALUE,
 /* [STDOUT_FILENO] = */INVALID_HANDLE_VALUE,
 /* [STDERR_FILENO] = */INVALID_HANDLE_VALUE,
};
void *w32_stdhandle(int fd) {
 void *result;
 assert(fd >= STDIN_FILENO && fd <= STDERR_FILENO);
 if ((result = std_handles[fd]) > (void *)FD_MAX) {
  /* Lazily initialize the std handle buffer. */
  void *newresult = GetStdHandle(std_handles_ids[fd]);
  result = __sync_val_compare_and_swap(&std_handles[fd],result,newresult);
 }
 return result;
}

PUBLIC int (dup2)(int fdsrc, int fddst) {
 if ((unsigned int)fddst <= STDERR_FILENO) {
  if unlikely(!SetStdHandle(std_handles_ids[fddst],W32_FTOH(fdsrc)))
     return w32_seterror(W32_ERRORCTX_DUP2);
  return 0;
 }
 errno = EBADF;
 return -1;
}

PUBLIC int (dup3)(int fdsrc, int fddst, int flags) {
 if ((unsigned int)fddst <= STDERR_FILENO) {
  HANDLE hsrc = W32_FTOH(fdsrc);
  if unlikely(!SetStdHandle(std_handles_ids[fddst],hsrc))
     return w32_seterror(W32_ERRORCTX_DUP2);
  if unlikely(!SetHandleInformation(hsrc,HANDLE_FLAG_INHERIT,
                                  ~(flags&FD_CLOEXEC))
              ) return w32_seterror(W32_ERRORCTX_FCNTL);
  return 0;
 }
 errno = EBADF;
 return -1;
}

PUBLIC __off_t (lseek)(int fd, __off_t offset, int whence) {
 DWORD result = SetFilePointer(W32_FTOH(fd),offset,NULL,whence);
 if (result == INVALID_SET_FILE_POINTER &&
     GetLastError()) return (__off_t)w32_seterror(W32_ERRORCTX_LSEEK);
 return (__off_t)result;
}
PUBLIC __off64_t (lseek64)(int fd, __off64_t offset, int whence) {
 DWORD result; LONG highoffset = offset >> 32;
 result = SetFilePointer(W32_FTOH(fd),(LONG)offset,&highoffset,whence);
 if (result == INVALID_SET_FILE_POINTER &&
     GetLastError()) return (__off64_t)w32_seterror(W32_ERRORCTX_LSEEK);
 return (__off64_t)result | (__off64_t)highoffset << 32;
}

PUBLIC int (ftruncate64)(int fd, __off64_t length) {
 DWORD result; LONG highoffset = length >> 32;
 LONG old_offlo,old_offhi = 0; HANDLE hnd = W32_FTOH(fd);
 old_offlo = SetFilePointer(hnd,(LONG)length,&old_offhi,SEEK_CUR);
 if (old_offlo == INVALID_SET_FILE_POINTER && GetLastError()) goto err_seek;
 result = SetFilePointer(hnd,(LONG)length,&highoffset,SEEK_SET);
 if (result == INVALID_SET_FILE_POINTER && GetLastError()) goto err_seek;
 if (!SetEndOfFile(hnd)) return w32_seterror(W32_ERRORCTX_TRUNCATE);
 result = SetFilePointer(hnd,(LONG)old_offlo,&old_offhi,SEEK_SET);
 if (result == INVALID_SET_FILE_POINTER && GetLastError()) goto err_seek;
err_seek: return w32_seterror(W32_ERRORCTX_LSEEK);
}
PUBLIC int (ftruncate)(int fd, __off_t length) {
 return ftruncate64(fd,(__off64_t)length);
}

PUBLIC int (truncate64)(char const *file, __off64_t length) {
 int result,fd = open(file,O_WRONLY);
 if (fd < 0) return fd;
 result = ftruncate64(fd,length);
 if (close(fd)) result = -1;
 return result;
}
PUBLIC int (truncate)(char const *file, __off_t length) {
 return truncate64(file,(__off64_t)length);
}


PUBLIC int (close)(int fd) {
 if (!CloseHandle(W32_FTOH(fd)))
     return w32_seterror(W32_ERRORCTX_CLOSE);
 return 0;
}

PUBLIC ssize_t (read)(int fd, void *buf, size_t nbytes) {
 DWORD result;
 if (!ReadFile(W32_FTOH(fd),buf,nbytes,&result,NULL))
      return (ssize_t)w32_seterror(W32_ERRORCTX_READ);
 return (ssize_t)result;
}
PUBLIC ssize_t (write)(int fd, void const *buf, size_t nbytes) {
 DWORD result;
 if (!WriteFile(W32_FTOH(fd),buf,nbytes,&result,NULL))
      return (ssize_t)w32_seterror(W32_ERRORCTX_WRITE);
 return (ssize_t)result;
}
PUBLIC ssize_t (pread)(int fd, void *buf, size_t nbytes, __off_t offset) {
 DWORD result;
 OVERLAPPED overlap;
 memset(&overlap,0,sizeof(overlap));
 overlap.Offset = offset;
 if (!ReadFile(W32_FTOH(fd),buf,nbytes,&result,&overlap))
      return (ssize_t)w32_seterror(W32_ERRORCTX_READ);
 return (ssize_t)result;
}
PUBLIC ssize_t (pwrite)(int fd, void const *buf, size_t nbytes, __off_t offset) {
 DWORD result;
 OVERLAPPED overlap;
 memset(&overlap,0,sizeof(overlap));
 overlap.Offset = offset;
 if (!WriteFile(W32_FTOH(fd),buf,nbytes,&result,&overlap))
      return (ssize_t)w32_seterror(W32_ERRORCTX_WRITE);
 return (ssize_t)result;
}
PUBLIC ssize_t (pread64)(int fd, void *buf, size_t nbytes, __off64_t offset) {
 DWORD result;
 OVERLAPPED overlap;
 memset(&overlap,0,sizeof(overlap));
 overlap.Offset     = (DWORD)(offset);
 overlap.OffsetHigh = (DWORD)(offset >> 32);
 if (!ReadFile(W32_FTOH(fd),buf,nbytes,&result,&overlap))
      return (ssize_t)w32_seterror(W32_ERRORCTX_READ);
 return (ssize_t)result;
}
PUBLIC ssize_t (pwrite64)(int fd, void const *buf, size_t nbytes, __off64_t offset) {
 DWORD result;
 OVERLAPPED overlap;
 memset(&overlap,0,sizeof(overlap));
 overlap.Offset     = (DWORD)(offset);
 overlap.OffsetHigh = (DWORD)(offset >> 32);
 if (!WriteFile(W32_FTOH(fd),buf,nbytes,&result,&overlap))
      return (ssize_t)w32_seterror(W32_ERRORCTX_WRITE);
 return (ssize_t)result;
}

PUBLIC int (pipe)(int pipedes[2]) {
 HANDLE hread,hwrite;
 if unlikely(!CreatePipe(&hread,&hwrite,NULL,0x400))
    return w32_seterror(W32_ERRORCTX_PIPE);
 pipedes[0] = W32_HTOF(hread);
 pipedes[1] = W32_HTOF(hwrite);
 return 0;
}
PUBLIC int (pipe2)(int pipedes[2], int flags) {
 HANDLE hread,hwrite;
 if unlikely(!CreatePipe(&hread,&hwrite,NULL,0x400))
    return w32_seterror(W32_ERRORCTX_PIPE);
 if unlikely(!SetHandleInformation(hread,HANDLE_FLAG_INHERIT,~(flags&FD_CLOEXEC))) goto err;
 if unlikely(!SetHandleInformation(hwrite,HANDLE_FLAG_INHERIT,~(flags&FD_CLOEXEC))) goto err;
 pipedes[0] = W32_HTOF(hread);
 pipedes[1] = W32_HTOF(hwrite);
 return 0;
err: w32_seterror(W32_ERRORCTX_FCNTL);
 CloseHandle(hread);
 CloseHandle(hwrite);
 return -1;
}

PUBLIC int (syncfs)(int fd) [[alias("fsync")]];
PUBLIC int (fdatasync)(int fd) [[alias("fsync")]];
PUBLIC int (fsync)(int fd) {
 if (!FlushFileBuffers(W32_FTOH(fd)))
     return w32_seterror(W32_ERRORCTX_FSYMC);
 return 0;
}
PUBLIC int (getpagesize)(void) {
#if defined(__i386__)
 return 4096;
#else
#error FIXME
#endif
}


static HANDLE
_hnd_spawnve(char const *path,
             char *const argv[],
             char *const envp[]) {
 /* TODO: CreateProcessA() */
}
static HANDLE
_hnd_wspawnve(wchar_t const *path,
              char *const argv[],
              char *const envp[]) {
 /* TODO: CreateProcessW() */
}



static int
_execve(char const *path,
        char *const argv[],
        char *const envp[]) {
 /* TODO: CreateProcessA() */
}
static int
_wexecve(wchar_t const *path,
         char *const argv[],
         char *const envp[]) {
 /* TODO: CreateProcessW() */
}

PUBLIC int (fexecve)(int fd, char *const argv[], char *const envp[]) {
 int result; wchar_t *wpath,buf[W32_FILENAME_BUFSIZE];
 wpath = w32_allocfdname(buf,fd);
 result = _wexecve(wpath,argv,envp);
 w32_freefilename(wpath,buf);
 return result;
}
PUBLIC int (execve)(char const *path, char *const argv[], char *const envp[]) {
 int result; wchar_t *wpath,buf[W32_FILENAME_BUFSIZE];
 if ((result = _execve(path,argv,envp)) == 0) return result;
 wpath = w32_allocfilename(buf,path);
 result = _wexecve(wpath,argv,envp);
 w32_freefilename(wpath,buf);
 return result;
}
PUBLIC int (execvpe)(char const *file, char *const argv[], char *const envp[]) {
 char *path = getenv("PATH");
 if (!path) path = "";
 /* TODO: Split 'path' by ';' */
 return execve(file,argv,envp);
}
PUBLIC int (execv)(char const *path, char *const argv[]) { return execve(path,argv,environ); }
PUBLIC int (execvp)(char const *file, char *const argv[]) { return execvpe(file,argv,environ); }
PUBLIC int (spawnv)(int mode, char const *path, char *const argv[]) { return spawnve(mode,path,argv,environ); }
PUBLIC int (spawnvp)(int mode, char const *file, char *const argv[]) { return spawnvpe(mode,file,argv,environ); }
PUBLIC int (spawnve)(int mode, char const *path, char *const argv[], char *const envp[]) { /* TODO */ }
PUBLIC int (spawnvpe)(int mode, char const *file, char *const argv[], char *const envp[]) { /* TODO */ }

PUBLIC int (execl)(char const *path, char const *arg, ...) { /* TODO */ }
PUBLIC int (execle)(char const *path, char const *arg, ...) { /* TODO */ }
PUBLIC int (execlp)(char const *file, char const *arg, ...) { /* TODO */ }
PUBLIC int (execlpe)(char const *file, char const *arg, ...) { /* TODO */ }
PUBLIC int (spawnl)(int mode, char const *path, char const *arg, ...) { /* TODO */ }
PUBLIC int (spawnle)(int mode, char const *path, char const *arg, ...) { /* TODO */ }
PUBLIC int (spawnlp)(int mode, char const *file, char const *arg, ...) { /* TODO */ }
PUBLIC int (spawnlpe)(int mode, char const *file, char const *arg, ...) { /* TODO */ }
PUBLIC __pid_t (getpid)(void) { return GetCurrentProcessId(); }

/* Directly link '_exit' against 'Kernel32.dll:ExitProcess' */
PUBLIC _Noreturn void (_exit)(int code) [[alias("ExitProcess")]];
PUBLIC _Noreturn void (_Exit)(int code) [[alias("_exit")]];

PUBLIC int (link)(char const *from, char const *to) {
 wchar_t *fnam,fbuf[W32_FILENAME_BUFSIZE];
 wchar_t *tnam,tbuf[W32_FILENAME_BUFSIZE];
 int result = 0;
 if (CreateHardLinkA(to,from,NULL)) goto done;
 fnam = w32_allocfilename(fbuf,from);
 tnam = w32_allocfilename(tbuf,to);
 if (!CreateHardLinkW(tnam,fnam,NULL))
      result = w32_seterror(W32_ERRORCTX_LINK);
 w32_freefilename(fnam,fbuf);
 w32_freefilename(tnam,tbuf);
done: return result;
}
PUBLIC int (symlink)(char const *from, char const *to) {
 return symlinkat(from,AT_FDCWD,to);
}
PUBLIC int (symlinkat)(char const *from, int tofd, char const *to) {
 wchar_t *fnam,fbuf[W32_FILENAME_BUFSIZE];
 wchar_t *tnam,tbuf[W32_FILENAME_BUFSIZE];
 DWORD link_flags; int result = 0;
 if (tofd == AT_FDCWD) {
  link_flags = GetFileAttributesA(from);
  if (link_flags == INVALID_FILE_ATTRIBUTES) goto widefix;
  link_flags = (link_flags&FILE_ATTRIBUTE_DIRECTORY)
              ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0;
  if (CreateSymbolicLinkA(to,from,link_flags)) goto done;
 }
widefix:
 fnam = w32_allocfilename(fbuf,from);
 tnam = w32_allocfilenameat(tbuf,tofd,to);
 link_flags = GetFileAttributesW(fnam);
 if (link_flags == INVALID_FILE_ATTRIBUTES) goto err;
 link_flags = (link_flags&FILE_ATTRIBUTE_DIRECTORY)
             ? SYMBOLIC_LINK_FLAG_DIRECTORY : 0;
 if (!CreateHardLinkW(tnam,fnam,NULL))
err:  result = w32_seterror(W32_ERRORCTX_SYMLINK);
 w32_freefilename(fnam,fbuf);
 w32_freefilename(tnam,tbuf);
done: return result;
}

PUBLIC ssize_t (readlink)(char const *__restrict path,
                          char *__restrict buf, size_t len) {
 return readlinkat(AT_FDCWD,path,buf,len);
}
PUBLIC int (unlink)(char const *name) { return unlinkat(AT_FDCWD,name,0); }
PUBLIC int (rmdir)(char const *name) { return unlinkat(AT_FDCWD,name,AT_REMOVEDIR); }

PUBLIC ssize_t (readlinkat)(int fd, char const *__restrict path,
                            char *__restrict buf, size_t len) {
 /* TODO: deemon implemented this before. */
 errno = ENOSYS;
 return -1;
}
PUBLIC int (unlinkat)(int fd, char const *name, int flag) {
 /* TODO */
 errno = ENOSYS;
 return -1;
}

PUBLIC int (gethostname)(char *name, size_t len) {
 DWORD dlen = (DWORD)len;
 if unlikely(!GetComputerNameA(name,&dlen))
    return w32_seterror(W32_ERRORCTX_NONE);
 return 0;
}
PUBLIC int (sethostname)(char const *name, size_t len) {
 if unlikely(!SetComputerNameA(strndupa(name,len)))
    return w32_seterror(W32_ERRORCTX_NONE);
 return 0;
}

PUBLIC int (getdomainname)(char *name, size_t len) {
 DWORD dlen = (DWORD)len;
 if unlikely(!GetComputerNameExA(ComputerNameDnsDomain,name,&dlen))
    return w32_seterror(W32_ERRORCTX_NONE);
 return 0;
}
PUBLIC int (setdomainname)(char const *name, size_t len) {
 if unlikely(!SetComputerNameExA(ComputerNameDnsDomain,strndupa(name,len)))
    return w32_seterror(W32_ERRORCTX_NONE);
 return 0;
}


PUBLIC char **__environ [[alias("environ")]];
PUBLIC char **environ = NULL;



DECL_END

#else
#error FIXME
#endif


#endif /* !GUARD_LIB_SRC_LIBC_UNISTD_C */
