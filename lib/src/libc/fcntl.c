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
#ifndef GUARD_LIB_SRC_LIBC_FCNTL_C
#define GUARD_LIB_SRC_LIBC_FCNTL_C 1
#undef _FILE_OFFSET_BITS

#include "common.h"

#include <bits/types.h>
#include <bits/fcntl.h>
#include <fcntl.h>

#ifdef _WIN32
#include <wchar.h>
#include <Windows.h>
#include <errno.h>
#include <ioapiset.h>
#include <unistd.h>

DECL_BEGIN

static DWORD const oflag_cdisp[8] = {
 /* [0]                      = */OPEN_EXISTING,
 /* [O_CREAT]                = */CREATE_ALWAYS,
 /* [O_EXCL]                 = */OPEN_EXISTING,
 /* [O_EXCL|O_CREAT]         = */CREATE_NEW,
 /* [O_TRUNC]                = */TRUNCATE_EXISTING,
 /* [O_TRUNC|O_CREAT]        = */OPEN_ALWAYS,
 /* [O_TRUNC|O_EXCL]         = */TRUNCATE_EXISTING,
 /* [O_TRUNC|O_EXCL|O_CREAT] = */CREATE_NEW,
};

#define OFLAG_GET_ACCESS(f) (__O_ACCESS_FLAGS|(((f)&__O_ACCESS_MASK) >> __O_ACCESS_SHIFT))
#define OFLAG_GET_DISP(f)   (__O_DISP_FLAGS|oflag_cdisp[((f) >> __O_DISP_SHIFT)&(__O_DISP_MASK >> __O_DISP_SHIFT)])
#define OFLAG_GET_ATTR(f)   \
 (__O_ATTR_FLAGS|((((f)&__O_ATTR_MASK) >> __O_ATTR_SHIFT) << __O_ATTR_RSHIFT)|\
  __O_ATTR2_FLAGS|((((f)&__O_ATTR2_MASK) >> __O_ATTR2_SHIFT) << __O_ATTR2_RSHIFT))
#if __O_SHARE_MASK
#define OFLAG_GET_SHARE(f)  (__O_SHARE_FLAGS|((((f)&__O_SHARE_MASK) >> __O_SHARE_SHIFT) << __O_SHARE_RSHIFT))
#else
#define OFLAG_GET_SHARE(f)   __O_SHARE_FLAGS
#endif

PUBLIC int (fcntl)(int fd, int cmd, ...) {
 switch (cmd) {

 case F_DUPFD: return dup(fd);

 {
  DWORD flags;
 case F_GETFD:
  if unlikely(!GetHandleInformation(W32_FTOH(fd),&flags)) goto err;
  return (int)~(flags&HANDLE_FLAG_INHERIT);
 } break;

 {
  __builtin_va_list args;
 case F_SETFD:
  __builtin_va_start(args,cmd);
  if unlikely(!SetHandleInformation(W32_FTOH(fd),HANDLE_FLAG_INHERIT,
            ~(__builtin_va_arg(args,int)&FD_CLOEXEC))) goto err;
  return 0;
 } break;

 default: break;
 }
 errno = ENOSYS;
 return -1;
err:
 return w32_seterror(W32_ERRORCTX_FCNTL);
}

static int (open_impl)(int fd, char const *file, int oflag, mode_t mode) {
 return openat(AT_FDCWD,file,oflag);
 wchar_t *wname,buf[W32_FILENAME_BUFSIZE];
 int result; DWORD attr = OFLAG_GET_ATTR(oflag);
 if ((oflag&O_CREAT) && !(mode&0444))
      attr |= FILE_ATTRIBUTE_READONLY;
 result = W32_HTOF(CreateFileA(file,OFLAG_GET_ACCESS(oflag),
                               OFLAG_GET_SHARE(oflag),NULL,
                               OFLAG_GET_DISP(oflag),attr,NULL));
 if (result <= FD_MAX) return result;
 wname = w32_allocfilenameat(buf,fd,file);
 result = W32_HTOF(CreateFileW(wname,OFLAG_GET_ACCESS(oflag),
                               OFLAG_GET_SHARE(oflag),NULL,
                               OFLAG_GET_DISP(oflag),attr,NULL));
 w32_freefilename(wname,buf);
 if (result <= FD_MAX) return result;
 return w32_seterror(W32_ERRORCTX_OPEN);
}


PUBLIC int (open64)(char const *file, int oflag, ...) [[alias("open")]];
PUBLIC int (open)(char const *file, int oflag, ...) {
 int result;
 if (oflag&O_CREAT) {
  __builtin_va_list args;
  __builtin_va_start(args,oflag);
  result = open_impl(AT_FDCWD,file,oflag,
                     __builtin_va_arg(args,mode_t));
  __builtin_va_end(args);
 } else {
  result = open_impl(AT_FDCWD,file,oflag,0644);
 }
 return result;
}

PUBLIC int (creat64)(char const *file, __mode_t mode) [[alias("creat")]];
PUBLIC int (creat)(char const *file, __mode_t mode) {
 return open(file,O_CREAT|O_WRONLY|O_TRUNC,mode);
}


PUBLIC int (openat64)(int fd, char const *file, int oflag, ...) [[alias("openat")]];
PUBLIC int (openat)(int fd, char const *file, int oflag, ...) {
 int result;
 if (oflag&O_CREAT) {
  __builtin_va_list args;
  __builtin_va_start(args,oflag);
  result = open_impl(fd,file,oflag,
                     __builtin_va_arg(args,mode_t));
  __builtin_va_end(args);
 } else {
  result = open_impl(fd,file,oflag,0644);
 }
 return result;
}



PUBLIC int (lockf64)(int fd, int cmd, __off64_t len) {
 errno = ENOSYS; /* TODO */
 return -1;
}
PUBLIC int (lockf)(int fd, int cmd, __off_t len) {
 return lockf64(fd,cmd,len);
}

PUBLIC int (posix_fadvise64)(int fd, __off64_t offset, __off64_t len, int advise) {
 errno = ENOSYS; /* TODO */
 return -1;
}
PUBLIC int (posix_fadvise)(int fd, __off_t offset, __off_t len, int advise) {
 return posix_fadvise64(fd,offset,len,advise);
}
PUBLIC int (posix_fallocate64)(int fd, __off64_t offset, __off64_t len) {
 errno = ENOSYS; /* TODO */
 return -1;
}
PUBLIC int (posix_fallocate)(int fd, __off_t offset, __off_t len) {
 return posix_fallocate64(fd,offset,len);
}




DECL_END

#else
#error FIXME
#endif


#endif /* !GUARD_LIB_SRC_LIBC_FCNTL_C */
