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

#if __has_include_next(<dirent.h>)
#include_next <dirent.h>
#else
#include <__stdinc.h>

__STRICT_ANSI_HEADER

#include <bits/types.h>

#ifdef __USE_XOPEN
#ifndef __USE_FILE_OFFSET64
typedef __ino_t   ino_t;
#else
typedef __ino64_t ino_t;
#endif
#ifdef __USE_LARGEFILE64
typedef __ino64_t ino64_t;
#endif
#endif

#include <bits/dirent.h>

#if defined(__USE_MISC) && !defined(d_fileno)
#	define d_ino d_fileno
#endif

#ifdef _DIRENT_HAVE_D_NAMLEN
#	define _D_EXACT_NAMLEN(d) ((d)->d_namlen)
#	define _D_ALLOC_NAMLEN(d) (_D_EXACT_NAMLEN(d)+1)
#else
#	define _D_EXACT_NAMLEN(d)  __builtin_strlen((d)->d_name)
#ifdef _DIRENT_HAVE_D_RECLEN
#	define _D_ALLOC_NAMLEN(d) (((char *)(d)+(d)->d_reclen)-&(d)->d_name[0])
#else
#	define _D_ALLOC_NAMLEN(d) (sizeof((d)->d_name) > 1 ? sizeof((d)->d_name) : _D_EXACT_NAMLEN(d)+1)
#endif
#endif

#ifdef __USE_MISC
enum {
	DT_UNKNOWN = 0,
	DT_FIFO    = 1,
	DT_CHR     = 2,
	DT_DIR     = 4,
	DT_BLK     = 6,
	DT_REG     = 8,
	DT_LNK     = 10,
	DT_SOCK    = 12,
	DT_WHT     = 14
};
#	define DT_UNKNOWN  DT_UNKNOWN
#	define DT_FIFO     DT_FIFO
#	define DT_CHR      DT_CHR
#	define DT_DIR      DT_DIR
#	define DT_BLK      DT_BLK
#	define DT_REG      DT_REG
#	define DT_LNK      DT_LNK
#	define DT_SOCK     DT_SOCK
#	define DT_WHT      DT_WHT
#	define IFTODT(mode)    (((mode) & 0170000) >> 12)
#	define DTTOIF(dirtype) ((dirtype) << 12)
#endif

typedef struct __dirstream DIR;

#ifdef __CRT_MSVC
struct __dirstream {
	__INTPTR_TYPE__ __ds_hnd;
union{
	struct dirent   __ds_ent;
	struct dirent64 __ds_ent64;
};
};

__IMP __INTPTR_TYPE__ (__msvc_findfirst32)(char const *__name, void *__data) __asm__("_findfirst32");
__IMP int (__msvc_findnext32)(__INTPTR_TYPE__ __hnd, void *__data) __asm__("_findnext32");
__IMP __INTPTR_TYPE__ (__msvc_findfirst64)(char const *__name, void *__data) __asm__("_findfirst64");
__IMP int (__msvc_findnext64)(__INTPTR_TYPE__ __hnd, void *__data) __asm__("_findnext64");
__IMP int (__msvc_findclose)(__INTPTR_TYPE__ __hnd) __asm__("_findclose");

#ifdef __USE_FILE_OFFSET64
#	define __msvc_findfirst  __msvc_findfirst64
#	define __msvc_findnext   __msvc_findnext64
#else
#	define __msvc_findfirst  __msvc_findfirst32
#	define __msvc_findnext   __msvc_findnext32
#endif

__inline__ __WUNUSED DIR *(opendir)(char const *__name) {
	DIR *__r = (DIR *)__builtin_malloc(sizeof(DIR));
	if (__r) {
		__SIZE_TYPE__ __nlen; char *__ptr;
		__nlen = __builtin_strnlen(__name,259);
		__ptr = (char *)__builtin_memcpy(__r->__ds_ent.d_name,__name,__nlen*sizeof(char));
		while (__nlen--) { if (*__ptr == '/') *__ptr == '\\'; ++__ptr; }
		__ptr[0] = '\\';
		__ptr[1] = '*';
		__ptr[2] = '\0';
		__r->__ds_hnd = (__INTPTR_TYPE__)-1;
	}
	return __r;
}

__inline__ int (closedir)(DIR *__dirp) {
	if (__dirp) {
		__msvc_findclose(__dirp->__ds_hnd);
		__builtin_free(__dirp);
	}
	return 0;
}

#define __WIN32_FILE_ATTRIBUTE_DIRECTORY     0x00000010  
#define __WIN32_FILE_ATTRIBUTE_REPARSE_POINT 0x00000400  

__inline__ __WUNUSED struct dirent *(readdir)(DIR *__dirp) {
	if (__dirp->__ds_hnd == (__INTPTR_TYPE__)-1) {
		__dirp->__ds_hnd = __msvc_findfirst(__dirp->__ds_ent.d_name,
		                                   &__dirp->__ds_ent.__msvc_attrib);
		if (__dirp->__ds_hnd == (__INTPTR_TYPE__)-1) return __NULL__;
	} else if (__msvc_findnext(__dirp->__ds_hnd,&__dirp->__ds_ent.__msvc_attrib)) {
		return __NULL__;
	}
	if (__dirp->__ds_ent.__msvc_attrib&__WIN32_FILE_ATTRIBUTE_DIRECTORY)
	     __dirp->__ds_ent.d_type = DT_DIR;
	else if (__dirp->__ds_ent.__msvc_attrib&__WIN32_FILE_ATTRIBUTE_REPARSE_POINT)
	     __dirp->__ds_ent.d_type = DT_LNK;
	else __dirp->__ds_ent.d_type = DT_REG;
	return &__dirp->__ds_ent;
}

#ifdef __USE_LARGEFILE64
#ifdef __USE_FILE_OFFSET64
#define readdir64(__dirp) (struct dirent64 *)readdir((__dirp))
#else
__inline__ __WUNUSED struct dirent64 *(readdir64)(DIR *__dirp) {
	if (__dirp->__ds_hnd == (__INTPTR_TYPE__)-1) {
		__dirp->__ds_hnd = __msvc_findfirst64(__dirp->__ds_ent.d_name,
		                                     &__dirp->__ds_ent64.__msvc_attrib);
		if (__dirp->__ds_hnd == (__INTPTR_TYPE__)-1) return __NULL__;
	} else if (__msvc_findnext64(__dirp->__ds_hnd,&__dirp->__ds_ent64.__msvc_attrib)) {
		return __NULL__;
	}
	if (__dirp->__ds_ent.__msvc_attrib&__WIN32_FILE_ATTRIBUTE_DIRECTORY)
	     __dirp->__ds_ent.d_type = DT_DIR;
	else if (__dirp->__ds_ent.__msvc_attrib&__WIN32_FILE_ATTRIBUTE_REPARSE_POINT)
	     __dirp->__ds_ent.d_type = DT_LNK;
	else __dirp->__ds_ent.d_type = DT_REG;
	return &__dirp->__ds_ent64;
}
#endif
#endif

#undef __msvc_findfirst
#undef __msvc_findnext

#else
__IMP DIR *(opendir)(char const *__name);
__IMP int (closedir)(DIR *__dirp);
__IMP __WUNUSED struct dirent *(readdir)(DIR *__dirp)
#ifdef __USE_FILE_OFFSET64
	__asm__("readdir64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __WUNUSED struct dirent64 *(readdir64)(DIR *__dirp);
#endif
#endif

#ifdef __USE_XOPEN2K8
__IMP __CRT_UNSUPPORTED_MSVC DIR *(fdopendir)(int __fd);
#endif

#ifdef __USE_POSIX
__IMP __CRT_UNSUPPORTED_MSVC
int (readdir_r)(DIR *__restrict __dirp,
                struct dirent *__restrict __entry,
                struct dirent **__restrict __result)
#ifdef __USE_FILE_OFFSET64
	__asm__("readdir64_r")
#endif
;

#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC
int (readdir64_r)(DIR *__restrict __dirp,
                  struct dirent64 *__restrict __entry,
                  struct dirent64 **__restrict __result);
#endif
#endif

__IMP __CRT_UNSUPPORTED_MSVC void (rewinddir)(DIR *__dirp);

#if defined(__USE_MISC) || defined(__USE_XOPEN)
__IMP __CRT_UNSUPPORTED_MSVC void (seekdir)(DIR *__dirp, long int __pos);
__IMP __CRT_UNSUPPORTED_MSVC long int (telldir)(DIR *__dirp);
#endif

#ifdef __USE_XOPEN2K8
__IMP __CRT_UNSUPPORTED_MSVC int (dirfd)(DIR *__dirp);
#if defined(__OPTIMIZE__) && defined(_DIR_dirfd)
#	define dirfd(dirp)  _DIR_dirfd(dirp)
#endif

#ifdef __USE_MISC
#ifndef MAXNAMLEN
#ifdef __CRT_MSVC
#	define MAXNAMLEN    260
#else /* CRT... */
#if __has_include(<bits/posix1_lim.h>)
#	include <bits/posix1_lim.h>
#endif
#ifdef NAME_MAX
#	define MAXNAMLEN    NAME_MAX
#else
#	define MAXNAMLEN    255
#endif
#endif /* !CRT... */
#endif /* !MAXNAMLEN */
#endif /* __USE_MISC */

typedef __SIZE_TYPE__ size_t;

__IMP __CRT_UNSUPPORTED_MSVC
int (scandir)(char const *__restrict __dir,
              struct dirent ***__restrict __namelist,
              int (*__selector) (struct dirent const *),
              int (*__cmp) (struct dirent const **,
                            struct dirent const **))
#ifdef __USE_FILE_OFFSET64
	__asm__("scandir64")
#endif
;

#if defined(__USE_GNU) && defined(__USE_LARGEFILE64)
__IMP __CRT_UNSUPPORTED_MSVC
int (scandir64)(char const *__restrict __dir,
                struct dirent64 ***__restrict __namelist,
                int (*__selector) (struct dirent64 const *),
                int (*__cmp) (struct dirent64 const **,
                              struct dirent64 const **));
#endif

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC
int (scandirat)(int __dfd, char const *__restrict __dir,
                struct dirent ***__restrict __namelist,
                int (*__selector) (struct dirent const *),
                int (*__cmp) (struct dirent const **,
                              struct dirent const **))
#ifdef __USE_FILE_OFFSET64
	__asm__("scandirat64")
#endif
;

#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC
int (scandirat64)(int __dfd, char const *__restrict __dir,
                  struct dirent64 ***__restrict __namelist,
                  int (*__selector) (struct dirent64 const *),
                  int (*__cmp) (struct dirent64 const **,
                                struct dirent64 const **));
#endif
#endif

__IMP __CRT_UNSUPPORTED_MSVC
int (alphasort)(struct dirent const **__e1,
                struct dirent const **__e2)
#ifdef __USE_FILE_OFFSET64
	__asm__("alphasort64")
#endif
;
#if defined(__USE_GNU) && defined(__USE_LARGEFILE64)
__IMP __CRT_UNSUPPORTED_MSVC
int (alphasort64)(struct dirent64 const **__e1,
                  struct dirent64 const **__e2);
#endif
#endif /* __USE_XOPEN2K8 */

#ifdef __USE_MISC
__IMP __CRT_UNSUPPORTED_MSVC
__ssize_t (getdirentries)(int __fd, char *__restrict __buf,
                          size_t __nbytes, __off_t *__restrict __basep)
#ifdef __USE_FILE_OFFSET64
	__asm__("getdirentries64")
#endif
;

#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC
__ssize_t (getdirentries64)(int __fd, char *__restrict __buf,
                            size_t __nbytes, __off64_t *__restrict __basep);
#endif
#endif /* __USE_MISC */

#ifdef __USE_GNU
__IMP __CRT_UNSUPPORTED_MSVC
int (versionsort)(struct dirent const **__e1,
                  struct dirent const **__e2)
#ifdef __USE_FILE_OFFSET64
	__asm__("versionsort64")
#endif
;
#ifdef __USE_LARGEFILE64
__IMP __CRT_UNSUPPORTED_MSVC
int (versionsort64)(struct dirent64 const **__e1,
                    struct dirent64 const **__e2);
#endif
#endif /* __USE_GNU */

#endif /* !include_next... */
