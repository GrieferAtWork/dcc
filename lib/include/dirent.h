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
#include <bits/types.h>

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
#define DT_UNKNOWN  DT_UNKNOWN
#define DT_FIFO     DT_FIFO
#define DT_CHR      DT_CHR
#define DT_DIR      DT_DIR
#define DT_BLK      DT_BLK
#define DT_REG      DT_REG
#define DT_LNK      DT_LNK
#define DT_SOCK     DT_SOCK
#define DT_WHT      DT_WHT
#define IFTODT(mode)    (((mode) & 0170000) >> 12)
#define DTTOIF(dirtype) ((dirtype) << 12)
typedef struct __dirstream DIR;

#ifdef __CRT_MSVC

struct dirent {
#ifdef __USE_FILE_OFFSET64
 __ino64_t       d_ino;
#else
 __ino_t         d_ino;
#endif
union{
 unsigned char   d_type;
 unsigned        __msvc_attrib;
};
#ifdef __USE_FILE_OFFSET64
 __time64_t      __msvc_time_create;
 __time64_t      __msvc_time_access;
 __time64_t      __msvc_time_write;
 __UINT64_TYPE__ __msvc_size;
#else
 __time32_t      __msvc_time_create;
 __time32_t      __msvc_time_access;
 __time32_t      __msvc_time_write;
 __UINT32_TYPE__ __msvc_size;
#endif
 char            d_name[262];
};

struct dirent64 {
 __ino64_t       d_ino;
union{
 unsigned char   d_type;
 unsigned        __msvc_attrib;
};
 __time64_t      __msvc_time_create;
 __time64_t      __msvc_time_access;
 __time64_t      __msvc_time_write;
 __UINT64_TYPE__ __msvc_size;
 char            d_name[262];
};

struct __dirstream {
 __INTPTR_TYPE__ __ds_hnd;
union{
 struct dirent   __ds_ent;
 struct dirent64 __ds_ent64;
};
};

__inline__ __WUNUSED DIR *(opendir)(const char *__name);
__inline__ int (closedir)(DIR *__dirp);
__inline__ __WUNUSED struct dirent *(readdir)(DIR *__dirp);
#ifdef __USE_LARGEFILE64
#ifdef __USE_FILE_OFFSET64
[[__alias__("readdir")]]
#endif
__inline__ __WUNUSED struct dirent64 *(readdir64)(DIR *__dirp);
#endif

__IMP __INTPTR_TYPE__ (__msvc_findfirst32)(const char *__name, void *__data) __asm__("_findfirst32");
__IMP int (__msvc_findnext32)(__INTPTR_TYPE__ __hnd, void *__data) __asm__("_findnext32");
__IMP __INTPTR_TYPE__ (__msvc_findfirst64)(const char *__name, void *__data) __asm__("_findfirst64");
__IMP int (__msvc_findnext64)(__INTPTR_TYPE__ __hnd, void *__data) __asm__("_findnext64");
__IMP int (__msvc_findclose)(__INTPTR_TYPE__ __hnd) __asm__("_findclose");

#ifdef __USE_FILE_OFFSET64
#define __msvc_findfirst  __msvc_findfirst64
#define __msvc_findnext   __msvc_findnext64
#else
#define __msvc_findfirst  __msvc_findfirst32
#define __msvc_findnext   __msvc_findnext32
#endif


__inline__ DIR *(opendir)(const char *__name) {
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

__inline__ struct dirent *(readdir)(DIR *__dirp) {
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
#ifndef __USE_FILE_OFFSET64
__inline__ struct dirent64 *(readdir64)(DIR *__dirp) {
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
#error TODO
#endif

#endif
