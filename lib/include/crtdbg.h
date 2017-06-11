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

#include <__stdinc.h>

#if __has_include_next(<crtdbg.h>)
#include_next <crtdbg.h>
#else

//_CRTDBG_MAP_ALLOC
//defined(__CRT_MSVC)

#if !defined(NDEBUG) && defined(__CRT_MSVC)
/* Link debug CRT against functions from 'msvcrt.dll' */


#define _CRTDBG_ALLOC_MEM_DF        0x01
#define _CRTDBG_DELAY_FREE_MEM_DF   0x02
#define _CRTDBG_CHECK_ALWAYS_DF     0x04
#define _CRTDBG_RESERVED_DF         0x08
#define _CRTDBG_CHECK_CRT_DF        0x10
#define _CRTDBG_LEAK_CHECK_DF       0x20
#define _CRTDBG_CHECK_EVERY_16_DF   0x00100000
#define _CRTDBG_CHECK_EVERY_128_DF  0x00800000
#define _CRTDBG_CHECK_EVERY_1024_DF 0x04000000
#define _CRTDBG_CHECK_DEFAULT_DF    0
#define _CRTDBG_REPORT_FLAG        (-1)
#define _BLOCK_TYPE(block)         ((block) & 0xFFFF)
#define _BLOCK_SUBTYPE(block)      ((block) >> 16 & 0xFFFF)

#define _FREE_BLOCK      0
#define _NORMAL_BLOCK    1
#define _CRT_BLOCK       2
#define _IGNORE_BLOCK    3
#define _CLIENT_BLOCK    4
#define _MAX_BLOCKS      5

typedef void (__cdecl *_CRT_DUMP_CLIENT)(void *,__SIZE_TYPE__);

struct _CrtMemBlockHeader;
typedef struct _CrtMemState {
 struct _CrtMemBlockHeader *pBlockHeader;
 __SIZE_TYPE__ lCounts[_MAX_BLOCKS];
 __SIZE_TYPE__ lSizes[_MAX_BLOCKS];
 __SIZE_TYPE__ lHighWaterCount;
 __SIZE_TYPE__ lTotalCount;
} _CrtMemState;

#define _HOOK_ALLOC     1
#define _HOOK_REALLOC   2
#define _HOOK_FREE      3
typedef int (__cdecl *_CRT_ALLOC_HOOK)(int,void *,__SIZE_TYPE__,int,__MS_LONG,const unsigned char *,int);

typedef void *_HFILE;
typedef int (__cdecl *_CRT_REPORT_HOOK)(int,char *,int *);
typedef int (__cdecl *_CRT_REPORT_HOOKW)(int,__WCHAR_TYPE__ *,int *);

#define _CRT_WARN                0
#define _CRT_ERROR               1
#define _CRT_ASSERT              2
#define _CRT_ERRCNT              3
#define _CRTDBG_MODE_FILE      0x1
#define _CRTDBG_MODE_DEBUG     0x2
#define _CRTDBG_MODE_WNDW      0x4
#define _CRTDBG_REPORT_MODE    (-1)
#define _CRTDBG_INVALID_HFILE ((_HFILE)(__UINTPTR_TYPE__)-1)
#define _CRTDBG_HFILE_ERROR   ((_HFILE)(__UINTPTR_TYPE__)-2)
#define _CRTDBG_FILE_STDOUT   ((_HFILE)(__UINTPTR_TYPE__)-4)
#define _CRTDBG_FILE_STDERR   ((_HFILE)(__UINTPTR_TYPE__)-5)
#define _CRTDBG_REPORT_FILE   ((_HFILE)(__UINTPTR_TYPE__)-6)

__IMP _CRT_REPORT_HOOK (__cdecl _CrtGetReportHook)(void);
__IMP _CRT_REPORT_HOOK (__cdecl _CrtSetReportHook)(_CRT_REPORT_HOOK __pfnnewhook);
__IMP int (__cdecl _CrtSetReportHook2)(int __mode, _CRT_REPORT_HOOK __pfnnewhook);
__IMP int (__cdecl _CrtSetReportHookW2)(int __mode, _CRT_REPORT_HOOKW __pfnnewhook);
__IMP int (__cdecl _CrtSetReportMode)(int __reporttype, int __reportmode);
__IMP _HFILE (__cdecl _CrtSetReportFile)(int __reporttype, _HFILE __reportfile);
__IMP int (__cdecl _CrtDbgReport)(int __reporttype, const char *__filename, int __linenumber, const char *__modulename, const char *__format, ...);
__IMP __SIZE_TYPE__ (__cdecl _CrtSetDebugFillThreshold)(__SIZE_TYPE__ __newdebugfillthreshold);
__IMP int (__cdecl _CrtDbgReportW)(int __reporttype, const __WCHAR_TYPE__ *__filename, int __linenumber, const __WCHAR_TYPE__ *__modulename, const __WCHAR_TYPE__ *__format, ...);

__IMP extern __MS_LONG _crtBreakAlloc;
__IMP __MS_LONG (__cdecl _CrtSetBreakAlloc)(__MS_LONG __breakalloc);

__IMP __WUNUSED void *(__cdecl _malloc_dbg)(__SIZE_TYPE__ __size, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _calloc_dbg)(__SIZE_TYPE__ __count, __SIZE_TYPE__ __size, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _realloc_dbg)(void *__memory, __SIZE_TYPE__ __newsize, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _recalloc_dbg)(void *__memory, __SIZE_TYPE__ __numofelements, __SIZE_TYPE__ __sizeofelements, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _expand_dbg)(void *__memory, __SIZE_TYPE__ __newsize, int __blocktype, const char *__filename, int __linenumber);
__IMP void (__cdecl _free_dbg)(void *__memory, int __blocktype);
__IMP __SIZE_TYPE__ (__cdecl _msize_dbg)(void *__memory, int __blocktype);
__IMP __SIZE_TYPE__ (__cdecl _aligned_msize_dbg)(void *__memory, __SIZE_TYPE__ __alignment, __SIZE_TYPE__ __offset);
__IMP __WUNUSED void *(__cdecl _aligned_malloc_dbg)(__SIZE_TYPE__ __size, __SIZE_TYPE__ __alignment, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _aligned_realloc_dbg)(void *__memory, __SIZE_TYPE__ __newsize, __SIZE_TYPE__ __alignment, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _aligned_recalloc_dbg)(void *__memory, __SIZE_TYPE__ __numofelements, __SIZE_TYPE__ __sizeofelements, __SIZE_TYPE__ __alignment, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _aligned_offset_malloc_dbg)(__SIZE_TYPE__ __size, __SIZE_TYPE__ __alignment, __SIZE_TYPE__ __offset, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _aligned_offset_realloc_dbg)(void *__memory, __SIZE_TYPE__ __newsize, __SIZE_TYPE__ __alignment, __SIZE_TYPE__ __offset, const char *__filename, int __linenumber);
__IMP __WUNUSED void *(__cdecl _aligned_offset_recalloc_dbg)(void *__memory, __SIZE_TYPE__ __numofelements, __SIZE_TYPE__ __sizeofelements, __SIZE_TYPE__ __alignment, __SIZE_TYPE__ __offset, const char *__filename, int __linenumber);
__IMP void (__cdecl _aligned_free_dbg)(void *__memory);
__IMP __WUNUSED char *(__cdecl _strdup_dbg)(const char *__str, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED __WCHAR_TYPE__ *(__cdecl _wcsdup_dbg)(const __WCHAR_TYPE__ *__str, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED char *(__cdecl _tempnam_dbg)(const char *__dirname, const char *__fileprefix, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED __WCHAR_TYPE__ *(__cdecl _wtempnam_dbg)(const __WCHAR_TYPE__ *__dirname, const __WCHAR_TYPE__ *__fileprefix, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED char *(__cdecl _fullpath_dbg)(char * __fullpath, const char *__path, __SIZE_TYPE__ __sizeinbytes, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED __WCHAR_TYPE__ *(__cdecl _wfullpath_dbg)(__WCHAR_TYPE__ * __fullpath, const __WCHAR_TYPE__ *__path, __SIZE_TYPE__ _SizeInWords, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED char *(__cdecl _getcwd_dbg)(char *__dstbuf, int __sizeinbytes, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED __WCHAR_TYPE__ *(__cdecl _wgetcwd_dbg)(__WCHAR_TYPE__ *__dstbuf, int _SizeInWords, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED char *(__cdecl _getdcwd_dbg)(int __drive, char *__dstbuf, int __sizeinbytes, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED __WCHAR_TYPE__ *(__cdecl _wgetdcwd_dbg)(int __drive, __WCHAR_TYPE__ *__dstbuf, int _SizeInWords, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED char *(__cdecl _getdcwd_lk_dbg)(int __drive, char *__dstbuf, int __sizeinbytes, int __blocktype, const char *__filename, int __linenumber);
__IMP __WUNUSED __WCHAR_TYPE__ *(__cdecl _wgetdcwd_lk_dbg)(int __drive, __WCHAR_TYPE__ *__dstbuf, int _SizeInWords, int __blocktype, const char *__filename, int __linenumber);
__IMP /*errno_t*/int (__cdecl _dupenv_s_dbg)(char **__pbuffer, __SIZE_TYPE__ *__pbuffersizeinbytes, const char *__varname, int __blocktype, const char *__filename, int __linenumber);
__IMP /*errno_t*/int (__cdecl _wdupenv_s_dbg)(__WCHAR_TYPE__ **__pbuffer, __SIZE_TYPE__ *__pbuffersizeinwords, const __WCHAR_TYPE__ *__varname, int __blocktype, const char *__filename, int __linenumber);
#define _malloca_dbg _malloc_dbg
#define _freea_dbg   _free_dbg


__IMP _CRT_ALLOC_HOOK (__cdecl _CrtGetAllocHook)(void);
__IMP _CRT_ALLOC_HOOK (__cdecl _CrtSetAllocHook)(_CRT_ALLOC_HOOK __pfnnewhook);
__IMP extern int _crtDbgFlag;
__IMP int (__cdecl _CrtCheckMemory)(void);
__IMP int (__cdecl _CrtSetDbgFlag)(int __newflag);
__IMP void (__cdecl _CrtDoForAllClientObjects)(void (__cdecl *__pfn)(void *,void *), void *__context);
__IMP __WUNUSED int (__cdecl _CrtIsValidPointer)(void const *__ptr, unsigned int __bytes, int __readwrite);
__IMP __WUNUSED int (__cdecl _CrtIsValidHeapPointer)(void const *__heapptr);
__IMP int (__cdecl _CrtIsMemoryBlock)(void const *__memory, unsigned int __bytes, __MS_LONG *__requestnumber, char **__filename, int *__linenumber);
__IMP __WUNUSED int (__cdecl _CrtReportBlockType)(void const *__memory);
__IMP _CRT_DUMP_CLIENT (__cdecl _CrtGetDumpClient)(void);
__IMP _CRT_DUMP_CLIENT (__cdecl _CrtSetDumpClient)(_CRT_DUMP_CLIENT _PFnNewDump);
__IMP void (__cdecl _CrtMemCheckpoint)(_CrtMemState *__state);
__IMP int (__cdecl _CrtMemDifference)(_CrtMemState *__state, _CrtMemState const *__oldstate, _CrtMemState const *__newstate);
__IMP void (__cdecl _CrtMemDumpAllObjectsSince)(_CrtMemState const *__state);
__IMP void (__cdecl _CrtMemDumpStatistics)(_CrtMemState const *__state);
__IMP int (__cdecl _CrtDumpMemoryLeaks)(void);
__IMP int (__cdecl _CrtSetCheckCount)(int __checkcount);
__IMP int (__cdecl _CrtGetCheckCount)(void);



#define _ASSERT_EXPR(expr,msg) \
        (void)((expr) || \
               (1 != _CrtDbgReport(_CRT_ASSERT,__FILE__,__LINE__,__NULL__,"%s",msg)) || \
               (_CrtDbgBreak(),0))

#ifndef _ASSERT
#define _ASSERT(expr)   _ASSERT_EXPR((expr),__NULL__)
#endif
#ifndef _ASSERTE
#define _ASSERTE(expr)  _ASSERT_EXPR((expr),#expr)
#endif
#ifndef _ASSERT_BASE
#define _ASSERT_BASE    _ASSERT_EXPR
#endif

#if __has_builtin(__builtin_breakpoint)
#define _CrtDbgBreak __builtin_breakpoint
#else /* __builtin_breakpoint */
__IMP void __cdecl _CrtDbgBreak(void);
#endif /* !__builtin_breakpoint */



#if defined(_CRTDBG_MAP_ALLOC) && !defined(__INTELLISENSE__)
#define malloc(s)                            _malloc_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define calloc(c,s)                          _calloc_dbg(c,s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define realloc(p,s)                         _realloc_dbg(p,s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _recalloc(p,c,s)                     _recalloc_dbg(p,c,s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _expand(p,s)                         _expand_dbg(p,s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define free(p)                              _free_dbg(p,_NORMAL_BLOCK)
#define _msize(p)                            _msize_dbg(p,_NORMAL_BLOCK)
#define _aligned_msize(p,a,o)                _aligned_msize_dbg(p,a,o)
#define _aligned_malloc(s,a)                 _aligned_malloc_dbg(s,a,__FILE__,__LINE__)
#define _aligned_realloc(p,s,a)              _aligned_realloc_dbg(p,s,a,__FILE__,__LINE__)
#define _aligned_recalloc(p,c,s,a)           _aligned_recalloc_dbg(p,c,s,a,__FILE__,__LINE__)
#define _aligned_offset_malloc(s,a,o)        _aligned_offset_malloc_dbg(s,a,o,__FILE__,__LINE__)
#define _aligned_offset_realloc(p,s,a,o)     _aligned_offset_realloc_dbg(p,s,a,o,__FILE__,__LINE__)
#define _aligned_offset_recalloc(p,c,s,a,o)  _aligned_offset_recalloc_dbg(p,c,s,a,o,__FILE__,__LINE__)
#define _aligned_free(p)                     _aligned_free_dbg(p)
#define _malloca(s)                          _malloca_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _freea(p)                            _freea_dbg(p,_NORMAL_BLOCK)
#define _strdup(s)                           _strdup_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _wcsdup(s)                           _wcsdup_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _mbsdup(s)                           _strdup_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _tempnam(s1,s2)                      _tempnam_dbg(s1,s2,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _wtempnam(s1,s2)                     _wtempnam_dbg(s1,s2,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _fullpath(s1,s2,le)                  _fullpath_dbg(s1,s2,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _wfullpath(s1,s2,le)                 _wfullpath_dbg(s1,s2,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _getcwd(s,le)                        _getcwd_dbg(s,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _wgetcwd(s,le)                       _wgetcwd_dbg(s,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _getdcwd(d,s,le)                     _getdcwd_dbg(d,s,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _wgetdcwd(d,s,le)                    _wgetdcwd_dbg(d,s,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _dupenv_s(ps1,size,s2)               _dupenv_s_dbg(ps1,size,s2,_NORMAL_BLOCK,__FILE__,__LINE__)
#define _wdupenv_s(ps1,size,s2)              _wdupenv_s_dbg(ps1,size,s2,_NORMAL_BLOCK,__FILE__,__LINE__)
#define strdup(s)                            _strdup_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define wcsdup(s)                            _wcsdup_dbg(s,_NORMAL_BLOCK,__FILE__,__LINE__)
#define tempnam(s1,s2)                       _tempnam_dbg(s1,s2,_NORMAL_BLOCK,__FILE__,__LINE__)
#define getcwd(s,le)                         _getcwd_dbg(s,le,_NORMAL_BLOCK,__FILE__,__LINE__)
#endif  /* _CRTDBG_MAP_ALLOC */

#else
/* Originally, this was inside '#ifndef _DEBUG' */

#ifndef _ASSERT
#define _ASSERT(expr)   (void)0
#endif
#ifndef _ASSERTE
#define _ASSERTE(expr)  (void)0
#endif
#ifndef _ASSERT_EXPR
#define _ASSERT_EXPR(expr,expr_str) (void)0
#endif
#ifndef _ASSERT_BASE
#define _ASSERT_BASE _ASSERT_EXPR
#endif

#define _malloc_dbg(s,t,f,l)                         malloc(s)
#define _calloc_dbg(c,s,t,f,l)                       calloc(c,s)
#define _realloc_dbg(p,s,t,f,l)                      realloc(p,s)
#define _recalloc_dbg(p,c,s,t,f,l)                  _recalloc(p,c,s)
#define _expand_dbg(p,s,t,f,l)                      _expand(p,s)
#define _free_dbg(p,t)                               free(p)
#define _msize_dbg(p,t)                             _msize(p)
#define _aligned_msize_dbg(p,a,o)                   _aligned_msize(p,a,o)
#define _aligned_malloc_dbg(s,a,f,l)                _aligned_malloc(s,a)
#define _aligned_realloc_dbg(p,s,a,f,l)             _aligned_realloc(p,s,a)
#define _aligned_recalloc_dbg(p,c,s,a,f,l)          _aligned_recalloc(p,c,s,a)
#define _aligned_free_dbg(p)                        _aligned_free(p)
#define _aligned_offset_malloc_dbg(s,a,o,f,l)       _aligned_offset_malloc(s,a,o)
#define _aligned_offset_realloc_dbg(p,s,a,o,f,l)    _aligned_offset_realloc(p,s,a,o)
#define _aligned_offset_recalloc_dbg(p,c,s,a,o,f,l) _aligned_offset_recalloc(p,c,s,a,o)
#define _malloca_dbg(s,t,f,l)                       _malloca(s)
#define _freea_dbg(p,t)                             _freea(p)
#define _strdup_dbg(s,t,f,l)                        _strdup(s)
#define _wcsdup_dbg(s,t,f,l)                        _wcsdup(s)
#define _mbsdup_dbg(s,t,f,l)                        _mbsdup(s)
#define _tempnam_dbg(s1,s2,t,f,l)                   _tempnam(s1,s2)
#define _wtempnam_dbg(s1,s2,t,f,l)                  _wtempnam(s1,s2)
#define _fullpath_dbg(s1,s2,le,t,f,l)               _fullpath(s1,s2,le)
#define _wfullpath_dbg(s1,s2,le,t,f,l)              _wfullpath(s1,s2,le)
#define _getcwd_dbg(s,le,t,f,l)                     _getcwd(s,le)
#define _wgetcwd_dbg(s,le,t,f,l)                    _wgetcwd(s,le)
#define _getdcwd_dbg(d,s,le,t,f,l)                  _getdcwd(d,s,le)
#define _wgetdcwd_dbg(d,s,le,t,f,l)                 _wgetdcwd(d,s,le)
#define _getdcwd_lk_dbg(d,s,le,t,f,l)               _getdcwd(d,s,le)
#define _wgetdcwd_lk_dbg(d,s,le,t,f,l)              _wgetdcwd(d,s,le)
#define _dupenv_s_dbg(ps1,size,s2,t,f,l)            _dupenv_s(ps1,size,s2)
#define _wdupenv_s_dbg(ps1,size,s2,t,f,l)           _wdupenv_s(ps1,size,s2)
#define _CrtSetReportHook(f)                      ((_CRT_REPORT_HOOK)0)
#define _CrtGetReportHook()                       ((_CRT_REPORT_HOOK)0)
#define _CrtSetReportHook2(t,f)                     0
#define _CrtSetReportHookW2(t,f)                    0
#define _CrtSetReportMode(t,f)                      0
#define _CrtSetReportFile(t,f)                    ((_HFILE)0)
#define _CrtDbgBreak()                             (void)0
#define _CrtSetBreakAlloc(a)                        0l
#define _CrtSetAllocHook(f)                       ((_CRT_ALLOC_HOOK)0)
#define _CrtGetAllocHook()                        ((_CRT_ALLOC_HOOK)0)
#define _CrtCheckMemory()                           1
#define _CrtSetDbgFlag(f)                           0
#define _CrtDoForAllClientObjects(f,c)             (void)0
#define _CrtIsValidPointer(p,n,r)                   1
#define _CrtIsValidHeapPointer(p)                   1
#define _CrtIsMemoryBlock(p,t,r,f,l)                1
#define _CrtReportBlockType(p)                    (-1)
#define _CrtSetDumpClient(f)                      ((_CRT_DUMP_CLIENT)0)
#define _CrtGetDumpClient()                       ((_CRT_DUMP_CLIENT)0)
#define _CrtMemCheckpoint(s)                       (void)0
#define _CrtMemDifference(s1,s2,s3)                 0
#define _CrtMemDumpAllObjectsSince(s)              (void)0
#define _CrtMemDumpStatistics(s)                   (void)0
#define _CrtDumpMemoryLeaks()                       0
#define _CrtSetDebugFillThreshold(t)              ((__SIZE_TYPE__)0)
#define _CrtSetCheckCount(f)                        0
#define _CrtGetCheckCount()                         0
#endif

#endif



