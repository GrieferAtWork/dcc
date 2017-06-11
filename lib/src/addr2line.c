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
#ifndef GUARD_LIB_SRC_ADDR2LINE_C
#define GUARD_LIB_SRC_ADDR2LINE_C 1

/* Compile with: $ dcc -nostdlib -c -o int64.o int64.c */
/* Declare everything with hidden visibility. */
#pragma warning("-Wno-c99")

/* NOTE: This file is included when linking with '-g' */

#include <dcc.h>
#include <stddef.h>
#include <stdint.h>

#include "addr2line.h"

/* NOTE: This data structure must mirror the offsets found in '/src/dcc/unit-debug.c' */
struct sec_info {
 struct sec_info const *si_next; /*< [0..1] Information about the next section. */
 void                  *si_addr; /*< [1..1] Section start address. */
 size_t                 si_size; /*< Section size. */
 a2l_op_t const        *si_a2l;  /*< [0..1] Addr2line code. */
};


extern struct sec_info const __dcc_dbg_secinfo  [[weak,visibility("hidden")]];
extern char const            __dcc_dbg_strtab[] [[weak]] __asm__(A2L_STRING_SECTION);

__attribute__((visibility("default")))
_Bool __dcc_dbg_addr2line(void *ip, lc_t *info) {
 struct sec_info const *iter = &__dcc_dbg_secinfo;
 if (info) {
  while (iter) {
   if ((uintptr_t)ip >= (uintptr_t)iter->si_addr &&
       (uintptr_t)ip <= (uintptr_t)iter->si_addr+iter->si_size) {
    /* Found the section associated with this EIP */
    struct A2lState state; a2l_addr_t addr;
    a2l_op_t const *code = iter->si_a2l;
    if (!code) break;
    A2lState_RESET(&state);
    addr = (a2l_addr_t)((uintptr_t)ip-(uintptr_t)iter->si_addr);
    /* printf("Found %p in %p ... %p\n",
     *        ip,iter->si_addr,
     *       (uintptr_t)iter->si_addr+iter->si_size);
     */
    if (!A2L_NAME(a2l_exec)(&state,&code,addr)) break;
    /* Managed to capture the given address!
     * Fill in all available information. */
    info->path = (state.s_features&A2L_STATE_HASPATH) ? __dcc_dbg_strtab+state.s_path : NULL;
    info->file = (state.s_features&A2L_STATE_HASFILE) ? __dcc_dbg_strtab+state.s_file : NULL;
    info->name = (state.s_features&A2L_STATE_HASNAME) ? __dcc_dbg_strtab+state.s_name : NULL;
    info->line = (state.s_features&A2L_STATE_HASLINE) ? state.s_line+1 : 0;
    info->col  = (state.s_features&A2L_STATE_HASCOL)  ? state.s_col+1  : 0;
    return 1;
   }
   iter = iter->si_next;
  }
  info->path = NULL;
  info->file = NULL;
  info->name = NULL;
  info->line = 0;
  info->col  = 0;
 }
 return 0;
}


#ifndef NDEBUG
#if defined(_WIN32) || defined(__CYGWIN32__)
#ifdef __i386__

typedef unsigned char BYTE;
typedef long          LONG;
typedef unsigned long DWORD,*LPDWORD;
typedef unsigned long ULONG_PTR;
typedef void         *PVOID;
typedef void const   *LPCVOID;
typedef void         *HANDLE;
typedef int           BOOL;
typedef char const   *LPCSTR;
typedef /* ... */void *LPOVERLAPPED;

#define WINAPI __stdcall

#define SIZE_OF_80387_REGISTERS      80
#define MAXIMUM_SUPPORTED_EXTENSION  512
#define EXCEPTION_MAXIMUM_PARAMETERS 15

typedef struct _FLOATING_SAVE_AREA {
    DWORD   ControlWord;
    DWORD   StatusWord;
    DWORD   TagWord;
    DWORD   ErrorOffset;
    DWORD   ErrorSelector;
    DWORD   DataOffset;
    DWORD   DataSelector;
    BYTE    RegisterArea[SIZE_OF_80387_REGISTERS];
    DWORD   Spare0;
} FLOATING_SAVE_AREA;


typedef struct _CONTEXT {
    DWORD ContextFlags;
    DWORD   Dr0;
    DWORD   Dr1;
    DWORD   Dr2;
    DWORD   Dr3;
    DWORD   Dr6;
    DWORD   Dr7;
    FLOATING_SAVE_AREA FloatSave;
    DWORD   SegGs;
    DWORD   SegFs;
    DWORD   SegEs;
    DWORD   SegDs;
    DWORD   Edi;
    DWORD   Esi;
    DWORD   Ebx;
    DWORD   Edx;
    DWORD   Ecx;
    DWORD   Eax;
    DWORD   Ebp;
    DWORD   Eip;
    DWORD   SegCs;
    DWORD   EFlags;
    DWORD   Esp;
    DWORD   SegSs;
    BYTE    ExtendedRegisters[MAXIMUM_SUPPORTED_EXTENSION];
} CONTEXT;


typedef struct _EXCEPTION_RECORD {
    DWORD    ExceptionCode;
    DWORD ExceptionFlags;
    struct _EXCEPTION_RECORD *ExceptionRecord;
    PVOID ExceptionAddress;
    DWORD NumberParameters;
    ULONG_PTR ExceptionInformation[EXCEPTION_MAXIMUM_PARAMETERS];
} EXCEPTION_RECORD;

typedef EXCEPTION_RECORD *PEXCEPTION_RECORD;


typedef CONTEXT *PCONTEXT;

typedef struct _EXCEPTION_POINTERS {
    PEXCEPTION_RECORD ExceptionRecord;
    PCONTEXT ContextRecord;
} EXCEPTION_POINTERS, *PEXCEPTION_POINTERS;

typedef LONG (WINAPI *PTOP_LEVEL_EXCEPTION_FILTER)(PEXCEPTION_POINTERS ExceptionInfo);
typedef PTOP_LEVEL_EXCEPTION_FILTER LPTOP_LEVEL_EXCEPTION_FILTER;


#define STD_INPUT_HANDLE    ((DWORD)-10)
#define STD_OUTPUT_HANDLE   ((DWORD)-11)
#define STD_ERROR_HANDLE    ((DWORD)-12)

#define K32   [[lib("Kernel32.dll")]]

K32 LPTOP_LEVEL_EXCEPTION_FILTER WINAPI SetUnhandledExceptionFilter(LPTOP_LEVEL_EXCEPTION_FILTER);
K32 BOOL WINAPI WriteFile(HANDLE hFile, LPCVOID lpBuffer, DWORD nNumberOfBytesToWrite, LPDWORD lpNumberOfBytesWritten, LPOVERLAPPED lpOverlapped);
K32 HANDLE WINAPI GetStdHandle(DWORD nStdHandle);
K32 void WINAPI OutputDebugStringA(LPCSTR lpOutputString);


#include <stddef.h>
#include <dcc.h>


/* Use direct I/O in case libc is compromised */
#define TB_PRINT(x) tb_print(x,sizeof(x)-sizeof(char))
static void tb_print(char const *s, size_t len) {
 WriteFile(GetStdHandle(STD_ERROR_HANDLE),s,len,0,0);
 OutputDebugStringA(s);
}
static void tb_prints(char const *s) {
 char const *end = s;
 while (*end) ++end;
 tb_print(s,(size_t)(end-s)*sizeof(char));
}
static void tb_printi(uintptr_t i) {
 char buf[33],*iter = buf+32;
 *iter = '\0';
 if (!i) *--iter = '0';
 else do {
  *--iter = (char)('0'+(i % 10));
 } while ((i /= 10) != 0);
 tb_print(iter,(size_t)((buf+32)-iter)*sizeof(char));
}
static void tb_printx(uintptr_t i) {
 char buf[9],*iter = buf+8;
 *iter = '\0';
 while (iter != buf) {
  uint8_t x = (uint8_t)(i&0xf);
  --iter;
  if (x >= 10) *iter = (char)('A'+(x-10));
  else         *iter = (char)('0'+x);
  i >>= 4;
 }
 tb_print(buf,8*sizeof(char));
}


static void print_addr(void *p, size_t i) {
 lc_t info;
 _addr2line(p,&info);
 if (info.path) { tb_prints(info.path); TB_PRINT("/"); }
 if (info.file) tb_prints(info.file); else TB_PRINT("??" "?");
 TB_PRINT("(");
 tb_printi(info.line);
 TB_PRINT(",");
 tb_printi(info.col);
 TB_PRINT(") : ");
 if (info.name) tb_prints(info.name); else TB_PRINT("??" "?");
 TB_PRINT(" : ");
 tb_printx((uintptr_t)p);
 TB_PRINT(" (Frame ");
 tb_printi((uintptr_t)i);
 TB_PRINT(")\n");
}

struct frame {
 struct frame *caller;
 void         *addr;
};

static LONG __stdcall tb_handler(PEXCEPTION_POINTERS ExceptionInfo) {
 TB_PRINT("Unhandled exception\n");
 if (ExceptionInfo) {
  PCONTEXT ctx = ExceptionInfo->ContextRecord;
  PEXCEPTION_RECORD record = ExceptionInfo->ExceptionRecord;
  /* Display a traceback. */
  if (ctx) {
   struct frame *iter,*start,*check;
   size_t num,index = 0;
   print_addr((void *)ctx->Eip,0);
   iter = start = (struct frame *)ctx->Ebp;
   while (iter) {
    check = start,num = 0;
    while (num < index) {
     if (check == iter) {
      TB_PRINT("Recursion: Frame ");
      tb_printi(index+1);
      TB_PRINT(" (");
      tb_printx((uintptr_t)check);
      TB_PRINT(") == Frame ");
      tb_printi(num+1);
      TB_PRINT(" (");
      tb_printx((uintptr_t)iter);
      TB_PRINT(")\n");
      goto done_tb;
     }
     check = check->caller;
     ++num;
    }
    print_addr(iter->addr,++index);
    iter = iter->caller;
   }
  }
done_tb:
  /* Display additional informations. */
  if (record) {
   TB_PRINT("CODE = "),tb_printx((uintptr_t)record->ExceptionCode),TB_PRINT("\n");
   TB_PRINT("FLAG = "),tb_printx((uintptr_t)record->ExceptionFlags),TB_PRINT("\n");
   TB_PRINT("ADDR = "),tb_printx((uintptr_t)record->ExceptionAddress),TB_PRINT("\n");
  }
 }
 return 0;
}


[[visibility("hidden")]]
void __dcc_dbg_init_exc_tracebacks(void) {
 SetUnhandledExceptionFilter(&tb_handler);
}

#endif /* __i386__ */
#endif /* _WIN32 */
#endif /* !NDEBUG */

#endif /* !GUARD_LIB_SRC_ADDR2LINE_C */
