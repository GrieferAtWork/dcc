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
/* DISCLAIMER: This file contains structures and declarations from <Windows.h> */
#ifndef GUARD_DCC_LINKER_PE_H
#define GUARD_DCC_LINKER_PE_H 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/lexer.h>

#ifdef _WIN32
#include <Windows.h>
#endif

DCC_DECL_BEGIN

#ifndef _WIN32
typedef struct _IMAGE_DOS_HEADER {
  WORD   e_magic;
  WORD   e_cblp;
  WORD   e_cp;
  WORD   e_crlc;
  WORD   e_cparhdr;
  WORD   e_minalloc;
  WORD   e_maxalloc;
  WORD   e_ss;
  WORD   e_sp;
  WORD   e_csum;
  WORD   e_ip;
  WORD   e_cs;
  WORD   e_lfarlc;
  WORD   e_ovno;
  WORD   e_res[4];
  WORD   e_oemid;
  WORD   e_oeminfo;
  WORD   e_res2[10];
  LONG   e_lfanew;
} IMAGE_DOS_HEADER, *PIMAGE_DOS_HEADER;
/* TODO: Add all the other PE-related structures */
#endif

#if DCC_TARGET_CPU == DCC_CPU_X86_64
#define DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER IMAGE_OPTIONAL_HEADER64
#elif defined(_WIN64)
#define DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER IMAGE_OPTIONAL_HEADER32
#else
#define DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER IMAGE_OPTIONAL_HEADER
#endif

#if DCC_TARGET_CPU == DCC_CPU_X86_64
#define DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER_MAGIC  IMAGE_NT_OPTIONAL_HDR64_MAGIC
#else
#define DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER_MAGIC  IMAGE_NT_OPTIONAL_HDR32_MAGIC
#endif


typedef struct {
 /* The .exe/.dll file starts here. */
 IMAGE_DOS_HEADER                    dhdr;       /* Dos header. */
 BYTE                                dcod[0x40]; /* 16-bit dos code. */
 DWORD                               ntsg;       /* NT Signature (== IMAGE_NT_SIGNATURE). */
 IMAGE_FILE_HEADER                   fhdr;       /* File header. */
 DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER ohdr;       /* Optional headers. */
 /* Section data is located here... */
} PE_HEADER;

typedef struct {
 DWORD                               ntsg; /* NT Signature (== IMAGE_NT_SIGNATURE). */
 IMAGE_FILE_HEADER                   fhdr; /* File header. */
 DCC_PE_TARGET_IMAGE_OPTIONAL_HEADER ohdr; /* Optional headers. */
} NT_HEADER;



#if DCC_TARGET_IA32(0)
#   define DCC_PE_TARGET_MACHINE   IMAGE_FILE_MACHINE_I386
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_PE_TARGET_MACHINE   IMAGE_FILE_MACHINE_AMD64
#else
#   error "UNKNOWN TARGET MACHINE"
#endif


#if DCC_LIBFORMAT_DYN_PE || DCC_LIBFORMAT_STA_PE

/* Utilities for parsing PE binaries. */
INTERN LONG  pe_readhdr(stream_t s);
INTERN char *pe_readzstring(stream_t fd, size_t *s);

#endif /* ... */


DCC_DECL_END

#endif /* !GUARD_DCC_LINKER_PE_H */
