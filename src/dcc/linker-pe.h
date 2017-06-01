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
#include <dcc/stream.h>

#ifndef __has_include
#define __has_include(x) 0
#endif

#if (!!(DCC_HOST_OS&DCC_OS_F_WINDOWS) || \
        __has_include(<Windows.h>))
#include <dcc_winmin.h>
#else
#include <stdint.h>
#endif

DCC_DECL_BEGIN

#if !defined(IMAGE_DOS_SIGNATURE)

typedef uint32_t DWORD;
typedef uint8_t  BYTE;
typedef uint16_t WORD;
typedef int32_t  LONG;
typedef int64_t  LONGLONG;
typedef uint32_t ULONG;
typedef uint64_t ULONGLONG;

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

typedef struct _IMAGE_FILE_HEADER {
    WORD    Machine;
    WORD    NumberOfSections;
    DWORD   TimeDateStamp;
    DWORD   PointerToSymbolTable;
    DWORD   NumberOfSymbols;
    WORD    SizeOfOptionalHeader;
    WORD    Characteristics;
} IMAGE_FILE_HEADER, *PIMAGE_FILE_HEADER;

#define IMAGE_FILE_RELOCS_STRIPPED           0x0001
#define IMAGE_FILE_EXECUTABLE_IMAGE          0x0002
#define IMAGE_FILE_LINE_NUMS_STRIPPED        0x0004
#define IMAGE_FILE_LOCAL_SYMS_STRIPPED       0x0008
#define IMAGE_FILE_AGGRESIVE_WS_TRIM         0x0010
#define IMAGE_FILE_LARGE_ADDRESS_AWARE       0x0020
#define IMAGE_FILE_BYTES_REVERSED_LO         0x0080
#define IMAGE_FILE_32BIT_MACHINE             0x0100
#define IMAGE_FILE_DEBUG_STRIPPED            0x0200
#define IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP   0x0400
#define IMAGE_FILE_NET_RUN_FROM_SWAP         0x0800
#define IMAGE_FILE_SYSTEM                    0x1000
#define IMAGE_FILE_DLL                       0x2000
#define IMAGE_FILE_UP_SYSTEM_ONLY            0x4000
#define IMAGE_FILE_BYTES_REVERSED_HI         0x8000

#define IMAGE_FILE_MACHINE_UNKNOWN           0
#define IMAGE_FILE_MACHINE_I386              0x014c
#define IMAGE_FILE_MACHINE_R3000             0x0162
#define IMAGE_FILE_MACHINE_R4000             0x0166
#define IMAGE_FILE_MACHINE_R10000            0x0168
#define IMAGE_FILE_MACHINE_WCEMIPSV2         0x0169
#define IMAGE_FILE_MACHINE_ALPHA             0x0184
#define IMAGE_FILE_MACHINE_SH3               0x01a2
#define IMAGE_FILE_MACHINE_SH3DSP            0x01a3
#define IMAGE_FILE_MACHINE_SH3E              0x01a4
#define IMAGE_FILE_MACHINE_SH4               0x01a6
#define IMAGE_FILE_MACHINE_SH5               0x01a8
#define IMAGE_FILE_MACHINE_ARM               0x01c0
#define IMAGE_FILE_MACHINE_THUMB             0x01c2
#define IMAGE_FILE_MACHINE_ARMNT             0x01c4
#define IMAGE_FILE_MACHINE_AM33              0x01d3
#define IMAGE_FILE_MACHINE_POWERPC           0x01F0
#define IMAGE_FILE_MACHINE_POWERPCFP         0x01f1
#define IMAGE_FILE_MACHINE_IA64              0x0200
#define IMAGE_FILE_MACHINE_MIPS16            0x0266
#define IMAGE_FILE_MACHINE_ALPHA64           0x0284
#define IMAGE_FILE_MACHINE_MIPSFPU           0x0366
#define IMAGE_FILE_MACHINE_MIPSFPU16         0x0466
#define IMAGE_FILE_MACHINE_AXP64             IMAGE_FILE_MACHINE_ALPHA64
#define IMAGE_FILE_MACHINE_TRICORE           0x0520
#define IMAGE_FILE_MACHINE_CEF               0x0CEF
#define IMAGE_FILE_MACHINE_EBC               0x0EBC
#define IMAGE_FILE_MACHINE_AMD64             0x8664
#define IMAGE_FILE_MACHINE_M32R              0x9041
#define IMAGE_FILE_MACHINE_CEE               0xC0EE



#define IMAGE_NT_OPTIONAL_HDR32_MAGIC      0x10b
#define IMAGE_NT_OPTIONAL_HDR64_MAGIC      0x20b

typedef struct _IMAGE_DATA_DIRECTORY {
    DWORD   VirtualAddress;
    DWORD   Size;
} IMAGE_DATA_DIRECTORY, *PIMAGE_DATA_DIRECTORY;
#define IMAGE_NUMBEROF_DIRECTORY_ENTRIES    16

typedef struct _IMAGE_OPTIONAL_HEADER {
    WORD    Magic;
    BYTE    MajorLinkerVersion;
    BYTE    MinorLinkerVersion;
    DWORD   SizeOfCode;
    DWORD   SizeOfInitializedData;
    DWORD   SizeOfUninitializedData;
    DWORD   AddressOfEntryPoint;
    DWORD   BaseOfCode;
    DWORD   BaseOfData;
    DWORD   ImageBase;
    DWORD   SectionAlignment;
    DWORD   FileAlignment;
    WORD    MajorOperatingSystemVersion;
    WORD    MinorOperatingSystemVersion;
    WORD    MajorImageVersion;
    WORD    MinorImageVersion;
    WORD    MajorSubsystemVersion;
    WORD    MinorSubsystemVersion;
    DWORD   Win32VersionValue;
    DWORD   SizeOfImage;
    DWORD   SizeOfHeaders;
    DWORD   CheckSum;
    WORD    Subsystem;
    WORD    DllCharacteristics;
    DWORD   SizeOfStackReserve;
    DWORD   SizeOfStackCommit;
    DWORD   SizeOfHeapReserve;
    DWORD   SizeOfHeapCommit;
    DWORD   LoaderFlags;
    DWORD   NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER32, *PIMAGE_OPTIONAL_HEADER32;

typedef struct _IMAGE_OPTIONAL_HEADER64 {
    WORD        Magic;
    BYTE        MajorLinkerVersion;
    BYTE        MinorLinkerVersion;
    DWORD       SizeOfCode;
    DWORD       SizeOfInitializedData;
    DWORD       SizeOfUninitializedData;
    DWORD       AddressOfEntryPoint;
    DWORD       BaseOfCode;
    ULONGLONG   ImageBase;
    DWORD       SectionAlignment;
    DWORD       FileAlignment;
    WORD        MajorOperatingSystemVersion;
    WORD        MinorOperatingSystemVersion;
    WORD        MajorImageVersion;
    WORD        MinorImageVersion;
    WORD        MajorSubsystemVersion;
    WORD        MinorSubsystemVersion;
    DWORD       Win32VersionValue;
    DWORD       SizeOfImage;
    DWORD       SizeOfHeaders;
    DWORD       CheckSum;
    WORD        Subsystem;
    WORD        DllCharacteristics;
    ULONGLONG   SizeOfStackReserve;
    ULONGLONG   SizeOfStackCommit;
    ULONGLONG   SizeOfHeapReserve;
    ULONGLONG   SizeOfHeapCommit;
    DWORD       LoaderFlags;
    DWORD       NumberOfRvaAndSizes;
    IMAGE_DATA_DIRECTORY DataDirectory[IMAGE_NUMBEROF_DIRECTORY_ENTRIES];
} IMAGE_OPTIONAL_HEADER64, *PIMAGE_OPTIONAL_HEADER64;

typedef IMAGE_OPTIONAL_HEADER32      IMAGE_OPTIONAL_HEADER;
typedef PIMAGE_OPTIONAL_HEADER32     PIMAGE_OPTIONAL_HEADER;
#define IMAGE_NT_OPTIONAL_HDR_MAGIC  IMAGE_NT_OPTIONAL_HDR32_MAGIC

#define IMAGE_DIRECTORY_ENTRY_EXPORT          0
#define IMAGE_DIRECTORY_ENTRY_IMPORT          1
#define IMAGE_DIRECTORY_ENTRY_RESOURCE        2
#define IMAGE_DIRECTORY_ENTRY_EXCEPTION       3
#define IMAGE_DIRECTORY_ENTRY_SECURITY        4
#define IMAGE_DIRECTORY_ENTRY_BASERELOC       5
#define IMAGE_DIRECTORY_ENTRY_DEBUG           6
#define IMAGE_DIRECTORY_ENTRY_ARCHITECTURE    7
#define IMAGE_DIRECTORY_ENTRY_GLOBALPTR       8
#define IMAGE_DIRECTORY_ENTRY_TLS             9
#define IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG    10
#define IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT   11
#define IMAGE_DIRECTORY_ENTRY_IAT            12
#define IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT   13
#define IMAGE_DIRECTORY_ENTRY_COM_DESCRIPTOR 14


#define IMAGE_DOS_SIGNATURE  0x5A4D
#define IMAGE_NT_SIGNATURE   0x00004550

#define IMAGE_SIZEOF_SHORT_NAME              8
typedef struct _IMAGE_SECTION_HEADER {
    BYTE    Name[IMAGE_SIZEOF_SHORT_NAME];
    union {
            DWORD   PhysicalAddress;
            DWORD   VirtualSize;
    } Misc;
    DWORD   VirtualAddress;
    DWORD   SizeOfRawData;
    DWORD   PointerToRawData;
    DWORD   PointerToRelocations;
    DWORD   PointerToLinenumbers;
    WORD    NumberOfRelocations;
    WORD    NumberOfLinenumbers;
    DWORD   Characteristics;
} IMAGE_SECTION_HEADER, *PIMAGE_SECTION_HEADER;

//      IMAGE_SCN_TYPE_REG                   0x00000000
//      IMAGE_SCN_TYPE_DSECT                 0x00000001
//      IMAGE_SCN_TYPE_NOLOAD                0x00000002
//      IMAGE_SCN_TYPE_GROUP                 0x00000004
#define IMAGE_SCN_TYPE_NO_PAD                0x00000008
//      IMAGE_SCN_TYPE_COPY                  0x00000010
#define IMAGE_SCN_CNT_CODE                   0x00000020
#define IMAGE_SCN_CNT_INITIALIZED_DATA       0x00000040
#define IMAGE_SCN_CNT_UNINITIALIZED_DATA     0x00000080
#define IMAGE_SCN_LNK_OTHER                  0x00000100
#define IMAGE_SCN_LNK_INFO                   0x00000200
//      IMAGE_SCN_TYPE_OVER                  0x00000400
#define IMAGE_SCN_LNK_REMOVE                 0x00000800
#define IMAGE_SCN_LNK_COMDAT                 0x00001000
//                                           0x00002000
//      IMAGE_SCN_MEM_PROTECTED              0x00004000
#define IMAGE_SCN_NO_DEFER_SPEC_EXC          0x00004000
#define IMAGE_SCN_GPREL                      0x00008000
#define IMAGE_SCN_MEM_FARDATA                0x00008000
//      IMAGE_SCN_MEM_SYSHEAP                0x00010000
#define IMAGE_SCN_MEM_PURGEABLE              0x00020000
#define IMAGE_SCN_MEM_16BIT                  0x00020000
#define IMAGE_SCN_MEM_LOCKED                 0x00040000
#define IMAGE_SCN_MEM_PRELOAD                0x00080000

#define IMAGE_SCN_ALIGN_1BYTES               0x00100000
#define IMAGE_SCN_ALIGN_2BYTES               0x00200000
#define IMAGE_SCN_ALIGN_4BYTES               0x00300000
#define IMAGE_SCN_ALIGN_8BYTES               0x00400000
#define IMAGE_SCN_ALIGN_16BYTES              0x00500000
#define IMAGE_SCN_ALIGN_32BYTES              0x00600000
#define IMAGE_SCN_ALIGN_64BYTES              0x00700000
#define IMAGE_SCN_ALIGN_128BYTES             0x00800000
#define IMAGE_SCN_ALIGN_256BYTES             0x00900000
#define IMAGE_SCN_ALIGN_512BYTES             0x00A00000
#define IMAGE_SCN_ALIGN_1024BYTES            0x00B00000
#define IMAGE_SCN_ALIGN_2048BYTES            0x00C00000
#define IMAGE_SCN_ALIGN_4096BYTES            0x00D00000
#define IMAGE_SCN_ALIGN_8192BYTES            0x00E00000
#define IMAGE_SCN_ALIGN_MASK                 0x00F00000

#define IMAGE_SCN_LNK_NRELOC_OVFL            0x01000000
#define IMAGE_SCN_MEM_DISCARDABLE            0x02000000
#define IMAGE_SCN_MEM_NOT_CACHED             0x04000000
#define IMAGE_SCN_MEM_NOT_PAGED              0x08000000
#define IMAGE_SCN_MEM_SHARED                 0x10000000
#define IMAGE_SCN_MEM_EXECUTE                0x20000000
#define IMAGE_SCN_MEM_READ                   0x40000000
#define IMAGE_SCN_MEM_WRITE                  0x80000000

#define IMAGE_SUBSYSTEM_UNKNOWN              0
#define IMAGE_SUBSYSTEM_NATIVE               1
#define IMAGE_SUBSYSTEM_WINDOWS_GUI          2
#define IMAGE_SUBSYSTEM_WINDOWS_CUI          3
#define IMAGE_SUBSYSTEM_OS2_CUI              5
#define IMAGE_SUBSYSTEM_POSIX_CUI            7
#define IMAGE_SUBSYSTEM_NATIVE_WINDOWS       8
#define IMAGE_SUBSYSTEM_WINDOWS_CE_GUI       9
#define IMAGE_SUBSYSTEM_EFI_APPLICATION      10
#define IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER  11
#define IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER   12
#define IMAGE_SUBSYSTEM_EFI_ROM              13
#define IMAGE_SUBSYSTEM_XBOX                 14
#define IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION 16

typedef struct _IMAGE_EXPORT_DIRECTORY {
    DWORD   Characteristics;
    DWORD   TimeDateStamp;
    WORD    MajorVersion;
    WORD    MinorVersion;
    DWORD   Name;
    DWORD   Base;
    DWORD   NumberOfFunctions;
    DWORD   NumberOfNames;
    DWORD   AddressOfFunctions;
    DWORD   AddressOfNames;
    DWORD   AddressOfNameOrdinals;
} IMAGE_EXPORT_DIRECTORY, *PIMAGE_EXPORT_DIRECTORY;


typedef struct _IMAGE_BASE_RELOCATION {
    DWORD   VirtualAddress;
    DWORD   SizeOfBlock;
//  WORD    TypeOffset[1];
} IMAGE_BASE_RELOCATION,*PIMAGE_BASE_RELOCATION;


#define IMAGE_REL_BASED_ABSOLUTE              0
#define IMAGE_REL_BASED_HIGH                  1
#define IMAGE_REL_BASED_LOW                   2
#define IMAGE_REL_BASED_HIGHLOW               3
#define IMAGE_REL_BASED_HIGHADJ               4
#define IMAGE_REL_BASED_MACHINE_SPECIFIC_5    5
#define IMAGE_REL_BASED_RESERVED              6
#define IMAGE_REL_BASED_MACHINE_SPECIFIC_7    7
#define IMAGE_REL_BASED_MACHINE_SPECIFIC_8    8
#define IMAGE_REL_BASED_MACHINE_SPECIFIC_9    9
#define IMAGE_REL_BASED_DIR64                 10

#define IMAGE_REL_BASED_IA64_IMM64            9
#define IMAGE_REL_BASED_MIPS_JMPADDR          5
#define IMAGE_REL_BASED_MIPS_JMPADDR16        9
#define IMAGE_REL_BASED_ARM_MOV32             5
#define IMAGE_REL_BASED_THUMB_MOV32           7


typedef struct _IMAGE_IMPORT_DESCRIPTOR {
    union {
        DWORD   Characteristics;
        DWORD   OriginalFirstThunk;
    };
    DWORD   TimeDateStamp;
    DWORD   ForwarderChain;
    DWORD   Name;
    DWORD   FirstThunk;
} IMAGE_IMPORT_DESCRIPTOR;
typedef IMAGE_IMPORT_DESCRIPTOR *PIMAGE_IMPORT_DESCRIPTOR;



#endif /* ... */

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


/* Prefix for ITA wrapper symbols. */
#define ITA_PREFIX "__imp_"


#if DCC_TARGET_IA32(0)
#   define DCC_PE_TARGET_MACHINE   IMAGE_FILE_MACHINE_I386
#elif DCC_TARGET_CPU == DCC_CPU_X86_64
#   define DCC_PE_TARGET_MACHINE   IMAGE_FILE_MACHINE_AMD64
#else
#   error "UNKNOWN TARGET MACHINE"
#endif


#if DCC_LIBFORMAT_PE_DYNAMIC || DCC_LIBFORMAT_PE_STATIC

/* Utilities for parsing PE binaries. */
INTERN LONG  pe_readhdr(stream_t s, soff_t start);
INTERN char *pe_readzstring(stream_t fd, size_t *s);

#endif /* ... */


DCC_DECL_END

#endif /* !GUARD_DCC_LINKER_PE_H */
