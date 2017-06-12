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
#ifndef GUARD_LIB_SRC_LIBC_STRING_C
#define GUARD_LIB_SRC_LIBC_STRING_C 1

#include "common.h"
#include <stdint.h>
#include <stddef.h>

DECL_BEGIN

#ifdef __i386__
#ifndef __INTELLISENSE__
#define CPU_I386
#endif
#endif


#ifdef CPU_I386
ASM {
.global memcpy
memcpy:
	pushl   %esi
	pushl   %edi
	mov     12(%esp), %edi /* Load <dst> */
	mov     16(%esp), %esi /* Load <src> */
	mov     20(%esp), %ecx /* Load <n> */
	mov     %edi, %eax     /* Backup <dst> into the return register EAX */
	cld
	rep movsb
	popl    %edi
	popl    %esi
	ret
.size memcpy, . - memcpy
}
#else/**/
PUBLIC void *memcpy(void *dst, void const *src, size_t n) {
 void *result = dst;
 while (n >= sizeof(int)) {
  *(int *)dst = *(int *)src;
  (uintptr_t &)dst += sizeof(int);
  (uintptr_t &)src += sizeof(int);
  n -= sizeof(int);
 }
 while (n) {
  *(uint8_t *)dst = *(uint8_t *)src;
  ++(uintptr_t &)dst;
  ++(uintptr_t &)src;
  --n;
 }
 return result;
}
#endif

#ifdef CPU_I386
ASM {
.global memcmp
memcmp:
	pushl   %esi
	pushl   %edi
	xorl    %eax, %eax     /* Clear EAX (Required to zero upper register memory) */
	movl    $-1, %edx      /* Fill EDX with -1, as will be returned when a < b */
	mov     12(%esp), %edi /* Load <a> */
	mov     16(%esp), %esi /* Load <b> */
	mov     20(%esp), %ecx /* Load <n> */
	cld
	repe cmpsb
	setg    %al            /* EAX = a > b; (0/1) */
	cmovl   %edx, %eax     /* if (a < b) EAX = -1; */
	popl    %edi
	popl    %esi
	ret
.size memcmp, . - memcmp
}
#else/**/
PUBLIC int memcmp(void const *a, void const *b, size_t n) {
 int8_t result = 0;
 while (n) {
  result = *(int8_t *)a-*(int8_t *)b;
  if (result) break;
  ++(uintptr_t &)a;
  ++(uintptr_t &)b;
  --n;
 }
 return result;
}
#endif

#ifdef CPU_I386
ASM {
.global memset
memset:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <dst> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	mov     %edi, %edx     /* Backup <dst> into the return register EAX */
	cld
	repe stosb
	popl    %edi
	mov     %edx, %eax
	ret
.size memset, . - memset
}
#else/**/
PUBLIC void *memset(void *dst, int byte, size_t n) {
 void *result = dst;
 while (n) {
  *(uint8_t *)dst = (uint8_t)byte;
  ++(uintptr_t &)dst;
  --n;
 }
 return result;
}
#endif

#ifdef CPU_I386
ASM {
.global memchr
memchr:
	pushl   %edi
	xorl    %edx, %edx     /* Clear 'EDX' for later */
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	cld
	repne scasb
	dec     %edi           /* Navigate back to the matched character. */
	cmovne  %edx, %edi     /* Store NULL in 'EDI' when the character wasn't found. */
	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size memchr, . - memchr
}

ASM {
.global memend
memend:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	cld
	repne scasb
	jne     1f             /* Keep the pointer at <p>+<n> if <byte> wasn't found. */
	dec     %edi           /* Navigate back to the matched character. */
1:	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size memend, . - memend
}

ASM {
.global memlen
memlen:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	mov     %edi, %edx     /* Backup <p> */
	cld
	repne scasb
	jne     1f             /* Keep the pointer at <p>+<n> if <byte> wasn't found. */
	dec     %edi           /* Navigate back to the matched character. */
1:	mov     %edi, %eax     /* Move EDI into the return register. */
	subl    %edx, %eax     /* Subtract <p> from EAX. */
	popl    %edi
	ret
.size memlen, . - memlen
}
#else/**/
PUBLIC void *memchr(void const *p, int byte, size_t n) {
 while (n) {
  if (*(uint8_t *)p == (uint8_t)byte) return (void *)p;
  ++(uintptr_t &)p;
  --n;
 }
 return NULL;
}
PUBLIC void *memend(void const *p, int byte, size_t n) {
 while (n) {
  if (*(uint8_t *)p == (uint8_t)byte) break;
  ++(uintptr_t &)p;
  --n;
 }
 return (void *)p;
}
PUBLIC size_t memlen(void const *p, int byte, size_t n) {
 return (size_t)((uintptr_t)memend(p,byte,n)-(uintptr_t)p);
}
#endif

#ifdef CPU_I386
ASM {
.global memrchr
memrchr:
	pushl   %edi
	xorl    %edx, %edx     /* Clear 'EDX' for later */
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	addl    %ecx, %edi     /* Add <n> to <p>, thereby pointing at the byte after the last to be searched */
	dec     %edi           /* Point at the last byte to-be searched. */
	std
	repne scasb
	inc     %edi           /* Navigate back to the matched character. */
	cmovne  %edx, %edi     /* Store NULL in 'EDI' when the character wasn't found. */
	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size memrchr, . - memrchr
}

ASM {
.global memrend
memrend:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	addl    %ecx, %edi     /* Add <n> to <p>, thereby pointing at the byte after the last to be searched */
	dec     %edi           /* Point at the last byte to-be searched. */
	std
	repne scasb
	jne     1f             /* Keep the pointer at <p>-1 if <byte> wasn't found. */
	inc     %edi           /* Navigate back to the matched character. */
1:	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size memrend, . - memrend
}

ASM {
.global memrlen
memrlen:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     16(%esp), %ecx /* Load <n> */
	mov     %edi, %edx     /* Backup <p> */
	addl    %ecx, %edi     /* Add <n> to <p>, thereby pointing at the byte after the last to be searched */
	dec     %edi           /* Point at the last byte to-be searched. */
	std
	repne scasb
	jne     1f             /* Keep the pointer at <p>-1 if <byte> wasn't found. */
	inc     %edi           /* Navigate back to the matched character. */
1:	mov     %edi, %eax     /* Move EDI into the return register. */
	subl    %edx, %eax     /* Subtract <p> from EAX. */
	popl    %edi
	ret
.size memrlen, . - memrlen
}

#else/**/
PUBLIC void *memrchr(void const *p, int byte, size_t n) {
 (uintptr_t &)p += n;
 while (n) {
  --(uintptr_t &)p;
  if (*(uint8_t *)p == (uint8_t)byte) return (void *)p;
  --n;
 }
 return NULL;
}
PUBLIC void *memrend(void const *p, int byte, size_t n) {
 (uintptr_t &)p += n;
 while (n) {
  --(uintptr_t &)p;
  if (*(uint8_t *)p == (uint8_t)byte) break;
  --n;
 }
 return (void *)p;
}
PUBLIC size_t memrlen(void const *p, int byte, size_t n) {
 return (size_t)((uintptr_t)memrend(p,byte,n)-(uintptr_t)p);
}
#endif



#ifdef CPU_I386
ASM {
.global rawmemchr
rawmemchr:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     $-1, %ecx      /* Unlimited search size. */
	cld
	repne scasb
	dec     %edi           /* Navigate back to the matched character. */
	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size rawmemchr, . - rawmemchr
}

ASM {
.global rawmemlen
rawmemlen:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     $-1, %ecx      /* Unlimited search size. */
	cld
	repne scasb
	inc     %ecx
	not     %ecx
	mov     %ecx, %eax
	popl    %edi
	ret
.size rawmemlen, . - rawmemlen
}
#else/**/
PUBLIC void *rawmemchr(void const *p, int byte) {
 for (;;) {
  if (*(uint8_t *)p == (uint8_t)byte) break;
  ++(uintptr_t &)p;
 }
 return (void *)p;
}
PUBLIC size_t rawmemlen(void const *p, int byte) {
 return (size_t)((uintptr_t)rawmemchr(p,byte)-(uintptr_t)p);
}
#endif

#ifdef CPU_I386
ASM {
.global rawmemrchr
rawmemrchr:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     $-1, %ecx      /* Unlimited search size. */
	dec     %edi           /* Point at the last byte to-be searched. */
	std
	repne scasb
	inc     %edi           /* Navigate back to the matched character. */
	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size rawmemrchr, . - rawmemrchr
}

ASM {
.global rawmemrlen
rawmemrlen:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %eax /* Load <byte> */
	mov     $-1, %ecx      /* Unlimited search size. */
	dec     %edi           /* Point at the last byte to-be searched. */
	std
	repne scasb
	mov     %ecx, %eax
	popl    %edi
	ret
.size rawmemrlen, . - rawmemrlen
}

#else/**/
PUBLIC void *rawmemrchr(void const *p, int byte) {
 for (;;) {
  --(uintptr_t &)p;
  if (*(uint8_t *)p == (uint8_t)byte) break;
 }
 return (void *)p;
}
PUBLIC size_t rawmemrlen(void const *p, int byte) {
 return (size_t)((uintptr_t)rawmemrchr(p,byte)-(uintptr_t)p);
}
#endif

#ifdef CPU_I386
ASM {
.global strend
strend:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	xorl    %eax, %eax     /* Search for a ZERO-character. */
	mov     $-1, %ecx      /* Unlimited search size. */
	cld
	repne scasb
	dec     %edi           /* Navigate back to the matched character. */
	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size strend, . - strend
}

ASM {
.global strlen
strlen:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	xorl    %eax, %eax     /* Search for a ZERO-character. */
	mov     $-1, %ecx      /* Unlimited search size. */
	cld
	repne scasb
	inc     %ecx
	not     %ecx
	mov     %ecx, %eax
	popl    %edi
	ret
.size strlen, . - strlen
}
#else/**/
PUBLIC char *strend(char const *p) {
 while (*p) ++p;
 return (char *)p;
}
PUBLIC size_t strlen(char const *p) {
 char const *start = p;
 while (*p) ++p;
 return (size_t)(p-start);
}
#endif


#ifdef CPU_I386
ASM {
.global strnend
strnend:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %ecx /* Load <n> */
	xorl    %eax, %eax     /* Search for a ZERO-character */
	cld
	repne scasb
	jne     1f             /* Keep the pointer at <p>+<n> if '\0' wasn't found. */
	dec     %edi           /* Navigate back to the matched character. */
1:	mov     %edi, %eax     /* Move EDI into the return register. */
	popl    %edi
	ret
.size strnend, . - strnend
}

ASM {
.global strnlen
strnlen:
	pushl   %edi
	mov     8(%esp), %edi  /* Load <p> */
	mov     12(%esp), %ecx /* Load <n> */
	xorl    %eax, %eax     /* Search for a ZERO-character */
	mov     %edi, %edx     /* Backup <p> */
	cld
	repne scasb
	jne     1f             /* Keep the pointer at <p>+<n> if '\0' wasn't found. */
	dec     %edi           /* Navigate back to the matched character. */
1:	mov     %edi, %eax     /* Move EDI into the return register. */
	subl    %edx, %eax     /* Subtract <p> from EAX. */
	popl    %edi
	ret
.size strnlen, . - strnlen
}
#else/**/
PUBLIC char *strnend(char const *p, size_t n) {
 while (n) {
  if (!*p) break;
  ++(uintptr_t &)p;
  --n;
 }
 return (char *)p;
}
PUBLIC size_t strnlen(char const *p, size_t n) {
 char const *start = p;
 while (n) {
  if (!*p) break;
  ++(uintptr_t &)p;
  --n;
 }
 return (size_t)(p-start);
}
#endif


DECL_END

#endif /* !GUARD_LIB_SRC_LIBC_STRING_C */
