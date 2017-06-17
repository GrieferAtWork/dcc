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
/* Only used for PE targets. */
#include "crtdbg.h"
#if defined(__PE__) || defined(CRTDBG_INIT_ALLOCA)

#ifdef __i386__

#define PAGESIZE  4096

.hidden __pe_alloca
.hidden __pe_alloca_eax
__pe_alloca:
	popl     %edx          /* Return address. */
	popl     %eax          /* Allocation count. */
	jmp      7f
__pe_alloca_eax:
	popl     %edx          /* Return address. */
7:
#ifdef CRTDBG_INIT_ALLOCA
	cmpl     $8,  %eax
	jl       1f
	pushfl
	pushl    %edi
	leal  -8(%eax), %ecx
	movl     %esp,  %edi
	decl     %edi
	movb     $CRTDBG_INIT_ALLOCA, %al
	std
	rep stosb
	incl     %edi
	movl     %edi,  %eax
	popl	 %edi
	popfl
	movl     $(0x01010101*CRTDBG_INIT_ALLOCA), -4(%esp)
	movl     $(0x01010101*CRTDBG_INIT_ALLOCA), -8(%esp)
	movl     %eax,  %esp
	jmp      2f
1:	decl     %esp
	movb     $CRTDBG_INIT_ALLOCA, (%esp)
	dec      %eax
	test	 %eax, %eax
	jnz		 1b
	movl     %esp, %eax
2:
#else
	cmpl $PAGESIZE,   %eax
	jl       2f
1:	subl $PAGESIZE,   %esp  /* Advance to the next page. */
	testb    $0,     (%esp) /* Touch the page. */
	subl $PAGESIZE,   %eax  /* Advance the size counter. */
	jg       1b             /* ZF == 0 && OF == SF */
2:	subl     %eax,    %esp
#endif
	jmpl    *%edx
.size __pe_alloca_eax, . - __pe_alloca_eax
.size __pe_alloca, . - __pe_alloca

#else
#	error FIXME
#endif
#endif /* __PE__ || CRTDBG_INIT_ALLOCA */
