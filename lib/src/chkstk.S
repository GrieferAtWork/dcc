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
#ifdef __PE__
#ifdef __i386__

/* Similar to __builtin_alloca(), this function decrements
 * '%esp' in order to allocate stack memory dynamically.
 * Yet unlike '__builtin_alloca()', it performs special
 * handling for large allocations in order to prevent
 * the stack from prematurely overflowing.
 *
 * Consider the following situation:
 * >> int vlogf(char const *format, va_list args) {
 * >>     char buffer[32768];
 * >>     vsnprintf(buffer,sizeof(buffer),format,args);
 * >>     write(log_fd,buffer,strlen(buffer)*sizeof(char));
 * >>     return 0;
 * >> }
 * As you can see, the buffer is _very_ large.
 * But who's to say that it shouldn't be?
 *
 * Now remember that the stack grows downwards, meaning that the
 * first access to 'buffer' will access a byte 32K bytes (8 pages)
 * before the last page the kernel saw us access, meaning that there
 * might be a chance (or in this case guaranty) that the kernel will
 * think we're trying to access an out-of-bounds pointer and simply
 * SEGFAULT on us.
 *
 * To prevent this, those 32K bytes of memory must somehow be
 * touched in reverse order so-as to ensure the page closest to
 * the end of the kernel-allocate stack (aka. the Guard Page) is
 * touched before all other pages are:
 * >> void *allocate_buffer(size_t size) {
 * >>     while (size >= PAGESIZE) {
 * >>         TOUCH_GUARD_PAGE();
 * >>         size -= PAGESIZE;
 * >>         ADD_TO_STACK_POINTER(-PAGESIZE);
 * >>     }
 * >>     ADD_TO_STACK_POINTER(-size);
 * >>     return GET_STACK_POINTER();
 * >> }
 * >>
 * >> int vlogf(char const *format, va_list args) {
 * >>     char *buffer = allocate_buffer(32768);
 * >>     vsnprintf(buffer,sizeof(buffer),format,args);
 * >>     write(log_fd,buffer,strlen(buffer)*sizeof(char));
 * >>     return 0;
 * >> }
 *
 * This is where '__chkstk' comes in to play, as it is the
 * function I so blatently called 'allocate_buffer' above.
 * Although functioning somewhat different and not actually
 * offering a real prototy (see below), the above pseudo-code
 * showcases exactly what this function really does.
 *
 * Semantially speaking, this entire thing is just a glorified
 * way of doing 'sub %eax, %esp' with the added benefit of
 * touching all pages between the old 'ESP' and the new, as
 * well as indirectly create a stack-frame
 *
 * IN:  EAX: Amount of bytes to allocate in ESP
 * IN:  ESP: ESP+0 = SETUP_RETURN_ADDRESS;
 *           ESP+4 = CALLER_RETURN_ADDRESS;
 * IN:  EBP: OLD_FRAME_ADDRESS;
 *
 * OUT: EAX: Undefined.
 * OUT: ESP: ESP   = EBP - IN(EAX);
 * OUT: EBP: EBP   = OLD(ESP);
 *           EBP+0 = OLD_FRAME_ADDRESS;
 *           EBP+4 = CALLER_RETURN_ADDRESS;
 * CLOBBER: EFLAGS (relation flags) (IN|OUT: EAX, ESP, EBP)
 * 
 * ASSUMES: IN(EAX) >= __SIZEOF_POINTER__
 */
.hidden __chkstk
#define PAGESIZE  4096
__chkstk:
	xchg     %ebp,   (%esp) /* Load the caller return address into EBP and store the old EBP. */
	mov      %ebp, -4(%esp) /* Store the 'SETUP_RETURN_ADDRESS' within caller-allocated stack memory. */
	mov      %esp,    %ebp  /* Load the frame pointer into EBP. */
#ifndef NDEBUG
	/* In debug-mode, this function is also called for small allocations
	 * Because of its special debug capabilities of filling stack-memory
	 * with special bytes, aiding in the detection of uninitialized variables.
	 * Because of that, bypass whole-page touching when less than one should
	 * be allocated (meaning that the lowest address is either already part
	 * of the stack, or lies within the associated guard page) */
	cmpl $PAGESIZE,   %eax
	jl       2f
#endif
1:	subl $PAGESIZE,   %esp  /* Advance to the next page. */
	testb    $0,     (%esp) /* Touch the page. */
	subl $PAGESIZE,   %eax  /* Advance the size counter. */
	/* 'subl' sets the flags the same way 'cmpl $PAGESIZE, %eax' would have.
	 * With that in mind, continue iteration if 'OLD(EAX) > PAGESIZE' */
	jg       1b             /* ZF == 0 && OF == SF */
#ifndef NDEBUG
2:
#endif
	/* EAX may be negative now as we may have over-allocated a bit.
	 * This error can easily be un-done by subtracting EAX from ESP */
	subl     %eax,    %esp
	/* And we're done! */
#ifndef NDEBUG
	/* DEBUG: memset(%esp,0xcd,%ebp-%esp);
	 * >> Fill all newly allocated stack-memory with '0xcd' */
	pushl    %edi
	pushl    %ecx
	pushl    %edx
	pushfl                  /* Preserve the direction flag. */
	movl  -4(%ebp),   %edx  /* Save 'SETUP_RETURN_ADDRESS' to EDX before its overwritten. */
	leal  16(%esp),   %edi  /* EDI = ESP+16 */
	movl     %ebp,    %ecx
	subl     %edi,    %ecx  /* ECX = EBP-(ESP+16) */
	movb     $0xff,   %al   /* AL  = 0xff */
#	movb     $0xcd,   %al   /* AL  = 0xcd */
	cld						/* Clear the direction bit. */
	rep      stosb          /* while (ECX--) *EDI++ = AL; */
	movl     %edx,    %eax  /* Load 'SETUP_RETURN_ADDRESS' back into EAX */
	popfl
	popl     %edx
	popl     %ecx
	popl     %edi
	jmpl    *%eax           /* Now simply jump to 'SETUP_RETURN_ADDRESS' */
#else
	jmpl *-4(%ebp)          /* Now simply jump to 'SETUP_RETURN_ADDRESS' */
#endif
.size __chkstk, . - __chkstk

#else
#	error FIXME
#endif
#endif /* __PE__ */
