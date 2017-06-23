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
#ifndef GUARD_DRT_DRT_H
#define GUARD_DRT_DRT_H 1

#include <dcc/common.h>
#include <dcc/target.h>
#include <dcc/unit.h>


/* 
 * How does DCC manage to execute C code _while_ it is still compiling said code?
 * 
 * Answering that question will take a while, so I'm going to break down the
 * answer into different segments covering various problems (and solutions).
 * NOTE: For the duration of this explanation I will refer to the
 *       direct nature of DCC's DRT (DirectRunTime) compilation
 *       mode as a non-blocking design model.
 *
 * #1: The simple stuff:
 *      - Lexer/Preprocessor/Tokenizer
 *      - Parser
 * #2: The somewhat more complicated stuff:
 *      - Assembler
 * #3: The really complicated stuff:
 *      - Linker
 * #4: The WTF! - That just sounds crazy stuff:
 *      - Execution
 *
 * Lexer/Preprocessor/Tokenizer:
 *   This isn't really the place to discuss this, but using TPP as underlying
 *   engine, the C preprocessor pseudo-language at no point contains any
 *   feature that would require advanced knowledge of major chunks of
 *   information further down the file.
 *   With that in mind, building a non-blocking preprocessor is as simple
 *   as setting the goal of never reading the entirety of an input file,
 *   and instead always working with 
 *   At this point I could go into further detail on some of TPP's optimizations,
 *   but in the end you can easily convince yourself of its non-blocking
 *   nature by running '$ dcc --tok -E -', which will launch DCC in tokenized
 *   preprocessor-mode, with the input file set to STDIN, meaning you can write 
 *   to your hearts content with the only buffering being line-buffering.
 * 
 * 
 * Parser:
 *   Building ontop of a non-blocking preprocessor, anything but a 
 *   non-blocking Parser should seem more complicated than exactly
 *   that.
 *   Though it should be noted that DCC's parser is fully non-blocking,
 *   by which I am referring to the fact that at no point will tokens
 *   be parsed prematurely, such as with a function one would naïvely
 *   call 'peek_token'.
 *   While something similar does exist, at no point will the runtime
 *   yield a token, only to later revert to a previous token.
 *   It will at places yield a token and reconfigure the lexer to
 *   yield the same token again, but never goes any further than that.
 *   Sadly, considering this, a few minor restrictions must apply to
 *   any C source file that is attempted to-be compiled with DCC, as
 *   with the presence of labels in C, any compiler wanting to parse
 *   them is forced to implement some form of token look-ahead.
 *   >>    int x = 10;
 *   >>my_label:
 *   >>    if (x-- >= 0) goto my_label;
 *   >>    printf("x = %d\n",x); // -2
 *   Let's look at this line:
 *   >>my_label:
 *     ^^^^^^^^
 *   The compiler is currently looking at the token 'my_label' and
 *   tries to figure out if it might possibly be a label (which it is).
 *   Just like any other self-respecting compiler, of course DCC
 *   also recognized this as a proper label, by doing some manual
 *   scanning in search for what is likely to be the next token.
 *   In this case, the deduction is quite simple, and it will be
 *   in almost all cases.
 *   Some things that might make this harder include preprocessor directives
 *   or comments, of which DCC is only able to handle the latter, yet
 *   the most notorious case is as follows:
 *   >>#define DOPPELPUNKT   :
 *   >>    int x = 10;
 *   >>my_label DOPPELPUNKT
 *   >>    if (x-- >= 0) goto my_label;
 *   >>#if __SIZEOF_POINTER__ <= 4
 *   >>my_other_label1
 *   >>#else
 *   >>my_other_label2
 *   >>#endif
 *   >>               :
 *   >>    printf("x = %d\n",x); // -2
 *   Technically fully compliant to STD-C, DCC isn't able to
 *   recognize any of the label definitions you can find above.
 *   A fixed version that DCC could parse as well would look like this:
 *   >>    int x = 10;
 *   >>my_label:
 *   >>    if (x-- >= 0) goto my_label;
 *   >>#if __SIZEOF_POINTER__ <= 4
 *   >>my_other_label1:
 *   >>#else
 *   >>my_other_label2:
 *   >>#endif
 *   >>    printf("x = %d\n",x); // -2
 *   Yet considering the stylistic choices that would lead a programmer
 *   do anything else that the latter lead to the choice of omitting
 *   support for code that would cause your eyes to start burning.
 *   It should also be noted that DCC does not make use of any kind
 *   of AST generation, directly using a V-Stack (commonly called
 *   Value-Stack, but in DCC uniformly referred to Virtual-Stack)
 *
 *
 * Assembler:
 *   Just as mentioned above, this is where it gets complicated,
 *   and where we encounter the first couple of problems.
 *   For the duration of this passage, i386+ assembly will be used.
 *   Let's take a look at a regular, old function:
 *   >> void my_loop(void) {
 *   >>     int i;
 *   >>     for (i = 0; i < 10; ++i) {
 *   >>         int z = get_value(i);
 *   >>         printf("value(%d) = %d\n",i,z);
 *   >>     }
 *   >> }
 *   The assembly for this function might then look like this:
 *   >>.global my_loop
 *   >>my_loop:
 *   >>    push   %esp
 *   >>    movl   %esp, %ebp
 *   >>    subl   $8,   %esp
 *   >>    movl   $0,   -4(%ebp) // i = 0
 *   >>1:  cmpl   $0xa, -4(%ebp) // i < 10
 *   >>    jge    1f
 *   >>    jmp    2f
 *   >>3:  incl   -4(%ebp)       // ++i
 *   >>    jmp    1b
 *   >>2:  pushl  -4(%ebp)
 *   >>    call   get_value      // get_value(i)
 *   >>    add    $4,   %esp
 *   >>    mov    %eax, -8(%ebp) // z = get_value...
 *   >>    pushl  -8(%ebp)
 *   >>    pushl  -4(%ebp)
 *   >>    push   $"value(%d) = %d\n"
 *   >>    call   printf         // printf(...)
 *   >>    add    $12,  %esp
 *   >>    jmp    3b
 *   >>1:  leave
 *   >>    ret
 *   >>.size my_loop, . - my_loop
 *   As you can see in the assembly, one work-around for ensuring
 *   proper execution order of the loop-incrementer at the end of
 *   the loop can already be seen in the seemingly (and admittedly)
 *   pointless double-jump around the code for '++i'.
 *   The reason for this lies within the fact that DCC doesn't
 *   generate AST and doesn't cache code before generation of
 *   assembly, meaning that the assembler has no choice but
 *   to output code as it appears (that is left-to-right, top-bottom),
 *   no matter how many jump are required to ensure proper execution
 *   order.
 *   Yet the most pressing matter, and something you may have already
 *   noticed, is the '$8' in 'subl ..., %esp'.
 *   Let me explain what this does: It allocates 8 bytes of stack
 *   memory that are used for local variables during execution of
 *   the accompanying function.
 *   But looking further down the line, 
 *   You may not like the current solution, but long-story-short:
 *   DCC simply pre-allocated a _lot_ of memory for every stack
 *   frame, although only does so in direct compilation mode.
 *   Yet this problem isn't fixed easily:
 *   You might think about relocating frame sizes at a later
 *   point, maybe stating that the DRT thread can be paused
 *   and its stack pointer adjusted. But that is a _very_ bad
 *   idea for the fact that in C or assembly, pointers cannot
 *   be tracked, meaning stack-allocated data structures would
 *   be broken if ESP was blindly to-be changed.
 *   Another problem would be breaking functions further up
 *   the stack in the even that DRT is currently inside of
 *   'get_value', at which point the user is still writing
 *   'my_loop' when deciding to add a 3rd variable.
 *   At that point, 'ESP' had already been (somehow) adjusted
 *   to allow for 8 bytes of stack memory, when 4 more bytes
 *   suddenly get added, yet there is nowhere to put them
 *   and no way of relocation ESP again, mainly because it is
 *   being used for addressing local variables inside of 'get_value'.
 *   So considering these problems, preallocating a _very_ big
 *   stack-frame beforehand remains a very good solution.
 *   NOTE: Future versions of DCC will potentially optimize
 *         on this by updating such a function's prolog
 *         once it has been compiled to mirror the correct
 *        (and smaller) frame size.
 *         But until then, remember that this isn't even a
 *         problem outside of DRT mode ('dcc -d')
 *
 *
 * Linker:   
 *   Everything above you'd probably have been able to figure out
 *   even without the explanation. - But how does one link code
 *   without knowing what will belong where, or how to relocate
 *   code while it is running.
 *   Let me answer the latter first: You don't! - And you don't even try to!
 *   Instead, you simply design an execution-address-layout with
 *   sufficient space addressable memory for every section of the executable.
 *   In the end, this simply means doing something like
 *   this in the initialization of every section:
 *   >> my_section->drt_address = current_drt_address;
 *   >> current_drt_address += MAX_SIZE_FOR_ANY_SECTION;
 *   Again: This was something you probably could have guessed.
 *          But how does something like this work:
 *   >> int add(int x, int y);
 *   >> int main() {
 *   >>     int r = add(10,20);
 *   >>     printf("r = %d\n",r);
 *   >>     return 0;
 *   >> }
 *   >> 
 *   >> [...]
 *   >> 
 *   >> // For the first part of the anwer below,
 *   >> // the below definition doesn't exist.
 *   >> int add(int x, int y) {
 *   >>     return x+y;
 *   >> }
 *
 *   To answer that question, let's look at the assembly again:
 *   >> .global add
 *   >> .global main
 *   >> main:
 *   >>    push   %esp
 *   >>    movl   %esp, %ebp
 *   >>    pushl  $20
 *   >>    pushl  $10
 *   >>    call   add
 *   >>    addl   $8, %esp
 *   >>    movl   $0, %eax
 *   >>    ret
 *   >> .size main, . - main
 *
 *   As should be obvious, the definition of 'add' is
 *   nowhere to be seen, so what does the code call when
 *   it is running? What kind of black magic is this?
 *   Well... I may have lied a bit about how the assembly looks.
 *   In particular: The following is a better representation
 *   of what the 'call add' line looks like:
 *   >>    mov    add, %eax
 *   >>    call   add
 *   Or an even more binary-representative view:
 *   >>    mov    0xfffff001, %eax
 *   >>    call   0xfffff001
 *
 *   What!? That's not helping anything! Why are you loading 'add'?
 *   What happens here is a premature access to 'add' that will
 *   touch the memory address, essentially forcing a SEGFAULT
 *   that can be captured, alongside information about where
 *   it originated from.
 *   Essentially, looking at the SEGFAULT caused by the 'mov'
 *   instruction, DRT can figure out that the user-code is
 *   planning to access data at '0xfffff001'.
 *   But this isn't what's interesting about this.
 *   What is interesting though, is the fact that DRT can ask DCC
 *   to essentially do the following when this is encountered, using
 *   a system I like to call 'Lazy Relocation':
 *     Given the address of an instruction, scan its memory
 *     and all contained bytes until the end of the 'mov' opcode
 *     for relocations.
 *     Following that, suspend the DRT until all symbols referred
 *     to by those relocations have been defined, at which point
 *     all section code already committed for execution by DRT
 *     shall be updated to contain the proper addresses for that
 *     symbol.
 *     This essentially means that calling an undefined symbol
 *     'add' will cause DRT to wait until the symbol is defined
 *     when it first encounters a reference to the symbol:
 *   >> int main() {
 *   >>     WAIT_FOR_SYMBOL("add");
 *   >>     int r = add(10,20);
 *   >>     WAIT_FOR_SYMBOL("printf");
 *   >>     printf("r = %d\n",r);
 *   >>     return 0;
 *   >> }
 *   >>
 *   >> int add(int x, int y) {
 *   >>     TRIGGER_SYMBOL_DEFINED("add");
 *   >>     return x+y;
 *   >> }
 *      
 *   One problem not currently handled is the following:  
 *   >> extern int x[6];
 *   >> int *px = &x[3];
 *   >> 
 *   >> int main() {
 *   >>     WAIT_FOR_SYMBOL("px"); // OK: 'px' is defined
 *   >>     printf("px  = %p\n",px);
 *   >>     printf("*px = %d\n",*px);
 *   >> }
 *   >> 
 *   >> int x[6] = { 2,1,6,3,42,17 };
 *   
 *   The naïve solution implemented by DCC is to recursively look at
 *   the definition of a symbol during DRT relocations, considering
 *   its size and address to recursively wait for any other
 *   symbols it may be referring to.
 *   
 *   But getting back to a simpler problem: How does DRT
 *   know when to wait for more code in its own function?
 *   After all: there isn't anything to dereference when
 *   there is no code at all...
 *
 *   >> int main() {
 *   >>     printf("Hello World\n");
 *   >>     // For the sake of this example, the below code doesn't exist yet.
 *   >>     printf("Hello Griefer\n");
 *   >>     return 0;
 *   >> }
 *   
 *   The answer to this is quite simple and has to do with the default
 *   initialization of DRT memory, which is a simple memset() with '0xf4' bytes.
 *   On i386+, this refers to the 'hlt' instruction, an opcode that is rarely
 *   ever used, and if used, cannot be executed in ring #3 without a #PF IRQ,
 *   or in other words: Only a kernel is allowed to execute it, and if you
 *   try to, an exception will be triggered. Using, and capturing that exception,
 *   figuring out that execution has reached missing code is as simple as
 *   handling said exception and doing a similar kind of wait that suspends
 *   DRT execution until 'printf("Hello Griefer\n");' is eventually parsed.
 *
 *   At this point I should mention that the DRT uses a copy of regular
 *   section memory, that is copied at so-called synchronization points,
 *   meaning that there is no chance of executing a partially written opcode,
 *   or writing a memory address while that same address is being executed.
 *
 *   Shared library can easily be loaded using 'LoadLibraryA'/'dlopen',
 *   after which it is is a simple matter of using 'GetProcAddress'/'dlsym'
 *   for retrieving the absolute symbol address that can be used for
 *   linking relocation against an import section.
 *
 *   RESTRICTIONS:
 *     - DRT is currently limited to a single thread, and even though
 *       it is quite possible to extend this limit, user-code must
 *       somehow user compiler-provided functionality for creating
 *       new threads and/or inform DCC once a new thread has been
 *       launched so-as to setup exception handlers properly.
 *     - Windows SEH exception handling cannot be used as doing so
 *       would interfere with the exception handlers set up to
 *       capture and process exception codes used for synchronizing
 *       the DRT execution environment with DCC's section data.
 *       NOTE: The same goes for unit signal hooks.
 *       (TODO: DRT is lacking hooks for unix signals...)
 *     - With changes to assembly, code will not be identical to
 *       that produced when DRT is disabled ('-d' is not passed on
 *       the commandline), as well as the inability to output DRT
 *       code, as well as generate an ELF/PE executable.
 */


#if DCC_CONFIG_HAVE_DRT
#include <stdint.h>
#ifndef _WIN32
#include <pthread.h>
#endif

DCC_DECL_BEGIN

#ifdef _MSC_VER
#pragma warning(push)
#pragma warning(disable: 4201)
#endif

#define DCPUGPREGISTER_OFFSETOF_AX   0
#define DCPUGPREGISTER_OFFSETOF_AL   0
#define DCPUGPREGISTER_OFFSETOF_AH   1
#define DCPUGPREGISTER_OFFSETOF_CX  (DCC_TARGET_SIZEOF_POINTER)
#define DCPUGPREGISTER_OFFSETOF_CL  (DCC_TARGET_SIZEOF_POINTER)
#define DCPUGPREGISTER_OFFSETOF_CH  (DCC_TARGET_SIZEOF_POINTER+1)
#define DCPUGPREGISTER_OFFSETOF_DX  (DCC_TARGET_SIZEOF_POINTER*2)
#define DCPUGPREGISTER_OFFSETOF_DL  (DCC_TARGET_SIZEOF_POINTER*2)
#define DCPUGPREGISTER_OFFSETOF_DH  (DCC_TARGET_SIZEOF_POINTER*2+1)
#define DCPUGPREGISTER_OFFSETOF_BX  (DCC_TARGET_SIZEOF_POINTER*3)
#define DCPUGPREGISTER_OFFSETOF_BL  (DCC_TARGET_SIZEOF_POINTER*3)
#define DCPUGPREGISTER_OFFSETOF_BH  (DCC_TARGET_SIZEOF_POINTER*3+1)
#define DCPUGPREGISTER_OFFSETOF_SP  (DCC_TARGET_SIZEOF_POINTER*4)
#define DCPUGPREGISTER_OFFSETOF_BP  (DCC_TARGET_SIZEOF_POINTER*5)
#define DCPUGPREGISTER_OFFSETOF_SI  (DCC_TARGET_SIZEOF_POINTER*6)
#define DCPUGPREGISTER_OFFSETOF_DI  (DCC_TARGET_SIZEOF_POINTER*7)
#define DCPUGPREGISTER_SIZEOF       (DCC_TARGET_SIZEOF_POINTER*8)
union DCPUGPRegister {
#if DCC_TARGET_HASI(I_X86)
#if DCC_TARGET_HASF(F_X86_64)
#define REGISTER8(name) \
 union{ uint64_t u_r##name; uint32_t u_e##name; uint16_t u_##name; \
         int64_t s_r##name;  int32_t s_e##name;  int16_t s_##name; \
 }
#define REGISTER4(name) \
 union{ uint64_t u_r##name##x; uint32_t u_e##name##x; uint16_t u_##name##x; struct { uint8_t u_##name##l,u_##name##h; }; \
         int64_t s_r##name##x;  int32_t s_e##name##x;  int16_t s_##name##x; struct {  int8_t s_##name##l,u_##name##h; }; \
 }
#else /* F_X86_64 */
#define REGISTER8(name) \
 union{ uint32_t u_e##name; uint16_t u_##name; \
         int32_t s_e##name;  int16_t s_##name; \
 }
#define REGISTER4(name) \
 union{ uint32_t u_e##name##x; uint16_t u_##name##x; struct { uint8_t u_##name##l,u_##name##h; }; \
         int32_t s_e##name##x;  int16_t s_##name##x; struct {  int8_t s_##name##l,s_##name##h; }; \
 }
#endif /* !F_X86_64 */
struct{
 /* Pretty names for general purpose registers. */
 REGISTER4(a);  /* AX */
 REGISTER4(c);  /* CX */
 REGISTER4(d);  /* DX */
 REGISTER4(b);  /* BX */
 REGISTER8(sp); /* SP */
 REGISTER8(bp); /* BP */
 REGISTER8(si); /* SI */
 REGISTER8(di); /* DI */
};
#undef REGISTER8
#undef REGISTER4
 target_off_t s_gp[8]; /* Signed GP registers. */
 target_ptr_t u_gp[8]; /* Unsigned GP registers. */
#else
#error FIXME
#endif
};

#define DCPUEFREGISTER_OFFSETOF_FLAGS 0
#define DCPUEFREGISTER_SIZEOF         4
union DCPUEFRegister {
 uint16_t i_flags;
 uint32_t i_eflags;
#if DCC_TARGET_HASF(F_X86_64)
#undef DCPUEFREGISTER_SIZEOF
#define DCPUEFREGISTER_SIZEOF 8
 uint64_t i_rflags;
#endif
};

#define DCPUIPREGISTER_OFFSETOF_IP 0
#define DCPUIPREGISTER_SIZEOF      4
union DCPUIPRegister {
 uint16_t i_ip;
 uint32_t i_eip;
#if DCC_TARGET_HASF(F_X86_64)
#undef DCPUIPREGISTER_SIZEOF
#define DCPUIPREGISTER_SIZEOF 8
 uint64_t i_rip;
#endif
 void    *i_code; /* The absolute code address of the runtime.
                   * This member is usually be located within a known section. */
};

#ifdef _MSC_VER
#pragma warning(pop)
#endif


/* Structure used to store the CPU state. */

#define DCPUSTATE_OFFSETOF_GPREG   0
#define DCPUSTATE_OFFSETOF_EFREG   DCPUGPREGISTER_SIZEOF
#define DCPUSTATE_OFFSETOF_IPREG  (DCPUGPREGISTER_SIZEOF+DCPUEFREGISTER_SIZEOF)
#define DCPUSTATE_SIZEOF (DCPUGPREGISTER_SIZEOF+DCPUEFREGISTER_SIZEOF+DCPUIPREGISTER_SIZEOF)
struct DCPUState {
 union DCPUGPRegister cs_gpreg; /*< General purpose register. */
 union DCPUEFRegister cs_efreg; /*< CPU Eflags register. */
 union DCPUIPRegister cs_ipreg; /*< Instruction pointer register. */
 /* TODO: Floating point registers? */
};


#ifdef _WIN32
typedef void     *DCC(thread_t);
typedef DWORD     DCC(threadid_t);
typedef void     *DCC(semaphore_t);
#define DRT_HAVE_THREAD_ID
#else
typedef pthread_t DCC(thread_t);
typedef sem_t     DCC(semaphore_t);
#endif


#define DRT_EVENT_NONE        0x00000000 /*< No event. */
#define DRT_EVENT_MIRROR_TEXT 0x00000001 /*< Load text. */
#define DRT_EVENT_MIRROR_DATA 0x00000002 /*< Load data. */
#define DRT_EVENT_MIRROR_RELO 0x00000003 /*< TODO: Missing: Load relocations. */

struct DRTUserEvent {
 DCC(semaphore_t)   ue_sem;  /*< Semaphore that the user-thread may wait for when  */
 /*atomic*/uint32_t ue_code; /*< Event code (One of 'DRT_EVENT_*'). */
 union{
  struct { /* DRT_EVENT_MIRROR_TEXT */
   void  *te_addr;       /*< [in]  Start of text data to fetch. */
   size_t te_relc_ok;    /*< [out] Amount of newly loaded relocations. */
   size_t te_size_ok;    /*< [out] Amount of newly loaded text bytes. */
   size_t te_size_total; /*< [out] Total amount of bytes that were checked. */
  } ue_text;
  struct { /* DRT_EVENT_MIRROR_DATA */
   void  *de_addr;    /*< [in]  Start of text data to fetch. */
   size_t de_size;    /*< [in]  Min amount of bytes to try and mirror (may not be ZERO(0)). 
                       *  [out] Amount of successfully loaded bytes (Or ZERO(0) if a faulty address was given).
                       *        When this field is '(size_t)-1' upon exit, an invalid pointer was given. */
  } ue_data;
  struct { /* DRT_EVENT_MIRROR_RELO */
   void  *re_addr;    /*< [in]  Start of memory to scan for unresolved relocations. */
   size_t re_size;    /*< [in]  Amount of bytes to scan.
                       *  [out] Amount of bytes that were scanned. */
  } ue_relo;
 };
};

#if DCC_TARGET_BIN == DCC_BINARY_PE
#define DRT_PEIND_SLOTSIZE 64
struct DRTPEInd {
 struct DRTPEInd  *i_next;                    /*< [0..1][owned] Next cache entry. */
 size_t            i_using;                   /*< Amount of entries in use within this IND cache. */
 DCC(target_ptr_t) i_ptr[DRT_PEIND_SLOTSIZE]; /*< Vector of fixed-address PE indirection pointers. */
};
struct DRTPEIndirectionCache {
 struct DRTPEInd *ic_first; /*< [0..1][owned] First cache entry. */
};
#endif


#define DRT_FLAG_NONE     0x00000000
#define DRT_FLAG_ENABLED  0x00000001 /*< Set if DRT is enabled. */
#define DRT_FLAG_STARTED  0x00000002 /*< Set if DRT has been started. */
#define DRT_FLAG_JOINING  0x00000004 /*< Set once the compiler starts joining DRT. */
#define DRT_FLAG_JOINING2 0x00000008 /*< Set after 'DRT_FLAG_JOINING' when the compiler isn't going to do anything anymore. */

#define DRT_DEFAULT_STACKSIZE  0x00010000 /*< Default stack size. */
#define DRT_DEFAULT_FRAMESIZE  0x00001000 /*< Default frame size. */
#define DRT_DEFAULT_MAXSECTION 0x00100000 /*< Default max-section size. */
#define DRT_DEFAULT_BASEADDR   0x40000000 /*< Starting address for DRT section memory (NOTE: Only meant as a hint; may not be used on some hosts). */

#define DRT_FAULT_ADDRESS 0xfffff001 /* Faulty address used for filling in unresolved relocations in user-code. */

struct DRT {
 uint32_t            rt_flags;      /*< [flags] RT flags (Set of 'DRT_FLAG_*'). */
 DCC(target_siz_t)   rt_stacksize;  /*< Initial stack size for the RT thread. */
 DCC(target_siz_t)   rt_framesize;  /*< Default size of a stack-frame while its final size isn't known yet. */
 DCC(target_siz_t)   rt_maxsection; /*< The min amount of addressable bytes that
                                     *  should be reserved between RT section. */
 uint8_t DRT_USER   *rt_baseaddr;   /*< The effective image base-address for DRT code.
                                     *  This address usually lies in high memory to
                                     *  ensure that the DRT has a lot of addressable
                                     *  memory to call its own. */
 uint8_t DRT_USER   *rt_nextaddr;   /*< The base address of the next uninitialized section. */
 DCC(thread_t)       rt_thread;     /*< [valid_if(DRT_FLAG_STARTED)] RT thread handle. */
#ifdef DRT_HAVE_THREAD_ID
 DCC(threadid_t)     rt_threadid;   /*< [valid_if(DRT_FLAG_STARTED)] RT thread id. */
#endif
 struct DRTUserEvent rt_event;      /*< [valid_if(DRT_FLAG_STARTED)] The pending user-thread event. */
#if DCC_TARGET_BIN == DCC_BINARY_PE
 struct DRTPEIndirectionCache rt_peind; /*< Cache of PE indirection pointers. */
#endif
};



DCCDAT struct DRT DRT_Current;

/* Initialize/Finalize DRT data structures. */
DCCFUN void DRT_Init(void);
DCCFUN void DRT_Quit(void);

#if DCC_TARGET_BIN == DCC_BINARY_PE
/* Allocate a fixed-address PE indirection pointer.
 * @return: NULL: Failed to allocate a fixed indirection slot. */
DCCFUN DCC(target_ptr_t) *DRT_AllocPEIndirection(void);
#endif

/* Enable DRT (called when '-d' is passed on the commandline) */
#define DRT_Enable()  (void)(DRT_Current.rt_flags |= DRT_FLAG_ENABLED)

/* Start the DRT execution thread.
 * The thread will begin execution at 'entry_point'.
 * All registers will be pre-initialized to those
 * described by 'misc_state', with a new exceptions:
 *  - Kernel-hooks have been configured for lazy initialization of section data.
 *  - The instruction pointer is fixed to point at 'entry_point'
 *  - A unique stack of dynamic memory has been allocated for
 *    the thread meaning that X86's 'SP' and 'BP' pointers
 *    now point to the base of that stack.
 *  - An empty stack-frame with a return address pointing
 *    to code that will terminate the application will
 *    be called should the given 'entry_point' ever return.
 * WARNING: The caller is responsible to enable DRT because calling this function.
 */
DCCFUN void DRT_Start(struct DCCSym *__restrict entry_point,
                      struct DCPUState const *misc_state);

/* Activate the given CPU state. */
DCCFUN DCC_ATTRIBUTE_NORETURN void
DRT_SetCPUState(struct DCPUState const *__restrict state);

#define DRT_ENABLED()  (DRT_Current.rt_flags&DRT_FLAG_ENABLED)
#define DRT_STARTED()  (DRT_Current.rt_flags&DRT_FLAG_STARTED)

/* DRT synchronization point.
 * This function should be called periodically from the compiler thread
 * to give DRT a chance for synchronizing section data, as well as
 * exchange other messages such as relocation information without
 * the need of expensive individual synchronization of said data
 * by implicitly ensuring thread safety by having the compiler thread
 * post its data to DRT on its own behalf instead of DRT trying to
 * read data randomly and either doing some hacky shenanigans or
 * needing to setup hundreds of locks around every section or symbol.
 * NOTE: This function is a no-op when DRT is disabled.
 * 
 * 'DRT_SyncAll' behaves similar to 'DRT_Sync', but will not
 * return until all user-requests for data have been handled.
 * With that in mind, 'DRT_SyncAll' will also eventually join()
 * the user-thread, meaning that it should only be called once
 * all input files have been compiled.
 * @return: DRT_SYNC_NONE:       No data was synchronized.
 * @return: DRT_SYNC_OK:         Data was synchronized.
 * @return: DRT_SYNC_UNRESOLVED: Failed to synchronize data due to undefined symbols / missing data.
 * @return: DRT_SYNC_FAULT:      Failed to synchronize data due to an invalid address range.
 */
#ifdef __INTELLISENSE__
DCCFUN int DRT_Sync(void);
DCCFUN int DRT_SyncAll(void);
#else
DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_Sync(int warn_failure);
DCCFUN int DCC_ATTRIBUTE_FASTCALL DRT_H_SyncAll(void);
#define DRT_Sync()    (DRT_STARTED() ? DRT_H_Sync(0) : 0)
#define DRT_SyncAll() (DRT_STARTED() ? DRT_H_SyncAll() : 0)
#endif
#define DRT_SYNC_NONE       0
#define DRT_SYNC_OK         1
#define DRT_SYNC_UNRESOLVED 2
#define DRT_SYNC_FAULT      3

#ifdef DCC_PRIVATE_API
/* Allocate/Free virtual memory, or set permissions.
 * @param: vaddr:       The virtual address to allocate memory at.
 *                      [DRT_VMall] When 'DRT_VANY', let the kernel decide where to allocate memory.
 * @param: n_bytes:     The amount of bytes of allocate/update/free.
 * @param: perm:        Memory permissions (Set of 'DCC_SYMFLAG_SEC_(R|W|X)')
 * @return: * :         Virtual base address of allocated memory (always equal to 'vaddr' if not 'DRT_VANY')
 * @return: DRT_VERROR: Failed to allocate memory for one reason or another (_NO_ lexer error is set). */
INTERN void DRT_USER *DRT_VMall(void DRT_USER *vaddr, size_t n_bytes, DCC(symflag_t) prot);
INTERN void DRT_USER *DRT_VProt(void DRT_USER *vaddr, size_t n_bytes, DCC(symflag_t) prot);
INTERN void           DRT_VFree(void DRT_USER *vaddr, size_t n_bytes);
#define DRT_VANY   ((void DRT_USER *)(uintptr_t)0)
#if DCC_HOST_OS == DCC_OS_WINDOWS
#define DRT_VERROR ((void DRT_USER *)(uintptr_t)0)
#else
#define DRT_VERROR ((void DRT_USER *)(uintptr_t)-1)
#endif
#endif /* DCC_PRIVATE_API */


#ifdef DCC_PRIVATE_API
#ifdef __INTELLISENSE__
DCCDAT struct DRT drt;
#else
#define drt  DRT_Current
#endif
#endif

DCC_DECL_END
#else /* DCC_CONFIG_HAVE_DRT */
#define DRT_Init()            (void)0
#define DRT_Quit()            (void)0
#define DRT_Sync()            (void)0
#define DRT_SyncAll()         (void)0
#define DRT_ENABLED()          DCC_MACRO_COND(0)
#define DRT_STARTED()          DCC_MACRO_COND(0)
#define DRT_SignalSymbol(sym)           (void)0
#define DRT_VERROR           ((void DRT_USER *)(uintptr_t)0)
#endif /* !DCC_CONFIG_HAVE_DRT */

#endif /* !GUARD_DRT_DRT_H */
