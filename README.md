# dcc - Direct C Compiler

A <b>C99 compliant</b> C compiler with extensions implementing many extensions and features, as well as arbirary-length integer arithmetic.

Currently only able to target <b>I386 and above</b>, support for x86-64 is planned and already partially implemented.

Supported output formats are <b>ELF</b>, windows <b>PE</b>, as well as <b>direct execution</b> of generated code.

DCC supports <b>AT&T</b> inline assembly syntax, emulating gcc's <code>\_\_asm\_\_</code> statement and the GNU assembler as well as direct parsing of assembly sources.

Using TPP as preprocessor to implement a fully featured perprocessor, DCC implements many GCC extensions such as <code>\_\_asm\_\_</code>, <code>\_\_builtin\_constant\_p</code>, many <code>\_\_attribute\_\_</code>-s, <code>\_\_typeof\_\_</code>, <code>\_\_auto\_type</code>, and many more, including my own twist on awesome C extensions.

Development on DCC started on <b>17.04.2017</b>, eversince then being the usual <b>one-person project</b>.

## Current state:
Note that DCC is still fairly early in its development, meaning that anything can still change and that more features will be added eventually.
  - Link against windows PE binaries/libraries (<b>\*.dll</b>).
  - Statically link against PE binaries (as is clone everything from a <b>\*.dll</b>)
  - Dynamically/Statically link against ELF binaries/libraries/object files (<b>\*</b>, <b>\*.so</b>, <b>\*.o</b>)
  - Output windows PE binary/library (<b>\*.exe</b>, <b>\*.dll</b>).
  - Output linux ELF binary/library (<b>\*</b>, <b>\*.so</b>).
  - Output ELF relocatable object files (<b>\*.o</b>)
  - Process and merge multiple source-/object files/static libraries.
  - Compiling DCC is only tested and working in Visual Studio.
  - Full STD-C compliance up to C99
 
## Planned features:
  - Full ELF target binary support.
  - Support for X86-64/AMD64 CPU architectures.
  - Compiling DCC under GCC (may already work; won't be too hard if it doesn't).
  - Compiling DCC on linux (most of the work's already there, but nothing's tested yet).
  - Compiling DCC with DCC (because every C compiler must be able to do that!).
  - Generation of debug information (recognizeable by gdb).
  - Finish many partially implemented features (see below).
  - Support for ~true~ thread-local storage (aka. segment-based)

## Features (Compiler):
  - DCC as host compiler can easily be detected with <code>defined(\_\_DCC\_VERSION\_\_)</code>.
  - Using TPP as preprocessor, <b>every existing</b> preprocessor extension is supported, as well as all that are exclusive to mine.
  - Live-compilation-mode directly generates assembly.
  - C-conforming symbol forward/backward declaration.
  - K&R-C compatible
  - Full STD-C89/90 compliance
  - Full STD-C95 compliance
  - Full STD-C99 compliance
  - Supports all C standard types.
  - Supports 64-bit <code>long long</code> integrals (using double-register storage).
  - Supports all C control statements.
  - Supports C11 <code>\_Generic</code>.
  - Supports C11 <code>\_Atomic</code> (Not fully implemented).
  - Supports C99 <code>\_Bool</code>.
  - Supports C99 <code>\_\_func\_\_</code> builtin identifier.
  - Supports Variable declaration in if-expressions and for-initializers.
  - Supports nested function declaration, as well as access to variables from surrounding scopes.
  - Supports C++ lvalue types (<code>int y = 10; int &x = y;</code>).
  - Supports C structure bitfields
  - Support for GCC statement-expressions: <code>int x = ({ int z = 10; z+20; }); // x == 30</code>.
  - Support for <code>\_\_FUNCTION\_\_</code> and <code>\_\_PRETTY\_FUNCTION\_\_</code>, including use by concat with other strings: <code>char *s = "Function " \_\_FUNCTION\_\_ " was called"; printf("%s\n",s);</code>.
  - Support for GCC <code>\_\_sync\_*</code> builtin functions (<code>\_\_sync\_val\_compare\_and\_swap(&x,10,20)</code>).
  - Supports all compiler-slangs for alignof: <code>\_Alignof</code>, <code>\_\_alignof</code>, <code>\_\_alignof\_\_</code> and <code>\_\_builtin\_alignof</code>.
  - Support for compile-time type deduction from expressions: <code>typeof</code>, <code>\_\_typeof</code>, <code>\_\_typeof\_\_</code>.
  - Support for GCC scoped labels: <code>\_\_label\_\_</code>.
  - Support for GCC-style inline assembly: <code>\_\_asm\_\_("ret")</code>.
  - Support for MSVC fixed-length integer types: <code>\_\_int(8|16|32|64)</code>.
  - Support for GCC <code>\_\_auto\_type</code> (as well as special interpretation of <code>auto</code> when not used as storage class. - <code>auto int x = 42</code> auto is storage class; <code>auto y = 10;</code> auto declares automatic type deduction).
  - Support for C99 variable-length arrays: <code>int x = 10; int y[x*2]; assert(sizeof(x) == 80);</code>.
  - Support for old (pre-STDC: K&R-C) function declarations/implementations.
  - Support for new (post-STDC: C90+) function declarations/implementations.
  - Support for floating-point types (Assembly generator is not implemented yet).
  - Inherited from assembly: Named register identifiers.
    - <code>int x = %eax;</code> (CPU-specific, on i386 compiles to <code>mov %eax, x</code>).
  - Inherited from assembly: Get current text address.
    - <code>void *p = .;</code> (Evaluates to the current text address with <code>void *</code> typing).
  - Use label names in expressions:
    - <code>void *p = &&my\_label; my\_label: printf("p = %p\n",p);</code>
  - Support for new & old GCC structure/array initializer:
    - dot-field: <code>struct { int x,y; } p = { .x = 10, .y = 20 };</code>
    - field-collon: <code>struct point { int x,y; } p = { x: 10, y: 20 };</code>
    - array-subscript: <code>int alpha[256] = { ['a' ... 'z'] = 1, ['A' ... 'Z'] = 1, ['\_'] = 1 };</code>
  - Support for runtime brace-initializers: <code>struct point p = { .x = get_x(), .y = get_y() };</code>
  - Split between struct/union/enum, declaration and label namespaces:<code>foo: struct foo foo; // Valid code and 3 different 'foo'</code>
  - Support for unnamed struct/union inlining:
    - <code>union foo { \_\_int32 x; struct { \_\_int16 a,b; }; };</code>
      - <code>offsetof(union foo.x) == 0</code>, <code>offsetof(union foo.a) == 0</code>, <code>offsetof(union foo.b) == 2</code>
  - Support for builtin functions offering special compile-time optimizations, or functionality (Every builtin can be queried with <code>\_\_has\_builtin(...)</code>):
    - <code>char const (&\_\_builtin\_typestr(type\_or\_expr t))[];</code>
      - Accepting arguments just like 'sizeof', return a human-readable representation of the [expression's] type as a compile-time array of characters allocated in the '.string' section.
    - <code>\_Bool \_\_builtin\_constant\_p(expr x);</code>
    - <code>expr \_\_builtin\_choose\_expr(constexpr \_Bool c, expr tt, expr ff);</code>
    - <code>\_Bool \_\_builtin\_types\_compatible\_p(type t1, type t2);</code>
    - <code>void \_\_builtin\_unreachable(void) \_\_attribute\_\_((noreturn));</code>
    - <code>void \_\_builtin\_trap(void) \_\_attribute\_\_((noreturn));</code>
    - <code>void \_\_builtin\_breakpoint(void);</code>
      - Emit a CPU-specific instruction to break into a debugging environment
    - <code>void *\_\_builtin\_alloca(size\_t s);</code>
    - <code>void *\_\_builtin\_alloca\_with\_align(size\_t s, size\_t a);</code>
    - <code>void \_\_builtin\_assume(expr x),\_\_assume(expr x);</code>
    - <code>long \_\_builtin\_expect(long x, long e);</code>
    - <code>const char (&\_\_builtin\_FILE(void))[];</code>
    - <code>int \_\_builtin\_LINE(void);</code>
    - <code>const char (&\_\_builtin\_FUNCTION(void))[];</code>
    - <code>void *\_\_builtin\_assume\_aligned(void *p, size\_t align, ...);</code>
    - <code>size\_t \_\_builtin\_offsetof(typename T, members...);</code>
    - <code>T (\_\_builtin\_bitfield(T expr, constexpr int const\_index, constexpr int const\_size)) : const\_size;</code>
      - Access a given sub-range of bits of any integral expression, the same way access is performed for structure bit-fields.
    - <code>typedef ... \_\_builtin\_va\_list;</code>
    - <code>void \_\_builtin\_va\_start(\_\_builtin\_va\_list &ap, T &start);</code>
    - <code>void \_\_builtin\_va\_end(\_\_builtin\_va\_list &ap);</code>
    - <code>void \_\_builtin\_va\_copy(\_\_builtin\_va\_list &dstap, \_\_builtin\_va\_list &srcap);</code>
    - <code>T \_\_builtin\_va\_arg(\_\_builtin\_va\_list &ap, typename T);</code>
      - Compiler-provided var-args helpers for generating smallest-possible code
    - <code>int \_\_builtin\_setjmp(T &buf);</code>
    - <code>void \_\_builtin\_longjmp(T &buf, int sig) \_\_attribute\_\_((noreturn));</code>
      - Requires: <code>sizeof(T) == \_\_SIZEOF\_JMP\_BUF\_\_</code>
      - Compile-time best-result code generation for register save to 'buf'
      - Optimizations for 'sig' known to never be '0'
    - <code>uint16\_t \_\_builtin\_bswap16(uint16\_t x);</code>
    - <code>uint32\_t \_\_builtin\_bswap32(uint32\_t x);</code>
    - <code>uint64\_t \_\_builtin\_bswap64(uint64\_t x);</code>
    - <code>int \_\_builtin\_ffs(int x);</code>
    - <code>int \_\_builtin\_ffsl(long x);</code>
    - <code>int \_\_builtin\_ffsll(long long x);</code>
    - <code>int \_\_builtin\_clz(int x);</code>
    - <code>int \_\_builtin\_clzl(long x);</code>
    - <code>int \_\_builtin\_clzll(long long x);</code>
      - Generate inline code with per-case optimizations for best results
    - <code>T \_\_builtin\_bswapcc(T x, size\_t s = sizeof(T));</code>
    - <code>int \_\_builtin\_ffscc(T x, size\_t s = sizeof(T));</code>
    - <code>int \_\_builtin\_clzcc(T x, size\_t s = sizeof(T));</code>
      - General purpose functions that works for any size
    - <code>void *\_\_builtin\_memcpy(void *dst, void const *src, size\_t s);</code>
      - Replace with inlined code for sizes known at compile-time
      - Warn about dst/src known to overlap
    - <code>void *\_\_builtin\_memmove(void *dst, void const *src, size\_t s);</code>
      - Optimize away dst == src cases
      - Hint about dst/src never overlapping
    - <code>void *\_\_builtin\_memset(void *dst, int byte, size\_t s);</code>
      - Replace with inlined code for sizes known at compile-time
    - <code>int \_\_builtin\_memcmp(void const *a, void const *b, size\_t s);</code>
      - Replace with compile-time constant for constant
      - Replace with inline code for sizes known at compile-time
    - <code>size\_t \_\_builtin\_strlen(char const *s);</code>
      - Resolve length of static strings at compile-time
  - Split between declaration and assembly name (aka. <code>\_\_asm\_\_("foo")</code> suffix in declarations)
  - Arbitrary size arithmetic operations (The sky's the limit; as well as your binary size bloated with hundreds of add-instructions for one line of source code).
  - Support for deemon's 'pack' keyword (now called <code>\_\_pack</code>):
    - Can be used to emit parenthesis almost everywhere (except in the preprocessor, or when calling macros)
  - Explicit alignment of code, data, or entire sections in-source
  - Support for <code>#pragma comment(lib,"foo")</code> to link against a given library "foo"
  - Support for <code>#pragma pack(...)</code>
  - Supports GCC builtin macros for fixed-length integral constants (<code>\_\_(U)INT(8|16|32|64|MAX)\_C(...)</code>).
  - GCC-compatible predefined CPU macros, such as <code>\_\_i386\_\_</code> or <code>\_\_LP64\_\_</code>.
  - Support for GCC builtin macros, such as <code>\_\_SIZEOF\_POINTER\_\_</code>, <code>\_\_SIZE\_TYPE\_\_</code>, etc.

## Features (Attributes):
  - Ever attribute can be written in one of three ways:
    - GCC attribte syntax (e.g.: <code>\_\_attribute\_\_((noreturn))</code>)
    - cxx-11 attributes syntax (e.g.: <code>[[noreturn]]</code>)
    - MSVC declspec syntax (e.g.: <code>\_\_declspec(noreturn)</code>)
  - The name of an attribute (in the above examples <code>noreturn</code>) can be written with any number of leading, or terminating underscores to prevent ambiguity with user-defined macros:
    - <code>\_\_attribute\_\_((\_\_\_\_noreturn\_))</code> is the same as <code>\_\_attribute\_\_((noreturn))</code>
  - The following attributes (as supported by other compiler) are recognized:
    - <code>\_\_attribute\_\_((noreturn*))</code>
    - <code>\_\_attribute\_\_((warn\_unused\_result*))</code>
    - <code>\_\_attribute\_\_((weak*))</code>
    - <code>\_\_attribute\_\_((dllexport*))</code>
    - <code>\_\_attribute\_\_((dllimport*))</code>
    - <code>\_\_attribute\_\_((visibility("default")))</code>
    - <code>\_\_attribute\_\_((alias("my\_alias")))</code>
    - <code>\_\_attribute\_\_((weakref("my\_alias")))</code>
    - <code>\_\_attribute\_\_((used*))</code>
    - <code>\_\_attribute\_\_((unused*))</code>
    - <code>\_\_attribute\_\_((cdecl*))</code>
    - <code>\_\_attribute\_\_((stdcall*))</code>
    - <code>\_\_attribute\_\_((thiscall*))</code>
    - <code>\_\_attribute\_\_((fastcall*))</code>
    - <code>\_\_attribute\_\_((section(".text")))</code>
    - <code>\_\_attribute\_\_((regparm(x)))</code>
    - <code>\_\_attribute\_\_((naked*))</code>
    - <code>\_\_attribute\_\_((deprecated))</code>
    - <code>\_\_attribute\_\_((deprecated(msg)))</code>
    - <code>\_\_attribute\_\_((aligned(x)))</code>
    - <code>\_\_attribute\_\_((packed*))</code>
    - <code>\_\_attribute\_\_((transparent\_union*))</code>
    - <code>\_\_attribute\_\_((mode(x)))</code> (Underscores surrounding <code>x</code> are ignored)
    - All attribute names marked with '*' accept an optional suffix that adds an enabled-dependency on a compiler-time expression. (e.g.: <code>\_\_attribute\_\_((noreturn(sizeof(int) == 4)))</code> - Mark as noreturn, if <code>int</code> is <code>4</code> bytes wide)
  - Attributes not currently implemented (But planned to be):
    - <code>\_\_attribute\_\_((constructor))</code>
    - <code>\_\_attribute\_\_((constructor(priority)))</code>
    - <code>\_\_attribute\_\_((destructor))</code>
    - <code>\_\_attribute\_\_((destructor(priority)))</code>
    - <code>\_\_attribute\_\_((ms\_struct))</code>
    - <code>\_\_attribute\_\_((gcc\_struct))</code>
  - Attributes ignored without warning:
    - <code>\_\_attribute\_\_((noinline...))</code>
    - <code>\_\_attribute\_\_((returns\_twice...))</code>
    - <code>\_\_attribute\_\_((force\_align\_arg\_pointer...))</code>
    - <code>\_\_attribute\_\_((cold...))</code>
    - <code>\_\_attribute\_\_((hot...))</code>
    - <code>\_\_attribute\_\_((pure...))</code>
    - <code>\_\_attribute\_\_((nothrow...))</code>
    - <code>\_\_attribute\_\_((noclone...))</code>
    - <code>\_\_attribute\_\_((nonnull...))</code>
    - <code>\_\_attribute\_\_((malloc...))</code>
    - <code>\_\_attribute\_\_((leaf...))</code>
    - <code>\_\_attribute\_\_((format\_arg...))</code>
    - <code>\_\_attribute\_\_((format...))</code>
    - <code>\_\_attribute\_\_((externally\_visible...))</code>
    - <code>\_\_attribute\_\_((alloc\_size...))</code>
    - <code>\_\_attribute\_\_((always\_inline...))</code>
    - <code>\_\_attribute\_\_((gnu\_inline...))</code>
    - <code>\_\_attribute\_\_((artificial...))</code>
  - New attributes added by DCC:
    - <code>\_\_attribute\_\_((lib("foo")))</code>
      - Most effective for PE targets: 'foo' is the name of the DLL file that the associated declaration should be linked against.
      - Using this attribute, one can link against DLL files that don't exist at compile-time, or create artificial dependencies on ELF targets.
    - <code>\_\_attribute\_\_((arithmetic*))</code>
      - Used on struct types of arbirary size to enable arithmetic operations with said structure. Using this attribute you could easily create e.g.: a 512-bit integer type.
        - Most operators are implemented through inline-code, but some (mul,div,mod,shl,shr,sar) generate calls to external symbols.
      - When this attribute is present, the associated structure type can be modified with 'signed'/'unsigned' to control the sign-behavior.
  - In addition, the following keywords can be used anywhere attributes are allowed.
    - <code>{\_}\_cdecl</code>: Same as <code>\_\_attribute\_\_((cdecl))</code>
    - <code>{\_}\_stdcall</code>: Same as <code>\_\_attribute\_\_((stdcall))</code>
    - <code>{\_}\_fastcall</code>: Same as <code>\_\_attribute\_\_((fastcall))</code>
    - <code>\_\_thiscall</code>: Same as <code>\_\_attribute\_\_((thiscall))</code>

## Features (Warnings):
  - DCC features an enourmous amount of warnings covering everything from code quality, to value truncation, to syntax errors, to unresolved references during linkage, etc...
  - Any warning can be configured as
    - <b>Disabled</b>: (Compilation is continued, but based on severity, generated assembly/binary may be wrong)
    - <b>Enabled</b>: Emit a warning, but continue compilation as if it was disabled
    - <b>Error</b>: Emit an error message and halt compilation at the next convenient location
    - <b>Supress</b>: Works recursively: Handle the warning as <b>Disabled</b> for every time it is suppressed before reverting its state to before it was.
  - Warnings are sorted into named groups that can be disabled as a whole. The main group of a warning is always displayed when it is emit. (e.g.: <code>W1401("-WSyntax"): Expected ']', but got ...</code>)
  - The global warning state can be pushed/popped from usercode:
    - <b>Push</b>:
      - <code>#pragma warning(push)</code>
      - <code>#pragma GCC diagnostic push</code>
    - <b>Pop</b>:
      - <code>#pragma warning(pop)</code>
      - <code>#pragma GCC diagnostic pop</code>
  - Individual warnings/warning group states can be explicitly defined from usercode:
    - <b>Disabled</b>:
      - <code>#pragma warning("[-][W]no-\<name\>")</code>
      - <code>#pragma warning(disable: \<IDS\>)</code>
      - <code>#pragma warning(disable: "[-][W]\<name\>")</code>
      - <code>#pragma GCC diagnostic ignored "[-][W]\<name\>"</code>
    - <b>Enabled</b>:
      - <code>#pragma warning(enable: \<IDS\>)</code>
      - <code>#pragma warning(enable: "[-][W]\<name\>")</code>
      - <code>#pragma GCC diagnostic warning "[-][W]\<name\>"</code>
    - <b>Error</b>:
      - <code>#pragma warning(error: \<IDS\>)</code>
      - <code>#pragma warning(error: "[-][W]\<name\>")</code>
      - <code>#pragma GCC diagnostic error "[-][W]\<name\>"</code>
    - <b>Suppress</b> (once for every time a warning/group is listed):
      - <code>#pragma warning(suppress: \<IDS\>)</code>
      - <code>#pragma warning(suppress: "[-][W]\<name\>")</code>
      - <code>#pragma warning("[-][W]sup-\<name\>")</code>
      - <code>#pragma warning("[-][W]suppress-\<name\>")</code>
    - Revert to default state:
      - <code>#pragma warning(default: \<IDS\>)</code>
      - <code>#pragma warning(default: "[-][W]\<name\>")</code>
      - <code>#pragma warning("[-][W]def-\<name\>")</code>
    - <code>IDS</code> is a space-separated list of individual warning IDS as integral constants
      - Besides belonging to any number of groups, each warning also has an ID
      - Use of these <code>IDS</code> should be refrained from, as they might change randomly
    - Similar to the <code>extension</code>-pragma, <code>#pragma warning(...)</code> accepts a comma-seperated list of commands.
      - <code>#pragma warning(push,disable: "-Wsyntax")</code>
  - All warnings can be enabled/disabled on-the-fly using pragmas:
    - <code>#pragma warning(push|pop)</code> Push/pop currently enabled extensions
    - <code>#pragma warning("-W\<name\>")</code> Enable warning 'name'
    - <code>#pragma warning("-Wno-\<name\>")</code> Disable warning 'name'
  - <code>#pragma GCC system\_header</code> treats the current input file as though all warnings disabled
    - Mainly meant for headers in <i>/fixinclude</i> which may re-define type declarations, but are not meant to cause any problems

## Features (Extensions):
  - Extensions are implemented in two different ways:
    - Extensions that are always enabled, but emit a warning when used.
      - The warning can either be disabled individually (e.g.: <code>#pragma warning("-Wno-declaration-in-if")</code>).
      - Or all extension warnings can be disabled using <code>#pragma warning("-Wno-extensions")</code>.
      - Don't let yourself be fooled. Writing <code>"-Wno-extensions"</code> disables warnings about extensions, not extensions themself!
      - Some warnings are also emit for deprecated or newer language features.
      - <code>"constant-case-expressions"</code>: Emit for old-style function declarations.
      - <code>"old-function-decl"</code>: Emit for old-style function declarations.
    - Extensions that may change semantics and can therefor be disabled.
      - All of these extensions can be enabled/disabled on-the-fly using pragmas:
        - As comma-seperated list in <code>#pragma extension(...)</code>
          - <code>push</code>: Push currently enabled extensions (e.g.: <code>#pragma extension(push)</code>)
          - <code>pop</code>: Pop previously enabled extensions (e.g.: <code>#pragma extension(pop)</code>)
          - <code>"[-][f]\<name\>"</code>: Enable extension <code>name</code> (e.g.: <code>#pragma extension("-fmacro-recursion")</code>)
          - <code>"[-][f]no-\<name\>"</code>: Disable extension <code>name</code> (e.g.: <code>#pragma extension("-fno-macro-recursion")</code>)
      - <code>"expression-statements"</code>: Recognize GCC statement-expressions.
      - <code>"label-expressions"</code>: Allow use of labels in expression (prefixed by <code>&&</code>).
      - <code>"local-labels"</code>: Allow labels to be scoped (using GCC's <code>\_\_label\_\_</code> syntax).
      - <code>"gcc-attributes"</code>: Recognize GCC <code>\_\_attribute\_\_((...))</code> syntax.
      - <code>"msvc-attributes"</code>: Recognize MSVC <code>\_\_declspec(...)</code> syntax.
      - <code>"cxx-11-attributes"</code>: Recognize c++11 <code>[[...]]</code> syntax.
      - <code>"attribute-conditions"</code>: Allow optional conditional expression to follow a switch-attribute.
      - <code>"calling-convention-attributes"</code>: Recognize MSVC stand-alone calling convention attributes (e.g.: <code>\_\_cdecl</code>).
      - <code>"fixed-length-integer-types"</code>: Recognize fixed-length integer types (<code>\_\_int(8|16|32|64)</code>).
      - <code>"asm-registers-in-expressions"</code>: Allow assembly registers to be used in expressions (e.g.: <code>int x = %eax;</code>).
      - <code>"asm-address-in-expressions"</code>: Allow assembly registers to be used in expressions (e.g.: <code>int x = %eax;</code>).
      - <code>"void-arithmetic"</code>: <code>sizeof(void) == \_\_has\_extension("void-arithmetic") ? 1 : 0</code>.
      - <code>"struct-compatible"</code>: When enabled, same-layout structures are compatible, when disabled, only same-declaration structs are.
      - <code>"auto-in-type-expressions"</code>: Allow <code>auto</code> be be used either as storage class, or as alias for <code>\_\_auto\_type</code>.
      - <code>"variable-length-arrays"</code>: Allow declaration of C99 VLA variables.
      - <code>"function-string-literals"</code>: Treat <code>\_\_FUNCTION\_\_</code> and <code>\_\_PRETTY\_FUNCTION\_\_</code> as language-level string literals.
      - <code>"if-else-optional-true"</code>: Recognize GCC if-else syntax <code>int x = (p ?: other\_p)-\>x; // Same as '(p ? p : other\_p)-\>x'</code>.
      - <code>"fixed-length-integrals"</code>: Recognize MSVC fixed-length integer suffix: <code>\_\_int32 x = 42i32;</code>.
      - <code>"macro-recursion"</code>: Enable/Disable TCC recursive macro declaration.
      - Many more extensions are provided by TPP to control preprocessor syntax, such as <code>#include\_next</code> directives. Their list is too long to be documented here.

## Features (Optimization):
  - Dead code elimination
    - Correct deduction on merging branches, such as if-statement with two dead branches
    - Re-enable control flow when encountering a label
    - Correctly interpretation of <code>\_\_builtin\_unreachable()</code>
    - Correctly interpretation of <code>\_\_{builtin\_}assume(0)</code>
  - Automatic constant propagation
    - Even capable of handling generic offsetof: <code>(size\_t)&((struct foo *)0)-\>bar</code>
  - Automatic removal of unused symbols/data
    - Recursively delete unused functions/data symbols from generated binary
    - Can be suppressed for any symbol using <code>\_\_attribute\_\_((used))</code>
  - Automatic merging of data in sections marked with <code>M</code> (merge) (Not fully implemented, because of missing re-use counter; the rest already works)
    - Using the same string (or sub-string) more than once will only allocate a single data segment:
      - <code>printf("foobar\n"); printf("bar\n");</code> Re-use <code>"bar\n\0"</code> as a sub-string of <code>"foobar\n\0"</code>

## Features (Assembler):
  - Full AT&T Assembly support
  - Extension for fixed-length
  - Supported assembly directives are:
    - <code>.align \<N\> [, \<FILL\>]</code>
    - <code>.skip \<N\> [, \<FILL\>]</code>
    - <code>.space \<N\> [, \<FILL\>]</code>
    - <code>.quad \<I\></code>
    - <code>.short \<I\></code>
    - <code>.byte \<I\></code>
    - <code>.word \<I\></code>
    - <code>.hword \<I\></code>
    - <code>.octa \<I\></code>
    - <code>.long \<I\></code>
    - <code>.int \<I\></code>
    - <code>.fill \<REPEAT\> [, \<SIZE\> [, \<FILL\>]]</code>
    - <code>. = \<ORG\></code>
    - <code>.org \<ORG\></code>
    - <code>.extern \<SYM\></code>
    - <code>.global \<SYM\></code>
    - <code>.globl \<SYM\></code>
    - <code>.protected \<SYM\></code>
    - <code>.hidden \<SYM\></code>
    - <code>.internal \<SYM\></code>
    - <code>.weak \<SYM\></code>
    - <code>.size \<SYM\>, <\SIZE\></code>
    - <code>.string \<STR\></code>
    - <code>.ascii \<STR\></code>
    - <code>.asciz \<STR\></code>
    - <code>.text</code>
    - <code>.data</code>
    - <code>.bss</code>
    - <code>.section</code>
    - <code>.previous</code>
    - <code>.set \<SYM\>, \<VAL\></code>
    - <code>.include \<NAME\></code>
    - <code>.incbin \<NAME\> [, \<SKIP\> [, \<MAX\>]]</code>
  - CPU-specific, recognized directives:
    - I386+
      - <code>.code16</code>
      - <code>.code32</code>
    - X86-64
      - <code>.code64</code>
  - Directives ignored without warning:
    - <code>.file ...</code>
    - <code>.ident ...</code>
    - <code>.type ...</code>
    - <code>.lflags ...</code>
    - <code>.line ...</code>
    - <code>.ln ...</code>

## Features (Linker):
  - Integrated linker allows for direct (and very fast) creation of executables
  - Merge multiple source files into a single compilation unit
  - ELF-style visibility control/attributes (<code>\_\_attribute\_\_((visibility(...)))</code>)
  - Directly link against already-generated PE binaries
  - Add new library dependencies from source code (<code>#pragma comment(lib,...)</code>)
  - Output to PE binary (<b>\*.exe</b>/<b>\*.dll</b>)
 
 
 
