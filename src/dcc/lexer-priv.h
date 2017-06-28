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
#ifndef GUARD_DCC_LEXER_PRIV_H
#define GUARD_DCC_LEXER_PRIV_H 1

#include <dcc/common.h>
#include <dcc/lexer.h>
#include <dcc/target.h>

DCC_DECL_BEGIN

struct TPPKeyword;
struct DCCSym;
struct DCCDecl;

#define LEXDECL INTDEF
#define LEXPRIV INTERN

LEXDECL struct TPPKeyword *DCC_PARSE_CALL tok_without_underscores(void);

/* Peek the first character of what will likely be the next token. */
LEXDECL char *DCC_PARSE_CALL peek_next_token(struct TPPFile **tok_file);
LEXDECL char *DCC_PARSE_CALL peek_next_advance(char *p, struct TPPFile *__restrict *tok_file);
LEXDECL struct TPPKeyword *DCC_PARSE_CALL peek_next_keyword(int create_missing);

/* Using data retrieved by calling 'peek_next_token', return the keyword
 * associated with the given token text, or NULL if allocation/lookup failed,
 * or when the given token doesn't describe a keyword.
 * NOTE: Escaped linefeeds are automatically parsed.
 * WARNING: The caller is responsible to ensure that 'tok_begin' lies with the given 'tok_file' */
LEXDECL struct TPPKeyword *DCC_PARSE_CALL
peek_keyword(struct TPPFile *__restrict tok_file,
             char *__restrict tok_begin, int create_missing);


INTDEF void DCCDecl_CalculateFunctionOffsets(struct DCCDecl *__restrict funtydecl);
INTDEF void DCCDecl_CalculateStructureOffsets(struct DCCDecl *__restrict self);
INTDEF void DCCType_SetTypeMode(struct DCCType *__restrict self, uint32_t mode_flags);
INTDEF void DCCType_PromoteFunArg(struct DCCType *__restrict self); /* Do type promotions on a function argument type (e.g.: 'array/V-array --> pointer') */

#define ATTRKIND_ATTRIBUTE 0
#define ATTRKIND_DECLSPEC  1
#define ATTRKIND_BRACKETS  2
LEXDECL void DCC_PARSE_CALL DCCParse_AttrContent(struct DCCAttrDecl *__restrict self, int kind);

LEXDECL void DCC_PARSE_CALL DCCParse_CTypeNewArgumentList(struct DCCDecl *__restrict funtydecl, struct DCCType *opt_firsttype, struct TPPKeyword *opt_firstname, struct DCCAttrDecl *opt_firstattr);
LEXDECL void DCC_PARSE_CALL DCCParse_CTypeOldArgumentList(struct DCCDecl *__restrict funtydecl, struct TPPKeyword  *opt_firstname, struct DCCAttrDecl *opt_firstattr);
LEXDECL void DCC_PARSE_CALL DCCParse_CTypeOldArgumentListDefWithBase(struct DCCDecl *__restrict funtydecl, struct DCCType const *__restrict base_type, struct DCCAttrDecl const *__restrict base_attr);
LEXDECL void DCC_PARSE_CALL DCCParse_CTypeOldArgumentListDef(struct DCCDecl *__restrict funtydecl, struct DCCAttrDecl *empty_attr);
LEXDECL void DCC_PARSE_CALL DCCParse_CTypeOldArgumentListDefWithFirstBase(struct DCCDecl *__restrict funtydecl, struct DCCType *__restrict firstbase_type, struct DCCAttrDecl *__restrict firstbase_attr);
LEXDECL void DCC_PARSE_CALL DCCParse_CTypeArrayExt(struct DCCType *__restrict self, struct DCCAttrDecl *__restrict attr);
LEXDECL void DCC_PARSE_CALL DCCParse_CTypeTrail(struct DCCType *__restrict self, struct DCCAttrDecl *__restrict attr);

LEXDECL void DCC_PARSE_CALL DCCParse_WarnAllocaInLoop(void);
LEXDECL rc_t DCC_PARSE_CALL DCCParse_Register(void);
LEXDECL int  DCC_PARSE_CALL DCCParse_ExprMissingSym(void);
LEXDECL int  DCC_PARSE_CALL DCCParse_ExprType(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprUnarySuffix(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprDefault(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprIf(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprGeneric(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprUnary(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprProd(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprSum(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprShift(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprCmp(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprCmpEq(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprAnd(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprXor(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprOr(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprLAnd(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprLXor(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprLOr(void);
LEXDECL void DCC_PARSE_CALL DCCParse_ExprCond(void);

/* Fix a given type using the value from 'vbottom'. */
LEXDECL void DCC_PARSE_CALL DCCParse_FixType(struct DCCType *__restrict type);


LEXDECL void DCC_PARSE_CALL DCCParse_SkipExpr(void);
LEXDECL void DCC_PARSE_CALL DCCParse_DeadStmt(struct DCCSym *dead_jmp);

LEXDECL void DCC_PARSE_CALL
DCCParse_Function(struct DCCDecl *fun_decl, struct TPPKeyword const *asmname,
                  struct DCCType const *__restrict fun_type,
                  struct DCCAttrDecl const *__restrict fun_attr);
LEXDECL int DCC_PARSE_CALL
DCCParse_OneDeclWithBase(struct DCCType *__restrict base_type,
                         struct DCCAttrDecl *__restrict base_attr,
                         int *__restrict has_real_base_type);
LEXDECL int DCC_PARSE_CALL
DCCParse_OneDeclWithType(struct DCCType *__restrict decl_type,
                         struct DCCAttrDecl *__restrict decl_attr,
                         struct TPPKeyword *__restrict decl_name);

LEXDECL void DCC_PARSE_CALL DCCParse_DoWhileStmt(void);    /* do ... while(...); */
LEXDECL void DCC_PARSE_CALL DCCParse_WhileStmt(void);      /* while (...) ... */
LEXDECL void DCC_PARSE_CALL DCCParse_ForStmt(void);        /* for (...) ... */
LEXDECL void DCC_PARSE_CALL DCCParse_IfStmt(void);         /* if (...) ... */
LEXDECL void DCC_PARSE_CALL DCCParse_AsmStmt(int global);  /* __asm__(...) */
LEXDECL void DCC_PARSE_CALL DCCParse_Scope(pflag_t f);     /* { ... } */
LEXDECL void DCC_PARSE_CALL DCCParse_ScopeText(pflag_t f); /* {> ... <} */

/* Parse declarations using the typedef storage class. */
LEXDECL void DCC_PARSE_CALL
DCCParse_Typedef(struct DCCType *__restrict base,
                 struct DCCAttrDecl *__restrict attr);

LEXDECL struct DCCStructField *
DCCDecl_FindStructField(struct DCCDecl const *__restrict self,
                        struct TPPKeyword const *__restrict member_name,
                        target_off_t *path_offset);

/* INTRINSIC */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinTypeStr(void);           /* char const (&__builtin_typestr(type t))[]; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinConstantP(void);         /* _Bool __builtin_constant_p(expr x); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinChooseExpr(void);        /* expr __builtin_choose_expr(constexpr _Bool c, expr tt, expr ff); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinTypesCompatibleP(void);  /* _Bool __builtin_types_compatible_p(type t1, type t2); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFence(void);             /* void __builtin_unreachable(void),__builtin_trap(void),__builtin_breakpoint(void); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinBSwap(void);             /* unsigned __int16 __builtin_bswap16(unsigned __int16 x);
                                                                      * unsigned __int32 __builtin_bswap32(unsigned __int32 x);
                                                                      * unsigned __int64 __builtin_bswap64(unsigned __int64 x); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAssume(void);            /* void __builtin_assume(expr x); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinExpect(void);            /* long __builtin_expect(long x, long e); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFILE(void);              /* char const (&__builtin_FILE(void))[]; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinLINE(void);              /* int __builtin_LINE(void); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFUNCTION(void);          /* char const (&__builtin_FUNCTION(void))[]; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinBitField(void);          /* int (__builtin_bitfield(int &expr, int const_index, int const_size)): const_size; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinOffsetof(void);          /* size_t __builtin_offsetof(type t, member_chain s); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAssumeAligned(void);     /* void *__builtin_assume_aligned(void *p, size_t align, ...); */

/* ALLOCA */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAlloca(void);            /* void *__builtin_alloca(size_t s); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAllocaWithAlign(void);   /* void *__builtin_alloca_with_align(size_t s, size_t a); */

/* STDARG */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaStart(void);           /* void __builtin_va_start(va_list &ap, type &before_start, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaCopy(void);            /* void __builtin_va_copy(va_list &dst, va_list &src, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaEnd(void);             /* void __builtin_va_end(va_list &ap, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaArg(void);             /* type __builtin_va_arg(va_list &ap, type t, ...); */

/* SETJMP */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinSetJmp(void);            /* int __builtin_setjmp(T (&buf)[N]); // (sizeof(T)*N) == DCC_TARGET_SIZEOF_JMP_BUF */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinLongJmp(void);           /* void __builtin_longjmp(T (&buf)[N], int val) __attribute__((noreturn)); // (sizeof(T)*N) == DCC_TARGET_SIZEOF_JMP_BUF */

/* SYNC */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncCompareAndSwap(void);       /* bool __sync_bool_compare_and_swap(type *p, type oldval, type newval, ...);
                                                                      * type __sync_val_compare_and_swap(type *p, type oldval, type newval, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncSynchronize(void);          /* void __sync_synchronize(...); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncBinary(void);               /* (type|void) __sync_...(type *ptr, type value, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncUnary(void);                /* (type|void) __sync_...(type *ptr, ...); */

/* RETURN-ADDR */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinReturnAddr(void);        /* void *__builtin_return_address(unsigned int level),__builtin_frame_address(unsigned int level); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinExtractReturnAddr(void); /* void *__builtin_extract_return_addr(void *p),__builtin_frob_return_address(void *p); */

/* STRING */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMemcpy(void);            /* void *__builtin_memcpy(void *dst, void const *src, size_t size),__builtin_memmove(void *dst, void const *src, size_t size); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMemset(void);            /* void *__builtin_memset(void *dst, int byte, size_t size); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMemcmp(void);            /* int __builtin_memcmp(void const *a, void const *b, size_t size); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinStrlen(void);            /* size_t __builtin_strlen(char const *str),__builtin_strnlen(char const *str, size_t max); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinScas(void);              /* ... (Various string scanning builtins); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinStrcpy(void);            /* char *__builtin_strcpy(char *dst, char const *src),__builtin_strncpy(char *dst, char const *src, size_t max);
                                                                      * char *__builtin_strcat(char *dst, char const *src),__builtin_strncat(char *dst, char const *src, size_t max); */

/* DYNAMIC MEMORY */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMalloc(void);            /* void *__builtin_malloc(size_t n_bytes),__builtin_calloc(size_t count, size_t n_bytes); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinRealloc(void);           /* void *__builtin_realloc(void *ptr, size_t n_bytes); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFree(void);              /* void  __builtin_free(void *ptr); */

/* CPU */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinCPUInit(void);           /* void __builtin_cpu_init(void); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinCPUQuery(void);          /* int __builtin_cpu_is(char const *cpuname),__builtin_cpu_supports(char const *feature); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinCPUVendor(void);         /* char (&__builtin_cpu_vendor([char *buf]))[...], (&__builtin_cpu_brand([char *buf]))[...]; */

/* MISC */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinScanner(void);           /* int __builtin_ffs(int x),__builtin_ffsl(long x),__builtin_ffsll(long long x),__builtin_ffscc(auto x, size_t s);
                                                                      * int __builtin_clz(int x),__builtin_clzl(long x),__builtin_clzll(long long x),__builtin_clzcc(auto x, size_t s); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinCType(void);             /* int __builtin_(is|to)XXX(int ch); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinNoop(void);              /* int __builtin_noop(...),__noop(...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMinMax(void);            /* auto __builtin_min(...),__builtin_max(...); */

/* DRT */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFetchSym(void);          /* void __builtin_fetchsym(char const *name); */


INTDEF void DCC_VSTACK_CALL DCCVStack_PushSym_vfun(struct DCCSym *__restrict sym);  /* void sym(); */
INTDEF void DCC_VSTACK_CALL DCCVStack_PushSym_vpfun(struct DCCSym *__restrict sym); /* void *sym(); */
INTDEF void DCC_VSTACK_CALL DCCVStack_PushSym_cpfun(struct DCCSym *__restrict sym); /* char *sym(); */
INTDEF void DCC_VSTACK_CALL DCCVStack_PushSym_ifun(struct DCCSym *__restrict sym);  /* int sym(); */
INTDEF void DCC_VSTACK_CALL DCCVStack_PushSym_szfun(struct DCCSym *__restrict sym); /* size_t sym(); */

LEXDECL /*ref*/struct TPPString *DCC_PARSE_CALL DCCParse_GetFunction(void);
LEXDECL /*ref*/struct TPPString *DCC_PARSE_CALL DCCParse_GetPrettyFunction(void);
LEXDECL /*ref*/struct TPPString *DCC_PARSE_CALL DCCParse_OneStringExt(void);


/* Re-used from TPP */
#define CH_ISALPHA     0x01
#define CH_ISDIGIT     0x02
#define CH_ISSPACE     0x04
#define CH_ISANSI      0x08 /*< The character is an ansi-alpha character. */
#define CH_ISTRIGRAPH  0x10 /*< The character can be used as the 3th byte of a trigraph. */
#define CH_ISMULTICHAR 0x20 /*< The character is the first byte of a multi-character token (non-keyword; non-number; non-string). */
#define CH_ISLF        0x40 /*< The character is '\r' or '\n'. */
#define CH_ISZERO      0x80 /*< The character is '\0'. */

INTDEF uint8_t const chrattr[256];
#define tpp_isalpha(ch)         (chrattr[(uint8_t)(ch)]&CH_ISALPHA)
#define tpp_isdigit(ch)         (chrattr[(uint8_t)(ch)]&CH_ISDIGIT)
#define tpp_isalnum(ch)         (chrattr[(uint8_t)(ch)]&(CH_ISALPHA|CH_ISDIGIT))
#define tpp_isansi(ch)          (chrattr[(uint8_t)(ch)]&CH_ISANSI)
#define tpp_isspace(ch)         (chrattr[(uint8_t)(ch)]&CH_ISSPACE)
#define tpp_isspace_nz(ch)     ((chrattr[(uint8_t)(ch)]&(CH_ISSPACE|CH_ISZERO))==CH_ISSPACE)
#define tpp_islf(ch)            (chrattr[(uint8_t)(ch)]&CH_ISLF)
#define tpp_islforzero(ch)      (chrattr[(uint8_t)(ch)]&(CH_ISLF|CH_ISZERO))
#define tpp_isspace_nolf(ch)   ((chrattr[(uint8_t)(ch)]&(CH_ISSPACE|CH_ISLF))==CH_ISSPACE)
#define tpp_isnospace_orlf(ch) ((chrattr[(uint8_t)(ch)]&(CH_ISSPACE|CH_ISLF))!=CH_ISSPACE)
#define tpp_istrigraph(ch)      (chrattr[(uint8_t)(ch)]&CH_ISTRIGRAPH)
#define tpp_ismultichar(ch)     (chrattr[(uint8_t)(ch)]&CH_ISMULTICHAR)


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_PRIV_H */
