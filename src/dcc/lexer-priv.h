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
#include <dcc/target.h>
#include <dcc/lexer.h>

DCC_DECL_BEGIN

struct TPPKeyword;
struct DCCSym;
struct DCCDecl;

#define LEXDECL extern
#define LEXPRIV /* nothing */

LEXDECL void DCCSym_CalculateStructureOffsets(struct DCCDecl *__restrict self);

LEXDECL struct TPPKeyword *DCC_PARSE_CALL tok_without_underscores(void);
LEXDECL char *DCC_PARSE_CALL peek_next_token(void); /* Peek the first character of what will likely be the next token. */

/* Do type promotions on a function argument type (e.g.: 'array/V-array --> pointer') */
LEXDECL void DCC_PARSE_CALL DCCType_PromoteFunArg(struct DCCType *__restrict self);

#define ATTRKIND_ATTRIBUTE 0
#define ATTRKIND_DECLSPEC  1
#define ATTRKIND_BRACKETS  2
LEXDECL void DCC_PARSE_CALL DCCParse_AttrContent(struct DCCAttrDecl *__restrict self, int kind);

LEXDECL void DCC_PARSE_CALL DCCParse_CTypeSuffix2(struct DCCType *__restrict self, struct DCCAttrDecl *__restrict attr);

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

LEXDECL int DCC_PARSE_CALL DCCParse_TryFunctionPrototype(struct DCCDecl *__restrict fundecl, int try_parse);
/* Fix a given type using the value from 'vbottom'. */
LEXDECL void DCC_PARSE_CALL DCCParse_FixType(struct DCCType *__restrict type);


LEXDECL void DCC_PARSE_CALL DCCParse_SkipExpr(void);
LEXDECL void DCC_PARSE_CALL DCCParse_DeadStmt(struct DCCSym *dead_jmp);

LEXPRIV void DCC_PARSE_CALL
DCCParse_OldFunctionArgument(struct DCCArgAllocator *__restrict allocator,
                             struct DCCType const *__restrict base_type,
                             struct DCCAttrDecl const *__restrict base_attr);
LEXDECL int DCC_PARSE_CALL
DCCParse_Function(struct DCCDecl *fun_decl, struct TPPKeyword const *asmname,
                  struct DCCType const *__restrict fun_type,
                  struct DCCAttrDecl const *__restrict fun_attr);
LEXDECL int DCC_PARSE_CALL
DCCParse_DeclWithBase(struct DCCType const *__restrict base_type,
                      struct DCCAttrDecl const *__restrict base_attr);
LEXDECL int DCC_PARSE_CALL
DCCParse_OneDeclWithBase(struct DCCType const *__restrict base_type,
                         struct DCCAttrDecl const *__restrict base_attr);

LEXDECL void DCC_PARSE_CALL DCCParse_DoWhileStmt(void);    /* do ... while(...); */
LEXDECL void DCC_PARSE_CALL DCCParse_WhileStmt(void);      /* while (...) ... */
LEXDECL void DCC_PARSE_CALL DCCParse_ForStmt(void);        /* for (...) ... */
LEXDECL void DCC_PARSE_CALL DCCParse_IfStmt(void);         /* if (...) ... */
LEXDECL void DCC_PARSE_CALL DCCParse_AsmStmt(int global);  /* __asm__(...) */
LEXDECL void DCC_PARSE_CALL DCCParse_Scope(pflag_t f);     /* { ... } */
LEXDECL void DCC_PARSE_CALL DCCParse_ScopeText(pflag_t f); /* {> ... <} */
LEXDECL int  DCC_PARSE_CALL DCCParse_GlobalDecl(void);

/* Parse declarations using the typedef storage class. */
LEXDECL void DCC_PARSE_CALL
DCCParse_Typedef(struct DCCType *__restrict base,
                 struct DCCAttrDecl *__restrict attr);

LEXDECL struct DCCStructField *
DCCDecl_FindStructField(struct DCCDecl const *__restrict self,
                        struct TPPKeyword const *__restrict member_name);

LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinTypeStr(void);           /* char const (&__builtin_typestr(type t))[]; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinConstantP(void);         /* _Bool __builtin_constant_p(expr x); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinChooseExpr(void);        /* expr __builtin_choose_expr(constexpr _Bool c, expr tt, expr ff); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinTypesCompatibleP(void);  /* _Bool __builtin_types_compatible_p(type t1, type t2); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFence(void);             /* void __builtin_unreachable(void),__builtin_trap(void),__builtin_breakpoint(void); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinBSwap(void);             /* unsigned __int16 __builtin_bswap16(unsigned __int16 x);
                                                                      * unsigned __int32 __builtin_bswap32(unsigned __int32 x);
                                                                      * unsigned __int64 __builtin_bswap64(unsigned __int64 x); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAlloca(void);            /* void *__builtin_alloca(size_t s); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAllocaWithAlign(void);   /* void *__builtin_alloca_with_align(size_t s, size_t a); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAssume(void);            /* void __builtin_assume(expr x); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinExpect(void);            /* long __builtin_expect(long x, long e); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFILE(void);              /* char const (&__builtin_FILE(void))[]; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinLINE(void);              /* int __builtin_LINE(void); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinFUNCTION(void);          /* char const (&__builtin_FUNCTION(void))[]; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinBitField(void);          /* int (__builtin_bitfield(int &expr, int const_index, int const_size)): const_size; */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinOffsetof(void);          /* size_t __builtin_offsetof(type t, member_chain s); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinAssumeAligned(void);     /* void *__builtin_assume_aligned(void *p, size_t align, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinScanner(void);           /* int __builtin_ffs(int x),__builtin_ffsl(long x),__builtin_ffsll(long long x),__builtin_ffscc(auto x, size_t s);
                                                                      * int __builtin_clz(int x),__builtin_clzl(long x),__builtin_clzll(long long x),__builtin_clzcc(auto x, size_t s); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaStart(void);           /* void __builtin_va_start(va_list &ap, type &before_start, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaCopy(void);            /* void __builtin_va_copy(va_list &dst, va_list &src, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaEnd(void);             /* void __builtin_va_end(va_list &ap, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinVaArg(void);             /* type __builtin_va_arg(va_list &ap, type t, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinSetJmp(void);            /* int __builtin_setjmp(T (&buf)[N]); // (sizeof(T)*N) == DCC_TARGET_SIZEOF_JMP_BUF */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinLongJmp(void);           /* void __builtin_longjmp(T (&buf)[N], int val) __attribute__((noreturn)); // (sizeof(T)*N) == DCC_TARGET_SIZEOF_JMP_BUF */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMemcpy(void);            /* void *__builtin_memcpy(void *dst, void const *src, size_t size),__builtin_memmove(void *dst, void const *src, size_t size); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMemset(void);            /* void *__builtin_memset(void *dst, int byte, size_t size); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinMemcmp(void);            /* int __builtin_memcmp(void const *a, void const *b, size_t size); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinStrlen(void);            /* size_t __builtin_strlen(char const *str); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncCompareAndSwap(void);       /* bool __sync_bool_compare_and_swap(type *p, type oldval, type newval, ...);
                                                                      * type __sync_val_compare_and_swap(type *p, type oldval, type newval, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncSynchronize(void);          /* void __sync_synchronize(...); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncBinary(void);               /* (type|void) __sync_...(type *ptr, type value, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_SyncUnary(void);                /* (type|void) __sync_...(type *ptr, ...); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinReturnAddr(void);        /* void *__builtin_return_address(unsigned int level),__builtin_frame_address(unsigned int level); */
LEXDECL void DCC_PARSE_CALL DCCParse_BuiltinExtractReturnAddr(void); /* void *__builtin_extract_return_addr(void *p),__builtin_frob_return_address(void *p); */

LEXDECL /*ref*/struct TPPString *DCC_PARSE_CALL DCCParse_GetFunction(void);
LEXDECL /*ref*/struct TPPString *DCC_PARSE_CALL DCCParse_GetPrettyFunction(void);
LEXPRIV /*ref*/struct TPPString *DCC_PARSE_CALL DCCParse_OneStringExt(void);


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_PRIV_H */
