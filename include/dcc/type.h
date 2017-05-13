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
#ifndef GUARD_DCC_TYPE_H
#define GUARD_DCC_TYPE_H 1

#include "common.h"
#include "target.h"
#include "lexer.h"

#include <stddef.h>
#include <stdint.h>

DCC_DECL_BEGIN

typedef uint16_t DCC(scopeid_t);

struct TPPFile;
struct DCCSym;

/* Basic type IDs. */
#define DCCTYPE_INT                 0x00000000 /* 32-bit integer 'int' (Must always be '0') */
#define DCCTYPE_BYTE                0x00000001 /* 8-bit integer 'char' */
#define DCCTYPE_WORD                0x00000002 /* 16-bit integer 'short' */
#define DCCTYPE_LLONG               0x00000003 /* 64-bit integer 'long long' */
#define DCCTYPE_UNSIGNED            0x00000004 /* FLAG: Unsigned type (applicable to 'DCCTYPE_INT', 'DCCTYPE_BYTE', 'DCCTYPE_WORD' and 'DCCTYPE_LLONG') */
#define DCCTYPE_ISBUILTIN(x) (!((x)&0x00000ff0))
#define DCCTYPE_FLOAT               0x00000008 /* Builtin type: 'float' */
#define DCCTYPE_DOUBLE              0x00000009 /* Builtin type: 'double' */
#define DCCTYPE_LDOUBLE             0x0000000a /* Builtin type: 'long double' */
#define DCCTYPE_BOOL                0x0000000b /* Builtin type: '_Bool' */
//#define DCCTYPE_FUTURE0           0x0000000c /* Unused */
//#define DCCTYPE_FUTURE1           0x0000000d /* Unused */
#define DCCTYPE_AUTO                0x0000000e /* Builtin type: '__auto_type' (NOTE: May _not_ be merged with qualifiers!) */
#define DCCTYPE_VOID                0x0000000f /* Builtin type: 'void' */

#define DCCTYPE_ISINT(t)      (!((t)&~(DCCTYPE_BUILTIN|7)))
#define DCCTYPE_ISFLOATT(t)   ((0x0700 >>              (t))&1)
#define DCCTYPE_ISFLOAT(t)    ((0x0700 >> DCCTYPE_BASIC(t))&1)
#define DCCTYPE_ISDECIMAL(t)  (DCCTYPE_BASIC(t) < DCCTYPE_BOOL)
/* 0x00000efc == (DCCTYPE_GROUPMASK|DCCTYPE_BASICMASK)&~(DCCTYPE_STRUCTURE|DCCTYPE_UNSIGNED) */
#define DCCTYPE_ISUNSIGNED(t) (((t)&0x00000efc) == DCCTYPE_UNSIGNED || DCCTYPE_ISBASIC(t,DCCTYPE_BOOL))

/* Fixed-length names. (TODO: Adjust these for target specs). */
#define DCCTYPE_INT8       DCCTYPE_BYTE
#define DCCTYPE_INT16      DCCTYPE_WORD
#define DCCTYPE_INT32      DCCTYPE_INT
#define DCCTYPE_INT64      DCCTYPE_LLONG
#define DCCTYPE_PRIVATE_INTN(bits) DCC_PRIVATE_PP_CAT(DCCTYPE_INT,bits)
#define DCCTYPE_INTN(bytes)        DCCTYPE_PRIVATE_INTN(DCC_MUL8(bytes))

/* Target names. */
#define DCCTYPE_LONG      DCCTYPE_INTN(DCC_TARGET_SIZEOF_LONG)
#define DCCTYPE_SHORT     DCCTYPE_INTN(DCC_TARGET_SIZEOF_SHORT)
#define DCCTYPE_CHAR      DCCTYPE_INTN(DCC_TARGET_SIZEOF_CHAR)
#define DCCTYPE_PTRDIFF   DCCTYPE_INTN(DCC_TARGET_SIZEOF_PTRDIFF_T)
#define DCCTYPE_SIZE      DCCTYPE_INTN(DCC_TARGET_SIZEOF_SIZE_T)
#define DCCTYPE_INTPTR    DCCTYPE_INTN(DCC_TARGET_SIZEOF_POINTER)

/* Type groups. */
#define DCCTYPE_BUILTIN   0x00000000 /* Type group: builtin type ('t_base' must be NULL)
                                      * HINT: The associated pointer type can be found in 'DCCType_BuiltinPointers[DCCTYPE_BASIC(...)]') */
#define DCCTYPE_POINTER   0x00000010 /* Type group: pointer ('t_base' is [1..1] and points to the pointer base) */
#define DCCTYPE_ARRAY     0x00000030 /* Fixed/variadic-length array. 't_base' is the array base; Array size is stored
                                      * in 't_base->d_tdecl.td_size' when 't_base->d_kind != DCC_DECLKIND_VLA'. */
#define DCCTYPE_FUNCTION  0x00000040 /* Function type. */
#define DCCTYPE_VARRAY    0x00000060 /* Compile-time variable-length array (Requires an initialized that will transform this
                                      * type into a normal array. NOTE: When used in an argument list, promoted to pointer). */
#define DCCTYPE_LVALUE    0x00000080 /* C++-style lvalue type. */
#define DCCTYPE_STRUCTURE 0x00000100 /* Structure type ('t_base' is the struct/union type declaration).
                                      * NOTE: Arithmetic structure types may be or'd together with 'DCCTYPE_UNSIGNED'. */

/* Type flags. */
#define DCCTYPE_CONST     0x00001000 /* FLAG: Constant type. */
#define DCCTYPE_VOLATILE  0x00002000 /* FLAG: volatile type. */
#define DCCTYPE_QUAL     (DCCTYPE_CONST|DCCTYPE_VOLATILE) /* Mask for type qualifiers. */
#define DCCTYPE_ATOMIC    0x00020000 /* FLAG: TODO: atomic-seqcst read/write access (warn if not possible). */
#define DCCTYPE_AUTOMATIC 0x00000000 /* STORAGE: Automatic storage duration (NOTE: Must always be ZERO(0)). */
#define DCCTYPE_ALTLONG   0x01000000 /* FLAG: Alternative type 'long' (Used to differentiate during type-matching). */
#define DCCTYPE_STATIC    0x10000000 /* STORAGE: Static storage duration. */
#define DCCTYPE_EXTERN    0x20000000 /* STORAGE: Extern storage duration. */
#define DCCTYPE_REGISTER  0x30000000 /* STORAGE: Prefer register storage (TODO: Currently ignored). */
#define DCCTYPE_TYPEDEF   0x40000000 /* STORAGE: Declare as type. */
#define DCCTYPE_INLINE    0x80000000 /* STORAGE:FLAG: Inline storage (Same as static in forward declarations; otherwise: inline memory). */
#define DCCTYPE_STOREBASE 0x70000000 /* Mask for basic storage flags. */
#define DCCTYPE_STOREMASK 0xf0000000 /* Mask for storage flags. */


/* Type masks. */
#define DCCTYPE_BASICMASK 0x0000000f /* Mask for base type ids. */
#define DCCTYPE_GROUPMASK 0x00000ff0 /* Mask for type group. */
#define DCCTYPE_FLAGSMASK 0xfffff000 /* Mask for type flags. */
#define DCCTYPE_ASTMASK   0x0f000000 /* Alternate type-names mask. */

/* Type helper macros. */
#define DCCTYPE_BASIC(t)             ((t)&DCCTYPE_BASICMASK)
#define DCCTYPE_GROUP(t)             ((t)&DCCTYPE_GROUPMASK)
#define DCCTYPE_FLAGS(t)             ((t)&DCCTYPE_FLAGSMASK)
#define DCCTYPE_ISBASIC(t,x)         (((t)&(DCCTYPE_BASICMASK|DCCTYPE_GROUPMASK)) == (DCCTYPE_BUILTIN|(x)))
#define DCCTYPE_ISSIGNLESSBASIC(t,x) (((t)&((DCCTYPE_BASICMASK&~(DCCTYPE_UNSIGNED))|DCCTYPE_GROUPMASK)) == (DCCTYPE_BUILTIN|(x)))



typedef uint32_t DCC(tyid_t); /*< Type id+flags ('DCCTYPE_*'). */
struct DCCType {
#ifdef __INTELLISENSE__
 tyid_t                 t_type; /*< Type id+flags ('DCCTYPE_*'). */
#else
 DCC(tyid_t)            t_type; /*< Type id+flags ('DCCTYPE_*'). */
#endif
 /*ref*/struct DCCDecl *t_base; /*< [0..1] Type base (Always has the 'DCC_DECLKIND_TYPE' flag set).
                                 *   - Pointer types use this to describe the pointer base.
                                 *   - array types use this to describe the base type.
                                 *   - struct/union types use this to describe the struct declaration. */
};
#define DCCType_Quit(self) (void)(!(self)->t_base || (DCCDecl_Decref((self)->t_base),0))
#define DCCType_InitCopy(self,right) \
 (void)(*(self) = *(right),(!(self)->t_base || (DCCDecl_Incref((self)->t_base),0)))


/* Special builtin types describing the pointers to the 8 builtin types. */
DCCDAT struct DCCType const DCCType_BuiltinPointers[16];
DCCDAT struct DCCType const DCCType_BuiltinConstPointers[16];

/* Turn the given type into its various underlying/base forms.
 * NOTE: All of these functions expect 'self' to already be fully initialized. */
DCCFUN void DCCType_MkPointer(struct DCCType *__restrict self);
DCCFUN void DCCType_MkLValue(struct DCCType *__restrict self);
DCCFUN void DCCType_MkBase(struct DCCType *__restrict self);
DCCFUN void DCCType_MkArray(struct DCCType *__restrict self, DCC(target_ptr_t) n_elem);
DCCFUN void DCCType_MkVArray(struct DCCType *__restrict self);
DCCFUN void DCCType_MkOldFunc(struct DCCType *__restrict self);
DCCFUN void DCCType_MkVLA(struct DCCType *__restrict self,
                          DCC(target_off_t) abssize_ebp_offset,
                          DCC(scopeid_t) scope);
DCCFUN void DCCType_ForceDynamic(struct DCCType *__restrict self);

/* Returns non-ZERO(0) if the given type is complete. */
DCCFUN int DCCType_IsComplete(struct DCCType const *__restrict self);

/* Try to fix an incomplete type, doing some guessing as to what the intended type was.
 * NOTE: no-op if 'DCCType_IsComplete' would have returned ZERO(0)
 * WARNING: This function doesn't guaranty being able to fix _all_ incomplete types! */
DCCFUN void DCCType_FixComplete(struct DCCType *__restrict self);

/* Determine size and alignment of the type described by 'self' */
DCCFUN DCC(target_ptr_t)
DCCType_Sizeof(struct DCCType const *__restrict self,
               DCC(target_ptr_t) *__restrict align,
               int real_sizeof);


/* Check if the given types are compatible (that is: are equal)
 * @param: unqualified: When 1, discard 'DCCTYPE_QUAL' modifiers on the first pass.
 *                      When 2, discard 'DCCTYPE_QUAL' modifiers recursively.
 * @return: 0/1: boolean-style indicating compatibility. */
DCCFUN int
DCCType_IsCompatible(struct DCCType const *__restrict a,
                     struct DCCType const *__restrict b,
                     int unqualified);


/* Generates the string representation of a given type 'self'
 * The string is written to the provided buffer, but will
 * never write more than was provided through 'buflen'.
 * @return: * : The minimum amount of bytes required for a full representation.
 */
DCCFUN size_t
DCCType_ToString(char *buf, size_t buflen,
                 struct DCCType const *__restrict self,
                 struct TPPKeyword const *name);

/* Similar to 'DCCType_ToString' (and never returns NULL)
 * This function also generates a string representation
 * of the given type, but returns an empty string if
 * it failed to allocate the dynamic buffer. */
DCCFUN /*ref*/struct TPPString *
DCCType_ToTPPString(struct DCCType const *__restrict self,
                    struct TPPKeyword const *name);


/* Check the given type 'self' to be writable and emit a warning if it isn't. */
DCCFUN void DCCType_CheckWritable(struct DCCType const *__restrict self);
DCCFUN void DCCType_CheckWritablePtr(struct DCCType const *__restrict self);

DCC_DECL_END

#endif /* !GUARD_DCC_TYPE_H */