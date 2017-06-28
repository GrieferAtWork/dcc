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
#ifndef GUARD_DCC_LEXER_CTYPE_GUESS_C_INL
#define GUARD_DCC_LEXER_CTYPE_GUESS_C_INL 1

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/lexer.h>
#include <dcc/unit.h>

#include "lexer-priv.h"

#ifdef _MSC_VER
#include <string.h>
#else
#include <ctype.h>
#endif

DCC_DECL_BEGIN

#define DCC_MTYPES_ALWAYS_ALL 1

#ifdef _MSC_VER
#   define DCC_memcasecmp memicmp
#else
LOCAL int
DCC_memcasecmp(void const *__restrict a,
               void const *__restrict b,
               size_t n) {
 signed char const *aiter = (char const *)a;
 signed char const *biter = (char const *)b;
 signed char diff = 0;
 while (n--) {
  /* The way this is used, 'b' is always lower-case.
   * And if it isn't, it's not supposed to matter. */
  diff = tolower(*aiter++) -
         /*tolower*/(*biter++);
  if (diff) break;
 }
 return (int)diff;
}
#endif

INTDEF struct DCCDecl t_int;
INTDEF struct DCCDecl t_byte;
INTDEF struct DCCDecl t_word;
INTDEF struct DCCDecl t_ib8;
INTDEF struct DCCDecl t_uint;
INTDEF struct DCCDecl t_ubyte;
INTDEF struct DCCDecl t_uword;
INTDEF struct DCCDecl t_uib8;
INTDEF struct DCCDecl t_float;
INTDEF struct DCCDecl t_double;
INTDEF struct DCCDecl t_ldouble;
INTDEF struct DCCDecl t_bool;
INTDEF struct DCCDecl t_void;
INTDEF struct DCCDecl t_const_int;
INTDEF struct DCCDecl t_const_byte;
INTDEF struct DCCDecl t_const_word;
INTDEF struct DCCDecl t_const_ib8;
INTDEF struct DCCDecl t_const_uint;
INTDEF struct DCCDecl t_const_ubyte;
INTDEF struct DCCDecl t_const_uword;
INTDEF struct DCCDecl t_const_uib8;
INTDEF struct DCCDecl t_const_float;
INTDEF struct DCCDecl t_const_double;
INTDEF struct DCCDecl t_const_ldouble;
INTDEF struct DCCDecl t_const_bool;
INTDEF struct DCCDecl t_const_void;

struct missing_type {
 char const    *mt_name; /*< [1..1|eof(0..0)] Name of the type. */
 size_t         mt_size; /*< == strlen(mt_name). */
 struct DCCType mt_type; /*< The type itself. */
};
#define TYPE(name,ty)       {name,DCC_COMPILER_STRLEN(name),{ty,NULL}}
#define XTYPE(name,ty,base) {name,DCC_COMPILER_STRLEN(name),{ty,base}}
PRIVATE struct missing_type const mtypes[] = {
 /* Recognize a collection of standard types by name.
  * (Improves error handling for unknown types) */
 TYPE("size",DCCTYPE_SIZE|DCCTYPE_UNSIGNED),
 TYPE("ptrdiff",DCCTYPE_PTRDIFF),
 TYPE("ssize",DCCTYPE_SIZE),
 TYPE("intmax",DCCTYPE_INTMAX),
 TYPE("uintmax",DCCTYPE_INTMAX|DCCTYPE_UNSIGNED),

 /* These definitions implement a _lot_ of slangs:
  * >> s8 / u8 / int8 / uint8 / int8_t / uint8_t / INT8 / SInt8 / s_int8 / etc...
  */
#ifdef DCCTYPE_INT8
 TYPE("8",DCCTYPE_INT8),
 TYPE("int8",DCCTYPE_INT8),
#endif
#ifdef DCCTYPE_INT16
 TYPE("16",DCCTYPE_INT16),
 TYPE("int16",DCCTYPE_INT16),
#endif
#ifdef DCCTYPE_INT32
 TYPE("32",DCCTYPE_INT32),
 TYPE("int32",DCCTYPE_INT32),
#endif
#ifdef DCCTYPE_INT64
 TYPE("64",DCCTYPE_INT64),
 TYPE("int64",DCCTYPE_INT64),
#endif

 TYPE("int_least8",DCCTYPE_INT_LEAST8),
 TYPE("int_least16",DCCTYPE_INT_LEAST16),
 TYPE("int_least32",DCCTYPE_INT_LEAST32),
 TYPE("int_least64",DCCTYPE_INT_LEAST64),
 TYPE("int_fast8",DCCTYPE_INT_FAST8),
 TYPE("int_fast16",DCCTYPE_INT_FAST16),
 TYPE("int_fast32",DCCTYPE_INT_FAST32),
 TYPE("int_fast64",DCCTYPE_INT_FAST64),
 TYPE("intptr",DCCTYPE_INTPTR),
 TYPE("sig_atomic",DCCTYPE_SIG_ATOMIC|DCCTYPE_UNSIGNED),

 /* default/alternative spellings for builtin types. */
 TYPE("char",DCCTYPE_CHAR),
 TYPE("wchar",DCCTYPE_WCHAR|DCCTYPE_UNSIGNED),
 TYPE("short",DCCTYPE_SHORT),
 TYPE("int",DCCTYPE_INT),
 TYPE("long",DCCTYPE_LONG|DCCTYPE_ALTLONG),
 TYPE("llong",DCCTYPE_LLONG),
 TYPE("long_long",DCCTYPE_LLONG),
 TYPE("signed",DCCTYPE_INT),
 TYPE("unsigned",DCCTYPE_INT|DCCTYPE_UNSIGNED),
 TYPE("float",DCCTYPE_FLOAT),
 TYPE("double",DCCTYPE_DOUBLE),
 TYPE("ldouble",DCCTYPE_LDOUBLE),
 TYPE("long_double",DCCTYPE_LDOUBLE),
 TYPE("void",DCCTYPE_VOID),
 TYPE("bool",DCCTYPE_BOOL),
 TYPE("Bool",DCCTYPE_BOOL),
 TYPE("auto",DCCTYPE_AUTO),
 TYPE("auto_type",DCCTYPE_AUTO),
 TYPE("byte",DCCTYPE_BYTE|DCCTYPE_UNSIGNED),
 TYPE("word",DCCTYPE_WORD|DCCTYPE_UNSIGNED),

#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS) || DCC_MTYPES_ALWAYS_ALL
 /* Type names traditionally used in windows. */
 TYPE("BOOL",DCCTYPE_INT), /* Yes: This is supposed to be 'int' */
 TYPE("boolean",DCCTYPE_BYTE|DCCTYPE_UNSIGNED),
 TYPE("dword",DCCTYPE_LONG|DCCTYPE_ALTLONG|DCCTYPE_UNSIGNED),
 TYPE("qword",DCCTYPE_LLONG|DCCTYPE_UNSIGNED),
 TYPE("longlong",DCCTYPE_LLONG),
 TYPE("int_ptr",DCCTYPE_INTPTR),
#if DCC_TARGET_SIZEOF_POINTER == DCC_TARGET_SIZEOF_LONG
 TYPE("long_ptr",DCCTYPE_LONG|DCCTYPE_ALTLONG),
#else
 TYPE("long_ptr",DCCTYPE_INTPTR),
#endif
#endif
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS) || \
    !!(DCC_TARGET_OS&DCC_OS_F_UNIX) || DCC_MTYPES_ALWAYS_ALL
#if DCC_TARGET_SIZEOF_LONG*DCC_TARGET_BITPERBYTE == 32
 TYPE("long32",DCCTYPE_LONG|DCCTYPE_ALTLONG),
#else
 TYPE("long32",DCCTYPE_INT32),
#endif
#endif

#if !!(DCC_TARGET_OS&DCC_OS_F_UNIX) || DCC_MTYPES_ALWAYS_ALL
 TYPE("quad",DCCTYPE_LLONG),
#endif

 /* Admittedly questionable types. */
 /* Traditionally 32-bit, meaning that most implicitly assembly
  * symbols still retain this typing for backwards compatibility.
  * A library may be defined like this:
  * >> time_t time(time_t *ptm)
  * >> #ifdef _CRT_USE_64BIT_TIME
  * >>     __asm__("time64")
  * >> #endif
  * >> ;
  * The assembly name for the 'time' declaration is
  * 'time64' only when 'time_t' is (assumably) 64 bits long.
  * When one forgets to include the <time.h> header,
  * any call to the 'time' function will call an assembly symbol
  * named 'time', which (assumably) accepts 32-bit time_t arguments. */
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS)
 TYPE("time",DCCTYPE_INT32),
#if DCC_TARGET_SIZEOF_LONG*DCC_TARGET_BITPERBYTE == 32
 TYPE("clock",DCCTYPE_LONG|DCCTYPE_ALTLONG),
#else
 TYPE("clock",DCCTYPE_INT32),
#endif
#else
 TYPE("time",DCCTYPE_INTPTR),
 TYPE("clock",DCCTYPE_INTPTR),
#endif
#if !!(DCC_TARGET_OS&DCC_OS_F_UNIX) || DCC_MTYPES_ALWAYS_ALL
 /* This is practically a guaranty because of the following:
  * >> int open(char const *filename, int oflag, ...);
  * The hidden 3rd argument is accepts the same values
  * any 'mode_t' variable must be able to represent.
  * Considering that fact, as well as the fact that
  * standard-conforming C compilers must promote
  * variadic function arguments to at least int-typing, it
  * is practically a must that 'mode_t' is defined as 'int'.
  * Technically, 'mode_t' could be smaller than 'int', and
  * I've never seen any implementation that defined it to be.
  */
 TYPE("mode",DCCTYPE_INT),
 TYPE("off",DCCTYPE_INTPTR),
 TYPE("off64",DCCTYPE_INT64),
 TYPE("pid",DCCTYPE_INT32),
 TYPE("id",DCCTYPE_INT32|DCCTYPE_UNSIGNED),
 TYPE("useconds",DCCTYPE_INT32|DCCTYPE_UNSIGNED),
 TYPE("suseconds",DCCTYPE_INTPTR|DCCTYPE_UNSIGNED),
 TYPE("key",DCCTYPE_INT32),
 TYPE("clockid",DCCTYPE_INT32),
 XTYPE("timer",DCCTYPE_POINTER,&t_void),
#endif
 {NULL,0,{0,NULL}},
};
#undef XTYPE
#undef TYPE

PUBLIC int DCC_PARSE_CALL
DCCParse_CTypeGuess(struct DCCType *__restrict self,
                    struct DCCAttrDecl *__restrict attr,
                    char const *__restrict name, size_t size) {
 int result;
 struct TPPKeyword *user_kwd;
 struct DCCDecl *user_decl;
 struct missing_type const *iter;
 assert(self);
 assert(attr);
 assert(name);
 /* Strip ignored prefix/suffix strings. */
 for (;;) {
  if (size && name[0] == '_') { ++name,--size; continue; }
  if (size >= 3 && !DCC_memcasecmp(name,"int",3*sizeof(char))) { name += 3,size -= 3; continue; }
  break;
 }
 for (;;) {
  if (size >= 1 && name[size-1] == '_') { --size; continue; }
  if (size >= 2 && !memcmp(name+size-2,"_t",2*sizeof(char))) { size -= 2; continue; }
  if (size >= 4 && !DCC_memcasecmp(name+size-4,"type",4*sizeof(char))) { size -= 4; continue; }
  break;
 }

 /* With less modifiers, try to search for user-defined types again. */
 if ((user_kwd  = TPPLexer_LookupKeyword(name,size,0)) != NULL &&
     (user_decl = DCCCompiler_GetDecl(user_kwd,DCC_NS_LOCALS)) != NULL &&
     (user_decl->d_kind&DCC_DECLKIND_TYPE)) {
  self->t_type = user_decl->d_type.t_type;
  self->t_base = user_decl->d_type.t_base;
  if (user_decl->d_attr) DCCAttrDecl_Merge(attr,user_decl->d_attr);
  if (self->t_base) DCCDecl_Incref(self->t_base);
  result = 1;
  goto end;
 }

 /* Search a set of recognized builtin types. */
 for (iter = mtypes; iter->mt_name; ++iter) {
  if (iter->mt_size == size &&
     !memcmp(iter->mt_name,name,size*sizeof(char))) {
   DCCType_InitCopy(self,&iter->mt_type);
   result = 1;
   goto end;
  }
 }

 /* Ignoring casing this time, repeat the same search. */
 for (iter = mtypes; iter->mt_name; ++iter) {
  if (iter->mt_size == size &&
     !DCC_memcasecmp(iter->mt_name,name,size*sizeof(char))) {
   DCCType_InitCopy(self,&iter->mt_type);
   result = 1;
   goto end;
  }
 }

 /* Check some common modifiers. */
 if (size > 7 && !DCC_memcasecmp(name,"atomic_",7*sizeof(char))) {
  /* Automatically recognize 'atomic_*' prefix, as used in <stdatomic.h> */
  result = DCCParse_CTypeGuess(self,attr,name+7,size-7);
  if likely(result) {
   /* Only allow 'atomic_' modifiers on integral types. */
   if ((DCCTYPE_GROUP(self->t_type) != DCCTYPE_POINTER) &&
       (DCCTYPE_GROUP(self->t_type) != DCCTYPE_BUILTIN ||
        DCCTYPE_ISFLOAT(self->t_type))) result = 0;
   else self->t_type |= DCCTYPE_ATOMIC;
  }
  goto end;
 }
 if (size && (name[0] == 'u' || name[0] == 'U' ||
              name[0] == 's' || name[0] == 'S')) {
  /* signed/unsigned modifiers. */
  result = DCCParse_CTypeGuess(self,attr,name+1,size-1);
  if (result) {
   if (!DCCType_ALLOWSIGN(self)) result = 0;
   else if (name[0] == 'u' || name[0] == 'U')
        self->t_type |=   DCCTYPE_UNSIGNED;
   else self->t_type &= ~(DCCTYPE_UNSIGNED);
  }
  goto end;
 }
#if !!(DCC_TARGET_OS&DCC_OS_F_WINDOWS) || DCC_MTYPES_ALWAYS_ALL
 /* Windows-style leading type modifiers (e.g.: 'typedef void *PVOID,*LPVOID;') */
 if (size > 2 && name[0] == 'L' && name[1] == 'P') { ++name,--size; goto mk_pointer; }
 if (size > 1 && name[0] == 'P') {
mk_pointer:
  result = DCCParse_CTypeGuess(self,attr,name+1,size-1);
  if (result) DCCType_MkPointer(self);
  goto end;
 }
 if (size > 1 && (name[0] == 'C')) {
  result = DCCParse_CTypeGuess(self,attr,name+1,size-1);
  if (result) self->t_type |= (name[0] == 'C' ? DCCTYPE_CONST : DCCTYPE_VOLATILE);
  goto end;
 }
#endif

 /* Default to 'int'. */
 result       = 0;
 self->t_type = DCCTYPE_INT;
 self->t_base = NULL;
end:
 DCCType_ASSERT(self);
 return result;
}


DCC_DECL_END

#endif /* !GUARD_DCC_LEXER_CTYPE_GUESS_C_INL */
