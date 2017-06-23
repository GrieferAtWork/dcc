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
#ifndef GUARD_DCC_DL_H
#define GUARD_DCC_DL_H 1

#include "common.h"
#include "target.h"

#if !!(DCC_HOST_OS&DCC_OS_F_WINDOWS)
#include <dcc_winmin.h>

DCC_DECL_BEGIN

typedef HMODULE DCC(dl_t);
#define DCC_DLERROR          NULL
#define DCC_DLERROR_IS_ZERO  1
#define DCC_dlopen(filename) LoadLibraryA(filename)
#define DCC_dlsym(dl,name)   GetProcAddress(dl,name)
#define DCC_dlclose(dl)      FreeLibrary(dl)

DCC_DECL_END

#elif !!(DCC_HOST_OS&DCC_OS_F_UNIX) || __has_include(<dlfcn.h>)
#include <dlfcn.h>

DCC_DECL_BEGIN

typedef void *DCC(dl_t);
#define DCC_DLERROR          NULL
#define DCC_DLERROR_IS_ZERO  1
#define DCC_dlopen(filename) dlopen(filename,RTLD_GLOBAL|RTLD_LAZY)
#define DCC_dlsym(dl,name)   dlsym(dl,name)
#define DCC_dlclose(dl)      dlclose(dl)

DCC_DECL_END

#else
#error FIXME
#endif

#ifndef DCC_DLERROR_IS_ZERO
#define DCC_DLERROR_IS_ZERO 0
#endif

#endif /* !GUARD_DCC_DL_H */
