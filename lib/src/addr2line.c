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
/* Compile with: $ dcc -nostdlib -c -o int64.o int64.c */
/* Declare everything with hidden visibility. */
#pragma warning("-Wno-c99")

/* NOTE: This file is included when linking with '-g' */

#include <dcc.h>
#include <stddef.h>
#include <stdint.h>

#include "addr2line.h"

/* NOTE: This data structure must mirror the offsets found in '/src/dcc/unit-debug.c' */
struct sec_info {
	struct sec_info const *si_next; /*< [0..1] Information about the next section. */
	void                  *si_addr; /*< [1..1] Section start address. */
	size_t                 si_size; /*< Section size. */
	a2l_op_t              *si_a2l;  /*< [0..1] Addr2line code. */
};


extern struct sec_info const __dcc_dbg_secinfo  [[weak,visibility("hidden")]];
extern char const            __dcc_dbg_strtab[] [[weak]] __asm__(A2L_STRING_SECTION);

__attribute__((visibility("default")))
_Bool __dcc_dbg_addr2line(void *ip, lc_t *info) {
	struct sec_info const *iter = &__dcc_dbg_secinfo;
	if (info) {
		while (iter) {
			if ((uintptr_t)ip >= (uintptr_t)iter->si_addr &&
				(uintptr_t)ip <= (uintptr_t)iter->si_addr+iter->si_size) {
				/* Found the section associated with this EIP */
				struct A2LState state; a2l_addr_t addr;
				//printf("Found A2L for %p in %p ... %p at %p\n",
				//       ip,iter->si_addr,iter->si_addr+iter->si_size,iter->si_a2l);
				A2LState_INIT(&state,iter->si_a2l);
				if (!state.s_code) break;
				addr = (a2l_addr_t)((uintptr_t)ip-(uintptr_t)iter->si_addr);
				if (!A2L_NAME(a2l_exec)(&state,addr)) break;
				/* Managed to capture the given address!
				 * Fill in all available information. */
				info->line = (state.s_features&A2L_STATE_HASLINE) ? state.s_line+1 : 0;
				info->col  = (state.s_features&A2L_STATE_HASCOL) ? state.s_col+1 : 0;
				info->path = (state.s_features&A2L_STATE_HASPATH) ? __dcc_dbg_strtab+state.s_path : NULL;
				info->file = (state.s_features&A2L_STATE_HASFILE) ? __dcc_dbg_strtab+state.s_file : NULL;
				return 1;
			}
			iter = iter->si_next;
		}
		info->path = NULL;
		info->file = NULL;
		info->line = 0;
		info->col  = 0;
	}
	return 0;
}



