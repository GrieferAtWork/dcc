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
#ifndef GUARD_DCC_UNIT_FREEDAT_C_INL
#define GUARD_DCC_UNIT_FREEDAT_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/unit.h>

DCC_DECL_BEGIN

LOCAL struct DCCFreeRange *
DCCFreeRange_New(target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange *result;
 assert(size);
 result = (struct DCCFreeRange *)malloc(sizeof(struct DCCFreeRange));
 if likely(result) {
  result->fr_addr = addr;
  result->fr_size = size;
 }
 return result;
}

PUBLIC int
DCCFreeData_InitCopy(struct DCCFreeData *__restrict self,
                     struct DCCFreeData const *__restrict right) {
 struct DCCFreeRange *iter,**ptarget,*range_copy;
 assert(self);
 assert(right);
 ptarget = &self->fd_begin;
 iter = right->fd_begin;
 while (iter) {
  range_copy = (struct DCCFreeRange *)malloc(sizeof(struct DCCFreeRange));
  if unlikely(!range_copy) goto err;
  range_copy->fr_addr = iter->fr_addr;
  range_copy->fr_size = iter->fr_size;
  *ptarget = range_copy;
  ptarget = &range_copy->fr_next;
  iter = iter->fr_next;
 }
 *ptarget = NULL;
 return 1;
err:
 if (ptarget != &self->fd_begin) {
  range_copy = self->fd_begin;
  for (;;) {
   iter = range_copy->fr_next;
   free(range_copy);
   /* The following isn't really a dereference, because of the section reference! */
   if (ptarget != &range_copy->fr_next) break;
   range_copy = iter;
  }
 }
 return 0;
}

PUBLIC void
DCCFreeData_Quit(struct DCCFreeData *__restrict self) {
 struct DCCFreeRange *iter,*next;
 assert(self);
 iter = self->fd_begin;
 while (iter) {
  next = iter->fr_next;
  free(iter);
  iter = next;
 }
}

PUBLIC int
DCCFreeData_Has(struct DCCFreeData *__restrict self,
                target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange *iter;
 target_ptr_t addr_end;
 assert(self);
 if unlikely(!size) return 0;
 addr_end = addr+size;
 iter = self->fd_begin;
 while (iter) {
  assert(!iter->fr_next ||
          iter->fr_addr+iter->fr_size <
          iter->fr_next->fr_addr);
  if (addr_end > iter->fr_addr &&
      addr     < iter->fr_addr+iter->fr_size) {
   if (addr     >= iter->fr_addr &&
       addr_end <= iter->fr_addr+iter->fr_size) return 2;
   return 1;
  }
  iter = iter->fr_next;
 }
 return 0;
}


PUBLIC target_ptr_t
DCCFreeData_AcquireBelow(struct DCCFreeData *__restrict self,
                         target_ptr_t below_addr, target_siz_t size,
                         target_siz_t align, target_siz_t offset) {
 struct DCCFreeRange *iter,**piter;
 target_ptr_t result;
 assert(self);
 if unlikely(!size) return 0;
 piter = &self->fd_begin;
 while ((iter = *piter) != NULL) {
  if (iter->fr_addr >= below_addr) break;
  if (iter->fr_size >= size) {
   result  =   iter->fr_addr;
   result +=  (iter->fr_size-size);
   result +=  (align-1);
   result &= ~(align-1);
   result +=   offset;
   assert(result < below_addr);
   if (result      >= iter->fr_addr &&
       result+size <= iter->fr_addr+iter->fr_size) {
    if (result == iter->fr_addr) {
     /* Take away from the front */
     iter->fr_addr += size;
     iter->fr_size -= size;
     if unlikely(!iter->fr_size) {
      *piter = iter->fr_next;
      free(iter);
#if DCC_DEBUG
      iter = *piter;
#endif
     }
    } else if (result+size == iter->fr_addr+iter->fr_size) {
     /* Take away from the back */
     iter->fr_size -= size;
    } else {
     struct DCCFreeRange *newrange;
     /* Create a new slice. */
     newrange = DCCFreeRange_New(result+size,
                                (iter->fr_addr+iter->fr_size)-
                                (result+size));
     if unlikely(!newrange) break;
     iter->fr_size     = (target_siz_t)(result-iter->fr_addr);
     newrange->fr_next = iter->fr_next;
     iter->fr_next     = newrange;
    }
#if DCC_DEBUG
    assert(!iter || iter->fr_size);
    assert(!iter || !iter->fr_next ||
            iter->fr_addr <
            iter->fr_next->fr_addr+
            iter->fr_next->fr_size);
#endif
    return result;
   }
  }
  piter = &iter->fr_next;
 }
 return DCC_FREEDATA_INVPTR;
}
PUBLIC target_ptr_t
DCCFreeData_AcquireAt(struct DCCFreeData *__restrict self,
                      target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange *iter,**piter;
 target_ptr_t addr_end;
 assert(self);
 if unlikely(!size) return addr;
 piter = &self->fd_begin;
 addr_end = addr+size;
 while ((iter = *piter) != NULL) {
  if (iter->fr_addr >= addr_end) break;
  if (addr     >= iter->fr_addr &&
      addr_end <= iter->fr_addr+iter->fr_size) {
   /* The requested address range lies within this part. */
   if (addr == iter->fr_addr) {
    /* Take away from the front */
    iter->fr_addr += size;
    iter->fr_size -= size;
    if unlikely(!iter->fr_size) {
     *piter = iter->fr_next;
     free(iter);
#if DCC_DEBUG
     iter = *piter;
#endif
    }
   } else if (addr_end == iter->fr_addr+iter->fr_size) {
    /* Take away from the back */
    iter->fr_size -= size;
   } else {
    struct DCCFreeRange *newrange;
    /* Create a new slice. */
    newrange = DCCFreeRange_New(addr_end,
                               (iter->fr_addr+iter->fr_size)-addr_end);
    if unlikely(!newrange) break;
    iter->fr_size     = (target_siz_t)(addr-iter->fr_addr);
    newrange->fr_next = iter->fr_next;
    iter->fr_next     = newrange;
   }
#if DCC_DEBUG
   assert(!iter || iter->fr_size);
   assert(!iter || !iter->fr_next ||
           iter->fr_addr <
           iter->fr_next->fr_addr+
           iter->fr_next->fr_size);
#endif
   return addr;
  }
  piter = &iter->fr_next;
 }
 return DCC_FREEDATA_INVPTR;
}

PUBLIC void
DCCFreeData_Release(struct DCCFreeData *__restrict self,
                    target_ptr_t addr, target_siz_t size) {
 struct DCCFreeRange **piter,*iter,*newslot;
 target_ptr_t addr_end = addr+size;
 if unlikely(!size) return;
 assertf(!DCCFreeData_Has(self,addr,size),
         "Address range %#lx..%#lx has already been marked as free",
        (unsigned long)addr,(unsigned long)(addr+size));
 piter = &self->fd_begin;
 while ((iter = *piter) != NULL) {
  assert(iter->fr_size);
  assert(!iter->fr_next || iter->fr_addr+iter->fr_size < iter->fr_next->fr_addr);
  if (addr_end > iter->fr_addr) goto next;
  assert(addr_end <= iter->fr_addr);
  if (addr_end == iter->fr_addr) {
   /* Extend the current range below. */
   iter->fr_size += size;
   iter->fr_addr -= size;
   if (piter != &self->fd_begin) {
    /* Check for merge below. */
    newslot = (struct DCCFreeRange *)((uintptr_t)piter-offsetof(struct DCCFreeRange,fr_next));
    assert(newslot->fr_next == iter);
    assert(iter->fr_addr >= newslot->fr_addr+newslot->fr_size);
    if (iter->fr_addr == newslot->fr_addr+newslot->fr_size) {
     newslot->fr_size += iter->fr_size;
     newslot->fr_next  = iter->fr_next;
     free(iter);
    }
   }
   return;
  }
  /* Insert a new range before the current. */
  if (piter != &self->fd_begin) {
   newslot = (struct DCCFreeRange *)((uintptr_t)piter-offsetof(struct DCCFreeRange,fr_next));
   assert(newslot->fr_size);
   assert(addr >= newslot->fr_addr+newslot->fr_size);
   if (newslot->fr_addr+newslot->fr_size == addr) {
    /* Special case: Extend the previous range. */
    newslot->fr_size += size;
    assert(newslot->fr_next == iter);
    assertf(newslot->fr_addr+newslot->fr_size != iter->fr_addr,
            "The current range should have been down-extended above!");
    goto found_prefslot;
   }
   assert(addr > newslot->fr_addr+newslot->fr_size);
  }
  assert(addr_end < iter->fr_addr);
  newslot = DCCFreeRange_New(addr,size);
  if unlikely(!newslot) return;
  newslot->fr_next = iter;
  *piter = newslot;
found_prefslot:
  assert(newslot->fr_next == iter);
  assert(newslot->fr_addr+newslot->fr_size < iter->fr_addr);
  return;
next:
  piter = &iter->fr_next;
 }
 /* Append at the end. */
 assert(!*piter);
 if (piter != &self->fd_begin) {
  newslot = (struct DCCFreeRange *)((uintptr_t)piter-offsetof(struct DCCFreeRange,fr_next));
  /* Special case: Extend the last range. */
  if (newslot->fr_addr+newslot->fr_size == addr) {
   newslot->fr_size += size;
   return;
  }
 }
 newslot = DCCFreeRange_New(addr,size);
 if unlikely(!newslot) return;
 newslot->fr_next = NULL;
 *piter = newslot; /* Inherit object. */
}

DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_FREEDAT_C_INL */
