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
#ifndef GUARD_DCC_UNIT_SECMEM_C_INL
#define GUARD_DCC_UNIT_SECMEM_C_INL 1

#define DCC(x) x

#include <dcc/common.h>
#include <dcc/compiler.h>
#include <dcc/unit.h>

#include <string.h>

#if DCC_DEBUG && 0
#define REFLOG(x) (printf x,fflush(stdout))
#include <stdio.h>
#else
#define REFLOG(x) (void)0
#endif

DCC_DECL_BEGIN

LOCAL struct DCCAllocRange *
DCCAllocRange_New(target_ptr_t addr,
                  target_siz_t size,
                  unsigned int refcnt) {
 struct DCCAllocRange *result;
 assert(refcnt);
 assert(size);
 assert(!(size&0x80000000));
 result = (struct DCCAllocRange *)malloc(sizeof(struct DCCAllocRange));
 if unlikely(!result) TPPLexer_SetErr();
 else {
  result->ar_addr = addr;
  result->ar_size = size;
  result->ar_refcnt = refcnt;
 }
 return result;
}


/* Same as 'DCCSection_DFree', but validates the addr|size arguments
 * and fixes them while emitting a warning if they are out-of-bounds.
 * >> Since symbol size can be set from assembly using the '.size' directive,
 *    we must make sure that out-of-bounds symbol sizes don't cause undefined
 *    behavior, but are warned about instead.
 * NOTE: In addition, this function handles the even of
 *      'self' being the currently selected section. */
LOCAL void
DCCSection_DSafeFree(struct DCCSection *__restrict self,
                     target_ptr_t addr, target_siz_t size) {
 target_ptr_t sym_end;
 target_siz_t siz_max;
 DCCSection_TBEGIN(self);
 siz_max = (target_siz_t)(self->sc_dat.sd_text.tb_max-
                          self->sc_dat.sd_text.tb_begin);
 sym_end = addr+size;
 if (sym_end < addr || sym_end > siz_max) {
  WARN(W_LINKER_SYMBOL_SIZE_OUT_OF_BOUNDS,
      (target_ptr_t)addr,
      (target_ptr_t)(addr+size),
       self->sc_start.sy_name,
      (target_ptr_t)siz_max);
  if (addr > siz_max) addr = siz_max;
  size = siz_max-addr;
 }
 DCCSection_DFree(self,addr,size);
 DCCSection_TEND(self);
}





#if 1
LOCAL int
DCCSection_DIncrefN(struct DCCSection *__restrict self,
                    target_ptr_t addr, target_siz_t size,
                    unsigned int n_refcnt) {
 struct DCCAllocRange **prange,*range;
 struct DCCAllocRange *newrange;
 target_siz_t underflow;
 int result = 1;
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 assert(!DCCFreeData_Has(&self->sc_dat.sd_free,addr,size));
 assert(n_refcnt);
 if unlikely(!OK) goto err; /* Don't do anything if an error occurred. */
 if unlikely(!size) goto end; /* nothing to do here! */
 REFLOG(("+++ incref('%s',%#lx,%lu) x%u\n",self->sc_start.sy_name->k_name,
        (unsigned long)addr,(unsigned long)size,n_refcnt));
 for (prange = &self->sc_dat.sd_alloc;
     (range  = *prange) != NULL;
      prange = &range->ar_next) {
#define CURR_RANGE       (*range)
#define NEXT_RANGE       (*range->ar_next)
#define PREV_RANGE       (*(struct DCCAllocRange *)((uintptr_t)prange-DCC_COMPILER_OFFSETOF(struct DCCAllocRange,ar_next)))
#define HAS_NEXT_RANGE   (range->ar_next != NULL)
#define HAS_PREV_RANGE   (prange != &self->sc_dat.sd_alloc)
#define CURR_RANGE_BEGIN (CURR_RANGE.ar_addr)
#if DCC_DEBUG
#define ASSERT_RELATION() \
  (assert(!prange || !HAS_PREV_RANGE || (PREV_RANGE.ar_addr+PREV_RANGE.ar_size <= CURR_RANGE.ar_addr)),\
              assert(!HAS_NEXT_RANGE || (NEXT_RANGE.ar_addr >= CURR_RANGE.ar_addr+CURR_RANGE.ar_size)))
#else
#define ASSERT_RELATION() (void)0
#endif
  assert(size);
  assert(CURR_RANGE.ar_size);
  assert(CURR_RANGE.ar_refcnt);
  ASSERT_RELATION();
  if (addr < CURR_RANGE_BEGIN) {
   assert(!HAS_PREV_RANGE ||
           PREV_RANGE.ar_addr+PREV_RANGE.ar_size <= addr);
   /* Handle any memory below the current range. */
   underflow = CURR_RANGE_BEGIN-addr;
   if (underflow > size) {
    /* Special case: The incref() region is completely below the current range.
     *            >> Must insert a whole, new range below. */
    assert(addr+size < CURR_RANGE_BEGIN);
    newrange = DCCAllocRange_New(addr,size,n_refcnt);
    if unlikely(!newrange) goto err;
    newrange->ar_next = range;
    *prange = newrange;
    goto end;
   }
   if (CURR_RANGE.ar_refcnt == n_refcnt) {
    /* Can directly extend the current range. */
    CURR_RANGE.ar_addr -= underflow;
    CURR_RANGE.ar_size += underflow;
    assert(CURR_RANGE.ar_size);
    assert(!HAS_PREV_RANGE ||
            PREV_RANGE.ar_addr+PREV_RANGE.ar_size <= CURR_RANGE.ar_addr);
    if (HAS_PREV_RANGE &&
        CURR_RANGE.ar_refcnt == PREV_RANGE.ar_refcnt &&
        CURR_RANGE.ar_addr   == PREV_RANGE.ar_addr+PREV_RANGE.ar_size) {
merge_curr_prev:
     /* Must merge the previous range with the current. */
     PREV_RANGE.ar_size += CURR_RANGE.ar_size;
     PREV_RANGE.ar_next  = CURR_RANGE.ar_next;
     assert(PREV_RANGE.ar_size);
     free(range);
     range = &PREV_RANGE;
#if DCC_DEBUG
     prange = NULL;
#endif
    }
    assert(CURR_RANGE.ar_size);
   } else {
    /* Must insert a new range before the current one. */
    newrange = DCCAllocRange_New(addr,underflow,n_refcnt);
    if unlikely(!newrange) goto err;
    newrange->ar_next = range;
    *prange           = newrange;
    prange = &newrange->ar_next;
   }
   ASSERT_RELATION();
   size -= underflow;
   if (!size) goto end;
   addr += underflow;
#if DCC_DEBUG
   assert(!prange || range == *prange);
#endif
  }
  assert(addr >= CURR_RANGE_BEGIN);
#define CURR_RANGE_END   (range_end)
  {
   target_ptr_t range_end;
   range_end = CURR_RANGE.ar_addr+CURR_RANGE.ar_size;
   if (addr > range_end) goto next; /* Skip ahead if we're not there, yet. */
   assert(addr >= CURR_RANGE_BEGIN);
   assert(addr <= CURR_RANGE_END);
   assert(!HAS_NEXT_RANGE || NEXT_RANGE.ar_addr >= range_end);
   if (addr == CURR_RANGE_END) {
    /* Extend upwards, or insert a new range after the current. */
#define overflow  underflow
    if (HAS_NEXT_RANGE) {
     overflow = (target_siz_t)(NEXT_RANGE.ar_addr-range_end);
     if (overflow > size) overflow = size;
    } else {
     overflow = size;
    }
    /* Without any overflow, this means that there is another
     * region bordering directly against the current one,
     * meaning we'll have to incref that region in the next pass. */
    if (!overflow) goto next;
    if (CURR_RANGE.ar_refcnt == n_refcnt) {
     CURR_RANGE.ar_size += overflow;
    } else {
     /* Must insert a new region after the current. */
     newrange = DCCAllocRange_New(range_end,overflow,n_refcnt);
     if unlikely(!newrange) goto err;
     newrange->ar_next = range->ar_next;
     range->ar_next    = newrange;
     range             = newrange;
    }
    assert(CURR_RANGE.ar_size);
    ASSERT_RELATION();
    size -= overflow;
    if (!size) goto end;
    addr      += overflow;
    range_end += overflow;
    /* When we're here, that means that we've just inserted
     * a small cross-over range, or extended to current to
     * border against the next.
     * In both cases, we know that the remainder of the
     * incref address range borders against the end of
     * the current range, as well as the next range starts
     * where the current range ends. */
    assert(addr == range_end);
    assert(HAS_NEXT_RANGE);
    if (CURR_RANGE.ar_refcnt == NEXT_RANGE.ar_refcnt) {
     /* Must merge this range with the next.
      * This can happen when the current range was extended and
      * now borders against the next with a matching reference count. */
     newrange            = &NEXT_RANGE;
     CURR_RANGE.ar_size += newrange->ar_size;
     CURR_RANGE.ar_next  = newrange->ar_next;
     free(newrange);
    } else {
     range = range->ar_next;
     assert(addr == range->ar_addr);
    }
    range_end = CURR_RANGE.ar_addr+CURR_RANGE.ar_size;
#undef overflow
   }
   assert(range_end == CURR_RANGE.ar_addr+CURR_RANGE.ar_size);
   assert(addr >= CURR_RANGE_BEGIN);
   assert(addr <  CURR_RANGE_END);
   if (addr == CURR_RANGE_BEGIN) {
    int must_continue = 0;
#define overlap  underflow
    /* Either a perfect match, or must split the region. */
    overlap = CURR_RANGE.ar_size;
    if (overlap > size) overlap = size;
    assert(overlap);
    if (overlap == CURR_RANGE.ar_size) {
     /* Perfect match! */
     CURR_RANGE.ar_refcnt += n_refcnt;
     /* Check if we must merge with the next region. */
     if (HAS_NEXT_RANGE &&
         CURR_RANGE.ar_refcnt                  == NEXT_RANGE.ar_refcnt &&
         CURR_RANGE.ar_addr+CURR_RANGE.ar_size == NEXT_RANGE.ar_addr) {
      /* Because of this merge, the current range is
       * extended, although additional data may have
       * to be incref'ed. */
      must_continue = 1;
      newrange      = range->ar_next;
      CURR_RANGE.ar_size += newrange->ar_size;
      CURR_RANGE.ar_next  = newrange->ar_next;
      free(newrange);
     }
    } else {
     /* Partial match. */
     assert(CURR_RANGE.ar_size > overlap);
     newrange = DCCAllocRange_New(CURR_RANGE.ar_addr+overlap,
                                  CURR_RANGE.ar_size-overlap,
                                  CURR_RANGE.ar_refcnt);
     if unlikely(!newrange) goto err;
     newrange->ar_next     = CURR_RANGE.ar_next;
     CURR_RANGE.ar_size    = overlap;
     CURR_RANGE.ar_next    = newrange;
     CURR_RANGE.ar_refcnt += n_refcnt;
    }
    assert(CURR_RANGE.ar_size);
    ASSERT_RELATION();
    assert(addr == CURR_RANGE_BEGIN);
    if (HAS_PREV_RANGE &&
        CURR_RANGE.ar_refcnt == PREV_RANGE.ar_refcnt &&
        CURR_RANGE.ar_addr   == PREV_RANGE.ar_addr+PREV_RANGE.ar_size) {
     /* Merge the current range with the previous. */
     goto merge_curr_prev;
    }
    size -= overlap;
    if (!size) goto end;
    addr += overlap;
#undef overlap
    if (!must_continue) {
     assert(addr == CURR_RANGE.ar_addr+CURR_RANGE.ar_size);
     goto next;
    }
    /* This can happen when the region was merged with its successor,
     * even though we'll have to split them again when increfind data
     * for that successor. */
    range_end = CURR_RANGE.ar_addr+CURR_RANGE.ar_size;
   }
   assert(addr > CURR_RANGE_BEGIN);
   assert(addr < CURR_RANGE_END);
#define subsize underflow
   /* Last possibility: Sub-range */
   subsize = CURR_RANGE_END-addr;
   if (subsize > size) subsize = size;
   if (addr+subsize == CURR_RANGE_END) {
    /* Special case: The remainder of the current range is affected.
     *            >> The range must only be split into 2 parts. */
    newrange = DCCAllocRange_New(addr,subsize,CURR_RANGE.ar_refcnt+n_refcnt);
    if unlikely(!newrange) goto err;
    newrange->ar_next   = CURR_RANGE.ar_next;
    CURR_RANGE.ar_next  = newrange;
    CURR_RANGE.ar_size -= subsize;
    assert(CURR_RANGE.ar_size);
#if DCC_DEBUG
    prange = &CURR_RANGE.ar_next;
    range  = newrange;
#endif
   } else {
    struct DCCAllocRange *cenrange;
    assert(addr+size < CURR_RANGE_END);
    /* Last case: The remainder of the requested address
     *            range is a sub-range of the current.
     *         >> Must skip the current range into 3 parts. */
    cenrange = DCCAllocRange_New(addr,size,CURR_RANGE.ar_refcnt+n_refcnt);
    if unlikely(!cenrange) goto err;
    assert(CURR_RANGE_END > addr);
    assert((CURR_RANGE_END-addr) > size);
    newrange = DCCAllocRange_New(addr+size,
                                (CURR_RANGE_END-addr)-size,
                                 CURR_RANGE.ar_refcnt);
    if unlikely(!newrange) { free(cenrange); goto err; }
    newrange->ar_next  = CURR_RANGE.ar_next;
    cenrange->ar_next  = newrange;
    CURR_RANGE.ar_next = cenrange;
    CURR_RANGE.ar_size = addr-CURR_RANGE.ar_addr;
    assert(CURR_RANGE.ar_size);
#if DCC_DEBUG
    prange = &cenrange->ar_next;
    range  = newrange;
#endif
   }
   ASSERT_RELATION();
   size -= subsize;
   if (!size) goto end;
   addr += subsize;
#if DCC_DEBUG
   assert(addr == CURR_RANGE.ar_addr+CURR_RANGE.ar_size);
#endif
#undef subsize
  }
#undef CURR_RANGE_END
next:;
 }
 assert(prange);
 assert(range == *prange);
 assert(!range);
 assert(prange == &self->sc_dat.sd_alloc ||
        PREV_RANGE.ar_addr+PREV_RANGE.ar_size <= addr);
 newrange = DCCAllocRange_New(addr,size,n_refcnt);
 if unlikely(!newrange) goto err;
 newrange->ar_next = NULL;
 *prange           = newrange;
#undef CURR_RANGE_BEGIN
#undef HAS_PREV_RANGE
#undef HAS_NEXT_RANGE
#undef PREV_RANGE
#undef NEXT_RANGE
#undef CURR_RANGE
end: return result;
err: result = 0; goto end;
}
#else

LOCAL int
DCCSection_DIncrefN(struct DCCSection *__restrict self,
                    target_ptr_t addr, target_siz_t size,
                    unsigned int n_refcnt) {
 struct DCCAllocRange **prange,*range;
 struct DCCAllocRange *newrange;
 target_ptr_t addr_end,range_end;
 int result = 1;
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 assert(n_refcnt);
 if unlikely(!OK) goto err; /* Don't do anything if an error occurred. */
 if unlikely(!size) goto end; /* nothing to do here! */
 REFLOG(("DCCSection_DIncref: incref('%s',%#lx,%lu)\n",self->sc_start.sy_name->k_name,
        (unsigned long)addr,(unsigned long)size));
 addr_end = addr+size;
 prange = &self->sc_dat.sd_alloc;
 /* TODO: This function (seems) to work, but it could be written _MUCH_ better! */
 while ((range = *prange) != NULL) {
  target_siz_t bytes_until_next_range;
  assert(size);
  assert(range->ar_size);
  assert(range->ar_refcnt);
  assert(addr_end == addr+size);
  range_end = range->ar_addr+range->ar_size;
  //assert(addr >= range->ar_addr);
  assert(range_end > range->ar_addr);
  assert(!range->ar_next || range_end <= range->ar_next->ar_addr);

  if (range_end >= addr) {
   target_siz_t insbefore_size;
   /* The requested range is located before this one. */
   if (range->ar_refcnt == n_refcnt &&
       range->ar_addr   == addr_end) {
    /* Extend this range downwards. */
    range->ar_addr -= size;
    if (prange != &self->sc_dat.sd_alloc) {
     struct DCCAllocRange *prev_range;
     prev_range = ((struct DCCAllocRange *)((uintptr_t)prange-DCC_COMPILER_OFFSETOF(struct DCCAllocRange,ar_next)));
     assert(prev_range->ar_next == range);
     assert(prev_range->ar_addr+prev_range->ar_size <= range->ar_addr);
     if (prev_range->ar_addr+prev_range->ar_size == range->ar_addr &&
         prev_range->ar_refcnt == range->ar_refcnt) {
      /* Special case: Merge these two ranges. */
      prev_range->ar_size += range->ar_size;
      prev_range->ar_next  = range->ar_next;
      free(range);
     }
     assert(!prev_range->ar_next ||
             prev_range->ar_addr+prev_range->ar_size <=
             prev_range->ar_next->ar_addr);
    }
    goto end;
   }
   if (range->ar_addr >= addr) {
    insbefore_size = range->ar_addr-addr;
    if (insbefore_size) {
     if (insbefore_size > size)
         insbefore_size = size;
     assert(insbefore_size);
     /* Must insert a new range before this one! */
     newrange = DCCAllocRange_New(addr,insbefore_size,n_refcnt);
     if unlikely(!newrange) goto err;
     newrange->ar_next = range; /* Inherit object. */
     *prange           = newrange; /* Inherit object. */
     size             -= insbefore_size;
     assert(newrange->ar_next == range);
     assert(newrange->ar_addr+newrange->ar_size <= range->ar_addr);
     assert(!range->ar_next ||
             range->ar_addr+range->ar_size <=
             range->ar_next->ar_addr);
     if unlikely(!size) goto end;
     addr += insbefore_size;
     range = newrange;
     range_end = addr;
     assert(range_end == range->ar_addr+range->ar_size);
    } else if ((assert(addr      == range->ar_addr),
                       range_end == addr_end)) {
     assert(range->ar_addr == addr);
     assert(range->ar_size == size);
     /* This entire range is the perfect match! */
     range->ar_refcnt += n_refcnt;
     /* Must check neighboring ranges for merging. */
     newrange = range->ar_next;
     if (newrange &&
         newrange->ar_refcnt == range->ar_refcnt &&
         newrange->ar_addr == range_end) {
      /* Merge with range above. */
      range->ar_size += newrange->ar_size;
      range->ar_next  = newrange->ar_next;
      free(newrange);
     }
     assert(!range->ar_next ||
             range->ar_addr+range->ar_size <=
             range->ar_next->ar_addr);
     assert(addr == range->ar_addr);
     if (prange != &self->sc_dat.sd_alloc) {
      newrange = ((struct DCCAllocRange *)((uintptr_t)prange-DCC_COMPILER_OFFSETOF(struct DCCAllocRange,ar_next)));
      assert(newrange->ar_next == range);
      assert(newrange->ar_addr+newrange->ar_size <= range->ar_addr);
      if (newrange->ar_refcnt == range->ar_refcnt &&
          newrange->ar_addr+newrange->ar_size == addr) {
       /* Merge with range below. */
       newrange->ar_size += range->ar_size;
       newrange->ar_next  = range->ar_next;
       free(range);
      }
      assert(!newrange->ar_next ||
              newrange->ar_addr+newrange->ar_size <=
              newrange->ar_next->ar_addr);
     }
     goto end;
    }
   }
  }
  assert(addr >= range->ar_addr);
  assert(range_end == range->ar_addr+range->ar_size);
  if (range_end < addr) goto next;
  assert(range_end >= addr);
  /* Partial overlap near the middle, or end of 'range', or perfect adjacency. */
  if (addr_end <= range_end) {
   /* Partial overlap!
    * >> Must split the range into 2/3 parts! */
   assert(addr > range->ar_addr);
   assert(addr_end <= range_end);
   if (addr_end == range_end) {
    /* split & Incref at upper memory. */
    newrange = DCCAllocRange_New(addr,size,range->ar_refcnt+n_refcnt);
    if unlikely(!newrange) goto err;
    newrange->ar_next = range->ar_next;
    assert(!newrange->ar_next ||
            newrange->ar_addr+newrange->ar_size <=
            newrange->ar_next->ar_addr);
    range->ar_next    = newrange;
    range->ar_size    = addr-range->ar_addr;
   } else {
    struct DCCAllocRange *insrange;
    /* Must split into 3 parts! */
    assert(addr > range->ar_addr);
    assert(addr_end < range_end);
    insrange = DCCAllocRange_New(addr,size,range->ar_refcnt+n_refcnt);
    if unlikely(!insrange) goto err;
    newrange = DCCAllocRange_New(addr_end,range_end-addr_end,
                                 range->ar_refcnt);
    if unlikely(!newrange) { free(insrange); goto err; }
    range->ar_size    = addr-range->ar_addr;
    newrange->ar_next = range->ar_next;
    insrange->ar_next = newrange;
    range->ar_next    = insrange;
    assert(range->ar_size);
    assert(insrange->ar_size);
    assert(newrange->ar_size);
    assert(range->ar_next    == insrange);
    assert(insrange->ar_next == newrange);
    assert(range->ar_addr+range->ar_size       == insrange->ar_addr);
    assert(insrange->ar_addr+insrange->ar_size == newrange->ar_addr);
    assert(range->ar_refcnt == newrange->ar_refcnt);
    assert(range->ar_refcnt == insrange->ar_refcnt-n_refcnt);
   }
   assert(!range->ar_next || range->ar_addr+range->ar_size <= range->ar_next->ar_addr);
   goto end;
  }
  assert(addr >= range->ar_addr);
  if (addr < range_end) {
   if (addr == range->ar_addr) {
    /* This range is fully covered. */
    range->ar_refcnt += n_refcnt;
    size -= range->ar_size;
    if unlikely(!size) goto end;
    addr += range->ar_size;
   } else {
    /* Must split the range. */
    target_siz_t incsize = (range_end-addr);
    assert(incsize);
    assert(addr >= range->ar_addr);
    assert(addr <  range_end);
    newrange = DCCAllocRange_New(addr,incsize,range->ar_refcnt+n_refcnt);
    if unlikely(!newrange) goto err;
    newrange->ar_next = range->ar_next;
    range->ar_next    = newrange;
    range->ar_size   -= incsize;
    assert(range->ar_size);
    assert(!newrange->ar_next || newrange->ar_addr+newrange->ar_size <= newrange->ar_next->ar_addr);
    assert(range->ar_addr+range->ar_size <= newrange->ar_addr);
    size -= incsize;
    if (!size) goto end;
    range_end = addr;
    addr += incsize;
   }
  }
  assert(range_end == range->ar_addr+range->ar_size);
  if (range->ar_next) {
   assert(range->ar_next->ar_addr >= range_end);
   bytes_until_next_range = range->ar_next->ar_addr-range_end;
   if (bytes_until_next_range > size)
       bytes_until_next_range = size;
  } else {
   bytes_until_next_range = size;
  }
  if (!bytes_until_next_range) goto next;
  if (range->ar_refcnt == n_refcnt &&
      range_end        == addr) {
   /* Perfect adjacency (Extend upwards). */
   range->ar_size += bytes_until_next_range;
  } else {
   /* Insert a new range after 'range'. */
   newrange = DCCAllocRange_New(addr,bytes_until_next_range,n_refcnt);
   if unlikely(!newrange) goto err;
   newrange->ar_next = range->ar_next;
   assert(!newrange->ar_next ||
           newrange->ar_addr+newrange->ar_size <=
           newrange->ar_next->ar_addr);
   range->ar_next    = newrange;
   assert(range->ar_addr+range->ar_size <= newrange->ar_addr);
   range             = newrange;
  }
  assert(!range->ar_next ||
          range->ar_addr+range->ar_size <=
          range->ar_next->ar_addr);
  size -= bytes_until_next_range;
  if unlikely(!size) goto end;
  addr += bytes_until_next_range;
next:
  prange = &range->ar_next;
 }
 /* Append a new range. */
 assert(range == *prange);
 assert(!range);
 assert(prange == &self->sc_dat.sd_alloc ||
       ((struct DCCAllocRange *)((uintptr_t)prange-DCC_COMPILER_OFFSETOF(struct DCCAllocRange,ar_next)))->ar_addr+
       ((struct DCCAllocRange *)((uintptr_t)prange-DCC_COMPILER_OFFSETOF(struct DCCAllocRange,ar_next)))->ar_size <= addr);
 range = DCCAllocRange_New(addr,size,n_refcnt);
 if unlikely(!range) goto err;
 range->ar_next = NULL;
 *prange = range;
end: return result;
err: result = 0; goto end;
}
#endif

#if 1
PUBLIC int
DCCSection_DDecref(struct DCCSection *__restrict self,
                   target_ptr_t addr, target_siz_t size) {
 struct DCCAllocRange **prange,*range,*newrange;
 target_siz_t overlap;
 int result = 1;
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 if unlikely(!OK) goto err; /* Don't do anything if an error occurred. */
 assert(!DCCFreeData_Has(&self->sc_dat.sd_free,addr,size));
 prange   = &self->sc_dat.sd_alloc;
 REFLOG(("--- decref('%s',%#lx,%lu)\n",self->sc_start.sy_name->k_name,
        (unsigned long)addr,(unsigned long)size));
 /* Search range and only free reference that dropped to ZERO(0). */
 while(size) {
#define CURR_RANGE       (*range)
#define NEXT_RANGE       (*range->ar_next)
#define PREV_RANGE       (*(struct DCCAllocRange *)((uintptr_t)prange-DCC_COMPILER_OFFSETOF(struct DCCAllocRange,ar_next)))
#define HAS_NEXT_RANGE   (range->ar_next != NULL)
#define HAS_PREV_RANGE   (prange != &self->sc_dat.sd_alloc)
#define CURR_RANGE_BEGIN (CURR_RANGE.ar_addr)
#if DCC_DEBUG
#define ASSERT_RELATION() \
  (assert(!prange || !HAS_PREV_RANGE || (PREV_RANGE.ar_addr+PREV_RANGE.ar_size <= CURR_RANGE.ar_addr)),\
              assert(!HAS_NEXT_RANGE || (NEXT_RANGE.ar_addr >= CURR_RANGE.ar_addr+CURR_RANGE.ar_size)))
#else
#define ASSERT_RELATION() (void)0
#endif
  range = *prange;
  assert(size);
  assert(CURR_RANGE.ar_size);
  assert(CURR_RANGE.ar_refcnt);
  assertf(addr >= CURR_RANGE_BEGIN,
          "No reference count mapping for address range %#lx...%#lx in '%s'",
         (unsigned long)addr,(unsigned long)(addr+size),
          self->sc_start.sy_name->k_name);
  ASSERT_RELATION();
  if (addr == CURR_RANGE_BEGIN) {
again_rangestart:
   /* Truncate before, split, or delete this range. */
   overlap = CURR_RANGE.ar_size;
   if (overlap > size) overlap = size;
   assert(overlap);
   if (CURR_RANGE.ar_refcnt == 1) {
    /* Last reference. - We can modify this range directly. */
    DCCSection_DSafeFree(self,addr,overlap);
    assert(CURR_RANGE.ar_size >= overlap);
    CURR_RANGE.ar_size -= overlap;
    if (CURR_RANGE.ar_size)
     CURR_RANGE.ar_addr += overlap;
    else {
     /* Delete this range. */
     *prange = range->ar_next;
     free(range);
     range = *prange;
    }
    assert(!range || CURR_RANGE.ar_size);
   } else {
    if (overlap != CURR_RANGE.ar_size) {
     /* Must split the range at 'addr+overlap' */
     assert(CURR_RANGE.ar_size > overlap);
     newrange = DCCAllocRange_New(addr+overlap,
                                  CURR_RANGE.ar_size-overlap,
                                  CURR_RANGE.ar_refcnt);
     if unlikely(!newrange) goto err;
     newrange->ar_next  = CURR_RANGE.ar_next;
     CURR_RANGE.ar_next = newrange;
     CURR_RANGE.ar_size = overlap;
    }
    assert(addr    == CURR_RANGE.ar_addr);
    assert(overlap == CURR_RANGE.ar_size);
    /* Drop a reference from the range's remainder. */
    --CURR_RANGE.ar_refcnt;
    if (HAS_PREV_RANGE &&
        PREV_RANGE.ar_refcnt                  == CURR_RANGE.ar_refcnt &&
        PREV_RANGE.ar_addr+PREV_RANGE.ar_size == CURR_RANGE.ar_addr) {
     /* Merge this range with its predecessor. */
     PREV_RANGE.ar_size += CURR_RANGE.ar_size;
     assert(PREV_RANGE.ar_size > CURR_RANGE.ar_size);
     PREV_RANGE.ar_next  = CURR_RANGE.ar_next;
     free(range);
     range = &PREV_RANGE;
#if DCC_DEBUG
     prange = NULL;
#endif
    }
    if (HAS_NEXT_RANGE &&
        NEXT_RANGE.ar_refcnt == CURR_RANGE.ar_refcnt &&
        NEXT_RANGE.ar_addr   == CURR_RANGE.ar_addr+CURR_RANGE.ar_size) {
     newrange = &NEXT_RANGE;
     /* Merge this range with its successor. */
     CURR_RANGE.ar_size += newrange->ar_size;
     assert(CURR_RANGE.ar_size > newrange->ar_size);
     CURR_RANGE.ar_next  = newrange->ar_next;
     free(newrange);
    }
    assert(CURR_RANGE.ar_size);
   }
#if DCC_DEBUG
   if (range) ASSERT_RELATION();
#endif
   /* Update the decref address/size */
   size -= overlap;
   if (!size) goto end;
   addr += overlap;
   assert(range);
   /* This can happen when multiple successive ranges must be decref'ed. */
   if (addr == CURR_RANGE_BEGIN)
       goto again_rangestart;
  }
  assert(addr > CURR_RANGE_BEGIN);
  {
   target_ptr_t range_end;
   range_end = CURR_RANGE.ar_addr+CURR_RANGE.ar_size;
#define CURR_RANGE_END range_end
   if (addr >= CURR_RANGE_END) goto next; /* These aren't the ranges you're looking for... */
   assert(addr > CURR_RANGE_BEGIN);
   assert(addr < CURR_RANGE_END);
   overlap = CURR_RANGE_END-addr;
   if (overlap > size) overlap = size;
   assert(overlap < CURR_RANGE.ar_size);
   if (addr+overlap == CURR_RANGE_END) {
    /* Truncate after 'addr+overlap' or split the current range into 2 parts. */
    if (CURR_RANGE.ar_refcnt == 1) {
     DCCSection_DSafeFree(self,addr,overlap);
    } else {
     /* Must split the current range after 'addr' */
     newrange = DCCAllocRange_New(addr,overlap,
                                  CURR_RANGE.ar_refcnt-1);
     if unlikely(!newrange) goto err;
     newrange->ar_next  = CURR_RANGE.ar_next;
     CURR_RANGE.ar_next = newrange;
    }
    assert(overlap < CURR_RANGE.ar_size);
    CURR_RANGE.ar_size -= overlap;
   } else {
    assert(overlap == size);
    /* Split the current range into 3 parts. */
    if (CURR_RANGE.ar_refcnt == 1) {
     /* Can re-use parts of the current range. */
     assert(CURR_RANGE_END > (addr+overlap));
     newrange = DCCAllocRange_New(addr+overlap,
                                  CURR_RANGE_END-(addr+overlap),
                                  CURR_RANGE.ar_refcnt);
     if unlikely(!newrange) goto err;
     DCCSection_DSafeFree(self,addr,overlap);
     newrange->ar_next  = CURR_RANGE.ar_next;
     CURR_RANGE.ar_next = newrange;
     assert(addr > CURR_RANGE_BEGIN);
     CURR_RANGE.ar_size = addr-CURR_RANGE_BEGIN;
    } else {
     struct DCCAllocRange *insrange;
     /* Must really split the range into 3 parts. */
     insrange = DCCAllocRange_New(addr,overlap,
                                  CURR_RANGE.ar_refcnt-1);
     if unlikely(!insrange) goto err;
     assert(CURR_RANGE_END > (addr+overlap));
     newrange = DCCAllocRange_New(addr+overlap,
                                  CURR_RANGE_END-(addr+overlap),
                                  CURR_RANGE.ar_refcnt);
     if unlikely(!newrange) { free(insrange); goto err; }
     newrange->ar_next  = CURR_RANGE.ar_next;
     insrange->ar_next  = newrange;
     CURR_RANGE.ar_next = insrange;
     assert(addr > CURR_RANGE_BEGIN);
     CURR_RANGE.ar_size = addr-CURR_RANGE_BEGIN;
    }
    ASSERT_RELATION();
    goto end;
   }
   size -= overlap;
   if (!size) goto end;
   addr += overlap;
#undef CURR_RANGE_END
  }
next:;
  prange = &range->ar_next;
#undef CURR_RANGE_BEGIN
#undef HAS_PREV_RANGE
#undef HAS_NEXT_RANGE
#undef PREV_RANGE
#undef NEXT_RANGE
#undef CURR_RANGE
 }
end: return result;
err: result = 0; goto end;
}
#else
PUBLIC int
DCCSection_DDecref(struct DCCSection *__restrict self,
                   target_ptr_t addr, target_siz_t size) {
 struct DCCAllocRange **prange,*range,*newrange;
 target_ptr_t addr_end,range_end;
 int result = 1;
 assert(self);
 assert(!DCCSection_ISIMPORT(self));
 if unlikely(!OK) goto err; /* Don't do anything if an error occurred. */
 prange   = &self->sc_dat.sd_alloc;
 addr_end = addr+size;
 REFLOG(("--- decref('%s',%#lx,%lu)\n",self->sc_start.sy_name->k_name,
        (unsigned long)addr,(unsigned long)size));
 /* Search range and only free reference that dropped to ZERO(0). */
 while (size) {
  assert(addr_end == addr+size);
  range = *prange;
  assertf(range && addr >= range->ar_addr,
          "No reference count mapping for address range %#lx...%#lx in '%s'",
         (unsigned long)addr,(unsigned long)(addr+size),
          self->sc_start.sy_name->k_name);
  assert(range->ar_size);
  assert(range->ar_refcnt);
  range_end = range->ar_addr+range->ar_size;
  assert(!range->ar_next || range_end <= range->ar_next->ar_addr);
  if (range_end > addr) {
   /* Overlap found! */
   assert(range->ar_addr <= addr);
   if (range->ar_addr == addr) {
    /* Matching lower bound. */
    if (addr_end >= range_end) {
     /* All bounds are matching! */
     assert(size >= range->ar_size);
     size -= range->ar_size;
     addr += range->ar_size;
     if (!--range->ar_refcnt) {
      *prange = range->ar_next;
      DCCSection_DSafeFree(self,range->ar_addr,range->ar_size);
      free(range);
     }
     assert((!*prange) || !(*prange)->ar_next ||
            (*prange)->ar_addr+(*prange)->ar_size <= (*prange)->ar_next->ar_addr);
     continue;
    } else {
     target_siz_t delsize;
     assert(addr_end >= range->ar_addr);
     delsize = (addr_end-range->ar_addr);
     assert(delsize < range->ar_size);
     assert(addr == range->ar_addr);
     DCCSection_DSafeFree(self,addr,delsize);
     if (range->ar_refcnt == 1) {
      /* Must shrink this range. */
      range->ar_addr += delsize;
      range->ar_size -= delsize;
     } else {
      /* Must split this range. */
      newrange = DCCAllocRange_New(addr+delsize,
                                   range_end-(addr+delsize),
                                   range->ar_refcnt);
      if unlikely(!newrange) goto err;
      newrange->ar_next = range->ar_next;
      range->ar_next    = newrange;
      range->ar_size    = delsize;
      --range->ar_refcnt;
     }
     assert(range->ar_size);
     assert(!range->ar_next || range->ar_addr+range->ar_size <= range->ar_next->ar_addr);
     size -= delsize;
     if (!size) goto end;
     addr += delsize;
    }
   } else if ((assert(range->ar_addr < addr),
                      range_end <= addr_end)) {
    /* Matching upper bound. */
    target_siz_t delsize;
    assert(range_end >= addr);
    delsize = range_end-addr;
    assert(addr == range_end-delsize);
    if (range->ar_refcnt == 1) {
     /* Must shrink this range. */
    } else {
     /* Must split this range. */
     newrange = DCCAllocRange_New(addr,delsize,range->ar_refcnt);
     if unlikely(!newrange) goto err;
     newrange->ar_next = range->ar_next;
     range->ar_next = newrange;
     --range->ar_refcnt;
    }
    range->ar_size -= delsize;
    assert(range->ar_size);
    assert(!range->ar_next || range->ar_addr+range->ar_size <= range->ar_next->ar_addr);
    size -= delsize;
    if (!size) goto end;
    addr += delsize;
   } else {
    /* Decref sub-range: must split this range twice. */
    assert(addr > range->ar_addr);
    assert(addr_end < range_end);
    if (range->ar_refcnt == 1) {
     /* We can fake the second split by re-using that would otherwise be deleted. */
     newrange = DCCAllocRange_New(addr_end,
                                 (target_siz_t)(range_end-addr_end),
                                  1);
     if unlikely(!newrange) goto err;
     newrange->ar_next = range->ar_next;
     range->ar_next = newrange;
     range->ar_size = (target_siz_t)(addr-range->ar_addr);
     DCCSection_DSafeFree(self,addr,size);
     assert(range->ar_size);
     assert(newrange->ar_size);
     assert(range->ar_next == newrange);
     assert(range->ar_addr+range->ar_size <= newrange->ar_addr);
    } else {
     struct DCCAllocRange *insrange;
     /* Well... Now we must ~actually~ create two sub-ranges. */
     insrange = DCCAllocRange_New(addr,size,range->ar_refcnt-1);
     if unlikely(!insrange) goto err;
     newrange = DCCAllocRange_New(addr_end,
                                 (target_siz_t)(range_end-addr_end),
                                  range->ar_refcnt);
     if unlikely(!newrange) { free(insrange); goto err; }
     newrange->ar_next = range->ar_next;
     insrange->ar_next = newrange;
     range->ar_next    = insrange;
     range->ar_size    = (target_siz_t)(addr-range->ar_addr);
     /* Make sure the ranges were linked properly. */
     assert(range->ar_size);
     assert(insrange->ar_size);
     assert(newrange->ar_size);
     assert(range->ar_next    == insrange);
     assert(insrange->ar_next == newrange);
     assert(range->ar_addr+range->ar_size       == insrange->ar_addr);
     assert(insrange->ar_addr+insrange->ar_size == newrange->ar_addr);
     assert(range->ar_refcnt == newrange->ar_refcnt);
     assert(range->ar_refcnt == insrange->ar_refcnt+1);
    }
    goto end;
   }
  }
  prange = &range->ar_next;
 }
end: return result;
err: result = 0; goto end;
}
#endif

PUBLIC int
DCCSection_DIncref(struct DCCSection *__restrict self,
                   target_ptr_t addr, target_siz_t size) {
 return DCCSection_DIncrefN(self,addr,size,1);
}
INTERN int
DCCSection_DIncrefN_impl(struct DCCSection *__restrict self,
                         target_ptr_t addr, target_siz_t size,
                         unsigned int n_refcnt) {
 return DCCSection_DIncrefN(self,addr,size,n_refcnt);
}

DCC_DECL_END

#endif /* !GUARD_DCC_UNIT_SECMEM_C_INL */
