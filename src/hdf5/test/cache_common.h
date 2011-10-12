/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
 * Copyright by The HDF Group.                                               *
 * Copyright by the Board of Trustees of the University of Illinois.         *
 * All rights reserved.                                                      *
 *                                                                           *
 * This file is part of HDF5.  The full HDF5 copyright notice, including     *
 * terms governing use, modification, and redistribution, is contained in    *
 * the files COPYING and Copyright.html.  COPYING can be found at the root   *
 * of the source code distribution tree; Copyright.html can be found at the  *
 * root level of an installed copy of the electronic HDF5 document set and   *
 * is linked from the top-level documents page.  It can also be found at     *
 * http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have          *
 * access to either file, you may request a copy from help@hdfgroup.org.     *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

/* Programmer:  John Mainzer
 *              10/27/05
 *
 *		This file contains common #defines, type definitions, and
 *		externs for tests of the cache implemented in H5C.c
 */
#ifndef _CACHE_COMMON_H
#define _CACHE_COMMON_H

#define H5C_PACKAGE             /*suppress error about including H5Cpkg   */
#define H5F_PACKAGE             /*suppress error about including H5Fpkg   */

/* Include library header files */
#include "H5ACprivate.h"
#include "H5Cpkg.h"
#include "H5Fpkg.h"
#include "H5Iprivate.h"

/* Include test header files */
#include "h5test.h"

#define NO_CHANGE       	-1

/* with apologies for the abuse of terminology... */

#define PICO_ENTRY_TYPE		0
#define NANO_ENTRY_TYPE		1
#define MICRO_ENTRY_TYPE	2
#define TINY_ENTRY_TYPE		3
#define SMALL_ENTRY_TYPE	4
#define MEDIUM_ENTRY_TYPE	5
#define LARGE_ENTRY_TYPE	6
#define HUGE_ENTRY_TYPE		7
#define MONSTER_ENTRY_TYPE	8
#define VARIABLE_ENTRY_TYPE	9

#define NUMBER_OF_ENTRY_TYPES   10

#define PICO_ENTRY_SIZE		(size_t)1
#define NANO_ENTRY_SIZE		(size_t)4
#define MICRO_ENTRY_SIZE	(size_t)16
#define TINY_ENTRY_SIZE		(size_t)64
#define SMALL_ENTRY_SIZE	(size_t)256
#define MEDIUM_ENTRY_SIZE	(size_t)1024
#define LARGE_ENTRY_SIZE	(size_t)(4 * 1024)
#define HUGE_ENTRY_SIZE		(size_t)(16 * 1024)
#define MONSTER_ENTRY_SIZE	(size_t)(64 * 1024)
#define VARIABLE_ENTRY_SIZE	(size_t)(10 * 1024)

#define NUM_PICO_ENTRIES	(10 * 1024)
#define NUM_NANO_ENTRIES	(10 * 1024)
#define NUM_MICRO_ENTRIES	(10 * 1024)
#define NUM_TINY_ENTRIES	(10 * 1024)
#define NUM_SMALL_ENTRIES	(10 * 1024)
#define NUM_MEDIUM_ENTRIES	(10 * 1024)
#define NUM_LARGE_ENTRIES	(10 * 1024)
#define NUM_HUGE_ENTRIES	(10 * 1024)
#define NUM_MONSTER_ENTRIES	(10 * 1024)
#define NUM_VARIABLE_ENTRIES	(10 * 1024)

#define MAX_ENTRIES		(10 * 1024)


/* The choice of the BASE_ADDR below is arbitrary -- it just has to be
 * larger than the superblock.
 */
#define BASE_ADDR		(haddr_t)1024
#define PICO_BASE_ADDR		BASE_ADDR
#define NANO_BASE_ADDR		(haddr_t)(PICO_BASE_ADDR + \
                                      (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_BASE_ADDR		(haddr_t)(NANO_BASE_ADDR + \
                                      (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_BASE_ADDR		(haddr_t)(MICRO_BASE_ADDR + \
			              (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_BASE_ADDR		(haddr_t)(TINY_BASE_ADDR + \
                                      (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_BASE_ADDR	(haddr_t)(SMALL_BASE_ADDR + \
                                      (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_BASE_ADDR		(haddr_t)(MEDIUM_BASE_ADDR + \
                                      (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_BASE_ADDR		(haddr_t)(LARGE_BASE_ADDR + \
                                      (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_BASE_ADDR	(haddr_t)(HUGE_BASE_ADDR + \
                                      (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))
#define VARIABLE_BASE_ADDR	(haddr_t)(MONSTER_BASE_ADDR + \
				     (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))

#define PICO_ALT_BASE_ADDR	(haddr_t)(VARIABLE_BASE_ADDR + \
			           (VARIABLE_ENTRY_SIZE * NUM_VARIABLE_ENTRIES))
#define NANO_ALT_BASE_ADDR	(haddr_t)(PICO_ALT_BASE_ADDR + \
                                      (PICO_ENTRY_SIZE * NUM_PICO_ENTRIES))
#define MICRO_ALT_BASE_ADDR	(haddr_t)(NANO_ALT_BASE_ADDR + \
                                      (NANO_ENTRY_SIZE * NUM_NANO_ENTRIES))
#define TINY_ALT_BASE_ADDR	(haddr_t)(MICRO_ALT_BASE_ADDR + \
			              (MICRO_ENTRY_SIZE * NUM_MICRO_ENTRIES))
#define SMALL_ALT_BASE_ADDR	(haddr_t)(TINY_ALT_BASE_ADDR + \
                                      (TINY_ENTRY_SIZE * NUM_TINY_ENTRIES))
#define MEDIUM_ALT_BASE_ADDR	(haddr_t)(SMALL_ALT_BASE_ADDR + \
                                      (SMALL_ENTRY_SIZE * NUM_SMALL_ENTRIES))
#define LARGE_ALT_BASE_ADDR	(haddr_t)(MEDIUM_ALT_BASE_ADDR + \
                                      (MEDIUM_ENTRY_SIZE * NUM_MEDIUM_ENTRIES))
#define HUGE_ALT_BASE_ADDR	(haddr_t)(LARGE_ALT_BASE_ADDR + \
                                      (LARGE_ENTRY_SIZE * NUM_LARGE_ENTRIES))
#define MONSTER_ALT_BASE_ADDR	(haddr_t)(HUGE_ALT_BASE_ADDR + \
                                      (HUGE_ENTRY_SIZE * NUM_HUGE_ENTRIES))
#define VARIABLE_ALT_BASE_ADDR	(haddr_t)(MONSTER_ALT_BASE_ADDR + \
                                     (MONSTER_ENTRY_SIZE * NUM_MONSTER_ENTRIES))
#define MAX_ADDR		(haddr_t)(VARIABLE_ALT_BASE_ADDR + \
				   (VARIABLE_ENTRY_SIZE * NUM_VARIABLE_ENTRIES))
#define ADDR_SPACE_SIZE		(haddr_t)(MAX_ADDR - BASE_ADDR)

#define MAX_PINS	8	/* Maximum number of entries that can be
				 * directly pinned by a single entry.
				 */

#define FLUSH_OP__NO_OP		0
#define FLUSH_OP__DIRTY		1
#define FLUSH_OP__RESIZE	2
#define FLUSH_OP__MOVE		3
#define FLUSH_OP__MAX_OP	3

#define MAX_FLUSH_OPS		10	/* Maximum number of flush operations
					 * that can be associated with a
					 * cache entry.
					 */

typedef struct flush_op
{
    int			op_code;	/* integer op code indicating the
					 * operation to be performed.  At
					 * present it must be one of:
					 *
					 *   FLUSH_OP__NO_OP
					 *   FLUSH_OP__DIRTY
					 *   FLUSH_OP__RESIZE
					 *   FLUSH_OP__MOVE
					 */
    int			type;		/* type code of the cache entry that
					 * is the target of the operation.
					 * This value is passed into the
					 * function implementing the flush
					 * operation.
					 */
    int			idx;		/* index of the cache entry that
					 * is the target of the operation.
					 * This value is passed into the
                                         * function implementing the flush
                                         * operation.
					 */
    hbool_t		flag;		/* boolean flag passed into the
					 * function implementing the flush
					 * operation.  The meaning of the
					 * flag is dependant upon the flush
					 * operation:
					 *
					 * FLUSH_OP__DIRTY: TRUE iff the
					 *   target is pinned, and is to
					 *   be dirtied via the
					 *   H5C_mark_entry_dirty()
					 *   call.
					 *
					 * FLUSH_OP__RESIZE: TRUE iff the
					 *   target is pinned, and is to
					 *   be resized via the
					 *   H5C_resize_entry()
					 *   call.
					 *
					 * FLUSH_OP__MOVE: TRUE iff the
					 *    target is to be moved to
					 *    its main address.
					 */
    size_t		size;		/* New target size in the
					 * FLUSH_OP__MOVE operation.
					 * Unused elsewhere.
					 */
} flush_op;

typedef struct test_entry_t
{
    H5C_cache_entry_t	  header;	/* entry data used by the cache
					 * -- must be first
                               		 */
    struct test_entry_t * self; 	/* pointer to this entry -- used for
					 * sanity checking.
                                         */
    H5C_t               * cache_ptr;	/* pointer to the cache in which
					 * the entry resides, or NULL if the
					 * entry is not in cache.
					 */
    haddr_t		  addr;         /* where the cache thinks this entry
                                         * is located
                                         */
    hbool_t		  at_main_addr;	/* boolean flag indicating whether
					 * the entry is supposed to be at
					 * either its main or alternate
					 * address.
     					 */
    haddr_t		  main_addr;    /* initial location of the entry
                                         */
    haddr_t		  alt_addr;	/* location to which the entry
					 * can be relocated or "moved"
                                         */
    size_t		  size;         /* how big the cache thinks this
                                         * entry is
                                         */
    int32_t		  type;		/* indicates which entry array this
					 * entry is in
                                         */
    int32_t		  index;	/* index in its entry array
                                         */
    int32_t		  reads;	/* number of times this entry has
					 * been loaded.
                                         */
    int32_t		  writes;	/* number of times this entry has
                                         * been written
                                         */
    hbool_t		  is_dirty;	/* entry has been modified since
                                         * last write
                                         */
    hbool_t		  is_protected;	/* entry should currently be on
					 * the cache's protected list.
                                         */
    hbool_t		  is_read_only; /* TRUE iff the entry should be
					 * protected read only.
					 */
    int			  ro_ref_count; /* Number of outstanding read only
					 * protects on the entry.
					 */
    hbool_t		  is_pinned;	/* entry is currently pinned in
					 * the cache.
                                         */
    int			  pinning_ref_count; /* Number of entries that
					 * pin this entry in the cache.
					 * When this count drops to zero,
					 * this entry should be unpinned.
					 */
    int			  num_pins;     /* Number of entries that this
					 * entry pins in the cache.  This
					 * value must be in the range
					 * [0, MAX_PINS].
					 */
    int			  pin_type[MAX_PINS]; /* array of the types of entries
					 * pinned by this entry.
					 */
    int			  pin_idx[MAX_PINS]; /* array of the indicies of
					 * entries pinned by this entry.
					 */
    int			  num_flush_ops; /* integer field containing the
					 * number of flush operations to
					 * be executed when the entry is
					 * flushed.  This value must lie in
					 * the closed interval
					 * [0, MAX_FLUSH_OPS].
					 */
    struct flush_op	  flush_ops[MAX_FLUSH_OPS]; /* Array of instances
					 * of struct flush_op detailing the
					 * flush operations (if any) that
					 * are to be executed when the entry
					 * is flushed from the cache.
					 *
					 * num_flush_ops contains the number
					 * of valid entries in this array.
					 */
    hbool_t		  flush_op_self_resize_in_progress; /* Boolean flag
					 * that is set to TRUE iff this
					 * entry is being flushed, it has
					 * been resized by a resize flush
					 * op, and the flush function has
					 * not yet returned,  This field is
					 * used to turn off overactive santity
					 * checking code that would otherwise
					 * cause a false test failure.
					 */
    hbool_t		  loaded;       /* entry has been loaded since the
                                         * last time it was reset.
                                         */
    hbool_t		  cleared;      /* entry has been cleared since the
                                         * last time it was reset.
                                         */
    hbool_t		  flushed;      /* entry has been flushed since the
                                         * last time it was reset.
                                         */
    hbool_t               destroyed;    /* entry has been destroyed since the
                                         * last time it was reset.
                                         */
} test_entry_t;

/* The following are cut down test versions of the hash table manipulation
 * macros from H5Cpkg.h, which have been further modified to avoid references
 * to the error reporting macros.  Needless to say, these macros must be
 * updated as necessary.
 */

#define H5C__HASH_MASK          ((size_t)(H5C__HASH_TABLE_LEN - 1) << 3)
#define H5C__HASH_FCN(x)        (int)(((x) & H5C__HASH_MASK) >> 3)

#define H5C_TEST__PRE_HT_SEARCH_SC(cache_ptr, Addr)          \
if ( ( (cache_ptr) == NULL ) ||                              \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||           \
     ( (cache_ptr)->index_size !=                            \
       ((cache_ptr)->clean_index_size + (cache_ptr)->dirty_index_size) ) || \
     ( ! H5F_addr_defined(Addr) ) ||                         \
     ( H5C__HASH_FCN(Addr) < 0 ) ||                          \
     ( H5C__HASH_FCN(Addr) >= H5C__HASH_TABLE_LEN ) ) {      \
    HDfprintf(stdout, "Pre HT search SC failed.\n");         \
}

#define H5C_TEST__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k) \
if ( ( (cache_ptr) == NULL ) ||                                   \
     ( (cache_ptr)->magic != H5C__H5C_T_MAGIC ) ||                \
     ( (cache_ptr)->index_len < 1 ) ||                            \
     ( (entry_ptr) == NULL ) ||                                   \
     ( (cache_ptr)->index_size < (entry_ptr)->size ) ||           \
     ( (cache_ptr)->index_size !=                                 \
       ((cache_ptr)->clean_index_size + (cache_ptr)->dirty_index_size) ) || \
     ( H5F_addr_ne((entry_ptr)->addr, (Addr)) ) ||                \
     ( (entry_ptr)->size <= 0 ) ||                                \
     ( ((cache_ptr)->index)[k] == NULL ) ||                       \
     ( ( ((cache_ptr)->index)[k] != (entry_ptr) ) &&              \
       ( (entry_ptr)->ht_prev == NULL ) ) ||                      \
     ( ( ((cache_ptr)->index)[k] == (entry_ptr) ) &&              \
       ( (entry_ptr)->ht_prev != NULL ) ) ||                      \
     ( ( (entry_ptr)->ht_prev != NULL ) &&                        \
       ( (entry_ptr)->ht_prev->ht_next != (entry_ptr) ) ) ||      \
     ( ( (entry_ptr)->ht_next != NULL ) &&                        \
       ( (entry_ptr)->ht_next->ht_prev != (entry_ptr) ) ) ) {     \
    HDfprintf(stdout, "Post successful HT search SC failed.\n");  \
}

#define H5C_TEST__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k)      \
if ( ( (cache_ptr) == NULL ) ||                                        \
     ( ((cache_ptr)->index)[k] != (entry_ptr) ) ||                     \
     ( (entry_ptr)->ht_prev != NULL ) ) {                              \
    HDfprintf(stdout, "Post HT shift to front failed.\n");             \
}

#define H5C_TEST__SEARCH_INDEX(cache_ptr, Addr, entry_ptr)              \
{                                                                       \
    int k;                                                              \
    int depth = 0;                                                      \
    H5C_TEST__PRE_HT_SEARCH_SC(cache_ptr, Addr)                         \
    k = H5C__HASH_FCN(Addr);                                            \
    entry_ptr = ((cache_ptr)->index)[k];                                \
    while ( ( entry_ptr ) && ( H5F_addr_ne(Addr, (entry_ptr)->addr) ) ) \
    {                                                                   \
        (entry_ptr) = (entry_ptr)->ht_next;                             \
        (depth)++;                                                      \
    }                                                                   \
    if ( entry_ptr )                                                    \
    {                                                                   \
        H5C_TEST__POST_SUC_HT_SEARCH_SC(cache_ptr, entry_ptr, Addr, k)  \
        if ( entry_ptr != ((cache_ptr)->index)[k] )                     \
        {                                                               \
            if ( (entry_ptr)->ht_next )                                 \
            {                                                           \
                (entry_ptr)->ht_next->ht_prev = (entry_ptr)->ht_prev;   \
            }                                                           \
            HDassert( (entry_ptr)->ht_prev != NULL );                   \
            (entry_ptr)->ht_prev->ht_next = (entry_ptr)->ht_next;       \
            ((cache_ptr)->index)[k]->ht_prev = (entry_ptr);             \
            (entry_ptr)->ht_next = ((cache_ptr)->index)[k];             \
            (entry_ptr)->ht_prev = NULL;                                \
            ((cache_ptr)->index)[k] = (entry_ptr);                      \
            H5C_TEST__POST_HT_SHIFT_TO_FRONT(cache_ptr, entry_ptr, k)   \
        }                                                               \
    }                                                                   \
}

/* Macros used in H5AC level tests */

#define CACHE_CONFIGS_EQUAL(a, b, cmp_set_init, cmp_init_size)        \
  ( ( (a).version                 == (b).version ) &&                 \
    ( (a).rpt_fcn_enabled         == (b).rpt_fcn_enabled ) &&         \
    ( (a).open_trace_file         == (b).open_trace_file ) &&         \
    ( (a).close_trace_file        == (b).close_trace_file ) &&        \
    ( ( (a).open_trace_file == FALSE ) ||                             \
      ( strcmp((a).trace_file_name, (b).trace_file_name) == 0 ) ) &&  \
    ( (a).evictions_enabled       == (b).evictions_enabled ) &&       \
    ( ( ! cmp_set_init ) ||                                           \
      ( (a).set_initial_size      == (b).set_initial_size ) ) &&      \
    ( ( ! cmp_init_size ) ||                                          \
      ( (a).initial_size          == (b).initial_size ) ) &&          \
    ( (a).min_clean_fraction      == (b).min_clean_fraction ) &&      \
    ( (a).max_size                == (b).max_size ) &&                \
    ( (a).min_size                == (b).min_size ) &&                \
    ( (a).epoch_length            == (b).epoch_length ) &&            \
    ( (a).incr_mode               == (b).incr_mode ) &&               \
    ( (a).lower_hr_threshold      == (b).lower_hr_threshold ) &&      \
    ( (a).increment               == (b).increment ) &&               \
    ( (a).apply_max_increment     == (b).apply_max_increment ) &&     \
    ( (a).max_increment           == (b).max_increment ) &&           \
    ( (a).flash_incr_mode         == (b).flash_incr_mode ) &&         \
    ( (a).flash_multiple          == (b).flash_multiple ) &&          \
    ( (a).flash_threshold         == (b).flash_threshold ) &&         \
    ( (a).decr_mode               == (b).decr_mode ) &&               \
    ( (a).upper_hr_threshold      == (b).upper_hr_threshold ) &&      \
    ( (a).decrement               == (b).decrement ) &&               \
    ( (a).apply_max_decrement     == (b).apply_max_decrement ) &&     \
    ( (a).max_decrement           == (b).max_decrement ) &&           \
    ( (a).epochs_before_eviction  == (b).epochs_before_eviction ) &&  \
    ( (a).apply_empty_reserve     == (b).apply_empty_reserve ) &&     \
    ( (a).empty_reserve           == (b).empty_reserve ) &&           \
    ( (a).dirty_bytes_threshold   == (b).dirty_bytes_threshold ) &&   \
    ( (a).metadata_write_strategy == (b).metadata_write_strategy ) )

#define XLATE_EXT_TO_INT_MDC_CONFIG(i, e)                           \
{                                                                   \
    (i).version                = H5C__CURR_AUTO_SIZE_CTL_VER;       \
    if ( (e).rpt_fcn_enabled )                                      \
        (i).rpt_fcn            = H5C_def_auto_resize_rpt_fcn;       \
    else                                                            \
        (i).rpt_fcn            = NULL;                              \
    (i).set_initial_size       = (e).set_initial_size;              \
    (i).initial_size           = (e).initial_size;                  \
    (i).min_clean_fraction     = (e).min_clean_fraction;            \
    (i).max_size               = (e).max_size;                      \
    (i).min_size               = (e).min_size;                      \
    (i).epoch_length           = (long int)((e).epoch_length);      \
    (i).incr_mode              = (e).incr_mode;                     \
    (i).lower_hr_threshold     = (e).lower_hr_threshold;            \
    (i).increment              = (e).increment;                     \
    (i).apply_max_increment    = (e).apply_max_increment;           \
    (i).max_increment          = (e).max_increment;                 \
    (i).flash_incr_mode        = (e).flash_incr_mode;               \
    (i).flash_multiple         = (e).flash_multiple;                \
    (i).flash_threshold        = (e).flash_threshold;               \
    (i).decr_mode              = (e).decr_mode;                     \
    (i).upper_hr_threshold     = (e).upper_hr_threshold;            \
    (i).flash_incr_mode        = (e).flash_incr_mode;               \
    (i).flash_multiple         = (e).flash_multiple;                \
    (i).flash_threshold        = (e).flash_threshold;               \
    (i).decrement              = (e).decrement;                     \
    (i).apply_max_decrement    = (e).apply_max_decrement;           \
    (i).max_decrement          = (e).max_decrement;                 \
    (i).epochs_before_eviction = (int)((e).epochs_before_eviction); \
    (i).apply_empty_reserve    = (e).apply_empty_reserve;           \
    (i).empty_reserve          = (e).empty_reserve;                 \
}

/* Epsilon for floating-point comparisons */
#define FP_EPSILON 0.000001


/* misc type definitions */

struct expected_entry_status
{
    int			entry_type;
    int                 entry_index;
    size_t              size;
    unsigned char	in_cache;
    unsigned char       at_main_addr;
    unsigned char	is_dirty;
    unsigned char	is_protected;
    unsigned char	is_pinned;
    unsigned char	loaded;
    unsigned char	cleared;
    unsigned char	flushed;
    unsigned char	destroyed;
};




/* global variable externs: */

extern const char *FILENAME[3];

extern hbool_t write_permitted;
extern hbool_t pass; /* set to false on error */
extern hbool_t skip_long_tests;
extern hbool_t run_full_test;
extern const char *failure_mssg;

extern test_entry_t * entries[NUMBER_OF_ENTRY_TYPES];
extern const int32_t max_indices[NUMBER_OF_ENTRY_TYPES];
extern const size_t entry_sizes[NUMBER_OF_ENTRY_TYPES];
extern const haddr_t base_addrs[NUMBER_OF_ENTRY_TYPES];
extern const haddr_t alt_base_addrs[NUMBER_OF_ENTRY_TYPES];
extern const char * entry_type_names[NUMBER_OF_ENTRY_TYPES];


/* call back function declarations: */

herr_t check_write_permitted(const H5F_t * f,
                             hid_t dxpl_id,
                             hbool_t * write_permitted_ptr);

/* callback table extern */

extern const H5C_class_t types[NUMBER_OF_ENTRY_TYPES];


/* function declarations: */

void add_flush_op(int target_type,
                  int target_idx,
                  int op_code,
                  int type,
                  int idx,
                  hbool_t flag,
                  size_t size);


void addr_to_type_and_index(haddr_t addr,
                            int32_t * type_ptr,
                            int32_t * index_ptr);

#if 0 /* keep this for a while -- it may be useful */
haddr_t type_and_index_to_addr(int32_t type,
                               int32_t idx);
#endif

void dirty_entry(H5F_t * file_ptr,
                 int32_t type,
                 int32_t idx,
                 hbool_t dirty_pin);

void expunge_entry(H5F_t * file_ptr,
                   int32_t type,
                   int32_t idx);

void insert_entry(H5F_t * file_ptr,
                  int32_t type,
                  int32_t idx,
                  unsigned int flags);

void mark_entry_dirty(int32_t type,
		      int32_t idx);

void move_entry(H5C_t * cache_ptr,
                int32_t type,
                int32_t idx,
                hbool_t main_addr);

void protect_entry(H5F_t * file_ptr,
                   int32_t type,
                   int32_t idx);

void protect_entry_ro(H5F_t * file_ptr,
                      int32_t type,
                      int32_t idx);

hbool_t entry_in_cache(H5C_t * cache_ptr,
                       int32_t type,
                       int32_t idx);

void create_pinned_entry_dependency(H5F_t * file_ptr,
		                    int pinning_type,
		                    int pinning_idx,
		                    int pinned_type,
		                    int pinned_idx);

void reset_entries(void);

void resize_entry(H5F_t * file_ptr,
                  int32_t type,
                  int32_t idx,
                  size_t new_size,
                  hbool_t in_cache);

H5F_t *setup_cache(size_t max_cache_size, size_t min_clean_size);

void row_major_scan_forward(H5F_t * file_ptr,
                            int32_t lag,
                            hbool_t verbose,
                            hbool_t reset_stats,
                            hbool_t display_stats,
                            hbool_t display_detailed_stats,
                            hbool_t do_inserts,
                            hbool_t do_moves,
                            hbool_t move_to_main_addr,
                            hbool_t do_destroys,
                            hbool_t do_mult_ro_protects,
                            int dirty_destroys,
                            int dirty_unprotects);

void hl_row_major_scan_forward(H5F_t * file_ptr,
                               int32_t max_index,
                               hbool_t verbose,
                               hbool_t reset_stats,
                               hbool_t display_stats,
                               hbool_t display_detailed_stats,
                               hbool_t do_inserts);

void row_major_scan_backward(H5F_t * file_ptr,
                             int32_t lag,
                             hbool_t verbose,
                             hbool_t reset_stats,
                             hbool_t display_stats,
                             hbool_t display_detailed_stats,
                             hbool_t do_inserts,
                             hbool_t do_moves,
                             hbool_t move_to_main_addr,
                             hbool_t do_destroys,
                             hbool_t do_mult_ro_protects,
                             int dirty_destroys,
                             int dirty_unprotects);

void hl_row_major_scan_backward(H5F_t * file_ptr,
                                int32_t max_index,
                                hbool_t verbose,
                                hbool_t reset_stats,
                                hbool_t display_stats,
                                hbool_t display_detailed_stats,
                                hbool_t do_inserts);

void col_major_scan_forward(H5F_t * file_ptr,
                            int32_t lag,
                            hbool_t verbose,
                            hbool_t reset_stats,
                            hbool_t display_stats,
                            hbool_t display_detailed_stats,
                            hbool_t do_inserts,
                            int dirty_unprotects);

void hl_col_major_scan_forward(H5F_t * file_ptr,
                               int32_t max_index,
                               hbool_t verbose,
                               hbool_t reset_stats,
                               hbool_t display_stats,
                               hbool_t display_detailed_stats,
                               hbool_t do_inserts,
                               int dirty_unprotects);

void col_major_scan_backward(H5F_t * file_ptr,
                             int32_t lag,
                             hbool_t verbose,
                             hbool_t reset_stats,
                             hbool_t display_stats,
                             hbool_t display_detailed_stats,
                             hbool_t do_inserts,
                             int dirty_unprotects);

void hl_col_major_scan_backward(H5F_t * file_ptr,
                                int32_t max_index,
                                hbool_t verbose,
                                hbool_t reset_stats,
                                hbool_t display_stats,
                                hbool_t display_detailed_stats,
                                hbool_t do_inserts,
                                int dirty_unprotects);

void takedown_cache(H5F_t * file_ptr,
                    hbool_t dump_stats,
                    hbool_t dump_detailed_stats);

void flush_cache(H5F_t * file_ptr,
                 hbool_t destroy_entries,
                 hbool_t dump_stats,
                 hbool_t dump_detailed_stats);

void unpin_entry(int32_t type,
                 int32_t idx);

void unprotect_entry(H5F_t * file_ptr,
                     int32_t type,
                     int32_t idx,
                     unsigned int flags);

void verify_clean(void);

void verify_entry_status(H5C_t * cache_ptr,
		         int tag,
                         int num_entries,
                         struct expected_entry_status expected[]);

void verify_unprotected(void);

/*** H5AC level utility functions ***/

void check_and_validate_cache_hit_rate(hid_t file_id,
                                       double * hit_rate_ptr,
                                       hbool_t dump_data,
                                       int64_t min_accesses,
                                       double min_hit_rate);

void check_and_validate_cache_size(hid_t file_id,
                                   size_t * max_size_ptr,
                                   size_t * min_clean_size_ptr,
                                   size_t * cur_size_ptr,
                                   int32_t * cur_num_entries_ptr,
                                   hbool_t dump_data);

void validate_mdc_config(hid_t file_id,
                         H5AC_cache_config_t * ext_config_ptr,
                         hbool_t compare_init,
                         int test_num);

#endif /* _CACHE_COMMON_H */

