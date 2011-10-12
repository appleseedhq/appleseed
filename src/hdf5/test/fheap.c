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

/* Programmer:  Quincey Koziol <koziol@ncsa.uiuc.edu>
 *              Friday, February 24, 2006
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5HF package.
 * This file also needs to access the fractal heap testing code.
 */
#define H5HF_PACKAGE
#define H5HF_TESTING
#include "H5HFpkg.h"		/* Fractal heaps			*/

/* Other private headers that this test requires */
#include "H5Iprivate.h"
#include "H5MMprivate.h"	/* Memory management			*/
#include "H5Vprivate.h"		/* Vectors and arrays 			*/

/* Max. testfile name length */
#define FHEAP_FILENAME_LEN      1024

/* Object size macros */
#define SMALL_OBJ_SIZE1         10
#define SMALL_OBJ_SIZE2         20
#define NUM_FILL_OBJS           11

/* "Small" heap creation parameters */
#define SMALL_DBLOCK_OVERHEAD 21                /* Overhead for direct blocks */
#define SMALL_CHECKSUM_DBLOCKS TRUE             /* Whether to checksum direct blocks */
#define SMALL_MAN_WIDTH   4                     /* Managed obj. table width */
#define SMALL_MAN_START_BLOCK_SIZE 512          /* Managed obj. starting block size */
#define SMALL_MAN_MAX_DIRECT_SIZE (64 * 1024)   /* Managed obj. max. direct block size */
#define SMALL_MAN_MAX_INDEX 32                  /* Managed obj. # of bits for total heap size */
#define SMALL_MAN_START_ROOT_ROWS 1             /* Managed obj. starting # of root indirect block rows */
#define SMALL_ID_LEN 0                          /* "Default" heap ID length */
#define SMALL_STAND_SIZE  (SMALL_MAN_MAX_DIRECT_SIZE - SMALL_DBLOCK_OVERHEAD)           /* Standalone obj. min. size */

/* "Large" heap creation parameters */
#define LARGE_DBLOCK_OVERHEAD 21                /* Overhead for direct blocks */
                                                /* (coincidentally the same size as for small direct blocks) */
#define LARGE_CHECKSUM_DBLOCKS FALSE            /* Whether to checksum direct blocks */
#define LARGE_MAN_WIDTH  32                     /* Managed obj. table width */
#define LARGE_MAN_START_BLOCK_SIZE 4096         /* Managed obj. starting block size */
#define LARGE_MAN_MAX_DIRECT_SIZE (1024 * 1024) /* Managed obj. max. direct block size */
#define LARGE_MAN_MAX_INDEX 64                  /* Managed obj. # of bits for total heap size */
#define LARGE_MAN_START_ROOT_ROWS 1             /* Managed obj. starting # of root indirect block rows */
#define LARGE_ID_LEN 0                          /* "Default" heap ID length */
#define LARGE_STAND_SIZE  (LARGE_MAN_MAX_DIRECT_SIZE - LARGE_DBLOCK_OVERHEAD)           /* Standalone obj. min. size */

/* Define this macro to enable all insertion tests */
/* #define ALL_INSERT_TESTS */

/* Heap metadata macros */
#define MAX_HEAP_ID_LEN        64               /* Max. # of bytes to use for heap ID */
#define HEAP_ID_LEN             7               /* # of bytes to use for heap ID */
#define SMALL_HEAP_ID_LEN       7               /* # of bytes to use for "small" heap's IDs */
#define LARGE_HEAP_ID_LEN      12               /* # of bytes to use for "large" heap's IDs */
#define HEAP_MAX_ROOT_ROWS(fh) H5HF_get_max_root_rows(fh)       /* Max. # of rows in root indirect block */
#define DTABLE_WIDTH(fh) H5HF_get_dtable_width_test(fh) /* Width of doubling table for heap */
#define DTABLE_MAX_DROWS(fh) H5HF_get_dtable_max_drows_test(fh) /* Max. # of direct block rows in any indirect block */
#define IBLOCK_MAX_DROWS(fh, pos) H5HF_get_iblock_max_drows_test(fh, pos) /* Max. # of direct block rows in a indirect block */
#define DBLOCK_SIZE(fh, r) H5HF_get_dblock_size_test(fh, r)     /* Size of a direct block in a given row */
#define DBLOCK_FREE(fh, r) H5HF_get_dblock_free_test(fh, r)     /* Free space in a direct block of a given row */

const char *FILENAME[] = {
    "fheap",
    NULL
};

/* Types of tests to perform */
typedef enum {
    FHEAP_TEST_NORMAL,          /* "Normal" test, with no testing parameters set */
    FHEAP_TEST_REOPEN,          /* Set the reopen_heap flag */
    FHEAP_TEST_NTESTS           /* The number of test types, must be last */
} fheap_test_type_t;

/* Order to delete objects */
typedef enum {
    FHEAP_DEL_FORWARD,          /* Delete objects from 0 -> nobjs */
    FHEAP_DEL_REVERSE,          /* Delete objects from nobjs -> 0 */
    FHEAP_DEL_HEAP,             /* Delete entire heap at once */
    FHEAP_DEL_NDIRS             /* The number of different deletion orders, must be last */
} fheap_test_del_dir_t;

/* Order to delete objects */
typedef enum {
    FHEAP_DEL_DRAIN_ALL,        /* Don't drain half of objects first */
    FHEAP_DEL_DRAIN_HALF,       /* Don't drain half of objects first */
    FHEAP_DEL_DRAIN_N           /* The number of different ways to drain, must be last */
} fheap_test_del_drain_t;

/* Size of objects for "bulk" filling heap blocks */
typedef enum {
    FHEAP_TEST_FILL_LARGE,      /* Fill heap blocks with "large" objects */
    FHEAP_TEST_FILL_SINGLE,     /* Fill heap blocks with single object */
    FHEAP_TEST_FILL_N           /* The number of different ways to test filling heap blocks, must be last */
} fheap_test_fill_t;

/* Whether to compress blocks (during random tests) */
typedef enum {
    FHEAP_TEST_NO_COMPRESS,     /* Don't compress direct blocks */
    FHEAP_TEST_COMPRESS,        /* Compress direct blocks */
    FHEAP_TEST_COMP_N           /* The number of different ways to test compressing heap blocks, must be last */
} fheap_test_comp_t;

/* Testing parameters */
typedef struct fheap_test_param_t {
    fheap_test_type_t reopen_heap;      /* Whether to re-open the heap during the test */
    fheap_test_del_dir_t del_dir;       /* Whether to delete objects forward or reverse */
    fheap_test_del_drain_t drain_half;  /* Whether to drain half of the objects & refill, when deleting objects */
    fheap_test_fill_t fill;             /* How to "bulk" fill heap blocks */
    size_t actual_id_len;               /* The actual length of heap IDs for a test */
    fheap_test_comp_t comp;             /* Whether to compressed the blocks or not */
} fheap_test_param_t;

/* Heap state information */
typedef struct fheap_heap_state_t {
    size_t      man_nobjs;              /* # of managed objects within heap */
    hsize_t     man_size;               /* Size of managed object heap */
    hsize_t     man_alloc_size;         /* Size of managed object heap allocated */
    hsize_t     man_free_space;         /* Managed object free space within heap */
    size_t      huge_nobjs;             /* # of 'huge' objects within heap */
    hsize_t     huge_size;              /* Size of 'huge' object heap */
    size_t      tiny_nobjs;             /* # of 'tiny' objects within heap */
    hsize_t     tiny_size;              /* Size of 'tiny' object heap */
} fheap_heap_state_t;

/* Heap IDs to retain */
typedef struct fheap_heap_ids_t {
    size_t      num_ids;        /* # of heap IDs in array */
    size_t      alloc_ids;      /* # of heap IDs allocated in array */
    unsigned char *ids;         /* Array of object heap IDs */
    size_t *lens;               /* Array of object lengths */
    size_t *offs;               /* Array of object offsets (in global shared write buffer) */
} fheap_heap_ids_t;

/* Local variables */
unsigned char *shared_wobj_g;   /* Pointer to shared write buffer for objects */
unsigned char *shared_robj_g;   /* Pointer to shared read buffer for objects */
size_t shared_obj_size_g;       /* Size of shared objects */
unsigned char *shared_ids_g = NULL;     /* Array of shared object heap IDs */
size_t *shared_lens_g = NULL;   /* Array of shared object lengths */
size_t *shared_offs_g = NULL;   /* Array of shared object offsets */
size_t shared_alloc_ids_g = 0;  /* # of shared heap IDs allocated in array */

/* Local routines */
static int init_small_cparam(H5HF_create_t *cparam);
static int init_large_cparam(H5HF_create_t *cparam);
static int check_stats(const H5HF_t *fh, const fheap_heap_state_t *state);
static int del_objs(H5F_t *f, hid_t dxpl, H5HF_t **fh, fheap_test_param_t *tparam,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids);


/*-------------------------------------------------------------------------
 * Function:	init_small_cparam
 *
 * Purpose:	Initialize heap creation parameter structure with small
 *              settings
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
init_small_cparam(H5HF_create_t *cparam)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(H5HF_create_t));

    /* General parameters */
    cparam->id_len = SMALL_ID_LEN;
    cparam->max_man_size = SMALL_STAND_SIZE;
    cparam->checksum_dblocks = SMALL_CHECKSUM_DBLOCKS;

    /* Managed object doubling-table parameters */
    cparam->managed.width = SMALL_MAN_WIDTH;
    cparam->managed.start_block_size = SMALL_MAN_START_BLOCK_SIZE;
    cparam->managed.max_direct_size = SMALL_MAN_MAX_DIRECT_SIZE;
    cparam->managed.max_index = SMALL_MAN_MAX_INDEX;
    cparam->managed.start_root_rows = SMALL_MAN_START_ROOT_ROWS;

    return(0);
} /* init_small_cparam() */


/*-------------------------------------------------------------------------
 * Function:	init_large_cparam
 *
 * Purpose:	Initialize heap creation parameter structure with large
 *              settings
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
init_large_cparam(H5HF_create_t *cparam)
{
    /* Wipe out background */
    HDmemset(cparam, 0, sizeof(H5HF_create_t));

    /* General parameters */
    cparam->id_len = LARGE_ID_LEN;
    cparam->max_man_size = LARGE_STAND_SIZE;
    cparam->checksum_dblocks = LARGE_CHECKSUM_DBLOCKS;

    /* Managed object doubling-table parameters */
    cparam->managed.width = LARGE_MAN_WIDTH;
    cparam->managed.start_block_size = LARGE_MAN_START_BLOCK_SIZE;
    cparam->managed.max_direct_size = LARGE_MAN_MAX_DIRECT_SIZE;
    cparam->managed.max_index = LARGE_MAN_MAX_INDEX;
    cparam->managed.start_root_rows = LARGE_MAN_START_ROOT_ROWS;

    return(0);
} /* init_large_cparam() */


/*-------------------------------------------------------------------------
 * Function:	check_stats
 *
 * Purpose:	Verify stats for a heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
check_stats(const H5HF_t *fh, const fheap_heap_state_t *state)
{
    H5HF_stat_t heap_stats;             /* Statistics about the heap */

    /* Get statistics for heap and verify they are correct */
    if(H5HF_stat_info(fh, &heap_stats) < 0)
        FAIL_STACK_ERROR
    if(heap_stats.man_nobjs != state->man_nobjs) {
        HDfprintf(stdout, "heap_stats.man_nobjs = %Hu, state->man_nobjs = %Zu\n", heap_stats.man_nobjs, state->man_nobjs);
        TEST_ERROR
    } /* end if */
    if(heap_stats.man_size != state->man_size) {
        HDfprintf(stdout, "heap_stats.man_size = %Hu, state->man_size = %Hu\n", heap_stats.man_size, state->man_size);
        TEST_ERROR
    } /* end if */
    if(heap_stats.man_alloc_size != state->man_alloc_size) {
        HDfprintf(stdout, "heap_stats.man_alloc_size = %Hu, state->man_alloc_size = %Hu\n", heap_stats.man_alloc_size, state->man_alloc_size);
        TEST_ERROR
    } /* end if */
    if(heap_stats.man_free_space != state->man_free_space) {
        HDfprintf(stdout, "heap_stats.man_free_space = %Hu, state->man_free_space = %Hu\n", heap_stats.man_free_space, state->man_free_space);
        TEST_ERROR
    } /* end if */
    if(heap_stats.huge_nobjs != state->huge_nobjs) {
        HDfprintf(stdout, "heap_stats.huge_nobjs = %Hu, state->huge_nobjs = %Zu\n", heap_stats.huge_nobjs, state->huge_nobjs);
        TEST_ERROR
    } /* end if */
    if(heap_stats.huge_size != state->huge_size) {
        HDfprintf(stdout, "heap_stats.huge_size = %Hu, state->huge_size = %Hu\n", heap_stats.huge_size, state->huge_size);
        TEST_ERROR
    } /* end if */
    if(heap_stats.tiny_nobjs != state->tiny_nobjs) {
        HDfprintf(stdout, "heap_stats.tiny_nobjs = %Hu, state->tiny_nobjs = %Zu\n", heap_stats.tiny_nobjs, state->tiny_nobjs);
        TEST_ERROR
    } /* end if */
    if(heap_stats.tiny_size != state->tiny_size) {
        HDfprintf(stdout, "heap_stats.tiny_size = %Hu, state->tiny_size = %Hu\n", heap_stats.tiny_size, state->tiny_size);
        TEST_ERROR
    } /* end if */

    /* All tests passed */
    return(0);

error:
    return(1);
} /* check_stats() */


/*-------------------------------------------------------------------------
 * Function:	op_memcpy
 *
 * Purpose:	Perform 'memcpy' for an object
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, September 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static herr_t
op_memcpy(const void *obj, size_t obj_len, void *op_data)
{
    /* Make copy of the object */
    HDmemcpy(op_data, obj, obj_len);

    return(SUCCEED);
} /* op_memcpy() */


/*-------------------------------------------------------------------------
 * Function:	add_obj
 *
 * Purpose:	Add an object to heap
 *
 * Note:        The following fields in the 'state' structure are set to
 *              the values expected _after_ any block created for the object:
 *                      man_size
 *                      man_alloc_size
 *                      man_free_space
 *
 *              The following fields in the 'state' structure are set to
 *              the current state, before any block has been created:
 *                      nobjs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
add_obj(H5HF_t *fh, hid_t dxpl, size_t obj_off,
    size_t obj_size, fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned char heap_id[MAX_HEAP_ID_LEN]; /* Heap ID for object inserted */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t id_len;                      /* Size of fractal heap IDs */
    size_t robj_size;                   /* Object size read in */

    /* Sanity check */
    HDassert(fh);

    /* Initialize object buffer */
    obj = &shared_wobj_g[obj_off];

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > MAX_HEAP_ID_LEN)
        TEST_ERROR

    /* Insert object */
    HDmemset(heap_id, 0, id_len);
    if(H5HF_insert(fh, dxpl, obj_size, obj, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for tracking the heap's state */
    if(state) {
        size_t      tiny_max_len;           /* Max. length of tiny objects */
        hbool_t     tiny_len_extended;      /* Do tiny objects use two bytes for the length? */

        /* Check information about tiny objects */
        if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
            FAIL_STACK_ERROR

        /* Adjust state of heap */
        if(obj_size <= tiny_max_len) {
            state->tiny_nobjs++;
            state->tiny_size += obj_size;
        } /* end if */
        else {
            state->man_nobjs++;
            state->man_free_space -= obj_size;
        } /* end else */

        /* Check free space left in heap */
        if(check_stats(fh, state))
            TEST_ERROR
    } /* end if */

    /* Read in object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(obj, shared_robj_g, obj_size))
        TEST_ERROR

    /* If the heap IDs are to be retained, append them to the list */
    if(keep_ids) {
        /* Check for needing to increase size of heap ID array */
        if(keep_ids->num_ids + 1 > keep_ids->alloc_ids) {
            keep_ids->alloc_ids = MAX(1024, (keep_ids->alloc_ids * 2));
            if(NULL == (keep_ids->ids = H5MM_realloc(keep_ids->ids, id_len * keep_ids->alloc_ids)))
                TEST_ERROR
            if(NULL == (keep_ids->lens = H5MM_realloc(keep_ids->lens, sizeof(size_t) * keep_ids->alloc_ids)))
                TEST_ERROR
            if(NULL == (keep_ids->offs = H5MM_realloc(keep_ids->offs, sizeof(size_t) * keep_ids->alloc_ids)))
                TEST_ERROR
        } /* end if */

        /* Append the object info onto the array */
        HDmemcpy(&keep_ids->ids[keep_ids->num_ids * id_len], heap_id, id_len);
        keep_ids->lens[keep_ids->num_ids] = obj_size;
        keep_ids->offs[keep_ids->num_ids] = obj_off;

        /* Increment the number of IDs kept */
        keep_ids->num_ids++;
    } /* end if */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* add_obj() */


/*-------------------------------------------------------------------------
 * Function:	get_del_string
 *
 * Purpose:	Return string describing the kind of deletion to perform
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static char *
get_del_string(const fheap_test_param_t *tparam)
{
    char *str;

    /* Remove half of total objects from heap */
    if(tparam->del_dir == FHEAP_DEL_FORWARD)
        if(tparam->drain_half == FHEAP_DEL_DRAIN_ALL)
            str = HDstrdup("(all - forward)");
        else
            str = HDstrdup("(half, refill, all - forward)");
    else if(tparam->del_dir == FHEAP_DEL_REVERSE)
        if(tparam->drain_half == FHEAP_DEL_DRAIN_ALL)
            str = HDstrdup("(all - reverse)");
        else
            str = HDstrdup("(half, refill, all - reverse)");
    else
        str = HDstrdup("(all - deleting heap)");

    return(str);
} /* get_del_string() */


/*-------------------------------------------------------------------------
 * Function:	get_fill_size
 *
 * Purpose:	Retrieve the size of objects to "bulk" fill blocks with
 *
 * Return:	Size of object to pass down to "fill_heap" routine on
 *              success/can't fail
 *
 * Programmer:	Quincey Koziol
 *              Thursday, July 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static size_t
get_fill_size(const fheap_test_param_t *tparam)
{
    switch(tparam->fill) {
        case FHEAP_TEST_FILL_LARGE:
            return((size_t)(-1));

        case FHEAP_TEST_FILL_SINGLE:
            return((size_t)0);

        default:
            HDassert(0 && "Unknown bulk fill type?!?");
    } /* end switch */

    return(0);
} /* get_fill_size() */


/*-------------------------------------------------------------------------
 * Function:	begin_test
 *
 * Purpose:	Perform common "test being" operations
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
begin_test(fheap_test_param_t *tparam, const char *base_desc,
    fheap_heap_ids_t *keep_ids, size_t *fill_size)
{
    char *del_str = NULL;               /* Deletion order description */
    char *test_desc = NULL;             /* Test description */

    /*
     * Test filling & removing all (small) objects from root direct block of absolute heap
     */
    del_str = get_del_string(tparam);
    HDassert(del_str);
    test_desc = H5MM_malloc(HDstrlen(del_str) + HDstrlen(base_desc));
    sprintf(test_desc, base_desc, del_str);
    TESTING(test_desc);
    H5MM_xfree(del_str);
    H5MM_xfree(test_desc);

    /* Initialize the heap ID structure */
    HDmemset(keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Retrieve "bulk" filling object size */
    if(fill_size)
        *fill_size = get_fill_size(tparam);

    /* Success */
    return(0);
} /* end begin_test() */


/*-------------------------------------------------------------------------
 * Function:	reopen_file
 *
 * Purpose:	Perform common "re-open" operations on file & heap for testing
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_file(hid_t *file, H5F_t **f, const char *filename, hid_t fapl, hid_t dxpl,
    H5HF_t **fh, haddr_t fh_addr, const fheap_test_param_t *tparam)
{
    /* Check for closing & re-opening the heap */
    /* (actually will close & re-open the file as well) */
    if(tparam->reopen_heap) {
        /* Close heap */
        if(H5HF_close(*fh, dxpl) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(*file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((*file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (*f = H5I_object(*file)))
            FAIL_STACK_ERROR

        /* Re-open heap */
        if(NULL == (*fh = H5HF_open(*f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Success */
    return(0);

error:
    return(-1);
} /* end reopen_file() */


/*-------------------------------------------------------------------------
 * Function:	open_heap
 *
 * Purpose:	Perform common "open" operations on file & heap for testing
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
open_heap(char *filename, hid_t fapl, hid_t dxpl, const H5HF_create_t *cparam,
    const fheap_test_param_t *tparam, hid_t *file, H5F_t **f, H5HF_t **fh,
    haddr_t *fh_addr, fheap_heap_state_t *state, h5_stat_size_t *empty_size)
{
    size_t      id_len;                 /* Size of fractal heap IDs */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, (size_t)FHEAP_FILENAME_LEN);

    /* Create the file to work on */
    if((*file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Check for deleting the entire heap */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Get a pointer to the internal file object */
        if(NULL == (*f = H5I_object(*file)))
            FAIL_STACK_ERROR

        /* Create absolute heap */
        if(NULL == (*fh = H5HF_create(*f, dxpl, cparam)))
            FAIL_STACK_ERROR
        if(H5HF_get_id_len(*fh, &id_len) < 0)
            FAIL_STACK_ERROR
        if(id_len > tparam->actual_id_len)
            TEST_ERROR
        if(H5HF_get_heap_addr(*fh, fh_addr) < 0)
            FAIL_STACK_ERROR
        if(!H5F_addr_defined(*fh_addr))
            TEST_ERROR
        HDmemset(state, 0, sizeof(fheap_heap_state_t));
        if(check_stats(*fh, state))
            TEST_ERROR

        /* Prepare for querying the size of a file with an empty heap */

        /* Close (empty) heap */
        if(H5HF_close(*fh, dxpl) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close file */
    if(H5Fclose(*file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((*empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((*file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (*f = H5I_object(*file)))
        FAIL_STACK_ERROR

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Create absolute heap */
        if(NULL == (*fh = H5HF_create(*f, dxpl, cparam)))
            FAIL_STACK_ERROR
        if(H5HF_get_id_len(*fh, &id_len) < 0)
            FAIL_STACK_ERROR
        if(id_len > tparam->actual_id_len)
            TEST_ERROR
        if(H5HF_get_heap_addr(*fh, fh_addr) < 0)
            FAIL_STACK_ERROR
        if(!H5F_addr_defined(*fh_addr))
            TEST_ERROR
        HDmemset(state, 0, sizeof(fheap_heap_state_t));
        if(check_stats(*fh, state))
            TEST_ERROR
    } /* end if */
    else {
        /* Re-open heap */
        if(NULL == (*fh = H5HF_open(*f, dxpl, *fh_addr)))
            FAIL_STACK_ERROR
    } /* end else */

    /* Success */
    return(0);

error:
    return(-1);
} /* end open_heap() */


/*-------------------------------------------------------------------------
 * Function:	reopen_heap
 *
 * Purpose:	Perform common "re-open" operations on heap for testing
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
reopen_heap(H5F_t *f, hid_t dxpl, H5HF_t **fh, haddr_t fh_addr,
    const fheap_test_param_t *tparam)
{
    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        /* Close (empty) heap */
        if(H5HF_close(*fh, dxpl) < 0)
            FAIL_STACK_ERROR

        /* Re-open heap */
        if(NULL == (*fh = H5HF_open(f, dxpl, fh_addr)))
            FAIL_STACK_ERROR
    } /* end if */

    /* Success */
    return(0);

error:
    return(-1);
} /* end reopen_heap() */


/*-------------------------------------------------------------------------
 * Function:	close_heap
 *
 * Purpose:	Perform common "close" operations on file & heap for testing
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August  4, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
close_heap(char *filename, hid_t fapl, hid_t dxpl, fheap_test_param_t *tparam,
    hid_t file, H5F_t *f, H5HF_t **fh, haddr_t fh_addr,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids,
    h5_stat_size_t empty_size)
{
    h5_stat_size_t       file_size;              /* Size of file currently */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    if(check_stats(*fh, state))
        TEST_ERROR

    /* Check for deleting the objects in the heap */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Delete objects inserted (either forward or reverse order) */
        if(del_objs(f, dxpl, fh, tparam, state, keep_ids))
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the fractal heap */
    if(H5HF_close(*fh, dxpl) < 0)
        FAIL_STACK_ERROR
    *fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Success */
    return(0);

error:
    return(-1);
} /* end close_heap() */


/*-------------------------------------------------------------------------
 * Function:	del_objs_half_refill
 *
 * Purpose:	Remove half of objects from heap and refill
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
del_objs_half_refill(H5F_t *f, hid_t dxpl, H5HF_t **fh, fheap_test_param_t *tparam,
    fheap_heap_ids_t *keep_ids)
{
    unsigned char *wobj;        /* Buffer for object to insert */
    haddr_t fh_addr = HADDR_UNDEF;      /* Address of fractal heap */
    size_t id_len;              /* Size of fractal heap IDs */
    size_t half_nobjs;          /* Half of total # of objects */
    size_t obj_idx;             /* Index of the object to remove */
    size_t u;                   /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(*fh);
    HDassert(keep_ids);

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        if(H5HF_get_heap_addr(*fh, &fh_addr) < 0)
            FAIL_STACK_ERROR
        if(!H5F_addr_defined(fh_addr))
            TEST_ERROR
    } /* end if */

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(*fh, &id_len) < 0)
        FAIL_STACK_ERROR

    /* Remove half of total objects from heap */
    if(tparam->del_dir == FHEAP_DEL_FORWARD)
        obj_idx = 0;
    else
        obj_idx = keep_ids->num_ids - 1;
    half_nobjs = keep_ids->num_ids / 2;
    for(u = 0; u < half_nobjs; u++) {
        /* Remove object from heap */
        if(H5HF_remove(*fh, dxpl, &keep_ids->ids[id_len * obj_idx]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Adjust index of object to delete next */
        if(tparam->del_dir == FHEAP_DEL_FORWARD)
            obj_idx++;
        else
            obj_idx--;
    } /* end for */

    /* Re-insert half of total objects back into heap */
    if(tparam->del_dir == FHEAP_DEL_FORWARD)
        obj_idx = 0;
    else
        obj_idx = keep_ids->num_ids - 1;
    for(u = 0; u < half_nobjs; u++) {
        /* Re-insert object */
        wobj = &shared_wobj_g[keep_ids->offs[obj_idx]];
        if(H5HF_insert(*fh, dxpl, keep_ids->lens[obj_idx], wobj, &keep_ids->ids[id_len * obj_idx]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Adjust index of object to delete next */
        if(tparam->del_dir == FHEAP_DEL_FORWARD)
            obj_idx++;
        else
            obj_idx--;
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* del_objs_half_refill() */


/*-------------------------------------------------------------------------
 * Function:	del_objs
 *
 * Purpose:	Remove objects from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
del_objs(H5F_t *f, hid_t dxpl, H5HF_t **fh, fheap_test_param_t *tparam,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    haddr_t fh_addr = HADDR_UNDEF;      /* Address of fractal heap */
    size_t id_len;              /* Size of fractal heap IDs */
    size_t obj_idx;             /* Index of the object to remove */
    size_t u;                   /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(*fh);
    HDassert(state);
    HDassert(keep_ids);

    /* Check for first deleting half of objects & then re-inserting them */
    if(tparam->drain_half == FHEAP_DEL_DRAIN_HALF)
        if(del_objs_half_refill(f, dxpl, fh, tparam, keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(tparam->reopen_heap) {
        if(H5HF_get_heap_addr(*fh, &fh_addr) < 0)
            FAIL_STACK_ERROR
        if(!H5F_addr_defined(fh_addr))
            TEST_ERROR
    } /* end if */

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(*fh, &id_len) < 0)
        FAIL_STACK_ERROR

    /* Remove all objects from heap */
    if(tparam->del_dir == FHEAP_DEL_FORWARD)
        obj_idx = 0;
    else
        obj_idx = keep_ids->num_ids - 1;
    for(u = 0; u < keep_ids->num_ids; u++) {
        /* Remove object from heap */
        if(H5HF_remove(*fh, dxpl, &keep_ids->ids[id_len * obj_idx]) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Adjust index of object to delete next */
        if(tparam->del_dir == FHEAP_DEL_FORWARD)
            obj_idx++;
        else
            obj_idx--;
    } /* end for */

    /* Heap should be completely empty now, reset our state */
    HDmemset(state, 0, sizeof(fheap_heap_state_t));

    /* Check up on heap... */
    if(check_stats(*fh, state))
        TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* del_objs() */


/*-------------------------------------------------------------------------
 * Function:	fill_heap
 *
 * Purpose:	Insert (small) objects to fill up the free space in a heap block
 *
 * Note:        The following fields in the 'state' structure are set to
 *              the values expected _after_ the block has been created:
 *                      man_size
 *                      man_alloc_size
 *                      man_free_space
 *
 *              The following fields in the 'state' structure are set to
 *              the current state, before the block has been created:
 *                      nobjs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_heap(H5HF_t *fh, hid_t dxpl, unsigned block_row, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned char *wobj;                /* Buffer for object to insert */
    unsigned char *curr_id_ptr;         /* Pointer into shared ID array */
    size_t *curr_len_ptr;               /* Pointer into shared length array */
    size_t *curr_off_ptr;               /* Pointer into shared offset array */
    size_t      num_ids = 0;            /* # of heap IDs in array */
    size_t      data_size;              /* Size of data portion of heap block */
    size_t      last_obj_len;           /* Size of last object inserted into heap */
    size_t      obj_off;                /* Offset of object in shared write buffer */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);
    HDassert(obj_size + 256 < shared_obj_size_g);

    /* Initialize starting information */
    data_size = (size_t)DBLOCK_FREE(fh, block_row);
    wobj = shared_wobj_g;
    curr_id_ptr = shared_ids_g;
    curr_len_ptr = shared_lens_g;
    curr_off_ptr = shared_offs_g;
    obj_off = 0;

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR

    /* Check for some "magic" object sizes */
    if(obj_size == 0)
        obj_size = data_size;
    else if(obj_size == (size_t)(-1))
        obj_size = (data_size / NUM_FILL_OBJS) + 1;

    /* Loop over inserting objects into the root direct block, until there's no more space */
    while(data_size >= obj_size) {
        /* Increment object count */
        num_ids++;

        /* Check for needing to increase size of heap ID array */
        if(num_ids > shared_alloc_ids_g) {
            shared_alloc_ids_g = MAX(1024, (shared_alloc_ids_g * 2));
            if(NULL == (shared_ids_g = H5MM_realloc(shared_ids_g, id_len * shared_alloc_ids_g)))
                TEST_ERROR
            if(NULL == (shared_lens_g = H5MM_realloc(shared_lens_g, sizeof(size_t) * shared_alloc_ids_g)))
                TEST_ERROR
            if(NULL == (shared_offs_g = H5MM_realloc(shared_offs_g, sizeof(size_t) * shared_alloc_ids_g)))
                TEST_ERROR
            curr_id_ptr = &shared_ids_g[(num_ids - 1) * id_len];
            curr_len_ptr = &shared_lens_g[(num_ids - 1)];
            curr_off_ptr = &shared_offs_g[(num_ids - 1)];
        } /* end if */

        /* Insert object */
        if(H5HF_insert(fh, dxpl, obj_size, wobj, curr_id_ptr) < 0)
            FAIL_STACK_ERROR
        *curr_len_ptr = obj_size;
        *curr_off_ptr = obj_off;

        /* Adjust state of heap */
        state->man_nobjs++;
        state->man_free_space -= obj_size;

        /* Check stats for heap */
        if(check_stats(fh, state))
            TEST_ERROR

        /* Adjust object & ID pointers */
        wobj++;
        obj_off++;
        if(obj_off > 255) {
            wobj = shared_wobj_g;
            obj_off = 0;
        } /* end if */
        curr_id_ptr += id_len;
        curr_len_ptr++;
        curr_off_ptr++;

        /* Decrement space left in block */
        data_size -= obj_size;
    } /* end while */

    /* Check for adding smaller last object to heap block */
    if(data_size > 0) {
        /* Set size of last object in block */
        last_obj_len = data_size;

        /* Increment object count */
        num_ids++;

        /* Check for needing to increase size of heap ID array */
        if(num_ids > shared_alloc_ids_g) {
            shared_alloc_ids_g = MAX(1024, (shared_alloc_ids_g * 2));
            if(NULL == (shared_ids_g = H5MM_realloc(shared_ids_g, id_len * shared_alloc_ids_g)))
                TEST_ERROR
            if(NULL == (shared_lens_g = H5MM_realloc(shared_lens_g, sizeof(size_t) * shared_alloc_ids_g)))
                TEST_ERROR
            if(NULL == (shared_offs_g = H5MM_realloc(shared_offs_g, sizeof(size_t) * shared_alloc_ids_g)))
                TEST_ERROR
            curr_id_ptr = &shared_ids_g[(num_ids - 1) * id_len];
            curr_len_ptr = &shared_lens_g[(num_ids - 1)];
            curr_off_ptr = &shared_offs_g[(num_ids - 1)];
        } /* end if */

        /* Insert last object into the heap, using the remaining free space */
        if(H5HF_insert(fh, dxpl, last_obj_len, wobj, curr_id_ptr) < 0)
            FAIL_STACK_ERROR
        *curr_len_ptr = last_obj_len;
        *curr_off_ptr = obj_off;

        /* Adjust state of heap */
        state->man_nobjs++;
        state->man_free_space -= last_obj_len;

        /* Verify that the heap is full */
        if(check_stats(fh, state))
            TEST_ERROR
    } /* end if */
    else
        last_obj_len = obj_size;     /* Normal sized last object */

    /* Verify reading the objects written out */

    /* Verify all the objects */
    wobj = shared_wobj_g;
    curr_id_ptr = shared_ids_g;
    curr_len_ptr = shared_lens_g;
    curr_off_ptr = shared_offs_g;
    for(u = 0; u < num_ids; u++) {
        /* Read in object */
        if(H5HF_read(fh, dxpl, curr_id_ptr, shared_robj_g) < 0)
            FAIL_STACK_ERROR

        /* Check that object is correct */
        wobj = &shared_wobj_g[*curr_off_ptr];
        if(HDmemcmp(wobj, shared_robj_g, *curr_len_ptr))
            TEST_ERROR

        /* Adjust object & ID pointers */
        curr_id_ptr += id_len;
        curr_len_ptr++;
        curr_off_ptr++;
    } /* end for */

    /* If the heap IDs are to be retained, append them to the list */
    if(keep_ids) {
        /* Check for needing to increase size of heap ID array */
        if(keep_ids->num_ids + num_ids > keep_ids->alloc_ids) {
            keep_ids->alloc_ids = MAX(1024, (keep_ids->alloc_ids * 2));
            if(NULL == (keep_ids->ids = H5MM_realloc(keep_ids->ids, id_len * keep_ids->alloc_ids)))
                TEST_ERROR
            if(NULL == (keep_ids->lens = H5MM_realloc(keep_ids->lens, sizeof(size_t) * keep_ids->alloc_ids)))
                TEST_ERROR
            if(NULL == (keep_ids->offs = H5MM_realloc(keep_ids->offs, sizeof(size_t) * keep_ids->alloc_ids)))
                TEST_ERROR
        } /* end if */

        /* Append the IDs onto the array */
        HDmemcpy(&keep_ids->ids[keep_ids->num_ids * id_len], shared_ids_g, (num_ids * id_len));
        HDmemcpy(&keep_ids->lens[keep_ids->num_ids], shared_lens_g, (num_ids * sizeof(size_t)));
        HDmemcpy(&keep_ids->offs[keep_ids->num_ids], shared_offs_g, (num_ids * sizeof(size_t)));

        /* Increment the number of IDs kept */
        keep_ids->num_ids += num_ids;
    } /* end if */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_heap() */


/*-------------------------------------------------------------------------
 * Function:	fill_root_row
 *
 * Purpose:	Fill up a row of direct blocks in the root indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_root_row(H5HF_t *fh, hid_t dxpl, unsigned row, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    hsize_t     first_free_space;       /* Size of free space in heap after the first block */
    hsize_t     all_free_space;         /* Size of free space in heap after all blocks */
    hsize_t     first_heap_size;        /* Total size of the heap after the first block */
    hsize_t     all_heap_size;          /* Total size of the heap after all blocks */
    size_t      block_size;             /* Block size for row */
    size_t      block_free;             /* Free space in empty block of this row */
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    expand_rows;            /* # of rows to expand heap by */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);

    /* Get some information for the heap */
    block_size = (size_t)DBLOCK_SIZE(fh, row);
    block_free = (size_t)DBLOCK_FREE(fh, row);
    width = DTABLE_WIDTH(fh);

    /* Compute the number of rows to expand heap by */
    if(row < 2)
        expand_rows = 1;
    else if(POWER_OF_TWO(row))
        expand_rows = row;
    else
        expand_rows = 0;

    /* Compute first block & all blocks heap size & free space */
    if(state->man_size == 0) {
        first_heap_size = block_size;
        first_free_space = block_free;
        all_heap_size = width * block_size;
        all_free_space = (width - 1) * block_free;
    } /* end if */
    else if(expand_rows == 0) {
        all_heap_size = state->man_size;
        all_free_space = state->man_free_space;
        first_heap_size = all_heap_size;
        first_free_space = all_free_space;
        all_free_space -= block_free;      /* Account for shift from first free space */
    } /* end if */
    else {
        all_heap_size = state->man_size;
        all_free_space = 0;
        for(u = 0; u < expand_rows; u++) {
            all_heap_size += width * DBLOCK_SIZE(fh, row + u);
            all_free_space += width * DBLOCK_FREE(fh, row + u);
        } /* end for */
        first_heap_size = all_heap_size;
        first_free_space = all_free_space;
        all_free_space -= block_free;      /* Account for shift from first free space */
    } /* end else */

    /* Loop over filling direct blocks, until root indirect row is full */
    state->man_size = first_heap_size;
    state->man_free_space = first_free_space;
    for(u = 0; u < width; u++) {
        /* Set heap's size & free space correctly */
        if(u == 1) {
            state->man_size = all_heap_size;
            state->man_free_space = all_free_space;
        } /* end if */

        /* Account for new block added */
        state->man_alloc_size += block_size;

        /* Fill a direct heap block up */
        if(fill_heap(fh, dxpl, row, obj_size, state, keep_ids))
            TEST_ERROR
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_root_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_partial row
 *
 * Purpose:	Fill up part of a row of direct blocks in an non-root indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_partial_row(H5HF_t *fh, hid_t dxpl, unsigned row, unsigned width,
    size_t obj_size, fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    size_t      block_size;             /* Size of direct block in this row */
    unsigned    u;                      /* Local index variable */

    /* Sanity check */
    HDassert(fh);
    HDassert(state);

    /* Get some information for the heap */
    block_size = (size_t)DBLOCK_SIZE(fh, row);

    /* Loop over filling direct blocks, until indirect row is full */
    for(u = 0; u < width; u++) {
        /* Adjust stats for new block */
        state->man_alloc_size += block_size;

        /* Fill a direct heap block up */
        if(fill_heap(fh, dxpl, row, obj_size, state, keep_ids))
            TEST_ERROR
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_partial_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_row
 *
 * Purpose:	Fill up entire row of direct blocks in an non-root indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_row(H5HF_t *fh, hid_t dxpl, unsigned row, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    /* Sanity check */
    HDassert(fh);
    HDassert(state);

    /* Fill the entire row (with the partial row fill routine) */
    if(fill_partial_row(fh, dxpl, row, DTABLE_WIDTH(fh), obj_size, state, keep_ids))
        TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_root_direct
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in the root indirect block
 *              (Generally used to create & fill up direct blocks in a new
 *              indirect block)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_root_direct(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    max_dblock_rows;        /* Max. # of direct block rows in indirect block */
    unsigned    row;                    /* Row being created */

    /* Get heap info */
    max_dblock_rows = DTABLE_MAX_DROWS(fh);
    HDassert(max_dblock_rows);

    /* Loop over rows */
    for(row = 0; row < max_dblock_rows; row++)
        if(fill_root_row(fh, dxpl, row, obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	fill_2nd_indirect
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a second-level indirect block (which only has
 *              direct blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_2nd_indirect(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    max_dblock_rows;        /* Max. # of direct block rows in indirect block */
    unsigned    row;                    /* Current row to create */

    /* Get some information for the heap */
    max_dblock_rows = IBLOCK_MAX_DROWS(fh, pos);
    HDassert(max_dblock_rows);

    /* Loop over rows */
    for(row = 0; row < max_dblock_rows; row++)
        if(fill_row(fh, dxpl, row, obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_2nd_direct() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_direct
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks up to the maximum direct block size
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_direct(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    max_dblock_rows;        /* Max. # of direct block rows in indirect block */
    unsigned    row;                    /* Row being created */

    /* Get heap info */
    max_dblock_rows = DTABLE_MAX_DROWS(fh);
    HDassert(max_dblock_rows);

    /* Loop over rows */
    for(row = 0; row < max_dblock_rows; row++)
        if(fill_row(fh, dxpl, row, obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_all_direct() */


/*-------------------------------------------------------------------------
 * Function:	fill_2nd_indirect_row
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a row of second-level indirect block (which only
 *              have direct blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_2nd_indirect_row(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over row of indirect blocks */
    for(u = 0; u < width; u++)
        if(fill_2nd_indirect(fh, dxpl, pos, obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_2nd_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_2nd_indirect_rows
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in all rows of second-level indirect blocks (which only
 *              have direct blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_2nd_indirect_rows(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over rows of 2nd level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(width) + 1); u++)
        if(fill_2nd_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_2nd_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_3rd_indirect
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a third-level indirect block (which
 *              has one more level of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_3rd_indirect(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    u;                      /* Local index variable */

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, obj_size, state, keep_ids))
        TEST_ERROR

    /* Fill rows of recursive indirect blocks in third level indirect block */
    for(u = 0; u < pos; u++)
        if(fill_2nd_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_3rd_indirect() */


/*-------------------------------------------------------------------------
 * Function:	fill_3rd_indirect_row
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a row of third-level indirect block (which
 *              have one more level of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_3rd_indirect_row(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;              /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over row of 3rd level indirect blocks */
    for(u = 0; u < width; u++)
        /* Fill third level indirect block */
        if(fill_3rd_indirect(fh, dxpl, pos, obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_3rd_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_3rd_indirect_rows
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in all rows of third-level indirect blocks (which
 *              have one more level of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_3rd_indirect_rows(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over rows of 3rd level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(width) + 1); u++)
        /* Fill row of 3rd level indirect blocks */
        if(fill_3rd_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
            TEST_ERROR

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_all_3rd_direct_rows() */


/*-------------------------------------------------------------------------
 * Function:	fill_4th_indirect_row
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in a row of fourth-level indirect blocks (which
 *              have two more levels of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_4th_indirect_row(H5HF_t *fh, hid_t dxpl, unsigned pos, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u, v;                   /* Local index variables */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over row of 4th level indirect blocks */
    for(u = 0; u < width; u++) {
        /* Fill all direct block rows in fourth level indirect block */
        if(fill_all_direct(fh, dxpl, obj_size, state, keep_ids))
            TEST_ERROR

        /* Fill all rows of 2nd level deep indirect blocks in fourth level indirect block */
        if(fill_all_2nd_indirect_rows(fh, dxpl, obj_size, state, keep_ids))
            TEST_ERROR

        /* Fill rows of third level indirect blocks in fourth level indirect block */
        for(v = 0; v < pos; v++)
            if(fill_3rd_indirect_row(fh, dxpl, (v + 1), obj_size, state, keep_ids))
                TEST_ERROR
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_4th_direct_row() */


/*-------------------------------------------------------------------------
 * Function:	fill_all_4th_indirect_rows
 *
 * Purpose:	Insert (small) objects to fill up the free space in all direct
 *              heap blocks in all rows of fourth-level indirect blocks (which
 *              have two more levels of indirect blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
fill_all_4th_indirect_rows(H5HF_t *fh, hid_t dxpl, size_t obj_size,
    fheap_heap_state_t *state, fheap_heap_ids_t *keep_ids)
{
    unsigned    width;                  /* Width of heap's doubling table */
    unsigned    u;                      /* Local index variable */

    /* Get some information for the heap */
    width = DTABLE_WIDTH(fh);

    /* Loop over rows of 4th level deep indirect blocks */
    for(u = 0; u < (H5V_log2_of2(width) + 1); u++) {
        /* Fill row of 4th level indirect blocks */
        if(fill_4th_indirect_row(fh, dxpl, (u + 1), obj_size, state, keep_ids))
            TEST_ERROR

        /* Account for root indirect block doubling # of rows again */
        /* (From 16 rows to the max. # of rows: 22) */
        /* (Note: this is tied to the particular doubling table/heap creation parameters) */
        if(u == 0) {
            unsigned max_root_rows;     /* Maximum # of rows in root indirect block */
            unsigned row;               /* Row in heap */

            /* Get some information for the heap */
            max_root_rows = HEAP_MAX_ROOT_ROWS(fh);

            /* Increase heap size & free space */
            for(row = 16; row < max_root_rows; row++) {
                state->man_size += width * DBLOCK_SIZE(fh, row);
                state->man_free_space += width * DBLOCK_FREE(fh, row);
            } /* end for */
        } /* end if */
    } /* end for */

    /* Operations succeeded */
    return(0);

error:
    return(1);
} /* fill_all_4th_direct_rows() */


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Create fractal heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Heap address in file */
    h5_stat_size_t       empty_size;             /* File size, w/o heap */
    h5_stat_size_t       file_size;              /* File size, after deleting heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /*
     * Test fractal heap creation
     */
    TESTING("fractal heap creation");

    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        TEST_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        TEST_ERROR
    PASSED()

    /* Query the type of address mapping */
    TESTING("query heap creation parameters");
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5HF_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Delete heap */
    if(H5HF_delete(f, H5P_DATASET_XFER_DEFAULT, fh_addr) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_reopen
 *
 * Purpose:	Create & reopen a fractal heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_reopen(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /*
     * Test fractal heap creation
     */

    TESTING("create, close & reopen fractal heap");

    /* Create heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        TEST_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Query the type of address mapping */
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5HF_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_reopen() */


/*-------------------------------------------------------------------------
 * Function:	test_open_twice
 *
 * Purpose:	Open a fractal heap twice
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_open_twice(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t	file2 = -1;             /* File ID */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5F_t	*f2 = NULL;             /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    H5HF_t      *fh2 = NULL;            /* 2nd fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Re-open the file */
    if((file2 = H5Freopen(file)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f2 = H5I_object(file2)))
        FAIL_STACK_ERROR

    /*
     * Test fractal heap creation
     */

    TESTING("open fractal heap twice");

    /* Create heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        TEST_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Open the heap again */
    if(NULL == (fh2 = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Query the type of address mapping */
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh2, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5HF_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Close the second fractal heap wrapper */
    if(H5HF_close(fh2, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh2 = NULL;

    /* Open the fractal heap through the second file handle */
    if(NULL == (fh2 = H5HF_open(f2, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Query the type of address mapping */
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh2, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5HF_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Close the first fractal heap wrapper */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the first file */
    /* (close before second file, to detect error on internal heap header's
     *  shared file information)
     */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Close the second fractal heap wrapper */
    if(H5HF_close(fh2, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh2 = NULL;

    /* Close the second file */
    if(H5Fclose(file2) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);
        if(fh2)
            H5HF_close(fh2, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
	H5Fclose(file2);
    } H5E_END_TRY;
    return(1);
} /* test_open_twice() */


/*-------------------------------------------------------------------------
 * Function:	test_delete_open
 *
 * Purpose:	Delete opened fractal heap (& open deleted heap)
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, January  5, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_delete_open(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t UNUSED *tparam)
{
    hid_t	file = -1;              /* File ID */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_create_t test_cparam;          /* Creation parameters for heap */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    H5HF_t      *fh2 = NULL;            /* 2nd fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;    /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;     /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/no heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Display test banner */
    TESTING("deleting open fractal heap");

    /* Create heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        TEST_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Open the heap again */
    if(NULL == (fh2 = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Request that the heap be deleted */
    if(H5HF_delete(f, H5P_DATASET_XFER_DEFAULT, fh_addr) < 0)
        FAIL_STACK_ERROR

    /* Query the type of address mapping */
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh2, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5HF_cmp_cparam_test(cparam, &test_cparam))
        TEST_ERROR

    /* Close the second fractal heap wrapper */
    if(H5HF_close(fh2, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh2 = NULL;

    /* Try re-opening the heap again (should fail, as heap will be deleted) */
    H5E_BEGIN_TRY {
        fh2 = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr);
    } H5E_END_TRY;
    if(fh2) {
        /* Close opened heap */
        H5HF_close(fh2, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */

    /* Close the first fractal heap wrapper */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

#ifdef QAK
    /* Try re-opening the heap again (should fail, as heap is now deleted) */
    H5E_BEGIN_TRY {
        fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr);
    } H5E_END_TRY;
    if(fh) {
        /* Close opened heap */
        H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);

        /* Indicate error */
        TEST_ERROR
    } /* end if */
#endif /* QAK */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, H5P_DATASET_XFER_DEFAULT);
        if(fh2)
            H5HF_close(fh2, H5P_DATASET_XFER_DEFAULT);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_delete_open() */


/*-------------------------------------------------------------------------
 * Function:	test_id_limits
 *
 * Purpose:	Test limits for heap ID lengths
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_id_limits(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    unsigned    deflate_level;          /* Deflation level */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      tiny_max_len;           /* Max. length of tiny objects */
    hbool_t     tiny_len_extended;      /* Do tiny objects use two bytes for the length? */
    hbool_t     huge_ids_direct;        /* Are 'huge' objects directly acccessed? */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Display testing message */
    TESTING("limits of heap ID lengths")

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));


    /* Set the 'default' heap ID length */
    tmp_cparam.id_len = 0;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != (HEAP_ID_LEN - 1))
        TEST_ERROR
    if(tiny_len_extended != FALSE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != FALSE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length to the size needed for directly accessing 'huge' objects */
    /* (with no I/O pipeline filters) */
    tmp_cparam.id_len = 1;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 17)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 16)
        TEST_ERROR
    if(tiny_len_extended != FALSE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != TRUE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length to the size needed for directly accessing 'huge' objects */
    /* (with I/O pipeline filters) */
    tmp_cparam.id_len = 1;

    /* Set an I/O filter for heap data */
    deflate_level = 6;
    if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
        FAIL_STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 29)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 27)
        TEST_ERROR
    if(tiny_len_extended != TRUE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != TRUE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Release the I/O pipeline filter information */
    H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline);


    /* Set the heap ID length to a size that's too small for 'managed' heap IDs */
    tmp_cparam.id_len = 3;

    /* Create absolute heap */
    H5E_BEGIN_TRY {
        fh = H5HF_create(f, dxpl, &tmp_cparam);
    } H5E_END_TRY;
    if(NULL != fh)
        FAIL_STACK_ERROR


    /* Set the heap ID length a size that's large enough for 'tiny' & 'managed'
     *  objects, but too small for directly accessing 'huge' objects
     */
    tmp_cparam.id_len = 8;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 8)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 7)
        TEST_ERROR
    if(tiny_len_extended != FALSE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != FALSE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length a size that's large enough for directly
     *  directly accessing 'huge' objects
     */
    tmp_cparam.id_len = 17;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 17)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 16)
        TEST_ERROR
    if(tiny_len_extended != FALSE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != TRUE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length to the low side of the boundary condition for
     *  encoding 'tiny' objects in one byte
     */
    tmp_cparam.id_len = 18;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 18)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 16)
        TEST_ERROR
    if(tiny_len_extended != FALSE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != TRUE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length to the high side of the boundary condition for
     *  encoding 'tiny' objects in one byte
     */
    tmp_cparam.id_len = 19;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 19)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 17)
        TEST_ERROR
    if(tiny_len_extended != TRUE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != TRUE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length a size that's larger than what is needed for
     *  directly accessing 'huge' objects
     */
    tmp_cparam.id_len = 45;

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Test ID length information for heap */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != 45)
        FAIL_STACK_ERROR
    if(H5HF_get_tiny_info_test(fh, &tiny_max_len, &tiny_len_extended) < 0)
        FAIL_STACK_ERROR
    if(tiny_max_len != 43)
        TEST_ERROR
    if(tiny_len_extended != TRUE)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(huge_ids_direct != TRUE)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;


    /* Set the heap ID length to a size that's too large to encode the length
     *  of 'tiny' objects
     */
    tmp_cparam.id_len = H5HF_MAX_ID_LEN + 1;

    /* Create absolute heap */
    H5E_BEGIN_TRY {
        fh = H5HF_create(f, dxpl, &tmp_cparam);
    } H5E_END_TRY;
    if(NULL != fh)
        FAIL_STACK_ERROR


    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_id_limits() */


/*-------------------------------------------------------------------------
 * Function:	test_filtered_create
 *
 * Purpose:	Test creating a heap with I/O filters
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_filtered_create(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    H5HF_create_t test_cparam;          /* Temporary local heap creation parameters */
    unsigned    deflate_level;          /* Deflation level */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Display testing message */
    TESTING("creating heaps with I/O filters")

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));


    /* Set an I/O filter for heap data */
    deflate_level = 6;
    if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
        FAIL_STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, &tmp_cparam)))
        FAIL_STACK_ERROR

    /* Get heap's address */
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Query the heap creation parameters */
    HDmemset(&test_cparam, 0, sizeof(H5HF_create_t));
    if(H5HF_get_cparam_test(fh, &test_cparam) < 0)
        FAIL_STACK_ERROR
    if(H5HF_cmp_cparam_test(&tmp_cparam, &test_cparam))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR


    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Release the I/O pipeline filter information */
    H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline);
    H5O_msg_reset(H5O_PLINE_ID, &test_cparam.pline);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_filtered_create() */


/*-------------------------------------------------------------------------
 * Function:	test_size
 *
 * Purpose:	Test querying heap size
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 14, 2007
 *
 *-------------------------------------------------------------------------
 */
static int
test_size(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    hsize_t     empty_heap_size;        /* Total size of empty heap on disk */
    hsize_t     one_heap_size;          /* Total size of heap on disk after inserting one object */
    hsize_t     heap_size;              /* Total size of heap on disk */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Display testing message */
    TESTING("querying heap statistics")


    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR

    /* Get heap's address */
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR

    /* Get an empty heap's size */
    empty_heap_size = 0;
    if(H5HF_size(fh, dxpl, &empty_heap_size) < 0)
        FAIL_STACK_ERROR
    if(empty_heap_size == 0)
        TEST_ERROR

    /* Insert an object */
    if(add_obj(fh, dxpl, (size_t)0, (size_t)10, NULL, NULL) < 0)
        TEST_ERROR

    /* Get the heap's size after inserting one object */
    one_heap_size = 0;
    if(H5HF_size(fh, dxpl, &one_heap_size) < 0)
        FAIL_STACK_ERROR
    if(one_heap_size <= empty_heap_size)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR


    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /* Check the heap's size */
    heap_size = 0;
    if(H5HF_size(fh, dxpl, &heap_size) < 0)
        FAIL_STACK_ERROR
    if(heap_size != one_heap_size)
        TEST_ERROR

    /* Insert another object */
    if(add_obj(fh, dxpl, (size_t)1, (size_t)10, NULL, NULL) < 0)
        TEST_ERROR

    /* Check the heap's size */
    heap_size = 0;
    if(H5HF_size(fh, dxpl, &heap_size) < 0)
        FAIL_STACK_ERROR
    if(heap_size != one_heap_size)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR


    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_size() */


/*-------------------------------------------------------------------------
 * Function:    test_reopen_hdr
 *
 * Purpose:     Test opening a header through one file handle, closing
 *              that file handle, then reopening through a different file
 *              handle that was open the whole time.  The header should
 *              stay in cache between the two opens.
 *
 * Return:      Success:        0
 *              Failure:        1
 *
 * Programmer:  Neil Fortner
 *              Tuesday, September 14, 2010
 *
 *-------------------------------------------------------------------------
 */
static unsigned
test_reopen_hdr(hid_t fapl, H5HF_create_t *cparam)
{
    hid_t       file1 = -1;             /* File ID */
    hid_t       file2 = -2;             /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char        filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t       *f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    hsize_t     heap_size;              /* Total size of heap on disk */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file1 = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file1)))
        FAIL_STACK_ERROR

    /* Display testing message */
    TESTING("reopening header through different file")


    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR

    /* Get heap's address */
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR

    /* Insert an object */
    if(add_obj(fh, dxpl, (size_t)0, (size_t)10, NULL, NULL) < 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file1) < 0)
        FAIL_STACK_ERROR


    /* Re-open the file */
    if((file1 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file again */
    if((file2 = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object (file1) */
    if(NULL == (f = (H5F_t *)H5I_object(file1)))
        FAIL_STACK_ERROR

    /* Open the heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /* Close the heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file (file1) */
    if(H5Fclose(file1) < 0)
        FAIL_STACK_ERROR


    /* Get a pointer to the internal file object (file2) */
    if(NULL == (f = (H5F_t *)H5I_object(file2)))
        FAIL_STACK_ERROR

    /* Reopen the heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /* Check the heap's size */
    heap_size = 0;
    if(H5HF_size(fh, dxpl, &heap_size) < 0)
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR


    /* Close the file (file2) */
    if(H5Fclose(file2) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
        H5Fclose(file1);
        H5Fclose(file2);
    } H5E_END_TRY;
    return(1);
} /* test_reopen_hdr() */

#ifndef QAK2

/*-------------------------------------------------------------------------
 * Function:	test_man_insert_weird
 *
 * Purpose:	Test inserting "weird" sized objects into absolute heap
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_weird(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_state_t state;           /* State of fractal heap */
    herr_t      ret;                    /* Generic return value */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        TEST_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /*
     * Test inserting "weird" sized objects into heap
     */
    TESTING("inserting 'weird' sized objects into absolute heap");

    /* Attempt to insert 0-sized object into heap */
    H5E_BEGIN_TRY {
        ret = H5HF_insert(fh, dxpl, (size_t)0, shared_wobj_g, heap_id);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR
    H5Eclear2(H5E_DEFAULT);

    /* Insert a 1-sized object into heap (should be a 'tiny' object) */
    if(add_obj(fh, dxpl, (size_t)10, (size_t)1, &state, NULL))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check for correctly sized heap */
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_weird() */

#ifdef ALL_INSERT_TESTS

/*-------------------------------------------------------------------------
 * Function:	test_man_insert_first
 *
 * Purpose:	Test inserting first object into absolute heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_first(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        TEST_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        TEST_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting first (small) object into absolute heap");
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check for correctly sized heap */
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_first() */


/*-------------------------------------------------------------------------
 * Function:	test_man_insert_second
 *
 * Purpose:	Test inserting two objects into absolute heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_second(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting first (small) object into absolute heap
     */
    TESTING("inserting two (small) objects into absolute heap");
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert second object */
    if(add_obj(fh, dxpl, (size_t)20, SMALL_OBJ_SIZE2, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_second() */


/*-------------------------------------------------------------------------
 * Function:	test_man_insert_root_mult
 *
 * Purpose:	Test inserting mult. objects into absolute heap, up to the
 *              limit of a root direct block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_root_mult(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) object into absolute heap
     */
    TESTING("inserting objects to fill absolute heap's root direct block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill the heap up */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_root_mult() */


/*-------------------------------------------------------------------------
 * Function:	test_man_insert_force_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, filling the
 *              root direct block and forcing the root block to be converted
 *              into an indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_force_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test forcing creation of indirect root block & second direct block
     */
    TESTING("inserting objects to create root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill the heap up */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force root indirect block creation */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_force_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_insert_fill_second
 *
 * Purpose:	Test inserting mult. objects into absolute heap, filling the
 *              root direct block, forcing the root block to be converted
 *              into an indirect block and filling the secnod indirect block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_fill_second(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill second direct block
     */
    TESTING("inserting objects to fill second direct block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill the first direct block heap up */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_fill_second() */


/*-------------------------------------------------------------------------
 * Function:	test_man_insert_third_direct
 *
 * Purpose:	Test inserting mult. objects into absolute heap, filling the
 *              root direct block, forcing the root block to be converted
 *              into an indirect block, filling the secnod indirect block and
 *              creating a third direct block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_insert_third_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to create third direct block
     */
    TESTING("inserting objects to create third direct block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill the first direct block up */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill the second direct block heap up (also creates initial root indirect block) */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of third direct block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_insert_third_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_first_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_first_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill first row in root indirect block
     */
    TESTING("inserting objects to fill first row of root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill first row of [root] indirect block */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_first_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_start_second_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block, then add another object to start second row.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_start_second_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to start second row of root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill first root indirect row */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force expanding root indirect block to two rows */
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_alloc_size += DBLOCK_SIZE(fh, 1);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 1);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_start_second_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_second_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block, then fill the second row also.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_second_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to start second row in root indirect block
     */
    TESTING("inserting objects to fill second row of root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill first root indirect row */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill second root indirect row */
    if(fill_root_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_second_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_start_third_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first row of root indirect
 *              block, fill the second row also, then add another object to
 *              start the third row.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_start_third_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to start third row in root indirect block
     */
    TESTING("inserting objects to start third row of root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill first root indirect row */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill second root indirect row */
    if(fill_root_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force expanding root indirect block to four rows */
    /* (Goes to four rows because it's doubling) */
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_start_third_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_fourth_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill first four rows of root indirect
 *              block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_fourth_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variables */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill four rows in root indirect block
     */
    TESTING("inserting objects to fill four rows of root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Loop over rows */
    for(u = 0; u < 4; u++)
        if(fill_root_row(fh, dxpl, u, fill_size, &state, NULL))
            FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_fourth_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_all_root_direct
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_all_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct  rows in root indirect block
     */
    TESTING("inserting objects to fill all direct rows of root indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill all direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_all_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_first_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block and create first recursive indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 20, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_first_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to force creation of first recursive indirect block
     */
    TESTING("inserting objects to create first recursive indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of first recursive indirect block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_first_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_second_direct_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block and start second
 *              direct block in that indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_second_direct_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to force creation of second direct
     *  block in first recursive indirect block
     */
    TESTING("inserting objects to create second direct block in first recursive indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill the first direct block in the recursive indirect block up */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of second direct block in
     * first recursive indirect block
     */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_second_direct_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_first_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block and filling all
 *              direct blocks in that indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_first_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first recursive indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_first_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_second_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block, filling all
 *              direct blocks in that indirect block and adding another
 *              object to force creation of second recursive indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_second_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second recursive indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of second
     * recursive indirect block
     */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_second_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_second_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block, filling all
 *              direct blocks in that indirect block and then create second
 *              recursive indirect block and fill all direct blocks in that
 *              indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_second_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in second recursive indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill first recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill 2nd recursive indirect block */
    if(fill_2nd_indirect(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_second_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_recursive_indirect_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, create first recursive indirect block, filling all
 *              direct blocks in that indirect block and then create second
 *              recursive indirect block and fill all direct blocks in that
 *              indirect block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 21, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill all direct blocks in first row of recursive indirect block");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks in root indirect block up */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill row of recursive indirect blocks */
    if(fill_2nd_indirect_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_recursive_indirect_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_start_2nd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the first row of indirect
 *              blocks and start on first block in second row of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_start_2nd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start second row of recursive indirect blocks");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks in root indirect block up */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill row of recursive indirect blocks */
    if(fill_2nd_indirect_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of second
     * recursive indirect block
     */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_start_2nd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_recursive_indirect_two_deep
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_recursive_indirect_two_deep(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill recursive indirect blocks two levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_recursive_indirect_two_deep() */


/*-------------------------------------------------------------------------
 * Function:	test_man_start_3rd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and start first direct block
 *              in 3rd level of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_start_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start recursive indirect blocks three levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of third level deep
     * recursive indirect block
     */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_start_3rd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_first_3rd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and fill first indirect block
 *              in 3rd level of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_first_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first indirect block of recursive indirect blocks three levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill row of recursive indirect blocks in third level indirect block */
    if(fill_2nd_indirect_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_first_3rd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_3rd_recursive_indirect_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and fill all indirect blocks
 *              first row of 3rd level of indirect blocks
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_3rd_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill 1st row of 3rd level indirect blocks */
    if(fill_3rd_indirect_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_3rd_recursive_indirect_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_all_3rd_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep and fill all indirect blocks
 *              that are three levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_all_3rd_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill row of indirect blocks in recursive indirect blocks three levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_all_3rd_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_start_4th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and start first direct block that
 *              is four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_start_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to start first direct block in recursive indirect blocks four levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of four level deep
     * recursive indirect block
     */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_start_4th_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_first_4th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and fill the first (3rd level)
 *              indirect block that is four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_first_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first (3rd level) indirect block in recursive indirect block four levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level deep indirect blocks in fourth level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row of 3rd level deep indirect blocks in fourth level indirect block */
    if(fill_3rd_indirect_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_first_4th_recursive_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_4th_recursive_indirect_row
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and fill the first row of
 *              indirect block that is four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_4th_recursive_indirect_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in first recursive indirect block
     */
    TESTING("inserting objects to fill first row of recursive indirect blocks four levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill 1st row of 4th level indirect blocks */
    if(fill_4th_indirect_row(fh, dxpl, 1, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_4th_recursive_indirect_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_all_4th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep and fill all rows of
 *              indirect blocks that are four levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_all_4th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep
     */
    TESTING("inserting objects to fill all rows of recursive indirect blocks four levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 4th level indirect blocks */
    if(fill_all_4th_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_all_4th_recursive_indirect() */
#endif /* ALL_INSERT_TESTS */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_man_start_5th_recursive_indirect
 *
 * Purpose:	Test inserting mult. objects into absolute heap, creating
 *              enough direct blocks to fill all direct rows of root indirect
 *              block, fill all direct blocks in the row of indirect
 *              blocks that are 2 levels deep, fill all indirect blocks
 *              that are three levels deep, fill all rows of indirect blocks
 *              that are four levels deep and start first direct block in
 *              indirect blocks five levels deep
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_start_5th_recursive_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, H5P_DATASET_XFER_DEFAULT, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));

    /*
     * Test inserting mult. (small) objects to fill all direct
     *  blocks in recursive indirect blocks four levels deep and add one more
     *  block, to make a five level deep structure
     */
    TESTING("inserting objects to create first direct block in recursive indirect blocks five levels deep");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Fill direct blocks up in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap & file */
    if(reopen_file(&file, &f, filename, fapl, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap & file */
    if(reopen_file(&file, &f, filename, fapl, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap & file */
    if(reopen_file(&file, &f, filename, fapl, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 4th level indirect blocks */
    if(fill_all_4th_indirect_rows(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap & file */
    if(reopen_file(&file, &f, filename, fapl, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to force creation of five level deep
     * recursive indirect block
     */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, (size_t)SMALL_OBJ_SIZE1, &state, NULL))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_start_5th_recursive_indirect() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_man_remove_bogus
 *
 * Purpose:	Test removing bogus heap IDs
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_bogus(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    unsigned long seed = 0;             /* Random # seed */
    size_t      id_len;                 /* Size of fractal heap IDs */
    hsize_t     obj_off;                /* Offset of object in heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /*
     * Test removing bogus IDs from heap
     */
    TESTING("removing bad heap IDs from absolute heap");

    /* Retrieve "bulk" filling object size */
    fill_size = get_fill_size(tparam);

    /* Choose random # seed */
    seed = (unsigned long)HDtime(NULL);
#ifdef QAK
/* seed = (unsigned long)1155438845; */
HDfprintf(stderr, "Random # seed was: %lu\n", seed);
#endif /* QAK */
    HDsrandom(seed);

    /* Set heap ID to random (non-null) value */
    heap_id[0] = H5HF_ID_VERS_CURR | H5HF_ID_TYPE_MAN;
    for(u = 1; u < HEAP_ID_LEN; u++)
        heap_id[u] = HDrandom() + 1;

    /* Try removing bogus heap ID from empty heap */
    H5E_BEGIN_TRY {
        ret = H5HF_remove(fh, dxpl, heap_id);
    } H5E_END_TRY;
    if(ret >= 0)
        FAIL_STACK_ERROR

    /* Fill root direct blocks */
    if(fill_root_direct(fh, dxpl, fill_size, &state, NULL))
        FAIL_STACK_ERROR

    /* Get offset of random heap ID */
    if(H5HF_get_id_off_test(fh, heap_id, &obj_off) < 0)
        FAIL_STACK_ERROR

    /* Make certain we can't accidentally use a valid heap ID */
    while(obj_off < state.man_size) {
        /* Set heap ID to random (non-null) value */
        heap_id[0] = H5HF_ID_VERS_CURR | H5HF_ID_TYPE_MAN;
        for(u = 1; u < HEAP_ID_LEN; u++)
            heap_id[u] = HDrandom() + 1;

        /* Get offset of random heap ID */
        if(H5HF_get_id_off_test(fh, heap_id, &obj_off) < 0)
            FAIL_STACK_ERROR
    } /* end while */

    /* Try removing bogus heap ID from heap w/objects */
    H5E_BEGIN_TRY {
        ret = H5HF_remove(fh, dxpl, heap_id);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR
    H5Eclear2(H5E_DEFAULT);

    /* Try reading bogus heap ID from heap w/objects */
    H5E_BEGIN_TRY {
        ret = H5HF_read(fh, dxpl, heap_id, shared_robj_g);
    } H5E_END_TRY;
    if(ret >= 0)
        TEST_ERROR
    H5Eclear2(H5E_DEFAULT);

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    HDfprintf(stderr, "Random # seed was: %lu\n", seed);
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_bogus() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_one
 *
 * Purpose:	Test removing single object from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_one(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    unsigned char obj[SMALL_OBJ_SIZE1]; /* Buffer for object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing first (small) object from absolute heap
     */
    TESTING("removing single object from absolute heap");

    /* Initialize the buffer for objects to insert */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0) - sizeof(obj);
    state.man_nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove object from heap */
    if(H5HF_remove(fh, dxpl, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.man_nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_one() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_two
 *
 * Purpose:	Test removing two objects from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_two(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for first object */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for second object */
    unsigned char obj[SMALL_OBJ_SIZE1]; /* Buffer for object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing two (small) objects from absolute heap
     */
    TESTING("removing two objects from absolute heap");

    /* Initialize the buffer for objects to insert */
    for(u = 0; u < sizeof(obj); u++)
        obj[u] = u;

    /* Insert first object into heap */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &heap_id1) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0) - sizeof(obj);
    state.man_nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Insert second object into heap */
    if(H5HF_insert(fh, dxpl, sizeof(obj), obj, &heap_id2) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_free_space -= sizeof(obj);
    state.man_nobjs++;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove first object from heap */
    if(H5HF_remove(fh, dxpl, heap_id1) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_free_space += sizeof(obj);
    state.man_nobjs--;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove second object from heap */
    if(H5HF_remove(fh, dxpl, heap_id2) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.man_nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_two() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_one_larger
 *
 * Purpose:	Test removing single larger (but < standalone size) object
 *              from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June  6, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_one_larger(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t      obj_len;                /* Length of object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing one larger object from absolute heap
     */
    TESTING("removing single larger object from absolute heap");

    /* Set up object to insert */
    obj_len = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    for(u = 0; u < 4; u++) {
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size = DBLOCK_SIZE(fh, 3);
    state.man_free_space -= obj_len;
    state.man_nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove object from heap */
    if(H5HF_remove(fh, dxpl, heap_id) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.man_nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_one_larger() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_two_larger
 *
 * Purpose:	Test removing two larger (but < standalone size) objects
 *              from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, June 10, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_two_larger(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for first object */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for second object */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t      obj_len;                /* Length of object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing two larger objects from absolute heap
     */
    if(tparam->del_dir == FHEAP_DEL_FORWARD)
        TESTING("removing two larger objects from absolute heap (forward)")
    else
        TESTING("removing two larger objects from absolute heap (reverse)")

    /* Set up first object to insert */
    obj_len = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id1) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    for(u = 0; u < 4; u++) {
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size = DBLOCK_SIZE(fh, 3);
    state.man_free_space -= obj_len;
    state.man_nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Set up second object to insert */
    obj_len = (size_t)DBLOCK_SIZE(fh, 4) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id2) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    /* (Goes to 8 rows because of doubling) */
    for(u = 4; u < 8; u++) {
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size += DBLOCK_SIZE(fh, 5);
    state.man_free_space -= obj_len;
    state.man_nobjs = 2;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove objects in different orders */
    if(tparam->del_dir == FHEAP_DEL_FORWARD) {
        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 3);
        state.man_free_space += DBLOCK_SIZE(fh, 2) + 1;
        state.man_nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
            FAIL_STACK_ERROR
    } /* end if */
    else {
        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        /* (Goes to 4 rows because of halving) */
        for(u = 4; u < 8; u++) {
            state.man_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_free_space -= DBLOCK_FREE(fh, u) * cparam->managed.width;
        } /* end for */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 5);
        state.man_free_space += DBLOCK_SIZE(fh, 4) + 1;
        state.man_nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
            FAIL_STACK_ERROR
    } /* end else */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.man_nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_two_larger() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_three_larger
 *
 * Purpose:	Test removing three larger (but < standalone size) objects
 *              from heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 12, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_three_larger(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for first object */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for second object */
    unsigned char heap_id3[HEAP_ID_LEN]; /* Heap ID for third object */
    unsigned char *obj;                 /* Buffer for object to insert */
    size_t      obj_len;                /* Length of object to insert */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        STACK_ERROR

    /* Create absolute heap */
    if(NULL == (fh = H5HF_create(f, dxpl, cparam)))
        FAIL_STACK_ERROR
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > HEAP_ID_LEN)
        FAIL_STACK_ERROR
    if(H5HF_get_heap_addr(fh, &fh_addr) < 0)
        FAIL_STACK_ERROR
    if(!H5F_addr_defined(fh_addr))
        FAIL_STACK_ERROR
    HDmemset(&state, 0, sizeof(fheap_heap_state_t));
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Prepare for querying the size of a file with an empty heap */

    /* Close (empty) heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file w/empty heap*/
    if((empty_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open heap */
    if(NULL == (fh = H5HF_open(f, dxpl, fh_addr)))
        FAIL_STACK_ERROR

    /*
     * Test removing three larger objects from absolute heap
     */
    if(tparam->del_dir == FHEAP_DEL_FORWARD)
        TESTING("removing three larger objects from absolute heap (forward)")
    else
        TESTING("removing three larger objects from absolute heap (reverse)")

    /* Set up first object to insert */
    obj_len = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id1) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    for(u = 0; u < 4; u++) {
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size = DBLOCK_SIZE(fh, 3);
    state.man_free_space -= obj_len;
    state.man_nobjs = 1;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Set up second object to insert */
    obj_len = (size_t)DBLOCK_SIZE(fh, 4) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id2) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    /* (Goes to 8 rows because of doubling) */
    for(u = 4; u < 8; u++) {
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size += DBLOCK_SIZE(fh, 5);
    state.man_free_space -= obj_len;
    state.man_nobjs = 2;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Set up third object to insert */
    obj_len = (size_t)DBLOCK_SIZE(fh, 7) + 1;
    obj = shared_wobj_g;

    /* Insert object into heap */
    if(H5HF_insert(fh, dxpl, obj_len, obj, &heap_id3) < 0)
        FAIL_STACK_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    /* (Goes to 16 rows because of doubling) */
    for(u = 8; u < 16; u++) {
        state.man_size += DBLOCK_SIZE(fh, u) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u) * cparam->managed.width;
    } /* end for */
    state.man_alloc_size += DBLOCK_SIZE(fh, 8);
    state.man_free_space -= obj_len;
    state.man_nobjs = 3;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Remove objects in different orders */
    if(tparam->del_dir == FHEAP_DEL_FORWARD) {
        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 3);
        state.man_free_space += DBLOCK_SIZE(fh, 2) + 1;
        state.man_nobjs = 2;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 5);
        state.man_free_space += DBLOCK_SIZE(fh, 4) + 1;
        state.man_nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove third object from heap */
        if(H5HF_remove(fh, dxpl, heap_id3) < 0)
            FAIL_STACK_ERROR
    } /* end if */
    else {
        /* Remove third object from heap */
        if(H5HF_remove(fh, dxpl, heap_id3) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        /* (Goes to 8 rows because of halving) */
        for(u = 8; u < 16; u++) {
            state.man_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_free_space -= DBLOCK_FREE(fh, u) * cparam->managed.width;
        } /* end for */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 8);
        state.man_free_space += DBLOCK_SIZE(fh, 7) + 1;
        state.man_nobjs = 2;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove second object from heap */
        if(H5HF_remove(fh, dxpl, heap_id2) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        /* (Goes to 4 rows because of halving) */
        for(u = 4; u < 8; u++) {
            state.man_size -= DBLOCK_SIZE(fh, u) * cparam->managed.width;
            state.man_free_space -= DBLOCK_FREE(fh, u) * cparam->managed.width;
        } /* end for */
        state.man_alloc_size -= DBLOCK_SIZE(fh, 5);
        state.man_free_space += DBLOCK_SIZE(fh, 4) + 1;
        state.man_nobjs = 1;
        if(check_stats(fh, &state))
            FAIL_STACK_ERROR

        /* Remove first object from heap */
        if(H5HF_remove(fh, dxpl, heap_id1) < 0)
            FAIL_STACK_ERROR
    } /* end else */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = 0;
    state.man_alloc_size = 0;
    state.man_free_space = 0;
    state.man_nobjs = 0;
    if(check_stats(fh, &state))
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        TEST_ERROR

    /* Close the file */
    if(H5Fclose(file) < 0)
        TEST_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Verify the file is correct size */
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */
    if(file_size != empty_size)
        TEST_ERROR

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_three_larger() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_man_remove_root_direct
 *
 * Purpose:	Test filling and removing all objects from root direct block in
 *              heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from root direct block of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill the heap up */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_two_direct
 *
 * Purpose:	Test filling and removing all objects from (first) two direct
 *              blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 22, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_two_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from two direct blocks of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill the first block in heap */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Fill the second block in heap */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_two_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_first_row
 *
 * Purpose:	Test filling and removing all objects from first row of direct
 *              blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June  5, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_first_row(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from first row of direct blocks of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill first row of direct blocks */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_first_row() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_first_two_rows
 *
 * Purpose:	Test filling and removing all objects from first two rows of
 *              direct blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, June 12, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_first_two_rows(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from first two rows of direct blocks of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill first two rows of direct blocks */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_root_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_first_two_rows() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_first_four_rows
 *
 * Purpose:	Test filling and removing all objects from first four rows of
 *              direct blocks in heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_first_four_rows(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from first four rows of direct blocks of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill first two rows of direct blocks */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_root_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_root_row(fh, dxpl, 2, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_root_row(fh, dxpl, 3, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_first_four_rows() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_all_root_direct
 *
 * Purpose:	Test filling and removing all objects from all direct blocks
 *              in root indirect block of heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_all_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from all direct blocks of root group in absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_all_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_2nd_indirect
 *
 * Purpose:	Test filling and removing all objects up to 2nd level indirect
 *              blocks of heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, June 13, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_2nd_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from 2nd level indirect blocks of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_2nd_indirect() */


/*-------------------------------------------------------------------------
 * Function:	test_man_remove_3rd_indirect
 *
 * Purpose:	Test filling and removing all objects up to 3rd level indirect
 *              blocks of heap
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_remove_3rd_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "removing all objects from 3rd level indirect blocks of absolute heap %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_remove_3rd_indirect() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_man_skip_start_block
 *
 * Purpose:	Test inserting object into absolute heap which is too large
 *              for starting block size, which forces root indirect block
 *              creation
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, March 27, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_skip_start_block(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "inserting object that is too large for starting block, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_skip_start_block() */


/*-------------------------------------------------------------------------
 * Function:	test_man_skip_start_block_add_back
 *
 * Purpose:	Test inserting object into absolute heap which is too large
 *              for starting block size, which forces root indirect block
 *              creation, then add object which fits in skipped direct block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_skip_start_block_add_back(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "skipping starting block, then adding object back to first block, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Insert object too large for starting block size */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the heap block just created */
    obj_size = (size_t)DBLOCK_FREE(fh, 2) - obj_size;
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert second "real" object, which should go in earlier direct block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)20, (size_t)SMALL_OBJ_SIZE2, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_skip_start_block_add_back() */


/*-------------------------------------------------------------------------
 * Function:	test_man_skip_start_block_add_skipped
 *
 * Purpose:	Test inserting object into absolute heap which is too large
 *              for starting block size, which forces root indirect block
 *              creation, then add objects to fill skipped direct blocks
 *              and add another object to start on next "normal" block
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, March 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "skipping starting block, then adding objects to backfill and extend, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Insert object too large for starting block size */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the heap block just created */
    obj_size = (size_t)DBLOCK_FREE(fh, 2) - obj_size;
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add rows of blocks to "backfill" direct blocks that were skipped */
    if(fill_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert another object, which should extend direct blocks, instead of backfill */
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)20, (size_t)SMALL_OBJ_SIZE2, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_skip_2nd_block
 *
 * Purpose:	Test inserting object into absolute heap which is small
 *              enough for starting block size, then add object too large
 *              for any blocks in first row of direct blocks, to force
 *              early creation of indirect block (and range of skipped blocks)
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_skip_2nd_block(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert object to initial block, then add object too large for starting direct blocks, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Insert small object, to create root direct block */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, (size_t)SMALL_OBJ_SIZE1, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the second object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    state.man_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    state.man_free_space += (cparam->managed.width - 1 )* DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_skip_2nd_block() */


/*-------------------------------------------------------------------------
 * Function:	test_man_skip_2nd_block_add_skipped
 *
 * Purpose:	Test inserting object into absolute heap which is small
 *              enough for starting block size, then add object too large
 *              for any blocks in first row of direct blocks, to force
 *              early creation of indirect block (and range of skipped blocks).
 *              Then add more objects to fill up remainder of initial direct
 *              block and all the skipped blocks, and one more object (to
 *              start next "normal" block).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April  1, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_skip_2nd_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert object to initial block, then add object too large for starting direct blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    v;                      /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Insert small object, to create root direct block */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, (size_t)SMALL_OBJ_SIZE1, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the second object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    state.man_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    state.man_free_space += (cparam->managed.width - 1 )* DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the (smaller) heap block just created */
    obj_size = (size_t)DBLOCK_FREE(fh, 0) - SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill remainder of 2 * start size block */
    obj_size = (size_t)DBLOCK_FREE(fh, 2) - ((size_t)DBLOCK_SIZE(fh, 0) + 1);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects to fill remaining rows of the starting block size */

    /* Fill remainder of first row of direct heap blocks up */
    for(v = 0; v < (cparam->managed.width - 1); v++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(fill_heap(fh, dxpl, 0, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Fill second row of direct blocks */
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to create new 2 * start size direct block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)10, (size_t)SMALL_OBJ_SIZE1, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_skip_2nd_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_one_partial_skip_2nd_block_add_skipped
 *
 * Purpose:	Test filling initial direct block, then add object small enough
 *              for initial block size (to create root indirect block), then
 *              add object too large for any blocks in first three rows of
 *              direct blocks, to force extension of indirect block (and range
 *              of skipped blocks).
 *
 *              Then add more objects to fill up remainder of partial direct
 *              block and all the skipped blocks, and one more object (to
 *              start next "normal" block).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_one_partial_skip_2nd_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "skipping blocks with indirect root, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u;                      /* Local index variable */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill initial direct block */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    if(fill_heap(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert small object, to create root indirect block */
    state.man_size += (cparam->managed.width - 1) * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size += DBLOCK_SIZE(fh, 0);
    state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
    if(add_obj(fh, dxpl, (size_t)10, (size_t)SMALL_OBJ_SIZE1, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the (smaller) heap block just created */
    obj_size = (size_t)DBLOCK_FREE(fh, 0) - SMALL_OBJ_SIZE1;
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill remainder of 4 * start size block */
    obj_size = (size_t)DBLOCK_FREE(fh, 3) - ((size_t)DBLOCK_SIZE(fh, 2) + 1);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects to fill remaining heaps in first row */
    for(u = 0; u < (cparam->managed.width - 2); u++) {
        /* Fill a direct heap block up */
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(fill_heap(fh, dxpl, 0, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects to fill remaining heaps in second row */
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects to fill remaining heaps in third row */
    if(fill_row(fh, dxpl, 2, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to create new 4 * start size direct block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, (size_t)SMALL_OBJ_SIZE1, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_one_partial_skip_2nd_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_row_skip_add_skipped
 *
 * Purpose:	Test filling first row of direct blocks, then
 *              add object too large for any blocks in first three rows of
 *              direct blocks, to force extension of indirect block (and range
 *              of skipped blocks).
 *
 *              Then add more objects to fill up remainder of partial direct
 *              block and all the skipped blocks, and one more object (to
 *              start next "normal" block).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_row_skip_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling first row, then skipping rows, then backfill and extend, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill first row of direct blocks */
    if(fill_root_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 2);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill remainder of 4 * start size block */
    obj_size = (size_t)DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects to fill remaining heaps in second row */
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects to fill remaining heaps in third row */
    if(fill_row(fh, dxpl, 2, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to create new 4 * start size direct block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_row_skip_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_skip_direct_skip_indirect_two_rows_add_skipped
 *
 * Purpose:	Test adding object too large for all but the last row in the
 *              direct blocks in root indirect block, then
 *              add object too large for initial block in first two rows of
 *              indirect blocks, to force extension of non-root
 *              indirect block (and range of skipped blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_skip_direct_skip_indirect_two_rows_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_direct_rows;        /* Number of rows (of direct blocks) in root indirect block */
    unsigned    row;                    /* Current row */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "skipping direct blocks to last row and skipping two rows of root indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    v;                      /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Compute # direct block rows in root indirect block */
    num_direct_rows = DTABLE_MAX_DROWS(fh);

    /* Compute heap size & free space when half direct blocks allocated */
    row = 0;
    do {
        state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < (num_direct_rows / 2));

    /* Insert object to extend root block to middle of root direct blocks
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, row - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, row - 1);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Compute heap size & free space when all direct blocks allocated */
    do {
        state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < num_direct_rows);

    /* Insert large objects into last row of direct blocks in root indirect
     * block, to force extension of root indirect block that covers the first
     * row of indirect blocks in root indirect block
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_direct_rows - 2) + 1;
    for(v = 0; v < cparam->managed.width; v++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, num_direct_rows - 1);
        if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Compute heap size & free space when root indirect block doubles again */
    do {
        state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
        state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        row++;
    } while(row < (2 * num_direct_rows));

    /* Insert large object, to force creation of indirect blocks with
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_direct_rows - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_direct_rows - 1);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_skip_direct_skip_indirect_two_rows_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_direct_skip_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for initial block in first row of direct
 *              blocks in indirect block, to force extension of non-root
 *              indirect block (and range of skipped blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_direct_skip_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks and skipping blocks in non-root indirect block, then backfill and extend, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add rows of blocks to "backfill" direct blocks that were skipped */
    if(fill_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the (biggest) heap block created */
    obj_size = (size_t)DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill direct block heaps with 2 * initial block size in nested indirect block */
    if(fill_row(fh, dxpl, 2, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert one more object, to create new 4 * start size direct block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_direct_skip_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_direct_skip_2nd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for all direct blocks in first row of
 *              indirect blocks, to force skipping a row of indirect blocks
 *              (and range of skipped blocks), then backfill all direct blocks
 *              skipped and extend to next "normal" direct block.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  3, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_direct_skip_2nd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    row;                    /* Current row in indirect block */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks and skipping row of non-root indirect blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u;                      /* Local index variable */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of direct blocks that are smaller than large object's block size */
    for(row = 0; row < num_first_indirect_rows; row++) {
        /* Fill rows of direct blocks in skipped indirect blocks */
        for(u = 0; u < cparam->managed.width; u++)
            if(fill_row(fh, dxpl, row, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Fill row of direct blocks in largest (i.e. non-skipped) indirect block */
        if(fill_row(fh, dxpl, row, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_direct_skip_2nd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_2nd_direct_less_one_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, except the last
 *              one, then insert object insert object that is too large to
 *              hold in row of 2nd level indirect blocks (forcing the use of
 *              the next row of 2nd level blocks), then backfill all skipped
 *              direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_2nd_direct_less_one_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, except last one, and insert object too large for 2nd level indirect blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u;                      /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row (except one) of 2nd level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++)
        /* Fill all rows of 2nd level indirect blocks in root block */
        if(fill_2nd_indirect(fh, dxpl, 1, fill_size, &state, &keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in 2nd level indirect block's direct blocks
     * (and rows of next 2nd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in skipped 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_2nd_direct_less_one_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for all direct blocks in first row of
 *              indirect blocks, to force skipping a row of indirect blocks
 *              (and range of skipped blocks), then add object that is too
 *              large for initial block size in skipped indirect blocks, then
 *              backfill all direct blocks and extend to next "normal" direct
 *              block (but insert first block of backfilling with object
 *              too large for initial block size in skipped indirect block
 *              row's direct blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    row;                    /* Current row in indirect block */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks and skipping row of non-root indirect blocks, then skip row of direct blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u;                      /* Local index variable */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object too large for initial block size in skipped indirect blocks */
    obj_size = (size_t)DBLOCK_SIZE(fh, 3) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, 4);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (medium) block just created */
    obj_size = (size_t)DBLOCK_FREE(fh, 4) - obj_size;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Finish off blocks in row of medium block size (just to make row filling easier below) */
    obj_size = (size_t)DBLOCK_FREE(fh, 4);
    for(u = 1; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 4);
        if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of direct blocks that are smaller than large object's block size */
    for(row = 0; row < num_first_indirect_rows; row++) {
        /* Fill rows of direct blocks in skipped indirect blocks */
        for(u = 0; u < cparam->managed.width; u++)
            if(fill_row(fh, dxpl, row, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Fill row of direct blocks in largest (i.e. non-skipped) indirect block */
        /* (Skip the row of blocks filled above) */
        if(row != 4)
            if(fill_row(fh, dxpl, row, fill_size, &state, &keep_ids))
                TEST_ERROR
    } /* end while */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_direct_skip_indirect_two_rows_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for initial block in first two rows of
 *              indirect blocks, to force extension of non-root
 *              indirect block (and range of skipped blocks).
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_direct_skip_indirect_two_rows_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    max_dblock_rows;        /* Max. # of rows (of direct blocks) in the root indirect block */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks and skipping two rows of root indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
    max_dblock_rows = DTABLE_MAX_DROWS(fh);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, max_dblock_rows - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 1);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the (biggest) heap block created */
    obj_size = (size_t)DBLOCK_FREE(fh, max_dblock_rows - 1) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in indirect block's direct blocks */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in first row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block rows in second row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in used 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows in second row of skipped 2nd level indirect blocks (and used 2nd level block) */

    /* Direct block rows in skipped 2nd level indirect blocks */
    for(v = 0; v < cparam->managed.width; v++)
        if(fill_row(fh, dxpl, num_first_indirect_rows, fill_size, &state, &keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Direct block row in used 2nd level indirect block */
    if(fill_row(fh, dxpl, num_first_indirect_rows, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 1);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_direct_skip_indirect_two_rows_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_direct_skip_indirect_two_rows_skip_indirect_row_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              add object too large for initial block in first two rows of
 *              indirect blocks, to force extension of non-root
 *              indirect block, then add object too large for first row of
 *              indirect blocks, (and ranges of skipped blocks), then backfill
 *              and extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_direct_skip_indirect_two_rows_skip_indirect_row_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    unsigned    max_dblock_rows;        /* Max. # of rows (of direct blocks) in the root indirect block */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks and skipping two rows of root indirect block, skip one row of root indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
    max_dblock_rows = DTABLE_MAX_DROWS(fh);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of two rows of indirect blocks and
     * range of skipped blocks that are too small to hold the large object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, max_dblock_rows - 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 1);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the (biggest) heap block created */
    obj_size = (size_t)DBLOCK_FREE(fh, max_dblock_rows - 1) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object that can't fit in first row of indirect blocks
     * previously skipped, but is small enough to fit into second row of
     * skipped blocks.
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, max_dblock_rows - 3) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 2);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert an object to fill up the (2nd biggest) heap block created */
    obj_size = (size_t)DBLOCK_FREE(fh, max_dblock_rows - 2) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in indirect block's direct blocks */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in first row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block rows in second row of skipped 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in used 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows in second row of skipped 2nd level indirect blocks (and used 2nd level block) */

    /* Finish blocks in partially used 2nd level indirect block */
    if(fill_partial_row(fh, dxpl, num_first_indirect_rows, cparam->managed.width - 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Direct block rows in skipped 2nd level indirect blocks */
    /* (less the one indirect block already used) */
    for(v = 0; v < cparam->managed.width - 1; v++)
        if(fill_row(fh, dxpl, num_first_indirect_rows, fill_size, &state, &keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Direct block row in used 3rd row 2nd level indirect block */
    if(fill_row(fh, dxpl, num_first_indirect_rows, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, max_dblock_rows - 1);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_direct_skip_indirect_two_rows_skip_indirect_row_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_2nd_direct_skip_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, the insert object
 *              that is too large to hold in first row of direct blocks of
 *              3rd level indirect block, then backfill & extend all skipped
 *              3rd level indirect block's direct blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_2nd_direct_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, and skip first rows of direct blocks of 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in 3rd level indirect block's direct blocks */
    if(fill_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_row(fh, dxpl, 2, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_2nd_direct_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks in 3rd level indirect block, then insert object
 *              that is too large to hold in first row of direct blocks of
 *              3rd level indirect block's first 2nd level indirect block, then
 *              backfill & extend all skipped 2nd level indirect block's direct
 *              blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first rows of direct blocks of 3rd level indirect block's 2nd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 2) + 1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, 3) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (3rd level indirect block's) 2nd level
     *  indirect block's direct blocks
     */
    if(fill_row(fh, dxpl, 0, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR
    if(fill_row(fh, dxpl, 2, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, 3);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks in 3rd level indirect block, then insert object
 *              that is too large to hold in first row of 2nd level indirect
 *              blocks of 3rd level indirect block, then backfill & extend all
 *              skipped direct blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first row of indirect blocks of 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (first 3rd level indirect block's) 2nd level
     *  indirect block's direct blocks
     *  (and second 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks in 3rd level indirect block, then insert object
 *              that is too large to hold in first & second rows of 2nd level
 *              indirect blocks (although this 3rd level indirect block only
 *              has one row of 2nd level indirect blocks) of 3rd level indirect
 *             block, then backfill & extend all skipped direct blocks.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, April 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect block's direct blocks, and skip first two rows of indirect blocks of 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in third level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows + 1);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows + 1) - obj_size;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (first 3rd level indirect block's) 2nd level
     *  indirect block's direct blocks
     *  (and second 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Fill row of direct blocks in second 3rd level indirect block */
    if(fill_row(fh, dxpl, num_first_indirect_rows, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows + 1);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, all 3rd level
 *              indirect blocks in first row except the last one, fill direct
 *              blocks in last 3rd level indirect block, then insert object
 *              insert object that is too large to hold in last 3rd level
 *              indirect block's row of 2nd level indirect blocks (forcing the
 *              use of the next row of 3rd level blocks), then backfill all
 *              skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tues, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling first row of 3rd level indirect blocks, except last one, fill all direct blocks in last 3rd level indirect block, and insert object too large for it's 2nd level indirect blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in root indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row (except one) of 3rd level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++)
        /* Fill 3rd level indirect block */
        if(fill_3rd_indirect(fh, dxpl, 1, fill_size, &state, &keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in last third level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in 2nd level indirect block's direct blocks
     * (and rows of next 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in current 3rd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, all 3rd level
 *              indirect blocks in first row, fill direct blocks in 2nd row 3rd
 *              level indirect block, fill all direct blocks in 1st row of
 *              2nd level indirect blocks except the last one, then insert
 *              object that is too large to hold in 3rd level indirect block's
 *              first row of 2nd level indirect blocks (forcing the use of the
 *              next row of 2nd level blocks), then backfill all skipped direct
 *              blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tues, April 18, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling first row of 3rd level indirect blocks, fill all direct blocks in next 3rd level indirect block, fill all 1st row of 2nd level indirect blocks, except last one, and insert object too large for 2nd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u;                      /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row of 3rd level indirect blocks */
    if(fill_3rd_indirect_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 2nd row third level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row (except one) of 2nd level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++)
        if(fill_2nd_indirect(fh, dxpl, 1, fill_size, &state, &keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in 2nd level indirect block's direct blocks
     * (and rows of next 2nd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in skipped 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR

        /* Direct block row in current 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_3rd_direct_fill_direct_skip_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, then
 *              fill all direct blocks in 4th level indirect block, then
 *              insert object that is too large to hold in first row of 2nd
 *              level indirect blocks of 4th level indirect block, then
 *              backfill all skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Saturday, April 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_3rd_direct_fill_direct_skip_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill 4th level indirect block's direct blocks, and skip first row of 2nd indirect blocks of 4th level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (first 4th level indirect block's) 2nd level
     *  indirect block's direct blocks
     *  (and second row of 2nd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 2nd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_3rd_direct_fill_direct_skip_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, then
 *              fill all direct blocks and 2nd level indirect blocks in 4th
 *              level indirect block, then
 *              insert object that is too large to hold in first row of 2nd
 *              level indirect blocks of 4th level indirect block's first
 *              3rd level indirect block, then
 *              backfill all skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill 4th level indirect block's direct, 2nd level indirect blocks and 3rd level direct block, and skip first row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in fourth level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in fourth level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in fourth level indirect block's 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (first 4th level indirect block's first 3rd
     *  level block's) 2nd level indirect block's direct blocks
     *  (and rows of 2nd 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, fill all
 *              direct & indirect blocks in first row of 4th level indirect
 *              blocks, then fill all direct blocks in first row of 3rd level
 *              indirect blocks in 4th level indirect block, fill direct blocks
 *              in first block of 2nd row of 3rd level indirect blocks in 4th
 *              level indirect block, then insert object insert object that is
 *              too large to hold in first row of 2nd level indirect blocks of
 *              3rd level indirect block (in 4th level indirect block), then
 *              backfill all skipped direct blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 4th level indirect blocks, fill 2nd row 4th level indirect block's direct, 2nd level indirect blocks, first row of 3rd level indirect blocks, 3rd level direct block in 2nd row, and skip first row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row of 4th level indirect blocks */
    if(fill_4th_indirect_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Account for root indirect block doubling # of rows again */
    /* (From 16 rows to the max. # of rows: 22) */
    /* (Note: this is tied to the particular doubling table/heap creation parameters) */
    {
        unsigned max_root_rows;     /* Maximum # of rows in root indirect block */
        unsigned row;               /* Row in heap */

        /* Get some information for the heap */
        max_root_rows = HEAP_MAX_ROOT_ROWS(fh);

        /* Increase heap size & free space */
        for(row = 16; row < max_root_rows; row++) {
            state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
            state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        } /* end for */
    } /* end if */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 2nd row 4th level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in 2nd row 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row of 3rd level indirect blocks in 2nd row 4th level indirect block */
    if(fill_3rd_indirect_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 4th level indirect block's 2nd row of 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (first block in 2nd row  4th level indirect
     *  block's first 3rd level block's) 2nd level indirect block's direct
     * blocks (and rows of 2nd 3rd level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 3rd level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, fill all
 *              direct & indirect blocks in 4th level indirect
 *              block, then fill all direct blocks in first row of 3rd
 *              level indirect blocks in 4th level indirect block except
 *              the last (3rd level indirect block) in 4th level indirect block,
 *              fill direct blocks in last 3rd level indirect block, then
 *              insert object insert object that is too large to hold in first
 *              row of 2nd level indirect blocks of 3rd level indirect block
 *              (in 4th level indirect block) (forcing the use of the next
 *              4th level block), then backfill all skipped direct blocks &
 *              extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 3rd level indirect blocks in 4th level indirect block except last 3rd level block, fill direct blocks in 3rd level block, and skip row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 4th level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row (except one) of 3rd level indirect blocks in 4th level indirect block */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all direct block rows in 3rd level indirect block */
        if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
            TEST_ERROR

        /* Fill row of 2nd level indirect blocks in 3rd level indirect block */
        if(fill_2nd_indirect_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 4th level indirect block's last 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (4th level indirect block's first 3rd level
     * block's) 2nd level indirect block's direct blocks (and rows of next 4th
     * level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 4th level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped() */


/*-------------------------------------------------------------------------
 * Function:	test_man_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped
 *
 * Purpose:	Test filling all direct blocks in root indirect block and all
 *              direct blocks in 2nd level indirect blocks, fill all direct
 *              blocks and indirect blocks in 3rd level indirect block, fill all
 *              direct & indirect blocks in first row of 4th level indirect
 *              blocks, except last one, then fill all direct blocks in first
 *              row of 3rd level indirect blocks in 4th level indirect block
 *              except the last (3rd level indirect block) in 4th level
 *              indirect block, fill direct blocks in last 3rd level indirect
 *              block, then insert object insert object that is too large to
 *              hold in row of 2nd level indirect blocks in 3rd level indirect
 *              block (in 4th level indirect block) (forcing the use of the
 *              next row of 4th level blocks), then backfill all skipped direct
 *              blocks & extend.
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April 17, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "filling direct blocks, filling 2nd level indirect blocks, filling 3rd level indirect blocks, fill first row of 4th level indirect blocks, except last one, fill first row of 3rd level indirect blocks in last 4th level indirect block except last 3rd level block, fill direct blocks in 3rd level block, and skip row of 2nd indirect blocks of 4th level indirect block's 3rd level indirect block, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve info about heap */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 3rd level indirect blocks */
    if(fill_all_3rd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill first row (except one) of 4th level indirect blocks */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all direct block rows in 4th level indirect block */
        if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
            TEST_ERROR

        /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
        if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
            TEST_ERROR

        /* Fill row of 3rd level indirect blocks in 4th level indirect block */
        if(fill_3rd_indirect_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 4th level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in 4th level indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill row (except one) of 3rd level indirect blocks in 4th level indirect block */
    for(u = 0; u < cparam->managed.width - 1; u++) {
        /* Fill all direct block rows in 3rd level indirect block */
        if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
            TEST_ERROR

        /* Fill row of 2nd level indirect blocks in 3rd level indirect block */
        if(fill_2nd_indirect_row(fh, dxpl, 1, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all direct block rows in 4th level indirect block's last 3rd level indirect block */
    if(fill_all_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Account for root indirect block doubling # of rows again */
    /* (From 16 rows to the max. # of rows: 22) */
    /* (Note: this is tied to the particular doubling table/heap creation parameters) */
    {
        unsigned max_root_rows;     /* Maximum # of rows in root indirect block */
        unsigned row;               /* Row in heap */

        /* Get some information for the heap */
        max_root_rows = HEAP_MAX_ROOT_ROWS(fh);

        /* Increase heap size & free space */
        for(row = 16; row < max_root_rows; row++) {
            state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, row);
            state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, row);
        } /* end for */
    } /* end if */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert large object, to force creation of indirect block and
     * range of skipped (indirect) blocks that are too small to hold the large
     * object
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, num_first_indirect_rows - 1) + 1;
#ifdef QAK
HDfprintf(stderr, "obj_size = %Zu\n", obj_size);
#endif /* QAK */
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object to fill space in (large) block created */
    obj_size = (size_t)DBLOCK_FREE(fh, num_first_indirect_rows) - obj_size;
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill rows skipped over in (4th level indirect block's first 3rd level
     * block's) 2nd level indirect block's direct blocks (and rows of next 4th
     * level indirect block's direct blocks)
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        /* Direct block rows in 2nd level indirect blocks */
        for(v = 0; v < cparam->managed.width; v++)
            if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
                TEST_ERROR

        /* Direct block row in 4th level indirect block */
        if(fill_row(fh, dxpl, u, fill_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Add one more object, to create another "large" block */
    obj_size = SMALL_OBJ_SIZE1;
    state.man_alloc_size += DBLOCK_SIZE(fh, num_first_indirect_rows);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_man_frag_simple
 *
 * Purpose:	Test inserting objects small enough to fit into first row of
 *              direct blocks, but not to share a block with another object,
 *              until start-block-size * 2 blocks are reached.  Then, go back
 *              and fill in the space in the blocks skipped.
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, July 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_frag_simple(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "fragmenting small blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u;                      /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Insert objects small enough to fit into initial blocks, but not to
     * share them with other objects of the same size, until the next larger
     * block size is reached.
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) / 2;
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
            TEST_ERROR
        if(u == 0) {
            state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
            state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
        } /* end if */
    } /* end for */
    state.man_size += DBLOCK_SIZE(fh, 1) * cparam->managed.width;
    state.man_free_space += DBLOCK_FREE(fh, 1) * cparam->managed.width;
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 1);
        if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* (Account for doubling root indirect block for rows 3-4 */
    for(u = 0; u < 2; u++) {
        state.man_size += DBLOCK_SIZE(fh, u + 2) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 2) * cparam->managed.width;
    } /* end for */

    /* Add one more object, to create a 2 * start_block_size block */
    state.man_alloc_size += DBLOCK_SIZE(fh, 2);
    if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Go back and fill in direct blocks of initial block size (which have large free space in them) */
    obj_size = (size_t)DBLOCK_FREE(fh, 0) - obj_size;
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
            TEST_ERROR
    for(u = 0; u < cparam->managed.width; u++)
        if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
            TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill in 2 * start_block_size block */
    obj_size = (size_t)DBLOCK_FREE(fh, 2) - ((size_t)DBLOCK_SIZE(fh, 0) / 2);
    if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
        TEST_ERROR


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_frag_simple() */


/*-------------------------------------------------------------------------
 * Function:	test_man_frag_direct
 *
 * Purpose:	Test inserting small object to fit into each direct block
 *              in root block, but not to share a block with another object,
 *              Then, go back and fill in the space in the blocks skipped.
 *
 *              Then, go back and remove all objects
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 25, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_frag_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    root_direct_rows;       /* Number of rows in root indirect block */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "fragmenting direct blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Retrieve # of direct rows in root indirect block */
    root_direct_rows = H5HF_get_dtable_max_drows_test(fh);

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) / 2;
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0);
    /* First row */
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 0);
        if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
            TEST_ERROR
        if(u == 0) {
            state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
            state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 0);
        } /* end if */
    } /* end for */
    state.man_size += DBLOCK_SIZE(fh, 1) * cparam->managed.width;
    state.man_free_space += DBLOCK_FREE(fh, 1) * cparam->managed.width;
    /* Second row */
    for(u = 0; u < cparam->managed.width; u++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 1);
        if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* (Account for doubling root indirect block for rows 3-4 */
    for(u = 0; u < 2; u++) {
        state.man_size += DBLOCK_SIZE(fh, u + 2) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 2) * cparam->managed.width;
    } /* end for */

    /* Rows 3-4 */
    for(u = 0; u < 2; u++) {
        obj_size = (size_t)DBLOCK_SIZE(fh, u + 2) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u + 2);
            if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
                TEST_ERROR
        } /* end for */
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* (Account for doubling root indirect block for rows 5-8 */
    for(u = 0; u < 4; u++) {
        state.man_size += DBLOCK_SIZE(fh, u + 4) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 4) * cparam->managed.width;
    } /* end for */

    /* Rows 5-8 */
    for(u = 0; u < 4; u++) {
        obj_size = (size_t)DBLOCK_SIZE(fh, u + 4) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u + 4);
            if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
                TEST_ERROR
        } /* end for */
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* (Account for doubling root indirect block for rows 9-16 */
    for(u = 0; u < 8; u++) {
        state.man_size += DBLOCK_SIZE(fh, u + 8) * cparam->managed.width;
        state.man_free_space += DBLOCK_FREE(fh, u + 8) * cparam->managed.width;
    } /* end for */

    /* Row 9 (last direct block row) */
    obj_size = (size_t)DBLOCK_SIZE(fh, 8) / 2;
    for(v = 0; v < cparam->managed.width; v++) {
        state.man_alloc_size += DBLOCK_SIZE(fh, 8);
        if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
            TEST_ERROR
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Go back and backfill all root block's direct blocks */
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = (size_t)DBLOCK_FREE(fh, u) - ((size_t)DBLOCK_SIZE(fh, u) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
                TEST_ERROR
    } /* end for */


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_frag_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_frag_2nd_direct
 *
 * Purpose:	Test filling all direct blocks in root indirect block, then
 *              inserting small object to fit into each direct block
 *              in 2nd level indirect block, but not to share a block with
 *              another object.
 *              Then, go back and fill in the space in the blocks skipped.
 *
 *              Then, go back and remove all the objects
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 25, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_frag_2nd_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    num_first_indirect_rows;        /* Number of rows (of direct blocks) in each of the first indirect blocks */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "fill root direct blocks, then fragment 2nd level indirect block's direct blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Compute # of bits used in first row */
    num_first_indirect_rows = IBLOCK_MAX_DROWS(fh, 1);
#ifdef QAK
HDfprintf(stderr, "num_first_indirect_rows = %u\n", num_first_indirect_rows);
#endif /* QAK */

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    for(u = 0; u < num_first_indirect_rows; u++) {
        obj_size = (size_t)DBLOCK_SIZE(fh, u) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u);
            if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
                TEST_ERROR
        } /* end for */
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Go back and backfill all 2nd level indirect block's direct blocks */
    for(u = 0; u < num_first_indirect_rows; u++) {
        obj_size = (size_t)DBLOCK_FREE(fh, u) - ((size_t)DBLOCK_SIZE(fh, u) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
                TEST_ERROR
    } /* end for */


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_frag_2nd_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_man_frag_3rd_direct
 *
 * Purpose:	Test filling all direct blocks in root indirect block and
 *              all 2nd level indirect blocks, then
 *              inserting small object to fit into each direct block
 *              in 3rd level indirect block, but not to share a block with
 *              another object.
 *              Then, go back and fill in the space in the blocks skipped.
 *
 *              Then, go back and remove all objects
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, July 25, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_man_frag_3rd_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned    root_direct_rows;       /* Number of rows in root indirect block */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      fill_size;              /* Size of objects for "bulk" filled blocks */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "fill root direct blocks and 2nd level indirect blocks, then fragment 3rd level indirect block's direct blocks, then backfill and extend, then remove all objects %s";       /* Test description */
    unsigned    u, v;                   /* Local index variables */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, &fill_size) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR


    /* Compute # of direct rows in root indirect block */
    root_direct_rows = DTABLE_MAX_DROWS(fh);

    /* Fill direct blocks in root indirect block */
    if(fill_root_direct(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Fill all rows of 2nd level indirect blocks in root indirect block */
    if(fill_all_2nd_indirect_rows(fh, dxpl, fill_size, &state, &keep_ids))
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert objects small enough to fit into each direct block, but not to
     * share them with other objects of the same size.
     */
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = (size_t)DBLOCK_SIZE(fh, u) / 2;
        for(v = 0; v < cparam->managed.width; v++) {
            state.man_alloc_size += DBLOCK_SIZE(fh, u);
            if(add_obj(fh, dxpl, (size_t)10, obj_size, &state, &keep_ids))
                TEST_ERROR
        } /* end for */
    } /* end for */

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Go back and backfill all 3rd level indirect block's direct blocks */
    for(u = 0; u < root_direct_rows; u++) {
        obj_size = (size_t)DBLOCK_FREE(fh, u) - ((size_t)DBLOCK_SIZE(fh, u) / 2);
        for(v = 0; v < cparam->managed.width; v++)
            if(add_obj(fh, dxpl, (size_t)20, obj_size, &state, &keep_ids))
                TEST_ERROR
    } /* end for */


    /* Perform common file & heap close operations */
    if(close_heap(filename, fapl, dxpl, tparam, file, f, &fh, fh_addr, &state, &keep_ids, empty_size) < 0)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_man_frag_3rd_direct() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_huge_insert_one
 *
 * Purpose:	Test inserting one huge object in the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August  7, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_huge_insert_one(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert one huge object, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'huge' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size = obj_size;
    state.huge_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Remove object from heap */
        if(H5HF_remove(fh, dxpl, heap_id) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        state.huge_size = 0;
        state.huge_nobjs = 0;
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_huge_insert_one() */


/*-------------------------------------------------------------------------
 * Function:	test_huge_insert_two
 *
 * Purpose:	Test inserting two huge objects in the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_huge_insert_two(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for first object */
    unsigned char *heap_id2 = NULL;     /* Heap ID for second object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert two huge objects, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id2 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'huge' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size = obj_size;
    state.huge_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert second object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id2) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id2, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size ;
    state.huge_nobjs = 2;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in second huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id2, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        if(tparam->del_dir == FHEAP_DEL_FORWARD) {
            /* Remove first object from heap */
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size = obj_size;
            state.huge_nobjs = 1;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size = 0;
            state.huge_nobjs = 0;
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end if */
        else {
            /* Remove second object from heap */
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size = obj_size;
            state.huge_nobjs = 1;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove first object from heap */
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size = 0;
            state.huge_nobjs = 0;
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end else */
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(heap_id2);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(heap_id2);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_huge_insert_two() */


/*-------------------------------------------------------------------------
 * Function:	test_huge_insert_three
 *
 * Purpose:	Test inserting three huge objects in the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_huge_insert_three(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for first object */
    unsigned char *heap_id2 = NULL;     /* Heap ID for second object */
    unsigned char *heap_id3 = NULL;     /* Heap ID for third object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert three huge objects, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id2 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id3 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'huge' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert first object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size = obj_size;
    state.huge_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in first huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert second object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id2) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id2, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs = 2;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in second huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id2, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert third object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 3;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id3) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id3, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs = 3;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in third huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id3, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        if(tparam->del_dir == FHEAP_DEL_FORWARD) {
            /* Remove first object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove third object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id3) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end if */
        else {
            /* Remove third object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id3) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove first object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end else */
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(heap_id2);
    H5MM_xfree(heap_id3);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(heap_id2);
        H5MM_xfree(heap_id3);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_huge_insert_three() */


/*-------------------------------------------------------------------------
 * Function:	test_huge_insert_mix
 *
 * Purpose:	Test inserting a mix of 'normal' & 'huge' objects in the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, August 11, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_huge_insert_mix(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for first object */
    unsigned char *heap_id2 = NULL;     /* Heap ID for second object */
    unsigned char *heap_id3 = NULL;     /* Heap ID for third object */
    unsigned char *heap_id4 = NULL;     /* Heap ID for fourth object */
    unsigned char *heap_id5 = NULL;     /* Heap ID for fifth object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert mix of normal & huge objects, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id2 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id3 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id4 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id5 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'huge' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert first object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in first huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert second object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id2) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id2, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in second huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id2, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert third object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 3;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id3) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id3, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in third huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id3, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert fourth object small enough to fit into 'normal' heap blocks */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id4) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id4, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += DBLOCK_FREE(fh, 2) - obj_size;
    state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 2);
    state.man_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in fourth ('normal') object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id4, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id4, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert fifth object small enough to fit into 'normal' heap blocks */
    obj_size = (size_t)DBLOCK_SIZE(fh, 3) + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id5) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id5, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    /* (account for doubling of root indirect block) */
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 4);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 5);
    state.man_alloc_size += DBLOCK_SIZE(fh, 4);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    state.man_free_space += DBLOCK_FREE(fh, 4) - obj_size;
    state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 4);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 5);
    state.man_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in fifth ('normal') object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id5, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id5, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        if(tparam->del_dir == FHEAP_DEL_FORWARD) {
            /* Remove first object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove third object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id3) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove fourth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id4) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Remove fifth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id5) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR
        } /* end if */
        else {
            /* Remove fifth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id5) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Remove fourth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id4) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Reset 'managed' object statistics after they are all removed  */
            state.man_nobjs = 0;
            state.man_size = 0;
            state.man_alloc_size = 0;
            state.man_free_space = 0;

            /* Remove third object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id3) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove first object from heap */
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR
        } /* end else */

        /* Check up on heap... */
        HDmemset(&state, 0, sizeof(fheap_heap_state_t));
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(heap_id2);
    H5MM_xfree(heap_id3);
    H5MM_xfree(heap_id4);
    H5MM_xfree(heap_id5);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(heap_id2);
        H5MM_xfree(heap_id3);
        H5MM_xfree(heap_id4);
        H5MM_xfree(heap_id5);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_huge_insert_mix() */


/*-------------------------------------------------------------------------
 * Function:	test_filtered_huge
 *
 * Purpose:	Test storing 'huge' object in a heap with I/O filters
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, August 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_filtered_huge(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    deflate_level;          /* Deflation level */
    unsigned    old_actual_id_len = 0;  /* Old actual ID length */
    hbool_t     huge_ids_direct;        /* Are 'huge' objects directly acccessed? */
    const char *base_desc = "insert 'huge' object into heap with I/O filters, then remove %s";       /* Test description */

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));

    /* Set an I/O filter for heap data */
    deflate_level = 6;
    if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
        FAIL_STACK_ERROR

    /* Adjust actual ID length, if asking for IDs that can directly access 'huge' objects */
    if(cparam->id_len == 1) {
        old_actual_id_len = tparam->actual_id_len;
        tparam->actual_id_len = 29;   /* 1 + 8 (file address size) + 8 (file length size) + 4 (filter mask length) + 8 (object length size) */
    } /* end if */

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, &tmp_cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR


    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'huge' object's heap IDs are correct form */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR
    if(H5HF_get_huge_info_test(fh, NULL, &huge_ids_direct) < 0)
        FAIL_STACK_ERROR
    if(cparam->id_len == 1) {
        if(huge_ids_direct != TRUE)
            TEST_ERROR
    } /* end if */
    else
        if(tparam->actual_id_len >= 29) {
            if(huge_ids_direct != TRUE)
                TEST_ERROR
        } /* end if */
        else {
            if(huge_ids_direct != FALSE)
                TEST_ERROR
        } /* end else */

    /* Insert object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

/* QAK */
#ifdef QAK
    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR
#endif /* QAK */
/* QAK */

    /* Check up on heap... */
    state.huge_size = obj_size;
    state.huge_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Remove object from heap */
        if(H5HF_remove(fh, dxpl, heap_id) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        state.huge_size = 0;
        state.huge_nobjs = 0;
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu, file_size = %lu\n", (unsigned long)empty_size, (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Reset actual ID length, if asking for IDs that can directly access 'huge' objects */
    if(cparam->id_len == 1)
        tparam->actual_id_len = old_actual_id_len;

    /* Free resources */
    H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline); /* Release the I/O pipeline filter information */
    H5MM_xfree(heap_id);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_filtered_huge() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_tiny_insert_one
 *
 * Purpose:	Test inserting one tiny object in the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_tiny_insert_one(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert one tiny object, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'tiny' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert object small enough to encode in heap ID */
    obj_size = tparam->actual_id_len - 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_TINY)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.tiny_size = obj_size;
    state.tiny_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in tiny object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Remove object from heap */
        if(H5HF_remove(fh, dxpl, heap_id) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        state.tiny_size = 0;
        state.tiny_nobjs = 0;
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_tiny_insert_one() */


/*-------------------------------------------------------------------------
 * Function:	test_tiny_insert_two
 *
 * Purpose:	Test inserting two tiny objects in the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_tiny_insert_two(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for first object */
    unsigned char *heap_id2 = NULL;     /* Heap ID for second object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert two tiny objects, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id2 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'tiny' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert object small enough to encode in heap ID */
    obj_size = tparam->actual_id_len - 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_TINY)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.tiny_size = obj_size;
    state.tiny_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in tiny object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert second object small enough to encode in heap ID */
    obj_size = tparam->actual_id_len - 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id2) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id2, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_TINY)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.tiny_size += obj_size ;
    state.tiny_nobjs = 2;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in second tiny object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id2, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        if(tparam->del_dir == FHEAP_DEL_FORWARD) {
            /* Remove first object from heap */
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size = obj_size;
            state.tiny_nobjs = 1;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size = 0;
            state.tiny_nobjs = 0;
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end if */
        else {
            /* Remove second object from heap */
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size = obj_size;
            state.tiny_nobjs = 1;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove first object from heap */
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size = 0;
            state.tiny_nobjs = 0;
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end else */
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(heap_id2);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(heap_id2);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_tiny_insert_two() */


/*-------------------------------------------------------------------------
 * Function:	test_tiny_insert_mix
 *
 * Purpose:	Test inserting a mix of 'normal', 'huge' & 'tiny' objects in
 *              the heap
 *
 *              Then, remove all the objects, in various ways
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_tiny_insert_mix(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    size_t      id_len;                 /* Size of fractal heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    unsigned char *heap_id = NULL;      /* Heap ID for first object */
    unsigned char *heap_id2 = NULL;     /* Heap ID for second object */
    unsigned char *heap_id3 = NULL;     /* Heap ID for third object */
    unsigned char *heap_id4 = NULL;     /* Heap ID for fourth object */
    unsigned char *heap_id5 = NULL;     /* Heap ID for fifth object */
    unsigned char *heap_id6 = NULL;     /* Heap ID for sixth object */
    unsigned char *heap_id7 = NULL;     /* Heap ID for seventh object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    const char *base_desc = "insert mix of normal, huge & tiny objects, then remove %s";       /* Test description */

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Allocate heap ID(s) */
    if(NULL == (heap_id = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id2 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id3 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id4 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id5 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id6 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR
    if(NULL == (heap_id7 = H5MM_malloc(tparam->actual_id_len)))
        TEST_ERROR

    /* Make certain that 'tiny' object's heap IDs are correct size */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len != tparam->actual_id_len)
        TEST_ERROR

    /* Insert first object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in first huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on first huge object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert second object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id2) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id2, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in second huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id2, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on second huge object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id2, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert third object too large for managed heap blocks */
    obj_size = SMALL_STAND_SIZE + 3;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id3) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id3, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_HUGE)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.huge_size += obj_size;
    state.huge_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in third huge object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id3, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on third huge object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id3, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert fourth object small enough to fit into 'normal' heap blocks */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id4) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id4, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 1);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 2);
    state.man_alloc_size = DBLOCK_SIZE(fh, 2);
    state.man_free_space = cparam->managed.width * DBLOCK_FREE(fh, 0);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 1);
    state.man_free_space += DBLOCK_FREE(fh, 2) - obj_size;
    state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 2);
    state.man_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in fourth ('normal') object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id4, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id4, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on fourth ('normal') object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id4, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert fifth object small enough to fit into 'normal' heap blocks */
    obj_size = (size_t)DBLOCK_SIZE(fh, 3) + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id5) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id5, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    /* (account for doubling of root indirect block) */
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 3);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 4);
    state.man_size += cparam->managed.width * DBLOCK_SIZE(fh, 5);
    state.man_alloc_size += DBLOCK_SIZE(fh, 4);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 3);
    state.man_free_space += DBLOCK_FREE(fh, 4) - obj_size;
    state.man_free_space += (cparam->managed.width - 1) * DBLOCK_FREE(fh, 4);
    state.man_free_space += cparam->managed.width * DBLOCK_FREE(fh, 5);
    state.man_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in fifth ('normal') object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id5, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id5, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on fifth ('normal') object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id5, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR


    /* Insert sixth object small enough to encode in heap ID */
    obj_size = tparam->actual_id_len - 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id6) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id6, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_TINY)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.tiny_size = obj_size;
    state.tiny_nobjs = 1;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in tiny object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id6, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id6, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on sixth ('tiny') object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id6, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Insert seventh object small enough to encode in heap ID */
    obj_size = tparam->actual_id_len - 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id7) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id7, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_TINY)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Check up on heap... */
    state.tiny_size += obj_size;
    state.tiny_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in tiny object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id7, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id7, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Check 'op' functionality on seventh ('tiny') object */
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_op(fh, dxpl, heap_id7, op_memcpy, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        if(tparam->del_dir == FHEAP_DEL_FORWARD) {
            /* Remove first object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove third object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id3) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove fourth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id4) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Remove fifth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id5) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Reset 'managed' object statistics after they are all removed  */
            state.man_nobjs = 0;
            state.man_size = 0;
            state.man_alloc_size = 0;
            state.man_free_space = 0;

            /* Remove sixth object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id6, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id6) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size -= robj_size;
            state.tiny_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove seventh object from heap */
            if(H5HF_remove(fh, dxpl, heap_id7) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR
        } /* end if */
        else {
            /* Remove seventh object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id7, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id7) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size -= robj_size;
            state.tiny_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove sixth object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id6, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id6) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.tiny_size -= robj_size;
            state.tiny_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove fifth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id5) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Remove fourth ('normal') object from heap */
            if(H5HF_remove(fh, dxpl, heap_id4) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Reset 'managed' object statistics after they are all removed  */
            state.man_nobjs = 0;
            state.man_size = 0;
            state.man_alloc_size = 0;
            state.man_free_space = 0;

            /* Remove third object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id3, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id3) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove second object from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR

            /* Check up on heap... */
            state.huge_size -= robj_size;
            state.huge_nobjs--;
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Remove first object from heap */
            if(H5HF_remove(fh, dxpl, heap_id) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR
        } /* end else */

        /* Check up on heap... */
        HDmemset(&state, 0, sizeof(fheap_heap_state_t));
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */


    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    H5MM_xfree(heap_id);
    H5MM_xfree(heap_id2);
    H5MM_xfree(heap_id3);
    H5MM_xfree(heap_id4);
    H5MM_xfree(heap_id5);
    H5MM_xfree(heap_id6);
    H5MM_xfree(heap_id7);
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(heap_id);
        H5MM_xfree(heap_id2);
        H5MM_xfree(heap_id3);
        H5MM_xfree(heap_id4);
        H5MM_xfree(heap_id5);
        H5MM_xfree(heap_id6);
        H5MM_xfree(heap_id7);
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_tiny_insert_mix() */
#endif /* QAK */
#endif /* QAK2 */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_filtered_man_root_direct
 *
 * Purpose:	Test storing one 'managed' object in a heap with I/O filters
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, August 14, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_filtered_man_root_direct(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
#ifdef NOT_YET
    h5_stat_size_t       file_size;              /* Size of file currently */
#endif /* NOT_YET */
    unsigned char heap_id[HEAP_ID_LEN]; /* Heap ID for object */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    deflate_level;          /* Deflation level */
    const char *base_desc = "insert one 'managed' object into heap with I/O filters, then remove %s";       /* Test description */

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));

    /* Set an I/O filter for heap data */
    deflate_level = 6;
    if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
        FAIL_STACK_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, &tmp_cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR


    /* Insert object small enough to fit into direct heap block */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) / 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Check up on heap... */
    state.man_size = DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = DBLOCK_SIZE(fh, 0);
    state.man_free_space = DBLOCK_FREE(fh, 0) - obj_size;
    state.man_nobjs++;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in ('normal') object */
    if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Remove object from heap */
        if(H5HF_get_obj_len(fh, dxpl, heap_id, &robj_size) < 0)
            FAIL_STACK_ERROR
        if(H5HF_remove(fh, dxpl, heap_id) < 0)
            FAIL_STACK_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Check up on heap... */
        HDmemset(&state, 0, sizeof(fheap_heap_state_t));
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

/* Needs file free space to be persistent */
#ifdef NOT_YET
    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu, file_size = %lu\n", (unsigned long)empty_size, (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR
#endif /* NOT_YET */

    /* Free resources */
    H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline); /* Release the I/O pipeline filter information */

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_filtered_man_root_direct() */


/*-------------------------------------------------------------------------
 * Function:	test_filtered_man_root_indirect
 *
 * Purpose:	Test storing several objects in a 'managed heap with I/O filters
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, October 24, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_filtered_man_root_indirect(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
#ifdef NOT_YET
    h5_stat_size_t       file_size;              /* Size of file currently */
#endif /* NOT_YET */
    unsigned char heap_id1[HEAP_ID_LEN]; /* Heap ID for object #1 */
    unsigned char heap_id2[HEAP_ID_LEN]; /* Heap ID for object #2 */
    size_t      obj_size;               /* Size of object */
    size_t      robj_size;              /* Size of object read */
    unsigned char obj_type;             /* Type of storage for object */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    deflate_level;          /* Deflation level */
    const char *base_desc = "insert two 'managed' objects into heap with I/O filters, then remove %s";       /* Test description */

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));

    /* Set an I/O filter for heap data */
    deflate_level = 6;
    if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
        FAIL_STACK_ERROR

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, &tmp_cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Perform common test initialization operations */
    if(begin_test(tparam, base_desc, &keep_ids, NULL) < 0)
        TEST_ERROR


    /* Insert object #1, small enough to fit into direct heap block */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) / 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id1) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id1, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Check for closing & re-opening the heap */
    if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
        TEST_ERROR

    /* Insert object #2, small enough to fit into direct heap block */
    obj_size = (size_t)DBLOCK_SIZE(fh, 0) / 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, heap_id2) < 0)
        FAIL_STACK_ERROR
    if(H5HF_get_id_type_test(heap_id2, &obj_type) < 0)
        FAIL_STACK_ERROR
    if(obj_type != H5HF_ID_TYPE_MAN)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Check up on heap... */
    state.man_size = cparam->managed.width * DBLOCK_SIZE(fh, 0);
    state.man_alloc_size = 2 * DBLOCK_SIZE(fh, 0);
    state.man_free_space = (cparam->managed.width * DBLOCK_FREE(fh, 0)) - (obj_size * 2);
    state.man_nobjs += 2;
    if(check_stats(fh, &state))
        TEST_ERROR

    /* Read in ('normal') object #1 */
    if(H5HF_get_obj_len(fh, dxpl, heap_id1, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id1, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Read in ('normal') object #2 */
    if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
        FAIL_STACK_ERROR
    if(obj_size != robj_size)
        TEST_ERROR
    HDmemset(shared_robj_g, 0, obj_size);
    if(H5HF_read(fh, dxpl, heap_id2, shared_robj_g) < 0)
        FAIL_STACK_ERROR
    if(HDmemcmp(shared_wobj_g, shared_robj_g, obj_size))
        TEST_ERROR

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        if(tparam->del_dir == FHEAP_DEL_FORWARD) {
            /* Remove object #1 from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id1, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id1) < 0)
                FAIL_STACK_ERROR

            /* Close the fractal heap */
            if(H5HF_close(fh, dxpl) < 0)
                FAIL_STACK_ERROR
            fh = NULL;

            /* Close the file */
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            /* Re-open the file */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = H5I_object(file)))
                FAIL_STACK_ERROR

            /* Re-open the heap */
            if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
                FAIL_STACK_ERROR

            /* Remove object #2 from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Check up on heap... */
            HDmemset(&state, 0, sizeof(fheap_heap_state_t));
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Close the fractal heap */
            if(H5HF_close(fh, dxpl) < 0)
                FAIL_STACK_ERROR
            fh = NULL;

            /* Close the file */
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            /* Re-open the file */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = H5I_object(file)))
                FAIL_STACK_ERROR

            /* Re-open the heap */
            if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
                FAIL_STACK_ERROR

            /* Check up on heap... */
            HDmemset(&state, 0, sizeof(fheap_heap_state_t));
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end if */
        else {
            /* Remove object #2 from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id2, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id2) < 0)
                FAIL_STACK_ERROR

            /* Close the fractal heap */
            if(H5HF_close(fh, dxpl) < 0)
                FAIL_STACK_ERROR
            fh = NULL;

            /* Close the file */
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            /* Re-open the file */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = H5I_object(file)))
                FAIL_STACK_ERROR

            /* Re-open the heap */
            if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
                FAIL_STACK_ERROR

            /* Remove object #1 from heap */
            if(H5HF_get_obj_len(fh, dxpl, heap_id1, &robj_size) < 0)
                FAIL_STACK_ERROR
            if(H5HF_remove(fh, dxpl, heap_id1) < 0)
                FAIL_STACK_ERROR

            /* Check up on heap... */
            HDmemset(&state, 0, sizeof(fheap_heap_state_t));
            if(check_stats(fh, &state))
                TEST_ERROR

            /* Close the fractal heap */
            if(H5HF_close(fh, dxpl) < 0)
                FAIL_STACK_ERROR
            fh = NULL;

            /* Close the file */
            if(H5Fclose(file) < 0)
                FAIL_STACK_ERROR

            /* Re-open the file */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                FAIL_STACK_ERROR

            /* Get a pointer to the internal file object */
            if(NULL == (f = H5I_object(file)))
                FAIL_STACK_ERROR

            /* Re-open the heap */
            if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
                FAIL_STACK_ERROR

            /* Check up on heap... */
            HDmemset(&state, 0, sizeof(fheap_heap_state_t));
            if(check_stats(fh, &state))
                TEST_ERROR
        } /* end else */
    } /* end if */

    /* Close the fractal heap */
    if(H5HF_close(fh, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

/* Needs file free space to be persistent */
#ifdef NOT_YET
    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu, file_size = %lu\n", (unsigned long)empty_size, (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR
#endif /* NOT_YET */

    /* Free resources */
    H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline); /* Release the I/O pipeline filter information */

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_filtered_man_root_indirect() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_random
 *
 * Purpose:	Test inserting random sized objects into a heap, and read
 *              them back.
 *
 *              Then, go back and remove all objects
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May  9, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_random(hsize_t size_limit, hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned long seed = 0;             /* Random # seed */
    hsize_t     total_obj_added;        /* Size of objects added */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    size_t      obj_size;               /* Size of object */
    size_t      obj_loc;                /* Location of object in buffer */
    fheap_heap_state_t state;           /* State of fractal heap */
    size_t      u;                      /* Local index variable */

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));

    /* Check if we are compressing the blocks */
    if(tparam->comp == FHEAP_TEST_COMPRESS) {
        unsigned    deflate_level;          /* Deflation level */

        /* Set an I/O filter for heap data */
        deflate_level = 6;
        if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, &tmp_cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > MAX_HEAP_ID_LEN)
        TEST_ERROR

    /*
     * Display testing message
     */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        if(tparam->comp == FHEAP_TEST_COMPRESS)
            TESTING("inserting random-sized objects in heap with compressed blocks, then remove all objects (all - deleting heap)")
        else
            TESTING("inserting random-sized objects, then remove all objects (all - deleting heap)")
    } /* end if */
    else {
        if(tparam->comp == FHEAP_TEST_COMPRESS)
            TESTING("inserting random-sized objects in heap with compressed blocks, then remove all objects (all - random)")
        else
            TESTING("inserting random-sized objects, then remove all objects (all - random)")
    } /* end else */

    /* Choose random # seed */
    seed = (unsigned long)HDtime(NULL);
#ifdef QAK
/* seed = (unsigned long)1156158635; */
HDfprintf(stderr, "Random # seed was: %lu\n", seed);
#endif /* QAK */
    HDsrandom(seed);

    /* Loop over adding objects to the heap, until the size limit is reached */
    total_obj_added = 0;
    while(total_obj_added < size_limit) {
        /* Choose a random size of object (from 1 up to above standalone block size limit) */
        obj_size = (((uint32_t)HDrandom() % (tmp_cparam.max_man_size + 255)) + 1);
        obj_loc = (tmp_cparam.max_man_size + 255) - obj_size;

        /* Insert object */
        if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
            TEST_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Increment the amount of objects added */
        total_obj_added += obj_size;
    } /* end while */
#ifdef QAK
HDfprintf(stderr, "keep_ids.num_ids = %Zu, total_obj_added = %Hu, size_limit = %Hu\n", keep_ids.num_ids, total_obj_added, size_limit);
#endif /* QAK */

    /* Randomize the order of the IDs kept */
    for(u = 0; u < keep_ids.num_ids; u++) {
        size_t pos;             /* Position to swap with */

        /* Choose a position to swap with */
        /* (0 is current position) */
        pos = ((size_t)HDrandom() % (keep_ids.num_ids - u));

        /* If we chose a different position, swap with it */
        if(pos > 0) {
            unsigned char temp_id[MAX_HEAP_ID_LEN];         /* Temp. heap ID holder */

            /* Swap current position with future position */
            /* (just swap the heap ID, the len & offset isn't used */
            HDmemcpy(temp_id, &keep_ids.ids[u * id_len], id_len);
            HDmemcpy(&keep_ids.ids[u * id_len], &keep_ids.ids[(u + pos) * id_len], id_len);
            HDmemcpy(&keep_ids.ids[(u + pos) * id_len], temp_id, id_len);
        } /* end if */
    } /* end for */

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Delete objects inserted */
        for(u = 0; u < keep_ids.num_ids; u++) {
            /* Remove object from heap */
            if(H5HF_remove(fh, dxpl, &keep_ids.ids[id_len * u]) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR
        } /* end for */

        /* Check up on heap... */
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    if(tparam->comp == FHEAP_TEST_COMPRESS)
        H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline); /* Release the I/O pipeline filter information */

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    HDfprintf(stderr, "Random # seed was: %lu\n", seed);
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_random() */


/*-------------------------------------------------------------------------
 * Function:	test_random_pow2
 *
 * Purpose:	Test inserting random sized objects with a "power of 2
 *              distribution" (which favors small objects) into a heap,
 *              and read them back.
 *
 *              Then, go back and remove all objects
 *
 * Return:	Success:	0
 *
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, May 15, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_random_pow2(hsize_t size_limit, hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    unsigned long seed = 0;             /* Random # seed */
    hsize_t     total_obj_added;        /* Size of objects added */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    h5_stat_size_t       file_size;              /* Size of file currently */
    size_t      obj_size;               /* Size of object */
    size_t      obj_loc;                /* Location of object in buffer */
    fheap_heap_state_t state;           /* State of fractal heap */
    size_t      u;                      /* Local index variable */

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));

    /* Check if we are compressing the blocks */
    if(tparam->comp == FHEAP_TEST_COMPRESS) {
        unsigned    deflate_level;          /* Deflation level */

        /* Set an I/O filter for heap data */
        deflate_level = 6;
        if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, &tmp_cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > MAX_HEAP_ID_LEN)
        TEST_ERROR

    /*
     * Display testing message
     */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        if(tparam->comp == FHEAP_TEST_COMPRESS)
            TESTING("inserting random-sized objects with power of 2 distribution in heap with compressed blocks, then remove all objects (all - deleting heap)")
        else
            TESTING("inserting random-sized objects with power of 2 distribution, then remove all objects (all - deleting heap)")
    } /* end if */
    else {
        if(tparam->comp == FHEAP_TEST_COMPRESS)
            TESTING("inserting random-sized objects with power of 2 distribution in heap with compressed blocks, then remove all objects (all - random)")
        else
            TESTING("inserting random-sized objects with power of 2 distribution, then remove all objects (all - random)")
    } /* end else */

    /* Choose random # seed */
    seed = (unsigned long)HDtime(NULL);
#ifdef QAK
/* seed = (unsigned long)1155181717; */
HDfprintf(stderr, "Random # seed was: %lu\n", seed);
#endif /* QAK */
    HDsrandom(seed);

    /* Loop over adding objects to the heap, until the size limit is reached */
    total_obj_added = 0;
    while(total_obj_added < size_limit) {
        unsigned size_range = (tmp_cparam.managed.start_block_size / 8);       /* Object size range */

        /* Determine the size of the range for this object */
        /* (50% of the objects inserted will use the initial size range,
         *      25% of the objects will be twice as large, 12.5% will be
         *      four times larger, etc.)
         */
        while(HDrandom() < (RAND_MAX / 2) && size_range < tmp_cparam.max_man_size)
            size_range *= 2;
        if(size_range > (tmp_cparam.max_man_size + 255))
            size_range = tmp_cparam.max_man_size + 255;

        /* Choose a random size of object (from 1 up to stand alone block size) */
        obj_size = (((unsigned)HDrandom() % (size_range - 1)) + 1);
        obj_loc = (tmp_cparam.max_man_size + 255) - obj_size;

        /* Insert object */
        if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
            TEST_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Increment the amount of objects added */
        total_obj_added += obj_size;
    } /* end while */
#ifdef QAK
HDfprintf(stderr, "keep_ids.num_ids = %Zu, total_obj_added = %Hu, size_limit = %Hu\n", keep_ids.num_ids, total_obj_added, size_limit);
#endif /* QAK */

    /* Randomize the order of the IDs kept */
    for(u = 0; u < keep_ids.num_ids; u++) {
        size_t pos;             /* Position to swap with */

        /* Choose a position to swap with */
        /* (0 is current position) */
        pos = ((size_t)HDrandom() % (keep_ids.num_ids - u));

        /* If we chose a different position, swap with it */
        if(pos > 0) {
            unsigned char temp_id[MAX_HEAP_ID_LEN];         /* Temp. heap ID holder */

            /* Swap current position with future position */
            /* (just swap the heap ID, the len & offset isn't used */
            HDmemcpy(temp_id, &keep_ids.ids[u * id_len], id_len);
            HDmemcpy(&keep_ids.ids[u * id_len], &keep_ids.ids[(u + pos) * id_len], id_len);
            HDmemcpy(&keep_ids.ids[(u + pos) * id_len], temp_id, id_len);
        } /* end if */
    } /* end for */

    /* Delete individual objects, if we won't be deleting the entire heap later */
    if(tparam->del_dir != FHEAP_DEL_HEAP) {
        /* Delete objects inserted */
        for(u = 0; u < keep_ids.num_ids; u++) {
            /* Remove object from heap */
            if(H5HF_remove(fh, dxpl, &keep_ids.ids[id_len * u]) < 0)
                FAIL_STACK_ERROR

            /* Check for closing & re-opening the heap */
            if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
                TEST_ERROR
        } /* end for */

        /* Check up on heap... */
        if(check_stats(fh, &state))
            TEST_ERROR
    } /* end if */

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Check for deleting the entire heap */
    if(tparam->del_dir == FHEAP_DEL_HEAP) {
        /* Delete heap */
        if(H5HF_delete(f, dxpl, fh_addr) < 0)
            FAIL_STACK_ERROR
    } /* end if */

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR
#ifdef QAK
HDfprintf(stderr, "empty_size = %lu\n", (unsigned long)empty_size);
HDfprintf(stderr, "file_size = %lu\n", (unsigned long)file_size);
#endif /* QAK */

    /* Verify the file is correct size */
    if(file_size != empty_size)
        TEST_ERROR

    /* Free resources */
    if(tparam->comp == FHEAP_TEST_COMPRESS)
        H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline); /* Release the I/O pipeline filter information */

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    HDfprintf(stderr, "Random # seed was: %lu\n", seed);
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_random_pow2() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_write
 *
 * Purpose:	Test inserting objects, then changing the value for them.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Monday, December 18, 2006
 *
 *-------------------------------------------------------------------------
 */
/* Custom filter used to verify that the filters are actually called and do not
 * just silently fail */
static hbool_t test_write_filter_called;
static size_t test_write_filter(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
    const unsigned int UNUSED cd_values[], size_t nbytes, size_t UNUSED *buf_size,
    void UNUSED **buf)
{
    test_write_filter_called = TRUE;

    return nbytes;
} /* end link_filter_filter */

static int
test_write(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    H5HF_create_t tmp_cparam;           /* Local heap creation parameters */
    size_t      id_len;                 /* Size of fractal heap IDs */
    unsigned char tiny_heap_id[HEAP_ID_LEN]; /* Heap ID for 'tiny' object */
    unsigned char huge_heap_id[HEAP_ID_LEN]; /* Heap ID for 'huge' object */
    hbool_t     id_changed = FALSE;     /* Whether the heap ID changed */
    unsigned char *rewrite_obj = NULL;  /* Pointer to re-write buffer for objects */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      obj_loc;                /* Location of object in buffer */
    fheap_heap_state_t state;           /* State of fractal heap */
    unsigned    u;                      /* Local index variable */
    herr_t      ret;                    /* Generic return value */

    /*
     * Display testing message
     */
    if(tparam->comp == FHEAP_TEST_COMPRESS)
        TESTING("writing objects in heap with compressed blocks")
    else
        TESTING("writing objects in heap")

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Copy heap creation properties */
    HDmemcpy(&tmp_cparam, cparam, sizeof(H5HF_create_t));

    /* Check if we are compressing the blocks */
    if(tparam->comp == FHEAP_TEST_COMPRESS) {
        H5Z_class2_t filter_class;          /* Custom filter */
        unsigned    deflate_level;          /* Deflation level */

        /* Set an I/O filter for heap data */
        deflate_level = 6;
        if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_DEFLATE, H5Z_FLAG_OPTIONAL, (size_t)1, &deflate_level) < 0)
            FAIL_STACK_ERROR

        /* Register and append custom filter */
        filter_class.version = H5Z_CLASS_T_VERS;
        filter_class.id = H5Z_FILTER_RESERVED + 43;
        filter_class.encoder_present = TRUE;
        filter_class.decoder_present = TRUE;
        filter_class.name = "custom_fheap_filter";
        filter_class.can_apply = NULL;
        filter_class.set_local = NULL;
        filter_class.filter = test_write_filter;
        if(H5Zregister(&filter_class) < 0) TEST_ERROR
        if(H5Z_append(&tmp_cparam.pline, H5Z_FILTER_RESERVED + 43, 0, 0, NULL) < 0)
            FAIL_STACK_ERROR
        test_write_filter_called = FALSE;
    } /* end if */

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, &tmp_cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > MAX_HEAP_ID_LEN)
        TEST_ERROR


    /* Create 'tiny' and 'huge' objects */
    obj_size = id_len / 2;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, tiny_heap_id) < 0)
        FAIL_STACK_ERROR

    obj_size = tmp_cparam.max_man_size + 1;
    if(H5HF_insert(fh, dxpl, obj_size, shared_wobj_g, huge_heap_id) < 0)
        FAIL_STACK_ERROR

    /* Verify that writing to 'huge' objects works for un-filtered heaps */
    H5E_BEGIN_TRY {
        ret = H5HF_write(fh, dxpl, huge_heap_id, &id_changed, shared_wobj_g);
    } H5E_END_TRY;
    HDassert(!id_changed);
    if(tparam->comp == FHEAP_TEST_COMPRESS) {
        if(ret >= 0)
            TEST_ERROR
    } /* end if */
    else {
        if(ret < 0)
            FAIL_STACK_ERROR
    } /* end else */

    /* Verify that writing to 'tiny' objects return failure (for now) */
    H5E_BEGIN_TRY {
        ret = H5HF_write(fh, dxpl, tiny_heap_id, &id_changed, shared_wobj_g);
    } H5E_END_TRY;
    HDassert(!id_changed);
    if(ret >= 0)
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Verify that the custom filter has been applied for the huge object (if
     * applicable) */
    if(tparam->comp == FHEAP_TEST_COMPRESS) {
        if(!test_write_filter_called) TEST_ERROR
        test_write_filter_called = FALSE;
    } /* end if */


    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Initialize data to overwrite with */
    rewrite_obj = H5MM_malloc(shared_obj_size_g);
    for(u = 0; u < shared_obj_size_g; u++)
        rewrite_obj[u] = shared_wobj_g[u] * 2;

    /* Insert different sized objects, but stay out of "tiny" and "huge" zones */
    obj_size = 20;
    for(u = 0; u < 40; u++) {
        obj_loc = u;
        if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
            TEST_ERROR

        /* Check for closing & re-opening the heap */
        if(reopen_heap(f, dxpl, &fh, fh_addr, tparam) < 0)
            TEST_ERROR

        /* Overwrite data just written */
        if(H5HF_write(fh, dxpl, &keep_ids.ids[id_len * u], &id_changed, rewrite_obj) < 0)
            FAIL_STACK_ERROR
        HDassert(!id_changed);

        /* Read data back in */
        if(H5HF_read(fh, dxpl, &keep_ids.ids[id_len * u], shared_robj_g) < 0)
            FAIL_STACK_ERROR

        /* Compare data read in */
        if(HDmemcmp(rewrite_obj, shared_robj_g, obj_size))
            TEST_ERROR

        /* Change size of data to write */
        if(u < 20)
            obj_size *= 1.3;
        else
            obj_size /= 1.3;
    } /* end for */

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Verify that the custom filter has been applied to the managed objects (if
     * applicable) */
    if(tparam->comp == FHEAP_TEST_COMPRESS)
        if(!test_write_filter_called) TEST_ERROR


    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Verify changed objects */
    obj_size = 20;
    for(u = 0; u < 40; u++) {
        /* Read data back in */
        if(H5HF_read(fh, dxpl, &keep_ids.ids[id_len * u], shared_robj_g) < 0)
            FAIL_STACK_ERROR

        /* Compare data read in */
        if(HDmemcmp(rewrite_obj, shared_robj_g, obj_size))
            TEST_ERROR

        /* Change size of data to write */
        if(u < 20)
            obj_size *= 1.3;
        else
            obj_size /= 1.3;
    } /* end for */

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Free resources */
    if(tparam->comp == FHEAP_TEST_COMPRESS)
        H5O_msg_reset(H5O_PLINE_ID, &tmp_cparam.pline); /* Release the I/O pipeline filter information */

    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);
    H5MM_xfree(rewrite_obj);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        H5MM_xfree(rewrite_obj);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_write() */
#endif /* QAK */

#ifndef QAK

/*-------------------------------------------------------------------------
 * Function:	test_bug1
 *
 * Purpose:	Test inserting several objects, then deleting one and
 *              re-inserting an object, along with opening and closing
 *              the file.
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, November 28, 2006
 *
 *-------------------------------------------------------------------------
 */
static int
test_bug1(hid_t fapl, H5HF_create_t *cparam, fheap_test_param_t *tparam)
{
    hid_t	file = -1;              /* File ID */
    hid_t       dxpl = H5P_DATASET_XFER_DEFAULT;     /* DXPL to use */
    char	filename[FHEAP_FILENAME_LEN];         /* Filename to use */
    H5F_t	*f = NULL;              /* Internal file object pointer */
    H5HF_t      *fh = NULL;             /* Fractal heap wrapper */
    haddr_t     fh_addr;                /* Address of fractal heap */
    size_t      id_len;                 /* Size of fractal heap IDs */
    fheap_heap_ids_t keep_ids;          /* Structure to retain heap IDs */
    h5_stat_size_t       empty_size;             /* Size of a file with an empty heap */
    size_t      obj_size;               /* Size of object */
    size_t      obj_loc;                /* Location of object in buffer */
    fheap_heap_state_t state;           /* State of fractal heap */

    /*
     * Display testing message
     */
    TESTING("bug1: inserting several objects & removing one, then re-inserting")

    /* Initialize the heap ID structure */
    HDmemset(&keep_ids, 0, sizeof(fheap_heap_ids_t));

    /* Perform common file & heap open operations */
    if(open_heap(filename, fapl, dxpl, cparam, tparam, &file, &f, &fh, &fh_addr, &state, &empty_size) < 0)
        TEST_ERROR

    /* Get information about heap ID lengths */
    if(H5HF_get_id_len(fh, &id_len) < 0)
        FAIL_STACK_ERROR
    if(id_len > MAX_HEAP_ID_LEN)
        TEST_ERROR

    /* Insert objects */
    obj_size = 44;
    obj_loc = 1;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    obj_size = 484;
    obj_loc = 2;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    obj_size = 168;
    obj_loc = 3;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    obj_size = 96;
    obj_loc = 4;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    obj_size = 568;
    obj_loc = 5;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    obj_size = 568;
    obj_loc = 6;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR



    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Remove one of the objects */
    if(H5HF_remove(fh, dxpl, &keep_ids.ids[id_len * 4]) < 0)
        FAIL_STACK_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR


    /* Re-open the file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = H5I_object(file)))
        FAIL_STACK_ERROR

    /* Re-open the heap */
    if(NULL == (fh = H5HF_open(f, H5P_DATASET_XFER_DEFAULT, fh_addr)))
        FAIL_STACK_ERROR

    /* Insert another object */
    obj_size = 208;
    obj_loc = 6;
    if(add_obj(fh, dxpl, obj_loc, obj_size, NULL, &keep_ids))
        TEST_ERROR

    /* Close the fractal heap */
    if(H5HF_close(fh, dxpl) < 0)
        FAIL_STACK_ERROR
    fh = NULL;

    /* Close the file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR


    /* Free resources */
    H5MM_xfree(keep_ids.ids);
    H5MM_xfree(keep_ids.lens);
    H5MM_xfree(keep_ids.offs);

    /* All tests passed */
    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5MM_xfree(keep_ids.ids);
        H5MM_xfree(keep_ids.lens);
        H5MM_xfree(keep_ids.offs);
        if(fh)
            H5HF_close(fh, dxpl);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_bug1() */
#endif /* QAK */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the fractal heap code
 *
 * Return:	Success:
 *
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Friday, February 24, 2006
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    fheap_test_param_t tparam;          /* Testing parameters */
    H5HF_create_t small_cparam;         /* Creation parameters for "small" heap */
    H5HF_create_t large_cparam;         /* Creation parameters for "large" heap */
    hid_t	fapl = -1;              /* File access property list for data files */
    fheap_test_type_t curr_test;        /* Current test being worked on */
    unsigned    u;                      /* Local index variable */
    unsigned	nerrors = 0;            /* Cumulative error count */
    int		ExpressMode;            /* Express testing level */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    ExpressMode = GetTestExpress();
    if(ExpressMode > 1)
	printf("***Express test mode on.  Some tests may be skipped\n");

    /* Initialize heap creation parameters */
    init_small_cparam(&small_cparam);
    init_large_cparam(&large_cparam);

    /* Allocate space for the shared objects */
    shared_obj_size_g = large_cparam.max_man_size + 256;
    shared_wobj_g = H5MM_malloc(shared_obj_size_g);
    shared_robj_g = H5MM_malloc(shared_obj_size_g);

    /* Initialize the shared write buffer for objects */
    for(u = 0; u < shared_obj_size_g; u++)
        shared_wobj_g[u] = (unsigned char)u;

    /* Iterate over the testing parameters */
#ifndef QAK
    for(curr_test = FHEAP_TEST_NORMAL; curr_test < FHEAP_TEST_NTESTS; curr_test++) {
#else /* QAK */
HDfprintf(stderr, "Uncomment test loop!\n");
curr_test = FHEAP_TEST_NORMAL;
/* curr_test = FHEAP_TEST_REOPEN; */
#endif /* QAK */
        /* Clear the testing parameters */
        HDmemset(&tparam, 0, sizeof(fheap_test_param_t));
        tparam.actual_id_len = HEAP_ID_LEN;

        /* Set appropriate testing parameters for each test */
        switch(curr_test) {
            /* "Normal" testing parameters */
            case FHEAP_TEST_NORMAL:
                puts("Testing with normal parameters");
                break;

            /* "Re-open heap" testing parameters */
            case FHEAP_TEST_REOPEN:
                puts("Testing with reopen heap flag set");
                tparam.reopen_heap = FHEAP_TEST_REOPEN;
                break;

            /* An unknown test? */
            default:
                goto error;
        } /* end switch */

        /* Test fractal heap creation */
#ifndef QAK
        nerrors += test_create(fapl, &small_cparam, &tparam);
        nerrors += test_reopen(fapl, &small_cparam, &tparam);
        nerrors += test_open_twice(fapl, &small_cparam, &tparam);
        nerrors += test_delete_open(fapl, &small_cparam, &tparam);
        nerrors += test_id_limits(fapl, &small_cparam);
        nerrors += test_filtered_create(fapl, &small_cparam);
        nerrors += test_size(fapl, &small_cparam);
#ifndef H5_CANNOT_OPEN_TWICE
        nerrors += test_reopen_hdr(fapl, &small_cparam);
#endif /*H5_CANNOT_OPEN_TWICE*/
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK2
#ifndef QAK
        {
        fheap_test_fill_t fill;        /* Size of objects to fill heap blocks with */

#ifndef QAK2
        /* Filling with different sized objects */
        for(fill = FHEAP_TEST_FILL_LARGE; fill < FHEAP_TEST_FILL_N; fill++) {
#else /* QAK2 */
HDfprintf(stderr, "Uncomment test loop!\n");
fill = FHEAP_TEST_FILL_LARGE;
/* fill = FHEAP_TEST_FILL_SINGLE; */
#endif /* QAK2 */
            tparam.fill = fill;

            /* Set appropriate testing parameters for each test */
            switch(fill) {
                /* "Bulk fill" heap blocks with 'large' objects */
                case FHEAP_TEST_FILL_LARGE:
                    puts("Bulk-filling blocks w/large objects");
                    break;

                /* "Bulk fill" heap blocks with 'single' objects */
                case FHEAP_TEST_FILL_SINGLE:
                    puts("Bulk-filling blocks w/single object");
                    break;

                /* An unknown test? */
                default:
                    goto error;
            } /* end switch */

            /*
             * Test fractal heap managed object insertion
             */

            /* "Weird" sized objects */
            nerrors += test_man_insert_weird(fapl, &small_cparam, &tparam);

#ifdef ALL_INSERT_TESTS
            /* "Standard" sized objects, building from simple to complex heaps */
            nerrors += test_man_insert_first(fapl, &small_cparam, &tparam);
            nerrors += test_man_insert_second(fapl, &small_cparam, &tparam);
            nerrors += test_man_insert_root_mult(fapl, &small_cparam, &tparam);
            nerrors += test_man_insert_force_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_insert_fill_second(fapl, &small_cparam, &tparam);
            nerrors += test_man_insert_third_direct(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_first_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_start_second_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_second_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_start_third_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_fourth_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_all_root_direct(fapl, &small_cparam, &tparam);
            nerrors += test_man_first_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_second_direct_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_first_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_second_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_second_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_recursive_indirect_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_start_2nd_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_recursive_indirect_two_deep(fapl, &small_cparam, &tparam);
            nerrors += test_man_start_3rd_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_first_3rd_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_3rd_recursive_indirect_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_all_3rd_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_start_4th_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_first_4th_recursive_indirect(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_4th_recursive_indirect_row(fapl, &small_cparam, &tparam);
            nerrors += test_man_fill_all_4th_recursive_indirect(fapl, &small_cparam, &tparam);
#endif /* ALL_INSERT_TESTS */
            /* If this test fails, uncomment the tests above, which build up to this
             * level of complexity gradually. -QAK
             */
#ifndef QAK
            if(ExpressMode > 1)
                printf("***Express test mode on.  test_man_start_5th_recursive_indirect is skipped\n");
            else
                nerrors += test_man_start_5th_recursive_indirect(fapl, &small_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

            /*
             * Test fractal heap object deletion
             */
            /* Simple removal */
#ifndef QAK
            nerrors += test_man_remove_bogus(fapl, &small_cparam, &tparam);
            nerrors += test_man_remove_one(fapl, &small_cparam, &tparam);
            nerrors += test_man_remove_two(fapl, &small_cparam, &tparam);
            nerrors += test_man_remove_one_larger(fapl, &small_cparam, &tparam);
            tparam.del_dir = FHEAP_DEL_FORWARD;
            nerrors += test_man_remove_two_larger(fapl, &small_cparam, &tparam);
            tparam.del_dir = FHEAP_DEL_REVERSE;
            nerrors += test_man_remove_two_larger(fapl, &small_cparam, &tparam);
            tparam.del_dir = FHEAP_DEL_FORWARD;
            nerrors += test_man_remove_three_larger(fapl, &small_cparam, &tparam);
            tparam.del_dir = FHEAP_DEL_REVERSE;
            nerrors += test_man_remove_three_larger(fapl, &small_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
#ifndef QAK2
            {
            fheap_test_del_dir_t del_dir;        /* Deletion direction */
            fheap_test_del_drain_t drain_half;   /* Deletion draining */

            /* More complex removal patterns */
            for(del_dir = FHEAP_DEL_FORWARD; del_dir < FHEAP_DEL_NDIRS; del_dir++) {
                tparam.del_dir = del_dir;
                for(drain_half = FHEAP_DEL_DRAIN_ALL; drain_half < FHEAP_DEL_DRAIN_N; drain_half++) {
                    tparam.drain_half = drain_half;
#else /* QAK2 */
HDfprintf(stderr, "Uncomment test loops!\n");
/* tparam.del_dir = FHEAP_DEL_FORWARD; */
/* tparam.del_dir = FHEAP_DEL_REVERSE; */
tparam.del_dir = FHEAP_DEL_HEAP;
tparam.drain_half = FHEAP_DEL_DRAIN_ALL;
/* tparam.drain_half = FHEAP_DEL_DRAIN_HALF; */
#endif /* QAK2 */
                    /* Don't need to test deletion directions when deleting entire heap */
                    if(tparam.del_dir == FHEAP_DEL_HEAP && tparam.drain_half > FHEAP_DEL_DRAIN_ALL)
                        break;

#ifndef QAK
                    /* Simple insertion patterns */
                    nerrors += test_man_remove_root_direct(fapl, &small_cparam, &tparam);
                    nerrors += test_man_remove_two_direct(fapl, &small_cparam, &tparam);
                    nerrors += test_man_remove_first_row(fapl, &small_cparam, &tparam);
                    nerrors += test_man_remove_first_two_rows(fapl, &small_cparam, &tparam);
                    nerrors += test_man_remove_first_four_rows(fapl, &small_cparam, &tparam);
                    if(ExpressMode > 1)
                        printf("***Express test mode on.  Some tests skipped\n");
                    else {
                        nerrors += test_man_remove_all_root_direct(fapl, &small_cparam, &tparam);
                        nerrors += test_man_remove_2nd_indirect(fapl, &small_cparam, &tparam);
                        nerrors += test_man_remove_3rd_indirect(fapl, &small_cparam, &tparam);
                    } /* end else */
#endif /* QAK */

#ifndef QAK
                    /* Skip blocks insertion */
                    /* (covers insertion & deletion of skipped blocks) */
                    nerrors += test_man_skip_start_block(fapl, &small_cparam, &tparam);
                    nerrors += test_man_skip_start_block_add_back(fapl, &small_cparam, &tparam);
                    nerrors += test_man_skip_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_skip_2nd_block(fapl, &small_cparam, &tparam);
                    nerrors += test_man_skip_2nd_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_one_partial_skip_2nd_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_row_skip_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_skip_direct_skip_indirect_two_rows_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_direct_skip_indirect_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_direct_skip_2nd_indirect_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_2nd_direct_less_one_wrap_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_direct_skip_2nd_indirect_skip_2nd_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_direct_skip_indirect_two_rows_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_direct_skip_indirect_two_rows_skip_indirect_row_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_2nd_direct_skip_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_2nd_direct_skip_2nd_indirect_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_2nd_direct_fill_direct_skip2_3rd_indirect_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_3rd_direct_less_one_fill_direct_wrap_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    nerrors += test_man_fill_1st_row_3rd_direct_fill_2nd_direct_less_one_wrap_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    if(ExpressMode > 1)
                        printf("***Express test mode on.  Some tests skipped\n");
                    else {
                        nerrors += test_man_fill_3rd_direct_fill_direct_skip_start_block_add_skipped(fapl, &small_cparam, &tparam);
                        nerrors += test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_start_block_add_skipped(fapl, &small_cparam, &tparam);
                        nerrors += test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_two_rows_start_block_add_skipped(fapl, &small_cparam, &tparam);
                        nerrors += test_man_fill_3rd_direct_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(fapl, &small_cparam, &tparam);
                        nerrors += test_man_fill_4th_direct_less_one_fill_2nd_direct_fill_direct_skip_3rd_indirect_wrap_start_block_add_skipped(fapl, &small_cparam, &tparam);
                    } /* end else */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
                    /* Fragmented insertion patterns */
                    /* (covers insertion & deletion of fragmented blocks) */
                    nerrors += test_man_frag_simple(fapl, &small_cparam, &tparam);
                    nerrors += test_man_frag_direct(fapl, &small_cparam, &tparam);
                    nerrors += test_man_frag_2nd_direct(fapl, &small_cparam, &tparam);
                    nerrors += test_man_frag_3rd_direct(fapl, &small_cparam, &tparam);
#else /* QAK */
    HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifndef QAK2
                } /* end for */
            } /* end for */

            /* Reset deletion drain parameter */
            tparam.drain_half = FHEAP_DEL_DRAIN_ALL;

            } /* end block */
#endif /* QAK2 */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifndef QAK2
            } /* end for */
#endif /* QAK2 */
        } /* end block */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

        /*
         * Test fractal heap 'huge' & 'tiny' object insertion & deletion
         */
#ifndef QAK
        {
        fheap_test_del_dir_t del_dir;   /* Deletion direction */
        unsigned id_len;                /* Length of heap IDs */

        /* Test "normal" & "direct" storage of 'huge' & 'tiny' heap IDs */
        for(id_len = 0; id_len < 3; id_len++) {
            /* Set the ID length for this test */
            small_cparam.id_len = id_len;

            /* Print information about each test */
            switch(id_len) {
                /* Use "normal" form for 'huge' object's heap IDs */
                case 0:
                    puts("Using 'normal' heap ID format for 'huge' objects");
                    break;

                /* Use "direct" form for 'huge' object's heap IDs */
                case 1:
                    puts("Using 'direct' heap ID format for 'huge' objects");

                    /* Adjust actual length of heap IDs for directly storing 'huge' object's file offset & length in heap ID */
                    tparam.actual_id_len = 17;   /* 1 + 8 (file address size) + 8 (file length size) */
                    break;

                /* Use "direct" storage for 'huge' objects and larger IDs for 'tiny' objects */
                case 2:
                    small_cparam.id_len = 37;
                    puts("Using 'direct' heap ID format for 'huge' objects and larger IDs for 'tiny' objects");
                    tparam.actual_id_len = 37;
                    break;

                /* An unknown test? */
                default:
                    goto error;
            } /* end switch */

            /* Try several different methods of deleting objects */
            for(del_dir = FHEAP_DEL_FORWARD; del_dir < FHEAP_DEL_NDIRS; del_dir++) {
                tparam.del_dir = del_dir;

                /* Test 'huge' object insert & delete */
#ifndef QAK
                nerrors += test_huge_insert_one(fapl, &small_cparam, &tparam);
                nerrors += test_huge_insert_two(fapl, &small_cparam, &tparam);
                nerrors += test_huge_insert_three(fapl, &small_cparam, &tparam);
                nerrors += test_huge_insert_mix(fapl, &small_cparam, &tparam);
                nerrors += test_filtered_huge(fapl, &small_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
                /* Test 'tiny' object insert & delete */
                nerrors += test_tiny_insert_one(fapl, &small_cparam, &tparam);
                nerrors += test_tiny_insert_two(fapl, &small_cparam, &tparam);
                nerrors += test_tiny_insert_mix(fapl, &small_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
            } /* end for */
        } /* end for */

        /* Reset the "normal" heap ID lengths */
        small_cparam.id_len = 0;
        tparam.actual_id_len = HEAP_ID_LEN;
        } /* end block */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#else /* QAK2 */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK2 */

        /* Test I/O filter support */

#ifndef QAK
        /* Try several different methods of deleting objects */
        {
        fheap_test_del_dir_t del_dir;   /* Deletion direction */

        for(del_dir = FHEAP_DEL_FORWARD; del_dir < FHEAP_DEL_NDIRS; del_dir++) {
            tparam.del_dir = del_dir;

            /* Controlled tests */
/* XXX: Re-enable file size checks in these tests, after the file has persistent free space tracking working */
            nerrors += test_filtered_man_root_direct(fapl, &small_cparam, &tparam);
            nerrors += test_filtered_man_root_indirect(fapl, &small_cparam, &tparam);

            /* Random tests, with compressed blocks */
            tparam.comp = FHEAP_TEST_COMPRESS;
            nerrors += test_random((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(50*1000*1000) : (hsize_t)(25*1000*1000)), fapl, &small_cparam, &tparam);
            nerrors += test_random_pow2((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(50*1000*1000) : (hsize_t)(2*1000*1000)), fapl, &small_cparam, &tparam);

            /* Reset block compression */
            tparam.comp = FHEAP_TEST_NO_COMPRESS;
        } /* end for */
        } /* end block */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
        /* Random object insertion & deletion */
        if(ExpressMode > 1)
            printf("***Express test mode on.  Some tests skipped\n");
        else {
#ifndef QAK
            /* Random tests using "small" heap creation parameters */
            puts("Using 'small' heap creation parameters");

            /* (reduce size of tests when re-opening each time) */
/* XXX: Try to speed things up enough that these tests don't have to be reduced when re-opening */
            tparam.del_dir = FHEAP_DEL_FORWARD;
            nerrors += test_random((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(50*1000*1000)), fapl, &small_cparam, &tparam);
            nerrors += test_random_pow2((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(4*1000*1000)), fapl, &small_cparam, &tparam);

            tparam.del_dir = FHEAP_DEL_HEAP;
            nerrors += test_random((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(50*1000*1000)), fapl, &small_cparam, &tparam);
            nerrors += test_random_pow2((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(4*1000*1000)), fapl, &small_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
            /* Random tests using "large" heap creation parameters */
            puts("Using 'large' heap creation parameters");
            tparam.actual_id_len = LARGE_HEAP_ID_LEN;

            /* (reduce size of tests when re-opening each time) */
/* XXX: Try to speed things up enough that these tests don't have to be reduced when re-opening */
            tparam.del_dir = FHEAP_DEL_FORWARD;
            nerrors += test_random((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(50*1000*1000)), fapl, &large_cparam, &tparam);
            nerrors += test_random_pow2((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(4*1000*1000)), fapl, &large_cparam, &tparam);

            tparam.del_dir = FHEAP_DEL_HEAP;
            nerrors += test_random((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(50*1000*1000)), fapl, &large_cparam, &tparam);
            nerrors += test_random_pow2((curr_test == FHEAP_TEST_NORMAL ? (hsize_t)(100*1000*1000) : (hsize_t)(4*1000*1000)), fapl, &large_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

            /* Reset the "normal" heap ID length */
            tparam.actual_id_len = SMALL_HEAP_ID_LEN;
        } /* end else */
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

#ifndef QAK
        /* Test object writing support */

        /* Basic object writing */
        nerrors += test_write(fapl, &small_cparam, &tparam);

        /* Writing objects in heap with filters */
        tparam.comp = FHEAP_TEST_COMPRESS;
        nerrors += test_write(fapl, &small_cparam, &tparam);

        /* Reset block compression */
        tparam.comp = FHEAP_TEST_NO_COMPRESS;
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */
#ifndef QAK
    } /* end for */
#endif /* QAK */

    /* Tests that address specific bugs */
#ifndef QAK
    nerrors += test_bug1(fapl, &small_cparam, &tparam);
#else /* QAK */
HDfprintf(stderr, "Uncomment tests!\n");
#endif /* QAK */

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    puts("All fractal heap tests passed.");

    /* Release space for the shared objects */
    H5MM_xfree(shared_wobj_g);
    H5MM_xfree(shared_robj_g);
    H5MM_xfree(shared_ids_g);
    H5MM_xfree(shared_lens_g);
    H5MM_xfree(shared_offs_g);

    /* Clean up file used */
#ifndef QAK
    h5_cleanup(FILENAME, fapl);
#else /* QAK */
HDfprintf(stderr, "Uncomment cleanup!\n");
#endif /* QAK */

    return 0;

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5MM_xfree(shared_wobj_g);
        H5MM_xfree(shared_robj_g);
        H5MM_xfree(shared_ids_g);
        H5MM_xfree(shared_lens_g);
        H5MM_xfree(shared_offs_g);
	H5Pclose(fapl);
    } H5E_END_TRY;
    return 1;
} /* end main() */

