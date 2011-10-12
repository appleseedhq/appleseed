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
 *              Tuesday, May 3, 2005
 */
#include "h5test.h"

/*
 * This file needs to access private datatypes from the H5MP package.
 * This file also needs to access the memory pool testing code.
 */
#define H5MP_PACKAGE
#define H5MP_TESTING
#include "H5MPpkg.h"		/* Memory Pools				*/

/* Other private headers that this test requires */

/* Local macros */
#define MPOOL_PAGE_SIZE         H5MP_PAGE_SIZE_DEFAULT
#define MPOOL_FLAGS             H5MP_FLG_DEFAULT
#define MPOOL_NUM_NORMAL_BLOCKS 15
#define MPOOL_NORMAL_BLOCK      512
#define MPOOL_LARGE_BLOCK       (MPOOL_PAGE_SIZE * 3)
#define MPOOL_NUM_SMALL_BLOCKS  64
#define MPOOL_SMALL_BLOCK       1
#define MPOOL_NUM_RANDOM        10*1024
#define MPOOL_RANDOM_MAX_SIZE   (MPOOL_PAGE_SIZE * 2)


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Test trivial creating & closing memory pool
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 3, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_create(void)
{
    H5MP_pool_t *mp;            /* Memory pool */
    H5MP_page_t *page;          /* Memory pool page */
    size_t free_size;           /* Free size in pool */

    /*
     * Test memory pool creation
     */
    TESTING("memory pool creation");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Check free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != 0)
        TEST_ERROR

    /* Check first page */
    if(H5MP_get_pool_first_page(mp, &page) < 0)
        TEST_ERROR
    if(page != NULL)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_create() */


/*-------------------------------------------------------------------------
 * Function:	test_close_one
 *
 * Purpose:	Tests closing pool with one block allocated
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 6, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_close_one(void)
{
    H5MP_pool_t *mp;            /* Memory pool */

    /*
     * Test memory pool closing
     */
    TESTING("closing pool with blocks still allocated in one page");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    if(NULL == H5MP_malloc(mp, (size_t)MPOOL_NORMAL_BLOCK))
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_close_one() */


/*-------------------------------------------------------------------------
 * Function:	test_allocate_first
 *
 * Purpose:	Tests allocating first block in pool
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 3, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_allocate_first(void)
{
    H5MP_pool_t *mp;            /* Memory pool */
    H5MP_page_t *page;          /* Memory pool page */
    size_t free_size;           /* Free size in pool */
    void *spc;                  /* Pointer to space allocated */

    /*
     * Test memory pool allocation
     */
    TESTING("allocating first block in pool");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    if(NULL == (spc = H5MP_malloc(mp, (size_t)MPOOL_NORMAL_BLOCK)))
        TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t)) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Get first page */
    if(H5MP_get_pool_first_page(mp, &page) < 0)
        TEST_ERROR
    if(page == NULL)
        TEST_ERROR

    /* Check page's free space */
    if(H5MP_get_page_free_size(page, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t)) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Check next page */
    if(H5MP_get_page_next_page(page, &page) < 0)
        TEST_ERROR
    if(page != NULL)
        TEST_ERROR

    /* Free space in pool */
    H5MP_free(mp, spc);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t)))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    TESTING("allocating large first block in pool");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    if(NULL == (spc = H5MP_malloc(mp, (size_t)MPOOL_LARGE_BLOCK)))
        TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != 0)
        TEST_ERROR

    /* Get first page */
    if(H5MP_get_pool_first_page(mp, &page) < 0)
        TEST_ERROR
    if(page == NULL)
        TEST_ERROR

    /* Check page's free space */
    if(H5MP_get_page_free_size(page, &free_size) < 0)
        TEST_ERROR
    if(free_size != 0)
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Check next page */
    if(H5MP_get_page_next_page(page, &page) < 0)
        TEST_ERROR
    if(page != NULL)
        TEST_ERROR

    /* Free space in pool */
    H5MP_free(mp, spc);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_LARGE_BLOCK + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t)))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_allocate_first() */


/*-------------------------------------------------------------------------
 * Function:	test_allocate_split
 *
 * Purpose:	Tests allocating block in pool that requires splitting
 *              existing block
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 3, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_allocate_split(void)
{
    H5MP_pool_t *mp;            /* Memory pool */
    size_t free_size;           /* Free size in pool */
    void *spc1;                 /* Pointer to space allocated */
    void *spc2;                 /* Pointer to space allocated */

    /*
     * Test memory pool allocation
     */
    TESTING("splitting block in pool");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    if(NULL == (spc1 = H5MP_malloc(mp, (size_t)MPOOL_NORMAL_BLOCK)))
        TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t)) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Allocate more space in pool */
    if(NULL == (spc2 = H5MP_malloc(mp, (size_t)MPOOL_NORMAL_BLOCK)))
        TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (((H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t))) * 2) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Free first block in pool */
    H5MP_free(mp, spc1);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t)) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Free second block in pool (should merge with first block) */
    H5MP_free(mp, spc2);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t)))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_allocate_split() */


/*-------------------------------------------------------------------------
 * Function:	test_allocate_many_small
 *
 * Purpose:	Tests allocating many small blocks in a pool
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 6, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_allocate_many_small(void)
{
    H5MP_pool_t *mp;            /* Memory pool */
    size_t free_size;           /* Free size in pool */
    void *spc[MPOOL_NUM_SMALL_BLOCKS]; /* Pointers to space allocated */
    int i;                      /* Local index variable */

    /*
     * Test memory pool allocation
     */
    TESTING("allocating many small blocks");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    for(i = 0; i < MPOOL_NUM_SMALL_BLOCKS; i++)
        if(NULL == (spc[i] = H5MP_malloc(mp, (size_t)MPOOL_SMALL_BLOCK)))
            TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (((H5MP_BLOCK_ALIGN(MPOOL_SMALL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t))) * MPOOL_NUM_SMALL_BLOCKS) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Free blocks in pool */
    /* (Tests free block merging with block after it */
    for(i = (MPOOL_NUM_SMALL_BLOCKS - 1); i >= 0; i--)
        H5MP_free(mp, spc[i]);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t)))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_allocate_many_small() */


/*-------------------------------------------------------------------------
 * Function:	test_allocate_new_page
 *
 * Purpose:	Tests allocating block in pool that requires allocating
 *              new page
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 6, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_allocate_new_page(void)
{
    H5MP_pool_t *mp;            /* Memory pool */
    size_t free_size;           /* Free size in pool */
    size_t u;                   /* Local index variable */
    void *spc[MPOOL_NUM_NORMAL_BLOCKS]; /* Pointer to space allocated */
    void *spc1;                 /* Pointer to space allocated */
    void *spc2;                 /* Pointer to space allocated */

    /*
     * Test memory pool allocation
     */
    TESTING("allocate normal-sized block in new page");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    for(u = 0; u < MPOOL_NUM_NORMAL_BLOCKS; u++)
        if(NULL == (spc[u] = H5MP_malloc(mp, (size_t)MPOOL_NORMAL_BLOCK)))
            TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != (MPOOL_PAGE_SIZE * 3) - (((H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t))) * MPOOL_NUM_NORMAL_BLOCKS) + (H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t)) * 3)))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Free blocks in pool */
    /* (Free alternating blocks, in two passes, which tests block merging w/both neighbors) */
    for(u = 0; u < MPOOL_NUM_NORMAL_BLOCKS; u+=2)
        H5MP_free(mp, spc[u]);
    for(u = 1; u < MPOOL_NUM_NORMAL_BLOCKS; u+=2)
        H5MP_free(mp, spc[u]);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != ((MPOOL_PAGE_SIZE - H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))) * 3))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    TESTING("allocate large-sized block in new page");

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space in pool */
    /* (Normal sized block) */
    if(NULL == (spc1 = H5MP_malloc(mp, (size_t)MPOOL_NORMAL_BLOCK)))
        TEST_ERROR
    /* (Larger sized block) */
    if(NULL == (spc2 = H5MP_malloc(mp, (size_t)MPOOL_LARGE_BLOCK)))
        TEST_ERROR

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != MPOOL_PAGE_SIZE - (H5MP_BLOCK_ALIGN(MPOOL_NORMAL_BLOCK) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t)) + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Free blocks in pool */
    H5MP_free(mp, spc1);
    H5MP_free(mp, spc2);

    /* Check pool's free space */
    if(H5MP_get_pool_free_size(mp, &free_size) < 0)
        TEST_ERROR
    if(free_size != ((MPOOL_PAGE_SIZE - H5MP_BLOCK_ALIGN(sizeof(H5MP_page_t))) +
            MPOOL_LARGE_BLOCK + H5MP_BLOCK_ALIGN(sizeof(H5MP_page_blk_t))))
        TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_allocate_new_page() */


/*-------------------------------------------------------------------------
 * Function:	test_allocate_random
 *
 * Purpose:	Tests allocating random sized blocks in pool
 *
 * Return:	Success:	0
 *		Failure:	1
 *
 * Programmer:	Quincey Koziol
 *              Friday, May 6, 2005
 *
 *-------------------------------------------------------------------------
 */
static int
test_allocate_random(void)
{
    H5MP_pool_t *mp;            /* Memory pool */
    size_t u;                   /* Local index variable */
    time_t curr_time;           /* Current time, for seeding random number generator */
    size_t *blk_size = NULL;    /* Pointer to block sizes */
    void **spc = NULL;          /* Pointer to space allocated */
    size_t swap_idx;            /* Location to swap with when shuffling */
    void *swap_ptr;             /* Pointer to swap when shuffling */

    /*
     * Test memory pool allocation
     */
    TESTING("allocate many random sized blocks");

    /* Initialize random number seed */
    curr_time = HDtime(NULL);
#ifdef QAK
curr_time=1115412944;
HDfprintf(stderr,"curr_time=%lu\n",(unsigned long)curr_time);
#endif /* QAK */
    HDsrandom((unsigned)curr_time);

    /* Create a memory pool */
    if(NULL == (mp = H5MP_create((size_t)MPOOL_PAGE_SIZE, MPOOL_FLAGS)))
        TEST_ERROR

    /* Allocate space for the block sizes */
    if(NULL == (blk_size = (size_t *)HDmalloc(sizeof(size_t) * MPOOL_NUM_RANDOM)))
        TEST_ERROR

    /* Allocate space for the block pointers */
    if(NULL == (spc = (void **)HDmalloc(sizeof(void *) * MPOOL_NUM_RANDOM)))
        TEST_ERROR

    /* Initialize the block sizes with random values */
    for(u = 0; u < MPOOL_NUM_RANDOM; u++)
        blk_size[u] = (size_t)(HDrandom() % MPOOL_RANDOM_MAX_SIZE) + 1;

    /* Allocate space in pool */
    for(u = 0; u < MPOOL_NUM_RANDOM; u++)
        if(NULL == (spc[u] = H5MP_malloc(mp, blk_size[u])))
            TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Shuffle pointers to free */
    for(u = 0; u < MPOOL_NUM_RANDOM; u++) {
        swap_idx = (size_t)(HDrandom() % (MPOOL_NUM_RANDOM - u)) + u;
        swap_ptr = spc[u];
        spc[u] = spc[swap_idx];
        spc[swap_idx] = swap_ptr;
    } /* end for */

    /* Free blocks in pool */
    for(u = 0; u < MPOOL_NUM_RANDOM; u++)
        H5MP_free(mp, spc[u]);

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Initialize the block sizes with random values */
    for(u = 0; u < MPOOL_NUM_RANDOM; u++)
        blk_size[u] = (size_t)(HDrandom() % MPOOL_RANDOM_MAX_SIZE) + 1;

    /* Allocate space in pool (again) */
    /* (Leave allocated to test closing pool with many blocks still allocated) */
    for(u = 0; u < MPOOL_NUM_RANDOM; u++)
        if(NULL == (spc[u] = H5MP_malloc(mp, blk_size[u])))
            TEST_ERROR

    /* Check that free space totals match */
    if(H5MP_pool_is_free_size_correct(mp) <= 0)
        TEST_ERROR

    /* Close the memory pool */
    if(H5MP_close(mp) < 0)
        TEST_ERROR

    /* Free memory for block sizes & pointers */
    HDfree(blk_size);
    HDfree(spc);

    PASSED();

    return 0;

error:
    if(blk_size)
        HDfree(blk_size);
    if(spc)
        HDfree(spc);
    H5E_BEGIN_TRY {
        if(mp)
            H5MP_close(mp);
    } H5E_END_TRY;

    return 1;
} /* test_allocate_random() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Test the memory pool code
 *
 * Return:	Success:
 *		Failure:
 *
 * Programmer:	Quincey Koziol
 *              Tuesday, May 3, 2005
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    int nerrors = 0;

    /* Reset library */
    h5_reset();

    /* Test memory pool creation */
    nerrors += test_create();

    /* Test memory pool space closing */
    nerrors += test_close_one();

    /* Test memory pool space allocation */
    nerrors += test_allocate_first();
    nerrors += test_allocate_split();
    nerrors += test_allocate_many_small();
    nerrors += test_allocate_new_page();
    nerrors += test_allocate_random();

    if(nerrors)
        goto error;
    puts("All memory pool tests passed.");

    return 0;

error:
    puts("*** TESTS FAILED ***");
    return 1;
}

