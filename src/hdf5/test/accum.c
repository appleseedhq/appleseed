/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
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

/* Programmer:  Mike McGreevy
 *              October 7, 2010
 */
#include "h5test.h"

#define H5F_PACKAGE
#include "H5Fpkg.h"
#include "H5FDprivate.h"
#include "H5Iprivate.h"

/* Filename */
#define FILENAME "accum.h5"

/* "big" I/O test values */
#define BIG_BUF_SIZE (6 * 1024 * 1024)

/* Random I/O test values */
#define RANDOM_BUF_SIZE (1 * 1024 * 1024)
#define MAX_RANDOM_SEGMENTS (5 * 1024)
#define RAND_SEG_LEN    (1024)
#define RANDOM_BASE_OFF (1024 * 1024)

/* Make file global to all tests */
H5F_t * f = NULL;

/* Function Prototypes */
unsigned test_write_read(void);
unsigned test_write_read_nonacc_front(void);
unsigned test_write_read_nonacc_end(void);
unsigned test_accum_overlap(void);
unsigned test_accum_overlap_clean(void);
unsigned test_accum_overlap_size(void);
unsigned test_accum_non_overlap_size(void);
unsigned test_accum_adjust(void);
unsigned test_read_after(void);
unsigned test_free(void);
unsigned test_big(void);
unsigned test_random_write(void);

/* Helper Function Prototypes */
void accum_printf(void);

/* Private Test H5Faccum Function Wrappers */
#define accum_write(a,s,b) H5F_block_write(f, H5FD_MEM_DEFAULT, (haddr_t)(a), (size_t)(s), H5P_DATASET_XFER_DEFAULT, (b))
#define accum_read(a,s,b)  H5F_block_read(f, H5FD_MEM_DEFAULT, (haddr_t)(a), (size_t)(s), H5P_DATASET_XFER_DEFAULT, (b))
#define accum_free(a,s)  H5F_accum_free(f, H5P_DATASET_XFER_DEFAULT, H5FD_MEM_DEFAULT, (haddr_t)(a), (hsize_t)(s))
#define accum_flush()    H5F_accum_flush(f, H5P_DATASET_XFER_DEFAULT)
#define accum_reset()    H5F_accum_reset(f, H5P_DATASET_XFER_DEFAULT, TRUE)

/* ================= */
/* Main Test Routine */
/* ================= */


/*-------------------------------------------------------------------------
 * Function:    main
 * 
 * Purpose:     Test the metadata accumulator code
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Mike McGreevy
 *              October 7, 2010
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    unsigned nerrors = 0;        /* track errors */
    hid_t fid = -1;

    /* Test Setup */
    puts("Testing the metadata accumulator");

    /* Create a test file */
    if((fid = H5Fcreate(FILENAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Get H5F_t * to internal file structure */
    if(NULL == (f = (H5F_t *)H5I_object(fid))) FAIL_STACK_ERROR

    /* We'll be writing lots of garbage data, so extend the
        file a ways. 10MB should do. */
    if(H5FD_set_eoa(f->shared->lf, H5FD_MEM_DEFAULT, (haddr_t)(1024*1024*10)) < 0) FAIL_STACK_ERROR

    /* Reset metadata accumulator for the file */
    if(accum_reset() < 0) FAIL_STACK_ERROR

    /* Test Functions */
    nerrors += test_write_read();
    nerrors += test_write_read_nonacc_front();
    nerrors += test_write_read_nonacc_end();
    nerrors += test_accum_overlap();
    nerrors += test_accum_overlap_clean();
    nerrors += test_accum_overlap_size();
    nerrors += test_accum_non_overlap_size();
    nerrors += test_accum_adjust();
    nerrors += test_read_after();
    nerrors += test_free();
    nerrors += test_big();
    nerrors += test_random_write();

    /* End of test code, close and delete file */
    if(H5Fclose(fid) < 0) TEST_ERROR
    HDremove(FILENAME);

    if(nerrors)
        goto error;
    puts("All metadata accumulator tests passed.");

    return 0;

error: 
    puts("*** TESTS FAILED ***");
    return 1;
} /* end main() */

/* ============================= */
/* Individual Unit Test Routines */
/* ============================= */


/*-------------------------------------------------------------------------
 * Function:    test_write_read
 * 
 * Purpose:     Simple test to write to then read from metadata accumulator.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Mike McGreevy
 *              October 7, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_write_read(void)
{
    int i = 0;
    int *write_buf, *read_buf;

    TESTING("simple write/read to/from metadata accumulator");

    /* Allocate buffers */
    write_buf = (int *)HDmalloc(1024 * sizeof(int));
    HDassert(write_buf);
    read_buf = (int *)HDcalloc(1024, sizeof(int));
    HDassert(read_buf);

    /* Fill buffer with data, zero out read buffer */
    for(i = 0; i < 1024; i++)
        write_buf[i] = i + 1;

    /* Do a simple write/read/verify of data */
    /* Write 1KB at Address 0 */
    if(accum_write(0, 1024, write_buf) < 0) FAIL_STACK_ERROR;
    if(accum_read(0, 1024, read_buf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(write_buf, read_buf, 1024) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(write_buf);
    HDfree(read_buf);

    return 0;

error:
    /* Release memory */
    HDfree(write_buf);
    HDfree(read_buf);

    return 1;
} /* test_write_read */ 


/*-------------------------------------------------------------------------
 * Function:    test_write_read_nonacc_front
 * 
 * Purpose:     Simple test to write to then read from before metadata accumulator.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Allen Byrne
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_write_read_nonacc_front(void)
{
    int i = 0;
    int *write_buf, *read_buf;

    TESTING("simple write/read to/from before metadata accumulator");

    /* Allocate buffers */
    write_buf = (int *)HDmalloc(2048 * sizeof(int));
    HDassert(write_buf);
    read_buf = (int *)HDcalloc(2048, sizeof(int));
    HDassert(read_buf);

    /* Fill buffer with data, zero out read buffer */
    for(i = 0; i < 2048; i++)
        write_buf[i] = i + 1;

    /* Do a simple write/read/verify of data */
    /* Write 1KB at Address 0 */
    if(accum_write(0, 1024, write_buf) < 0) FAIL_STACK_ERROR;
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_reset() < 0) FAIL_STACK_ERROR;
    if(accum_write(1024, 1024, write_buf) < 0) FAIL_STACK_ERROR;
    if(accum_read(0, 1024, read_buf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(write_buf, read_buf, 1024) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(write_buf);
    HDfree(read_buf);

    return 0;

error:
    /* Release memory */
    HDfree(write_buf);
    HDfree(read_buf);

    return 1;
} /* test_write_read */ 


/*-------------------------------------------------------------------------
 * Function:    test_write_read_nonacc_end
 * 
 * Purpose:     Simple test to write to then read from after metadata accumulator.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Allen Byrne
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_write_read_nonacc_end(void)
{
    int i = 0;
    int *write_buf, *read_buf;

    TESTING("simple write/read to/from after metadata accumulator");

    /* Allocate buffers */
    write_buf = (int *)HDmalloc(2048 * sizeof(int));
    HDassert(write_buf);
    read_buf = (int *)HDcalloc(2048, sizeof(int));
    HDassert(read_buf);

    /* Fill buffer with data, zero out read buffer */
    for(i = 0; i < 2048; i++)
        write_buf[i] = i + 1;

    /* Do a simple write/read/verify of data */
    /* Write 1KB at Address 0 */
    if(accum_write(1024, 1024, write_buf) < 0) FAIL_STACK_ERROR;
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_reset() < 0) FAIL_STACK_ERROR;
    if(accum_write(0, 1024, write_buf) < 0) FAIL_STACK_ERROR;
    if(accum_read(1024, 1024, read_buf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(write_buf, read_buf, 1024) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(write_buf);
    HDfree(read_buf);

    return 0;

error:
    /* Release memory */
    HDfree(write_buf);
    HDfree(read_buf);

    return 1;
} /* test_write_read */ 


/*-------------------------------------------------------------------------
 * Function:    test_free
 *
 * Purpose:     Simple test to free metadata accumulator.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Raymond Lu
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_free(void)
{
    int i = 0;
    int32_t *wbuf = NULL;
    int32_t *rbuf = NULL;
    int32_t *expect = NULL;

    TESTING("simple freeing metadata accumulator");

    /* Write and free the whole accumulator. */ 
    wbuf = (int32_t *)HDmalloc(256 * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int32_t *)HDmalloc(256 * sizeof(int32_t));
    HDassert(rbuf);
    expect = (int32_t *)HDmalloc(256 * sizeof(int32_t));
    HDassert(expect);

    /* Fill buffer with data */
    for(i = 0; i < 256; i++)
        wbuf[i] = (int32_t)(i + 1);

    if(accum_write(0, 256 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;

    if(accum_free(0, 256 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Free an empty accumulator */
    if(accum_free(0, 256 * 1024 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Write second quarter of the accumulator */
    if(accum_write(64 * sizeof(int32_t), 64 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;

    /* Free the second quarter of the accumulator, the requested area 
     * is bigger than the data region on the right side. */
    if(accum_free(64 * sizeof(int32_t), 65 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;


    /* Write half of the accumulator. */ 
    if(accum_write(0, 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;

    /* Free the first block of 4B */
    if(accum_free(0, sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(1 * sizeof(int32_t), 127 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf + 1, rbuf, 127 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Free the block of 4B at 127*4B */
    if(accum_free(127 * sizeof(int32_t), sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(1 * sizeof(int32_t), 126 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf + 1, rbuf, 126 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Free the block of 4B at 2*4B */
    if(accum_free(2 * sizeof(int32_t), sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(1 * sizeof(int32_t), 1 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf + 1, rbuf, 1 * sizeof(int32_t)) != 0) TEST_ERROR;
    if(accum_read(3 * sizeof(int32_t), 124 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf + 3, rbuf, 124 * sizeof(int32_t)) != 0) TEST_ERROR;


    /* Test freeing section that overlaps the start of the accumulator and is
     * entirely before dirty section */
    if(accum_write(64 * sizeof(int32_t), 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 64, wbuf, 128 * sizeof(int32_t));
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_write(68 * sizeof(int32_t), 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 68, wbuf, 4 * sizeof(int32_t));
    if(accum_free(62 * sizeof(int32_t), 4 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(66 * sizeof(int32_t), 126 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(expect + 66, rbuf, 126 * sizeof(int32_t)) != 0) TEST_ERROR;


    /* Test freeing section that overlaps the start of the accumulator and
     * completely contains dirty section */
    if(accum_write(64 * sizeof(int32_t), 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 64, wbuf, 128 * sizeof(int32_t));
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_write(68 * sizeof(int32_t), 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 68, wbuf, 4 * sizeof(int32_t));
    if(accum_free(62 * sizeof(int32_t), 16 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(78 * sizeof(int32_t), 114 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(expect + 78, rbuf, 114 * sizeof(int32_t)) != 0) TEST_ERROR;


    /* Test freeing section completely contained in accumulator and is entirely
     * before dirty section */
    if(accum_write(64 * sizeof(int32_t), 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 64, wbuf, 128 * sizeof(int32_t));
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_write(72 * sizeof(int32_t), 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 72, wbuf, 4 * sizeof(int32_t));
    if(accum_free(66 * sizeof(int32_t), 4 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(70 * sizeof(int32_t), 122 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(expect + 70, rbuf, 122 * sizeof(int32_t)) != 0) TEST_ERROR;


    /* Test freeing section completely contained in accumulator, starts before
     * dirty section, and ends in dirty section */
    if(accum_write(64 * sizeof(int32_t), 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 64, wbuf, 128 * sizeof(int32_t));
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_write(72 * sizeof(int32_t), 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 72, wbuf, 4 * sizeof(int32_t));
    if(accum_free(70 * sizeof(int32_t), 4 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(74 * sizeof(int32_t), 118 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(expect + 74, rbuf, 118 * sizeof(int32_t)) != 0) TEST_ERROR;


    /* Test freeing section completely contained in accumulator and completely
     * contains dirty section */
    if(accum_write(64 * sizeof(int32_t), 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 64, wbuf, 128 * sizeof(int32_t));
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_write(72 * sizeof(int32_t), 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 72, wbuf, 4 * sizeof(int32_t));
    if(accum_free(70 * sizeof(int32_t), 8 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(78 * sizeof(int32_t), 114 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(expect + 78, rbuf, 114 * sizeof(int32_t)) != 0) TEST_ERROR;


    /* Test freeing section completely contained in accumulator, starts at start
     * of dirty section, and ends in dirty section */
    if(accum_write(64 * sizeof(int32_t), 128 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 64, wbuf, 128 * sizeof(int32_t));
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    if(accum_write(72 * sizeof(int32_t), 8 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    HDmemcpy(expect + 72, wbuf, 8 * sizeof(int32_t));
    if(accum_free(72 * sizeof(int32_t), 4 * sizeof(int32_t)) < 0) FAIL_STACK_ERROR;

    /* Check that the accumulator still contains the correct data */
    if(accum_read(76 * sizeof(int32_t), 116 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(expect + 76, rbuf, 116 * sizeof(int32_t)) != 0) TEST_ERROR;

    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(expect);

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    return 0;

error:
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(expect);

    return 1;
} /* test_free */ 


/*-------------------------------------------------------------------------
 * Function:    test_accum_overlap
 * 
 * Purpose:     This test will write a series of pieces of data
 *              to the accumulator with the goal of overlapping
 *              the writes in various different ways.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Mike McGreevy
 *              October 7, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_accum_overlap(void)
{
    int i = 0;
    int32_t *wbuf, *rbuf;

    TESTING("overlapping write to metadata accumulator");

    /* Allocate buffers */
    wbuf = (int32_t *)HDmalloc(4096 * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int32_t *)HDcalloc(4096, sizeof(int32_t));
    HDassert(rbuf);

    /* Case 1: No metadata in accumulator */
    /* Write 10 1's at address 40 */
    /* @0:|          1111111111| */
    /* Put some data in the accumulator initially */
    for(i = 0; i < 10; i++)
        wbuf[i] = 1;
    if(accum_write(40, 10 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(40, 10 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 10 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 2: End of new piece aligns with start of accumulated data */
    /* Write 5 2's at address 20 */
    /* @0:|     222221111111111| */
    for(i = 0; i < 5; i++)
        wbuf[i] = 2;
    if(accum_write(20, 5 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(20, 5 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 5 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 3: Start of new piece aligns with start of accumulated data */
    /* Write 3 3's at address 20 */
    /* @0:|     333221111111111| */
    for(i = 0; i < 3; i++)
        wbuf[i] = 3;
    if(accum_write(20, 3 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(20, 3 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 3 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 4: New piece overlaps start of accumulated data */
    /* Write 5 4's at address 8 */
    /* @0:|  444443221111111111| */
    for(i = 0; i < 5; i++)
        wbuf[i] = 4;
    if(accum_write(8, 5 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(8, 5 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 5 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 5: New piece completely within accumulated data */
    /* Write 4 5's at address 48 */
    /* @0:|  444443221155551111| */
    for(i = 0; i < 4; i++)
        wbuf[i] = 5;
    if(accum_write(48, 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(48, 4 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 4 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 6: End of new piece aligns with end of accumulated data */
    /* Write 3 6's at address 68 */
    /* @0:|  444443221155551666| */
    for(i = 0; i < 3; i++)
        wbuf[i] = 6;
    if(accum_write(68, 3 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(68, 3 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 3 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 7: New piece overlaps end of accumulated data */
    /* Write 5 7's at address 76 */
    /* @0:|  4444432211555516677777| */
    for(i = 0; i < 5; i++)
        wbuf[i] = 7;
    if(accum_write(76, 5 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(76, 5 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 5 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 8: Start of new piece aligns with end of accumulated data */
    /* Write 3 8's at address 96 */
    /* @0:|  4444432211555516677777888| */
    for(i = 0; i < 3; i++)
        wbuf[i] = 8;
    if(accum_write(96, 3 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(96, 3 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 3 * sizeof(int32_t)) != 0) TEST_ERROR;
 
    /* Set up expected data buffer and verify contents of
        accumulator as constructed by cases 1-8, above */    
    for(i = 0; i < 5; i++)
        wbuf[i] = 4;
    for(i = 5; i < 6; i++)
        wbuf[i] = 3;
    for(i = 6; i < 8; i++)
        wbuf[i] = 2;
    for(i = 8; i < 10; i++)
        wbuf[i] = 1;
    for(i = 10; i < 14; i++)
        wbuf[i] = 5;
    for(i = 14; i < 15; i++)
        wbuf[i] = 1;
    for(i = 15; i < 17; i++)
        wbuf[i] = 6;
    for(i = 17; i < 22; i++)
        wbuf[i] = 7;
    for(i = 22; i < 25; i++)
        wbuf[i] = 8;
    if(accum_read(8, 25 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 25 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 9: New piece completely before accumulated data */
    /* Write 1 9 at address 0 */
    /* @0:|9 4444432211555516677777888| */
    for(i = 0; i < 1; i++)
        wbuf[i] = 9;
    if(accum_write(0, 1 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(0, 1 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 1 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 10: New piece completely after accumulated data */
    /* Write 4 3's at address 116 */
    /* @0:|9 4444432211555516677777888  3333| */
    for(i = 0; i < 4; i++)
        wbuf[i] = 3;
    if(accum_write(116, 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(116, 4 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 4 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 11: New piece completely overlaps accumulated data */
    /* Write 6 4's at address 112 */
    /* @0:|9 4444432211555516677777888 444444| */
    for(i = 0; i < 6; i++)
        wbuf[i] = 4;
    if(accum_write(112, 6 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(112, 6 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 6 * sizeof(int32_t)) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 1;
} /* test_accum_overlap */


/*-------------------------------------------------------------------------
 * Function:    test_accum_overlap_clean
 *
 * Purpose:     This test will write a series of pieces of data
 *              to the accumulator with the goal of overlapping
 *              the writes in various different ways, with clean
 *              areas in the accumulator.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Neil Fortner
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_accum_overlap_clean(void)
{
    int i = 0;
    int32_t *wbuf, *rbuf;

    TESTING("overlapping write to partially clean metadata accumulator");

    /* Allocate buffers */
    wbuf = (int32_t *)HDmalloc(4096 * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int32_t *)HDcalloc(4096, sizeof(int32_t));
    HDassert(rbuf);

    /* Case 1: No metadata in accumulator */
    /* Write 10 1's at address 40 */
    /* @0:|          1111111111| */
    /* Put some data in the accumulator initially */
    for(i = 0; i < 10; i++)
        wbuf[i] = 1;
    if(accum_write(40, 10 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(40, 10 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 10 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 2: End of new piece aligns with start of clean accumulated data */
    /* Write 5 2's at address 20 */
    /* @0:|     222221111111111| */
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    for(i = 0; i < 5; i++)
        wbuf[i] = 2;
    if(accum_write(20, 5 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(20, 5 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 5 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 3: Start of new piece aligns with start of accumulated data,
     * completely encloses dirty section of accumulator */
    /* Write 6 3's at address 20 */
    /* @0:|  333333111111111| */
    for(i = 0; i < 6; i++)
        wbuf[i] = 3;
    if(accum_write(20, 6 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(20, 6 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 6 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 4: New piece completely within accumulated data, overlaps
     * end of dirty section of accumulator */
    /* Write 2 4's at address 40 */
    /* @0:|  333334411111111| */
    for(i = 0; i < 2; i++)
        wbuf[i] = 4;
    if(accum_write(40, 2 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(40, 2 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 2 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 5: New piece completely within accumulated data, completely
     * after dirty section of accumulator */
    /* Write 2 5's at address 52 */
    /* @0:|  333334415511111| */
    for(i = 0; i < 2; i++)
        wbuf[i] = 5;
    if(accum_write(52, 2 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(52, 2 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 2 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 6: New piece completely within clean accumulated data */
    /* Write 3 6's at address 44 */
    /* @0:|  333334666511111| */
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    for(i = 0; i < 3; i++)
        wbuf[i] = 6;
    if(accum_write(44, 3 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(44, 3 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 3 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 7: New piece overlaps start of clean accumulated data */
    /* Write 2 7's at address 16 */
    /* @0:|  7733334666511111| */
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    for(i = 0; i < 2; i++)
        wbuf[i] = 7;
    if(accum_write(16, 2 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(16, 2 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 2 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 8: New piece overlaps start of accumulated data, completely
     * encloses dirty section of accumulator */
    /* Write 4 8's at address 12 */
    /* @0:|  88883334666511111| */
    for(i = 0; i < 4; i++)
        wbuf[i] = 8;
    if(accum_write(12, 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(12, 4 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 4 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 9: Start of new piece aligns with end of clean accumulated data */
    /* Write 3 9's at address 80 */
    /* @0:|  88883334666511111999| */
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    for(i = 0; i < 3; i++)
        wbuf[i] = 9;
    if(accum_write(80, 3 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(80, 3 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 3 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 10: New piece overlaps end of clean accumulated data */
    /* Write 3 2's at address 88 */
    /* @0:|  888833346665111119922| */
    if(accum_flush() < 0) FAIL_STACK_ERROR;
    for(i = 0; i < 2; i++)
        wbuf[i] = 2;
    if(accum_write(88, 2 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(88, 2 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 2 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 11: New piece overlaps end of accumulated data, completely encloses
     * dirty section of accumulator */
    /* Write 4 7's at address 84 */
    /* @0:|  8888333466651111197777| */
    for(i = 0; i < 4; i++)
        wbuf[i] = 7;
    if(accum_write(84, 4 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(84, 4 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 4 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Set up expected data buffer and verify contents of
        accumulator as constructed by cases 1-11, above */
    for(i = 0; i < 4; i++)
        wbuf[i] = 8;
    for(i = 4; i < 7; i++)
        wbuf[i] = 3;
    for(i = 7; i < 8; i++)
        wbuf[i] = 4;
    for(i = 8; i < 11; i++)
        wbuf[i] = 6;
    for(i = 11; i < 12; i++)
        wbuf[i] = 5;
    for(i = 12; i < 17; i++)
        wbuf[i] = 1;
    for(i = 17; i < 18; i++)
        wbuf[i] = 9;
    for(i = 18; i < 22; i++)
        wbuf[i] = 7;
    if(accum_read(12, 22 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 22 * sizeof(int32_t)) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 1;
} /* test_accum_overlap_clean */


/*-------------------------------------------------------------------------
 * Function:    test_accum_non_overlap_size
 * 
 * Purpose:     This test will write a series of pieces of data
 *              to the accumulator with the goal of not overlapping
 *              the writes with a data size larger then the accum size.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Allen Byrne
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_accum_non_overlap_size(void)
{
    int i = 0;
    int32_t *wbuf, *rbuf;

    TESTING("non-overlapping write to accumulator larger then accum_size");

    /* Allocate buffers */
    wbuf = (int *)HDmalloc(4096 * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int *)HDcalloc(4096, sizeof(int32_t));
    HDassert(rbuf);

    /* Case 1: No metadata in accumulator */
    /* Write 10 1's at address 140 */
    /* @0:|     1111111111| */
    /* Put some data in the accumulator initially */
    for(i = 0; i < 10; i++)
        wbuf[i] = 1;
    if(accum_write(140, 10 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(140, 10 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 10 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 9: New piece completely before accumulated data */
    /* Write 20 9 at address 0 */
    /* @0:|9   1111111111| */
    for(i = 0; i < 20; i++)
        wbuf[i] = 9;
    if(accum_write(0, 20 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(0, 20 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 20 * sizeof(int32_t)) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 1;
} /* test_accum_non_overlap_size */

/*-------------------------------------------------------------------------
 * Function:    test_accum_overlap_size
 *
 * Purpose:     This test will write a series of pieces of data
 *              to the accumulator with the goal of overlapping
 *              the writes with a data size completely overlapping 
 *              the accumulator at both ends.
 *
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 *
 * Programmer:  Allen Byrne
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_accum_overlap_size(void)
{
    int i = 0;
    int32_t *wbuf, *rbuf;

    TESTING("overlapping write to accumulator larger then accum_size");

    /* Allocate buffers */
    wbuf = (int32_t *)HDmalloc(4096 * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int32_t *)HDcalloc(4096, sizeof(int32_t));
    HDassert(rbuf);

    /* Case 1: No metadata in accumulator */
    /* Write 10 1's at address 64 */
    /* @0:|     1111111111| */
    /* Put some data in the accumulator initially */
    for(i = 0; i < 10; i++)
        wbuf[i] = 1;
    if(accum_write(64, 10 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(64, 10 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 10 * sizeof(int32_t)) != 0) TEST_ERROR;

    /* Case 9: New piece completely before accumulated data */
    /* Write 72 9 at address 60  */
    /* @0:|9   1111111111| */
    for(i = 0; i < 72; i++)
        wbuf[i] = 9;
    if(accum_write(60, 72 * sizeof(int32_t), wbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(60, 72 * sizeof(int32_t), rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 72 * sizeof(int32_t)) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 1;
} /* test_accum_overlap_size */


/*-------------------------------------------------------------------------
 * Function:    test_accum_adjust
 * 
 * Purpose:     This test examines the various ways the accumulator might
 *              adjust itself as a result of data appending or prepending
 *              to it.
 *
 *              This test program covers all the code in H5F_accum_adjust, 
 *              but NOT all possible paths through said code. It only covers
 *              six potential paths through the function. (Again, though, each
 *              piece of code within an if/else statement in H5F_accum_adjust is
 *              covered by one of the paths in this test function). Since there 
 *              are a ridiculous number of total possible paths through this 
 *              function due to its large number of embedded if/else statements,
 *              that's certainly a lot of different test cases to write by hand. 
 *              (Though if someone comes across this code and has some free 
 *              time, go for it).
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Mike McGreevy
 *              October 11, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned
test_accum_adjust(void)
{
    int i = 0;
    int s = 1048576;    /* size of buffer */
    int32_t *wbuf, *rbuf;

    TESTING("accumulator adjustments after append/prepend of data");

    /* Allocate buffers */
    wbuf = (int32_t *)HDmalloc((size_t)s * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int32_t *)HDcalloc((size_t)s, sizeof(int32_t));
    HDassert(rbuf);

    /* Fill up write buffer */
    for(i = 0; i < s; i++)
        wbuf[i] = i + 1;

    /* ================================================================ */
    /* CASE 1: Prepending small block to large, fully dirty accumulator */
    /* ================================================================ */

    /* Write data to the accumulator to fill it just under 1MB (max size),
     * but not quite full. This will force the accumulator to, on subsequent
     * writes, a) have to adjust since it's nearly full, and b) prevent
     * an increase in size because it's already at it's maximum size */
    if(accum_write((1024 * 1024), (1024 * 1024) - 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a small (1KB) block that prepends to the front of the accumulator. */ 
    /* ==> Accumulator will need more buffer space */
    /* ==> Accumulator will try to resize, but see that it's getting too big */
    /* ==> Size of new block is less than half maximum size of accumulator */
    /* ==> New block is being prepended to accumulator */
    /* ==> Accumulator is dirty, it will be flushed. */
    /* ==> Dirty region overlaps region to eliminate from accumulator */
    if(accum_write((1024 * 1024) - 1024, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read back and verify first write */
    if(accum_read((1024 * 1024), (1024 * 1024) - 1, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, (1024 * 1024) - 1) != 0) TEST_ERROR;

    /* Read back and verify second write */
    if(accum_read((1024 * 1024) - 1024, 1024, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 1024) != 0) TEST_ERROR;
    
    /* Reset accumulator for next case */
    if(accum_reset() < 0) FAIL_STACK_ERROR;

    /* ================================================================ */
    /* Case 2: Prepending large block to large, fully dirty accumulator */
    /* ================================================================ */

    /* Write data to the accumulator to fill it just under 1MB (max size),
     * but not quite full. This will force the accumulator to, on subsequent
     * writes, a) have to adjust since it's nearly full, and b) prevent
     * an increase in size because it's already at it's maximum size */
    if(accum_write((1024 * 1024), (1024 * 1024) - 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a large (just under 1MB) block to the front of the accumulator. */
    /* ==> Accumulator will need more buffer space */
    /* ==> Accumulator will try to resize, but see that it's getting too big */
    /* ==> Size of new block is larger than half maximum size of accumulator */
    /* ==> New block is being prepended to accumulator */
    /* ==> Accumulator is dirty, it will be flushed. */
    /* ==> Dirty region overlaps region to eliminate from accumulator */
    if(accum_write(5, (1024 * 1024) - 5, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read back and verify both pieces of data */
    if(accum_read(1048576, 1048575, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 1048576) != 0) TEST_ERROR;

    if(accum_read(5, 1048571, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 1048571) != 0) TEST_ERROR;

    /* Reset accumulator for next case */
    if(accum_reset() < 0) FAIL_STACK_ERROR;

    /* ========================================================= */
    /* Case 3: Appending small block to large, clean accumulator */
    /* ========================================================= */

    /* Write data to the accumulator to fill it just under 1MB (max size),
     * but not quite full. This will force the accumulator to, on subsequent
     * writes, a) have to adjust since it's nearly full, and b) prevent
     * an increase in size because it's already at it's maximum size */
    if(accum_write(0, (1024 * 1024) - 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Flush the accumulator -- we want to test the case when
        accumulator contains clean data */
    if(accum_flush() < 0) FAIL_STACK_ERROR

    /* Write a small (1KB) block to the end of the accumulator */
    /* ==> Accumulator will need more buffer space */
    /* ==> Accumulator will try to resize, but see that it's getting too big */
    /* ==> Size of new block is larger than half maximum size of accumulator */
    /* ==> New block being appended to accumulator */
    /* ==> Accumulator is NOT dirty */
    /* ==> Since we're appending, need to adjust location of accumulator */
    if(accum_write((1024 * 1024) - 1, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a piece of metadata outside current accumulator to force write
        to disk */
    if(accum_write(0, 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read in the piece we wrote to disk above, and then verify that 
        the data is as expected */
    if(accum_read((1024 * 1024) - 1, 1024, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 1024) != 0) TEST_ERROR;

    /* Reset accumulator for next case */
    if(accum_reset() < 0) FAIL_STACK_ERROR;

    /* ==================================================================== */
    /* Case 4: Appending small block to large, partially dirty accumulator, */
    /*         with existing dirty region NOT aligning with the new block   */
    /* ==================================================================== */

    /* Write data to the accumulator to fill it just under 1MB (max size),
     * but not quite full. This will force the accumulator to, on subsequent
     * writes, a) have to adjust since it's nearly full, and b) prevent
     * an increase in size because it's already at it's maximum size */
    if(accum_write(0, (1024 * 1024) - 5, wbuf) < 0) FAIL_STACK_ERROR;

    /* Flush the accumulator to clean it */
    if(accum_flush() < 0) FAIL_STACK_ERROR

    /* write to part of the accumulator so just the start of it is dirty */
    if(accum_write(0, 5, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a small (~340KB) piece of data to the other end of the accumulator */
    /* ==> Accumulator will need more buffer space */
    /* ==> Accumulator will try to resize, but see that it's getting too big */
    /* ==> Size of new block is less than than half maximum size of accumulator */
    /* ==> New block being appended to accumulator */
    /* ==> We can slide the dirty region down, to accomodate the request */
    /* ==> Max Buffer Size - (dirty offset + adjust size) >= 2 * size) */ 
    /* ==> Need to adjust location of accumulator while appending */
    /* ==> Accumulator will need to be reallocated */
    if(accum_write(1048571, 349523, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a piece of metadata outside current accumulator to force write
        to disk */
    if(accum_write(1398900, 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read in the piece we wrote to disk above, and then verify that 
        the data is as expected */
    if(accum_read(1048571, 349523, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 349523) != 0) TEST_ERROR;

    /* Reset accumulator for next case */
    if(accum_reset() < 0) FAIL_STACK_ERROR;

    /* ==================================================================== */
    /* Case 5: Appending small block to large, partially dirty accumulator, */
    /*         with existing dirty region aligning with new block           */
    /* ==================================================================== */

    /* Write data to the accumulator to fill it just under max size (but not full) */
    if(accum_write(0, (1024 * 1024) - 5, wbuf) < 0) FAIL_STACK_ERROR;

    /* Flush the accumulator to clean it */
    if(accum_flush() < 0) FAIL_STACK_ERROR

    /* write to part of the accumulator so it's dirty, but not entirely dirty */
    /* (just the begging few bytes will be clean) */
    if(accum_write(10, (1024 * 1024) - 15, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a small piece of data to the dirty end of the accumulator */
    /* ==> Accumulator will need more buffer space */
    /* ==> Accumulator will try to resize, but see that it's getting too big */
    /* ==> Size of new block is less than than half maximum size of accumulator */
    /* ==> New block being appended to accumulator */
    /* ==> We can slide the dirty region down, to accomodate the request */
    /* ==> Max Buffer Size - (dirty offset + adjust size) < 2 * size) */ 
    /* ==> Need to adjust location of accumulator while appending */
    if(accum_write((1024 * 1024) - 5, 10, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a piece of metadata outside current accumulator to force write
        to disk */
    if(accum_write(0, 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read in the piece we wrote to disk above, and then verify that 
        the data is as expected */
    if(accum_read((1024 * 1024) - 5, 10, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 10) != 0) TEST_ERROR;

    /* Reset accumulator for next case */
    if(accum_reset() < 0) FAIL_STACK_ERROR;

    /* ================================================================= */
    /* Case 6: Appending small block to large, fully dirty accumulator   */
    /* ================================================================= */

    /* Write data to the accumulator to fill it just under 1MB (max size),
     * but not quite full. This will force the accumulator to, on subsequent
     * writes, a) have to adjust since it's nearly full, and b) prevent
     * an increase in size because it's already at it's maximum size */
    if(accum_write(0, (1024 * 1024) - 5, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a small (~340KB) piece of data to the end of the accumulator */
    /* ==> Accumulator will need more buffer space */
    /* ==> Accumulator will try to resize, but see that it's getting too big */
    /* ==> Size of new block is less than than half maximum size of accumulator */
    /* ==> New block being appended to accumulator */
    /* ==> We cannot slide dirty region down, it's all dirty */
    /* ==> Dirty region overlaps region to eliminate from accumulator */
    /* ==> Need to adjust location of accumulator while appending */
    if(accum_write(1048571, 349523, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a piece of metadata outside current accumulator to force write
        to disk */
    if(accum_write(1398900, 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read in the piece we wrote to disk above, and then verify that 
        the data is as expected */
    if(accum_read(1048571, 349523, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 349523) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 1;
} /* test_accum_adjust */


/*-------------------------------------------------------------------------
 * Function:    test_read_after
 * 
 * Purpose:     This test will verify the case when metadata is read partly 
 *              from the accumulator and partly from disk.  The test will 
 *              write a block of data at address 512, force the data to be
 *              written to disk, write new data partially overlapping the 
 *              original block from below, then read data at address 512.  
 *              The data read should be partly new and partly original. 
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Larry Knox
 *              October 8, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned 
test_read_after(void)
{
    int i = 0;
    int s = 128;    /* size of buffer */
    int32_t *wbuf, *rbuf;

    TESTING("reading data from both accumulator and disk");

    /* Allocate buffers */
    wbuf = (int32_t *)HDmalloc((size_t)s * sizeof(int32_t));
    HDassert(wbuf);
    rbuf = (int32_t *)HDcalloc((size_t)s, sizeof(int32_t));
    HDassert(rbuf);

    /* Fill up write buffer with 1s */
    for(i = 0; i < s; i++)
        wbuf[i] = 1;

    /* Write data to the accumulator to fill it. */
    if(accum_write(512, 512, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write a piece of metadata outside current accumulator to force write
        to disk */
    if(accum_write(0, 1, wbuf) < 0) FAIL_STACK_ERROR;

    /* Fill up write buffer with 2s */
    for(i = 0; i < s; i++)
        wbuf[i] = 2;

    /* Write a block of 2s of the original size that will overlap the lower half
        of the original block */
    if(accum_write(256, 512, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read 128 bytes at the original address, and then  */ 
    if(accum_read(512, 512, rbuf) < 0) FAIL_STACK_ERROR;

    /* Set the second half of wbuf back to 1s */ 
    for(i = 64; i < s; i++)
        wbuf[i] = 1;

    /* Read in the piece we wrote to disk above, and then verify that 
        the data is as expected */
    if(accum_read(512, 512, rbuf) < 0) FAIL_STACK_ERROR;
    if(HDmemcmp(wbuf, rbuf, 128) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);

    return 1;
} /* end test_read_after */


/*-------------------------------------------------------------------------
 * Function:    test_big
 * 
 * Purpose:     This test exercises writing large pieces of metadata to the
 *		file.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Quincey Koziol
 *              October 12, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned 
test_big(void)
{
    uint8_t *wbuf, *wbuf2, *rbuf, *zbuf;        /* Buffers for reading & writing, etc */
    unsigned u;                         /* Local index variable */

    /* Allocate space for the write & read buffers */
    wbuf = (uint8_t *)HDmalloc(BIG_BUF_SIZE);
    HDassert(wbuf);
    wbuf2 = (uint8_t *)HDmalloc(BIG_BUF_SIZE);
    HDassert(wbuf2);
    rbuf = (uint8_t *)HDcalloc(BIG_BUF_SIZE + 1536, 1);
    HDassert(rbuf);
    zbuf = (uint8_t *)HDcalloc(BIG_BUF_SIZE + 1536, 1);
    HDassert(zbuf);

    /* Initialize write buffers */
    for(u = 0; u < BIG_BUF_SIZE; u++) {
        wbuf[u] = (uint8_t)u;
        wbuf2[u] = (uint8_t)(u + 1);
    } /* end for */

    TESTING("large metadata I/O operations");

    /* Write large data segment to file */
    if(accum_write(0, BIG_BUF_SIZE, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read entire segment back from file */
    if(accum_read(0, BIG_BUF_SIZE, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf, rbuf, BIG_BUF_SIZE) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(0, BIG_BUF_SIZE, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to middle of accumulator */
    if(accum_write(1024, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read entire segment back from file */
    /* (Read covers entire dirty region) */
    if(accum_read(0, BIG_BUF_SIZE, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(zbuf, rbuf, 1024) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf, rbuf + 1024, 1024) != 0) TEST_ERROR;
    if(HDmemcmp(zbuf, rbuf + 2048, (BIG_BUF_SIZE - 2048)) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(1024, 1024, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to overlap with end of "big" region */
    if(accum_write(BIG_BUF_SIZE - 512, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read entire segment back from file */
    /* (Read covers bottom half of dirty region) */
    if(accum_read(0, BIG_BUF_SIZE, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(zbuf, rbuf, (BIG_BUF_SIZE - 512)) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf, rbuf + (BIG_BUF_SIZE - 512), 512) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(BIG_BUF_SIZE - 512, 1024, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to overlap with beginning of "big" region */
    if(accum_write(0, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read entire segment back from file */
    /* (Read covers bottom half of dirty region) */
    if(accum_read(512, BIG_BUF_SIZE, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf + 512, rbuf, 512) != 0) TEST_ERROR;
    if(HDmemcmp(zbuf, rbuf + 512, (BIG_BUF_SIZE - 512)) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(0, 1024, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to middle of accumulator */
    /* (With write buffer #1) */
    if(accum_write(1024, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Write covers entire dirty region) */
    if(accum_write(0, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read entire segment back from file */
    if(accum_read(0, BIG_BUF_SIZE, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf2, rbuf, BIG_BUF_SIZE) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(0, BIG_BUF_SIZE, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to overlap with end of "big" region */
    /* (With write buffer #1) */
    if(accum_write(BIG_BUF_SIZE - 512, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Read covers bottom half of dirty region) */
    if(accum_write(0, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read both segments back from file */
    if(accum_read(0, BIG_BUF_SIZE + 512, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf2, rbuf, BIG_BUF_SIZE) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf + 512, rbuf + BIG_BUF_SIZE, 512) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(0, BIG_BUF_SIZE + 512, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE + 512);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to be past "big" region */
    /* (With write buffer #1) */
    if(accum_write(BIG_BUF_SIZE + 512, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read section before "big" region */
    /* (To enlarge accumulator, to it will intersect with big write) */
    if(accum_read(BIG_BUF_SIZE - 512, 1024, rbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Doesn't overlap with small section) */
    if(accum_write(0, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read both segments & gap back from file */
    if(accum_read(0, BIG_BUF_SIZE + 1024, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf2, rbuf, BIG_BUF_SIZE) != 0) TEST_ERROR;
    if(HDmemcmp(zbuf, rbuf + BIG_BUF_SIZE, 512) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf, rbuf + BIG_BUF_SIZE + 512, 512) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(0, BIG_BUF_SIZE + 1536, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE + 1024);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section to be past "big" region */
    /* (With write buffer #1) */
    if(accum_write(BIG_BUF_SIZE + 512, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read section before "big" region */
    /* (To enlarge accumulator, so it will intersect with big write) */
    if(accum_read(BIG_BUF_SIZE - 512, 1024, rbuf) < 0) FAIL_STACK_ERROR;
    if(accum_read(BIG_BUF_SIZE + 1536, 1024, rbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Overwriting dirty region, but not invalidating entire accumulator) */
    if(accum_write(1536, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read both segments & gap back from file */
    if(accum_read(0, BIG_BUF_SIZE + 1536, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(zbuf, rbuf, 1536) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf2, rbuf + 1536, BIG_BUF_SIZE) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(1536, BIG_BUF_SIZE, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE + 1536);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section before "big" region */
    /* (With write buffer #1) */
    if(accum_write(1024, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read section before "big" region */
    /* (To enlarge accumulator, so it will intersect with big write) */
    if(accum_read(0, 1024, rbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Overwriting dirty region, but not invalidating entire accumulator) */
    if(accum_write(512, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read both segments & gap back from file */
    if(accum_read(0, BIG_BUF_SIZE + 512, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(zbuf, rbuf, 512) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf2, rbuf + 512, BIG_BUF_SIZE) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(512, BIG_BUF_SIZE, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE + 512);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section before "big" region */
    /* (With write buffer #1) */
    if(accum_write(0, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read section before "big" region */
    /* (To enlarge accumulator, so it will intersect with big write) */
    if(accum_read(1024, 1024, rbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Avoiding dirty region, and not invalidating entire accumulator) */
    if(accum_write(1536, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read both segments & gap back from file */
    if(accum_read(0, BIG_BUF_SIZE + 1536, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf, rbuf, 1024) != 0) TEST_ERROR;
    if(HDmemcmp(zbuf, rbuf + 1024, 512) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf2, rbuf + 1536, BIG_BUF_SIZE) != 0) TEST_ERROR;


    /* Reset data in file back to zeros & reset the read buffer */
    if(accum_write(0, BIG_BUF_SIZE + 1536, zbuf) < 0) FAIL_STACK_ERROR;
    HDmemset(rbuf, 0, BIG_BUF_SIZE + 1536);
    if(accum_reset() < 0) FAIL_STACK_ERROR;


    /* Write small section before "big" region */
    /* (With write buffer #1) */
    if(accum_write(0, 1024, wbuf) < 0) FAIL_STACK_ERROR;

    /* Read section before "big" region */
    /* (To enlarge accumulator, so it will intersect with big write) */
    if(accum_read(1024, 1024, rbuf) < 0) FAIL_STACK_ERROR;

    /* Write entire segment to from file */
    /* (With write buffer #2) */
    /* (Partially overwriting dirty region, and not invalidating entire accumulator) */
    if(accum_write(512, BIG_BUF_SIZE, wbuf2) < 0) FAIL_STACK_ERROR;

    /* Read both segments back from file */
    if(accum_read(0, BIG_BUF_SIZE + 512, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read */
    if(HDmemcmp(wbuf, rbuf, 512) != 0) TEST_ERROR;
    if(HDmemcmp(wbuf2, rbuf + 512, BIG_BUF_SIZE) != 0) TEST_ERROR;


    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(wbuf2);
    HDfree(rbuf);
    HDfree(zbuf);

    return 0;

error:
    HDfree(wbuf);
    HDfree(wbuf2);
    HDfree(rbuf);
    HDfree(zbuf);

    return 1;
} /* end test_big() */


/*-------------------------------------------------------------------------
 * Function:    test_random_write
 * 
 * Purpose:     This test writes random pieces of data to the file and
 *		then reads it all back.
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Quincey Koziol
 *              October 11, 2010
 *
 *-------------------------------------------------------------------------
 */
unsigned 
test_random_write(void)
{
    uint8_t *wbuf, *rbuf;       /* Buffers for reading & writing */
    unsigned long seed = 0;     /* Random # seed */
    size_t *off;                /* Offset of buffer segments to write */
    size_t *len;                /* Size of buffer segments to write */
    size_t cur_off;             /* Current offset */
    size_t nsegments;           /* Number of segments to write */
    size_t swap;                /* Position to swap with */
    unsigned u;                 /* Local index variable */

    /* Allocate space for the write & read buffers */
    wbuf = (uint8_t *)malloc(RANDOM_BUF_SIZE);
    HDassert(wbuf);
    rbuf = (uint8_t *)calloc(RANDOM_BUF_SIZE, 1);
    HDassert(rbuf);

    /* Initialize write buffer */
    for(u = 0; u < RANDOM_BUF_SIZE; u++)
        wbuf[u] = (uint8_t)u;

    TESTING("random writes to accumulator");

    /* Choose random # seed */
    seed = (unsigned long)HDtime(NULL);
#ifdef QAK
/* seed = (unsigned long)1155438845; */
HDfprintf(stderr, "Random # seed was: %lu\n", seed);
#endif /* QAK */
    HDsrandom(seed);

    /* Allocate space for the segment length buffer */
    off = (size_t *)malloc(MAX_RANDOM_SEGMENTS * sizeof(size_t));
    HDassert(off);
    len = (size_t *)malloc(MAX_RANDOM_SEGMENTS * sizeof(size_t));
    HDassert(len);

    /* Randomly choose lengths of segments */
    cur_off = 0;
    for(u = 0; u < MAX_RANDOM_SEGMENTS; ) {
        size_t length = 0;      /* Length of current segment */

        /* Choose random length of segment, allowing for variance */
        do {
            length += (size_t)(HDrandom() % RAND_SEG_LEN) + 1;
        } while((HDrandom() & 256) >= 128); /* end while */

        /* Check for going off end of buffer */
        if((cur_off + length) > RANDOM_BUF_SIZE)
            length = RANDOM_BUF_SIZE - cur_off;

        /* Set offset & length of segment */
        off[u] = cur_off;
        len[u] = length;

        /* Advance array offset */
        u++;

        /* Advance current offset */
        cur_off += length;

        /* If we've used up entire buffer before hitting limit of segments, get out */
        if(cur_off >= RANDOM_BUF_SIZE)
            break;
    } /* end for */
    nsegments = u;

    /* Increase length of last segment, if it doesn't reach end of buffer */
    if(nsegments < MAX_RANDOM_SEGMENTS)
        len[nsegments - 1] = RANDOM_BUF_SIZE - off[nsegments - 1];

    /* Shuffle order of segments, to randomize positions to write */
    for(u = 0; u < nsegments; u++) {
        size_t tmp;        /* Temporary holder for offset & length values */

        /* Choose value within next few elements to to swap with */
        swap = ((size_t)HDrandom() % 8) + u;
        if(swap >= nsegments)
            swap = nsegments - 1;

        /* Swap values */
        tmp = off[u]; off[u] = off[swap]; off[swap] = tmp;
        tmp = len[u]; len[u] = len[swap]; len[swap] = tmp;
    } /* end for */

    /* Write data segments to file */
    for(u = 0; u < nsegments; u++) {
        if(accum_write(RANDOM_BASE_OFF + off[u], len[u], wbuf + off[u]) < 0) FAIL_STACK_ERROR;

        /* Verify individual reads */
        if(accum_read(RANDOM_BASE_OFF + off[u], len[u], rbuf) < 0) FAIL_STACK_ERROR;
        if(HDmemcmp(wbuf + off[u], rbuf, len[u]) != 0) TEST_ERROR;
    } /* end for */

    /* Read entire region back from file */
    if(accum_read(RANDOM_BASE_OFF, RANDOM_BUF_SIZE, rbuf) < 0) FAIL_STACK_ERROR;

    /* Verify data read back in */
    if(HDmemcmp(wbuf, rbuf, RANDOM_BUF_SIZE) != 0) TEST_ERROR;

    if(accum_reset() < 0) FAIL_STACK_ERROR;

    PASSED();

    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(off);
    HDfree(len);

    return 0;

error:
    /* Release memory */
    HDfree(wbuf);
    HDfree(rbuf);
    HDfree(off);
    HDfree(len);

    HDfprintf(stderr, "Random # seed was: %lu\n", seed);
    return 1;
} /* end test_random_write() */


/*-------------------------------------------------------------------------
 * Function:    accum_printf
 * 
 * Purpose:     Debug function to print some stats about the accumulator
 * 
 * Return:      Success: SUCCEED
 *              Failure: FAIL
 * 
 * Programmer:  Mike McGreevy
 *              October 7, 2010
 *
 *-------------------------------------------------------------------------
 */
void
accum_printf(void)
{
    H5F_meta_accum_t * accum = &f->shared->accum;

    printf("\n");
    printf("Current contents of accumulator:\n");
    if (accum->alloc_size == 0) {
        printf("=====================================================\n");
        printf(" No accumulator allocated.\n");
        printf("=====================================================\n");
    } else {
        printf("=====================================================\n");
        printf(" accumulator allocated size == %lu\n", (unsigned long)accum->alloc_size);
        printf(" accumulated data size      == %lu\n", (unsigned long)accum->size);
        printf(" accumulator dirty?         == %d\n", accum->dirty);
        printf("=====================================================\n");
        printf(" start of accumulated data, loc = %llu\n", accum->loc);
        if (accum->dirty) printf(" start of dirty region, loc = %llu\n", accum->loc + accum->dirty_off);
        if (accum->dirty) printf(" end of dirty region,   loc = %llu\n", accum->loc + accum->dirty_off + accum->dirty_len);
        printf(" end of accumulated data,   loc = %llu\n", accum->loc + accum->size);
        printf(" end of accumulator allocation,   loc = %llu\n", accum->loc + accum->alloc_size);
        printf("=====================================================\n");
    }
    printf("\n\n");
} /* accum_printf() */

