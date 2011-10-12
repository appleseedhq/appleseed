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

/*
 *  Tests for file memory management consist of 3 parts:
 *	test_mf_eoa_*()	  tests for file meomory that interact with file allocation
 *	test_mf_fs_*() 	  tests for file memory that interact with the free-space manager
 *	test_mf_aggr_*()  tests for file memory that interact with the aggregators
 *	test_mf_align_*() tests for file memory with alignment setting
 */

#include "h5test.h"

#define H5MF_PACKAGE
#include "H5MFpkg.h"

#define H5FS_PACKAGE
#include "H5FSpkg.h"

#define H5F_PACKAGE
#define H5F_TESTING
#include "H5Fpkg.h"

#include "H5FLprivate.h"
#include "H5Iprivate.h"
#include "H5Vprivate.h"

#define FILENAME_LEN		1024

#define TEST_BLOCK_SIZE5	5
#define TEST_BLOCK_SIZE20	20
#define TEST_BLOCK_SIZE30	30
#define TEST_BLOCK_SIZE40	40
#define TEST_BLOCK_SIZE50	50
#define TEST_BLOCK_SIZE80	80
#define TEST_BLOCK_SIZE200 	200
#define TEST_BLOCK_SIZE600 	600
#define TEST_BLOCK_SIZE700 	700
#define TEST_BLOCK_SIZE1034 	1034
#define TEST_BLOCK_SIZE1970	1970
#define TEST_BLOCK_SIZE2058	2058
#define TEST_BLOCK_SIZE8000	8000
#define TEST_BLOCK_SIZE2048	2048

#define TEST_BLOCK_ADDR70	70
#define TEST_BLOCK_ADDR100	100

#define TEST_ALIGN1024		1024
#define TEST_ALIGN4096		4096

const char *FILENAME[] = {
    "mf",
    NULL
};

typedef enum {
    TEST_NORMAL,          	/* size of aggregator is >= alignment size */
    TEST_AGGR_SMALL,          	/* size of aggregator is smaller than alignment size */
    TEST_NTESTS           	/* The number of test types, must be last */
} test_type_t;

typedef struct frspace_state_t {
    hsize_t tot_space;          /* Total amount of space tracked              */
    hsize_t tot_sect_count;     /* Total # of sections tracked                */
    hsize_t serial_sect_count;  /* # of serializable sections tracked         */
    hsize_t ghost_sect_count;   /* # of un-serializable sections tracked      */
} frspace_state_t;


static int check_stats(const H5F_t *, const H5FS_t *, frspace_state_t *);
static unsigned test_mf_eoa(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_eoa_shrink(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_eoa_extend(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_tmp(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_fs_start(hid_t fapl);
static unsigned test_mf_fs_alloc_free(hid_t fapl);
static unsigned test_mf_fs_extend(hid_t fapl);
static unsigned test_mf_fs_absorb(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc1(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc2(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc3(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc4(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc5(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc6(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_alloc7(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_extend(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_aggr_absorb(const char *env_h5_drvr, hid_t fapl);
static unsigned test_mf_align_eoa(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_fs(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc1(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc2(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc3(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc4(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc5(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);
static unsigned test_mf_align_alloc6(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl);

/*
 * Verify statistics for the free-space manager
 */
static int
check_stats(const H5F_t *f, const H5FS_t *frsp, frspace_state_t *state)
{
    H5FS_stat_t frspace_stats;             /* Statistics about the heap */

    /* Get statistics for free-space and verify they are correct */
    if(H5FS_stat_info(f, frsp, &frspace_stats) < 0)
        FAIL_STACK_ERROR

    if(frspace_stats.tot_space != state->tot_space) {
        HDfprintf(stdout, "frspace_stats.tot_space = %Hu, state->tot_space = %Zu\n",
	    frspace_stats.tot_space, state->tot_space);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.tot_sect_count != state->tot_sect_count) {
        HDfprintf(stdout, "frspace_stats.tot_sect_count = %Hu, state->tot_sect_count = %Hu\n",
	    frspace_stats.tot_sect_count, state->tot_sect_count);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.serial_sect_count != state->serial_sect_count) {
        HDfprintf(stdout, "frspace_stats.serial_sect_count = %Hu, state->serial_sect_count = %Hu\n",
	    frspace_stats.serial_sect_count, state->serial_sect_count);
        TEST_ERROR
    } /* end if */
    if(frspace_stats.ghost_sect_count != state->ghost_sect_count) {
        HDfprintf(stdout, "frspace_stats.ghost_sect_count = %Hu, state->ghost_sect_count = %Hu\n",
	    frspace_stats.ghost_sect_count, state->ghost_sect_count);
        TEST_ERROR
    } /* end if */

    /* All tests passed */
    return(0);

error:
    return(1);
} /* check_stats() */

/*
 * To verify that blocks are allocated from file allocation
 *
 * Set up:
 * 	Turn off using meta/small data aggregator
 * 	There is nothing in free-space manager
 *
 * Allocate two blocks which should be from file allocation
 */
static unsigned
test_mf_eoa(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    hid_t		fapl_new = -1;		/* copy of fapl */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;      /* file size */
    H5FD_mem_t 		type;
    haddr_t		addr1, addr2;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MM_alloc() of file allocation");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Turn off using meta/small data aggregator */
        H5Pset_meta_block_size(fapl_new, (hsize_t)0);
        H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if (addr1 < (haddr_t)file_size)
            TEST_ERROR

        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if (addr2 < (haddr_t)file_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != (file_size+TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        if(H5Pclose(fapl_new) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support continuous address space");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_eoa() */

/*
 * To verify that an allocated block from file allocation is shrunk.
 *
 * Set up:
 * 	Turn off using meta/small data aggregator
 * 	There is nothing in free-space manager
 *
 *	Test 1: Allocate a block of 30 from file allocation
 * 		H5MF_try_shrink() the block by 30 : succeed
 *	Test 2: Allocate a block of 30 from file allocation
 * 		H5MF_try_shrink() the block by 20 : fail
 *	Test 3: Allocate a block of 30 from file allocation
 * 		H5MF_try_shrink() the block by 40 : fail
 *	Test 4: Allocate a block of 30 from file allocation
 * 		H5MF_try_shrink() the block by 20 from the end: succeed
 *
 */
static unsigned
test_mf_eoa_shrink(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    hid_t		fapl_new = -1;		/* copy of fapl */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t 		type;
    haddr_t		addr;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_shrink() of file allocation: test 1");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Turn off using meta/small data aggregator */
        H5Pset_meta_block_size(fapl_new, (hsize_t)0);
        H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        if (addr < (haddr_t)file_size)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != ma_size) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != (file_size+TEST_BLOCK_SIZE30))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* should succeed */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30) <= 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if(new_ma_addr != ma_addr)
            TEST_ERROR
        if(new_ma_size != ma_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of file allocation: test 2");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        if (addr < (haddr_t)file_size)
            TEST_ERROR

        /* should not succeed in shrinking */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30 - 10) > 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != ma_size) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TEST_BLOCK_SIZE30))
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */


    TESTING("H5MF_try_shrink() of file allocation: test 3");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* should not succeed in shrinking */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30 + 10) > 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != ma_size) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TEST_BLOCK_SIZE30))
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of file allocation: test 4");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* should succeed in shrinking */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr+10, (hsize_t)(TEST_BLOCK_SIZE30 - 10)) <= 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &ma_size);
        if(new_ma_addr != ma_addr)
            TEST_ERROR
        if(new_ma_size != ma_size)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + 10))
            TEST_ERROR

        if(H5Pclose(fapl_new) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_eoa_shrink() */

/*
 * To verify that an allocated block from file allocation is extended.
 *
 * Set up:
 * 	Turn off using meta/small data aggregator
 * 	There is nothing in free-space manager
 *
 * Test 1: Allocate a block of 30
 *	H5MF_try_extend() the block of size 30 by 50: succeed
 *
 * Test 2: Allocate a block of 30
 * 	H5MF_try_extend() the block of size 20 by 50: fail
 */
static unsigned
test_mf_eoa_extend(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              	/* File ID */
    hid_t		fapl_new = -1;			/* copy of fapl */
    char		filename[FILENAME_LEN]; 	/* Filename to use */
    H5F_t		*f = NULL;              	/* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;  	/* File size */
    H5FD_mem_t 		type;
    haddr_t		addr;
    htri_t      	extended;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_extend() of file allocation: test 1");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Turn off using meta/small data aggregator */
        if(H5Pset_meta_block_size(fapl_new, (hsize_t)0) < 0)
            FAIL_STACK_ERROR
        if(H5Pset_small_data_block_size(fapl_new, (hsize_t)0) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        if (addr < (haddr_t)file_size)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TEST_BLOCK_SIZE30))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* should succeed */
        extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)addr, (hsize_t)TEST_BLOCK_SIZE30, (hsize_t)TEST_BLOCK_SIZE50);

        if(extended <= 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != (file_size + TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50))
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_extend() of file allocation: test 2");

    /* Skip test when using VFDs that has different address spaces for each
     *  type of metadata allocation.
     */
    if(contig_addr_vfd) {
        /* Re-open the file with meta/small data setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        if(addr < (haddr_t)file_size)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if(new_ma_addr != ma_addr)
            TEST_ERROR

        extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)addr, (hsize_t)(TEST_BLOCK_SIZE30-10), (hsize_t)(TEST_BLOCK_SIZE50));

        /* should not succeed */
        if(extended > 0)
            TEST_ERROR

        /* nothing should be changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size + TEST_BLOCK_SIZE30)
            TEST_ERROR

        if(H5Pclose(fapl_new) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_eoa_extend() */

/*
 * To verify that temporary blocks are allocated correctly
 *
 * Set up:
 * 	There is nothing in free-space manager
 *
 * Tests:
 *      Allocate a reasonable-sized temporary block
 *      Check that the temporary address is high enough
 *      Check that file I/O with the temporary address fails
 *      Check that freeing a temporary address fails
 *      Check that closing the file doesn't change the file's size
 *      Check that overlapping normal & temporary address space fails:
 *         - Reopen the file
 *         - Allocate enough temporary space to use ~1/3 of the file
 *         - Allocate enough 'normal' space to use ~1/3 of the file
 *         - Check that allocating another 1/2 of the file as temporary address
 *              space fails
 *         - Check that allocating another 1/2 of the file as normal address
 *              space fails
 */
static unsigned
test_mf_tmp(const char *env_h5_drvr, hid_t fapl)
{
    hid_t	file = -1;              /* File ID */

    TESTING("'temporary' file space allocation");

    /* Can't run this test with multi-file VFDs */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        char		filename[FILENAME_LEN]; /* Filename to use */
        H5F_t		*f = NULL;              /* Internal file object pointer */
        h5_stat_size_t  file_size, new_file_size;      /* file size */
        haddr_t         maxaddr;                /* File's max. address */
        haddr_t		tmp_addr;               /* Temporary space file address */
        haddr_t		norm_addr;              /* Normal space file address */
        haddr_t		check_addr;             /* File address for checking for errors */
        unsigned char   buf = 0;                /* Buffer to read/write with */
        herr_t          status;                 /* Generic status value */

        /* Set the filename to use for this test */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR


        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Retrieve the file's maxaddr */
        if(H5F_get_maxaddr_test(file, &maxaddr) < 0)
            FAIL_STACK_ERROR

        /* Allocate some temporary address space */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc_tmp(f, (hsize_t)TEST_BLOCK_SIZE30)))
            FAIL_STACK_ERROR

        /* Check if temporary file address is valid */
        if(!H5F_IS_TMP_ADDR(f, tmp_addr))
            TEST_ERROR
        if(tmp_addr < (haddr_t)(maxaddr - TEST_BLOCK_SIZE30))
            TEST_ERROR

        /* Reading & writing with a temporary address value should fail */
        H5E_BEGIN_TRY {
            status = H5F_block_read(f, H5FD_MEM_SUPER, tmp_addr, sizeof(buf), H5P_DATASET_XFER_DEFAULT, &buf);
        } H5E_END_TRY;
        if(status >= 0)
            TEST_ERROR
        H5E_BEGIN_TRY {
            status = H5F_block_write(f, H5FD_MEM_SUPER, tmp_addr, sizeof(buf), H5P_DATASET_XFER_DEFAULT, &buf);
        } H5E_END_TRY;
        if(status >= 0)
            TEST_ERROR

        /* Freeing a temporary address value should fail */
        H5E_BEGIN_TRY {
            status = H5MF_xfree(f, H5FD_MEM_SUPER, H5P_DATASET_XFER_DEFAULT, tmp_addr, (hsize_t)TEST_BLOCK_SIZE30);
        } H5E_END_TRY;
        if(status >= 0)
            TEST_ERROR

        /* Close the file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR


        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate 1/3 of the file as temporary address space */
        if(HADDR_UNDEF == (tmp_addr = H5MF_alloc_tmp(f, (hsize_t)(maxaddr / 3))))
            FAIL_STACK_ERROR
        if(!H5F_IS_TMP_ADDR(f, tmp_addr))
            TEST_ERROR

        /* Allocate 1/3 of the file as normal address space */
        if(HADDR_UNDEF == (norm_addr = H5MF_alloc(f, H5FD_MEM_DRAW, H5P_DATASET_XFER_DEFAULT, (hsize_t)(maxaddr / 3))))
            FAIL_STACK_ERROR
        if(H5F_IS_TMP_ADDR(f, norm_addr))
            TEST_ERROR

        /* Test that pushing temporary space allocation into normal space fails */
        H5E_BEGIN_TRY {
            check_addr = H5MF_alloc_tmp(f, (hsize_t)(maxaddr / 3));
        } H5E_END_TRY;
        if(H5F_addr_defined(check_addr))
            TEST_ERROR

        /* Test that pushing normal space allocation into temporary space fails */
        H5E_BEGIN_TRY {
            check_addr = H5MF_alloc(f, H5FD_MEM_DRAW, H5P_DATASET_XFER_DEFAULT, (hsize_t)(maxaddr / 3));
        } H5E_END_TRY;
        if(H5F_addr_defined(check_addr))
            TEST_ERROR

        /* Free the normal block (so the file doesn't blow up to a huge size) */
        if(H5MF_xfree(f, H5FD_MEM_DRAW, H5P_DATASET_XFER_DEFAULT, norm_addr, (hsize_t)(maxaddr / 3)) < 0)
            FAIL_STACK_ERROR

        /* Close the file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if(new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support continuous address space");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_tmp() */

/*
 * To verify that the free-space manager is created or opened
 *
 * Set up:
 * 	Turn off using meta/small data aggregator
 */
static unsigned
test_mf_fs_start(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    hid_t		fapl_new = -1;		/* copy of fapl */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t 		type;
    frspace_state_t 	state;


    TESTING("H5MF_alloc_create()/H5MF_alloc_open() of free-space manager");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file  */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Turn off using meta/small data aggregator */
    H5Pset_meta_block_size(fapl_new, (hsize_t)0);
    H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Start up free-space manager */
    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(new_file_size != file_size)
	TEST_ERROR

    if(H5Pclose(fapl_new) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_start() */


/*
 * To verify that a block is allocated/freed from/to the free-space manager
 *
 * Set up:
 * 	Turn off using meta/small data aggregator
 *
 * Test 1:
 *	Add section A to free-space manager (addr=70, size=30)
 *	Allocate a block of size=30
 *	The returned space's address should be same as section A's address
 *	Deallocate the block which will be returned to the free-space manager
 * Test 2:
 *	Add section A to free-space manager (addr=70, size=30)
 *	Allocate a block of size=20
 *	The returned space's address should be same as section A's address
 *	There should still be space of 10 left in the free-space manager
 *	Deallocate the block which will be returned to free-space manager
 * Test 3:
 *	Add section A to free-space manager (addr=70, size=30)
 *	Allocate a block of size=40
 *	The free-space manager is unable to fulfill the request
 *	The block is allocated from file allocation
 *	Deallocate the block which will be returned to free-space manager
 *	(the space is shrunk and freed since it is at end of file)
 */
static unsigned
test_mf_fs_alloc_free(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    hid_t		fapl_new = -1;		/* copy of fapl */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; 	/* file size */
    H5FD_mem_t 		type;
    H5MF_free_section_t *sect_node = NULL;
    haddr_t		addr;
    frspace_state_t 	state;
    H5MF_sect_ud_t 	udata;
    H5FS_section_info_t *node;

    TESTING("H5MF_alloc()/H5MF_xfree() of free-space manager:test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Turn off using meta/small data aggregator */
    H5Pset_meta_block_size(fapl_new, (hsize_t)0);
    H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space */
    if (addr != TEST_BLOCK_ADDR70)
	TEST_ERROR

    state.tot_space -= TEST_BLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the block to free-space */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);

    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove section A from free-space */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
            (hsize_t)TEST_BLOCK_SIZE30, (H5FS_section_info_t **)&node) < 0)
	TEST_ERROR

    /* Free the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
	TEST_ERROR

    PASSED()

    TESTING("H5MF_alloc()/H5MF_xfree() of free-space manager:test 2");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 20 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE20));

    /* Verify that the allocated block is section A in free-space manager */
    if (addr != TEST_BLOCK_ADDR70)
	TEST_ERROR

    /* should still have 1 section of size 10 left in free-space manager */
    state.tot_space -= (TEST_BLOCK_SIZE20);

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the block to free-space manager */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)(TEST_BLOCK_SIZE20));

    /* Still 1 section in free-space because of merging */
    state.tot_space += TEST_BLOCK_SIZE20;
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove section A from free-space manager */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)TEST_BLOCK_SIZE30, (H5FS_section_info_t **)&node) < 0)
	FAIL_STACK_ERROR

    /* Free the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
	TEST_ERROR

    PASSED()

    TESTING("H5MF_alloc()/H5MF_xfree() of free-space manager:test 3");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /*
     * Allocate a block of 40.
     * Since free-space manager cannot fulfull the request,
     * the block is obtained from file allocation
     */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE40));

    /* Verify that the allocated block is not section A in free-space */
    if (addr == TEST_BLOCK_ADDR70)
	TEST_ERROR

    /* free-space info should be the same  */
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove section A from free-space */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)TEST_BLOCK_SIZE30, (H5FS_section_info_t **)&node) < 0)
	FAIL_STACK_ERROR

    /* Free the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the block of size 40 to free-space */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)(TEST_BLOCK_SIZE40));

    /*
     * Free-space info is the same.
     * The block is returned to free-space.
     * It is shrunk and freed because it is at end of file.
     */
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(new_file_size != file_size)
	TEST_ERROR

    if(H5Pclose(fapl_new) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_alloc_free() */


/*
 * To verify that a block allocated from the free-space manager can be extended
 *
 * Set up:
 * 	Turn off using meta/small data aggregator
 *
 * Test 1:
 *	Add section A to free-space manager: addr=70, size=30
 *	Allocate a block of size 30 from free-space manager
 *	Add section B to free-space manager: addr=100, size=50
 *	Try to extend the allocated block by requested-size=50
 *	Succeed: section A adjoins section B (70+30=100 which is section B's address) and
 *		 requested-size (50) is equal to the size of section B
 * Test 2:
 *	Add section A to free-space manager: addr=70, size=30
 *	Allocate a block of size 30 from free-space manager
 *	Add section B to free-space manager: addr=100, size=50
 *	Try to extend the allocated block by requested-size=60
 *	Fail: section A adjoins section B (70+30=100 which is section B's address) but
 *	      requested-size (60) > size of section B (50)
 *
 * Test 3:
 *	Add section A to free-space manager: addr=70, size=30
 *	Allocate a block of size 30 from free-space manager
 *	Add section B to free-space manager: addr=100, size=50
 *	Try to extend the allocated block by requested-size=40
 *	Succeed: section A adjoins section B (70+30=100 which is section B's address) and
 *		 requested-size (40) < size of section B (50), therefore,
 *		 a section of 10 is left in the free-space manager
 * Test 4:
 *	Add section A to free-space manager: addr=70, size=20
 *	Allocate a block of size 20 from free-space manager
 *	Add section B to free-space manager: addr=100, size=50
 * 	Try to extend the allocated block by 50 from the free-space_manager:
 *	Fail: section A does not adjoin section B (70+20 != address of section B) even though
 *	      the requested-size (50) equal to size of section B (50)
 */
static unsigned
test_mf_fs_extend(hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    hid_t		fapl_new = -1;		/* copy of fapl */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t 		type;
    H5MF_free_section_t *sect_node1 = NULL, *sect_node2=NULL;
    haddr_t		addr;
    frspace_state_t 	state;          	/* State of free space*/
    H5MF_sect_ud_t 	udata;
    H5FS_section_info_t *node;
    htri_t      	extended;

    TESTING("H5MF_try_extend() of free-space manager:test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    if((fapl_new = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of a file */
    if((file_size = h5_get_file_size(filename, fapl)) < 0)
        TEST_ERROR

    /* Turn off using meta/small data aggregator */
    H5Pset_meta_block_size(fapl_new, (hsize_t)0);
    H5Pset_small_data_block_size(fapl_new, (hsize_t)0);

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node1, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space manager */
    if (addr != TEST_BLOCK_ADDR70)
	TEST_ERROR

    state.tot_space -= TEST_BLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR100, (hsize_t)TEST_BLOCK_SIZE50);

    /* Add section B to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node2, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    state.tot_space += TEST_BLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30, (hsize_t)TEST_BLOCK_SIZE50);

    /* should succeed */
    if(extended <= 0)
	TEST_ERROR

    /* Section B is removed from free-space manager */
    state.tot_space -= TEST_BLOCK_SIZE50;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the extended block to free-space manager */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50));

    /* Verify that the extended block is back into free-space */
    state.tot_space += TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50;
    state.tot_sect_count = 1;
    state.serial_sect_count = 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove the extended block */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50), (H5FS_section_info_t **)&node) < 0)
	TEST_ERROR

    /* Remove the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
	TEST_ERROR

    PASSED()

    TESTING("H5MF_try_extend() of free-space manager:test 2");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node1, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space manager */
    if (addr != TEST_BLOCK_ADDR70)
	TEST_ERROR

    state.tot_space -= TEST_BLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR100, (hsize_t)TEST_BLOCK_SIZE50);

    /* Add section B to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node2, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    state.tot_space += TEST_BLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30, (hsize_t)(TEST_BLOCK_SIZE50+10));

    /* Should not be able to extend the allocated block */
    if(extended)
	TEST_ERROR

    /* free-space info should remain the same */
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the allocated block A to free-space */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);

    /* the returned section A is merged with section B in free-space */
    /* rest of the info remains the same */
    state.tot_space += TEST_BLOCK_SIZE30;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove the merged sections A & B from free-space */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50), (H5FS_section_info_t **)&node) < 0)
	TEST_ERROR

    /* Remove the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
	TEST_ERROR

    PASSED()

    TESTING("H5MF_try_extend() of free-space manager:test 3");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node1, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE30;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 30 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

    /* Verify that the allocated block is section A in free-space manager */
    if (addr != TEST_BLOCK_ADDR70)
	TEST_ERROR

    state.tot_space -= TEST_BLOCK_SIZE30;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR100, (hsize_t)TEST_BLOCK_SIZE50);

    /* Add section B to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node2, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    state.tot_space += TEST_BLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE30, (hsize_t)(TEST_BLOCK_SIZE40));

    /* Should succeed in extending the allocated block */
    if(extended <=0)
	TEST_ERROR

    /* Should have 1 section of size=10 left in free-space manager */
    state.tot_space -= (TEST_BLOCK_SIZE40);
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the extended block  */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE40));

    /* rest info is same, the extended section returned is merged with the section in free-space */
    state.tot_space += (TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE40);

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove the merged sections A & B from free-space */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50), (H5FS_section_info_t **)&node) < 0)
	TEST_ERROR

    /* Remove the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if (new_file_size != file_size)
	TEST_ERROR

    PASSED()

    TESTING("H5MF_try_extend() of free-space manager:test 4");

    /* Re-open the file with meta/small data setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl_new)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    /* Create section A */
    sect_node1 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)(TEST_BLOCK_SIZE30-10));

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A of size=20 to free-space */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node1, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += (TEST_BLOCK_SIZE30-10);
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of size=20 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE30-10));

    /* Verify that the allocated block is section A in free-space manager */
    if (addr != TEST_BLOCK_ADDR70)
	TEST_ERROR

    state.tot_space -= (TEST_BLOCK_SIZE30-10);
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Create section B */
    sect_node2 = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR100, (hsize_t)TEST_BLOCK_SIZE50);

    /* Add section B to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node2, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    state.tot_space += TEST_BLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Try to extend the allocated block */
    extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)TEST_BLOCK_ADDR70, (hsize_t)(TEST_BLOCK_SIZE30-10), (hsize_t)TEST_BLOCK_SIZE50);

    /* Should not succeed in extending the allocated block */
    if(extended)
	TEST_ERROR

    /* Free-space info should be the same */
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the allocated block */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)(TEST_BLOCK_SIZE30-10));

    state.tot_space += (TEST_BLOCK_SIZE30-10);
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Remove section A from free-space manger */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)(TEST_BLOCK_SIZE30-10), (H5FS_section_info_t **)&node) < 0)
	TEST_ERROR

    /* Remove the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    /* Remove section B from free-space manager */
    if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
		    (hsize_t)TEST_BLOCK_SIZE50, (H5FS_section_info_t **)&node) < 0)
	TEST_ERROR

    /* Remove the free-space section node */
    if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
	TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* Get the size of the file */
    if((new_file_size = h5_get_file_size(filename, fapl_new)) < 0)
        TEST_ERROR

    /* Verify the file is the correct size */
    if(new_file_size != file_size)
	TEST_ERROR

    if(H5Pclose(fapl_new) < 0)
        FAIL_STACK_ERROR

    PASSED()

    return(0);

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_new);
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_extend() */

/*
 * To verify that an aggregator is absorbed into a section.
 *
 *	Test 1: To aborb the aggregator onto the beginning of the section
 *		Allocate block A from meta_aggr
 *		Create a free-space section node with an address that adjoins
 *			the end of meta_aggr and a size to make the aggregator
 *			get absorbed into the section.
 *		The adding of the section to free-space will call H5MF_aggr_absorb(),
 *			which will absorb meta_aggr to the section:
 *			  section size + remaining size of aggregator is > aggr->alloc_size,
 *			  section is allowed to absorb an aggregator (allow_sect_absorb is true)
 *
 *	Test 2: To absorb the aggregator onto the end of the section
 *		Allocate block A from meta_aggr
 *		Allocate block B from sdata_aggr
 *		Create a free-space section node with an address that adjoins
 *			the beginning of meta_aggr and a size to make the
 *			aggregator get absorbed into the section.
 *		The adding of the section to free-space will call H5MF_aggr_absorb(),
 *			which will absorb meta_aggr to the section:
 *			  section size + remaining size of aggregator is > aggr->alloc_size,
 *			  section is allowed to absorb an aggregator (allow_sect_absorb is true)
 */
static unsigned
test_mf_fs_absorb(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FD_mem_t 		type, stype;
    haddr_t		addr, saddr;
    haddr_t 		ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0;
    H5MF_free_section_t *sect_node=NULL;
    H5MF_sect_ud_t 	udata;
    H5FS_section_info_t *node;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("A free-space section absorbs an aggregator: test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;

        if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
            TEST_ERROR

        if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
            TEST_ERROR
        if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
            TEST_ERROR

        /* Allocate a section from meta_aggr */
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* Add a section to free-space that adjoins end of the aggregator */
        sect_node = H5MF_sect_simple_new((haddr_t)(ma_addr+ma_size), (hsize_t)TEST_BLOCK_SIZE2048);

        /* Construct user data for callbacks */
        udata.f = f;
        udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
        udata.alloc_type = type;
        udata.allow_sect_absorb = TRUE;

        /* When adding, meta_aggr is absorbed onto the beginning of the section */
        if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
                FAIL_STACK_ERROR

        /* Verify that the section did absorb the aggregator */
        if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
                        (hsize_t)TEST_BLOCK_SIZE2048, (H5FS_section_info_t **)&node) < 0)
            TEST_ERROR

        if (node->addr != ma_addr)	TEST_ERROR
        if (node->size != (ma_size + TEST_BLOCK_SIZE2048))	TEST_ERROR

        /* Remove the free-space section node */
        if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
            TEST_ERROR

        /* Restore info for aggregator */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        /* Remove section from meta_aggr */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("A free-space section absorbs an aggregator: test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;

        if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
            TEST_ERROR

        if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
            TEST_ERROR
        if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
            TEST_ERROR

        /* Allocate a section from meta_aggr */
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        /* Allocate a section from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Add a section to free-space that adjoins the beginning of meta_aggr */
        sect_node = H5MF_sect_simple_new((haddr_t)addr, (hsize_t)TEST_BLOCK_SIZE30);

        /* Construct user data for callbacks */
        udata.f = f;
        udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
        udata.alloc_type = type;
        udata.allow_sect_absorb = TRUE;

        /* When adding, meta_aggr is absorbed onto the end of the section */
        if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
                FAIL_STACK_ERROR

        /* Verify that the section did absorb the aggregator */
        if(H5FS_sect_find(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type],
                        (hsize_t)(ma_size+TEST_BLOCK_SIZE30), (H5FS_section_info_t **)&node) < 0)
            TEST_ERROR

        if ((node->addr + TEST_BLOCK_SIZE30) != ma_addr)	TEST_ERROR
        if (node->size != (ma_size + TEST_BLOCK_SIZE30))	TEST_ERROR

        /* free the free-space section node */
        if(H5MF_sect_simple_free((H5FS_section_info_t *)node) < 0)
            TEST_ERROR

        /* restore info to meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        /* Remove section from meta_aggr */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);
        /* Remove section from sdata_aggr */
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_fs_absorb() */

/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		The first block of 30 is allocated from meta_aggr
 *		There is space of 2018 left in meta_aggr
 *
 *	Allocate second block (50) from meta_aggr:
 *		request-size is <= what is left in meta_aggr
 *	Result:
 *		The second block of 50 is allocated from meta_aggr
 *		There is space of 1968 left in meta_aggr
 */
static unsigned
test_mf_aggr_alloc1(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t 		type;
    haddr_t		addr1, addr2;
    haddr_t 		ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file  */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr2+TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != (file_size+TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Free the two blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc1() */

/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		There is space of 2018 left in meta_aggr
 *
 *	Allocate second block (50) from meta_aggr:
 *		request-size is <= what is left in meta_aggr
 *	Result:
 *		The second block of 50 is allocated from what is left in meta_aggr
 *		There is space of 1968 left in meta_aggr
 *
 *	Allocate third block (2058) from meta_aggr:
 *		request-size is > what is left in meta_aggr and is >= meta_aggr->alloc_size
 *		meta_aggr is at EOA
 *	Result:
 *		A block of request-size is extended via file allocation and is merged with meta_aggr
 *		The block of 2058 is allocated out of meta_aggr
 *		There is space of 1968 left in meta_aggr
 */
static unsigned
test_mf_aggr_alloc2(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t 		type;
    haddr_t		addr1, addr2, addr3;
    haddr_t 		ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of a file */
        if((file_size= h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr2+TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE2058);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr3+TEST_BLOCK_SIZE2058) != ma_addr)
            TEST_ERROR

        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        /* Unused space is freed from the end of the file */
        if (new_file_size != (file_size+TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50+TEST_BLOCK_SIZE2058))
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50+TEST_BLOCK_SIZE2058);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc2() */

/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr : (nothing in the aggregator)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from there
 *		There is space of 2018 left in meta_aggr
 *
 *	Allocate second block (50) from meta_aggr:
 *		request-size is <= what is left in meta_aggr
 *	Result:
 *		The second block of 50 is allocated from what is left in meta_aggr
 *		There is space of 1968 left in meta_aggr
 *
 *	Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *		request-size is > what is left in other_aggr and is < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from there
 *		There is space of 2018 left in sdata_aggr
 *
 *	Allocate the third block (2058) from meta_aggr:
 *		request-size is > what is left in meta_aggr and >= meta_aggr->alloc_size
 *		sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *	Result: A block of request-size is obtained via file allocation
 *		The new block's address is returned
 *		Nothing is changed in meta_aggr and sdata_aggr
 *
 *	Allocate fourth block (50) from meta_aggr:
 *		request-size is <= what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		The fourth block of 50 is allocated from what is left in meta_aggr
 *		There is space of 1968 left in meta_aggr
 */
static unsigned
test_mf_aggr_alloc3(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size; /* file size */
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, addr3, addr4, saddr1;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0;
    haddr_t 		sdata_addr=HADDR_UNDEF;
    hsize_t 		sdata_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator: test 3");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr2+TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr1+TEST_BLOCK_SIZE30) != sdata_addr)
            TEST_ERROR
        if (sdata_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30)) TEST_ERROR

        /* Allocate third block, which is from  file allocation not from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE2058));

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        /* info for meta_aggr shouldn't be changed */
        if (addr3 != (sdata_addr+sdata_size)) TEST_ERROR
        if ((addr3+TEST_BLOCK_SIZE2058) == new_ma_addr) TEST_ERROR
        if ((new_ma_addr != ma_addr) || (new_ma_size != ma_size)) TEST_ERROR

        /* Allocate fourth block, which should be from meta_aggr */
        addr4 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr4+TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE2058);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr4, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr1, (hsize_t)TEST_BLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc3() */


/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		There is space of 2018 left in meta_aggr
 *		The first block of 30 is allocated from there
 *
 *	Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *		request-size is > what is left in sdata_aggr and < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from there
 *
 *	Allocate the second block (2018) from sdata_aggr:
 *		request-size is <= what is left in sdata_aggr and < sdata_aggr->alloc_size
 *		request-size is < sdata_aggr->alloc_size
 *	Result:
 *		The block is allocated from what is left in sdata_aggr (all used up)
 *
 *	Allocate third block (50) from sdata_aggr :
 *		request-size is > what is left in sdata_aggr and < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is extended via file allocation
 *		The third block of 50 is allocated from there
 *		There is space of 1998 left in the sdata_aggr
 *
 *	Allocate second block (2058) from meta_aggr:
 *		request-size is > what is left in meta_aggr and >= meta_aggr->alloc_size
 *		sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *	Result:
 *		unused spaced in sdata_aggr is freed to free-space and is shrunk
 *		sdata_aggr is reset to 0
 *		A block of request-size is obtained via file allocation
 *		The new block's address is returned
 *		The block does not adjoin meta_aggr
 *		meta_aggr's info is unchanged
 */
static unsigned
test_mf_aggr_alloc4(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;    /* File size */
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, saddr1, saddr2, saddr3;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0, sdata_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 4");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file  */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30))
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr1+TEST_BLOCK_SIZE30) != sdata_addr)
            TEST_ERROR

        /* Allocate second block from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30));
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (saddr2+(TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30) != sdata_addr)
            TEST_ERROR

        /* Allocate third block from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);

        if ((saddr3+TEST_BLOCK_SIZE50) != sdata_addr)
            TEST_ERROR
        if(sdata_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE50))
            TEST_ERROR

        /* Allocate second block of 2058, which is from file allocation, not from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE2058);

        if (addr2 != sdata_addr)
            TEST_ERROR

        /* sdata_aggr is reset 0 */
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((sdata_addr != 0) && (sdata_size != 0))
            TEST_ERROR

        /* info is unchanged in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if ((new_ma_addr != ma_addr) && (new_ma_size != ma_size))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE2058);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr2, (hsize_t)TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr3, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc4() */

/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocate from there
 *
 *	Allocate second block (50) from meta_aggr:
 *		request-size is < what is left in meta_aggr
 *	Result:
 *		The second block of 50 is allocated from what is left there
 *		There is space of 1968 left in the meta_aggr
 *
 *	Allocate third block (1970) from meta_aggr:
 *		request-size is > what is left in meta_aggr and is < meta_aggr->alloc_size
 *	Result: A block of meta_aggr->alloc_size is extended via file allocation and is absorbed into the meta_aggr
 *		The block of 1970 is allocated from there
 *		There is space of 2046 left in meta_aggr
 *
 */
static unsigned
test_mf_aggr_alloc5(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;  /* File size */
    H5FD_mem_t 		type;
    haddr_t		addr1, addr2, addr3;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 5");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (addr2+TEST_BLOCK_SIZE50 != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate third block from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE1970);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (addr3 != ma_addr) TEST_ERROR
        if ((addr3+TEST_BLOCK_SIZE1970) != new_ma_addr) TEST_ERROR
        if (new_ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE1970 - ma_size)))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE1970);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc5() */

/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from there
 *
 *	Allocate second block (50) from meta_aggr:
 *		request-size is <= what is left in meta_aggr
 *	Result:
 *		The second block of 50 is allocated from what is left in meta_aggr
 *		There is space of 1968 left in meta_aggr
 *
 *	Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *		request-size is > what is left in sdata_aggr and is < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from there
 *		There is space of 2018 left in sdata_aggr
 *
 *	Allocate third block (1970) from meta_aggr:
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *		sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation.
 *		The block does not adjoin meta_aggr
 *		sdata_aggr is untouched
 *		meta_aggr's unused space of [880, 1968] is freed to free-space
 *		meta_aggr is updated to point to the new block
 */
static unsigned
test_mf_aggr_alloc6(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;  /* file size */
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, addr3, saddr1;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0, sdata_size=0;
    frspace_state_t 	state;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 6");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;

        /* Allocate first block from meta_aggr */
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (addr2+TEST_BLOCK_SIZE50 != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr1+TEST_BLOCK_SIZE30) != sdata_addr) TEST_ERROR
        if (sdata_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30)) TEST_ERROR

        /* Allocate third block from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE1970);

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if ((addr3+TEST_BLOCK_SIZE1970) != new_ma_addr) TEST_ERROR
        if (addr3 != (sdata_addr+sdata_size)) TEST_ERROR

        if ((ma_addr+TEST_BLOCK_SIZE1970) == new_ma_addr) TEST_ERROR
        if (new_ma_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE1970))
            TEST_ERROR

        /* Verify that meta_aggr's unused space of 1968 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        state.tot_space += (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50));
        state.tot_sect_count += 1;
        state.serial_sect_count += 1;

        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE1970);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr1, (hsize_t)TEST_BLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((new_file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc6() */

/*
 * To verify that blocks are allocated from the aggregator
 *
 *	Allocate first block (30) from meta_aggr: (nothing in meta_aggr)
 *		request-size is > what is left in meta_aggr and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from there
 *
 *	Allocate second block (50) from meta_aggr:
 *		request-size is <= what is left in meta_aggr
 *	Result:
 *		The second block of 50 is allocated from what is left in the aggregator
 *		There is space of 1968 left in the meta_aggr
 *
 *	Allocate first block (30) from sdata_aggr: (nothing in sdata_aggr)
 *		request-size is > what is left in sdata_aggr->size and < sdata_aggr->alloc_size
 * 	Result:
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocate from there
 *
 *	Allocate second block (2018) from sdata_aggr:
 *		request-size is <= what is left in sdata_aggr and is < sdata_aggr->alloc_size
 *	Result:
 *		The second block of 2018 is allocated from what is left in sdata_aggr (all used up)
 *
 *	Allocate third block (50) from sdata_aggr:
 *		request-size is > what is left in sdata_aggr and < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is extended via file allocation
 *		The third block of 50 is allocated from there
 *
 *	Allocate third block (1970) from meta_aggr:
 *		request-size is > what is left in meta_aggr and is < meta_aggr->alloc_size
 *		sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *	Result:
 *		unused space in sdata_aggr is freed to free-space and is shrunk
 *		sdata_aggr is reset to 0
 *		A block of meta_aggr->alloc_size is obtained via file allocation
 *		The block does not adjoin meta_aggr
 *		meta_aggr's unused space of [880, 1968] is freed to free-space
 *		meta_aggr is updated to point to the new block
 */
static unsigned
test_mf_aggr_alloc7(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      empty_size, file_size;
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, addr3, saddr1, saddr2, saddr3;
    haddr_t 		ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, sdata_size=0;
    frspace_state_t 	state;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_alloc() of meta/sdata aggregator:test 7");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate the first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate the second block from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (addr2+TEST_BLOCK_SIZE50 != ma_addr)
            TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50)))
            TEST_ERROR

        /* Allocate the first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr1+TEST_BLOCK_SIZE30) != sdata_addr)
            TEST_ERROR

        /* Allocate the second block from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30);

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr2+(TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30)) != sdata_addr)
            TEST_ERROR
        if (sdata_size != 0) TEST_ERROR

        /* Allocate the third block from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr3+TEST_BLOCK_SIZE50) != sdata_addr)
            TEST_ERROR
        if (sdata_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE50))
            TEST_ERROR

        /* Allocate the third block from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE1970);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (addr3 != sdata_addr) TEST_ERROR
        if ((addr3 + TEST_BLOCK_SIZE1970) != ma_addr) TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE1970)) TEST_ERROR

        /* sdata_aggr info is reset to 0 */
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != 0) TEST_ERROR
        if (sdata_size != 0) TEST_ERROR

        /* Verify that meta_aggr's unused space of 1968 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        state.tot_space += (TEST_BLOCK_SIZE2048 - (TEST_BLOCK_SIZE30 + TEST_BLOCK_SIZE50));
        state.tot_sect_count += 1;
        state.serial_sect_count += 1;

        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        /* Free all the allocated blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE1970);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr2, (hsize_t)(TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30));
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr3, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_alloc7() */

/*
 * To verify that a block can be extended from the aggregator
 *
 *	Test 1: Allocate block A from meta_aggr which is at end of file
 *		Try to extend a block which adjoins the aggregator
 *		H5MF_try_extend() succeeds: meta_aggr is extended by extended-request and meta_aggr's info is updated
 *
 *	Test 2: Allocate block A from meta_aggr
 *		Allocate block B from sdata_aggr so that meta_aggr is not at end of file
 *		Try to extend a block which adjoins meta_aggr and meta_aggr can fulfill the extended-request
 *		H5MF_try_extend() succeeds: the block is extended into the aggregator
 *
 *	Test 3: Allocate block A from meta_aggr
 *		Allocate block B from sdata_aggr so that meta_aggr is not at end of file
 *		Try to extend a block which adjoins meta_aggr but meta_aggr cannot fulfill the extended-request
 *		H5MF_try_extend() fails
 */
static unsigned
test_mf_aggr_extend(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      empty_size, file_size;
    H5FD_mem_t 		type, stype;
    haddr_t		new_addr, addr, saddr;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0, sdata_size=0;
    htri_t      	extended;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_extend() of meta/sdata aggregator: test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate the first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Adjust meta_aggr's info info for testing */
        f->shared->meta_aggr.addr = addr;
        f->shared->meta_aggr.size = f->shared->meta_aggr.alloc_size;

        new_addr = addr - 10;

        /* Try to extend the block */
        extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TEST_BLOCK_SIZE50));

        /* should succeed */
        if(!extended)
            TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (new_ma_addr != (addr+TEST_BLOCK_SIZE50))
            TEST_ERROR
        if (new_ma_size != f->shared->meta_aggr.alloc_size) TEST_ERROR

        /* Restore info for meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        /* Free the allocated blocks */
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, (ma_addr+ma_size), (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_extend() of meta/sdata aggregator: test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate the first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate the first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr+TEST_BLOCK_SIZE50) != sdata_addr)
            TEST_ERROR

        /* Adjust meta_aggr's info info for testing */
        f->shared->meta_aggr.addr = addr;
        f->shared->meta_aggr.size = f->shared->meta_aggr.alloc_size;

        new_addr = addr - 10;

        /* should be able to fulfill request from the aggreqator itself */
        extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TEST_BLOCK_SIZE50));

        if(!extended)
            TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (new_ma_addr != (addr+TEST_BLOCK_SIZE50))
            TEST_ERROR
        if (new_ma_size != (f->shared->meta_aggr.alloc_size-TEST_BLOCK_SIZE50))
            TEST_ERROR

        /* Restore info for meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_extend() of meta/sdata aggregator: test 3");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate first block from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate first block from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr+TEST_BLOCK_SIZE50) != sdata_addr)
            TEST_ERROR

        /* Adjust meta_aggr's info info for testing */
        f->shared->meta_aggr.addr = addr;
        f->shared->meta_aggr.size = 0;

        new_addr = addr - 10;

        /* unable to fulfill request from the aggreqator itself */
        extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)new_addr, (hsize_t)10, (hsize_t)(TEST_BLOCK_SIZE50));

        if(extended)
            TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (new_ma_addr != addr) TEST_ERROR
        if (new_ma_size != 0) TEST_ERROR

        /* restore info for meta_aggr */
        f->shared->meta_aggr.addr = ma_addr;
        f->shared->meta_aggr.size = ma_size;

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_extend() */

/*
 * To verify that a block is absorbed into an aggregator
 *
 * MF_try_shrink() only allows blocks to be absorbed into an aggregator
 *
 *	Test 1: H5MF_alloc() block A from meta_aggr
 *		H5MF_try_shrink() block A should merge it back into meta_aggr
 *			since block A adjoins the beginning of meta_aggr
 *
 *	Test 2: H5MF_alloc() block A from meta_aggr
 *		H5MF_alloc() block B from sdata_aggr
 *		H5MF_try_shrink() block B should merge it onto the end of meta_aggr
 *			because H5F_FS_MERGE_METADATA|H5F_FS_MERGE_RAWDATA is on for
 *			sec2 driver's FLMAP_SINGLE
 *
 *	Test 3: H5MF_alloc() block A from meta_aggr
 *		H5MF_alloc() block B from meta_aggr
 *		H5MF_alloc() block C from meta_aggr
 *		H5MF_try_shrink() block B should fail since it does not adjoin the
 *			beginning nor the end of meta_aggr
 */
static unsigned
test_mf_aggr_absorb(const char *env_h5_drvr, hid_t fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      empty_size, file_size;
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, addr3, saddr1;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    haddr_t 		sdata_addr=HADDR_UNDEF, new_sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0;
    hsize_t 		sdata_size=0, new_sdata_size=0;
    hbool_t             contig_addr_vfd;        /* Whether VFD used has a contigous address space */

    TESTING("H5MF_try_shrink() of meta/sdata aggregator: test 1");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    contig_addr_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(contig_addr_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((empty_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate block A from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        ma_addr = new_ma_addr - TEST_BLOCK_SIZE30;

        if((addr1 + TEST_BLOCK_SIZE30) != new_ma_addr)
            TEST_ERROR

        /* should succeed */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30) <= 0)
            TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);

        if (new_ma_addr != ma_addr) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of meta/sdata aggregator: test 2");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate block A from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr) TEST_ERROR
        if (ma_size != (TEST_BLOCK_SIZE2048 - TEST_BLOCK_SIZE30)) TEST_ERROR

        /* Allocate block B from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);

        /* should succeed */
        if(H5MF_try_shrink(f, stype, H5P_DATASET_XFER_DEFAULT, saddr1, (hsize_t)TEST_BLOCK_SIZE50) <= 0)
            TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &new_sdata_addr, &new_sdata_size);
        if (new_sdata_addr != sdata_addr) TEST_ERROR
        if (new_sdata_size != sdata_size) TEST_ERROR

        /* meta_aggr info should be updated because the block is absorbed into the meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR
        if (new_ma_size != (ma_size+TEST_BLOCK_SIZE50)) TEST_ERROR

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    TESTING("H5MF_try_shrink() of meta/sdata aggregator: test 3");

    /* Skip test when using VFDs that don't use the metadata aggregator */
    if(contig_addr_vfd) {
        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* Allocate block A from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* Allocate block B from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr2+TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /* Allocate block C from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50));
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr3+TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /* should not succeed */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50) > 0)
            TEST_ERROR

        /* aggregator info should be the same as before */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (new_ma_addr != ma_addr) TEST_ERROR

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)(TEST_BLOCK_SIZE30+TEST_BLOCK_SIZE50));

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Verify the file is the correct size */
        if (file_size != empty_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support metadata aggregator");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_aggr_absorb() */

/*
 * To verify that a block allocated from file allocation is aligned, can be shrunk and extended
 *
 * Alignment = 1024 or 4096
 *
 * Test 1:
 * 	Turn off using meta data aggregator
 * 	Allocate a block of 30 which should be from file allocation
 *	Result:
 *		The return address should be aligned
 *		A fragment [800, 224] or [800, 3296] is freed to free-space
 *		EOA is 1054 or 4126
 *
 * 	Allocate a block of 50 which should be from file allocation
 *	Result:
 *		The return address should be aligned
 *		A fragment [1054, 994] or [4126, 4066] is freed to free-space
 *		EOA is 2098 or 8242
 * Test 2:
 * 	Turn off using meta data aggregator
 * 	Allocate a block which should be from file allocation
 *	The return address should be aligned
 *	H5MF_try_shrink() the block with aligned address should succeed
 *
 * Test 3:
 * 	Turn off using meta data aggregator
 * 	Allocate a block which should be from file allocation
 *	The return address should be aligned
 *	H5MF_try_extend() the block with aligned address should succeed
 */
static unsigned
test_mf_align_eoa(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    hid_t		fapl1;
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size, new_file_size;
    H5FD_mem_t 		type;
    haddr_t		addr1, addr2;
    haddr_t 		ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0;
    htri_t 		extended;
    frspace_state_t 	state;
    hsize_t		alignment=0, mis_align=0, tmp=0, accum=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MM_alloc() of file allocation with alignment: test 1");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Turn off using meta/small data aggregator */
        if((fapl1 = H5Pcopy(new_fapl)) < 0) TEST_ERROR

        H5Pset_meta_block_size(fapl1, (hsize_t)0);
        H5Pset_small_data_block_size(fapl1, (hsize_t)0);

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(fapl1, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
             mis_align = alignment - tmp;

        accum = mis_align + TEST_BLOCK_SIZE30;

        /* Allocate a block of 30 from file allocation */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* there should be nothing in the aggregator */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if (ma_addr || ma_size) TEST_ERROR

        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
            if(check_stats(f, f->shared->fs_man[type], &state))
                TEST_ERROR
        }

        /* calculate fragment for alignment of block 50 */
        mis_align = 0;
        if ((tmp = ((hsize_t)file_size + accum) % alignment))
             mis_align = alignment - tmp;
        accum += (mis_align + TEST_BLOCK_SIZE50);

        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* there should be nothing in the aggregator */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if (ma_addr || ma_size) TEST_ERROR

        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
            if(check_stats(f, f->shared->fs_man[type], &state))
                TEST_ERROR
        }

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((new_file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        if (new_file_size != file_size)
            TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    TESTING("H5MF_try_shrink() of file allocation with alignment: test 2");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    if(have_alloc_vfd) {
        /* Re-open the file with alignment and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* allocate a block of 50 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* address should be aligned */
        if (addr1 % alignment) TEST_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* shrink the block */
        if(H5MF_try_shrink(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE50) <= 0)
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((new_file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        if (new_file_size != (file_size-TEST_BLOCK_SIZE50)) TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    TESTING("H5MF_try_extend() of file allocation with alignment: test 3");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    if(have_alloc_vfd) {
        /* Re-open the file with alignment and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* allocate a block of 50 */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* address should be aligned */
        if (addr1 % alignment) TEST_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        /* Re-open the file */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl1)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* try to extend the block */
        extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)addr1, (hsize_t)TEST_BLOCK_SIZE50, (hsize_t)TEST_BLOCK_SIZE30);

        if (extended <=0) TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        if((new_file_size = h5_get_file_size(filename, fapl1)) < 0)
            TEST_ERROR

        if (new_file_size != (file_size+TEST_BLOCK_SIZE30)) TEST_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_eoa() */

/*
 * To verify that a block allocated from the free-space manager is aligned
 *
 * Alignment = 1024 or 4096
 *
 * Test 1:
 *	Add section A with an aligned address to free-space manager (addr=alignment, size=50)
 *	Allocate a block of size=50
 *	The returned space's address should be same as section A's address
 *
 * Test 2:
 *	Add section A to free-space manager (addr=70, size=8000):
 *		section A is mis-aligned but the size is big enough for allocation with alignment
 *	Allocate a block of size=600
 *	The returned space should be allocated from section A with an aligned address:
 *		address=alignment  size=600
 *	There will be 2 sections in free-space: (alignment = 1024 or alignment = 4096)
 *		the fragment left from aligning section A: [70, 954] or [70, 4026]
 *		the section left after allocating block A: [1624, 416] or [4696, 3374]
 *	H5MF_try_extend() the block of size 600 by 200 should succeed:
 *		the existing fragment left from aligning section A: [70, 954] or [70, 4026]
 *		the section left after extending block A: [1824, 216] or [4896, 3174]
 *
 * Test 3:
 *	Add section A to free-space manager (addr=70, size=700):
 *		section A is mis-aligned but the size is not big enough for allocation with alignment
 *	Allocate a block of size=40
 *	The free-space manager is unable to fulfill the request
 *	The block is allocated from file allocation and should be aligned
 */
static unsigned
test_mf_align_fs(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    h5_stat_size_t      file_size;
    H5F_t		*f = NULL;              /* Internal file object pointer */
    H5FD_mem_t 		type;
    H5MF_free_section_t *sect_node = NULL;
    haddr_t		addr;
    frspace_state_t 	state;
    H5MF_sect_ud_t 	udata;
    htri_t 		extended;
    hsize_t		alignment=0, tmp=0, mis_align=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MF_alloc() of free-space manager with alignment: test 1");

    /* Set the filename to use for this test (dependent on fapl) */
    h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

    /* Create the file to work on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /* get alignment setting */
    if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
        TEST_ERROR

    /* Re-open the file with alignment setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    sect_node = H5MF_sect_simple_new((haddr_t)alignment, (hsize_t)TEST_BLOCK_SIZE50);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 50 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

    /* Verify that the allocated block is section A in free-space */
    if (addr != (haddr_t)alignment) TEST_ERROR
    if (addr % alignment) TEST_ERROR

    state.tot_space -= TEST_BLOCK_SIZE50;
    state.tot_sect_count -= 1;
    state.serial_sect_count -= 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the block to free-space */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)TEST_BLOCK_SIZE50);

    state.tot_space += TEST_BLOCK_SIZE50;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    TESTING("H5MF_alloc() of free-space manager with alignment: test 2");


    /* Re-open the file with alignment setting */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
        FAIL_STACK_ERROR

    /* Get a pointer to the internal file object */
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    type = H5FD_MEM_SUPER;

    if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
        TEST_ERROR

    if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
	TEST_ERROR
    if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
	TEST_ERROR

    sect_node = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE8000);

    /* Construct user data for callbacks */
    udata.f = f;
    udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
    udata.alloc_type = type;
    udata.allow_sect_absorb = TRUE;

    /* Add section A to free-space manager */
    if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
	    FAIL_STACK_ERROR

    HDmemset(&state, 0, sizeof(frspace_state_t));
    state.tot_space += TEST_BLOCK_SIZE8000;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Allocate a block of 600 */
    addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE600);

    /* Verify that the allocated block is aligned */
    if (addr % alignment) TEST_ERROR

    /* should have 1 more section in free-space */
    state.tot_space -= TEST_BLOCK_SIZE600;
    state.tot_sect_count += 1;
    state.serial_sect_count += 1;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* try to extend the block */
    extended = H5MF_try_extend(f, H5P_DATASET_XFER_DEFAULT, type, (haddr_t)addr, (hsize_t)TEST_BLOCK_SIZE600, (hsize_t)TEST_BLOCK_SIZE200);

    if (extended <=0) TEST_ERROR

    /* space should be decreased by 200, # of sections remain the same */
    state.tot_space -= TEST_BLOCK_SIZE200;

    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    /* Free the block to free-space manager */
    H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr, (hsize_t)(TEST_BLOCK_SIZE600+TEST_BLOCK_SIZE200));

    /* only 1 section in free-space because of merging */
    state.tot_space += (TEST_BLOCK_SIZE600+TEST_BLOCK_SIZE200);
    state.tot_sect_count = 1;
    state.serial_sect_count = 1;
    if(check_stats(f, f->shared->fs_man[type], &state))
        TEST_ERROR

    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    PASSED()

    TESTING("H5MF_alloc() of free-space manager with alignment: test 3");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        if((file_size = h5_get_file_size(filename, new_fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        type = H5FD_MEM_SUPER;

        if(H5MF_alloc_start(f, H5P_DATASET_XFER_DEFAULT, type) < 0)
            TEST_ERROR

        if (f->shared->fs_state[type] != H5F_FS_STATE_OPEN)
            TEST_ERROR
        if (f->shared->fs_man[type]->client != H5FS_CLIENT_FILE_ID)
            TEST_ERROR

        sect_node = H5MF_sect_simple_new((haddr_t)TEST_BLOCK_ADDR70, (hsize_t)TEST_BLOCK_SIZE700);

        /* Construct user data for callbacks */
        udata.f = f;
        udata.dxpl_id = H5P_DATASET_XFER_DEFAULT;
        udata.alloc_type = type;
        udata.allow_sect_absorb = TRUE;

        /* Add section A to free-space manager */
        if (H5FS_sect_add(f, H5P_DATASET_XFER_DEFAULT, f->shared->fs_man[type], (H5FS_section_info_t *)sect_node, H5FS_ADD_RETURNED_SPACE, &udata))
                FAIL_STACK_ERROR

        HDmemset(&state, 0, sizeof(frspace_state_t));
        state.tot_space += TEST_BLOCK_SIZE700;
        state.tot_sect_count += 1;
        state.serial_sect_count += 1;

        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR
        /*
         * Allocate a block of 40
         * Since free-space manager cannot fulfull the request because of alignment,
         * the block is obtained from file allocation
         */
        addr = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)(TEST_BLOCK_SIZE40));

        /* Verify that the allocated block is aligned */
        if (addr % alignment)
            TEST_ERROR

        /* verify that the allocated block is from file allocation, not section A in free-space */
        if (!(addr >= (haddr_t)file_size)) TEST_ERROR

        /* calculate fragment for alignment of block 40 from file allocation */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* free-space info should be the same  */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_fs() */

/*
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (nothing in the aggregator)
 *		request-size > aggr->size and < aggr->alloc_size
 *	Result:
 *		An "aggr->alloc_size" block is allocated from file allocation for the aggregator
 *		EOA is 3072
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *		There is space of 2018 left in meta_aggr
 *
 *	Allocate second block (50) from meta_aggr:
 *		(request-size + fragment size) <= aggr->size
 *	Result:
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *		There is space of 974 left in meta_aggr
 *
 *	Allocate third block (80) from meta_aggr:
 *		(request-size + fragment size) > aggr->size
 *		request-size < meta_aggr->alloc_size
 *		fragment size < (meta_aggr->alloc_size - request-size)
 *		meta_aggr is at EOA
 *	Result:
 *		A block of "meta_aggr->alloc_size" is extended from file allocation for meta_aggr
 *		EOA is 5120
 *		The third block of 80 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[2098, 974]
 *		There is space of 1968 left in meta_aggr
 *
 *	Allocate fourth block (1970) from meta_aggr:
 *		(request-size + fragment size) is <= aggr->size
 *		fragment size > (aggr->alloc_size - request-size)
 *		meta_aggr is at EOA
 *	Result:
 *		A block of aggr->alloc_size + fragment size - (aggr->alloc_size - request-size))
 *			 is extended from file allocation for meta_aggr
 *		The third block of 1970 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[3152, 944]
 *		There is space of 1968 left in meta_aggr
 *		EOA is at 8034
 *
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (aggregator is empty)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A meta_aggr->alloc_size block is allocated from file allocation for the aggregator
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 6144
 *
 *	Allocate second block (50) from meta_aggr:
 *		(request-size + fragment size) is > meta_aggr->size
 *		request-size < meta_aggr->alloc_size
 *		fragment size > (meta_aggr->alloc_size - request-size)
 *		meta_aggr is at EOA
 *	Result:
 *		A block of meta_aggr->alloc_size + (fragment size - (meta_aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 10260
 *
 *	Allocate third block (80) from meta_aggr:
 *		(request-size + fragment size) is > meta_aggr->size
 *		request-size < meta_aggr->alloc_size
 *		fragment size > (meta_aggr->alloc_size - request-size)
 *		meta_aggr is at EOA
 *	Result:
 *		A block of meta_aggr->alloc_size + (fragment size - (meta_aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		The third block of 80 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[8242, 4046]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 14386
 *
 *	Allocate fourth block (1970) from meta_aggr:
 *		(request-size + fragment size) > meta_aggr->size
 *		request-size < meta_aggr->alloc_size
 *		fragment size > (meta_aggr->alloc_size - request-size)
 *		meta_aggr is at EOA
 *	Result:
 *		A block of meta_aggr->alloc_size + (fragment size - (meta_aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		The fourth block of 1970 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[12368, 4016]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 20372
 */
static unsigned
test_mf_align_alloc1(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;              /* File size */

    H5FD_mem_t 		type;
    haddr_t		addr1, addr2, addr3, addr4;
    frspace_state_t 	state;
    haddr_t 		ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, mis_align=0;
    hsize_t		alignment=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 1");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1 + TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 50 */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr2 + TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 80 */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if (addr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr3 + TEST_BLOCK_SIZE80) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 1970 */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 1970 from meta_aggr */
        addr4 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE1970);

        /* Verify that the allocated block is aligned */
        if (addr4 % alignment) TEST_ERROR

        /* fragment for alignment of block 1970 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr4 + TEST_BLOCK_SIZE1970) != ma_addr)
            TEST_ERROR

        /* Verify total size of free space after all the allocations */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE80);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE1970);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc1() */

/*
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A meta_aggr->alloc_size block is allocated from file allocation for the aggregator
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *		There is space of 2018 left in meta_aggr
 *		EOA is 3072
 *
 *	Allocate second block (50) from meta_aggr:
 *		(request-size+fragment size) <= aggr->size
 *	Result:
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *		There is space of 974 left in meta_aggr
 *
 *	Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *		request-size is > sdata_aggr->size and < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		The first block of 30 is allocated from sdata_aggr and should be aligned
 *		EOA is 5120
 *
 *	Allocate third block (80) from meta_aggr:
 *		request-size+fragment size is > meta_aggr->size
 *		sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		The unused space in meta_aggr is freed to free-space [2098, 974]
 *		meta_aggr is updated to point to the new block
 *		The third block of 80 is allocated from meta_aggr and should be aligned
 *		EOA is 7168
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > aggr->size and < aggr->alloc_size
 *	Result:
 *		A meta_aggr->alloc_size block is allocated from file allocation for the aggregator
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *		There is space of 2018 left meta_aggr
 *		EOA is at 6144
 *
 *	Allocate second block (50) from meta_aggr:
 *		(request-size + fragment size) > aggr->size
 *		request-size < aggr->alloc_size
 *		fragment size > (aggr->alloc_size - request-size)
 *	Result:
 *		A block of aggr->alloc_size + (fragment size - (aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 10260
 *
 *	Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *		request-size is > sdata_aggr->size and < sdata_aggr->alloc_size
 *		meta_aggr is at EOA and has used up more than meta_aggr->alloc_size
 *	Result:
 *		The remaining space in meta_aggr is freed to free-space [8242, 2018] and shrunk since at EOF
 *		meta_aggr is reset to 0
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		Fragment from alignment of file allocation is freed to free-space: [8242, 4046]
 *		The first block of 30 is allocated from sdata_aggr and should be aligned
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 14336
 *
 *	Allocate third block (80) from meta_aggr:
 *		(request-size + fragment size) is > meta_aggr->size
 *		request-size < meta_aggr->alloc_size
 *		sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation for the aggregator
 *		Fragment from alignment of file allocation is freed to free-space:[14336, 2048]
 *		Since this fragment adjoins sdata_aggr and fulfills "absorb" condition,
 *		  the space left in sdata_aggr is absorbed into the fragment and freed to free-space: [12318, 2018]
 *		other_aggr is reset to 0
 *		The third block of 80 is allocated from the aggregator and should be aligned
 *		There is space of 1968 left in meta_aggr
 *		EOA is at 18432
 */
static unsigned
test_mf_align_alloc2(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;              /* File size */
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, addr3, saddr1;
    frspace_state_t 	state;
    haddr_t 		ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, sdata_size=0, mis_align=0;
    hsize_t		alignment=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 2");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1 + TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr2 + TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /*
         * Calculate fragment for alignment of block 30 in sdata_aggr:
         *
         * For alignment = 1024, alloc_size = 2048:
         *  block 30 is allocated from (ma_addr + ma_size),
         *	which is already aligned
         *
         * For alignment = 4096, alloc_size = 2048:
         *	since remaining space in meta_aggr is freed and shrunk,
         *	block 30 is allocated from ma_addr
         */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = ((ma_addr + ma_size) % alignment)))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* fragment for alignment of block 30 for sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* Verify that the allocated block is aligned */
        if (saddr1 % alignment) TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if (sdata_addr != (saddr1 + TEST_BLOCK_SIZE30)) TEST_ERROR

        /*
         * Calculate fragment for the allocation of block 80 from meta_aggr:
         *
         * For alignment = 1024, alloc_size = 2048:
         * 	fragment for unused space in meta_aggr is freed to free-space
         * For alignment = 4096, alloc_size = 2048:
         * 	fragment from alignment of file allocation absorbs sdata_aggr's remaining space
         */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = (sdata_addr % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 80 from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if (addr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr3 + TEST_BLOCK_SIZE80) != ma_addr)
            TEST_ERROR

        /* Verify total size of free space after all the allocations */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr1, (hsize_t)TEST_BLOCK_SIZE30);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr2, (hsize_t)TEST_BLOCK_SIZE50);
        H5MF_xfree(f, type, H5P_DATASET_XFER_DEFAULT, addr3, (hsize_t)TEST_BLOCK_SIZE80);
        H5MF_xfree(f, stype, H5P_DATASET_XFER_DEFAULT, saddr1, (hsize_t)TEST_BLOCK_SIZE30);

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc2() */

/*
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation for the aggregator
 *		Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in meta_aggr
 *		EOA is 3072
 *
 *	Allocate second block (50) from meta_aggr:
 *		(request-size+fragment size) is <= aggr->size
 *	Result:
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *		There is space of 974 left in the aggregator
 *
 *	Allocate first block (30) from other_aggr: (nothing in other_aggr)
 *		request-size is > what is left in other_aggr->size and < other_aggr->alloc_size
 *	Result:
 *		A "other_aggr->alloc_size" block is allocated from file allocation for other_aggr
 *		The first block of 30 is allocated from other_aggr and should be aligned
 *		There is space of 2018 left in other_aggr->size
 *		EOA is 5120
 *
 *	Allocate second block (50) from sdata_aggr:
 *		(request-size+fragment size) < sdata_aggr->size
 *	Result:
 *		The second block of 50 is allocated from sdata_aggr and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[3102, 994]
 *		There is space of 974 left in sdata_aggr
 *
 *	Allocate third block (80) from sdata_aggr:
 *		(request-size+fragment size) is >= sdata_aggr->size
 *		request-size < sdata_aggr->alloc_size
 *		sdata_aggr is at EOA
 *	Result:
 *		Another block of sdata_aggr->alloc_size is extended from file allocation for sdata_aggr
 *		The third block of 80 is allocated from sdata_aggr and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4146, 974]
 *		There is space of 1968 left in sdata_aggr
 *		EOA is 7168
 *
 *	Allocate third block (1034) from meta_aggregator:
 *		(request-size + alignment) > meta_aggr->size but < meta_aggr->alloc_size
 *		sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *	Result:
 *		The unused space in sdata_aggr is freed to free-space [5200, 1968] then shrunk
 *		sdata_aggr is reset to 0
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space [5200, 944]
 *		The unused space in meta_aggr is freed to free-space [2098, 974]
 *		The meta_aggr is updated to point to the new space
 *		The block of 1034 is allocated from the new block and should be aligned
 *		There is space of 1014 left in meta_aggr
 *		EOA is 8192
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > what is left in aggr->size and < aggr->alloc_size
 *	Result:
 *		A meta_aggr->alloc block is allocated from file allocation for the aggregator
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 6144
 *
 *	Allocate second block (50) from meta_aggr:
 *		(request-size + fragment size) is > what is left in aggr->size
 *		request-size < aggr->alloc_size
 *		fragment size > (aggr->alloc_size - request-size)
 *	Result:
 *		A block of aggr->alloc_size + (fragment size - (aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *		There is space of 2018 left in meta_aggr
 *		EOA is at 10260
 *
 *	Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *		request-size > sdata_aggr->size and < sdata_aggr->alloc_size
 *		meta_aggr is at EOA and has used up more than meta_aggr->alloc_size
 *	Result:
 *		The remaining space in meta_aggr is freed to free-space [8242, 2018] and shrunk
 *			since at EOF
 *		meta_aggr is reset to 0
 *		A block of sdata_aggr->alloc_size is obtained via file allocation
 *		Fragment from alignment of file allocation is freed to free-space: [8242, 4046]
 *		The first block of 30 is allocated from sdata_aggr and should be aligned
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 14336
 *
 *	Allocate second block (50) from sdata_aggr:
 *		request-size is > sdata_aggr->size
 *		request-size < sdata_aggr->alloc_size
 *		fragment size > (sdata_aggr->alloc_size - request-size)
 *	Result:
 *		A block of sdata_aggr->alloc_size + (fragment size - (sdata_aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		Fragment from alignment of aggregator allocation is freed to free-space:[12318, 4066]
 *		The second block of 50 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in the sdata_aggr
 *		EOA is at 18452
 *
 *	Allocate third block (80) from sdata_aggr:
 *		request-size + fragment size is > sdata_aggr->size
 *		request-size < sdata_aggr->alloc_size
 *		fragment size > (sdata_aggr->alloc_size - request-size)
 *	Result:
 *		A block of sdata_aggr->alloc_size + (fragment size - (sdata_aggr->alloc_size - request-size)
 *			is allocated from file allocation for the aggregator
 *		Fragment from alignment of aggregator allocation is freed to free-space:[16434, 4046]
 *		The third block of 80 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in the sdata_aggr
 *		EOA is at 22578
 *
 *	Allocate third block (1034) from meta_aggregator:
 *		(request-size + fragment size) is > meta_aggr->size but request-size < meta_aggr->alloc_size
 *		sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *	Result:
 *		The remaining space in sdata_aggr is freed to free-space [20560, 2018] then shrunk
 *		sdata_aggr is reset to 0
 *		There is nothing in meta_aggr
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space [20560, 4016]
 *		EOA is 26624
 *		The meta_aggr is updated to point to the new space
 *		The block of 1034 is allocated from the new block and should be aligned
 *		There is space of 1014 left in meta_aggr
 */
static unsigned
test_mf_align_alloc3(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, addr3;
    haddr_t		saddr1, saddr2, saddr3;
    frspace_state_t 	state;
    haddr_t 		ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, sdata_size=0, mis_align=0;
    hsize_t		alignment=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 3");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1 + TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 50 */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr2 + TEST_BLOCK_SIZE50) != ma_addr)
            TEST_ERROR

        /*
         * Calculate fragment for alignment of block 30 in sdata_aggr:
         *
         * For alignment = 1024, alloc_size = 2048:
         *  block 30 is allocated from (ma_addr + ma_size),
         *  which is already aligned
         *
         * For alignment = 4096, alloc_size = 2048:
         *  since remaining space in meta_aggr is freed and shrunk,
         *  block 30 is allocated from ma_addr
         */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = ((ma_addr + ma_size) % alignment)))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (saddr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 for sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr1+TEST_BLOCK_SIZE30)) TEST_ERROR

        /* calculate fragment for alignment of block 50 in sdata_aggr */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (saddr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 for sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr2 + TEST_BLOCK_SIZE50)) TEST_ERROR

        /* calculate fragment for alignment of block 80 in sdata_aggr */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 80 from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if (saddr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 for sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr3 + TEST_BLOCK_SIZE80) != sdata_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 1034 */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 1034 for meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE1034);

        /* Verify that the allocated block is aligned */
        if (addr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 1034 for meta_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* calculate unused space in meta_aggr that is freed to free-space after block 1034 */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;

        /* fragment for unused space in meta_aggr after block 1034 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr3 + TEST_BLOCK_SIZE1034) != ma_addr)
            TEST_ERROR

        /* Verify total size of free space after all allocations */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc3() */


/*
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *		The first block of 30 is allocated from meta_aggr and should be aligned
 *		There is space of 2018 left in meta_aggr
 *		EOA is 3072
 *
 *	Allocate second block (2058) from meta_aggr:
 *		(request-size+fragment) is > meta_aggr->size and request-size is > meta_aggr->alloc_size
 *		meta_aggr is at EOA
 *	Result:
 *		The second block of 2058 + fragment is extended and merged together with meta_aggr
 *		The block of 2058 is allocated out of the aggregator
 *		Fragment from alignment of aggregator allocation is freed to free-space:[1054, 994]
 *		There is space of 2018 (same as before) left in meta_aggr
 *		EOA is 6124
 *
 *	Allocate third block (5) from meta_aggr:
 *		request-size+fragment < meta_aggr->size
 *	Result:
 *		A block of 5 is allocated from the aggregator
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4106, 1014]
 *		There is space of 999 left in meta_aggr
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *		The first block of 30 is allocated from meta_aggr and should be aligned
 *		There is space of 2018 left in meta_aggr
 *		EOA is 6144
 *
 *	Allocate second block (2058) from meta_aggr:
 *		(request-size+fragment) is > meta_aggr->size and request-size is > meta_aggr->alloc_size
 *		meta_aggr is at EOA
 *	Result:
 *		The second block of 2058 + fragment is extended and merged together with meta_aggr
 *		The block of 2058 is allocated out of the aggregator
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4126, 4066]
 *		There is space of 2018 (same as before) left in meta_aggr
 *		EOA is 12268
 *
 *	Allocate third block (5) from meta_aggr:
 *		request-size+fragment is > meta_aggr->size
 *		request-size < meta_aggr->alloc_size
 *		fragment < (meta_aggr->alloc_size - request-size)
 *		meta_aggr is at EOA
 *	Result:
 *		A block of meta_aggr->alloc_size is extended from file allocation for the aggregator
 *		A block of 5 is allocated from the aggregator
 *		Fragment from alignment of aggregator allocation is freed to free-space:[10250, 2038]
 *		There is space of 2023 left in meta_aggr
 *
 */
static unsigned
test_mf_align_alloc4(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t 		type;
    haddr_t		addr1, addr2, addr3;
    frspace_state_t 	state;
    haddr_t 		ma_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, saved_ma_size=0;
    hsize_t		alignment=0, mis_align=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 4");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        saved_ma_size = ma_size;
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr) TEST_ERROR

        /* calculate fragment for alignment of block 2058 */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 2058 from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE2058);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 2058 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr2 + TEST_BLOCK_SIZE2058) != ma_addr) TEST_ERROR

        /* meta_aggr->size remains the same */
        if (ma_size != saved_ma_size) TEST_ERROR

        /* calculate fragment for alignment of block 5 from meta_aggr */
        mis_align = 0;
        if ((tmp = ma_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 5 from meta_aggr */
        addr3 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE5);

        /* fragment for alignment of block 5 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* Verify that the allocated block is aligned */
        if (addr3 % alignment) TEST_ERROR

        /* Verify total size of free space after all allocations */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc4() */

/*
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *		The first block of 30 is allocated from meta_aggr and should be aligned
 *		There is space of 2018 left in meta_aggr
 *		EOA is 3072
 *
 *	Allocate first block (30) from sdata_aggr: (nothing in the aggregator)
 *		A block of sdata_aggr->alloc_size is allocated from file allocation
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 5120
 *
 *	Allocate second block (2058) from meta_aggr:
 *		(request-size + fragment size) > meta_aggr->size and > meta_aggr->alloc_size
 *		sdata_aggr is at EOA but has not used up sdata_aggr->alloc_size
 *	Result:
 *		A block of 2058 is allocated from file allocation
 *		EOA is 7178
 *		Nothing is changed in meta_aggr and sdata_aggr
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *		The first block of 30 is allocated from meta_aggr and should be aligned
 *		There is space of 2018 left in meta_aggr
 *		EOA is 6144
 *
 *	Allocate first block (30) from sdata_aggr: (meta_aggr is empty)
 *		meta_aggr is at EOA but has not used up more than meta_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[6144, 2048]
 *		This fragment adjoins meta_aggr and fulfills "absorb" condition,
 *			the remaining space left in meta_aggr is absorbed into the fragment and
 *			freed to free-space: [4126, 2018]
 *		meta_aggr is reset to 0
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 10240
 *
 *	Allocate second block (2058) from meta_aggr:
 *		request-size + fragment size is > meta_aggr->size
 *		request_size is > meta_aggr->alloc_size
 *		sdata_aggr is at EOA but has not used up more than sdata_aggr->alloc_size
 *	Result:
 *		A block of 2058 is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[10240, 2048]
 *		This fragment adjoins sdata_aggr and fulfills "absorb" condition,
 *			the remaining space left in sdata_aggr is absorbed into the fragment and
 *			freed to free-space: [8222, 2018]
 *		sdata_aggr is reset to 0
 *		EOA is 14346
 *		meta_aggr and sdata_aggr are all 0
 */
static unsigned
test_mf_align_alloc5(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2, saddr1;
    frspace_state_t 	state;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF;
    haddr_t 		sdata_addr=HADDR_UNDEF, new_sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0, sdata_size=0, new_sdata_size=0;
    hsize_t		alignment=0, mis_align=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */


    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 5");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);

        if ((addr1 + TEST_BLOCK_SIZE30) != ma_addr) TEST_ERROR

        /* fragment for alignment of block 30 is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* calculate fragment for alignment of block 30 from sdata_aggr */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = (ma_addr + ma_size) % alignment))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (saddr1 % alignment) TEST_ERROR

        /* fragment of alignment for block 30 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if ((saddr1+TEST_BLOCK_SIZE30) != sdata_addr) TEST_ERROR

        /* calculate fragment for alignment of block 2058 from meta_aggr */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = (sdata_addr + sdata_size) % alignment))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = (sdata_addr % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 2058 from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE2058);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 2058 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        /* Verify total size of free space after all allocations */
        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        /* nothing is changed in meta_aggr */
        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        if (alignment == TEST_ALIGN1024 && (new_ma_addr != ma_addr || new_ma_size != ma_size))
            TEST_ERROR
        else if (alignment == TEST_ALIGN4096 && (new_ma_addr != 0 || new_ma_size != 0))
            TEST_ERROR

        /* nothing is changed in sdata_aggr */
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &new_sdata_addr, &new_sdata_size);
        if (alignment == TEST_ALIGN1024 && (new_sdata_addr != sdata_addr || new_sdata_size != sdata_size))
            TEST_ERROR
        else if (alignment == TEST_ALIGN4096 && ((new_sdata_addr != 0 || new_sdata_size != 0)))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc5() */


/*
 * To verify that blocks allocated from the aggregator are aligned
 *
 * Alignment = 1024 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is empty)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[800, 224]
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in meta_aggr->size
 *		EOA is 3072
 *
 *	Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *		request_size > sdata_aggr->size and < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is allocated from file allocation
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 5120
 *
 *	Allocate second block (50) from sdata_aggr:
 *		(request-size+fragment size) <= sdata_aggr->size
 *	Result:
 *		The second block of 50 is allocated from sdata_aggr and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[3102, 994]
 *		There is space of 974 left in sdata_aggr
 *
 *	Allocate third block (80) from sdata_aggr:
 *		(request-size+fragment size) > sdata_aggr->size
 *		request-size < sdata_aggr->alloc_size
 *		fragment size < (sdata_aggr->alloc_size - request-size)
 *	Result:
 *		Another block of sdata_aggr->alloc_size block is extended from file allocation
 *			for sdata_aggr
 *		The third block of 80 is allocated from sdata_aggr and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[4146, 974]
 *		There is space of 1968 left in sdata_aggr
 *		EOA is 7168
 *
 *	Allocate second block (2058) from meta_aggr:
 *		request-size + fragment size is > meta_aggr->size
 *		request-size is > meta_aggr->alloc_size
 *		sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *	Result:
 *		The remaining space in sdata_aggr is freed to free-space and shrunk
 *		sdata_aggr is reset to 0
 *		A block of 2058 is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[5200, 944]
 *		EOA is at 8202
 *		meta_aggr is unchanged
 *
 * Alignment = 4096 	aggr->alloc_size = 2048
 *
 *	Allocate first block (30) from meta_aggr: (meta_aggr is emtpy)
 *		request-size is > meta_aggr->size and < meta_aggr->alloc_size
 *	Result:
 *		A block of meta_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[800, 3296]
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in meta_aggr
 *		EOA is 6144
 *
 *	Allocate first block (30) from sdata_aggr: (sdata_aggr is empty)
 *		request_size > sdata_aggr->size and < sdata_aggr->alloc_size
 *	Result:
 *		A block of sdata_aggr->alloc_size is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space: [6144, 2048]
 *		This fragment adjoins meta_aggr and fulfills "absorb" condition,
 *			the remaining space left in meta_aggr is absorbed into the fragment and
 *			freed to free-space:[4126, 2018]
 *		meta_aggr is reset to 0
 *		The first block of 30 is allocated from the aggregator and should be aligned
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 5120
 *
 *	Allocate second block (50) from sdata_aggr:
 *		(request-size+fragment size) is <= sdata_aggr->size
 *		request-size < sdata_aggr->alloc_size
 *		fragment size > (sdata_aggr->alloc_size - request-size)
 *	Result:
 *		A block of sdata_aggr->alloc_size + (fragment size - (sdata_aggr->alloc_size - request-size))
 *			is extended from file allocation for the aggregator
 *		The second block of 50 is allocated from sdata_aggr and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[8222, 4066]
 *		There is space of 2018 left in sdata_aggr
 *		EOA is at 14356
 *
 *	Allocate third block (80) from sdata_aggr:
 *		(request-size+fragment size) is > sdata_aggr->size
 *		request-size < sdata_aggr->alloc_size
 *		fragment size > (sdata_aggr->alloc_size - request-size)
 *	Result:
 *		A block of sdata_aggr->alloc_size+(fragment size-(sdata_aggr->alloc_size-request-size))
 *			is extended from file allocation for sdata_aggr
 *		The third block of 80 is allocated from sdata_aggr and should be aligned
 *		Fragment from alignment of aggregator allocation is freed to free-space:[12338, 4046]
 *		There is space of 2018 left in sdata_aggr
 *		EOA is 18482
 *
 *	Allocate second block (2058) from meta_aggr:
 *		request-size + fragment size is > meta_aggr->size
 *		request-size is > meta_aggr->alloc_size
 *		sdata_aggr is at EOA and has used up more than sdata_aggr->alloc_size
 *	Result:
 *		The remaining space in sdata_aggr is freed to free-space and shrunk: [16464, 2018]
 *		sdata_aggr is reset to 0
 *		A block of 2058 is allocated from file allocation
 *		Fragment from alignment of file allocation is freed to free-space:[16464, 4016]
 *		EOA is at 22538
 *		meta_aggr is unchanged
 */
static unsigned
test_mf_align_alloc6(const char *env_h5_drvr, hid_t fapl, hid_t new_fapl)
{
    hid_t		file = -1;              /* File ID */
    char		filename[FILENAME_LEN]; /* Filename to use */
    H5F_t		*f = NULL;              /* Internal file object pointer */
    h5_stat_size_t      file_size;
    H5FD_mem_t 		type, stype;
    haddr_t		addr1, addr2;
    haddr_t		saddr1, saddr2, saddr3;
    frspace_state_t 	state;
    haddr_t 		ma_addr=HADDR_UNDEF, new_ma_addr=HADDR_UNDEF, sdata_addr=HADDR_UNDEF;
    hsize_t 		ma_size=0, new_ma_size=0, sdata_size=0;
    hsize_t		alignment=0, mis_align=0, tmp=0;
    hbool_t             have_alloc_vfd;        /* Whether VFD used has an 'alloc' callback */

    TESTING("H5MF_alloc() of meta/sdata aggregator with alignment: test 6");

    /* Skip test when using VFDs that have their own 'alloc' callback, which
     *  don't push mis-aligned space fragments on the file free space list
     */
    have_alloc_vfd = (hbool_t)(HDstrcmp(env_h5_drvr, "stdio")
            && HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi"));
    if(have_alloc_vfd) {
        /* Set the filename to use for this test (dependent on fapl) */
        h5_fixname(FILENAME[0], fapl, filename, sizeof(filename));

        /* Create the file to work on (without alignment) */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            FAIL_STACK_ERROR

        /* Close file */
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        /* Get the size of the file */
        if((file_size = h5_get_file_size(filename, fapl)) < 0)
            TEST_ERROR

        /* Re-open the file with alignment setting and meta/sdata setting */
        if((file = H5Fopen(filename, H5F_ACC_RDWR, new_fapl)) < 0)
            FAIL_STACK_ERROR

        /* Get a pointer to the internal file object */
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR

        /* get alignment setting */
        if(H5Pget_alignment(new_fapl, NULL, &alignment) < 0)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 */
        if ((tmp = (hsize_t)file_size % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from meta_aggr */
        type = H5FD_MEM_SUPER;
        addr1 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (addr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 in meta_aggr is freed to free-space */
        HDmemset(&state, 0, sizeof(frspace_state_t));
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &ma_addr, &ma_size);
        if ((addr1+TEST_BLOCK_SIZE30) != ma_addr)
            TEST_ERROR

        /* calculate fragment for alignment of block 30 in sdata_aggr */
        mis_align = 0;
        if ((alignment == TEST_ALIGN1024) && (tmp = (ma_addr + ma_size) % alignment))
            mis_align = alignment - tmp;
        else if ((alignment == TEST_ALIGN4096) && (tmp = (ma_addr % alignment)))
            mis_align = alignment - tmp;

        /* Allocate a block of 30 from sdata_aggr */
        stype = H5FD_MEM_DRAW;
        saddr1 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE30);

        /* Verify that the allocated block is aligned */
        if (saddr1 % alignment) TEST_ERROR

        /* fragment for alignment of block 30 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr1+TEST_BLOCK_SIZE30)) TEST_ERROR

        /* calculate fragment for alignment of block 50 in sdata_aggr */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 50 from sdata_aggr */
        saddr2 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE50);

        /* Verify that the allocated block is aligned */
        if (saddr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 50 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr2+TEST_BLOCK_SIZE50)) TEST_ERROR

        /* calculate fragment for alignment of block 80 in sdata_aggr */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 80 from sdata_aggr */
        saddr3 = H5MF_alloc(f, stype, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE80);

        /* Verify that the allocated block is aligned */
        if (saddr3 % alignment) TEST_ERROR

        /* fragment for alignment of block 80 in sdata_aggr is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);
        if (sdata_addr != (saddr3+TEST_BLOCK_SIZE80)) TEST_ERROR

        /* calculate fragment for alignment of block 2058 */
        /* remaining space in sdata_aggr is freed and shrunk */
        mis_align = 0;
        if ((tmp = sdata_addr % alignment))
            mis_align = alignment - tmp;

        /* Allocate a block of 2058 from meta_aggr */
        addr2 = H5MF_alloc(f, type, H5P_DATASET_XFER_DEFAULT, (hsize_t)TEST_BLOCK_SIZE2058);

        /* Verify that the allocated block is aligned */
        if (addr2 % alignment) TEST_ERROR

        /* fragment for alignment of block 2058 is freed to free-space */
        if (mis_align) {
            state.tot_space += mis_align;
            state.tot_sect_count += 1;
            state.serial_sect_count += 1;
        }

        H5MF_aggr_query(f, &(f->shared->meta_aggr), &new_ma_addr, &new_ma_size);
        H5MF_aggr_query(f, &(f->shared->sdata_aggr), &sdata_addr, &sdata_size);

        if (alignment == TEST_ALIGN1024 && (new_ma_addr != ma_addr || new_ma_size != ma_size))
            TEST_ERROR
        else if (alignment == TEST_ALIGN4096 && (new_ma_addr != 0 || new_ma_size != 0))
            TEST_ERROR

        if (sdata_addr != 0 || sdata_size != 0)
            TEST_ERROR

        if(check_stats(f, f->shared->fs_man[type], &state))
            TEST_ERROR

        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR

        PASSED()
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support mis-aligned fragments");
    } /* end else */

    return(0);

error:
    H5E_BEGIN_TRY {
	H5Fclose(file);
    } H5E_END_TRY;
    return(1);
} /* test_mf_align_alloc6() */

int
main(void)
{
    hid_t       fapl = -1;	   /* File access property list for data files */
    hid_t       new_fapl = -1;	   /* File access property list for alignment & aggr setting */
    unsigned    nerrors = 0;       /* Cumulative error count */
    test_type_t	curr_test;	   /* Current test being worked on */
    const char  *env_h5_drvr;      /* File Driver value from environment */

    /* Get the VFD to use */
    env_h5_drvr = HDgetenv("HDF5_DRIVER");
    if(env_h5_drvr == NULL)
        env_h5_drvr = "nomatch";

    fapl = h5_fileaccess();

    /* Make a copy of the FAPL before adjusting the alignment */
    if((new_fapl = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* alignment is not set for the following tests */
    if(H5Pset_alignment(fapl, (hsize_t)1, (hsize_t)1) < 0)
	TEST_ERROR

    /* meta/small data is set to 2048 for the following tests */
    if(H5Pset_meta_block_size(fapl, (hsize_t)TEST_BLOCK_SIZE2048) < 0)
	TEST_ERROR
    if(H5Pset_small_data_block_size(fapl, (hsize_t)TEST_BLOCK_SIZE2048) < 0)
	TEST_ERROR

    /* interaction with file allocation */
    nerrors += test_mf_eoa(env_h5_drvr, fapl);
    nerrors += test_mf_eoa_shrink(env_h5_drvr, fapl);
    nerrors += test_mf_eoa_extend(env_h5_drvr, fapl);

    /* interaction with temporary file space allocation */
    nerrors += test_mf_tmp(env_h5_drvr, fapl);

    /* interaction with free-space manager */
    nerrors += test_mf_fs_start(fapl);
    nerrors += test_mf_fs_alloc_free(fapl);
    nerrors += test_mf_fs_extend(fapl);
    nerrors += test_mf_fs_absorb(env_h5_drvr, fapl);

    /* interaction with meta/sdata aggregator */
    nerrors += test_mf_aggr_alloc1(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc2(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc3(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc4(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc5(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc6(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_alloc7(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_extend(env_h5_drvr, fapl);
    nerrors += test_mf_aggr_absorb(env_h5_drvr, fapl);

    /* Tests for alignment */
    for(curr_test = TEST_NORMAL; curr_test < TEST_NTESTS; H5_INC_ENUM(test_type_t, curr_test)) {

	switch(curr_test) {
            case TEST_NORMAL: /* set alignment = 1024 */
		if(H5Pset_alignment(new_fapl, (hsize_t)0, (hsize_t)TEST_ALIGN1024) < 0)
		    TEST_ERROR
                break;

            case TEST_AGGR_SMALL: /* set alignment = 4096 */
		if(H5Pset_alignment(new_fapl, (hsize_t)0, (hsize_t)TEST_ALIGN4096) < 0)
		    TEST_ERROR
                break;

            case TEST_NTESTS:
            default:
                TEST_ERROR;
		break;
	} /* end switch */

	nerrors += test_mf_align_eoa(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_fs(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_alloc1(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_alloc2(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_alloc3(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_alloc4(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_alloc5(env_h5_drvr, fapl, new_fapl);
	nerrors += test_mf_align_alloc6(env_h5_drvr, fapl, new_fapl);
    } /* end if */

    if(H5Pclose(new_fapl) < 0)
        FAIL_STACK_ERROR
    h5_cleanup(FILENAME, fapl);

    if(nerrors)
        goto error;
    puts("All free-space manager tests for file memory passed.");

    return(0);

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Pclose(fapl);
        H5Pclose(new_fapl);
    } H5E_END_TRY;
    return(1);
} /* main() */
