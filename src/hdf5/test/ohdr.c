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

/* Programmer:  Robb Matzke <matzke@llnl.gov>
 *              Tuesday, November 24, 1998
 */
#include "h5test.h"
#include "H5srcdir.h"
#include "H5Iprivate.h"

/*
 * This file needs to access private datatypes from the H5O package.
 * This file also needs to access the object header testing code.
 */
#define H5O_PACKAGE
#define H5O_TESTING
#include "H5Opkg.h"

/*
 * This file needs to access private datatypes from the H5G package.
 */
#define H5G_PACKAGE
#include "H5Gpkg.h"

const char *FILENAME[] = {
    "ohdr",
    NULL
};

/* The tbogus.h5 is generated from gen_bogus.c in HDF5 'test' directory.
 * To get this data file, define H5O_ENABLE_BOGUS in src/H5Oprivate, rebuild
 * the library and simply compile gen_bogus.c with that HDF5 library and run it.
 */
#define FILE_BOGUS "tbogus.h5"

/*
 *  Verify that messages are moved forward into a "continuation message":
 *	Create an object header with several continuation chunks
 *	Remove a message in the last chunk
 *	The remaining message(s) in the last chunk should be moved forward into the continuation message
 *	The process will repeat when the continuation message is big enough to hold all the
 *		messages in the last chunk.
 *	Result: the number of chunks should be reduced
 */
static herr_t
test_cont(char *filename, hid_t fapl)
{
    hid_t	file=-1;
    H5F_t	*f = NULL;
    H5O_hdr_info_t hdr_info;
    H5O_loc_t	oh_locA, oh_locB;
    time_t	time_new;
    const char	*short_name = "T";
    const char	*long_name = "This is the message";
    size_t	nchunks;

    TESTING("object header continuation block");

    HDmemset(&oh_locA, 0, sizeof(oh_locA));
    HDmemset(&oh_locB, 0, sizeof(oh_locB));

    /* Create the file to operate on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR
    if(NULL == (f = (H5F_t *)H5I_object(file))) FAIL_STACK_ERROR

    if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)H5O_MIN_SIZE, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_locA/*out*/) < 0)
            FAIL_STACK_ERROR

    if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)H5O_MIN_SIZE, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_locB/*out*/) < 0)
            FAIL_STACK_ERROR

    time_new = 11111111;

    if(H5O_msg_create(&oh_locA, H5O_NAME_ID, 0, 0, &long_name, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR
    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR
    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locA, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locB, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
        FAIL_STACK_ERROR

    if(H5O_msg_create(&oh_locA, H5O_NAME_ID, 0, 0, &short_name, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR

    if(1 != H5O_link(&oh_locA, 1, H5P_DATASET_XFER_DEFAULT))
        FAIL_STACK_ERROR
    if(1 != H5O_link(&oh_locB, 1, H5P_DATASET_XFER_DEFAULT))
        FAIL_STACK_ERROR
    if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR
    if(H5O_expunge_chunks_test(&oh_locA, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR

    if(H5O_get_hdr_info(&oh_locA, H5P_DATASET_XFER_DEFAULT, &hdr_info) < 0)
	FAIL_STACK_ERROR
    nchunks = hdr_info.nchunks;

    /* remove the 1st H5O_NAME_ID message */
    if(H5O_msg_remove(&oh_locA, H5O_NAME_ID, 0, FALSE, H5P_DATASET_XFER_DEFAULT) < 0)
	FAIL_STACK_ERROR

    if(H5O_get_hdr_info(&oh_locA, H5P_DATASET_XFER_DEFAULT, &hdr_info) < 0)
	FAIL_STACK_ERROR

    if(hdr_info.nchunks >= nchunks)
	TEST_ERROR

    if(H5O_close(&oh_locA) < 0)
	FAIL_STACK_ERROR
    if(H5O_close(&oh_locB) < 0)
	FAIL_STACK_ERROR
    if(H5Fclose(file) < 0)
	FAIL_STACK_ERROR

    PASSED();


    return 0;

error:
    H5E_BEGIN_TRY {
        H5O_close(&oh_locA);
        H5O_close(&oh_locB);
        H5Fclose(file);
    } H5E_END_TRY;

    return -1;
} /* test_cont() */

/*
 *  Verify that object headers are held in the cache until they are linked
 *      to a location in the graph, or assigned an ID.  This is done by
 *      creating an object header, then forcing it out of the cache by creating
 *      local heaps until the object header is evicted from the cache, then
 *      modifying the object header.  The refcount on the object header is
 *      checked as verifying that the object header has remained in the cache.
 */
static herr_t
test_ohdr_cache(char *filename, hid_t fapl)
{
    hid_t	file = -1;              /* File ID */
    hid_t       my_fapl;                /* FAPL ID */
    hid_t       my_dxpl;                /* DXPL ID */
    H5AC_cache_config_t mdc_config;     /* Metadata cache configuration info */
    H5F_t	*f = NULL;              /* File handle */
    H5HL_t      *lheap, *lheap2, *lheap3; /* Pointer to local heaps */
    haddr_t     lheap_addr, lheap_addr2, lheap_addr3; /* Local heap addresses */
    H5O_loc_t	oh_loc;                 /* Object header location */
    time_t	time_new;               /* Time value for modification time message */
    unsigned    rc;                     /* Refcount for object */

    TESTING("object header creation in cache");

    /* Make a copy of the FAPL */
    if((my_fapl = H5Pcopy(fapl)) < 0)
        FAIL_STACK_ERROR

    /* Tweak down the size of the metadata cache to only 64K */
    mdc_config.version = H5AC__CURR_CACHE_CONFIG_VERSION;
    if(H5Pget_mdc_config(my_fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR
    mdc_config.set_initial_size = TRUE;
    mdc_config.initial_size = 32 * 1024;
    mdc_config.max_size = 64 * 1024;
    mdc_config.min_size = 8 * 1024;
    if(H5Pset_mdc_config(my_fapl, &mdc_config) < 0)
        FAIL_STACK_ERROR

    /* Make a copy of the default DXPL */
    if((my_dxpl = H5Pcopy(H5P_DATASET_XFER_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Create the file to operate on */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(my_fapl) < 0)
	FAIL_STACK_ERROR
    if(NULL == (f = (H5F_t *)H5I_object(file)))
        FAIL_STACK_ERROR

    /* Create object (local heap) that occupies most of cache */
    if(H5HL_create(f, my_dxpl, (31 * 1024), &lheap_addr) < 0)
        FAIL_STACK_ERROR

    /* Protect local heap (which actually pins it in the cache) */
    if(NULL == (lheap = H5HL_protect(f, my_dxpl, lheap_addr, H5AC_READ)))
        FAIL_STACK_ERROR

    /* Create an object header */
    HDmemset(&oh_loc, 0, sizeof(oh_loc));
    if(H5O_create(f, my_dxpl, (size_t)2048, (size_t)1, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
        FAIL_STACK_ERROR

    /* Query object header information */
    rc = 0;
    if(H5O_get_rc(&oh_loc, my_dxpl, &rc) < 0)
        FAIL_STACK_ERROR
    if(0 != rc)
        TEST_ERROR

    /* Create object (local heap) that occupies most of cache */
    if(H5HL_create(f, my_dxpl, (31 * 1024), &lheap_addr2) < 0)
        FAIL_STACK_ERROR

    /* Protect local heap (which actually pins it in the cache) */
    if(NULL == (lheap2 = H5HL_protect(f, my_dxpl, lheap_addr2, H5AC_READ)))
        FAIL_STACK_ERROR

    /* Unprotect local heap (which actually unpins it from the cache) */
    if(H5HL_unprotect(lheap2) < 0)
        FAIL_STACK_ERROR

    /* Create object header message in new object header */
    time_new = 11111111;
    if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, my_dxpl) < 0)
        FAIL_STACK_ERROR

    /* Create object (local heap) that occupies most of cache */
    if(H5HL_create(f, my_dxpl, (31 * 1024), &lheap_addr3) < 0)
        FAIL_STACK_ERROR

    /* Protect local heap (which actually pins it in the cache) */
    if(NULL == (lheap3 = H5HL_protect(f, my_dxpl, lheap_addr3, H5AC_READ)))
        FAIL_STACK_ERROR

    /* Unprotect local heap (which actually unpins it from the cache) */
    if(H5HL_unprotect(lheap3) < 0)
        FAIL_STACK_ERROR

    /* Query object header information */
    /* (Note that this is somewhat of a weak test, since it doesn't actually
     *  verify that the object header was evicted from the cache, but it's
     *  very difficult to verify when an entry is evicted from the cache in
     *  a non-invasive way -QAK)
     */
    rc = 0;
    if(H5O_get_rc(&oh_loc, my_dxpl, &rc) < 0)
        FAIL_STACK_ERROR
    if(0 != rc)
        TEST_ERROR

    /* Decrement reference count o object header */
    if(H5O_dec_rc_by_loc(&oh_loc, my_dxpl) < 0)
        FAIL_STACK_ERROR

    /* Close object header created */
    if(H5O_close(&oh_loc) < 0)
        FAIL_STACK_ERROR

    /* Unprotect local heap (which actually unpins it from the cache) */
    if(H5HL_unprotect(lheap) < 0)
        FAIL_STACK_ERROR

    if(H5Pclose(my_dxpl) < 0)
	FAIL_STACK_ERROR
    if(H5Fclose(file) < 0)
	FAIL_STACK_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;

    return -1;
} /* test_ohdr_cache() */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:     Exercise private object header behavior and routines
 *
 * Return:	Success:        0
 *		Failure:        1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, November 24, 1998
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    hid_t	fapl = -1, file = -1;
    hid_t	dset = -1;
    H5F_t	*f = NULL;
    char	filename[1024];
    H5O_hdr_info_t hdr_info;            /* Object info */
    H5O_loc_t	oh_loc, oh_loc2;        /* Object header locations */
    time_t	time_new, ro;
    int         chunkno;                /* Chunk index for message */
    int		i;                      /* Local index variable */
    hbool_t     b;                      /* Index for "new format" loop */
    herr_t      ret;                    /* Generic return value */

    /* Reset library */
    h5_reset();
    fapl = h5_fileaccess();
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Loop over old & new formats */
    for(b = FALSE; b <= TRUE; b++) {
        /* Display info about testing */
        if(b)
            HDputs("Using new file format:");
        else
            HDputs("Using default file format:");

        /* Set the format to use for the file */
        if(H5Pset_libver_bounds(fapl, (b ? H5F_LIBVER_LATEST : H5F_LIBVER_EARLIEST), H5F_LIBVER_LATEST) < 0)
            FAIL_STACK_ERROR

	/* test on object continuation block */
	if(test_cont(filename, fapl) < 0)
            FAIL_STACK_ERROR

        /* Create the file to operate on */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            TEST_ERROR
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR


        /*
         * Test object header creation
         * (using default group creation property list only because it's convenient)
         */
        TESTING("object header creation");
        HDmemset(&oh_loc, 0, sizeof(oh_loc));
        if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
            FAIL_STACK_ERROR
        PASSED();


        /* create a new message */
        TESTING("message creation");
        time_new = 11111111;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(1 != H5O_link(&oh_loc, 1, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR
        PASSED();


        /*
         * Test modification of an existing message.
         */
        TESTING("message modification");
        time_new = 33333333;
        if(H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR

        /* Make certain that chunk #0 in the object header can be encoded with a 1-byte size */
        if(H5O_get_hdr_info(&oh_loc, H5P_DATASET_XFER_DEFAULT, &hdr_info) < 0)
            FAIL_STACK_ERROR
        if(hdr_info.space.total >=256)
            TEST_ERROR

        PASSED();

        /*
         * Test creation of a bunch of messages one after another to see
         * what happens when the object header overflows in core.
         * (Use 'old' MTIME message here, because it is large enough to be
         *  replaced with a continuation message (the new one is too small)
         *  and the library doesn't understand how to migrate more than one
         *  message from an object header currently - QAK - 10/8/03)
         */
        TESTING("object header overflow in memory");
        for(i = 0; i < 40; i++) {
            time_new = (i + 1) * 1000 + 1000000;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
        } /* end for */
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR

        /* Make certain that chunk #0 in the object header will be encoded with a 2-byte size */
        if(H5O_get_hdr_info(&oh_loc, H5P_DATASET_XFER_DEFAULT, &hdr_info) < 0)
            FAIL_STACK_ERROR
        if(hdr_info.space.total < 256)
            TEST_ERROR

        PASSED();

        /* Close & re-open file & object header */
        /* (makes certain that an object header in the new format that transitions
         *  between 1-byte chunk #0 size encoding and 2-byte chunk #0 size encoding
         *  works correctly - QAK)
         */
        TESTING("close & re-open object header");
        if(H5O_close(&oh_loc) < 0)
            FAIL_STACK_ERROR
        if(H5Fclose(file) < 0)
            FAIL_STACK_ERROR
        if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
            FAIL_STACK_ERROR
        if(NULL == (f = (H5F_t *)H5I_object(file)))
            FAIL_STACK_ERROR
        oh_loc.file = f;
        if(H5O_open(&oh_loc) < 0)
            FAIL_STACK_ERROR
        PASSED();

        /*
         * Test creation of a bunch of messages one after another to see
         * what happens when the object header overflows on disk.
         */
        TESTING("object header overflow on disk");
        for(i = 0; i < 10; i++) {
            time_new = (i + 1) * 1000 + 10;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
            if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
            if(H5AC_expunge_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
                FAIL_STACK_ERROR
        } /* end for */
        PASSED();

        /*
         * Delete all time messages.
         */
        TESTING("message deletion");
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(H5O_msg_read(&oh_loc, H5O_MTIME_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        PASSED();


        /*
         * Constant message handling.
         * (can't write to them, but should be able to remove them)
         */
        TESTING("constant message handling");
        time_new = 22222222;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, H5O_MSG_FLAG_CONSTANT, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_flush(f, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        if(H5AC_expunge_entry(f, H5P_DATASET_XFER_DEFAULT, H5AC_OHDR, oh_loc.addr, H5AC__NO_FLAGS_SET) < 0)
            FAIL_STACK_ERROR
        if(NULL == H5O_msg_read(&oh_loc, H5O_MTIME_NEW_ID, &ro, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR
        if(ro != time_new)
            TEST_ERROR
        time_new = 33333333;
        H5E_BEGIN_TRY {
            ret = H5O_msg_write(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0)
            TEST_ERROR
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR
        PASSED();


        /* release resources */
        TESTING("object header closing");
        if(H5O_close(&oh_loc) < 0)
            FAIL_STACK_ERROR
        PASSED();

        /*
         * Test moving message to first chunk
         */
        TESTING("locking messages");
        HDmemset(&oh_loc, 0, sizeof(oh_loc));
        if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
            FAIL_STACK_ERROR
        if(1 != H5O_link(&oh_loc, 1, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR

        /* Create second object header, to guarantee that first object header uses multiple chunks */
        HDmemset(&oh_loc2, 0, sizeof(oh_loc2));
        if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc2/*out*/) < 0)
            FAIL_STACK_ERROR
        if(1 != H5O_link(&oh_loc2, 1, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR

        /* Fill object header with messages, creating multiple chunks */
        for(i = 0; i < 10; i++) {
            time_new = (i + 1) * 1000 + 10;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
        } /* end for */

        /* Get # of object header chunks */
        if(H5O_get_hdr_info(&oh_loc, H5P_DATASET_XFER_DEFAULT, &hdr_info) < 0)
            FAIL_STACK_ERROR
        if(hdr_info.nchunks != 2)
            TEST_ERROR

        /* Add message to lock to object header */
        time_new = 11111111;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Verify chunk index for message */
        if((chunkno = H5O_msg_get_chunkno(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        if(chunkno != 1)
            TEST_ERROR

        /* Lock the message into the chunk */
        if(H5O_msg_lock(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Attempt to lock the message twice */
        H5E_BEGIN_TRY {
            ret = H5O_msg_lock(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0)
            TEST_ERROR

        /* Delete all the other messages, which would move the message into
         * chunk #0, if it wasn't locked
         */
        if(H5O_msg_remove(&oh_loc, H5O_MTIME_NEW_ID, H5O_ALL, TRUE, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Verify chunk index for message */
        if((chunkno = H5O_msg_get_chunkno(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        if(chunkno != 1)
            TEST_ERROR

        /* Unlock the message */
        if(H5O_msg_unlock(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Attempt to unlock the message twice */
        H5E_BEGIN_TRY {
            ret = H5O_msg_unlock(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT);
        } H5E_END_TRY;
        if(ret >= 0)
            TEST_ERROR

        /* Close object headers */
        if(H5O_close(&oh_loc2) < 0)
            FAIL_STACK_ERROR
        if(H5O_close(&oh_loc) < 0)
            FAIL_STACK_ERROR

        /* Open first object header */
        HDmemset(&oh_loc, 0, sizeof(oh_loc));
        if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc/*out*/) < 0)
            FAIL_STACK_ERROR
        if(1 != H5O_link(&oh_loc, 1, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR

        /* Create second object header, to guarantee that first object header uses multiple chunks */
        HDmemset(&oh_loc2, 0, sizeof(oh_loc2));
        if(H5O_create(f, H5P_DATASET_XFER_DEFAULT, (size_t)64, (size_t)0, H5P_GROUP_CREATE_DEFAULT, &oh_loc2/*out*/) < 0)
            FAIL_STACK_ERROR
        if(1 != H5O_link(&oh_loc2, 1, H5P_DATASET_XFER_DEFAULT))
            FAIL_STACK_ERROR

        /* Add message to move to object header */
        time_new = 11111111;
        if(H5O_msg_create(&oh_loc, H5O_MTIME_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Verify chunk index for message */
        if((chunkno = H5O_msg_get_chunkno(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        if(chunkno != 0)
            TEST_ERROR

        /* Lock the message into the chunk */
        if(H5O_msg_lock(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Fill object header with messages, creating multiple chunks */
        /* (would normally move locked message to new chunk) */
        for(i = 0; i < 10; i++) {
            time_new = (i + 1) * 1000 + 10;
            if(H5O_msg_create(&oh_loc, H5O_MTIME_NEW_ID, 0, 0, &time_new, H5P_DATASET_XFER_DEFAULT) < 0)
                FAIL_STACK_ERROR
        } /* end for */

        /* Get # of object header chunks */
        if(H5O_get_hdr_info(&oh_loc, H5P_DATASET_XFER_DEFAULT, &hdr_info) < 0)
            FAIL_STACK_ERROR
        if(hdr_info.nchunks != 2)
            TEST_ERROR

        /* Verify chunk index for message */
        if((chunkno = H5O_msg_get_chunkno(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT)) < 0)
            FAIL_STACK_ERROR
        if(chunkno != 0)
            TEST_ERROR

        /* Unlock the message */
        if(H5O_msg_unlock(&oh_loc, H5O_MTIME_ID, H5P_DATASET_XFER_DEFAULT) < 0)
            FAIL_STACK_ERROR

        /* Close object headers */
        if(H5O_close(&oh_loc2) < 0)
            FAIL_STACK_ERROR
        if(H5O_close(&oh_loc) < 0)
            FAIL_STACK_ERROR

        PASSED();


        /* Test reading datasets with undefined object header messages */
        HDputs("Accessing objects with unknown header messages:");
        {
            hid_t file2;                    /* File ID for 'bogus' object file */
            hid_t sid;                      /* Dataspace ID */
            hid_t aid;                      /* Attribute ID */
            char testpath[512] = "";
            char testfile[512] = "";
            char *srcdir = HDgetenv("srcdir");

            /* Build path to all test files */
            if(srcdir && ((HDstrlen(srcdir) + 2) < sizeof(testpath))) {
                HDstrcpy(testpath, srcdir);
                HDstrcat(testpath, "/");
            } /* end if */

            /* Build path to test file */
            if(srcdir && ((HDstrlen(testpath) + HDstrlen(FILE_BOGUS) + 1) < sizeof(testfile)))
                HDstrcpy(testfile, testpath);
            HDstrcat(testfile, FILE_BOGUS);

            TESTING("object with unknown header message and no flags set");

            /* Open the file with objects that have unknown header messages (generated with gen_bogus.c) */
            if((file2 = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
                TEST_ERROR

            /* Open the dataset with the unknown header message, but no extra flags */
            if((dset = H5Dopen2(file2, "/Dataset1", H5P_DEFAULT)) < 0)
                TEST_ERROR
            if(H5Dclose(dset) < 0)
                TEST_ERROR

            PASSED();

            TESTING("object with unknown header message & 'fail if unknown' flag set");

            /* Attempt to open the dataset with the unknown header message, and "fail if unknown" flag */
            H5E_BEGIN_TRY {
                dset = H5Dopen2(file2, "/Dataset2", H5P_DEFAULT);
            } H5E_END_TRY;
            if(dset >= 0) {
                H5Dclose(dset);
                TEST_ERROR
            } /* end if */

            PASSED();

            TESTING("object with unknown header message & 'mark if unknown' flag set");

            /* Copy object with "mark if unknown" flag on message into file that can be modified */
            if(H5Ocopy(file2, "/Dataset3", file, "/Dataset3", H5P_DEFAULT, H5P_DEFAULT) < 0)
                TEST_ERROR

            /* Close the file we created (to flush changes to file) */
            if(H5Fclose(file) < 0)
                TEST_ERROR

            /* Re-open the file created, with read-only permissions */
            if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
                TEST_ERROR

            /* Open the dataset with the "mark if unknown" message */
            if((dset = H5Dopen2(file, "/Dataset3", H5P_DEFAULT)) < 0)
                TEST_ERROR

            /* Check that the "unknown" message was _NOT_ marked */
            if(H5O_check_msg_marked_test(dset, FALSE) < 0)
                FAIL_STACK_ERROR

            /* Close the dataset */
            if(H5Dclose(dset) < 0)
                TEST_ERROR

            /* Close the file we created (to flush change to object header) */
            if(H5Fclose(file) < 0)
                TEST_ERROR

            /* Re-open the file created */
            if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
                TEST_ERROR

            /* Open the dataset with the "mark if unknown" message */
            if((dset = H5Dopen2(file, "/Dataset3", H5P_DEFAULT)) < 0)
                TEST_ERROR

            /* Create data space */
            if((sid = H5Screate(H5S_SCALAR)) < 0)
                FAIL_STACK_ERROR

            /* Create an attribute, to get the object header into write access */
            if((aid = H5Acreate2(dset, "Attr", H5T_NATIVE_INT, sid, H5P_DEFAULT, H5P_DEFAULT)) < 0)
                FAIL_STACK_ERROR

            /* Close dataspace */
            if(H5Sclose(sid) < 0)
                FAIL_STACK_ERROR

            /* Close attribute */
            if(H5Aclose(aid) < 0)
                FAIL_STACK_ERROR

            /* Close the dataset */
            if(H5Dclose(dset) < 0)
                TEST_ERROR

            /* Close the file we created (to flush change to object header) */
            if(H5Fclose(file) < 0)
                TEST_ERROR

            /* Re-open the file created */
            if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
                TEST_ERROR

            /* Re-open the dataset with the "mark if unknown" message */
            if((dset = H5Dopen2(file, "/Dataset3", H5P_DEFAULT)) < 0)
                TEST_ERROR

            /* Check that the "unknown" message was marked */
            if(H5O_check_msg_marked_test(dset, TRUE) < 0)
                FAIL_STACK_ERROR

            /* Close the dataset */
            if(H5Dclose(dset) < 0)
                TEST_ERROR


            /* Close the file with the bogus objects */
            if(H5Fclose(file2) < 0)
                TEST_ERROR

            PASSED();
        }

        /* Close the file we created */
        if(H5Fclose(file) < 0)
            TEST_ERROR

	/* Test object header creation metadata cache issues */
	if(test_ohdr_cache(filename, fapl) < 0)
            TEST_ERROR
    } /* end for */

    /* Verify symbol table messages are cached */
    if(h5_verify_cached_stabs(FILENAME, fapl) < 0) TEST_ERROR

    puts("All object header tests passed.");
    h5_cleanup(FILENAME, fapl);
    return(0);

error:
    puts("*** TESTS FAILED ***");
    H5E_BEGIN_TRY {
        H5Fclose(file);
    } H5E_END_TRY;

    return(1);
} /* end main() */

