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
 * Programmer:  Raymond Lu <songyulu@hdfgroup.org>
 *              7 September 2010
 *
 * Purpose:     Make sure dataset, file, and library can close properly when a
 *              mandatory filter fails.
 */

#include "h5test.h"
#include "H5srcdir.h"

#define DSET_NAME 		"dset_fail"
#define H5Z_FILTER_FAIL_TEST    312
#define DIM                     10
#define FILTER_CHUNK_DIM        2

const char *FILENAME[] = {
    "filter_fail_with_cache",
    "filter_fail_without_cache",
    NULL
};

static size_t filter_fail(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);

/* This message derives from H5Z */
const H5Z_class2_t H5Z_FAIL_TEST[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version */
    H5Z_FILTER_FAIL_TEST,	/* Filter id number		*/
    1, 1,                       /* Encoding and decoding enabled */
    "filter_fail_test",		/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_fail,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:    filter_fail
 *
 * Purpose:     For testing library's behavior when a mandatory filter 
 *              fails to write a chunk. 
 *
 * Return:	Success:	Data chunk size
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              7 September 2010
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_fail(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    int   *dst = (int*)(*buf);
    size_t         ret_value = 0;

    if(flags & H5Z_FLAG_REVERSE) { /* do nothing during read */
        *buf_size = nbytes;
        ret_value = nbytes;
    }  /* end if */
    else { /* Write data */
        /* If it's the last chunk, pretend to fail. Otherwise, do nothing. */ 
        if(*dst == 8 || *dst == 9) {
            ret_value = 0;
        } else {
            *buf_size = nbytes;
            ret_value = *buf_size;
        }
    } /* end else */

    return ret_value;
} /* end filter_fail() */


/*-------------------------------------------------------------------------
 * Function:    test_filter_write_failure
 *
 * Purpose:     Tests the library's behavior when a mandate filter returns 
 *              failure.  There're only 5 chunks with each of them having
 *              2 integers.  The filter will fail in the last chunk.  The 
 *              dataset should release all resources even though the last 
 *              chunk can't be flushed to file.  The file should close
 *              successfully.
 *
 * Return:  
 *              Success:         0
 *              Failure:         -1
 *
 * Programmer:  Raymond Lu
 *              25 August 2010
 *
 * Modifications:
 *              Raymond Lu
 *              5 Oct 2010
 *              Test when the chunk cache is enable and disabled to make 
 *              sure the library behaves properly.
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_write(char *file_name, hid_t my_fapl, hbool_t cache_enabled)
{
    hid_t        file = -1;
    hid_t        dataset=-1;                /* dataset ID */
    hid_t        sid=-1;                   /* dataspace ID */
    hid_t        dcpl=-1;                  /* dataset creation property list ID */
    hsize_t      dims[1]={DIM};           /* dataspace dimension - 10*/
    hsize_t      chunk_dims[1]={FILTER_CHUNK_DIM}; /* chunk dimension - 2*/
    int          points[DIM];          /* Data */
    herr_t       ret;                   /* generic return value */
    int          i;

    if(cache_enabled) {
        TESTING("data writing when a mandatory filter fails and chunk cache is enabled");
    } else {
        TESTING("data writing when a mandatory filter fails and chunk cache is disabled");
    }

    /* Create file */
    if((file = H5Fcreate(file_name, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0) TEST_ERROR

    /* create the data space */
    if((sid = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR

    /* Create dcpl and register the filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR

    if(H5Pset_chunk(dcpl, 1, chunk_dims) < 0) TEST_ERROR

    if(H5Zregister (H5Z_FAIL_TEST) < 0) TEST_ERROR

    /* Check that the filter was registered */
    if(TRUE != H5Zfilter_avail(H5Z_FILTER_FAIL_TEST)) FAIL_STACK_ERROR

    /* Enable the filter as mandatory */
    if(H5Pset_filter(dcpl, H5Z_FILTER_FAIL_TEST, 0, (size_t)0, NULL) < 0)
        TEST_ERROR

    /* create a dataset */
    if((dataset = H5Dcreate2(file, DSET_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR 

    /* Initialize the write buffer */
    for(i = 0; i < DIM; i++)
        points[i] = i;

    /* Write data.  If the chunk cache is enabled, H5Dwrite should succeed.  If it is
     * diabled, H5Dwrite should fail. */
    if(cache_enabled) {
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, points) < 0) 
            TEST_ERROR
    } else {
        /* Data writing should fail */
        H5E_BEGIN_TRY {
            ret = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, points);
        } H5E_END_TRY;
        if(ret >= 0) {
	    H5_FAILED();
	    puts("    Data writing is supposed to fail because the chunk can't be written to file.");
	    TEST_ERROR
        }
    }

    /* clean up objects used for this test */
    if(H5Pclose (dcpl) < 0) TEST_ERROR
    if(H5Sclose (sid) < 0) TEST_ERROR

    /* Close dataset.  If the chunk cache is enabled, the flushing of chunks should fail
     * during H5Dclose.  If it is diabled, H5Dwrite should fail but H5Dclose should succeed. */
    if(cache_enabled) {
        H5E_BEGIN_TRY {
            ret = H5Dclose (dataset);
        } H5E_END_TRY;
        if(ret >= 0) {
	    H5_FAILED();
	    puts("    Dataset is supposed to fail because the chunk can't be flushed to file.");
	    TEST_ERROR
        }
    } else {
        if(H5Dclose (dataset) < 0)
            TEST_ERROR
    }

    /* Even though H5Dclose or H5Dwrite fails, it should release all resources.  
     * So the file should close successfully. */
    if(H5Fclose (file) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(sid);
        H5Dclose(dataset);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
} /* end test_filter_write() */


/*-------------------------------------------------------------------------
 * Function:    test_filter_read
 *
 * Purpose:     Tests the library's behavior when a mandate filter returns 
 *              failure.  The first 4 chunks should be in the file.  The 
 *              last chunk should not.
 *
 * Return:  
 *              Success:         0
 *              Failure:         -1
 *
 * Programmer:  Raymond Lu
 *              25 August 2010
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_read(char *file_name, hid_t my_fapl)
{
    hid_t        file = -1;
    hid_t        dataset=-1;                /* dataset ID */
    hid_t        sid = -1;
    hid_t        mspace = -1;
    hsize_t      dims[1]={DIM};           /* dataspace dimension - 10*/
    int          rbuf[DIM];          /* Data */
    hsize_t      dset_size = 0;          /* Dataset storage size */
    hsize_t      hs_offset[H5S_MAX_RANK];
    hsize_t      hs_size[H5S_MAX_RANK];
    hsize_t      stride[1] = {2};
    hsize_t      zero[8];
    hsize_t      nelmts = DIM/2;
    int          i;

    TESTING("data reading when a mandatory filter fails");

    /* Open file */
    if((file = H5Fopen(file_name, H5F_ACC_RDONLY, my_fapl)) < 0) TEST_ERROR

    /* Open dataset */
    if((dataset = H5Dopen2(file, DSET_NAME, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Verify the storage size is equal to 4 chunks */
    if((dset_size = H5Dget_storage_size(dataset)) == 0) TEST_ERROR

    if(dset_size != 4 * FILTER_CHUNK_DIM * sizeof(int)) TEST_ERROR

    /* Read the chunks */
    HDmemset(rbuf, 0, DIM * sizeof(int));
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR

    /* Check that the values read are the same as the values written.
     * The last chunk should not be in the file. */
    for(i = 0; i < DIM; i++) {
        if(i < DIM-2 && rbuf[i] != i) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            printf("    rbuf[%d]=%d\n", i, rbuf[i]);
            TEST_ERROR
        } else if(i >= DIM-2 && rbuf[i] != 0) {
            H5_FAILED();
            printf("    No value should be read.\n");
            printf("    At index %d\n", i);
            printf("    rbuf[%d]=%d\n", i, rbuf[i]);
            TEST_ERROR
        }
    }

    /* Try to read in hyperslab simulating the h5dump's way of printing data */
    if((sid = H5Dget_space(dataset)) < 0) TEST_ERROR

    HDmemset(hs_offset, 0, sizeof(hs_offset));
    HDmemset(hs_size,   0, sizeof(hs_size));
    hs_size[0] = DIM/2;

    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, stride, hs_size, NULL) < 0)
        TEST_ERROR

    /* create the data space */
    if((mspace = H5Screate_simple(1, dims, NULL)) < 0) TEST_ERROR

    HDmemset(zero, 0, sizeof zero);

    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, zero, stride, &nelmts, NULL) < 0)
        TEST_ERROR

    HDmemset(rbuf, 0, DIM * sizeof(int));
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR

    /* Check that the values read are the same as the values written. 
     * The last chunk should not be in the file. */
    for(i = 0; i < DIM; i+=2) {
        if(i < DIM-2 && rbuf[i] != i) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %d\n", i);
            printf("    rbuf[%d]=%d\n", i, rbuf[i]);
            TEST_ERROR
        } else if(i >= DIM-2 && rbuf[i] != 0) {
            H5_FAILED();
            printf("    No value should be read.\n");
            printf("    At index %d\n", i);
            printf("    rbuf[%d]=%d\n", i, rbuf[i]);
            TEST_ERROR
        }
    }

    if(H5Sclose (sid) < 0) TEST_ERROR
    if(H5Sclose (mspace) < 0) TEST_ERROR
    if(H5Dclose (dataset) < 0) TEST_ERROR
    if(H5Fclose (file) < 0) TEST_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid);
        H5Sclose(mspace);
        H5Dclose(dataset);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
} /* end test_filter_read() */

/*-------------------------------------------------------------------------
 * Function:    main
 *
 * Purpose:     Tests the library's behavior when a mandate filter returns 
 *              failure.
 *
 * Return:      Success:        exit(0)
 *              Failure:        exit(1)
 * 
 * Programmer:  Raymond Lu
 *              25 August 2010
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
int main(void)
{
    hid_t       fapl;
    int         mdc_nelmts  = 0;
    size_t      rdcc_nelmts = 0;
    size_t      rdcc_nbytes = 0;
    double      rdcc_w0     = 0;
    char        filename[1024];
    unsigned 	nerrors = 0;

    h5_reset();
    fapl = h5_fileaccess();

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* The chunk cache is used so that the flushing of data chunks happens
     * during H5Dclose.  All values are default. */
    nerrors += (test_filter_write(filename, fapl, TRUE) < 0	? 1 : 0);
    nerrors += (test_filter_read(filename, fapl) < 0		? 1 : 0);

    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);

    /* Disable the chunk cache so that the writing of data chunks happens
     * during H5Dwrite. */
    if(H5Pset_cache(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
        TEST_ERROR

    nerrors += (test_filter_write(filename, fapl, FALSE) < 0	? 1 : 0);
    nerrors += (test_filter_read(filename, fapl) < 0		? 1 : 0);

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl);

    /* Make sure we can close the library */
    if(H5close() < 0) TEST_ERROR

    if (nerrors) TEST_ERROR

    return 0;

error:
    if (nerrors) {
        printf("***** %u FAILURE%s! *****\n",
               nerrors, 1==nerrors?"":"S");
        HDexit(1);
    }
}
