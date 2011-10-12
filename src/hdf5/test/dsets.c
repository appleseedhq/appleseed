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
 * Programmer:	Robb Matzke <matzke@llnl.gov>
 *		Tuesday, December  9, 1997
 *
 * Purpose:	Tests the dataset interface (H5D)
 */

#include <stdlib.h>
#include <time.h>

#include "h5test.h"
#include "H5srcdir.h"
#ifdef H5_HAVE_SZLIB_H
#   include "szlib.h"
#endif

/*
 * This file needs to access private datatypes from the H5Z package.
 */
#define H5Z_PACKAGE
#include "H5Zpkg.h"


const char *FILENAME[] = {
    "dataset",
    "compact_dataset",
    "dset_offset",
    "max_compact_dataset",
    "simple",
    "set_local",
    "random_chunks",
    "huge_chunks",
    "chunk_cache",
    "big_chunk",
    "chunk_expand",
    "copy_dcpl_newfile",
    "layout_extend",
    NULL
};
#define FILENAME_BUF_SIZE       1024
#define KB                      1024

#define FILE_DEFLATE_NAME       "deflate.h5"

/* Dataset names for testing filters */
#define DSET_DEFAULT_NAME	"default"
#define DSET_CHUNKED_NAME	"chunked"
#define DSET_COMPACT_NAME       "compact"
#define DSET_SIMPLE_IO_NAME	"simple_io"
#define DSET_USERBLOCK_IO_NAME	"userblock_io"
#define DSET_COMPACT_IO_NAME    "compact_io"
#define DSET_COMPACT_MAX_NAME   "max_compact"
#define DSET_COMPACT_MAX2_NAME   "max_compact_2"
#define DSET_CONV_BUF_NAME	"conv_buf"
#define DSET_TCONV_NAME		"tconv"
#define DSET_DEFLATE_NAME	"deflate"
#ifdef H5_HAVE_FILTER_SZIP
#define DSET_SZIP_NAME          "szip"
#endif /* H5_HAVE_FILTER_SZIP */
#define DSET_SHUFFLE_NAME	"shuffle"
#define DSET_FLETCHER32_NAME	"fletcher32"
#define DSET_FLETCHER32_NAME_2	"fletcher32_2"
#define DSET_FLETCHER32_NAME_3	"fletcher32_3"
#define DSET_SHUF_DEF_FLET_NAME	"shuffle+deflate+fletcher32"
#define DSET_SHUF_DEF_FLET_NAME_2	"shuffle+deflate+fletcher32_2"
#if defined H5_HAVE_FILTER_SZIP && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32
#define DSET_SHUF_SZIP_FLET_NAME	"shuffle+szip+fletcher32"
#define DSET_SHUF_SZIP_FLET_NAME_2	"shuffle+szip+fletcher32_2"
#endif /* defined H5_HAVE_FILTER_SZIP && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32 */

#define DSET_BOGUS_NAME		"bogus"
#define DSET_MISSING_NAME	"missing"
#define DSET_CAN_APPLY_NAME	"can_apply"
#define DSET_CAN_APPLY_NAME2	"can_apply2"
#ifdef H5_HAVE_FILTER_SZIP
#define DSET_CAN_APPLY_SZIP_NAME	"can_apply_szip"
#endif /* H5_HAVE_FILTER_SZIP */
#define DSET_SET_LOCAL_NAME	"set_local"
#define DSET_SET_LOCAL_NAME_2	"set_local_2"
#define DSET_ONEBYTE_SHUF_NAME	"onebyte_shuffle"
#define DSET_NBIT_INT_NAME             "nbit_int"
#define DSET_NBIT_FLOAT_NAME           "nbit_float"
#define DSET_NBIT_DOUBLE_NAME          "nbit_double"
#define DSET_NBIT_ARRAY_NAME           "nbit_array"
#define DSET_NBIT_COMPOUND_NAME        "nbit_compound"
#define DSET_NBIT_COMPOUND_NAME_2      "nbit_compound_2"
#define DSET_NBIT_COMPOUND_NAME_3      "nbit_compound_3"
#define DSET_NBIT_INT_SIZE_NAME        "nbit_int_size"
#define DSET_NBIT_FLT_SIZE_NAME        "nbit_flt_size"
#define DSET_SCALEOFFSET_INT_NAME      "scaleoffset_int"
#define DSET_SCALEOFFSET_INT_NAME_2    "scaleoffset_int_2"
#define DSET_SCALEOFFSET_FLOAT_NAME    "scaleoffset_float"
#define DSET_SCALEOFFSET_FLOAT_NAME_2  "scaleoffset_float_2"
#define DSET_SCALEOFFSET_DOUBLE_NAME   "scaleoffset_double"
#define DSET_SCALEOFFSET_DOUBLE_NAME_2 "scaleoffset_double_2"
#define DSET_COMPARE_DCPL_NAME		"compare_dcpl"
#define DSET_COMPARE_DCPL_NAME_2	"compare_dcpl_2"
#define DSET_COPY_DCPL_NAME_1		"copy_dcpl_1"
#define DSET_COPY_DCPL_NAME_2		"copy_dcpl_2"
#define COPY_DCPL_EXTFILE_NAME          "ext_file"
#define DSET_DEPREC_NAME		"deprecated"
#define DSET_DEPREC_NAME_CHUNKED	"deprecated_chunked"
#define DSET_DEPREC_NAME_COMPACT	"deprecated_compact"
#define DSET_DEPREC_NAME_FILTER         "deprecated_filter"

#define USER_BLOCK              1024
#define SIXTY_FOUR_KB           65536

/* Temporary filter IDs used for testing */
#define H5Z_FILTER_BOGUS	305
#define H5Z_FILTER_CORRUPT	306
#define H5Z_FILTER_CAN_APPLY_TEST	307
#define H5Z_FILTER_SET_LOCAL_TEST	308
#define H5Z_FILTER_DEPREC       309
#define H5Z_FILTER_EXPAND	310
#define H5Z_FILTER_CAN_APPLY_TEST2	311

/* Flags for testing filters */
#define DISABLE_FLETCHER32      0
#define ENABLE_FLETCHER32       1
#define DATA_CORRUPTED          1
#define DATA_NOT_CORRUPTED      0

/* Parameters for the "set local" test */
#define BOGUS2_PERM_NPARMS      2       /* Number of "permanent" parameters */
#define BOGUS2_PARAM_1          13      /* (No particular meaning, just for checking value) */
#define BOGUS2_PARAM_2          35      /* (No particular meaning, just for checking value) */
#define BOGUS2_ALL_NPARMS       4       /* Total number of parameter = permanent + "local" parameters */

/* Dimensionality for conversion buffer test */
#define DIM1          100  /* Dim. Size of data member # 1 */
#define DIM2         5000  /* Dim. Size of data member # 2 */
#define DIM3           10  /* Dim. Size of data member # 3 */

/* Parameters for internal filter test */
#define FILTER_CHUNK_DIM1       2
#define FILTER_CHUNK_DIM2       25
#define FILTER_HS_OFFSET1       7
#define FILTER_HS_OFFSET2       30
#define FILTER_HS_SIZE1         4
#define FILTER_HS_SIZE2         50

/* Names for noencoder test */
#ifdef H5_HAVE_FILTER_SZIP
#define NOENCODER_FILENAME "noencoder.h5"
#define NOENCODER_COPY_FILENAME "noencoder.h5.copy"
#define NOENCODER_TEST_DATASET "noencoder_tdset.h5"
#define NOENCODER_SZIP_DATASET "noencoder_szip_dset.h5"
#define NOENCODER_SZIP_SHUFF_FLETCH_DATASET "noencoder_szip_shuffle_fletcher_dset.h5"
#endif /* H5_HAVE_FILTER_SZIP */

/* Names for zero-dim test */
#define ZERODIM_DATASET "zerodim"

/* Parameters for zero-dim test */
#define MISSING_CHUNK_DATASET   "missing_chunk"
#define MISSING_CHUNK_DIM       100

/* Names for random chunks test */
#define NPOINTS         50

/* Parameters for huge chunks test */
#define HUGE_DATASET            "Dataset"
#define HUGE_DIM                ((hsize_t)16 * 1024 * 1024 * 1024)
#define HUGE_CHUNK_DIM          ((hsize_t)2 * 1024 * 1024 * 1024)
#define TOO_HUGE_CHUNK_DIM      ((hsize_t)4 * 1024 * 1024 * 1024)
#define HUGE_DATASET2           "Dataset2"
#define HUGE_DIM2_0             ((hsize_t)16 * 1024)
#define HUGE_DIM2_1             ((hsize_t)16 * 1024)
#define HUGE_DIM2_2             ((hsize_t)16 * 1024)
#define HUGE_CHUNK_DIM2_0       ((hsize_t)2 * 1024)
#define HUGE_CHUNK_DIM2_1       ((hsize_t)1024)
#define HUGE_CHUNK_DIM2_2       ((hsize_t)1024)
#define TOO_HUGE_CHUNK_DIM2_0   ((hsize_t)4 * 1024)
#define TOO_HUGE_CHUNK_DIM2_1   ((hsize_t)1024)
#define TOO_HUGE_CHUNK_DIM2_2   ((hsize_t)1024)

/* Parameters for testing bypassing chunk cache */
#define BYPASS_DATASET1          "Dset1"
#define BYPASS_DATASET2          "Dset2"
#define BYPASS_DIM               1000
#define BYPASS_CHUNK_DIM         500
#define BYPASS_FILL_VALUE        7

/* Declarations for test_idx_compatible() */
#define	FIXED_IDX_FILE	"fixed_idx.h5"
#define DSET            "dset"
#define DSET_FILTER     "dset_filter"

/* Shared global arrays */
#define DSET_DIM1       100
#define DSET_DIM2       200
int	points[DSET_DIM1][DSET_DIM2], check[DSET_DIM1][DSET_DIM2];
double	points_dbl[DSET_DIM1][DSET_DIM2], check_dbl[DSET_DIM1][DSET_DIM2];

/* Local prototypes for filter functions */
static size_t filter_bogus(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static htri_t can_apply_bogus(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static herr_t set_local_bogus2(hid_t dcpl_id, hid_t type_id, hid_t space_id);
static size_t filter_bogus2(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_bogus3(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_corrupt(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);
static size_t filter_expand(unsigned int flags, size_t cd_nelmts,
    const unsigned int *cd_values, size_t nbytes, size_t *buf_size, void **buf);


/*-------------------------------------------------------------------------
 * Function:	test_create
 *
 * Purpose:	Attempts to create a dataset.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_create(hid_t file)
{
    hid_t	dataset, space, small_space, create_parms;
    hsize_t	dims[2], small_dims[2];
    herr_t	status;
    hsize_t	csize[2];

    TESTING("create, open, close");

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small data space for compact dataset */
    small_dims[0] = 16;
    small_dims[1] = 8;
    small_space = H5Screate_simple(2, small_dims, NULL);
    assert(space>=0);

    /*
     * Create a dataset using the default dataset creation properties.	We're
     * not sure what they are, so we won't check.
     */
    dataset = H5Dcreate2(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
			H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    if(dataset < 0) goto error;

    /* Close the dataset */
    if(H5Dclose(dataset) < 0) goto error;

    /* Add a comment to the dataset */
    status = H5Oset_comment_by_name(file, DSET_DEFAULT_NAME, "This is a dataset", H5P_DEFAULT);
    if(status < 0) goto error;

    /*
     * Try creating a dataset that already exists.  This should fail since a
     * dataset can only be created once.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
	dataset = H5Dcreate2(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
			    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
	H5_FAILED();
	puts("    Library allowed overwrite of existing dataset.");
	goto error;
    }

    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) goto error;
    if((dataset = H5Dopen2(file, DSET_DEFAULT_NAME, H5P_DEFAULT)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Try opening a non-existent dataset. This should fail since new datasets
     * cannot be created with this function.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
	dataset = H5Dopen2(file, "does_not_exist", H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
	H5_FAILED();
	puts("    Opened a non-existent dataset.");
	goto error;
    }

    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);

    /* Attempt to create a dataset with invalid chunk sizes */
    csize[0] = dims[0]*2;
    csize[1] = dims[1]*2;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);
    H5E_BEGIN_TRY {
        dataset = H5Dcreate2(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
			H5P_DEFAULT, create_parms, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
	H5_FAILED();
	puts("    Opened a dataset with incorrect chunking parameters.");
	goto error;
    }

    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    dataset = H5Dcreate2(file, DSET_CHUNKED_NAME, H5T_NATIVE_DOUBLE, space,
			H5P_DEFAULT, create_parms, H5P_DEFAULT);
    if(dataset < 0) goto error;
    H5Pclose(create_parms);

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) goto error;

    /*
     * Close the chunked dataset.
     */
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Create a compact dataset, then close it.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    status = H5Pset_layout(create_parms, H5D_COMPACT);
    assert(status >= 0);
    status = H5Pset_alloc_time(create_parms, H5D_ALLOC_TIME_EARLY);
    assert(status >= 0);

    dataset = H5Dcreate2(file, DSET_COMPACT_NAME, H5T_NATIVE_DOUBLE,
                        small_space, H5P_DEFAULT, create_parms, H5P_DEFAULT);
    if(dataset < 0) goto error;
    H5Pclose(create_parms);
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
    return 0;

 error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_simple_io
 *
 * Purpose:	Tests simple I/O.  That is, reading and writing a complete
 *		multi-dimensional array without data type or data space
 *		conversions, without compression, and stored contiguously.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, December 10, 1997
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_simple_io(const char *env_h5_drvr, hid_t fapl)
{
    char                filename[FILENAME_BUF_SIZE];
    hid_t		file, dataset, space, xfer;
    int			i, j, n;
    hsize_t		dims[2];
    void		*tconv_buf = NULL;
    int                 f;
    haddr_t             offset;
    int                 rdata[DSET_DIM1][DSET_DIM2];

    TESTING("simple I/O");

    /* Can't run this test with multi-file VFDs */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        h5_fixname(FILENAME[4], fapl, filename, sizeof filename);

        /* Initialize the dataset */
        for(i = n = 0; i < DSET_DIM1; i++)
            for(j = 0; j < DSET_DIM2; j++)
                points[i][j] = n++;

        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
            goto error;

        /* Create the data space */
        dims[0] = DSET_DIM1;
        dims[1] = DSET_DIM2;
        if((space = H5Screate_simple(2, dims, NULL)) < 0) goto error;

        /* Create a small conversion buffer to test strip mining */
        tconv_buf = HDmalloc((size_t)1000);
        xfer = H5Pcreate(H5P_DATASET_XFER);
        assert(xfer>=0);
        if(H5Pset_buffer (xfer, (size_t)1000, tconv_buf, NULL) < 0) goto error;

        /* Create the dataset */
        if((dataset = H5Dcreate2(file, DSET_SIMPLE_IO_NAME, H5T_NATIVE_INT, space,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

        /* Test dataset address.  Should be undefined. */
        if(H5Dget_offset(dataset) != HADDR_UNDEF) goto error;

        /* Write the data to the dataset */
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, points) < 0)
            goto error;

        /* Test dataset address in file. Open the same file as a C file, seek
         * the data position as H5Dget_offset points to, read the dataset, and
         * compare it with the data written in.*/
        if((offset=H5Dget_offset(dataset))==HADDR_UNDEF) goto error;

        /* Read the dataset back */
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, xfer, check) < 0)
            goto error;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < DSET_DIM1; i++) {
            for(j = 0; j < DSET_DIM2; j++) {
                if(points[i][j] != check[i][j]) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %d,%d\n", i, j);
                    goto error;
                }
            }
        }

        if(H5Pclose (xfer) < 0) goto error;
        if(H5Dclose(dataset) < 0) goto error;
        if(H5Fclose(file) < 0) goto error;

        f = HDopen(filename, O_RDONLY, 0);
        HDlseek(f, (off_t)offset, SEEK_SET);
        HDread(f, rdata, sizeof(int)*DSET_DIM1*DSET_DIM2);

        /* Check that the values read are the same as the values written */
        for(i = 0; i < DSET_DIM1; i++) {
            for(j = 0; j < DSET_DIM2; j++) {
                if(points[i][j] != rdata[i][j]) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %d,%d\n", i, j);
                    goto error;
                }
            }
        }

        HDclose(f);

        free (tconv_buf);
        PASSED();
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support continuous address space");
    } /* end else */

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_userblock_offset
 *
 * Purpose:	Tests H5Dget_offset when user block exists.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Wednesday, November 27, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_userblock_offset(const char *env_h5_drvr, hid_t fapl)
{
    char                filename[FILENAME_BUF_SIZE];
    hid_t		file, fcpl, dataset, space;
    int			i, j;
    hsize_t		dims[2];
    int                   f;
    haddr_t             offset;
    int                 rdata[DSET_DIM1][DSET_DIM2];

    TESTING("dataset offset with user block");

    /* Can't run this test with multi-file VFDs */
    if(HDstrcmp(env_h5_drvr, "split") && HDstrcmp(env_h5_drvr, "multi") && HDstrcmp(env_h5_drvr, "family")) {
        h5_fixname(FILENAME[2], fapl, filename, sizeof filename);

        if((fcpl=H5Pcreate(H5P_FILE_CREATE)) < 0) goto error;
        if(H5Pset_userblock(fcpl, (hsize_t)USER_BLOCK) < 0) goto error;

        if((file=H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
            goto error;

        /* Create the data space */
        dims[0] = DSET_DIM1;
        dims[1] = DSET_DIM2;
        if((space = H5Screate_simple(2, dims, NULL)) < 0) goto error;

        /* Create the dataset */
        if((dataset = H5Dcreate2(file, DSET_USERBLOCK_IO_NAME, H5T_NATIVE_INT, space,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

        /* Write the data to the dataset */
        if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0)
            goto error;

        /* Test dataset address in file. Open the same file as a C file, seek
         * the data position as H5Dget_offset points to, read the dataset, and
         * compare it with the data written in.*/
        if((offset = H5Dget_offset(dataset)) == HADDR_UNDEF) goto error;

        if(H5Dclose(dataset) < 0) goto error;
        if(H5Fclose(file) < 0) goto error;

        f = HDopen(filename, O_RDONLY, 0);
        HDlseek(f, (off_t)offset, SEEK_SET);
        HDread(f, rdata, sizeof(int)*DSET_DIM1*DSET_DIM2);

        /* Check that the values read are the same as the values written */
        for(i = 0; i < DSET_DIM1; i++) {
            for(j = 0; j < DSET_DIM2; j++) {
                if(points[i][j] != rdata[i][j]) {
                    H5_FAILED();
                    printf("    Read different values than written.\n");
                    printf("    At index %d,%d\n", i, j);
                    goto error;
                }
            }
        }

        HDclose(f);

        PASSED();
    } /* end if */
    else {
	SKIPPED();
	puts("    Current VFD doesn't support continuous address space");
    } /* end else */

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_compact_io
 *
 * Purpose:     Tests compact dataset I/O.  That is, reading and writing a
 *              complete multi-dimensional array without data type or data
 *              space conversions, without compression, and store in
 *              compact dataset.
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              August 8, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compact_io(hid_t fapl)
{
    hid_t       file, dataset, space, plist;
    hsize_t     dims[2];
    int         wbuf[16][8], rbuf[16][8];
    char	filename[FILENAME_BUF_SIZE];
    int         i, j, n;

    TESTING("compact dataset I/O");

    /* Initialize data */
    n = 0;
    for(i = 0; i < 16; i++)
        for(j = 0; j < 8; j++)
            wbuf[i][j] = n++;

    /* Create a small data space for compact dataset */
    dims[0] = 16;
    dims[1] = 8;
    if((space = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create a file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Create property list for compact dataset creation */
    if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_layout(plist, H5D_COMPACT) < 0) TEST_ERROR
    if(H5Pset_alloc_time(plist, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR

    /* Create and write to a compact dataset */
    if((dataset = H5Dcreate2(file, DSET_COMPACT_IO_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT)) < 0)
        TEST_ERROR

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset) != HADDR_UNDEF) TEST_ERROR

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        TEST_ERROR

    /* Test dataset address.  Should be undefined. */
    if(H5Dget_offset(dataset)!=HADDR_UNDEF) TEST_ERROR

    /* Close file */
    if(H5Sclose(space) < 0) TEST_ERROR
    if(H5Pclose(plist) < 0) TEST_ERROR
    if(H5Dclose(dataset) < 0) TEST_ERROR
    if(H5Fclose(file) < 0) TEST_ERROR

    /*
     * Open the file and check data
     */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        TEST_ERROR
    if((dataset = H5Dopen2(file, DSET_COMPACT_IO_NAME, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        TEST_ERROR

     /* Check that the values read are the same as the values written */
     for(i = 0; i < 16; i++)
         for(j = 0; j < 8; j++)
             if(rbuf[i][j] != wbuf[i][j]) {
                 H5_FAILED();
                 printf("    Read different values than written.\n");
                 printf("    At index %d,%d\n", i, j);
                 printf("    wbuf[%d][%d]=%d\n", i, j, wbuf[i][j]);
                 printf("    rbuf[%d][%d]=%d\n", i, j, rbuf[i][j]);
                 goto error;
             } /* end if */

     if(H5Dclose(dataset) < 0) TEST_ERROR
     if(H5Fclose(file) < 0) TEST_ERROR

     PASSED();
     return 0;

 error:
     return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_max_compact
 *
 * Purpose:     Tests compact dataset of maximal size.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              August 8, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_max_compact(hid_t fapl)
{
    hid_t       file = -1;
    hid_t       dataset = -1;
    hid_t       space = -1;
    hid_t       plist = -1;
    hsize_t     dims[1];
    size_t      compact_size;
    int        *wbuf = NULL;
    int        *rbuf = NULL;
    char	filename[FILENAME_BUF_SIZE];
    int         n;
    size_t      u;

    TESTING("compact dataset of maximal size");

    /* Test compact dataset of size 64KB-64 */

    /* Initialize data */
    compact_size = (SIXTY_FOUR_KB - 64) / sizeof(int);

    if(NULL == (wbuf = (int *)HDmalloc(sizeof(int) * compact_size)))
        TEST_ERROR
    if(NULL == (rbuf = (int *)HDmalloc(sizeof(int) * compact_size)))
        TEST_ERROR

    n = 0;
    for(u = 0; u < compact_size; u++)
        wbuf[u] = n++;

    /* Create a small data space for compact dataset */
    dims[0] = (hsize_t)compact_size;
    if((space = H5Screate_simple(1, dims, NULL)) < 0)
        FAIL_STACK_ERROR

    /* Create a file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create property list for compact dataset creation */
    if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(plist, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR

    /* Create and write to a compact dataset */
    if((dataset = H5Dcreate2(file, DSET_COMPACT_MAX_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, wbuf) < 0)
        FAIL_STACK_ERROR

    /* Close file */
    if(H5Sclose(space) < 0)
        FAIL_STACK_ERROR
    if(H5Pclose(plist) < 0)
        FAIL_STACK_ERROR
    if(H5Dclose(dataset) < 0)
        FAIL_STACK_ERROR
    if(H5Fclose(file) < 0)
        FAIL_STACK_ERROR

    /*
     * Open the file and check data
     */
    if((file = H5Fopen(filename, H5F_ACC_RDONLY, fapl)) < 0)
        FAIL_STACK_ERROR
    if((dataset = H5Dopen2(file, DSET_COMPACT_MAX_NAME, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rbuf) < 0)
        FAIL_STACK_ERROR

    /* Check that the values read are the same as the values written */
    for(u = 0; u < compact_size; u++)
        if(rbuf[u] != wbuf[u]) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %u\n", (unsigned)u);
            goto error;
        } /* end if */

     if(H5Dclose(dataset) < 0)
         FAIL_STACK_ERROR
     if(H5Fclose(file) < 0)
         FAIL_STACK_ERROR
     HDfree(wbuf);
     wbuf = NULL;
     HDfree(rbuf);
     rbuf = NULL;

     /* Test compact dataset of size 64KB */

     /* Create a data space for compact dataset */
     compact_size = SIXTY_FOUR_KB / sizeof(int);
     dims[0] = (hsize_t)compact_size;
     if((space = H5Screate_simple(1, dims, NULL)) < 0)
         FAIL_STACK_ERROR

     /* Open file */
     if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0)
         goto error;

     /* Create property list for compact dataset creation */
     if((plist = H5Pcreate(H5P_DATASET_CREATE)) < 0)
         FAIL_STACK_ERROR
     if(H5Pset_layout(plist, H5D_COMPACT) < 0)
         FAIL_STACK_ERROR

     /* Create and write to a compact dataset */
     H5E_BEGIN_TRY {
         H5Dcreate2(file, DSET_COMPACT_MAX2_NAME, H5T_NATIVE_INT, space, H5P_DEFAULT, plist, H5P_DEFAULT);
     } H5E_END_TRY;

     /* Close file */
     if(H5Sclose(space) < 0)
         FAIL_STACK_ERROR
     if(H5Pclose(plist) < 0)
         FAIL_STACK_ERROR
     if(H5Fclose(file) < 0)
         FAIL_STACK_ERROR

     PASSED();
     return 0;

error:
    if(wbuf)
        HDfree(wbuf);
    if(rbuf)
        HDfree(rbuf);

    H5E_BEGIN_TRY {
        /* Close file */
        H5Sclose(space);
        H5Pclose(plist);
        H5Dclose(dataset);
        H5Fclose(file);
    } H5E_END_TRY;

     return -1;
} /* end test_max_compact() */


/*-------------------------------------------------------------------------
 * Function:    test_layout_extend
 *
 * Purpose:     Verify that the creation of extendible dataset with dataspace:
 *		cur_dims < max_dims (max_dims can be fixed size or H5S_UNLIMITED)
 *		will behave as follows:
 *			H5D_COMPACT layout: fail
 *			H5D_CONTIGUOUS layout: fail
 *			H5D_CHUNKED layout: succeed
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Vailin Choi; August 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_layout_extend(hid_t fapl)
{
    char filename[FILENAME_BUF_SIZE];	/* File name */
    hid_t fid = -1; 				/* File id */
    hid_t sid_fix = -1, sid_unlim = -1; 	/* Dataspace id */
    hid_t dcpl_compact = -1, dcpl_contig = -1, dcpl_chunked = -1; /* Dataset creation property list id */
    hid_t did_fixed = -1, did_unlim = -1;	/* Dataset id */
    hsize_t cur_size[1] = {10};		/* Current size of dataspace */
    hsize_t max_unlim[1] = {H5S_UNLIMITED};		/* Maximum size of dataspace (unlimited) */
    hsize_t max_fix[1] = {100};				/* Maximum size of dataspace (fixed) */
    hsize_t chunk_dim[1] = {10};			/* Chunk size */

    TESTING("extendible dataset with various layout");

    /* Create a file */
    h5_fixname(FILENAME[12], fapl, filename, sizeof filename);
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* Create dataspace */
    if((sid_fix = H5Screate_simple(1, cur_size, max_fix)) < 0)
        FAIL_STACK_ERROR
    if((sid_unlim = H5Screate_simple(1, cur_size, max_unlim)) < 0)
        FAIL_STACK_ERROR

    /* Create property list for compact dataset creation */
    if((dcpl_compact = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl_compact, H5D_COMPACT) < 0)
        FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (fixed max_dims) should fail */
    H5E_BEGIN_TRY {
	if(H5Dcreate2(fid, "compact", H5T_NATIVE_INT, sid_fix, H5P_DEFAULT, dcpl_compact, H5P_DEFAULT) != FAIL)
	    TEST_ERROR
    } H5E_END_TRY;

    /* Create dataset with extendible dataspace (unlimited max_dims) should fail */
    H5E_BEGIN_TRY {
	if(H5Dcreate2(fid, "compact", H5T_NATIVE_INT, sid_unlim, H5P_DEFAULT, dcpl_compact, H5P_DEFAULT) != FAIL)
	    TEST_ERROR
    } H5E_END_TRY;

    /* Create property list for contiguous dataset creation */
    if((dcpl_contig = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if((H5Pset_layout(dcpl_contig, H5D_CONTIGUOUS)) < 0)
        FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (fixed max_dims) should fail */
    H5E_BEGIN_TRY {
	if(H5Dcreate2(fid, "contig", H5T_NATIVE_INT, sid_fix, H5P_DEFAULT, dcpl_contig, H5P_DEFAULT) != FAIL)
	    TEST_ERROR
    } H5E_END_TRY;

    /* Create dataset with extendible dataspace (unlimited max_dims) should fail*/
    H5E_BEGIN_TRY {
	if(H5Dcreate2(fid, "contig", H5T_NATIVE_INT, sid_unlim, H5P_DEFAULT, dcpl_contig, H5P_DEFAULT) != FAIL)
	    TEST_ERROR
    } H5E_END_TRY;

    /* Create property list for chunked dataset creation */
    if((dcpl_chunked = H5Pcreate(H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl_chunked, H5D_CHUNKED) < 0)
        FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl_chunked, 1, chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (fixed max_dims) should succeed */
    if((did_fixed = H5Dcreate2(fid, "chunked_fixed", H5T_NATIVE_INT, sid_fix, H5P_DEFAULT, dcpl_chunked, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Create dataset with extendible dataspace (unlimited max_dims) should succeed */
    if((did_unlim = H5Dcreate2(fid, "chunked_unlim", H5T_NATIVE_INT, sid_unlim, H5P_DEFAULT, dcpl_chunked, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Closing */
    if(H5Sclose(sid_fix) < 0) FAIL_STACK_ERROR
    if(H5Sclose(sid_unlim) < 0) FAIL_STACK_ERROR

    if(H5Pclose(dcpl_compact) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl_contig) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl_chunked) < 0) FAIL_STACK_ERROR

    if(H5Dclose(did_fixed) < 0) FAIL_STACK_ERROR
    if(H5Dclose(did_unlim) < 0) FAIL_STACK_ERROR

    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(sid_fix);
        H5Sclose(sid_unlim);
        H5Pclose(dcpl_compact);
        H5Pclose(dcpl_contig);
        H5Pclose(dcpl_chunked);
        H5Dclose(did_fixed);
        H5Dclose(did_unlim);
        H5Fclose(fid);
    } H5E_END_TRY;

     return -1;
} /* end test_layout_extend() */


/*-------------------------------------------------------------------------
 * Function:	test_conv_buffer
 *
 * Purpose:	Test size of data type conversion buffer.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *		Monday, May 12, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_conv_buffer(hid_t fid)
{
    typedef struct
    {
        int      a[DIM1][DIM2][DIM3];
        float    b[DIM2];
        double   c[DIM3];
    } CmpField;

    typedef struct
    {
        float    b[DIM2];
        double   c[DIM3];
    } CmpFieldR;

    herr_t       status = -1;
    int          j, k, l;

    CmpField     *cf;
    CmpFieldR    *cfrR;

    hid_t       dataset = -1; /* dataset ID             */
    hid_t       space   = -1; /* data space ID          */
    hid_t       ctype1, ctype2; /* data type ID           */
    hid_t       arr_type1, arr_type2, arr_type3, arr_type4, arr_type5;
    hsize_t     dimsa[3];
    hsize_t     dimsb[1];
    hsize_t     dimsc[1];
    hid_t       xfer_list;
    size_t      size;

    TESTING("data type conversion buffer size");

    cf = (CmpField *)HDcalloc((size_t)1, sizeof(CmpField));

    /* Populate the data members */
    for(j = 0; j < DIM1; j++)
	for(k = 0; k < DIM2; k++)
	    for(l = 0; l < DIM3; l++)
		cf->a[j][k][l] = 10*(j+1) + l + k;

    for(j = 0; j < DIM2; j++)
	cf->b[j] = (float)(100.*(j+1) + 0.01*j);

    for(j = 0; j < DIM3; j++)
	cf->c[j] = 100.*(j+1) + 0.02*j;


  /* Create data space */
  if((space=H5Screate(H5S_SCALAR)) < 0) goto error;

  /* Add  members to the compound data type */
  dimsa[0] = DIM1;
  dimsa[1] = DIM2;
  dimsa[2] = DIM3;
  dimsb[0] = DIM2;
  dimsc[0] = DIM3;

  /* Create the memory data type */
  if((ctype1 = H5Tcreate(H5T_COMPOUND, sizeof (CmpField))) < 0) goto error;

  if((arr_type1 = H5Tarray_create2(H5T_NATIVE_INT, 3, dimsa)) < 0) goto error;
  if((arr_type2 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, dimsb)) < 0) goto error;
  if((arr_type3 = H5Tarray_create2(H5T_NATIVE_DOUBLE, 1, dimsc)) < 0) goto error;

  if(H5Tinsert(ctype1, "A", HOFFSET(CmpField, a), arr_type1) < 0) goto error;
  if(H5Tinsert(ctype1, "B", HOFFSET(CmpField, b), arr_type2) < 0) goto error;
  if(H5Tinsert(ctype1, "C", HOFFSET(CmpField, c), arr_type3) < 0) goto error;

  /* Create the dataset */
  if((dataset = H5Dcreate2(fid, DSET_CONV_BUF_NAME, ctype1, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;
  if(H5Dwrite(dataset, ctype1, H5S_ALL, H5S_ALL, H5P_DEFAULT, cf) < 0) goto error;

  if((ctype2 = H5Tcreate(H5T_COMPOUND, sizeof (CmpFieldR))) < 0) goto error;

  if((arr_type4 = H5Tarray_create2(H5T_NATIVE_FLOAT, 1, dimsb)) < 0) goto error;
  if((arr_type5 = H5Tarray_create2(H5T_NATIVE_DOUBLE, 1, dimsc)) < 0) goto error;

  if(H5Tinsert(ctype2, "B", HOFFSET(CmpFieldR, b), arr_type4) < 0) goto error;
  if(H5Tinsert(ctype2, "C", HOFFSET(CmpFieldR, c), arr_type5) < 0) goto error;

  /* Read should succeed since library will set conversion buffer big enough */
  cfrR = (CmpFieldR *)HDcalloc((size_t)1, sizeof(CmpFieldR));
  if(H5Dread(dataset, ctype2, H5S_ALL, H5S_ALL, H5P_DEFAULT, cfrR) < 0) goto error;

  /* Read should fail since conversion buffer isn't big enough */
  xfer_list = H5Pcreate(H5P_DATASET_XFER);
  size = (DIM2 * DIM3 * (sizeof(int))+ DIM2 * (sizeof(float))+
         DIM3 * (sizeof(double)));
  if(H5Pset_buffer(xfer_list, size, NULL, NULL) < 0) goto error;

  H5E_BEGIN_TRY {
    status = H5Dread(dataset, ctype2, H5S_ALL, H5S_ALL, xfer_list, cfrR);
  } H5E_END_TRY;
  if(status >= 0) {
      H5_FAILED();
      puts("    Library shouldn't allow conversion buffer too small");
      goto error;
  }

  /* Read will succeed since conversion buffer is big enough */
  size = (DIM1 * DIM2 * DIM3 * (sizeof(int))+ DIM2 * (sizeof(float))+
         DIM3 * (sizeof(double)));
  if(H5Pset_buffer(xfer_list, size, NULL, NULL) < 0) goto error;

  if(H5Dread(dataset, ctype2, H5S_ALL, H5S_ALL, xfer_list, cfrR) < 0) goto error;


  if(H5Pclose(xfer_list) < 0) goto error;
  if(H5Sclose(space) < 0) goto error;
  if(H5Tclose(arr_type1) < 0) goto error;
  if(H5Tclose(arr_type2) < 0) goto error;
  if(H5Tclose(arr_type3) < 0) goto error;
  if(H5Tclose(ctype1) < 0) goto error;
  if(H5Tclose(ctype2) < 0) goto error;
  if(H5Tclose(arr_type4) < 0) goto error;
  if(H5Tclose(arr_type5) < 0) goto error;
  if(H5Dclose(dataset) < 0) goto error;

  if(cf)
    HDfree(cf);
  if(cfrR)
    HDfree(cfrR);
  puts(" PASSED");
  return(0);

error:
  return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_tconv
 *
 * Purpose:	Test some simple data type conversion stuff.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *		Wednesday, January 14, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_tconv(hid_t file)
{
    char	*out = NULL, *in = NULL;
    hsize_t	dims[1];
    hid_t	space = -1, dataset = -1;
    int		i;

    if ((out = (char *)HDmalloc((size_t)(4 * 1000 * 1000))) == NULL)
        goto error;
    if ((in = (char *)HDmalloc((size_t)(4 * 1000 * 1000))) == NULL)
        goto error;

    TESTING("data type conversion");

    /* Initialize the dataset */
    for(i = 0; i < 1000000; i++) {
        out[i * 4 + 0] = 0x11;
        out[i * 4 + 1] = 0x22;
        out[i * 4 + 2] = 0x33;
        out[i * 4 + 3] = 0x44;
    } /* end for */

    /* Create the data space */
    dims[0] = 1000000;
    if((space = H5Screate_simple (1, dims, NULL)) < 0) goto error;

    /* Create the data set */
    if((dataset = H5Dcreate2(file, DSET_TCONV_NAME, H5T_STD_I32LE, space,
			     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        goto error;

    /* Write the data to the dataset */
    if(H5Dwrite(dataset, H5T_STD_I32LE, H5S_ALL, H5S_ALL, H5P_DEFAULT, out) < 0)
        goto error;

    /* Read data with byte order conversion */
    if(H5Dread(dataset, H5T_STD_I32BE, H5S_ALL, H5S_ALL, H5P_DEFAULT, in) < 0)
        goto error;

    /* Check */
    for(i = 0; i < 1000000; i++) {
        if(in[4 * i + 0] != out[4 * i + 3] ||
                in[4 * i + 1] != out[4 * i + 2] ||
                in[4 * i + 2] != out[4 * i + 1] ||
                in[4 * i + 3] != out[4 * i + 0]) {
            H5_FAILED();
            puts("    Read with byte order conversion failed.");
            goto error;
        }
    }

    if(H5Dclose(dataset) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    HDfree(out);
    HDfree(in);

    puts(" PASSED");
    return 0;

error:
    if(out)
        HDfree(out);
    if(in)
        HDfree(in);

    H5E_BEGIN_TRY {
        H5Dclose(dataset);
        H5Sclose(space);
    } H5E_END_TRY;

    return -1;
}

/* This message derives from H5Z */
const H5Z_class2_t H5Z_BOGUS[1] = {{
    H5Z_CLASS_T_VERS,       /* H5Z_class_t version */
    H5Z_FILTER_BOGUS,		/* Filter id number		*/
    1, 1,               /* Encoding and decoding enabled */
    "bogus",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	can_apply_bogus
 *
 * Purpose:	A bogus 'can apply' callback that returns 0 for H5T_NATIVE_DOUBLE
 *              dataype, but returns 1 for all other datatypes
 *
 * Return:	Success:	Described above
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
static htri_t
can_apply_bogus(hid_t UNUSED dcpl_id, hid_t type_id, hid_t UNUSED space_id)
{
    if(H5Tequal(type_id,H5T_NATIVE_DOUBLE))
        return 0;
    else if(H5Tequal(type_id,H5T_NATIVE_INT))
        return 1;
    else
        return -1;
}


/*-------------------------------------------------------------------------
 * Function:	filter_bogus
 *
 * Purpose:	A bogus compression method that doesn't do anything.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Robb Matzke
 *              Tuesday, April 21, 1998
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
    return nbytes;
}


/*-------------------------------------------------------------------------
 * Function:	set_local_bogus2
 *
 * Purpose:	A 'set local' callback that stores the size of the datatype
 *              and adds it to all the H5T_NATIVE_INT values during
 *              filter operation.
 *
 * Return:	Success:	non-negative
 *		Failure:	negative
 *
 * Programmer:	Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
set_local_bogus2(hid_t dcpl_id, hid_t type_id, hid_t UNUSED space_id)
{
    unsigned add_on=0;      /* Value to add to data going through */
    unsigned flags;         /* Filter flags */
    size_t cd_nelmts=BOGUS2_PERM_NPARMS;        /* Number of filter parameters */
    unsigned cd_values[4];  /* Filter parameters */

    /* Check for native integer datatype and set private property */
    if(H5Tequal(type_id,H5T_NATIVE_INT)>0)
        add_on=(unsigned)H5Tget_size(type_id);

    /* Get the filter's current parameters */
    if(H5Pget_filter_by_id2(dcpl_id, H5Z_FILTER_SET_LOCAL_TEST, &flags, &cd_nelmts, cd_values, (size_t)0, NULL, NULL) < 0)
        return(FAIL);

    /* Check that the parameter values were passed along correctly */
    if(cd_values[0]!=BOGUS2_PARAM_1)
        return(FAIL);
    if(cd_values[1]!=BOGUS2_PARAM_2)
        return(FAIL);

    /* Set "local" parameters for this dataset */
    cd_values[2]=(unsigned)(add_on>0);    /* Flag to indicate data is modified */
    cd_values[3]=add_on;        /* Amount the data was modified by */

    /* Modify the filter's parameters for this dataset */
    if(H5Pmodify_filter(dcpl_id, H5Z_FILTER_SET_LOCAL_TEST, flags, (size_t)BOGUS2_ALL_NPARMS,
            cd_values) < 0)
        return(FAIL);

    return(SUCCEED);
} /* end set_local_bogus2() */


/*-------------------------------------------------------------------------
 * Function:	filter_bogus2
 *
 * Purpose:	A filter method that adds a value to data values on writing
 *              (if the parameter is set), but does not modify data values on
 *              reading (so that correct operation of the filter can be
 *              checked).
 *
 * Return:	Success:	Data chunk size
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  7, 2003
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus2(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    /* Check for the correct number of parameters */
    if(cd_nelmts!=BOGUS2_ALL_NPARMS)
        return(0);

    /* Check that permanent parameters are set correctly */
    if(cd_values[0]!=BOGUS2_PARAM_1)
        return(0);
    if(cd_values[1]!=BOGUS2_PARAM_2)
        return(0);

    /* Check if this filter is supposed to do something */
    if(cd_values[2]>0) {
        /* Check whether we are "uncompressing" */
        if(flags & H5Z_FLAG_REVERSE) {
            /* Do nothing */
        } /* end if */
        /* "Compressing" */
        else {
            unsigned add_on=cd_values[3];   /* Get "add on" value */
            int *int_ptr=(int *)*buf;          /* Pointer to the data values */
            size_t buf_left=*buf_size;  /* Amount of data buffer left to process */

            /* Add the "add on" value to all the data values */
            while(buf_left>0) {
                *int_ptr++ += (int)add_on;
                buf_left -= sizeof(int);
            } /* end while */
        } /* end else */

        return(nbytes);
    } /* end if */
    /* Filter is "no op" */
    else
        return(nbytes);
}


/*-------------------------------------------------------------------------
 * Function:	filter_bogus3
 *
 * Purpose:	A bogus compression method that returns a failure.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              4 August 2010
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_bogus3(unsigned int UNUSED flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t UNUSED nbytes,
      size_t UNUSED *buf_size, void UNUSED **buf)
{
    return 0;
}

/* This message derives from H5Z */
const H5Z_class2_t H5Z_CORRUPT[1] = {{
    H5Z_CLASS_T_VERS,            /* H5Z_class_t version */
    H5Z_FILTER_CORRUPT,		/* Filter id number		*/
    1, 1,               /* Encoding and decoding enabled */
    "corrupt",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_corrupt,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:    filter_corrupt
 *
 * Purpose:     For testing Fletcher32 checksum.  modify data slightly during
 *              writing so that when data is read back, the checksum should
 *              fail.
 *
 * Return:	Success:	Data chunk size
 *
 *		Failure:	0
 *
 * Programmer:	Raymond Lu
 *              Jan 14, 2003
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_corrupt(unsigned int flags, size_t cd_nelmts,
      const unsigned int *cd_values, size_t nbytes,
      size_t *buf_size, void **buf)
{
    void  *data = NULL;
    unsigned char  *dst = (unsigned char*)(*buf);
    unsigned int   offset;
    unsigned int   length;
    unsigned int   value;
    size_t         ret_value = 0;

    if(cd_nelmts != 3 || !cd_values)
        TEST_ERROR
    offset = cd_values[0];
    length = cd_values[1];
    value  = cd_values[2];
    if(offset > nbytes || (offset + length) > nbytes || length < sizeof(unsigned int))
        TEST_ERROR

    if(NULL == (data = HDmalloc((size_t)length))) 
        TEST_ERROR
    HDmemset(data, (int)value, (size_t)length);

    if(flags & H5Z_FLAG_REVERSE) { /* Varify data is actually corrupted during read */
        dst += offset;
        if(HDmemcmp(data, dst, (size_t)length) != 0)
            TEST_ERROR
        else {
            *buf_size = nbytes;
            ret_value = nbytes;
        } /* end else */
    }  /* end if */
    else { /* Write corrupted data */
        dst += offset;
        HDmemcpy(dst, data, (size_t)length);
        *buf_size = nbytes;
        ret_value = *buf_size;
    } /* end else */

error:
    if(data)
        HDfree(data);

    return ret_value;
} /* end filter_corrupt() */


/*-------------------------------------------------------------------------
 * Function:    filter_cb_cont
 *
 * Purpose:     Callback function to handle checksum failure.  Let it continue.
 *
 * Return:      continue
 *
 * Programmer:	Raymond Lu
 *              Jan 14, 2003
 *
 *-------------------------------------------------------------------------
 */
static H5Z_cb_return_t
filter_cb_cont(H5Z_filter_t filter, void UNUSED *buf, size_t UNUSED buf_size,
           void UNUSED *op_data)
{
    if(H5Z_FILTER_FLETCHER32==filter)
       return H5Z_CB_CONT;
    else
        return H5Z_CB_FAIL;
}


/*-------------------------------------------------------------------------
 * Function:    filter_cb_fail
 *
 * Purpose:     Callback function to handle checksum failure.  Let it fail.
 *
 * Return:      fail
 *
 * Programmer:	Raymond Lu
 *              Jan 14, 2003
 *
 *-------------------------------------------------------------------------
 */
static H5Z_cb_return_t
filter_cb_fail(H5Z_filter_t filter, void UNUSED *buf, size_t UNUSED buf_size,
           void UNUSED *op_data)
{
    if(H5Z_FILTER_FLETCHER32==filter)
       return H5Z_CB_FAIL;
    else
       return H5Z_CB_CONT;
}


/*-------------------------------------------------------------------------
 * Function:	test_filter_internal
 *
 * Purpose:	Tests dataset compression. If compression is requested when
 *		it hasn't been compiled into the library (such as when
 *		updating an existing compressed dataset) then data is sent to
 *		the file uncompressed but no errors are returned.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_internal(hid_t fid, const char *name, hid_t dcpl, int if_fletcher32,
                     int corrupted, hsize_t *dset_size)
{
    hid_t		dataset;        /* Dataset ID */
    hid_t		dxpl;           /* Dataset xfer property list ID */
    hid_t		write_dxpl;     /* Dataset xfer property list ID for writing */
    hid_t		sid;            /* Dataspace ID */
    const hsize_t	size[2] = {DSET_DIM1, DSET_DIM2};           /* Dataspace dimensions */
    const hsize_t	hs_offset[2] = {FILTER_HS_OFFSET1, FILTER_HS_OFFSET2}; /* Hyperslab offset */
    const hsize_t	hs_size[2] = {FILTER_HS_SIZE1, FILTER_HS_SIZE2};   /* Hyperslab size */
    void		*tconv_buf = NULL;      /* Temporary conversion buffer */
    size_t		i, j, n;        /* Local index variables */
    herr_t              status;         /* Error status */

    /* Create the data space */
    if((sid = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /*
     * Create a small conversion buffer to test strip mining. We
     * might as well test all we can!
     */
    if((dxpl = H5Pcreate(H5P_DATASET_XFER)) < 0) goto error;
    tconv_buf = HDmalloc((size_t)1000);
    if(H5Pset_buffer(dxpl, (size_t)1000, tconv_buf, NULL) < 0) goto error;
    if((write_dxpl = H5Pcopy(dxpl)) < 0) TEST_ERROR;

    if(if_fletcher32==DISABLE_FLETCHER32) {
        if(H5Pset_edc_check(dxpl, H5Z_DISABLE_EDC) < 0)
            goto error;
        if(H5Z_DISABLE_EDC != H5Pget_edc_check(dxpl))
            goto error;
    }

    TESTING("    filters (setup)");

    /* Check if all the filters are available */
    if(H5Pall_filters_avail(dcpl)!=TRUE) {
        H5_FAILED();
        printf("    Line %d: Incorrect filter availability\n",__LINE__);
        goto error;
    } /* end if */

    /* Create the dataset */
    if((dataset = H5Dcreate2(fid, name, H5T_NATIVE_INT, sid, H5P_DEFAULT,
			     dcpl, H5P_DEFAULT)) < 0) goto error;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 1: Read uninitialized data.  It should be zero.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (uninitialized read)");

    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
	TEST_ERROR;

    for(i=0; i<(size_t)size[0]; i++) {
	for(j=0; j<(size_t)size[1]; j++) {
	    if(0!=check[i][j]) {
		H5_FAILED();
		printf("    Read a non-zero value.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 2: Test filters by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (write)");

    for(i=n=0; i<size[0]; i++) {
	for(j=0; j<size[1]; j++) {
	    points[i][j] = (int)(n++);
	}
    }

    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
	TEST_ERROR;

    if((*dset_size=H5Dget_storage_size(dataset))==0) TEST_ERROR;

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 3: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (read)");

    /* Read the dataset back */
    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;
    } else {
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
	   TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<size[0]; i++) {
	   for(j=0; j<size[1]; j++) {
	       if(points[i][j] != check[i][j]) {
		  H5_FAILED();
		  fprintf(stderr,"    Read different values than written.\n");
		  fprintf(stderr,"    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
		  fprintf(stderr,"    At original: %d\n", (int)points[i][j]);
		  fprintf(stderr,"    At returned: %d\n", (int)check[i][j]);
		  goto error;
	       }
	   }
        }
    }

    PASSED();

    /*----------------------------------------------------------------------
     * STEP 4: Write new data over the top of the old data.  The new data is
     * random thus not very compressible, and will cause the chunks to move
     * around as they grow.  We only change values for the left half of the
     * dataset although we rewrite the whole thing.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (modify)");

    for(i=0; i<size[0]; i++) {
	for(j=0; j<size[1]/2; j++) {
	    points[i][j] = (int)HDrandom ();
	}
    }
    if(H5Dwrite (dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, points) < 0)
	TEST_ERROR;

    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;
    } else {
        /* Read the dataset back and check it */
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
	   TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<size[0]; i++) {
	   for(j=0; j<size[1]; j++) {
	       if(points[i][j] != check[i][j]) {
		  H5_FAILED();
		  printf("    Read different values than written.\n");
		  printf("    At index %lu,%lu\n",
		           (unsigned long)i, (unsigned long)j);
		  goto error;
	       }
	   }
        }
    }

    if((*dset_size=H5Dget_storage_size(dataset))==0) TEST_ERROR;
    PASSED();

    /*----------------------------------------------------------------------
     * STEP 5: Close the dataset and then open it and read it again.  This
     * insures that the filters message is picked up properly from the
     * object header.
     *----------------------------------------------------------------------
     */
    TESTING("    filters (re-open)");

    if(H5Dclose(dataset) < 0) TEST_ERROR;
    if((dataset = H5Dopen2(fid, name, H5P_DEFAULT)) < 0) TEST_ERROR;

    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status >= 0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;

        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status = H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status >= 0) TEST_ERROR;
    } /* end if */
    else {
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i = 0; i < size[0]; i++)
	   for(j = 0; j < size[1]; j++)
	       if(points[i][j] != check[i][j]) {
		  H5_FAILED();
		  printf("    Read different values than written.\n");
		  printf("    At index %lu,%lu\n",
		        (unsigned long)i, (unsigned long)j);
		  goto error;
	       } /* end if */
    } /* end else */

    PASSED();


    /*----------------------------------------------------------------------
     * STEP 6: Test partial I/O by writing to and then reading from a
     * hyperslab of the dataset.  The hyperslab does not line up on chunk
     * boundaries (we know that case already works from above tests).
     *----------------------------------------------------------------------
     */
    TESTING("    filters (partial I/O)");

    for(i=0; i<(size_t)hs_size[0]; i++) {
	for(j=0; j<(size_t)hs_size[1]; j++) {
	    points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j] = (int)HDrandom();
	}
    }
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size,
			    NULL) < 0) TEST_ERROR;
    /* (Use the "read" DXPL because partial I/O on corrupted data test needs to ignore errors during writing) */
    if(H5Dwrite (dataset, H5T_NATIVE_INT, sid, sid, dxpl, points) < 0)
	TEST_ERROR;

    if(corrupted) {
        /* Default behavior is failure when data is corrupted. */
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;

        /* Callback decides to continue inspite data is corrupted. */
        if(H5Pset_filter_callback(dxpl, filter_cb_cont, NULL) < 0) TEST_ERROR;
        if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, dxpl, check) < 0)
            TEST_ERROR;

        /* Callback decides to fail when data is corrupted. */
        if(H5Pset_filter_callback(write_dxpl, filter_cb_fail, NULL) < 0) TEST_ERROR;
        /* (Use the "write" DXPL in order to make certain corruption is seen) */
        H5E_BEGIN_TRY {
            status=H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, write_dxpl, check);
        } H5E_END_TRY;
        if(status>=0) TEST_ERROR;
    } else {
        if(H5Dread (dataset, H5T_NATIVE_INT, sid, sid, dxpl, check) < 0)
	   TEST_ERROR;

        /* Check that the values read are the same as the values written */
        for(i=0; i<(size_t)hs_size[0]; i++) {
	   for(j=0; j<(size_t)hs_size[1]; j++) {
	       if(points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j] !=
                      check[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]) {
		  H5_FAILED();
		  fprintf(stderr,"    Read different values than written.\n");
		  fprintf(stderr,"    At index %lu,%lu\n",
		         (unsigned long)((size_t)hs_offset[0]+i),
		         (unsigned long)((size_t)hs_offset[1]+j));
		  fprintf(stderr,"    At original: %d\n",
		         (int)points[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]);
		  fprintf(stderr,"    At returned: %d\n",
		         (int)check[(size_t)hs_offset[0]+i][(size_t)hs_offset[1]+j]);
		  goto error;
	       }
	   }
        }
    }

    PASSED();

    /* Get the storage size of the dataset */
    if((*dset_size=H5Dget_storage_size(dataset))==0) goto error;
    /* Clean up objects used for this test */
    if(H5Dclose (dataset) < 0) goto error;
    if(H5Sclose (sid) < 0) goto error;
    if(H5Pclose (dxpl) < 0) goto error;
    free (tconv_buf);

    return(0);

error:
    if(tconv_buf)
        free (tconv_buf);
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_filter_noencoder
 *
 * Purpose:	Tests filters with no encoder present.  Ensures that data
 *			can still be decoded correctly and that errors are thrown
 *			when the application tries to write.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Nat Furrer and James Laird
 *              Monday, June 7, 2004
 *
 *-------------------------------------------------------------------------
 */
#ifdef H5_HAVE_FILTER_SZIP
static herr_t
test_filter_noencoder(const char *dset_name)
{
    hid_t file_id = -1;
    hid_t dset_id = -1;
    hid_t test_dset_id = -1;
    hid_t dcpl_id = -1;
    hid_t space_id = -1;
    hsize_t dims = 10;
    herr_t err;
    int test_ints[10] = { 12 };
    int read_buf[10];
    int i;

    /* Make a local copy of the file since this test writes to the data file
       from svn. */
    if (h5_make_local_copy(NOENCODER_FILENAME, NOENCODER_COPY_FILENAME) < 0)
        goto error;

    /* Open file */
    file_id = H5Fopen(NOENCODER_COPY_FILENAME, H5F_ACC_RDWR, H5P_DEFAULT);
    if(file_id < 0) goto error;

    dset_id = H5Dopen2(file_id, dset_name, H5P_DEFAULT);
    if(dset_id < 0) goto error;

    space_id = H5Screate_simple(1, &dims, NULL);
    if(space_id < 0) goto error;

    TESTING("    decoding without encoder");

    /* Read the dataset and make sure the decoder is working correctly */
    err = H5Dread(dset_id, H5T_NATIVE_INT, space_id, space_id, H5P_DEFAULT, read_buf);
    if(err < 0) goto error;

    for(i = 0; i < 10; i++)
        if(read_buf[i] != i)
            goto error;

    H5Sclose(space_id);

    PASSED();

    /* Attempt to copy the DCPL and use it to create a new dataset.
     * Since the filter does not have an encoder, the creation
     * should fail.
     */
    TESTING("    trying to write without encoder");

    dcpl_id = H5Dget_create_plist(dset_id);
    if(dcpl_id < 0) goto error;

    space_id = H5Screate_simple(1, &dims, NULL);
    if(space_id < 0) goto error;

    H5E_BEGIN_TRY{
    test_dset_id = H5Dcreate2(file_id, NOENCODER_TEST_DATASET, H5T_NATIVE_INT, space_id, H5P_DEFAULT, dcpl_id, H5P_DEFAULT);
    }H5E_END_TRY

    if(test_dset_id >= 0) goto error;

    /* Attempt to extend the dataset.  This should fail because
     * the dataset has a fill value and is instructed to fill on
     * allocation.
     */
    dims = 20; /* Dataset is originally of size 10 */
    H5E_BEGIN_TRY{
    err = H5Dset_extent(dset_id, &dims);
    }H5E_END_TRY

    if(err >= 0) goto error;

    /* Attempt to write to the dataset.  This should fail because
     * the filter does not have an encoder.
     */
    H5E_BEGIN_TRY{
    err = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, test_ints);
    }H5E_END_TRY

    if(err >= 0) goto error;

    H5Fclose(file_id);
    H5Dclose(dset_id);
    H5Sclose(space_id);
    H5Pclose(dcpl_id);

    PASSED();

    return 0;

error:
    H5_FAILED();
    if(dset_id != -1)
        H5Dclose(dset_id);
    if(test_dset_id != -1)
        H5Dclose(test_dset_id);
    if(space_id != -1)
        H5Sclose(space_id);
    if(dcpl_id != -1)
        H5Pclose(dcpl_id);
    if(file_id != -1)
        H5Fclose(file_id);

    return -1;
}
#endif /* H5_HAVE_FILTER_SZIP */

/*-------------------------------------------------------------------------
 * Function:    test_get_filter_info
 *
 * Purpose:     Tests the H5Zget_filter_info function.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Nat Furrer and James Laird
 *              Thursday, June 10, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_get_filter_info(void)
{
  unsigned int flags;  /* flags returned from H5Zget_filter_info */
  herr_t err;

  TESTING("H5Zget_filter_info");

  /* Verify that each filter is reported as having the right combination
   * of encoder and decoder.
   */
#ifdef H5_HAVE_FILTER_FLETCHER32
  if(H5Zget_filter_info(H5Z_FILTER_FLETCHER32, &flags) < 0) TEST_ERROR

  if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
     ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
      TEST_ERROR
#endif

#ifdef H5_HAVE_FILTER_SHUFFLE
  if(H5Zget_filter_info(H5Z_FILTER_SHUFFLE, &flags) < 0) TEST_ERROR

  if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
     ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
      TEST_ERROR
#endif

#ifdef H5_HAVE_FILTER_DEFLATE
  if(H5Zget_filter_info(H5Z_FILTER_DEFLATE, &flags) < 0) TEST_ERROR

  if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
     ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
      TEST_ERROR
#endif

#ifdef H5_HAVE_FILTER_SZIP
    if(H5Zget_filter_info(H5Z_FILTER_SZIP, &flags) < 0) TEST_ERROR

    if(SZ_encoder_enabled()) {
        if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) == 0) ||
                ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
            TEST_ERROR
    } /* end if */
    else {
        if(((flags & H5Z_FILTER_CONFIG_ENCODE_ENABLED) != 0) ||
                ((flags & H5Z_FILTER_CONFIG_DECODE_ENABLED) == 0))
            TEST_ERROR
    } /* end else */
#endif /* H5_HAVE_FILTER_SZIP */

  /* Verify that get_filter_info throws an error when given a bad filter */
  /* (Depends on 1.6 compatibility flag) */
  H5E_BEGIN_TRY {
    err = H5Zget_filter_info(-1, &flags);
  } H5E_END_TRY;
  if(err >= 0) TEST_ERROR

  PASSED();
  return 0;

error:
  return -1;
}

/*-------------------------------------------------------------------------
 * Function:	test_filters
 *
 * Purpose:	Tests dataset filter.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Wednesday, April 15, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters(hid_t file, hid_t
#ifndef H5_HAVE_FILTER_SZIP
UNUSED
#endif /* H5_HAVE_FILTER_SZIP */
    fapl)
{
    hid_t	dc;                 /* Dataset creation property list ID */
    const hsize_t chunk_size[2] = {FILTER_CHUNK_DIM1, FILTER_CHUNK_DIM2};  /* Chunk dimensions */
    hsize_t     null_size;          /* Size of dataset with null filter */

#ifdef H5_HAVE_FILTER_FLETCHER32
    hsize_t     fletcher32_size;       /* Size of dataset with Fletcher32 checksum */
    unsigned    data_corrupt[3];     /* position and length of data to be corrupted */
#endif /* H5_HAVE_FILTER_FLETCHER32 */

#ifdef H5_HAVE_FILTER_DEFLATE
    hsize_t     deflate_size;       /* Size of dataset with deflate filter */
#endif /* H5_HAVE_FILTER_DEFLATE */

#ifdef H5_HAVE_FILTER_SZIP
    hsize_t     szip_size;       /* Size of dataset with szip filter */
    unsigned szip_options_mask=H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block=4;
#endif /* H5_HAVE_FILTER_SZIP */

#ifdef H5_HAVE_FILTER_SHUFFLE
    hsize_t     shuffle_size;       /* Size of dataset with shuffle filter */
#endif /* H5_HAVE_FILTER_SHUFFLE */

#if(defined H5_HAVE_FILTER_DEFLATE | defined H5_HAVE_FILTER_SZIP) && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32
    hsize_t     combo_size;     /* Size of dataset with shuffle+deflate filter */
#endif /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */

    /* test the H5Zget_filter_info function */
    if(test_get_filter_info() < 0) goto error;

    /*----------------------------------------------------------
     * STEP 0: Test null I/O filter by itself.
     *----------------------------------------------------------
     */
    puts("Testing 'null' filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Zregister (H5Z_BOGUS) < 0) goto error;
    if(H5Pset_filter(dc, H5Z_FILTER_BOGUS, 0, (size_t)0, NULL) < 0) goto error;

    if(test_filter_internal(file,DSET_BOGUS_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&null_size) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    /*----------------------------------------------------------
     * STEP 1: Test Fletcher32 Checksum by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_FLETCHER32
    puts("Testing Fletcher32 checksum(enabled for read)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_filter(dc, H5Z_FILTER_FLETCHER32, 0, (size_t)0, NULL) < 0) goto error;

    /* Enable checksum during read */
    if(test_filter_internal(file,DSET_FLETCHER32_NAME,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&fletcher32_size) < 0) goto error;
    if(fletcher32_size<=null_size) {
        H5_FAILED();
        puts("    Size after checksumming is incorrect.");
        goto error;
    } /* end if */

    /* Disable checksum during read */
    puts("Testing Fletcher32 checksum(disabled for read)");
    if(test_filter_internal(file,DSET_FLETCHER32_NAME_2,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&fletcher32_size) < 0) goto error;
    if(fletcher32_size<=null_size) {
        H5_FAILED();
        puts("    Size after checksumming is incorrect.");
        goto error;
    } /* end if */

    /* Try to corrupt data and see if checksum fails */
    puts("Testing Fletcher32 checksum(when data is corrupted)");
    data_corrupt[0] = 52;
    data_corrupt[1] = 33;
    data_corrupt[2] = 27;

    if(H5Zregister (H5Z_CORRUPT) < 0) goto error;
    if(H5Pset_filter(dc, H5Z_FILTER_CORRUPT, 0, (size_t)3, data_corrupt) < 0) goto error;
    if(test_filter_internal(file,DSET_FLETCHER32_NAME_3,dc,DISABLE_FLETCHER32,DATA_CORRUPTED,&fletcher32_size) < 0) goto error;
    if(fletcher32_size<=null_size) {
        H5_FAILED();
        puts("    Size after checksumming is incorrect.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

#else /* H5_HAVE_FILTER_FLETCHER32 */
    TESTING("fletcher32 checksum");
    SKIPPED();
    puts("    Fletcher32 checksum not enabled");
#endif /* H5_HAVE_FILTER_FLETCHER32 */

    /*----------------------------------------------------------
     * STEP 2: Test deflation by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_DEFLATE
    puts("Testing deflate filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;

    if(test_filter_internal(file,DSET_DEFLATE_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&deflate_size) < 0) goto error;
    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE */
    TESTING("deflate filter");
    SKIPPED();
    puts("    Deflate filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 3: Test szip compression by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SZIP
    TESTING("szip filter (with encoder)");
    if( h5_szip_can_encode() == 1) {
        if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
        if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;

	puts("");
	if(H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block) < 0) goto error;
	if(test_filter_internal(file,DSET_SZIP_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&szip_size) < 0) goto error;
        if(H5Pclose (dc) < 0) goto error;
    } else {
	SKIPPED();
    }

    TESTING("szip filter (without encoder)");

    if( h5_szip_can_encode() != 1) {
	puts("");
	if(test_filter_noencoder(NOENCODER_SZIP_DATASET) < 0) goto error;
    } else {
	SKIPPED();
    }

#else /* H5_HAVE_FILTER_SZIP */
    TESTING("szip filter");
    SKIPPED();
    puts("    Szip filter not enabled");
#endif /* H5_HAVE_FILTER_SZIP */

    /*----------------------------------------------------------
     * STEP 4: Test shuffling by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SHUFFLE
    puts("Testing shuffle filter");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;

    if(test_filter_internal(file,DSET_SHUFFLE_NAME,dc,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&shuffle_size) < 0) goto error;
    if(shuffle_size!=null_size) {
        H5_FAILED();
        puts("    Shuffled size not the same as uncompressed size.");
        goto error;
    } /* end if */

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
#else /* H5_HAVE_FILTER_SHUFFLE */
    TESTING("shuffle filter");
    SKIPPED();
    puts("    Shuffle filter not enabled");
#endif /* H5_HAVE_FILTER_SHUFFLE */

    /*----------------------------------------------------------
     * STEP 5: Test shuffle + deflate + checksum in any order.
     *----------------------------------------------------------
     */
#if defined H5_HAVE_FILTER_DEFLATE && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32
    puts("Testing shuffle+deflate+checksum filters(checksum first)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_fletcher32 (dc) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;

    if(test_filter_internal(file,DSET_SHUF_DEF_FLET_NAME,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    puts("Testing shuffle+deflate+checksum filters(checksum last)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;
    if(H5Pset_deflate (dc, 6) < 0) goto error;
    if(H5Pset_fletcher32 (dc) < 0) goto error;

    if(test_filter_internal(file,DSET_SHUF_DEF_FLET_NAME_2,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;
#else /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */
    TESTING("shuffle+deflate+fletcher32 filters");
    SKIPPED();
    puts("    Deflate, shuffle, or fletcher32 checksum filter not enabled");
#endif /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */

    /*----------------------------------------------------------
     * STEP 6: Test shuffle + szip + checksum in any order.
     *----------------------------------------------------------
     */
#if defined H5_HAVE_FILTER_SZIP && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32

    TESTING("shuffle+szip+checksum filters(checksum first, with encoder)");
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_fletcher32 (dc) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;

	/* Make sure encoding is enabled */
    if( h5_szip_can_encode() == 1) {
	puts("");
	if(H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block) < 0) goto error;
	if(test_filter_internal(file,DSET_SHUF_SZIP_FLET_NAME,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;
    } else {
		SKIPPED();
    }

    TESTING("shuffle+szip+checksum filters(checksum first, without encoder)");

    if( h5_szip_can_encode() != 1) {
	puts("");
	if(test_filter_noencoder(NOENCODER_SZIP_SHUFF_FLETCH_DATASET) < 0) goto error;
    } else {
		SKIPPED();
    }

    /* Clean up objects used for this test */
    if(H5Pclose (dc) < 0) goto error;

    TESTING("shuffle+szip+checksum filters(checksum last, with encoder)");

    /* Make sure encoding is enabled */
    if( h5_szip_can_encode() == 1) {
	puts("");
	if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
	if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
	if(H5Pset_shuffle (dc) < 0) goto error;
	if(H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block) < 0) goto error;
	if(H5Pset_fletcher32 (dc) < 0) goto error;

	if(test_filter_internal(file,DSET_SHUF_SZIP_FLET_NAME_2,dc,ENABLE_FLETCHER32,DATA_NOT_CORRUPTED,&combo_size) < 0) goto error;

	/* Clean up objects used for this test */
	if(H5Pclose (dc) < 0) goto error;

    } else {
	SKIPPED();
    }

#else /* H5_HAVE_FILTER_SZIP && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */
    TESTING("shuffle+szip+fletcher32 filters");
    SKIPPED();
    puts("    Szip, shuffle, or fletcher32 checksum filter not enabled");
#endif /* H5_HAVE_FILTER_SZIP && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_missing_filter
 *
 * Purpose:	Tests library behavior when filter is missing
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Thursday, November 14, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_missing_filter(hid_t file)
{
    hid_t       fid;            /* File ID */
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    size_t      i,j;            /* Local index variables */
    herr_t      ret;            /* Generic return value */
    char testfile[512]="";      /* Buffer to hold name of existing test file */
    char *srcdir = HDgetenv("srcdir");    /* The source directory, if we are using the --srcdir configure option */

    TESTING("dataset access with missing filter");

    /* Unregister the deflate filter */
#ifdef H5_HAVE_FILTER_DEFLATE
        /* Verify deflate filter is registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=TRUE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter not available\n",__LINE__);
            goto error;
        } /* end if */

        /* Unregister deflate filter (use internal function) */
        if(H5Z_unregister(H5Z_FILTER_DEFLATE) < 0) {
            H5_FAILED();
            printf("    Line %d: Can't unregister deflate filter\n",__LINE__);
            goto error;
        } /* end if */
#endif /* H5_HAVE_FILTER_DEFLATE */
        /* Verify deflate filter is not registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=FALSE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter available\n",__LINE__);
            goto error;
        } /* end if */

    /* Create dcpl with deflate filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_deflate(dcpl, 9) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set deflate filter\n",__LINE__);
        goto error;
    } /* end if */

    /* Check if all the filters are available */
    ret=H5Pall_filters_avail(dcpl);
    if(ret<0) {
        H5_FAILED();
        printf("    Line %d: Can't check filter availability\n",__LINE__);
        goto error;
    } /* end if */
    if(ret!=FALSE) {
        H5_FAILED();
        printf("    Line %d: Filter shouldn't be available\n",__LINE__);
        goto error;
    } /* end if */

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_MISSING_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        printf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        printf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if(0 == (dset_size = H5Dget_storage_size(dsid))) {
        H5_FAILED();
        printf("    Line %d: Error querying dataset size, dset_size=%lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    /* (i.e. the deflation filter we asked for was silently ignored) */
    if((H5Tget_size(H5T_NATIVE_INT) * DSET_DIM1 * DSET_DIM2) != dset_size) {
        H5_FAILED();
        printf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)dims[0]; i++) {
	for(j=0; j<(size_t)dims[1]; j++) {
	    if(points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Line %d: Read different values than written.\n",__LINE__);
		printf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
		printf("    At original: %d\n",points[i][j]);
		printf("    At returned: %d\n",check[i][j]);
		goto error;
	    } /* end if */
	} /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    /* Try reading existing dataset with deflate filter */

    /* Compose the name of the file to open, using the srcdir, if appropriate */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(FILE_DEFLATE_NAME) + 1) < sizeof(testfile))){
	HDstrcpy(testfile, srcdir);
	HDstrcat(testfile, "/");
    }
    HDstrcat(testfile, FILE_DEFLATE_NAME);

    /* Open existing file */
    if((fid = H5Fopen(testfile, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open existing deflated file\n", __LINE__);
        goto error;
    } /* end if */

    /* Open dataset */
    if((dsid = H5Dopen2(fid, "Dataset1", H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Read data (should fail, since deflate filter is missing) */
    H5E_BEGIN_TRY {
        ret = H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        printf("    Line %d: Should not be able to read dataset data\n", __LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Close existing file */
    if(H5Fclose(fid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close file\n", __LINE__);
        goto error;
    } /* end if */

    /* Re-register the deflate filter */
        /* Verify deflate filter is not registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=FALSE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter available\n",__LINE__);
            goto error;
        } /* end if */
#ifdef H5_HAVE_FILTER_DEFLATE
        /* Register deflate filter (use internal function to avoid range checks) */
        if(H5Z_register(H5Z_DEFLATE) < 0) {
            H5_FAILED();
            printf("    Line %d: Can't unregister deflate filter\n",__LINE__);
            goto error;
        } /* end if */

        /* Verify deflate filter is registered currently */
        if(H5Zfilter_avail(H5Z_FILTER_DEFLATE)!=TRUE) {
            H5_FAILED();
            printf("    Line %d: Deflate filter not available\n",__LINE__);
            goto error;
        } /* end if */
#endif /* H5_HAVE_FILTER_DEFLATE */

    PASSED();
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_onebyte_shuffle
 *
 * Purpose:	Tests the 8-bit array with shuffling algorithm.
 *              The shuffled array  should be the same result as
 *              that before the shuffling.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Kent Yang
 *              Wednesday, Nov. 13th, 2002
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_onebyte_shuffle(hid_t file)
{
#ifdef H5_HAVE_FILTER_SHUFFLE
    hid_t		dataset, space,dc;
    const hsize_t	size[2] = {10, 20};
    const hsize_t       chunk_size[2] = {10, 20};
    unsigned char       orig_data[10][20];
    unsigned char       new_data[10][20];
    size_t		i, j;
#else /* H5_HAVE_FILTER_SHUFFLE */
    const char		*not_supported= "    Data shuffling is not enabled.";
#endif /* H5_HAVE_FILTER_SHUFFLE */

    TESTING("8-bit shuffling (setup)");

#ifdef H5_HAVE_FILTER_SHUFFLE
    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use shuffling algorithm with 8-bit  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk (dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_shuffle (dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_ONEBYTE_SHUF_NAME, H5T_NATIVE_UCHAR,
			     space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    for(i= 0;i< 10; i++)
      for(j = 0; j < 20; j++)
	orig_data[i][j] = (unsigned char)HDrandom();

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test shuffling by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("8-bit shuffling (write)");

#ifdef H5_HAVE_FILTER_SHUFFLE
    if(H5Dwrite(dataset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		 orig_data) < 0)
	goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("8-bit shuffling (read)");

#ifdef H5_HAVE_FILTER_SHUFFLE
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_UCHAR, H5S_ALL, H5S_ALL, H5P_DEFAULT,
		new_data) < 0)
	goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
	for(j=0; j<(size_t)size[1]; j++) {
	    if(new_data[i][j] != orig_data[i][j]) {
		H5_FAILED();
		printf("    Read different values than written.\n");
		printf("    At index %lu,%lu\n",
		       (unsigned long)i, (unsigned long)j);
		goto error;
	    }
	}
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Pclose (dc) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_int
 *
 * Purpose:     Tests the integer datatype for nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, Dec. 23th, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_int(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    hid_t               dataset, datatype, mem_datatype, space, dc;
    hsize_t             size[2] = {2, 5};
    hsize_t             chunk_size[2] = {2,5};
    int                 orig_data[2][5];
    int                 new_data[2][5];
    unsigned int        mask;
    size_t              precision, offset;
    size_t             i, j;
#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    puts("Testing nbit filter");
    TESTING("    nbit int (setup)");
#ifdef H5_HAVE_FILTER_NBIT
    /* Define dataset datatype (integer), and set precision, offset */
    datatype = H5Tcopy(H5T_NATIVE_INT);
    precision = 17; /* precision includes sign bit */
    if(H5Tset_precision(datatype,precision) < 0) goto error;
    offset = 4;
    if(H5Tset_offset(datatype,offset) < 0) goto error;

    /* Copy to memory datatype before setting order */
    mem_datatype = H5Tcopy(datatype);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_INT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data, assuming size of long long >= size of int */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = (int)(((long long)HDrandom() %
                           (long long)HDpow(2.0, (double)(precision - 1))) << offset);

        /* even-numbered values are negtive */
        if((i*size[1]+j+1)%2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit int (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, mem_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit int (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, mem_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     * Use mask for checking the significant bits, ignoring the padding bits
     */
    mask = ~((unsigned)~0 << (precision + offset)) & ((unsigned)~0 << offset);
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if((new_data[i][j] & mask) != (orig_data[i][j] & mask)) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Tclose(mem_datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_float
 *
 * Purpose:     Tests the float datatype of nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Friday, Jan. 21th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_float(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    /* orig_data[] are initialized to be within the range that can be represented by
     * dataset datatype (no precision loss during datatype conversion)
     */
    float               orig_data[2][5] = {{(float)188384.00, (float)19.103516, (float)-1.0831790e9, (float)-84.242188,
    (float)5.2045898}, {(float)-49140.000, (float)2350.2500, (float)-3.2110596e-1, (float)6.4998865e-5, (float)-0.0000000}};
    float               new_data[2][5];
    size_t              precision, offset;
    size_t             i, j;
#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit float (setup)");
#ifdef H5_HAVE_FILTER_NBIT
    /* Define user-defined single-precision floating-point type for dataset */
    datatype = H5Tcopy(H5T_IEEE_F32BE);
    if(H5Tset_fields(datatype, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0) goto error;
    offset = 7;
    if(H5Tset_offset(datatype,offset) < 0) goto error;
    precision = 20;
    if(H5Tset_precision(datatype,precision) < 0) goto error;
    if(H5Tset_size(datatype, (size_t)4) < 0) goto error;
    if(H5Tset_ebias(datatype, (size_t)31) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_FLOAT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit float (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit float (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     * Assume size of int = size of float
     */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(!(orig_data[i][j]==orig_data[i][j])) continue;  /* skip if value is NaN */
            if(new_data[i][j] != orig_data[i][j]) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_double
 *
 * Purpose:     Tests the double datatype of nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, Jan. 26th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_double(hid_t file)
{
/* assume unsigned int and float has the same number of bytes */
#ifdef H5_HAVE_FILTER_NBIT
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    /* orig_data[] are initialized to be within the range that can be represented by
     * dataset datatype (no precision loss during datatype conversion)
     */
    double              orig_data[2][5] = {{1.6081706885101836e+60, -255.32099170994480,
    1.2677579992621376e-61, 64568.289448797700, -1.0619721778839084e-75}, {2.1499497833454840e+56,
    6.6562295504670740e-3, -1.5747263393432150, 1.0711093225222612, -9.8971679387636870e-1}};
    double              new_data[2][5];
    size_t              precision, offset;
    size_t             i, j;
#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit double (setup)");
#ifdef H5_HAVE_FILTER_NBIT
    /* Define user-defined doule-precision floating-point type for dataset */
    datatype = H5Tcopy(H5T_IEEE_F64BE);
    if(H5Tset_fields(datatype, (size_t)55, (size_t)46, (size_t)9, (size_t)5, (size_t)41) < 0) goto error;
    offset = 5;
    if(H5Tset_offset(datatype,offset) < 0) goto error;
    precision = 51;
    if(H5Tset_precision(datatype,precision) < 0) goto error;
    if(H5Tset_size(datatype, (size_t)8) < 0) goto error;
    if(H5Tset_ebias(datatype, (size_t)255) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_DOUBLE_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit double (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit double (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     * Assume size of long long = size of double
     */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(!(orig_data[i][j]==orig_data[i][j])) continue;  /* skip if value is NaN */
            if(new_data[i][j] != orig_data[i][j]) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_array
 *
 * Purpose:     Tests the simple version array datatype for nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, Jan. 18th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_array(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    hid_t               dataset, base_datatype, array_datatype, space, dc;
    hid_t               mem_base_datatype, mem_array_datatype;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       adims[2] = {3, 2};
    const hsize_t       chunk_size[2] = {2,5};
    unsigned int        orig_data[2][5][3][2];
    unsigned int        new_data[2][5][3][2];
    size_t              precision, offset;
    size_t             i, j, m, n;
#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit array (setup)");
#ifdef H5_HAVE_FILTER_NBIT
    /* Define dataset array datatype's base datatype and set precision, offset */
    base_datatype = H5Tcopy(H5T_NATIVE_UINT);
    precision = 22;
    if(H5Tset_precision(base_datatype,precision) < 0) goto error;
    offset = 7;
    if(H5Tset_offset(base_datatype,offset) < 0) goto error;

    /* Copy to memory array datatype's base datatype before setting order */
    mem_base_datatype = H5Tcopy(base_datatype);

    /* Set order of dataset array datatype's base datatype */
    if(H5Tset_order(base_datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create dataset array datatype */
    array_datatype = H5Tarray_create2(base_datatype, 2, adims);

    /* Create memory array datatype */
    mem_array_datatype = H5Tarray_create2(mem_base_datatype, 2, adims);

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_ARRAY_NAME, array_datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data, assuming size of long long >= size of unsigned int */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++)
        for(m = 0; m < (size_t)adims[0]; m++)
          for(n = 0; n < (size_t)adims[1]; n++)
            orig_data[i][j][m][n] = (unsigned int)(((long long)HDrandom() %
                                     (long long)HDpow(2.0, (double)precision)) << offset);
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit array (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, mem_array_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit array (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, mem_array_datatype, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     */
    for(i=0; i<(size_t)size[0]; i++)
      for(j=0; j<(size_t)size[1]; j++)
        for(m = 0; m < (size_t)adims[0]; m++)
          for(n = 0; n < (size_t)adims[1]; n++) {
            if(new_data[i][j][m][n]!= orig_data[i][j][m][n]) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu,%lu,%lu\n",
                (unsigned long)i, (unsigned long)j, (unsigned long)m, (unsigned long)n);
                goto error;
            }
          }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(array_datatype) < 0) goto error;
    if(H5Tclose(base_datatype) < 0) goto error;
    if(H5Tclose(mem_array_datatype) < 0) goto error;
    if(H5Tclose(mem_base_datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_compound
 *
 * Purpose:     Tests a simple version of compound datatype of nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, Jan. 18th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_compound(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    typedef struct {     /* Struct with atomic fields */
        int i;
        char c;
        short s;
        float f;
    } atomic;
    hid_t               i_tid, c_tid, s_tid, f_tid;
    hid_t               cmpd_tid; /* atomic compound datatype */
    hid_t               mem_cmpd_tid; /* memory atomic compound datatype */
    size_t              precision[3] = {15, 7, 10};
    size_t              offset[3] = {9, 0, 3};
    hid_t               dataset, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    const float         float_val[2][5] = {{(float)188384.00, (float)19.103516, (float)-1.0831790e9, (float)-84.242188,
    (float)5.2045898}, {(float)-49140.000, (float)2350.2500, (float)-3.2110596e-1, (float)6.4998865e-5, (float)-0.0000000}};
    atomic              orig_data[2][5];
    atomic              new_data[2][5];
    unsigned int        i_mask, s_mask, c_mask;
    size_t             i, j;

#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit compound (setup)");
#ifdef H5_HAVE_FILTER_NBIT
    /* Define datatypes of members of compound datatype */
    i_tid=H5Tcopy(H5T_NATIVE_INT);
    c_tid=H5Tcopy(H5T_NATIVE_CHAR);
    s_tid=H5Tcopy(H5T_NATIVE_SHORT);
    f_tid=H5Tcopy(H5T_IEEE_F32BE);

    /* Set precision and offset etc. */
    if(H5Tset_precision(i_tid,precision[0]) < 0) goto error;
    if(H5Tset_offset(i_tid,offset[0]) < 0) goto error;

    if(H5Tset_precision(c_tid,precision[1]) < 0) goto error;
    if(H5Tset_offset(c_tid,offset[1]) < 0) goto error;

    if(H5Tset_precision(s_tid,precision[2]) < 0) goto error;
    if(H5Tset_offset(s_tid,offset[2]) < 0) goto error;

    if(H5Tset_fields(f_tid, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0) goto error;
    if(H5Tset_offset(f_tid, (size_t)7) < 0) goto error;
    if(H5Tset_precision(f_tid, (size_t)20) < 0) goto error;
    if(H5Tset_size(f_tid, (size_t)4) < 0) goto error;
    if(H5Tset_ebias(f_tid, (size_t)31) < 0) goto error;

    /* Create a memory compound datatype before setting the order */
    mem_cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(atomic));
    if(H5Tinsert(mem_cmpd_tid, "i", HOFFSET(atomic, i), i_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid, "c", HOFFSET(atomic, c), c_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid, "s", HOFFSET(atomic, s), s_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid, "f", HOFFSET(atomic, f), H5T_NATIVE_FLOAT) < 0) goto error;

    /* Create a dataset compound datatype and insert some atomic types */
    cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(atomic));
    if(H5Tinsert(cmpd_tid, "i", HOFFSET(atomic, i), i_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "c", HOFFSET(atomic, c), c_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "s", HOFFSET(atomic, s), s_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "f", HOFFSET(atomic, f), f_tid) < 0) goto error;

    /* Set order of dataset compound datatype */
    if(H5Tset_order(cmpd_tid, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_COMPOUND_NAME, cmpd_tid,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data, assuming size of long long >= size of member datatypes */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j].i = (int)(((long long)HDrandom() %
                             (long long)HDpow(2.0, (double)(precision[0]-1))) << offset[0]);
        orig_data[i][j].c = (char)(((long long)HDrandom() %
                             (long long)HDpow(2.0, (double)(precision[1]-1))) << offset[1]);
        orig_data[i][j].s = (short)(((long long)HDrandom() %
                             (long long)HDpow(2.0, (double)(precision[2]-1))) << offset[2]);
        orig_data[i][j].f = float_val[i][j];

        /* some even-numbered integer values are negtive */
        if((i*size[1]+j+1)%2 == 0) {
            orig_data[i][j].i = -orig_data[i][j].i;
            orig_data[i][j].s = (short)-orig_data[i][j].s;
        }
      }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, mem_cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, mem_cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     * Use mask for checking the significant bits, ignoring the padding bits
     */
    i_mask = ~((unsigned)~0 << (precision[0] + offset[0])) & ((unsigned)~0 << offset[0]);
    c_mask = ~((unsigned)~0 << (precision[1] + offset[1])) & ((unsigned)~0 << offset[1]);
    s_mask = ~((unsigned)~0 << (precision[2] + offset[2])) & ((unsigned)~0 << offset[2]);
    for(i=0; i<size[0]; i++) {
        for(j=0; j<size[1]; j++) {
            if((new_data[i][j].i & i_mask) != (orig_data[i][j].i & i_mask) ||
                (new_data[i][j].c & c_mask) != (orig_data[i][j].c & c_mask) ||
                (new_data[i][j].s & s_mask) != (orig_data[i][j].s & s_mask) ||
                (orig_data[i][j].f==orig_data[i][j].f && new_data[i][j].f != orig_data[i][j].f))
            {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(i_tid) < 0) goto error;
    if(H5Tclose(c_tid) < 0) goto error;
    if(H5Tclose(s_tid) < 0) goto error;
    if(H5Tclose(f_tid) < 0) goto error;
    if(H5Tclose(cmpd_tid) < 0) goto error;
    if(H5Tclose(mem_cmpd_tid) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_compound_2
 *
 * Purpose:     Tests a complex version of compound datatype of nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, Jan. 18th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_compound_2(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    typedef struct {     /* Struct with atomic fields */
        int i;
        char c;
        short s;
        float f;
    } atomic;

    typedef struct {     /* Struct with complex fields */
        atomic a;
        unsigned int v;
        char   b[2][2];
        atomic d[2][2];
    } complex;

    hid_t               i_tid, c_tid, s_tid, f_tid, v_tid;
    hid_t               cmpd_tid1; /* atomic compound datatype */
    hid_t               cmpd_tid2; /* complex compound datatype */
    hid_t               mem_cmpd_tid1; /* memory atomic compound datatype */
    hid_t               mem_cmpd_tid2; /* memory complex compound datatype */
    hid_t               base_tid;      /* simple array datatype's base datatype */
    hid_t               array_tid;     /* simple array datatype */
    hid_t               array_cmplx_tid;     /* complex array datatype */
    hid_t               mem_array_cmplx_tid; /* memory complex array datatype */
    const hsize_t       array_dims[2] = {2, 2};
    size_t              precision[5] = {31, 8, 10, 23, 8};
    size_t              offset[5] = {1, 0, 3, 5, 0};
    hid_t               dataset, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2, 5};
    const float         float_val[2][5] = {{(float)188384.00, (float)19.103516, (float)-1.0831790e9, (float)-84.242188,
    (float)5.2045898}, {(float)-49140.000, (float)2350.2500, (float)-3.2110596e-1, (float)6.4998865e-5, (float)-0.0000000}};
    complex             orig_data[2][5];
    complex             new_data[2][5];
    unsigned int        i_mask, s_mask, c_mask, b_mask;
    size_t             i, j, m, n, b_failed, d_failed;

#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit compound complex (setup)");
#ifdef H5_HAVE_FILTER_NBIT
    /* Define datatypes of members of compound datatype */
    i_tid=H5Tcopy(H5T_NATIVE_INT);
    c_tid=H5Tcopy(H5T_NATIVE_CHAR);
    s_tid=H5Tcopy(H5T_NATIVE_SHORT);
    v_tid=H5Tcopy(H5T_NATIVE_UINT);
    f_tid=H5Tcopy(H5T_IEEE_F32BE);

    /* Set precision and offset etc. of atomic compound datatype members */
    if(H5Tset_precision(i_tid,precision[0]) < 0) goto error;
    if(H5Tset_offset(i_tid,offset[0]) < 0) goto error;

    if(H5Tset_precision(c_tid,precision[1]) < 0) goto error;
    if(H5Tset_offset(c_tid,offset[1]) < 0) goto error;

    if(H5Tset_precision(s_tid,precision[2]) < 0) goto error;
    if(H5Tset_offset(s_tid,offset[2]) < 0) goto error;

    if(H5Tset_fields(f_tid, (size_t)26, (size_t)20, (size_t)6, (size_t)7, (size_t)13) < 0) goto error;
    if(H5Tset_offset(f_tid, (size_t)7) < 0) goto error;
    if(H5Tset_precision(f_tid, (size_t)20) < 0) goto error;
    if(H5Tset_size(f_tid, (size_t)4) < 0) goto error;
    if(H5Tset_ebias(f_tid, (size_t)31) < 0) goto error;

    /* Create a memory atomic compound datatype before setting the order */
    mem_cmpd_tid1 = H5Tcreate(H5T_COMPOUND, sizeof(atomic));
    if(H5Tinsert(mem_cmpd_tid1, "i", HOFFSET(atomic, i), i_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid1, "c", HOFFSET(atomic, c), c_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid1, "s", HOFFSET(atomic, s), s_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid1, "f", HOFFSET(atomic, f), H5T_NATIVE_FLOAT) < 0) goto error;

    /* Create a dataset atomic compound datatype and insert some atomic types */
    cmpd_tid1 = H5Tcreate(H5T_COMPOUND, sizeof(atomic));
    if(H5Tinsert(cmpd_tid1, "i", HOFFSET(atomic, i), i_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid1, "c", HOFFSET(atomic, c), c_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid1, "s", HOFFSET(atomic, s), s_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid1, "f", HOFFSET(atomic, f), f_tid) < 0) goto error;

    /* Set order of dataset compound datatype */
    if(H5Tset_order(cmpd_tid1, H5T_ORDER_BE) < 0) goto error;

    /* Set precision and offset of the other data member */
    if(H5Tset_precision(v_tid,precision[3]) < 0) goto error;
    if(H5Tset_offset(v_tid,offset[3]) < 0) goto error;

    /* Create the simple array datatype */
    base_tid = H5Tcopy(H5T_NATIVE_CHAR);
    if(H5Tset_precision(base_tid,precision[4]) < 0) goto error;
    if(H5Tset_offset(base_tid,offset[4]) < 0) goto error;
    array_tid = H5Tarray_create2(base_tid, 2, array_dims);

    /* Create the complex memory and dataset array datatype */
    array_cmplx_tid = H5Tarray_create2(cmpd_tid1, 2, array_dims);
    mem_array_cmplx_tid = H5Tarray_create2(mem_cmpd_tid1, 2, array_dims);

    /* Create a memory complex compound datatype before setting the order */
    mem_cmpd_tid2 = H5Tcreate(H5T_COMPOUND, sizeof(complex));
    if(H5Tinsert(mem_cmpd_tid2, "a", HOFFSET(complex, a), mem_cmpd_tid1) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid2, "v", HOFFSET(complex, v), v_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid2, "b", HOFFSET(complex, b), array_tid) < 0) goto error;
    if(H5Tinsert(mem_cmpd_tid2, "d", HOFFSET(complex, d), mem_array_cmplx_tid) < 0) goto error;

    /* Set order of dataset other complex compound member datatype */
    if(H5Tset_order(v_tid, H5T_ORDER_BE) < 0) goto error;

    /* Create a dataset complex compound datatype and insert members */
    cmpd_tid2 = H5Tcreate(H5T_COMPOUND, sizeof(complex));
    if(H5Tinsert(cmpd_tid2, "a", HOFFSET(complex, a), cmpd_tid1) < 0) goto error;
    if(H5Tinsert(cmpd_tid2, "v", HOFFSET(complex, v), v_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid2, "b", HOFFSET(complex, b), array_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid2, "d", HOFFSET(complex, d), array_cmplx_tid) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_COMPOUND_NAME_2, cmpd_tid2,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data, assuming size of long long >= size of member datatypes */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j].a.i = (int)(((long long)HDrandom() %
                               (long long)HDpow(2.0, (double)(precision[0]-1))) << offset[0]);
        orig_data[i][j].a.c = (char)(((long long)HDrandom() %
                               (long long)HDpow(2.0, (double)(precision[1]-1))) << offset[1]);
        orig_data[i][j].a.s = (short)(-((long long)HDrandom() %
                               (long long)HDpow(2.0, (double)(precision[2]-1))) << offset[2]);
        orig_data[i][j].a.f = float_val[i][j];

        orig_data[i][j].v = (unsigned int)(((long long)HDrandom() %
                             (long long)HDpow(2.0, (double)precision[3])) << offset[3]);

        for(m = 0; m < (size_t)array_dims[0]; m++)
          for(n = 0; n < (size_t)array_dims[1]; n++)
            orig_data[i][j].b[m][n] = (char)(((long long)HDrandom() %
                                       (long long)HDpow(2.0, (double)(precision[4]-1))) << offset[4]);

        for(m = 0; m < (size_t)array_dims[0]; m++)
          for(n = 0; n < (size_t)array_dims[1]; n++) {
            orig_data[i][j].d[m][n].i = (int)(-((long long)HDrandom() %
                                         (long long)HDpow(2.0, (double)(precision[0]-1))) << offset[0]);
            orig_data[i][j].d[m][n].c = (char)(((long long)HDrandom() %
                                         (long long)HDpow(2.0, (double)(precision[1]-1))) << offset[1]);
            orig_data[i][j].d[m][n].s = (short)(((long long)HDrandom() %
                                         (long long)HDpow(2.0, (double)(precision[2]-1))) << offset[2]);
            orig_data[i][j].d[m][n].f = float_val[i][j];
          }
      }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound complex (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, mem_cmpd_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound complex (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, mem_cmpd_tid2, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written
     * Use mask for checking the significant bits, ignoring the padding bits
     */
    i_mask = ~((unsigned)~0 << (precision[0] + offset[0])) & ((unsigned)~0 << offset[0]);
    c_mask = ~((unsigned)~0 << (precision[1] + offset[1])) & ((unsigned)~0 << offset[1]);
    s_mask = ~((unsigned)~0 << (precision[2] + offset[2])) & ((unsigned)~0 << offset[2]);
    b_mask = ~((unsigned)~0 << (precision[4] + offset[4])) & ((unsigned)~0 << offset[4]);
    for(i=0; i<(size_t)size[0]; i++) {
      for(j=0; j<(size_t)size[1]; j++) {
        b_failed = 0;
        d_failed = 0;

        for(m = 0; m < (size_t)array_dims[0]; m++)
          for(n = 0; n < (size_t)array_dims[1]; n++)
             if((new_data[i][j].b[m][n]&b_mask)!=(orig_data[i][j].b[m][n]&b_mask)) {
                b_failed = 1;
                goto out;
             }

        for(m = 0; m < (size_t)array_dims[0]; m++)
          for(n = 0; n < (size_t)array_dims[1]; n++)
             if((new_data[i][j].d[m][n].i & i_mask)!=(orig_data[i][j].d[m][n].i & i_mask)||
                (new_data[i][j].d[m][n].c & c_mask)!=(orig_data[i][j].d[m][n].c & c_mask)||
                (new_data[i][j].d[m][n].s & s_mask)!=(orig_data[i][j].d[m][n].s & s_mask)||
                (new_data[i][j].d[m][n].f==new_data[i][j].d[m][n].f &&
                 new_data[i][j].d[m][n].f != new_data[i][j].d[m][n].f)) {
                d_failed = 1;
                goto out;
             }

        out:
        if((new_data[i][j].a.i & i_mask)!=(orig_data[i][j].a.i & i_mask)||
           (new_data[i][j].a.c & c_mask)!=(orig_data[i][j].a.c & c_mask)||
           (new_data[i][j].a.s & s_mask)!=(orig_data[i][j].a.s & s_mask)||
           (new_data[i][j].a.f==new_data[i][j].a.f &&
            new_data[i][j].a.f != new_data[i][j].a.f)||
            new_data[i][j].v != orig_data[i][j].v || b_failed || d_failed) {
           H5_FAILED();
           printf("    Read different values than written.\n");
           printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
           goto error;
        }
      }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(i_tid) < 0) goto error;
    if(H5Tclose(c_tid) < 0) goto error;
    if(H5Tclose(s_tid) < 0) goto error;
    if(H5Tclose(f_tid) < 0) goto error;
    if(H5Tclose(v_tid) < 0) goto error;
    if(H5Tclose(cmpd_tid2) < 0) goto error;
    if(H5Tclose(cmpd_tid1) < 0) goto error;
    if(H5Tclose(mem_cmpd_tid2) < 0) goto error;
    if(H5Tclose(mem_cmpd_tid1) < 0) goto error;
    if(H5Tclose(array_tid) < 0) goto error;
    if(H5Tclose(base_tid) < 0) goto error;
    if(H5Tclose(array_cmplx_tid) < 0) goto error;
    if(H5Tclose(mem_array_cmplx_tid) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_compound_3
 *
 * Purpose:     Tests no-op datatypes in compound datatype for nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Thursday, Mar. 31th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_compound_3(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    typedef struct {     /* Struct with some no-op type fields */
        int i;              /* integer field, NOT a no-op type */
        char str[30];       /* fixed-length string, no-op type */
        char *vl_str;       /* varible-length string, no-op type */
        hvl_t v;            /* VL datatype field, no-op type */
        hobj_ref_t r;       /* Object reference field, no-op type */
        unsigned char o[5]; /* Opaque field, no-op type */
    } atomic;
    hid_t               i_tid, str_tid, vl_str_tid, v_tid, o_tid;
    hid_t               cmpd_tid; /* atomic compound datatype */
    hid_t               dataset, space, dc, obj_ref_dataset = -1;
    const hsize_t       size[1] = {5};
    const hsize_t       chunk_size[1] = {5};
    atomic              orig_data[5];
    atomic              new_data[5];
    size_t             i, k, j;

#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit compound with no-op type (setup)");
#ifdef H5_HAVE_FILTER_NBIT

    /* Define datatypes of members of compound datatype */
    i_tid=H5Tcopy(H5T_NATIVE_INT);
    if(H5Tset_precision(i_tid, (size_t)17) < 0) goto error;

    str_tid=H5Tcopy(H5T_C_S1);
    if(H5Tset_size(str_tid, (size_t)30) < 0) goto error;

    vl_str_tid = H5Tcopy(H5T_C_S1);
    if(H5Tset_size(vl_str_tid,H5T_VARIABLE) < 0) goto error;

    if((v_tid = H5Tvlen_create(H5T_NATIVE_UINT)) < 0) goto error;

    if((o_tid = H5Tcreate(H5T_OPAQUE, (size_t)5)) < 0) goto error;
    if(H5Tset_tag(o_tid, "testing opaque field") < 0) goto error;

    /* Create a dataset compound datatype and insert some atomic types */
    cmpd_tid = H5Tcreate(H5T_COMPOUND, sizeof(atomic));
    if(H5Tinsert(cmpd_tid, "i", HOFFSET(atomic, i), i_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "str", HOFFSET(atomic, str), str_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "vl_str", HOFFSET(atomic, vl_str), vl_str_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "v", HOFFSET(atomic, v), v_tid) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "r", HOFFSET(atomic, r), H5T_STD_REF_OBJ) < 0) goto error;
    if(H5Tinsert(cmpd_tid, "o", HOFFSET(atomic, o), o_tid) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(1, size, NULL)) < 0) goto error;

    /* Use nbit filter  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dc, 1, chunk_size) < 0) goto error;
    if(H5Pset_nbit(dc) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_NBIT_COMPOUND_NAME_3, cmpd_tid,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the dataset object reference points to */
    if((obj_ref_dataset = H5Dcreate2(file, "nbit_obj_ref", H5T_NATIVE_INT,
                                     space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i = 0; i < (size_t)size[0]; i++) {
        HDmemset(&orig_data[i], 0, sizeof(orig_data[i]));
        orig_data[i].i = HDrandom() % (long)HDpow(2.0, 17.0 - 1.0);
        HDstrcpy(orig_data[i].str, "fixed-length C string");
        orig_data[i].vl_str = HDstrdup("variable-length C string");

        orig_data[i].v.p = HDmalloc((size_t)(i+1)*sizeof(unsigned int));
        orig_data[i].v.len = (size_t)i+1;
        for(k = 0; k < (i+1); k++) ((unsigned int *)orig_data[i].v.p)[k] = (unsigned int)(i*100 + k);

        /* Create reference to the dataset "nbit_obj_ref" */
        if(H5Rcreate(&orig_data[i].r, file, "nbit_obj_ref", H5R_OBJECT, -1) < 0) goto error;

        for(j = 0; j < 5; j++) orig_data[i].o[j] = (unsigned char)(i + j);
    }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test nbit by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound with no-op type (write)");

#ifdef H5_HAVE_FILTER_NBIT
    if(H5Dwrite(dataset, cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0)
        goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    nbit compound with no-op type (read)");

#ifdef H5_HAVE_FILTER_NBIT
    /* Read the dataset back */
    if(H5Dread(dataset, cmpd_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0)
        goto error;

    /* Check that the values read are the same as the values written */
    for(i = 0; i < (size_t)size[0]; i++) {
        if(new_data[i].i != orig_data[i].i ||
           strcmp(new_data[i].str, orig_data[i].str) !=0 ||
           strcmp(new_data[i].vl_str, orig_data[i].vl_str) !=0 ||
           new_data[i].v.len != orig_data[i].v.len ||
           new_data[i].r != orig_data[i].r)
        {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %lu\n", (unsigned long)i);
            goto error;
        }

        for(k=0; k<i+1; k++)
            if(((unsigned int *)orig_data[i].v.p)[k] !=((unsigned int *)new_data[i].v.p)[k])
            {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu\n", (unsigned long)i);
                goto error;
            }

        for(j=0; j<5; j++)
            if(orig_data[i].o[j] != new_data[i].o[j])
            {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu\n", (unsigned long)i);
                goto error;
            }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Dvlen_reclaim(cmpd_tid, space, H5P_DEFAULT, new_data) < 0) goto error;
    if(H5Dvlen_reclaim(cmpd_tid, space, H5P_DEFAULT, orig_data) < 0) goto error;
    if(H5Tclose(i_tid) < 0) goto error;
    if(H5Tclose(str_tid) < 0) goto error;
    if(H5Tclose(vl_str_tid) < 0) goto error;
    if(H5Tclose(v_tid) < 0) goto error;
    if(H5Tclose(o_tid) < 0) goto error;
    if(H5Tclose(cmpd_tid) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(obj_ref_dataset) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;

error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_int_size
 *
 * Purpose:     Tests the correct size of the integer datatype for nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              19 November 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_int_size(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    hid_t   dataspace, dataset, datatype, mem_datatype, dset_create_props;
    hsize_t dims[2], chunk_size[2];
    hsize_t dset_size = 0;
    int     orig_data[DSET_DIM1][DSET_DIM2];
    int     i, j;
    size_t  precision, offset;
#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit integer dataset size");
#ifdef H5_HAVE_FILTER_NBIT

   /* Define dataset datatype (integer), and set precision, offset */
   if((datatype = H5Tcopy(H5T_NATIVE_INT)) < 0) {
       H5_FAILED();
       printf("    line %d: H5Tcopy failed\n",__LINE__);
       goto error;
   } /* end if */

   precision = 16; /* precision includes sign bit */
   if(H5Tset_precision(datatype,precision)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_precision failed\n",__LINE__);
       goto error;
   } /* end if */

   offset = 8;
   if(H5Tset_offset(datatype,offset)<0) {
       H5_FAILED();
       printf("    line %d: H5Tset_offset failed\n",__LINE__);
       goto error;
   } /* end if */

   /* Copy to memory datatype */
   if((mem_datatype = H5Tcopy(datatype)) < 0) {
       H5_FAILED();
       printf("    line %d: H5Tcopy failed\n",__LINE__);
       goto error;
   } /* end if */

   /* Set order of dataset datatype */
   if(H5Tset_order(datatype, H5T_ORDER_BE)<0) {
        H5_FAILED();
       printf("    line %d: H5Pset_order failed\n",__LINE__);
       goto error;
   } /* end if */
  
   if(H5Tset_size(datatype, 4)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_size failed\n",__LINE__);
       goto error;
   } /* end if */

  /* Initiliaze data buffer with random data within correct range
   * corresponding to the memory datatype's precision and offset.
   */
   for (i=0; i < DSET_DIM1; i++)
       for (j=0; j < DSET_DIM2; j++)
           orig_data[i][j] = rand() % (int)pow(2, precision-1) <<offset;


   /* Describe the dataspace. */
   dims[0] = DSET_DIM1;
   dims[1] = DSET_DIM2;
   if((dataspace = H5Screate_simple (2, dims, NULL))<0) {
       H5_FAILED();
       printf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set the dataset creation property list to specify the chunks
   */
   chunk_size[0] = DSET_DIM1/10;
   chunk_size[1] = DSET_DIM2/10;
   if((dset_create_props = H5Pcreate (H5P_DATASET_CREATE))<0) {
       H5_FAILED();
       printf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Pset_chunk (dset_create_props, 2, chunk_size)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_chunk failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set for n-bit compression
   */
   if(H5Pset_nbit (dset_create_props)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_nbit failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Create a new dataset within the file.
   */
   if((dataset = H5Dcreate2 (file, DSET_NBIT_INT_SIZE_NAME, datatype,
                            dataspace, H5P_DEFAULT, 
                            dset_create_props, H5P_DEFAULT))<0) {
       H5_FAILED();
       printf("    line %d: H5dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Write the array to the file.
   */
   if(H5Dwrite (dataset, mem_datatype, H5S_ALL, H5S_ALL,
                H5P_DEFAULT, orig_data)<0) {
       H5_FAILED();
       printf("    Line %d: H5Dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

   /* 
    * Get the precision of the data type
    */
   if((precision = H5Tget_precision(datatype)) == 0) {
       H5_FAILED();
       printf("    Line %d: wrong precision size: %zu\n",__LINE__, precision);
       goto error;
   } /* end if */

   /* 
    * The size of the dataset after compression should around 2 * DSET_DIM1 * DSET_DIM2
    */
   if((dset_size = H5Dget_storage_size(dataset)) < DSET_DIM1*DSET_DIM2*(precision/8) || 
       dset_size > DSET_DIM1*DSET_DIM2*(precision/8) + 1*KB) {
       H5_FAILED();
       HDfprintf(stdout, "    Line %d: wrong dataset size: %Hu\n",__LINE__, dset_size);
       goto error;
   } /* end if */

   H5Tclose (datatype);
   H5Tclose (mem_datatype);
   H5Dclose (dataset);
   H5Sclose (dataspace);
   H5Pclose (dset_create_props);

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

   return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_nbit_flt_size
 *
 * Purpose:     Tests the correct size of the floating-number datatype for 
 *              nbit filter
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Raymond Lu
 *              19 November 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_nbit_flt_size(hid_t file)
{
#ifdef H5_HAVE_FILTER_NBIT
    hid_t   dataspace, dataset, datatype, dset_create_props;
    hsize_t dims[2], chunk_size[2];
    hsize_t dset_size = 0;
    float   orig_data[DSET_DIM1][DSET_DIM2];
    int     i, j;
    size_t  precision, offset;
    size_t  spos, epos, esize, mpos, msize;
#else /* H5_HAVE_FILTER_NBIT */
    const char          *not_supported= "    Nbit is not enabled.";
#endif /* H5_HAVE_FILTER_NBIT */

    TESTING("    nbit floating-number dataset size");
#ifdef H5_HAVE_FILTER_NBIT

  /* Define floating-point type for dataset
   *-------------------------------------------------------------------
   * size=4 byte, precision=16 bits, offset=8 bits,
   * mantissa size=9 bits, mantissa position=8,
   * exponent size=6 bits, exponent position=17,
   * exponent bias=31.
   * It can be illustrated in little-endian order as:
   * (S - sign bit, E - exponent bit, M - mantissa bit,
   *  ? - padding bit)
   *
   *           3        2        1        0
   *       ???????? SEEEEEEM MMMMMMMM ????????
   *
   * To create a new floating-point type, the following
   * properties must be set in the order of
   *     set fields -> set offset -> set precision -> set size.
   * All these properties must be set before the type can function.
   * Other properties can be set anytime. Derived type size cannot
   * be expanded bigger than original size but can be decreased.
   * There should be no holes among the significant bits. Exponent
   * bias usually is set 2^(n-1)-1, where n is the exponent size.
   *-------------------------------------------------------------------*/
   if((datatype = H5Tcopy(H5T_IEEE_F32LE)) < 0) {
       H5_FAILED();
       printf("    line %d: H5Tcopy failed\n",__LINE__);
       goto error;
   } /* end if */

   msize = 9;
   spos = 23;
   epos = 17;
   esize = 6;
   mpos = 8;
   offset = 8;
   precision = 16;

   if(H5Tset_fields(datatype, spos, epos, esize, mpos, msize)<0) {
       H5_FAILED();
       printf("    line %d: H5Tset_fields failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_offset(datatype,offset)<0) {
       H5_FAILED();
       printf("    line %d: H5Tset_offset failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_precision(datatype,precision)<0) {
       H5_FAILED();
       printf("    line %d: H5Tset_precision failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Tset_size(datatype, 4)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_size failed\n",__LINE__);
       goto error;
   } /* end if */

   /* Set order of dataset datatype */
   if(H5Tset_order(datatype, H5T_ORDER_BE)<0) {
        H5_FAILED();
       printf("    line %d: H5Pset_order failed\n",__LINE__);
       goto error;
   } /* end if */
  
   if(H5Tset_ebias(datatype, 31)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_size failed\n",__LINE__);
       goto error;
   } /* end if */

  /* 
   * Initiliaze data buffer with random data 
   */
   for (i=0; i < DSET_DIM1; i++)
       for (j=0; j < DSET_DIM2; j++)
           orig_data[i][j] = (rand() % 1234567) / 2;


   /* Describe the dataspace. */
   dims[0] = DSET_DIM1;
   dims[1] = DSET_DIM2;
   if((dataspace = H5Screate_simple (2, dims, NULL))<0) {
       H5_FAILED();
       printf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set the dataset creation property list to specify the chunks
   */
   chunk_size[0] = DSET_DIM1/10;
   chunk_size[1] = DSET_DIM2/10;
   if((dset_create_props = H5Pcreate (H5P_DATASET_CREATE))<0) {
       H5_FAILED();
       printf("    line %d: H5Pcreate failed\n",__LINE__);
       goto error;
   } /* end if */

   if(H5Pset_chunk (dset_create_props, 2, chunk_size)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_chunk failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Set for n-bit compression
   */
   if(H5Pset_nbit (dset_create_props)<0) {
       H5_FAILED();
       printf("    line %d: H5Pset_nbit failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Create a new dataset within the file.
   */
   if((dataset = H5Dcreate2 (file, DSET_NBIT_FLT_SIZE_NAME, datatype,
                            dataspace, H5P_DEFAULT, 
                            dset_create_props, H5P_DEFAULT))<0) {
       H5_FAILED();
       printf("    line %d: H5dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

  /*
   * Write the array to the file.
   */
   if(H5Dwrite (dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
                H5P_DEFAULT, orig_data)<0) {
       H5_FAILED();
       printf("    Line %d: H5Dwrite failed\n",__LINE__);
       goto error;
   } /* end if */

   /* 
    * Get the precision of the data type
    */
   if((precision = H5Tget_precision(datatype)) == 0) {
       H5_FAILED();
       printf("    Line %d: wrong precision size: %zu\n",__LINE__, precision);
       goto error;
   } /* end if */

   /* 
    * The size of the dataset after compression should around 2 * DSET_DIM1 * DSET_DIM2
    */
   if((dset_size = H5Dget_storage_size(dataset)) < DSET_DIM1*DSET_DIM2*(precision/8) || 
       dset_size > DSET_DIM1*DSET_DIM2*(precision/8) + 1*KB) {
       H5_FAILED();
       HDfprintf(stdout, "    Line %d: wrong dataset size: %Hu\n",__LINE__, dset_size);
       goto error;
   } /* end if */

   H5Tclose (datatype);
   H5Dclose (dataset);
   H5Sclose (dataspace);
   H5Pclose (dset_create_props);

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

   return 0;
error:
    return -1;
}

/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_int
 *
 * Purpose:     Tests the integer datatype for scaleoffset filter
 *              with fill value not defined
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Monday, Feb. 14th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_int(hid_t file)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    int                 orig_data[2][5];
    int                 new_data[2][5];
    size_t             i, j;
#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "    Scaleoffset is not enabled.";
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    puts("Testing scaleoffset filter");
    TESTING("    scaleoffset int without fill value (setup)");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    datatype = H5Tcopy(H5T_NATIVE_INT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Fill value undefined */
    if(H5Pset_fill_value(dc, datatype, NULL) < 0) goto error;

    /* Set up to use scaleoffset filter, let library calculate minbits */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_INT,H5Z_SO_INT_MINBITS_DEFAULT) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_INT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = HDrandom() % 10000;

        /* even-numbered values are negtive */
        if((i*size[1]+j+1)%2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int without fill value (write)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    if(H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int without fill value (read)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(new_data[i][j] != orig_data[i][j]) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_int_2
 *
 * Purpose:     Tests the integer datatype for scaleoffset filter
 *              with fill value set
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Tuesday, March 15th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_int_2(hid_t file)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t               dataset, datatype, space, mspace, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    int                 orig_data[2][5];
    int                 new_data[2][5];
    hsize_t             start[2]; /* Start of hyperslab */
    hsize_t             stride[2]; /* Stride of hyperslab */
    hsize_t             count[2];  /* Block count */
    hsize_t             block[2];  /* Block sizes */
    int                 fillval;
    size_t              j;
#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "    Scaleoffset is not enabled.";
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    TESTING("    scaleoffset int with fill value (setup)");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    datatype = H5Tcopy(H5T_NATIVE_INT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space for the dataset */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Set fill value */
    fillval = 10000;
    if(H5Pset_fill_value(dc, H5T_NATIVE_INT, &fillval) < 0) goto error;

    /* Set up to use scaleoffset filter, let library calculate minbits */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_INT,H5Z_SO_INT_MINBITS_DEFAULT) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_INT_NAME_2, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the memory data space */
    if((mspace = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Select hyperslab for data to write, using 1x5 blocks,
     * (1,1) stride and (1,1) count starting at the position (0,0).
     */
    start[0]  = 0; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = 1; block[1]  = 5;
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start,
                           stride, count, block) < 0) goto error;

    /* Initialize data of hyperslab */
    for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[0][j] = (int)HDrandom() % 10000;

        /* even-numbered values are negtive */
        if((j+1)%2 == 0)
            orig_data[0][j] = -orig_data[0][j];
    }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int with fill value (write)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* only data in the hyperslab will be written, other value should be fill value */
    if(H5Dwrite(dataset, H5T_NATIVE_INT, mspace, mspace, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset int with fill value (read)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_INT, mspace, mspace, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(j=0; j<(size_t)size[1]; j++) {
        if(new_data[0][j] != orig_data[0][j]) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %lu,%lu\n", (unsigned long)0, (unsigned long)j);
            goto error;
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_float
 *
 * Purpose:     Tests the float datatype for scaleoffset filter, with fill
 *              value undefined, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, Apr. 20th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_float(hid_t file)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    float               orig_data[2][5];
    float               new_data[2][5];
    size_t              i, j;
#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "    Scaleoffset is not enabled.";
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    TESTING("    scaleoffset float without fill value, D-scaling (setup)");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    datatype = H5Tcopy(H5T_NATIVE_FLOAT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Fill value undefined */
    if(H5Pset_fill_value(dc, datatype, NULL) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 3,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,3) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_FLOAT_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = (float)((HDrandom() % 100000) / (float)1000.0);

        /* even-numbered values are negtive */
        if((i*size[1]+j+1)%2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float without fill value, D-scaling (write)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float without fill value, D-scaling (read)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(HDfabs(new_data[i][j]-orig_data[i][j]) > HDpow(10.0, -3.0)) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_float_2
 *
 * Purpose:     Tests the float datatype for scaleoffset filter, with fill
 *              value set, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Wednesday, Apr. 20th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_float_2(hid_t file)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t               dataset, datatype, space, mspace, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    float               orig_data[2][5];
    float               new_data[2][5];
    float               fillval;
    hsize_t             start[2];  /* Start of hyperslab */
    hsize_t             stride[2]; /* Stride of hyperslab */
    hsize_t             count[2];  /* Block count */
    hsize_t             block[2];  /* Block sizes */
    size_t              j;
#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "    Scaleoffset is not enabled.";
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    TESTING("    scaleoffset float with fill value, D-scaling (setup)");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    datatype = H5Tcopy(H5T_NATIVE_FLOAT);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space for the dataset */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Set fill value */
    fillval = 10000.0;
    if(H5Pset_fill_value(dc, H5T_NATIVE_FLOAT, &fillval) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 3,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,3) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_FLOAT_NAME_2, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the memory data space */
    if((mspace = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Select hyperslab for data to write, using 1x5 blocks,
     * (1,1) stride and (1,1) count starting at the position (0,0).
     */
    start[0]  = 0; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = 1; block[1]  = 5;
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start,
                           stride, count, block) < 0) goto error;

    /* Initialize data of hyperslab */
    for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[0][j] = (float)((HDrandom() % 100000) / (float)1000.0);

        /* even-numbered values are negtive */
        if((j+1)%2 == 0)
            orig_data[0][j] = -orig_data[0][j];
    }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float with fill value, D-scaling (write)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* only data in the hyperslab will be written, other value should be fill value */
    if(H5Dwrite(dataset, H5T_NATIVE_FLOAT, mspace, mspace, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset float with fill value, D-scaling (read)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_FLOAT, mspace, mspace, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(j=0; j<(size_t)size[1]; j++) {
        if(HDfabs(new_data[0][j]-orig_data[0][j]) > HDpow(10.0, -3.0)) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %lu,%lu\n", (unsigned long)0, (unsigned long)j);
            goto error;
        }
    }
    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_double
 *
 * Purpose:     Tests the double datatype for scaleoffset filter, with fill
 *              value undefined, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Monday, Apr. 25th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_double(hid_t file)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t               dataset, datatype, space, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    double              orig_data[2][5];
    double              new_data[2][5];
    size_t              i, j;
#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "    Scaleoffset is not enabled.";
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    TESTING("    scaleoffset double without fill value, D-scaling (setup)");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    datatype = H5Tcopy(H5T_NATIVE_DOUBLE);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Fill value undefined */
    if(H5Pset_fill_value(dc, datatype, NULL) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 7,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,7) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_DOUBLE_NAME, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Initialize data */
    for(i= 0;i< (size_t)size[0]; i++)
      for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[i][j] = (HDrandom() % 10000000) / 10000000.0;

        /* even-numbered values are negtive */
        if((i*size[1]+j+1)%2 == 0)
            orig_data[i][j] = -orig_data[i][j];
      }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double without fill value, D-scaling (write)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double without fill value, D-scaling (read)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)size[0]; i++) {
        for(j=0; j<(size_t)size[1]; j++) {
            if(HDfabs(new_data[i][j]-orig_data[i][j]) > HDpow(10.0, -7.0)) {
                H5_FAILED();
                printf("    Read different values than written.\n");
                printf("    At index %lu,%lu\n", (unsigned long)i, (unsigned long)j);
                goto error;
            }
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:    test_scaleoffset_double_2
 *
 * Purpose:     Tests the double datatype for scaleoffset filter, with fill
 *              value set, using variable-minimum-bits method
 *
 * Return:      Success:        0
 *
 *              Failure:        -1
 *
 * Programmer:  Xiaowen Wu
 *              Monday, Apr. 25th, 2005
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_scaleoffset_double_2(hid_t file)
{
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    hid_t               dataset, datatype, space, mspace, dc;
    const hsize_t       size[2] = {2, 5};
    const hsize_t       chunk_size[2] = {2,5};
    double              orig_data[2][5];
    double              new_data[2][5];
    double              fillval;
    hsize_t             start[2];  /* Start of hyperslab */
    hsize_t             stride[2]; /* Stride of hyperslab */
    hsize_t             count[2];  /* Block count */
    hsize_t             block[2];  /* Block sizes */
    size_t              j;
#else /* H5_HAVE_FILTER_SCALEOFFSET */
    const char          *not_supported= "    Scaleoffset is not enabled.";
#endif /* H5_HAVE_FILTER_SCALEOFFSET */

    TESTING("    scaleoffset double with fill value, D-scaling (setup)");
#ifdef H5_HAVE_FILTER_SCALEOFFSET
    datatype = H5Tcopy(H5T_NATIVE_DOUBLE);

    /* Set order of dataset datatype */
    if(H5Tset_order(datatype, H5T_ORDER_BE) < 0) goto error;

    /* Create the data space for the dataset */
    if((space = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Create the dataset property list  */
    if((dc = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;

    /* Set fill value */
    fillval = 10000.0;
    if(H5Pset_fill_value(dc, H5T_NATIVE_DOUBLE, &fillval) < 0) goto error;

    /* Set up to use scaleoffset filter, decimal scale factor is 7,
     * use variable-minimum-bits method
     */
    if(H5Pset_chunk(dc, 2, chunk_size) < 0) goto error;
    if(H5Pset_scaleoffset(dc, H5Z_SO_FLOAT_DSCALE,7) < 0) goto error;

    /* Create the dataset */
    if((dataset = H5Dcreate2(file, DSET_SCALEOFFSET_DOUBLE_NAME_2, datatype,
                             space, H5P_DEFAULT, dc, H5P_DEFAULT)) < 0) goto error;

    /* Create the memory data space */
    if((mspace = H5Screate_simple(2, size, NULL)) < 0) goto error;

    /* Select hyperslab for data to write, using 1x5 blocks,
     * (1,1) stride and (1,1) count starting at the position (0,0).
     */
    start[0]  = 0; start[1]  = 0;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 1; count[1]  = 1;
    block[0]  = 1; block[1]  = 5;
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start,
                           stride, count, block) < 0) goto error;

    /* Initialize data of hyperslab */
    for(j = 0; j < (size_t)size[1]; j++) {
        orig_data[0][j] = (HDrandom() % 10000000) / 10000000.0;

        /* even-numbered values are negtive */
        if((j+1)%2 == 0)
            orig_data[0][j] = -orig_data[0][j];
    }

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 1: Test scaleoffset by setting up a chunked dataset and writing
     * to it.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double with fill value, D-scaling (write)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* only data in the hyperslab will be written, other value should be fill value */
    if(H5Dwrite(dataset, H5T_NATIVE_DOUBLE, mspace, mspace, H5P_DEFAULT,
                 orig_data) < 0) goto error;
    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif

    /*----------------------------------------------------------------------
     * STEP 2: Try to read the data we just wrote.
     *----------------------------------------------------------------------
     */
    TESTING("    scaleoffset double with fill value, D-scaling (read)");

#ifdef H5_HAVE_FILTER_SCALEOFFSET
    /* Read the dataset back */
    if(H5Dread(dataset, H5T_NATIVE_DOUBLE, mspace, mspace, H5P_DEFAULT,
                new_data) < 0) goto error;

    /* Check that the values read are the same as the values written */
    for(j=0; j<(size_t)size[1]; j++) {
        if(HDfabs(new_data[0][j]-orig_data[0][j]) > HDpow(10.0, -7.0)) {
            H5_FAILED();
            printf("    Read different values than written.\n");
            printf("    At index %lu,%lu\n", (unsigned long)0, (unsigned long)j);
            goto error;
        }
    }

    /*----------------------------------------------------------------------
     * Cleanup
     *----------------------------------------------------------------------
     */
    if(H5Tclose(datatype) < 0) goto error;
    if(H5Pclose(dc) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    PASSED();
#else
    SKIPPED();
    puts(not_supported);
#endif
    return 0;
error:
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_multiopen
 *
 * Purpose:	Tests that a bug no longer exists.  If a dataset is opened
 *		twice and one of the handles is used to extend the dataset,
 *		then the other handle should return the new size when
 *		queried.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Tuesday, June  9, 1998
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_multiopen (hid_t file)
{
    hid_t		dcpl = -1, space = -1, dset1 = -1, dset2 = -1;
    hsize_t		cur_size[1] = {10};
    static hsize_t	max_size[1] = {H5S_UNLIMITED};
    hsize_t		tmp_size[1];

    TESTING("multi-open with extending");

    /* Create the dataset and open it twice */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 1, cur_size) < 0) goto error;
    if((space = H5Screate_simple(1, cur_size, max_size)) < 0) goto error;
    if((dset1 = H5Dcreate2(file, "multiopen", H5T_NATIVE_INT, space, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;
    if((dset2 = H5Dopen2(dset1, ".", H5P_DEFAULT)) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;

    /* Extend with the first handle */
    cur_size[0] = 20;
    if(H5Dset_extent(dset1, cur_size) < 0) goto error;

    /* Get the size from the second handle */
    if((space = H5Dget_space(dset2)) < 0) goto error;
    if(H5Sget_simple_extent_dims(space, tmp_size, NULL) < 0) goto error;
    if(cur_size[0] != tmp_size[0]) {
	H5_FAILED();
	printf("    Got %d instead of %d!\n", (int)tmp_size[0], (int)cur_size[0]);
	goto error;
    } /* end if */

    if(H5Dclose(dset1) < 0) goto error;
    if(H5Dclose(dset2) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Pclose(dcpl) < 0) goto error;

    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Dclose(dset1);
	H5Dclose(dset2);
	H5Sclose(space);
	H5Pclose(dcpl);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function:	test_types
 *
 * Purpose:	Make some datasets with various types so we can test h5ls.
 *
 * Return:	Success:	0
 *
 *		Failure:	-1
 *
 * Programmer:	Robb Matzke
 *              Monday, June  7, 1999
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_types(hid_t file)
{
    hid_t		grp=-1, type=-1, space=-1, dset=-1;
    size_t		i;
    hsize_t		nelmts;
    unsigned char	buf[32];

    TESTING("various datatypes");
    if((grp = H5Gcreate2(file, "typetests", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0) goto error;

    /* bitfield_1 */
    nelmts = sizeof(buf);
    if((type=H5Tcopy(H5T_STD_B8LE)) < 0 ||
	(space=H5Screate_simple(1, &nelmts, NULL)) < 0 ||
	(dset=H5Dcreate2(grp, "bitfield_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    for(i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
	goto error;

    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* bitfield_2 */
    nelmts = sizeof(buf)/2;
    if((type=H5Tcopy(H5T_STD_B16LE)) < 0 ||
	(space=H5Screate_simple(1, &nelmts, NULL)) < 0 ||
	(dset=H5Dcreate2(grp, "bitfield_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    for(i=0; i<sizeof buf; i++) buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
	goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* opaque_1 */
    nelmts = sizeof(buf);
    if((type = H5Tcreate(H5T_OPAQUE, (size_t)1)) < 0 ||
            H5Tset_tag(type, "testing 1-byte opaque type") < 0 ||
            (space = H5Screate_simple(1, &nelmts, NULL)) < 0 ||
            (dset = H5Dcreate2(grp, "opaque_1", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    for(i = 0; i < sizeof buf; i++)
        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* opaque_2 */
    nelmts = sizeof(buf)/4;
    if((type = H5Tcreate(H5T_OPAQUE, (size_t)4)) < 0 ||
            H5Tset_tag(type, "testing 4-byte opaque type") < 0 ||
            (space = H5Screate_simple(1, &nelmts, NULL)) < 0 ||
            (dset = H5Dcreate2(grp, "opaque_2", type, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
	goto error;
    for(i = 0; i < sizeof buf; i++)
        buf[i] = (unsigned char)0xff ^ (unsigned char)i;
    if(H5Dwrite(dset, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0) goto error;
    if(H5Sclose(space) < 0) goto error;
    if(H5Tclose(type) < 0) goto error;
    if(H5Dclose(dset) < 0) goto error;

    /* Cleanup */
    if(H5Gclose(grp) < 0) goto error;
    PASSED();
    return 0;

 error:
    H5E_BEGIN_TRY {
	H5Gclose(grp);
	H5Tclose(type);
	H5Sclose(space);
	H5Dclose(dset);
    } H5E_END_TRY;
    return -1;
}

/* This message derives from H5Z */
const H5Z_class2_t H5Z_CAN_APPLY_TEST[1] = {{
    H5Z_CLASS_T_VERS,
    H5Z_FILTER_CAN_APPLY_TEST,	/* Filter id number		*/
    1, 1,
    "can_apply_test",		/* Filter name for debugging	*/
    can_apply_bogus,            /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	test_can_apply
 *
 * Purpose:	Tests library behavior when filter indicates it can't
 *              apply to certain combinations of creation parameters.
 *              The filter is mandate.  If the CAN_APPLY callback function
 *              indicates wrong datatype, the dataset creation should fail.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Friday, April  5, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_can_apply(hid_t file)
{
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    size_t      i,j;            /* Local index variables */

    TESTING("dataset filter 'can apply' callback");

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Zregister (H5Z_CAN_APPLY_TEST) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't register 'can apply' filter\n",__LINE__);
        goto error;
    }
    /* The filter is mandate. */
    if(H5Pset_filter(dcpl, H5Z_FILTER_CAN_APPLY_TEST, 0, (size_t)0, NULL) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set bogus filter\n",__LINE__);
        goto error;
    }

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    /* (Should fail because the 'can apply' function should indicate inappropriate 
     * combination.  And the filter is mandate.) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't have created dataset!\n",__LINE__);
        H5Dclose(dsid);
        goto error;
    } /* end if */

    /* (Should fail because the 'can apply' function should fail) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME, H5T_NATIVE_FLOAT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't have created dataset!\n",__LINE__);
        H5Dclose(dsid);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        printf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        printf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size=H5Dget_storage_size(dsid))==0) {
        H5_FAILED();
        printf("    Line %d: Error querying dataset size\n",__LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_INT)*dims[0]*dims[1])!=dset_size) {
        H5_FAILED();
        printf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)dims[0]; i++) {
	for(j=0; j<(size_t)dims[1]; j++) {
	    if(points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Line %d: Read different values than written.\n",__LINE__);
		printf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
		printf("    At original: %d\n",points[i][j]);
		printf("    At returned: %d\n",check[i][j]);
		goto error;
	    } /* end if */
	} /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
    return 0;

error:
    return -1;
} /* end test_can_apply() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_CAN_APPLY_TEST2[1] = {{
    H5Z_CLASS_T_VERS,
    H5Z_FILTER_CAN_APPLY_TEST2,	/* Filter id number		*/
    1, 1,
    "can_apply_test",		/* Filter name for debugging	*/
    can_apply_bogus,            /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_bogus3,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	test_can_apply2
 *
 * Purpose:	Tests library behavior when an optional filter indicates 
 *              it can't apply to certain combinations of creation 
 *              parameters.  The filter function FILTER_BOGUS3 does nothing
 *              than returning a failure.  Because the filter is optional, 
 *              the library skips the filter even though the CAN_APPLY_BOGUS
 *              indicates the datatype DOUBLE can't apply to the dataset.  
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              4 August 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_can_apply2(hid_t file)
{
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    size_t      i,j;            /* Local index variables */

    TESTING("dataset filter 'can apply' callback second");

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Zregister (H5Z_CAN_APPLY_TEST2) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't register 'can apply' filter\n",__LINE__);
        goto error;
    }
    /* The filter is optional. */
    if(H5Pset_filter(dcpl, H5Z_FILTER_CAN_APPLY_TEST2, H5Z_FLAG_OPTIONAL, (size_t)0, NULL) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set bogus filter\n",__LINE__);
        goto error;
    }

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_CAN_APPLY_NAME2, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        printf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Flush the file (to clear the cache) */
    if(H5Fflush(file, H5F_SCOPE_GLOBAL) < 0) {
        H5_FAILED();
        printf("    Line %d: Error flushing file\n",__LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size=H5Dget_storage_size(dsid))==0) {
        H5_FAILED();
        printf("    Line %d: Error querying dataset size\n",__LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_DOUBLE)*dims[0]*dims[1])!=dset_size) {
        H5_FAILED();
        printf("    Line %d: Incorrect dataset size: %lu\n",__LINE__,(unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the same as the values written */
    for(i=0; i<(size_t)dims[0]; i++) {
	for(j=0; j<(size_t)dims[1]; j++) {
	    if(points[i][j] != check[i][j]) {
		H5_FAILED();
		printf("    Line %d: Read different values than written.\n",__LINE__);
		printf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
		printf("    At original: %d\n",points[i][j]);
		printf("    At returned: %d\n",check[i][j]);
		goto error;
	    } /* end if */
	} /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
    return 0;

error:
    return -1;
} /* end test_can_apply2() */



/*-------------------------------------------------------------------------
 * Function:	test_can_apply_szip
 *
 * Purpose:	Tests library behavior when szip filter indicates it can't
 *              apply to certain combinations of creation parameters
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  7, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_can_apply_szip(hid_t
#ifndef H5_HAVE_FILTER_SZIP
UNUSED
#endif /* H5_HAVE_FILTER_SZIP */
file)
{
#ifdef H5_HAVE_FILTER_SZIP
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    unsigned szip_options_mask=H5_SZIP_NN_OPTION_MASK;
    unsigned szip_pixels_per_block;
    const hsize_t dims[2] = {500, 4096};        /* Dataspace dimensions */
    const hsize_t dims2[2] = {4, 2};            /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {250, 2048};  /* Chunk dimensions */
    const hsize_t chunk_dims2[2] = {2, 1};      /* Chunk dimensions */
    herr_t      ret;            /* Status value */
#endif /* H5_HAVE_FILTER_SZIP */

    TESTING("dataset szip filter 'can apply' callback");

#ifdef H5_HAVE_FILTER_SZIP

    if(h5_szip_can_encode() == 1) {
    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */

    /* Set (invalid at property set time) szip parameters */
    szip_pixels_per_block=3;
    H5E_BEGIN_TRY {
        ret=H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't be able to set szip filter\n",__LINE__);
        goto error;
    }

    /* Set (invalid at property set time) szip parameters */
    szip_pixels_per_block=512;
    H5E_BEGIN_TRY {
        ret=H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block);
    } H5E_END_TRY;
    if(ret>=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't be able to set szip filter\n",__LINE__);
        goto error;
    }

    /* Set (invalid at dataset creation time) szip parameters */
    szip_pixels_per_block=2;
    if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set szip filter\n",__LINE__);
        goto error;
    }

    /* Create new dataset */
    /* (Should succeed; according to the new algorithm, scanline should be reset
        to 2*128 satisfying 'maximum blocks per scanline' condition) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_SZIP_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid <=0) {
        H5_FAILED();
        printf("    Line %d: Should have created dataset!\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */

    /* Create another data space */
    if((sid = H5Screate_simple(2, dims2, NULL)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims2) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */

    /* Set (invalid at dataset creation time) szip parameters */
    szip_pixels_per_block=32;
    if(H5Pset_szip (dcpl, szip_options_mask, szip_pixels_per_block) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set szip filter\n",__LINE__);
        goto error;
    }

    /* Create new dataset */
    /* (Should fail because the 'can apply' filter should indicate inappropriate combination) */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(file, DSET_CAN_APPLY_SZIP_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't have created dataset!\n",__LINE__);
        H5Dclose(dsid);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
} else {
    SKIPPED();
    puts("    Szip encoding is not enabled.");
}
#else /* H5_HAVE_FILTER_SZIP */
    SKIPPED();
    puts("    Szip filter is not enabled.");
#endif /* H5_HAVE_FILTER_SZIP */
    return 0;

#ifdef H5_HAVE_FILTER_SZIP
error:
    return -1;
#endif /* H5_HAVE_FILTER_SZIP */
} /* end test_can_apply_szip() */


/* This message derives from H5Z */
const H5Z_class2_t H5Z_SET_LOCAL_TEST[1] = {{
    H5Z_CLASS_T_VERS,
    H5Z_FILTER_SET_LOCAL_TEST,	/* Filter id number		*/
    1, 1,
    "set_local_test",		/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    set_local_bogus2,           /* The "set local" callback     */
    filter_bogus2,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function:	test_set_local
 *
 * Purpose:	Tests library behavior for "set local" filter callback
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Monday, April  7, 2003
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_set_local(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       file;           /* File ID */
    hid_t       dsid;           /* Dataset ID */
    hid_t       sid;            /* Dataspace ID */
    hid_t       dcpl;           /* Dataspace creation property list ID */
    const hsize_t dims[2] = {DSET_DIM1, DSET_DIM2};         /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {2, 25};      /* Chunk dimensions */
    hsize_t     dset_size;      /* Dataset size */
    unsigned    cd_values[2]={BOGUS2_PARAM_1, BOGUS2_PARAM_2};   /* Parameters for Bogus2 filter */
    size_t      i,j;          /* Local index variables */
    double      n;          /* Local index variables */

    TESTING("dataset filter 'set local' callback");

    h5_fixname(FILENAME[5], fapl, filename, sizeof filename);

    /* Initialize the integer & floating-point dataset */
    n=1.0;
    for(i = 0; i < DSET_DIM1; i++)
	for(j = 0; j < DSET_DIM2; j++) {
	    points[i][j] = (int)n++;
	    points_dbl[i][j] = (double)1.5*n++;
	}

    /* Open file */
    if((file=H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open file\n",__LINE__);
	goto error;
    }

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dcpl\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set chunk sizes\n",__LINE__);
        goto error;
    } /* end if */
    if(H5Zregister (H5Z_SET_LOCAL_TEST) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't register 'set local' filter\n",__LINE__);
        goto error;
    }
    if(H5Pset_filter(dcpl, H5Z_FILTER_SET_LOCAL_TEST, 0, (size_t)BOGUS2_PERM_NPARMS, cd_values) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't set bogus2 filter\n",__LINE__);
        goto error;
    }

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataspace\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    if((dsid = H5Dcreate2(file, DSET_SET_LOCAL_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points) < 0) {
        H5_FAILED();
        printf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Create new dataset */
    /* (Shouldn't get modified by output filter) */
    if((dsid = H5Dcreate2(file, DSET_SET_LOCAL_NAME_2, H5T_NATIVE_DOUBLE, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't create dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Write data */
    if(H5Dwrite(dsid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, points_dbl) < 0) {
        H5_FAILED();
        printf("    Line %d: Error writing dataset data\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close dataspace */
    if(H5Sclose(sid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataspace\n", __LINE__);
        goto error;
    } /* end if */

    /* Close dataset creation property list */
    if(H5Pclose(dcpl) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dcpl\n", __LINE__);
        goto error;
    } /* end if */

    /* Close file (flushes & empties cache) */
    if(H5Fclose(file) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close file\n", __LINE__);
        goto error;
    } /* end if */

    /* Open file */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open file\n", __LINE__);
        goto error;
    }

    /* Re-open dataset */
    if((dsid = H5Dopen2(file, DSET_SET_LOCAL_NAME, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size = H5Dget_storage_size(dsid)) == 0) {
        H5_FAILED();
        printf("    Line %d: Error querying dataset size\n", __LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_INT) * dims[0] * dims[1]) != dset_size) {
        H5_FAILED();
        printf("    Line %d: Incorrect dataset size: %lu\n", __LINE__, (unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, check) < 0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n", __LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the modified version of what was written */
    for(i=0; i<dims[0]; i++) {
	for(j=0; j<dims[1]; j++) {
	    if((points[i][j]+(int)sizeof(int)) != check[i][j]) {
		H5_FAILED();
		printf("    Line %d: Read different values than written.\n",__LINE__);
		printf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
		printf("    At original: %d\n",points[i][j]);
		printf("    At returned: %d\n",check[i][j]);
		goto error;
	    } /* end if */
	} /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Re-open second dataset */
    if((dsid = H5Dopen2(file, DSET_SET_LOCAL_NAME_2, H5P_DEFAULT)) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't open dataset\n", __LINE__);
        goto error;
    } /* end if */

    /* Query the dataset's size on disk */
    if((dset_size = H5Dget_storage_size(dsid)) == 0) {
        H5_FAILED();
        printf("    Line %d: Error querying dataset size\n", __LINE__);
        goto error;
    } /* end if */

    /* Verify that the size indicates data is uncompressed */
    if((H5Tget_size(H5T_NATIVE_DOUBLE) * dims[0] * dims[1]) != dset_size) {
        H5_FAILED();
        printf("    Line %d: Incorrect dataset size: %lu\n", __LINE__, (unsigned long)dset_size);
        goto error;
    } /* end if */

    /* Read data */
    if(H5Dread(dsid, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, check_dbl) < 0) {
        H5_FAILED();
        printf("    Line %d: Error reading dataset data\n", __LINE__);
        goto error;
    } /* end if */

    /* Compare data */
    /* Check that the values read are the modified version of what was written */
    for(i=0; i<dims[0]; i++) {
	for(j=0; j<dims[1]; j++) {
	    /* If the difference between two values is greater than 0.001%, they're
             * considered not equal. */
            if(!DBL_REL_EQUAL(points_dbl[i][j],check_dbl[i][j],0.00001)) {
		H5_FAILED();
		printf("    Line %d: Read different values than written.\n",__LINE__);
		printf("    At index %lu,%lu\n", (unsigned long)(i), (unsigned long)(j));
		printf("    At original: %f\n",points_dbl[i][j]);
		printf("    At returned: %f\n",check_dbl[i][j]);
		goto error;
	    } /* end if */
	} /* end for */
    } /* end for */

    /* Close dataset */
    if(H5Dclose(dsid) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close dataset\n",__LINE__);
        goto error;
    } /* end if */

    /* Close file */
    if(H5Fclose(file) < 0) {
        H5_FAILED();
        printf("    Line %d: Can't close file\n",__LINE__);
        goto error;
    } /* end if */


    PASSED();
    return 0;

error:
    return -1;
} /* end test_set_local() */


/*-------------------------------------------------------------------------
 * Function:	test_compare_dcpl
 *
 * Purpose:	Verifies that if the same DCPL was used to create two
 *              datasets, the DCPLs retrieved from each dataset should
 *              compare equal.
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Quincey Koziol
 *              Wednesday, January  7, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_compare_dcpl(hid_t file)
{
    hid_t       dsid=(-1);      /* Dataset ID */
    hid_t       sid=(-1);       /* Dataspace ID */
    hid_t       dcpl=(-1);      /* Dataspace creation property list ID */
    hid_t       dcpl1=(-1),dcpl2=(-1);          /* Dataspace creation property list IDs from datasets */
    const hsize_t dims[2] = {500, 4096};        /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {250, 2048};  /* Chunk dimensions */

    TESTING("comparing dataset creation property lists");

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR

    /* Set gzip parameter (if available) */
#ifdef H5_HAVE_FILTER_DEFLATE
    if(H5Pset_deflate (dcpl, 9) < 0) TEST_ERROR
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Create first dataset */
    if((dsid = H5Dcreate2(file, DSET_COMPARE_DCPL_NAME, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get copy of dataset's dataset creation property list */
    if((dcpl1=H5Dget_create_plist(dsid)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid) < 0) TEST_ERROR

    /* Create second dataset */
    if((dsid = H5Dcreate2(file, DSET_COMPARE_DCPL_NAME_2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get copy of dataset's dataset creation property list */
    if((dcpl2=H5Dget_create_plist(dsid)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid) < 0) TEST_ERROR

    /* Close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Compare dataset creation property lists */
    if(H5Pequal(dcpl1,dcpl2)<=0) TEST_ERROR

    /* Close dataset creation property lists */
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Pclose(dcpl1) < 0) TEST_ERROR
    if(H5Pclose(dcpl2) < 0) TEST_ERROR


    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
    } H5E_END_TRY;
    return -1;
} /* end test_compare_dcpl() */


/*-------------------------------------------------------------------------
 * Function:	test_copy_dcpl
 *
 * Purpose:	Verifies whether the copy of dataset creation property
 *              list works.  It tests the DCPL for chunked layout with
 *              filter and for contiguous layout with external storage.
 *              (Please see #1608 in Bugzilla)
 *
 * Return:	Success:	0
 *		Failure:	-1
 *
 * Programmer:	Raymond Lu
 *              28 January 2010
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_copy_dcpl(hid_t file, hid_t fapl)
{
    hid_t       dsid1=(-1), dsid2=(-1);         /* Dataset ID */
    hid_t       new_dsid1=(-1), new_dsid2=(-1); /* Dataset ID */
    hid_t       sid=(-1);                       /* Dataspace ID */
    hid_t       dcpl=(-1);                      /* Dataset creation property list ID */
    hid_t       dcpl1=(-1),dcpl2=(-1);          /* Copies of creation property list IDs */
    hid_t       dcpl1_copy=(-1),dcpl2_copy=(-1);/* Copies of creation property list IDs */
    const hsize_t dims[2] = {500, 4096};        /* Dataspace dimensions */
    const hsize_t chunk_dims[2] = {250, 2048};  /* Chunk dimensions */
    char	filename[FILENAME_BUF_SIZE];
    hid_t       new_file=(-1);

    TESTING("copying dataset creation property lists");

    /* Create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) TEST_ERROR

    /* Create dcpl with special filter */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) TEST_ERROR
    if(H5Pset_fletcher32(dcpl) < 0) TEST_ERROR

    /* Create first dataset of chunking with filter */
    if((dsid1 = H5Dcreate2(file, DSET_COPY_DCPL_NAME_1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl,
        H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid1) < 0) TEST_ERROR

    /* Reopen the first dataset */
    if((dsid1 = H5Dopen2(file, DSET_COPY_DCPL_NAME_1, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get the copy of dataset's creation property list */
    if((dcpl1=H5Dget_create_plist(dsid1)) < 0) TEST_ERROR
    if((dcpl1_copy = H5Pcopy(dcpl1)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid1) < 0) TEST_ERROR

    /* Change the DCPL for contiguous layout with external storage.  The size of the reserved
     * space in the external file is the size of the dataset - 500*4096*sizeof(int).
     * There's no need to clean up the external file since the library doesn't create it
     * until the data is written to it. */
    if(H5Pset_layout(dcpl, H5D_CONTIGUOUS) < 0) TEST_ERROR
    if(H5Premove_filter(dcpl, H5Z_FILTER_FLETCHER32) < 0) TEST_ERROR
    if(H5Pset_external(dcpl, COPY_DCPL_EXTFILE_NAME, (off_t)0, (hsize_t)(500 * 4096 * sizeof(int))) < 0) TEST_ERROR

    /* Create second dataset of contiguous layout with external storage */
    if((dsid2 = H5Dcreate2(file, DSET_COPY_DCPL_NAME_2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl,
        H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid2) < 0) TEST_ERROR

    /* Reopen the second dataset */
    if((dsid2 = H5Dopen2(file, DSET_COPY_DCPL_NAME_2, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Get copy of dataset's dataset creation property list */
    if((dcpl2=H5Dget_create_plist(dsid2)) < 0) TEST_ERROR
    if((dcpl2_copy = H5Pcopy(dcpl2)) < 0) TEST_ERROR

    /* Close dataset */
    if(H5Dclose (dsid2) < 0) TEST_ERROR

    /* Create a second file and create 2 datasets with the copies of the DCPLs in the first
     * file.  Test whether the copies of DCPLs work. */
    h5_fixname(FILENAME[11], fapl, filename, sizeof filename);
    if((new_file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) < 0)
        TEST_ERROR

    if((new_dsid1 = H5Dcreate2(new_file, DSET_COPY_DCPL_NAME_1, H5T_NATIVE_INT, sid,
			H5P_DEFAULT, dcpl1_copy, H5P_DEFAULT)) < 0) TEST_ERROR

    if((new_dsid2 = H5Dcreate2(new_file, DSET_COPY_DCPL_NAME_2, H5T_NATIVE_INT, sid,
			H5P_DEFAULT, dcpl2_copy, H5P_DEFAULT)) < 0) TEST_ERROR

    /* Close dataspace */
    if(H5Sclose(sid) < 0) TEST_ERROR

    /* Close datasets */
    if(H5Dclose (new_dsid1) < 0) TEST_ERROR
    if(H5Dclose (new_dsid2) < 0) TEST_ERROR

    /* Close the second file */
    if(H5Fclose (new_file) < 0) TEST_ERROR

    /* Close dataset creation property lists */
    if(H5Pclose(dcpl) < 0) TEST_ERROR
    if(H5Pclose(dcpl1) < 0) TEST_ERROR
    if(H5Pclose(dcpl2) < 0) TEST_ERROR
    if(H5Pclose(dcpl1_copy) < 0) TEST_ERROR
    if(H5Pclose(dcpl2_copy) < 0) TEST_ERROR

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(dsid1);
        H5Dclose(dsid2);
        H5Dclose(new_dsid1);
        H5Dclose(new_dsid2);
        H5Sclose(sid);
        H5Pclose(dcpl);
        H5Pclose(dcpl1);
        H5Pclose(dcpl2);
        H5Pclose(dcpl1_copy);
        H5Pclose(dcpl2_copy);
    } H5E_END_TRY;
    return -1;
} /* end test_copy_dcpl() */


/*-------------------------------------------------------------------------
 * Function: test_filter_delete
 *
 * Purpose: Tests deletion of filters from a dataset creation property list
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Pedro Vicente
 *              Monday, January 26, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filter_delete(hid_t file)
{
    H5Z_filter_t filtn;                 /* filter identification number */
    hid_t        dsid=-1;                  /* dataset ID */
    hid_t        sid=-1;                   /* dataspace ID */
    hid_t        dcpl=-1;                  /* dataset creation property list ID */
    hid_t        dcpl1=-1;                 /* dataset creation property list ID */
    hsize_t      dims[2]={20,20};       /* dataspace dimensions */
    hsize_t      chunk_dims[2]={10,10}; /* chunk dimensions */
    int          nfilters;              /* number of filters in DCPL */
    unsigned     flags;                 /* flags for filter */
    herr_t       ret;                   /* generic return value */
    int          i;

    TESTING("filter deletion");

#if defined H5_HAVE_FILTER_DEFLATE && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32
    /* create the data space */
    if((sid = H5Screate_simple(2, dims, NULL)) < 0) goto error;

    /* create dcpl  */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 2, chunk_dims) < 0) goto error;

    if(H5Pset_fletcher32 (dcpl) < 0) goto error;
    if(H5Pset_deflate (dcpl, 6) < 0) goto error;
    if(H5Pset_shuffle (dcpl) < 0) goto error;

    /* create a dataset */
    if((dsid = H5Dcreate2(file,"dsetdel", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) goto error;

    /* get copy of dataset's dataset creation property list */
    if((dcpl1=H5Dget_create_plist(dsid)) < 0) goto error;

   /*----------------------------------------------------------------------
    * delete the deflate filter
    *----------------------------------------------------------------------
    */
    /* delete the deflate filter */
    if(H5Premove_filter(dcpl1,H5Z_FILTER_DEFLATE) < 0) goto error;

    /* get information about filters */
    if((nfilters = H5Pget_nfilters(dcpl1)) < 0) goto error;

    /* check if filter was deleted */
    for(i=0; i<nfilters; i++) {
        filtn = H5Pget_filter2(dcpl1, (unsigned)i, NULL, NULL, NULL, (size_t)0, NULL, NULL);
        if(H5Z_FILTER_DEFLATE==filtn)
            goto error;
    }

    /* try to get the info for the deflate filter */
    H5E_BEGIN_TRY {
        ret = H5Pget_filter_by_id2(dcpl1, H5Z_FILTER_DEFLATE, &flags, NULL, NULL, (size_t)0, NULL, NULL);
    } H5E_END_TRY;
    if(ret >=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't have deleted filter!\n",__LINE__);
        goto error;
    } /* end if */

    /* try to delete the deflate filter again */
    H5E_BEGIN_TRY {
        ret=H5Premove_filter(dcpl1,H5Z_FILTER_DEFLATE);
    } H5E_END_TRY;
    if(ret >=0) {
        H5_FAILED();
        printf("    Line %d: Shouldn't have deleted filter!\n",__LINE__);
        goto error;
    } /* end if */

   /*----------------------------------------------------------------------
    * delete all filters
    *----------------------------------------------------------------------
    */
    /* delete all filters */
    if(H5Premove_filter(dcpl1,H5Z_FILTER_ALL) < 0) goto error;

    /* get information about filters */
    if((nfilters = H5Pget_nfilters(dcpl1)) < 0) goto error;

    /* check if filters were deleted */
    if(nfilters)goto error;

   /*----------------------------------------------------------------------
    * close
    *----------------------------------------------------------------------
    */

    /* clean up objects used for this test */
    if(H5Pclose (dcpl) < 0) goto error;
    if(H5Pclose (dcpl1) < 0) goto error;
    if(H5Dclose (dsid) < 0) goto error;
    if(H5Sclose (sid) < 0) goto error;

    PASSED();
#else
    SKIPPED();
#endif
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(dcpl1);
        H5Dclose(dsid);
        H5Sclose(sid);
    } H5E_END_TRY;
    return -1;
} /* end test_filter_delete() */



/*-------------------------------------------------------------------------
 * Function: auxread_fdata
 *
 * Purpose: reads a dataset "NAME" from FID
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Pedro Vicente
 *              Monday, March 8, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
auxread_fdata(hid_t fid, const char *name)
{
    hid_t     dset_id=-1;           /* dataset ID */
    hid_t     dcpl_id=-1;           /* dataset creation property list ID */
    hid_t     space_id=-1;          /* space ID */
    hid_t     ftype_id=-1;          /* file data type ID */
    hid_t     mtype_id=-1;          /* memory data type ID */
    size_t    msize;             /* memory size of memory type */
    void      *buf=NULL;         /* data buffer */
    hsize_t   nelmts;            /* number of elements in dataset */
    int       rank;              /* rank of dataset */
    hsize_t   dims[H5S_MAX_RANK];/* dimensions of dataset */
    int       i;

    if((dset_id = H5Dopen2(fid, name, H5P_DEFAULT)) < 0)
        goto error;
    if((space_id = H5Dget_space(dset_id)) < 0)
        goto error;
    if((ftype_id = H5Dget_type(dset_id)) < 0)
        goto error;
    if((dcpl_id = H5Dget_create_plist(dset_id)) < 0)
        goto error;
    if((rank = H5Sget_simple_extent_ndims(space_id)) < 0)
        goto error;
    HDmemset(dims, 0, sizeof dims);
    if(H5Sget_simple_extent_dims(space_id, dims, NULL) < 0)
        goto error;
    nelmts = 1;
    for(i = 0; i < rank; i++)
        nelmts *= dims[i];
    if((mtype_id = H5Tget_native_type(ftype_id, H5T_DIR_DEFAULT)) < 0)
        goto error;
    if((msize = H5Tget_size(mtype_id)) == 0)
        goto error;

    if(nelmts) {
        buf = (void *)HDmalloc((size_t)(nelmts * msize));
        if(buf == NULL) {
            printf( "cannot read into memory\n" );
            goto error;
        }
        if(H5Dread(dset_id, mtype_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf) < 0)
            goto error;
    }

    if(H5Pclose(dcpl_id) < 0)
        goto error;
    if(H5Sclose(space_id) < 0)
        goto error;
    if(H5Dclose(dset_id) < 0)
        goto error;
    if(buf)
        free(buf);

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl_id);
        H5Sclose(space_id);
        H5Dclose(dset_id);
        H5Tclose(ftype_id);
        H5Tclose(mtype_id);
        if(buf)
            free(buf);
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * Function: test_filters_endianess
 *
 * Purpose: Reads/writes data with filters (big-endian/little-endian data)
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Pedro Vicente
 *              Monday, March 8, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_filters_endianess(void)
{
    hid_t     fid=-1;                   /* file ID */
    hid_t     dsid=-1;                  /* dataset ID */
    hid_t     sid=-1;                   /* dataspace ID */
    hid_t     dcpl=-1;                  /* dataset creation property list ID */
    char      *srcdir = getenv("srcdir"); /* the source directory */
    char      data_file[512]="";          /* buffer to hold name of existing file */

    TESTING("filters with big-endian/little-endian data");

#if defined H5_HAVE_FILTER_FLETCHER32
   /*-------------------------------------------------------------------------
    * step 1: open a file written on a little-endian machine
    *-------------------------------------------------------------------------
    */

    /* compose the name of the file to open, using the srcdir, if appropriate */
    HDstrcpy(data_file, "");
    if(srcdir) {
        HDstrcpy(data_file, srcdir);
        HDstrcat(data_file, "/");
    }
    HDstrcat(data_file, "test_filters_le.hdf5");

    /* open */
    if((fid = H5Fopen(data_file, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* read */
    if(auxread_fdata(fid,"dset") < 0) TEST_ERROR

    /* close */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

   /*-------------------------------------------------------------------------
    * step 2: open a file written on a big-endian machine
    *-------------------------------------------------------------------------
    */

    /* compose the name of the file to open, using the srcdir, if appropriate */
    HDstrcpy(data_file, "");
    if(srcdir) {
        HDstrcpy(data_file, srcdir);
        HDstrcat(data_file, "/");
    }
    HDstrcat(data_file, "test_filters_be.hdf5");

    /* open */
    if((fid = H5Fopen(data_file, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* read */
    if(auxread_fdata(fid,"dset") < 0) TEST_ERROR

    /* close */
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
#else
    SKIPPED();
#endif
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end test_filters_endianess() */


/*-------------------------------------------------------------------------
 * Function: test_zero_dims
 *
 * Purpose: Tests read/writes to zero-sized extendible datasets
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, July 27, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_zero_dims(hid_t file)
{
    hid_t       s = -1, d = -1, dcpl = -1;
    hsize_t     dsize = 0, dmax = H5S_UNLIMITED, csize = 5;
    herr_t      ret;

    TESTING("I/O on datasets with zero-sized dims");

    /* 
     * One-dimensional dataset 
     */
    if((s = H5Screate_simple(1, &dsize, &dmax)) < 0) FAIL_STACK_ERROR

    /* Try creating chunked dataset with undefined chunk dimensions */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_layout(dcpl, H5D_CHUNKED) < 0) FAIL_STACK_ERROR

    H5E_BEGIN_TRY {
        d = H5Dcreate2(file, ZERODIM_DATASET, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(d > 0) {
        H5Dclose(d);
        FAIL_PUTS_ERROR("created dataset with undefined chunk dimensions")
    } /* end if */

    /* Try creating chunked dataset with zero-sized chunk dimensions */
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl, 1, &dsize);
    } H5E_END_TRY;
    if(ret > 0)
        FAIL_PUTS_ERROR("set zero-sized chunk dimensions")

    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR

    /* Create the zero-sized extendible dataset */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR
    if(H5Pset_chunk(dcpl, 1, &csize) < 0) FAIL_STACK_ERROR
    if((d = H5Dcreate2(file, ZERODIM_DATASET, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Just a no-op */
    if(H5Dwrite(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, (void*)911) < 0) FAIL_STACK_ERROR

    if(H5Dclose(d) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Sclose(s) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(d);
        H5Sclose(s);
    } H5E_END_TRY;
    return -1;
} /* end test_zero_dims() */


/*-------------------------------------------------------------------------
 * Function: test_missing_chunk
 *
 * Purpose: Tests that reads from chunked dataset with undefined fill value and
 *              not all chunks written don't overwrite data in user's buffer
 *              for missing chunks.
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *              Tuesday, August 25, 2004
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_missing_chunk(hid_t file)
{
    hid_t       s = -1, d = -1, dcpl = -1;
    hsize_t	hs_start[1];
    hsize_t	hs_stride[1],
                hs_count[1],
                hs_block[1];
    int         wdata[MISSING_CHUNK_DIM],
                rdata[MISSING_CHUNK_DIM];
    hsize_t     dsize=100, dmax=H5S_UNLIMITED;
    hsize_t	csize=5;
    size_t      u;

    TESTING("Read dataset with unwritten chunk & undefined fill value");

    /* Initialize data for 1-D dataset */
    for(u = 0; u < MISSING_CHUNK_DIM; u++) {
        wdata[u] = (int)u;
        rdata[u] = 911;
    } /* end for */

    /* Create dataspace */
    if((s = H5Screate_simple(1, &dsize, &dmax)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set to chunked */
    if(H5Pset_chunk(dcpl, 1, &csize) < 0) TEST_ERROR;

    /* Undefine fill value */
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, NULL) < 0) TEST_ERROR;

    /* Create dataset */
    if((d = H5Dcreate2(file, MISSING_CHUNK_DATASET, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Select elements in every other chunk for 1-D dataset */
    hs_start[0]=0;
    hs_stride[0]=10;
    hs_count[0]=10;
    hs_block[0]=5;
    if(H5Sselect_hyperslab(s, H5S_SELECT_SET, hs_start, hs_stride, hs_count,
			    hs_block) < 0) TEST_ERROR;

    /* Write selected data */
    if(H5Dwrite(d, H5T_NATIVE_INT, s, s, H5P_DEFAULT, wdata) < 0) TEST_ERROR;

    /* Read all data */
    if(H5Dread(d, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata) < 0) TEST_ERROR;

    /* Validata values read */
    for(u=0; u<MISSING_CHUNK_DIM; u++) {
        if((u%10)>=5) {
            if(rdata[u]!=911) {
                printf("    Line %d: Incorrect value, rdata[%u]=%d\n",__LINE__,(unsigned)u,rdata[u]);
                TEST_ERROR;
            } /* end if */
        } /* end if */
        else {
            if(rdata[u]!=wdata[u]) {
                printf("    Line %d: Incorrect value, wdata[%u]=%d, rdata[%u]=%d\n",__LINE__,(unsigned)u,wdata[u],(unsigned)u,rdata[u]);
                TEST_ERROR;
            } /* end if */
        } /* end else */
    } /* end for */

    /* Close everything */
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(d);
        H5Sclose(s);
    } H5E_END_TRY;
    return -1;
} /* end test_missing_chunk() */


/*-------------------------------------------------------------------------
 * Function: test_random_chunks
 *
 * Purpose: Tests that write/read on randomly selected chunks in 2 datasets.
 *              One dataset has fixed dimensions, and the other has unlimited
 *              dimensions which are extended before write/read operations.
 *
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Christian Chilan
 *             Monday, March 26, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_random_chunks(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       s=-1, m=-1, d=-1, dcpl=-1, file=-1;
    int         wbuf[NPOINTS],
                rbuf[NPOINTS],
                check2[20][20];
    hsize_t     coord[NPOINTS][2];
    hsize_t     dsize[2]={100,100}, dmax[2]={H5S_UNLIMITED, H5S_UNLIMITED}, csize[2]={10,10}, nsize[2]={200,200};
    hsize_t     msize[1]={NPOINTS};
    const char  dname[]="dataset";
    int         chunk_row, chunk_col;
    size_t      i, j;


    TESTING("Write/read on randomly selected chunks");

    assert(NPOINTS < 100);

    h5_fixname(FILENAME[6], fapl, filename, sizeof filename);

    /* Create file for first test */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create dataspace */
    if((s = H5Screate_simple(2, dsize, NULL)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set chunked layout */
    if(H5Pset_chunk(dcpl, 2, csize) < 0) TEST_ERROR;

    /* Set early allocation time */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR;

    /* Create dataset */
    if((d = H5Dcreate2(file, dname, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Initialization of check array for repeated coordinates */
    for(i=0; i<dsize[0]/csize[0]; i++)
        for(j=0; j<dsize[1]/csize[1]; j++)
            check2[i][j] = 0;

    /* Generate random point coordinates. Only one point is selected per chunk */
    for(i=0; i<NPOINTS; i++){
        do {
            chunk_row = (int)HDrandom () % (int)(dsize[0]/csize[0]);
            chunk_col = (int)HDrandom () % (int)(dsize[1]/csize[1]);
        } while (check2[chunk_row][chunk_col]);

        wbuf[i] = check2[chunk_row][chunk_col] = chunk_row+chunk_col+1;
        coord[i][0] = (hsize_t)chunk_row * csize[0];
        coord[i][1] = (hsize_t)chunk_col * csize[1];
    }

    /* Create dataspace for write buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for writing */
    if(H5Sselect_elements(s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Write into dataset */
    if(H5Dwrite(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

    /* Close resources*/
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    /* Open file again */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Open dataset */
    if((d = H5Dopen2(file, dname, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get dataset dataspace */
    if((s = H5Dget_space(d)) < 0) TEST_ERROR;

    /* Create dataspace for read buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for reading */
    if(H5Sselect_elements (s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Read from dataset */
    if(H5Dread(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

    /* Verify that written and read data are the same */
    for(i = 0; i < NPOINTS; i++)
        if(rbuf[i] != wbuf[i]){
            printf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",__LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
            printf("             coord[%u] = {%lu, %lu}\n", (unsigned)i, (unsigned long)coord[i][0], (unsigned long)coord[i][1]);
            TEST_ERROR;
        } /* end if */

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;


    /* Create file for second test */
    if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) TEST_ERROR;

    /* Create dataspace with unlimited maximum dimensions */
    if((s = H5Screate_simple(2, dsize, dmax)) < 0) TEST_ERROR;

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) TEST_ERROR;

    /* Set chunked layout */
    if(H5Pset_chunk(dcpl, 2, csize) < 0) TEST_ERROR;

    /* Set allocation time to early */
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0) TEST_ERROR;

    /* Create dataset */
    if((d = H5Dcreate2(file, dname, H5T_NATIVE_INT, s, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Extend both dimensions of the dataset */
    if(H5Dset_extent(d, nsize) < 0) TEST_ERROR;

    /* Reset the dataset dataspace to new dimensions */
    if(H5Sset_extent_simple(s, 2, nsize, dmax) < 0) TEST_ERROR;

    /* Initialize check buffer for repeated coordinates */
    for(i = 0; i < nsize[0]/csize[0]; i++)
        for(j = 0; j < nsize[1] / csize[1]; j++)
            check2[i][j] = 0;

    /* Generate random point coordinates. Only one point is selected per chunk */
    for(i = 0; i < NPOINTS; i++){
        do {
            chunk_row = (int)HDrandom() % (int)(nsize[0] / csize[0]);
            chunk_col = (int)HDrandom() % (int)(nsize[1] / csize[1]);
        } while (check2[chunk_row][chunk_col]);

        wbuf[i] = check2[chunk_row][chunk_col] = chunk_row + chunk_col + 1;
        coord[i][0] = (hsize_t)chunk_row * csize[0];
        coord[i][1] = (hsize_t)chunk_col * csize[1];
    }

    /* Create dataspace for write buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for writing */
    if(H5Sselect_elements(s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Write into dataset */
    if(H5Dwrite(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, wbuf) < 0) TEST_ERROR;

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Pclose(dcpl) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    /* Open file again */
    if((file = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) TEST_ERROR;

    /* Open dataset */
    if((d = H5Dopen2(file, dname, H5P_DEFAULT)) < 0) TEST_ERROR;

    /* Get dataset dataspace */
    if((s = H5Dget_space(d)) < 0) TEST_ERROR;

    /* Create dataspace for read buffer */
    if((m = H5Screate_simple(1, msize, NULL)) < 0) TEST_ERROR;

    /* Select the random points for reading */
    if(H5Sselect_elements (s, H5S_SELECT_SET, (size_t)NPOINTS, (const hsize_t *)coord) < 0) TEST_ERROR;

    /* Read from dataset */
    if(H5Dread(d, H5T_NATIVE_INT, m, s, H5P_DEFAULT, rbuf) < 0) TEST_ERROR;

    /* Verify that written and read data are the same */
    for(i = 0; i < NPOINTS; i++)
        if(rbuf[i] != wbuf[i]){
            printf("    Line %d: Incorrect value, wbuf[%u]=%d, rbuf[%u]=%d\n",__LINE__,(unsigned)i,wbuf[i],(unsigned)i,rbuf[i]);
                TEST_ERROR;
        } /* end if */

    /* Close resources */
    if(H5Sclose(s) < 0) TEST_ERROR;
    if(H5Sclose(m) < 0) TEST_ERROR;
    if(H5Dclose(d) < 0) TEST_ERROR;
    if(H5Fclose(file) < 0) TEST_ERROR;

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Sclose(s);
        H5Sclose(m);
        H5Dclose(d);
        H5Fclose(file);
    } H5E_END_TRY;
    return -1;
} /* end test_random_chunks() */

#ifndef H5_NO_DEPRECATED_SYMBOLS
/* Empty can_apply and set_local callbacks */
static htri_t
can_apply_deprec(hid_t UNUSED dcpl_id, hid_t UNUSED type_id, hid_t UNUSED space_id)
{
    return 1;
}

static herr_t
set_local_deprec(hid_t UNUSED dcpl_id, hid_t UNUSED type_id, hid_t UNUSED space_id)
{
    return(SUCCEED);
}

/* Old style H5Z_class_t, essentially a copy of the "bogus" filter */
const H5Z_class1_t H5Z_DEPREC[1] = {{
    H5Z_FILTER_DEPREC,		/* Filter id number		*/
    "deprec",			/* Filter name for debugging	*/
    can_apply_deprec,           /* The "can apply" callback     */
    set_local_deprec,           /* The "set local" callback     */
    filter_bogus,		/* The actual filter function	*/
}};


/*-------------------------------------------------------------------------
 * Function: test_deprec
 *
 * Purpose: Tests deprecated API symbols
 *
 * Return: Success: 0
 *  Failure: -1
 *
 * Programmer: Quincey Koziol
 *             Monday, October 8, 2007
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_deprec(hid_t file)
{
    hid_t	dataset, space, small_space, create_parms, dcpl;
    hsize_t	dims[2], small_dims[2];
    hsize_t     deprec_size;
    herr_t	status;
    hsize_t	csize[2];

    TESTING("deprecated API routines");

    /* Create the data space */
    dims[0] = 256;
    dims[1] = 512;
    space = H5Screate_simple(2, dims, NULL);
    assert(space>=0);

    /* Create a small data space for compact dataset */
    small_dims[0] = 16;
    small_dims[1] = 8;
    small_space = H5Screate_simple(2, small_dims, NULL);
    assert(space>=0);

    /*
     * Create a dataset using the default dataset creation properties.	We're
     * not sure what they are, so we won't check.
     */
    if((dataset = H5Dcreate1(file, DSET_DEPREC_NAME, H5T_NATIVE_DOUBLE, space, H5P_DEFAULT)) < 0) goto error;

    /* Close the dataset */
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Try creating a dataset that already exists.  This should fail since a
     * dataset can only be created once.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
	dataset = H5Dcreate1(file, DSET_DEFAULT_NAME, H5T_NATIVE_DOUBLE, space,
			    H5P_DEFAULT);
    } H5E_END_TRY;
    if(dataset >= 0) {
	H5_FAILED();
	puts("    Library allowed overwrite of existing dataset.");
	goto error;
    }

    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if((dataset = H5Dopen1(file, DSET_DEPREC_NAME)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Try opening a non-existent dataset. This should fail since new datasets
     * cannot be created with this function.  Temporarily turn off error
     * reporting.
     */
    H5E_BEGIN_TRY {
	dataset = H5Dopen1(file, "does_not_exist");
    } H5E_END_TRY;
    if(dataset >= 0) {
	H5_FAILED();
	puts("    Opened a non-existent dataset.");
	goto error;
    }

    /*
     * Create a new dataset that uses chunked storage instead of the default
     * layout.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);

    /* Add the deflate filter, if available */
#if defined H5_HAVE_FILTER_DEFLATE
{
    H5Z_filter_t filtn;                 /* filter identification number */
    size_t cd_nelmts = 1;               /* Number of filter parameters */
    unsigned cd_value;                  /* Filter parameter */

    if(H5Pset_deflate(create_parms, 6) < 0) goto error;

    /* Check for the deflate filter */
    filtn = H5Pget_filter1(create_parms, (unsigned)0, NULL, &cd_nelmts, &cd_value, (size_t)0, NULL);
    if(H5Z_FILTER_DEFLATE != filtn)
        goto error;
    if(1 != cd_nelmts)
        goto error;
    if(6 != cd_value)
        goto error;

    /* Check for the deflate filter */
    if(H5Pget_filter_by_id1(create_parms, H5Z_FILTER_DEFLATE, NULL, &cd_nelmts, &cd_value, (size_t)0, NULL) < 0) goto error;
    if(1 != cd_nelmts)
        goto error;
    if(6 != cd_value)
        goto error;
}
#endif /* H5_HAVE_FILTER_DEFLATE */

    /* Attempt to create a dataset with invalid chunk sizes */
    csize[0] = dims[0]*2;
    csize[1] = dims[1]*2;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);
    H5E_BEGIN_TRY {
        dataset = H5Dcreate1(file, DSET_DEPREC_NAME_CHUNKED, H5T_NATIVE_DOUBLE, space,
			create_parms);
    } H5E_END_TRY;
    if(dataset >= 0) {
	H5_FAILED();
	puts("    Opened a dataset with incorrect chunking parameters.");
	goto error;
    }

    csize[0] = 5;
    csize[1] = 100;
    status = H5Pset_chunk(create_parms, 2, csize);
    assert(status >= 0);

    if((dataset = H5Dcreate1(file, DSET_DEPREC_NAME_CHUNKED, H5T_NATIVE_DOUBLE, space, create_parms)) < 0) goto error;
    H5Pclose(create_parms);

    /*
     * Close the chunked dataset.
     */
    if(H5Dclose(dataset) < 0) goto error;


    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if((dataset = H5Dopen1(file, DSET_DEPREC_NAME_CHUNKED)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Create a compact dataset, then close it.
     */
    create_parms = H5Pcreate(H5P_DATASET_CREATE);
    assert(create_parms >= 0);
    status = H5Pset_layout(create_parms, H5D_COMPACT);
    assert(status >= 0);
    status = H5Pset_alloc_time(create_parms, H5D_ALLOC_TIME_EARLY);
    assert(status >= 0);

    if((dataset = H5Dcreate1(file, DSET_DEPREC_NAME_COMPACT, H5T_NATIVE_DOUBLE, small_space, create_parms)) < 0) goto error;
    H5Pclose(create_parms);
    if(H5Dclose(dataset) < 0) goto error;

    /*
     * Open the dataset we created above and then close it.  This is how
     * existing datasets are accessed.
     */
    if((dataset = H5Dopen1(file, DSET_DEPREC_NAME_COMPACT)) < 0) goto error;
    if(H5Dclose(dataset) < 0) goto error;

    /* Test H5Zregister with deprecated H5Z_class1_t */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) goto error;
    if(H5Pset_chunk(dcpl, 2, csize) < 0) goto error;
    if(H5Zregister(H5Z_DEPREC) < 0) goto error;
    if(H5Pset_filter(dcpl, H5Z_FILTER_DEPREC, 0, (size_t)0, NULL) < 0) goto error;

    puts("");
    if(test_filter_internal(file,DSET_DEPREC_NAME_FILTER,dcpl,DISABLE_FLETCHER32,DATA_NOT_CORRUPTED,&deprec_size) < 0) goto error;

    if(H5Pclose(dcpl) < 0) goto error;

    return 0;

 error:
    return -1;
} /* end test_deprec() */
#endif /* H5_NO_DEPRECATED_SYMBOLS */


/*-------------------------------------------------------------------------
 * Function: test_huge_chunks
 *
 * Purpose: Tests that datasets with chunks >4GB can't be created.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Quincey Koziol
 *              Thursday, May  1, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_huge_chunks(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, chunk_dim; /* Dataset and chunk dimensions */
    hsize_t     dim2[3], chunk_dim2[3];  /* Dataset and chunk dimensions */
    herr_t      ret;            /* Generic return value */

    TESTING("creating dataset with >4GB chunks");

    h5_fixname(FILENAME[7], fapl, filename, sizeof filename);

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Try to set too large of a chunk for 1-D dataset (# of elements) */
    chunk_dim = TOO_HUGE_CHUNK_DIM;
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl, 1, &chunk_dim);
    } H5E_END_TRY;
    if(ret >= 0)
	FAIL_PUTS_ERROR("    Set chunk size with too large of chunk dimensions.")

    /* Try to set too large of a chunk for n-D dataset (# of elements) */
    chunk_dim2[0] = TOO_HUGE_CHUNK_DIM2_0;
    chunk_dim2[1] = TOO_HUGE_CHUNK_DIM2_1;
    chunk_dim2[2] = TOO_HUGE_CHUNK_DIM2_2;
    H5E_BEGIN_TRY {
        ret = H5Pset_chunk(dcpl, 3, chunk_dim2);
    } H5E_END_TRY;
    if(ret >= 0)
	FAIL_PUTS_ERROR("    Set chunk size with too large of chunk dimensions.")

    /* Set 1-D chunk size */
    chunk_dim = HUGE_CHUNK_DIM;
    if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = HUGE_DIM;
    if((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR

    /* Try to create dataset */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(fid, HUGE_DATASET, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >= 0)
	FAIL_PUTS_ERROR("    1-D Dataset with too large of chunk dimensions created.")

    /* Close 1-D dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR


    /* Set n-D chunk size */
    chunk_dim2[0] = HUGE_CHUNK_DIM2_0;
    chunk_dim2[1] = HUGE_CHUNK_DIM2_1;
    chunk_dim2[2] = HUGE_CHUNK_DIM2_2;
    if(H5Pset_chunk(dcpl, 3, chunk_dim2) < 0) FAIL_STACK_ERROR

    /* Create n-D dataspace */
    dim2[0] = HUGE_DIM2_0;
    dim2[1] = HUGE_DIM2_1;
    dim2[2] = HUGE_DIM2_2;
    if((sid = H5Screate_simple(3, dim2, NULL)) < 0) FAIL_STACK_ERROR

    /* Try to create dataset */
    H5E_BEGIN_TRY {
        dsid = H5Dcreate2(fid, HUGE_DATASET2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
    } H5E_END_TRY;
    if(dsid >= 0)
	FAIL_PUTS_ERROR("    n-D Dataset with too large of chunk dimensions created.")

    /* Close n-D dataspace */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

    /* Close everything else */
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end test_huge_chunks() */


/*-------------------------------------------------------------------------
 * Function: test_chunk_cache
 *
 * Purpose: Tests API for setting rdcc info on a DAPL, and interaction
 *          with the corresponding properties in the file structure.
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Neil Fortner
 *              Wednesday, October 29, 2008
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_cache(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       fapl_local = -1; /* Local fapl */
    hid_t       fapl_def = -1;  /* Default fapl */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       dapl1 = -1;     /* Dataset access property list ID */
    hid_t       dapl2 = -1;     /* Dataset access property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, chunk_dim; /* Dataset and chunk dimensions */
    size_t      nslots_1, nslots_2, nslots_3, nslots_4; /* rdcc number of elements */
    size_t      nbytes_1, nbytes_2, nbytes_3, nbytes_4; /* rdcc number of bytes */
    size_t      nlinks;         /* Number of link traversals */
    double      w0_1, w0_2, w0_3, w0_4; /* rdcc preemption policy */

    TESTING("dataset chunk cache configuration");

    /* Create a default fapl and dapl */
    if ((fapl_def = H5Pcreate(H5P_FILE_ACCESS)) < 0) FAIL_STACK_ERROR
    if ((dapl1 = H5Pcreate(H5P_DATASET_ACCESS)) < 0) FAIL_STACK_ERROR

    /* Verify that H5Pget_chunk_cache(dapl) returns the same values as are in
     * the default fapl.
     */
    if (H5Pget_cache(fapl_def, NULL, &nslots_1, &nbytes_1, &w0_1) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl1, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_1 != nslots_4) || (nbytes_1 != nbytes_4) || !DBL_ABS_EQUAL(w0_1, w0_4))
        FAIL_PUTS_ERROR("    Cache values from default dapl do not match those from fapl.")

    /* Set a lapl property on dapl1 (to verify inheritance) */
    if (H5Pset_nlinks(dapl1, (size_t)134) < 0) FAIL_STACK_ERROR
    if (H5Pget_nlinks(dapl1, &nlinks) < 0) FAIL_STACK_ERROR
    if (nlinks != 134)
        FAIL_PUTS_ERROR("    nlinks parameter not set properly on dapl.")

    /* Copy fapl passed to this function (as we will be modifying it) */
    if ((fapl_local = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

    /* Set new rdcc settings on fapl */
    nslots_2 = nslots_1 * 2;
    nbytes_2 = nbytes_1 * 2;
    w0_2 = w0_1 / 2.;
    if (H5Pset_cache(fapl_local, 0, nslots_2, nbytes_2, w0_2) < 0) FAIL_STACK_ERROR

    h5_fixname(FILENAME[8], fapl, filename, sizeof filename);

    /* Create file */
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_local)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if ((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Set chunking */
    chunk_dim = 10;
    if (H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = 100;
    if ((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR

    /* Create dataset with default dapl */
    if ((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, dapl1)) < 0)
        FAIL_STACK_ERROR

    /* Retrieve dapl from dataset, verfiy cache values are the same as on fapl_local */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Set new values on dapl1.  nbytes will be set to default, so the file
     * property will override this setting */
    nslots_3 = nslots_2 * 2;
    nbytes_3 = H5D_CHUNK_CACHE_NBYTES_DEFAULT;
    w0_3 = w0_2 / 2;
    if (H5Pset_chunk_cache(dapl1, nslots_3, nbytes_3, w0_3) < 0) FAIL_STACK_ERROR

    /* Close dataset, reopen with dapl1.  Note the use of a dapl with H5Oopen */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Oopen(fid, "dset", dapl1)) < 0) FAIL_STACK_ERROR

    /* Retrieve dapl from dataset, verfiy cache values are the same as on dapl1 */
    /* Note we rely on the knowledge that H5Pget_chunk_cache retrieves these
     * values directly from the dataset structure, and not from a copy of the
     * dapl used to open the dataset (which is not preserved).
     */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_2 != nbytes_4) || !DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from dapl1.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Close dataset, reopen with H5P_DEFAULT as dapl */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Retrieve dapl from dataset, verfiy cache values are the same on fapl_local */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Similary, test use of H5Dcreate2 with H5P_DEFAULT */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Dcreate2(fid, "dset2", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    /* Don't close dapl2, we will use it in the next section */

    /* Modify cache values on fapl_local */
    nbytes_3 = nbytes_2 * 2;
    if (H5Pset_cache(fapl_local, 0, nslots_3, nbytes_3, w0_3) < 0) FAIL_STACK_ERROR

    /* Close and reopen file with new fapl_local */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if (H5Fclose(fid) < 0) FAIL_STACK_ERROR
    if ((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl_local)) < 0) FAIL_STACK_ERROR

    /* Verify that dapl2 retrieved earlier (using values from the old fapl)
     * sets its values in the new file (test use of H5Dopen2 with a dapl)
     */
    if ((dsid = H5Dopen2(fid, "dset", dapl2)) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR /* Close dapl2, to avoid id leak */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_2 != nslots_4) || (nbytes_2 != nbytes_4) || !DBL_ABS_EQUAL(w0_2, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from dapl2.")

    /* Test H5D_CHUNK_CACHE_NSLOTS_DEFAULT and H5D_CHUNK_CACHE_W0_DEFAULT */
    nslots_2 = H5D_CHUNK_CACHE_NSLOTS_DEFAULT;
    w0_2 = H5D_CHUNK_CACHE_W0_DEFAULT;
    if (H5Pset_chunk_cache(dapl2, nslots_2, nbytes_2, w0_2) < 0) FAIL_STACK_ERROR

    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Dopen2(fid, "dset", dapl2)) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR /* Close dapl2, to avoid id leak */
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_2 != nbytes_4) || !DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those expected.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Verify that the file has indeed started using the new cache values (test
     * use of H5Oopen with H5P_DEFAULT) */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if ((dsid = H5Oopen(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR
    if ((dapl2 = H5Dget_access_plist(dsid)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_3 != nbytes_4) || !DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from retrieved dapl do not match those from fapl.")
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR

    /* Verify functionality of H5Pcopy with a dapl */
    if ((dapl2 = H5Pcopy(dapl1)) < 0) FAIL_STACK_ERROR
    if (H5Pget_chunk_cache(dapl2, &nslots_4, &nbytes_4, &w0_4) < 0) FAIL_STACK_ERROR
    if ((nslots_3 != nslots_4) || (nbytes_1 != nbytes_4) || !DBL_ABS_EQUAL(w0_3, w0_4))
        FAIL_PUTS_ERROR("    Cache values from dapl2 do not match those from dapl1.")

    /* Close */
    if (H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if (H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if (H5Pclose(fapl_local) < 0) FAIL_STACK_ERROR
    if (H5Pclose(fapl_def) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl1) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dapl2) < 0) FAIL_STACK_ERROR
    if (H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if (H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(fapl_local);
        H5Pclose(fapl_def);
        H5Pclose(dapl1);
        H5Pclose(dapl2);
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end test_chunk_cache() */


/*-------------------------------------------------------------------------
 * Function:    test_big_chunks_bypass_cache
 *
 * Purpose:     When the chunk size is bigger than the cache size and the
 *              chunk isn't on disk, this test verifies that the library
 *              bypasses the cache.
 *
 * Note:        This test is not very conclusive - it doesn't actually check
 *              if the chunks bypass the cache... :-(  -QAK
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Raymond Lu
 *              11 Feb 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_big_chunks_bypass_cache(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       fapl_local = -1; /* File access property list ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, chunk_dim; /* Dataset and chunk dimensions */
    size_t      rdcc_nelmts, rdcc_nbytes;
    int         fvalue = BYPASS_FILL_VALUE;
    hsize_t     count, stride, offset, block;
    static int  wdata[BYPASS_CHUNK_DIM/2], rdata1[BYPASS_DIM],
                rdata2[BYPASS_CHUNK_DIM/2];
    int         i, j;

    TESTING("big chunks bypassing the cache");

    h5_fixname(FILENAME[9], fapl, filename, sizeof filename);

    /* Copy fapl passed to this function (as we will be modifying it) */
    if((fapl_local = H5Pcopy(fapl)) < 0) FAIL_STACK_ERROR

    /* Define cache size to be smaller than chunk size */
    rdcc_nelmts = BYPASS_CHUNK_DIM/5;
    rdcc_nbytes = sizeof(int)*BYPASS_CHUNK_DIM/5;
    if(H5Pset_cache(fapl_local, 0, rdcc_nelmts, rdcc_nbytes, (double)0.0) < 0) FAIL_STACK_ERROR

    /* Create file */
    if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl_local)) < 0) FAIL_STACK_ERROR

    /* Create 1-D dataspace */
    dim = BYPASS_DIM;
    if((sid = H5Screate_simple(1, &dim, NULL)) < 0) FAIL_STACK_ERROR

    /* Create dataset creation property list */
    if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

    /* Define chunk size.  There will be only 2 chunks in the dataset. */
    chunk_dim = BYPASS_CHUNK_DIM;
    if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

    /* Define fill value, fill time, and chunk allocation time */
    if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fvalue) < 0) FAIL_STACK_ERROR
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET) < 0) FAIL_STACK_ERROR
    if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_INCR) < 0) FAIL_STACK_ERROR

    /* Create the first 1-D dataset */
    if((dsid = H5Dcreate2(fid, BYPASS_DATASET1, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    /* Select first chunk to write the data */
    offset = 0;
    count = 1;
    stride = 1;
    block = BYPASS_CHUNK_DIM / 2;
    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &offset, &stride, &count, &block) < 0)
        FAIL_STACK_ERROR

    /* Initialize data to write */
    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
        wdata[i] = i;

    /* This write should go through the cache because fill value is used. */
    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, wdata) < 0)
        FAIL_STACK_ERROR

    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Reopen the dataset */
    if((dsid = H5Dopen2(fid, BYPASS_DATASET1, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Reads both 2 chunks.  Reading the second chunk should bypass the cache because the
     * chunk is bigger than the cache size and it isn't allocated on disk. */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, rdata1) < 0)
        FAIL_STACK_ERROR

    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
        if(rdata1[i] != i) {
            printf("    Read different values than written in the 1st chunk.\n");
            printf("    At line %d and index %d, rdata1 = %d. It should be %d.\n", __LINE__, i, rdata1[i], i);
            TEST_ERROR
        } /* end if */

    for(j = BYPASS_CHUNK_DIM / 2; j < BYPASS_DIM; j++)
        if(rdata1[j] != fvalue) {
            printf("    Read different values than written in the 2nd chunk.\n");
            printf("    At line %d and index %d, rdata1 = %d. It should be %d.\n", __LINE__, i, rdata1[i], fvalue);
            TEST_ERROR
        } /* end if */

    /* Close the first dataset */
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Create a second dataset without fill value.  This time, both write
     * and read should bypass the cache because the chunk is bigger than the
     * cache size and it's not allocated on disk. */
    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_NEVER) < 0) FAIL_STACK_ERROR

    if((dsid = H5Dcreate2(fid, BYPASS_DATASET2, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR

    if(H5Dwrite(dsid, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, wdata) < 0)
        FAIL_STACK_ERROR

    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR

    /* Reopen the dataset */
    if((dsid = H5Dopen2(fid, BYPASS_DATASET2, H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

    /* Read back only the part that was written to the file.  Reading the
     * half chunk should bypass the cache because the chunk is bigger than
     * the cache size. */
    if(H5Dread(dsid, H5T_NATIVE_INT, H5S_ALL, sid, H5P_DEFAULT, rdata2) < 0)

    for(i = 0; i < BYPASS_CHUNK_DIM / 2; i++)
        if(rdata2[i] != i) {
            printf("    Read different values than written in the chunk.\n");
            printf("    At line %d and index %d, rdata2 = %d. It should be %d.\n", __LINE__, i, rdata2[i], i);
            TEST_ERROR
        } /* end if */

    /* Close IDs */
    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
    if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
    if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
    if(H5Pclose(fapl_local) < 0) FAIL_STACK_ERROR
    if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Pclose(fapl_local);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end test_big_chunks_bypass_cache() */

/* This message derives from H5Z */
const H5Z_class2_t H5Z_EXPAND[1] = {{
    H5Z_CLASS_T_VERS,           /* H5Z_class_t version */
    H5Z_FILTER_EXPAND,		/* Filter id number		*/
    1, 1,                       /* Encoding and decoding enabled */
    "expand",			/* Filter name for debugging	*/
    NULL,                       /* The "can apply" callback     */
    NULL,                       /* The "set local" callback     */
    filter_expand,		/* The actual filter function	*/
}};

/* Global "expansion factor" for filter_expand() routine */
static size_t filter_expand_factor_g = 0;


/*-------------------------------------------------------------------------
 * Function:    filter_expand
 *
 * Purpose:     For testing library's behavior when a filter expands a chunk
 *              too much.
 *
 * Note:	This filter doesn't actually re-allocate the buffer to be
 *		larger, it just changes the buffer size to a value that's too
 *		large.  The library should throw an error before using the
 *		incorrect buffer information.
 *
 * Return:	Success:	Data chunk size
 *		Failure:	0
 *
 * Programmer:	Quincey Koziol
 *              Mar 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static size_t
filter_expand(unsigned int flags, size_t UNUSED cd_nelmts,
      const unsigned int UNUSED *cd_values, size_t nbytes,
      size_t *buf_size, void UNUSED **buf)
{
    size_t         ret_value = 0;

    if(flags & H5Z_FLAG_REVERSE) {
        /* Don't do anything when filter is applied in reverse */
        *buf_size = nbytes;
        ret_value = nbytes;
    } /* end if */
    else {
        /* Check for expanding the chunk */
        if(filter_expand_factor_g > 0) {
            /* Expand the buffer size beyond what can be encoded */
            *buf_size = nbytes * 256 * 256 * 256 * filter_expand_factor_g;
            ret_value = *buf_size;
        } /* end if */
        else {
            /* Don't expand the chunk's size */
            *buf_size = nbytes;
            ret_value = nbytes;
        } /* end else */
    } /* end else */

    return ret_value;
} /* end filter_expand() */


/*-------------------------------------------------------------------------
 * Function: test_chunk_expand
 *
 * Purpose: Tests support for proper error handling when a chunk expands
 *              too much after a filter is applied
 *
 * Return:      Success: 0
 *              Failure: -1
 *
 * Programmer:  Quincey Koziol
 *              Tuesday, March 31, 2009
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_chunk_expand(hid_t fapl)
{
    char        filename[FILENAME_BUF_SIZE];
    hid_t       fid = -1;       /* File ID */
    hid_t       dcpl = -1;      /* Dataset creation property list ID */
    hid_t       sid = -1;       /* Dataspace ID */
    hid_t       scalar_sid = -1;/* Scalar dataspace ID */
    hid_t       dsid = -1;      /* Dataset ID */
    hsize_t     dim, max_dim, chunk_dim; /* Dataset and chunk dimensions */
    hsize_t	hs_offset;      /* Hyperslab offset */
    hsize_t	hs_size;        /* Hyperslab size */
    H5D_alloc_time_t alloc_time;        /* Storage allocation time */
    unsigned    write_elem, read_elem;  /* Element written/read */
    unsigned    u;              /* Local index variable */
    herr_t      status;         /* Generic return value */

    TESTING("filter expanding chunks too much");

    h5_fixname(FILENAME[10], fapl, filename, sizeof filename);

    if(sizeof(size_t) <= 4) {
	SKIPPED();
	puts("    Current machine can't test for error");
    } /* end if */
    else {
        /* Register "expansion" filter */
        if(H5Zregister(H5Z_EXPAND) < 0) FAIL_STACK_ERROR

        /* Check that the filter was registered */
        if(TRUE != H5Zfilter_avail(H5Z_FILTER_EXPAND)) FAIL_STACK_ERROR

        /* Loop over storage allocation time */
        for(alloc_time = H5D_ALLOC_TIME_EARLY; alloc_time <= H5D_ALLOC_TIME_INCR; alloc_time++) {
            /* Create file */
            if((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0) FAIL_STACK_ERROR

            /* Create dataset creation property list */
            if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0) FAIL_STACK_ERROR

            /* Set chunking */
            chunk_dim = 10;
            if(H5Pset_chunk(dcpl, 1, &chunk_dim) < 0) FAIL_STACK_ERROR

            /* Set fill time */
            if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0) FAIL_STACK_ERROR

            /* Set allocation time */
            if(H5Pset_alloc_time(dcpl, alloc_time) < 0) FAIL_STACK_ERROR

            /* Set "expand" filter */
            if(H5Pset_filter(dcpl, H5Z_FILTER_EXPAND, 0, (size_t)0, NULL) < 0) FAIL_STACK_ERROR

            /* Create scalar dataspace */
            if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

            /* Create 1-D dataspace */
            dim = 100;
            max_dim = H5S_UNLIMITED;
            if((sid = H5Screate_simple(1, &dim, &max_dim)) < 0) FAIL_STACK_ERROR

            /* Create chunked dataset */
            if(H5D_ALLOC_TIME_EARLY == alloc_time) {
                /* Make the expansion factor large enough to cause failure right away */
                filter_expand_factor_g = 8;

                H5E_BEGIN_TRY {
                    dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
                } H5E_END_TRY;
                if(dsid >= 0) FAIL_PUTS_ERROR("should fail to create dataset when allocation time is early");
            } /* end if */
            else {
                if((dsid = H5Dcreate2(fid, "dset", H5T_NATIVE_UINT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
                    FAIL_STACK_ERROR

                /* Fill elements */
                hs_size = 1;
                for(u = 0; u < 100; u++) {
                    /* Select a single element in the dataset */
                    hs_offset = u;
                    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR

                    /* Read (unwritten) element from dataset */
                    read_elem = 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                    /* Verify unwritten element is fill value (0) */
                    if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

                    /* Don't expand chunks yet */
                    filter_expand_factor_g = 0;

                    /* Write element to dataset */
                    write_elem = u;
                    if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR

                    /* Read element from dataset */
                    read_elem = write_elem + 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                    /* Verify written element is read in */
                    if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");

                    /* Expand chunks now */
                    filter_expand_factor_g = 8;

                    /* Write element to dataset */
                    write_elem = u;
                    H5E_BEGIN_TRY {
                        status = H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem);
                    } H5E_END_TRY;
                    if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");
                } /* end for */

                /* Incrementally extend dataset and verify write/reads */
                while(dim < 1000) {
                    /* Extend dataset */
                    dim += 100;
                    if(H5Dset_extent(dsid, &dim) < 0) FAIL_STACK_ERROR

                    /* Close old dataspace */
                    if(H5Sclose(sid) < 0) FAIL_STACK_ERROR

                    /* Get dataspace for dataset now */
                    if((sid = H5Dget_space(dsid)) < 0) FAIL_STACK_ERROR

                    /* Fill new elements */
                    hs_size = 1;
                    for(u = 0; u < 100; u++) {
                        /* Select a single element in the dataset */
                        hs_offset = (dim + u) - 100;
                        if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR

                        /* Read (unwritten) element from dataset */
                        read_elem = 1;
                        if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                        /* Verify unwritten element is fill value (0) */
                        if(read_elem != 0) FAIL_PUTS_ERROR("invalid unwritten element read");

                        /* Don't expand chunks yet */
                        filter_expand_factor_g = 0;

                        /* Write element to dataset */
                        write_elem = u;
                        if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR

                        /* Read element from dataset */
                        read_elem = write_elem + 1;
                        if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                        /* Verify written element is read in */
                        if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");

                        /* Expand chunks now */
                        filter_expand_factor_g = 8;

                        /* Write element to dataset */
                        write_elem = u;
                        H5E_BEGIN_TRY {
                            status = H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem);
                        } H5E_END_TRY;
                        if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");
                    } /* end for */
                } /* end while */

                /* Close dataset */
                if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
            } /* end else */

            /* Close everything */
            if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
            if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
            if(H5Pclose(dcpl) < 0) FAIL_STACK_ERROR
            if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

            /* If the dataset was created, do some extra testing */
            if(H5D_ALLOC_TIME_EARLY != alloc_time) {
                /* Re-open file & dataset */
                if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

                /* Open dataset */
                if((dsid = H5Dopen2(fid, "dset", H5P_DEFAULT)) < 0) FAIL_STACK_ERROR

                /* Create scalar dataspace */
                if((scalar_sid = H5Screate(H5S_SCALAR)) < 0) FAIL_STACK_ERROR

                /* Get dataspace for dataset now */
                if((sid = H5Dget_space(dsid)) < 0) FAIL_STACK_ERROR

                /* Read elements */
                hs_size = 1;
                for(u = 0; u < 1000; u++) {
                    /* Select a single element in the dataset */
                    hs_offset = u;
                    if(H5Sselect_hyperslab(sid, H5S_SELECT_SET, &hs_offset, NULL, &hs_size, NULL) < 0) FAIL_STACK_ERROR

                    /* Read element from dataset */
                    read_elem = u + 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                    /* Verify unwritten element is proper value */
                    if(read_elem != (u % 100)) FAIL_PUTS_ERROR("invalid element read");

                    /* Don't expand chunks yet */
                    filter_expand_factor_g = 0;

                    /* Write element to dataset */
                    write_elem = u % 100;
                    if(H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem) < 0) FAIL_STACK_ERROR

                    /* Read element from dataset */
                    read_elem = write_elem + 1;
                    if(H5Dread(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &read_elem) < 0) FAIL_STACK_ERROR

                    /* Verify written element is read in */
                    if(read_elem != write_elem) FAIL_PUTS_ERROR("invalid written element read");

                    /* Expand chunks now */
                    filter_expand_factor_g = 8;

                    /* Write element to dataset */
                    write_elem = u % 100;
                    H5E_BEGIN_TRY {
                        status = H5Dwrite(dsid, H5T_NATIVE_UINT, scalar_sid, sid, H5P_DEFAULT, &write_elem);
                    } H5E_END_TRY;
                    if(status >= 0) FAIL_PUTS_ERROR("should fail to write to dataset when allocation time is not early");
                } /* end for */

                /* Close everything */
                if(H5Sclose(sid) < 0) FAIL_STACK_ERROR
                if(H5Sclose(scalar_sid) < 0) FAIL_STACK_ERROR
                if(H5Dclose(dsid) < 0) FAIL_STACK_ERROR
                if(H5Fclose(fid) < 0) FAIL_STACK_ERROR

                /* Re-open file */
                if((fid = H5Fopen(filename, H5F_ACC_RDWR, fapl)) < 0) FAIL_STACK_ERROR

                /* Delete dataset */
                if(H5Ldelete(fid, "dset", H5P_DEFAULT) < 0) FAIL_STACK_ERROR

                /* Close everything */
                if(H5Fclose(fid) < 0) FAIL_STACK_ERROR
            } /* end if */
        } /* end for */

        /* Unregister "expansion" filter */
        if(H5Zunregister(H5Z_FILTER_EXPAND) < 0) FAIL_STACK_ERROR

        /* Check that the filter was unregistered */
        if(FALSE != H5Zfilter_avail(H5Z_FILTER_EXPAND)) FAIL_STACK_ERROR

        PASSED();
    } /* end else */

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
        H5Dclose(dsid);
        H5Sclose(sid);
        H5Sclose(scalar_sid);
        H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* end test_chunk_expand() */

/*-------------------------------------------------------------------------
 *
 *  test_idx_compatible():
 *	Verify that the 1.8 branch cannot read datasets that use
 *	Fixed Array indexing method.
 *
 *-------------------------------------------------------------------------
 */
static herr_t
test_idx_compatible(void)
{
    hid_t	fid = -1;	/* File id */
    hid_t       did = -1;	/* Dataset id */
    char  	*srcdir = HDgetenv("srcdir"); /* where the src code is located */
    char        filename[FILENAME_BUF_SIZE] = "";  /* old test file name */

    /* Output message about test being performed */
    TESTING("Compatibility for datasets that use Fixed Array indexing\n");

    /* Generate correct name for test file by prepending the source path */
    if(srcdir && ((HDstrlen(srcdir) + HDstrlen(FIXED_IDX_FILE) + 1) < sizeof(filename))) {
	HDstrcpy(filename, srcdir);
	HDstrcat(filename, "/");
    }
    HDstrcat(filename, FIXED_IDX_FILE);

    /* Open the file */
    if((fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT)) < 0)
	FAIL_STACK_ERROR

    /* Should not able to read the dataset w/o filter that use Fixed Array indexing */
    H5E_BEGIN_TRY {
	if((did = H5Dopen2(fid, DSET, H5P_DEFAULT)) != FAIL)
	    TEST_ERROR
    } H5E_END_TRY;

    /* Should not able to read the dataset w/ filter that use Fixed Array indexing */
    H5E_BEGIN_TRY {
	if((did = H5Dopen2(fid, DSET_FILTER, H5P_DEFAULT)) != FAIL)
	    TEST_ERROR
    } H5E_END_TRY;

    if(H5Fclose(fid) < 0)
	FAIL_STACK_ERROR

    PASSED();
    return 0;

error:
    H5E_BEGIN_TRY {
        H5Dclose(did);
	H5Fclose(fid);
    } H5E_END_TRY;
    return -1;
} /* test_idx_compatible */


/*-------------------------------------------------------------------------
 * Function:	main
 *
 * Purpose:	Tests the dataset interface (H5D)
 *
 * Return:	Success:	exit(0)
 *
 *		Failure:	exit(1)
 *
 * Programmer:	Robb Matzke
 *		Tuesday, December  9, 1997
 *
 *-------------------------------------------------------------------------
 */
int
main(void)
{
    char		filename[FILENAME_BUF_SIZE];
    hid_t		file, grp, fapl, fapl2;
    hbool_t new_format;
    int mdc_nelmts;
    size_t rdcc_nelmts;
    size_t rdcc_nbytes;
    double rdcc_w0;
    int	nerrors = 0;
    const char *envval;

    /* Don't run this test using certain file drivers */
    envval = HDgetenv("HDF5_DRIVER");
    if(envval == NULL)
        envval = "nomatch";

    /* Set the random # seed */
    HDsrandom((unsigned)HDtime(NULL));

    /* Testing setup */
    h5_reset();
    fapl = h5_fileaccess();

    /* Turn off the chunk cache, so all the chunks are immediately written to disk */
    if(H5Pget_cache(fapl, &mdc_nelmts, &rdcc_nelmts, &rdcc_nbytes, &rdcc_w0) < 0)
        goto error;
    rdcc_nbytes = 0;
    if(H5Pset_cache(fapl, mdc_nelmts, rdcc_nelmts, rdcc_nbytes, rdcc_w0) < 0)
        goto error;

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);

    /* Test with old & new format groups */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Set the FAPL for the type of format */
        if(new_format) {
            puts("\nTesting with new file format:");
            my_fapl = fapl2;
        } /* end if */
        else {
            puts("Testing with old file format:");
            my_fapl = fapl;
        } /* end else */

        /* Create the file for this test */
        if((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, my_fapl)) < 0)
            goto error;

        /* Cause the library to emit initial messages */
        if((grp = H5Gcreate2(file, "emit diagnostics", H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) < 0)
            goto error;
        if(H5Oset_comment(grp, "Causes diagnostic messages to be emitted") < 0)
            goto error;
        if(H5Gclose(grp) < 0)
            goto error;

        nerrors += (test_create(file) < 0 			? 1 : 0);
        nerrors += (test_simple_io(envval, my_fapl) < 0		? 1 : 0);
        nerrors += (test_compact_io(my_fapl) < 0  		? 1 : 0);
        nerrors += (test_max_compact(my_fapl) < 0  		? 1 : 0);
        nerrors += (test_conv_buffer(file) < 0		? 1 : 0);
        nerrors += (test_tconv(file) < 0			? 1 : 0);
        nerrors += (test_filters(file, my_fapl) < 0		? 1 : 0);
        nerrors += (test_onebyte_shuffle(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_int(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_float(file) < 0         	? 1 : 0);
        nerrors += (test_nbit_double(file) < 0         	? 1 : 0);
        nerrors += (test_nbit_array(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_compound(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_compound_2(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_compound_3(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_int_size(file) < 0 		? 1 : 0);
        nerrors += (test_nbit_flt_size(file) < 0 		? 1 : 0);
        nerrors += (test_scaleoffset_int(file) < 0 		? 1 : 0);
        nerrors += (test_scaleoffset_int_2(file) < 0 	? 1 : 0);
        nerrors += (test_scaleoffset_float(file) < 0 	? 1 : 0);
        nerrors += (test_scaleoffset_float_2(file) < 0 	? 1 : 0);
        nerrors += (test_scaleoffset_double(file) < 0 	? 1 : 0);
        nerrors += (test_scaleoffset_double_2(file) < 0 	? 1 : 0);
        nerrors += (test_multiopen (file) < 0		? 1 : 0);
        nerrors += (test_types(file) < 0       		? 1 : 0);
        nerrors += (test_userblock_offset(envval, my_fapl) < 0     	? 1 : 0);
        nerrors += (test_missing_filter(file) < 0		? 1 : 0);
        nerrors += (test_can_apply(file) < 0		? 1 : 0);
        nerrors += (test_can_apply2(file) < 0		? 1 : 0);
        nerrors += (test_set_local(my_fapl) < 0		? 1 : 0);
        nerrors += (test_can_apply_szip(file) < 0		? 1 : 0);
        nerrors += (test_compare_dcpl(file) < 0		? 1 : 0);
        nerrors += (test_copy_dcpl(file, my_fapl) < 0	? 1 : 0);
        nerrors += (test_filter_delete(file) < 0		? 1 : 0);
        nerrors += (test_filters_endianess() < 0	? 1 : 0);
        nerrors += (test_zero_dims(file) < 0		? 1 : 0);
        nerrors += (test_missing_chunk(file) < 0		? 1 : 0);
        nerrors += (test_random_chunks(my_fapl) < 0		? 1 : 0);
#ifndef H5_NO_DEPRECATED_SYMBOLS
        nerrors += (test_deprec(file) < 0			? 1 : 0);
#endif /* H5_NO_DEPRECATED_SYMBOLS */
        nerrors += (test_huge_chunks(my_fapl) < 0		? 1 : 0);
        nerrors += (test_chunk_cache(my_fapl) < 0		? 1 : 0);
        nerrors += (test_big_chunks_bypass_cache(my_fapl) < 0   ? 1 : 0);
        nerrors += (test_chunk_expand(my_fapl) < 0		? 1 : 0);
	nerrors += (test_idx_compatible() < 0  			? 1 : 0);
	nerrors += (test_layout_extend(my_fapl) < 0		? 1 : 0);

        if(H5Fclose(file) < 0)
            goto error;
    } /* end for */

    /* Close 2nd FAPL */
    if(H5Pclose(fapl2) < 0) TEST_ERROR

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    if(nerrors)
        goto error;
    printf("All dataset tests passed.\n");
#ifdef H5_HAVE_FILTER_SZIP
    if (GetTestCleanup())
        HDremove(NOENCODER_COPY_FILENAME);
#endif /* H5_HAVE_FILTER_SZIP */
    h5_cleanup(FILENAME, fapl);

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d DATASET TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}

