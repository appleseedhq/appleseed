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
 * Programmer:  Pedro Vicente <pvn@ncsa.uiuc.edu>
 *              April 12, 2002
 *
 * Purpose:     Tests the H5Dset_extent call
 */

#include <time.h>
#include <stdlib.h>
#include "hdf5.h"
#include "h5test.h"

/*-------------------------------------------------------------------------
 *
 * Tests the function H5Dset_extent.
 *
 *-------------------------------------------------------------------------
 */

const char *FILENAME[] = {
    "set_extent1",
    "set_extent2",
    "set_extent3",
    "set_extent4",
    "set_extent5",
    "set_extent6",
    NULL
};

#define NAME_BUF_SIZE   1024
#define EXT_FILE_NAME1 "ext1.bin"
#define EXT_FILE_NAME2 "ext2.bin"

#define CONFIG_COMPRESS         0x01u
#define CONFIG_FILL             0x02u
#define CONFIG_EARLY_ALLOC      0x04u
#define CONFIG_ALL              (CONFIG_COMPRESS + CONFIG_FILL                 \
                                + CONFIG_EARLY_ALLOC)
#define FILL_VALUE -1
#define DO_RANKS_PRINT_CONFIG(TEST) {                                          \
    printf("  Config:\n");                                                     \
    printf("   Test: %s\n", TEST);                                             \
    printf("   Compression: %s\n", (config & CONFIG_COMPRESS ? "yes" : "no")); \
    printf("   Fill value: %s\n", (do_fillvalue ? "yes" : "no"));              \
    printf("   Early allocation: %s\n", (config & CONFIG_EARLY_ALLOC ? "yes"   \
            : "no"));                                                          \
} /* end DO_RANKS_PRINT_CONFIG */

#define RANK1 1
#define RANK2 2
#define RANK3 3
#define DIM0  5
#define DIM1  5
#define DIM2  5
#define DIMS0 3
#define DIMS1 3
#define DIMS2 3
#define DIME0 7
#define DIME1 7
#define DIME2 7
#define ISTORE_IK  64
#define RAND4_NITER 100
#define RAND4_SPARSE_SWITCH 10
#define RAND4_FAIL_DUMP(NDIM_SETS, J, K, L, M) {                               \
    H5_FAILED(); AT();                                                         \
    test_random_rank4_dump(NDIM_SETS, dim_log, cdims, J, K, L, M);             \
    goto error;                                                                \
} /* end RAND4_FAIL_DUMP */

static int do_ranks( hid_t fapl );
static int do_layouts( hid_t fapl );

static int test_rank1( hid_t fapl,
                       hid_t dcpl,
                       hbool_t do_fill_value,
                       hbool_t set_istore_k);
static int test_rank2( hid_t fapl,
                       hid_t dcpl,
                       hbool_t do_fill_value,
                       hbool_t set_istore_k);
static int test_rank3( hid_t fapl,
                       hid_t dcpl,
                       hbool_t do_fill_value,
                       hbool_t set_istore_k);
static int test_random_rank4( hid_t fapl,
                              hid_t dcpl,
                              hbool_t do_fillvalue,
                              hbool_t do_sparse);

static int test_external( hid_t fapl );
static int test_layouts( H5D_layout_t layout, hid_t fapl );
static void test_random_rank4_dump( unsigned ndim_sets, hsize_t dim_log[][4],
        hsize_t cdims[4], int j, int k, int l, int m );

/*-------------------------------------------------------------------------
 * main
 *-------------------------------------------------------------------------
 */

int main( void )
{
    hid_t fapl;                 /* file access property list */
    hid_t fapl2;                /* file access property list w/latest format set */
    hbool_t new_format;         /* Whether to use the latest file format */
    hbool_t chunk_cache;        /* Whether to enable chunk caching */
    int	  nerrors = 0;

    /* Initialize random number seed */
    HDsrandom((unsigned)HDtime(NULL));

    h5_reset();
    fapl = h5_fileaccess();

    /* Copy the file access property list */
    if((fapl2 = H5Pcopy(fapl)) < 0) TEST_ERROR

    /* Set chunk cache so only part of the chunks can be cached on fapl */
    if(H5Pset_cache(fapl, 0, (size_t)8, 256 * sizeof(int), 0.75) < 0) TEST_ERROR

    /* Disable chunk caching on fapl2 */
    if(H5Pset_cache(fapl2, 0, (size_t)0, (size_t)0, 0.) < 0) TEST_ERROR

    /* Set the "use the latest version of the format" bounds for creating objects in the file */
    if(H5Pset_libver_bounds(fapl2, H5F_LIBVER_LATEST, H5F_LIBVER_LATEST) < 0) TEST_ERROR

    /* Test with old & new format groups */
    for(new_format = FALSE; new_format <= TRUE; new_format++) {
        hid_t my_fapl;

        /* Test chunked datasets with and without chunk cache */
        for(chunk_cache = FALSE; chunk_cache <= TRUE; chunk_cache++) {
            /* Output message about the type of format */
            if(new_format)
                printf("Testing with new file format");
            else
                printf("Testing with old file format");

            /* Set the FAPL for the chunk cache settings */
            if(chunk_cache) {
                puts(" and chunk cache enabled:");
                my_fapl = fapl;
            } /* end if */
            else {
                puts(" and chunk cache disabled:");
                my_fapl = fapl2;
            } /* end else */

            /* Set the FAPL for the type of format */
            if(new_format) {
                /* Set the "use the latest version of the format" bounds for
                 * creating objects in the file */
                if(H5Pset_libver_bounds(my_fapl, H5F_LIBVER_LATEST,
                        H5F_LIBVER_LATEST) < 0) TEST_ERROR
            } /* end if */
            else
                /* Set the "use the earliest version of the format" bounds for
                 * creating objects in the file */
                if(H5Pset_libver_bounds(my_fapl, H5F_LIBVER_EARLIEST,
                        H5F_LIBVER_LATEST) < 0) TEST_ERROR

            /* Tests which use chunked datasets */
            nerrors += do_ranks( my_fapl ) < 0 ? 1 : 0;
        } /* end for */

        /* Tests which do not use chunked datasets */
        nerrors += test_external( fapl ) < 0 ? 1 : 0;
        nerrors += do_layouts( fapl ) < 0 ? 1 : 0;
    } /* end for */

    /* Close 2nd FAPL */
    if(H5Pclose(fapl2) < 0) TEST_ERROR

    /* Verify symbol table messages are cached */
    nerrors += (h5_verify_cached_stabs(FILENAME, fapl) < 0 ? 1 : 0);

    h5_cleanup(FILENAME, fapl);

    HDremove(EXT_FILE_NAME1);
    HDremove(EXT_FILE_NAME2);

    if(nerrors)
        goto error;
    puts("All H5Dset_extent tests passed.");

    return 0;

error:
    nerrors = MAX(1, nerrors);
    printf("***** %d H5Dset_extent TEST%s FAILED! *****\n",
            nerrors, 1 == nerrors ? "" : "S");
    return 1;
}


/*-------------------------------------------------------------------------
* test with several ranks
*-------------------------------------------------------------------------
*/
static int do_ranks( hid_t fapl )
{

    hbool_t     do_fillvalue = 0;
    hid_t       dcpl = -1;
    int         fillvalue = FILL_VALUE;
    unsigned    config;

    TESTING_2("datasets with ranks 1 to 4 (all configurations)");

    /* Loop over different configurations for tests */
    for(config=0; config<=CONFIG_ALL; config++) {
        /* Create DCPL and add appropriate settings */
        if((dcpl = H5Pcreate(H5P_DATASET_CREATE)) < 0)
            TEST_ERROR

        if(config & CONFIG_COMPRESS) {
#ifdef H5_HAVE_FILTER_DEFLATE
            if(H5Pset_deflate(dcpl, 9) < 0)
                TEST_ERROR
#else /* H5_HAVE_FILTER_DEFLATE */
            if(H5Pclose(dcpl) < 0)
                TEST_ERROR
            continue;
#endif /* H5_HAVE_FILTER_DEFLATE */
        } /* end if */

        if(config & CONFIG_FILL) {
            do_fillvalue = TRUE;
            if(H5Pset_fill_value(dcpl, H5T_NATIVE_INT, &fillvalue) < 0)
                TEST_ERROR
        } /* end if */
        else
            do_fillvalue = FALSE;

        if(config & CONFIG_EARLY_ALLOC)
            if(H5Pset_alloc_time(dcpl, H5D_ALLOC_TIME_EARLY) < 0)
                TEST_ERROR

        /* Run tests */
        if(do_fillvalue) {
            unsigned ifset;

            /* Iterate over different fill times */
            for(ifset=0; ifset<=1; ifset++) {
                if(ifset) {
                    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET) < 0)
                        TEST_ERROR
                } /* end if */
                else
                    if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0)
                        TEST_ERROR

                if(test_rank1(fapl, dcpl, do_fillvalue, FALSE) < 0) {
                    DO_RANKS_PRINT_CONFIG("Rank 1")
                    printf("   Fill time: %s\n", (ifset ? "H5D_FILL_TIME_IFSET"
                            : "H5D_FILL_TIME_ALLOC"));
                    goto error;
                } /* end if */
                if(test_rank2(fapl, dcpl, do_fillvalue, FALSE) < 0) {
                    DO_RANKS_PRINT_CONFIG("Rank 2")
                    printf("   Fill time: %s\n", (ifset ? "H5D_FILL_TIME_IFSET"
                            : "H5D_FILL_TIME_ALLOC"));
                    goto error;
                } /* end if */
                if(test_rank3(fapl, dcpl, do_fillvalue, FALSE) < 0) {
                    DO_RANKS_PRINT_CONFIG("Rank 3")
                    printf("   Fill time: %s\n", (ifset ? "H5D_FILL_TIME_IFSET"
                            : "H5D_FILL_TIME_ALLOC"));
                    goto error;
                } /* end if */
                if(test_rank2(fapl, dcpl, do_fillvalue, TRUE) < 0) {
                    DO_RANKS_PRINT_CONFIG("Rank 2 with non-default indexed storage B-tree")
                    printf("   Fill time: %s\n", (ifset ? "H5D_FILL_TIME_IFSET"
                            : "H5D_FILL_TIME_ALLOC"));
                    goto error;
                } /* end if */
            } /* end for */
        } /* end if */
        else {
            /* These tests expect fill values to be written even if there is no
             * fill value defined */
            if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_ALLOC) < 0)
                TEST_ERROR

            if(test_rank1(fapl, dcpl, do_fillvalue, FALSE) < 0) {
                DO_RANKS_PRINT_CONFIG("Rank 1")
                goto error;
            } /* end if */
            if(test_rank2(fapl, dcpl, do_fillvalue, FALSE) < 0) {
                DO_RANKS_PRINT_CONFIG("Rank 2")
                goto error;
            } /* end if */
            if(test_rank3(fapl, dcpl, do_fillvalue, FALSE) < 0) {
                DO_RANKS_PRINT_CONFIG("Rank 3")
                goto error;
            } /* end if */
            if(test_rank2(fapl, dcpl, do_fillvalue, TRUE) < 0) {
                DO_RANKS_PRINT_CONFIG("Rank 2 with non-default indexed storage B-tree")
                goto error;
            } /* end if */
        } /* end else */

        /* The rank 4 test expects the fill value to be written only if
         * defined */
        if(H5Pset_fill_time(dcpl, H5D_FILL_TIME_IFSET) < 0)
            TEST_ERROR

        if(test_random_rank4(fapl, dcpl, do_fillvalue, FALSE) < 0) {
            DO_RANKS_PRINT_CONFIG("Randomized rank 4")
            goto error;
        } /* end if */

        if(!(config & CONFIG_EARLY_ALLOC))
            if(test_random_rank4(fapl, dcpl, do_fillvalue, TRUE) < 0) {
                DO_RANKS_PRINT_CONFIG("Randomized rank 4 with sparse allocation")
                goto error;
            } /* end if */

        /* Close dcpl */
        if(H5Pclose(dcpl) < 0)
            TEST_ERROR
    } /* end for */

    PASSED();

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Pclose(dcpl);
    } H5E_END_TRY

    return -1;
} /* end do_ranks */


/*-------------------------------------------------------------------------
* test with different storage layouts
*-------------------------------------------------------------------------
*/
static int do_layouts( hid_t fapl )
{

    TESTING("storage layout use");

    if (test_layouts( H5D_COMPACT, fapl ) < 0)
        goto error;

    if (test_layouts( H5D_CONTIGUOUS, fapl ) < 0)
        goto error;

    PASSED();

    return 0;

error:
    return -1;
}

/*-------------------------------------------------------------------------
 * test usage with a 1D rank
 *-------------------------------------------------------------------------
 */

static int test_rank1( hid_t fapl,
                       hid_t dcpl,
                       hbool_t do_fill_value,
                       hbool_t set_istore_k)
{

    hid_t   fid=-1;
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   my_dcpl=-1;
    hid_t   fcpl;
    hsize_t dims_o[RANK1] = {DIM0};   /* original dimensions */
    hsize_t dims_s[RANK1] = {DIMS0};  /* shrinking dimensions */
    hsize_t dims_e[RANK1] = {DIME0};  /* extended dimensions */
    hsize_t dims_c[RANK1] = {2};      /* chunk dimensions */
    hsize_t dims_r[RANK1];            /* read dimensions */
    hsize_t maxdims[RANK1] = {H5S_UNLIMITED};
    int     buf_o[DIM0];
    int     buf_s[DIMS0];
    int     buf_e[DIME0];
    int     buf_r[DIM0];
    int     i;
    int     comp_value;
    char    filename[NAME_BUF_SIZE];

    if ( do_fill_value )
    {
        comp_value = FILL_VALUE;
    }
    else
    {
        comp_value = 0;
    }


    for( i = 0; i < DIM0; i++ )
    {

        buf_o[i] = 2;
    }

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
    {
        TEST_ERROR
    }

    if  ( set_istore_k )
    {
        /* set non-default indexed storage B-tree internal 'K' value */
        if (H5Pset_istore_k(fcpl,ISTORE_IK) < 0)
        {
            TEST_ERROR
        }

    }
    /* create a new file */
    h5_fixname(FILENAME[0], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
    {
        TEST_ERROR
    }

    /* close property list */
    if(H5Pclose(fcpl) < 0)
    {
        TEST_ERROR
    }

    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK1, dims_o, maxdims)) < 0)
    {
        TEST_ERROR
    }

    /* modify dataset creation properties, i.e. enable chunking. */
    if ((my_dcpl = H5Pcopy (dcpl)) < 0)
    {
        TEST_ERROR
    }
    if (H5Pset_chunk(my_dcpl, RANK1, dims_c) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * create, write dataset
    *-------------------------------------------------------------------------
    */

    /* create a dataset */
    if ((did = H5Dcreate2(fid , "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, my_dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }

    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0)
    {
        TEST_ERROR
    }


#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {

        printf("%d ", buf_o[i]);

    }
    printf("\n");
#endif



    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }


    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_e[i])
            TEST_ERROR
    }

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0)
        TEST_ERROR



#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {

        printf("%d ", buf_e[i]);

    }
    printf("\n");
#endif




    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {

        if ( i >= DIM0  )
        {
            if(buf_e[i] != comp_value)
            {
                printf("buf_e[%d] = %d\n", i, buf_e[i]);
                printf("value = %d\n", comp_value);
                TEST_ERROR
            }
        }
        else
        {
            if(buf_e[i] != buf_o[i])
                TEST_ERROR
        }
    }




    /*-------------------------------------------------------------------------
    * shrink
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }



    /* for this case we close and reopen file */
    if ( set_istore_k )
    {

        if (H5Dclose(did) < 0)
        {
            TEST_ERROR
        }
        if (H5Fclose(fid) < 0)
        {
            TEST_ERROR
        }

        if ((fid = H5Fopen( filename, H5F_ACC_RDWR, fapl ))<0)
        {
            TEST_ERROR
        }

        if ((did = H5Dopen2( fid , "dset1", H5P_DEFAULT ))<0)
        {
            TEST_ERROR
        }



    }


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0)
    {
        TEST_ERROR
    }

#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {

        printf("%d ", buf_s[i]);
    }
    printf("\n");

#endif




    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {

        if (  buf_s[i] != buf_o[i] )
        {
            printf("buf_s[%d] = %d\n", i, buf_s[i]);
            printf("buf_o[%d] = %d\n", i, buf_o[i]);
            TEST_ERROR
        }
    }


    /*-------------------------------------------------------------------------
    * expand it back to original size
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array */
    if (H5Dset_extent(did, dims_o) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions. */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }


    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_o[i])
            TEST_ERROR
    }


    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0)
        TEST_ERROR

#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {

        printf("%d ", buf_r[i]);

    }
    printf("\n");
#endif



    /* compare the read array with the original array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {

        if (i >= DIMS0 )
        {
            if(buf_r[i] != comp_value)
            {
                printf("buf_r[%d] = %d\n", i, buf_r[i] );
                printf("value = %d\n", comp_value);
                TEST_ERROR
            }
        }
        else
        {
            if(buf_r[i] != buf_o[i])
                TEST_ERROR
        }
    }


    /*-------------------------------------------------------------------------
    * shrink to 0
    *
    *-------------------------------------------------------------------------
    */

    dims_s[0] = 0;

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK1; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * close dataset
    *-------------------------------------------------------------------------
    */

    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }





     /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */


    if ((sid = H5Screate_simple(RANK1, dims_o, maxdims)) < 0)
    {
        TEST_ERROR
    }
    if ((did = H5Dcreate2(fid , "dset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, my_dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }
    /* set new dimensions for the array */
    dims_o[ 0 ] = 0;
    if (H5Dset_extent( did , dims_o ) < 0)
    {
        TEST_ERROR
    }



    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }
    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }




    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */


    if (H5Pclose(my_dcpl) < 0)
    {
        TEST_ERROR
    }

    if (H5Fclose( fid ) < 0)
    {
        TEST_ERROR
    }


    return 0;



error:

    H5E_BEGIN_TRY
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( my_dcpl  );
        H5Pclose( fcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;

}

/*-------------------------------------------------------------------------
 * test usage with a 2D rank
 *-------------------------------------------------------------------------
 */

static int test_rank2( hid_t fapl,
                       hid_t dcpl,
                       hbool_t do_fill_value,
                       hbool_t set_istore_k)
{

    hid_t   fid=-1;
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   my_dcpl=-1;
    hid_t   fcpl;
    hsize_t dims_o[RANK2] = {DIM0,DIM1};    /* original dimensions */
    hsize_t dims_s[RANK2] = {DIMS0,DIMS1};  /* shrinking dimensions */
    hsize_t dims_e[RANK2] = {DIME0,DIME1};  /* extended dimensions */
    hsize_t dims_c[RANK2] = {2,2};          /* chunk dimensions */
    hsize_t dims_r[RANK2];                  /* read dimensions */
    hsize_t maxdims[RANK2] = {H5S_UNLIMITED,H5S_UNLIMITED};
    int     buf_o[DIM0][DIM1];
    int     buf_s[DIMS0][DIMS1];
    int     buf_e[DIME0][DIME1];
    int     buf_r[DIM0][DIM1];
    int     i, j;
    int     comp_value;
    char    filename[NAME_BUF_SIZE];

    if ( do_fill_value )
    {
        comp_value = FILL_VALUE;
    }
    else
    {
        comp_value = 0;
    }


    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            buf_o[i][j] = 2;
        }
    }

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
    {
        TEST_ERROR
    }

    if  ( set_istore_k )
    {
        /* set non-default indexed storage B-tree internal 'K' value */
        if (H5Pset_istore_k(fcpl,ISTORE_IK) < 0)
        {
            TEST_ERROR
        }

    }

    /* create a new file */
    h5_fixname(FILENAME[1], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
    {
        TEST_ERROR
    }



    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK2, dims_o, maxdims)) < 0)
    {
        TEST_ERROR
    }

    /* modify dataset creation properties, i.e. enable chunking. */
    if ((my_dcpl = H5Pcopy (dcpl)) < 0)
    {
        TEST_ERROR
    }
    if (H5Pset_chunk(my_dcpl, RANK2, dims_c) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * Procedure 1
    * a.	Write an array AxB. These are the dimensions for creating the dataset
    * b.	Define a greater array CxD where C > A and D > B
    * c.	Read data back
    * d.	Verify if new dimensions are C and D
    * e.	Verify if data from A to C and B to D is what it is to be expected
    *
    * original data is
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *
    *-------------------------------------------------------------------------
    */

    /* create a dataset */
    if ((did = H5Dcreate2(fid , "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, my_dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }

    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0)
    {
        TEST_ERROR
    }


#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            printf("%d ", buf_o[i][j]);
        }
        printf("\n");
    }
#endif



    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    * data is now, extended space was initialized with fill value or default value
    *
    *  2 2 2 2 1 1 1
    *  2 2 2 2 1 1 1
    *  2 2 2 2 1 1 1
    *  2 2 2 2 1 1 1
    *  1 1 1 1 1 1 1
    *  1 1 1 1 1 1 1
    *  1 1 1 1 1 1 1
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }


    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_e[i])
            TEST_ERROR
    }

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0)
        TEST_ERROR



#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_e[i][j]);
        }
        printf("\n");
    }
#endif




    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            if ( i >= DIM0 || j >= DIM1 )
            {
                if(buf_e[i][j] != comp_value)
                {
                    printf("buf_e[%d][%d] = %d\n", i, j, buf_e[i][j]);
                    printf("value = %d\n", comp_value);
                    TEST_ERROR
                }
            }
            else
            {
                if(buf_e[i][j] != buf_o[i][j])
                    TEST_ERROR
            }
        }
    }




    /*-------------------------------------------------------------------------
    *
    * Procedure 2
    * a.	Define a smaller array ExF where E < A and F < B
    * b.	Read data back
    * c.	Verify if new dimensions are E and F
    * d.	Verify if data up until E and F is what to be expected
    *
    * data is now
    *
    *  2 2
    *  2 2
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }



    /* for this case we close and reopen file */
    if ( set_istore_k )
    {

        if (H5Dclose(did) < 0)
        {
            TEST_ERROR
        }
        if (H5Fclose(fid) < 0)
        {
            TEST_ERROR
        }

        if ((fid = H5Fopen( filename, H5F_ACC_RDWR, fapl ))<0)
        {
            TEST_ERROR
        }

        if ((did = H5Dopen2( fid , "dset1", H5P_DEFAULT ))<0)
        {
            TEST_ERROR
        }



    }


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0)
    {
        TEST_ERROR
    }

#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_s[i][j]);
        }
        printf("\n");
    }
#endif




    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        for( j = 0; j < (int)dims_r[1]; j++ )
        {
            if (  buf_s[i][j] != buf_o[i][j] )
            {
                printf("buf_s[%d][%d] = %d\n", i, j, buf_s[i][j]);
                printf("buf_o[%d][%d] = %d\n", i, j, buf_o[i][j]);
                TEST_ERROR
            }
        }
    }


    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it back to original size
    * data is now, extended space was initialized with fill value or default value
    *
    *  2 2 1 1
    *  2 2 1 1
    *  1 1 1 1
    *  1 1 1 1
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array */
    if (H5Dset_extent(did, dims_o) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions. */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_o[i])
            TEST_ERROR
    }


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0)
        TEST_ERROR

#if defined (H5_SET_EXTENT_DEBUG2)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_r[i][j]);
        }
        printf("\n");
    }
#endif



    /* compare the read array with the original array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            if (i >= DIMS0 || j >= DIMS1)
            {
                if(buf_r[i][j] != comp_value)
                {
                    printf("buf_r[%d][%d] = %d\n", i, j, buf_r[i][j]);
                    printf("value = %d\n", comp_value);
                    TEST_ERROR
                }
            }
            else
            {
                if(buf_r[i][j] != buf_o[i][j])
                    TEST_ERROR
            }
        }
    }

    /*-------------------------------------------------------------------------
    * shrink to 0
    *
    *-------------------------------------------------------------------------
    */

    dims_s[0] = 0;
    dims_s[1] = 0;

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }


    /*-------------------------------------------------------------------------
    * expand then shrink to 0 in dimension 1 while expanding again in
    * dimension 0
    *
    *-------------------------------------------------------------------------
    */


    /* expand to original dimensions for the array. */
    if (H5Dset_extent(did , dims_o) < 0)
    {
        TEST_ERROR
    }

    dims_s[0] = dims_e[0];
    dims_s[1] = 0;

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }


    /*-------------------------------------------------------------------------
    * close dataset
    *-------------------------------------------------------------------------
    */

    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }


     /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */


    if ((sid = H5Screate_simple(RANK2, dims_o, maxdims)) < 0)
    {
        TEST_ERROR
    }
    if ((did = H5Dcreate2(fid , "dset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, my_dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }
    /* set new dimensions for the array */
    dims_o[ 0 ] = 0;
    dims_o[ 1 ] = 0;
    if (H5Dset_extent( did , dims_o ) < 0)
    {
        TEST_ERROR
    }



    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }
    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }




    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */


    if (H5Pclose(my_dcpl) < 0)
    {
        TEST_ERROR
    }

    /* close file creation property list */
    if(H5Pclose(fcpl) < 0)
    {
        TEST_ERROR
    }

    if (H5Fclose( fid ) < 0)
    {
        TEST_ERROR
    }


    return 0;



error:

    H5E_BEGIN_TRY
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( my_dcpl  );
        H5Pclose( fcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;

}




/*-------------------------------------------------------------------------
 * test usage with a 3D rank
 *-------------------------------------------------------------------------
 */

static int test_rank3( hid_t fapl,
                       hid_t dcpl,
                       hbool_t do_fill_value,
                       hbool_t set_istore_k)
{

    hid_t   fid=-1;
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   my_dcpl=-1;
    hid_t   fcpl;
    hsize_t dims_o[RANK3] = {DIM0,DIM1,DIM2};    /* original dimensions */
    hsize_t dims_s[RANK3] = {DIMS0,DIMS1,DIMS2}; /* shrinking dimensions */
    hsize_t dims_e[RANK3] = {DIME0,DIME1,DIME2}; /* extended dimensions */
    hsize_t dims_c[RANK3] = {2,2,2};             /* chunk dimensions */
    hsize_t dims_r[RANK3];                       /* read dimensions */
    hsize_t maxdims[RANK3] = {H5S_UNLIMITED,H5S_UNLIMITED,H5S_UNLIMITED};
    int     buf_o[DIM0][DIM1][DIM2];
    int     buf_s[DIMS0][DIMS1][DIMS2];
    int     buf_e[DIME0][DIME1][DIME2];
    int     buf_r[DIM0][DIM1][DIM2];
    int     i, j, k;
    int     comp_value;
    char	filename[NAME_BUF_SIZE];

    if ( do_fill_value )
    {
        comp_value = FILL_VALUE;
    }
    else
    {
        comp_value = 0;
    }


    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            for( k = 0; k < DIM2; k++ )
            {
                buf_o[i][j][k] = 2;
            }
        }
    }

    /* create a file creation property list */
    if ((fcpl = H5Pcreate(H5P_FILE_CREATE)) < 0)
    {
        TEST_ERROR
    }

    if  ( set_istore_k )
    {
        /* set non-default indexed storage B-tree internal 'K' value */
        if (H5Pset_istore_k(fcpl,ISTORE_IK) < 0)
        {
            TEST_ERROR
        }

    }
    /* create a new file */
    h5_fixname(FILENAME[2], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, fcpl, fapl)) < 0)
    {
        TEST_ERROR
    }

    /* close property list */
    if(H5Pclose(fcpl) < 0)
    {
        TEST_ERROR
    }

    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK3, dims_o, maxdims)) < 0)
    {
        TEST_ERROR
    }

    /* modify dataset creation properties, i.e. enable chunking. */
    if ((my_dcpl = H5Pcopy (dcpl)) < 0)
    {
        TEST_ERROR
    }
    if (H5Pset_chunk(my_dcpl, RANK3, dims_c) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * create, write array
    *-------------------------------------------------------------------------
    */

    /* create a dataset */
    if ((did = H5Dcreate2(fid , "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, my_dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }

    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0)
    {
        TEST_ERROR
    }


#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            for( k = 0; k < (int)dims_o[2]; k++ )
            {
                printf("%d ", buf_o[i][j][k]);
            }
            printf("[%d] ", j);
        }
        printf("\n");

    }
    printf("\n");
#endif



    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }


    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_e[i])
            TEST_ERROR
    }

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0)
        TEST_ERROR



#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                printf("%d ", buf_e[i][j][k]);
            }
            printf("[%d] ", j);
        }
        printf("\n");

    }
    printf("\n");
#endif




    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                if ( i >= DIM0 || j >= DIM1 || k >= DIM2 )
                {
                    if(buf_e[i][j][k] != comp_value)
                    {
                        printf("buf_e[%d][%d][%d] = %d\n", i, j, k, buf_e[i][j][k] );
                        printf("value = %d\n", comp_value);
                        TEST_ERROR
                    }
                }
                else
                {
                    if(buf_e[i][j][k] != buf_o[i][j][k] )
                        TEST_ERROR
                }
            }
        }
    }




    /*-------------------------------------------------------------------------
    * shrink
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }



    /* for this case we close and reopen file */
    if ( set_istore_k )
    {

        if (H5Dclose(did) < 0)
        {
            TEST_ERROR
        }
        if (H5Fclose(fid) < 0)
        {
            TEST_ERROR
        }

        if ((fid = H5Fopen( filename, H5F_ACC_RDWR, fapl ))<0)
        {
            TEST_ERROR
        }

        if ((did = H5Dopen2( fid , "dset1", H5P_DEFAULT ))<0)
        {
            TEST_ERROR
        }



    }


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0)
    {
        TEST_ERROR
    }

#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                printf("%d ", buf_s[i][j][k]);
            }
             printf("[%d] ", j);
        }
        printf("\n");

    }
    printf("\n");
#endif




    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        for( j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                if (  buf_s[i][j][k] != buf_o[i][j][k] )
                {
                    printf("buf_s[%d][%d][%d] = %d\n", i, j, k, buf_s[i][j][k] );
                    printf("buf_o[%d][%d][%d] = %d\n", i, j, k, buf_o[i][j][k] );
                    TEST_ERROR
                }
            }
        }
    }


    /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it back to original size
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array */
    if (H5Dset_extent(did, dims_o) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions. */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_o[i])
            TEST_ERROR
    }

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0)
        TEST_ERROR

#if defined (H5_SET_EXTENT_DEBUG3)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {

                printf("%d ", buf_r[i][j][k]);
            }
             printf("[%d] ", j);
        }
        printf("\n");

    }
    printf("\n");
#endif



    /* compare the read array with the original array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            for( k = 0; k < (int)dims_r[2]; k++ )
            {
                if (i >= DIMS0 || j >= DIMS1 || k >= DIMS2 )
                {
                    if( buf_r[i][j][k] != comp_value )
                    {
                        printf("buf_r[%d][%d][%d] = %d\n", i, j, k, buf_r[i][j][k] );
                        printf("value = %d\n", comp_value);
                        TEST_ERROR
                    }
                }
                else
                {
                    if(buf_r[i][j][k] != buf_o[i][j][k])
                        TEST_ERROR
                }
            }
        }
    }


    /*-------------------------------------------------------------------------
    * shrink to 0
    *
    *-------------------------------------------------------------------------
    */

    dims_s[0] = 0;
    dims_s[1] = 0;
    dims_s[2] = 0;

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
    {
        TEST_ERROR
    }

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK3; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }



    /*-------------------------------------------------------------------------
    * close dataset
    *-------------------------------------------------------------------------
    */

    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }


    /*-------------------------------------------------------------------------
    * test a dataset with non initialized chunks
    *-------------------------------------------------------------------------
    */


    if ((sid = H5Screate_simple(RANK3, dims_o, maxdims)) < 0)
    {
        TEST_ERROR
    }
    if ((did = H5Dcreate2(fid , "dset3", H5T_NATIVE_INT, sid, H5P_DEFAULT, my_dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }
    /* set new dimensions for the array */
    dims_o[ 0 ] = 0;
    dims_o[ 1 ] = 0;
    dims_o[ 2 ] = 0;
    if (H5Dset_extent( did , dims_o ) < 0)
    {
        TEST_ERROR
    }



    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }
    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }




    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */


    if (H5Pclose(my_dcpl) < 0)
    {
        TEST_ERROR
    }

    if (H5Fclose( fid ) < 0)
    {
        TEST_ERROR
    }


    return 0;



error:

    H5E_BEGIN_TRY
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( my_dcpl  );
        H5Pclose( fcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;

}


/*-------------------------------------------------------------------------
 * test usage with external storage
 *-------------------------------------------------------------------------
 */
static int test_external( hid_t fapl )
{

    hid_t   fid=-1;
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    hsize_t dims_o[RANK2] = {DIM0,DIM1};    /* original dimensions */
    hsize_t dims_s[RANK2] = {DIMS0,DIMS1};  /* shrinking dimensions */
    hsize_t dims_e[RANK2] = {DIME0,DIM1};   /* extended dimensions, dimension 1 is the original */
    hsize_t dims_r[RANK2];                  /* read dimensions */
    hsize_t maxdims[RANK2] = {DIME0,DIM1};  /* only the first dimension can be extendible */
    int     buf_o[DIM0][DIM1];              /* original buffer, for writing */
    int     buf_s[DIMS0][DIMS1];            /* shrinked buffer, for reading */
    int     buf_e[DIME0][DIM1];             /* extended buffer, for writing, dimension 1 is the original */
    int     buf_ro[DIM0][DIM1];             /* original buffer for reading */
    int     i, j;
    int     comp_value = 0;
    char	filename[NAME_BUF_SIZE];


    hsize_t size;        /* number of bytes reserved in the file for the data */
    hsize_t max_size[2];

    max_size[0] = dims_e[0];
    max_size[1] = dims_e[1];
    size = max_size[0] * max_size[1] * sizeof(int) / 2;


    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            buf_o[i][j] = 2;
        }
    }

    TESTING("external file use");

    /* create a new file */
    h5_fixname(FILENAME[3], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        FAIL_STACK_ERROR

    /* modify dataset creation properties */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
        FAIL_STACK_ERROR

    if(H5Pset_external(dcpl, EXT_FILE_NAME1, (off_t)0, size) < 0)
        FAIL_STACK_ERROR

    if(H5Pset_external(dcpl, EXT_FILE_NAME2, (off_t)0, size) < 0)
        FAIL_STACK_ERROR

    {

        char	name[256];		/*external file name		*/
        off_t	file_offset;    /*external file offset		*/
        hsize_t	file_size;		/*sizeof external file segment	*/

        if(H5Pget_external(dcpl, 0, sizeof(name), name, &file_offset,
                &file_size) < 0)
            FAIL_STACK_ERROR

    }



    /*-------------------------------------------------------------------------
    * Write an array AxB. These are the dimensions for creating the dataset
    *
    * original data is
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *
    *-------------------------------------------------------------------------
    */


    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK2, dims_o, maxdims)) < 0)
        FAIL_STACK_ERROR
    if ((did = H5Dcreate2(fid , "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
        FAIL_STACK_ERROR
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0)
        FAIL_STACK_ERROR
    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

     /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_ro) < 0)
        FAIL_STACK_ERROR

#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            printf("%d ", buf_ro[i][j]);
        }
        printf("\n");
    }
#endif



    /*-------------------------------------------------------------------------
    * expand
    *-------------------------------------------------------------------------
    */

     /*-------------------------------------------------------------------------
    * set new dimensions for the array; expand it
    * data is now, extended space was initialized with default value
    *
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  2 2 2 2
    *  0 0 0 0
    *  0 0 0 0
    *  0 0 0 0
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_e) < 0)
        FAIL_STACK_ERROR

     /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
        FAIL_STACK_ERROR

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
        FAIL_STACK_ERROR

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR


    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_e[i])
            TEST_ERROR
    }

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_e) < 0)
        FAIL_STACK_ERROR



#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_e[i][j]);
        }
        printf("\n");
    }
#endif




    /* compare the read array with the expanded array */
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            if ( i >= DIM0 || j >= DIM1 )
            {
                if(buf_e[i][j] != comp_value)
                {
                    printf("buf_e[%d][%d] = %d\n", i, j, buf_e[i][j]);
                    printf("value = %d\n", comp_value);
                    TEST_ERROR
                }
            }
            else
            {
                if(buf_e[i][j] != buf_o[i][j])
                    TEST_ERROR
            }
        }
    }



     /*-------------------------------------------------------------------------
    * shrink
    *
    * data is now
    *
    *  2 2
    *  2 2
    *
    *-------------------------------------------------------------------------
    */

    /* set new dimensions for the array. */
    if (H5Dset_extent(did , dims_s) < 0)
        FAIL_STACK_ERROR

    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
        FAIL_STACK_ERROR

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
        FAIL_STACK_ERROR

    if (H5Sclose(sid) < 0)
        FAIL_STACK_ERROR

    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_s[i])
            TEST_ERROR
    }




    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_s ) < 0)
        FAIL_STACK_ERROR

#if defined (H5_SET_EXTENT_DEBUG)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_s[i][j]);
        }
        printf("\n");
    }
#endif




    /* compare the read array with the shrinked array */
    for( i = 0; i < (int)dims_r[0]; i++ )
    {
        for( j = 0; j < (int)dims_r[1]; j++ )
        {
            if (  buf_s[i][j] != buf_o[i][j] )
            {
                printf("buf_s[%d][%d] = %d\n", i, j, buf_s[i][j]);
                printf("buf_o[%d][%d] = %d\n", i, j, buf_o[i][j]);
                TEST_ERROR
            }
        }
    }


    /*-------------------------------------------------------------------------
    * negative test
    * try to extend dimension above maximum
    *-------------------------------------------------------------------------
    */


    dims_e[1] = DIME1;


    H5E_BEGIN_TRY
    {


        /* set new dimensions for the array. */
        if (H5Dset_extent(did , dims_e) == SUCCEED)
        {
            TEST_ERROR
        }

    } H5E_END_TRY;



    /*-------------------------------------------------------------------------
    * close property list
    *-------------------------------------------------------------------------
    */


    if (H5Pclose(dcpl) < 0)
        FAIL_STACK_ERROR

    if (H5Dclose( did ) < 0)
        FAIL_STACK_ERROR

    if (H5Fclose( fid ) < 0)
        FAIL_STACK_ERROR

    PASSED();

    return 0;

error:

    H5E_BEGIN_TRY
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;
}


/*-------------------------------------------------------------------------
 * test usage with layouts compact and contiguous
 *-------------------------------------------------------------------------
 */
static int test_layouts( H5D_layout_t layout, hid_t fapl )
{

    hid_t   fid=-1;
    hid_t   did=-1;
    hid_t   sid=-1;
    hid_t   dcpl=-1;
    herr_t  ret;
    hsize_t dims_o[RANK2] = {DIM0,DIM1};    /* original dimensions */
    hsize_t dims_s[RANK2] = {DIMS0,DIMS1};  /* shrinking dimensions */
    hsize_t dims_e[RANK2] = {DIME0,DIME1};  /* extended dimensions */
    hsize_t dims_r[RANK2];                  /* read dimensions */
    int     buf_o[DIM0][DIM1];
    int     buf_r[DIM0][DIM1];
    int     i, j;
    char	filename[NAME_BUF_SIZE];

    for( i = 0; i < DIM0; i++ )
    {
        for( j = 0; j < DIM1; j++ )
        {
            buf_o[i][j] = 2;
        }
    }


    /* create a new file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);
    if ((fid = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
    {
        TEST_ERROR
    }

    /* create the data space with unlimited dimensions. */
    if ((sid = H5Screate_simple(RANK2, dims_o, NULL)) < 0)
    {
        TEST_ERROR
    }

    /* modify dataset creation properties */
    if ((dcpl = H5Pcreate (H5P_DATASET_CREATE)) < 0)
    {
        TEST_ERROR
    }

    if (H5Pset_layout (dcpl, layout) < 0)
    {
        TEST_ERROR
    }

    /* create a dataset */
    if ((did = H5Dcreate2(fid , "dset1", H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT)) < 0)
    {
        TEST_ERROR
    }

    /* write */
    if (H5Dwrite(did , H5T_NATIVE_INT, sid, H5S_ALL, H5P_DEFAULT, buf_o) < 0)
    {
        TEST_ERROR
    }


#if defined (H5_SET_EXTENT_DEBUG4)
    printf("\n");
    for (i = 0; i < (int)dims_o[0]; i++ )
    {
        for (j = 0; j < (int)dims_o[1]; j++ )
        {
            printf("%d ", buf_o[i][j]);
        }
        printf("\n");
    }
#endif



    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /*-------------------------------------------------------------------------
    * negative test
    * try to extend dimension
    *-------------------------------------------------------------------------
    */

    H5E_BEGIN_TRY
    {

        ret = H5Dset_extent(did , dims_e);

    } H5E_END_TRY;

    if(ret >= 0)
        TEST_ERROR




    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }


    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_o[i])
            TEST_ERROR
    }

    /* read */
    if (H5Dread(did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r) < 0)
        TEST_ERROR



#if defined (H5_SET_EXTENT_DEBUG4)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_r[i][j]);
        }
        printf("\n");
    }
#endif



    /*-------------------------------------------------------------------------
    * negative test
    * try to shrink dimension
    *-------------------------------------------------------------------------
    */

    H5E_BEGIN_TRY
    {

        ret = H5Dset_extent(did , dims_s);

    } H5E_END_TRY;

    if(ret >= 0)
        TEST_ERROR


    /* get the space */
    if ((sid = H5Dget_space(did)) < 0)
    {
        TEST_ERROR
    }

    /* get dimensions */
    if (H5Sget_simple_extent_dims(sid, dims_r, NULL) < 0)
    {
        TEST_ERROR
    }

    if (H5Sclose(sid) < 0)
    {
        TEST_ERROR
    }

    /* check dimensions */
    for( i = 0; i < RANK2; i++ )
    {
        if (dims_r[i] != dims_o[i])
            TEST_ERROR
    }


    /*-------------------------------------------------------------------------
    * read
    *-------------------------------------------------------------------------
    */

    /* read */
    if (H5Dread( did, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, buf_r ) < 0)
    {
        TEST_ERROR
    }

#if defined (H5_SET_EXTENT_DEBUG4)
    printf("\n");
    for (i = 0; i < (int)dims_r[0]; i++ )
    {
        for (j = 0; j < (int)dims_r[1]; j++ )
        {
            printf("%d ", buf_r[i][j]);
        }
        printf("\n");
    }
#endif



    /*-------------------------------------------------------------------------
    * close
    *-------------------------------------------------------------------------
    */

    if (H5Dclose(did) < 0)
    {
        TEST_ERROR
    }

    if (H5Pclose(dcpl) < 0)
    {
        TEST_ERROR
    }

    if (H5Fclose( fid ) < 0)
    {
        TEST_ERROR
    }


    return 0;



error:

    H5E_BEGIN_TRY
    {
        H5Dclose( did );
        H5Sclose( sid );
        H5Pclose( dcpl  );
        H5Fclose( fid );
    } H5E_END_TRY;
    return -1;

}


/*-------------------------------------------------------------------------
 * Function:    test_random_rank4
 *
 * Purpose:     Test expanding and shrinking a rank 4 dataset in a
 *              randomized fashion.  Verifies that data is preserved (and
 *              filled, if do_fillvalue is true) as expected.
 *
 * Return:      Success:        0
 *              Failure:        -1
 *
 * Programmer:  Neil Fortner
 *              Monday, January 11, 2010
 *
 *-------------------------------------------------------------------------
 */
static int test_random_rank4( hid_t fapl, hid_t dcpl, hbool_t do_fillvalue,
    hbool_t do_sparse )
{
    hid_t       file = -1;
    hid_t       dset = -1;
    hid_t       fspace = -1;
    hid_t       mspace = -1;
    hid_t       my_dcpl = -1;
    hsize_t     dims[4];                        /* Dataset's dimensions */
    hsize_t     old_dims[4];                    /* Old dataset dimensions */
    hsize_t     min_unwritten_dims[4];          /* Minimum dimensions since last write */
    hsize_t     *valid_dims = old_dims;         /* Dimensions of region still containing written data */
    hsize_t     cdims[4];                       /* Chunk dimensions */
    const hsize_t mdims[4] = {10, 10, 10, 10};  /* Memory buffer dimensions */
    const hsize_t start[4] = {0, 0, 0, 0};      /* Start for hyperslabe operations on memory */
    static int  rbuf[10][10][10][10];           /* Read buffer */
    static int  wbuf[10][10][10][10];           /* Write buffer */
    static hsize_t dim_log[RAND4_NITER+1][4];   /* Log of dataset dimensions */
    hbool_t     zero_dim = FALSE;               /* Whether a dimension is 0 */
    hbool_t     writing = TRUE;                 /* Whether we're writing to the dset */
    volatile unsigned i, j, k, l, m;            /* Local indices */
    char        filename[NAME_BUF_SIZE];

    /* create a new file */
    h5_fixname(FILENAME[4], fapl, filename, sizeof filename);
    if ((file = H5Fcreate(filename, H5F_ACC_TRUNC, H5P_DEFAULT, fapl)) < 0)
        TEST_ERROR

    /* Generate random chunk dimensions, 2-4 */
    for(i=0; i<4; i++)
        cdims[i] = (hsize_t)((HDrandom() % 3) + 2);

    /* Generate initial dataset size, 1-10 */
    for(i=0; i<4; i++) {
        dims[i] = (hsize_t)((HDrandom() % 10) + 1);
        dim_log[0][i] = dims[i];
    } /* end for */

    /* Create dataset */
    if((fspace = H5Screate_simple(4, dims, mdims)) < 0)
        TEST_ERROR
    if((my_dcpl = H5Pcopy(dcpl)) < 0)
        TEST_ERROR
    if(H5Pset_chunk(my_dcpl, 4, cdims) < 0)
        TEST_ERROR
    if((dset = H5Dcreate2(file, "dset", H5T_NATIVE_INT, fspace, H5P_DEFAULT,
            my_dcpl, H5P_DEFAULT)) < 0)
        TEST_ERROR
    if(H5Sclose(fspace) < 0)
        TEST_ERROR

    /* Create memory space, and set initial selection */
    if((mspace = H5Screate_simple(4, mdims, NULL)) < 0)
        TEST_ERROR
    if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start, NULL, dims, NULL)
            < 0)
        TEST_ERROR

    /* Main loop */
    for(i=0; i<RAND4_NITER; i++) {

        /* Generate random write buffer */
        if(writing && !zero_dim) {
            for(j=0; j<dims[0]; j++)
                for(k=0; k<dims[1]; k++)
                    for(l=0; l<dims[2]; l++)
                        for(m=0; m<dims[3]; m++)
                            wbuf[j][k][l][m] = HDrandom();

            /* Write data */
            if(H5Dwrite(dset, H5T_NATIVE_INT, mspace, H5S_ALL, H5P_DEFAULT,
                    wbuf) < 0)
                RAND4_FAIL_DUMP(i+1, -1, -1, -1, -1)
        } /* end if */

        /* Generate new dataset size, 0-10 (0 much less likely) */
        zero_dim = FALSE;
        for(j=0; j<4; j++) {
            old_dims[j] = dims[j];
            if((dims[j] = (hsize_t)(HDrandom() % 11)) == 0)
                if((dims[j] = (hsize_t)(HDrandom() % 11)) == 0)
                    zero_dim = TRUE;
            dim_log[i+1][j] = dims[j];
        } /* end for */

        /* If writing is disabled, update min_unwritten_dims */
        if(!writing)
            for(j=0; j<4; j++)
                if(old_dims[j] < min_unwritten_dims[j])
                    min_unwritten_dims[j] = old_dims[j];

        /* Resize dataset */
        if(H5Dset_extent(dset, dims) < 0)
            RAND4_FAIL_DUMP(i+2, -1, -1, -1, -1)

        if(!zero_dim) {
            /* Read data from resized dataset */
            if(H5Sselect_hyperslab(mspace, H5S_SELECT_SET, start, NULL, dims,
                    NULL) < 0)
                RAND4_FAIL_DUMP(i+2, -1, -1, -1, -1)
            if(H5Dread(dset, H5T_NATIVE_INT, mspace, H5S_ALL, H5P_DEFAULT, rbuf)
                    < 0)
                RAND4_FAIL_DUMP(i+2, -1, -1, -1, -1)

            /* Verify correctness of read data */
            if(do_fillvalue) {
                for(j=0; j<dims[0]; j++)
                    for(k=0; k<dims[1]; k++)
                        for(l=0; l<dims[2]; l++)
                            for(m=0; m<dims[3]; m++)
                                if(j >= valid_dims[0] || k >= valid_dims[1]
                                        || l >= valid_dims[2]
                                        || m >= valid_dims[3]) {
                                    if(FILL_VALUE != rbuf[j][k][l][m])
                                        RAND4_FAIL_DUMP(i+2, (int)j, (int)k, (int)l, (int)m)
                                } /* end if */
                                else
                                    if(wbuf[j][k][l][m] != rbuf[j][k][l][m])
                                        RAND4_FAIL_DUMP(i+2, (int)j, (int)k, (int)l, (int)m)
            } /* end if */
            else {
                for(j=0; j<MIN(dims[0],valid_dims[0]); j++)
                    for(k=0; k<MIN(dims[1],valid_dims[1]); k++)
                        for(l=0; l<MIN(dims[2],valid_dims[2]); l++)
                            for(m=0; m<MIN(dims[3],valid_dims[3]); m++)
                                if(wbuf[j][k][l][m] != rbuf[j][k][l][m])
                                    RAND4_FAIL_DUMP(i+2, (int)j, (int)k, (int)l, (int)m)
            } /* end else */
        } /* end if */

        /* Handle the switch between writing and not writing */
        if(do_sparse && !(i % RAND4_SPARSE_SWITCH)) {
            writing = !writing;
            if(!writing) {
                for(j=0; j<4; j++)
                    min_unwritten_dims[j] = old_dims[j];
                valid_dims = min_unwritten_dims;
            } /* end if */
            else
                valid_dims = old_dims;
        } /* end if */
    } /* end for */

    /* Close */
    if(H5Sclose(mspace) < 0)
        TEST_ERROR
    if(H5Pclose(my_dcpl) < 0)
        TEST_ERROR
    if(H5Dclose(dset) < 0)
        TEST_ERROR
    if(H5Fclose(file) < 0)
        TEST_ERROR

    return 0;

error:
    H5E_BEGIN_TRY {
        H5Sclose(fspace);
        H5Sclose(mspace);
        H5Pclose(dcpl);
        H5Dclose(dset);
        H5Fclose(file);
    } H5E_END_TRY
    return -1;
} /* end test_random_rank4 */

/*
 * test_random_rank4_dump: Dump debugging info from test_random_rank4 to screen
 * after failure.
 */
static void test_random_rank4_dump( unsigned ndim_sets, hsize_t dim_log[][4],
        hsize_t cdims[4], int j, int k, int l, int m )
{
    unsigned i;

    printf("  Chunk dimensions: ( %u, %u, %u, %u )\n", (unsigned)cdims[0],
            (unsigned)cdims[1], (unsigned)cdims[2], (unsigned)cdims[3]);
    printf("  Log of dataset dimensions (oldest first):\n");
    for(i=0; i<ndim_sets; i++)
        printf("  Iteration %-3u: ( %2u, %2u, %2u, %2u )\n", i,
                (unsigned)dim_log[i][0], (unsigned)dim_log[i][1],
                (unsigned)dim_log[i][2], (unsigned)dim_log[i][3]);
    if(j>=0)
        printf("  First incorrect value read: ( %d, %d, %d, %d )\n", j, k, l,
                m);

    return;
} /* end test_random_rank4_dump */

