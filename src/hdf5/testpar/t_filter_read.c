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
 * This verifies the correctness of parallel reading of a dataset that has been
 * written serially using filters.
 *
 * Created by: Christian Chilan
 * Date: 2007/05/15
 */


#include "testphdf5.h"

#ifdef H5_HAVE_SZLIB_H
#   include "szlib.h"
#endif

static int mpi_size, mpi_rank;

/* Chunk sizes */
#define CHUNK_DIM1       7
#define CHUNK_DIM2       27

/* Sizes of the vertical hyperslabs. Total dataset size is
   {HS_DIM1, HS_DIM2 * mpi_size } */
#define HS_DIM1       200
#define HS_DIM2       100


/*-------------------------------------------------------------------------
 * Function:	filter_read_internal
 *
 * Purpose:     Tests parallel reading of a 2D dataset written serially using
 *              filters. During the parallel reading phase, the dataset is
 *              divided evenly among the processors in vertical hyperslabs.
 *
 * Programmer:  Christian Chilan
 *              Tuesday, May 15, 2007
 *
 *-------------------------------------------------------------------------
 */
static void
filter_read_internal(const char *filename, hid_t dcpl,
                     hsize_t *dset_size)
{
    hid_t		file, dataset;        /* HDF5 IDs */
    hid_t		access_plist;         /* Access property list ID */
    hid_t		sid, memspace;            /* Dataspace IDs */
    hsize_t	        size[2];           /* Dataspace dimensions */
    hsize_t	        hs_offset[2]; /* Hyperslab offset */
    hsize_t	        hs_size[2];   /* Hyperslab size */
    size_t		i, j;        /* Local index variables */
    char                name[32] = "dataset";
    herr_t              hrc;         /* Error status */
    int                 *points = NULL; /* Writing buffer for entire dataset */
    int                 *check = NULL; /* Reading buffer for selected hyperslab */

    /* set up MPI parameters */
    MPI_Comm_size(MPI_COMM_WORLD,&mpi_size);
    MPI_Comm_rank(MPI_COMM_WORLD,&mpi_rank);

    /* set sizes for dataset and hyperslabs */
    hs_size[0] = size[0] = HS_DIM1;
    hs_size[1] = HS_DIM2;

    size[1] = hs_size[1] * mpi_size;

    hs_offset[0] = 0;
    hs_offset[1] = hs_size[1] * mpi_rank;

    /* Create the data space */
    sid = H5Screate_simple(2, size, NULL);
    VRFY(sid>=0, "H5Screate_simple");

    /* Create buffers */
    points = (int *)HDmalloc(size[0] * size[1] * sizeof(int));
    VRFY(points!=NULL, "HDmalloc");

    check = (int *)HDmalloc(hs_size[0] * hs_size[1] * sizeof(int));
    VRFY(check!=NULL, "HDmalloc");

    /* Initialize writing buffer with random data */
    for(i = 0; i < size[0]; i++)
        for(j = 0; j < size[1]; j++)
            points[i * size[1]+j] = (int)(i+j+7);

    VRFY(H5Pall_filters_avail(dcpl), "Incorrect filter availability");

    /* Serial write phase */
    if(MAINPROCESS) {

        file = H5Fcreate(h5_rmprefix(filename), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        VRFY(file>=0, "H5Fcreate");

        /* Create the dataset */
        dataset = H5Dcreate2(file, name, H5T_NATIVE_INT, sid, H5P_DEFAULT, dcpl, H5P_DEFAULT);
        VRFY(dataset>=0, "H5Dcreate2");

        hrc = H5Dwrite(dataset, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, points);
        VRFY(hrc>=0, "H5Dwrite");

        *dset_size = H5Dget_storage_size(dataset);
        VRFY(*dset_size>0, "H5Dget_storage_size");

        hrc = H5Dclose (dataset);
        VRFY(hrc>=0, "H5Dclose");

        hrc = H5Fclose (file);
        VRFY(hrc>=0, "H5Fclose");
    }

    MPI_Barrier(MPI_COMM_WORLD);

    /* Parallel read phase */
    /* Set up MPIO file access property lists */
    access_plist  = H5Pcreate(H5P_FILE_ACCESS);
    VRFY((access_plist >= 0), "H5Pcreate");

    hrc = H5Pset_fapl_mpio(access_plist, MPI_COMM_WORLD, MPI_INFO_NULL);
    VRFY((hrc >= 0), "H5Pset_fapl_mpio");

    /* Open the file */
    file = H5Fopen(filename, H5F_ACC_RDWR, access_plist);
    VRFY((file >= 0), "H5Fopen");

    dataset = H5Dopen2(file, name, H5P_DEFAULT);
    VRFY((dataset >= 0), "H5Dopen2");

    hrc = H5Sselect_hyperslab(sid, H5S_SELECT_SET, hs_offset, NULL, hs_size, NULL);
    VRFY(hrc>=0, "H5Sselect_hyperslab");

    memspace = H5Screate_simple(2, hs_size, NULL);
    VRFY(memspace>=0, "H5Screate_simple");

    hrc = H5Dread (dataset, H5T_NATIVE_INT, memspace, sid, H5P_DEFAULT, check);
    VRFY(hrc>=0, "H5Dread");

    /* Check that the values read are the same as the values written */
    for (i=0; i<hs_size[0]; i++) {
        for (j=0; j<hs_size[1]; j++) {
            if(points[i*size[1]+(size_t)hs_offset[1]+j] !=
                      check[i*hs_size[1]+j]) {
		  fprintf(stderr,"    Read different values than written.\n");
		  fprintf(stderr,"    At index %lu,%lu\n",
		         (unsigned long)(i),
		         (unsigned long)(hs_offset[1]+j));
		  fprintf(stderr,"    At original: %d\n",
		         (int)points[i*size[1]+(size_t)hs_offset[1]+j]);
		  fprintf(stderr,"    At returned: %d\n",
		         (int)check[i*hs_size[1]+j]);
                  VRFY(FALSE, "");
	    }
	}
    }

    /* Get the storage size of the dataset */
    *dset_size=H5Dget_storage_size(dataset);
    VRFY(*dset_size!=0, "H5Dget_storage_size");

    /* Clean up objects used for this test */
    hrc = H5Dclose (dataset);
    VRFY(hrc>=0, "H5Dclose");

    hrc = H5Sclose (sid);
    VRFY(hrc>=0, "H5Sclose");

    hrc = H5Sclose (memspace);
    VRFY(hrc>=0, "H5Sclose");

    hrc = H5Pclose (access_plist);
    VRFY(hrc>=0, "H5Pclose");

    hrc = H5Fclose (file);
    VRFY(hrc>=0, "H5Fclose");

    free(points);
    free(check);

    MPI_Barrier(MPI_COMM_WORLD);
}


/*-------------------------------------------------------------------------
 * Function:    test_filter_read
 *
 * Purpose:	Tests parallel reading of datasets written serially using
 *              several (combinations of) filters.
 *
 * Programmer:	Christian Chilan
 *              Tuesday, May 15, 2007
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */

void
test_filter_read(void)
{
    hid_t	dc;                 /* HDF5 IDs */
    const hsize_t chunk_size[2] = {CHUNK_DIM1, CHUNK_DIM2};  /* Chunk dimensions */
    hsize_t     null_size;          /* Size of dataset without filters */
    herr_t      hrc;
    const char *filename;
#ifdef H5_HAVE_FILTER_FLETCHER32
    hsize_t     fletcher32_size;       /* Size of dataset with Fletcher32 checksum */
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

    filename = GetTestParameters();

    if(VERBOSE_MED)
        printf("Parallel reading of dataset written with filters %s\n", filename);

    /*----------------------------------------------------------
     * STEP 0: Test without filters.
     *----------------------------------------------------------
     */
    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0,"H5Pcreate");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0,"H5Pset_chunk");

    filter_read_internal(filename,dc,&null_size);

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0,"H5Pclose");

    /*----------------------------------------------------------
     * STEP 1: Test Fletcher32 Checksum by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_FLETCHER32

    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0,"H5Pset_filter");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0,"H5Pset_filter");

    hrc = H5Pset_filter (dc,H5Z_FILTER_FLETCHER32,0,0,NULL);
    VRFY(hrc>=0,"H5Pset_filter");

    filter_read_internal(filename,dc,&fletcher32_size);
    VRFY(fletcher32_size > null_size,"Size after checksumming is incorrect.");

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0, "H5Pclose");

#endif /* H5_HAVE_FILTER_FLETCHER32 */

    /*----------------------------------------------------------
     * STEP 2: Test deflation by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_DEFLATE

    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0, "H5Pcreate");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0, "H5Pset_chunk");

    hrc = H5Pset_deflate (dc, 6);
    VRFY(hrc>=0, "H5Pset_deflate");

    filter_read_internal(filename,dc,&deflate_size);

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0, "H5Pclose");

#endif /* H5_HAVE_FILTER_DEFLATE */

    /*----------------------------------------------------------
     * STEP 3: Test szip compression by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SZIP
    if(h5_szip_can_encode() == 1) {
        dc = H5Pcreate(H5P_DATASET_CREATE);
        VRFY(dc>=0, "H5Pcreate");

        hrc = H5Pset_chunk (dc, 2, chunk_size);
        VRFY(hrc>=0, "H5Pset_chunk");

	hrc = H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block);
        VRFY(hrc>=0, "H5Pset_szip");

	filter_read_internal(filename,dc,&szip_size);

        /* Clean up objects used for this test */
        hrc = H5Pclose (dc);
        VRFY(hrc>=0, "H5Pclose");
    }
#endif /* H5_HAVE_FILTER_SZIP */

    /*----------------------------------------------------------
     * STEP 4: Test shuffling by itself.
     *----------------------------------------------------------
     */
#ifdef H5_HAVE_FILTER_SHUFFLE
    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0, "H5Pcreate");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0, "H5Pset_chunk");

    hrc = H5Pset_shuffle (dc);
    VRFY(hrc>=0, "H5Pset_shuffle");

    filter_read_internal(filename,dc,&shuffle_size);
    VRFY(shuffle_size==null_size,"Shuffled size not the same as uncompressed size.");

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0, "H5Pclose");

#endif /* H5_HAVE_FILTER_SHUFFLE */

    /*----------------------------------------------------------
     * STEP 5: Test shuffle + deflate + checksum in any order.
     *----------------------------------------------------------
     */
#if defined H5_HAVE_FILTER_DEFLATE && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32
    /* Testing shuffle+deflate+checksum filters (checksum first) */
    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0, "H5Pcreate");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0, "H5Pset_chunk");

    hrc = H5Pset_fletcher32 (dc);
    VRFY(hrc>=0, "H5Pset_fletcher32");

    hrc = H5Pset_shuffle (dc);
    VRFY(hrc>=0, "H5Pset_shuffle");

    hrc = H5Pset_deflate (dc, 6);
    VRFY(hrc>=0, "H5Pset_deflate");

    filter_read_internal(filename,dc,&combo_size);

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0, "H5Pclose");

    /* Testing shuffle+deflate+checksum filters (checksum last) */
    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0, "H5Pcreate");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0, "H5Pset_chunk");

    hrc = H5Pset_shuffle (dc);
    VRFY(hrc>=0, "H5Pset_shuffle");

    hrc = H5Pset_deflate (dc, 6);
    VRFY(hrc>=0, "H5Pset_deflate");

    hrc = H5Pset_fletcher32 (dc);
    VRFY(hrc>=0, "H5Pset_fletcher32");

    filter_read_internal(filename,dc,&combo_size);

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0, "H5Pclose");

#endif /* H5_HAVE_FILTER_DEFLATE && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */

    /*----------------------------------------------------------
     * STEP 6: Test shuffle + szip + checksum in any order.
     *----------------------------------------------------------
     */
#if defined H5_HAVE_FILTER_SZIP && defined H5_HAVE_FILTER_SHUFFLE && defined H5_HAVE_FILTER_FLETCHER32

    /* Testing shuffle+szip(with encoder)+checksum filters(checksum first) */
    dc = H5Pcreate(H5P_DATASET_CREATE);
    VRFY(dc>=0, "H5Pcreate");

    hrc = H5Pset_chunk (dc, 2, chunk_size);
    VRFY(hrc>=0, "H5Pset_chunk");

    hrc = H5Pset_fletcher32 (dc);
    VRFY(hrc>=0, "H5Pset_fletcher32");

    hrc = H5Pset_shuffle (dc);
    VRFY(hrc>=0, "H5Pset_shuffle");

    /* Make sure encoding is enabled */
    if(h5_szip_can_encode() == 1) {
	hrc = H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block);
        VRFY(hrc>=0, "H5Pset_szip");

	filter_read_internal(filename,dc,&combo_size);
    }

    /* Clean up objects used for this test */
    hrc = H5Pclose (dc);
    VRFY(hrc>=0, "H5Pclose");

    /* Testing shuffle+szip(with encoder)+checksum filters(checksum last) */
    /* Make sure encoding is enabled */
    if(h5_szip_can_encode() == 1) {
	dc = H5Pcreate(H5P_DATASET_CREATE);
        VRFY(dc>=0, "H5Pcreate");

	hrc = H5Pset_chunk (dc, 2, chunk_size);
        VRFY(hrc>=0, "H5Pset_chunk");

	hrc = H5Pset_shuffle (dc);
        VRFY(hrc>=0, "H5Pset_shuffle");

	hrc = H5Pset_szip(dc, szip_options_mask, szip_pixels_per_block);
        VRFY(hrc>=0, "H5Pset_szip");

	hrc = H5Pset_fletcher32 (dc);
        VRFY(hrc>=0, "H5Pset_fletcher32");

	filter_read_internal(filename,dc,&combo_size);

	/* Clean up objects used for this test */
	hrc = H5Pclose (dc);
        VRFY(hrc>=0, "H5Pclose");
    }

#endif /* H5_HAVE_FILTER_SZIP && H5_HAVE_FILTER_SHUFFLE && H5_HAVE_FILTER_FLETCHER32 */
}

