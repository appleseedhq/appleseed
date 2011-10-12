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
 *   This example shows how to read data from a chunked dataset.
 *   We will read from the file created by h5_extend_write.c
 */

#include "hdf5.h"

#define H5FILE_NAME        "SDSextendible.h5"
#define DATASETNAME "ExtendibleArray"
#define RANK         2
#define RANKC        1
#define NX           10
#define NY           5

int
main (void)
{
    hid_t       file;                        /* handles */
    hid_t       dataset;
    hid_t       filespace;
    hid_t       memspace;
    hid_t       cparms;
    hsize_t     dims[2];                     /* dataset and chunk dimensions*/
    hsize_t     chunk_dims[2];
    hsize_t     col_dims[1];
    hsize_t     count[2];
    hsize_t     offset[2];

    herr_t      status, status_n;

    int         data_out[NX][NY];  /* buffer for dataset to be read */
    int         chunk_out[2][5];   /* buffer for chunk to be read */
    int         column[10];        /* buffer for column to be read */
    int         rank, rank_chunk;
    int		i, j;



    /*
     * Open the file and the dataset.
     */
    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT);

    /*
     * Get dataset rank and dimension.
     */

    filespace = H5Dget_space(dataset);    /* Get filespace handle first. */
    rank      = H5Sget_simple_extent_ndims(filespace);
    status_n  = H5Sget_simple_extent_dims(filespace, dims, NULL);
    printf("dataset rank %d, dimensions %lu x %lu\n",
	   rank, (unsigned long)(dims[0]), (unsigned long)(dims[1]));

    /*
     * Define the memory space to read dataset.
     */
    memspace = H5Screate_simple(RANK,dims,NULL);

    /*
     * Read dataset back and display.
     */
    status = H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace,
		     H5P_DEFAULT, data_out);
    printf("\n");
    printf("Dataset: \n");
    for (j = 0; j < dims[0]; j++) {
	for (i = 0; i < dims[1]; i++) printf("%d ", data_out[j][i]);
	printf("\n");
    }

    /*
     * Close/release resources.
     */
    H5Sclose(memspace);

    /*
     *	    dataset rank 2, dimensions 10 x 5
     *	    chunk rank 2, dimensions 2 x 5

     *	    Dataset:
     *	    1 1 1 3 3
     *	    1 1 1 3 3
     *	    1 1 1 0 0
     *	    2 0 0 0 0
     *	    2 0 0 0 0
     *	    2 0 0 0 0
     *	    2 0 0 0 0
     *	    2 0 0 0 0
     *	    2 0 0 0 0
     *	    2 0 0 0 0
     */

    /*
     * Read the third column from the dataset.
     * First define memory dataspace, then define hyperslab
     * and read it into column array.
     */
    col_dims[0] = 10;
    memspace =  H5Screate_simple(RANKC, col_dims, NULL);

    /*
     * Define the column (hyperslab) to read.
     */
    offset[0] = 0;
    offset[1] = 2;
    count[0]  = 10;
    count[1]  = 1;
    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL,
				 count, NULL);
    status = H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace,
		     H5P_DEFAULT, column);
    printf("\n");
    printf("Third column: \n");
    for (i = 0; i < 10; i++) {
	printf("%d \n", column[i]);
    }

    /*
     * Close/release resources.
     */
    H5Sclose(memspace);

    /*
     *	    Third column:
     *	    1
     *	    1
     *	    1
     *	    0
     *	    0
     *	    0
     *	    0
     *	    0
     *	    0
     *	    0
     */

    /*
     * Get creation properties list.
     */
    cparms = H5Dget_create_plist(dataset); /* Get properties handle first. */

    if (H5D_CHUNKED == H5Pget_layout(cparms))  {

	/*
	 * Get chunking information: rank and dimensions
	 */
	rank_chunk = H5Pget_chunk(cparms, 2, chunk_dims);
	printf("chunk rank %d, dimensions %lu x %lu\n", rank_chunk,
	       (unsigned long)(chunk_dims[0]), (unsigned long)(chunk_dims[1]));

        /*
         * Define the memory space to read a chunk.
         */
        memspace = H5Screate_simple(rank_chunk,chunk_dims,NULL);

        /*
         * Define chunk in the file (hyperslab) to read.
         */
        offset[0] = 2;
        offset[1] = 0;
        count[0]  = chunk_dims[0];
        count[1]  = chunk_dims[1];
        status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL,
                                     count, NULL);

        /*
         * Read chunk back and display.
         */
        status = H5Dread(dataset, H5T_NATIVE_INT, memspace, filespace,
                         H5P_DEFAULT, chunk_out);
        printf("\n");
        printf("Chunk: \n");
        for (j = 0; j < chunk_dims[0]; j++) {
            for (i = 0; i < chunk_dims[1]; i++) printf("%d ", chunk_out[j][i]);
            printf("\n");
        }
        /*
         *	 Chunk:
         *	 1 1 1 0 0
         *	 2 0 0 0 0
         */

        /*
         * Close/release resources.
         */
        H5Sclose(memspace);
    }

    /*
     * Close/release resources.
     */
    H5Pclose(cparms);
    H5Dclose(dataset);
    H5Sclose(filespace);
    H5Fclose(file);

    return 0;
}
