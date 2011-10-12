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
 *  This program shows how the H5Sselect_hyperslab and H5Sselect_elements
 *  functions are used to write selected data from memory to the file.
 *  Program takes 48 elements from the linear buffer and writes them into
 *  the matrix using 3x2 blocks, (4,3) stride and (2,4) count.
 *  Then four elements  of the matrix are overwritten with the new values and
 *  file is closed. Program reopens the file and selects the union of two
 *  hyperslabs in the dataset in the file. Then it reads the selection into the
 *  memory dataset preserving the shape of the selection.
 */

#include "hdf5.h"

#define H5FILE_NAME "Select.h5"

#define MSPACE1_RANK     1          /* Rank of the first dataset in memory */
#define MSPACE1_DIM      50         /* Dataset size in memory */

#define MSPACE2_RANK     1          /* Rank of the second dataset in memory */
#define MSPACE2_DIM      4          /* Dataset size in memory */

#define FSPACE_RANK      2          /* Dataset rank as it is stored in the file */
#define FSPACE_DIM1      8          /* Dimension sizes of the dataset as it is
                                       stored in the file */
#define FSPACE_DIM2      12

                                    /* We will read dataset back from the file
                                       to the dataset in memory with these
                                       dataspace parameters. */
#define MSPACE_RANK      2
#define MSPACE_DIM1      8
#define MSPACE_DIM2      9

#define NPOINTS          4          /* Number of points that will be selected
                                       and overwritten */
int
main (void)
{

   hid_t   file, dataset;           /* File and dataset identifiers */
   hid_t   mid1, mid2, mid, fid;    /* Dataspace identifiers */
   hid_t   plist;                   /* Dataset property list identifier */

   hsize_t dim1[] = {MSPACE1_DIM};  /* Dimension size of the first dataset
                                       (in memory) */
   hsize_t dim2[] = {MSPACE2_DIM};  /* Dimension size of the second dataset
                                       (in memory */
   hsize_t fdim[] = {FSPACE_DIM1, FSPACE_DIM2};
                                    /* Dimension sizes of the dataset (on disk) */
   hsize_t mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the
                                                   dataset in memory when we
                                                   read selection from the
                                                   dataset on the disk */

   hsize_t start[2];  /* Start of hyperslab */
   hsize_t stride[2]; /* Stride of hyperslab */
   hsize_t count[2];  /* Block count */
   hsize_t block[2];  /* Block sizes */

   hsize_t coord[NPOINTS][FSPACE_RANK]; /* Array to store selected points
                                            from the file dataspace */
   herr_t ret;
   unsigned i,j;
   int fillvalue = 0;   /* Fill value for the dataset */

   int    matrix_out[MSPACE_DIM1][MSPACE_DIM2]; /* Buffer to read from the
                                                   dataset */
   int    vector[MSPACE1_DIM];
   int    values[] = {53, 59, 61, 67};  /* New values to be written */

   /*
    * Buffers' initialization.
    */
   vector[0] = vector[MSPACE1_DIM - 1] = -1;
   for(i = 1; i < MSPACE1_DIM - 1; i++)
       vector[i] = i;

   /*
    * Create a file.
    */
   file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

   /*
    * Create property list for a dataset and set up fill values.
    */
   plist = H5Pcreate(H5P_DATASET_CREATE);
   ret   = H5Pset_fill_value(plist, H5T_NATIVE_INT, &fillvalue);

    /*
     * Create dataspace for the dataset in the file.
     */
    fid = H5Screate_simple(FSPACE_RANK, fdim, NULL);

    /*
     * Create dataset in the file. Notice that creation
     * property list plist is used.
     */
    dataset = H5Dcreate2(file, "Matrix in file", H5T_NATIVE_INT, fid, H5P_DEFAULT, plist, H5P_DEFAULT);

    /*
     * Select hyperslab for the dataset in the file, using 3x2 blocks,
     * (4,3) stride and (2,4) count starting at the position (0,1).
     */
    start[0]  = 0; start[1]  = 1;
    stride[0] = 4; stride[1] = 3;
    count[0]  = 2; count[1]  = 4;
    block[0]  = 3; block[1]  = 2;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Create dataspace for the first dataset.
     */
    mid1 = H5Screate_simple(MSPACE1_RANK, dim1, NULL);

    /*
     * Select hyperslab.
     * We will use 48 elements of the vector buffer starting at the second element.
     * Selected elements are 1 2 3 . . . 48
     */
    start[0]  = 1;
    stride[0] = 1;
    count[0]  = 48;
    block[0]  = 1;
    ret = H5Sselect_hyperslab(mid1, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Write selection from the vector buffer to the dataset in the file.
     *
     * File dataset should look like this:
     *                    0  1  2  0  3  4  0  5  6  0  7  8
     *                    0  9 10  0 11 12  0 13 14  0 15 16
     *                    0 17 18  0 19 20  0 21 22  0 23 24
     *                    0  0  0  0  0  0  0  0  0  0  0  0
     *                    0 25 26  0 27 28  0 29 30  0 31 32
     *                    0 33 34  0 35 36  0 37 38  0 39 40
     *                    0 41 42  0 43 44  0 45 46  0 47 48
     *                    0  0  0  0  0  0  0  0  0  0  0  0
     */
     ret = H5Dwrite(dataset, H5T_NATIVE_INT, mid1, fid, H5P_DEFAULT, vector);

    /*
     * Reset the selection for the file dataspace fid.
     */
    ret = H5Sselect_none(fid);

    /*
     * Create dataspace for the second dataset.
     */
    mid2 = H5Screate_simple(MSPACE2_RANK, dim2, NULL);

    /*
     * Select sequence of NPOINTS points in the file dataspace.
     */
    coord[0][0] = 0; coord[0][1] = 0;
    coord[1][0] = 3; coord[1][1] = 3;
    coord[2][0] = 3; coord[2][1] = 5;
    coord[3][0] = 5; coord[3][1] = 6;

    ret = H5Sselect_elements(fid, H5S_SELECT_SET, NPOINTS, (const hsize_t *)coord);

    /*
     * Write new selection of points to the dataset.
     */
    ret = H5Dwrite(dataset, H5T_NATIVE_INT, mid2, fid, H5P_DEFAULT, values);

    /*
     * File dataset should look like this:
     *                   53  1  2  0  3  4  0  5  6  0  7  8
     *                    0  9 10  0 11 12  0 13 14  0 15 16
     *                    0 17 18  0 19 20  0 21 22  0 23 24
     *                    0  0  0 59  0 61  0  0  0  0  0  0
     *                    0 25 26  0 27 28  0 29 30  0 31 32
     *                    0 33 34  0 35 36 67 37 38  0 39 40
     *                    0 41 42  0 43 44  0 45 46  0 47 48
     *                    0  0  0  0  0  0  0  0  0  0  0  0
     *
     */

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid1);
    ret = H5Sclose(mid2);
    ret = H5Sclose(fid);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    /*
     * Close the file.
     */
    ret = H5Fclose(file);

    /*
     * Open the file.
     */
    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDONLY, H5P_DEFAULT);

    /*
     * Open the dataset.
     */
    dataset = H5Dopen2(file, "Matrix in file", H5P_DEFAULT);

    /*
     * Get dataspace of the open dataset.
     */
    fid = H5Dget_space(dataset);

    /*
     * Select first hyperslab for the dataset in the file. The following
     * elements are selected:
     *                     10  0 11 12
     *                     18  0 19 20
     *                      0 59  0 61
     *
     */
    start[0] = 1; start[1] = 2;
    block[0] = 1; block[1] = 1;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 3; count[1]  = 4;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_SET, start, stride, count, block);

    /*
     * Add second selected hyperslab to the selection.
     * The following elements are selected:
     *                    19 20  0 21 22
     *                     0 61  0  0  0
     *                    27 28  0 29 30
     *                    35 36 67 37 38
     *                    43 44  0 45 46
     *                     0  0  0  0  0
     * Note that two hyperslabs overlap. Common elements are:
     *                                              19 20
     *                                               0 61
     */
    start[0] = 2; start[1] = 4;
    block[0] = 1; block[1] = 1;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 6; count[1]  = 5;
    ret = H5Sselect_hyperslab(fid, H5S_SELECT_OR, start, stride, count, block);

    /*
     * Create memory dataspace.
     */
    mid = H5Screate_simple(MSPACE_RANK, mdim, NULL);

    /*
     * Select two hyperslabs in memory. Hyperslabs has the same
     * size and shape as the selected hyperslabs for the file dataspace.
     */
    start[0] = 0; start[1] = 0;
    block[0] = 1; block[1] = 1;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 3; count[1]  = 4;
    ret = H5Sselect_hyperslab(mid, H5S_SELECT_SET, start, stride, count, block);

    start[0] = 1; start[1] = 2;
    block[0] = 1; block[1] = 1;
    stride[0] = 1; stride[1] = 1;
    count[0]  = 6; count[1]  = 5;
    ret = H5Sselect_hyperslab(mid, H5S_SELECT_OR, start, stride, count, block);

    /*
     * Initialize data buffer.
     */
    for (i = 0; i < MSPACE_DIM1; i++) {
       for (j = 0; j < MSPACE_DIM2; j++)
            matrix_out[i][j] = 0;
    }
    /*
     * Read data back to the buffer matrix_out.
     */
    ret = H5Dread(dataset, H5T_NATIVE_INT, mid, fid,
                  H5P_DEFAULT, matrix_out);

    /*
     * Display the result. Memory dataset is:
     *
     *                    10  0 11 12  0  0  0  0  0
     *                    18  0 19 20  0 21 22  0  0
     *                     0 59  0 61  0  0  0  0  0
     *                     0  0 27 28  0 29 30  0  0
     *                     0  0 35 36 67 37 38  0  0
     *                     0  0 43 44  0 45 46  0  0
     *                     0  0  0  0  0  0  0  0  0
     *                     0  0  0  0  0  0  0  0  0
     */
    for(i = 0; i < MSPACE_DIM1; i++) {
        for(j = 0; j < MSPACE_DIM2; j++)
            printf("%3d  ", matrix_out[i][j]);
        printf("\n");
    }

    /*
     * Close memory file and memory dataspaces.
     */
    ret = H5Sclose(mid);
    ret = H5Sclose(fid);

    /*
     * Close dataset.
     */
    ret = H5Dclose(dataset);

    /*
     * Close property list
     */
    ret = H5Pclose(plist);

    /*
     * Close the file.
     */
    ret = H5Fclose(file);

    return 0;
}

