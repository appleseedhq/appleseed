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
 *   This example reads hyperslab from the SDS.h5 file
 *   created by h5_write.c program into two-dimensional
 *   plane of the three-dimensional array.
 *   Information about dataset in the SDS.h5 file is obtained.
 */

#include "hdf5.h"

#define H5FILE_NAME        "SDS.h5"
#define DATASETNAME "IntArray"
#define NX_SUB  3           /* hyperslab dimensions */
#define NY_SUB  4
#define NX 7           /* output buffer dimensions */
#define NY 7
#define NZ  3
#define RANK         2
#define RANK_OUT     3

int
main (void)
{
    hid_t       file, dataset;         /* handles */
    hid_t       datatype, dataspace;
    hid_t       memspace;
    H5T_class_t t_class;                 /* data type class */
    H5T_order_t order;                 /* data order */
    size_t      size;                  /*
				        * size of the data element
				        * stored in file
				        */
    hsize_t     dimsm[3];              /* memory space dimensions */
    hsize_t     dims_out[2];           /* dataset dimensions */
    herr_t      status;

    int         data_out[NX][NY][NZ ]; /* output buffer */

    hsize_t      count[2];              /* size of the hyperslab in the file */
    hsize_t      offset[2];             /* hyperslab offset in the file */
    hsize_t      count_out[3];          /* size of the hyperslab in memory */
    hsize_t      offset_out[3];         /* hyperslab offset in memory */
    int          i, j, k, status_n, rank;

    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) {
	    for (k = 0; k < NZ ; k++)
		data_out[j][i][k] = 0;
	}
    }

    /*
     * Open the file and the dataset.
     */
    file = H5Fopen(H5FILE_NAME, H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset = H5Dopen2(file, DATASETNAME, H5P_DEFAULT);

    /*
     * Get datatype and dataspace handles and then query
     * dataset class, order, size, rank and dimensions.
     */
    datatype  = H5Dget_type(dataset);     /* datatype handle */
    t_class     = H5Tget_class(datatype);
    if (t_class == H5T_INTEGER) printf("Data set has INTEGER type \n");
    order     = H5Tget_order(datatype);
    if (order == H5T_ORDER_LE) printf("Little endian order \n");

    size  = H5Tget_size(datatype);
    printf(" Data size is %d \n", (int)size);

    dataspace = H5Dget_space(dataset);    /* dataspace handle */
    rank      = H5Sget_simple_extent_ndims(dataspace);
    status_n  = H5Sget_simple_extent_dims(dataspace, dims_out, NULL);
    printf("rank %d, dimensions %lu x %lu \n", rank,
	   (unsigned long)(dims_out[0]), (unsigned long)(dims_out[1]));

    /*
     * Define hyperslab in the dataset.
     */
    offset[0] = 1;
    offset[1] = 2;
    count[0]  = NX_SUB;
    count[1]  = NY_SUB;
    status = H5Sselect_hyperslab(dataspace, H5S_SELECT_SET, offset, NULL,
				 count, NULL);

    /*
     * Define the memory dataspace.
     */
    dimsm[0] = NX;
    dimsm[1] = NY;
    dimsm[2] = NZ ;
    memspace = H5Screate_simple(RANK_OUT,dimsm,NULL);

    /*
     * Define memory hyperslab.
     */
    offset_out[0] = 3;
    offset_out[1] = 0;
    offset_out[2] = 0;
    count_out[0]  = NX_SUB;
    count_out[1]  = NY_SUB;
    count_out[2]  = 1;
    status = H5Sselect_hyperslab(memspace, H5S_SELECT_SET, offset_out, NULL,
				 count_out, NULL);

    /*
     * Read data from hyperslab in the file into the hyperslab in
     * memory and display.
     */
    status = H5Dread(dataset, H5T_NATIVE_INT, memspace, dataspace,
		     H5P_DEFAULT, data_out);
    for (j = 0; j < NX; j++) {
	for (i = 0; i < NY; i++) printf("%d ", data_out[j][i][0]);
	printf("\n");
    }
    /*
     * 0 0 0 0 0 0 0
     * 0 0 0 0 0 0 0
     * 0 0 0 0 0 0 0
     * 3 4 5 6 0 0 0
     * 4 5 6 7 0 0 0
     * 5 6 7 8 0 0 0
     * 0 0 0 0 0 0 0
     */

    /*
     * Close/release resources.
     */
    H5Tclose(datatype);
    H5Dclose(dataset);
    H5Sclose(dataspace);
    H5Sclose(memspace);
    H5Fclose(file);

    return 0;
}
