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
 *   This example shows how to work with extendible dataset.
 *   In the current version of the library dataset MUST be
 *   chunked.
 *
 */

#include "hdf5.h"

#define H5FILE_NAME        "SDSextendible.h5"
#define DATASETNAME "ExtendibleArray"
#define RANK         2
#define NX     10
#define NY     5

int
main (void)
{
    hid_t       file;                          /* handles */
    hid_t       dataspace, dataset;
    hid_t       filespace;
    hid_t       cparms;
    hsize_t      dims[2]  = { 3, 3};            /*
						 * dataset dimensions
						 * at the creation time
						 */
    hsize_t      dims1[2] = { 3, 3};            /* data1 dimensions */
    hsize_t      dims2[2] = { 7, 1};            /* data2 dimensions */
    hsize_t      dims3[2] = { 2, 2};            /* data3 dimensions */

    hsize_t      maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
    hsize_t      chunk_dims[2] ={2, 5};
    hsize_t      size[2];
    hsize_t      offset[2];

    herr_t      status;

    int         data1[3][3] = { {1, 1, 1},       /* data to write */
				{1, 1, 1},
				{1, 1, 1} };

    int         data2[7]    = { 2, 2, 2, 2, 2, 2, 2};

    int         data3[2][2] = { {3, 3},
				{3, 3} };
    int fillvalue = 0;

    /*
     * Create the data space with unlimited dimensions.
     */
    dataspace = H5Screate_simple(RANK, dims, maxdims);

    /*
     * Create a new file. If file exists its contents will be overwritten.
     */
    file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Modify dataset creation properties, i.e. enable chunking.
     */
    cparms = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_chunk( cparms, RANK, chunk_dims);
    status = H5Pset_fill_value (cparms, H5T_NATIVE_INT, &fillvalue );

    /*
     * Create a new dataset within the file using cparms
     * creation properties.
     */
    dataset = H5Dcreate2(file, DATASETNAME, H5T_NATIVE_INT, dataspace, H5P_DEFAULT,
			cparms, H5P_DEFAULT);

    /*
     * Extend the dataset. This call assures that dataset is at least 3 x 3.
     */
    size[0]   = 3;
    size[1]   = 3;
    status = H5Dset_extent(dataset, size);

    /*
     * Select a hyperslab.
     */
    filespace = H5Dget_space(dataset);
    offset[0] = 0;
    offset[1] = 0;
    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL,
				 dims1, NULL);

    /*
     * Write the data to the hyperslab.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, filespace,
		      H5P_DEFAULT, data1);

    /*
     * Extend the dataset. Dataset becomes 10 x 3.
     */
    dims[0]   = dims1[0] + dims2[0];
    size[0]   = dims[0];
    size[1]   = dims[1];
    status = H5Dset_extent(dataset, size);

    /*
     * Select a hyperslab.
     */
    filespace = H5Dget_space(dataset);
    offset[0] = 3;
    offset[1] = 0;
    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL,
				 dims2, NULL);

    /*
     * Define memory space
     */
    dataspace = H5Screate_simple(RANK, dims2, NULL);

    /*
     * Write the data to the hyperslab.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, filespace,
		      H5P_DEFAULT, data2);

    /*
     * Extend the dataset. Dataset becomes 10 x 5.
     */
    dims[1]   = dims1[1] + dims3[1];
    size[0]   = dims[0];
    size[1]   = dims[1];
    status = H5Dset_extent(dataset, size);

    /*
     * Select a hyperslab
     */
    filespace = H5Dget_space(dataset);
    offset[0] = 0;
    offset[1] = 3;
    status = H5Sselect_hyperslab(filespace, H5S_SELECT_SET, offset, NULL,
				 dims3, NULL);

    /*
     * Define memory space.
     */
    dataspace = H5Screate_simple(RANK, dims3, NULL);

    /*
     * Write the data to the hyperslab.
     */
    status = H5Dwrite(dataset, H5T_NATIVE_INT, dataspace, filespace,
		      H5P_DEFAULT, data3);

    /*
     * Resulting dataset
     *
     *	 3 3 3 2 2
     *	 3 3 3 2 2
     *	 3 3 3 0 0
     *	 2 0 0 0 0
     *	 2 0 0 0 0
     *	 2 0 0 0 0
     *	 2 0 0 0 0
     *	 2 0 0 0 0
     *	 2 0 0 0 0
     *	 2 0 0 0 0
     */
    /*
     * Close/release resources.
     */
    H5Dclose(dataset);
    H5Sclose(dataspace);
    H5Sclose(filespace);
    H5Pclose(cparms);
    H5Fclose(file);

    return 0;
}
