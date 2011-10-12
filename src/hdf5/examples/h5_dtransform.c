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
 *  This example demonstrates how the data transform features of
 *  HDF5 works.
 *
 * (1)
 *  The test first writes out data, with no data transform set.
 *  Then, the test reads back this data with a data transform applied.
 *
 * (2)
 *  Then, the test writes a new set of data, with a data transform set.
 *  Then, the test reads this new set of data, without a data set.
 *
 * (3)
 *  Lastly, the test reads the previous set of data (that was written out
 *  with a data transform) with a data transform set for the read.
 *
 *  (4)
 *  Get the transform from the property using H5Pget_data_transform.
 */

#include "hdf5.h"

#define ROWS    12
#define COLS    18

const float windchillF[ROWS][COLS] =
    {   {36.0, 31.0, 25.0, 19.0, 13.0, 7.0, 1.0, -5.0, -11.0, -16.0, -22.0, -28.0, -34.0, -40.0, -46.0, -52.0, -57.0, -63.0 },
        {34.0, 27.0, 21.0, 15.0, 9.0, 3.0, -4.0, -10.0, -16.0, -22.0, -28.0, -35.0, -41.0, -47.0, -53.0, -59.0, -66.0, -72.0 } ,
        {32.0, 25.0, 19.0, 13.0, 6.0, 0.0, -7.0, -13.0, -19.0, -26.0, -32.0, -39.0, -45.0, -51.0, -58.0, -64.0, -71.0, -77.0 },
        {30.0, 24.0, 17.0, 11.0, 4.0, -2.0, -9.0, -15.0, -22.0, -29.0, -35.0, -42.0, -48.0, -55.0, -61.0, -68.0, -74.0, -81.0 },
        {29.0, 23.0, 16.0, 9.0, 3.0, -4.0, -11.0, -17.0, -24.0, -31.0, -37.0, -44.0, -51.0, -58.0, -64.0, -71.0, -78.0, -84.0 },
        {28.0, 22.0, 15.0, 8.0, 1.0, -5.0, -12.0, -19.0, -26.0, -33.0, -39.0, -46.0, -53.0, -60.0, -67.0, -73.0, -80.0, -87.0 },
        {28.0, 21.0, 14.0, 7.0, 0.0, -7.0, -14.0, -21.0, -27.0, -34.0, -41.0, -48.0, -55.0, -62.0, -69.0, -76.0, -82.0, -89.0 },
        {27.0, 20.0, 13.0, 6.0, -1.0, -8.0, -15.0, -22.0, -29.0, -36.0, -43.0, -50.0, -57.0, -64.0, -71.0, -78.0, -84.0, -91.0 },
        {26.0, 19.0, 12.0, 5.0, -2.0, -9.0, -16.0, -23.0, -30.0, -37.0, -44.0, -51.0, -58.0, -65.0, -72.0, -79.0, -86.0, -93.0 },
        {26.0, 19.0, 12.0, 4.0, -3.0, -10.0, -17.0, -24.0, -31.0, -38.0, -45.0, -52.0, -60.0, -67.0, -74.0, -81.0, -88.0, -95.0},
        {25.0, 18.0, 11.0, 4.0, -3.0, -11.0, -18.0, -25.0, -32.0, -39.0, -46.0, -54.0, -61.0, -68.0, -75.0, -82.0, -89.0, -97.0},
        {25.0, 17.0, 10.0, 3.0, -4.0, -11.0, -19.0, -26.0, -33.0, -40.0, -48.0, -55.0, -62.0, -69.0, -76.0, -84.0, -91.0, -98.0}
    };

#define PRINT(array)				\
{						\
    for(i=0; i<ROWS; i++)			\
    {						\
	for(j=0; j<COLS; j++)			\
	    printf("%6.2f ", array[i][j]);	\
	printf("\n");				\
    }						\
}




int
main (void)
{
    hid_t       file, dataset;         /* file and dataset handles */
    hid_t       dataspace;   /* handles */
    hsize_t     dimsf[2];              /* dataset dimensions */
    herr_t      status;
    hid_t	dxpl_id_f_to_c, dxpl_id_c_to_f;	/* data transform handles */
    const char* f_to_c = "(5/9.0)*(x-32)";
    const char* c_to_f =  "(9/5.0)*x + 32";
    char*	transform;
    float	windchillC[ROWS][COLS];
    int 	i,j, transform_size;

    /*
     * Create a new file using H5F_ACC_TRUNC access,
     * default file creation properties, and default file
     * access properties.
     */
    file = H5Fcreate("dtransform.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Describe the size of the array and create the data space for fixed
     * size dataset.
     */
    dimsf[0] = ROWS;
    dimsf[1] = COLS;
    dataspace = H5Screate_simple(2, dimsf, NULL);

    /*
     * Create a new dataset within the file using defined dataspace and
     * datatype and default dataset creation properties.
     */
    dataset = H5Dcreate2(file, "data_no_trans", H5T_NATIVE_FLOAT, dataspace, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
    printf("\nOriginal Data: \n");

    PRINT(windchillF);




/****************  PART 1 **************/
    /*
     * Write the data to the dataset using default transfer properties (ie, no transform set)
     */
    status = H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
		      H5P_DEFAULT, windchillF);

    /* Create the dataset transfer property list */
    dxpl_id_f_to_c = H5Pcreate(H5P_DATASET_XFER);

    /* Set the data transform */
    H5Pset_data_transform(dxpl_id_f_to_c, f_to_c);

    /* Read out the data with the data transform */
    H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_f_to_c, windchillC);

    /* Print the data from the read*/
    printf("\nData with no write transform, but a read transform: \n");
    PRINT(windchillC);


/****************  PART 2 **************/
    /*
     * Write the data to the dataset with the f_to_c transform set
     */
    status = H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
		      dxpl_id_f_to_c, windchillF);

    /* Read out the data with the default transfer list (ie, no transform set) */
    H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, windchillC);

    /* Print the data from the read*/
    printf("\nData with write transform, but no read transform: \n");
    PRINT(windchillC);

/************** PART 3 ***************/


    /* Create the dataset transfer property list */
    dxpl_id_c_to_f = H5Pcreate(H5P_DATASET_XFER);

    /* Set the data transform to be used on the read*/
    H5Pset_data_transform(dxpl_id_c_to_f, c_to_f);


    /*
     * Write the data to the dataset using the f_to_c transform
     */
    status = H5Dwrite(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL,
		      dxpl_id_f_to_c, windchillF);

    /* Read the data with the c_to_f data transform */
    H5Dread(dataset, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, dxpl_id_c_to_f, windchillC);

    /* Print the data from the read*/
    printf("\nData with both read and write data transform: \n");
    PRINT(windchillC);

/************** PART 4 **************/
    transform_size = H5Pget_data_transform(dxpl_id_f_to_c, NULL, 0);
    transform = (char*) malloc(transform_size+1);
    H5Pget_data_transform(dxpl_id_f_to_c, transform, transform_size+1);

    printf("\nTransform string (from dxpl_id_f_to_c) is: %s\n", transform);

    /*
     * Close/release resources.
     */
    H5Pclose(dxpl_id_c_to_f);
    H5Pclose(dxpl_id_f_to_c);
    H5Sclose(dataspace);
    H5Dclose(dataset);
    H5Fclose(file);

    return 0;
}
