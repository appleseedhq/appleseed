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
 *  This program shows how the select_hyperslab and select_elements
 *  functions are used to write selected data from memory to the file.
 *  Program takes 48 elements from the linear buffer and writes them into
 *  the matrix using 3x2 blocks, (4,3) stride and (2,4) count.
 *  Then four elements  of the matrix are overwritten with the new values and
 *  file is closed. Program reopens the file and reads and displays the result.
 */

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cout;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

const H5std_string FILE_NAME( "Select.h5" );
const H5std_string DATASET_NAME( "Matrix in file" );
const int   MSPACE1_RANK = 1;	// Rank of the first dataset in memory
const int   MSPACE1_DIM = 50;   // Dataset size in memory
const int   MSPACE2_RANK = 1;	// Rank of the second dataset in memory
const int   MSPACE2_DIM = 4;	// Dataset size in memory
const int   FSPACE_RANK = 2;	// Dataset rank as it is stored in the file
const int   FSPACE_DIM1 = 8;	// Dimension sizes of the dataset as it is
const int   FSPACE_DIM2 = 12;	// 	stored in the file
const int   MSPACE_RANK = 2;	// Rank of the first dataset in memory
const int   MSPACE_DIM1 = 8;	// We will read dataset back from the file
const int   MSPACE_DIM2 = 9;	// 	to the dataset in memory with these
				// 	dataspace parameters
const int   NPOINTS = 4;	// Number of points that will be selected
				//	and overwritten

int main (void)
{
    int   i,j; // loop indices */

    /*
     * Try block to detect exceptions raised by any of the calls inside it
     */
    try
    {
	/*
	 * Turn off the auto-printing when failure occurs so that we can
	 * handle the errors appropriately
	 */
	Exception::dontPrint();

	/*
	 * Create a file.
	 */
	H5File* file = new H5File( FILE_NAME, H5F_ACC_TRUNC );

	/*
	* Create property list for a dataset and set up fill values.
	*/
	int fillvalue = 0;   /* Fill value for the dataset */
	DSetCreatPropList plist;
	plist.setFillValue(PredType::NATIVE_INT, &fillvalue);

	/*
	 * Create dataspace for the dataset in the file.
	 */
	hsize_t fdim[] = {FSPACE_DIM1, FSPACE_DIM2}; // dim sizes of ds (on disk)
	DataSpace fspace( FSPACE_RANK, fdim );

	/*
	 * Create dataset and write it into the file.
	 */
	DataSet* dataset = new DataSet(file->createDataSet(
		DATASET_NAME, PredType::NATIVE_INT, fspace, plist));

	/*
	 * Select hyperslab for the dataset in the file, using 3x2 blocks,
	 * (4,3) stride and (2,4) count starting at the position (0,1).
	 */
	hsize_t start[2]; // Start of hyperslab
	hsize_t stride[2]; // Stride of hyperslab
	hsize_t count[2];  // Block count
	hsize_t block[2];  // Block sizes
	start[0]  = 0; start[1]  = 1;
	stride[0] = 4; stride[1] = 3;
	count[0]  = 2; count[1]  = 4;
	block[0]  = 3; block[1]  = 2;
	fspace.selectHyperslab( H5S_SELECT_SET, count, start, stride, block);

	/*
	 * Create dataspace for the first dataset.
	 */
	hsize_t dim1[] = {MSPACE1_DIM};  /* Dimension size of the first dataset
	                                   (in memory) */
	DataSpace mspace1( MSPACE1_RANK, dim1 );

	/*
	 * Select hyperslab.
	 * We will use 48 elements of the vector buffer starting at the
	 * second element.  Selected elements are 1 2 3 . . . 48
	 */
	start[0]  = 1;
	stride[0] = 1;
	count[0]  = 48;
	block[0]  = 1;
	mspace1.selectHyperslab( H5S_SELECT_SET, count, start, stride, block);

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
	int    vector[MSPACE1_DIM];	// vector buffer for dset

	/*
	 * Buffer initialization.
	 */
	vector[0] = vector[MSPACE1_DIM - 1] = -1;
	for (i = 1; i < MSPACE1_DIM - 1; i++)
	    vector[i] = i;

	dataset->write( vector, PredType::NATIVE_INT, mspace1, fspace );

	/*
	 * Reset the selection for the file dataspace fid.
	 */
	fspace.selectNone();

	/*
	 * Create dataspace for the second dataset.
	 */
	hsize_t dim2[] = {MSPACE2_DIM};  /* Dimension size of the second dataset
	                                   (in memory */
	DataSpace mspace2( MSPACE2_RANK, dim2 );

	/*
	 * Select sequence of NPOINTS points in the file dataspace.
	 */
	hsize_t coord[NPOINTS][FSPACE_RANK]; /* Array to store selected points
	                                        from the file dataspace */
	coord[0][0] = 0; coord[0][1] = 0;
	coord[1][0] = 3; coord[1][1] = 3;
	coord[2][0] = 3; coord[2][1] = 5;
	coord[3][0] = 5; coord[3][1] = 6;

	fspace.selectElements( H5S_SELECT_SET, NPOINTS, (const hsize_t *)coord);

	/*
	 * Write new selection of points to the dataset.
	 */
	int    values[] = {53, 59, 61, 67};  /* New values to be written */
	dataset->write( values, PredType::NATIVE_INT, mspace2, fspace );

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
	 * Close the dataset and the file.
	 */
	delete dataset;
	delete file;

	/*
	 * Open the file.
	 */
	file = new H5File( FILE_NAME, H5F_ACC_RDONLY );

	/*
	 * Open the dataset.
	 */
	dataset = new DataSet( file->openDataSet( DATASET_NAME ));

	/*
	 * Get dataspace of the dataset.
	 */
	fspace = dataset->getSpace();

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
	fspace.selectHyperslab(H5S_SELECT_SET, count, start, stride, block);

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
	fspace.selectHyperslab(H5S_SELECT_OR, count, start, stride, block);

	/*
	 * Create memory dataspace.
	 */
	hsize_t mdim[] = {MSPACE_DIM1, MSPACE_DIM2}; /* Dimension sizes of the
                                                   dataset in memory when we
                                                   read selection from the
                                                   dataset on the disk */
	DataSpace mspace(MSPACE_RANK, mdim);

	/*
	 * Select two hyperslabs in memory. Hyperslabs has the same
	 * size and shape as the selected hyperslabs for the file dataspace.
	 */
	start[0] = 0; start[1] = 0;
	block[0] = 1; block[1] = 1;
	stride[0] = 1; stride[1] = 1;
	count[0]  = 3; count[1]  = 4;
	mspace.selectHyperslab(H5S_SELECT_SET, count, start, stride, block);
	start[0] = 1; start[1] = 2;
	block[0] = 1; block[1] = 1;
	stride[0] = 1; stride[1] = 1;
	count[0]  = 6; count[1]  = 5;
	mspace.selectHyperslab(H5S_SELECT_OR, count, start, stride, block);

	/*
	 * Initialize data buffer.
	 */
	int matrix_out[MSPACE_DIM1][MSPACE_DIM2];
	for (i = 0; i < MSPACE_DIM1; i++)
	    for (j = 0; j < MSPACE_DIM2; j++)
		matrix_out[i][j] = 0;

	/*
	 * Read data back to the buffer matrix.
	 */
	dataset->read(matrix_out, PredType::NATIVE_INT, mspace, fspace);

	/*
	 * Display the result.  Memory dataset is:
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
	for (i=0; i < MSPACE_DIM1; i++)
	{
	    for(j=0; j < MSPACE_DIM2; j++)
		cout << matrix_out[i][j] << "  ";
	    cout << endl;
	}

	/*
	 * Close the dataset and the file.
	 */
	delete dataset;
	delete file;
   }  // end of try block

   // catch failure caused by the H5File operations
   catch( FileIException error )
   {
	error.printError();
	return -1;
   }

   // catch failure caused by the DataSet operations
   catch( DataSetIException error )
   {
	error.printError();
	return -1;
   }

   // catch failure caused by the DataSpace operations
   catch( DataSpaceIException error )
   {
	error.printError();
	return -1;
   }

   return 0;
}
