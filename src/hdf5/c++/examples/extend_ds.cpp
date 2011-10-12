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

const H5std_string FILE_NAME( "SDSextendible.h5" );
const H5std_string DATASET_NAME( "ExtendibleArray" );
const int      NX = 10;
const int      NY = 5;
const int      RANK = 2;

int main (void)
{
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
       * Create the data space with unlimited dimensions.
       */
      hsize_t      dims[2]  = { 3, 3};  // dataset dimensions at creation
      hsize_t      maxdims[2] = {H5S_UNLIMITED, H5S_UNLIMITED};
      DataSpace mspace1( RANK, dims, maxdims);

      /*
       * Create a new file. If file exists its contents will be overwritten.
       */
      H5File file( FILE_NAME, H5F_ACC_TRUNC );

      /*
       * Modify dataset creation properties, i.e. enable chunking.
       */
      DSetCreatPropList cparms;

      hsize_t      chunk_dims[2] ={2, 5};
      cparms.setChunk( RANK, chunk_dims );

      /*
       * Set fill value for the dataset
       */
      int fill_val = 0;
      cparms.setFillValue( PredType::NATIVE_INT, &fill_val);

      /*
       * Create a new dataset within the file using cparms
       * creation properties.
       */
      DataSet dataset = file.createDataSet( DATASET_NAME, PredType::NATIVE_INT, mspace1, cparms);

      /*
       * Extend the dataset. This call assures that dataset is at least 3 x 3.
       */
      hsize_t      size[2];
      size[0]   = 3;
      size[1]   = 3;
      dataset.extend( size );

      /*
       * Select a hyperslab.
       */
      DataSpace fspace1 = dataset.getSpace ();
      hsize_t     offset[2];
      offset[0] = 0;
      offset[1] = 0;
      hsize_t      dims1[2] = { 3, 3};            /* data1 dimensions */
      fspace1.selectHyperslab( H5S_SELECT_SET, dims1, offset );

      /*
       * Write the data to the hyperslab.
       */
      int       data1[3][3] = { {1, 1, 1},       /* data to write */
				{1, 1, 1},
				{1, 1, 1} };
      dataset.write( data1, PredType::NATIVE_INT, mspace1, fspace1 );

      /*
       * Extend the dataset. Dataset becomes 10 x 3.
       */
      hsize_t   dims2[2] = { 7, 1};            /* data2 dimensions */
      dims[0]   = dims1[0] + dims2[0];
      size[0]   = dims[0];
      size[1]   = dims[1];
      dataset.extend( size );

      /*
       * Select a hyperslab.
       */
      DataSpace fspace2 = dataset.getSpace ();
      offset[0] = 3;
      offset[1] = 0;
      fspace2.selectHyperslab( H5S_SELECT_SET, dims2, offset );

      /*
       * Define memory space
       */
      DataSpace mspace2( RANK, dims2 );

      /*
       * Write the data to the hyperslab.
       */
      int  data2[7]    = { 2, 2, 2, 2, 2, 2, 2};
      dataset.write( data2, PredType::NATIVE_INT, mspace2, fspace2 );

      /*
       * Extend the dataset. Dataset becomes 10 x 5.
       */
      hsize_t   dims3[2] = { 2, 2};            /* data3 dimensions */
      dims[1]   = dims1[1] + dims3[1];
      size[0]   = dims[0];
      size[1]   = dims[1];
      dataset.extend( size );

      /*
       * Select a hyperslab
       */
      DataSpace fspace3 = dataset.getSpace ();
      offset[0] = 0;
      offset[1] = 3;
      fspace3.selectHyperslab( H5S_SELECT_SET, dims3, offset );

      /*
       * Define memory space.
       */
      DataSpace mspace3( RANK, dims3 );

      /*
       * Write the data to the hyperslab.
       */
      int         data3[2][2] = { {3, 3}, {3, 3} };
      dataset.write( data3, PredType::NATIVE_INT, mspace3, fspace3 );

      /*
       * Read the data from this dataset and display it.
       */
      int i, j;
      int data_out[NX][NY];
      for (i = 0; i < NX; i++)
      {
         for (j = 0; j < NY; j++)
            data_out[i][j] = 0;
      }
      dataset.read( data_out, PredType::NATIVE_INT );
      /*
       * Resulting dataset
       *
       *	 1 1 1 3 3
       *	 1 1 1 3 3
       *	 1 1 1 0 0
       *	 2 0 0 0 0
       *	 2 0 0 0 0
       *	 2 0 0 0 0
       *	 2 0 0 0 0
       *	 2 0 0 0 0
       *	 2 0 0 0 0
       *	 2 0 0 0 0
       */
      /*
       * Display the result.
       */
      for (i=0; i < NX; i++)
      {
          for(j=0; j < NY; j++)
             cout << data_out[i][j] << "  ";
          cout << endl;
      }
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

   // catch failure caused by the DataSpace operations
   catch( DataTypeIException error )
   {
      error.printError();
      return -1;
   }
   return 0;
}
