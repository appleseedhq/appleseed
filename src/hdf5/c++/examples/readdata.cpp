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

//
//      This example reads hyperslab from the SDS.h5 file into
//      two-dimensional plane of a three-dimensional array.  Various
//      information about the dataset in the SDS.h5 file is obtained.
//

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

const H5std_string FILE_NAME( "SDS.h5" );
const H5std_string DATASET_NAME( "IntArray" );
const int    NX_SUB = 3;	// hyperslab dimensions
const int    NY_SUB = 4;
const int    NX = 7;		// output buffer dimensions
const int    NY = 7;
const int    NZ = 3;
const int    RANK_OUT = 3;

int main (void)
{
   /*
    * Output buffer initialization.
    */
   int i, j, k;
   int         data_out[NX][NY][NZ ]; /* output buffer */
   for (j = 0; j < NX; j++)
   {
      for (i = 0; i < NY; i++)
      {
 	 for (k = 0; k < NZ ; k++)
	    data_out[j][i][k] = 0;
      }
   }

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
       * Open the specified file and the specified dataset in the file.
       */
      H5File file( FILE_NAME, H5F_ACC_RDONLY );
      DataSet dataset = file.openDataSet( DATASET_NAME );

      /*
       * Get the class of the datatype that is used by the dataset.
       */
      H5T_class_t type_class = dataset.getTypeClass();

      /*
       * Get class of datatype and print message if it's an integer.
       */
      if( type_class == H5T_INTEGER )
      {
	 cout << "Data set has INTEGER type" << endl;

         /*
	  * Get the integer datatype
          */
	 IntType intype = dataset.getIntType();

         /*
          * Get order of datatype and print message if it's a little endian.
          */
	 H5std_string order_string;
         H5T_order_t order = intype.getOrder( order_string );
	 cout << order_string << endl;

         /*
          * Get size of the data element stored in file and print it.
          */
         size_t size = intype.getSize();
         cout << "Data size is " << size << endl;
      }

      /*
       * Get dataspace of the dataset.
       */
      DataSpace dataspace = dataset.getSpace();

      /*
       * Get the number of dimensions in the dataspace.
       */
      int rank = dataspace.getSimpleExtentNdims();

      /*
       * Get the dimension size of each dimension in the dataspace and
       * display them.
       */
      hsize_t dims_out[2];
      int ndims = dataspace.getSimpleExtentDims( dims_out, NULL);
      cout << "rank " << rank << ", dimensions " <<
	      (unsigned long)(dims_out[0]) << " x " <<
	      (unsigned long)(dims_out[1]) << endl;

      /*
       * Define hyperslab in the dataset; implicitly giving strike and
       * block NULL.
       */
      hsize_t      offset[2];	// hyperslab offset in the file
      hsize_t      count[2];	// size of the hyperslab in the file
      offset[0] = 1;
      offset[1] = 2;
      count[0]  = NX_SUB;
      count[1]  = NY_SUB;
      dataspace.selectHyperslab( H5S_SELECT_SET, count, offset );

      /*
       * Define the memory dataspace.
       */
      hsize_t     dimsm[3];              /* memory space dimensions */
      dimsm[0] = NX;
      dimsm[1] = NY;
      dimsm[2] = NZ ;
      DataSpace memspace( RANK_OUT, dimsm );

      /*
       * Define memory hyperslab.
       */
      hsize_t      offset_out[3];	// hyperslab offset in memory
      hsize_t      count_out[3];	// size of the hyperslab in memory
      offset_out[0] = 3;
      offset_out[1] = 0;
      offset_out[2] = 0;
      count_out[0]  = NX_SUB;
      count_out[1]  = NY_SUB;
      count_out[2]  = 1;
      memspace.selectHyperslab( H5S_SELECT_SET, count_out, offset_out );

      /*
       * Read data from hyperslab in the file into the hyperslab in
       * memory and display the data.
       */
      dataset.read( data_out, PredType::NATIVE_INT, memspace, dataspace );

      for (j = 0; j < NX; j++)
      {
	for (i = 0; i < NY; i++)
	   cout << data_out[j][i][0] << " ";
	cout << endl;
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

   return 0;  // successfully terminated
}

