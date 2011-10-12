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
 * This example shows how to create a compound datatype,
 * write an array which has the compound datatype to the file,
 * and read back fields' subsets.
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

const H5std_string FILE_NAME( "SDScompound.h5" );
const H5std_string DATASET_NAME( "ArrayOfStructures" );
const H5std_string MEMBER1( "a_name" );
const H5std_string MEMBER2( "b_name" );
const H5std_string MEMBER3( "c_name" );
const int   LENGTH = 10;
const int   RANK = 1;

int main(void)
{
   /* First structure  and dataset*/
   typedef struct s1_t {
	int    a;
	float  b;
	double c;
   } s1_t;

   /* Second structure (subset of s1_t)  and dataset*/
   typedef struct s2_t {
	double c;
	int    a;
   } s2_t;

   // Try block to detect exceptions raised by any of the calls inside it
   try
   {
      /*
       * Initialize the data
       */
      int  i;
      s1_t s1[LENGTH];
      for (i = 0; i< LENGTH; i++)
      {
         s1[i].a = i;
         s1[i].b = i*i;
         s1[i].c = 1./(i+1);
      }

      /*
       * Turn off the auto-printing when failure occurs so that we can
       * handle the errors appropriately
       */
      Exception::dontPrint();

      /*
       * Create the data space.
       */
      hsize_t dim[] = {LENGTH};   /* Dataspace dimensions */
      DataSpace space( RANK, dim );

      /*
       * Create the file.
       */
      H5File* file = new H5File( FILE_NAME, H5F_ACC_TRUNC );

      /*
       * Create the memory datatype.
       */
      CompType mtype1( sizeof(s1_t) );
      mtype1.insertMember( MEMBER1, HOFFSET(s1_t, a), PredType::NATIVE_INT);
      mtype1.insertMember( MEMBER3, HOFFSET(s1_t, c), PredType::NATIVE_DOUBLE);
      mtype1.insertMember( MEMBER2, HOFFSET(s1_t, b), PredType::NATIVE_FLOAT);

      /*
       * Create the dataset.
       */
      DataSet* dataset;
      dataset = new DataSet(file->createDataSet(DATASET_NAME, mtype1, space));

      /*
       * Write data to the dataset;
       */
      dataset->write( s1, mtype1 );

      /*
       * Release resources
       */
      delete dataset;
      delete file;

      /*
       * Open the file and the dataset.
       */
      file = new H5File( FILE_NAME, H5F_ACC_RDONLY );
      dataset = new DataSet (file->openDataSet( DATASET_NAME ));

      /*
       * Create a datatype for s2
       */
      CompType mtype2( sizeof(s2_t) );

      mtype2.insertMember( MEMBER3, HOFFSET(s2_t, c), PredType::NATIVE_DOUBLE);
      mtype2.insertMember( MEMBER1, HOFFSET(s2_t, a), PredType::NATIVE_INT);

      /*
       * Read two fields c and a from s1 dataset. Fields in the file
       * are found by their names "c_name" and "a_name".
       */
      s2_t s2[LENGTH];
      dataset->read( s2, mtype2 );

      /*
       * Display the fields
       */
      cout << endl << "Field c : " << endl;
      for( i = 0; i < LENGTH; i++)
	 cout << s2[i].c << " ";
      cout << endl;

      cout << endl << "Field a : " << endl;
      for( i = 0; i < LENGTH; i++)
	 cout << s2[i].a << " ";
      cout << endl;

      /*
       * Create a datatype for s3.
       */
      CompType mtype3( sizeof(float) );

      mtype3.insertMember( MEMBER2, 0, PredType::NATIVE_FLOAT);

      /*
       * Read field b from s1 dataset. Field in the file is found by its name.
       */
      float s3[LENGTH];  // Third "structure" - used to read float field of s1
      dataset->read( s3, mtype3 );

      /*
       * Display the field
       */
      cout << endl << "Field b : " << endl;
      for( i = 0; i < LENGTH; i++)
	 cout << s3[i] << " ";
      cout << endl;

      /*
       * Release resources
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

   // catch failure caused by the DataSpace operations
   catch( DataTypeIException error )
   {
      error.printError();
      return -1;
   }

   return 0;
}
