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

/*****************************************************************************
   FILE
   th5s.cpp - HDF5 C++ testing the functionalities associated with the
        C dataspace interface (H5S)

   EXTERNAL ROUTINES/VARIABLES:

 ***************************************************************************/

#ifdef OLD_HEADER_FILENAME
#include <iostream.h>
#else
#include <iostream>
#endif
#include <string>

#ifndef H5_NO_NAMESPACE
#ifndef H5_NO_STD
    using std::cerr;
    using std::endl;
#endif  // H5_NO_STD
#endif

#include "H5Cpp.h"	// C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"	// C++ utilility header file
#include "H5srcdir.h"	// srcdir querying header file

const H5std_string    TESTFILE("th5s.h5");
const H5std_string    DATAFILE("th5s1.h5");

/* 3-D dataset with fixed dimensions */
const H5std_string SPACE1_NAME("Space1");
const int SPACE1_RANK = 3;
const int SPACE1_DIM1 = 3;
const int SPACE1_DIM2 = 15;
const int SPACE1_DIM3 = 13;

/* 4-D dataset with one unlimited dimension */
const H5std_string SPACE2_NAME("Space2");
const int SPACE2_RANK = 4;
const int SPACE2_DIM1 = 0;
const int SPACE2_DIM2 = 15;
const int SPACE2_DIM3 = 13;
const int SPACE2_DIM4 = 23;
const hsize_t SPACE2_MAX1 = H5S_UNLIMITED;
const hsize_t SPACE2_MAX2 = 15;
const hsize_t SPACE2_MAX3 = 13;
const hsize_t SPACE2_MAX4 = 23;

/* Scalar dataset with simple datatype */
const H5std_string SPACE3_NAME("Scalar1");
const int SPACE3_RANK = 0;
unsigned space3_data=65;

/* Scalar dataset with compound datatype */
const H5std_string SPACE4_NAME("Scalar2");
const H5std_string SPACE4_FIELDNAME1("c1");
const H5std_string SPACE4_FIELDNAME2("u");
const H5std_string SPACE4_FIELDNAME3("f");
const H5std_string SPACE4_FIELDNAME4("c2");
size_t space4_field1_off=0;
size_t space4_field2_off=0;
size_t space4_field3_off=0;
size_t space4_field4_off=0;
struct space4_struct {
    char c1;
    unsigned u;
    float f;
    char c2;
 } space4_data={'v',987123,(float)-3.14,'g'}; /* Test data for 4th dataspace */

/* Null dataspace */
int space5_data = 7;

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s_basic
 *
 * Purpose:	Test basic H5S (dataspace) code
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *              Mar 2001
 *
 * Modifications:
 *      January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hssize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *     April 12, 2011: Raymond Lu
 *                     Starting from the 1.8.7 release, we allow dimension
 *                     size to be zero.  So I took out the test against it.
 *-------------------------------------------------------------------------
 */
static void test_h5s_basic()
{
    hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t		dims2[] = {SPACE2_DIM1, SPACE2_DIM2, SPACE2_DIM3,
				   SPACE2_DIM4};
    hsize_t		dims3[H5S_MAX_RANK+1];
    hsize_t		tmax[4];

    // Output message about test being performed
    SUBTEST("Dataspace Manipulation");

    try {
	// Create simple dataspace sid1
	DataSpace sid1 (SPACE1_RANK, dims1 );

	// Get simple extent npoints of the dataspace sid1 and verify it
	hssize_t	n;	 	// Number of dataspace elements
	n = sid1.getSimpleExtentNpoints();
	verify_val((long)n, (long)(SPACE1_DIM1 * SPACE1_DIM2 * SPACE1_DIM3),
	   "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Get the logical rank of dataspace sid1 and verify it
	int	rank;		// Logical rank of dataspace
	rank = sid1.getSimpleExtentNdims();
	verify_val(rank, SPACE1_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Retrieves dimension size of dataspace sid1 and verify it
	int ndims;		// Number of dimensions
	hsize_t	tdims[4];	// Dimension array to test with
	ndims = sid1.getSimpleExtentDims( tdims );
	verify_val(ndims, SPACE1_RANK, "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);
	verify_val(HDmemcmp(tdims, dims1, SPACE1_RANK * sizeof(unsigned)), 0,
	   "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);

	// Create simple dataspace sid2
	hsize_t	max2[] = {SPACE2_MAX1, SPACE2_MAX2, SPACE2_MAX3, SPACE2_MAX4};
	DataSpace sid2 (SPACE2_RANK, dims2, max2);

	// Get simple extent npoints of dataspace sid2 and verify it
	n = sid2.getSimpleExtentNpoints();
	verify_val((long)n, (long)(SPACE2_DIM1 * SPACE2_DIM2 * SPACE2_DIM3 * SPACE2_DIM4),
	   "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Get the logical rank of dataspace sid2 and verify it
	rank = sid2.getSimpleExtentNdims();
	verify_val(rank, SPACE2_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Retrieves dimension size and max size of dataspace sid2 and
	// verify them
	ndims = sid2.getSimpleExtentDims( tdims, tmax );
	verify_val(HDmemcmp(tdims, dims2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);
	verify_val(HDmemcmp(tmax, max2, SPACE2_RANK * sizeof(unsigned)), 0,
	   "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);

	// Check to be sure we can't create a simple data space that has too
	// many dimensions.
	try {
	    DataSpace manydims_ds(H5S_MAX_RANK+1, dims3, NULL);

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("DataSpace constructor", "Library allowed overwrite of existing dataset");
	}
	catch( DataSpaceIException E ) // Simple data space with too many dims
	{} // do nothing, exception expected

       /*
	* Try reading a file that has been prepared that has a dataset with a
	* higher dimensionality than what the library can handle.
	*
	* If this test fails and the H5S_MAX_RANK variable has changed, follow
	* the instructions in space_overflow.c for regenating the th5s.h5 file.
	*/
	char testfile[512]="";
	char *srcdir = getenv("srcdir");
	if (srcdir && ((strlen(srcdir) + strlen(TESTFILE.c_str()) + 1) < sizeof(testfile))){
	    strcpy(testfile, srcdir);
	    strcat(testfile, "/");
	}
	strcat(testfile, TESTFILE.c_str());

	// Create file
	H5File fid1(testfile, H5F_ACC_RDONLY);

	// Try to open the dataset that has higher dimensionality than
	// what the library can handle and this operation should fail.
	try {
	    DataSet dset1 = fid1.openDataSet( "dset" );

	    // Should FAIL but didn't, so throw an invalid action exception
	    throw InvalidActionException("H5File::openDataSet", "Opening a dataset with higher dimensionality than what the library can handle");
	}
	catch( FileIException E ) // catching higher dimensionality dataset
	{} // do nothing, exception expected

    // CHECK_I(ret, "H5Fclose");  // leave this here, later, fake a failure
		// in the p_close see how this will handle it. - BMR

	PASSED();
    }	// end of try block

    catch (InvalidActionException E)
    {
        cerr << " FAILED" << endl;
        cerr << "    <<<  " << E.getDetailMsg() << "  >>>" << endl << endl;
    }
    // catch all other exceptions
    catch (Exception E)
    {
        issue_fail_msg("test_h5s_basic()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_h5s_basic()

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s_scalar_write
 *
 * Purpose:	Test scalar H5S (dataspace) writing code
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *              Mar 2001
 *
 * Modifications:
 *      January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hssize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *-------------------------------------------------------------------------
 */
static void test_h5s_scalar_write()
{
    // Output message about test being performed
    SUBTEST("Scalar Dataspace Writing");

    try {
	// Create file
	H5File fid1(DATAFILE, H5F_ACC_TRUNC);

	// Create scalar dataspace
	DataSpace sid1(SPACE3_RANK, NULL);

	//n = H5Sget_simple_extent_npoints(sid1);
	hssize_t	n;	 	// Number of dataspace elements
	n = sid1.getSimpleExtentNpoints();
	verify_val((long)n, 1, "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	int	rank;		// Logical rank of dataspace
	rank = sid1.getSimpleExtentNdims();
	verify_val(rank, SPACE3_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Retrieves dimension size of dataspace sid1 and verify it
	int ndims;		// Number of dimensions
	hsize_t	tdims[4];	// Dimension array to test with
	ndims = sid1.getSimpleExtentDims( tdims );
	verify_val(ndims, 0, "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);

	// Verify extent type
	H5S_class_t ext_type;   // Extent type
	ext_type = sid1.getSimpleExtentType();
	verify_val(ext_type, H5S_SCALAR, "DataSpace::getSimpleExtentType", __LINE__, __FILE__);

	// Create and write a dataset
	DataSet dataset = fid1.createDataSet("Dataset1", PredType::NATIVE_UINT,sid1);
	dataset.write(&space3_data, PredType::NATIVE_UINT);

	PASSED();
    } // end of try block
    catch (Exception E)
    {
	issue_fail_msg("test_h5s_scalar_write()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_h5s_scalar_write()

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s_scalar_read
 *
 * Purpose:	Test scalar H5S (dataspace) reading code
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *              Mar 2001
 *
 * Modifications:
 *      January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hssize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *-------------------------------------------------------------------------
 */
static void test_h5s_scalar_read()
{
    hsize_t		tdims[4];	// Dimension array to test with

    // Output message about test being performed
    SUBTEST("Scalar Dataspace Reading");

    try {
	// Create file
	H5File fid1(DATAFILE, H5F_ACC_RDWR);

	// Create a dataset
	DataSet dataset = fid1.openDataSet("Dataset1");

	DataSpace sid1 = dataset.getSpace();

	// Get the number of dataspace elements
	hssize_t n = sid1.getSimpleExtentNpoints();
	verify_val((long)n, 1, "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Get the logical rank of the dataspace
	int ndims = sid1.getSimpleExtentNdims();
	verify_val(ndims, SPACE3_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	ndims = sid1.getSimpleExtentDims(tdims);
	verify_val(ndims, 0, "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);

	// Read data back and verify it
	unsigned      	rdata;      	// Scalar data read in
	dataset.read(&rdata, PredType::NATIVE_UINT);
	verify_val(rdata, space3_data, "DataSet::read", __LINE__, __FILE__);

	PASSED();
    }   // end of try block
    catch (Exception E)
    {
	// all the exceptions caused by negative returned values by C APIs
	issue_fail_msg("test_h5s_scalar_read()", __LINE__, __FILE__, E.getCDetailMsg());
    }

}   // test_h5s_scalar_read()

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s_null
 *
 * Purpose:	Test null H5S (dataspace) code
 *
 * Return:	none
 *
 * Programmer:	Raymond Lu (using C version)
 *              May 18, 2004
 *
 * Modifications:
 *      January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hssize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *-------------------------------------------------------------------------
 */
static void test_h5s_null()
{
    // Output message about test being performed
    SUBTEST("Null Dataspace Writing");

    try {
	// Create file
	H5File fid1(DATAFILE, H5F_ACC_TRUNC);

	// Create scalar dataspace
	DataSpace sid1(H5S_NULL);

	hssize_t	n;	 	// Number of dataspace elements
	n = sid1.getSimpleExtentNpoints();
	verify_val((long)n, 0, "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Create a dataset
	DataSet dataset = fid1.createDataSet("Dataset1", PredType::NATIVE_UINT,sid1);

        // Try to write nothing to the dataset
	dataset.write(&space5_data, PredType::NATIVE_INT);

        // Read the data.  Make sure no change to the buffer
	dataset.read(&space5_data, PredType::NATIVE_INT);
	verify_val(space5_data, 7, "DataSet::read", __LINE__, __FILE__);

	PASSED();
    } // end of try block
    catch (Exception E)
    {
	issue_fail_msg("test_h5s_null()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_h5s_null()

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s_compound_scalar_write
 *
 * Purpose:	Test scalar H5S (dataspace) writing for compound
 *		datatypes
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *              Mar 2001
 *
 * Modifications:
 *      January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hssize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *-------------------------------------------------------------------------
 */
static void test_h5s_compound_scalar_write()
{
    // Output message about test being performed
    SUBTEST("Compound Dataspace Writing");

    try {
	// Create file
	H5File fid1(DATAFILE, H5F_ACC_TRUNC);

	// Create the compound datatype.
	CompType tid1(sizeof(struct space4_struct));
	space4_field1_off=HOFFSET(struct space4_struct, c1);
	tid1.insertMember(SPACE4_FIELDNAME1, space4_field1_off,
		    PredType::NATIVE_SCHAR);
	space4_field2_off=HOFFSET(struct space4_struct, u);
	tid1.insertMember(SPACE4_FIELDNAME2, space4_field2_off,
		    PredType::NATIVE_UINT);
	space4_field3_off=HOFFSET(struct space4_struct, f);
	tid1.insertMember(SPACE4_FIELDNAME3, space4_field3_off,
		    PredType::NATIVE_FLOAT);
	space4_field4_off=HOFFSET(struct space4_struct, c2);
	tid1.insertMember(SPACE4_FIELDNAME4, space4_field4_off,
		    PredType::NATIVE_SCHAR);

	// Create scalar dataspace
	DataSpace sid1(SPACE3_RANK, NULL);

	// Get the number of dataspace elements
	hssize_t n = sid1.getSimpleExtentNpoints();
	verify_val((long)n, 1, "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Get the logical rank of the dataspace
	int ndims = sid1.getSimpleExtentNdims();
	verify_val(ndims, SPACE3_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	hsize_t		tdims[4];	// Dimension array to test with
	ndims = sid1.getSimpleExtentDims(tdims);
	verify_val(ndims, 0, "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);

	// Create and write a dataset
	DataSet dataset = fid1.createDataSet("Dataset1", tid1, sid1);
	dataset.write(&space4_data, tid1);

	PASSED();
    }	// end of try block
    catch (Exception E)
    {
	// all the exceptions caused by negative returned values by C APIs
	issue_fail_msg("test_h5s_compound_scalar_write()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_h5s_compound_scalar_write()

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s_compound_scalar_read
 *
 * Purpose:	Test scalar H5S (dataspace) reading for compound
 *		datatypes
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *              Mar 2001
 *
 * Modifications:
 *      January, 2005: C tests' macro VERIFY casts values to 'long' for all
 *		       cases.  Since there are no operator<< for 'long long'
 *		       or int64 in VS C++ ostream, I casted the hssize_t values
 *		       passed to verify_val to 'long' as well.  If problems
 *		       arises later, this will have to be specificly handled
 *		       with a special routine.
 *-------------------------------------------------------------------------
 */
static void test_h5s_compound_scalar_read()
{
    hsize_t		tdims[4];	// Dimension array to test with

    // Output message about test being performed
    SUBTEST("Compound Dataspace Reading");
    try {
	// Create file
	H5File fid1(DATAFILE, H5F_ACC_RDWR);

	// Create a dataset
	DataSet dataset = fid1.openDataSet("Dataset1");

	DataSpace sid1 = dataset.getSpace();

	// Get the number of dataspace elements
	hssize_t n = sid1.getSimpleExtentNpoints();
	verify_val((long)n, 1, "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Get the logical rank of the dataspace
	int ndims = sid1.getSimpleExtentNdims();
	verify_val(ndims, SPACE3_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	ndims = sid1.getSimpleExtentDims(tdims);
	verify_val(ndims, 0, "DataSpace::getSimpleExtentDims", __LINE__, __FILE__);

	// Get the datatype of this dataset.
	CompType type(dataset);

	struct space4_struct rdata; 	// Scalar data read in
	dataset.read(&rdata, type);

	// Verify read data
	if(HDmemcmp(&space4_data,&rdata,sizeof(struct space4_struct)))
	{
            cerr << "scalar data different: space4_data.c1="
		<< space4_data.c1 << ", read_data4.c1=" << rdata.c1 << endl;
            cerr << "scalar data different: space4_data.u="
		<< space4_data.u << ", read_data4.u=" << rdata.u << endl;
            cerr << "scalar data different: space4_data.f="
		<< space4_data.f << ", read_data4.f=" << rdata.f << endl;
            TestErrPrintf("scalar data different: space4_data.c1=%c, read_data4.c1=%c\n",
		space4_data.c1, rdata.c2);
	} // end if
	PASSED();
    }   // end of try block
    catch (Exception E)
    {
	// all the exceptions caused by negative returned values by C APIs
	issue_fail_msg("test_h5s_compound_scalar_read()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_h5s_compound_scalar_read()

/*-------------------------------------------------------------------------
 *
 * Function:	test_h5s
 *
 * Purpose:	Main dataspace testing routine
 *
 * Return:	none
 *
 * Programmer:	Binh-Minh Ribler (using C version)
 *              Mar 2001
 *
 * Modifications:
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_h5s()
{
    // Output message about test being performed
    //MESSAGE("Testing Dataspaces\n");
    MESSAGE(5, ("Testing Dataspaces\n"));

    test_h5s_basic();		// Test basic H5S code
    test_h5s_scalar_write();	// Test scalar H5S writing code
    test_h5s_scalar_read();	// Test scalar H5S reading code
    test_h5s_null();		// Test null H5S code
    test_h5s_compound_scalar_write();	// Test compound datatype scalar H5S writing code
    test_h5s_compound_scalar_read();	// Test compound datatype scalar H5S reading code
}   // test_h5s()


/*-------------------------------------------------------------------------
 * Function:	cleanup_h5s
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Albert Cheng
 *              July 2, 1998
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_h5s()
{
    HDremove(DATAFILE.c_str());
}   // cleanup_h5s

