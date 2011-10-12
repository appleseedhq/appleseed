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
   tattr.cpp - HDF5 C++ testing the functionalities associated with the
        C attribute interface (H5A)

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

const H5std_string	FILENAME("tattr.h5");
const H5std_string	ATTR_TMP_NAME("temp_name");
const size_t		ATTR_MAX_DIMS = 7;

/* 3-D dataset with fixed dimensions */
const int SPACE1_RANK = 3;
const int SPACE1_DIM1 = 3;
const int SPACE1_DIM2 = 15;
const int SPACE1_DIM3 = 13;

/* Object names */
const H5std_string DSET1_NAME("Dataset1");
const H5std_string GROUP1_NAME("/Group1");
const H5std_string TYPE1_NAME("/Type");

/* Attribute Rank & Dimensions */
const H5std_string ATTR1_NAME("Attr1");
const int ATTR1_RANK = 1;
const int ATTR1_DIM1 = 3;
int attr_data1[ATTR1_DIM1]={512,-234,98123}; /* Test data for 1st attribute */

const H5std_string ATTR2_NAME("Attr2");
const int ATTR2_RANK = 2;
const int ATTR2_DIM1 = 2;
const int ATTR2_DIM2 = 2;
int attr_data2[ATTR2_DIM1][ATTR2_DIM2]={{7614,-416},{197814,-3}}; /* Test data for 2nd attribute */

const H5std_string ATTR3_NAME("Attr3");
const int ATTR3_RANK = 3;
const hsize_t ATTR3_DIM1 = 2;
const hsize_t ATTR3_DIM2 = 2;
const hsize_t ATTR3_DIM3 = 2;
double attr_data3[ATTR3_DIM1][ATTR3_DIM2][ATTR3_DIM3]={{{2.3,-26.1},{0.123,-10.0}},{{981724.2,-0.91827},{2.0,23.0}}}; /* Test data for 3rd attribute */

const H5std_string ATTR4_NAME("Attr4");
const int ATTR4_RANK = 2;
const hsize_t ATTR4_DIM1 = 2;
const hsize_t ATTR4_DIM2 = 2;

const H5std_string ATTR4_FIELDNAME1("i");
const H5std_string ATTR4_FIELDNAME2("d");
const H5std_string ATTR4_FIELDNAME3("c");
size_t attr4_field1_off=0;
size_t attr4_field2_off=0;
size_t attr4_field3_off=0;
struct attr4_struct {
    int i;
    double d;
    char c;
 } attr_data4[ATTR4_DIM1][ATTR4_DIM2]={{{3,-26.1,'d'},{-100000, 0.123,'3'}},
    {{-23,981724.2,'Q'},{0,2.0,'\n'}}}; // Test data for 4th attribute

const H5std_string ATTR5_NAME("Attr5");
const int ATTR5_RANK = 0;
float attr_data5 = (float)-5.123;    	// Test data for 5th attribute

/* Info for another attribute */
const H5std_string ATTR1A_NAME("Attr1_a");
int attr_data1a[ATTR1_DIM1]={256,11945,-22107};

/****************************************************************
**
**  test_attr_basic_write(): Test basic write attribute.
**      Tests integer attributes on both datasets and groups
**
****************************************************************/
static void test_attr_basic_write()
{
    hsize_t dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
    hsize_t dims2[] = {ATTR1_DIM1};
    hsize_t dims3[] = {ATTR2_DIM1,ATTR2_DIM2};
    int     read_data1[ATTR1_DIM1]={0}; // Buffer for reading 1st attribute
    int     i;

    // Output message about test being performed
    SUBTEST("Basic Attribute Writing Functions");

    try {
	// Create file
	H5File fid1 (FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for dataset
	DataSpace ds_space (SPACE1_RANK, dims1);

	/*
	 *		Test attribute with dataset
	 */

	// Create a dataset
	DataSet dataset = fid1.createDataSet(DSET1_NAME, PredType::NATIVE_UCHAR, ds_space);

	// Create dataspace for attribute
	DataSpace att_space (ATTR1_RANK, dims2);

	// Create an attribute for the dataset
	Attribute ds_attr1 = dataset.createAttribute (ATTR1_NAME, PredType::NATIVE_INT, att_space);

	// Try creating an attribute that already exists.  This should fail
	// since two attributes cannot have the same name.  If an exception
	// is not thrown for this action by createAttribute, then throw an
	// invalid action exception.
	try {
	    Attribute invalid_attr = dataset.createAttribute (ATTR1_NAME, PredType::NATIVE_INT, att_space);

	    // continuation here, that means no exception has been thrown
	    throw InvalidActionException("H5File::createDataSet", "Library allowed overwrite of existing dataset");
	}
	catch (AttributeIException E) // catching invalid creating attribute
        {} // do nothing, exception expected

	// Write attribute information
	ds_attr1.write (PredType::NATIVE_INT, attr_data1);

	// Read attribute information immediately, without closing attribute
	ds_attr1.read (PredType::NATIVE_INT, read_data1);

	// Verify values read in
	for(i=0; i<ATTR1_DIM1; i++)
	    if(attr_data1[i]!=read_data1[i])
		TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d,read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

	// Create another attribute for this dataset
	Attribute ds_attr2 = dataset.createAttribute (ATTR1A_NAME, PredType::NATIVE_INT, att_space);

	// Write attribute information
	ds_attr2.write (PredType::NATIVE_INT, attr_data1a);

	// Read attribute information immediately, without closing attribute
	ds_attr2.read (PredType::NATIVE_INT, read_data1);

	// Verify values read in
	for(i=0; i<ATTR1_DIM1; i++)
	    if(attr_data1a[i]!=read_data1[i])
		TestErrPrintf("%d: attribute data different: attr_data1a[%d]=%d,read_data1[%d]=%d\n",__LINE__,i,attr_data1a[i],i,read_data1[i]);

	// Close both attributes
	ds_attr1.close();
	ds_attr2.close();

	/*
	 *		Test attribute with group
	 */

	// Create group in file fid1
	Group group = fid1.createGroup (GROUP1_NAME);

	// Create dataspace for attribute
	DataSpace sid3(ATTR2_RANK, dims3);

	// Create an attribute for the group
	Attribute gr_attr = group.createAttribute (ATTR2_NAME, PredType::NATIVE_INT, sid3);

	// Check storage size for attribute
	hsize_t attr_size = gr_attr.getStorageSize();
	verify_val((long)attr_size, (long)(ATTR2_DIM1*ATTR2_DIM2*sizeof(int)),
			"Attribute::getStorageSize",__LINE__,__FILE__);

	// Try to create the same attribute again (should fail)
	try {
	    Attribute invalid_attr = group.createAttribute (ATTR2_NAME, PredType::NATIVE_INT, sid3);
            // continuation here, that means no exception has been thrown
            throw InvalidActionException("H5Group::createAttribute",
			"Attempting to create an existing attribute");
	}
        catch (AttributeIException E) // catching invalid creating attribute
        {} // do nothing, exception expected

	// Write attribute information
	gr_attr.write (PredType::NATIVE_INT, attr_data2);

	// Check storage size for attribute
	attr_size = gr_attr.getStorageSize();
	verify_val((long)attr_size, (long)(ATTR2_DIM1*ATTR2_DIM2*sizeof(int)),
			"Attribute::getStorageSize",  __LINE__, __FILE__);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_basic_write()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_basic_write()

/****************************************************************
**
**  test_attr_rename(): Test renaming attribute function.
**
****************************************************************/
static void test_attr_rename()
{
    int read_data1[ATTR1_DIM1]={0}; // Buffer for reading the attribute
    int i;

	// Output message about test being performed
    SUBTEST("Rename Attribute Function");

    try {
	// Open file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	// Open the dataset
	DataSet dataset = fid1.openDataSet(DSET1_NAME);

	// Check rename

	// change attribute name
	dataset.renameAttr(ATTR1_NAME, ATTR_TMP_NAME);

	// Open attribute again
	Attribute attr1(dataset.openAttribute(ATTR_TMP_NAME));

	// Verify new attribute name
	H5std_string attr_name = attr1.getName();
	verify_val(attr_name, ATTR_TMP_NAME, "Attribute::getName", __LINE__, __FILE__);

	// Read attribute information immediately, without closing attribute
	attr1.read (PredType::NATIVE_INT, read_data1);

	// Verify values read in
	for(i=0; i<ATTR1_DIM1; i++)
	    if(attr_data1[i]!=read_data1[i])
		TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d,read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

	// Close attribute
    	attr1.close();

	// Open the second attribute
	Attribute attr2(dataset.openAttribute(ATTR1A_NAME));

	// Verify second attribute name
	H5std_string attr2_name = attr2.getName();
	verify_val(attr2_name, ATTR1A_NAME, "Attribute::getName", __LINE__, __FILE__);

	// Read attribute information immediately, without closing attribute
	attr2.read (PredType::NATIVE_INT, read_data1);

	// Verify values read in
	for(i=0; i<ATTR1_DIM1; i++)
	    if(attr_data1a[i]!=read_data1[i])
		TestErrPrintf("%d: attribute data different: attr_data1a[%d]=%d,read_data1[%d]=%d\n",__LINE__,i,attr_data1a[i],i,read_data1[i]);

	// Close attribute
	attr2.close();

	// Change first attribute back to the original name
	dataset.renameAttr(ATTR_TMP_NAME, ATTR1_NAME);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_rename()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_rename()

/********************************************************************
**
**  test_attr_basic_read(): Test basic read attribute.
**
********************************************************************/
static void test_attr_basic_read()
{
    int i, j;

    // Output message about test being performed
    SUBTEST("Basic Attribute Reading Functions");

    try {
	// Open file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	// Open the dataset
	DataSet dataset = fid1.openDataSet(DSET1_NAME);

	// Verify the correct number of attributes
	int num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 2, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Open an attribute for the dataset
	Attribute ds_attr=dataset.openAttribute(ATTR1_NAME);

	// Read attribute information
	int read_data1[ATTR1_DIM1]={0}; // Buffer for reading 1st attribute
	ds_attr.read(PredType::NATIVE_INT, &read_data1);

	// Verify values read in
	for(i=0; i<ATTR1_DIM1; i++)
            if(attr_data1[i]!=read_data1[i])
		TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d, read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

	/*
	 *		Test attribute with group
	 */
	// Open the group
	Group group = fid1.openGroup(GROUP1_NAME);

	// Verify the correct number of attributes
	num_attrs = group.getNumAttrs();
	verify_val(num_attrs, 1, "H5Group::getNumAttrs", __LINE__, __FILE__);

	// Open an attribute for the group
	Attribute gr_attr = group.openAttribute(ATTR2_NAME);

	// Buffer for reading 2nd attribute
	int read_data2[ATTR2_DIM1][ATTR2_DIM2]={{0}};

	// Read attribute information
	gr_attr.read(PredType::NATIVE_INT, read_data2);

	// Verify values read in
	for(i=0; i<ATTR2_DIM1; i++)
            for(j=0; j<ATTR2_DIM2; j++)
        	if(attr_data2[i][j]!=read_data2[i][j]) {
		    TestErrPrintf("%d: attribute data different: attr_data2[%d][%d]=%d, read_data2[%d][%d]=%d\n",__LINE__, i,j,attr_data2[i][j],i,j,read_data2[i][j]);
		}
	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_basic_read()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_basic_read()

/****************************************************************
**
**  test_attr_compound_write(): Tests compound datatype attributes
**
****************************************************************/
static void test_attr_compound_write()
{

	// Output message about test being performed
    SUBTEST("Multiple Attribute Functions");

    try {
	// Create file
	H5File fid1(FILENAME.c_str(), H5F_ACC_TRUNC);

	// Create dataspace for dataset
	hsize_t		dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
	DataSpace sid1(SPACE1_RANK, dims1);

	// Create a dataset
	DataSet dataset = fid1.createDataSet(DSET1_NAME, PredType::NATIVE_UCHAR,sid1);

	// Create the attribute datatype.
	CompType comp_type(sizeof(struct attr4_struct));

	attr4_field1_off = HOFFSET(struct attr4_struct, i);
	comp_type.insertMember(ATTR4_FIELDNAME1, attr4_field1_off, PredType::NATIVE_INT);

	attr4_field2_off = HOFFSET(struct attr4_struct, d);
	comp_type.insertMember(ATTR4_FIELDNAME2, attr4_field2_off, PredType::NATIVE_DOUBLE);

	attr4_field3_off = HOFFSET(struct attr4_struct, c);
	comp_type.insertMember(ATTR4_FIELDNAME3, attr4_field3_off, PredType::NATIVE_SCHAR);

	// Create dataspace for 1st attribute
	hsize_t		dims2[] = {ATTR4_DIM1,ATTR4_DIM2};
	DataSpace sid2(ATTR4_RANK, dims2);

	// Create complex attribute for the dataset
	Attribute attr = dataset.createAttribute(ATTR4_NAME, comp_type, sid2);

	// Try to create the same attribute again (should fail)
	try {
	    Attribute invalid_attr = dataset.createAttribute (ATTR4_NAME, comp_type, sid2);
	}
        catch (AttributeIException E) // catching invalid creating attribute
        {} // do nothing, exception expected

	// Write complex attribute data
	attr.write(comp_type, attr_data4);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_compound_write()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_compound_write()

/****************************************************************
**
**  test_attr_compound_read(): Test basic H5A (attribute) code.
**
****************************************************************/
static void test_attr_compound_read()
{
    hsize_t dims[ATTR_MAX_DIMS];	// Attribute dimensions
    size_t      size;   // Attribute datatype size as stored in file
    size_t      offset; // Attribute datatype field offset
    struct attr4_struct read_data4[ATTR4_DIM1][ATTR4_DIM2]; // Buffer for reading 4th attribute
    int     i,j;

    // Output message about test being performed
    SUBTEST("Basic Attribute Functions");

    try {
	// Open file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	// Open the dataset
	DataSet dataset = fid1.openDataSet(DSET1_NAME);

	// Verify the correct number of attributes
	int num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 1, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Open 1st attribute for the dataset
	Attribute attr = dataset.openAttribute((unsigned)0);

	/* Verify Dataspace */

	// Get the dataspace of the attribute
	DataSpace space = attr.getSpace();

	// Get the rank of the dataspace and verify it
	int rank = space.getSimpleExtentNdims();
	verify_val(rank, ATTR4_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Get the dims of the dataspace and verify them
	int ndims = space.getSimpleExtentDims(dims);
	if(dims[0]!=ATTR4_DIM1)
        verify_val((long)dims[0], (long)ATTR4_DIM1, "DataSpace::getSimpleExtentDims",__LINE__, __FILE__);
        verify_val((long)dims[1], (long)ATTR4_DIM2, "DataSpace::getSimpleExtentDims",__LINE__, __FILE__);

	// Get the class of the datatype that is used by attr
	H5T_class_t type_class = attr.getTypeClass();

	// Verify that the type is of compound datatype
	verify_val(type_class, H5T_COMPOUND, "Attribute::getTypeClass", __LINE__, __FILE__);

	// Get the compound datatype
	CompType datatype = attr.getCompType();

	// Verify the number of fields in the datatype, which must be 3
	int fields = datatype.getNmembers();
	verify_val(fields, 3, "CompType::getNmembers", __LINE__, __FILE__);

	// Verify that the fields have the same names as when the type
	// was created
	for(i=0; i<fields; i++)
	{
	    H5std_string fieldname = datatype.getMemberName(i);
	    if(!((fieldname == ATTR4_FIELDNAME1) ||
		(fieldname == ATTR4_FIELDNAME2) ||
		(fieldname == ATTR4_FIELDNAME3)))
            TestErrPrintf("%d:invalid field name for field #%d: %s\n",__LINE__,i,fieldname.c_str());
	} /* end for */

	offset = datatype.getMemberOffset(0);
	verify_val(offset, attr4_field1_off, "DataType::getMemberOffset", __LINE__, __FILE__);

	offset = datatype.getMemberOffset(1);
	verify_val(offset, attr4_field2_off, "DataType::getMemberOffset", __LINE__, __FILE__);

	offset = datatype.getMemberOffset(2);
	verify_val(offset, attr4_field3_off, "DataType::getMemberOffset", __LINE__, __FILE__);

	/* Verify each field's type, class & size */

	// Get and verify the type class of the first member
	type_class = datatype.getMemberClass(0);
	verify_val(type_class, H5T_INTEGER, "DataType::getMemberClass", __LINE__, __FILE__);
	// Get and verify the order of this member's type
	IntType i_type = datatype.getMemberIntType(0);
	H5T_order_t order = i_type.getOrder();
	verify_val(order, PredType::NATIVE_INT.getOrder(), "DataType::getOrder", __LINE__, __FILE__);

	// Get and verify the size of this member's type
	size = i_type.getSize();
	verify_val(size, PredType::NATIVE_INT.getSize(), "DataType::getSize", __LINE__, __FILE__);

	// Get and verify class, order, and size of the second member's type
	type_class = datatype.getMemberClass(1);
	verify_val(type_class, H5T_FLOAT, "DataType::getMemberClass", __LINE__, __FILE__);
	FloatType f_type = datatype.getMemberFloatType(1);
	order = f_type.getOrder();
	verify_val(order, PredType::NATIVE_DOUBLE.getOrder(), "DataType::getOrder", __LINE__, __FILE__);
	size = f_type.getSize();
	verify_val(size, PredType::NATIVE_DOUBLE.getSize(), "DataType::getSize", __LINE__, __FILE__);

	// Get and verify class, order, and size of the third member's type
	type_class = datatype.getMemberClass(2);
	verify_val(type_class, H5T_INTEGER, "DataType::getMemberClass", __LINE__, __FILE__);
	// Note: H5T_INTEGER is correct here!

	StrType s_type = datatype.getMemberStrType(2);
	order = s_type.getOrder();
	verify_val(order, PredType::NATIVE_SCHAR.getOrder(), "DataType::getOrder", __LINE__, __FILE__);
	size = s_type.getSize();
	verify_val(size, PredType::NATIVE_SCHAR.getSize(), "DataType::getSize", __LINE__, __FILE__);

	// Read attribute information
	attr.read(datatype, read_data4);

	// Verify values read in
	for(i=0; i<ATTR4_DIM1; i++)
	    for(j=0; j<ATTR4_DIM2; j++)
		if(HDmemcmp(&attr_data4[i][j],&read_data4[i][j],sizeof(struct attr4_struct))) {
		    TestErrPrintf("%d:attribute data different: attr_data4[%d][%d].i=%d, read_data4[%d][%d].i=%d\n",__LINE__,i,j,attr_data4[i][j].i,i,j,read_data4[i][j].i);
		    TestErrPrintf("%d:attribute data different: attr_data4[%d][%d].d=%f, read_data4[%d][%d].d=%f\n",__LINE__,i,j,attr_data4[i][j].d,i,j,read_data4[i][j].d);
		    TestErrPrintf("%d:attribute data different: attr_data4[%d][%d].c=%c, read_data4[%d][%d].c=%c\n",__LINE__,i,j,attr_data4[i][j].c,i,j,read_data4[i][j].c);
             } /* end if */

	// Verify name
	H5std_string attr_name = attr.getName();
	verify_val(attr_name, ATTR4_NAME, "Attribute::getName", __LINE__, __FILE__);
	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_compound_read()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_compound_read()

/****************************************************************
**
**  test_attr_scalar_write(): Test scalar attribute writing functionality.
**
****************************************************************/
static void test_attr_scalar_write()
{
    // Output message about test being performed
    SUBTEST("Basic Scalar Attribute Writing Functions");

    try {
	// Create file
	H5File fid1(FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for dataset
	hsize_t	dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
	DataSpace sid1(SPACE1_RANK, dims1);

	// Create a dataset
	DataSet dataset = fid1.createDataSet(DSET1_NAME, PredType::NATIVE_UCHAR,sid1);

	// Close dataset's dataspace
	sid1.close();

	// Create dataspace for attribute
	DataSpace att_space(ATTR5_RANK, NULL);

	// Create an attribute for the dataset
	Attribute ds_attr = dataset.createAttribute (ATTR5_NAME, PredType::NATIVE_FLOAT, att_space);

	// Try creating an attribute that already exists.  This should fail
	// since two attributes cannot have the same name.  If an exception
	// is not thrown for this action by createAttribute, then throw an
	// invalid action exception.
	try {
	    Attribute invalid_attr = dataset.createAttribute (ATTR5_NAME, PredType::NATIVE_FLOAT, att_space);

	    // continuation here, that means no exception has been thrown
	    throw InvalidActionException("H5File::createDataSet", "Library allowed overwrite of existing dataset");
	}
	catch (AttributeIException E) // catching invalid creating attribute
        {} // do nothing, exception expected

	// Write attribute information
	ds_attr.write (PredType::NATIVE_FLOAT, &attr_data5);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_scalar_write()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_scalar_write()

/****************************************************************
**
**  test_attr_scalar_read(): Test scalar attribute reading functionality.
**
****************************************************************/
static void test_attr_scalar_read()
{
    // Output message about test being performed
    SUBTEST("Basic Scalar Attribute Reading Functions");

    try {
	// Open file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	// Open the dataset
	DataSet dataset = fid1.openDataSet(DSET1_NAME);

	// Verify the correct number of attributes
	int num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 1, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Open an attribute for the dataset
	Attribute ds_attr=dataset.openAttribute(ATTR5_NAME);

	// Read attribute information
	float read_data2=0.0;  // Buffer for reading 1st attribute
	ds_attr.read(PredType::NATIVE_FLOAT,&read_data2);
	verify_val(read_data2, attr_data5, "Attribute::read", __LINE__, __FILE__);

	// Get the dataspace of the attribute
	DataSpace att_space = ds_attr.getSpace();

	// Make certain the dataspace is scalar
	H5S_class_t space_type = att_space.getSimpleExtentType();
	verify_val(space_type, H5S_SCALAR, "DataSpace::getSimpleExtentType", __LINE__, __FILE__);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_scalar_read()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_scalar_read()

/****************************************************************
**
**  test_attr_mult_write(): Test multiple attributes
**
****************************************************************/
static void test_attr_mult_write()
{
    // Output message about test being performed
    SUBTEST("Multiple Attribute Writing Functions");

    try {
	// Create file
	H5File fid1 (FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for dataset
	hsize_t	dims1[] = {SPACE1_DIM1, SPACE1_DIM2, SPACE1_DIM3};
	DataSpace ds_space (SPACE1_RANK, dims1);

	// Create a dataset
	DataSet dataset = fid1.createDataSet(DSET1_NAME, PredType::NATIVE_UCHAR, ds_space);

	// Create dataspace for 1st attribute
	hsize_t	dims2[] = {ATTR1_DIM1};
	DataSpace att_space (ATTR1_RANK, dims2);

	// Create 1st attribute for the dataset
	Attribute ds_attr = dataset.createAttribute (ATTR1_NAME, PredType::NATIVE_INT, att_space);

	// Write attribute information
	ds_attr.write (PredType::NATIVE_INT, attr_data1);

	// Create dataspace for 2nd attribute
	hsize_t	dims3[] = {ATTR2_DIM1,ATTR2_DIM2};
	DataSpace att2_space (ATTR2_RANK, dims3);

	// Create 2nd attribute for the dataset
	Attribute ds_attr2 = dataset.createAttribute (ATTR2_NAME, PredType::NATIVE_INT, att2_space);

	// Write 2nd attribute information
	ds_attr2.write (PredType::NATIVE_INT, attr_data2);

	// Create dataspace for 3rd attribute
	hsize_t	dims4[] = {ATTR3_DIM1,ATTR3_DIM2,ATTR3_DIM3};
	DataSpace att3_space (ATTR3_RANK, dims4);

	// Create 3rd attribute for the dataset
	Attribute ds_attr3 = dataset.createAttribute (ATTR3_NAME, PredType::NATIVE_DOUBLE, att3_space);

	// Try creating an attribute that already exists.  This should fail
	// since two attributes cannot have the same name.  If an exception
	// is not thrown for this action by createAttribute, then throw an
	// invalid action exception.
	try {
	    Attribute invalid_attr = dataset.createAttribute (ATTR3_NAME, PredType::NATIVE_DOUBLE, att3_space);

	    // continuation here, that means no exception has been thrown
	    throw InvalidActionException("DataSet::createAttribute", "Attempting to create a duplicate attribute");
	}
	catch (AttributeIException E) // catching invalid creating attribute
        {} // do nothing, exception expected

	// Write 3rd attribute information
	ds_attr3.write (PredType::NATIVE_DOUBLE, attr_data3);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_mult_write()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_mult_write()

/****************************************************************
**
**  test_attr_mult_read(): Test reading multiple attributes.
**
****************************************************************/
static void test_attr_mult_read()
{
    int     read_data1[ATTR1_DIM1]={0}; // Buffer for reading 1st attribute
    int     read_data2[ATTR2_DIM1][ATTR2_DIM2]={{0}}; // Buffer for reading 2nd attribute
    double  read_data3[ATTR3_DIM1][ATTR3_DIM2][ATTR3_DIM3]={{{0}}}; // Buffer for reading 3rd attribute
    int     i,j,k;

	// Output message about test being performed
    SUBTEST("Multiple Attribute Reading Functions");

    try {
	// Open file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	// Open the dataset
	DataSet dataset = fid1.openDataSet(DSET1_NAME);

	// Verify the correct number of attributes
	int num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 3, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Open 1st attribute for the dataset
	Attribute attr = dataset.openAttribute((unsigned)0);

	/* Verify Dataspace */

	// Get the dataspace of the attribute
	DataSpace space = attr.getSpace();

	// Get the rank of the dataspace and verify it
	int rank = space.getSimpleExtentNdims();
	verify_val(rank, ATTR1_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Get the dims of the dataspace and verify them
	hsize_t dims[ATTR_MAX_DIMS];    // Attribute dimensions
	int ndims = space.getSimpleExtentDims(dims);
	if(dims[0]!=ATTR1_DIM1)
	    TestErrPrintf("%d:attribute dimensions different: dims[0]=%d, should be %d\n",__LINE__,(int)dims[0],ATTR1_DIM1);

	/* Verify Datatype */

        // Get the class of the datatype that is used by attr
        H5T_class_t type_class = attr.getTypeClass();

        // Verify that the type is of integer datatype
        verify_val(type_class, H5T_INTEGER, "Attribute::getTypeClass", __LINE__, __FILE__);

    	// Get the integer datatype
        IntType i_type1 = attr.getIntType();

	// Get and verify the order of this type
	H5T_order_t order = i_type1.getOrder();
	verify_val(order, PredType::NATIVE_INT.getOrder(), "DataType::getOrder", __LINE__, __FILE__);

	// Get and verify the size of this type
	size_t size = i_type1.getSize();
	verify_val(size, PredType::NATIVE_INT.getSize(), "DataType::getSize", __LINE__, __FILE__);

	// Read attribute information
	attr.read(PredType::NATIVE_INT, read_data1);

	// Verify values read in
	for(i=0; i<ATTR1_DIM1; i++)
	    if(attr_data1[i]!=read_data1[i])
		TestErrPrintf("%d: attribute data different: attr_data1[%d]=%d,read_data1[%d]=%d\n",__LINE__,i,attr_data1[i],i,read_data1[i]);

	// Verify Name
	H5std_string attr_name = attr.getName();
	verify_val(attr_name, ATTR1_NAME, "DataType::getName", __LINE__, __FILE__);

	attr.close();
	space.close();

	// Open 2nd attribute for the dataset
	attr = dataset.openAttribute((unsigned)1);

	/* Verify Dataspace */

	// Get the dataspace of the attribute
	space = attr.getSpace();

	// Get the rank of the dataspace and verify it
	rank = space.getSimpleExtentNdims();
	verify_val(rank, ATTR2_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Get the dims of the dataspace and verify them
	ndims = space.getSimpleExtentDims(dims);
	if(dims[0]!=ATTR2_DIM1)
	    TestErrPrintf("%d:attribute dimensions different: dims[0]=%d, should be %d\n",__LINE__,(int)dims[0],ATTR2_DIM1);
	if(dims[1]!=ATTR2_DIM2)
	    TestErrPrintf("%d:attribute dimensions different: dims[1]=%d, should be %d\n",__LINE__,(int)dims[1],ATTR2_DIM2);

	/* Verify Datatype */

        // Get the class of the datatype that is used by attr
        type_class = attr.getTypeClass();

        // Verify that the type is of integer datatype
        verify_val(type_class, H5T_INTEGER, "Attribute::getTypeClass", __LINE__, __FILE__);

    	// Get the integer datatype
        IntType i_type2 = attr.getIntType();

	// Get and verify the order of this type
	order = i_type2.getOrder();
	verify_val(order, PredType::NATIVE_INT.getOrder(), "DataType::getOrder", __LINE__, __FILE__);

	// Get and verify the size of this type
	size = i_type2.getSize();
	verify_val(size, PredType::NATIVE_INT.getSize(), "DataType::getSize", __LINE__, __FILE__);

	// Read attribute information
	attr.read(PredType::NATIVE_INT, read_data2);
	//attr.read(i_type, read_data2);

	// Verify values read in
	for(i=0; i<ATTR2_DIM1; i++)
	  for(j=0; j<ATTR2_DIM2; j++)
            if(attr_data2[i][j]!=read_data2[i][j])
                TestErrPrintf("%d: attribute data different: attr_data2[%d][%d]=%d, read_data2[%d][%d]=%d\n",__LINE__,i,j,attr_data2[i][j],i,j,read_data2[i][j]);

	// Verify Name
	attr_name = attr.getName();
	verify_val(attr_name, ATTR2_NAME, "DataType::getName", __LINE__, __FILE__);
	attr.close();
	space.close();

	// Open 3rd attribute for the dataset
	attr = dataset.openAttribute((unsigned)2);

	/* Verify Dataspace */

	// Get the dataspace of the attribute
	space = attr.getSpace();

	// Get the rank of the dataspace and verify it
	rank = space.getSimpleExtentNdims();
	verify_val(rank, ATTR3_RANK, "DataSpace::getSimpleExtentNdims", __LINE__, __FILE__);

	// Get the dims of the dataspace and verify them
	ndims = space.getSimpleExtentDims(dims);
	verify_val((long)dims[0],(long)ATTR3_DIM1,"attribute dimensions",__FILE__,__LINE__);
	verify_val((long)dims[1],(long)ATTR3_DIM2,"attribute dimensions",__FILE__,__LINE__);
	verify_val((long)dims[2],(long)ATTR3_DIM3,"attribute dimensions",__FILE__,__LINE__);

	/* Verify Datatype */

        // Get the class of the datatype that is used by attr
        type_class = attr.getTypeClass();

        // Verify that the type is of compound datatype
        verify_val(type_class, H5T_FLOAT, "Attribute::getTypeClass", __LINE__, __FILE__);

    	// Get the double datatype
        FloatType f_type = attr.getFloatType();

	// Get and verify the order of this type
	order = f_type.getOrder();
	verify_val(order, PredType::NATIVE_DOUBLE.getOrder(), "DataType::getOrder", __LINE__, __FILE__);

	// Get and verify the size of this type
	size = f_type.getSize();
	verify_val(size, PredType::NATIVE_DOUBLE.getSize(), "DataType::getSize", __LINE__, __FILE__);

	// Read attribute information
	attr.read(PredType::NATIVE_DOUBLE, read_data3);

	// Verify values read in
	for(i=0; i<ATTR3_DIM1; i++)
	    for(j=0; j<ATTR3_DIM2; j++)
		for(k=0; k<ATTR3_DIM3; k++)
		    if(attr_data3[i][j][k]!=read_data3[i][j][k])
			TestErrPrintf("%d: attribute data different: attr_data3[%d][%d][%d]=%f, read_data3[%d][%d][%d]=%f\n",__LINE__,i,j,k,attr_data3[i][j][k],i,j,k,read_data3[i][j][k]);

	// Verify Name
	attr_name = attr.getName();
	verify_val(attr_name, ATTR3_NAME, "DataType::getName", __LINE__, __FILE__);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_mult_read()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_mult_read()

/****************************************************************
**
**  test_attr_delete(): Test deleting attribute from different
**			hdf5 objects.
**
****************************************************************/
static void test_attr_delete()
{
    H5std_string  attr_name; // Buffer for attribute names

	// Output message about test being performed
    SUBTEST("Removing Attribute Function");

    try {
	// Open file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	// Open the dataset
	DataSet dataset = fid1.openDataSet(DSET1_NAME);

	// Verify the correct number of attributes
	int num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 3, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Try to delete bogus attribute, should fail.
	try {
	    dataset.removeAttr("Bogus");

	    // continuation here, that means no exception has been thrown
	    throw InvalidActionException("DataSet::removeAttr", "Attempting to remove non-existing attribute");
	}
	catch (AttributeIException E) // catching invalid removing attribute
        {} // do nothing, exception expected

	// Verify the correct number of attributes
	num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 3, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Delete middle (2nd) attribute
	dataset.removeAttr(ATTR2_NAME);

	// Verify the correct number of attributes
	num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 2, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Open 1st attribute for the dataset
	Attribute attr = dataset.openAttribute((unsigned)0);

	// Verify Name
	attr_name = attr.getName();
	verify_val(attr_name, ATTR1_NAME, "Attribute::getName", __LINE__, __FILE__);

	// Close attribute
	attr.close();

	// Open last (formally 3rd) attribute for the dataset
	attr = dataset.openAttribute((unsigned)1);

	// Verify Name
	attr_name = attr.getName();
	verify_val(attr_name, ATTR3_NAME, "Attribute::getName", __LINE__, __FILE__);

	attr.close();

	// Delete first attribute
	dataset.removeAttr(ATTR1_NAME);

	// Verify the correct number of attributes
	num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 1, "H5Object::getNumAttrs", __LINE__, __FILE__);

	// Open last (formally 3rd) attribute for the dataset
	attr = dataset.openAttribute((unsigned)0);

	// Verify Name
	attr_name = attr.getName();
	verify_val(attr_name, ATTR3_NAME, "Attribute::getName", __LINE__, __FILE__);
	// Close attribute
	attr.close();

	// Delete first attribute
	dataset.removeAttr(ATTR3_NAME);

	// Verify the correct number of attributes
	num_attrs = dataset.getNumAttrs();
	verify_val(num_attrs, 0, "H5Object::getNumAttrs", __LINE__, __FILE__);

	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_delete()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_delete()

/****************************************************************
**
**  test_attr_dtype_shared(): Test code for using shared datatypes
**				in attributes.
**
****************************************************************/
static void test_attr_dtype_shared()
{
    int data=8;                 /* Data to write */
    int rdata=0;                /* Read read in */
#ifndef H5_NO_DEPRECATED_SYMBOLS
    H5G_stat_t statbuf;         /* Object's information */
#endif /* H5_NO_DEPRECATED_SYMBOLS */
    h5_stat_size_t filesize;             /* Size of file after modifications */

    // Output message about test being performed
    SUBTEST("Shared Datatypes with Attributes");

    try {
	// Create a file
	H5File fid1(FILENAME, H5F_ACC_TRUNC);

	// Close file
	fid1.close();

	// Get size of file
	h5_stat_size_t empty_filesize;       // Size of empty file
	empty_filesize = h5_get_file_size(FILENAME.c_str(), H5P_DEFAULT);
	if (empty_filesize < 0)
            TestErrPrintf("Line %d: file size wrong!\n", __LINE__);

	// Open the file again
	fid1.openFile(FILENAME, H5F_ACC_RDWR);

	// Enclosing to work around the issue of unused variables and/or
	// objects created by copy constructors stay around until end of
	// scope, causing incorrect number of ref counts.
	{ // First enclosed block

	// Create a datatype to commit and use
	IntType dtype(PredType::NATIVE_INT);

	// Commit datatype to file
	dtype.commit(fid1, TYPE1_NAME);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 1, "DataType::getObjinfo", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Create dataspace for dataset
	DataSpace dspace;

	DataSet dset = fid1.createDataSet(DSET1_NAME, dtype, dspace);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 2, "H5File::getObjinfo", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Create attribute on dataset
	Attribute attr = dset.createAttribute(ATTR1_NAME,dtype,dspace);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 3, "DataSet::getObjinfo", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

        // Close attribute
        attr.close();

	// Delete attribute
	dset.removeAttr(ATTR1_NAME);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 2, "DataSet::getObjinfo after DataSet::removeAttr", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

        // Create attribute on dataset
        attr = dset.createAttribute(ATTR1_NAME,dtype,dspace);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 3, "DataSet::createAttribute", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Write data into the attribute
	attr.write(PredType::NATIVE_INT,&data);

	// Close attribute, dataset, dataspace, datatype, and file
	attr.close();
	dset.close();
	dspace.close();
	dtype.close();
	} // end of first enclosing

	fid1.close();

	// Open the file again
	fid1.openFile(FILENAME, H5F_ACC_RDWR);

	{ // Second enclosed block...

	// Open dataset
	DataSet *dset2 = new DataSet (fid1.openDataSet(DSET1_NAME));

	// Open attribute
	Attribute *attr2 = new Attribute (dset2->openAttribute(ATTR1_NAME));

	// Read data from the attribute
	attr2->read(PredType::NATIVE_INT, &rdata);
	verify_val(data, rdata, "Attribute::read", __LINE__, __FILE__);

	// Close attribute and dataset
	delete attr2;
	delete dset2;

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 3, "DataSet::openAttribute", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */
	} // end of second enclosing

	// Unlink the dataset
	fid1.unlink(DSET1_NAME);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Check reference count on named datatype
	fid1.getObjinfo(TYPE1_NAME, statbuf);
	verify_val((int)statbuf.nlink, 1, "H5File::unlink", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Unlink the named datatype
	fid1.unlink(TYPE1_NAME);

	// Close file
	fid1.close();

	// Check size of file
	filesize = h5_get_file_size(FILENAME.c_str(), H5P_DEFAULT);
	verify_val((long)filesize, (long)empty_filesize, "Checking file size", __LINE__, __FILE__);

	PASSED();
    }   // end try block

    catch (Exception E) {
	issue_fail_msg("test_attr_dtype_shared()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_attr_dtype_shared()

/****************************************************************
**
**  test_string_attr(): Test read/write string attribute.
**      Tests string attributes on groups.
**
****************************************************************/
/* Info for a string attribute */
const H5std_string ATTR1_FL_STR_NAME("String_attr 1");
const H5std_string ATTR2_FL_STR_NAME("String_attr 2");
const H5std_string ATTR_VL_STR_NAME("String_attr");
const H5std_string ATTRSTR_DATA("String Attribute");
const int ATTR_LEN = 17;

static void test_string_attr()
{
    // Output message about test being performed
    SUBTEST("I/O on FL and VL String Attributes");

    try {
	// Create file
	H5File fid1(FILENAME, H5F_ACC_RDWR);

	//
	// Fixed-lenth string attributes
	//
	// Create a fixed-length string datatype to refer to.
	StrType fls_type(0, ATTR_LEN);

	// Open the root group.
	Group root = fid1.openGroup("/");

	// Create dataspace for the attribute.
	DataSpace att_space (H5S_SCALAR);

	/* Test Attribute::write(...,const void *buf) with Fixed len string */

	// Create an attribute for the root group.
	Attribute gr_flattr1 = root.createAttribute(ATTR1_FL_STR_NAME, fls_type, att_space);

	// Write data to the attribute.
	gr_flattr1.write(fls_type, ATTRSTR_DATA.c_str());

	/* Test Attribute::write(...,const H5std_string& strg) with FL string */

	// Create an attribute for the root group.
	Attribute gr_flattr2 = root.createAttribute(ATTR2_FL_STR_NAME, fls_type, att_space);

	// Write data to the attribute.
	gr_flattr2.write(fls_type, ATTRSTR_DATA);

	/* Test Attribute::read(...,void *buf) with FL string */

	// Read and verify the attribute string as a string of chars.
	char flstring_att_check[ATTR_LEN];
	gr_flattr1.read(fls_type, flstring_att_check);
	if(HDstrcmp(flstring_att_check, ATTRSTR_DATA.c_str())!=0)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,flstring_att_check=%s\n",__LINE__, ATTRSTR_DATA.c_str(), flstring_att_check);

	// Read and verify the attribute string as a string of chars; buffer
	// is dynamically allocated.
	size_t attr_size = gr_flattr1.getInMemDataSize();
	char *fl_dyn_string_att_check;
	fl_dyn_string_att_check = new char[attr_size+1];
	gr_flattr1.read(fls_type, fl_dyn_string_att_check);
	if(HDstrcmp(fl_dyn_string_att_check, ATTRSTR_DATA.c_str())!=0)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,flstring_att_check=%s\n",__LINE__, ATTRSTR_DATA.c_str(), fl_dyn_string_att_check);
	delete []fl_dyn_string_att_check;

	/* Test Attribute::read(...,H5std_string& strg) with FL string */

	// Read and verify the attribute string as an std::string.
	H5std_string read_flstr1;
	gr_flattr1.read(fls_type, read_flstr1);
	if (read_flstr1 != ATTRSTR_DATA)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,read_flstr1=%s\n",__LINE__, ATTRSTR_DATA.c_str(), read_flstr1.c_str());

	// Read and verify the attribute string as a string of chars.
	HDstrcpy(flstring_att_check, "");
	gr_flattr2.read(fls_type, flstring_att_check);
	if(HDstrcmp(flstring_att_check, ATTRSTR_DATA.c_str())!=0)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,flstring_att_check=%s\n",__LINE__, ATTRSTR_DATA.c_str(), flstring_att_check);

	/* Test Attribute::read(...,H5std_string& strg) with FL string */

	// Read and verify the attribute string as an std::string.
	H5std_string read_flstr2;
	gr_flattr2.read(fls_type, read_flstr2);
	if (read_flstr2 != ATTRSTR_DATA)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,read_flstr2=%s\n",__LINE__, ATTRSTR_DATA.c_str(), read_flstr2.c_str());

	//
	// Variable-lenth string attributes
	//
	// Create a variable length string datatype to refer to.
	StrType vls_type(0, H5T_VARIABLE);

	// Create an attribute for the root group.
	Attribute gr_vlattr = root.createAttribute(ATTR_VL_STR_NAME, vls_type, att_space);

	// Write data to the attribute.
	gr_vlattr.write(vls_type, ATTRSTR_DATA);

	/* Test Attribute::read(...,void *buf) with Variable len string */
	// Read and verify the attribute string as a string of chars.
	char *string_att_check;
	gr_vlattr.read(vls_type, &string_att_check);
	if(HDstrcmp(string_att_check, ATTRSTR_DATA.c_str())!=0)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,string_att_check=%s\n",__LINE__, ATTRSTR_DATA.c_str(), string_att_check);
	HDfree(string_att_check);

	/* Test Attribute::read(...,H5std_string& strg) with VL string */
	// Read and verify the attribute string as an std::string.
	H5std_string read_str;
	gr_vlattr.read(vls_type, read_str);
	if (read_str != ATTRSTR_DATA)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,read_str=%s\n",__LINE__, ATTRSTR_DATA.c_str(), read_str.c_str());
	PASSED();
    } // end try block

    catch (Exception E) {
	issue_fail_msg("test_string_attr()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_string_attr()

/****************************************************************
**
**  test_attr(): Main attribute testing routine.
**
****************************************************************/
#ifdef __cplusplus
extern "C"
#endif
void test_attr()
{
    // Output message about test being performed
    //MESSAGE("Testing Attributes\n");
    MESSAGE(5, ("Testing Attributes\n"));

    test_attr_basic_write();	// Test basic H5A writing code
    test_attr_rename();		// Test renaming attribute
    test_attr_basic_read(); 	// Test basic H5A reading code

    test_attr_compound_write();	// Test complex datatype H5A writing code
    test_attr_compound_read();	// Test complex datatype H5A reading code

    test_attr_scalar_write();	// Test scalar dataspace H5A writing code
    test_attr_scalar_read();	// Test scalar dataspace H5A reading code

    test_attr_mult_write();	// Test writing multiple attributes
    test_attr_mult_read();	// Test reading multiple attributes
    test_attr_delete();		// Test deleting attributes

    test_attr_dtype_shared();	// Test using shared datatypes in attributes

    test_string_attr();		// Test read/write string attribute

}   // test_attr()

/*-------------------------------------------------------------------------
 * Function:	cleanup_attr
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
void cleanup_attr()
{
    HDremove(FILENAME.c_str());
}

