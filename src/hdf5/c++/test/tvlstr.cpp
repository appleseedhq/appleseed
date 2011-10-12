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
   tvlstr.cpp - HDF5 C++ testing the Variable-Length String functionality

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

#include "H5Cpp.h"      // C++ API header file

#ifndef H5_NO_NAMESPACE
    using namespace H5;
#endif

#include "h5cpputil.h"  // C++ utilility header file

// Data file used in most test functions
const H5std_string FILENAME("tvlstr.h5");

// 1-D dataset with fixed dimensions
const int SPACE1_RANK = 1;
const hsize_t SPACE1_DIM1 = 4;

// Utility functions - not used now, later though.
void *test_vlstr_alloc_custom(size_t size, void *info);
void test_vlstr_free_custom(void *mem, void *info);

/****************************************************************
**
**  test_vlstr_alloc_custom(): Test VL datatype custom memory
**	allocation routines.  This routine just uses malloc to
**	allocate the memory and increments the amount of memory
**	allocated.  It is passed into setVlenMemManager.
**
**  Note: exact copy from the C version.
**
****************************************************************/
void *test_vlstr_alloc_custom(size_t size, void *info)
{
    void *ret_value=NULL;	// Pointer to return
    size_t *mem_used=(size_t *)info;  // Get the pointer to the memory used
    size_t extra;		// Extra space needed

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */

    extra=MAX(sizeof(void *),sizeof(size_t));

    if((ret_value=HDmalloc(extra+size))!=NULL) {
	*(size_t *)ret_value=size;
	*mem_used+=size;
    } // end if
    ret_value = ((unsigned char *)ret_value) + extra;

    return(ret_value);
}

/****************************************************************
**
**  test_vlstr_free_custom(): Test VL datatype custom memory
**	allocation routines.  This routine just uses free to
**	release the memory and decrements the amount of memory
**	allocated.  It is passed into setVlenMemManager.
**
**  Note: exact copy from the C version.
**
****************************************************************/
void test_vlstr_free_custom(void *_mem, void *info)
{
    unsigned char *mem;
    size_t *mem_used=(size_t *)info;  // Get the pointer to the memory used
    size_t extra;		// Extra space needed

    /*
     *  This weird contortion is required on the DEC Alpha to keep the
     *  alignment correct - QAK
     */

    extra=MAX(sizeof(void *),sizeof(size_t));

    if(_mem!=NULL) {
        mem=((unsigned char *)_mem)-extra;
        *mem_used-=*(size_t *)mem;
        HDfree(mem);
    } // end if
}

/*-------------------------------------------------------------------------
 * Function:	test_vlstring_dataset
 *
 * Purpose:	Test writing/reading VL strings on datasets.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
// String for testing datasets
static char stastring_ds_write[1]={'A'};

// Info for a string dataset
const H5std_string DSET1_NAME("String_ds");
const H5std_string DSET1_DATA("String Dataset");

static void test_vlstring_dataset()
{
    char *dynstring_ds_write = NULL;
    char *string_ds_check = NULL;

    // Output message about test being performed
    SUBTEST("VL String on Datasets");

    try {
	// Open the file
	H5File file1(FILENAME, H5F_ACC_TRUNC);

	// Create a datatype to refer to.
	StrType vlst(0, H5T_VARIABLE);

	// Open the root group.
	Group root = file1.openGroup("/");

	// Create dataspace for the dataset.
	DataSpace ds_space (H5S_SCALAR);

	// Create an dataset in the root group.
	DataSet dset1 = root.createDataSet(DSET1_NAME, vlst, ds_space);

	// Write data to the dataset.
	dset1.write(DSET1_DATA, vlst);

	// Read and verify the dataset string as a string of chars.
	dset1.read(&string_ds_check, vlst);
	if(HDstrcmp(string_ds_check, DSET1_DATA.c_str())!=0)
	    TestErrPrintf("Line %d: Attribute data different: DSET1_DATA=%s,string_ds_check=%s\n",__LINE__, DSET1_DATA.c_str(), string_ds_check);

	HDfree(string_ds_check);  // note: no need for std::string test
        string_ds_check = NULL;

	// Read and verify the dataset string as an std::string.
	H5std_string read_str;
	dset1.read(read_str, vlst);
	if (read_str != DSET1_DATA)
	    TestErrPrintf("Line %d: Attribute data different: DSET1_DATA=%s,read_str=%s\n",__LINE__, DSET1_DATA.c_str(), read_str.c_str());

	// Close the dataset.
	dset1.close();

	// Test scalar type dataset with 1 value.
	dset1 = root.createDataSet("test_scalar_small", vlst, ds_space);

	dynstring_ds_write = (char*)HDcalloc(1, sizeof(char));
	HDmemset(dynstring_ds_write, 'A', 1);

	// Write data to the dataset, then read it back.
	dset1.write(&dynstring_ds_write, vlst);
	dset1.read(&string_ds_check, vlst);

	// Verify data read.
	if(HDstrcmp(string_ds_check,dynstring_ds_write)!=0)
	    TestErrPrintf("VL string datasets don't match!, dynstring_ds_write=%s, string_ds_check=%s\n",dynstring_ds_write,string_ds_check);
	HDfree(string_ds_check);
        string_ds_check = NULL;
	dset1.close();

	// Open dataset DSET1_NAME again.
	dset1 = root.openDataSet(DSET1_NAME);

	// Close dataset and file
	dset1.close();
	file1.close();

	PASSED();
    } // end try block

    // Catch all exceptions.
    catch (Exception E) {
	issue_fail_msg("test_vlstring_dataset()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(dynstring_ds_write)
        HDfree(dynstring_ds_write);
    if(string_ds_check)
	HDfree(string_ds_check);
}   // test_vlstring_dataset()

/*-------------------------------------------------------------------------
 * Function:	test_vlstring_array_dataset
 *
 * Purpose:	Test writing/reading VL string array to/from datasets.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler
 *		July, 2009
 *
 *-------------------------------------------------------------------------
 */
const H5std_string DSSTRARR_NAME("StringArray_dset");
static void test_vlstring_array_dataset()
{
    const char *string_ds_array[SPACE1_DIM1]= {
        "Line 1", "Line 2", "Line 3", "Line 4"
        };   // Information to write

    // Output message about test being performed
    SUBTEST("VL String Array on Datasets");

    H5File* file1 = NULL;
    try {
        // Create file.
	file1 = new H5File(FILENAME, H5F_ACC_RDWR);

        // Create dataspace for datasets.
        hsize_t dims1[] = {SPACE1_DIM1};
        DataSpace ds_space(SPACE1_RANK, dims1);

	// Create a datatype to refer to.
	StrType vlst(0, H5T_VARIABLE);

	// Create and write a dataset.
	DataSet dataset(file1->createDataSet(DSSTRARR_NAME, vlst, ds_space));
	dataset.write(string_ds_array, vlst);

	// Read and verify the dataset using strings of chars as buffer.
	// Note: reading by array of H5std_string doesn't work yet.
	char *string_ds_check[SPACE1_DIM1];
	dataset.read(string_ds_check, vlst);

	int ii;
	for (ii = 0; ii < SPACE1_DIM1; ii++)
	{
	    if(HDstrcmp(string_ds_check[ii], string_ds_array[ii])!=0)
		TestErrPrintf("Line %d: Dataset data different: written=%s,read=%s\n",__LINE__, string_ds_array[ii], string_ds_check[ii]);

	    HDfree(string_ds_check[ii]);
	}

	// Close objects that are no longer needed.
	dataset.close();
	ds_space.close();

	//
	// Test with scalar data space.
	//

	// Create H5S_SCALAR data space.
	DataSpace scalar_space;

	// Create and write another dataset.
	DataSet dataset2(file1->createDataSet("Dataset2", vlst, scalar_space));
	char *wdata2 = (char*)HDcalloc(65534, sizeof(char));
	HDmemset(wdata2, 'A', 65533);
	dataset2.write(&wdata2, vlst);

	char *rdata2;
	dataset2.read(&rdata2, vlst);
	if (HDstrcmp(wdata2, rdata2)!=0)
	    TestErrPrintf("Line %d: Dataset data different: written=%s,read=%s\n",__LINE__, wdata2, rdata2);

	// Release resources from second dataset operation.
	scalar_space.close();
	dataset2.close();
	HDfree(wdata2);
	HDfree(rdata2);

	// Close objects and file.
	dataset2.close();
	vlst.close();
	file1->close();

	PASSED();
    } // end try

    // Catch all exceptions.
    catch (Exception E)
    {
	issue_fail_msg("test_vlstring_array_dataset()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(file1)
	delete file1;
} // end test_vlstring_array_dataset()

/*-------------------------------------------------------------------------
 * Function:	test_vlstrings_special
 *
 * Purpose:	Test VL string code for special string cases, nil and
 *		zero-sized.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
static void test_vlstrings_special()
{
    const char *wdata[SPACE1_DIM1] = {"one", "two", "", "four"};
    const char *wdata2[SPACE1_DIM1] = {NULL, NULL, NULL, NULL};
    char *rdata[SPACE1_DIM1];   // Information read in

    // Output message about test being performed.
    SUBTEST("Special VL Strings");

    try {
	// Create file.
	H5File file1(FILENAME, H5F_ACC_TRUNC);

        // Create dataspace for datasets.
        hsize_t dims1[] = {SPACE1_DIM1};
        DataSpace sid1(SPACE1_RANK, dims1);

	// Create a datatype to refer to.
	StrType vlst(0, H5T_VARIABLE);

	// Create a dataset.
	DataSet dataset(file1.createDataSet("Dataset3", vlst, sid1));

	// Read from the dataset before writing data.
	dataset.read(rdata, vlst);

	// Check data read in.
	hsize_t i;      	// counting variable
	for (i=0; i<SPACE1_DIM1; i++)
	    if(rdata[i]!=NULL)
		TestErrPrintf("VL doesn't match!, rdata[%d]=%p\n",(int)i,rdata[i]);

	// Write dataset to disk, then read it back.
	dataset.write(wdata, vlst);
	dataset.read(rdata, vlst);

	// Compare data read in.
	for (i = 0; i < SPACE1_DIM1; i++) {
	    size_t wlen = HDstrlen(wdata[i]);
	    size_t rlen = HDstrlen(rdata[i]);
	    if(wlen != rlen) {
		TestErrPrintf("VL data lengths don't match!, strlen(wdata[%d])=%u, strlen(rdata[%d])=%u\n", (int)i, (unsigned)wlen, (int)i, (unsigned)rlen);
		continue;
	    } // end if
	    if(HDstrcmp(wdata[i],rdata[i]) != 0) {
		TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n", (int)i, wdata[i], (int)i, rdata[i]);
		continue;
	    } // end if
	} // end for

	// Reclaim the read VL data.
	DataSet::vlenReclaim((void *)rdata, vlst, sid1);

	// Close Dataset.
	dataset.close();

	/*
	 * Create another dataset to test nil strings.
	 */

	// Create the property list and set the fill value for the second
	// dataset.
	DSetCreatPropList dcpl;
	char *fill = NULL;	// Fill value
	dcpl.setFillValue(vlst, &fill);
	dataset = file1.createDataSet("Dataset4", vlst, sid1, dcpl);

	// Close dataset creation property list.
	dcpl.close();

	// Read from dataset before writing data.
	dataset.read(rdata, vlst);

	// Check data read in.
	for (i=0; i<SPACE1_DIM1; i++)
	  if(rdata[i]!=NULL)
	    TestErrPrintf("VL doesn't match!, rdata[%d]=%p\n",(int)i,rdata[i]);

	// Try to write nil strings to disk.
	dataset.write(wdata2, vlst);

	// Read nil strings back from disk.
	dataset.read(rdata, vlst);

	// Check data read in.
	for (i=0; i<SPACE1_DIM1; i++)
	  if(rdata[i]!=NULL)
	    TestErrPrintf("VL doesn't match!, rdata[%d]=%p\n",(int)i,rdata[i]);

	// Close objects and file.
	dataset.close();
	vlst.close();
	sid1.close();
	file1.close();

	PASSED();
    } // end try

    // Catch all exceptions.
    catch (Exception E)
    {
	issue_fail_msg("test_vlstrings_special()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_vlstrings_special

/*-------------------------------------------------------------------------
 * Function:	test_vlstring_type
 *
 * Purpose:	Test if VL string is treated as string.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
const H5std_string      VLSTR_TYPE("vl_string_type");
static void test_vlstring_type()
{
    // Output message about test being performed.
    SUBTEST("VL String Type");

    H5File* file1 = NULL;
    try {
	// Open file.
	file1 = new H5File(FILENAME, H5F_ACC_RDWR);

	// Create a datatype to refer to.
	StrType vlst(PredType::C_S1);

	// Change padding and verify it.
	vlst.setStrpad(H5T_STR_NULLPAD);
	H5T_str_t pad = vlst.getStrpad();
	verify_val(pad, H5T_STR_NULLPAD, "StrType::getStrpad", __LINE__, __FILE__);

	// Convert to variable-length string.
	vlst.setSize(H5T_VARIABLE);

	// Check if datatype is VL string.
	H5T_class_t type_class = vlst.getClass();
	verify_val(type_class, H5T_STRING, "DataType::getClass", __LINE__, __FILE__);
	bool is_variable_str = vlst.isVariableStr();
	verify_val(is_variable_str, true, "DataType::isVariableStr", __LINE__, __FILE__);

	// Check default character set and padding.
	H5T_cset_t cset = vlst.getCset();
	verify_val(cset, H5T_CSET_ASCII, "StrType::getCset", __LINE__, __FILE__);
	pad = vlst.getStrpad();
	verify_val(pad, H5T_STR_NULLPAD, "StrType::getStrpad", __LINE__, __FILE__);

	// Commit variable-length string datatype to storage.
	vlst.commit(*file1, VLSTR_TYPE);

	// Close datatype.
	vlst.close();

	// Try opening datatype again.
	vlst = file1->openStrType(VLSTR_TYPE);

	// Close datatype and file.
	vlst.close();
	file1->close();
        delete file1;

	// Open file.
	file1 = new H5File(FILENAME, H5F_ACC_RDWR);

	// Open the variable-length string datatype just created
	vlst = file1->openStrType(VLSTR_TYPE);

	// Verify character set and padding
	cset = vlst.getCset();
	verify_val(cset, H5T_CSET_ASCII, "StrType::getCset", __LINE__, __FILE__);
	pad = vlst.getStrpad();
	verify_val(pad, H5T_STR_NULLPAD, "StrType::getStrpad", __LINE__, __FILE__);

	// Close datatype and file
	vlst.close();
	file1->close();

	PASSED();
    } // end try block

    // Catch all exceptions.
    catch (Exception E)
    {
        issue_fail_msg("test_vlstring_type()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(file1)
	delete file1;
} // end test_vlstring_type()

/*-------------------------------------------------------------------------
 * Function:	test_compact_vlstring
 *
 * Purpose:	Test storing VL strings in compact datasets.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
static void test_compact_vlstring()
{
    // Output message about test being performed
    SUBTEST("VL Strings on Compact Dataset");

    try {
	// Create file
	H5File file1(FILENAME, H5F_ACC_TRUNC);

	// Create dataspace for datasets
        hsize_t dims1[] = {SPACE1_DIM1};
        DataSpace sid1(SPACE1_RANK, dims1);

	// Create a datatype to refer to
	StrType vlst(0, H5T_VARIABLE);

	// Create dataset create property list and set layout
	DSetCreatPropList plist;
	plist.setLayout(H5D_COMPACT);

	// Create a dataset
	DataSet dataset(file1.createDataSet("Dataset5", vlst, sid1, plist));

	// Write dataset to disk
	const char *wdata[SPACE1_DIM1] = {"one", "two", "three", "four"};
	dataset.write(wdata, vlst);

	// Read dataset from disk
	char *rdata[SPACE1_DIM1];   // Information read in
	dataset.read(rdata, vlst);

	// Compare data read in
	hsize_t i;
	for (i=0; i<SPACE1_DIM1; i++) {
	    if (HDstrlen(wdata[i])!=strlen(rdata[i])) {
		TestErrPrintf("VL data length don't match!, strlen(wdata[%d])=%d, strlen(rdata[%d])=%d\n",(int)i,(int)strlen(wdata[i]),(int)i,(int)strlen(rdata[i]));
		continue;
	    } // end if
	    if (HDstrcmp(wdata[i],rdata[i]) != 0) {
		TestErrPrintf("VL data values don't match!, wdata[%d]=%s, rdata[%d]=%s\n",(int)i,wdata[i],(int)i,rdata[i]);
		continue;
	    } // end if
	} // end for

	// Reclaim the read VL data
	DataSet::vlenReclaim((void *)rdata, vlst, sid1);

	// Close objects and file
	dataset.close();
	vlst.close();
	sid1.close();
	plist.close();
	file1.close();

	PASSED();
    } // end try

    // Catch all exceptions.
    catch (Exception E)
    {
        issue_fail_msg("test_compact_vlstrings()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_compact_vlstrings

/*-------------------------------------------------------------------------
 * Function:	test_vlstring_attribute
 *
 * Purpose:	Test writing/reading VL strings on attributes.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
// String for writing to attribute
static char *string_att_write=NULL;

// Info for a string attribute
const H5std_string ATTRSTR_NAME("String_attr");
const H5std_string ATTRSTR_DATA("String Attribute");

static void test_vlstring_attribute()
{
    // Output message about test being performed
    SUBTEST("VL String on Attributes");

    try {
	// Open the file
	H5File file1(FILENAME, H5F_ACC_RDWR);

	// Create a datatype to refer to.
	StrType vlst(0, H5T_VARIABLE);

	// Open the root group.
	Group root = file1.openGroup("/");

	// Create dataspace for the attribute.
	DataSpace att_space (H5S_SCALAR);

	// Create an attribute for the root group.
	Attribute gr_attr = root.createAttribute(ATTRSTR_NAME, vlst, att_space);

	// Write data to the attribute.
	gr_attr.write(vlst, ATTRSTR_DATA);

	// Read and verify the attribute string as a string of chars.
	char *string_att_check;
	gr_attr.read(vlst, &string_att_check);
	if(HDstrcmp(string_att_check, ATTRSTR_DATA.c_str())!=0)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,string_att_check=%s\n",__LINE__, ATTRSTR_DATA.c_str(), string_att_check);

	HDfree(string_att_check);  // note: no need for std::string test

	// Read and verify the attribute string as an std::string.
	H5std_string read_str;
	gr_attr.read(vlst, read_str);
	if (read_str != ATTRSTR_DATA)
	    TestErrPrintf("Line %d: Attribute data different: ATTRSTR_DATA=%s,read_str=%s\n",__LINE__, ATTRSTR_DATA.c_str(), read_str.c_str());

	// Close group's attribute.
	gr_attr.close();

	// Test creating a "large" sized string attribute
	gr_attr = root.createAttribute("test_scalar_large", vlst, att_space);

	string_att_write = (char*)HDcalloc(8192, sizeof(char));
	HDmemset(string_att_write, 'A', 8191);

	// Write data to the attribute, then read it back.
	gr_attr.write(vlst, &string_att_write);
	gr_attr.read(vlst, &string_att_check);

	// Verify data read.
	if(HDstrcmp(string_att_check,string_att_write)!=0)
	    TestErrPrintf("VL string attributes don't match!, string_att_write=%s, string_att_check=%s\n",string_att_write,string_att_check);

	// Release resources.
	HDfree(string_att_check);
	HDfree(string_att_write);
	gr_attr.close();
	file1.close();

	PASSED();
    } // end try block

    // Catch all exceptions.
    catch (Exception E) {
	issue_fail_msg("test_vlstring_attribute()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_vlstring_attribute()

/*-------------------------------------------------------------------------
 * Function:	test_read_vl_string_attribute
 *
 * Purpose:	Test reading VL strings from attributes.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
static void test_read_vl_string_attribute()
{

    // Output message about test being performed
    SUBTEST("reading VL String as attributes");

    try {
	// Open file
	H5File file1(FILENAME, H5F_ACC_RDONLY);

	// Create a datatype to refer to.
	StrType vlst(0, H5T_VARIABLE);

	// Open the root group and its attribute named ATTRSTR_NAME.
	Group root = file1.openGroup("/");
	Attribute att = root.openAttribute(ATTRSTR_NAME);

	// Test reading "normal" sized string attribute
	char *string_att_check;
	att.read(vlst, &string_att_check);
	if(HDstrcmp(string_att_check,ATTRSTR_DATA.c_str())!=0)
	    TestErrPrintf("VL string attributes don't match!, string_att=%s, string_att_check=%s\n",ATTRSTR_DATA.c_str(),string_att_check);
	HDfree(string_att_check);
	att.close();

	// Test reading "large" sized string attribute
	att = root.openAttribute("test_scalar_large");
	att.read(vlst, &string_att_check);
	if(HDstrcmp(string_att_check,string_att_write)!=0)
	    TestErrPrintf("VL string attributes don't match!, string_att_write=%s, string_att_check=%s\n",string_att_write,string_att_check);
	HDfree(string_att_check);
	HDfree(string_att_write);   // Free string allocated in test_write_vl_string_attribute

	// Close objects and file.
	att.close();
	vlst.close();
	root.close();
	file1.close();

	PASSED();
    } // end try

    // Catch all exceptions.
    catch (Exception E) {
	issue_fail_msg("test_read_vl_string_attribute()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // test_read_vl_string_attribute

/*-------------------------------------------------------------------------
 * Function:	test_vlstring_array_attribute
 *
 * Purpose:	Test writing/reading VL string array to/from attributes.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler
 *		July, 2009
 *
 *-------------------------------------------------------------------------
 */
const H5std_string ATTRSTRARR_NAME("StringArray_attr");

static void test_vlstring_array_attribute()
{
    const char *string_att_array[SPACE1_DIM1]= {
        "Line 1", "Line 2", "Line 3", "Line 4"
        };   // Information to write

    // Output message about test being performed
    SUBTEST("VL String Array on Attributes");

    try {
	// Open the file
	H5File file1(FILENAME, H5F_ACC_RDWR);

	// Create a datatype to refer to.
	StrType vlst(0, H5T_VARIABLE);

	// Open the root group.
	Group root = file1.openGroup("/");

        // Create dataspace for datasets.
        hsize_t dims1[] = {SPACE1_DIM1};
        DataSpace att_space(SPACE1_RANK, dims1);

	// Create an attribute for the root group.
	Attribute gr_attr = root.createAttribute(ATTRSTRARR_NAME, vlst, att_space);

	// Write data to the attribute.
	gr_attr.write(vlst, string_att_array);

	// Read and verify the attribute string as a string of chars.
	// Note: reading by array of H5std_string doesn't work yet.
	char *string_att_check[SPACE1_DIM1];
	gr_attr.read(vlst, &string_att_check);

	int ii;
	for (ii = 0; ii < SPACE1_DIM1; ii++)
	{
	    if(HDstrcmp(string_att_check[ii], string_att_array[ii])!=0)
		TestErrPrintf("Line %d: Attribute data different: written=%s,read=%s\n",__LINE__, string_att_check[ii], string_att_check[ii]);

	    HDfree(string_att_check[ii]);  // note: no need for std::string test
	}

	// Close group's attribute.
	gr_attr.close();
	file1.close();

	PASSED();
    } // end try block

    // Catch all exceptions.
    catch (Exception E) {
	issue_fail_msg("test_vlstring_array_attribute()", __LINE__, __FILE__, E.getCDetailMsg());
    }
}   // test_vlstring_array_attribute()

/* Helper routine for test_vl_rewrite() */
static void write_scalar_dset(H5File& file, DataType& type, DataSpace& space,
				char *name, char *data)
{
    DataSet dset;
    try {
	dset = file.createDataSet(name, type, space);
	dset.write(&data, type, space, space);
	dset.close();
    } // end try
    catch (FileIException ferr) {
	throw;
    }
    catch (DataSetIException derr) {
	throw;
    }
}

/* Helper routine for test_vl_rewrite() */
static void read_scalar_dset(H5File& file, DataType& type, DataSpace& space,
				char *name, char *data)
{
    char *data_read;
    DataSet dset;
    try {
	dset = file.openDataSet(name);
	dset.read(&data_read, type, space, space);
	dset.close();

	if(HDstrcmp(data, data_read))
	    TestErrPrintf("Expected %s for dataset %s but read %s\n", data, name, data_read);

	HDfree(data_read);
    } // end try
    catch (FileIException ferr) {
	throw;
    }
    catch (DataSetIException derr) {
	throw;
    }
}

/*-------------------------------------------------------------------------
 * Function:	test_vl_rewrite
 *
 * Purpose:	Test I/O on VL strings when many objects in the file
 *		have been linked/unlinked.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler (use C version)
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
const H5std_string FILENAME2("tvlstr2.h5");
const int REWRITE_NDATASETS = 32;
static void test_vl_rewrite()
{
    // Output message about test being performed
    SUBTEST("I/O on VL strings with link/unlink");

    try {
	// Create the files.
	H5File file1(FILENAME, H5F_ACC_TRUNC);
	H5File file2(FILENAME2, H5F_ACC_TRUNC);

	// Create the VL string datatype.
	StrType type(0, H5T_VARIABLE);

	// Create dataspace for the attribute.
	DataSpace space (H5S_SCALAR);

	// Create in file 1.
	int i;
	char name[256]; 	// Buffer for names & data
	for (i=0; i<REWRITE_NDATASETS; i++) {
	    sprintf(name, "/set_%d", i);
	    write_scalar_dset(file1, type, space, name, name);
	}

	// Effectively copy data from file 1 to 2.
	for (i=0; i<REWRITE_NDATASETS; i++) {
	    sprintf(name, "/set_%d", i);
	    read_scalar_dset(file1, type, space, name, name);
	    write_scalar_dset(file2, type, space, name, name);
	}

	// Read back from file 2.
	for (i=0; i<REWRITE_NDATASETS; i++) {
	    sprintf(name, "/set_%d", i);
	    read_scalar_dset(file2, type, space, name, name);
	}

	// Remove from file 2.
	for (i=0; i<REWRITE_NDATASETS; i++) {
	    sprintf(name, "/set_%d", i);
	    file2.unlink(name);
	}

	// Effectively copy from file 1 to file 2.
	for (i=0; i<REWRITE_NDATASETS; i++) {
	    sprintf(name, "/set_%d", i);
	    read_scalar_dset(file1, type, space, name, name);
	    write_scalar_dset(file2, type, space, name, name);
	}

	// Close objects and file.
	type.close();
	space.close();
	file1.close();
	file2.close();

	PASSED();
    } // end try

    // Catch all exceptions.
    catch (Exception E) {
	issue_fail_msg("test_vl_rewrite()", __LINE__, __FILE__, E.getCDetailMsg());
    }
} // end test_vl_rewrite()

/*-------------------------------------------------------------------------
 * Function:	test_vlstrings
 *
 * Purpose:	VL string testing main routine.
 *
 * Return:	None
 *
 * Programmer:	Binh-Minh Ribler
 *		January, 2007
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void test_vlstrings()
{
    // Output message about test being performed
    //MESSAGE("Testing Variable-Length Strings");
    MESSAGE(5, ("Testing Variable-Length Strings"));

    // These tests use the same file
    // Test basic VL string datatype
    test_vlstring_dataset();
    test_vlstrings_special();
    test_vlstring_type();
    test_compact_vlstring();

    // Test using VL strings in attributes
    test_vlstring_attribute();

    // Test using VL string array in attributes and datasets
    test_vlstring_array_attribute();
    test_vlstring_array_dataset();

    // Test writing VL datasets in files with lots of unlinking
    test_vl_rewrite();

}   // test_vlstrings()


/*-------------------------------------------------------------------------
 * Function:	cleanup_vlstrings
 *
 * Purpose:	Cleanup temporary test files
 *
 * Return:	none
 *
 * Programmer:	Quincey Koziol
 *              September 10, 1999
 *
 * Modifications:
 *
 *-------------------------------------------------------------------------
 */
#ifdef __cplusplus
extern "C"
#endif
void cleanup_vlstrings()
{
    HDremove(FILENAME.c_str());
    HDremove(FILENAME2.c_str());
}

