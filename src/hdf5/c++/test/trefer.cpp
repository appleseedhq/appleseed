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
   trefer.cpp - HDF5 C++ testing the functionalities associated with the C
		Reference interface (H5R)

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

const H5std_string      FILE1("trefer1.h5");
const H5std_string      FILE2("trefer2.h5");
const H5std_string      FILE3("trefer3.h5");
const H5std_string      DSET_DEFAULT_NAME("default");

// Dataset 1
const H5std_string      DSET1_NAME("Dataset1");
const int DSET1_LEN = 8;

const H5std_string MEMBER1( "a_name" );
const H5std_string MEMBER2( "b_name" );
const H5std_string MEMBER3( "c_name" );

// 1-D dataset with fixed dimensions
const H5std_string      SPACE1_NAME("Space1");
const int SPACE1_RANK = 1;
const int SPACE1_DIM1 = 4;

// 2-D dataset with fixed dimensions
const H5std_string      SPACE2_NAME("Space2");

// Larger 1-D dataset with fixed dimensions
const H5std_string      SPACE3_NAME("Space3");

// Compound datatype
typedef struct s1_t {
    unsigned int a;
    unsigned int b;
    float c;
} s1_t;

/****************************************************************
**
**  test_reference_obj(): Test basic object reference functionality.
**      Tests references to various kinds of objects
**
****************************************************************/
static void test_reference_obj(void)
{
    int    i;          // counting variables
    const  H5std_string write_comment="Foo!"; // Comments for group

    // Output message about test being performed
    SUBTEST("Object Reference Functions");

    H5File* file1 = NULL;
    try {
	hobj_ref_t *wbuf,      // buffer to write to disk
		   *rbuf,      // buffer read from disk
		   *tbuf;      // temp. buffer read from disk

	// Allocate write & read buffers
	int temp_size = MAX(sizeof(unsigned),sizeof(hobj_ref_t));
	wbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);
	rbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);
	tbuf=(hobj_ref_t*)HDmalloc(temp_size*SPACE1_DIM1);

        // Create file FILE1
        file1 = new H5File (FILE1, H5F_ACC_TRUNC);

	// Create dataspace for datasets
	hsize_t	dims1[] = {SPACE1_DIM1};
	DataSpace sid1(SPACE1_RANK, dims1);

	// Create a group
	Group group = file1->createGroup("Group1", (size_t)-1);

	// Set group's comment
	group.setComment(".", write_comment);

	// Create a dataset (inside /Group1)
	DataSet dataset = group.createDataSet(DSET1_NAME, PredType::NATIVE_UINT, sid1);

	unsigned *tu32;      // Temporary pointer to uint32 data
	for (tu32=(unsigned *)wbuf, i=0; i<SPACE1_DIM1; i++)
	    *tu32++=i*3; // from C test

	// Write selection to disk
	dataset.write(wbuf, PredType::NATIVE_UINT);

	// Close Dataset
	dataset.close();

	// Create another dataset (inside /Group1)
	dataset = group.createDataSet("Dataset2", PredType::NATIVE_UCHAR, sid1);

	// Close Dataset
	dataset.close();

	// Create a datatype to refer to
	CompType dtype1(sizeof(s1_t));

	// Insert fields
	dtype1.insertMember(MEMBER1, HOFFSET(s1_t, a), PredType::NATIVE_INT);
	dtype1.insertMember(MEMBER2, HOFFSET(s1_t, b), PredType::NATIVE_INT);
	dtype1.insertMember(MEMBER3, HOFFSET(s1_t, c), PredType::NATIVE_FLOAT);

	// Save datatype for later
	dtype1.commit(group, "Datatype1");

	// Close datatype and group
	dtype1.close();
	group.close();

	// Create a dataset
	dataset = file1->createDataSet("Dataset3", PredType::STD_REF_OBJ, sid1);

	// Create reference to dataset
	file1->reference(&wbuf[0], "/Group1/Dataset1");

#ifndef H5_NO_DEPRECATED_SYMBOLS
	H5G_obj_t obj_type = dataset.getObjType(&wbuf[0], H5R_OBJECT);
	verify_val(obj_type, H5G_DATASET, "DataSet::getObjType", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Create reference to dataset
	file1->reference(&wbuf[1], "/Group1/Dataset2");
#ifndef H5_NO_DEPRECATED_SYMBOLS
	obj_type = dataset.getObjType(&wbuf[1], H5R_OBJECT);
	verify_val(obj_type, H5G_DATASET, "DataSet::getObjType", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Create reference to group
	file1->reference(&wbuf[2], "/Group1");
#ifndef H5_NO_DEPRECATED_SYMBOLS
	obj_type = dataset.getObjType(&wbuf[2], H5R_OBJECT);
	verify_val(obj_type, H5G_GROUP, "DataSet::getObjType", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Create reference to named datatype
	file1->reference(&wbuf[3], "/Group1/Datatype1");
#ifndef H5_NO_DEPRECATED_SYMBOLS
	obj_type = dataset.getObjType(&wbuf[3], H5R_OBJECT);
	verify_val(obj_type, H5G_TYPE, "DataSet::getObjType", __LINE__, __FILE__);
#endif /* H5_NO_DEPRECATED_SYMBOLS */

	// Write selection to disk
	dataset.write(wbuf, PredType::STD_REF_OBJ);

	// Close disk dataspace, dataset, and file
	sid1.close();
	dataset.close();
	delete file1;

	// Re-open the file
	file1 = new H5File(FILE1, H5F_ACC_RDWR);

	// Open the dataset
	dataset = file1->openDataSet("/Dataset3");

	// Read selection from disk
	dataset.read(rbuf, PredType::STD_REF_OBJ);

	// Dereference dataset object by ctor, from the location where
	// 'dataset' is located
	DataSet dset2(dataset, &rbuf[0]);

	// Check information in the referenced dataset
	sid1 = dset2.getSpace();
	hssize_t n_elements = sid1.getSimpleExtentNpoints();
	verify_val((long)n_elements, 4, "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Read from disk
	dset2.read(tbuf, PredType::NATIVE_UINT);

	for(tu32=(unsigned *)tbuf,i=0; i<SPACE1_DIM1; i++,tu32++)
	   verify_val(*tu32, (uint32_t)(i*3), "DataSpace::getSimpleExtentNpoints", __LINE__, __FILE__);

	// Close dereferenced dataset
	dset2.close();

	// Dereference group object from the location where 'dataset' is located
	group.dereference(dataset, &rbuf[2]);

	// Get group's comment
	H5std_string read_comment1 = group.getComment(".", 10);
	verify_val(read_comment1, write_comment, "Group::getComment", __LINE__, __FILE__);

	// Test that getComment handles failures gracefully
	try {
	    H5std_string read_comment_tmp = group.getComment(NULL);
	}
	catch (Exception E) {} // We expect this to fail

	// Test reading the name of an item in the group

	// Test getObjnameByIdx(idx)
	H5std_string name;
	name = group.getObjnameByIdx(0);
	verify_val(name, DSET1_NAME, "Group::getObjnameByIdx", __LINE__, __FILE__);
	// Test getObjnameByIdx(hsize_t idx, H5std_string& name, size_t size)
	name.clear();
	ssize_t name_size = group.getObjnameByIdx(0, name, 5);
	verify_val(name, "Data", "Group::getObjnameByIdx(index,(std::string)buf,buf_len)", __LINE__, __FILE__);
	verify_val(name_size, DSET1_LEN, "Group::getObjnameByIdx(index,(std::string)buf,buf_len)", __LINE__, __FILE__);

	name.clear();
	name_size = group.getObjnameByIdx(0, name, name_size+1);
	verify_val(name, DSET1_NAME, "Group::getObjnameByIdx(index,(std::string)buf,buf_len)", __LINE__, __FILE__);
	verify_val(name_size, DSET1_LEN, "Group::getObjnameByIdx(index,(std::string)buf,buf_len)", __LINE__, __FILE__);

	// Test getObjnameByIdx(hsize_t idx, char* name, size_t size)
	char name_C[DSET1_LEN+1];
	group.getObjnameByIdx(0, name, name_size+1);
	verify_val(name, DSET1_NAME, "Group::getObjnameByIdx(index,(char*)buf,buf_len)", __LINE__, __FILE__);
	verify_val(name_size, DSET1_LEN, "Group::getObjnameByIdx(index,(char*)buf,buf_len)", __LINE__, __FILE__);

#ifndef H5_NO_DEPRECATED_SYMBOLS
	// Test getting the type of objects
	
	// Test getObjTypeByIdx(hsize_t idx)
	obj_type = group.getObjTypeByIdx(0);
	verify_val(obj_type, H5G_DATASET, "Group::getObjTypeByIdx(index)", __LINE__, __FILE__);

	// Test getObjTypeByIdx(hsize_t idx, char* type_name)
	obj_type = H5G_UNKNOWN;
	char type_name_C[256];
	obj_type = group.getObjTypeByIdx(0, type_name_C);
	verify_val(obj_type, H5G_DATASET, "Group::getObjTypeByIdx(index, (char*)name)", __LINE__, __FILE__);
	verify_val((const char*)type_name_C, (const char*)"dataset", "Group::getObjTypeByIdx(index, (char*)name)", __LINE__, __FILE__);

	// Test getObjTypeByIdx(hsize_t idx, H5std_string& type_name)
	obj_type = H5G_UNKNOWN;
	H5std_string type_name;
	obj_type = group.getObjTypeByIdx(0, type_name);
	verify_val(obj_type, H5G_DATASET, "Group::getObjTypeByIdx(index, (char*)name)", __LINE__, __FILE__);
	verify_val(type_name, "dataset", "Group::getObjTypeByIdx(index, (char*)name)", __LINE__, __FILE__);

#endif // ifndef H5_NO_DEPRECATED_SYMBOLS

	// Close group
	group.close();

	// Dereference group object using file to specify location
	group.dereference(*file1, &rbuf[2]);
	H5std_string read_comment2 = group.getComment(".", 10);
	verify_val(read_comment2, write_comment, "Group::getComment", __LINE__, __FILE__);
	group.close();

	// Dereference group object by ctor and using dataset to specify
	// location
	Group new_group(dataset, &rbuf[2]);
	H5std_string read_comment3 = new_group.getComment(".", 10);
	verify_val(read_comment3, write_comment, "Group::getComment", __LINE__, __FILE__);
	new_group.close();

	// Dereference datatype object from the location where 'dataset'
	// is located
	dtype1.dereference(dataset, &rbuf[3]);

	// Verify correct datatype
	H5T_class_t tclass = dtype1.getClass();
	verify_val(tclass, H5T_COMPOUND, "DataType::getClass", __LINE__, __FILE__);

	int n_members = dtype1.getNmembers();
	verify_val(n_members, 3, "DataType::getNmembers", __LINE__, __FILE__);

	// Close datatype
	dtype1.close();

	// Dereference datatype object by ctor, using file to specify location
	DataType dtype2(*file1, &rbuf[3]);

	// Verify correct datatype
	H5T_class_t tclass2 = dtype2.getClass();
	verify_val(tclass2, H5T_COMPOUND, "DataType::getClass", __LINE__, __FILE__);

	// Close datatype
	dtype2.close();

	// Close dataset and file
	dataset.close();
	file1->close();

	// Free memory buffers
	HDfree(wbuf);
	HDfree(rbuf);
	HDfree(tbuf);

	PASSED();
    } // end try
    catch (Exception E) {
	issue_fail_msg("test_reference_obj()", __LINE__, __FILE__, E.getCDetailMsg());
    }

    if(file1)
        delete file1;
}   // test_reference_obj()

/****************************************************************
**
**  test_reference(): Main reference testing routine.
**
****************************************************************/
#ifdef __cplusplus
extern "C"
#endif
void test_reference(void)
{
    // Output message about test being performed
    //MESSAGE("Testing References\n");
    MESSAGE(5, ("Testing References\n"));

    test_reference_obj();       // Test basic object reference functionality

}   // test_reference()


/****************************************************************
** Function:	cleanup_reference
** Purpose:	Cleanup temporary test files
** Return:	none
****************************************************************/
#ifdef __cplusplus
extern "C"
#endif
void cleanup_reference(void)
{
    HDremove(FILE1.c_str());
}

